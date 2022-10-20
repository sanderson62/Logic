      *((program: EL130.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL130 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 04/18/95 07:28:16.
000007*                            VMOD=2.055.
000008*
000009*
000010*AUTHOR.     LOGIC,INC.
000011*            DALLAS, TEXAS.
000012
000013*REMARKS.    TRANSACTION - EX19 - NEW CLAIM SETUP
000014*
000015******************************************************************
000016*                   C H A N G E   L O G
000017*
000018* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000019*-----------------------------------------------------------------
000020*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000021* EFFECTIVE    NUMBER
000022*-----------------------------------------------------------------
000023* 041002    2002040200004  SMVA  MAKE SURE CLAIM TYPE IS ALWAYS
000024*                               POPULATED -MOVE PI-LAST-CLAIM-TYPE
000025* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
000026* 031405                   PEMA  ADD PROCESSING FOR NEW CLAM TYP G
000027* 050807    2007020800002  AJRA  AUTO POPULATE I1 ADDRESS FROM CER
000028* 071508    2008071000003  AJRA  ADD EDIT FOR SOC SEC NUMBER
000029* 012009    2007042600001  PEMA  RESTRICT CLAIM TYPE FOR CID
000030* 041309    2009031600001  AJRA  ADD VIN TO DIAG FOR BENE 55 AND 5
000031* 100809    2009081800004  AJRA  ADD MSG FOR BENE CD 2O - 2V
000032* 061511    2011042000002  AJRA  VERIFY 2ND BENEFICIARY SSN
000033* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000034* 020513  CR2011090100001  PEMA  FORCE ENTRY OF SEX CODE
000035* 020613    2012092400007  AJRA  ADD CAUSAL STATE MESSAGE
000036* 042413    2013042300001  AJRA  FIX SPECIAL MSG WHEN MULTIPLE CER
000037* 052113    2012113000002  PEMA  ADD SPECIAL STUFF FOR SPP DDF
000038* 032514    2013111100001  AJRA  VERIFY SSN FOR DISAB PMTS
000039* 040814    2014030500002  AJRA  ADD ICD CODES
000040* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000041* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
000042* 070714    2014052800001  PEMA  correct read on erpdef for DCC
000043* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000044* 101917  CR2017083000003  TANA  ADD CONTRACT CONTESTABLE MSG
000045* 121417  IR2017121200001  PEMA  Correct assignment of ben period
000046* 011118  CR2016052500002  TANA  Add Message for pre-existing
000047* 052918  CR2018031500002  TANA  Add Message for filing time limit
000048* 061418  IR2018053000003  TANA  Fix Causal state / filing limit
000049* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
000050* 071720  CR2019112600001  TANA  Remove filing time limit error
000051* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000052* 090821  CR2021081200003  PEMA  Add error if inc > act cnc dt
000053* 110921  CR2021051200001  PEMA  Onbase Workflow project
000054* 022122  CR2021100800003  PEMA  Add B and H claim types
000055******************************************************************
000056
000057     EJECT
000058 ENVIRONMENT DIVISION.
000059 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000061 77  FILLER  PIC X(32)  VALUE '********************************'.
000062 77  FILLER  PIC X(32)  VALUE '*    EL130 WORKING STORAGE     *'.
000063 77  FILLER  PIC X(32)  VALUE '********** VMOD=2.055 **********'.
000064 77  A1                          PIC S999 COMP-3 VALUE +0.
000065 77  E1                          PIC S999 COMP-3 VALUE +0.
000066 77  s1                          pic s999 comp-3 value +0.
000067 77  s2                          pic s999 comp-3 value +0.
000068 77  P1                          PIC S999 COMP-3 VALUE +0.
000069 77  P2                          PIC S999 COMP-3 VALUE +0.
000070 77  WS-CRIT-PER-RECURRENT       PIC 99    VALUE zeros.
000071 77  WS-MAX-BENEFITS             PIC 99    VALUE ZEROS.
000072 77  WS-CRIT-PER-RTW-MOS         PIC 99    VALUE ZEROS.
000073 77  WS-EXCL-PERIOD              PIC S999 COMP-3 VALUE +0.
000074 77  WS-COV-ENDS                 PIC S999 COMP-3 VALUE +0.
000075 77  WS-ACC-PERIOD               PIC S999 COMP-3 VALUE +0.
000076 77  WS-MONTHS-BETWEEN           PIC S999 COMP-3 VALUE +0.
000077 77  WS-ERPDEF-SW                PIC X     VALUE ' '.
000078     88  ERPDEF-FOUND                 VALUE 'Y'.
000079 77  ws-monthly-benefit          pic s9(11)v99 comp-3 value +0.
000080 77  WS-BENEFITS-PREV-PAID       PIC S9(4)V999 VALUE +0  COMP-3.
000081 77  ws-dcc-product-code         pic xxx value spaces.
000082 77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
000083 77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
000084 77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
000085 77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
000086 77  ws-pd-bens                  pic s9(5) comp-3 value +0.
000087 77  ws-zero-bens-avail          pic x value ' '.
000088 77  WS-PRE-EXISTING-PER         PIC 99    VALUE ZEROS.
000089 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
000090
000091
000092*    COPY ELCSCTM.
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
000093
000094*    COPY ELCSCRTY.
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
000095     EJECT
000096 01  WS-DATE-AREA.
000097     05  SAVE-DATE           PIC X(8)    VALUE SPACES.
000098     05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
000099     05  SAVE-SEND-DT        PIC XX      VALUE LOW-VALUES.
000100
000101 01  ws-dcc-error-line.
000102     05  filler occurs 15.
000103         10  ws-error-no        pic x(4).
000104
000105 01  WS-CRI-SSN-WORK-AREA.
000106     05  WS-CRI-SOC-SEC-NO.
000107         10  WS-SS-REPT-CD       PIC X(5).
000108         10  WS-SS-LAST-NAME     PIC X(5).
000109         10  WS-SS-INITIAL       PIC X.
000110
000111     05  WS-CRI-REPORT-CODE.
000112         10  FILLER              PIC X(5).
000113         10  WS-CRI-RPT-CD-MOVE  PIC X(5).
000114
000115     05  WS-CRI-LAST-NAME.
000116         10  WS-CRI-NAME-MOVE    PIC X(5).
000117         10  FILLER              PIC X(10).
000118
000119 01  filler.
000120     05  ws-prev-inc-dt          pic xx value low-values.
000121     05  ws-mob-cert-ind         pic x value ' '.
000122         88  mob-cert        value 'M'.
000123     05  ws-eracct-startbr-ind   pic x  value spaces.
000124         88  eracct-browse-started  value 'Y'.
000125     05  ws-lo-acct-dt           pic xx value low-values.
000126     05  ws-hi-acct-dt           pic xx value low-values.
000127     05  ws-acct-status          pic x value spaces.
000128         88  acct-cancelled          value '3'.
000129     05  WS-I-SAY-STOP-IND       PIC X  VALUE ' '.
000130         88  i-say-STOP            value 'S'.
000131
000132 01  STANDARD-AREAS.
000133     12  er-1679-text        pic x(60) value
000134       '1679-N CONTRACT IS NOT CONTESTABLE'.
000135     12  er-1682-text        pic x(60) value
000136       '1682-N INC DATE > MOB ACCOUNT CANCEL DATE'.
000137     12  er-1683-text        pic x(60) value
000138       '1683-N INC DATE < CERTIFICATE EFF DATE'.
000139     12  SC-ITEM             PIC S9(4)   VALUE +0001      COMP.
000140     12  GETMAIN-SPACE       PIC X       VALUE SPACES.
000141     12  MAP-NAME            PIC X(8)    VALUE 'EL130A'.
000142     12  MAPSET-NAME         PIC X(8)    VALUE 'EL130S'.
000143     12  TRANS-ID            PIC X(4)    VALUE 'EX19'.
000144     12  START-TRANS-ID      PIC X(4)    VALUE 'EX58'.
000145     12  THIS-PGM            PIC X(8)    VALUE 'EL130'.
000146     12  PGM-NAME            PIC X(8).
000147     12  TIME-IN             PIC S9(7).
000148     12  TIME-OUT-R  REDEFINES TIME-IN.
000149         16  FILLER          PIC X.
000150         16  TIME-OUT        PIC 99V99.
000151         16  FILLER          PIC X(2).
000152     12  XCTL-005            PIC X(8)    VALUE 'EL005'.
000153     12  XCTL-010            PIC X(8)    VALUE 'EL010'.
000154     12  XCTL-114            PIC X(8)    VALUE 'EL114'.
000155     12  XCTL-126            PIC X(8)    VALUE 'EL126'.
000156     12  XCTL-127            PIC X(8)    VALUE 'EL127'.
000157     12  XCTL-727            PIC X(8)    VALUE 'EL727'.
000158     12  XCTL-131            PIC X(8)    VALUE 'EL131'.
000159     12  XCTL-132            PIC X(8)    VALUE 'EL132'.
000160     12  XCTL-141            PIC X(8)    VALUE 'EL141'.
000161     12  XCTL-150            PIC X(8)    VALUE 'EL150'.
000162     12  XCTL-157            PIC X(8)    VALUE 'EL157'.
000163     12  XCTL-650            PIC X(8)    VALUE 'EL650'.
000164     12  XCTL-659            PIC X(8)    VALUE 'EL659'.
000165     12  XCTL-6592           PIC X(8)    VALUE 'EL6592'.
000166     12  XCTL-725            PIC X(8)    VALUE 'EL725'.
000167     12  LINK-001            PIC X(8)    VALUE 'EL001'.
000168     12  LINK-004            PIC X(8)    VALUE 'EL004'.
000169     12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
000170     12  LINK-ELRTRM         PIC X(8)    VALUE 'ELRTRM'.
000171     12  LINK-1523           PIC X(8)    VALUE 'EL1523'.
000172     12  ELCNTL-DSID         PIC X(8)    VALUE 'ELCNTL'.
000173     12  ELMSTR-DSID         PIC X(8)    VALUE 'ELMSTR'.
000174     12  ELMSTR5-DSID        PIC X(8)    VALUE 'ELMSTR5'.
000175     12  ELTRLR-DSID         PIC X(8)    VALUE 'ELTRLR'.
000176     12  ERACCT2-DSID        PIC X(8)    VALUE 'ERACCT2'.
000177     12  ELCERT-DSID         PIC X(8)    VALUE 'ELCERT'.
000178     12  ELCERT5-DSID        PIC X(8)    VALUE 'ELCERT5'.
000179     12  ELACTQ-DSID         PIC X(8)    VALUE 'ELACTQ'.
000180     12  ELARCH-DSID         PIC X(8)    VALUE 'ELARCH'.
000181     12  ELBENE-DSID         PIC X(8)    VALUE 'ELBENE'.
000182     12  ERREIN-DSID         PIC X(8)    VALUE 'ERREIN'.
000183     12  DLYACTV-DSID        PIC X(8)    VALUE 'DLYACTV'.
000184     12  ERMAIL-DSID         PIC X(8)    VALUE 'ERMAIL'.
000185     12  ELCRTT-DSID         PIC X(8)    VALUE 'ELCRTT'.
000186     12  FILE-ID             PIC X(8).
000187     12  DLYACTV-LENGTH      PIC S9(04)    COMP     VALUE +25.
000188     12  ELACTQ-LENGTH       PIC S9(04)    COMP     VALUE +60.
000189     12  ELARCH-LENGTH       PIC S9(04)    COMP     VALUE +90.
000190     12  ELCERT-LENGTH       PIC S9(04)    COMP     VALUE +450.
000191     12  ELMSTR-LENGTH       PIC S9(04)    COMP     VALUE +350.
000192     12  ELTRLR-LENGTH       PIC S9(04)    COMP     VALUE +200.
000193     12  ELMSTR-GENERIC-LENGTH PIC S9(4)   VALUE +9      COMP.
000194     12  ERMAIL-LENGTH       PIC S9(04)    COMP     VALUE +374.
000195     12  ELCRTT-LENGTH       PIC S9(04)    COMP     VALUE +552.
000196
000197 01  TERM-CALCULATION-WORK-AREA     COMP-3.
000198     12  M                   PIC S9(7)V99           VALUE ZEROS.
000199     12  L                   PIC S9(7)V99           VALUE ZEROS.
000200     12  N                   PIC S9(3)              VALUE ZEROS.
000201     12  N-STORE             PIC S9(3)              VALUE ZEROS.
000202     12  NV-STORE            PIC S9(3)              VALUE ZEROS.
000203     12  I                   PIC S99V9(5)           VALUE ZEROS.
000204     12  A-N                 PIC S9(7)V9(8)         VALUE ZEROS.
000205     12  IA-N                PIC S9(7)V9(8)         VALUE ZEROS.
000206     12  V                   PIC S9(3)V9(14)        VALUE ZEROS.
000207     12  R                   PIC S9(3)              VALUE ZEROS.
000208     12  M1                  PIC S9(7)V99           VALUE ZEROS.
000209     12  V-EX-N              PIC S9(3)V9(14)        VALUE ZEROS.
000210     12  TERM1               PIC S9(8)V9(9)         VALUE ZEROS.
000211     12  TERM2               PIC S9(8)V9(9)         VALUE ZEROS.
000212     12  TERM3               PIC S9(8)V9(9)         VALUE ZEROS.
000213     12  TERM4               PIC S9(3)V9(14)        VALUE ZEROS.
000214     12  LEFT-TOT-1          PIC S9(9)V9(8)         VALUE ZEROS.
000215     12  RIGHT-TOT-1         PIC S9(9)V9(8)         VALUE ZEROS.
000216     12  RIGHT-TOT-2         PIC S9(9)V9(8)         VALUE ZEROS.
000217
000218 01  TERM-CALC-WORK-AREA.
000219     12  WS-AH-RATE          PIC S999V9(5)          VALUE ZEROS.
000220     12  WS-LF-RATE          PIC S999V9(5)          VALUE ZEROS.
000221     12  WS-TERM             PIC S9(3)              VALUE ZEROS.
000222     12  WS-TERM-REM         PIC S9(3)V99           VALUE ZEROS.
000223     12  WS-REMAIN           PIC S99                VALUE ZEROS.
000224     12  V-EXPONENTS.
000225        14  V-EXPONENT       PIC S9(3)V9(14) COMP-3 OCCURS 250.
000226     12  V-EX-ONETIME        PIC 9                  VALUE 1.
000227
000228 01  MISC-WORK-AREAS.
000229     12  SAVE-BENEFICIARY    PIC X(10)   VALUE LOW-VALUES.
000230     12  FILE-SWITCH         PIC X(4)    VALUE SPACES.
000231     12  CLAIM-SWITCH        PIC X       VALUE 'N'.
000232     12  QID.
000233         16  QID-TERM        PIC X(4).
000234         16  FILLER          PIC X(4)    VALUE '130A'.
000235     12  RETURNED-FROM       PIC X(8)    VALUE SPACES.
000236     12  SAVE-AT-PRIMARY-KEY PIC X(22)   VALUE SPACES.
000237     12  WS-GROUP-NO.
000238         16  WS-GROUP-NO-1   PIC X(01)   VALUE SPACES.
000239         16  WS-GROUP-NO-2-6 PIC X(05)   VALUE SPACES.
000240
000241     12  WS-RESPONSE         PIC S9(8)   COMP.
000242         88  WS-RESP-NORMAL              VALUE +00.
000243         88  WS-RESP-ERROR               VALUE +01.
000244         88  WS-RESP-NOTFND              VALUE +13.
000245         88  WS-RESP-DUPKEY              VALUE +15.
000246         88  WS-RESP-NOTOPEN             VALUE +19.
000247         88  WS-RESP-ENDFILE             VALUE +20.
000248
000249***************************************************************
000250*                                                             *
000251*    QID-MAP-LENGTH MUST BE ADJUSTED EVERY TIME THAT ANY      *
000252*    FIELDS ARE ADDED, DELETED, OR THE SIZE OF ANY FIELD      *
000253*    CHANGES WITH IN MAP EL130A.                              *
000254*                                                             *
000255***************************************************************
000256
000257     12  QID-MAP-LENGTH            PIC S9(4) VALUE +1084 COMP.
000258     12  WS-SAVE-CLAIM-MASTER.
000259         16  FILLER                    PIC XX.
000260         16  WS-SAVE-CLAIM-KEY.
000261             20  WS-SAVE-COMPANY-CD    PIC X.
000262             20  WS-SAVE-CARRIER       PIC X.
000263             20  WS-SAVE-CLAIM-NO      PIC X(7).
000264             20  WS-SAVE-CERT-NO       PIC X(11).
000265         16  FILLER                    PIC X(328).
000266     12  SAVE-CURRENT-DATE         PIC XX.
000267     12  SAVE-CONTROL              PIC X(39).
000268     12  SAVE-ERACCT-KEY           PIC X(26).
000269     12  SAVE-COUNTER              PIC S9(8)   COMP.
000270     12  SAVE-METHOD               PIC X.
000271     12  SAVE-COMPANY-ID           PIC XXX.
000272     12  SAVE-COMPANY-CD           PIC X.
000273     12  SAVE-JOURNAL-FILE-ID      PIC S9(4)   COMP.
000274     12  SAVE-CREDIT-USER          PIC X.
000275     12  SAVE-CLAIM-USER           PIC X.
000276     12  SAVE-LIFE-OVERRIDE-L1     PIC X.
000277     12  SAVE-LIFE-OVERRIDE-L2     PIC XX.
000278     12  SAVE-LIFE-OVERRIDE-L6     PIC X(6).
000279     12  SAVE-LIFE-OVERRIDE-L12    PIC X(12).
000280     12  SAVE-AH-OVERRIDE-L1       PIC X.
000281     12  SAVE-AH-OVERRIDE-L2       PIC XX.
000282     12  SAVE-AH-OVERRIDE-L6       PIC X(6).
000283     12  SAVE-AH-OVERRIDE-L12      PIC X(12).
000284     12  FILLER                    PIC X.
000285*    12  SAVE-CLAIM-ACCESS-CONTROL PIC X.
000286     12  SAVE-CERT-ACCESS-CONTROL  PIC X.
000287     12  SAVE-CARRIER-CONTROL-LEVEL    PIC X.
000288     12  SAVE-EL127-TO-EL130-CNTRL.
000289         16  SAVE-CERT-SELECT-CNT    PIC S9(04) COMP.
000290         16  SAVE-CERT-PROCESSED     PIC S9(04) COMP.
000291         16  SAVE-CERT-CONTROLS-EL127 OCCURS 5 TIMES.
000292             20  SAVE-EL127-CARRIER  PIC X.
000293             20  SAVE-EL127-GROUPING PIC X(6).
000294             20  SAVE-EL127-STATE    PIC X(2).
000295             20  SAVE-EL127-ACCOUNT  PIC X(10).
000296             20  SAVE-EL127-CERT-NO  PIC X(11).
000297             20  SAVE-EL127-EFF-DT   PIC XX.
000298     12  SAVE-EL659-TO-EL130-CNTRL.
000299         16  SAVE-EL659-CARRIER    PIC X.
000300         16  SAVE-EL659-GROUPING   PIC X(6).
000301         16  SAVE-EL659-STATE      PIC XX.
000302         16  SAVE-EL659-ACCOUNT    PIC X(10).
000303     12  DIVIDE-QUOT               PIC 999.
000304     12  DIVIDE-REM                PIC 999.
000305     12  WS-CURRENT-MANUAL-RESERVE PIC S9(5)V99 COMP-3.
000306     12  WS-ITD-ADDITIONAL-RESERVE PIC S9(5)V99 COMP-3.
000307     12  WS-ITD-PAID-EXPENSE       PIC S9(3)V99 COMP-3.
000308     12  WS-ITD-CHARGABLE-EXPENSE  PIC S9(5)V99 COMP-3.
000309     12  WS-INITIAL-MANUAL-RESERVE PIC S9(5)V99 COMP-3.
000310     12  WS-RESERVE-CONTROLS       PIC X(9).
000311     12  WS-EXPENSE-CONTROLS       PIC X(7).
000312     12  WS-REIN-TABLE.
000313         16  WS-REIN-1             PIC X            VALUE SPACE.
000314         16  WS-REIN-2             PIC X            VALUE SPACE.
000315         16  WS-REIN-3             PIC X            VALUE SPACE.
000316     12  WS-BENE-CALC-METHOD       PIC X.
000317     12  ws-lf-joint-indicator     pic x.
000318     12  ws-ah-joint-indicator     pic x.
000319     12  WS-EARNINGS-CALC          PIC X.
000320     12  WS-LF-EARNINGS-CALC       PIC X.
000321         88 TEX-REG                VALUE '4'.
000322         88 NET-PAY                VALUE '5'.
000323     12  WS-AH-EARNINGS-CALC       PIC X.
000324     12  WS-SPECIAL-CALC-CD        PIC X(01).
000325     12  WS-LF-SPECIAL-CALC-CD     PIC X(01).
000326     12  WS-AH-SPECIAL-CALC-CD     PIC X(01).
000327     12  WS-DIAGNOSIS-DESCRIPT     PIC X(60).
000328     12  WS-ICD-CODE-1             PIC X(8).
000329     12  WS-ICD-CODE-2             PIC X(8).
000330     12  WS-CLAIM-NUMBER.
000331         16  WS-CN-PREFIX.
000332             20  WS-CN-PRF-A PIC X.
000333             20  WS-CN-PRF-B PIC X.
000334         16  WS-CN-NUMBER    PIC 9(5).
000335     12  WS-CLAIM-NUMBER-R1 REDEFINES WS-CLAIM-NUMBER PIC 9(7).
000336     12  WS-CLAIM-SEQU.
000337         16  FILLER          PIC X    VALUE '('.
000338         16  WS-CURRENT-SEQU PIC Z9.
000339         16  FILLER          PIC X(4) VALUE ' OF '.
000340         16  WS-OF-SEQU      PIC Z9.
000341         16  FILLER          PIC X    VALUE ')'.
000342     12  DATE-WORK.
000343       14  FILLER                PIC XX.
000344       14  DATE-MM               PIC XX.
000345       14  DATE-DD               PIC XX.
000346       14  DATE-YY               PIC XX.
000347     12  CURR-DATE REDEFINES DATE-WORK.
000348         16  CURR-MM.
000349           18  CURR-M1           PIC 9.
000350           18  CURR-M2           PIC 9.
000351         16  FILLER              PIC X(5).
000352         16  CURR-YEAR           PIC 9.
000353     12  NUM-WORK   REDEFINES DATE-WORK   PIC 9(8).
000354     12  SUB                     PIC 99.
000355     12  MY-DATE.
000356         16  MY-MM               PIC XX.
000357         16  FILLER              PIC X VALUE '/'.
000358         16  MY-YY               PIC XX.
000359
000360     12  WS-EDIT-BEN-CODE        PIC XX.
000361         88  INVALID-BENEFIT-CODE   VALUE '  ' '00'
000362                                          '90' THRU '99'.
000363
000364     12  WS-NUM-HOLD             PIC S9(4)     VALUE +0  COMP-3.
000365     12  WS-BEN-HOLD             PIC XX.
000366     12  WS-BEN-ALPHA-HOLD.
000367         15  WS-BEN-ALPHA-2      PIC XX.
000368         15  WS-BEN-ALPHA-1      PIC X.
000369     12  WS-FORM-HOLD            PIC X(12)  VALUE SPACES.
000370     12  WS-REC-TYPE             PIC X.
000371     12  HOLD-LOAN-BAL           PIC 9(7)V99   VALUE ZEROS.
000372     12  DEEDIT-FIELD            PIC 9(10)V99  VALUE ZEROS.
000373     12  ARCH-NUMBER             PIC S9(8)   COMP.
000374     12  WS-REIN-REC-FOUND-SW    PIC X(01)   VALUE ' '.
000375         88  REIN-REC-NOT-FOUND              VALUE 'N'.
000376         88  REIN-REC-FOUND                  VALUE 'Y'.
000377
000378     12  WS-REC-FOUND-SW         PIC X(01)   VALUE ' '.
000379     12  WS-LETTER-SW            PIC X(01)   VALUE 'N'.
000380
000381
000382 01  CSO-WORK-FIELDS.
000383     12  WS-BLANK            PIC X       VALUE ' '.
000384
000385     12  WS-MA-CONTROL-PRIMARY.
000386         16  WS-MA-COMPANY-CD        PIC  X.
000387         16  WS-MA-CARRIER           PIC  X.
000388         16  WS-MA-GROUPING          PIC  X(6).
000389         16  WS-MA-STATE             PIC  XX.
000390         16  WS-MA-ACCOUNT           PIC  X(10).
000391         16  WS-MA-CERT-EFF-DT       PIC  XX.
000392         16  WS-MA-CERT-NO.
000393             20  WS-MA-CERT-PRIME    PIC  X(10).
000394             20  WS-MA-CERT-SFX      PIC  X.
000395
000396     12  WS-INSURED-ADDR-CNT         PIC  S9(1) VALUE +0.
000397     12  WS-NO-INSURED-ADDRESS       PIC  X(01) VALUE 'N'.
000398     12  WS-SOC-SEC-NUMBER.
000399         16  WS-SOC-SEC-NO       PIC 9(9).
000400         16  WS-SOC-SEC-BLANK    PIC X(2).
000401     12  WS-SOC-SEC-REDEF REDEFINES WS-SOC-SEC-NUMBER.
000402         16  WS-SSN-1-3          PIC 9(3).
000403         16  WS-SSN-DASH1        PIC X(1).
000404         16  WS-SSN-4-5          PIC 9(2).
000405         16  WS-SSN-DASH2        PIC X(1).
000406         16  WS-SSN-6-9          PIC 9(4).
000407
000408     12  WS-DIAG-VIN-MSG.
000409         16  FILLER          PIC X(11)
000410             VALUE 'VIN NUMBER '.
000411         16  WS-DIAG-VIN     PIC X(17).
000412
000413     12  WS-VERIFY-NOTE      PIC X(34)
000414         VALUE '2ND BENE SSN VERIFICATION REQUIRED'.
000415
000416     12  WS-CAUSAL-NOTE      PIC X(40)
000417         VALUE 'VERIFY IF DX RELATED TO PRIOR CONDITION '.
000418
000419     12  WS-VFY-SSN-NOTE     PIC X(25)
000420         VALUE 'SSN VERIFICATION REQUIRED'.
000421
000422     12  WS-FILING-NOTE     PIC X(30)
000423         VALUE 'FILING TIME LIMIT EXCEEDED'.
000424
000425 01  MAP-DATE-AREAS.
000426     12  WS-EFFDT            PIC XX      VALUE LOW-VALUES.
000427     12  WS-EXPIRE           PIC XX      VALUE LOW-VALUES.
000428     12  WS-ADD-ON-DT        PIC XX      VALUE LOW-VALUES.
000429     12  WS-BIRTHDT          PIC XX      VALUE LOW-VALUES.
000430     12  WS-INCUR            PIC XX      VALUE LOW-VALUES.
000431     12  WS-REPORT           PIC XX      VALUE LOW-VALUES.
000432     12  WS-ESTEND           PIC XX      VALUE LOW-VALUES.
000433     12  WS-ACVCNDT          PIC XX      VALUE LOW-VALUES.
000434     12  WS-LCVCNDT          PIC XX      VALUE LOW-VALUES.
000435     12  WS-TODAY-DT         PIC XX      VALUE LOW-VALUES.
000436     12  WS-ENTRYDT          PIC XX      VALUE LOW-VALUES.
000437     12  WS-MANRSV           PIC S9(6)V99  VALUE ZEROS COMP-3.
000438     12  WS-ACVRATE          PIC S9(4)V99  VALUE ZEROS COMP-3.
000439     12  WS-ACVBENE          PIC S9(9)V99  VALUE ZEROS COMP-3.
000440     12  WS-LCVRATE          PIC S9(4)V99  VALUE ZEROS COMP-3.
000441     12  WS-LCVBENE          PIC S9(9)V99  VALUE ZEROS COMP-3.
000442     12  WS-APR              PIC S999V9999 VALUE ZEROS COMP-3.
000443     12  WS-PMTFREQ          PIC 99        VALUE ZEROS COMP-3.
000444     12  WS-ASSOC-CERT-TOTAL PIC S99       VALUE ZEROS.
000445         88  NO-ASSOCIATED-CERTS           VALUE ZEROS.
000446     12  WS-ASSOC-CERT-SEQU  PIC S99       VALUE ZEROS.
000447     12  WS-READNEXT-SWITCH  PIC S99       VALUE +1.
000448     12  WS-ASSOCIATED-CERTS PIC S9        VALUE ZEROS COMP-3.
000449     12  ONE-OR-MIN1         PIC S9        VALUE +1    COMP-3.
000450
000451 01  ACCESS-KEYS.
000452     12  ELACTQ-KEY.
000453         16  ELACTQ-COMPANY-CD       PIC X.
000454         16  ELACTQ-CARRIER          PIC X.
000455         16  ELACTQ-CLAIM-NO         PIC X(7).
000456         16  ELACTQ-CERT-NO          PIC X(11).
000457     12  ELMSTR-KEY.
000458         16  MSTR-COMP-CD            PIC X.
000459         16  MSTR-CARRIER            PIC X.
000460         16  MSTR-CLAIM-NO           PIC X(7).
000461         16  MSTR-CERT-NO.
000462             20  MSTR-CERT-NO-PRIME  PIC X(10).
000463             20  MSTR-CERT-NO-SUFX   PIC X.
000464     12  SAVE-ELMSTR-KEY.
000465         16  SAVE-COMP-CD            PIC X.
000466         16  SAVE-CARRIER            PIC X.
000467         16  SAVE-CLAIM-NO           PIC X(7).
000468         16  SAVE-CERT-NO.
000469             20  SAVE-CERT-NO-PRIME  PIC X(10).
000470             20  SAVE-CERT-NO-SUFX   PIC X.
000471     12  ELMSTR5-KEY.
000472         16  MSTR5-COMP-CD           PIC X.
000473         16  MSTR5-CERT-NO.
000474             20  MSTR5-CERT-NO-PRIME PIC X(10).
000475             20  MSTR5-CERT-NO-SUFX  PIC X.
000476     12  INCUR-DTE-DUPE-SW           PIC X VALUE 'N'.
000477         88  NO-CLAIMS-FOR-CERT            VALUE 'N'.
000478         88  INCURRED-DATE-MATCH           VALUE 'Y'.
000479         88  CLAIM-RECORD-FOUND            VALUE 'H'.
000480     12  ELCNTL-KEY.
000481         16  CNTL-COMP-ID    PIC X(3).
000482         16  CNTL-REC-TYPE   PIC X.
000483         16  CNTL-ACCESS.
000484             20  FILLER      PIC XX.
000485             20  CNTL-BENEFIT.
000486                 24  FILLER  PIC X.
000487                 24  CNTL-CARRIER PIC X.
000488         16  CNTL-SEQ-NO     PIC S9(4)    COMP.
000489     12  ELCERT-KEY.
000490         16  CERT-COMP-CD    PIC X.
000491         16  CERT-CARRIER    PIC X.
000492         16  CERT-GROUPING   PIC X(6).
000493         16  CERT-STATE      PIC X(2).
000494         16  CERT-ACCOUNT.
000495             20  CERT-ACCOUNT-PREFIX PIC X(4).
000496             20  CERT-ACCOUNT-PRIME  PIC X(6).
000497         16  CERT-EFF-DT     PIC XX.
000498         16  CERT-CERT-NO.
000499             20  CERT-CERT-NO-PRIME  PIC X(10).
000500             20  CERT-CERT-NO-SUFX   PIC X.
000501     12  ELCERT-KEY-5.
000502         16  CERT-COMP-CD-5      PIC X.
000503         16  CERT-CERT-5.
000504             20  CERT-CERT-5-PRIME   PIC X(10).
000505             20  CERT-CERT-5-SUFX    PIC X.
000506     12  ERACCT-KEY.
000507         16  ACCT-COMP-CD    PIC X.
000508         16  ACCT-CARRIER    PIC X.
000509         16  ACCT-GROUPING   PIC X(6).
000510         16  ACCT-STATE      PIC X(2).
000511         16  ACCT-ACCOUNT    PIC X(10).
000512         16  ACCT-EXP-DT     PIC XX.
000513     12  ELTRLR-KEY.
000514         16  TRLR-COMP-CD    PIC X.
000515         16  TRLR-CARRIER    PIC X.
000516         16  TRLR-CLAIM-NO   PIC X(7).
000517         16  TRLR-CERT-NO.
000518             20  TRLR-CERT-NO-PRIME  PIC X(10).
000519             20  TRLR-CERT-NO-SUFX   PIC X.
000520         16  TRLR-SEQ-NO     PIC S9(4)   COMP.
000521         16  TRLR-TYPE       PIC X.
000522     12  ELBENE-KEY.
000523         16  BENE-COMP-CD    PIC X.
000524         16  BENE-RCD-TYPE   PIC X.
000525         16  BENE-CODE       PIC X(10).
000526     12  ERREIN-KEY.
000527         16  REIN-COMP-CD        PIC X(01).
000528         16  REIN-TYPE           PIC X(01).
000529         16  REIN-TABLE-CO.
000530             20  REIN-CODE-1     PIC X(01).
000531             20  REIN-CODE-2     PIC X(01).
000532             20  REIN-CODE-3     PIC X(01).
000533         16  FILLER              PIC X(03).
000534     12  ELCRTT-KEY.
000535         16  CTRLR-COMP-CD       PIC X.
000536         16  CTRLR-CARRIER       PIC X.
000537         16  CTRLR-GROUPING      PIC X(6).
000538         16  CTRLR-STATE         PIC X(2).
000539         16  CTRLR-ACCOUNT       PIC X(10).
000540         16  CTRLR-EFF-DT        PIC XX.
000541         16  CTRLR-CERT-NO       PIC X(11).
000542         16  CTRLR-REC-TYPE      PIC X.
000543
000544 01  ws-acct-found-sw            pic x value ' '.
000545     88  acct-found                value 'Y'.
000546 01  ERPDEF-KEY-SAVE             PIC X(18).
000547 01  ERPDEF-KEY.
000548     12  ERPDEF-COMPANY-CD       PIC X.
000549     12  ERPDEF-STATE            PIC XX.
000550     12  ERPDEF-PROD-CD          PIC XXX.
000551     12  F                       PIC X(7).
000552     12  ERPDEF-BEN-TYPE         PIC X.
000553     12  ERPDEF-BEN-CODE         PIC XX.
000554     12  ERPDEF-EXP-DT           PIC XX.
000555
000556 01  ERROR-MESSAGES.
000557     12  ER-0000                 PIC X(4)  VALUE '0000'.
000558     12  ER-0004                 PIC X(4)  VALUE '0004'.
000559     12  ER-0019                 PIC X(4)  VALUE '0019'.
000560     12  ER-0023                 PIC X(4)  VALUE '0023'.
000561     12  ER-0029                 PIC X(4)  VALUE '0029'.
000562     12  ER-0033                 PIC X(4)  VALUE '0033'.
000563     12  ER-0068                 PIC X(4)  VALUE '0068'.
000564     12  ER-0070                 PIC X(4)  VALUE '0070'.
000565     12  ER-0194                 PIC X(4)  VALUE '0194'.
000566     12  ER-0202                 PIC X(4)  VALUE '0202'.
000567     12  ER-0203                 PIC X(4)  VALUE '0203'.
000568     12  ER-0204                 PIC X(4)  VALUE '0204'.
000569     12  ER-0205                 PIC X(4)  VALUE '0205'.
000570     12  ER-0206                 PIC X(4)  VALUE '0206'.
000571     12  ER-0207                 PIC X(4)  VALUE '0207'.
000572     12  ER-0213                 PIC X(4)  VALUE '0213'.
000573     12  ER-0214                 PIC X(4)  VALUE '0214'.
000574     12  ER-0217                 PIC X(4)  VALUE '0217'.
000575     12  ER-0219                 PIC X(4)  VALUE '0219'.
000576     12  ER-0220                 PIC X(4)  VALUE '0220'.
000577     12  ER-0221                 PIC X(4)  VALUE '0221'.
000578     12  ER-0222                 PIC X(4)  VALUE '0222'.
000579     12  ER-0223                 PIC X(4)  VALUE '0223'.
000580     12  ER-0224                 PIC X(4)  VALUE '0224'.
000581     12  ER-0225                 PIC X(4)  VALUE '0225'.
000582     12  ER-0226                 PIC X(4)  VALUE '0226'.
000583     12  ER-0227                 PIC X(4)  VALUE '0227'.
000584     12  ER-0229                 PIC X(4)  VALUE '0229'.
000585     12  ER-0230                 PIC X(4)  VALUE '0230'.
000586     12  ER-0231                 PIC X(4)  VALUE '0231'.
000587     12  ER-0232                 PIC X(4)  VALUE '0232'.
000588     12  ER-0233                 PIC X(4)  VALUE '0233'.
000589     12  ER-0234                 PIC X(4)  VALUE '0234'.
000590     12  ER-0235                 PIC X(4)  VALUE '0235'.
000591     12  ER-0236                 PIC X(4)  VALUE '0236'.
000592     12  ER-0237                 PIC X(4)  VALUE '0237'.
000593     12  ER-0238                 PIC X(4)  VALUE '0238'.
000594     12  ER-0239                 PIC X(4)  VALUE '0239'.
000595     12  ER-0240                 PIC X(4)  VALUE '0240'.
000596     12  ER-0241                 PIC X(4)  VALUE '0241'.
000597     12  ER-0243                 PIC X(4)  VALUE '0243'.
000598     12  ER-0244                 PIC X(4)  VALUE '0244'.
000599     12  ER-0245                 PIC X(4)  VALUE '0245'.
000600     12  ER-0246                 PIC X(4)  VALUE '0246'.
000601     12  ER-0247                 PIC X(4)  VALUE '0247'.
000602     12  ER-0248                 PIC X(4)  VALUE '0248'.
000603     12  ER-0250                 PIC X(4)  VALUE '0250'.
000604     12  ER-0251                 PIC X(4)  VALUE '0251'.
000605     12  ER-0252                 PIC X(4)  VALUE '0252'.
000606     12  ER-0253                 PIC X(4)  VALUE '0253'.
000607     12  ER-0254                 PIC X(4)  VALUE '0254'.
000608     12  ER-0257                 PIC X(4)  VALUE '0257'.
000609     12  ER-0258                 PIC X(4)  VALUE '0258'.
000610     12  ER-0260                 PIC X(4)  VALUE '0260'.
000611     12  ER-0261                 PIC X(4)  VALUE '0261'.
000612     12  ER-0263                 PIC X(4)  VALUE '0263'.
000613     12  ER-0264                 PIC X(4)  VALUE '0264'.
000614     12  ER-0265                 PIC X(4)  VALUE '0265'.
000615     12  ER-0266                 PIC X(4)  VALUE '0266'.
000616     12  ER-0267                 PIC X(4)  VALUE '0267'.
000617     12  ER-0334                 PIC X(4)  VALUE '0334'.
000618     12  ER-0337                 PIC X(4)  VALUE '0337'.
000619     12  ER-0412                 PIC X(4)  VALUE '0412'.
000620     12  ER-0413                 PIC X(4)  VALUE '0413'.
000621     12  ER-0428                 PIC X(4)  VALUE '0428'.
000622     12  ER-0429                 PIC X(4)  VALUE '0429'.
000623     12  ER-0430                 PIC X(4)  VALUE '0430'.
000624     12  ER-0433                 PIC X(4)  VALUE '0433'.
000625     12  ER-0434                 PIC X(4)  VALUE '0434'.
000626     12  ER-0458                 PIC X(4)  VALUE '0458'.
000627     12  ER-0489                 PIC X(4)  VALUE '0489'.
000628     12  ER-0509                 PIC X(4)  VALUE '0509'.
000629     12  ER-0510                 PIC X(4)  VALUE '0510'.
000630     12  ER-0511                 PIC X(4)  VALUE '0511'.
000631     12  ER-0512                 PIC X(4)  VALUE '0512'.
000632     12  ER-0516                 PIC X(4)  VALUE '0516'.
000633     12  ER-0517                 PIC X(4)  VALUE '0517'.
000634     12  ER-0518                 PIC X(4)  VALUE '0518'.
000635     12  ER-0519                 PIC X(4)  VALUE '0519'.
000636     12  ER-0520                 PIC X(4)  VALUE '0520'.
000637     12  ER-0521                 PIC X(4)  VALUE '0521'.
000638     12  ER-0522                 PIC X(4)  VALUE '0522'.
000639     12  ER-0523                 PIC X(4)  VALUE '0523'.
000640     12  ER-0544                 PIC X(4)  VALUE '0544'.
000641     12  ER-0546                 PIC X(4)  VALUE '0546'.
000642     12  ER-0558                 PIC X(4)  VALUE '0558'.
000643     12  ER-0560                 PIC X(4)  VALUE '0560'.
000644     12  ER-0562                 PIC X(4)  VALUE '0562'.
000645     12  ER-0565                 PIC X(4)  VALUE '0565'.
000646     12  ER-0569                 PIC X(4)  VALUE '0569'.
000647     12  ER-0693                 PIC X(4)  VALUE '0693'.
000648     12  ER-0715                 PIC X(4)  VALUE '0715'.
000649     12  ER-0840                 PIC X(4)  VALUE '0840'.
000650     12  ER-0841                 PIC X(4)  VALUE '0841'.
000651     12  ER-0842                 PIC X(4)  VALUE '0842'.
000652     12  ER-0843                 PIC X(4)  VALUE '0843'.
000653     12  ER-0878                 PIC X(4)  VALUE '0878'.
000654     12  ER-0887                 PIC X(4)  VALUE '0887'.
000655     12  ER-0992                 PIC X(4)  VALUE '0992'.
000656     12  ER-1651                 PIC X(4)  VALUE '1651'.
000657     12  ER-1652                 PIC X(4)  VALUE '1652'.
000658     12  ER-1653                 PIC X(4)  VALUE '1653'.
000659     12  ER-1654                 PIC X(4)  VALUE '1654'.
000660     12  ER-1655                 PIC X(4)  VALUE '1655'.
000661     12  er-1659                 pic x(4)  value '1659'.
000662     12  er-1660                 pic x(4)  value '1660'.
000663     12  er-1661                 pic x(4)  value '1661'.
000664     12  er-1671                 pic x(4)  value '1671'.
000665     12  er-1672                 pic x(4)  value '1672'.
000666     12  er-1673                 pic x(4)  value '1673'.
000667     12  er-1675                 pic x(4)  value '1675'.
000668     12  er-1676                 pic x(4)  value '1676'.
000669     12  ER-1677                 PIC X(4)  VALUE '1677'.
000670     12  ER-1679                 PIC X(4)  VALUE '1679'.
000671     12  er-1682                 pic x(4)  value '1682'.
000672     12  er-1683                 pic x(4)  value '1683'.
000673     12  ER-2280                 PIC X(4)  VALUE '2280'.
000674     12  ER-2370                 PIC X(4)  VALUE '2370'.
000675     12  ER-2371                 PIC X(4)  VALUE '2371'.
000676     12  ER-2378                 PIC X(4)  VALUE '2378'.
000677     12  ER-2380                 PIC X(4)  VALUE '2380'.
000678     12  ER-2566                 PIC X(4)  VALUE '2566'.
000679     12  ER-2848                 PIC X(4)  VALUE '2848'.
000680     12  ER-3548                 PIC X(4)  VALUE '3548'.
000681     12  ER-7008                 PIC X(4)  VALUE '7008'.
000682     12  ER-7319                 PIC X(4)  VALUE '7319'.
000683     12  ER-7321                 PIC X(4)  VALUE '7321'.
000684     12  ER-7352                 PIC X(4)  VALUE '7352'.
000685     12  ER-7353                 PIC X(4)  VALUE '7353'.
000686     12  ER-7572                 PIC X(4)  VALUE '7572'.
000687     12  ER-7575                 PIC X(4)  VALUE '7575'.
000688     12  ER-7577                 PIC X(4)  VALUE '7577'.
000689     12  ER-7582                 PIC X(4)  VALUE '7582'.
000690     12  ER-7634                 PIC X(4)  VALUE '7634'.
000691     12  ER-7635                 PIC X(4)  VALUE '7635'.
000692     12  ER-7636                 PIC X(4)  VALUE '7636'.
000693     12  ER-7637                 PIC X(4)  VALUE '7637'.
000694     12  ER-7638                 PIC X(4)  VALUE '7638'.
000695     12  ER-7639                 PIC X(4)  VALUE '7639'.
000696     12  ER-7640                 PIC X(4)  VALUE '7640'.
000697     12  ER-7641                 PIC X(4)  VALUE '7641'.
000698     12  ER-7642                 PIC X(4)  VALUE '7642'.
000699     12  ER-7643                 PIC X(4)  VALUE '7643'.
000700     12  ER-7646                 PIC X(4)  VALUE '7646'.
000701     12  ER-7647                 PIC X(4)  VALUE '7647'.
000702     12  ER-7648                 PIC X(4)  VALUE '7648'.
000703     12  ER-7649                 PIC X(4)  VALUE '7649'.
000704     12  ER-7651                 PIC X(4)  VALUE '7651'.
000705     12  ER-7670                 PIC X(4)  VALUE '7670'.
000706     12  ER-7671                 PIC X(4)  VALUE '7671'.
000707     12  ER-7686                 PIC X(4)  VALUE '7686'.
000708     12  ER-7688                 PIC X(4)  VALUE '7688'.
000709     12  ER-7689                 PIC X(4)  VALUE '7689'.
000710     12  ER-7691                 PIC X(4)  VALUE '7691'.
000711
000712 01  work-flow-pass-area.
000713     05  pa-rec-type             pic x(4).
000714     05  pa-company-id           pic xxx.
000715     05  pa-rest-of-record       pic x(600).
000716
000717*    COPY ERCPDEF.
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
000718
000719*    COPY ELCLNKLT.
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
000720     EJECT
000721*    COPY ELCINTF.
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
000722     12  PI-REDEF     REDEFINES PI-PROGRAM-WORK-AREA.
000723         16  PI-MSTR-DT               PIC XX.
000724         16  PI-MSTR-WHEN             PIC S9(6)   COMP-3.
000725         16  PI-TRLR-DT               PIC XX.
000726         16  PI-TRLR-WHEN             PIC S9(6)   COMP-3.
000727         16  PI-LAST-CLAIM            PIC X(7).
000728         16  PI-LAST-CARR             PIC X.
000729         16  PI-LAST-CERT.
000730             20  PI-LAST-CERT-PRIME   PIC X(10).
000731             20  PI-LAST-CERT-SUFX    PIC X.
000732         16  PI-LAST-CLAIM-TYPE       PIC X.
000733         16  PI-CURSOR                PIC S9(4)   COMP.
000734         16  PI-SAVE-CERT.
000735             20  PI-SAVE-CERT-PRIME   PIC X(10).
000736             20  PI-SAVE-CERT-SUFX    PIC X.
000737         16  PI-SAVE-EFFDT            PIC X(2).
000738         16  PI-SAVE-ACCOUNT          PIC X(10).
000739         16  PI-INCURR-SW             PIC X.
000740         16  PI-PRT-OPT               PIC X.
000741         16  PI-ALT-PRT               PIC X(4).
000742         16  PI-CLAIM-DELETED-SWITCH  PIC X.
000743             88  CLAIM-DELETED-BY-EL131   VALUE 'D'.
000744         16  FILLER                   PIC X(272).
000745         16  PI-EL127-TO-EL130-CNTRL.
000746             20  PI-CERT-SELECT-CNT          PIC S9(04) COMP.
000747             20  PI-CERT-PROCESSED           PIC S9(04) COMP.
000748             20  PI-CERT-CONTROLS-EL127  OCCURS 5 TIMES.
000749                 24  PI-EL127-CARRIER        PIC X.
000750                 24  PI-EL127-GROUPING       PIC X(6).
000751                 24  PI-EL127-STATE          PIC X(2).
000752                 24  PI-EL127-ACCOUNT        PIC X(10).
000753                 24  PI-EL127-CERT-NO.
000754                     28  PI-EL127-CERT-PRIME PIC X(10).
000755                     28  PI-EL127-CERT-SUFX  PIC X.
000756                 24  PI-EL127-EFF-DT         PIC XX.
000757*        16  FILLER                          PIC X(50).
000758         16  FILLER                          PIC X(31).
000759         16  PI-EL659-TO-EL130-CNTRL.
000760             20  PI-EL659-CARRIER            PIC X.
000761             20  PI-EL659-GROUPING           PIC X(6).
000762             20  PI-EL659-STATE              PIC XX.
000763             20  PI-EL659-ACCOUNT            PIC X(10).
000764
000765         16  FILLER                          PIC X(19).
000766
000767         16  PI-SAVE-INCUR-DT                PIC X(02).
000768         16  PI-LETTER-ERROR-CODE            PIC 9(04).
000769*         16  FILLER                          PIC X(65).
000770         16  PI-ST-VFY-2ND-BENE              PIC X.
000771         16  PI-ST-CAUSAL-STATE              PIC X.
000772         16  pi-dcc-max-benefits             pic s999 comp-3.
000773         16  pi-dcc-max-amt                  pic s9(9)v99 comp-3.
000774         16  pi-emi-sub                      pic 99.
000775         16  FILLER                          PIC X(53).
000776
000777     EJECT
000778*    COPY EL130S.
      *>>((file: EL130S))
000001 01  EL130AI.
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
000016     05  COMPL PIC S9(0004) COMP.
000017     05  COMPF PIC  X(0001).
000018     05  FILLER REDEFINES COMPF.
000019         10  COMPA PIC  X(0001).
000020     05  COMPI PIC  X(0003).
000021*    -------------------------------
000022     05  SEQUL PIC S9(0004) COMP.
000023     05  SEQUF PIC  X(0001).
000024     05  FILLER REDEFINES SEQUF.
000025         10  SEQUA PIC  X(0001).
000026     05  SEQUI PIC  X(0010).
000027*    -------------------------------
000028     05  MAINTL PIC S9(0004) COMP.
000029     05  MAINTF PIC  X(0001).
000030     05  FILLER REDEFINES MAINTF.
000031         10  MAINTA PIC  X(0001).
000032     05  MAINTI PIC  X(0001).
000033*    -------------------------------
000034     05  CLMNOL PIC S9(0004) COMP.
000035     05  CLMNOF PIC  X(0001).
000036     05  FILLER REDEFINES CLMNOF.
000037         10  CLMNOA PIC  X(0001).
000038     05  CLMNOI PIC  X(0007).
000039*    -------------------------------
000040     05  CLMCARRL PIC S9(0004) COMP.
000041     05  CLMCARRF PIC  X(0001).
000042     05  FILLER REDEFINES CLMCARRF.
000043         10  CLMCARRA PIC  X(0001).
000044     05  CLMCARRI PIC  X(0001).
000045*    -------------------------------
000046     05  CLMTYPEL PIC S9(0004) COMP.
000047     05  CLMTYPEF PIC  X(0001).
000048     05  FILLER REDEFINES CLMTYPEF.
000049         10  CLMTYPEA PIC  X(0001).
000050     05  CLMTYPEI PIC  X(0001).
000051*    -------------------------------
000052     05  CERTNOL PIC S9(0004) COMP.
000053     05  CERTNOF PIC  X(0001).
000054     05  FILLER REDEFINES CERTNOF.
000055         10  CERTNOA PIC  X(0001).
000056     05  CERTNOI PIC  X(0010).
000057*    -------------------------------
000058     05  SUFXL PIC S9(0004) COMP.
000059     05  SUFXF PIC  X(0001).
000060     05  FILLER REDEFINES SUFXF.
000061         10  SUFXA PIC  X(0001).
000062     05  SUFXI PIC  X(0001).
000063*    -------------------------------
000064     05  PCERTNOL PIC S9(0004) COMP.
000065     05  PCERTNOF PIC  X(0001).
000066     05  FILLER REDEFINES PCERTNOF.
000067         10  PCERTNOA PIC  X(0001).
000068     05  PCERTNOI PIC  X(0010).
000069*    -------------------------------
000070     05  PSUFXL PIC S9(0004) COMP.
000071     05  PSUFXF PIC  X(0001).
000072     05  FILLER REDEFINES PSUFXF.
000073         10  PSUFXA PIC  X(0001).
000074     05  PSUFXI PIC  X(0001).
000075*    -------------------------------
000076     05  INCURL PIC S9(0004) COMP.
000077     05  INCURF PIC  X(0001).
000078     05  FILLER REDEFINES INCURF.
000079         10  INCURA PIC  X(0001).
000080     05  INCURI PIC  X(0008).
000081*    -------------------------------
000082     05  REPORTL PIC S9(0004) COMP.
000083     05  REPORTF PIC  X(0001).
000084     05  FILLER REDEFINES REPORTF.
000085         10  REPORTA PIC  X(0001).
000086     05  REPORTI PIC  X(0008).
000087*    -------------------------------
000088     05  ICD1L PIC S9(0004) COMP.
000089     05  ICD1F PIC  X(0001).
000090     05  FILLER REDEFINES ICD1F.
000091         10  ICD1A PIC  X(0001).
000092     05  ICD1I PIC  X(0008).
000093*    -------------------------------
000094     05  ICD2L PIC S9(0004) COMP.
000095     05  ICD2F PIC  X(0001).
000096     05  FILLER REDEFINES ICD2F.
000097         10  ICD2A PIC  X(0001).
000098     05  ICD2I PIC  X(0008).
000099*    -------------------------------
000100     05  DIAGL PIC S9(0004) COMP.
000101     05  DIAGF PIC  X(0001).
000102     05  FILLER REDEFINES DIAGF.
000103         10  DIAGA PIC  X(0001).
000104     05  DIAGI PIC  X(0060).
000105*    -------------------------------
000106     05  BENECDL PIC S9(0004) COMP.
000107     05  BENECDF PIC  X(0001).
000108     05  FILLER REDEFINES BENECDF.
000109         10  BENECDA PIC  X(0001).
000110     05  BENECDI PIC  X(0010).
000111*    -------------------------------
000112     05  BIRTHDTL PIC S9(0004) COMP.
000113     05  BIRTHDTF PIC  X(0001).
000114     05  FILLER REDEFINES BIRTHDTF.
000115         10  BIRTHDTA PIC  X(0001).
000116     05  BIRTHDTI PIC  X(0008).
000117*    -------------------------------
000118     05  SSNL PIC S9(0004) COMP.
000119     05  SSNF PIC  X(0001).
000120     05  FILLER REDEFINES SSNF.
000121         10  SSNA PIC  X(0001).
000122     05  SSNI PIC  X(0011).
000123*    -------------------------------
000124     05  SEXL PIC S9(0004) COMP.
000125     05  SEXF PIC  X(0001).
000126     05  FILLER REDEFINES SEXF.
000127         10  SEXA PIC  X(0001).
000128     05  SEXI PIC  X(0001).
000129*    -------------------------------
000130     05  INSTYPEL PIC S9(0004) COMP.
000131     05  INSTYPEF PIC  X(0001).
000132     05  FILLER REDEFINES INSTYPEF.
000133         10  INSTYPEA PIC  X(0001).
000134     05  INSTYPEI PIC  X(0001).
000135*    -------------------------------
000136     05  LSTNMEL PIC S9(0004) COMP.
000137     05  LSTNMEF PIC  X(0001).
000138     05  FILLER REDEFINES LSTNMEF.
000139         10  LSTNMEA PIC  X(0001).
000140     05  LSTNMEI PIC  X(0015).
000141*    -------------------------------
000142     05  FSTNMEL PIC S9(0004) COMP.
000143     05  FSTNMEF PIC  X(0001).
000144     05  FILLER REDEFINES FSTNMEF.
000145         10  FSTNMEA PIC  X(0001).
000146     05  FSTNMEI PIC  X(0012).
000147*    -------------------------------
000148     05  INITL PIC S9(0004) COMP.
000149     05  INITF PIC  X(0001).
000150     05  FILLER REDEFINES INITF.
000151         10  INITA PIC  X(0001).
000152     05  INITI PIC  X(0001).
000153*    -------------------------------
000154     05  LOANNOL PIC S9(0004) COMP.
000155     05  LOANNOF PIC  X(0001).
000156     05  FILLER REDEFINES LOANNOF.
000157         10  LOANNOA PIC  X(0001).
000158     05  LOANNOI PIC  X(0008).
000159*    -------------------------------
000160     05  CRTLNMEL PIC S9(0004) COMP.
000161     05  CRTLNMEF PIC  X(0001).
000162     05  FILLER REDEFINES CRTLNMEF.
000163         10  CRTLNMEA PIC  X(0001).
000164     05  CRTLNMEI PIC  X(0015).
000165*    -------------------------------
000166     05  CRTFNMEL PIC S9(0004) COMP.
000167     05  CRTFNMEF PIC  X(0001).
000168     05  FILLER REDEFINES CRTFNMEF.
000169         10  CRTFNMEA PIC  X(0001).
000170     05  CRTFNMEI PIC  X(0012).
000171*    -------------------------------
000172     05  CRTINITL PIC S9(0004) COMP.
000173     05  CRTINITF PIC  X(0001).
000174     05  FILLER REDEFINES CRTINITF.
000175         10  CRTINITA PIC  X(0001).
000176     05  CRTINITI PIC  X(0001).
000177*    -------------------------------
000178     05  LOANBALL PIC S9(0004) COMP.
000179     05  LOANBALF PIC  X(0001).
000180     05  FILLER REDEFINES LOANBALF.
000181         10  LOANBALA PIC  X(0001).
000182     05  LOANBALI PIC  9(7)V99.
000183*    -------------------------------
000184     05  MANRSVL PIC S9(0004) COMP.
000185     05  MANRSVF PIC  X(0001).
000186     05  FILLER REDEFINES MANRSVF.
000187         10  MANRSVA PIC  X(0001).
000188     05  MANRSVI PIC  9(7)V99.
000189*    -------------------------------
000190     05  SUPVL PIC S9(0004) COMP.
000191     05  SUPVF PIC  X(0001).
000192     05  FILLER REDEFINES SUPVF.
000193         10  SUPVA PIC  X(0001).
000194     05  SUPVI PIC  X(0001).
000195*    -------------------------------
000196     05  PRICDL PIC S9(0004) COMP.
000197     05  PRICDF PIC  X(0001).
000198     05  FILLER REDEFINES PRICDF.
000199         10  PRICDA PIC  X(0001).
000200     05  PRICDI PIC  X(0001).
000201*    -------------------------------
000202     05  PRODL PIC S9(0004) COMP.
000203     05  PRODF PIC  X(0001).
000204     05  FILLER REDEFINES PRODF.
000205         10  PRODA PIC  X(0001).
000206     05  PRODI PIC  X(0008).
000207*    -------------------------------
000208     05  PRODCDL PIC S9(0004) COMP.
000209     05  PRODCDF PIC  X(0001).
000210     05  FILLER REDEFINES PRODCDF.
000211         10  PRODCDA PIC  X(0001).
000212     05  PRODCDI PIC  X(0001).
000213*    -------------------------------
000214     05  FILETOL PIC S9(0004) COMP.
000215     05  FILETOF PIC  X(0001).
000216     05  FILLER REDEFINES FILETOF.
000217         10  FILETOA PIC  X(0001).
000218     05  FILETOI PIC  X(0004).
000219*    -------------------------------
000220     05  PROCCDL PIC S9(0004) COMP.
000221     05  PROCCDF PIC  X(0001).
000222     05  FILLER REDEFINES PROCCDF.
000223         10  PROCCDA PIC  X(0001).
000224     05  PROCCDI PIC  X(0004).
000225*    -------------------------------
000226     05  RELCLML PIC S9(0004) COMP.
000227     05  RELCLMF PIC  X(0001).
000228     05  FILLER REDEFINES RELCLMF.
000229         10  RELCLMA PIC  X(0001).
000230     05  RELCLMI PIC  X(0007).
000231*    -------------------------------
000232     05  PRTOPTL PIC S9(0004) COMP.
000233     05  PRTOPTF PIC  X(0001).
000234     05  FILLER REDEFINES PRTOPTF.
000235         10  PRTOPTA PIC  X(0001).
000236     05  PRTOPTI PIC  X(0001).
000237*    -------------------------------
000238     05  ALTPRTL PIC S9(0004) COMP.
000239     05  ALTPRTF PIC  X(0001).
000240     05  FILLER REDEFINES ALTPRTF.
000241         10  ALTPRTA PIC  X(0001).
000242     05  ALTPRTI PIC  X(0004).
000243*    -------------------------------
000244     05  CERTMTL PIC S9(0004) COMP.
000245     05  CERTMTF PIC  X(0001).
000246     05  FILLER REDEFINES CERTMTF.
000247         10  CERTMTA PIC  X(0001).
000248     05  CERTMTI PIC  X(0001).
000249*    -------------------------------
000250     05  CRTCARRL PIC S9(0004) COMP.
000251     05  CRTCARRF PIC  X(0001).
000252     05  FILLER REDEFINES CRTCARRF.
000253         10  CRTCARRA PIC  X(0001).
000254     05  CRTCARRI PIC  X(0001).
000255*    -------------------------------
000256     05  GROUPL PIC S9(0004) COMP.
000257     05  GROUPF PIC  X(0001).
000258     05  FILLER REDEFINES GROUPF.
000259         10  GROUPA PIC  X(0001).
000260     05  GROUPI PIC  X(0006).
000261*    -------------------------------
000262     05  STATEL PIC S9(0004) COMP.
000263     05  STATEF PIC  X(0001).
000264     05  FILLER REDEFINES STATEF.
000265         10  STATEA PIC  X(0001).
000266     05  STATEI PIC  X(0002).
000267*    -------------------------------
000268     05  ACCOUNTL PIC S9(0004) COMP.
000269     05  ACCOUNTF PIC  X(0001).
000270     05  FILLER REDEFINES ACCOUNTF.
000271         10  ACCOUNTA PIC  X(0001).
000272     05  ACCOUNTI PIC  X(0010).
000273*    -------------------------------
000274     05  EFFDTL PIC S9(0004) COMP.
000275     05  EFFDTF PIC  X(0001).
000276     05  FILLER REDEFINES EFFDTF.
000277         10  EFFDTA PIC  X(0001).
000278     05  EFFDTI PIC  X(0008).
000279*    -------------------------------
000280     05  CRTSSNL PIC S9(0004) COMP.
000281     05  CRTSSNF PIC  X(0001).
000282     05  FILLER REDEFINES CRTSSNF.
000283         10  CRTSSNA PIC  X(0001).
000284     05  CRTSSNI PIC  X(0011).
000285*    -------------------------------
000286     05  MEMCAPL PIC S9(0004) COMP.
000287     05  MEMCAPF PIC  X(0001).
000288     05  FILLER REDEFINES MEMCAPF.
000289         10  MEMCAPA PIC  X(0001).
000290     05  MEMCAPI PIC  X(0010).
000291*    -------------------------------
000292     05  MEMBERL PIC S9(0004) COMP.
000293     05  MEMBERF PIC  X(0001).
000294     05  FILLER REDEFINES MEMBERF.
000295         10  MEMBERA PIC  X(0001).
000296     05  MEMBERI PIC  X(0012).
000297*    -------------------------------
000298     05  REINCDL PIC S9(0004) COMP.
000299     05  REINCDF PIC  X(0001).
000300     05  FILLER REDEFINES REINCDF.
000301         10  REINCDA PIC  X(0001).
000302     05  REINCDI PIC  X(0001).
000303*    -------------------------------
000304     05  ISSAGEL PIC S9(0004) COMP.
000305     05  ISSAGEF PIC  X(0001).
000306     05  FILLER REDEFINES ISSAGEF.
000307         10  ISSAGEA PIC  X(0001).
000308     05  ISSAGEI PIC  X(0002).
000309*    -------------------------------
000310     05  APRL PIC S9(0004) COMP.
000311     05  APRF PIC  X(0001).
000312     05  FILLER REDEFINES APRF.
000313         10  APRA PIC  X(0001).
000314     05  APRI PIC  9(4)V9(4).
000315*    -------------------------------
000316     05  PMTFREQL PIC S9(0004) COMP.
000317     05  PMTFREQF PIC  X(0001).
000318     05  FILLER REDEFINES PMTFREQF.
000319         10  PMTFREQA PIC  X(0001).
000320     05  PMTFREQI PIC  99.
000321*    -------------------------------
000322     05  INDGRPL PIC S9(0004) COMP.
000323     05  INDGRPF PIC  X(0001).
000324     05  FILLER REDEFINES INDGRPF.
000325         10  INDGRPA PIC  X(0001).
000326     05  INDGRPI PIC  X(0001).
000327*    -------------------------------
000328     05  PREMTYPL PIC S9(0004) COMP.
000329     05  PREMTYPF PIC  X(0001).
000330     05  FILLER REDEFINES PREMTYPF.
000331         10  PREMTYPA PIC  X(0001).
000332     05  PREMTYPI PIC  X(0001).
000333*    -------------------------------
000334     05  ADDONDTL PIC S9(0004) COMP.
000335     05  ADDONDTF PIC  X(0001).
000336     05  FILLER REDEFINES ADDONDTF.
000337         10  ADDONDTA PIC  X(0001).
000338     05  ADDONDTI PIC  X(0008).
000339*    -------------------------------
000340     05  JNTLNMEL PIC S9(0004) COMP.
000341     05  JNTLNMEF PIC  X(0001).
000342     05  FILLER REDEFINES JNTLNMEF.
000343         10  JNTLNMEA PIC  X(0001).
000344     05  JNTLNMEI PIC  X(0015).
000345*    -------------------------------
000346     05  JNTFNMEL PIC S9(0004) COMP.
000347     05  JNTFNMEF PIC  X(0001).
000348     05  FILLER REDEFINES JNTFNMEF.
000349         10  JNTFNMEA PIC  X(0001).
000350     05  JNTFNMEI PIC  X(0002).
000351*    -------------------------------
000352     05  JNTINITL PIC S9(0004) COMP.
000353     05  JNTINITF PIC  X(0001).
000354     05  FILLER REDEFINES JNTINITF.
000355         10  JNTINITA PIC  X(0001).
000356     05  JNTINITI PIC  X(0001).
000357*    -------------------------------
000358     05  JNTAGEL PIC S9(0004) COMP.
000359     05  JNTAGEF PIC  X(0001).
000360     05  FILLER REDEFINES JNTAGEF.
000361         10  JNTAGEA PIC  X(0001).
000362     05  JNTAGEI PIC  X(0002).
000363*    -------------------------------
000364     05  LCVDSCRL PIC S9(0004) COMP.
000365     05  LCVDSCRF PIC  X(0001).
000366     05  FILLER REDEFINES LCVDSCRF.
000367         10  LCVDSCRA PIC  X(0001).
000368     05  LCVDSCRI PIC  X(0006).
000369*    -------------------------------
000370     05  LCVKINDL PIC S9(0004) COMP.
000371     05  LCVKINDF PIC  X(0001).
000372     05  FILLER REDEFINES LCVKINDF.
000373         10  LCVKINDA PIC  X(0001).
000374     05  LCVKINDI PIC  X(0003).
000375*    -------------------------------
000376     05  LCVCDL PIC S9(0004) COMP.
000377     05  LCVCDF PIC  X(0001).
000378     05  FILLER REDEFINES LCVCDF.
000379         10  LCVCDA PIC  X(0001).
000380     05  LCVCDI PIC  X(0002).
000381*    -------------------------------
000382     05  LCVOTRML PIC S9(0004) COMP.
000383     05  LCVOTRMF PIC  X(0001).
000384     05  FILLER REDEFINES LCVOTRMF.
000385         10  LCVOTRMA PIC  X(0001).
000386     05  LCVOTRMI PIC  999.
000387*    -------------------------------
000388     05  LCVRTRML PIC S9(0004) COMP.
000389     05  LCVRTRMF PIC  X(0001).
000390     05  FILLER REDEFINES LCVRTRMF.
000391         10  LCVRTRMA PIC  X(0001).
000392     05  LCVRTRMI PIC  X(0003).
000393*    -------------------------------
000394     05  LCVRATEL PIC S9(0004) COMP.
000395     05  LCVRATEF PIC  X(0001).
000396     05  FILLER REDEFINES LCVRATEF.
000397         10  LCVRATEA PIC  X(0001).
000398     05  LCVRATEI PIC  9999V99.
000399*    -------------------------------
000400     05  LCVBENEL PIC S9(0004) COMP.
000401     05  LCVBENEF PIC  X(0001).
000402     05  FILLER REDEFINES LCVBENEF.
000403         10  LCVBENEA PIC  X(0001).
000404     05  LCVBENEI PIC  9(9)V99.
000405*    -------------------------------
000406     05  LCVFORML PIC S9(0004) COMP.
000407     05  LCVFORMF PIC  X(0001).
000408     05  FILLER REDEFINES LCVFORMF.
000409         10  LCVFORMA PIC  X(0001).
000410     05  LCVFORMI PIC  X(0012).
000411*    -------------------------------
000412     05  LCVCNDTL PIC S9(0004) COMP.
000413     05  LCVCNDTF PIC  X(0001).
000414     05  FILLER REDEFINES LCVCNDTF.
000415         10  LCVCNDTA PIC  X(0001).
000416     05  LCVCNDTI PIC  X(0008).
000417*    -------------------------------
000418     05  LCVEXITL PIC S9(0004) COMP.
000419     05  LCVEXITF PIC  X(0001).
000420     05  FILLER REDEFINES LCVEXITF.
000421         10  LCVEXITA PIC  X(0001).
000422     05  LCVEXITI PIC  X(0008).
000423*    -------------------------------
000424     05  LCVSTATL PIC S9(0004) COMP.
000425     05  LCVSTATF PIC  X(0001).
000426     05  FILLER REDEFINES LCVSTATF.
000427         10  LCVSTATA PIC  X(0001).
000428     05  LCVSTATI PIC  X(0006).
000429*    -------------------------------
000430     05  ACVDSCRL PIC S9(0004) COMP.
000431     05  ACVDSCRF PIC  X(0001).
000432     05  FILLER REDEFINES ACVDSCRF.
000433         10  ACVDSCRA PIC  X(0001).
000434     05  ACVDSCRI PIC  X(0006).
000435*    -------------------------------
000436     05  ACVKINDL PIC S9(0004) COMP.
000437     05  ACVKINDF PIC  X(0001).
000438     05  FILLER REDEFINES ACVKINDF.
000439         10  ACVKINDA PIC  X(0001).
000440     05  ACVKINDI PIC  X(0003).
000441*    -------------------------------
000442     05  ACVCDL PIC S9(0004) COMP.
000443     05  ACVCDF PIC  X(0001).
000444     05  FILLER REDEFINES ACVCDF.
000445         10  ACVCDA PIC  X(0001).
000446     05  ACVCDI PIC  X(0002).
000447*    -------------------------------
000448     05  ACVOTRML PIC S9(0004) COMP.
000449     05  ACVOTRMF PIC  X(0001).
000450     05  FILLER REDEFINES ACVOTRMF.
000451         10  ACVOTRMA PIC  X(0001).
000452     05  ACVOTRMI PIC  999.
000453*    -------------------------------
000454     05  ACVRTRML PIC S9(0004) COMP.
000455     05  ACVRTRMF PIC  X(0001).
000456     05  FILLER REDEFINES ACVRTRMF.
000457         10  ACVRTRMA PIC  X(0001).
000458     05  ACVRTRMI PIC  X(0003).
000459*    -------------------------------
000460     05  ACVRATEL PIC S9(0004) COMP.
000461     05  ACVRATEF PIC  X(0001).
000462     05  FILLER REDEFINES ACVRATEF.
000463         10  ACVRATEA PIC  X(0001).
000464     05  ACVRATEI PIC  9999V99.
000465*    -------------------------------
000466     05  ACVBENEL PIC S9(0004) COMP.
000467     05  ACVBENEF PIC  X(0001).
000468     05  FILLER REDEFINES ACVBENEF.
000469         10  ACVBENEA PIC  X(0001).
000470     05  ACVBENEI PIC  9(9)V99.
000471*    -------------------------------
000472     05  ACVFORML PIC S9(0004) COMP.
000473     05  ACVFORMF PIC  X(0001).
000474     05  FILLER REDEFINES ACVFORMF.
000475         10  ACVFORMA PIC  X(0001).
000476     05  ACVFORMI PIC  X(0012).
000477*    -------------------------------
000478     05  ACVCNDTL PIC S9(0004) COMP.
000479     05  ACVCNDTF PIC  X(0001).
000480     05  FILLER REDEFINES ACVCNDTF.
000481         10  ACVCNDTA PIC  X(0001).
000482     05  ACVCNDTI PIC  X(0008).
000483*    -------------------------------
000484     05  ACVEXITL PIC S9(0004) COMP.
000485     05  ACVEXITF PIC  X(0001).
000486     05  FILLER REDEFINES ACVEXITF.
000487         10  ACVEXITA PIC  X(0001).
000488     05  ACVEXITI PIC  X(0008).
000489*    -------------------------------
000490     05  ACVSTATL PIC S9(0004) COMP.
000491     05  ACVSTATF PIC  X(0001).
000492     05  FILLER REDEFINES ACVSTATF.
000493         10  ACVSTATA PIC  X(0001).
000494     05  ACVSTATI PIC  X(0006).
000495*    -------------------------------
000496     05  ERRMSG1L PIC S9(0004) COMP.
000497     05  ERRMSG1F PIC  X(0001).
000498     05  FILLER REDEFINES ERRMSG1F.
000499         10  ERRMSG1A PIC  X(0001).
000500     05  ERRMSG1I PIC  X(0072).
000501*    -------------------------------
000502     05  ERRMSG2L PIC S9(0004) COMP.
000503     05  ERRMSG2F PIC  X(0001).
000504     05  FILLER REDEFINES ERRMSG2F.
000505         10  ERRMSG2A PIC  X(0001).
000506     05  ERRMSG2I PIC  X(0072).
000507*    -------------------------------
000508     05  BCERT1L PIC S9(0004) COMP.
000509     05  BCERT1F PIC  X(0001).
000510     05  FILLER REDEFINES BCERT1F.
000511         10  BCERT1A PIC  X(0001).
000512     05  BCERT1I PIC  X(0010).
000513*    -------------------------------
000514     05  BSUFX1L PIC S9(0004) COMP.
000515     05  BSUFX1F PIC  X(0001).
000516     05  FILLER REDEFINES BSUFX1F.
000517         10  BSUFX1A PIC  X(0001).
000518     05  BSUFX1I PIC  X(0001).
000519*    -------------------------------
000520     05  BCERT2L PIC S9(0004) COMP.
000521     05  BCERT2F PIC  X(0001).
000522     05  FILLER REDEFINES BCERT2F.
000523         10  BCERT2A PIC  X(0001).
000524     05  BCERT2I PIC  X(0010).
000525*    -------------------------------
000526     05  BSUFX2L PIC S9(0004) COMP.
000527     05  BSUFX2F PIC  X(0001).
000528     05  FILLER REDEFINES BSUFX2F.
000529         10  BSUFX2A PIC  X(0001).
000530     05  BSUFX2I PIC  X(0001).
000531*    -------------------------------
000532     05  BCERT3L PIC S9(0004) COMP.
000533     05  BCERT3F PIC  X(0001).
000534     05  FILLER REDEFINES BCERT3F.
000535         10  BCERT3A PIC  X(0001).
000536     05  BCERT3I PIC  X(0010).
000537*    -------------------------------
000538     05  BSUFX3L PIC S9(0004) COMP.
000539     05  BSUFX3F PIC  X(0001).
000540     05  FILLER REDEFINES BSUFX3F.
000541         10  BSUFX3A PIC  X(0001).
000542     05  BSUFX3I PIC  X(0001).
000543*    -------------------------------
000544     05  BCERT4L PIC S9(0004) COMP.
000545     05  BCERT4F PIC  X(0001).
000546     05  FILLER REDEFINES BCERT4F.
000547         10  BCERT4A PIC  X(0001).
000548     05  BCERT4I PIC  X(0010).
000549*    -------------------------------
000550     05  BSUFX4L PIC S9(0004) COMP.
000551     05  BSUFX4F PIC  X(0001).
000552     05  FILLER REDEFINES BSUFX4F.
000553         10  BSUFX4A PIC  X(0001).
000554     05  BSUFX4I PIC  X(0001).
000555*    -------------------------------
000556     05  BCERT5L PIC S9(0004) COMP.
000557     05  BCERT5F PIC  X(0001).
000558     05  FILLER REDEFINES BCERT5F.
000559         10  BCERT5A PIC  X(0001).
000560     05  BCERT5I PIC  X(0010).
000561*    -------------------------------
000562     05  BSUFX5L PIC S9(0004) COMP.
000563     05  BSUFX5F PIC  X(0001).
000564     05  FILLER REDEFINES BSUFX5F.
000565         10  BSUFX5A PIC  X(0001).
000566     05  BSUFX5I PIC  X(0001).
000567*    -------------------------------
000568     05  ENTERPFL PIC S9(0004) COMP.
000569     05  ENTERPFF PIC  X(0001).
000570     05  FILLER REDEFINES ENTERPFF.
000571         10  ENTERPFA PIC  X(0001).
000572     05  ENTERPFI PIC  99.
000573*    -------------------------------
000574     05  PF5L PIC S9(0004) COMP.
000575     05  PF5F PIC  X(0001).
000576     05  FILLER REDEFINES PF5F.
000577         10  PF5A PIC  X(0001).
000578     05  PF5I PIC  X(0013).
000579 01  EL130AO REDEFINES EL130AI.
000580     05  FILLER            PIC  X(0012).
000581*    -------------------------------
000582     05  FILLER            PIC  X(0003).
000583     05  RUNDTEO PIC  X(0008).
000584*    -------------------------------
000585     05  FILLER            PIC  X(0003).
000586     05  RUNTIMEO PIC  99.99.
000587*    -------------------------------
000588     05  FILLER            PIC  X(0003).
000589     05  COMPO PIC  X(0003).
000590*    -------------------------------
000591     05  FILLER            PIC  X(0003).
000592     05  SEQUO PIC  X(0010).
000593*    -------------------------------
000594     05  FILLER            PIC  X(0003).
000595     05  MAINTO PIC  X(0001).
000596*    -------------------------------
000597     05  FILLER            PIC  X(0003).
000598     05  CLMNOO PIC  X(0007).
000599*    -------------------------------
000600     05  FILLER            PIC  X(0003).
000601     05  CLMCARRO PIC  X(0001).
000602*    -------------------------------
000603     05  FILLER            PIC  X(0003).
000604     05  CLMTYPEO PIC  X(0001).
000605*    -------------------------------
000606     05  FILLER            PIC  X(0003).
000607     05  CERTNOO PIC  X(0010).
000608*    -------------------------------
000609     05  FILLER            PIC  X(0003).
000610     05  SUFXO PIC  X(0001).
000611*    -------------------------------
000612     05  FILLER            PIC  X(0003).
000613     05  PCERTNOO PIC  X(0010).
000614*    -------------------------------
000615     05  FILLER            PIC  X(0003).
000616     05  PSUFXO PIC  X(0001).
000617*    -------------------------------
000618     05  FILLER            PIC  X(0003).
000619     05  INCURO PIC  X(0008).
000620*    -------------------------------
000621     05  FILLER            PIC  X(0003).
000622     05  REPORTO PIC  X(0008).
000623*    -------------------------------
000624     05  FILLER            PIC  X(0003).
000625     05  ICD1O PIC  X(0008).
000626*    -------------------------------
000627     05  FILLER            PIC  X(0003).
000628     05  ICD2O PIC  X(0008).
000629*    -------------------------------
000630     05  FILLER            PIC  X(0003).
000631     05  DIAGO PIC  X(0060).
000632*    -------------------------------
000633     05  FILLER            PIC  X(0003).
000634     05  BENECDO PIC  X(0010).
000635*    -------------------------------
000636     05  FILLER            PIC  X(0003).
000637     05  BIRTHDTO PIC  X(0008).
000638*    -------------------------------
000639     05  FILLER            PIC  X(0003).
000640     05  SSNO PIC  X(0011).
000641*    -------------------------------
000642     05  FILLER            PIC  X(0003).
000643     05  SEXO PIC  X(0001).
000644*    -------------------------------
000645     05  FILLER            PIC  X(0003).
000646     05  INSTYPEO PIC  X(0001).
000647*    -------------------------------
000648     05  FILLER            PIC  X(0003).
000649     05  LSTNMEO PIC  X(0015).
000650*    -------------------------------
000651     05  FILLER            PIC  X(0003).
000652     05  FSTNMEO PIC  X(0012).
000653*    -------------------------------
000654     05  FILLER            PIC  X(0003).
000655     05  INITO PIC  X(0001).
000656*    -------------------------------
000657     05  FILLER            PIC  X(0003).
000658     05  LOANNOO PIC  X(0008).
000659*    -------------------------------
000660     05  FILLER            PIC  X(0003).
000661     05  CRTLNMEO PIC  X(0015).
000662*    -------------------------------
000663     05  FILLER            PIC  X(0003).
000664     05  CRTFNMEO PIC  X(0012).
000665*    -------------------------------
000666     05  FILLER            PIC  X(0003).
000667     05  CRTINITO PIC  X(0001).
000668*    -------------------------------
000669     05  FILLER            PIC  X(0003).
000670     05  LOANBALO PIC  ZZZZZ9.99.
000671*    -------------------------------
000672     05  FILLER            PIC  X(0003).
000673     05  MANRSVO PIC  Z(5)9.99.
000674*    -------------------------------
000675     05  FILLER            PIC  X(0003).
000676     05  SUPVO PIC  X(0001).
000677*    -------------------------------
000678     05  FILLER            PIC  X(0003).
000679     05  PRICDO PIC  X(0001).
000680*    -------------------------------
000681     05  FILLER            PIC  X(0003).
000682     05  PRODO PIC  X(0008).
000683*    -------------------------------
000684     05  FILLER            PIC  X(0003).
000685     05  PRODCDO PIC  X(0001).
000686*    -------------------------------
000687     05  FILLER            PIC  X(0003).
000688     05  FILETOO PIC  X(0004).
000689*    -------------------------------
000690     05  FILLER            PIC  X(0003).
000691     05  PROCCDO PIC  X(0004).
000692*    -------------------------------
000693     05  FILLER            PIC  X(0003).
000694     05  RELCLMO PIC  X(0007).
000695*    -------------------------------
000696     05  FILLER            PIC  X(0003).
000697     05  PRTOPTO PIC  X(0001).
000698*    -------------------------------
000699     05  FILLER            PIC  X(0003).
000700     05  ALTPRTO PIC  X(0004).
000701*    -------------------------------
000702     05  FILLER            PIC  X(0003).
000703     05  CERTMTO PIC  X(0001).
000704*    -------------------------------
000705     05  FILLER            PIC  X(0003).
000706     05  CRTCARRO PIC  X(0001).
000707*    -------------------------------
000708     05  FILLER            PIC  X(0003).
000709     05  GROUPO PIC  X(0006).
000710*    -------------------------------
000711     05  FILLER            PIC  X(0003).
000712     05  STATEO PIC  X(0002).
000713*    -------------------------------
000714     05  FILLER            PIC  X(0003).
000715     05  ACCOUNTO PIC  X(0010).
000716*    -------------------------------
000717     05  FILLER            PIC  X(0003).
000718     05  EFFDTO PIC  X(0008).
000719*    -------------------------------
000720     05  FILLER            PIC  X(0003).
000721     05  CRTSSNO PIC  X(0011).
000722*    -------------------------------
000723     05  FILLER            PIC  X(0003).
000724     05  MEMCAPO PIC  X(0010).
000725*    -------------------------------
000726     05  FILLER            PIC  X(0003).
000727     05  MEMBERO PIC  X(0012).
000728*    -------------------------------
000729     05  FILLER            PIC  X(0003).
000730     05  REINCDO PIC  X(0001).
000731*    -------------------------------
000732     05  FILLER            PIC  X(0003).
000733     05  ISSAGEO PIC  X(0002).
000734*    -------------------------------
000735     05  FILLER            PIC  X(0003).
000736     05  APRO PIC  9(3).9(4).
000737*    -------------------------------
000738     05  FILLER            PIC  X(0003).
000739     05  PMTFREQO PIC  99.
000740*    -------------------------------
000741     05  FILLER            PIC  X(0003).
000742     05  INDGRPO PIC  X(0001).
000743*    -------------------------------
000744     05  FILLER            PIC  X(0003).
000745     05  PREMTYPO PIC  X(0001).
000746*    -------------------------------
000747     05  FILLER            PIC  X(0003).
000748     05  ADDONDTO PIC  X(0008).
000749*    -------------------------------
000750     05  FILLER            PIC  X(0003).
000751     05  JNTLNMEO PIC  X(0015).
000752*    -------------------------------
000753     05  FILLER            PIC  X(0003).
000754     05  JNTFNMEO PIC  X(0002).
000755*    -------------------------------
000756     05  FILLER            PIC  X(0003).
000757     05  JNTINITO PIC  X(0001).
000758*    -------------------------------
000759     05  FILLER            PIC  X(0003).
000760     05  JNTAGEO PIC  X(0002).
000761*    -------------------------------
000762     05  FILLER            PIC  X(0003).
000763     05  LCVDSCRO PIC  X(0006).
000764*    -------------------------------
000765     05  FILLER            PIC  X(0003).
000766     05  LCVKINDO PIC  X(0003).
000767*    -------------------------------
000768     05  FILLER            PIC  X(0003).
000769     05  LCVCDO PIC  X(0002).
000770*    -------------------------------
000771     05  FILLER            PIC  X(0003).
000772     05  LCVOTRMO PIC  999.
000773*    -------------------------------
000774     05  FILLER            PIC  X(0003).
000775     05  LCVRTRMO PIC  ZZZ.
000776*    -------------------------------
000777     05  FILLER            PIC  X(0003).
000778     05  LCVRATEO PIC  ZZZ.ZZ.
000779*    -------------------------------
000780     05  FILLER            PIC  X(0003).
000781     05  LCVBENEO PIC  ZZZZZZZZ.ZZ.
000782*    -------------------------------
000783     05  FILLER            PIC  X(0003).
000784     05  LCVFORMO PIC  X(0012).
000785*    -------------------------------
000786     05  FILLER            PIC  X(0003).
000787     05  LCVCNDTO PIC  X(0008).
000788*    -------------------------------
000789     05  FILLER            PIC  X(0003).
000790     05  LCVEXITO PIC  X(0008).
000791*    -------------------------------
000792     05  FILLER            PIC  X(0003).
000793     05  LCVSTATO PIC  X(0006).
000794*    -------------------------------
000795     05  FILLER            PIC  X(0003).
000796     05  ACVDSCRO PIC  X(0006).
000797*    -------------------------------
000798     05  FILLER            PIC  X(0003).
000799     05  ACVKINDO PIC  X(0003).
000800*    -------------------------------
000801     05  FILLER            PIC  X(0003).
000802     05  ACVCDO PIC  X(0002).
000803*    -------------------------------
000804     05  FILLER            PIC  X(0003).
000805     05  ACVOTRMO PIC  999.
000806*    -------------------------------
000807     05  FILLER            PIC  X(0003).
000808     05  ACVRTRMO PIC  ZZZ.
000809*    -------------------------------
000810     05  FILLER            PIC  X(0003).
000811     05  ACVRATEO PIC  ZZZ.ZZ.
000812*    -------------------------------
000813     05  FILLER            PIC  X(0003).
000814     05  ACVBENEO PIC  ZZZZZZZZ.ZZ.
000815*    -------------------------------
000816     05  FILLER            PIC  X(0003).
000817     05  ACVFORMO PIC  X(0012).
000818*    -------------------------------
000819     05  FILLER            PIC  X(0003).
000820     05  ACVCNDTO PIC  X(0008).
000821*    -------------------------------
000822     05  FILLER            PIC  X(0003).
000823     05  ACVEXITO PIC  X(0008).
000824*    -------------------------------
000825     05  FILLER            PIC  X(0003).
000826     05  ACVSTATO PIC  X(0006).
000827*    -------------------------------
000828     05  FILLER            PIC  X(0003).
000829     05  ERRMSG1O PIC  X(0072).
000830*    -------------------------------
000831     05  FILLER            PIC  X(0003).
000832     05  ERRMSG2O PIC  X(0072).
000833*    -------------------------------
000834     05  FILLER            PIC  X(0003).
000835     05  BCERT1O PIC  X(0010).
000836*    -------------------------------
000837     05  FILLER            PIC  X(0003).
000838     05  BSUFX1O PIC  X(0001).
000839*    -------------------------------
000840     05  FILLER            PIC  X(0003).
000841     05  BCERT2O PIC  X(0010).
000842*    -------------------------------
000843     05  FILLER            PIC  X(0003).
000844     05  BSUFX2O PIC  X(0001).
000845*    -------------------------------
000846     05  FILLER            PIC  X(0003).
000847     05  BCERT3O PIC  X(0010).
000848*    -------------------------------
000849     05  FILLER            PIC  X(0003).
000850     05  BSUFX3O PIC  X(0001).
000851*    -------------------------------
000852     05  FILLER            PIC  X(0003).
000853     05  BCERT4O PIC  X(0010).
000854*    -------------------------------
000855     05  FILLER            PIC  X(0003).
000856     05  BSUFX4O PIC  X(0001).
000857*    -------------------------------
000858     05  FILLER            PIC  X(0003).
000859     05  BCERT5O PIC  X(0010).
000860*    -------------------------------
000861     05  FILLER            PIC  X(0003).
000862     05  BSUFX5O PIC  X(0001).
000863*    -------------------------------
000864     05  FILLER            PIC  X(0003).
000865     05  ENTERPFO PIC  X(0002).
000866*    -------------------------------
000867     05  FILLER            PIC  X(0003).
000868     05  PF5O PIC  X(0013).
000869*    -------------------------------
      *<<((file: EL130S))
000779     EJECT
000780*    COPY ELCDATE.
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
000781     EJECT
000782*    COPY ELCCALC.
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
000783     EJECT
000784*    COPY ELCLOGOF.
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
000785     EJECT
000786*    COPY ELCATTR.
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
000787     EJECT
000788*    COPY ELCEMIB.
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
000789     EJECT
000790*    COPY ELCJPFX.
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
000791         PIC X(512).
000792     EJECT
000793*    COPY ELCAID.
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
000794
000795 01  FILLER    REDEFINES DFHAID.
000796     12  FILLER              PIC X(8).
000797     12  PF-VALUES           PIC X       OCCURS 12.
000798
000799     EJECT
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
000801
000802 01  DFHCOMMAREA             PIC X(1024).
000803
000804*01 PARMLIST .
000805*    02  FILLER              PIC S9(8)   COMP.
000806*    02  ELMSTR-POINTER      PIC S9(8)   COMP.
000807*    02  ELCNTL-POINTER      PIC S9(8)   COMP.
000808*    02  ELCERT-POINTER      PIC S9(8)   COMP.
000809*    02  ERACCT-POINTER      PIC S9(8)   COMP.
000810*    02  ELTRLR-POINTER      PIC S9(8)   COMP.
000811*    02  ELBENE-POINTER      PIC S9(8)   COMP.
000812*    02  ELACTQ-POINTER      PIC S9(8)   COMP.
000813*    02  ELARCH-POINTER      PIC S9(8)   COMP.
000814*    02  ERREIN-POINTER      PIC S9(8)   COMP.
000815     EJECT
000816*    COPY ELCMSTR.
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
000817     EJECT
000818*    COPY ELCCNTL.
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
000819     EJECT
000820*    COPY ELCCERT.
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
000821     EJECT
000822*    COPY ERCACCT.
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
000823     EJECT
000824*    COPY ELCTRLR.
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
000825     EJECT
000826*    COPY ELCBENE.
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
000827     EJECT
000828*    COPY ELCDAR.
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
000829     EJECT
000830*    COPY ELCACTQ.
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
000831     EJECT
000832*    COPY ELCARCH.
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
000833     EJECT
000834*    COPY ERCREIN.
      *>>((file: ERCREIN))
000001******************************************************************
000002*                                                                *
000003*                            ERCREIN                             *
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.010                          *
000006*                                                                *
000007*   ONLINE CREDIT SYSTEM                                         *
000008*                                                                *
000009*   FILE DESCRIPTION = REINSURANCE MASTER FILE                   *
000010*                                                                *
000011*   FILE TYPE = VSAM,KSDS                                        *
000012*   RECORD SIZE = 4000  RECFORM = FIXED                          *
000013*                                                                *
000014*   BASE CLUSTER NAME = ERREIN                   RKP=2,LEN=8     *
000015*       ALTERNATE PATH = NONE                                    *
000016*                                                                *
000017*   LOG = NO                                                     *
000018*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000019*                                                                *
000020******************************************************************
000021*                   C H A N G E   L O G
000022*
000023* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000024*-----------------------------------------------------------------
000025*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000026* EFFECTIVE    NUMBER
000027*-----------------------------------------------------------------
000028* 103101    2001100100006  SMVA  ADD STATE EXHIBIT REPORT OPTION F
000029* 032707    2007032100006  PEMA  ADD EXCISE TAX CAPABILITY
000030******************************************************************
000031 01  REINSURANCE-RECORD.
000032     12  RE-RECORD-ID                      PIC XX.
000033         88  VALID-RE-ID                      VALUE 'RE'.
000034
000035     12  RE-CONTROL-PRIMARY.
000036         16  RE-COMPANY-CD                 PIC X.
000037         16  RE-KEY.
000038             20  RE-CODE                   PIC X.
000039                 88  RE-TABLE-RECORD          VALUE 'A'.
000040                 88  RE-COMPANY-RECORD        VALUE 'B'.
000041             20  RE-TABLE                  PIC XXX.
000042             20  FILLER                    PIC XXX.
000043         16  RE-COMPANY-KEY REDEFINES RE-KEY.
000044             20  FILLER                    PIC X.
000045             20  RE-COMPANY.
000046                 24  RE-COMP-PRIME         PIC XXX.
000047                 24  RE-COMP-SUB           PIC XXX.
000048
000049     12  RE-MAINT-INFORMATION.
000050         16  RE-LAST-MAINT-DT              PIC XX.
000051         16  RE-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000052         16  RE-LAST-MAINT-USER            PIC X(4).
000053         16  FILLER                        PIC X(10).
000054
000055     12  RE-TABLE-DATA.
000056         16  RE-100-COMP                   PIC 99.
000057
000058         16  RE-COMP-INFO    OCCURS 30 TIMES.
000059             20  RE-REI-COMP-NO.
000060                 24  RE-REI-COMP           PIC XXX.
000061                 24  RE-REI-COMP-SUB       PIC XXX.
000062             20  RE-LF-QC                  PIC X.
000063             20  RE-AH-QC                  PIC X.
000064             20  RE-LO-DATE                PIC 9(11)     COMP-3.
000065             20  RE-HI-DATE                PIC 9(11)     COMP-3.
000066             20  RE-LFAGE-LO               PIC 99.
000067             20  RE-LFAGE-HI               PIC 99.
000068             20  RE-AHAGE-LO               PIC 99.
000069             20  RE-AHAGE-HI               PIC 99.
000070             20  RE-LFTRM-LO               PIC S999       COMP-3.
000071             20  RE-LFTRM-HI               PIC S999       COMP-3.
000072             20  RE-AHTRM-LO               PIC S999       COMP-3.
000073             20  RE-AHTRM-HI               PIC S999       COMP-3.
000074             20  RE-LF-PCT                 PIC S9V9999    COMP-3.
000075             20  RE-AH-PCT                 PIC S9V9999    COMP-3.
000076             20  RE-LF-LIM-LO              PIC S9(9)V99   COMP-3.
000077             20  RE-LF-LIM-HI              PIC S9(9)V99   COMP-3.
000078             20  RE-LF-LO                  PIC S9(9)V99   COMP-3.
000079             20  RE-LF-HI                  PIC S9(9)V99   COMP-3.
000080             20  RE-AHBEN-LIM-LO           PIC S9(7)V99   COMP-3.
000081             20  RE-AHBEN-LIM-HI           PIC S9(7)V99   COMP-3.
000082             20  RE-AHBEN-LO               PIC S9(7)V99   COMP-3.
000083             20  RE-AHBEN-HI               PIC S9(7)V99   COMP-3.
000084             20  RE-AHMOA-LIM-LO           PIC S9(7)V99   COMP-3.
000085             20  RE-AHMOA-LIM-HI           PIC S9(7)V99   COMP-3.
000086             20  RE-AHMOA-LO               PIC S9(7)V99   COMP-3.
000087             20  RE-AHMOA-HI               PIC S9(7)V99   COMP-3.
000088             20  RE-LF-BEN-CODE            PIC X.
000089             20  RE-AH-BEN-CODE            PIC X.
000090             20  RE-INTERACTIVE            PIC X.
000091             20  RE-REMAINING              PIC X.
000092             20  RE-LF-RUNOFF-SW           PIC X.
000093             20  RE-AH-RUNOFF-SW           PIC X.
000094             20  FILLER                    PIC X(19).
000095
000096         16  RE-COMP-INFO-END              PIC X(6).
000097         16  RE-NSP-ST-CD-LF               PIC XX.
000098         16  RE-NSP-ST-CD-AH               PIC XX.
000099         16  RE-TABLE-CARRIER-SECURITY     PIC X.
000100             88  NO-TABLE-CARRIER-SECURITY    VALUE SPACE.
000101
000102         16  FILLER                        PIC X(27).
000103
000104     12  RE-COMPANY-DATA   REDEFINES   RE-TABLE-DATA.
000105         16  RE-NAME                       PIC X(30).
000106         16  RE-LF-PE                      PIC X.
000107         16  RE-AH-PE                      PIC X.
000108         16  RE-LF-FEE                     PIC S9V9999    COMP-3.
000109         16  RE-AH-FEE                     PIC S9V9999    COMP-3.
000110         16  RE-AH-PR-PCT                  PIC S9V9999    COMP-3.
000111         16  RE-AH-78-PCT                  PIC S9V9999    COMP-3.
000112         16  RE-PRT-ST                     PIC X.
000113         16  RE-PRT-OW                     PIC X.
000114         16  RE-MORT-CODE                  PIC X(4).
000115         16  RE-CLAIM-CODE                 PIC X.
000116         16  RE-ZERO-LF-FEE                PIC X.
000117         16  RE-ZERO-AH-FEE                PIC X.
000118         16  RE-CEDE-NAME                  PIC X(30).
000119         16  RE-LF-COMM                    PIC X.
000120         16  RE-AH-COMM                    PIC X.
000121         16  RE-LF-TAX                     PIC X.
000122         16  RE-AH-TAX                     PIC X.
000123         16  RE-CLM-INCURRED-LIM           PIC 9(11)  COMP-3.
000124         16  RE-LF-IBNR-PCT                PIC SV999      COMP-3.
000125         16  RE-AH-IBNR-PCT                PIC SV999      COMP-3.
000126
000127         16  RE-COMP-CARRIER-SECURITY      PIC X.
000128             88  NO-COMP-CARRIER-SECURITY     VALUE SPACE.
000129
000130         16  RE-LF-CEDING-FEE-BRACKETS.
000131             20  RE-LF-FEE-METHOD          PIC X.
000132                 88  RE-LF-FEE-BRACKETED         VALUE '1' '2'.
000133                 88  RE-LF-FEE-METHOD-1          VALUE '1'.
000134                 88  RE-LF-FEE-METHOD-2          VALUE '2'.
000135                 88  RE-LF-FEE-PERCENT           VALUE ' ' 'P'.
000136             20  RE-LF-FEE-BASIS           PIC X.
000137                 88  RE-LF-GROSS-CEDED             VALUE '1'.
000138                 88  RE-LF-NET-CEDED               VALUE '2'.
000139                 88  RE-LF-GROSS-WRITTEN           VALUE '3'.
000140                 88  RE-LF-NET-WRITTEN             VALUE '4'.
000141                 88  RE-LF-COMBINE-GROSS-CEDED     VALUE '5'.
000142                 88  RE-LF-COMBINE-NET-CEDED       VALUE '6'.
000143                 88  RE-LF-COMBINE-GROSS-WRITTEN   VALUE '7'.
000144                 88  RE-LF-COMBINE-NET-WRITTEN     VALUE '8'.
000145             20  FILLER                    PIC XXX.
000146             20  RE-LF-FEE-RANGES  OCCURS 6 TIMES.
000147                 24  RE-LF-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
000148                 24  RE-LF-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
000149
000150         16  RE-AH-CEDING-FEE-BRACKETS.
000151             20  RE-AH-FEE-METHOD          PIC X.
000152                 88  RE-AH-FEE-BRACKETED         VALUE '1' '2'.
000153                 88  RE-AH-FEE-METHOD-1          VALUE '1'.
000154                 88  RE-AH-FEE-METHOD-2          VALUE '2'.
000155                 88  RE-AH-FEE-PERCENT           VALUE ' ' 'P'.
000156             20  RE-AH-FEE-BASIS           PIC X.
000157                 88  RE-AH-GROSS-CEDED             VALUE '1'.
000158                 88  RE-AH-NET-CEDED               VALUE '2'.
000159                 88  RE-AH-GROSS-WRITTEN           VALUE '3'.
000160                 88  RE-AH-NET-WRITTEN             VALUE '4'.
000161                 88  RE-AH-COMBINE-GROSS-CEDED     VALUE '5'.
000162                 88  RE-AH-COMBINE-NET-CEDED       VALUE '6'.
000163                 88  RE-AH-COMBINE-GROSS-WRITTEN   VALUE '7'.
000164                 88  RE-AH-COMBINE-NET-WRITTEN     VALUE '8'.
000165             20  FILLER                    PIC XXX.
000166             20  RE-AH-FEE-RANGES  OCCURS 6 TIMES.
000167                 24  RE-AH-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
000168                 24  RE-AH-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
000169
000170         16  RE-EARNING-START-DT           PIC 9(11)  COMP-3.
000171
000172         16  RE-OLD-CEDING-STMT            PIC X.
000173
000174         16  RE-LF-CLM-PCT                 PIC S9V9999    COMP-3.
000175         16  RE-AH-CLM-PCT                 PIC S9V9999    COMP-3.
000176         16  RE-LF-CLM-MAX                 PIC S9(7)V99   COMP-3.
000177         16  RE-AH-CLM-MAX                 PIC S9(7)V99   COMP-3.
000178         16  RE-LF-PR-PCT                  PIC S9V9999    COMP-3.
000179         16  RE-LF-78-PCT                  PIC S9V9999    COMP-3.
000180         16  RE-REINS-GROUPING-CODE        PIC X(6).
000181         16  RE-MORT-SW                    PIC X.
000182         16  RE-CEDING-TYPE-FLAG           PIC X.
000183             88  RE-NO-CESSION-TYPE                VALUE ' '.
000184             88  RE-CEDED                          VALUE 'C'.
000185             88  RE-ASSUMED                        VALUE 'A'.
000186             88  RE-PHANTOM                        VALUE 'P'.
000187
000188         16  RE-CEDING-STMT-OPT-A          PIC X.
000189             88  REPORT-A-WANTED    VALUE ' ' 'Y'.
000190         16  RE-CEDING-STMT-OPT-B          PIC X.
000191             88  REPORT-B-WANTED    VALUE ' ' 'Y'.
000192         16  RE-CEDING-STMT-OPT-C          PIC X.
000193             88  REPORT-C-WANTED    VALUE ' ' 'Y'.
000194         16  RE-CEDING-STMT-OPT-D          PIC X.
000195             88  REPORT-D-WANTED    VALUE ' ' 'Y'.
000196         16  RE-CEDING-STMT-OPT-E          PIC X.
000197             88  REPORT-E-WANTED    VALUE ' ' 'Y'.
000198
000199         16  RE-PRT-CRSV                   PIC X.
000200
000201         16  RE-GL-CENTER                  PIC X(4).
000202
000203         16  RE-CUSTODIAL-BAL              PIC S9(7)V99   COMP-3.
000204
000205         16  RE-EARNING-STOP-DT            PIC 9(11)  COMP-3.
000206
000207         16  RE-EARN-STOP-CODE             PIC X.
000208             88  STOP-LIFE-EARNING  VALUE 'L' 'B'.
000209             88  STOP-AH-EARNING    VALUE 'A' 'B'.
000210
000211         16  RE-STATE-EXHIBIT-OPT-F        PIC X.
000212             88  RPTF-ECS152-WANTED VALUE ' ' 'Y'.
000213
000214         16  RE-EXCISE-TAX                 PIC S9V9999 COMP-3.
000215         16  FILLER                        PIC X(2281).
000216
000217         16  RE-DESC OCCURS 18 TIMES       PIC X(79).
000218
000219******************************************************************
      *<<((file: ERCREIN))
000835     EJECT
000836*    COPY ERCMAIL.
      *>>((file: ERCMAIL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCMAIL                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 374   RECFORM = FIX                            *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
000014*   ALTERNATE PATH    = NOT USED                                 *
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
000026* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
000027* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
000028* 111108                   PEMA  ADD CRED BENE ADDR2
000029******************************************************************
000030
000031 01  MAILING-DATA.
000032     12  MA-RECORD-ID                      PIC XX.
000033         88  VALID-MA-ID                       VALUE 'MA'.
000034
000035     12  MA-CONTROL-PRIMARY.
000036         16  MA-COMPANY-CD                 PIC X.
000037         16  MA-CARRIER                    PIC X.
000038         16  MA-GROUPING.
000039             20  MA-GROUPING-PREFIX        PIC XXX.
000040             20  MA-GROUPING-PRIME         PIC XXX.
000041         16  MA-STATE                      PIC XX.
000042         16  MA-ACCOUNT.
000043             20  MA-ACCOUNT-PREFIX         PIC X(4).
000044             20  MA-ACCOUNT-PRIME          PIC X(6).
000045         16  MA-CERT-EFF-DT                PIC XX.
000046         16  MA-CERT-NO.
000047             20  MA-CERT-PRIME             PIC X(10).
000048             20  MA-CERT-SFX               PIC X.
000049
000050     12  FILLER                            PIC XX.
000051
000052     12  MA-ACCESS-CONTROL.
000053         16  MA-SOURCE-SYSTEM              PIC XX.
000054             88  MA-FROM-CREDIT                VALUE 'CR'.
000055             88  MA-FROM-VSI                   VALUE 'VS'.
000056             88  MA-FROM-WARRANTY              VALUE 'WA'.
000057             88  MA-FROM-OTHER                 VALUE 'OT'.
000058         16  MA-RECORD-ADD-DT              PIC XX.
000059         16  MA-RECORD-ADDED-BY            PIC XXXX.
000060         16  MA-LAST-MAINT-DT              PIC XX.
000061         16  MA-LAST-MAINT-BY              PIC XXXX.
000062         16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
000063
000064     12  MA-PROFILE-INFO.
000065         16  MA-QUALIFY-CODE-1             PIC XX.
000066         16  MA-QUALIFY-CODE-2             PIC XX.
000067         16  MA-QUALIFY-CODE-3             PIC XX.
000068         16  MA-QUALIFY-CODE-4             PIC XX.
000069         16  MA-QUALIFY-CODE-5             PIC XX.
000070
000071         16  MA-INSURED-LAST-NAME          PIC X(15).
000072         16  MA-INSURED-FIRST-NAME         PIC X(10).
000073         16  MA-INSURED-MIDDLE-INIT        PIC X.
000074         16  MA-INSURED-ISSUE-AGE          PIC 99.
000075         16  MA-INSURED-BIRTH-DT           PIC XX.
000076         16  MA-INSURED-SEX                PIC X.
000077             88  MA-SEX-MALE                   VALUE 'M'.
000078             88  MA-SEX-FEMALE                 VALUE 'F'.
000079         16  MA-INSURED-SOC-SEC-NO         PIC X(11).
000080
000081         16  MA-ADDRESS-CORRECTED          PIC X.
000082         16  MA-JOINT-BIRTH-DT             PIC XX.
000083*        16  FILLER                        PIC X(12).
000084
000085         16  MA-ADDRESS-LINE-1             PIC X(30).
000086         16  MA-ADDRESS-LINE-2             PIC X(30).
000087         16  MA-CITY-STATE.
000088             20  MA-CITY                   PIC X(28).
000089             20  MA-ADDR-STATE             PIC XX.
000090         16  MA-ZIP.
000091             20  MA-ZIP-CODE.
000092                 24  MA-ZIP-CODE-1ST       PIC X(1).
000093                     88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
000094                 24  FILLER                PIC X(4).
000095             20  MA-ZIP-PLUS4              PIC X(4).
000096         16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
000097             20  MA-CAN-POSTAL-CODE-1      PIC X(3).
000098             20  MA-CAN-POSTAL-CODE-2      PIC X(3).
000099             20  FILLER                    PIC X(3).
000100
000101         16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
000102
000103         16  FILLER                        PIC XXX.
000104*        16  FILLER                        PIC X(10).
000105
000106
000107     12  MA-CRED-BENE-INFO.
000108         16  MA-CRED-BENE-NAME                 PIC X(25).
000109         16  MA-CRED-BENE-ADDR                 PIC X(30).
000110         16  MA-CRED-BENE-ADDR2                PIC X(30).
000111         16  MA-CRED-BENE-CTYST.
000112             20  MA-CRED-BENE-CITY             PIC X(28).
000113             20  MA-CRED-BENE-STATE            PIC XX.
000114         16  MA-CRED-BENE-ZIP.
000115             20  MA-CB-ZIP-CODE.
000116                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
000117                     88  MA-CB-CANADIAN-POST-CODE
000118                                           VALUE 'A' THRU 'Z'.
000119                 24  FILLER                    PIC X(4).
000120             20  MA-CB-ZIP-PLUS4               PIC X(4).
000121         16  MA-CB-CANADIAN-POSTAL-CODE
000122                            REDEFINES MA-CRED-BENE-ZIP.
000123             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
000124             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
000125             20  FILLER                        PIC X(3).
000126     12  MA-POST-CARD-MAIL-DATA.
000127         16  MA-MAIL-DATA OCCURS 7.
000128             20  MA-MAIL-TYPE              PIC X.
000129                 88  MA-12MO-MAILING           VALUE '1'.
000130                 88  MA-EXP-MAILING            VALUE '2'.
000131             20  MA-MAIL-STATUS            PIC X.
000132                 88  MA-MAIL-ST-MAILED         VALUE '1'.
000133                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
000134                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
000135             20  MA-MAIL-DATE              PIC XX.
000136     12  FILLER                            PIC XX.
000137     12  FILLER                            PIC XX.
000138*    12  FILLER                            PIC X(30).
000139******************************************************************
      *<<((file: ERCMAIL))
000837     EJECT
000838*    COPY ELCCRTT.
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
000839     EJECT
000840
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL130' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000841 VCOBOL-DUMMY-PROCEDURE.
000842     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
000843     MOVE '5'                    TO DC-OPTION-CODE.
000844     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
000845     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000846     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000847
000848     IF EIBCALEN = ZERO
000849         GO TO 8800-UNAUTHORIZED-ACCESS
000850     END-IF.
000851
000852     MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
000853     MOVE EIBTRMID               TO QID-TERM.
000854     MOVE +2                     TO EMI-NUMBER-OF-LINES
000855     move spaces                 to emi-error-lines
000856     MOVE '2'                    TO EMI-SWITCH2.
000857     MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6.
000858     MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6.
000859
000860     
      * EXEC CICS HANDLE CONDITION
000861*        QIDERR     (0100-TEST-ENTRY)
000862*        MAPFAIL    (8100-SEND-INITIAL-MAP)
000863*        PGMIDERR   (9600-PGMID-ERROR)
000864*        TERMIDERR  (8820-TERM-ERROR)
000865*        TRANSIDERR (8830-TRAN-ERROR)
000866*        ERROR      (9990-ABEND)
000867*    END-EXEC.
      *    MOVE '"$N?L[\.              ! " #00007349' TO DFHEIV0
           MOVE X'22244E3F4C5B5C2E20202020' &
                X'202020202020202020202120' &
                X'2220233030303037333439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000868
000869     IF PI-RETURN-TO-PROGRAM = THIS-PGM  OR
000870        PI-CALLING-PROGRAM   = XCTL-6592
000871         MOVE PI-CALLING-PROGRAM TO RETURNED-FROM
000872     ELSE
000873         MOVE SPACES             TO RETURNED-FROM
000874     END-IF.
000875
000876     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000877         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000878             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000879             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000880             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000881             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000882             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000883             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000884             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000885             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000886         ELSE
000887             MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
000888             MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
000889             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
000890             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
000891             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
000892             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
000893             MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
000894             MOVE SPACES               TO PI-SAVED-PROGRAM-6
000895         END-IF
000896     END-IF.
000897
000898     IF EIBTRNID = TRANS-ID
000899         IF EIBAID = DFHCLEAR
000900             GO TO 9400-CLEAR
000901         ELSE
000902             GO TO 0200-RECEIVE
000903         END-IF
000904     END-IF.
000905
000906     IF RETURNED-FROM NOT = SPACES
000907         GO TO 0600-RECOVER-TEMP-STORAGE
000908     END-IF.
000909
000910     MOVE LOW-VALUES TO EL130AO.
000911     move zeros to pi-dcc-max-amt
000912                   pi-dcc-max-benefits
000913
000914
000915     
      * EXEC CICS DELETEQ TS
000916*         QUEUE   (QID)
000917*    END-EXEC.
      *    MOVE '*&                    #   #00007404' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037343034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000918
000919 0100-TEST-ENTRY.
000920     IF PI-RETURN-TO-PROGRAM = XCTL-127 OR XCTL-727
000921         GO TO 0650-FROM-EL127
000922     ELSE
000923         MOVE ZEROS              TO PI-CERT-SELECT-CNT
000924                                    PI-CERT-PROCESSED
000925                                    PI-LETTER-ERROR-CODE
000926     END-IF.
000927
000928     IF PI-RETURN-TO-PROGRAM = XCTL-132
000929         GO TO 0640-FROM-EL132
000930     END-IF.
000931
000932     GO TO 8100-SEND-INITIAL-MAP.
000933
000934     EJECT
000935 0200-RECEIVE.
000936     MOVE LOW-VALUES             TO EL130AI.
000937
000938     IF PI-PROCESSOR-ID = 'LGXX'
000939*        NEXT SENTENCE
000940         CONTINUE
000941     ELSE
000942         
      * EXEC CICS READQ TS
000943*            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
000944*            INTO    (SECURITY-CONTROL)
000945*            LENGTH  (SC-COMM-LENGTH)
000946*            ITEM    (SC-ITEM)
000947*        END-EXEC
      *    MOVE '*$II   L              ''   #00007431' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037343331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
000948         MOVE SC-CLAIMS-DISPLAY (1)   TO  PI-DISPLAY-CAP
000949         MOVE SC-CLAIMS-UPDATE  (1)   TO  PI-MODIFY-CAP
000950         IF NOT DISPLAY-CAP
000951             MOVE 'READ'              TO  SM-READ
000952             PERFORM 9995-SECURITY-VIOLATION
000953             MOVE ER-0070             TO  EMI-ERROR
000954             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000955             GO TO 8100-SEND-INITIAL-MAP
000956         END-IF
000957     END-IF.
000958
000959     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000960         MOVE ER-7008 TO EMI-ERROR
000961         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000962         MOVE -1 TO MAINTL
000963         GO TO 8200-SEND-DATAONLY
000964     END-IF.
000965
000966
000967     
      * EXEC CICS RECEIVE
000968*         MAP     (MAP-NAME)
000969*         MAPSET  (MAPSET-NAME)
000970*         INTO    (EL130AI)
000971*    END-EXEC.
           MOVE LENGTH OF
            EL130AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00007456' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037343536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL130AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000972
000973     IF NOT PI-NO-CARRIER-SECURITY
000974        MOVE +1                  TO CLMCARRL
000975        MOVE PI-CARRIER-SECURITY TO CLMCARRI
000976     END-IF.
000977
000978     IF ENTERPFL = ZERO
000979         GO TO 0300-CHECK-PFKEYS
000980     END-IF.
000981
000982     IF EIBAID NOT = DFHENTER
000983         MOVE ER-0004            TO EMI-ERROR
000984         GO TO 0320-INPUT-ERROR
000985     END-IF.
000986
000987     IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)
000988         MOVE PF-VALUES (ENTERPFI) TO EIBAID
000989     ELSE
000990         MOVE ER-0029            TO EMI-ERROR
000991         GO TO 0320-INPUT-ERROR
000992     END-IF.
000993
000994 0300-CHECK-PFKEYS.
000995     IF EIBAID = DFHPF23
000996         GO TO 8810-PF23
000997     END-IF.
000998
000999     IF EIBAID = DFHPF24
001000         GO TO 9200-RETURN-MAIN-MENU
001001     END-IF.
001002
001003     IF EIBAID = DFHPF12
001004         GO TO 9500-PF12
001005     END-IF.
001006
001007     IF EIBAID = DFHPF3
001008         MOVE SPACES              TO MAINTI
001009         MOVE ZEROS               TO MAINTL
001010         PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
001011         MOVE XCTL-127            TO PGM-NAME
001012         GO TO 9300-XCTL
001013     END-IF.
001014
001015     IF EIBAID = DFHPF4
001016         PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
001017         MOVE XCTL-132            TO PGM-NAME
001018         GO TO 9300-XCTL
001019     END-IF.
001020
001021     IF EIBAID = DFHPF13
001022         IF PI-ACCESS-TO-BOTH-SYSTEMS
001023             PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
001024             MOVE PI-CARRIER          TO  PI-CR-CARRIER
001025             MOVE PI-GROUPING         TO  PI-CR-GROUPING
001026             MOVE PI-STATE            TO  PI-CR-STATE
001027             MOVE PI-ACCOUNT          TO  PI-CR-ACCOUNT
001028             MOVE XCTL-650            TO  PGM-NAME
001029             GO TO 9300-XCTL
001030         ELSE
001031             MOVE -1                  TO  ENTERPFL
001032             MOVE ER-2566             TO  EMI-ERROR
001033             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001034             GO TO 8100-SEND-INITIAL-MAP
001035         END-IF
001036     END-IF.
001037
001038     IF EIBAID = DFHPF5
001039         IF PI-CERT-PROCESSED LESS THAN PI-CERT-SELECT-CNT
001040             ADD +1 TO PI-CERT-PROCESSED
001041             GO TO 0650-FROM-EL127
001042         ELSE
001043             MOVE ER-7686         TO EMI-ERROR
001044             GO TO 0320-INPUT-ERROR
001045         END-IF
001046     END-IF.
001047
001048     IF EIBAID = DFHPF7 OR DFHPF8 OR DFHPF9
001049         GO TO 0800-TEST-PREVIOUS-CONTROL
001050     END-IF.
001051
001052     IF PI-HAS-CLAS-IC-CRDTCRD
001053         IF EIBAID = DFHPF10
001054             MOVE XCTL-725          TO PGM-NAME
001055             GO TO 9300-XCTL
001056         END-IF
001057     END-IF.
001058
001059     IF EIBAID = DFHPF14
001060         PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
001061         MOVE SPACES              TO PI-EL659-TO-EL130-CNTRL
001062         MOVE XCTL-659            TO PGM-NAME
001063         GO TO 9300-XCTL
001064     END-IF.
001065
001066     IF EIBAID = DFHPF15
001067         PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
001068         MOVE XCTL-114            TO PGM-NAME
001069         GO TO 9300-XCTL
001070     END-IF.
001071
001072
001073*    IF EIBAID = DFHPF11
001074*       GO TO 0600-RECOVER-TEMP-STORAGE
001075*    END-IF
001076
001077     IF EIBAID = DFHENTER OR DFHPF6 OR DFHPF11
001078         GO TO 0330-EDIT-DATA
001079     END-IF.
001080
001081     MOVE ER-0029                TO EMI-ERROR.
001082
001083 0320-INPUT-ERROR.
001084     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001085
001086     MOVE AL-UNBON               TO ENTERPFA.
001087
001088     IF ENTERPFL = ZERO
001089         MOVE -1                 TO MAINTL
001090     ELSE
001091         MOVE -1                 TO ENTERPFL
001092     END-IF.
001093
001094     GO TO 8200-SEND-DATAONLY.
001095
001096     EJECT
001097 0330-EDIT-DATA.
001098
001099     IF MAINTI = 'S'
001100         IF PRTOPTL GREATER THAN 0
001101             IF PRTOPTI NOT = 'N' AND 'L'
001102                 MOVE AL-UABON   TO  PRTOPTA
001103                 MOVE -1         TO  PRTOPTL
001104                 MOVE ER-0334    TO  EMI-ERROR
001105                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001106                 GO TO 8200-SEND-DATAONLY
001107             ELSE
001108                 IF PRTOPTI = 'L'
001109                     GO TO 0400-CREATE-ELACTQ
001110                 ELSE
001111                     GO TO 0480-PRINT-NOW
001112                 END-IF
001113             END-IF
001114         ELSE
001115             GO TO 1000-SHOW-CLAIM
001116         END-IF
001117     END-IF.
001118
001119     IF NOT MODIFY-CAP
001120         MOVE 'UPDATE'       TO  SM-READ
001121         PERFORM 9995-SECURITY-VIOLATION
001122         MOVE ER-0070        TO  EMI-ERROR
001123         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001124         GO TO 8100-SEND-INITIAL-MAP
001125     END-IF.
001126
001127     IF MAINTI = 'D'
001128         GO TO 2000-DELETE-CLAIM
001129     END-IF.
001130
001131     IF MAINTI = 'A'
001132         GO TO 3000-ADD-CLAIM
001133     END-IF.
001134
001135     IF MAINTI = 'I'
001136         GO TO 5000-INCURRED-CHANGE
001137     END-IF.
001138
001139     MOVE ER-0023                TO EMI-ERROR.
001140     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001141     MOVE -1                     TO MAINTL.
001142     MOVE AL-UABON               TO MAINTA.
001143
001144     GO TO 8200-SEND-DATAONLY.
001145
001146     EJECT
001147 0400-CREATE-ELACTQ.
001148     IF CLMNOI   = PI-LAST-CLAIM      AND
001149        CLMCARRI = PI-LAST-CARR       AND
001150        CERTNOI  = PI-LAST-CERT-PRIME AND
001151        SUFXI    = PI-LAST-CERT-SUFX
001152         MOVE PI-LAST-CLAIM      TO PI-CLAIM-NO
001153         MOVE PI-LAST-CARR       TO PI-CARRIER
001154         MOVE PI-LAST-CERT       TO PI-CERT-NO
001155     ELSE
001156         MOVE ER-0207            TO EMI-ERROR
001157         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001158         MOVE -1                 TO MAINTL
001159         GO TO 8200-SEND-DATAONLY
001160     END-IF.
001161
001162     IF (NOT PI-NO-CARRIER-SECURITY OR
001163         NOT PI-NO-ACCOUNT-SECURITY)
001164         MOVE AL-UABON      TO  PRTOPTA
001165         MOVE -1            TO  PRTOPTL
001166         MOVE ER-2378       TO  EMI-ERROR
001167         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001168         GO TO 8200-SEND-DATAONLY
001169     END-IF.
001170
001171     MOVE PI-COMPANY-CD      TO ELACTQ-COMPANY-CD.
001172     MOVE PI-LAST-CARR       TO ELACTQ-CARRIER.
001173     MOVE PI-LAST-CLAIM      TO ELACTQ-CLAIM-NO.
001174     MOVE PI-LAST-CERT       TO ELACTQ-CERT-NO.
001175
001176
001177     
      * EXEC CICS HANDLE CONDITION
001178*         NOTFND     (0450-ADD-ELACTQ-RECORD)
001179*    END-EXEC.
      *    MOVE '"$I                   ! # #00007666' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303037363636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001180
001181
001182     
      * EXEC CICS READ
001183*         UPDATE
001184*         DATASET    (ELACTQ-DSID)
001185*         SET        (ADDRESS OF ACTIVITY-QUE)
001186*         RIDFLD     (ELACTQ-KEY)
001187*    END-EXEC.
      *    MOVE '&"S        EU         (   #00007671' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037363731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001188
001189     MOVE '2'         TO AQ-PENDING-STATUS-FLAG.
001190     MOVE +130        TO AQ-LAST-UPDATED-BY.
001191
001192
001193     
      * EXEC CICS REWRITE
001194*         DATASET     (ELACTQ-DSID)
001195*         FROM        (ACTIVITY-QUE)
001196*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007682' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037363832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-DSID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001197
001198     GO TO 0470-STATUS-FINISH.
001199
001200 0450-ADD-ELACTQ-RECORD.
001201
001202     
      * EXEC CICS GETMAIN
001203*        SET     (ADDRESS OF ACTIVITY-QUE)
001204*        LENGTH  (ELACTQ-LENGTH)
001205*        INITIMG (GETMAIN-SPACE)
001206*    END-EXEC.
      *    MOVE ',"IL                  $   #00007691' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037363931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELACTQ-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001207
001208     MOVE 'AQ'              TO AQ-RECORD-ID.
001209     MOVE ELACTQ-KEY        TO AQ-CONTROL-PRIMARY.
001210     MOVE +0                TO AQ-PAYMENT-COUNTER
001211                               AQ-PMT-UNAPPROVED-COUNT.
001212     MOVE LOW-VALUES        TO AQ-RESEND-DATE
001213                               AQ-FOLLOWUP-DATE.
001214     MOVE '2'               TO AQ-PENDING-STATUS-FLAG.
001215     MOVE +130              TO AQ-LAST-UPDATED-BY.
001216
001217
001218     
      * EXEC CICS WRITE
001219*         DATASET     (ELACTQ-DSID)
001220*         FROM        (ACTIVITY-QUE)
001221*         RIDFLD      (ELACTQ-KEY)
001222*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007707' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037373037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-DSID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001223
001224 0470-STATUS-FINISH.
001225     MOVE AL-UANOF               TO  PRTOPTA
001226                                     ALTPRTA.
001227
001228     MOVE SPACES                 TO  PRTOPTO
001229                                     ALTPRTO.
001230
001231     MOVE ER-0000                TO EMI-ERROR.
001232     MOVE -1                     TO MAINTL.
001233     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001234     GO TO 8100-SEND-INITIAL-MAP.
001235
001236 0480-PRINT-NOW.
001237
001238     
      * EXEC CICS HANDLE CONDITION
001239*        TERMIDERR   (8820-TERM-ERROR)
001240*        TRANSIDERR  (8830-TRAN-ERROR)
001241*    END-EXEC.
      *    MOVE '"$[\                  ! $ #00007727' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303037373237' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001242
001243     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
001244     MOVE '1'                    TO  CNTL-REC-TYPE.
001245     MOVE SPACES                 TO  CNTL-ACCESS.
001246     MOVE +0                     TO  CNTL-SEQ-NO.
001247     MOVE 'CNTL'                 TO  FILE-SWITCH.
001248
001249     PERFORM 7970-READ-CNTL THRU 7970-EXIT.
001250
001251     IF CF-FORMS-PRINTER-ID = SPACES
001252         IF ALTPRTL NOT GREATER THAN 0
001253             MOVE ER-0337        TO  EMI-ERROR
001254             MOVE -1             TO  ALTPRTL
001255             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001256             GO TO 8200-SEND-DATAONLY
001257         END-IF
001258     END-IF.
001259
001260     IF ALTPRTL GREATER THAN 0
001261         MOVE ALTPRTI            TO  CF-FORMS-PRINTER-ID
001262     END-IF.
001263
001264     MOVE PI-LAST-CLAIM          TO  PI-CLAIM-NO.
001265     MOVE PI-LAST-CARR           TO  PI-CARRIER.
001266     MOVE PI-LAST-CERT           TO  PI-CERT-NO.
001267
001268
001269     
      * EXEC CICS START
001270*        TRANSID   (START-TRANS-ID)
001271*        TERMID    (CF-FORMS-PRINTER-ID)
001272*        FROM      (PROGRAM-INTERFACE-BLOCK)
001273*        LENGTH    (PI-COMM-LENGTH)
001274*    END-EXEC.
      *    MOVE '0( LFT                1   #00007758' TO DFHEIV0
           MOVE X'3028204C4654202020202020' &
                X'202020202020202020203120' &
                X'2020233030303037373538' TO DFHEIV0
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
           
001275
001276     MOVE AL-UANOF               TO  PRTOPTA
001277                                     ALTPRTA.
001278
001279     MOVE SPACES                 TO  PRTOPTO
001280                                     ALTPRTO.
001281
001282     MOVE ER-0000                TO  EMI-ERROR.
001283     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001284     MOVE -1                     TO  MAINTL.
001285     GO TO 8100-SEND-INITIAL-MAP.
001286
001287     EJECT
001288 0500-CREATE-TEMP-STORAGE.
001289     MOVE EIBCPOSN               TO PI-CURSOR.
001290
001291    move emi-line1               to errmsg1o
001292    move emi-line2               to errmsg2o
001293    move emi-sub                 to pi-emi-sub
001294
001295     
      * EXEC CICS WRITEQ TS
001296*        QUEUE   (QID)
001297*        FROM    (EL130AI)
001298*        LENGTH  (QID-MAP-LENGTH)
001299*    END-EXEC.
      *    MOVE '*"     L              ''   #00007784' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037373834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL130AI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001300
001301
001302     
      * EXEC CICS WRITEQ TS
001303*        QUEUE    (QID)
001304*        FROM     (PROGRAM-INTERFACE-BLOCK)
001305*        LENGTH   (PI-COMM-LENGTH)
001306*    END-EXEC.
      *    MOVE '*"     L              ''   #00007791' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037373931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001307
001308 0599-EXIT.
001309      EXIT.
001310
001311     EJECT
001312 0600-RECOVER-TEMP-STORAGE.
001313     MOVE PI-CONTROL-IN-PROGRESS TO SAVE-CONTROL.
001314     MOVE PI-EL127-TO-EL130-CNTRL
001315                                 TO SAVE-EL127-TO-EL130-CNTRL.
001316     MOVE PI-EL659-TO-EL130-CNTRL
001317                                 TO SAVE-EL659-TO-EL130-CNTRL.
001318     MOVE PI-COMPANY-ID          TO SAVE-COMPANY-ID.
001319     MOVE PI-COMPANY-CD          TO SAVE-COMPANY-CD.
001320     MOVE PI-JOURNAL-FILE-ID     TO SAVE-JOURNAL-FILE-ID.
001321     MOVE PI-CREDIT-USER         TO SAVE-CREDIT-USER.
001322     MOVE PI-CLAIM-USER          TO SAVE-CLAIM-USER.
001323     MOVE PI-LIFE-OVERRIDE-L1    TO SAVE-LIFE-OVERRIDE-L1.
001324     MOVE PI-LIFE-OVERRIDE-L2    TO SAVE-LIFE-OVERRIDE-L2.
001325     MOVE PI-LIFE-OVERRIDE-L6    TO SAVE-LIFE-OVERRIDE-L6.
001326     MOVE PI-LIFE-OVERRIDE-L12   TO SAVE-LIFE-OVERRIDE-L12.
001327     MOVE PI-AH-OVERRIDE-L1      TO SAVE-AH-OVERRIDE-L1.
001328     MOVE PI-AH-OVERRIDE-L2      TO SAVE-AH-OVERRIDE-L2.
001329     MOVE PI-AH-OVERRIDE-L6      TO SAVE-AH-OVERRIDE-L6.
001330     MOVE PI-AH-OVERRIDE-L12     TO SAVE-AH-OVERRIDE-L12.
001331     MOVE PI-CERT-ACCESS-CONTROL TO SAVE-CERT-ACCESS-CONTROL.
001332     MOVE PI-CARRIER-CONTROL-LEVEL  TO SAVE-CARRIER-CONTROL-LEVEL.
001333     MOVE PI-PROGRAM-WORK-AREA (1:10)
001334                                 TO SAVE-BENEFICIARY
001335
001336
001337     
      * EXEC CICS HANDLE CONDITION
001338*        NOTFND  (0660-NOT-FOUND)
001339*        QIDERR  (0670-QIDERR)
001340*    END-EXEC.
      *    MOVE '"$IN                  ! % #00007826' TO DFHEIV0
           MOVE X'2224494E2020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303037383236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001341
001342
001343     
      * EXEC CICS READQ TS
001344*        QUEUE    (QID)
001345*        INTO     (EL130AI)
001346*        LENGTH   (QID-MAP-LENGTH)
001347*    END-EXEC.
      *    MOVE '*$I    L              ''   #00007832' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037383332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL130AI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001348
001349     IF RETURNED-FROM = XCTL-131
001350        IF CLAIM-DELETED-BY-EL131
001351            MOVE LOW-VALUES      TO CERTNOI
001352                                    SUFXI
001353            MOVE ZEROS           TO CERTNOL
001354                                    SUFXL
001355         END-IF
001356     END-IF.
001357
001358     IF RETURNED-FROM = XCTL-131
001359         MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA
001360         MOVE ZEROS              TO PI-CERT-SELECT-CNT
001361                                    PI-CERT-PROCESSED
001362     ELSE
001363
001364         
      * EXEC CICS READQ TS
001365*             QUEUE   (QID)
001366*             INTO    (PROGRAM-INTERFACE-BLOCK)
001367*             LENGTH  (PI-COMM-LENGTH)
001368*        END-EXEC
      *    MOVE '*$I    L              ''   #00007853' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037383533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001369     END-IF.
001370
001371
001372     
      * EXEC CICS DELETEQ TS
001373*        QUEUE   (QID)
001374*    END-EXEC.
      *    MOVE '*&                    #   #00007861' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037383631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001375
001376     MOVE SAVE-CONTROL           TO PI-CONTROL-IN-PROGRESS.
001377
001378     IF RETURNED-FROM = XCTL-127 OR XCTL-132
001379         MOVE SAVE-COMPANY-CD        TO PI-COMPANY-CD
001380         MOVE SAVE-COMPANY-ID        TO PI-COMPANY-ID
001381         MOVE SAVE-JOURNAL-FILE-ID   TO PI-JOURNAL-FILE-ID
001382         MOVE SAVE-CREDIT-USER       TO PI-CREDIT-USER
001383         MOVE SAVE-CLAIM-USER        TO PI-CLAIM-USER
001384         MOVE SAVE-LIFE-OVERRIDE-L1  TO PI-LIFE-OVERRIDE-L1
001385         MOVE SAVE-LIFE-OVERRIDE-L2  TO PI-LIFE-OVERRIDE-L2
001386         MOVE SAVE-LIFE-OVERRIDE-L6  TO PI-LIFE-OVERRIDE-L6
001387         MOVE SAVE-LIFE-OVERRIDE-L12 TO PI-LIFE-OVERRIDE-L12
001388         MOVE SAVE-AH-OVERRIDE-L1    TO PI-AH-OVERRIDE-L1
001389         MOVE SAVE-AH-OVERRIDE-L2    TO PI-AH-OVERRIDE-L2
001390         MOVE SAVE-AH-OVERRIDE-L6    TO PI-AH-OVERRIDE-L6
001391         MOVE SAVE-AH-OVERRIDE-L12   TO PI-AH-OVERRIDE-L12
001392         MOVE SAVE-CERT-ACCESS-CONTROL
001393                                    TO PI-CERT-ACCESS-CONTROL
001394         MOVE SAVE-CARRIER-CONTROL-LEVEL
001395                                    TO PI-CARRIER-CONTROL-LEVEL
001396     END-IF.
001397
001398*    IF EIBAID = DFHPF11
001399*       move errmsg1i            to emi-line1
001400*       move errmsg2i            to emi-line2
001401*       move pi-emi-sub          to emi-sub
001402*       go to 8200-SEND-DATAONLY
001403*    end-if
001404     IF RETURNED-FROM = XCTL-127
001405         MOVE SAVE-EL127-TO-EL130-CNTRL
001406                                 TO PI-EL127-TO-EL130-CNTRL
001407     END-IF.
001408
001409     IF RETURNED-FROM = XCTL-6592
001410        MOVE SAVE-EL659-TO-EL130-CNTRL
001411                                 TO PI-EL659-TO-EL130-CNTRL
001412     END-IF.
001413
001414     IF RETURNED-FROM = XCTL-114
001415        IF SAVE-BENEFICIARY NOT = SPACES AND LOW-VALUES
001416           MOVE SAVE-BENEFICIARY TO BENECDI
001417           MOVE +10              TO BENECDL
001418           MOVE -1               TO BIRTHDTL
001419        END-IF
001420     END-IF.
001421
001422     PERFORM 3993-MODIFY-SCREEN-ATTRB THRU 3993-EXIT.
001423
001424     IF MAINTL NOT = ZERO
001425         MOVE AL-UANON           TO MAINTA
001426     END-IF.
001427
001428     IF ENTERPFL NOT = ZERO
001429         MOVE AL-UNNON           TO ENTERPFA
001430     END-IF.
001431
001432     IF RETURNED-FROM = XCTL-127
001433         GO TO 0650-FROM-EL127
001434     END-IF.
001435
001436     IF RETURNED-FROM = XCTL-132
001437         GO TO 0640-FROM-EL132
001438     END-IF.
001439
001440     IF RETURNED-FROM = XCTL-650
001441         MOVE AL-SABON           TO PF5A
001442         PERFORM 0690-HIGHLIGHT-CERTS THRU 0690-EXIT
001443     END-IF.
001444
001445     IF RETURNED-FROM = XCTL-6592
001446         GO TO 0630-FROM-EL6592
001447     END-IF.
001448
001449     IF MAINTI = 'S' OR 'I'
001450        GO TO 1000-SHOW-CLAIM
001451     END-IF.
001452
001453     GO TO 8100-SEND-INITIAL-MAP.
001454
001455     EJECT
001456 0630-FROM-EL6592.
001457     MOVE PI-EL659-CARRIER       TO CRTCARRO.
001458     MOVE PI-EL659-GROUPING      TO GROUPO.
001459     MOVE PI-EL659-STATE         TO STATEO.
001460     MOVE PI-EL659-ACCOUNT       TO ACCOUNTO.
001461
001462     MOVE AL-UANON               TO CRTCARRA
001463                                    GROUPA
001464                                    STATEA
001465                                    ACCOUNTA.
001466     GO TO 8100-SEND-INITIAL-MAP.
001467
001468 0640-FROM-EL132.
001469     IF PI-CLAIM-NO = SPACES
001470         GO TO 0645-NO-CLAIM-SELECTED
001471     END-IF.
001472
001473     MOVE LOW-VALUES             TO EL130AI.
001474     MOVE 'MSTR'                 TO FILE-SWITCH.
001475     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
001476     MOVE PI-CARRIER             TO MSTR-CARRIER.
001477     MOVE PI-CLAIM-NO            TO MSTR-CLAIM-NO
001478                                    CLMNOO.
001479     MOVE AL-UANON               TO CLMNOA.
001480     MOVE PI-CERT-NO             TO MSTR-CERT-NO.
001481
001482
001483     
      * EXEC CICS HANDLE CONDITION
001484*        NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
001485*    END-EXEC.
      *    MOVE '"$I                   ! & #00007972' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303037393732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001486
001487     PERFORM 7950-READ-CLAIM THRU 7950-EXIT.
001488     MOVE 'X'                    TO CLAIM-SWITCH.
001489     PERFORM 7910-READ-NINETY THRU 7910-EXIT.
001490     MOVE AT-INFO-LINE-1         TO WS-DIAGNOSIS-DESCRIPT.
001491     MOVE AT-ICD-CODE-1          TO WS-ICD-CODE-1.
001492     MOVE AT-ICD-CODE-2          TO WS-ICD-CODE-2.
001493     PERFORM 7910-READ-ACTV THRU 7910-EXIT.
001494     PERFORM 7000-BUILD-OUTPUT-MAP THRU 7099-EXIT.
001495
001496     MOVE ER-0213                TO EMI-ERROR.
001497     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001498     MOVE PI-CLAIM-NO            TO EMI-TEXT-VARIABLE (1).
001499     GO TO 8100-SEND-INITIAL-MAP.
001500
001501 0645-NO-CLAIM-SELECTED.
001502
001503     IF MAINTI = 'S' OR 'I'
001504        GO TO 1000-SHOW-CLAIM
001505     END-IF.
001506
001507     GO TO 8100-SEND-INITIAL-MAP.
001508
001509 0650-FROM-EL127.
001510
001511     IF PI-CERT-SELECT-CNT GREATER THAN +6
001512         MOVE ZEROS              TO PI-CERT-SELECT-CNT
001513                                    PI-CERT-PROCESSED
001514     END-IF.
001515
001516     IF PI-RETURN-TO-PROGRAM = XCTL-727
001517         MOVE +1                 TO PI-CERT-SELECT-CNT
001518                                    PI-CERT-PROCESSED
001519         MOVE PI-CARRIER         TO PI-EL127-CARRIER (1)
001520         MOVE PI-GROUPING        TO PI-EL127-GROUPING (1)
001521         MOVE PI-STATE           TO PI-EL127-STATE   (1)
001522         MOVE PI-ACCOUNT         TO PI-EL127-ACCOUNT (1)
001523         MOVE PI-CERT-EFF-DT     TO PI-EL127-EFF-DT  (1)
001524         MOVE PI-CERT-NO         TO PI-EL127-CERT-NO (1)
001525     END-IF.
001526
001527     IF PI-CERT-SELECT-CNT = ZERO
001528         IF CERTNOI NOT = SPACES AND LOW-VALUES
001529             MOVE CERTNOI        TO PI-CERT-NO
001530             MOVE SUFXI          TO PI-CERT-SFX
001531             GO TO 3000-READ-CERT
001532         ELSE
001533             GO TO 8100-SEND-INITIAL-MAP
001534         END-IF
001535     END-IF.
001536
001537     MOVE AL-SABON               TO PF5A.
001538
001539     IF PI-CERT-PROCESSED = 1
001540         PERFORM 0680-PLUG-SELECTED-CERTS THRU 0680-EXIT
001541     END-IF.
001542
001543     PERFORM 0690-HIGHLIGHT-CERTS    THRU 0690-EXIT.
001544     PERFORM 3993-MODIFY-CLAIM-ATTRB THRU 3993-EXIT.
001545
001546     MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
001547     MOVE PI-EL127-CARRIER (PI-CERT-PROCESSED)
001548                                 TO CERT-CARRIER
001549                                    PI-CARRIER.
001550     MOVE PI-EL127-GROUPING(PI-CERT-PROCESSED)
001551                                 TO CERT-GROUPING
001552                                    PI-GROUPING.
001553     MOVE PI-EL127-STATE   (PI-CERT-PROCESSED)
001554                                 TO CERT-STATE
001555                                    PI-STATE.
001556     MOVE PI-EL127-ACCOUNT (PI-CERT-PROCESSED)
001557                                 TO CERT-ACCOUNT
001558                                    PI-ACCOUNT.
001559     MOVE PI-EL127-EFF-DT  (PI-CERT-PROCESSED)
001560                                 TO CERT-EFF-DT
001561                                    PI-CERT-EFF-DT.
001562     MOVE PI-EL127-CERT-NO (PI-CERT-PROCESSED)
001563                                 TO CERT-CERT-NO
001564                                    PI-CERT-NO.
001565
001566     MOVE 'CERT'                 TO FILE-SWITCH.
001567     PERFORM 7940-READ-CERT THRU 7940-EXIT.
001568
001569     MOVE CERT-CERT-NO           TO PI-SAVE-CERT.
001570     MOVE CERT-ACCOUNT           TO PI-SAVE-ACCOUNT.
001571     MOVE CERT-EFF-DT            TO PI-SAVE-EFFDT.
001572
001573     IF CM-CLAIM-ATTACHED-COUNT NOT = ZERO
001574         MOVE PI-COMPANY-CD      TO MSTR5-COMP-CD
001575         MOVE CERT-CERT-NO       TO MSTR5-CERT-NO
001576         PERFORM 7500-BROWSE-FOR-DUPLICATE THRU 7500-EXIT
001577         IF INCURRED-DATE-MATCH
001578             MOVE ER-7352        TO EMI-ERROR
001579             MOVE -1             TO CERTNOL
001580             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001581         ELSE
001582             IF NO-CLAIMS-FOR-CERT
001583                 MOVE ER-7353    TO EMI-ERROR
001584                 MOVE -1         TO CERTNOL
001585                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001586             ELSE
001587                 MOVE ER-0558    TO EMI-ERROR
001588                 MOVE -1         TO CERTNOL
001589                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001590             END-IF
001591         END-IF
001592     END-IF.
001593
001594     MOVE SPACES                 TO PI-PRT-OPT
001595                                    PI-ALT-PRT.
001596
001597     PERFORM 7000-BUILD-OUTPUT-MAP THRU 7099-EXIT.
001598
001599     if (ws-lf-joint-indicator not = 'J')
001600        and (ws-ah-joint-indicator not = 'J')
001601        move 'P'                 to INSTYPEO
001602        MOVE AL-UANON            TO INSTYPEA
001603     end-if
001604
001605     IF LSTNMEL NOT GREATER ZERO
001606        AND CRTLNMEI NOT = (SPACES AND LOW-VALUES)
001607         MOVE CRTLNMEI           TO  LSTNMEO
001608         MOVE CRTFNMEI           TO  FSTNMEO
001609         MOVE CRTINITI           TO  INITO
001610         MOVE AL-UANON           TO  LSTNMEA
001611                                     FSTNMEA
001612                                     INITA
001613     END-IF.
001614
001615     IF SSNL GREATER ZERO
001616         GO TO 0655-SKIP-SOCIAL-SECURITY
001617     END-IF.
001618
001619     IF CRTSSNI = SPACES OR LOW-VALUES
001620         GO TO 0655-SKIP-SOCIAL-SECURITY
001621     END-IF.
001622
001623     IF CM-SSN-STATE   = CM-STATE AND
001624        CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME
001625            GO TO 0655-SKIP-SOCIAL-SECURITY
001626     END-IF.
001627
001628     MOVE CRTSSNI                TO SSNO
001629     MOVE AL-UANON               TO SSNA.
001630
001631 0655-SKIP-SOCIAL-SECURITY.
001632
001633     IF CLMCARRL NOT = ZERO  AND
001634        CLMCARRI NOT = CRTCARRI
001635        MOVE ER-0562             TO EMI-ERROR
001636        MOVE AL-UABON            TO CLMCARRA
001637        MOVE -1                  TO CLMCARRL
001638        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001639     END-IF.
001640
001641     IF CLMCARRL = ZERO
001642        MOVE AL-UANON            TO CLMCARRA
001643        MOVE CRTCARRI            TO CLMCARRI
001644     END-IF.
001645
001646     MOVE 'A'                    TO MAINTO.
001647     MOVE AL-UANON               TO MAINTA.
001648
001649     IF PI-RETURN-TO-PROGRAM = XCTL-127 OR XCTL-727
001650        GO TO 8100-SEND-INITIAL-MAP
001651     ELSE
001652        GO TO 8150-SEND-MAP-CURSOR
001653     END-IF.
001654
001655 0660-NOT-FOUND.
001656     MOVE ER-0214                TO EMI-ERROR.
001657     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001658     GO TO 8100-SEND-INITIAL-MAP.
001659
001660 0670-QIDERR.
001661     MOVE ER-0033                TO EMI-ERROR.
001662     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001663     GO TO 8100-SEND-INITIAL-MAP.
001664
001665     EJECT
001666 0680-PLUG-SELECTED-CERTS.
001667
001668     MOVE AL-SANON               TO BCERT1A
001669                                    BSUFX1A
001670                                    BCERT2A
001671                                    BSUFX2A
001672                                    BCERT3A
001673                                    BSUFX3A
001674                                    BCERT4A
001675                                    BSUFX4A
001676                                    BCERT5A
001677                                    BSUFX5A.
001678
001679     IF PCERTNOO = SPACES OR LOW-VALUES
001680         MOVE PI-EL127-CERT-PRIME (1)
001681                                 TO PCERTNOO
001682         MOVE PI-EL127-CERT-SUFX  (1)
001683                                 TO PSUFXO
001684     END-IF.
001685
001686     MOVE PI-EL127-CERT-PRIME (1)
001687                                 TO BCERT1O.
001688     MOVE PI-EL127-CERT-SUFX  (1)
001689                                 TO BSUFX1O.
001690
001691     IF PI-CERT-SELECT-CNT GREATER THAN 1
001692         MOVE PI-EL127-CERT-PRIME (2)
001693                                 TO BCERT2O
001694         MOVE PI-EL127-CERT-SUFX  (2)
001695                                 TO BSUFX2O
001696     ELSE
001697         MOVE LOW-VALUES         TO BCERT2O
001698                                    BSUFX2O
001699     END-IF.
001700
001701     IF PI-CERT-SELECT-CNT GREATER THAN 2
001702         MOVE PI-EL127-CERT-PRIME (3)
001703                                 TO BCERT3O
001704         MOVE PI-EL127-CERT-SUFX  (3)
001705                                 TO BSUFX3O
001706     ELSE
001707         MOVE LOW-VALUES         TO BCERT3O
001708                                    BSUFX3O
001709     END-IF.
001710
001711     IF PI-CERT-SELECT-CNT GREATER THAN 3
001712         MOVE PI-EL127-CERT-PRIME (4)
001713                                 TO BCERT4O
001714         MOVE PI-EL127-CERT-SUFX  (4)
001715                                 TO BSUFX4O
001716     ELSE
001717         MOVE LOW-VALUES         TO BCERT4O
001718                                    BSUFX4O
001719     END-IF.
001720
001721     IF PI-CERT-SELECT-CNT GREATER THAN 4
001722         MOVE PI-EL127-CERT-PRIME (5)
001723                                 TO BCERT5O
001724         MOVE PI-EL127-CERT-SUFX  (5)
001725                                 TO BSUFX5O
001726     ELSE
001727         MOVE LOW-VALUES         TO BCERT5O
001728                                    BSUFX5O
001729     END-IF.
001730
001731 0680-EXIT.
001732     EXIT.
001733
001734     EJECT
001735 0690-HIGHLIGHT-CERTS.
001736
001737     MOVE AL-UANON               TO PCERTNOA
001738                                    PSUFXA.
001739
001740     IF BCERT1O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
001741        BSUFX1O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
001742            MOVE AL-SABON        TO BCERT1A
001743                                    BSUFX1A
001744     ELSE
001745         MOVE AL-SANON           TO BCERT1A
001746                                    BSUFX1A
001747     END-IF.
001748
001749     IF BCERT2O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
001750        BSUFX2O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
001751            MOVE AL-SABON        TO BCERT2A
001752                                    BSUFX2A
001753     ELSE
001754         MOVE AL-SANON           TO BCERT2A
001755                                    BSUFX2A
001756     END-IF.
001757
001758     IF BCERT3O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
001759        BSUFX3O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
001760            MOVE AL-SABON        TO BCERT3A
001761                                    BSUFX3A
001762     ELSE
001763         MOVE AL-SANON           TO BCERT3A
001764                                    BSUFX3A
001765     END-IF.
001766
001767     IF BCERT4O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
001768        BSUFX4O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
001769            MOVE AL-SABON        TO BCERT4A
001770                                    BSUFX4A
001771     ELSE
001772         MOVE AL-SANON           TO BCERT4A
001773                                    BSUFX4A
001774     END-IF.
001775
001776     IF BCERT5O = PI-EL127-CERT-PRIME (PI-CERT-PROCESSED) AND
001777        BSUFX5O = PI-EL127-CERT-SUFX  (PI-CERT-PROCESSED)
001778            MOVE AL-SABON        TO BCERT5A
001779                                    BSUFX5A
001780     ELSE
001781         MOVE AL-SANON           TO BCERT5A
001782                                    BSUFX5A
001783     END-IF.
001784
001785 0690-EXIT.
001786     EXIT.
001787
001788     EJECT
001789 0800-TEST-PREVIOUS-CONTROL.
001790     IF CLMNOI   = PI-LAST-CLAIM       AND
001791        CLMCARRI = PI-LAST-CARR        AND
001792        CERTNOI  = PI-LAST-CERT-PRIME  AND
001793        SUFXI    = PI-LAST-CERT-SUFX
001794*        NEXT SENTENCE
001795         CONTINUE
001796     ELSE
001797         MOVE ER-0207            TO EMI-ERROR
001798         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001799         MOVE -1                 TO MAINTL
001800         GO TO 8200-SEND-DATAONLY
001801     END-IF.
001802
001803     PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT.
001804
001805     MOVE PI-LAST-CLAIM          TO PI-CLAIM-NO.
001806     MOVE PI-LAST-CARR           TO PI-CARRIER.
001807     MOVE PI-LAST-CERT           TO PI-CERT-NO.
001808
001809     IF EIBAID = DFHPF7
001810         MOVE XCTL-157           TO PGM-NAME
001811     END-IF.
001812
001813     IF EIBAID = DFHPF8
001814         MOVE XCTL-141           TO PGM-NAME
001815     END-IF.
001816
001817     IF EIBAID = DFHPF9
001818        MOVE SPACES              TO PI-SAVED-PROGRAM-1
001819                                    PI-SAVED-PROGRAM-2
001820                                    PI-SAVED-PROGRAM-3
001821                                    PI-SAVED-PROGRAM-4
001822                                    PI-SAVED-PROGRAM-5
001823                                    PI-SAVED-PROGRAM-6
001824        MOVE XCTL-126            TO PI-RETURN-TO-PROGRAM
001825        MOVE XCTL-132            TO PI-CALLING-PROGRAM
001826        MOVE XCTL-150            TO PGM-NAME
001827     END-IF.
001828
001829     GO TO 9300-XCTL.
001830
001831     EJECT
001832 1000-SHOW-CLAIM.
001833
001834     IF CLMNOL = ZERO
001835         MOVE -1                 TO CLMNOL
001836         MOVE AL-UABON           TO CLMNOA
001837         MOVE ER-0202            TO EMI-ERROR
001838         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001839     ELSE
001840         MOVE AL-UANON           TO CLMNOA
001841     END-IF.
001842
001843     IF PI-NO-CARRIER-SECURITY
001844        IF CLMCARRL = ZERO
001845             MOVE -1             TO CLMCARRL
001846             MOVE AL-UABON       TO CLMCARRA
001847             MOVE ER-0194        TO EMI-ERROR
001848             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001849         ELSE
001850             MOVE AL-UANON       TO CLMCARRA
001851         END-IF
001852     END-IF.
001853
001854     IF SUFXL = ZERO
001855         MOVE SPACES             TO SUFXI
001856         MOVE AL-UANON           TO SUFXA
001857     ELSE
001858         MOVE AL-UANON           TO SUFXA
001859     END-IF.
001860
001861     IF NOT EMI-NO-ERRORS
001862         GO TO 1050-SEND-CHECK
001863     END-IF.
001864
001865
001866     
      * EXEC CICS HANDLE CONDITION
001867*        NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
001868*    END-EXEC.
      *    MOVE '"$I                   ! '' #00008355' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303038333535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001869
001870     MOVE 'MSTR'                 TO FILE-SWITCH
001871     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
001872
001873     MOVE CLMCARRI               TO MSTR-CARRIER.
001874     MOVE CLMNOI                 TO MSTR-CLAIM-NO.
001875
001876     IF CERTNOL = ZERO     OR
001877        CERTNOI = SPACES   OR
001878        CERTNOI = LOW-VALUES
001879         PERFORM 7600-BROWSE-CLAIM THRU 7699-EXIT
001880         MOVE AL-SANON        TO CERTNOA
001881                                 SUFXA
001882     ELSE
001883         PERFORM 7610-BROWSE-CLAIM-LOOP THRU 7699-EXIT
001884     END-IF.
001885
001886     MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
001887     MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
001888
001889
001890     
      * EXEC CICS HANDLE CONDITION
001891*        NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
001892*    END-EXEC.
      *    MOVE '"$I                   ! ( #00008379' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303038333739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001893
001894     PERFORM 7950-READ-CLAIM THRU 7950-EXIT.
001895
001896     IF NOT PI-NO-ACCOUNT-SECURITY
001897         IF PI-ACCOUNT-SECURITY NOT = CL-CERT-ACCOUNT
001898           MOVE ER-2371          TO EMI-ERROR
001899           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001900           MOVE -1               TO MAINTL
001901           GO TO 8200-SEND-DATAONLY
001902         END-IF
001903     END-IF.
001904
001905
001906     
      * EXEC CICS HANDLE CONDITION
001907*        NOTFND   (1025-SHOW-RECORD-NOT-FOUND)
001908*    END-EXEC.
      *    MOVE '"$I                   ! ) #00008395' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303038333935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001909
001910     MOVE 'Y'                    TO CLAIM-SWITCH.
001911     PERFORM 7910-READ-NINETY THRU 7910-EXIT.
001912     MOVE AT-INFO-LINE-1         TO WS-DIAGNOSIS-DESCRIPT.
001913     MOVE AT-ICD-CODE-1          TO WS-ICD-CODE-1.
001914     MOVE AT-ICD-CODE-2          TO WS-ICD-CODE-2.
001915     PERFORM 7910-READ-ACTV THRU 7910-EXIT.
001916
001917 1000-SHOW-END-OF-PERFORM.
001918
001919     MOVE MSTR-COMP-CD           TO CERT-COMP-CD.
001920     MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
001921     MOVE CL-CERT-GROUPING       TO CERT-GROUPING.
001922     MOVE CL-CERT-STATE          TO CERT-STATE.
001923     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
001924     MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
001925     MOVE CL-CERT-NO             TO CERT-CERT-NO.
001926
001927     MOVE 'CERT'                 TO FILE-SWITCH.
001928
001929     PERFORM 7940-READ-CERT THRU 7940-EXIT.
001930
001931     MOVE CL-LAST-MAINT-DT       TO PI-MSTR-DT.
001932     MOVE CL-LAST-MAINT-HHMMSS   TO PI-MSTR-WHEN.
001933     MOVE AT-RECORDED-DT         TO PI-TRLR-DT.
001934     MOVE AT-LAST-MAINT-HHMMSS   TO PI-TRLR-WHEN.
001935     MOVE CLMNOI                 TO PI-LAST-CLAIM.
001936     MOVE CERTNOI                TO PI-LAST-CERT-PRIME.
001937     MOVE SUFXI                  TO PI-LAST-CERT-SUFX.
001938     MOVE CLMCARRI               TO PI-LAST-CARR.
001939
001940     PERFORM 7000-BUILD-OUTPUT-MAP THRU 7099-EXIT.
001941
001942     IF MAINTI = 'I'
001943         MOVE AL-SANOF TO  FSTNMEA   INITA     SEXA
001944                 BIRTHDTA  LSTNMEA   RELCLMA
001945                 LOANNOA   LOANBALA  BENECDA
001946         MOVE AL-SANON TO  SSNA
001947         MOVE SPACES   TO  INCURI    REPORTI
001948         MOVE 'Y'      TO PI-INCURR-SW
001949         MOVE ER-0522  TO EMI-ERROR
001950         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001951     ELSE
001952         MOVE ' '      TO PI-INCURR-SW
001953     END-IF.
001954
001955     MOVE -1                     TO MAINTL.
001956     MOVE MSTR-CARRIER           TO PI-CARRIER.
001957     MOVE CERT-GROUPING          TO PI-GROUPING.
001958     MOVE CERT-STATE             TO PI-STATE.
001959     MOVE CERT-ACCOUNT           TO PI-ACCOUNT.
001960     MOVE MSTR-CLAIM-NO          TO PI-CLAIM-NO.
001961     MOVE MSTR-CERT-NO           TO PI-CERT-NO.
001962     MOVE CERT-EFF-DT            TO PI-CERT-EFF-DT.
001963
001964     IF RETURNED-FROM NOT = SPACES
001965         GO TO 8100-SEND-INITIAL-MAP
001966     ELSE
001967         GO TO 8200-SEND-DATAONLY
001968     END-IF.
001969
001970 1025-SHOW-RECORD-NOT-FOUND.
001971
001972     IF FILE-SWITCH = 'MSTR'
001973         MOVE ER-0204            TO EMI-ERROR
001974     ELSE
001975         IF FILE-SWITCH = 'TRLR'
001976             MOVE ER-0205        TO EMI-ERROR
001977         ELSE
001978             MOVE ER-0206        TO EMI-ERROR
001979         END-IF
001980     END-IF.
001981
001982     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001983
001984 1050-SEND-CHECK.
001985
001986     IF RETURNED-FROM NOT = SPACES
001987         GO TO 8100-SEND-INITIAL-MAP
001988     ELSE
001989         MOVE -1                 TO MAINTL
001990         GO TO 8200-SEND-DATAONLY
001991     END-IF.
001992
001993 2000-DELETE-CLAIM.
001994     IF NOT MODIFY-CAP
001995         MOVE ER-0070            TO EMI-ERROR
001996         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001997         MOVE -1                 TO MAINTL
001998         GO TO 8200-SEND-DATAONLY
001999     END-IF.
002000
002001     IF CLMNOI   NOT = PI-LAST-CLAIM       OR
002002        CLMCARRI NOT = PI-LAST-CARR        OR
002003        CERTNOI  NOT = PI-LAST-CERT-PRIME  OR
002004        SUFXI    NOT = PI-LAST-CERT-SUFX
002005         MOVE ER-0207            TO EMI-ERROR
002006         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002007         MOVE -1                 TO MAINTL
002008         GO TO 8200-SEND-DATAONLY
002009     END-IF.
002010
002011
002012     
      * EXEC CICS HANDLE CONDITION
002013*        NOTFND   (2050-DELETE-NOT-FOUND)
002014*    END-EXEC.
      *    MOVE '"$I                   ! * #00008501' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303038353031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002015
002016     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
002017
002018     MOVE CLMCARRI               TO MSTR-CARRIER.
002019     MOVE CLMNOI                 TO MSTR-CLAIM-NO.
002020     MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
002021     MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
002022
002023     MOVE 'MSTR'                 TO FILE-SWITCH.
002024     PERFORM 7920-READ-CLAIM-UPDATE THRU 7920-EXIT.
002025
002026     MOVE 'Y'                    TO CLAIM-SWITCH.
002027
002028     IF PI-MSTR-DT   NOT = CL-LAST-MAINT-DT  OR
002029        PI-MSTR-WHEN NOT = CL-LAST-MAINT-HHMMSS
002030         MOVE ER-0068        TO EMI-ERROR
002031         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002032         MOVE -1             TO MAINTL
002033         GO TO 8200-SEND-DATAONLY
002034     END-IF.
002035
002036     IF CL-TRAILER-SEQ-CNT NOT = 4095
002037         MOVE ER-0569            TO EMI-ERROR
002038         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002039         MOVE -1                 TO MAINTL
002040         GO TO 8200-SEND-DATAONLY
002041     END-IF.
002042
002043     IF CL-CLAIM-PAYMENT-STATUS NOT NUMERIC
002044         MOVE ZEROS              TO CL-CLAIM-PAYMENT-STATUS
002045     END-IF.
002046
002047     IF CL-PURGED-DT NOT = LOW-VALUES
002048         MOVE ER-7691            TO EMI-ERROR
002049         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002050         MOVE -1                 TO MAINTL
002051         GO TO 8200-SEND-DATAONLY
002052     END-IF.
002053
002054     MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
002055     MOVE MSTR-CARRIER           TO TRLR-CARRIER.
002056     MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
002057     MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
002058     MOVE +0                     TO TRLR-SEQ-NO.
002059     MOVE '1'                    TO TRLR-TYPE.
002060
002061     MOVE 'TRLR'                 TO FILE-SWITCH.
002062
002063
002064     
      * EXEC CICS READ
002065*        UPDATE
002066*        DATASET  (ELTRLR-DSID)
002067*        SET      (ADDRESS OF ACTIVITY-TRAILERS)
002068*        RIDFLD   (ELTRLR-KEY)
002069*    END-EXEC.
      *    MOVE '&"S        EU         (   #00008553' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038353533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002070
002071     IF PI-TRLR-DT   NOT = AT-RECORDED-DT  OR
002072        PI-TRLR-WHEN NOT = AT-LAST-MAINT-HHMMSS
002073         MOVE ER-0068        TO EMI-ERROR
002074         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002075         MOVE -1             TO MAINTL
002076         GO TO 8200-SEND-DATAONLY
002077     END-IF.
002078
002079
002080     
      * EXEC CICS DELETE
002081*        DATASET   (ELTRLR-DSID)
002082*    END-EXEC.
      *    MOVE '&(                    &   #00008569' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303038353639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002083
002084     MOVE +90                    TO TRLR-SEQ-NO.
002085     MOVE '6'                    TO TRLR-TYPE.
002086
002087
002088     
      * EXEC CICS READ
002089*        UPDATE
002090*        DATASET  (ELTRLR-DSID)
002091*        SET      (ADDRESS OF ACTIVITY-TRAILERS)
002092*        RIDFLD   (ELTRLR-KEY)
002093*    END-EXEC.
      *    MOVE '&"S        EU         (   #00008577' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038353737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002094
002095
002096     
      * EXEC CICS DELETE
002097*        DATASET   (ELTRLR-DSID)
002098*    END-EXEC.
      *    MOVE '&(                    &   #00008585' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303038353835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002099
002100     MOVE +91                    TO TRLR-SEQ-NO.
002101     MOVE '6'                    TO TRLR-TYPE.
002102
002103
002104     
      * EXEC CICS READ
002105*        UPDATE
002106*        DATASET  (ELTRLR-DSID)
002107*        SET      (ADDRESS OF ACTIVITY-TRAILERS)
002108*        RIDFLD   (ELTRLR-KEY)
002109*        RESP     (WS-RESPONSE)
002110*    END-EXEC
      *    MOVE '&"S        EU         (  N#00008593' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303038353933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
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
002111
002112     IF WS-RESP-NORMAL
002113        
      * EXEC CICS DELETE
002114*          DATASET   (ELTRLR-DSID)
002115*       END-EXEC
      *    MOVE '&(                    &   #00008602' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303038363032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002116     END-IF.
002117
002118     MOVE MSTR-COMP-CD           TO CERT-COMP-CD.
002119     MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
002120     MOVE CL-CERT-GROUPING       TO CERT-GROUPING.
002121     MOVE CL-CERT-STATE          TO CERT-STATE.
002122     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
002123     MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
002124     MOVE CL-CERT-NO             TO CERT-CERT-NO.
002125
002126     MOVE 'CERT'                 TO FILE-SWITCH.
002127
002128     PERFORM 7930-READ-CERT-UPDATE THRU 7930-EXIT.
002129
002130     SUBTRACT +1 FROM CM-CLAIM-ATTACHED-COUNT GIVING WS-NUM-HOLD.
002131
002132     IF WS-NUM-HOLD NOT = +0
002133         GO TO 2005-REWRITE-CERT
002134     END-IF.
002135
002136     IF NOT CERT-WAS-CREATED
002137         MOVE SPACE              TO CM-CLAIM-INTERFACE-SW
002138         GO TO 2005-REWRITE-CERT
002139     END-IF.
002140
002141
002142     
      * EXEC CICS DELETE
002143*        DATASET   (ELCERT-DSID)
002144*    END-EXEC.
      *    MOVE '&(                    &   #00008631' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303038363331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002145
002146     GO TO 2010-DELETE-CL.
002147
002148 2005-REWRITE-CERT.
002149
002150     SUBTRACT +1 FROM CM-CLAIM-ATTACHED-COUNT.
002151
002152
002153     
      * EXEC CICS HANDLE CONDITION
002154*         DUPKEY   (2010-DELETE-CL)
002155*    END-EXEC.
      *    MOVE '"$$                   ! + #00008642' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303038363432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002156
002157
002158     
      * EXEC CICS REWRITE
002159*        DATASET  (ELCERT-DSID)
002160*        FROM     (CERTIFICATE-MASTER)
002161*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008647' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038363437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002162
002163 2010-DELETE-CL.
002164
002165
002166     
      * EXEC CICS DELETE
002167*        DATASET  (ELMSTR-DSID)
002168*    END-EXEC.
      *    MOVE '&(                    &   #00008655' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303038363535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002169
002170     MOVE -1                     TO ONE-OR-MIN1.
002171     PERFORM 7700-CHECK-SEQUENCE THRU 7799-EXIT.
002172
002173     IF WS-ASSOC-CERT-TOTAL NOT = ZERO
002174         MOVE CL-CONTROL-PRIMARY TO ELMSTR-KEY
002175                                    WS-SAVE-CLAIM-KEY
002176         MOVE +1                 TO ONE-OR-MIN1
002177         PERFORM 7710-RESEQUENCE-CLAIMS THRU 7799-EXIT
002178     END-IF.
002179
002180     MOVE ER-0000                TO EMI-ERROR.
002181     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002182     MOVE LOW-VALUES             TO EL130AO.
002183     MOVE -1                     TO MAINTL.
002184     MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.
002185     MOVE ZEROS                  TO PI-CERT-SELECT-CNT
002186                                    PI-CERT-PROCESSED
002187                                    PI-LETTER-ERROR-CODE.
002188     GO TO 8100-SEND-INITIAL-MAP.
002189
002190 2050-DELETE-NOT-FOUND.
002191
002192     IF FILE-SWITCH = 'MSTR'
002193         MOVE -1                 TO CLMNOL
002194         MOVE ER-0204            TO EMI-ERROR
002195     ELSE
002196         IF FILE-SWITCH = 'TRLR'
002197             MOVE -1             TO MAINTL
002198             MOVE ER-0205        TO EMI-ERROR
002199         ELSE
002200             MOVE -1             TO CERTNOL
002201             MOVE ER-0206        TO EMI-ERROR
002202         END-IF
002203     END-IF.
002204
002205     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002206     GO TO 8200-SEND-DATAONLY.
002207
002208 3000-ADD-CLAIM.
002209
002210     IF NOT MODIFY-CAP
002211         MOVE ER-0070            TO EMI-ERROR
002212         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002213         MOVE -1                 TO MAINTL
002214         GO TO 8200-SEND-DATAONLY
002215     END-IF.
002216
002217     PERFORM 6000-EDIT-CLAIM-DATA THRU 6000-EXIT.
002218
002219     IF CERTMTI = 'S'
002220         IF CERTNOI  = PI-SAVE-CERT-PRIME  AND
002221            SUFXI    = PI-SAVE-CERT-SUFX   AND
002222            WS-EFFDT = PI-SAVE-EFFDT       AND
002223            ACCOUNTI = PI-SAVE-ACCOUNT
002224             IF EMI-ERROR = 0
002225                 GO TO 3010-TEST-FOR-ERRORS
002226             ELSE
002227                 MOVE PI-COMPANY-CD    TO  CERT-COMP-CD
002228                 MOVE PI-CARRIER       TO  CERT-CARRIER
002229                 MOVE PI-GROUPING      TO  CERT-GROUPING
002230                 MOVE PI-STATE         TO  CERT-STATE
002231                 MOVE PI-ACCOUNT       TO  CERT-ACCOUNT
002232                 MOVE PI-CERT-EFF-DT   TO  CERT-EFF-DT
002233                 MOVE PI-CERT-NO       TO  CERT-CERT-NO
002234
002235                 
      * EXEC CICS HANDLE CONDITION
002236*                    NOTFND   (3010-TEST-FOR-ERRORS)
002237*                END-EXEC
      *    MOVE '"$I                   ! , #00008724' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303038373234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002238                 PERFORM 7940-READ-CERT THRU 7940-EXIT
002239                 PERFORM 7050-BUILD-MAP-CERT-DATA THRU 7099-EXIT
002240                 GO TO 3010-TEST-FOR-ERRORS
002241             END-IF
002242         END-IF
002243     END-IF.
002244
002245     IF CERTMTI = 'A'
002246         IF NOT PI-NO-CARRIER-SECURITY OR
002247             NOT PI-NO-ACCOUNT-SECURITY
002248             MOVE ER-2370          TO EMI-ERROR
002249             MOVE -1               TO MAINTL
002250             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002251             GO TO 8200-SEND-DATAONLY
002252         ELSE
002253             PERFORM 6300-REQUIRED-CERT-EDIT THRU 6399-EXIT
002254             GO TO 3010-TEST-FOR-ERRORS
002255         END-IF
002256     END-IF.
002257
002258     IF CERTNOL = ZEROS
002259        GO TO 3010-TEST-FOR-ERRORS
002260     END-IF.
002261
002262 3000-READ-CERT.
002263
002264     MOVE PI-COMPANY-CD          TO CERT-COMP-CD-5
002265                                    MSTR5-COMP-CD.
002266     MOVE CERTNOI                TO CERT-CERT-5-PRIME
002267                                    MSTR5-CERT-NO-PRIME.
002268     MOVE SUFXI                  TO CERT-CERT-5-SUFX
002269                                    MSTR5-CERT-NO-SUFX.
002270
002271
002272     
      * EXEC CICS HANDLE CONDITION
002273*         DUPKEY   (3008-DUP-ERROR)
002274*         NOTFND   (3009-NOT-FOUND)
002275*    END-EXEC
      *    MOVE '"$$I                  ! - #00008761' TO DFHEIV0
           MOVE X'222424492020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303038373631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002276
002277     PERFORM 7980-READ-CERT5 THRU 7980-EXIT.
002278
002279     IF NOT PI-NO-ACCOUNT-SECURITY
002280         IF PI-ACCOUNT-SECURITY NOT = CM-ACCOUNT
002281             MOVE ER-2371          TO EMI-ERROR
002282             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002283             MOVE -1               TO MAINTL
002284             GO TO 8200-SEND-DATAONLY
002285         END-IF
002286     END-IF.
002287
002288     MOVE CM-CERT-NO             TO PI-SAVE-CERT.
002289     MOVE CM-ACCOUNT             TO PI-SAVE-ACCOUNT.
002290     MOVE CM-CERT-EFF-DT         TO PI-SAVE-EFFDT.
002291     MOVE CM-COMPANY-CD          TO PI-COMPANY-CD.
002292     MOVE CM-CARRIER             TO PI-CARRIER.
002293     MOVE CM-GROUPING            TO PI-GROUPING.
002294     MOVE CM-STATE               TO PI-STATE.
002295     MOVE CM-ACCOUNT             TO PI-ACCOUNT.
002296     MOVE CM-CERT-EFF-DT         TO PI-CERT-EFF-DT.
002297     MOVE CM-CERT-NO             TO PI-CERT-NO.
002298     PERFORM 7050-BUILD-MAP-CERT-DATA THRU 7099-EXIT.
002299
002300     IF CRTLNMEI NOT = SPACES AND LOW-VALUES
002301         IF LSTNMEO = SPACES OR LOW-VALUES
002302             MOVE CRTLNMEI       TO  LSTNMEO
002303             MOVE AL-UANON       TO  LSTNMEA
002304         END-IF
002305     END-IF.
002306
002307     MOVE 'S'                    TO CERTMTO.
002308*    MOVE AL-UANON               TO CERTMTA  CERTNOA  CLMCARRA
002309     MOVE AL-SANON               TO CERTNOA.
002310     MOVE AL-UANON               TO CERTMTA   CLMCARRA
002311                                 EFFDTA ACCOUNTA STATEA   GROUPA.
002312     IF EMI-NO-ERRORS
002313         MOVE -1                  TO MAINTL
002314     END-IF.
002315
002316     IF CM-CLAIM-ATTACHED-COUNT NOT = ZERO
002317         PERFORM 7500-BROWSE-FOR-DUPLICATE THRU 7500-EXIT
002318         IF INCURRED-DATE-MATCH
002319             MOVE ER-7352         TO EMI-ERROR
002320             MOVE -1              TO CERTNOL
002321             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002322         ELSE
002323             IF NO-CLAIMS-FOR-CERT
002324                 MOVE ER-7353     TO EMI-ERROR
002325                 MOVE -1          TO CERTNOL
002326                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002327             ELSE
002328                 MOVE ER-0558     TO EMI-ERROR
002329                 MOVE -1          TO CERTNOL
002330                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002331                 GO TO 3010-TEST-FOR-ERRORS
002332             END-IF
002333         END-IF
002334     END-IF.
002335
002336 3007-SEND-SCREEN.
002337
002338     IF RETURNED-FROM = XCTL-132 OR XCTL-127
002339         GO TO 8150-SEND-MAP-CURSOR
002340     ELSE
002341         GO TO 8200-SEND-DATAONLY
002342     END-IF.
002343
002344 3008-DUP-ERROR.
002345
002346     MOVE ER-0560                TO EMI-ERROR.
002347     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002348     MOVE -1                     TO CERTNOL.
002349     GO TO 3007-SEND-SCREEN.
002350
002351 3009-NOT-FOUND.
002352
002353     MOVE ER-0214                TO EMI-ERROR.
002354     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002355     MOVE -1                     TO CERTNOL.
002356     GO TO 3007-SEND-SCREEN.
002357
002358 3010-TEST-FOR-ERRORS.
002359
002360     PERFORM 6200-EDIT-CERT-DATA           THRU 6200-EXIT.
002361     perform 6500-get-acct    thru 6500-exit
002362     PERFORM 6400-TEST-CLAIM-REASONABILITY THRU 6499-EXIT.
002363
002364*     perform 6500-get-acct    thru 6500-exit
002365
002366     MOVE ZEROS                  TO WS-MONTHS-BETWEEN
002367     move +0                     to e1
002368     move spaces                 to ws-dcc-error-line
002369     move ' '                    to ws-zero-bens-avail
002370
002371     if ws-dcc-product-code not = spaces
002372        PERFORM 3997-GET-ERPDEF  THRU 3997-EXIT
002373        IF ERPDEF-FOUND
002374           MOVE CM-CERT-EFF-DT   TO DC-BIN-DATE-1
002375           MOVE WS-INCUR         TO DC-BIN-DATE-2
002376           MOVE '1'              TO DC-OPTION-CODE
002377           MOVE +0               TO DC-ELAPSED-MONTHS
002378                                    DC-ELAPSED-DAYS
002379           PERFORM 9700-LINK-DATE-CONVERT
002380                                 THRU 9700-EXIT
002381           IF NO-CONVERSION-ERROR
002382              MOVE DC-ELAPSED-MONTHS
002383                                 TO WS-MONTHS-BETWEEN
002384              IF DC-odd-days-over > 1
002385                 ADD 1 TO WS-MONTHS-BETWEEN
002386              END-IF
002387           ELSE
002388              MOVE ZEROS         TO WS-MONTHS-BETWEEN
002389           END-IF
002390
002391           evaluate true
002392              when (ws-excl-period not = zeros)
002393                 and (ws-months-between <= ws-excl-period)
002394                 MOVE -1         TO MAINTL
002395                 add +1          to e1
002396                 move 'Y'        to ws-zero-bens-avail
002397                 MOVE ER-1651    TO EMI-ERROR
002398                                    ws-error-no (e1)
002399                 PERFORM 9900-ERROR-FORMAT
002400                                 THRU 9900-EXIT
002401              when (ws-cov-ends not = zeros)
002402                 and (ws-months-between > ws-cov-ends)
002403                 MOVE -1         TO MAINTL
002404                 add +1          to e1
002405                 move 'Y'        to ws-zero-bens-avail
002406                 MOVE ER-1653    TO EMI-ERROR
002407                                    ws-error-no (e1)
002408                 evaluate true
002409                    when clmtypei = 'L'
002410                       move '  LF  ' to emi-claim-type
002411                    when clmtypei = 'I'
002412                       move '  IU  ' to emi-claim-type
002413                    when clmtypei = 'F'
002414                       move '  FL  ' to emi-claim-type
002415                    when clmtypei = 'H'
002416                       move '  HS  ' to emi-claim-type
002417                    when clmtypei = 'B'
002418                       move '  BR  ' to emi-claim-type
002419                    when other
002420                       move '  AH  ' to emi-claim-type
002421                 end-evaluate
002422                 PERFORM 9900-ERROR-FORMAT
002423                                 THRU 9900-EXIT
002424              when (ws-acc-period not = zeros)
002425                 and (ws-months-between <= ws-acc-period)
002426                 MOVE -1         TO MAINTL
002427                 add +1          to e1
002428                 MOVE ER-1652    TO EMI-ERROR
002429                                    ws-error-no (e1)
002430                 PERFORM 9900-ERROR-FORMAT
002431                                 THRU 9900-EXIT
002432                 add +1          to e1
002433                 move er-1655    to emi-error
002434                                    ws-error-no (e1)
002435                 PERFORM 9900-ERROR-FORMAT
002436                                 THRU 9900-EXIT
002437              when clmtypei = 'I'
002438                 add +1          to e1
002439                 move er-1661    to emi-error
002440                                    ws-error-no (e1)
002441                 PERFORM 9900-ERROR-FORMAT
002442                                 THRU 9900-EXIT
002443           end-evaluate
002444           IF (WS-PRE-EXISTING-PER NOT = ZEROS)
002445              AND (WS-MONTHS-BETWEEN <= WS-PRE-EXISTING-PER)
002446              MOVE -1         TO MAINTL
002447              ADD +1          TO E1
002448              MOVE ER-1677    TO EMI-ERROR
002449                                 WS-ERROR-NO (E1)
002450              PERFORM 9900-ERROR-FORMAT
002451                              THRU 9900-EXIT
002452           END-IF
002453        END-IF
002454*       perform 3996-read-cert-claim-trailer
002455*                                thru 3996-exit
002456     END-IF
002457
002458     perform 3996-read-cert-claim-trailer
002459                                 thru 3996-exit
002460
002461     IF CERTMTI NOT = 'A'
002462         IF CM-CLAIM-ATTACHED-COUNT NOT = ZERO
002463             MOVE PI-COMPANY-CD       TO MSTR5-COMP-CD
002464             MOVE CERTNOI             TO MSTR5-CERT-NO-PRIME
002465             MOVE SUFXI               TO MSTR5-CERT-NO-SUFX
002466             PERFORM 7500-BROWSE-FOR-DUPLICATE THRU 7500-EXIT
002467             IF INCURRED-DATE-MATCH
002468                 MOVE ER-7352         TO EMI-ERROR
002469                 MOVE -1              TO CERTNOL
002470                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002471             ELSE
002472                 IF NO-CLAIMS-FOR-CERT
002473                     MOVE ER-7353     TO EMI-ERROR
002474                     MOVE -1          TO CERTNOL
002475                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002476                 ELSE
002477                     MOVE ER-0558     TO EMI-ERROR
002478                     MOVE -1          TO CERTNOL
002479                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002480                 END-IF
002481             END-IF
002482         END-IF
002483     END-IF.
002484
002485     IF EIBAID = DFHPF6  AND NOT FORCE-CAP
002486         MOVE ER-0433             TO EMI-ERROR
002487         MOVE -1                  TO MAINTL
002488         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002489         GO TO 8200-SEND-DATAONLY
002490     END-IF.
002491
002492     IF (EMI-NO-ERRORS)
002493        OR (ZEROS = EMI-FATAL-CTR AND EMI-FORCABLE-CTR)
002494         GO TO 3015-TRY-TO-BUILD
002495     END-IF.
002496
002497     IF EIBAID = DFHPF6
002498         IF EMI-FATAL-CTR NOT EQUAL ZERO
002499             MOVE ER-0434         TO EMI-ERROR
002500             MOVE -1              TO MAINTL
002501             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002502             GO TO 8200-SEND-DATAONLY
002503         ELSE
002504             MOVE +1              TO INCURL
002505             GO TO 3015-TRY-TO-BUILD
002506         END-IF
002507     END-IF.
002508
002509     IF RETURNED-FROM = XCTL-132 OR XCTL-127
002510         GO TO 8150-SEND-MAP-CURSOR
002511     ELSE
002512         GO TO 8200-SEND-DATAONLY
002513     END-IF.
002514
002515 3015-TRY-TO-BUILD.
002516
002517     IF CLMNOL EQUAL ZERO
002518         GO TO 3030-BUILD-CONT
002519     END-IF.
002520
002521
002522     
      * EXEC CICS HANDLE CONDITION
002523*        NOTFND   (3030-BUILD-CONT)
002524*    END-EXEC.
      *    MOVE '"$I                   ! . #00009011' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303039303131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002525
002526     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
002527
002528     MOVE CLMCARRI               TO MSTR-CARRIER.
002529     MOVE CLMNOI                 TO MSTR-CLAIM-NO.
002530     MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
002531     MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
002532
002533     MOVE 'MSTR'                 TO FILE-SWITCH.
002534
002535     PERFORM 7950-READ-CLAIM THRU 7950-EXIT.
002536
002537     MOVE ER-0221                TO EMI-ERROR.
002538
002539     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002540     MOVE -1                     TO MAINTL.
002541     GO TO 8200-SEND-DATAONLY.
002542
002543 3030-BUILD-CONT.
002544
002545     IF (CERTMTI NOT = 'A')
002546        and (pi-company-id not = 'DCC' and 'VPP')
002547         GO TO 3060-BUILD-CONT
002548     END-IF.
002549
002550
002551     
      * EXEC CICS HANDLE CONDITION
002552*        NOTFND   (3033-ACCT-NOT-FOUND)
002553*        ENDFILE  (3033-ACCT-NOT-FOUND)
002554*    END-EXEC.
      *    MOVE '"$I''                  ! / #00009040' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303039303430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002555
002556     MOVE SPACES                 TO ERACCT-KEY.
002557
002558     IF CARR-GROUP-ST-ACCNT-CNTL
002559         MOVE PI-COMPANY-CD      TO ACCT-COMP-CD
002560         MOVE CRTCARRI           TO ACCT-CARRIER
002561         MOVE GROUPI             TO ACCT-GROUPING
002562         MOVE STATEI             TO ACCT-STATE
002563         MOVE ACCOUNTI           TO ACCT-ACCOUNT
002564         GO TO 3031-MOVE-ACCT-KEY
002565     END-IF.
002566
002567     MOVE PI-COMPANY-CD          TO ACCT-COMP-CD.
002568     MOVE ACCOUNTI               TO ACCT-ACCOUNT.
002569
002570     IF PI-CERT-ACCESS-CONTROL = ' ' OR '2'
002571         MOVE STATEI             TO ACCT-STATE
002572     END-IF.
002573
002574     IF PI-CERT-ACCESS-CONTROL = '2' OR '4'
002575         MOVE CRTCARRI           TO ACCT-CARRIER
002576     END-IF.
002577
002578 3031-MOVE-ACCT-KEY.
002579
002580     MOVE ERACCT-KEY             TO SAVE-ERACCT-KEY.
002581
002582
002583     
      * EXEC CICS STARTBR
002584*        DATASET     (ERACCT2-DSID)
002585*        RIDFLD      (SAVE-ERACCT-KEY)
002586*        GENERIC
002587*        KEYLENGTH   (20)
002588*    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00009072' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'2020233030303039303732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 SAVE-ERACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002589
002590
002591     
      * EXEC CICS HANDLE CONDITION
002592*        ENDFILE   (3032-NOT-IN-DT-RANGE)
002593*    END-EXEC.
      *    MOVE '"$''                   ! 0 #00009080' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303039303830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002594
002595 3031-READ-ACCT-LOOP.
002596
002597     
      * EXEC CICS READNEXT
002598*        DATASET   (ERACCT2-DSID)
002599*        SET       (ADDRESS OF ACCOUNT-MASTER)
002600*        RIDFLD    (SAVE-ERACCT-KEY)
002601*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009086' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303039303836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 SAVE-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002602
002603     IF ACCT-COMP-CD  NOT = AM-COMPANY-CD-A1  OR
002604        ACCT-CARRIER  NOT = AM-VG-CARRIER     OR
002605        ACCT-GROUPING NOT = AM-VG-GROUPING    OR
002606        ACCT-STATE    NOT = AM-VG-STATE       OR
002607        ACCT-ACCOUNT  NOT = AM-VG-ACCOUNT
002608         GO TO 3033-ACCT-NOT-FOUND
002609     END-IF.
002610
002611     IF WS-EFFDT NOT LESS AM-EFFECTIVE-DT  AND
002612        WS-EFFDT LESS AM-EXPIRATION-DT
002613         MOVE PI-COMPANY-CD    TO CERT-COMP-CD
002614         MOVE AM-CARRIER       TO CERT-CARRIER
002615         MOVE AM-GROUPING      TO CERT-GROUPING
002616         MOVE AM-STATE         TO CERT-STATE
002617         MOVE AM-ACCOUNT       TO CERT-ACCOUNT
002618         MOVE WS-EFFDT         TO CERT-EFF-DT
002619         MOVE CERTNOI          TO CERT-CERT-NO-PRIME
002620         MOVE SUFXI            TO CERT-CERT-NO-SUFX
002621         MOVE AM-REI-TABLE     TO WS-REIN-TABLE
002622         GO TO 3034-CHECK-EMPLOYER-STATEMENT
002623     END-IF.
002624
002625     IF WS-EFFDT NOT LESS AM-EXPIRATION-DT
002626         GO TO 3031-READ-ACCT-LOOP
002627     END-IF.
002628
002629 3032-NOT-IN-DT-RANGE.
002630
002631     MOVE ER-0226                TO EMI-ERROR.
002632     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002633     MOVE -1                     TO EFFDTL.
002634     GO TO 8200-SEND-DATAONLY.
002635
002636 3033-ACCT-NOT-FOUND.
002637
002638     MOVE ER-0226 TO EMI-ERROR.
002639     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002640     MOVE -1                     TO ACCOUNTL.
002641     GO TO 8200-SEND-DATAONLY.
002642
002643 3034-CHECK-EMPLOYER-STATEMENT.
002644
002645     IF AM-EMPLOYER-STMT-USED = '1' OR '2' OR '3' OR 'Y'
002646*        NEXT SENTENCE
002647         CONTINUE
002648     ELSE
002649         GO TO  3034-CHECK-ACCOUNT-LIMITS
002650     END-IF.
002651
002652     MOVE CERT-EFF-DT            TO  DC-BIN-DATE-1.
002653     MOVE WS-INCUR               TO  DC-BIN-DATE-2.
002654     MOVE '1'                    TO  DC-OPTION-CODE.
002655     MOVE +0                     TO  DC-ELAPSED-MONTHS
002656                                     DC-ELAPSED-DAYS.
002657     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
002658
002659     IF (AM-EMPLOYER-STMT-USED = '1' AND
002660         DC-ELAPSED-DAYS LESS THAN +31)
002661                      OR
002662        (AM-EMPLOYER-STMT-USED = '2' AND
002663         DC-ELAPSED-DAYS LESS THAN +61)
002664                      OR
002665        (AM-EMPLOYER-STMT-USED = '3' AND
002666         DC-ELAPSED-DAYS LESS THAN +91)
002667            MOVE 'Y'             TO  AM-EMPLOYER-STMT-USED
002668     END-IF.
002669
002670 3034-CHECK-ACCOUNT-LIMITS.
002671
002672     IF CERTMTI NOT = 'A'
002673         GO TO 3060-BUILD-CONT
002674     END-IF.
002675
002676     IF AM-STATUS NOT = '0'
002677         MOVE ER-0225                TO EMI-ERROR
002678         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002679         MOVE -1                     TO ACCOUNTL
002680     END-IF.
002681
002682     IF EIBAID = DFHPF6
002683         IF EMI-FATAL-CTR NOT = ZERO
002684             MOVE ER-0434         TO EMI-ERROR
002685             MOVE -1              TO MAINTL
002686             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002687             GO TO 8200-SEND-DATAONLY
002688         END-IF
002689     END-IF.
002690
002691*    IF PI-COMPANY-ID NOT = 'CRI'
002692*        GO TO 3035-END-BROWSE
002693*    END-IF.
002694
002695 3035-END-BROWSE.
002696
002697     
      * EXEC CICS ENDBR
002698*        DATASET    (ERACCT2-DSID)
002699*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009186' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039313836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002700
002701 3040-CHECK-CERT-ALREADY-ON.
002702
002703     
      * EXEC CICS HANDLE CONDITION
002704*        NOTFND   (3070-BUILD-CONT)
002705*    END-EXEC.
      *    MOVE '"$I                   ! 1 #00009192' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303039313932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002706
002707     PERFORM 7940-READ-CERT THRU 7940-EXIT.
002708
002709     MOVE ER-0229                TO EMI-ERROR.
002710     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002711     MOVE -1                     TO MAINTL.
002712     GO TO 8200-SEND-DATAONLY.
002713
002714 3060-BUILD-CONT.
002715
002716     MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
002717     MOVE CRTCARRI               TO CERT-CARRIER.
002718     MOVE GROUPI                 TO CERT-GROUPING.
002719     MOVE STATEI                 TO CERT-STATE.
002720     MOVE PI-SAVE-ACCOUNT        TO CERT-ACCOUNT.
002721     MOVE CERTNOI                TO CERT-CERT-NO-PRIME.
002722     MOVE SUFXI                  TO CERT-CERT-NO-SUFX.
002723     MOVE PI-SAVE-EFFDT          TO CERT-EFF-DT.
002724
002725
002726     
      * EXEC CICS HANDLE CONDITION
002727*        NOTFND  (3065-CERT-NOT-FOUND)
002728*    END-EXEC.
      *    MOVE '"$I                   ! 2 #00009215' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303039323135' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002729
002730     PERFORM 7930-READ-CERT-UPDATE THRU 7930-EXIT.
002731
002732     GO TO 3070-BUILD-CONT.
002733
002734 3065-CERT-NOT-FOUND.
002735
002736     MOVE ER-0244                TO EMI-ERROR.
002737     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002738     MOVE -1                     TO CERTNOL.
002739     GO TO 8200-SEND-DATAONLY.
002740
002741 3070-BUILD-CONT.
002742
002743     IF CLMNOL NOT = ZERO
002744         GO TO 3200-BUILD-ZERO-TRAILER
002745     END-IF.
002746
002747     MOVE SPACES                 TO ELCNTL-KEY.
002748
002749     IF CONTROL-IS-ACTUAL-CARRIER
002750         MOVE PI-CARRIER         TO CNTL-CARRIER
002751     ELSE
002752         MOVE PI-CARRIER-CONTROL-LEVEL
002753                                 TO CNTL-CARRIER
002754     END-IF.
002755
002756     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
002757     MOVE '6'                    TO CNTL-REC-TYPE.
002758     MOVE +0                     TO CNTL-SEQ-NO.
002759
002760
002761     
      * EXEC CICS HANDLE CONDITION
002762*        NOTFND   (3100-CARRIER-NOT-FOUND)
002763*    END-EXEC.
      *    MOVE '"$I                   ! 3 #00009250' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3320233030303039323530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002764
002765
002766     
      * EXEC CICS READ
002767*        UPDATE
002768*        DATASET   (ELCNTL-DSID)
002769*        SET       (ADDRESS OF CONTROL-FILE)
002770*        RIDFLD    (ELCNTL-KEY)
002771*    END-EXEC.
      *    MOVE '&"S        EU         (   #00009255' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303039323535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002772
002773     IF CLAIM-NO-MANUAL
002774         GO TO 3120-CLAIM-MUST-BE-INPUT
002775     END-IF.
002776
002777     MOVE CF-CLAIM-NO-METHOD     TO SAVE-METHOD.
002778     MOVE CF-CLAIM-COUNTER       TO SAVE-COUNTER.
002779
002780     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' OR 'VPP'
002781           or 'FNL'
002782         CONTINUE
002783     ELSE
002784         GO TO 3080-ASSIGN-LOOP
002785     END-IF.
002786
002787
002788     
      * EXEC CICS UNLOCK
002789*        DATASET   (ELCNTL-DSID)
002790*    END-EXEC.
      *    MOVE '&*                    #   #00009277' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039323737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002791
002792     MOVE '1'                    TO CNTL-REC-TYPE.
002793     MOVE SPACES                 TO CNTL-ACCESS.
002794
002795     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
002796
002797
002798     
      * EXEC CICS HANDLE CONDITION
002799*        NOTFND   (3110-COMPANY-NOT-FOUND)
002800*    END-EXEC.
      *    MOVE '"$I                   ! 4 #00009287' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3420233030303039323837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002801
002802     PERFORM 7960-READ-CNTL-UPDATE THRU 7960-EXIT.
002803
002804     MOVE CF-CO-CLAIM-COUNTER    TO SAVE-COUNTER.
002805
002806 3080-ASSIGN-LOOP.
002807
002808     IF (SAVE-COUNTER = +99999   AND SAVE-METHOD = '2') OR
002809        (SAVE-COUNTER = +9999999 AND SAVE-METHOD = '3')
002810         MOVE +0                 TO SAVE-COUNTER
002811     END-IF.
002812
002813     ADD +1 TO SAVE-COUNTER.
002814
002815     IF SAVE-METHOD  = '3'
002816         MOVE SAVE-COUNTER        TO WS-CLAIM-NUMBER-R1
002817         GO TO 3090-SET-COMP
002818     END-IF.
002819
002820     IF SAVE-METHOD  = '4'
002821         MOVE SAVE-COUNTER       TO WS-CLAIM-NUMBER-R1
002822         MOVE CLMCARRI           TO WS-CN-PRF-A
002823         GO TO 3090-SET-COMP
002824     END-IF.
002825
002826     MOVE ZEROS                  TO WS-CLAIM-NUMBER.
002827     MOVE SAVE-COUNTER           TO WS-CN-NUMBER.
002828     MOVE SAVE-DATE              TO CURR-DATE.
002829
002830     IF CURR-MM LESS '10'
002831         MOVE CURR-M2             TO WS-CN-PRF-B
002832     END-IF.
002833
002834     IF CURR-MM = '10'
002835        MOVE 'A'                 TO WS-CN-PRF-B
002836     END-IF.
002837
002838     IF CURR-MM = '11'
002839        MOVE 'B'                 TO WS-CN-PRF-B
002840     END-IF.
002841
002842     IF CURR-MM = '12'
002843        MOVE 'C'                 TO WS-CN-PRF-B
002844     END-IF.
002845
002846     MOVE CURR-YEAR              TO WS-CN-PRF-A.
002847
002848 3090-SET-COMP.
002849
002850     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
002851     MOVE WS-CLAIM-NUMBER        TO MSTR-CLAIM-NO
002852                                    CLMNOO.
002853     MOVE AL-UANON               TO CLMNOA.
002854     MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
002855     MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
002856     MOVE CLMCARRI               TO MSTR-CARRIER.
002857
002858
002859     
      * EXEC CICS HANDLE CONDITION
002860*        NOTFND  (3200-BUILD-ZERO-TRAILER)
002861*    END-EXEC.
      *    MOVE '"$I                   ! 5 #00009348' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3520233030303039333438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002862
002863     PERFORM 7950-READ-CLAIM THRU 7950-EXIT.
002864
002865     GO TO 3080-ASSIGN-LOOP.
002866
002867 3100-CARRIER-NOT-FOUND.
002868
002869     MOVE ER-0252                TO EMI-ERROR.
002870     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002871     MOVE -1                     TO CLMCARRL.
002872     GO TO 8200-SEND-DATAONLY.
002873
002874 3110-COMPANY-NOT-FOUND.
002875
002876     MOVE ER-0254                TO EMI-ERROR.
002877     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002878     MOVE -1                     TO MAINTL.
002879     GO TO 8200-SEND-DATAONLY.
002880
002881 3120-CLAIM-MUST-BE-INPUT.
002882
002883     MOVE ER-0264                TO EMI-ERROR.
002884     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002885     MOVE -1                     TO CLMNOL.
002886     GO TO 8200-SEND-DATAONLY.
002887
002888 3200-BUILD-ZERO-TRAILER.
002889
002890     
      * EXEC CICS HANDLE CONDITION
002891*        NOTFND
002892*    END-EXEC.
      *    MOVE '"$¹                   ! 6 #00009379' TO DFHEIV0
           MOVE X'2224B9202020202020202020' &
                X'202020202020202020202120' &
                X'3620233030303039333739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002893
002894
002895     
      * EXEC CICS GETMAIN
002896*        SET     (ADDRESS OF ACTIVITY-TRAILERS)
002897*        LENGTH  (ELTRLR-LENGTH)
002898*    END-EXEC.
      *    MOVE '," L                  $   #00009384' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039333834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELTRLR-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002899
002900     MOVE SPACES                 TO ACTIVITY-TRAILERS.
002901     MOVE 'AT'                   TO AT-RECORD-ID.
002902
002903     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
002904     MOVE CLMCARRI               TO AT-CARRIER.
002905     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
002906     MOVE CERT-CERT-NO           TO AT-CERT-NO.
002907     MOVE +0                     TO AT-SEQUENCE-NO.
002908     MOVE '1'                    TO AT-TRAILER-TYPE.
002909
002910     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
002911     MOVE '5'                    TO DC-OPTION-CODE.
002912     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
002913     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
002914                                    AT-RESERVES-LAST-MAINT-DT.
002915
002916     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
002917                                    AT-RESERVES-LAST-UPDATED-BY.
002918
002919     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
002920
002921     MOVE WS-RESERVE-CONTROLS    TO AT-RESERVE-CONTROLS.
002922
002923     MOVE +0                     TO AT-FUTURE-RESERVE
002924                                    AT-IBNR-RESERVE
002925                                    AT-PAY-CURRENT-RESERVE
002926                                    AT-INITIAL-MANUAL-RESERVE
002927                                    AT-CURRENT-MANUAL-RESERVE
002928                                    AT-ITD-LIFE-REFUNDS
002929                                    AT-ITD-AH-REFUNDS
002930                                    AT-ITD-ADDITIONAL-RESERVE.
002931
002932     MOVE LOW-VALUES             TO AT-LAST-COMPUTED-DT.
002933
002934     IF MANRSVL NOT = ZERO
002935         MOVE WS-MANRSV          TO AT-INITIAL-MANUAL-RESERVE
002936                                    AT-CURRENT-MANUAL-RESERVE
002937     END-IF.
002938
002939     MOVE WS-EXPENSE-CONTROLS    TO AT-EXPENSE-CONTROLS.
002940
002941     MOVE +0                     TO AT-ITD-PAID-EXPENSES
002942                                    AT-ITD-CHARGEABLE-EXPENSE.
002943
002944     MOVE WS-TODAY-DT            TO AT-OPEN-CLOSE-DATE (1).
002945     MOVE 'O'                    TO AT-OPEN-CLOSE-TYPE (1).
002946     MOVE 'NEW'                  TO AT-OPEN-CLOSE-REASON (1).
002947
002948
002949     
      * EXEC CICS WRITE
002950*        DATASET   (ELTRLR-DSID)
002951*        FROM      (ACTIVITY-TRAILERS)
002952*        RIDFLD    (AT-CONTROL-PRIMARY)
002953*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009438' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039343338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002954
002955     MOVE AT-CONTROL-PRIMARY  TO  SAVE-AT-PRIMARY-KEY.
002956
002957     EJECT
002958
002959 3250-BUILD-I1-ADDRESS-TRAILER.
002960
002961     MOVE 'N'                    TO WS-NO-INSURED-ADDRESS.
002962     MOVE +0                     TO WS-INSURED-ADDR-CNT.
002963     MOVE PI-COMPANY-CD          TO WS-MA-COMPANY-CD.
002964     MOVE CRTCARRI               TO WS-MA-CARRIER.
002965     MOVE GROUPI                 TO WS-MA-GROUPING.
002966     MOVE STATEI                 TO WS-MA-STATE.
002967     MOVE PI-SAVE-ACCOUNT        TO WS-MA-ACCOUNT.
002968     MOVE CERTNOI                TO WS-MA-CERT-PRIME.
002969     MOVE SUFXI                  TO WS-MA-CERT-SFX.
002970     MOVE PI-SAVE-EFFDT          TO WS-MA-CERT-EFF-DT.
002971
002972     
      * EXEC CICS HANDLE CONDITION
002973*        NOTFND   (3290-NO-INSURED-ADDRESS)
002974*    END-EXEC.
      *    MOVE '"$I                   ! 7 #00009461' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3720233030303039343631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002975
002976     
      * EXEC CICS READ
002977*        EQUAL
002978*        DATASET   (ERMAIL-DSID)
002979*        SET       (ADDRESS OF MAILING-DATA)
002980*        RIDFLD    (WS-MA-CONTROL-PRIMARY)
002981*    END-EXEC.
      *    MOVE '&"S        E          (   #00009465' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039343635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-MA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002982
002983     IF MA-INSURED-LAST-NAME NOT GREATER THAN SPACES
002984         GO TO 3290-NO-INSURED-ADDRESS
002985     END-IF.
002986
002987     MOVE SPACES                 TO ACTIVITY-TRAILERS.
002988     MOVE 'AT'                   TO AT-RECORD-ID.
002989
002990     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
002991     MOVE CLMCARRI               TO AT-CARRIER.
002992     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
002993     MOVE CERT-CERT-NO           TO AT-CERT-NO.
002994     MOVE +1                     TO AT-SEQUENCE-NO.
002995     MOVE '5'                    TO AT-TRAILER-TYPE.
002996
002997     MOVE 'I'                    TO AT-ADDRESS-TYPE.
002998     IF MA-INSURED-MIDDLE-INIT > SPACES
002999        STRING MA-INSURED-FIRST-NAME DELIMITED BY '  '
003000               ' ' DELIMITED BY SIZE
003001               MA-INSURED-MIDDLE-INIT DELIMITED BY SIZE
003002               ' ' DELIMITED BY SIZE
003003               MA-INSURED-LAST-NAME DELIMITED BY SIZE
003004        INTO AT-MAIL-TO-NAME
003005     ELSE
003006        STRING MA-INSURED-FIRST-NAME DELIMITED BY '  '
003007               ' ' DELIMITED BY SIZE
003008               MA-INSURED-LAST-NAME DELIMITED BY SIZE
003009        INTO AT-MAIL-TO-NAME
003010     END-IF.
003011     MOVE MA-ADDRESS-LINE-1      TO AT-ADDRESS-LINE-1.
003012     MOVE MA-ADDRESS-LINE-2      TO AT-ADDRESS-LINE-2.
003013*    MOVE MA-CITY-STATE          TO AT-CITY-STATE.
003014     MOVE MA-CITY                TO AT-CITY
003015     MOVE MA-ADDR-STATE          TO AT-STATE
003016     MOVE MA-ZIP                 TO AT-ZIP
003017     MOVE MA-PHONE-NO            TO AT-PHONE-NO
003018
003019     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
003020                                    AT-ADDRESS-LAST-MAINT-DT.
003021
003022     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
003023                                    AT-ADDRESS-LAST-UPDATED-BY.
003024
003025     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
003026
003027     
      * EXEC CICS WRITE
003028*        DATASET   (ELTRLR-DSID)
003029*        FROM      (ACTIVITY-TRAILERS)
003030*        RIDFLD    (AT-CONTROL-PRIMARY)
003031*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009516' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039353136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003032
003033     ADD +1                      TO WS-INSURED-ADDR-CNT.
003034     GO TO 3300-BUILD-NINETY-TRAILER.
003035
003036 3290-NO-INSURED-ADDRESS.
003037      MOVE 'Y' TO WS-NO-INSURED-ADDRESS.
003038
003039 3300-BUILD-NINETY-TRAILER.
003040
003041     MOVE SPACES                 TO ACTIVITY-TRAILERS.
003042     MOVE 'AT'                   TO AT-RECORD-ID.
003043
003044     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
003045     MOVE CLMCARRI               TO AT-CARRIER.
003046
003047     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
003048     MOVE CERT-CERT-NO           TO AT-CERT-NO.
003049     MOVE +90                    TO AT-SEQUENCE-NO.
003050     MOVE '6'                    TO AT-TRAILER-TYPE.
003051
003052     IF DIAGL NOT = ZERO
003053         MOVE DIAGI              TO AT-INFO-LINE-1
003054     END-IF.
003055
003056     IF ICD1L NOT = ZERO
003057         MOVE ICD1I              TO AT-ICD-CODE-1
003058     END-IF.
003059
003060     IF ICD2L NOT = ZERO
003061         MOVE ICD2I              TO AT-ICD-CODE-2
003062     END-IF.
003063
003064     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
003065                                    AT-GEN-INFO-LAST-MAINT-DT.
003066
003067     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
003068                                    AT-GEN-INFO-LAST-UPDATED-BY.
003069     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
003070
003071
003072     
      * EXEC CICS WRITE
003073*        DATASET   (ELTRLR-DSID)
003074*        FROM      (ACTIVITY-TRAILERS)
003075*        RIDFLD    (AT-CONTROL-PRIMARY)
003076*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009561' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039353631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003077
003078     EJECT
003079 3400-CHECK-CERT-MAINT.
003080
003081     IF CERTMTI = 'A'
003082         GO TO 3600-BUILD-NEW-CERT
003083     END-IF.
003084
003085     IF NO-CLAIM-ATTACHED
003086         MOVE '1'                TO CM-CLAIM-INTERFACE-SW
003087     END-IF.
003088
003089     ADD +1                      TO CM-CLAIM-ATTACHED-COUNT.
003090
003091     IF CM-LF-CURRENT-STATUS = '2' OR
003092        CM-AH-CURRENT-STATUS = '2'
003093         GO TO 3400-REWRITE-CERT
003094     END-IF.
003095
003096     PERFORM 3700-BUILD-CERT THRU 3700-EXIT.
003097
003098 3400-REWRITE-CERT.
003099
003100
003101     
      * EXEC CICS HANDLE CONDITION
003102*        DUPKEY (3410-DUP-KEY)
003103*    END-EXEC.
      *    MOVE '"$$                   ! 8 #00009590' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'3820233030303039353930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003104
003105
003106     
      * EXEC CICS REWRITE
003107*        DATASET   (ELCERT-DSID)
003108*        FROM      (CERTIFICATE-MASTER)
003109*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009595' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039353935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003110
003111 3410-DUP-KEY.
003112
003113     GO TO 3800-BUILD-NEW-CLAIM.
003114
003115 3600-BUILD-NEW-CERT.
003116
003117     
      * EXEC CICS GETMAIN
003118*        SET      (ADDRESS OF CERTIFICATE-MASTER)
003119*        LENGTH   (ELCERT-LENGTH)
003120*    END-EXEC.
      *    MOVE '," L                  $   #00009606' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039363036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCERT-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003121
003122     MOVE SPACES                 TO CERTIFICATE-MASTER.
003123
003124     MOVE 'CM'                   TO CM-RECORD-ID.
003125
003126     MOVE PI-COMPANY-CD          TO CM-COMPANY-CD
003127                                    CM-COMPANY-CD-A1
003128                                    CM-COMPANY-CD-A2
003129                                    CM-COMPANY-CD-A4
003130                                    CM-COMPANY-CD-A5.
003131
003132     MOVE CERT-CARRIER           TO CM-CARRIER.
003133     MOVE CERT-GROUPING          TO CM-GROUPING.
003134     MOVE CERT-STATE             TO CM-STATE.
003135     MOVE CERT-ACCOUNT           TO CM-ACCOUNT.
003136     MOVE CERT-CERT-NO           TO CM-CERT-NO  CM-CERT-NO-A4.
003137     MOVE CERT-EFF-DT            TO CM-CERT-EFF-DT.
003138
003139     MOVE ZERO      TO CM-LF-ORIG-TERM       CM-LF-BENEFIT-AMT
003140                       CM-INSURED-ISSUE-AGE  CM-INSURED-JOINT-AGE
003141                       CM-LF-BENEFIT-CD      CM-AH-BENEFIT-CD
003142                       CM-LF-PREMIUM-AMT     CM-LF-REMAINING-AMT
003143                       CM-LF-ITD-CANCEL-AMT  CM-LF-ITD-DEATH-AMT
003144                       CM-LIVES              CM-LOAN-TERM
003145                       CM-AH-ITD-AH-PMT      CM-AH-NSP-PREMIUM-AMT
003146                       CM-AH-DEV-PCT         CM-AH-CRITICAL-PERIOD
003147                       CM-LF-TERM-IN-DAYS    CM-LF-ALT-PREMIUM-AMT
003148                       CM-LF-DEV-PCT         CM-LF-NSP-PREMIUM-AMT
003149                       CM-LF-CRITICAL-PERIOD CM-LF-ALT-BENEFIT-AMT
003150                       CM-LF-PREMIUM-RATE    CM-AH-PREMIUM-RATE
003151                       CM-LF-ALT-PREMIUM-RATE
003152                       CM-AH-ORIG-TERM       CM-AH-BENEFIT-AMT
003153                       CM-AH-PREMIUM-AMT     CM-AH-ITD-CANCEL-AMT
003154                       CM-AH-ITD-LUMP-PMT    CM-LOAN-APR
003155                       CM-LOAN-BALANCE       CM-PAY-FREQUENCY
003156                       CM-LIFE-COMM-PCT      CM-AH-COMM-PCT.
003157
003158     MOVE LOW-VALUES             TO CM-AH-LOAN-EXPIRE-DT
003159                                    CM-AH-CANCEL-DT
003160                                    CM-AH-SETTLEMENT-DT
003161                                    CM-AH-CANCEL-EXIT-DT
003162                                    CM-AH-SETTLEMENT-EXIT-DT
003163                                    CM-LF-LOAN-EXPIRE-DT
003164                                    CM-LF-CANCEL-DT
003165                                    CM-LF-DEATH-DT
003166                                    CM-LF-CANCEL-EXIT-DT
003167                                    CM-LF-DEATH-EXIT-DT
003168                                    CM-LAST-ADD-ON-DT
003169                                    CM-ENTRY-DT.
003170
003171     MOVE '2'                    TO CM-CLAIM-INTERFACE-SW.
003172     MOVE +1                     TO CM-CLAIM-ATTACHED-COUNT.
003173     MOVE LOW-VALUES             TO CM-LAST-MONTH-END.
003174
003175     MOVE CRTLNMEI               TO CM-INSURED-LAST-NAME
003176                                    CM-PART-LAST-NAME-A2
003177                                    CM-PART-LAST-NAME-A5.
003178
003179     MOVE CRTFNMEI               TO CM-INSURED-INITIAL1
003180                                    CM-INSURED-INITIAL1-A2
003181                                    CM-INSURED-INITIAL1-A5
003182                                    CM-INSURED-FIRST-NAME.
003183
003184     IF CRTINITL GREATER ZERO
003185         MOVE CRTINITI           TO CM-INSURED-INITIAL2
003186                                    CM-INSURED-INITIAL2-A2
003187                                    CM-INSURED-INITIAL2-A5
003188     ELSE
003189         MOVE SPACES             TO CM-INSURED-INITIAL2
003190                                    CM-INSURED-INITIAL2-A2
003191                                    CM-INSURED-INITIAL2-A5
003192     END-IF.
003193
003194     IF CM-INSURED-INITIALS = SPACES
003195         MOVE '**'               TO CM-INSURED-INITIALS
003196     END-IF.
003197
003198     IF MEMBERL GREATER ZERO
003199         MOVE MEMBERI              TO CM-MEMBER-NO
003200     ELSE
003201         MOVE CERT-STATE           TO CM-MEMB-STATE
003202         MOVE CERT-ACCOUNT-PRIME   TO CM-MEMB-ACCOUNT
003203         MOVE CM-INSURED-INITIALS  TO CM-INSURED-INITIALS-A5
003204         MOVE CM-INSURED-LAST-NAME TO CM-PART-LAST-NAME-A5
003205     END-IF.
003206
003207     IF JNTLNMEL GREATER ZERO
003208         MOVE JNTLNMEI           TO CM-JT-LAST-NAME
003209     END-IF.
003210
003211     IF JNTFNMEL GREATER ZERO
003212         MOVE JNTFNMEI           TO CM-JT-FIRST-NAME
003213     END-IF.
003214
003215     IF JNTINITL GREATER ZERO
003216         MOVE JNTINITI           TO CM-JT-INITIAL
003217     END-IF.
003218
003219     PERFORM 3700-BUILD-CERT THRU 3700-EXIT.
003220
003221
003222     
      * EXEC CICS HANDLE CONDITION
003223*        DUPKEY   (3690-CERT-WRITTEN)
003224*    END-EXEC.
      *    MOVE '"$$                   ! 9 #00009711' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'3920233030303039373131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003225
003226
003227     
      * EXEC CICS WRITE
003228*        DATASET   (ELCERT-DSID)
003229*        FROM      (CERTIFICATE-MASTER)
003230*        RIDFLD    (CM-CONTROL-PRIMARY)
003231*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00009716' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039373136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003232
003233 3690-CERT-WRITTEN.
003234
003235     GO TO 3800-BUILD-NEW-CLAIM.
003236
003237 3690-EXIT.
003238      EXIT.
003239
003240     EJECT
003241 3700-BUILD-CERT.
003242
003243     IF CERTMTI NOT = 'A'
003244         GO TO 3700-FINISH-CERT-BUILD
003245     END-IF.
003246
003247     IF CERTMTI = 'A'
003248         IF LCVCDL = ZEROS
003249             MOVE ZEROS          TO CM-LF-BENEFIT-CD
003250                                    CM-LF-ORIG-TERM
003251                                    CM-LF-BENEFIT-AMT
003252                                    CM-LF-PREMIUM-RATE
003253             GO TO 3700-BUILD-AH-COVERAGE-SIDE
003254         END-IF
003255     END-IF.
003256
003257     IF LCVCDL GREATER ZERO
003258         MOVE LCVCDI             TO CM-LF-BENEFIT-CD
003259     END-IF.
003260
003261     IF LCVOTRML GREATER ZERO
003262         MOVE LCVOTRMI           TO CM-LF-ORIG-TERM
003263     END-IF.
003264
003265     IF LCVBENEL GREATER ZERO
003266         MOVE WS-LCVBENE         TO CM-LF-BENEFIT-AMT
003267     END-IF.
003268
003269     IF LCVRATEL GREATER ZERO
003270         MOVE WS-LCVRATE         TO CM-LF-PREMIUM-RATE
003271     END-IF.
003272
003273     IF WS-EXPIRE NOT = LOW-VALUES
003274         MOVE WS-EXPIRE          TO CM-LF-LOAN-EXPIRE-DT
003275     END-IF.
003276
003277     IF LCVFORML GREATER ZERO
003278         MOVE LCVFORMI           TO CM-POLICY-FORM-NO
003279     END-IF.
003280
003281     IF LCVCNDTL = ZERO
003282        GO TO 3700-BUILD-AH-COVERAGE-SIDE
003283     END-IF.
003284
003285     IF WS-LCVCNDT NOT = LOW-VALUES
003286        MOVE '8'                 TO CM-LF-CURRENT-STATUS
003287        MOVE WS-LCVCNDT          TO CM-LF-CANCEL-DT
003288        GO TO 3700-BUILD-AH-COVERAGE-SIDE
003289     END-IF.
003290
003291     IF CM-LF-CURRENT-STATUS = '7'
003292        MOVE CM-LF-STATUS-AT-DEATH TO CM-LF-CURRENT-STATUS
003293        MOVE SPACES              TO CM-LF-STATUS-AT-DEATH
003294        MOVE LOW-VALUES          TO CM-LF-DEATH-EXIT-DT
003295                                    CM-LF-DEATH-DT
003296        GO TO 3700-BUILD-AH-COVERAGE-SIDE
003297     END-IF.
003298
003299     IF CM-LF-CURRENT-STATUS = '8'
003300        MOVE '1'                 TO CM-LF-CURRENT-STATUS
003301        MOVE LOW-VALUES          TO CM-LF-CANCEL-DT
003302     END-IF.
003303
003304 3700-BUILD-AH-COVERAGE-SIDE.
003305
003306     IF CERTMTI = 'A'
003307         IF ACVCDL = ZEROS
003308             MOVE ZEROS          TO CM-AH-BENEFIT-CD
003309                                    CM-AH-ORIG-TERM
003310                                    CM-AH-BENEFIT-AMT
003311                                    CM-AH-PREMIUM-RATE
003312             GO TO 3700-COVERAGE-BUILT
003313         END-IF
003314     END-IF.
003315
003316     IF ACVCDL GREATER ZERO
003317         MOVE ACVCDI             TO CM-AH-BENEFIT-CD
003318     END-IF.
003319
003320     IF ACVOTRML GREATER ZERO
003321         MOVE ACVOTRMI           TO CM-AH-ORIG-TERM
003322     END-IF.
003323
003324     IF ACVBENEL GREATER ZERO
003325         MOVE WS-ACVBENE         TO CM-AH-BENEFIT-AMT
003326     END-IF.
003327
003328     IF ACVRATEL GREATER ZERO
003329         MOVE WS-ACVRATE         TO CM-AH-PREMIUM-RATE
003330     END-IF.
003331
003332     IF WS-EXPIRE NOT = LOW-VALUES
003333         MOVE WS-EXPIRE          TO CM-AH-LOAN-EXPIRE-DT
003334     END-IF.
003335
003336     IF ACVFORML GREATER ZERO
003337         MOVE ACVFORMI           TO CM-POLICY-FORM-NO
003338     END-IF.
003339
003340     IF ACVCNDTL = ZERO
003341        GO TO 3700-COVERAGE-BUILT
003342     END-IF.
003343
003344     IF WS-ACVCNDT NOT = LOW-VALUES
003345        MOVE '8'                 TO CM-AH-CURRENT-STATUS
003346        MOVE WS-ACVCNDT          TO CM-AH-CANCEL-DT
003347        GO TO 3700-COVERAGE-BUILT
003348     END-IF.
003349
003350     IF CM-AH-CURRENT-STATUS = '6'
003351        MOVE CM-AH-STATUS-AT-SETTLEMENT TO CM-AH-CURRENT-STATUS
003352        MOVE SPACES              TO CM-AH-STATUS-AT-SETTLEMENT
003353        MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-EXIT-DT
003354        MOVE LOW-VALUES          TO CM-AH-SETTLEMENT-DT
003355        GO TO 3700-COVERAGE-BUILT
003356     END-IF.
003357
003358     IF CM-AH-CURRENT-STATUS = '8'
003359        MOVE '1'                 TO CM-AH-CURRENT-STATUS
003360        MOVE LOW-VALUES          TO CM-AH-CANCEL-DT
003361     END-IF.
003362
003363 3700-COVERAGE-BUILT.
003364
003365     IF ISSAGEL GREATER ZERO
003366         MOVE ISSAGEI            TO CM-INSURED-ISSUE-AGE
003367     END-IF.
003368
003369     IF JNTAGEL GREATER ZERO
003370         MOVE JNTAGEI            TO CM-INSURED-JOINT-AGE
003371     END-IF.
003372
003373     IF SEXL GREATER ZERO
003374         MOVE SEXI               TO CM-INSURED-SEX
003375     END-IF.
003376
003377     IF APRL GREATER ZERO
003378         MOVE WS-APR             TO CM-LOAN-APR
003379     END-IF.
003380
003381     IF PMTFREQL GREATER ZERO
003382         MOVE WS-PMTFREQ         TO CM-PAY-FREQUENCY
003383     END-IF.
003384
003385     IF INDGRPL GREATER ZERO
003386         MOVE INDGRPI            TO CM-IND-GRP-TYPE
003387     END-IF.
003388
003389     IF CERTMTI = 'A'
003390         MOVE WS-REIN-TABLE      TO CM-REIN-TABLE
003391     END-IF.
003392
003393     IF REINCDL GREATER ZERO
003394         MOVE REINCDI            TO CM-SPECIAL-REIN-CODE
003395                                    WS-REIN-1
003396                                    WS-REIN-2
003397                                    WS-REIN-3
003398         MOVE WS-REIN-TABLE      TO CM-REIN-TABLE
003399     END-IF.
003400
003401     IF WS-ADD-ON-DT NOT = LOW-VALUES
003402         MOVE WS-ADD-ON-DT   TO CM-LAST-ADD-ON-DT
003403     END-IF.
003404
003405     IF CRTSSNL GREATER ZERO
003406         MOVE CRTSSNI            TO CM-SOC-SEC-NO
003407     ELSE
003408         IF SSNL GREATER ZERO
003409             MOVE SSNI           TO CM-SOC-SEC-NO
003410         END-IF
003411     END-IF.
003412
003413     IF CRTSSNL = ZERO AND
003414        SSNL    = ZERO
003415         MOVE CM-STATE             TO CM-SSN-STATE
003416         MOVE CM-ACCOUNT-PRIME     TO CM-SSN-ACCOUNT
003417         MOVE CM-INSURED-INITIALS  TO CM-INSURED-INITIALS-A2
003418         MOVE CM-INSURED-LAST-NAME TO CM-PART-LAST-NAME-A2
003419     END-IF.
003420
003421     IF CM-LF-CURRENT-STATUS = SPACES
003422         MOVE '1'                TO CM-LF-CURRENT-STATUS
003423     END-IF.
003424
003425     IF CM-AH-CURRENT-STATUS = SPACES
003426         MOVE '1'                TO CM-AH-CURRENT-STATUS
003427     END-IF.
003428
003429     IF CERTMTI = 'A'
003430         MOVE SAVE-BIN-DATE      TO CM-ENTRY-DT
003431     END-IF.
003432
003433 3700-FINISH-CERT-BUILD.
003434
003435     IF LOANNOL GREATER ZERO
003436         MOVE LOANNOI            TO CM-LOAN-NUMBER
003437     END-IF.
003438
003439     IF LOANBALL GREATER ZERO
003440         MOVE HOLD-LOAN-BAL      TO CM-LOAN-BALANCE
003441     END-IF.
003442
003443     IF PREMTYPL GREATER ZERO
003444         MOVE PREMTYPI           TO CM-PREMIUM-TYPE
003445     END-IF.
003446
003447 3700-EXIT.
003448      EXIT.
003449
003450     EJECT
003451 3800-BUILD-NEW-CLAIM.
003452
003453*************************************************************
003454*****           START OF ACTIVITY FILE PROCESSING          **
003455*************************************************************
003456     IF PI-COMPANY-ID EQUAL 'CID' OR 'AHL' OR 'FNL'
003457         PERFORM 6700-OUTPUT-ACTIVITY-RECORD THRU
003458                 6700-EXIT
003459     END-IF.
003460*************************************************************
003461*****            END OF ACTIVITY FILE PROCESSING           **
003462*************************************************************
003463
003464
003465
003466     
      * EXEC CICS GETMAIN
003467*        SET     (ADDRESS OF CLAIM-MASTER)
003468*        LENGTH  (ELMSTR-LENGTH)
003469*    END-EXEC.
      *    MOVE '," L                  $   #00009955' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039393535' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELMSTR-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003470
003471     MOVE SPACES TO CLAIM-MASTER.
003472
003473     MOVE LOW-VALUES   TO       CL-INSURED-BIRTH-DT CL-INCURRED-DT
003474         CL-NEXT-FOLLOWUP-DT    CL-REPORTED-DT      CL-LAST-PMT-DT
003475         CL-EST-END-OF-DISAB-DT CL-PAID-THRU-DT   CL-LAST-CLOSE-DT
003476         CL-LAST-REOPEN-DT      CL-PURGED-DT        CL-RESTORED-DT
003477         CL-NEXT-AUTO-PAY-DT    CL-NEXT-RESEND-DT
003478         CL-LAST-ADD-ON-DT      CL-FILE-ESTABLISH-DT
003479         CL-LAST-MAINT-DT       CL-HISTORY-ARCHIVE-DT
003480         CL-ACTIVITY-MAINT-DT   cl-benefit-expiration-dt
003481
003482     MOVE +0 TO CL-LAST-PMT-AMT      CL-TOTAL-PAID-AMT
003483             CL-NO-OF-PMTS-MADE      CL-NO-OF-DAYS-PAID
003484             CL-LAST-INC-DT-CHANGE   CL-AUTO-PAY-SEQ
003485             CL-INSURED-ADDR-CNT     CL-ACCOUNT-ADDR-CNT
003486             CL-BENIF-ADDR-CNT       CL-EMPLOYER-ADDR-CNT
003487             CL-DOCTOR-ADDR-CNT      CL-OTHER-1-ADDR-CNT
003488             CL-OTHER-2-ADDR-CNT     CL-FATAL-ERROR-CNT
003489             CL-CLAIM-PAYMENT-STATUS CL-LAST-MAINT-HHMMSS
003490             CL-FORCEABLE-ERROR-CNT.
003491
003492     MOVE WS-INSURED-ADDR-CNT    TO CL-INSURED-ADDR-CNT.
003493
003494     MOVE ZEROS                  TO CL-ACTIVITY-CODE
003495                                    CL-LAPSE-REPORT-CODE
003496                                    CL-LAG-REPORT-CODE
003497                                    CL-CRITICAL-PERIOD
003498*                                   CL-CRIT-PER-RTW-MOS
003499     move 01                     to cl-benefit-period
003500
003501     MOVE EMI-FORCABLE-CTR       TO CL-FORCEABLE-ERROR-CNT.
003502     MOVE +4095                  TO CL-TRAILER-SEQ-CNT.
003503
003504     IF BENECDL NOT = ZEROS
003505        MOVE BENECDI             TO CL-BENEFICIARY
003506     END-IF.
003507
003508     MOVE 'CL'                   TO CL-RECORD-ID.
003509     MOVE 'CR'                   TO CL-SYSTEM-IDENTIFIER.
003510
003511     MOVE PI-COMPANY-CD  TO  CL-COMPANY-CD     CL-COMPANY-CD-A1
003512                             CL-COMPANY-CD-A2  CL-COMPANY-CD-A4
003513                             CL-COMPANY-CD-A5.
003514
003515     MOVE CLMCARRI               TO CL-CARRIER.
003516
003517     IF CLMNOL = ZERO
003518         MOVE WS-CLAIM-NUMBER    TO CL-CLAIM-NO
003519     ELSE
003520         MOVE CLMNOI             TO CL-CLAIM-NO
003521     END-IF.
003522
003523     MOVE CERTNOI                TO CL-CERT-PRIME
003524                                    CL-CERT-A4-PRIME.
003525
003526     MOVE SUFXI                  TO CL-CERT-SFX
003527                                    CL-CERT-A4-SFX.
003528
003529     MOVE PCERTNOI               TO CL-PRIME-CERT-PRIME.
003530
003531     MOVE PSUFXI                 TO CL-PRIME-CERT-SFX.
003532
003533     MOVE CL-PRIME-CERT-NO       TO PI-PRIMARY-CERT-NO.
003534
003535     MOVE LSTNMEI                TO CL-INSURED-LAST-NAME.
003536
003537     MOVE WS-TODAY-DT            TO CL-FILE-ESTABLISH-DT.
003538     MOVE ' '                    TO CL-LAST-MAINT-TYPE.
003539
003540     PERFORM 3995-BUILD-CLAIM-RECORD THRU 3995-EXIT.
003541
003542     IF CERTMTI = 'A'
003543         MOVE '2'                TO CL-CERT-ORIGIN
003544     ELSE
003545         MOVE '1'                TO CL-CERT-ORIGIN
003546     END-IF.
003547
003548     MOVE CERT-CARRIER        TO CL-CERT-CARRIER   PI-CARRIER.
003549     MOVE CERT-GROUPING       TO CL-CERT-GROUPING  PI-GROUPING.
003550     MOVE CERT-STATE          TO CL-CERT-STATE     PI-STATE.
003551     MOVE CERT-ACCOUNT        TO CL-CERT-ACCOUNT   PI-ACCOUNT.
003552     MOVE WS-EFFDT            TO CL-CERT-EFF-DT    PI-CERT-EFF-DT.
003553
003554 3820-REWRITE-CONTROL-FILE.
003555
003556     IF CLMNOL GREATER ZERO
003557         GO TO 3830-SEQUENCE-CLAIM-RECORDS
003558     END-IF.
003559
003560     IF CF-RECORD-TYPE = '6'
003561         MOVE SAVE-COUNTER    TO  CF-CLAIM-COUNTER
003562     ELSE
003563         MOVE SAVE-COUNTER    TO  CF-CO-CLAIM-COUNTER
003564     END-IF.
003565
003566
003567     
      * EXEC CICS REWRITE
003568*        DATASET   (ELCNTL-DSID)
003569*        FROM      (CONTROL-FILE)
003570*    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010056' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303130303536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003571
003572 3830-SEQUENCE-CLAIM-RECORDS.
003573
003574     MOVE CLAIM-MASTER           TO WS-SAVE-CLAIM-MASTER.
003575     MOVE CL-CONTROL-PRIMARY     TO ELMSTR-KEY.
003576     PERFORM 7700-CHECK-SEQUENCE THRU 7799-EXIT.
003577
003578     IF WS-ASSOC-CERT-TOTAL = ZERO
003579         MOVE 1                  TO CL-ASSOC-CERT-SEQU
003580                                    CL-ASSOC-CERT-TOTAL
003581         GO TO 3840-WRITE-CLAIM-MASTER
003582     END-IF.
003583
003584     IF CL-CONTROL-PRIMARY LESS THAN WS-SAVE-CLAIM-KEY
003585         MOVE CL-CONTROL-PRIMARY TO ELMSTR-KEY
003586     ELSE
003587         MOVE WS-SAVE-CLAIM-KEY  TO ELMSTR-KEY
003588     END-IF.
003589
003590     MOVE WS-SAVE-CLAIM-MASTER   TO CLAIM-MASTER.
003591     MOVE ZERO                   TO CL-ASSOC-CERT-SEQU.
003592     MOVE WS-ASSOC-CERT-TOTAL    TO CL-ASSOC-CERT-TOTAL.
003593
003594 3840-WRITE-CLAIM-MASTER.
003595
003596     PERFORM 6600-CHECK-AUTO-ACTIVITY THRU 6600-EXIT.
003597     IF WS-REC-FOUND-SW = 'Y'
003598         MOVE 01                 TO  CL-ACTIVITY-CODE
003599         MOVE SAVE-BIN-DATE      TO  CL-ACTIVITY-MAINT-DT
003600         MOVE 'ADD '             TO  CL-ACTIVITY-MAINT-TYPE
003601     END-IF.
003602
003603
003604     
      * EXEC CICS HANDLE CONDITION
003605*         DUPKEY    (3850-CLAIM-MSTR-WRITTEN)
003606*    END-EXEC.
      *    MOVE '"$$                   ! : #00010093' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'3A20233030303130303933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003607
003608** POPULATE THE CREDIT-CARD NO WITH THE CERT NO.
003609     MOVE CL-CERT-NO             TO CL-CCN-A5.
003610
003611     move 'CLMS'                 to pa-rec-type
003612     move pi-company-id          to pa-company-id
003613     move claim-master           to pa-rest-of-record
003614
003615     
      * EXEC CICS LINK
003616*        PROGRAM  ('WF001')
003617*        COMMAREA (work-flow-pass-area)
003618*        LENGTH   (604)
003619*    END-EXEC
           MOVE 'WF001' TO DFHEIV1
           MOVE 604
             TO DFHEIV11
      *    MOVE '."C                   (   #00010104' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303130313034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 work-flow-pass-area, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003620
003621     
      * EXEC CICS WRITE
003622*        DATASET   (ELMSTR-DSID)
003623*        FROM      (CLAIM-MASTER)
003624*        RIDFLD    (CL-CONTROL-PRIMARY)
003625*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010110' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130313130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 CL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003626
003627 3850-CLAIM-MSTR-WRITTEN.
003628
003629     IF WS-ASSOC-CERT-TOTAL NOT = ZERO
003630         PERFORM 7710-RESEQUENCE-CLAIMS THRU 7799-EXIT
003631     END-IF.
003632
003633     EJECT
003634 3990-ADD-DONE.
003635*    MOVE 1                     TO EMI-SWITCH1  EMI-SWITCH-AREA-1
003636*                                  EMI-SUB      EMI-SWITCH-AREA-2.
003637*    MOVE SPACES                TO EMI-ACTION-SWITCH
003638*                                  EMI-ERROR-LINES.
003639
003640     IF CLMNOL = ZERO
003641         move spaces             to emi-claim-no
003642         MOVE ER-0265            TO EMI-ERROR
003643         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003644         MOVE WS-CLAIM-NUMBER    TO EMI-TEXT-VARIABLE (EMI-SUB)
003645                                    PI-LAST-CLAIM
003646     ELSE
003647         MOVE CLMNOI             TO PI-LAST-CLAIM
003648                                    MSTR-CLAIM-NO
003649         MOVE ER-0000            TO EMI-ERROR
003650         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003651     END-IF.
003652
003653     IF (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O')
003654       AND (PI-ST-VFY-2ND-BENE = 'L' OR 'B')
003655         PERFORM 3994-BUILD-NINETY-THREE-TRAILER THRU 3994-EXIT
003656         MOVE ER-7575            TO EMI-ERROR
003657         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003658     END-IF.
003659
003660     IF CLMTYPEI NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
003661       AND (PI-ST-VFY-2ND-BENE = 'A' OR 'B')
003662         PERFORM 3994-BUILD-NINETY-THREE-TRAILER THRU 3994-EXIT
003663         MOVE ER-7582            TO EMI-ERROR
003664         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003665     END-IF.
003666
003667     IF PI-STATE = 'VA' OR 'PA' OR 'GA'
003668        PERFORM 3991-CHECK-2-YEAR-CONTESTABLE THRU 3991-EXIT
003669     END-IF.
003670
003671     IF WS-NO-INSURED-ADDRESS = 'Y'
003672         MOVE ER-3548            TO EMI-ERROR
003673         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003674     END-IF.
003675
003676     IF PI-ST-CAUSAL-STATE = 'B'  OR
003677       (PI-ST-CAUSAL-STATE = 'L' AND
003678             (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'))   OR
003679       (PI-ST-CAUSAL-STATE = 'A' AND
003680              CLMTYPEI = PI-AH-OVERRIDE-L1)
003681         PERFORM 3994B-BUILD-NINETY-FOUR-TRAILER THRU 3994B-EXIT
003682         MOVE ER-7577            TO EMI-ERROR
003683         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003684     END-IF.
003685
003686     if pi-company-id = 'DCC' OR 'VPP'
003687        PERFORM 3994C-BUILD-NINETY-FIVE-TRAILER
003688                                 THRU 3994C-EXIT
003689     end-if
003690
003691*052918IF CLMTYPEI = PI-AH-OVERRIDE-L1
003692*052918  AND PI-COMPANY-ID = 'CID'
003693*052918   SET ELAPSED-BETWEEN-BIN TO TRUE
003694*052918   MOVE ZERO               TO DC-ELAPSED-MONTHS
003695*052918                              DC-ELAPSED-DAYS
003696*052918
003697*052918   MOVE CL-INCURRED-DT  TO DC-BIN-DATE-1
003698*052918   MOVE WS-TODAY-DT TO DC-BIN-DATE-2
003699*052918   PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003700*052918   IF DC-ODD-DAYS-OVER > ZERO
003701*052918      ADD 1 TO DC-ELAPSED-MONTHS
003702*052918   END-IF
003703*052918
003704*052918   IF PI-STATE = 'HI'
003705*052918     AND DC-ELAPSED-MONTHS <= 18
003706*052918      CONTINUE
003707*052918   ELSE
003708*052918      IF DC-ELAPSED-MONTHS > 15
003709*052918         MOVE ER-7572            TO EMI-ERROR
003710*052918         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003711*052918         PERFORM 3992-BUILD-TRAILER THRU 3992-EXIT
003712*052918      END-IF
003713*052918   END-IF
003714*052918
003715*052918END-IF.
003716
003717     if clmtypei = 'L' or 'O'
003718        move cm-lf-benefit-cd    to ws-ben-hold
003719        move '4'                 to ws-rec-type
003720        perform 7100-read-benefit thru 7199-exit
003721        if ws-ben-alpha-hold <> spaces
003722           move ws-special-calc-cd
003723                                 to ws-lf-special-calc-cd
003724        end-if
003725     else
003726        move cm-ah-benefit-cd    to ws-ben-hold
003727        move '5'                 to ws-rec-type
003728        perform 7100-read-benefit thru 7199-exit
003729        if ws-ben-alpha-hold <> spaces
003730           move ws-special-calc-cd
003731                                 to ws-ah-special-calc-cd
003732        end-if
003733     end-if
003734
003735     move ' '                    to ws-mob-cert-ind
003736     if pi-company-id = 'CID'
003737        if ((CLMTYPEI = PI-LIFE-OVERRIDE-L1 or 'O')
003738           and (ws-lf-special-calc-cd = 'O'))
003739                      or
003740           ((clmtypei <> pi-life-override-l1 and 'O')
003741           and (ws-ah-special-calc-cd = 'O'))
003742           set mob-cert to true
003743        end-if
003744     else
003745        if (pi-company-id = 'DCC')
003746           and (cm-carrier = '7')
003747           set mob-cert to true
003748        end-if
003749     end-if
003750
003751     perform 7990-get-lo-hi-acct-dates
003752                                 thru 7990-exit
003753
003754     if (ws-incur >= ws-hi-acct-dt)
003755        and (acct-cancelled)
003756        and (mob-cert)
003757        MOVE er-1682             TO EMI-ERROR
003758        MOVE -1                  TO INCURL
003759        MOVE AL-UABON            TO INCURA
003760        PERFORM 9900-ERROR-FORMAT
003761                                 THRU 9900-EXIT
003762        PERFORM 3992-BUILD-TRAILER
003763                                 THRU 3992-EXIT
003764     end-if
003765
003766     if ws-incur < cm-cert-eff-dt
003767        MOVE er-1683             TO EMI-ERROR
003768        MOVE -1                  TO INCURL
003769        MOVE AL-UABON            TO INCURA
003770        PERFORM 9900-ERROR-FORMAT
003771                                 THRU 9900-EXIT
003772        PERFORM 3992-BUILD-TRAILER
003773                                 THRU 3992-EXIT
003774     end-if
003775
003776     .
003777 3990-continue.
003778
003779     MOVE CLMNOI                     TO  PI-CLAIM-NO.
003780     MOVE CLMCARRI                   TO  PI-CARRIER.
003781     MOVE CERTNOI                    TO  PI-CERT-PRIME.
003782     MOVE SUFXI                      TO  PI-CERT-SFX.
003783
003784     IF WS-LETTER-SW = 'Y'
003785         PERFORM 6600-START-AUTO-LETTER-WRITER THRU 6600-EXIT
003786         IF W-1523-ERROR-CODE NOT = ZEROS
003787             MOVE W-1523-ERROR-CODE  TO  EMI-ERROR
003788             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003789         END-IF
003790     END-IF.
003791
003792     perform 3998-update-cert-claim-trailer
003793                                 thru 3998-exit
003794
003795     MOVE CLMTYPEI               TO PI-LAST-CLAIM-TYPE.
003796     MOVE CERTNOI                TO PI-LAST-CERT-PRIME.
003797     MOVE SUFXI                  TO PI-LAST-CERT-SUFX.
003798     MOVE CLMCARRI               TO PI-LAST-CARR.
003799
003800     IF PRTOPTL GREATER THAN 0
003801         MOVE PRTOPTI            TO PI-PRT-OPT
003802     END-IF.
003803
003804     IF ALTPRTL GREATER THAN 0
003805         MOVE ALTPRTI            TO PI-ALT-PRT
003806     END-IF.
003807
003808     IF PI-CERT-PROCESSED NOT LESS THAN PI-CERT-SELECT-CNT
003809         MOVE LOW-VALUES         TO EL130AI
003810         MOVE PI-LAST-CERT-PRIME TO CERTNOO
003811         MOVE AL-SANON           TO CERTNOA
003812         MOVE PI-LAST-CERT-SUFX  TO SUFXO
003813         MOVE AL-SANON           TO SUFXA
003814         MOVE PI-LAST-CARR       TO CLMCARRO
003815         MOVE AL-UANON           TO CLMCARRA
003816         MOVE PI-LAST-CLAIM      TO CLMNOO
003817         MOVE AL-UANON           TO CLMNOA
003818         MOVE PI-LAST-CLAIM-TYPE TO CLMTYPEO
003819         MOVE AL-UANON           TO CLMTYPEA
003820     ELSE
003821         PERFORM 0690-HIGHLIGHT-CERTS THRU 0690-EXIT
003822         PERFORM 3993-MODIFY-CLAIM-ATTRB THRU 3993-EXIT
003823     END-IF.
003824
003825     IF PI-PRT-OPT = 'N' OR 'L'
003826         MOVE PI-PRT-OPT         TO PRTOPTO
003827         MOVE 1                  TO PRTOPTL
003828     END-IF.
003829
003830     IF PI-ALT-PRT NOT = SPACES AND LOW-VALUES
003831         MOVE PI-ALT-PRT         TO ALTPRTO
003832         MOVE 4                  TO ALTPRTL
003833     END-IF.
003834
003835     IF PRTOPTL GREATER THAN 0
003836         IF PRTOPTI = 'L'
003837             GO TO 0400-CREATE-ELACTQ
003838         ELSE
003839             GO TO 0480-PRINT-NOW
003840         END-IF
003841     END-IF.
003842
003843     move 'S'                    to mainti
003844     PERFORM 0500-CREATE-TEMP-STORAGE
003845                                 THRU 0599-EXIT
003846
003847     IF PI-CERT-PROCESSED NOT LESS THAN PI-CERT-SELECT-CNT
003848         GO TO 8100-SEND-INITIAL-MAP
003849     ELSE
003850         MOVE LOW-VALUES         TO MAINTO
003851         MOVE -1                 TO MAINTL
003852         GO TO 8200-SEND-DATAONLY
003853     END-IF.
003854
003855     EJECT
003856 3991-CHECK-2-YEAR-CONTESTABLE.
003857     SET ELAPSED-BETWEEN-BIN TO TRUE
003858     MOVE ZERO               TO DC-ELAPSED-MONTHS
003859                                DC-ELAPSED-DAYS
003860
003861     MOVE PI-SAVE-EFFDT  TO DC-BIN-DATE-1.
003862     MOVE CL-INCURRED-DT TO DC-BIN-DATE-2.
003863     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
003864     IF DC-ODD-DAYS-OVER > ZERO
003865        ADD 1 TO DC-ELAPSED-MONTHS
003866     END-IF
003867
003868     IF DC-ELAPSED-MONTHS <= 24
003869        MOVE CL-REPORTED-DT TO DC-BIN-DATE-2
003870        MOVE ZERO           TO DC-ELAPSED-MONTHS
003871                               DC-ELAPSED-DAYS
003872        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
003873        IF DC-ODD-DAYS-OVER > ZERO
003874           ADD 1 TO DC-ELAPSED-MONTHS
003875        END-IF
003876        IF DC-ELAPSED-MONTHS > 24
003877           MOVE ER-1679            TO EMI-ERROR
003878           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003879           PERFORM 3992-BUILD-TRAILER THRU 3992-EXIT
003880        END-IF
003881     END-IF.
003882
003883 3991-EXIT.
003884     EXIT.
003885 3992-BUILD-TRAILER.
003886
003887     MOVE SPACES             TO AT-GENERAL-INFO-TR
003888     INITIALIZE AT-GENERAL-INFO-TR
003889     MOVE ELMSTR-KEY         TO AT-CONTROL-PRIMARY
003890     MOVE 'AT'               TO AT-RECORD-ID
003891     evaluate true
003892        when emi-error = er-1679
003893           move er-1679-text     to at-info-line-1
003894        when emi-error = er-1682
003895           move er-1682-text     to at-info-line-1
003896        when emi-error = er-1683
003897           move er-1683-text     to at-info-line-1
003898     end-evaluate
003899     move +97                to at-sequence-no
003900*     IF EMI-ERROR = ER-7572
003901*        MOVE +97             TO AT-SEQUENCE-NO
003902*     ELSE
003903*        MOVE +96             TO AT-SEQUENCE-NO
003904*     END-IF
003905     MOVE '6'                TO AT-TRAILER-TYPE
003906     MOVE WS-TODAY-DT        TO AT-RECORDED-DT
003907                                AT-GEN-INFO-LAST-MAINT-DT
003908     MOVE PI-PROCESSOR-ID    TO AT-RECORDED-BY
003909                                AT-GEN-INFO-LAST-UPDATED-BY
003910     MOVE EIBTIME            TO AT-LAST-MAINT-HHMMSS
003911
003912*     IF EMI-ERROR = ER-7572
003913*        MOVE WS-FILING-NOTE TO AT-INFO-LINE-1
003914*     ELSE
003915*        MOVE EMI-LINE2 TO AT-INFO-LINE-1
003916*     END-IF
003917     MOVE SPACES             TO AT-INFO-LINE-2.
003918
003919 3992-WRITE.
003920
003921     
      * EXEC CICS HANDLE CONDITION
003922*        DUPREC    (3992-DUPREC)
003923*    END-EXEC.
      *    MOVE '"$%                   ! ; #00010410' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'3B20233030303130343130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003924
003925     
      * EXEC CICS WRITE
003926*         DATASET     ('ELTRLR')
003927*         FROM        (ACTIVITY-TRAILERS)
003928*         RIDFLD      (AT-CONTROL-PRIMARY)
003929*     END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00010414' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130343134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003930
003931     GO TO 3992-EXIT.
003932
003933 3992-DUPREC.
003934     SUBTRACT +1 FROM AT-SEQUENCE-NO.
003935     GO TO 3992-WRITE.
003936
003937 3992-EXIT.
003938     EXIT.
003939
003940 3993-MODIFY-SCREEN-ATTRB.
003941
003942     IF CERTMTL GREATER ZERO
003943         MOVE AL-UANON           TO CERTMTA
003944     END-IF.
003945
003946     IF EFFDTL GREATER ZERO
003947         MOVE AL-UANON           TO EFFDTA
003948     END-IF.
003949
003950     IF ACCOUNTL GREATER ZERO
003951         MOVE AL-UANON           TO ACCOUNTA
003952     END-IF.
003953
003954     IF STATEL GREATER ZERO
003955         MOVE AL-UANON           TO STATEA
003956     END-IF.
003957
003958     IF CRTCARRL GREATER ZERO
003959         MOVE AL-UANON           TO CRTCARRA
003960     END-IF.
003961
003962     IF GROUPL GREATER ZERO
003963         MOVE AL-UANON           TO GROUPA
003964     END-IF.
003965
003966     IF CRTLNMEL GREATER ZERO
003967         MOVE AL-UANON           TO CRTLNMEA
003968     END-IF.
003969
003970     IF CRTFNMEL GREATER ZERO
003971         MOVE AL-UANON           TO CRTFNMEA
003972     END-IF.
003973
003974     IF CRTINITL GREATER ZERO
003975         MOVE AL-UANON           TO CRTINITA
003976     END-IF.
003977
003978     IF ISSAGEL GREATER ZERO
003979         MOVE AL-UNNON           TO ISSAGEA
003980     END-IF.
003981
003982     IF JNTLNMEL GREATER ZERO
003983         MOVE AL-UANON           TO JNTLNMEA
003984     END-IF.
003985
003986     IF JNTFNMEL GREATER ZERO
003987         MOVE AL-UANON           TO JNTFNMEA
003988     END-IF.
003989
003990     IF JNTINITL GREATER ZERO
003991         MOVE AL-UANON           TO JNTINITA
003992     END-IF.
003993
003994     IF JNTAGEL GREATER ZERO
003995         MOVE AL-UNNON           TO JNTAGEA
003996     END-IF.
003997
003998     IF CRTSSNL GREATER ZERO
003999         MOVE AL-UANON           TO CRTSSNA
004000     END-IF.
004001
004002     IF ACVDSCRL GREATER ZERO
004003         MOVE AL-SANON           TO ACVDSCRA
004004     END-IF.
004005
004006     IF ACVKINDL GREATER ZERO
004007         MOVE AL-SANON           TO ACVKINDA
004008     END-IF.
004009
004010     IF ACVCDL GREATER ZERO
004011         MOVE AL-UANON           TO ACVCDA
004012     END-IF.
004013
004014     IF ACVOTRML GREATER ZERO
004015         MOVE AL-UNNON           TO ACVOTRMA
004016     END-IF.
004017
004018     IF ACVRTRML GREATER ZERO
004019         MOVE AL-UNNON           TO ACVRTRMA
004020     END-IF.
004021
004022     IF ACVRATEL GREATER ZERO
004023         MOVE AL-UNNON           TO ACVRATEA
004024     END-IF.
004025
004026     IF ACVBENEL GREATER ZERO
004027         MOVE AL-UNNON           TO ACVBENEA
004028     END-IF.
004029
004030     IF ACVFORML GREATER ZERO
004031         MOVE AL-UANON           TO ACVFORMA
004032     END-IF.
004033
004034     IF ACVCNDTL GREATER ZERO
004035         MOVE AL-UANON           TO ACVCNDTA
004036     END-IF.
004037
004038     IF ACVEXITL GREATER ZERO
004039         MOVE AL-UANON           TO ACVEXITA
004040     END-IF.
004041
004042     IF ACVSTATL GREATER ZERO
004043         MOVE AL-UANON           TO ACVSTATA
004044     END-IF.
004045
004046     IF LCVDSCRL GREATER ZERO
004047         MOVE AL-SANON           TO LCVDSCRA
004048     END-IF.
004049
004050     IF LCVKINDL GREATER ZERO
004051         MOVE AL-SANON           TO LCVKINDA
004052     END-IF.
004053
004054     IF LCVCDL GREATER ZERO
004055         MOVE AL-UANON           TO LCVCDA
004056     END-IF.
004057
004058     IF LCVOTRML GREATER ZERO
004059         MOVE AL-UNNON           TO LCVOTRMA
004060     END-IF.
004061
004062     IF LCVRTRML GREATER ZERO
004063         MOVE AL-UNNON           TO LCVRTRMA
004064     END-IF.
004065
004066     IF LCVRATEL GREATER ZERO
004067         MOVE AL-UNNON           TO LCVRATEA
004068     END-IF.
004069
004070     IF LCVBENEL GREATER ZERO
004071         MOVE AL-UNNON           TO LCVBENEA
004072     END-IF.
004073
004074     IF LCVFORML GREATER ZERO
004075         MOVE AL-UANON           TO LCVFORMA
004076     END-IF.
004077
004078     IF LCVCNDTL GREATER ZERO
004079         MOVE AL-UANON           TO LCVCNDTA
004080     END-IF.
004081
004082     IF LCVEXITL GREATER ZERO
004083         MOVE AL-UANON           TO LCVEXITA
004084     END-IF.
004085
004086     IF LCVSTATL GREATER ZERO
004087         MOVE AL-UANON           TO LCVSTATA
004088     END-IF.
004089
004090     IF MEMCAPL GREATER ZERO
004091         MOVE AL-UANON           TO MEMCAPA
004092     END-IF.
004093
004094     IF APRL GREATER ZERO
004095         MOVE AL-UNNON           TO APRA
004096     END-IF.
004097
004098     IF PMTFREQL GREATER ZERO
004099         MOVE AL-UNNON           TO PMTFREQA
004100     END-IF.
004101
004102     IF INDGRPL GREATER ZERO
004103         MOVE AL-UANON           TO INDGRPA
004104     END-IF.
004105
004106     IF PREMTYPL GREATER ZERO
004107         MOVE AL-UANON           TO PREMTYPA
004108     END-IF.
004109
004110     IF REINCDL GREATER ZERO
004111         MOVE AL-UANON           TO REINCDA
004112     END-IF.
004113
004114     IF ADDONDTL GREATER ZERO
004115         MOVE AL-UANON           TO ADDONDTA
004116     END-IF.
004117
004118     IF MEMBERL GREATER ZERO
004119         MOVE AL-UANON           TO MEMBERA
004120     END-IF.
004121
004122     IF BCERT1L GREATER ZERO
004123         MOVE AL-SANON           TO BCERT1A
004124                                    BSUFX1A
004125     END-IF.
004126
004127     IF BCERT2L GREATER ZERO
004128         MOVE AL-SANON           TO BCERT2A
004129                                    BSUFX2A
004130     END-IF.
004131
004132     IF BCERT3L GREATER ZERO
004133         MOVE AL-SANON           TO BCERT3A
004134                                    BSUFX3A
004135     END-IF.
004136
004137     IF BCERT4L GREATER ZERO
004138         MOVE AL-SANON           TO BCERT4A
004139                                    BSUFX4A
004140     END-IF.
004141
004142     IF BCERT5L GREATER ZERO
004143         MOVE AL-SANON           TO BCERT5A
004144                                    BSUFX5A
004145     END-IF.
004146
004147 3993-MODIFY-CLAIM-ATTRB.
004148
004149     IF PCERTNOL GREATER ZERO
004150         MOVE AL-UANON           TO PCERTNOA
004151     END-IF.
004152
004153     IF PSUFXL GREATER ZERO
004154         MOVE AL-UANON           TO PSUFXA
004155     END-IF.
004156
004157     IF LSTNMEL GREATER ZERO
004158         MOVE AL-UANON           TO LSTNMEA
004159     END-IF.
004160
004161     IF FSTNMEL GREATER ZERO
004162         MOVE AL-UANON           TO FSTNMEA
004163     END-IF.
004164
004165     IF INITL GREATER ZERO
004166         MOVE AL-UANON           TO INITA
004167     END-IF.
004168
004169
004170     IF SEXL GREATER ZERO
004171         MOVE AL-UANON           TO SEXA
004172     END-IF.
004173
004174     IF BIRTHDTL GREATER ZERO
004175         MOVE AL-UANON           TO BIRTHDTA
004176     END-IF.
004177
004178     IF SSNL GREATER ZERO
004179         MOVE AL-UANON           TO SSNA
004180     END-IF.
004181
004182     IF INCURL GREATER ZERO
004183         MOVE AL-UANON           TO INCURA
004184     END-IF.
004185
004186     IF REPORTL GREATER ZERO
004187         MOVE AL-UANON           TO REPORTA
004188     END-IF.
004189
004190*    IF ESTENDL GREATER ZERO
004191*        MOVE AL-UANON           TO ESTENDA
004192*    END-IF.
004193
004194     IF DIAGL GREATER ZERO
004195         MOVE AL-UANON           TO DIAGA
004196     END-IF.
004197*
004198     IF ICD1L GREATER ZERO
004199         MOVE AL-UANON           TO ICD1A
004200     END-IF.
004201
004202     IF ICD2L GREATER ZERO
004203         MOVE AL-UANON           TO ICD2A
004204     END-IF.
004205
004206*    IF CAUSEL GREATER ZERO
004207*        MOVE AL-UANON           TO CAUSEA
004208*    END-IF.
004209
004210     IF MANRSVL GREATER ZERO
004211         MOVE AL-UNNON           TO MANRSVA
004212     END-IF.
004213
004214     IF RELCLML GREATER ZERO
004215         MOVE AL-UANON           TO RELCLMA
004216     END-IF.
004217
004218     IF LOANNOL GREATER ZERO
004219         MOVE AL-UANON           TO LOANNOA
004220     END-IF.
004221
004222     IF LOANBALL GREATER ZERO
004223         MOVE AL-UNNON           TO LOANBALA
004224     END-IF.
004225
004226     IF PROCCDL GREATER ZERO
004227         MOVE AL-UANON           TO PROCCDA
004228     END-IF.
004229
004230     IF BENECDL GREATER THAN ZERO
004231         MOVE AL-UANON           TO  BENECDA
004232     END-IF.
004233
004234     IF PRICDL GREATER ZERO
004235         MOVE AL-UANON           TO PRICDA
004236     END-IF.
004237
004238     IF SUPVL GREATER ZERO
004239         MOVE AL-UANON           TO SUPVA
004240     END-IF.
004241
004242     IF FILETOL GREATER ZERO
004243         MOVE AL-UANON           TO FILETOA
004244     END-IF.
004245
004246
004247 3993-MODIFY-CLAIM-KEY-ATTRB.
004248
004249     IF CLMNOL GREATER ZERO
004250         MOVE AL-UANON           TO CLMNOA
004251     END-IF.
004252
004253     IF CLMCARRL GREATER ZERO
004254         MOVE AL-UANON           TO CLMCARRA
004255     END-IF.
004256
004257     IF CLMTYPEL GREATER ZERO
004258         MOVE AL-UANON           TO CLMTYPEA
004259     END-IF.
004260
004261     IF CERTNOL GREATER ZERO
004262         MOVE AL-SANON           TO CERTNOA
004263     END-IF.
004264
004265     IF SUFXL GREATER ZERO
004266         MOVE AL-SANON           TO SUFXA
004267     END-IF.
004268
004269 3993-EXIT.
004270     EXIT.
004271
004272     EJECT
004273 3994-BUILD-NINETY-THREE-TRAILER.
004274
004275     MOVE SPACES                 TO ACTIVITY-TRAILERS.
004276     MOVE 'AT'                   TO AT-RECORD-ID.
004277
004278     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
004279     MOVE CLMCARRI               TO AT-CARRIER.
004280
004281     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
004282     MOVE CERT-CERT-NO           TO AT-CERT-NO.
004283     MOVE +93                    TO AT-SEQUENCE-NO.
004284     MOVE '6'                    TO AT-TRAILER-TYPE.
004285     MOVE 'M'                    TO AT-INFO-TRAILER-TYPE.
004286
004287     IF CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'
004288         MOVE WS-VERIFY-NOTE     TO AT-INFO-LINE-1
004289     ELSE
004290         MOVE WS-VFY-SSN-NOTE    TO AT-INFO-LINE-1
004291     END-IF
004292
004293     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
004294                                    AT-GEN-INFO-LAST-MAINT-DT.
004295
004296     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
004297                                    AT-GEN-INFO-LAST-UPDATED-BY.
004298     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
004299
004300
004301     
      * EXEC CICS WRITE
004302*        DATASET   (ELTRLR-DSID)
004303*        FROM      (ACTIVITY-TRAILERS)
004304*        RIDFLD    (AT-CONTROL-PRIMARY)
004305*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010790' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130373930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004306
004307 3994-EXIT.
004308     EXIT.
004309
004310 3994B-BUILD-NINETY-FOUR-TRAILER.
004311
004312     MOVE SPACES                 TO ACTIVITY-TRAILERS.
004313     MOVE 'AT'                   TO AT-RECORD-ID.
004314
004315     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
004316     MOVE CLMCARRI               TO AT-CARRIER.
004317
004318     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
004319     MOVE CERT-CERT-NO           TO AT-CERT-NO.
004320     MOVE +94                    TO AT-SEQUENCE-NO.
004321     MOVE '6'                    TO AT-TRAILER-TYPE.
004322     MOVE 'M'                    TO AT-INFO-TRAILER-TYPE.
004323
004324     MOVE WS-CAUSAL-NOTE         TO AT-INFO-LINE-1.
004325
004326     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
004327                                    AT-GEN-INFO-LAST-MAINT-DT.
004328
004329     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
004330                                    AT-GEN-INFO-LAST-UPDATED-BY.
004331     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
004332
004333
004334     
      * EXEC CICS WRITE
004335*        DATASET   (ELTRLR-DSID)
004336*        FROM      (ACTIVITY-TRAILERS)
004337*        RIDFLD    (AT-CONTROL-PRIMARY)
004338*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010823' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130383233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004339
004340 3994B-EXIT.
004341     EXIT.
004342
004343 3994C-BUILD-NINETY-FIVE-TRAILER.
004344
004345     MOVE SPACES                 TO ACTIVITY-TRAILERS
004346     MOVE 'AT'                   TO AT-RECORD-ID
004347
004348     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD
004349     MOVE CLMCARRI               TO AT-CARRIER
004350
004351     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO
004352     MOVE CERT-CERT-NO           TO AT-CERT-NO
004353     MOVE +95                    TO AT-SEQUENCE-NO
004354     MOVE '6'                    TO AT-TRAILER-TYPE
004355     MOVE 'E'                    TO AT-INFO-TRAILER-TYPE
004356
004357     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT
004358                                    AT-GEN-INFO-LAST-MAINT-DT
004359
004360     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
004361                                    AT-GEN-INFO-LAST-UPDATED-BY
004362     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
004363
004364     perform varying e1 from +1 by +1 until
004365        ws-error-no (e1) = spaces
004366        move ws-error-no (e1)    to at-note-error-no (e1)
004367     end-perform
004368
004369     if at-info-line-1 not = spaces
004370        
      * EXEC CICS WRITE
004371*          DATASET  (ELTRLR-DSID)
004372*          FROM     (ACTIVITY-TRAILERS)
004373*          RIDFLD   (AT-CONTROL-PRIMARY)
004374*       END-EXEC
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00010859' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303130383539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004375     end-if
004376
004377     .
004378 3994C-EXIT.
004379     EXIT.
004380
004381 3995-BUILD-CLAIM-RECORD.
004382
004383     IF PROCCDL GREATER ZERO
004384         MOVE PROCCDI            TO CL-PROCESSOR-ID
004385     ELSE
004386         MOVE PI-PROCESSOR-ID    TO CL-PROCESSOR-ID
004387     END-IF.
004388
004389     IF FSTNMEL GREATER ZERO
004390         MOVE FSTNMEI            TO CL-INSURED-1ST-NAME
004391     END-IF.
004392
004393     IF INITL GREATER ZERO
004394         MOVE INITI              TO CL-INSURED-MID-INIT
004395     END-IF.
004396
004397     IF BIRTHDTL GREATER ZERO
004398         MOVE WS-BIRTHDT         TO CL-INSURED-BIRTH-DT
004399     END-IF.
004400
004401     IF SEXL GREATER ZERO
004402         MOVE SEXI               TO CL-INSURED-SEX-CD
004403     END-IF.
004404
004405     MOVE 'O'                    TO CL-CLAIM-STATUS.
004406
004407*    IF CLMTYPEL GREATER ZERO
004408     MOVE CLMTYPEI               TO CL-CLAIM-TYPE.
004409*    END-IF.
004410
004411     IF PREMTYPL GREATER ZERO
004412         MOVE PREMTYPI           TO CL-CLAIM-PREM-TYPE
004413     END-IF.
004414
004415     IF INCURL GREATER ZERO
004416         MOVE WS-INCUR           TO CL-INCURRED-DT
004417     END-IF.
004418
004419     IF (PI-COMPANY-ID = 'DCC' OR 'VPP')
004420        AND (ERPDEF-FOUND)
004421        MOVE WS-MAX-BENEFITS     TO CL-CRITICAL-PERIOD
004422*       MOVE WS-CRIT-PER-RECURRENT
004423*                                TO CL-CRIT-PER-RECURRENT
004424*       MOVE WS-CRIT-PER-RTW-MOS TO CL-CRIT-PER-RTW-MOS
004425        if ws-crit-per-recurrent > 01
004426           move zeros            to cl-benefit-period
004427        end-if
004428     ELSE
004429        IF CL-CLAIM-TYPE = 'L' OR 'P' OR 'O'
004430           MOVE CM-LF-CRITICAL-PERIOD
004431                                 TO CL-CRITICAL-PERIOD
004432        ELSE
004433           MOVE CM-AH-CRITICAL-PERIOD
004434                                 TO CL-CRITICAL-PERIOD
004435        END-IF
004436     END-IF
004437
004438     IF CL-CRITICAL-PERIOD NOT = ZEROS
004439        COMPUTE DC-ELAPSED-MONTHS =
004440           CL-CRITICAL-PERIOD - WS-BENEFITS-PREV-PAID
004441        MOVE CL-INCURRED-DT      TO DC-BIN-DATE-1
004442        MOVE ZEROS               TO DC-ELAPSED-DAYS
004443        MOVE '6'                 TO DC-OPTION-CODE
004444        PERFORM 9700-LINK-DATE-CONVERT
004445                                 THRU 9700-EXIT
004446        MOVE DC-BIN-DATE-2       TO CL-BENEFIT-EXPIRATION-DT
004447     END-IF
004448
004449     if instypel > zero
004450        move instypei            to cl-insured-type
004451     end-if
004452     IF REPORTL GREATER ZERO
004453         MOVE WS-REPORT          TO CL-REPORTED-DT
004454     END-IF.
004455
004456     IF EIBAID = DFHPF6
004457         IF WS-REPORT = LOW-VALUES  OR   REPORTL = ZEROS
004458             MOVE WS-TODAY-DT    TO CL-REPORTED-DT
004459                 IF EMI-FORCABLE-CTR = +1
004460                     MOVE ZEROS  TO CL-FORCEABLE-ERROR-CNT
004461                 END-IF
004462         END-IF
004463     END-IF.
004464
004465*    IF ESTENDL GREATER ZERO
004466*       MOVE WS-ESTEND           TO CL-EST-END-OF-DISAB-DT
004467*    END-IF.
004468
004469     IF ADDONDTL GREATER ZERO
004470         MOVE WS-ADD-ON-DT   TO CL-LAST-ADD-ON-DT
004471     END-IF.
004472
004473*    IF CAUSEL GREATER ZERO
004474*        MOVE CAUSEI             TO CL-CAUSE-CD
004475*    END-IF.
004476
004477     IF SSNL GREATER ZERO
004478         MOVE SSNI               TO CL-SOC-SEC-NO
004479     ELSE
004480         MOVE CERT-STATE         TO CL-SSN-STATE
004481         MOVE CERT-ACCOUNT-PRIME TO CL-SSN-ACCOUNT
004482         MOVE CL-INSURED-LAST-NAME TO CL-SSN-LN3
004483     END-IF.
004484
004485     IF PRICDL GREATER ZERO
004486         MOVE PRICDI             TO CL-PRIORITY-CD
004487     END-IF.
004488
004489     IF SUPVL GREATER ZERO
004490         MOVE SUPVI              TO CL-SUPV-ATTN-CD
004491     END-IF.
004492
004493     MOVE WS-TODAY-DT            TO CL-LAST-MAINT-DT.
004494     MOVE CL-PROCESSOR-ID        TO CL-LAST-MAINT-USER.
004495     MOVE EIBTIME                TO CL-LAST-MAINT-HHMMSS.
004496
004497     IF RELCLML GREATER ZERO
004498         MOVE RELCLMI            TO CL-RELATED-CLAIM-NO
004499     END-IF.
004500
004501     IF FILETOL GREATER ZERO
004502         MOVE FILETOI            TO CL-FILE-LOCATION
004503     END-IF.
004504
004505 3995-EXIT.
004506      EXIT.
004507
004508 3996-read-cert-claim-trailer.
004509
004510     move cm-ah-benefit-amt      to ws-monthly-benefit
004511     if (pi-dcc-max-amt < ws-monthly-benefit)
004512        and (pi-dcc-max-amt not = zeros)
004513        move pi-dcc-max-amt      to ws-monthly-benefit
004514     end-if
004515
004516     MOVE CM-COMPANY-CD          TO CTRLR-COMP-CD
004517     MOVE CM-CARRIER             TO CTRLR-CARRIER
004518     MOVE CM-GROUPING            TO CTRLR-GROUPING
004519     MOVE CM-STATE               TO CTRLR-STATE
004520     MOVE CM-ACCOUNT             TO CTRLR-ACCOUNT
004521     MOVE CM-CERT-EFF-DT         TO CTRLR-EFF-DT
004522     MOVE CM-CERT-NO             TO CTRLR-CERT-NO
004523     MOVE 'B'                    TO CTRLR-REC-TYPE
004524     
      * EXEC CICS READ
004525*       DATASET  (ELCRTT-DSID)
004526*       set     (address of CERTIFICATE-TRAILERS)
004527*       RIDFLD   (ELCRTT-KEY)
004528*       RESP     (WS-RESPONSE)
004529*    END-EXEC
      *    MOVE '&"S        E          (  N#00011013' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303131303133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
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
004530
004531***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
004532***                                                            ***
004533***  This may need some work here. I think I should accumulate ***
004534***  all the claims with the same claim type?????????          ***
004535***                                                            ***
004536***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
004537     IF WS-RESP-NORMAL
004538        move zeros to ws-tot-days-paid ws-tot-amt-paid
004539        perform varying s1 from +1 by +1 until
004540           (s1 > +24)
004541           or (cs-claim-no (s1) = spaces)
004542           if cs-claim-type (s1) = clmtypei
004543              move er-1659       to ws-error-no (e1)
004544              compute ws-tot-days-paid =
004545                 ws-tot-days-paid + cs-days-paid (s1)
004546              compute ws-tot-amt-paid =
004547                 ws-tot-amt-paid + cs-total-paid (s1)
004548           end-if
004549        end-perform
004550*        compute ws-pd-bens rounded = ws-tot-days-paid / 30
004551*        if (ws-pd-bens >= pi-dcc-max-benefits)
004552*           and (pi-dcc-max-benefits not = zeros)
004553*           add +1                to e1
004554*           MOVE ER-1660          TO EMI-ERROR
004555*                                    ws-error-no (e1)
004556*           PERFORM 9900-ERROR-FORMAT
004557*                                 THRU 9900-EXIT
004558*        end-if
004559     end-if
004560
004561**       if instypei = 'P'
004562**          move +1 to s1
004563**       else
004564**          MOVE +2 to s1
004565**       end-if
004566*
004567*        move +1 to s1
004568*        evaluate clmtypei
004569*           when 'A'
004570*              move +1 to s2
004571*           when 'I'
004572*              move +2 to s2
004573*           when 'G'
004574*              move +3 to s2
004575*           when 'L'
004576*              move +4 to s2
004577*           when 'P'
004578*              move +5 to s2
004579*        end-evaluate
004580*        if cs-claim-type (s1 s2) = clmtypei
004581*           if cs-no-of-claims (s1 s2) > zeros
004582*              move er-1659       to ws-error-no (e1)
004583*           end-if
004584*           compute WS-BENEFITS-PREV-PAID =
004585*              cs-total-paid (s1 s2) / ws-monthly-benefit
004586*           if (WS-BENEFITS-PREV-PAID >= pi-dcc-max-benefits)
004587*              and (pi-dcc-max-benefits not = zeros)
004588*              add +1             to e1
004589*              MOVE ER-1660       TO EMI-ERROR
004590*                                    ws-error-no (e1)
004591*              PERFORM 9900-ERROR-FORMAT
004592*                                 THRU 9900-EXIT
004593*           end-if
004594*        end-if
004595*     end-if
004596
004597     .
004598 3996-exit.
004599     exit.
004600
004601 3997-GET-ERPDEF.
004602
004603     if cm-clp-state = spaces or low-values or zeros
004604        move cm-state            to cm-clp-state
004605     end-if
004606     MOVE SPACES                 TO WS-ERPDEF-SW
004607     MOVE ZEROS                  TO WS-MAX-BENEFITS
004608                                    WS-CRIT-PER-RTW-MOS
004609                                    WS-CRIT-PER-RECURRENT
004610
004611     MOVE PI-COMPANY-CD       TO ERPDEF-KEY
004612     MOVE CM-clp-state        TO ERPDEF-STATE
004613     MOVE ws-dcc-product-code TO ERPDEF-PROD-CD
004614
004615***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
004616***                                                            ***
004617***  If they set up a claim where there is no coverage on the  ***
004618***  addendum with the intention of denying.  We need to do    ***
004619***  some funky stuff here.                                    ***
004620***                                                            ***
004621***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
004622
004623     evaluate true
004624        when (clmtypei = 'L' or 'P')
004625           and (cm-lf-benefit-cd not = '00' and '  ' and 'DD'
004626              and 'CU')
004627           move 'L'              to erpdef-ben-type
004628           move cm-lf-benefit-cd to erpdef-ben-code
004629        when (clmtypei not = 'L' and 'P')
004630           and (cm-ah-benefit-cd not = '00' and '  ')
004631           move 'A'              to erpdef-ben-type
004632           move cm-ah-benefit-cd to erpdef-ben-code
004633        when (clmtypei not = 'L' and 'P')
004634           and (cm-ah-benefit-cd = '00' or '  ')
004635           move 'L'              to erpdef-ben-type
004636           move cm-lf-benefit-cd to erpdef-ben-code
004637        when (clmtypei = 'L' or 'P')
004638           and (cm-lf-benefit-cd = '00' or '  ' or 'DD' or 'CU')
004639           move 'A'              to erpdef-ben-type
004640           move cm-ah-benefit-cd to erpdef-ben-code
004641        when other
004642           move 'A'              to erpdef-ben-type
004643           move cm-ah-benefit-cd to erpdef-ben-code
004644     end-evaluate
004645
004646     move cl-insured-birth-dt    to dc-bin-date-1
004647     move cl-incurred-dt         to dc-bin-date-2
004648     move '1'                    to dc-option-code
004649     PERFORM 9700-LINK-DATE-CONVERT
004650                                 THRU 9700-EXIT
004651     compute ws-att-age =
004652        dc-elapsed-months / 12
004653
004654     move zeros                  to dc-elapsed-months
004655                                    dc-elapsed-days
004656     move low-values to dc-bin-date-1 dc-bin-date-2
004657
004658*    MOVE 'A'                 TO ERPDEF-BEN-TYPE
004659*    MOVE CM-AH-BENEFIT-CD    TO ERPDEF-BEN-CODE
004660     MOVE CM-CERT-EFF-DT      TO ERPDEF-EXP-DT
004661     MOVE ERPDEF-KEY          TO ERPDEF-KEY-SAVE
004662
004663     
      * EXEC CICS STARTBR
004664*        DATASET  ('ERPDEF')
004665*        RIDFLD   (ERPDEF-KEY)
004666*        GTEQ
004667*        RESP     (WS-RESPONSE)
004668*    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00011152' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303131313532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004669
004670     IF WS-RESP-NORMAL
004671        
      * EXEC CICS READNEXT
004672*          DATASET  ('ERPDEF')
004673*          INTO     (PRODUCT-MASTER)
004674*          RIDFLD   (ERPDEF-KEY)
004675*          RESP     (WS-RESPONSE)
004676*       END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV12
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00011160' TO DFHEIV0
           MOVE X'262E494C2020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303131313630' TO DFHEIV0
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
004677
004678        IF WS-RESP-NORMAL
004679           IF (ERPDEF-KEY-SAVE (1:16) =
004680              PD-CONTROL-PRIMARY (1:16))
004681              AND (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
004682
004683              PERFORM VARYING A1 FROM +1 BY +1 UNTIL
004684                 (A1 > +11)
004685                 OR ((PD-PROD-CODE (A1) = CLMTYPEI)
004686                   AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
004687              END-PERFORM
004688              IF A1 < +12
004689                 SET ERPDEF-FOUND TO TRUE
004690                 move pd-max-amt (a1)
004691                              to pi-dcc-max-amt
004692                 MOVE PD-CRIT-PERIOD (A1)
004693                              TO WS-MAX-BENEFITS
004694                                 pi-dcc-max-benefits
004695                 if pd-rec-crit-period (a1) not numeric
004696                    move 01      to pd-rec-crit-period (a1)
004697                 end-if
004698                 MOVE PD-REC-CRIT-PERIOD (A1)
004699                              TO WS-CRIT-PER-RECURRENT
004700                 IF PD-RTW-MOS (A1) NUMERIC
004701                    MOVE PD-RTW-MOS (A1)
004702                              TO WS-CRIT-PER-RTW-MOS
004703                 ELSE
004704                    MOVE 0    TO WS-CRIT-PER-RTW-MOS
004705                 END-IF
004706                 IF PD-EXCLUSION-PERIOD-DAYS (A1) NUMERIC
004707                    MOVE PD-EXCLUSION-PERIOD-DAYS (A1)
004708                                 TO WS-EXCL-PERIOD
004709                 END-IF
004710                 IF PD-COVERAGE-ENDS-MOS (A1) NUMERIC
004711                    MOVE PD-COVERAGE-ENDS-MOS (A1)
004712                                 TO WS-COV-ENDS
004713                 END-IF
004714                 IF PD-ACCIDENT-ONLY-MOS (A1) NUMERIC
004715                    MOVE PD-ACCIDENT-ONLY-MOS (A1)
004716                                 TO WS-ACC-PERIOD
004717                 END-IF
004718                 IF PD-PRE-EXIST-EXCL-TYPE (A1) NUMERIC
004719                    MOVE PD-PRE-EXIST-EXCL-TYPE (A1)
004720                                 TO WS-PRE-EXISTING-PER
004721                 END-IF
004722              else
004723                 MOVE -1         TO MAINTL
004724                 move er-1673    to emi-error
004725                 PERFORM 9900-ERROR-FORMAT
004726                                 THRU 9900-EXIT
004727              END-IF
004728           else
004729              MOVE -1            TO MAINTL
004730              move er-1671       to emi-error
004731              PERFORM 9900-ERROR-FORMAT
004732                                 THRU 9900-EXIT
004733           END-IF
004734        END-IF
004735     END-IF
004736
004737     .
004738 3997-EXIT.
004739     EXIT.
004740
004741 3998-update-cert-claim-trailer.
004742
004743     
      * EXEC CICS GETMAIN
004744*       SET      (ADDRESS OF CERTIFICATE-TRAILERS)
004745*       LENGTH   (ELCRTT-LENGTH)
004746*       INITIMG  (GETMAIN-SPACE)
004747*    END-EXEC
      *    MOVE ',"IL                  $   #00011232' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131323332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCRTT-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004748
004749     MOVE CM-COMPANY-CD    TO CTRLR-COMP-CD
004750     MOVE CM-CARRIER       TO CTRLR-CARRIER
004751     MOVE CM-GROUPING      TO CTRLR-GROUPING
004752     MOVE CM-STATE         TO CTRLR-STATE
004753     MOVE CM-ACCOUNT       TO CTRLR-ACCOUNT
004754     MOVE CM-CERT-EFF-DT   TO CTRLR-EFF-DT
004755     MOVE CM-CERT-NO       TO CTRLR-CERT-NO
004756     MOVE 'B'              TO CTRLR-REC-TYPE
004757     
      * EXEC CICS READ
004758*       UPDATE
004759*       DATASET  (ELCRTT-DSID)
004760*       into     (CERTIFICATE-TRAILERS)
004761*       RIDFLD   (ELCRTT-KEY)
004762*       RESP     (WS-RESPONSE)
004763*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00011246' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'204E233030303131323436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004764     IF WS-RESP-NORMAL
004765        perform varying s1 from +1 by +1 until
004766           (s1 > +24)
004767           or (cs-claim-no (s1) = spaces)
004768        end-perform
004769        if s1 < +25
004770           perform 3999-upd-crt-trlr thru 3999-exit
004771           
      * EXEC CICS REWRITE
004772*             DATASET  (ELCRTT-DSID)
004773*             from     (CERTIFICATE-TRAILERS)
004774*             RESP     (WS-RESPONSE)
004775*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00011260' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'204E233030303131323630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004776        end-if
004777     else
004778        if ws-resp-notfnd
004779           move 'CS'             to certificate-trailers
004780           move cm-control-primary to cs-control-primary
004781           move 'B'              to cs-trailer-type
004782           perform varying s1 from +1 by +1 until s1 > +24
004783              move zeros         to cs-days-paid (s1)
004784                                    cs-total-paid (s1)
004785                                    cs-benefit-period (s1)
004786                                    cs-remaining-bens (s1)
004787           end-perform
004788           move +1               to s1
004789           perform 3999-upd-crt-trlr
004790                                 thru 3999-exit
004791           
      * EXEC CICS WRITE
004792*             DATASET  (ELCRTT-DSID)
004793*             from     (CERTIFICATE-TRAILERS)
004794*             RIDFLD   (cs-control-primary)
004795*             RESP     (WS-RESPONSE)
004796*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00011280' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303131323830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 cs-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004797        end-if
004798     END-IF
004799
004800     .
004801 3998-exit.
004802     exit.
004803
004804 3999-upd-crt-trlr.
004805
004806     move clmnoi                 to cs-claim-no       (s1)
004807     move clmtypei               to cs-claim-type     (s1)
004808     move instypei               to cs-insured-type   (s1)
004809
004810     if ws-crit-per-recurrent > 01
004811        move zeros               to cs-benefit-period(s1)
004812     else
004813        move 01                  to cs-benefit-period(s1)
004814     end-if
004815
004816*    move cl-benefit-period      to cs-benefit-period (s1)
004817*    move cl-critical-period     to cs-remaining-bens (s1)
004818     move zeros                  to cs-remaining-bens (s1)
004819
004820     .
004821 3999-exit.
004822     exit.
004823
004824 4000-CALCULATE-CERT-TERM.
004825
004826     PERFORM 4100-CALC-GROSS-TERM THRU 4200-EXIT.
004827
004828     MOVE N                      TO WS-TERM.
004829
004830     IF WS-TERM GREATER 120
004831         MOVE 120                TO WS-TERM
004832     END-IF.
004833
004834 4000-CALC-TERM-EXIT.
004835      EXIT.
004836
004837 4100-CALC-GROSS-TERM.
004838     MOVE HOLD-LOAN-BAL           TO L.
004839     MOVE WS-ACVBENE              TO M.
004840     COMPUTE N = L / M.
004841     IF N LESS 1
004842         MOVE 1  TO N
004843     END-IF.
004844
004845     IF LCVCDL GREATER ZERO
004846         COMPUTE WS-LF-RATE = WS-LCVRATE / +1000
004847     ELSE
004848         MOVE ZEROS               TO WS-LF-RATE
004849     END-IF.
004850
004851     IF ACVCDL GREATER ZERO
004852         COMPUTE WS-AH-RATE = WS-ACVRATE / +1000
004853     ELSE
004854         MOVE ZEROS               TO WS-AH-RATE
004855     END-IF.
004856
004857     COMPUTE I = WS-APR / +1200.
004858
004859 4105-LOOP.
004860     IF N GREATER 240
004861         GO TO 4200-EXIT
004862     END-IF.
004863
004864     PERFORM 4210-CALC-LEFT-RIGHTONE THRU 4220-EXIT.
004865
004866     IF LEFT-TOT-1 GREATER RIGHT-TOT-1
004867         ADD 1 TO N
004868         GO TO 4105-LOOP
004869     END-IF.
004870
004871 4200-EXIT.
004872      EXIT.
004873
004874 4210-CALC-LEFT-RIGHTONE.
004875      MOVE L         TO LEFT-TOT-1.
004876      SUBTRACT 1 FROM N.
004877      PERFORM 4300-CALC-A-N THRU 4300-EXIT.
004878      PERFORM 4350-CALC-IA-N THRU 4400-EXIT.
004879      ADD 1 TO N.
004880
004881 4211-CALC-TERM1.
004882      MOVE N         TO TERM1.
004883      MULTIPLY M BY TERM1.
004884
004885 4212-LOOP.
004886      COMPUTE TERM1 = (WS-AH-RATE + WS-LF-RATE) * TERM1.
004887      ADD 1 TO A-N.
004888      MULTIPLY A-N BY TERM1.
004889      SUBTRACT 1 FROM A-N.
004890      ADD TERM1 TO LEFT-TOT-1.
004891
004892 4213-CALC-TERM2.
004893      MOVE M         TO TERM2.
004894      COMPUTE TERM2 = (WS-AH-RATE + WS-LF-RATE) * TERM2.
004895      MULTIPLY IA-N BY TERM2.
004896      SUBTRACT TERM2 FROM LEFT-TOT-1.
004897
004898 4215-CALC-RIGHTONE.
004899      MOVE M         TO RIGHT-TOT-1.
004900      PERFORM 4300-CALC-A-N THRU 4300-EXIT.
004901      MULTIPLY A-N BY RIGHT-TOT-1.
004902
004903 4220-EXIT.
004904      EXIT.
004905
004906 4300-CALC-A-N.
004907     IF N LESS 1
004908         MOVE 0     TO A-N
004909         GO TO 4300-EXIT
004910     END-IF.
004911
004912     IF I = 0
004913         MOVE .00001 TO I
004914     END-IF.
004915
004916     ADD 1 TO I.
004917     DIVIDE I INTO 1 GIVING V.
004918     SUBTRACT 1 FROM I.
004919     PERFORM 4450-CALC-V-EX-N THRU 4490-EXIT.
004920     SUBTRACT V-EX-N FROM 1 GIVING TERM3.
004921     DIVIDE I INTO TERM3 GIVING A-N.
004922
004923 4300-EXIT.
004924      EXIT.
004925
004926 4350-CALC-IA-N.
004927     IF N LESS 1
004928         MOVE 0      TO IA-N
004929         GO TO 4400-EXIT
004930     END-IF.
004931
004932     ADD 1 TO N.
004933     PERFORM 4450-CALC-V-EX-N THRU 4490-EXIT.
004934     SUBTRACT 1 FROM N.
004935     MULTIPLY N BY V-EX-N GIVING TERM3.
004936     SUBTRACT TERM3 FROM A-N GIVING TERM3.
004937     SUBTRACT V FROM 1 GIVING TERM4.
004938     DIVIDE TERM4 INTO TERM3 GIVING IA-N.
004939
004940 4400-EXIT.
004941      EXIT.
004942
004943 4450-CALC-V-EX-N.
004944     IF N LESS 1
004945         MOVE 1    TO V-EX-N
004946         GO TO 4490-EXIT
004947     END-IF.
004948
004949     MOVE N        TO NV-STORE.
004950
004951     IF V-EX-ONETIME = 1  OR
004952        V NOT = V-EXPONENT (1)
004953          PERFORM 4470-BUILD-V-EX-TABLE THRU 4480-EXIT
004954     END-IF.
004955
004956 4460-LOOP.
004957     IF N GREATER 248
004958         MOVE 248 TO N
004959     END-IF.
004960
004961     IF N = 1
004962         MOVE V               TO V-EX-N
004963     ELSE
004964         MOVE V-EXPONENT (N)  TO V-EX-N
004965     END-IF.
004966
004967     GO TO 4490-EXIT.
004968
004969 4470-BUILD-V-EX-TABLE.
004970     MOVE 2      TO N.
004971     MOVE V      TO V-EXPONENT (1)
004972                    V-EX-N.
004973
004974 4471-LOOP.
004975     MULTIPLY V BY V-EX-N.
004976     MOVE V-EX-N   TO V-EXPONENT (N).
004977
004978     ADD 1 TO N.
004979     IF N LESS 248
004980         GO TO 4471-LOOP
004981     END-IF.
004982
004983     MOVE NV-STORE     TO N.
004984     MOVE ZERO         TO V-EX-ONETIME.
004985
004986 4480-EXIT.
004987      EXIT.
004988
004989 4490-EXIT.
004990      EXIT.
004991
004992 4500-CALC-NET-TERM.
004993     MOVE HOLD-LOAN-BAL           TO L.
004994     MOVE WS-ACVBENE              TO M.
004995     DIVIDE L BY M GIVING N REMAINDER WS-REMAIN.
004996
004997     IF WS-REMAIN GREATER ZERO
004998         ADD 1 TO N
004999     END-IF.
005000
005001     IF N = 0
005002         MOVE 1 TO N
005003     END-IF.
005004
005005 4700-EXIT.
005006      EXIT.
005007
005008 4999-CALC-TERM-EXIT.
005009     EXIT.
005010
005011     EJECT
005012 5000-INCURRED-CHANGE.
005013     IF NOT MODIFY-CAP
005014         MOVE ER-0070            TO EMI-ERROR
005015         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005016         MOVE -1                 TO MAINTL
005017         GO TO 8200-SEND-DATAONLY
005018     END-IF.
005019
005020     IF CLMNOI       NOT = PI-LAST-CLAIM       OR
005021        CLMCARRI     NOT = PI-LAST-CARR        OR
005022        CERTNOI      NOT = PI-LAST-CERT-PRIME  OR
005023        SUFXI        NOT = PI-LAST-CERT-SUFX   OR
005024        PI-INCURR-SW NOT = 'Y'
005025          GO TO 1000-SHOW-CLAIM
005026     END-IF.
005027
005028     PERFORM 6000-EDIT-CLAIM-DATA THRU 6000-EXIT.
005029     PERFORM 6200-EDIT-CERT-DATA  THRU 6200-EXIT.
005030     PERFORM 6400-TEST-CLAIM-REASONABILITY THRU 6499-EXIT.
005031
005032     IF EMI-NO-ERRORS
005033         GO TO 5000-TRY-TO-BUILD
005034     END-IF.
005035
005036     IF EIBAID = DFHPF6  AND NOT FORCE-CAP
005037        MOVE ER-0433             TO EMI-ERROR
005038        MOVE -1                  TO MAINTL
005039        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005040        GO TO 8200-SEND-DATAONLY
005041     END-IF.
005042
005043     IF EIBAID = DFHPF6
005044         IF EMI-FATAL-CTR NOT EQUAL ZERO
005045            MOVE ER-0434         TO EMI-ERROR
005046            MOVE -1              TO MAINTL
005047            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005048            GO TO 8200-SEND-DATAONLY
005049         ELSE
005050            MOVE +1              TO INCURL
005051            GO TO 5000-TRY-TO-BUILD
005052         END-IF
005053     END-IF.
005054
005055     GO TO 8200-SEND-DATAONLY.
005056
005057 5000-TRY-TO-BUILD.
005058
005059     MOVE PI-COMPANY-CD          TO MSTR-COMP-CD.
005060
005061     MOVE CLMCARRI               TO MSTR-CARRIER.
005062
005063     MOVE CLMNOI                 TO MSTR-CLAIM-NO.
005064     MOVE CERTNOI                TO MSTR-CERT-NO-PRIME.
005065     MOVE SUFXI                  TO MSTR-CERT-NO-SUFX.
005066
005067     PERFORM 7920-READ-CLAIM-UPDATE THRU 7920-EXIT.
005068
005069     IF CL-CLAIM-PAYMENT-STATUS NOT NUMERIC
005070         MOVE ZEROS              TO CL-CLAIM-PAYMENT-STATUS
005071     END-IF.
005072
005073     IF CL-PURGED-DT NOT = LOW-VALUES
005074
005075         
      * EXEC CICS UNLOCK
005076*            DATASET   (ELMSTR-DSID)
005077*        END-EXEC
      *    MOVE '&*                    #   #00011564' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131353634' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005078         MOVE ER-7691            TO EMI-ERROR
005079         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005080         MOVE -1                 TO MAINTL
005081         GO TO 8200-SEND-DATAONLY
005082     END-IF.
005083
005084     IF CL-AUTO-PAY-SEQ NOT = ZERO
005085
005086        
      * EXEC CICS UNLOCK
005087*            DATASET   (ELMSTR-DSID)
005088*       END-EXEC
      *    MOVE '&*                    #   #00011575' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303131353735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005089         MOVE ER-0523             TO EMI-ERROR
005090         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005091         MOVE -1                  TO MAINTL
005092         GO TO 8200-SEND-DATAONLY
005093     END-IF.
005094
005095     PERFORM 7900-READ-ACTV-UPDATE THRU 7900-EXIT.
005096
005097     MOVE AT-ITD-PAID-EXPENSES       TO WS-ITD-PAID-EXPENSE.
005098     MOVE AT-ITD-CHARGEABLE-EXPENSE  TO WS-ITD-CHARGABLE-EXPENSE.
005099     MOVE AT-INITIAL-MANUAL-RESERVE  TO WS-INITIAL-MANUAL-RESERVE.
005100     MOVE AT-CURRENT-MANUAL-RESERVE  TO WS-CURRENT-MANUAL-RESERVE.
005101     MOVE AT-ITD-ADDITIONAL-RESERVE  TO WS-ITD-ADDITIONAL-RESERVE.
005102
005103     IF MANRSVL GREATER ZERO
005104        MOVE WS-MANRSV           TO AT-INITIAL-MANUAL-RESERVE
005105                                    AT-CURRENT-MANUAL-RESERVE
005106     END-IF.
005107
005108     MOVE 1                      TO SUB.
005109
005110 5010-LOOP.
005111     IF SUB = 1
005112        MOVE WS-TODAY-DT         TO AT-OPEN-CLOSE-DATE   (SUB)
005113        MOVE 'O'                 TO AT-OPEN-CLOSE-TYPE   (SUB)
005114        MOVE 'NEW'               TO AT-OPEN-CLOSE-REASON (SUB)
005115     ELSE
005116        MOVE LOW-VALUES          TO AT-OPEN-CLOSE-DATE   (SUB)
005117        MOVE SPACES              TO AT-OPEN-CLOSE-TYPE   (SUB)
005118                                    AT-OPEN-CLOSE-REASON (SUB)
005119     END-IF.
005120
005121     IF SUB NOT = 6
005122        ADD 1                    TO SUB
005123        GO TO 5010-LOOP
005124     END-IF.
005125
005126 5020-UPDATE.
005127
005128     PERFORM 7915-REWRITE-TRAILER THRU 7915-EXIT.
005129
005130     IF DIAGL = ZERO
005131       AND ICD1L = ZERO
005132       AND ICD2L = ZERO
005133         PERFORM 7910-READ-NINETY THRU 7910-EXIT
005134         MOVE AT-INFO-LINE-1     TO WS-DIAGNOSIS-DESCRIPT
005135         MOVE AT-ICD-CODE-1      TO WS-ICD-CODE-1
005136         MOVE AT-ICD-CODE-2      TO WS-ICD-CODE-2
005137         GO TO 5025-BYPASS-UPDATE
005138     END-IF.
005139
005140     PERFORM 7900-READ-NINETY-UPDATE THRU 7900-EXIT.
005141
005142     MOVE AT-INFO-LINE-1         TO WS-DIAGNOSIS-DESCRIPT.
005143     IF DIAGL NOT = ZERO
005144         MOVE DIAGI              TO AT-INFO-LINE-1
005145     END-IF.
005146
005147     MOVE AT-ICD-CODE-1          TO WS-ICD-CODE-1.
005148     IF ICD1L NOT = ZERO
005149         MOVE ICD1I              TO AT-ICD-CODE-1
005150     END-IF.
005151
005152     MOVE AT-ICD-CODE-2          TO WS-ICD-CODE-2.
005153     IF ICD2L NOT = ZERO
005154         MOVE ICD2I              TO AT-ICD-CODE-2
005155     END-IF.
005156
005157     PERFORM 7915-REWRITE-TRAILER THRU 7915-EXIT.
005158
005159 5025-BYPASS-UPDATE.
005160
005161
005162     
      * EXEC CICS GETMAIN
005163*        SET      (ADDRESS OF ACTIVITY-TRAILERS)
005164*        LENGTH   (ELTRLR-LENGTH)
005165*    END-EXEC.
      *    MOVE '," L                  $   #00011651' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131363531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELTRLR-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005166
005167     MOVE SPACES                 TO ACTIVITY-TRAILERS.
005168     MOVE 'AT'                   TO AT-RECORD-ID.
005169
005170     MOVE PI-COMPANY-CD          TO AT-COMPANY-CD.
005171     MOVE MSTR-CARRIER           TO AT-CARRIER.
005172     MOVE MSTR-CLAIM-NO          TO AT-CLAIM-NO.
005173     MOVE MSTR-CERT-NO           TO AT-CERT-NO.
005174     MOVE CL-TRAILER-SEQ-CNT     TO AT-TRAILER-CNT-AT-CHG.
005175     SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
005176     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.
005177     MOVE '9'                    TO AT-TRAILER-TYPE.
005178     MOVE EIBDATE                TO DC-JULIAN-YYDDD.
005179     MOVE '5'                    TO DC-OPTION-CODE.
005180     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
005181     MOVE DC-BIN-DATE-1          TO AT-RECORDED-DT.
005182
005183     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY.
005184     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
005185
005186     MOVE CL-INCURRED-DT         TO AT-OLD-INCURRED-DT
005187                                    ws-prev-inc-dt
005188     MOVE CL-REPORTED-DT         TO AT-OLD-REPORTED-DT.
005189     MOVE CL-FILE-ESTABLISH-DT   TO AT-OLD-ESTABLISHED-DT.
005190     MOVE CL-TOTAL-PAID-AMT      TO AT-OLD-TOTAL-PAID.
005191     MOVE CL-NO-OF-DAYS-PAID     TO AT-OLD-DAYS-PAID.
005192     MOVE CL-NO-OF-PMTS-MADE     TO AT-OLD-NO-OF-PMTS.
005193     MOVE CL-PAID-THRU-DT        TO AT-OLD-PAID-THRU-DT.
005194     MOVE CL-LAST-PMT-DT         TO AT-LAST-PMT-MADE-DT.
005195     MOVE WS-DIAGNOSIS-DESCRIPT  TO AT-OLD-DIAG-DESCRIP.
005196     MOVE CL-CAUSE-CD            TO AT-OLD-DIAG-CODE.
005197     MOVE WS-ICD-CODE-1          TO AT-OLD-ICD-CODE-1.
005198     MOVE WS-ICD-CODE-2          TO AT-OLD-ICD-CODE-2.
005199
005200     MOVE WS-ITD-PAID-EXPENSE    TO AT-OLD-ITD-PAID-EXPENSE.
005201     MOVE WS-ITD-CHARGABLE-EXPENSE   TO AT-OLD-CHARGABLE-EXPENSE.
005202     MOVE WS-INITIAL-MANUAL-RESERVE  TO AT-OLD-INIT-MAN-RESV.
005203     MOVE WS-CURRENT-MANUAL-RESERVE  TO AT-OLD-CURRENT-MAN-RESV.
005204     MOVE WS-ITD-ADDITIONAL-RESERVE  TO AT-OLD-ADDL-MAN-RESV.
005205
005206
005207     
      * EXEC CICS WRITE
005208*        DATASET   (ELTRLR-DSID)
005209*        FROM      (ACTIVITY-TRAILERS)
005210*        RIDFLD    (AT-CONTROL-PRIMARY)
005211*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011696' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303131363936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005212
005213     MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
005214     MOVE CL-CERT-CARRIER        TO CERT-CARRIER.
005215     MOVE CL-CERT-GROUPING       TO CERT-GROUPING.
005216     MOVE CL-CERT-STATE          TO CERT-STATE.
005217     MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT.
005218     MOVE CL-CERT-NO             TO CERT-CERT-NO.
005219     MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT.
005220
005221
005222     
      * EXEC CICS HANDLE CONDITION
005223*        NOTFND   (5050-CERT-NOT-FOUND)
005224*    END-EXEC.
      *    MOVE '"$I                   ! < #00011711' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3C20233030303131373131' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005225
005226     PERFORM 7930-READ-CERT-UPDATE THRU 7930-EXIT.
005227
005228     PERFORM 3700-BUILD-CERT THRU 3700-EXIT.
005229
005230
005231     
      * EXEC CICS HANDLE CONDITION
005232*         DUPKEY   (5030-CERT-WRITTEN)
005233*    END-EXEC.
      *    MOVE '"$$                   ! = #00011720' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'3D20233030303131373230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005234
005235
005236     
      * EXEC CICS REWRITE
005237*        DATASET  (ELCERT-DSID)
005238*        FROM     (CERTIFICATE-MASTER)
005239*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011725' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131373235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005240
005241 5030-CERT-WRITTEN.
005242
005243     MOVE LOW-VALUES             TO CL-LAST-PMT-DT
005244                                    CL-INCURRED-DT
005245                                    CL-REPORTED-DT
005246                                    CL-EST-END-OF-DISAB-DT
005247                                    CL-PAID-THRU-DT.
005248
005249     MOVE ZEROS                  TO CL-TOTAL-PAID-AMT
005250                                    CL-NO-OF-PMTS-MADE
005251                                    CL-NO-OF-DAYS-PAID.
005252
005253     IF CLAIM-IS-CLOSED
005254        MOVE WS-TODAY-DT         TO CL-LAST-REOPEN-DT
005255        MOVE 'O'                 TO CL-CLAIM-STATUS
005256     END-IF.
005257
005258     PERFORM 3995-BUILD-CLAIM-RECORD THRU 3995-EXIT.
005259     MOVE '5'                    TO CL-LAST-MAINT-TYPE.
005260     MOVE EMI-FORCABLE-CTR       TO CL-FORCEABLE-ERROR-CNT.
005261
005262
005263     
      * EXEC CICS HANDLE CONDITION
005264*         DUPKEY   (5035-CLAIM-WRITTEN)
005265*    END-EXEC.
      *    MOVE '"$$                   ! > #00011752' TO DFHEIV0
           MOVE X'222424202020202020202020' &
                X'202020202020202020202120' &
                X'3E20233030303131373532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005266
005267
005268     
      * EXEC CICS REWRITE
005269*        DATASET   (ELMSTR-DSID)
005270*        FROM      (CLAIM-MASTER)
005271*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011757' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131373537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005272
005273 5035-CLAIM-WRITTEN.
005274
005275     if clmtypei = 'L' or 'O'
005276        move cm-lf-benefit-cd    to ws-ben-hold
005277        move '4'                 to ws-rec-type
005278        perform 7100-read-benefit thru 7199-exit
005279        if ws-ben-alpha-hold <> spaces
005280           move ws-special-calc-cd
005281                                 to ws-lf-special-calc-cd
005282        end-if
005283     else
005284        move cm-ah-benefit-cd    to ws-ben-hold
005285        move '5'                 to ws-rec-type
005286        perform 7100-read-benefit thru 7199-exit
005287        if ws-ben-alpha-hold <> spaces
005288           move ws-special-calc-cd
005289                                 to ws-ah-special-calc-cd
005290        end-if
005291     end-if
005292
005293     move ' '                    to ws-mob-cert-ind
005294     if pi-company-id = 'CID'
005295        if ((CLMTYPEI = PI-LIFE-OVERRIDE-L1 or 'O')
005296           and (ws-lf-special-calc-cd = 'O'))
005297                      or
005298           ((clmtypei <> pi-life-override-l1 and 'O')
005299           and (ws-ah-special-calc-cd = 'O'))
005300           set mob-cert to true
005301        end-if
005302     else
005303        if (pi-company-id = 'DCC')
005304           and (cm-carrier = '7')
005305           set mob-cert to true
005306        end-if
005307     end-if
005308
005309     perform 7990-get-lo-hi-acct-dates
005310                                 thru 7990-exit
005311
005312** if the prev inc dt >= hi dt then probably
005313** have error already set
005314     if ws-prev-inc-dt >= ws-hi-acct-dt
005315        continue
005316     else
005317        if (ws-incur >= ws-hi-acct-dt)
005318           and (acct-cancelled)
005319           and (mob-cert)
005320           MOVE er-1682          TO EMI-ERROR
005321           MOVE -1               TO INCURL
005322           MOVE AL-UABON         TO INCURA
005323           PERFORM 9900-ERROR-FORMAT
005324                                 THRU 9900-EXIT
005325           PERFORM 3992-BUILD-TRAILER
005326                                 THRU 3992-EXIT
005327        end-if
005328     end-if
005329
005330     if ws-prev-inc-dt < cm-cert-eff-dt
005331        continue
005332     else
005333        if ws-incur < cm-cert-eff-dt
005334           MOVE er-1683          TO EMI-ERROR
005335           MOVE -1               TO INCURL
005336           MOVE AL-UABON         TO INCURA
005337           PERFORM 9900-ERROR-FORMAT
005338                                 THRU 9900-EXIT
005339           PERFORM 3992-BUILD-TRAILER
005340                                 THRU 3992-EXIT
005341        end-if
005342     end-if
005343
005344     .
005345 5035-continue.
005346
005347     MOVE ER-0000                TO EMI-ERROR.
005348     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005349
005350     MOVE CLMNOI                 TO PI-LAST-CLAIM.
005351     MOVE CERTNOI                TO PI-LAST-CERT-PRIME.
005352     MOVE SUFXI                  TO PI-LAST-CERT-SUFX.
005353     MOVE CLMCARRI               TO PI-LAST-CARR.
005354
005355     IF PRTOPTL GREATER ZERO
005356         MOVE PRTOPTI            TO PI-PRT-OPT
005357     END-IF.
005358
005359     IF ALTPRTL GREATER ZERO
005360         MOVE ALTPRTI            TO PI-ALT-PRT
005361     END-IF.
005362
005363     MOVE LOW-VALUES             TO EL130AI.
005364
005365     MOVE PI-LAST-CERT-PRIME     TO CERTNOO.
005366     MOVE PI-LAST-CERT-SUFX      TO SUFXO.
005367     MOVE PI-LAST-CARR           TO CLMCARRO.
005368     MOVE PI-LAST-CLAIM          TO CLMNOO.
005369
005370     IF PI-PRT-OPT = 'N' OR 'L'
005371         MOVE PI-PRT-OPT         TO PRTOPTO
005372         MOVE 1                  TO PRTOPTL
005373     END-IF.
005374
005375     IF PI-ALT-PRT NOT = SPACES AND LOW-VALUES
005376         MOVE PI-ALT-PRT         TO ALTPRTO
005377         MOVE 4                  TO ALTPRTL
005378     END-IF.
005379
005380     MOVE AL-UANON               TO CLMNOA CLMCARRA.
005381     MOVE AL-SANON               TO CERTNOA SUFXA.
005382
005383     MOVE SPACES                 TO PI-INCURR-SW.
005384
005385     IF PRTOPTL GREATER ZERO
005386         IF PRTOPTI = 'L'
005387             GO TO 0400-CREATE-ELACTQ
005388         ELSE
005389             GO TO 0480-PRINT-NOW
005390         END-IF
005391     END-IF.
005392
005393     GO TO 8100-SEND-INITIAL-MAP.
005394
005395 5050-CERT-NOT-FOUND.
005396
005397     MOVE ER-0244                TO EMI-ERROR.
005398     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
005399     MOVE -1                     TO CERTNOL.
005400     GO TO 8200-SEND-DATAONLY.
005401
005402     EJECT
005403 5500-BUILD-FORM-TRAILER.
005404*************************************************************
005405*           THIS CODE IS FOR 'CRI' ONLY         04/18/88
005406*************************************************************
005407
005408     MOVE WS-TODAY-DT            TO SAVE-SEND-DT.
005409
005410     IF  WS-BEN-ALPHA-2 = '30'
005411         MOVE WS-TODAY-DT        TO DC-BIN-DATE-1
005412         MOVE '6'                TO DC-OPTION-CODE
005413         IF CL-CARRIER = 'C'
005414             MOVE +14            TO DC-ELAPSED-DAYS
005415             MOVE +0             TO DC-ELAPSED-MONTHS
005416             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
005417             MOVE DC-BIN-DATE-2  TO SAVE-SEND-DT
005418         ELSE
005419             MOVE +21            TO DC-ELAPSED-DAYS
005420             MOVE +0             TO DC-ELAPSED-MONTHS
005421             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
005422             MOVE DC-BIN-DATE-2      TO  SAVE-SEND-DT
005423         END-IF
005424     END-IF.
005425
005426
005427     
      * EXEC CICS GETMAIN
005428*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
005429*         LENGTH   (ELTRLR-LENGTH)
005430*         INITIMG  (GETMAIN-SPACE)
005431*    END-EXEC.
      *    MOVE ',"IL                  $   #00011916' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303131393136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELTRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005432
005433     MOVE SAVE-AT-PRIMARY-KEY    TO AT-CONTROL-PRIMARY.
005434     MOVE 'AT'                   TO AT-RECORD-ID.
005435
005436     SUBTRACT +1 FROM CL-TRAILER-SEQ-CNT.
005437     MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO.
005438     MOVE 'A'                    TO AT-TRAILER-TYPE.
005439     MOVE WS-TODAY-DT            TO AT-RECORDED-DT
005440                                    AT-FORM-LAST-MAINT-DT.
005441     MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
005442                                    AT-FORM-LAST-UPDATED-BY.
005443     MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
005444     MOVE SAVE-SEND-DT           TO AT-FORM-SEND-ON-DT.
005445     MOVE LOW-VALUES             TO AT-FORM-FOLLOW-UP-DT
005446                                    AT-FORM-RE-SEND-DT
005447                                    AT-FORM-ANSWERED-DT
005448                                    AT-EMP-FORM-ANSWERED-DT
005449                                    AT-PHY-FORM-ANSWERED-DT
005450                                    AT-EMP-FORM-SEND-ON-DT
005451                                    AT-FORM-PRINTED-DT
005452                                    AT-FORM-REPRINT-DT.
005453     MOVE '1'                    TO AT-FORM-TYPE.
005454
005455     MOVE SPACES                 TO AT-INSTRUCT-LN-1
005456                                    AT-INSTRUCT-LN-2
005457                                    AT-INSTRUCT-LN-3.
005458
005459     MOVE CL-INSURED-ADDR-CNT    TO AT-FORM-ADDR-SEQ-NO.
005460     MOVE 'I'                    TO AT-FORM-ADDRESS.
005461
005462     MOVE SAVE-SEND-DT           TO AT-PHY-FORM-SEND-ON-DT.
005463
005464     IF AM-EMPLOYER-STMT-USED = 'Y'
005465        MOVE SAVE-SEND-DT       TO AT-EMP-FORM-SEND-ON-DT
005466     END-IF.
005467
005468
005469     
      * EXEC CICS WRITE
005470*         DATASET  (ELTRLR-DSID)
005471*         FROM     (ACTIVITY-TRAILERS)
005472*         RIDFLD   (AT-CONTROL-PRIMARY)
005473*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00011958' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303131393538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005474
005475 5599-EXIT.
005476     EJECT
005477*************************************************************
005478*           THIS CODE IS FOR 'CRI' ONLY         04/18/88
005479*************************************************************
005480
005481 5700-BUILD-ARCHIVE-HEADER.
005482
005483     MOVE '1'                    TO CNTL-REC-TYPE.
005484     MOVE ZEROS                  TO CNTL-SEQ-NO.
005485     MOVE SPACES                 TO CNTL-ACCESS.
005486
005487
005488     
      * EXEC CICS READ
005489*         DATASET  (ELCNTL-DSID)
005490*         SET      (ADDRESS OF CONTROL-FILE)
005491*         RIDFLD   (ELCNTL-KEY)
005492*         UPDATE
005493*    END-EXEC.
      *    MOVE '&"S        EU         (   #00011977' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303131393737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005494
005495     ADD 1 TO CF-CO-ARCHIVE-COUNTER.
005496
005497     MOVE CF-CO-ARCHIVE-COUNTER  TO ARCH-NUMBER.
005498
005499
005500     
      * EXEC CICS REWRITE
005501*         FROM     (CONTROL-FILE)
005502*         DATASET  (ELCNTL-DSID)
005503*    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011989' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303131393839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005504
005505
005506     
      * EXEC CICS HANDLE CONDITION
005507*         NOTOPEN  (9990-ABEND)
005508*         DUPKEY   (5799-EXIT)
005509*    END-EXEC.
      *    MOVE '"$J$                  ! ? #00011995' TO DFHEIV0
           MOVE X'22244A242020202020202020' &
                X'202020202020202020202120' &
                X'3F20233030303131393935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005510
005511
005512     
      * EXEC CICS GETMAIN
005513*         SET     (ADDRESS OF LETTER-ARCHIVE)
005514*         LENGTH  (ELARCH-LENGTH)
005515*    END-EXEC.
      *    MOVE '," L                  $   #00012001' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303132303031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELARCH-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005516
005517     MOVE SPACES                 TO LETTER-ARCHIVE.
005518     MOVE 'LA'                   TO LA-RECORD-ID.
005519     MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO
005520                                    LA-ARCHIVE-NO-A1.
005521     MOVE '4'                    TO LA-RECORD-TYPE
005522                                    LA-RECORD-TYPE-A1.
005523     MOVE ZEROS                  TO LA-LINE-SEQ-NO
005524                                    LA-LINE-SEQ-NO-A1.
005525     MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
005526                                    LA-COMPANY-CD-A1.
005527     MOVE CL-CARRIER             TO LA4-CARRIER.
005528     MOVE CL-CLAIM-NO            TO LA4-CLAIM-NO.
005529     MOVE CL-CERT-NO             TO LA4-CERT-NO.
005530     MOVE CL-CERT-STATE          TO LA4-STATE.
005531     MOVE  ZEROS                 TO LA4-NO-OF-COPIES.
005532     MOVE PI-PROCESSOR-ID        TO LA4-PROCESSOR-CD.
005533     MOVE WS-TODAY-DT            TO LA4-CREATION-DT.
005534     MOVE LOW-VALUES             TO LA4-INITIAL-PRINT-DATE
005535                                    LA4-RESEND-PRINT-DATE
005536                                    LA4-FORM-REM-PRINT-DT
005537                                    LA4-RESEND-DATE.
005538     MOVE CL-TRAILER-SEQ-CNT     TO LA4-FORM-TRLR-SEQ.
005539     MOVE '1'                    TO LA4-FORM-TYPE.
005540
005541
005542     
      * EXEC CICS WRITE
005543*         DATASET  (ELARCH-DSID)
005544*         FROM     (LETTER-ARCHIVE)
005545*         RIDFLD   (LA-CONTROL-PRIMARY)
005546*    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00012031' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303132303331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELARCH-DSID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005547
005548 5799-EXIT.
005549     EJECT
005550 6000-EDIT-CLAIM-DATA.
005551
005552     MOVE SAVE-DATE              TO DC-GREG-DATE-1-EDIT.
005553     MOVE '2'                    TO DC-OPTION-CODE.
005554     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
005555     MOVE DC-BIN-DATE-1          TO WS-TODAY-DT.
005556
005557     IF MAINTL NOT = ZERO
005558         MOVE AL-UANON           TO MAINTA
005559     END-IF
005560
005561     IF CLMNOL GREATER ZERO
005562         IF CLMNOI = SPACES OR LOW-VALUES
005563             MOVE -1             TO CLMNOL
005564             MOVE AL-UABON       TO CLMNOA
005565             MOVE ER-7688        TO EMI-ERROR
005566             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005567         ELSE
005568             MOVE AL-UANON       TO CLMNOA
005569         END-IF
005570     END-IF.
005571
005572     IF CERTNOL = ZERO
005573         MOVE -1                 TO CERTNOL
005574         MOVE AL-SABON           TO CERTNOA
005575         MOVE ER-0203            TO EMI-ERROR
005576         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005577     ELSE
005578         MOVE AL-SANON           TO CERTNOA
005579     END-IF.
005580
005581     IF SUFXL = ZERO
005582         MOVE SPACES             TO SUFXI
005583         MOVE AL-SANON           TO SUFXA
005584     ELSE
005585         MOVE AL-SANON           TO SUFXA
005586     END-IF.
005587
005588     IF CLMCARRL = ZERO
005589         MOVE -1                 TO CLMCARRL
005590         MOVE ER-0194            TO EMI-ERROR
005591         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005592     ELSE
005593         MOVE CLMCARRI           TO PI-CARRIER
005594         IF PI-NO-CARRIER-SECURITY
005595             MOVE AL-UANON       TO CLMCARRA
005596         END-IF
005597     END-IF.
005598
005599     IF CLMTYPEL > ZERO
005600        IF ((PI-COMPANY-ID = 'DCC' OR 'VPP')
005601           AND (CLMTYPEI NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
005602              AND PI-AH-OVERRIDE-L1 AND 'I' AND 'G' AND 'F'
005603              AND 'B' AND 'H'))
005604                        OR
005605           ((PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL')
005606           AND (CLMTYPEI NOT = PI-LIFE-OVERRIDE-L1 AND 'O'
005607              AND PI-AH-OVERRIDE-L1))
005608              MOVE -1             TO CLMTYPEL
005609              MOVE AL-UABON       TO CLMTYPEA
005610              MOVE ER-7634        TO EMI-ERROR
005611              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005612        ELSE
005613           MOVE AL-UANON       TO CLMTYPEA
005614        END-IF
005615     ELSE
005616        IF MAINTI = 'A'
005617           MOVE -1             TO CLMTYPEL
005618           MOVE AL-UABON       TO CLMTYPEA
005619           MOVE ER-0546        TO EMI-ERROR
005620           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005621        END-IF
005622     END-IF
005623
005624     if instypel > ZERO
005625        if instypei = 'P' OR 'C'
005626           move al-uanon         to instypea
005627        else
005628           MOVE -1               TO INSTYPEL
005629           MOVE AL-UABON         TO INSTYPEA
005630           MOVE ER-1654          TO EMI-ERROR
005631           PERFORM 9900-ERROR-FORMAT
005632                                 THRU 9900-EXIT
005633        END-IF
005634     ELSE
005635        IF MAINTI = 'A'
005636           MOVE -1               TO INSTYPEL
005637           MOVE AL-UABON         TO INSTYPEA
005638           MOVE ER-1654          TO EMI-ERROR
005639           PERFORM 9900-ERROR-FORMAT
005640                                 THRU 9900-EXIT
005641        END-IF
005642     END-IF
005643
005644     if (instypel <> zeros)
005645        and (emi-error not = er-1654)
005646        and (instypei = 'C')
005647        and (crtfnmei = fstnmei)
005648        move er-1675             to emi-error
005649        MOVE -1                  TO INSTYPEL
005650        MOVE AL-UABON            TO INSTYPEA
005651        PERFORM 9900-ERROR-FORMAT
005652                                 THRU 9900-EXIT
005653    end-if
005654
005655     if (instypel <> zeros)
005656        and (emi-error not = er-1654)
005657        and (instypei = 'P')
005658        and (crtfnmei <> fstnmei)
005659        move er-1676             to emi-error
005660        MOVE -1                  TO INSTYPEL
005661        MOVE AL-UABON            TO INSTYPEA
005662        PERFORM 9900-ERROR-FORMAT
005663                                 THRU 9900-EXIT
005664    end-if
005665
005666     IF PCERTNOL = ZERO
005667        MOVE CERTNOI             TO PCERTNOI
005668        MOVE CERTNOL             TO PCERTNOL
005669        MOVE AL-UANON            TO PCERTNOA
005670     ELSE
005671        MOVE AL-UANON            TO PCERTNOA
005672     END-IF.
005673
005674     IF PSUFXL = ZERO
005675        MOVE SUFXI               TO PSUFXI
005676        MOVE SUFXL               TO PSUFXL
005677        MOVE AL-UANON            TO PSUFXA
005678     ELSE
005679        MOVE AL-UANON            TO PSUFXA
005680     END-IF.
005681
005682     IF MAINTI = 'I'
005683         GO TO 6020-EDIT
005684     END-IF.
005685
005686     IF LSTNMEL = ZERO AND CERTMTL NOT = ZEROS
005687         MOVE -1                 TO LSTNMEL
005688         MOVE ER-0236            TO EMI-ERROR
005689         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005690     ELSE
005691         MOVE AL-UANON           TO LSTNMEA
005692     END-IF.
005693
005694     if fstnmel not = zeros
005695        move al-uanon            to fstnmea
005696     end-if
005697     IF SEXL > ZEROS
005698        IF SEXI = 'M' OR 'F'
005699           move al-uanon         to sexa
005700           CONTINUE
005701        ELSE
005702           MOVE -1               TO SEXL
005703           MOVE AL-UABON         TO SEXA
005704           MOVE ER-0219          TO EMI-ERROR
005705           PERFORM 9900-ERROR-FORMAT
005706                                 THRU 9900-EXIT
005707        END-IF
005708     ELSE
005709        IF MAINTI = 'A'
005710           MOVE -1               TO SEXL
005711           MOVE AL-UABON         TO SEXA
005712           MOVE ER-0219          TO EMI-ERROR
005713           PERFORM 9900-ERROR-FORMAT
005714                                 THRU 9900-EXIT
005715        END-IF
005716     END-IF
005717
005718     IF SSNL GREATER THAN ZERO
005719         MOVE SSNI TO WS-SOC-SEC-NUMBER
005720         IF WS-SOC-SEC-NO NUMERIC AND
005721           (WS-SOC-SEC-BLANK = SPACES OR LOW-VALUES)
005722             NEXT SENTENCE
005723         ELSE
005724            IF WS-SSN-1-3 NUMERIC AND WS-SSN-4-5 NUMERIC AND
005725               WS-SSN-6-9 NUMERIC AND WS-SSN-DASH1 = '-' AND
005726               WS-SSN-DASH2 = '-'
005727                 NEXT SENTENCE
005728            ELSE
005729                 MOVE ER-0887        TO  EMI-ERROR
005730                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005731                 MOVE AL-UABON       TO  SSNA
005732                 MOVE -1             TO  SSNL
005733            END-IF
005734        END-IF
005735     END-IF.
005736
005737     IF BIRTHDTL = ZERO
005738         GO TO 6020-EDIT
005739     END-IF.
005740
005741     IF BIRTHDTL GREATER ZERO
005742         MOVE BIRTHDTI           TO DATE-WORK
005743         PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
005744         MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
005745         MOVE '4'                TO DC-OPTION-CODE
005746         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
005747         IF DATE-CONVERSION-ERROR
005748             MOVE AL-UABON       TO BIRTHDTA
005749             MOVE -1             TO BIRTHDTL
005750             MOVE ER-0220        TO EMI-ERROR
005751             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005752             GO TO 6020-EDIT
005753         END-IF
005754     END-IF.
005755
005756******************************************************************
005757**   IF CALCULATED BIRTH DATE GREATER THAN TODAYS DATE          **
005758**   USE THE CENTURY ADJUSTMENT SWITCH IN THE DATE ROUTINE      **
005759**   TO SUBTRACT 100 YEARS TO OBTAIN THE CORRECT BIRTH DATE.    **
005760******************************************************************
005761
005762     IF DC-BIN-DATE-1 GREATER THAN SAVE-BIN-DATE
005763         MOVE BIRTHDTI           TO  DATE-WORK
005764         PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
005765         MOVE NUM-WORK           TO  DC-GREG-DATE-1-MDY
005766         MOVE '4'                TO  DC-OPTION-CODE
005767         MOVE '1'                TO  DC-CENTURY-ADJUSTMENT
005768         MOVE +0                 TO  DC-ELAPSED-MONTHS
005769                                     DC-ELAPSED-DAYS
005770         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
005771         IF DATE-CONVERSION-ERROR
005772             MOVE AL-UABON       TO  BIRTHDTA
005773             MOVE -1             TO  BIRTHDTL
005774             MOVE ER-0220        TO  EMI-ERROR
005775             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005776             GO TO 6020-EDIT
005777         END-IF
005778     END-IF.
005779
005780     MOVE AL-UANON               TO  BIRTHDTA.
005781     MOVE DC-GREG-DATE-1-EDIT    TO  BIRTHDTI.
005782     MOVE DC-BIN-DATE-1          TO  WS-BIRTHDT.
005783     MOVE ' '                    TO  DC-CENTURY-ADJUSTMENT.
005784
005785 6020-EDIT.
005786
005787     IF INCURL GREATER ZERO
005788         MOVE INCURI             TO DATE-WORK
005789         PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
005790         MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
005791         MOVE '4'                TO DC-OPTION-CODE
005792         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
005793         IF DATE-CONVERSION-ERROR
005794             MOVE AL-UABON       TO INCURA
005795             MOVE -1             TO INCURL
005796             MOVE ER-0222        TO EMI-ERROR
005797             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005798         ELSE
005799             MOVE AL-UANON       TO INCURA
005800             MOVE DC-GREG-DATE-1-EDIT  TO INCURI
005801             MOVE DC-BIN-DATE-1  TO WS-INCUR
005802                                    PI-SAVE-INCUR-DT
005803             IF WS-INCUR GREATER THAN WS-TODAY-DT
005804                 MOVE ER-0511     TO EMI-ERROR
005805                 MOVE -1          TO INCURL
005806                 MOVE AL-UABON    TO INCURA
005807                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005808             ELSE
005809*                NEXT SENTENCE
005810                 CONTINUE
005811             END-IF
005812         END-IF
005813     ELSE
005814         MOVE ER-0516         TO EMI-ERROR
005815         MOVE AL-UABON        TO INCURA
005816         MOVE -1              TO INCURL
005817         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005818     END-IF.
005819
005820     IF REPORTL GREATER ZERO
005821         MOVE REPORTI            TO DATE-WORK
005822         PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
005823         MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
005824         MOVE '4'                TO DC-OPTION-CODE
005825         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
005826         IF DATE-CONVERSION-ERROR
005827             MOVE AL-UABON       TO REPORTA
005828             MOVE -1             TO REPORTL
005829             MOVE ER-0223        TO EMI-ERROR
005830             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005831         ELSE
005832             MOVE AL-UANON       TO REPORTA
005833             MOVE DC-GREG-DATE-1-EDIT  TO REPORTI
005834             MOVE DC-BIN-DATE-1  TO WS-REPORT
005835             IF WS-REPORT GREATER THAN WS-TODAY-DT
005836                 MOVE ER-0512     TO EMI-ERROR
005837                 MOVE -1          TO REPORTL
005838                 MOVE AL-UABON    TO REPORTA
005839                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005840             ELSE
005841*                NEXT SENTENCE
005842                 CONTINUE
005843             END-IF
005844         END-IF
005845     ELSE
005846         MOVE ER-0517         TO EMI-ERROR
005847         MOVE -1              TO REPORTL
005848         MOVE AL-UABON        TO REPORTA
005849         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005850     END-IF.
005851
005852     IF WS-REPORT NOT = LOW-VALUES
005853         IF WS-INCUR GREATER THAN WS-REPORT
005854             MOVE AL-UABON       TO REPORTA
005855             MOVE -1             TO REPORTL
005856             MOVE ER-0509        TO EMI-ERROR
005857             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005858         END-IF
005859     END-IF.
005860
005861     IF ICD1L GREATER THAN ZERO
005862         IF (ICD1I GREATER THAN SPACES) AND
005863            (ICD1I(4:1) NOT = '.')
005864             IF ICD1I(4:1) = ' '
005865                 MOVE '.' TO ICD1I(4:1)
005866             ELSE
005867                 IF ICD1I(8:1) = ' '
005868                     MOVE ICD1I(7:1) TO ICD1I(8:1)
005869                     MOVE ICD1I(6:1) TO ICD1I(7:1)
005870                     MOVE ICD1I(5:1) TO ICD1I(6:1)
005871                     MOVE ICD1I(4:1) TO ICD1I(5:1)
005872                     MOVE '.'        TO ICD1I(4:1)
005873                 END-IF
005874             END-IF
005875         END-IF
005876         IF (ICD1I GREATER THAN SPACES) AND
005877            (ICD1I(1:1) NOT > ' ' OR
005878             ICD1I(2:1) NOT > ' ' OR
005879             ICD1I(3:1) NOT > ' ' OR
005880             ICD1I(4:1) NOT = '.')
005881              MOVE ER-0992        TO  EMI-ERROR
005882              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005883              MOVE AL-UABON       TO ICD1A
005884              MOVE -1             TO ICD1L
005885         END-IF
005886     END-IF.
005887
005888     IF ICD2L GREATER THAN ZERO
005889         IF (ICD2I GREATER THAN SPACES) AND
005890            (ICD2I(4:1) NOT = '.')
005891             IF ICD2I(4:1) = ' '
005892                 MOVE '.' TO ICD2I(4:1)
005893             ELSE
005894                 IF ICD2I(8:1) = ' '
005895                     MOVE ICD2I(7:1) TO ICD2I(8:1)
005896                     MOVE ICD2I(6:1) TO ICD2I(7:1)
005897                     MOVE ICD2I(5:1) TO ICD2I(6:1)
005898                     MOVE ICD2I(4:1) TO ICD2I(5:1)
005899                     MOVE '.'        TO ICD2I(4:1)
005900                 END-IF
005901             END-IF
005902         END-IF
005903         IF (ICD2I GREATER THAN SPACES) AND
005904            (ICD2I(1:1) NOT > ' ' OR
005905             ICD2I(2:1) NOT > ' ' OR
005906             ICD2I(3:1) NOT > ' ' OR
005907             ICD2I(4:1) NOT = '.')
005908              MOVE ER-0992        TO  EMI-ERROR
005909              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005910              MOVE AL-UABON       TO ICD2A
005911              MOVE -1             TO ICD2L
005912         END-IF
005913     END-IF
005914
005915*    IF ESTENDL GREATER ZERO
005916*        MOVE ESTENDI            TO DATE-WORK
005917*        PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
005918*        MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
005919*        MOVE '4'                TO DC-OPTION-CODE
005920*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
005921*        IF DATE-CONVERSION-ERROR
005922*            MOVE AL-UABON       TO ESTENDA
005923*            MOVE -1             TO ESTENDL
005924*            MOVE ER-0224        TO EMI-ERROR
005925*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005926*        ELSE
005927*            MOVE AL-UANON       TO ESTENDA
005928*            MOVE DC-GREG-DATE-1-EDIT  TO ESTENDI
005929*            MOVE DC-BIN-DATE-1  TO WS-ESTEND
005930*            IF CLMTYPEI = PI-LIFE-OVERRIDE-L1
005931*               MOVE AL-UABON    TO ESTENDA
005932*               MOVE -1          TO ESTENDL
005933*               MOVE ER-0544     TO EMI-ERROR
005934*               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005935*            END-IF
005936*        END-IF
005937*    END-IF.
005938*
005939*    IF WS-ESTEND NOT = LOW-VALUES
005940*        IF WS-INCUR GREATER THAN WS-ESTEND
005941*            MOVE AL-UABON       TO ESTENDA
005942*            MOVE -1             TO ESTENDL
005943*            MOVE ER-0510        TO EMI-ERROR
005944*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005945*        END-IF
005946*    END-IF.
005947
005948     IF PRTOPTL GREATER ZERO
005949         IF PRTOPTI NOT = 'N' AND 'L'
005950             MOVE AL-UABON       TO PRTOPTA
005951             MOVE -1             TO PRTOPTL
005952             MOVE ER-0334        TO EMI-ERROR
005953             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005954         END-IF
005955     END-IF.
005956
005957     IF MANRSVL GREATER ZERO
005958         
      * EXEC CICS BIF DEEDIT
005959*            FIELD   (MANRSVI)
005960*            LENGTH  (9)
005961*        END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012447' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132343437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MANRSVI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
005962         IF MANRSVI NOT NUMERIC
005963             MOVE AL-UNBON       TO MANRSVA
005964             MOVE -1             TO MANRSVL
005965             MOVE ER-0489        TO EMI-ERROR
005966             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005967         ELSE
005968             MOVE AL-UNNON       TO MANRSVA
005969             MOVE MANRSVI        TO WS-MANRSV  MANRSVO
005970         END-IF
005971     END-IF.
005972
005973     MOVE SPACES                 TO ELCNTL-KEY.
005974
005975     IF CONTROL-IS-ACTUAL-CARRIER
005976         MOVE PI-CARRIER         TO CNTL-CARRIER
005977     ELSE
005978         MOVE PI-CARRIER-CONTROL-LEVEL TO CNTL-CARRIER
005979     END-IF.
005980
005981     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
005982     MOVE '6'                    TO CNTL-REC-TYPE.
005983     MOVE +0                     TO CNTL-SEQ-NO.
005984
005985
005986     
      * EXEC CICS HANDLE CONDITION
005987*        NOTFND   (3100-CARRIER-NOT-FOUND)
005988*    END-EXEC.
      *    MOVE '"$I                   ! @ #00012475' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4020233030303132343735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
005989
005990     PERFORM 7970-READ-CNTL THRU 7970-EXIT.
005991
005992     IF NOT CF-MANUAL-RESERVES-USED AND
005993           WS-MANRSV GREATER ZERO
005994         MOVE ER-0518             TO EMI-ERROR
005995         MOVE -1                  TO MANRSVL
005996         MOVE AL-UNBON            TO MANRSVA
005997         MOVE ZEROS               TO WS-MANRSV
005998         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
005999     END-IF.
006000
006001     MOVE CF-RESERVE-CONTROLS    TO WS-RESERVE-CONTROLS.
006002     MOVE CF-EXPENSE-CONTROLS    TO WS-EXPENSE-CONTROLS.
006003
006004     IF SUPVL GREATER ZERO
006005         IF SUPVI NOT = ' ' AND 'Y' AND 'N'
006006             MOVE -1             TO SUPVL
006007             MOVE AL-UABON       TO SUPVA
006008             MOVE ER-0230        TO EMI-ERROR
006009             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006010         ELSE
006011             MOVE AL-UANON       TO SUPVA
006012         END-IF
006013     END-IF.
006014
006015     IF PROCCDL = ZERO
006016        GO TO 6050-CONTINUE-EDITS
006017     END-IF.
006018
006019     MOVE SPACES                 TO ELCNTL-KEY.
006020     MOVE PROCCDI                TO CNTL-ACCESS.
006021
006022     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
006023     MOVE '2'                    TO CNTL-REC-TYPE.
006024     MOVE +0                     TO CNTL-SEQ-NO.
006025
006026
006027     
      * EXEC CICS HANDLE CONDITION
006028*        NOTFND   (6040-NO-PROCESSOR-RECORD)
006029*    END-EXEC.
      *    MOVE '"$I                   ! A #00012516' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4120233030303132353136' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006030
006031     PERFORM 7970-READ-CNTL THRU 7970-EXIT.
006032
006033     GO TO 6050-CONTINUE-EDITS.
006034
006035 6040-NO-PROCESSOR-RECORD.
006036
006037     MOVE ER-0019                TO EMI-ERROR.
006038     MOVE -1                     TO PROCCDL.
006039     MOVE AL-UABON               TO PROCCDA.
006040     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006041
006042 6050-CONTINUE-EDITS.
006043     IF (BENECDL = ZERO) OR (BENECDI = SPACES OR LOW-VALUES)
006044           GO TO 6070-CONTINUE-EDITS
006045     END-IF.
006046
006047     MOVE BENECDI                TO BENE-CODE.
006048     MOVE 'B'                    TO BENE-RCD-TYPE.
006049     MOVE PI-COMPANY-CD          TO BENE-COMP-CD.
006050
006051
006052     
      * EXEC CICS HANDLE CONDITION
006053*        NOTFND   (6060-NO-BENEFICIARY-RECORD)
006054*    END-EXEC.
      *    MOVE '"$I                   ! B #00012541' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4220233030303132353431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006055
006056     
      * EXEC CICS READ
006057*         DATASET   (ELBENE-DSID)
006058*         SET       (ADDRESS OF BENEFICIARY-MASTER)
006059*         RIDFLD    (ELBENE-KEY)
006060*    END-EXEC.
      *    MOVE '&"S        E          (   #00012545' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303132353435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006061
006062     GO TO 6070-CONTINUE-EDITS.
006063
006064 6060-NO-BENEFICIARY-RECORD.
006065
006066     MOVE ER-0565                TO EMI-ERROR.
006067     MOVE -1                     TO BENECDL.
006068     MOVE AL-UABON               TO BENECDA.
006069     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006070
006071
006072 6070-CONTINUE-EDITS.
006073     IF CERTMTL GREATER ZERO
006074         IF CERTMTI NOT = 'A' AND 'S'
006075             MOVE -1             TO CERTMTL
006076             MOVE AL-UABON       TO CERTMTA
006077             MOVE ER-0267        TO EMI-ERROR
006078             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006079         ELSE
006080             MOVE AL-UANON       TO CERTMTA
006081         END-IF
006082     ELSE
006083         GO TO 6000-EXIT
006084     END-IF.
006085
006086     IF EFFDTL GREATER ZERO
006087         MOVE EFFDTI             TO DATE-WORK
006088         PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
006089         MOVE NUM-WORK           TO DC-GREG-DATE-1-MDY
006090         MOVE '4'                TO DC-OPTION-CODE
006091         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
006092         IF DATE-CONVERSION-ERROR
006093             MOVE AL-UABON       TO EFFDTA
006094             MOVE -1             TO EFFDTL
006095             MOVE ER-0231        TO EMI-ERROR
006096             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006097         ELSE
006098             MOVE AL-UANON       TO EFFDTA
006099             MOVE DC-GREG-DATE-1-EDIT  TO EFFDTI
006100             MOVE DC-BIN-DATE-1  TO WS-EFFDT
006101         END-IF
006102     ELSE
006103         MOVE ER-0231            TO EMI-ERROR
006104         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006105         MOVE -1                 TO EFFDTL
006106         MOVE AL-UABON           TO EFFDTA
006107     END-IF.
006108
006109     IF ACCOUNTL = ZERO
006110         MOVE ER-0232            TO EMI-ERROR
006111         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006112         MOVE AL-UABON           TO ACCOUNTA
006113         MOVE -1                 TO ACCOUNTL
006114     ELSE
006115         MOVE AL-UANON           TO ACCOUNTA
006116     END-IF.
006117
006118     IF (PI-CERT-ACCESS-CONTROL = ' ' OR '1' OR '2')  AND
006119        (STATEL = ZERO)
006120         MOVE ER-0233        TO EMI-ERROR
006121         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006122         MOVE AL-UABON       TO STATEA
006123         MOVE -1             TO STATEL
006124     ELSE
006125         MOVE AL-UANON       TO STATEA
006126     END-IF.
006127
006128     IF (PI-CERT-ACCESS-CONTROL = '1' OR '2' OR '4') AND
006129        (CRTCARRL = ZERO)
006130         MOVE ER-0234        TO EMI-ERROR
006131         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006132         MOVE AL-UABON       TO CRTCARRA
006133         MOVE -1             TO CRTCARRL
006134     ELSE
006135         MOVE AL-UANON       TO CRTCARRA
006136     END-IF.
006137
006138     IF PI-CERT-ACCESS-CONTROL = '1' AND
006139        GROUPL = ZERO
006140         MOVE ER-0235            TO EMI-ERROR
006141         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006142         MOVE AL-UABON           TO GROUPA
006143         MOVE -1                 TO GROUPL
006144     ELSE
006145         MOVE AL-UANON           TO GROUPA
006146     END-IF.
006147
006148     IF CRTLNMEL = ZERO
006149         IF LSTNMEI = SPACES OR LOW-VALUES
006150             MOVE ER-0236            TO  EMI-ERROR
006151             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006152             MOVE AL-UABON           TO  CRTLNMEA
006153             MOVE -1                 TO  CRTLNMEL
006154         ELSE
006155             MOVE LSTNMEI            TO  CRTLNMEI
006156             MOVE AL-UANON           TO  CRTLNMEA
006157         END-IF
006158     ELSE
006159         MOVE AL-UANON               TO  CRTLNMEA
006160     END-IF.
006161
006162     IF CRTFNMEL = ZERO
006163         IF FSTNMEI = SPACES OR LOW-VALUES
006164*            NEXT SENTENCE
006165             CONTINUE
006166         ELSE
006167             MOVE FSTNMEI            TO  CRTFNMEI
006168             MOVE AL-UANON           TO  CRTFNMEA
006169         END-IF
006170     END-IF.
006171
006172     IF CRTINITL = ZERO
006173         IF INITI = SPACES OR LOW-VALUES
006174*            NEXT SENTENCE
006175             CONTINUE
006176         ELSE
006177             MOVE INITI              TO  CRTINITI
006178             MOVE AL-UANON           TO  CRTINITA
006179         END-IF
006180     END-IF.
006181
006182 6000-EXIT.
006183     EXIT.
006184
006185     EJECT
006186 6200-EDIT-CERT-DATA.
006187
006188     IF ISSAGEL GREATER ZERO
006189         IF ISSAGEI NOT NUMERIC
006190             MOVE ER-0237        TO EMI-ERROR
006191             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006192             MOVE -1             TO ISSAGEL
006193             MOVE AL-UNBON       TO ISSAGEA
006194         ELSE
006195             MOVE AL-UNNON       TO ISSAGEA
006196         END-IF
006197     END-IF.
006198
006199     IF JNTAGEL GREATER ZERO
006200         IF JNTAGEI NOT NUMERIC
006201             MOVE ER-0238        TO EMI-ERROR
006202             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006203             MOVE -1             TO JNTAGEL
006204             MOVE AL-UNBON       TO JNTAGEA
006205         ELSE
006206             MOVE AL-UNNON       TO JNTAGEA
006207         END-IF
006208     END-IF.
006209
006210     IF LCVCDL GREATER ZERO
006211         MOVE LCVCDI             TO WS-EDIT-BEN-CODE
006212         IF INVALID-BENEFIT-CODE
006213             MOVE ER-7635        TO EMI-ERROR
006214             MOVE -1             TO LCVCDL
006215             MOVE AL-UABON       TO LCVCDA
006216             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006217         ELSE
006218             MOVE LCVCDI         TO WS-BEN-HOLD
006219             MOVE '4'            TO WS-REC-TYPE
006220             PERFORM 7100-READ-BENEFIT THRU 7199-EXIT
006221             IF WS-BEN-ALPHA-HOLD = SPACES
006222                MOVE ER-7635     TO EMI-ERROR
006223                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006224                MOVE -1          TO LCVCDL
006225                MOVE AL-UABON    TO LCVCDA
006226             ELSE
006227                MOVE WS-BEN-ALPHA-HOLD  TO LCVKINDO
006228                MOVE WS-EARNINGS-CALC   TO WS-LF-EARNINGS-CALC
006229                MOVE WS-SPECIAL-CALC-CD TO WS-LF-SPECIAL-CALC-CD
006230                MOVE AL-UANON           TO LCVCDA
006231             END-IF
006232         END-IF
006233     END-IF.
006234
006235     IF ACVCDL GREATER ZERO
006236         MOVE ACVCDI             TO WS-EDIT-BEN-CODE
006237         IF INVALID-BENEFIT-CODE
006238             MOVE ER-7635        TO EMI-ERROR
006239             MOVE -1             TO ACVCDL
006240             MOVE AL-UABON       TO ACVCDA
006241             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006242         ELSE
006243             MOVE ACVCDI         TO WS-BEN-HOLD
006244             MOVE '5'            TO WS-REC-TYPE
006245             PERFORM 7100-READ-BENEFIT THRU 7199-EXIT
006246             IF WS-BEN-ALPHA-HOLD = SPACES
006247                MOVE ER-7635     TO EMI-ERROR
006248                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006249                MOVE -1          TO ACVCDL
006250                MOVE AL-UABON    TO ACVCDA
006251             ELSE
006252                MOVE WS-BEN-ALPHA-HOLD  TO ACVKINDO
006253                MOVE WS-EARNINGS-CALC   TO WS-AH-EARNINGS-CALC
006254                MOVE WS-SPECIAL-CALC-CD TO WS-AH-SPECIAL-CALC-CD
006255                MOVE AL-UANON           TO ACVCDA
006256             END-IF
006257         END-IF
006258     END-IF.
006259
006260     IF LCVOTRML GREATER ZERO
006261         IF LCVOTRMI NOT NUMERIC
006262             MOVE ER-7636        TO EMI-ERROR
006263             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006264             MOVE -1             TO LCVOTRML
006265             MOVE AL-UNBON       TO LCVOTRMA
006266         ELSE
006267             MOVE AL-UNNON       TO LCVOTRMA
006268         END-IF
006269     END-IF.
006270
006271     IF ACVOTRML GREATER ZERO
006272         IF ACVOTRMI NOT NUMERIC
006273             MOVE ER-7636        TO EMI-ERROR
006274             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006275             MOVE -1             TO ACVOTRML
006276             MOVE AL-UNBON       TO ACVOTRMA
006277         ELSE
006278             MOVE AL-UNNON       TO ACVOTRMA
006279         END-IF
006280     END-IF.
006281
006282     IF LCVRATEL GREATER ZERO
006283         
      * EXEC CICS BIF DEEDIT
006284*            FIELD    (LCVRATEI)
006285*            LENGTH   (06)
006286*        END-EXEC
           MOVE 06
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012772' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132373732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006287         IF LCVRATEI NOT NUMERIC
006288             MOVE AL-UNBON       TO LCVRATEA
006289             MOVE -1             TO LCVRATEL
006290             MOVE ER-2280        TO EMI-ERROR
006291             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006292         ELSE
006293             MOVE AL-UNNON       TO LCVRATEA
006294             MOVE LCVRATEI       TO WS-LCVRATE LCVRATEO
006295         END-IF
006296     END-IF.
006297
006298     IF ACVRATEL GREATER ZERO
006299         
      * EXEC CICS BIF DEEDIT
006300*            FIELD    (ACVRATEI)
006301*            LENGTH   (06)
006302*        END-EXEC
           MOVE 06
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012788' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132373838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVRATEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006303         IF ACVRATEI NOT NUMERIC
006304             MOVE AL-UNBON       TO ACVRATEA
006305             MOVE -1             TO ACVRATEL
006306             MOVE ER-2280        TO EMI-ERROR
006307             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006308         ELSE
006309             MOVE AL-UNNON       TO ACVRATEA
006310             MOVE ACVRATEI       TO WS-ACVRATE ACVRATEO
006311         END-IF
006312     END-IF.
006313
006314     IF LCVBENEL GREATER ZERO
006315         
      * EXEC CICS BIF DEEDIT
006316*            FIELD    (LCVBENEI)
006317*            LENGTH   (11)
006318*        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012804' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132383034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCVBENEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006319         IF LCVBENEI NOT NUMERIC
006320             MOVE AL-UNBON       TO LCVBENEA
006321             MOVE -1             TO LCVBENEL
006322             MOVE ER-7637        TO EMI-ERROR
006323             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006324         ELSE
006325             MOVE AL-UNNON       TO LCVBENEA
006326             MOVE LCVBENEI       TO WS-LCVBENE LCVBENEO
006327         END-IF
006328     END-IF.
006329
006330     IF ACVBENEL GREATER ZERO
006331         
      * EXEC CICS BIF DEEDIT
006332*            FIELD    (ACVBENEI)
006333*            LENGTH   (11)
006334*        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012820' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132383230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACVBENEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006335         IF ACVBENEI NOT NUMERIC
006336             MOVE AL-UNBON       TO ACVBENEA
006337             MOVE -1             TO ACVBENEL
006338             MOVE ER-7637        TO EMI-ERROR
006339             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006340         ELSE
006341             MOVE AL-UNNON       TO ACVBENEA
006342             MOVE ACVBENEI       TO WS-ACVBENE ACVBENEO
006343         END-IF
006344     END-IF.
006345
006346     IF LCVCNDTL GREATER ZERO
006347         IF LCVCNDTI NOT = SPACES
006348             MOVE LCVCNDTI        TO DATE-WORK
006349             PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
006350             MOVE NUM-WORK        TO DC-GREG-DATE-1-MDY
006351             MOVE '4'             TO DC-OPTION-CODE
006352             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
006353             IF DATE-CONVERSION-ERROR
006354                 MOVE AL-UABON    TO LCVCNDTA
006355                 MOVE -1          TO LCVCNDTL
006356                 MOVE ER-7638     TO EMI-ERROR
006357                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006358             ELSE
006359                 MOVE AL-UANON    TO LCVCNDTA
006360                 MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTI
006361                 MOVE DC-BIN-DATE-1 TO WS-LCVCNDT
006362             END-IF
006363         END-IF
006364     END-IF.
006365
006366     IF ACVCNDTL GREATER ZERO
006367         IF ACVCNDTI NOT = SPACES
006368             MOVE ACVCNDTI        TO DATE-WORK
006369             PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
006370             MOVE NUM-WORK        TO DC-GREG-DATE-1-MDY
006371             MOVE '4'             TO DC-OPTION-CODE
006372             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
006373             IF DATE-CONVERSION-ERROR
006374                 MOVE AL-UABON    TO ACVCNDTA
006375                 MOVE -1          TO ACVCNDTL
006376                 MOVE ER-7638     TO EMI-ERROR
006377                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006378             ELSE
006379                 MOVE AL-UANON    TO ACVCNDTA
006380                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTI
006381                 MOVE DC-BIN-DATE-1 TO WS-ACVCNDT
006382             END-IF
006383         END-IF
006384     END-IF.
006385
006386     IF ADDONDTL GREATER ZERO
006387         MOVE ADDONDTI        TO DATE-WORK
006388         PERFORM 6990-DEEDIT-DATE THRU 6990-EXIT
006389         MOVE NUM-WORK        TO DC-GREG-DATE-1-MDY
006390         MOVE '4'             TO DC-OPTION-CODE
006391         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
006392         IF DATE-CONVERSION-ERROR
006393             MOVE AL-UABON    TO ADDONDTA
006394             MOVE -1          TO ADDONDTL
006395             MOVE ER-7651     TO EMI-ERROR
006396             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006397         ELSE
006398             MOVE AL-UANON    TO ADDONDTA
006399             MOVE DC-GREG-DATE-1-EDIT TO ADDONDTI
006400             MOVE DC-BIN-DATE-1 TO WS-ADD-ON-DT
006401         END-IF
006402     END-IF.
006403
006404     IF APRL GREATER ZERO
006405         
      * EXEC CICS BIF DEEDIT
006406*            FIELD    (APRI)
006407*            LENGTH   (8)
006408*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012894' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132383934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APRI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006409         IF APRI NOT NUMERIC
006410             MOVE AL-UNBON       TO APRA
006411             MOVE -1             TO APRL
006412             MOVE ER-0248        TO EMI-ERROR
006413             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006414         ELSE
006415             MOVE AL-UNNON       TO APRA
006416             MOVE APRI           TO WS-APR APRO
006417         END-IF
006418     END-IF.
006419
006420     IF PMTFREQL GREATER ZERO
006421         
      * EXEC CICS BIF DEEDIT
006422*            FIELD   (PMTFREQI)
006423*            LENGTH  (2)
006424*        END-EXEC
           MOVE 2
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012910' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132393130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PMTFREQI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006425         IF PMTFREQI NOT NUMERIC
006426             MOVE -1             TO PMTFREQL
006427             MOVE AL-UNBON       TO PMTFREQA
006428             MOVE ER-0258        TO EMI-ERROR
006429             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006430         ELSE
006431             MOVE AL-UNNON       TO PMTFREQA
006432             MOVE PMTFREQI       TO WS-PMTFREQ  PMTFREQO
006433         END-IF
006434     END-IF.
006435
006436     IF INDGRPL GREATER ZERO
006437         IF INDGRPI = 'I' OR 'G'
006438             MOVE AL-UANON       TO INDGRPA
006439         ELSE
006440             MOVE -1 TO INDGRPL
006441             MOVE AL-UABON       TO INDGRPA
006442             MOVE ER-0260        TO EMI-ERROR
006443             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006444         END-IF
006445     END-IF.
006446
006447     IF LOANBALL GREATER ZERO
006448         MOVE LOANBALI           TO DEEDIT-FIELD
006449         
      * EXEC CICS BIF DEEDIT
006450*            FIELD   (DEEDIT-FIELD)
006451*            LENGTH  (12)
006452*        END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00012938' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303132393338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006453         IF DEEDIT-FIELD NOT NUMERIC
006454             MOVE AL-UNBON       TO LOANBALA
006455             MOVE -1             TO LOANBALL
006456             MOVE ER-0247        TO EMI-ERROR
006457             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006458         ELSE
006459             MOVE AL-UNNON       TO LOANBALA
006460             MOVE DEEDIT-FIELD   TO HOLD-LOAN-BAL  LOANBALO
006461         END-IF
006462     END-IF.
006463
006464     IF PREMTYPL GREATER ZERO
006465         IF (PREMTYPI = '1' OR '2' OR '3')
006466             MOVE AL-UANON       TO PREMTYPA
006467         ELSE
006468             MOVE -1             TO PREMTYPL
006469             MOVE AL-UABON       TO PREMTYPA
006470             MOVE ER-0227        TO EMI-ERROR
006471             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006472         END-IF
006473     END-IF.
006474
006475     IF REINCDL GREATER ZERO
006476         MOVE LOW-VALUES         TO  ERREIN-KEY
006477         MOVE PI-COMPANY-CD      TO  REIN-COMP-CD
006478         MOVE REINCDI            TO  REIN-CODE-1
006479                                     REIN-CODE-2
006480                                     REIN-CODE-3
006481         MOVE 'A'                TO  REIN-TYPE
006482         PERFORM 7992-READ-REIN THRU 7992-EXIT
006483         IF REIN-REC-NOT-FOUND
006484             MOVE ER-0266        TO  EMI-ERROR
006485             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006486             MOVE -1             TO  REINCDL
006487             MOVE AL-UABON       TO  REINCDA
006488         ELSE
006489             MOVE AL-UANON       TO  REINCDA
006490         END-IF
006491     END-IF.
006492
006493     IF CERTMTI NOT = 'A'
006494        GO TO 6200-EXIT
006495     END-IF.
006496
006497     IF PREMTYPI = '1'
006498         IF LCVCDL GREATER ZERO
006499             IF WS-LF-SPECIAL-CALC-CD = 'O'
006500                 MOVE ER-0841        TO  EMI-ERROR
006501                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006502                 MOVE -1             TO  PREMTYPL
006503                 MOVE AL-UABON       TO  PREMTYPA
006504             END-IF
006505         END-IF
006506     END-IF.
006507
006508     IF PREMTYPI = '1'
006509         IF ACVCDL GREATER ZERO
006510             IF WS-AH-SPECIAL-CALC-CD = 'O'
006511                 MOVE ER-0841        TO  EMI-ERROR
006512                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006513                 MOVE -1             TO  PREMTYPL
006514                 MOVE AL-UABON       TO  PREMTYPA
006515             END-IF
006516         END-IF
006517     END-IF.
006518
006519     IF PREMTYPI = '2'
006520         IF LCVCDL GREATER ZERO
006521             IF (PI-COMPANY-ID = 'HAN') AND
006522                (WS-LF-SPECIAL-CALC-CD = 'O' OR 'M')
006523                        OR
006524                 WS-LF-SPECIAL-CALC-CD = 'O'
006525*                NEXT SENTENCE
006526                 CONTINUE
006527             ELSE
006528                 MOVE ER-0840        TO  EMI-ERROR
006529                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006530                 MOVE -1             TO  PREMTYPL
006531                 MOVE AL-UABON       TO  PREMTYPA
006532             END-IF
006533         END-IF
006534     END-IF.
006535
006536     IF PREMTYPI = '2'
006537         IF ACVCDL GREATER ZERO
006538             IF (PI-COMPANY-ID = 'HAN')  AND
006539                (WS-AH-SPECIAL-CALC-CD = 'O' OR 'M')
006540                        OR
006541                 WS-AH-SPECIAL-CALC-CD = 'O'
006542*                NEXT SENTENCE
006543                 CONTINUE
006544             ELSE
006545                 MOVE ER-0840        TO  EMI-ERROR
006546                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006547                 MOVE -1             TO  PREMTYPL
006548                 MOVE AL-UABON       TO  PREMTYPA
006549             END-IF
006550         END-IF
006551     END-IF.
006552
006553     IF LCVCDL = ZEROS
006554        GO TO 6200-EXIT
006555     END-IF.
006556
006557     IF PMTFREQL NOT = ZEROS  AND NOT TEX-REG
006558         IF PMTFREQI NOT = ZEROS
006559             MOVE ER-0428        TO EMI-ERROR
006560             MOVE -1             TO PMTFREQL
006561             MOVE AL-UNBON       TO PMTFREQA
006562             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006563             GO TO 6200-CHECK-APR
006564         END-IF
006565     END-IF.
006566
006567     IF PMTFREQL = ZEROS AND TEX-REG
006568         MOVE ER-0429            TO EMI-ERROR
006569         MOVE -1                 TO PMTFREQL
006570         MOVE AL-UNBON           TO PMTFREQA
006571         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006572         GO TO 6200-CHECK-APR
006573     END-IF.
006574
006575     IF LCVOTRMI NOT NUMERIC
006576         GO TO 6200-CHECK-APR
006577     END-IF.
006578
006579     IF WS-PMTFREQ NOT = ZERO
006580         DIVIDE WS-PMTFREQ INTO LCVOTRMI
006581         GIVING DIVIDE-QUOT
006582         REMAINDER DIVIDE-REM
006583         IF DIVIDE-REM GREATER ZERO
006584             MOVE ER-0430            TO EMI-ERROR
006585             MOVE -1                 TO PMTFREQL
006586             MOVE AL-UNBON           TO PMTFREQA
006587             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006588         END-IF
006589     END-IF.
006590
006591 6200-CHECK-APR.
006592     IF NET-PAY AND APRL = ZEROS
006593        MOVE ER-0257             TO EMI-ERROR
006594        MOVE -1                  TO APRL
006595        MOVE AL-UABON            TO APRA
006596        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006597     END-IF.
006598
006599 6200-EXIT.
006600     EXIT.
006601
006602     EJECT
006603 6300-REQUIRED-CERT-EDIT.
006604
006605     IF CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
006606        OR 'B' OR 'H'
006607        GO TO 6300-REQUIRED-AH-EDIT
006608     END-IF
006609
006610     IF LCVCDL = ZERO
006611        MOVE ER-7639             TO EMI-ERROR
006612        MOVE -1                  TO LCVCDL
006613        MOVE AL-UABON            TO LCVCDA
006614        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006615     END-IF.
006616
006617     IF LCVOTRML = ZERO
006618        MOVE ER-7670             TO EMI-ERROR
006619        MOVE -1                  TO LCVOTRML
006620        MOVE AL-UNBON            TO LCVOTRMA
006621        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006622     END-IF.
006623
006624     IF LCVBENEL = ZERO
006625        MOVE ER-7671             TO EMI-ERROR
006626        MOVE -1                  TO LCVBENEL
006627        MOVE AL-UNBON            TO LCVBENEA
006628        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006629     END-IF.
006630
006631     GO TO 6399-EXIT.
006632
006633 6300-REQUIRED-AH-EDIT.
006634
006635     IF ACVCDL = ZERO
006636        MOVE ER-7639             TO EMI-ERROR
006637        MOVE -1                  TO ACVCDL
006638        MOVE AL-UABON            TO ACVCDA
006639        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006640     END-IF.
006641
006642     IF ACVOTRML = ZERO
006643         MOVE ER-7670             TO EMI-ERROR
006644         MOVE -1                  TO ACVOTRML
006645         MOVE AL-UNBON            TO ACVOTRMA
006646         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006647     END-IF.
006648
006649     IF ACVBENEL = ZERO
006650        MOVE ER-7671             TO EMI-ERROR
006651        MOVE -1                  TO ACVBENEL
006652        MOVE AL-UNBON            TO ACVBENEA
006653        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006654     END-IF.
006655
006656 6399-EXIT.
006657      EXIT.
006658
006659     EJECT
006660 6400-TEST-CLAIM-REASONABILITY.
006661
006662     IF WS-INCUR = LOW-VALUES
006663        GO TO 6499-EXIT
006664     END-IF.
006665
006666     IF WS-EFFDT NOT = LOW-VALUES
006667         IF WS-INCUR LESS WS-EFFDT
006668             MOVE ER-0458          TO EMI-ERROR
006669             MOVE -1               TO INCURL
006670             MOVE AL-UABON         TO INCURA
006671             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006672         END-IF
006673     END-IF.
006674
006675     IF CERTMTI = 'A'
006676        GO TO 6460-TEST-NEW-CERT
006677     END-IF.
006678
006679     MOVE PI-COMPANY-CD          TO CERT-COMP-CD.
006680     MOVE CRTCARRI               TO CERT-CARRIER.
006681     MOVE GROUPI                 TO CERT-GROUPING.
006682     MOVE STATEI                 TO CERT-STATE.
006683     MOVE PI-SAVE-ACCOUNT        TO CERT-ACCOUNT.
006684     MOVE PI-SAVE-CERT           TO CERT-CERT-NO.
006685     MOVE PI-SAVE-EFFDT          TO CERT-EFF-DT.
006686
006687
006688     
      * EXEC CICS HANDLE CONDITION
006689*         DUPKEY   (6499-EXIT)
006690*         NOTFND   (6499-EXIT)
006691*    END-EXEC.
      *    MOVE '"$$I                  ! C #00013177' TO DFHEIV0
           MOVE X'222424492020202020202020' &
                X'202020202020202020202120' &
                X'4320233030303133313737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006692
006693     PERFORM 7940-READ-CERT THRU 7940-EXIT.
006694
006695     IF (CLMTYPEI = PI-AH-OVERRIDE-L1  OR
006696        'I' OR 'G' OR 'F'
006697         OR 'B' OR 'H')
006698        MOVE CM-AH-ORIG-TERM     TO DC-ELAPSED-MONTHS
006699     ELSE
006700        MOVE CM-LF-ORIG-TERM     TO DC-ELAPSED-MONTHS
006701     END-IF.
006702
006703     MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
006704     MOVE '6'                    TO DC-OPTION-CODE.
006705     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
006706
006707     IF (WS-INCUR GREATER THAN DC-BIN-DATE-2)
006708        and (dc-elapsed-months > zeros)
006709        MOVE ER-0519             TO EMI-ERROR
006710        MOVE -1                  TO INCURL
006711        MOVE AL-UABON            TO INCURA
006712        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006713     END-IF.
006714
006715     IF CLMCARRI NOT = CRTCARRI
006716        MOVE ER-0562             TO EMI-ERROR
006717        MOVE AL-UABON            TO CLMCARRA
006718        MOVE -1                  TO CLMCARRL
006719        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006720     END-IF.
006721
006722     if (pi-company-id = 'DCC' OR 'VPP')
006723        and (ws-dcc-product-code <> spaces)
006724        continue
006725     else
006726      IF ((CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
006727          OR 'B' OR 'H') AND
006728        CM-AH-BENEFIT-CD = '00')
006729                  OR
006730        ((CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O') AND
006731        CM-LF-BENEFIT-CD = '00')
006732         MOVE ER-0521         TO EMI-ERROR
006733         MOVE AL-UABON        TO CLMTYPEA
006734         MOVE -1              TO CLMTYPEL
006735         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006736      END-IF
006737     end-if
006738
006739*    IF CLMTYPEI = 'I' AND
006740*       CM-AH-CRITICAL-PERIOD = +0
006741*        MOVE ER-0521         TO EMI-ERROR
006742*        MOVE AL-UABON        TO CLMTYPEA
006743*        MOVE -1              TO CLMTYPEL
006744*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006745*    END-IF
006746
006747*    IF PI-COMPANY-ID = 'CRI'
006748*        IF MAINTI = 'A'
006749*            IF CERT-PEND-ISSUE-ERROR  OR
006750*               CERT-PEND-ISSUE-RETURNED
006751*                MOVE -1                 TO CERTNOL
006752*                MOVE AL-UABON           TO CERTNOA
006753*                MOVE ER-7640            TO EMI-ERROR
006754*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006755*            END-IF
006756*        END-IF
006757*    END-IF.
006758
006759     IF (CM-AH-CANCEL-DT  NOT  =  LOW-VALUES AND
006760         (CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
006761          OR 'B' OR 'H'))
006762         IF WS-INCUR GREATER THAN CM-AH-CANCEL-DT
006763             GO TO 6450-DATE-ERROR
006764         END-IF
006765     END-IF.
006766
006767     IF (CM-LF-CANCEL-DT  NOT  =  LOW-VALUES AND
006768         (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'))
006769         IF WS-INCUR GREATER THAN CM-LF-CANCEL-DT
006770             GO TO 6450-DATE-ERROR
006771         END-IF
006772     END-IF.
006773
006774     IF CM-LF-DEATH-DT NOT = LOW-VALUES
006775         IF WS-INCUR GREATER THAN CM-LF-DEATH-DT
006776             GO TO 6450-DATE-ERROR
006777         END-IF
006778     END-IF.
006779
006780     GO TO 6499-EXIT.
006781
006782 6450-DATE-ERROR.
006783     MOVE ER-0520                TO EMI-ERROR.
006784     MOVE -1                     TO INCURL.
006785     MOVE AL-UABON               TO INCURA.
006786     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
006787
006788*    IF (PI-COMPANY-ID = 'FIM' OR 'LGX') AND
006789*       (EMI-SEVERITY-SAVE = 'X')
006790*        ADD 1                TO EMI-FORCABLE-CTR
006791*        SUBTRACT 1 FROM EMI-FATAL-CTR
006792*    END-IF.
006793
006794     GO TO 6499-EXIT.
006795
006796     EJECT
006797 6460-TEST-NEW-CERT.
006798
006799     IF NOT EMI-NO-ERRORS
006800        GO TO 6499-EXIT
006801     END-IF.
006802
006803     MOVE ZEROS                  TO DC-ELAPSED-MONTHS.
006804
006805     IF ((CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
006806          OR 'F' OR 'B' OR 'H') AND
006807         ACVOTRML GREATER ZERO)
006808        MOVE ACVOTRMI       TO DC-ELAPSED-MONTHS
006809     END-IF.
006810
006811     IF (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O') AND
006812         LCVOTRML GREATER ZERO
006813        MOVE LCVOTRMI       TO DC-ELAPSED-MONTHS
006814     END-IF.
006815
006816     MOVE WS-EFFDT               TO DC-BIN-DATE-1.
006817
006818*    IF PI-COMPANY-ID = 'HAN'
006819*        IF ADDONDTL GREATER ZERO
006820*            MOVE WS-ADD-ON-DT   TO  DC-BIN-DATE-1
006821*            IF CLMTYPEI = 'A'
006822*                IF ACVOTRMI NOT = ZEROS
006823*                    COMPUTE DC-ELAPSED-MONTHS = (ACVOTRMI - 1)
006824*                ELSE
006825*                    NEXT SENTENCE
006826*                    CONTINUE
006827*                END-IF
006828*            ELSE
006829*                IF LCVOTRMI NOT = ZEROS
006830*                    COMPUTE DC-ELAPSED-MONTHS = (LCVOTRMI - 1)
006831*                END-IF
006832*            END-IF
006833*        END-IF
006834*    END-IF.
006835
006836     MOVE '6'                    TO DC-OPTION-CODE.
006837     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
006838
006839     IF NO-CONVERSION-ERROR
006840         MOVE DC-BIN-DATE-2      TO WS-EXPIRE
006841     END-IF.
006842
006843     IF WS-INCUR GREATER THAN WS-EXPIRE
006844        MOVE ER-0519             TO EMI-ERROR
006845        MOVE -1                  TO INCURL
006846        MOVE AL-UABON            TO INCURA
006847        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006848     END-IF.
006849
006850     IF CLMCARRI NOT = CRTCARRI
006851        MOVE ER-0562             TO EMI-ERROR
006852        MOVE AL-UABON            TO CLMCARRA
006853        MOVE -1                  TO CLMCARRL
006854        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006855     END-IF.
006856
006857     if (pi-company-id = 'DCC' OR 'VPP')
006858        and (ws-dcc-product-code <> spaces)
006859        continue
006860     else
006861        IF (CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
006862            OR 'B' OR 'H')
006863           and (ACVCDL = ZEROS)
006864           MOVE ER-0521          TO EMI-ERROR
006865           MOVE AL-UABON         TO CLMTYPEA
006866           MOVE -1               TO CLMTYPEL
006867           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006868        END-IF
006869     end-if
006870
006871     if (pi-company-id = 'DCC' OR 'VPP')
006872        and (ws-dcc-product-code <> spaces)
006873        continue
006874     else
006875        IF (CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O')
006876           and (LCVCDL = ZEROS)
006877           MOVE ER-0521          TO EMI-ERROR
006878           MOVE AL-UABON         TO CLMTYPEA
006879           MOVE -1               TO CLMTYPEL
006880           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
006881        END-IF
006882     end-if
006883
006884     IF CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'
006885         IF LCVCNDTL GREATER ZERO
006886             IF WS-INCUR GREATER THAN WS-LCVCNDT
006887                 GO TO 6450-DATE-ERROR
006888             END-IF
006889         END-IF
006890     END-IF.
006891
006892     IF CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
006893        OR 'B' OR 'H'
006894         IF ACVCNDTL GREATER ZERO
006895             IF WS-INCUR GREATER THAN WS-ACVCNDT
006896                 GO TO 6450-DATE-ERROR
006897             END-IF
006898         END-IF
006899     END-IF.
006900
006901 6499-EXIT.
006902     EXIT.
006903
006904 6500-get-acct.
006905
006906     MOVE SPACES                 TO ERACCT-KEY
006907                                    ws-dcc-product-code
006908                                    ws-acct-found-sw
006909
006910     IF CARR-GROUP-ST-ACCNT-CNTL
006911        MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
006912        MOVE CRTCARRI            TO ACCT-CARRIER
006913        MOVE GROUPI              TO ACCT-GROUPING
006914        MOVE STATEI              TO ACCT-STATE
006915        MOVE ACCOUNTI            TO ACCT-ACCOUNT
006916        GO TO 6500-startbr
006917     END-IF
006918
006919     MOVE PI-COMPANY-CD          TO ACCT-COMP-CD
006920     MOVE ACCOUNTI               TO ACCT-ACCOUNT
006921
006922     IF PI-CERT-ACCESS-CONTROL = ' ' OR '2'
006923        MOVE STATEI             TO ACCT-STATE
006924     END-IF
006925
006926     IF PI-CERT-ACCESS-CONTROL = '2' OR '4'
006927        MOVE CRTCARRI           TO ACCT-CARRIER
006928     END-IF
006929
006930     .
006931 6500-startbr.
006932
006933     MOVE ERACCT-KEY             TO SAVE-ERACCT-KEY
006934
006935     
      * EXEC CICS STARTBR
006936*       DATASET     (ERACCT2-DSID)
006937*       RIDFLD      (SAVE-ERACCT-KEY)
006938*       GENERIC
006939*       KEYLENGTH   (20)
006940*       resp        (ws-response)
006941*    END-EXEC
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &  N#00013424' TO DFHEIV0
           MOVE X'262C2020204B472020202047' &
                X'202020202020202020202620' &
                X'204E233030303133343234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 SAVE-ERACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006942
006943     if not ws-resp-normal
006944        go to 6500-exit
006945     end-if
006946
006947     .
006948 6500-readnext.
006949
006950     
      * EXEC CICS READNEXT
006951*       DATASET   (ERACCT2-DSID)
006952*       SET       (ADDRESS OF ACCOUNT-MASTER)
006953*       RIDFLD    (SAVE-ERACCT-KEY)
006954*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013439' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303133343339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 SAVE-ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006955
006956     IF (ACCT-COMP-CD  NOT = AM-COMPANY-CD-A1)
006957        or (ACCT-CARRIER  NOT = AM-VG-CARRIER )
006958        or (ACCT-GROUPING NOT = AM-VG-GROUPING)
006959        or (ACCT-STATE    NOT = AM-VG-STATE   )
006960        or (ACCT-ACCOUNT  NOT = AM-VG-ACCOUNT )
006961        go to 6500-endbr
006962     END-IF
006963
006964     IF (WS-EFFDT >= AM-EFFECTIVE-DT)
006965        and (WS-EFFDT < AM-EXPIRATION-DT)
006966        set acct-found to true
006967        MOVE AM-dcc-product-code to ws-dcc-product-code
006968        GO TO 6500-endbr
006969     END-IF
006970
006971     IF WS-EFFDT >= AM-EXPIRATION-DT
006972         GO TO 6500-readnext
006973     END-IF
006974
006975     .
006976 6500-endbr.
006977
006978     
      * EXEC CICS ENDBR
006979*        DATASET    (ERACCT2-DSID)
006980*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013467' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303133343637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
006981
006982     .
006983 6500-exit.
006984     exit.
006985
006986 6600-CHECK-AUTO-ACTIVITY.
006987
006988     
      * EXEC CICS HANDLE CONDITION
006989*        NOTFND   (6600-NOT-FOUND)
006990*    END-EXEC.
      *    MOVE '"$I                   ! D #00013477' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4420233030303133343737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
006991
006992     MOVE PI-COMPANY-ID              TO  CNTL-COMP-ID.
006993     MOVE 'T'                        TO  CNTL-REC-TYPE.
006994     MOVE SPACES                     TO  CNTL-ACCESS.
006995     MOVE +0                         TO  CNTL-SEQ-NO.
006996
006997     PERFORM 7970-READ-CNTL THRU 7970-EXIT.
006998
006999     IF CF-SYS-ACTIVE-SW (1) = 'N' OR ' '
007000         MOVE 'N'                    TO  WS-REC-FOUND-SW
007001                                         WS-LETTER-SW
007002         GO TO 6600-EXIT
007003     END-IF.
007004
007005     IF CF-SYS-LETTER-ID (1) = SPACES OR LOW-VALUES
007006         MOVE 'N'                     TO  WS-LETTER-SW
007007     ELSE
007008         MOVE 'Y'                     TO  WS-LETTER-SW
007009     END-IF.
007010
007011     MOVE 'Y'                         TO  WS-REC-FOUND-SW.
007012     GO TO 6600-EXIT.
007013
007014 6600-START-AUTO-LETTER-WRITER.
007015
007016     MOVE LOW-VALUES                  TO  W-1523-LINKDATA.
007017     MOVE PROGRAM-INTERFACE-BLOCK     TO  W-1523-COMMON-PI-DATA.
007018
007019     MOVE CF-SYS-LETTER-ID (1)        TO  W-1523-FORM-NUMBER.
007020
007021     IF CF-SYS-RESEND-DAYS (1) NOT = ZEROS
007022         MOVE SAVE-BIN-DATE           TO  DC-BIN-DATE-1
007023         MOVE '6'                     TO  DC-OPTION-CODE
007024         MOVE CF-SYS-RESEND-DAYS (1)  TO  DC-ELAPSED-DAYS
007025         MOVE +0                      TO  DC-ELAPSED-MONTHS
007026         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007027         IF NO-CONVERSION-ERROR
007028             MOVE DC-BIN-DATE-2       TO  W-1523-RESEND-DATE
007029         ELSE
007030             MOVE LOW-VALUES          TO  W-1523-RESEND-DATE
007031         END-IF
007032     END-IF.
007033
007034     IF CF-SYS-FOLLOW-UP-DAYS (1) NOT = ZEROS
007035         MOVE SAVE-BIN-DATE           TO  DC-BIN-DATE-1
007036         MOVE '6'                     TO  DC-OPTION-CODE
007037         MOVE CF-SYS-FOLLOW-UP-DAYS (1)   TO  DC-ELAPSED-DAYS
007038         MOVE +0                      TO  DC-ELAPSED-MONTHS
007039         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007040         IF NO-CONVERSION-ERROR
007041             MOVE DC-BIN-DATE-2       TO  W-1523-FOLLOW-UP-DATE
007042         ELSE
007043             MOVE LOW-VALUES          TO  W-1523-FOLLOW-UP-DATE
007044         END-IF
007045     END-IF.
007046
007047
007048     
      * EXEC CICS LINK
007049*        PROGRAM    (LINK-1523)
007050*        COMMAREA   (W-1523-LINKDATA)
007051*        LENGTH     (W-1523-COMM-LENGTH)
007052*    END-EXEC.
      *    MOVE '."C                   (   #00013537' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303133353337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-1523, 
                 W-1523-LINKDATA, 
                 W-1523-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007053
007054     GO TO 6600-EXIT.
007055
007056 6600-NOT-FOUND.
007057     MOVE 'N'                         TO  WS-REC-FOUND-SW.
007058
007059 6600-EXIT.
007060     EXIT.
007061
007063*************************************************************
007064*****        START OF ACTIVITY FILE OUTPUT PROCESSING      **
007065*************************************************************
007066
007067 6700-OUTPUT-ACTIVITY-RECORD.
007068
007069
007070     
      * EXEC CICS GETMAIN
007071*        SET (ADDRESS OF DAILY-ACTIVITY-RECORD)
007072*        LENGTH (DLYACTV-LENGTH)
007073*        INITIMG (WS-BLANK)
007074*    END-EXEC.
      *    MOVE ',"IL                  $   #00013558' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303133353538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DLYACTV-LENGTH, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007075
007076     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
007077     MOVE PI-COMPANY-CD          TO DA-COMP-CD.
007078     MOVE CLMCARRI               TO DA-CARRIER.
007079     IF CLMNOL EQUAL ZERO
007080         MOVE WS-CLAIM-NUMBER    TO DA-CLAIM-NO
007081     ELSE
007082         MOVE CLMNOI             TO DA-CLAIM-NO
007083     END-IF.
007084     MOVE CERTNOI                TO DA-CERT-PRIME.
007085     MOVE SUFXI                  TO DA-CERT-SFX.
007086     MOVE +0                     TO DA-TRAILER-SEQ-NO.
007087     MOVE 'A'                    TO DA-RECORD-TYPE.
007088
007089     
      * EXEC CICS HANDLE CONDITION
007090*        NOTOPEN (6700-NOTOPEN-ERROR)
007091*        DUPREC (6700-EXIT)
007092*    END-EXEC.
      *    MOVE '"$J%                  ! E #00013577' TO DFHEIV0
           MOVE X'22244A252020202020202020' &
                X'202020202020202020202120' &
                X'4520233030303133353737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007093
007094     
      * EXEC CICS WRITE
007095*        DATASET (DLYACTV-DSID)
007096*        RIDFLD (DA-KEY)
007097*        FROM (DAILY-ACTIVITY-RECORD)
007098*        LENGTH (DLYACTV-LENGTH)
007099*    END-EXEC.
      *    MOVE '&$ L                  ''   #00013582' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303133353832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DLYACTV-DSID, 
                 DAILY-ACTIVITY-RECORD, 
                 DLYACTV-LENGTH, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007100     GO TO 6700-EXIT.
007101
007102 6700-NOTOPEN-ERROR.
007103     MOVE '2955'                 TO EMI-ERROR.
007104     MOVE -1                     TO MAINTL.
007105     MOVE AL-UANON               TO MAINTA.
007106     PERFORM 9900-ERROR-FORMAT THRU
007107             9900-EXIT.
007108     GO TO 8200-SEND-DATAONLY.
007109
007110 6700-EXIT.
007111     EXIT.
007112
007113*************************************************************
007114*****         END OF ACTIVITY FILE OUTPUT PROCESSING        *
007115*************************************************************
007117 6990-DEEDIT-DATE.
007118
007119     
      * EXEC CICS BIF DEEDIT
007120*        FIELD   (DATE-WORK)
007121*        LENGTH  (8)
007122*    END-EXEC.
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00013606' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303133363036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DATE-WORK, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007123 6990-EXIT.
007124     EXIT.
007125
007126     EJECT
007127 7000-BUILD-OUTPUT-MAP.
007128
007129     IF CLAIM-SWITCH = 'N'
007130         GO TO 7050-BUILD-MAP-CERT-DATA
007131     END-IF.
007132
007133     IF CL-ASSOC-CERT-TOTAL = +0 OR +1
007134         MOVE SPACES                 TO SEQUO
007135         MOVE AL-SABOF               TO SEQUA
007136     ELSE
007137         MOVE CL-ASSOC-CERT-SEQU     TO WS-CURRENT-SEQU
007138         MOVE CL-ASSOC-CERT-TOTAL    TO WS-OF-SEQU
007139         MOVE WS-CLAIM-SEQU          TO SEQUO
007140         MOVE AL-SABON               TO SEQUA
007141     END-IF.
007142
007143     MOVE CL-CARRIER             TO CLMCARRO.
007144     MOVE CL-CLAIM-TYPE          TO CLMTYPEO.
007145     MOVE CL-PRIME-CERT-PRIME    TO PCERTNOO.
007146     MOVE CL-PRIME-CERT-SFX      TO PSUFXO.
007147     MOVE CL-INSURED-LAST-NAME   TO LSTNMEO.
007148     MOVE CL-INSURED-1ST-NAME    TO FSTNMEO.
007149     MOVE CL-INSURED-MID-INIT    TO INITO.
007150     MOVE CL-INSURED-SEX-CD      TO SEXO.
007151     move cl-insured-type        to instypeo
007152*    MOVE CL-INSURED-OCC-CD      TO OCCCDO.
007153
007154     MOVE AL-UANON               TO CLMCARRA
007155                                    CLMTYPEA
007156                                    PCERTNOA
007157                                    PSUFXA
007158                                    LSTNMEA
007159                                    FSTNMEA
007160                                    INITA
007161                                    SEXA
007162
007163     MOVE CL-PRIME-CERT-NO       TO PI-PRIMARY-CERT-NO.
007164
007165     IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES
007166         MOVE ' '                 TO DC-OPTION-CODE
007167         MOVE CL-INSURED-BIRTH-DT TO DC-BIN-DATE-1
007168         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007169         IF NOT DATE-CONVERSION-ERROR
007170             MOVE DC-GREG-DATE-1-EDIT TO BIRTHDTO
007171             MOVE AL-UANON            TO BIRTHDTA
007172         END-IF
007173     END-IF.
007174
007175     MOVE CL-SOC-SEC-NO          TO SSNO.
007176     MOVE AL-UANON               TO SSNA.
007177
007178     IF CL-INCURRED-DT NOT = LOW-VALUES
007179         MOVE CL-INCURRED-DT     TO DC-BIN-DATE-1
007180         MOVE ' '                TO DC-OPTION-CODE
007181         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007182         IF NOT DATE-CONVERSION-ERROR
007183             MOVE DC-GREG-DATE-1-EDIT TO INCURO
007184             MOVE AL-UANON            TO INCURA
007185         END-IF
007186     END-IF.
007187
007188     IF CL-REPORTED-DT NOT = LOW-VALUES
007189         MOVE ' '                TO DC-OPTION-CODE
007190         MOVE CL-REPORTED-DT     TO DC-BIN-DATE-1
007191         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007192         IF NOT DATE-CONVERSION-ERROR
007193             MOVE DC-GREG-DATE-1-EDIT TO REPORTO
007194             MOVE AL-UANON            TO REPORTA
007195         END-IF
007196     END-IF.
007197
007198*    IF CL-EST-END-OF-DISAB-DT NOT = LOW-VALUES
007199*        MOVE ' '                 TO DC-OPTION-CODE
007200*        MOVE CL-EST-END-OF-DISAB-DT TO DC-BIN-DATE-1
007201*        PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007202*        IF NOT DATE-CONVERSION-ERROR
007203*            MOVE DC-GREG-DATE-1-EDIT TO ESTENDO
007204*            MOVE AL-UANON            TO ESTENDA
007205*        END-IF
007206*    END-IF.
007207
007208     MOVE WS-DIAGNOSIS-DESCRIPT  TO DIAGO.
007209     MOVE WS-ICD-CODE-1          TO ICD1O.
007210     MOVE WS-ICD-CODE-2          TO ICD2O.
007211*    MOVE CL-CAUSE-CD            TO CAUSEO.
007212     MOVE AT-CURRENT-MANUAL-RESERVE TO MANRSVO.
007213     MOVE CL-RELATED-CLAIM-NO    TO RELCLMO.
007214     MOVE CL-PROCESSOR-ID        TO PROCCDO.
007215     MOVE CL-PRIORITY-CD         TO PRICDO.
007216     MOVE AL-UANON               TO DIAGA
007217                                    ICD1A
007218                                    ICD2A
007219*                                   CAUSEA
007220                                    RELCLMA
007221                                    PROCCDA
007222                                    PRICDA.
007223     MOVE AL-UNNON               TO MANRSVA.
007224
007225*    IF PI-COMPANY-ID  = 'ACC'  OR  'FDL' OR 'LGX'
007226*        MOVE CL-PRODUCT-CD          TO PRODCDO
007227*        MOVE AL-UANON               TO PRODCDA
007228*    END-IF.
007229
007230     MOVE CL-SUPV-ATTN-CD        TO SUPVO.
007231     MOVE CL-FILE-LOCATION       TO FILETOO.
007232     MOVE CL-BENEFICIARY         TO BENECDO.
007233     MOVE AL-UANON               TO SUPVA
007234                                    FILETOA
007235                                    BENECDA.
007236
007237     IF CLAIM-SWITCH = 'X'
007238         GO TO 7099-EXIT
007239     END-IF.
007240
007241     EJECT
007242 7050-BUILD-MAP-CERT-DATA.
007243     MOVE CM-CERT-PRIME          TO CERTNOO.
007244     MOVE CM-CERT-SFX            TO SUFXO.
007245     MOVE AL-SANON               TO CERTNOA
007246                                    SUFXA.
007247
007248     IF CM-CERT-EFF-DT NOT = LOW-VALUES
007249         MOVE CM-CERT-EFF-DT     TO DC-BIN-DATE-1
007250         MOVE ' '                TO DC-OPTION-CODE
007251         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007252         IF NOT DATE-CONVERSION-ERROR
007253             MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
007254         END-IF
007255     END-IF.
007256
007257     MOVE CM-ACCOUNT             TO ACCOUNTO.
007258     MOVE CM-STATE               TO STATEO.
007259     MOVE CM-CARRIER             TO CRTCARRO.
007260     MOVE CM-GROUPING            TO GROUPO.
007261     MOVE CM-INSURED-LAST-NAME   TO CRTLNMEO.
007262     MOVE CM-INSURED-FIRST-NAME  TO CRTFNMEO.
007263     MOVE CM-INSURED-INITIAL2    TO CRTINITO.
007264     MOVE CM-INSURED-ISSUE-AGE   TO ISSAGEO.
007265     MOVE CM-JT-LAST-NAME        TO JNTLNMEO.
007266     MOVE CM-JT-FIRST-NAME       TO JNTFNMEO.
007267     MOVE CM-JT-INITIAL          TO JNTINITO.
007268     MOVE CM-INSURED-JOINT-AGE   TO JNTAGEO.
007269     MOVE AL-UANON               TO ACCOUNTA
007270                                    STATEA
007271                                    CRTCARRA
007272                                    GROUPA
007273                                    CRTLNMEA
007274                                    CRTFNMEA
007275                                    CRTINITA
007276                                    JNTLNMEA
007277                                    JNTFNMEA
007278                                    JNTINITA.
007279     MOVE AL-UNNON               TO ISSAGEA
007280                                    JNTAGEA.
007281
007282     IF CM-SSN-STATE   = CM-STATE AND
007283        CM-SSN-ACCOUNT = CM-ACCOUNT-PRIME
007284*        NEXT SENTENCE
007285         CONTINUE
007286     ELSE
007287         MOVE CM-SOC-SEC-NO      TO CRTSSNO
007288     END-IF.
007289
007290     MOVE AL-UANON               TO CRTSSNA.
007291
007292     MOVE SAVE-DATE              TO DC-GREG-DATE-1-EDIT.
007293     MOVE '2'                    TO DC-OPTION-CODE.
007294     PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
007295     MOVE DC-BIN-DATE-1          TO SAVE-CURRENT-DATE.
007296
007297*** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
007298     MOVE SPACES                 TO ELCNTL-KEY.
007299     MOVE CM-STATE               TO CNTL-ACCESS.
007300     MOVE '3'                    TO CNTL-REC-TYPE.
007301     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
007302     MOVE ZEROS                  TO CNTL-SEQ-NO.
007303
007304
007305     
      * EXEC CICS READ
007306*        DATASET  (ELCNTL-DSID)
007307*        SET      (ADDRESS OF CONTROL-FILE)
007308*        RIDFLD   (ELCNTL-KEY)
007309*        RESP     (WS-RESPONSE)
007310*    END-EXEC.
      *    MOVE '&"S        E          (  N#00013792' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303133373932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
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
           
007311
007312     IF WS-RESP-NOTFND
007313          MOVE ER-2848           TO EMI-ERROR
007314          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007315          GO TO 8100-SEND-INITIAL-MAP
007316     END-IF.
007317
007318     MOVE CF-ST-FREE-LOOK-PERIOD TO CP-FREE-LOOK.
007319     MOVE CF-ST-VFY-2ND-BENE TO PI-ST-VFY-2ND-BENE.
007320     MOVE CF-ST-CAUSAL-STATE TO PI-ST-CAUSAL-STATE.
007321     EJECT
007322     IF CM-LF-BENEFIT-CD = '00'
007323         MOVE ZEROS              TO LCVOTRMO
007324                                    LCVRTRMO
007325                                    LCVRATEO
007326                                    LCVBENEO
007327         MOVE SPACES             TO LCVDSCRO
007328                                    LCVCDO
007329                                    LCVKINDO
007330                                    LCVFORMO
007331                                    LCVCNDTO
007332                                    LCVEXITO
007333                                    LCVSTATO
007334         IF CLMTYPEL GREATER THAN ZERO AND
007335            CLMTYPEI = PI-LIFE-OVERRIDE-L1 OR 'O'
007336                MOVE ER-0521     TO EMI-ERROR
007337                MOVE AL-UABON    TO CLMTYPEA
007338                MOVE -1          TO CLMTYPEL
007339                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007340                GO TO 7060-FILL-AH-COVERAGE
007341         ELSE
007342             GO TO 7060-FILL-AH-COVERAGE
007343         END-IF
007344     END-IF.
007345
007346     MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
007347     MOVE AL-UANON               TO LCVDSCRA.
007348     MOVE CM-LF-BENEFIT-CD       TO WS-BEN-HOLD.
007349     MOVE '4'                    TO WS-REC-TYPE.
007350     PERFORM 7100-READ-BENEFIT THRU 7199-EXIT.
007351     MOVE WS-BEN-ALPHA-HOLD      TO LCVKINDO.
007352     MOVE CM-LF-BENEFIT-CD       TO LCVCDO.
007353     MOVE AL-UANON               TO LCVCDA.
007354     MOVE CM-LF-ORIG-TERM        TO LCVOTRMO
007355                                    CP-ORIGINAL-TERM.
007356     MOVE AL-UNNON               TO LCVOTRMA.
007357     MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
007358     MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
007359     MOVE SAVE-CURRENT-DATE      TO CP-VALUATION-DT.
007360     MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
007361     MOVE '4'                    TO CP-REM-TERM-METHOD.
007362     MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
007363
007364     PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.
007365     MOVE CP-REMAINING-TERM-3    TO LCVRTRMO.
007366
007367     IF CM-LF-PREMIUM-RATE NUMERIC
007368         MOVE CM-LF-PREMIUM-RATE TO LCVRATEO
007369     ELSE
007370         MOVE ZEROS              TO LCVRATEO
007371     END-IF.
007372
007373     MOVE AL-UNNON               TO LCVRATEA.
007374
007375     IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC
007376         MOVE ZEROS              TO CM-LF-ALT-BENEFIT-AMT
007377     END-IF.
007378
007379     COMPUTE LCVBENEO = CM-LF-BENEFIT-AMT + CM-LF-ALT-BENEFIT-AMT.
007380
007381     MOVE AL-UNNON               TO LCVBENEA.
007382
007383     IF CM-POLICY-FORM-NO NOT = SPACES
007384         MOVE CM-POLICY-FORM-NO  TO LCVFORMO
007385     ELSE
007386         MOVE WS-FORM-HOLD       TO LCVFORMO
007387     END-IF.
007388
007389     MOVE SPACES                 TO WS-FORM-HOLD.
007390     MOVE 'L'                    TO WS-REC-TYPE.
007391     PERFORM 7400-READ-STATE THRU 7499-EXIT.
007392
007393     IF WS-FORM-HOLD NOT = SPACES
007394         MOVE WS-FORM-HOLD       TO LCVFORMO
007395         MOVE AL-UANON           TO LCVFORMA
007396     END-IF.
007397
007398     IF CM-LF-CURRENT-STATUS = '8'
007399         IF CM-LF-CANCEL-DT NOT = LOW-VALUES
007400             MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1
007401             MOVE ' '             TO DC-OPTION-CODE
007402             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007403             IF NOT DATE-CONVERSION-ERROR
007404                 MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO
007405                 MOVE AL-UANON            TO LCVCNDTA
007406             END-IF
007407         END-IF
007408     END-IF.
007409
007410     IF CM-LF-CURRENT-STATUS = '7'
007411         IF CM-LF-DEATH-DT NOT = LOW-VALUES
007412             MOVE CM-LF-DEATH-DT TO DC-BIN-DATE-1
007413             MOVE ' '            TO DC-OPTION-CODE
007414             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007415             IF NOT DATE-CONVERSION-ERROR
007416                 MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO
007417                 MOVE AL-UANON   TO LCVCNDTA
007418             END-IF
007419         END-IF
007420     END-IF.
007421
007422     IF CM-LF-DEATH-EXIT-DT NOT = LOW-VALUES
007423         IF CM-LF-DEATH-EXIT-DT NOT = SPACES
007424             MOVE ' '            TO DC-OPTION-CODE
007425             MOVE CM-LF-DEATH-EXIT-DT
007426                                 TO DC-BIN-DATE-1
007427             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007428             IF NOT DATE-CONVERSION-ERROR
007429                 MOVE DC-GREG-DATE-1-EDIT
007430                                 TO LCVEXITO
007431             END-IF
007432         END-IF
007433     END-IF.
007434
007435     IF CM-LF-CURRENT-STATUS = '1' OR = '4'
007436         IF CP-REMAINING-TERM-3 = ZEROS
007437             MOVE 'EXPIRED'      TO LCVSTATO
007438         ELSE
007439             MOVE 'ACTIVE'       TO LCVSTATO
007440         END-IF
007441     END-IF.
007442
007443     IF CM-LF-CURRENT-STATUS = '2'
007444        MOVE 'PEND  '            TO LCVSTATO
007445     END-IF.
007446
007447     IF CM-LF-CURRENT-STATUS = '3'
007448        MOVE 'RESTORE'           TO LCVSTATO
007449     END-IF.
007450
007451     IF CM-LF-CURRENT-STATUS = '5'
007452        MOVE 'REISSUE'           TO LCVSTATO
007453     END-IF.
007454
007455     IF CM-LF-CURRENT-STATUS = '6'
007456        MOVE 'LMP DIS'           TO LCVSTATO
007457     END-IF.
007458
007459     IF CM-LF-CURRENT-STATUS = '7'
007460        MOVE 'DEATH  '           TO LCVSTATO
007461     END-IF.
007462
007463     IF CM-LF-CURRENT-STATUS = '8'
007464        MOVE 'CANCEL '           TO LCVSTATO
007465     END-IF.
007466
007467     IF CM-LF-CURRENT-STATUS = '9'
007468        MOVE 'RE-ONLY'           TO LCVSTATO
007469     END-IF.
007470
007471     IF CM-LF-CURRENT-STATUS = 'V'
007472        MOVE 'VOID   '           TO LCVSTATO
007473     END-IF.
007474
007475     IF CM-LF-CURRENT-STATUS = 'D'
007476        MOVE 'DECLINE'           TO LCVSTATO
007477     END-IF.
007478
007479     IF PI-COMPANY-ID = 'CID'
007480         IF CM-LF-BENEFIT-CD >= '2O' AND
007481            CM-LF-BENEFIT-CD <= '2V'
007482             MOVE ER-0878       TO EMI-ERROR
007483             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007484         END-IF
007485     END-IF.
007486
007487
007488     EJECT
007489 7060-FILL-AH-COVERAGE.
007490
007491     IF CM-AH-BENEFIT-CD = '00'
007492         MOVE ZEROS              TO ACVOTRMO
007493                                    ACVRTRMO
007494                                    ACVRATEO
007495                                    ACVBENEO
007496         MOVE SPACES             TO ACVDSCRO
007497                                    ACVCDO
007498                                    ACVKINDO
007499                                    ACVFORMO
007500                                    ACVCNDTO
007501                                    ACVEXITO
007502                                    ACVSTATO
007503         IF (CLMTYPEL GREATER THAN ZERO AND
007504             (CLMTYPEI = PI-AH-OVERRIDE-L1 OR 'I' OR 'G' OR 'F'
007505              OR 'B' OR 'H'))
007506             MOVE ER-0521     TO EMI-ERROR
007507             MOVE AL-UABON    TO CLMTYPEA
007508             MOVE -1          TO CLMTYPEL
007509             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
007510             GO TO 7070-BUILD-MAP-FINISH
007511         ELSE
007512             GO TO 7070-BUILD-MAP-FINISH
007513         END-IF
007514     END-IF.
007515
007516     MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
007517     MOVE CM-AH-BENEFIT-CD       TO ACVCDO WS-BEN-HOLD.
007518     MOVE AL-UANON               TO ACVDSCRA
007519                                    ACVCDA.
007520     MOVE '5'                    TO WS-REC-TYPE.
007521     PERFORM 7100-READ-BENEFIT THRU 7199-EXIT.
007522     MOVE WS-BEN-ALPHA-HOLD      TO ACVKINDO.
007523
007524     MOVE CM-AH-ORIG-TERM        TO ACVOTRMO
007525                                    CP-ORIGINAL-TERM.
007526     MOVE AL-UNNON               TO ACVOTRMA.
007527
007528     MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT.
007529     MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE.
007530     MOVE SAVE-CURRENT-DATE      TO CP-VALUATION-DT.
007531     MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
007532     MOVE '4'                    TO CP-REM-TERM-METHOD.
007533     MOVE PI-COMPANY-ID          TO CP-COMPANY-ID.
007534
007535     PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.
007536     MOVE CP-REMAINING-TERM-3    TO ACVRTRMO.
007537
007538     IF CM-AH-PREMIUM-RATE NUMERIC
007539         MOVE CM-AH-PREMIUM-RATE TO ACVRATEO
007540     ELSE
007541         MOVE ZEROS              TO ACVRATEO
007542     END-IF.
007543
007544     MOVE CM-AH-BENEFIT-AMT      TO ACVBENEO.
007545     MOVE AL-UNNON               TO ACVRATEA
007546                                    ACVBENEA.
007547
007548     IF CM-POLICY-FORM-NO NOT = SPACES
007549         MOVE CM-POLICY-FORM-NO  TO ACVFORMO
007550     ELSE
007551         MOVE WS-FORM-HOLD       TO ACVFORMO
007552         MOVE SPACES TO WS-FORM-HOLD
007553         MOVE 'A'                TO WS-REC-TYPE
007554         PERFORM 7400-READ-STATE THRU 7499-EXIT
007555         IF WS-FORM-HOLD NOT = SPACES
007556             MOVE WS-FORM-HOLD   TO ACVFORMO
007557         END-IF
007558     END-IF.
007559
007560     MOVE AL-UANON               TO ACVFORMA.
007561
007562     IF CM-AH-CURRENT-STATUS = '8'
007563         IF CM-AH-CANCEL-DT NOT = LOW-VALUES
007564             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1
007565             MOVE ' '             TO DC-OPTION-CODE
007566             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007567             IF NOT DATE-CONVERSION-ERROR
007568                MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO
007569                MOVE AL-UANON    TO ACVCNDTA
007570             END-IF
007571         END-IF
007572     END-IF.
007573
007574     IF CM-AH-CURRENT-STATUS = '6' OR '7'
007575         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES
007576             MOVE CM-AH-SETTLEMENT-DT TO DC-BIN-DATE-1
007577             MOVE ' '             TO DC-OPTION-CODE
007578             PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007579             IF NOT DATE-CONVERSION-ERROR
007580                MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO
007581                MOVE AL-UANON            TO ACVCNDTA
007582             END-IF
007583         END-IF
007584     END-IF.
007585
007586     IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES
007587         MOVE ' '                TO DC-OPTION-CODE
007588         MOVE CM-AH-SETTLEMENT-EXIT-DT TO DC-BIN-DATE-1
007589         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007590         IF NOT DATE-CONVERSION-ERROR
007591             MOVE DC-GREG-DATE-1-EDIT TO ACVEXITO
007592         END-IF
007593     END-IF.
007594
007595     IF CM-AH-CURRENT-STATUS = '1' OR = '4'
007596         IF CP-REMAINING-TERM-3 = ZEROS
007597             MOVE 'EXPIRED'      TO ACVSTATO
007598         ELSE
007599             MOVE 'ACTIVE'       TO ACVSTATO
007600         END-IF
007601     END-IF.
007602
007603     IF CM-AH-CURRENT-STATUS = '2'
007604         MOVE 'PEND  '           TO ACVSTATO
007605     END-IF.
007606
007607     IF CM-AH-CURRENT-STATUS = '3'
007608         MOVE 'RESTORE'          TO ACVSTATO
007609     END-IF.
007610
007611     IF CM-AH-CURRENT-STATUS = '5'
007612         MOVE 'REISSUE'          TO ACVSTATO
007613     END-IF.
007614
007615     IF CM-AH-CURRENT-STATUS = '6'
007616         MOVE 'LMP DIS'          TO ACVSTATO
007617     END-IF.
007618
007619     IF CM-AH-CURRENT-STATUS = '7'
007620         MOVE 'DEATH  '          TO ACVSTATO
007621     END-IF.
007622
007623     IF CM-AH-CURRENT-STATUS = '8'
007624         MOVE 'CANCEL '          TO ACVSTATO
007625     END-IF.
007626
007627     IF CM-AH-CURRENT-STATUS = '9'
007628         MOVE 'RE-ONLY'          TO ACVSTATO
007629     END-IF.
007630
007631     IF CM-AH-CURRENT-STATUS = 'V'
007632         MOVE 'VOID   '          TO ACVSTATO
007633     END-IF.
007634
007635     IF CM-AH-CURRENT-STATUS = 'D'
007636         MOVE 'DECLINE'          TO ACVSTATO
007637     END-IF.
007638
007639     IF PI-COMPANY-ID = 'DCC' OR 'VPP'
007640        IF CM-ACCOUNT (9:2) = 'BI'
007641           MOVE CM-COMPANY-CD    TO CTRLR-COMP-CD
007642           MOVE CM-CARRIER       TO CTRLR-CARRIER
007643           MOVE CM-GROUPING      TO CTRLR-GROUPING
007644           MOVE CM-STATE         TO CTRLR-STATE
007645           MOVE CM-ACCOUNT       TO CTRLR-ACCOUNT
007646           MOVE CM-CERT-EFF-DT   TO CTRLR-EFF-DT
007647           MOVE CM-CERT-NO       TO CTRLR-CERT-NO
007648           MOVE 'C'              TO CTRLR-REC-TYPE
007649           
      * EXEC CICS READ
007650*            DATASET  (ELCRTT-DSID)
007651*            SET      (ADDRESS OF CERTIFICATE-TRAILERS)
007652*            RIDFLD   (ELCRTT-KEY)
007653*            RESP     (WS-RESPONSE)
007654*          END-EXEC
      *    MOVE '&"S        E          (  N#00014136' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303134313336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-DSID, 
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
007655           IF WS-RESP-NORMAL
007656               MOVE CS-VIN-NUMBER   TO WS-DIAG-VIN
007657               MOVE WS-DIAG-VIN-MSG TO DIAGO
007658               MOVE AL-UANON        TO DIAGA
007659           END-IF
007660        END-IF
007661     END-IF.
007662
007663     EJECT
007664 7070-BUILD-MAP-FINISH.
007665     MOVE CM-LOAN-APR            TO APRO.
007666     MOVE CM-PAY-FREQUENCY       TO PMTFREQO.
007667     MOVE CM-IND-GRP-TYPE        TO INDGRPO.
007668     MOVE CM-PREMIUM-TYPE        TO PREMTYPO.
007669     MOVE AL-UNNON               TO APRA
007670                                    PMTFREQA.
007671     MOVE AL-UANON               TO INDGRPA
007672                                    PREMTYPA.
007673
007674     IF (CM-SPECIAL-REIN-CODE NOT = SPACES AND LOW-VALUES)
007675         MOVE CM-SPECIAL-REIN-CODE   TO REINCDO
007676         MOVE AL-UANON               TO REINCDA
007677     END-IF.
007678
007679     IF CM-LAST-ADD-ON-DT = SPACES OR LOW-VALUES
007680*        NEXT SENTENCE
007681         CONTINUE
007682     ELSE
007683         MOVE CM-LAST-ADD-ON-DT  TO DC-BIN-DATE-1
007684         MOVE ' '                TO DC-OPTION-CODE
007685         PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
007686         IF NOT DATE-CONVERSION-ERROR
007687             MOVE DC-GREG-DATE-1-EDIT TO ADDONDTO
007688             MOVE AL-UANON            TO ADDONDTA
007689         END-IF
007690     END-IF.
007691
007692
007693     IF CLAIM-SWITCH = 'N'
007694         IF SEXL = ZEROS
007695             MOVE CM-INSURED-SEX TO SEXO
007696*            MOVE AL-UANON       TO SEXA
007697         END-IF
007698     END-IF.
007699
007700
007701     IF CM-MEMB-ACCOUNT NOT = CM-ACCOUNT-PRIME
007702         MOVE CM-MEMBER-NO       TO MEMBERO
007703         MOVE AL-UANON           TO MEMBERA
007704     END-IF.
007705
007706     IF MAINTI = 'A'
007707         IF (LOANNOL = ZEROS OR
007708             EIBAID = DFHPF5)
007709             MOVE CM-LOAN-NUMBER TO LOANNOO
007710             MOVE AL-UANON       TO LOANNOA
007711         ELSE
007712*            NEXT SENTENCE
007713             CONTINUE
007714         END-IF
007715     ELSE
007716         MOVE CM-LOAN-NUMBER     TO LOANNOO
007717         MOVE AL-UANON           TO LOANNOA
007718     END-IF.
007719
007720     IF MAINTI = 'A'
007721         IF (LOANBALL = ZEROS OR
007722             EIBAID = DFHPF5)
007723             MOVE CM-LOAN-BALANCE TO LOANBALO
007724             MOVE AL-UNNON       TO LOANBALA
007725         ELSE
007726*            NEXT SENTENCE
007727             CONTINUE
007728         END-IF
007729     ELSE
007730         MOVE CM-LOAN-BALANCE    TO LOANBALO
007731         MOVE AL-UNNON           TO LOANBALA
007732     END-IF.
007733
007734     IF CM-PREMIUM-TYPE NOT = '3'
007735         IF NOT CERT-WAS-CREATED-FOR-CLAIM
007736             PERFORM 7200-PROTECT-CERT-FIELDS
007737         ELSE
007738             PERFORM 7300-RESET-KEY-ATTRBS
007739         END-IF
007740     ELSE
007741         PERFORM 7300-RESET-KEY-ATTRBS
007742     END-IF.
007743
007744*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'
007745*        NEXT SENTENCE
007746*        CONTINUE
007747*    ELSE
007748         MOVE AL-SANOF           TO LCVRATEA  ACVRATEA.
007749*    END-IF.
007750
007751 7099-EXIT.
007752     EXIT.
007753
007754     EJECT
007755 7100-READ-BENEFIT.
007756
007757     
      * EXEC CICS HANDLE CONDITION
007758*         NOTFND   (7120-NOT-FOUND)
007759*    END-EXEC.
      *    MOVE '"$I                   ! F #00014244' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4620233030303134323434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007760
007761     MOVE SPACES                 TO ELCNTL-KEY.
007762     MOVE WS-BEN-HOLD            TO CNTL-BENEFIT.
007763     MOVE WS-REC-TYPE            TO CNTL-REC-TYPE.
007764     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
007765     MOVE ZEROS                  TO CNTL-SEQ-NO.
007766
007767 7105-READ-FILE.
007768     PERFORM 7975-READ-CNTL-GTEQ THRU 7975-EXIT.
007769
007770     IF PI-COMPANY-ID NOT = CF-COMPANY-ID  OR
007771        WS-REC-TYPE   NOT = CF-RECORD-TYPE
007772          GO TO 7120-NOT-FOUND
007773     END-IF.
007774
007775     MOVE 1                      TO SUB.
007776 7110-LOOP.
007777     IF SUB = 9
007778         GO TO 7120-NOT-FOUND
007779     END-IF.
007780
007781     IF WS-BEN-HOLD NOT = CF-BENEFIT-CODE (SUB)
007782         ADD 1                   TO SUB
007783         GO TO 7110-LOOP
007784     END-IF.
007785
007786     if ws-rec-type = '4'
007787        move cf-joint-indicator (sub)
007788                                 to ws-lf-joint-indicator
007789     else
007790        move cf-joint-indicator (sub)
007791                                 to ws-ah-joint-indicator
007792     end-if
007793     MOVE CF-BENEFIT-ALPHA (SUB)    TO WS-BEN-ALPHA-HOLD.
007794     MOVE CF-CO-EARNINGS-CALC (SUB) TO WS-EARNINGS-CALC.
007795     MOVE CF-SPECIAL-CALC-CD (SUB)  TO WS-SPECIAL-CALC-CD.
007796
007797     GO TO 7199-EXIT.
007798
007799 7120-NOT-FOUND.
007800     MOVE SPACES                 TO WS-BEN-ALPHA-HOLD
007801                                    WS-FORM-HOLD.
007802
007803 7199-EXIT.
007804      EXIT.
007805
007806     EJECT
007807 7200-PROTECT-CERT-FIELDS.
007808     MOVE AL-SANON           TO CERTMTA EFFDTA ACCOUNTA STATEA
007809                                CRTCARRA GROUPA CRTLNMEA CRTFNMEA
007810                                LCVFORMA ACVFORMA JNTLNMEA
007811                                JNTFNMEA JNTINITA.
007812
007813     IF CM-LF-CURRENT-STATUS = '2' OR
007814        CM-AH-CURRENT-STATUS = '2'
007815         MOVE AL-SANON           TO LOANNOA LOANBALA PREMTYPA
007816     END-IF.
007817
007818     MOVE AL-SANOF TO
007819            CRTINITA   ISSAGEA    JNTAGEA    CRTSSNA   ADDONDTA
007820            LCVDSCRA   LCVCDA     LCVOTRMA   LCVBENEA  LCVCNDTA
007821            ACVDSCRA   ACVCDA     ACVOTRMA   ACVBENEA  ACVCNDTA
007822            APRA       PMTFREQA   INDGRPA    MEMBERA   REINCDA
007823            LCVRATEA   ACVRATEA.
007824
007825     MOVE 'S'                    TO CERTMTI.
007826
007827 7300-RESET-KEY-ATTRBS.
007828     MOVE AL-UANON           TO CERTMTA EFFDTA ACCOUNTA STATEA
007829                                CRTCARRA GROUPA CRTLNMEA CRTFNMEA
007830                                LCVFORMA ACVFORMA JNTLNMEA
007831                                JNTFNMEA JNTINITA.
007832
007833     IF NOT CERT-WAS-CREATED-FOR-CLAIM
007834         MOVE AL-SANOF           TO REINCDA
007835     END-IF.
007836
007837     MOVE 'S'                    TO CERTMTI.
007838
007839     EJECT
007840 7400-READ-STATE.
007841
007842     
      * EXEC CICS HANDLE CONDITION
007843*         NOTFND   (7499-EXIT)
007844*    END-EXEC.
      *    MOVE '"$I                   ! G #00014329' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4720233030303134333239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007845
007846     MOVE SPACES                 TO ELCNTL-KEY.
007847     MOVE CM-STATE               TO CNTL-ACCESS.
007848     MOVE '3'                    TO CNTL-REC-TYPE.
007849     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
007850     MOVE ZEROS                  TO CNTL-SEQ-NO.
007851
007852     PERFORM 7970-READ-CNTL THRU 7970-EXIT.
007853
007854     MOVE 1                      TO SUB.
007855
007856 7410-LOOP.
007857     IF SUB = 21
007858        GO TO 7499-EXIT
007859     END-IF.
007860
007861*    IF (WS-BEN-HOLD  = CF-ST-BENEFIT-CD (SUB))  AND
007862*       (WS-REC-TYPE  = CF-ST-BENEFIT-KIND (SUB))
007863*        MOVE CF-ST-FORM-NO (SUB) TO WS-FORM-HOLD
007864*        GO TO 7499-EXIT.
007865
007866     ADD 1                       TO SUB.
007867     GO TO 7410-LOOP.
007868
007869 7499-EXIT.
007870      EXIT.
007871
007872     EJECT
007873 7500-BROWSE-FOR-DUPLICATE.
007874
007875     MOVE 'N'                    TO INCUR-DTE-DUPE-SW.
007876
007877
007878     
      * EXEC CICS HANDLE CONDITION
007879*        NOTFND   (7500-EXIT)
007880*        DUPKEY   (7500-DUPLICATE-KEY)
007881*        ENDFILE  (7500-EXIT)
007882*    END-EXEC.
      *    MOVE '"$I$''                 ! H #00014365' TO DFHEIV0
           MOVE X'222449242720202020202020' &
                X'202020202020202020202120' &
                X'4820233030303134333635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007883
007884
007885     
      * EXEC CICS STARTBR
007886*        DATASET    (ELMSTR5-DSID)
007887*        RIDFLD     (ELMSTR5-KEY)
007888*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00014372' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303134333732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR5-DSID, 
                 ELMSTR5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007889
007890
007891     
      * EXEC CICS HANDLE CONDITION
007892*        ENDFILE  (7500-END-BROWSE)
007893*    END-EXEC.
      *    MOVE '"$''                   ! I #00014378' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'4920233030303134333738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007894
007895 7500-READ-CLAIM-LOOP.
007896
007897
007898     
      * EXEC CICS READNEXT
007899*        DATASET   (ELMSTR5-DSID)
007900*        SET       (ADDRESS OF CLAIM-MASTER)
007901*        RIDFLD    (ELMSTR5-KEY)
007902*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00014385' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303134333835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR5-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR5-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007903
007904 7500-DUPLICATE-KEY.
007905
007906     IF CL-COMPANY-CD     NOT = CM-COMPANY-CD  OR
007907        CL-CERT-CARRIER   NOT = CM-CARRIER     OR
007908        CL-CERT-GROUPING  NOT = CM-GROUPING    OR
007909        CL-CERT-STATE     NOT = CM-STATE       OR
007910        CL-CERT-ACCOUNT   NOT = CM-ACCOUNT     OR
007911        CL-CERT-EFF-DT    NOT = CM-CERT-EFF-DT OR
007912        CL-CERT-NO        NOT = CM-CERT-NO
007913           GO TO 7500-CHECK-MATCH-COUNT
007914     END-IF.
007915
007916     MOVE 'H'                    TO INCUR-DTE-DUPE-SW.
007917
007918     IF CL-INCURRED-DT = PI-SAVE-INCUR-DT
007919        MOVE 'Y'                 TO INCUR-DTE-DUPE-SW
007920        GO TO 7500-END-BROWSE
007921     ELSE
007922        GO TO 7500-READ-CLAIM-LOOP
007923     END-IF.
007924
007925 7500-CHECK-MATCH-COUNT.
007926
007927     IF CL-COMPANY-CD = CM-COMPANY-CD AND
007928        CL-CERT-NO    = CM-CERT-NO
007929          GO TO 7500-READ-CLAIM-LOOP
007930     END-IF.
007931
007932 7500-END-BROWSE.
007933
007934
007935     
      * EXEC CICS ENDBR
007936*        DATASET   (ELMSTR5-DSID)
007937*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014422' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134343232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR5-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007938
007939 7500-EXIT.
007940     EXIT.
007941
007942     EJECT
007943 7550-FALL-THRU-PARA.
007944
007945
007946     
      * EXEC CICS DUMP
007947*         DUMPCODE ('E130')
007948*         TASK
007949*    END-EXEC.
           MOVE 'E130' TO DFHEIV5
      *    MOVE '<"   T                $   #00014433' TO DFHEIV0
           MOVE X'3C2220202054202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134343333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007950
007951
007952     
      * EXEC CICS RETURN
007953*    END-EXEC.
      *    MOVE '.(                    ''   #00014439' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303134343339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007954
007955 7600-BROWSE-CLAIM.
007956
007957
007958     
      * EXEC CICS READ
007959*        DATASET(ELMSTR-DSID)
007960*        SET    (ADDRESS OF CLAIM-MASTER)
007961*        RIDFLD (ELMSTR-KEY)
007962*        GENERIC
007963*        EQUAL
007964*        KEYLENGTH(ELMSTR-GENERIC-LENGTH)
007965*    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00014445' TO DFHEIV0
           MOVE X'26225320204B472020202045' &
                X'202020202020202020202820' &
                X'2020233030303134343435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 ELMSTR-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007966
007967     MOVE CL-CERT-PRIME          TO CERTNOI.
007968     MOVE CL-CERT-SFX            TO SUFXI.
007969
007970 7610-BROWSE-CLAIM-LOOP.
007971
007972     MOVE LOW-VALUES             TO BCERT1O
007973                                    BSUFX1O
007974                                    BCERT2O
007975                                    BSUFX2O
007976                                    BCERT3O
007977                                    BSUFX3O
007978                                    BCERT4O
007979                                    BSUFX4O
007980                                    BCERT5O
007981                                    BSUFX5O.
007982
007983     MOVE ELMSTR-KEY             TO SAVE-ELMSTR-KEY.
007984
007985
007986     
      * EXEC CICS HANDLE CONDITION
007987*        ENDFILE  (7630-END-BROWSE)
007988*    END-EXEC.
      *    MOVE '"$''                   ! J #00014473' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'4A20233030303134343733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007989
007990
007991     
      * EXEC CICS STARTBR
007992*        DATASET    (ELMSTR-DSID)
007993*        RIDFLD     (ELMSTR-KEY)
007994*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00014478' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303134343738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
007995
007996     MOVE +1                     TO WS-ASSOCIATED-CERTS.
007997
007998 7620-READ-CLAIM-LOOP.
007999
008000
008001     
      * EXEC CICS READNEXT
008002*        DATASET   (ELMSTR-DSID)
008003*        SET       (ADDRESS OF CLAIM-MASTER)
008004*        RIDFLD    (ELMSTR-KEY)
008005*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00014488' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303134343838' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
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
           
008006
008007     IF CL-COMPANY-CD  NOT = PI-COMPANY-CD  OR
008008        CL-CARRIER     NOT = CLMCARRI       OR
008009        CL-CLAIM-NO    NOT = CLMNOI
008010          GO TO 7630-END-BROWSE
008011     END-IF.
008012
008013     IF WS-ASSOCIATED-CERTS = 1
008014         MOVE CL-CERT-PRIME      TO BCERT1O
008015         MOVE CL-CERT-SFX        TO BSUFX1O
008016     END-IF.
008017
008018     IF WS-ASSOCIATED-CERTS = 2
008019         MOVE CL-CERT-PRIME      TO BCERT2O
008020         MOVE CL-CERT-SFX        TO BSUFX2O
008021     END-IF.
008022
008023     IF WS-ASSOCIATED-CERTS = 3
008024         MOVE CL-CERT-PRIME      TO BCERT3O
008025         MOVE CL-CERT-SFX        TO BSUFX3O
008026     END-IF.
008027
008028     IF WS-ASSOCIATED-CERTS = 4
008029         MOVE CL-CERT-PRIME      TO BCERT4O
008030         MOVE CL-CERT-SFX        TO BSUFX4O
008031     END-IF.
008032
008033     IF WS-ASSOCIATED-CERTS = 5
008034         MOVE CL-CERT-PRIME      TO BCERT5O
008035         MOVE CL-CERT-SFX        TO BSUFX5O
008036     ELSE
008037         ADD +1                  TO WS-ASSOCIATED-CERTS
008038         GO TO 7620-READ-CLAIM-LOOP
008039     END-IF.
008040
008041 7630-END-BROWSE.
008042
008043
008044     
      * EXEC CICS ENDBR
008045*        DATASET   (ELMSTR-DSID)
008046*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014531' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134353331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008047
008048 7640-HIGHLIGHT-CERT-DISPLAYED.
008049
008050     MOVE SAVE-ELMSTR-KEY        TO ELMSTR-KEY.
008051     MOVE AL-SANON               TO BCERT1A
008052                                    BSUFX1A
008053                                    BCERT2A
008054                                    BSUFX2A
008055                                    BCERT3A
008056                                    BSUFX3A
008057                                    BCERT4A
008058                                    BSUFX4A
008059                                    BCERT5A
008060                                    BSUFX5A.
008061
008062     IF BCERT1O = CERTNOI AND
008063        BSUFX1O = SUFXI
008064            MOVE AL-SABON        TO BCERT1A
008065                                    BSUFX1A
008066     END-IF.
008067
008068     IF BCERT2O = CERTNOI AND
008069        BSUFX2O = SUFXI
008070            MOVE AL-SABON        TO BCERT2A
008071                                    BSUFX2A
008072     END-IF.
008073
008074     IF BCERT3O = CERTNOI AND
008075        BSUFX3O = SUFXI
008076            MOVE AL-SABON        TO BCERT3A
008077                                    BSUFX3A
008078     END-IF.
008079
008080     IF BCERT4O = CERTNOI AND
008081        BSUFX4O = SUFXI
008082            MOVE AL-SABON        TO BCERT4A
008083                                    BSUFX4A
008084     END-IF.
008085
008086     IF BCERT5O = CERTNOI AND
008087        BSUFX5O = SUFXI
008088            MOVE AL-SABON        TO BCERT5A
008089                                    BSUFX5A
008090     END-IF.
008091
008092 7699-EXIT.
008093     EXIT.
008094
008095     EJECT
008096 7700-CHECK-SEQUENCE.
008097
008098
008099     
      * EXEC CICS HANDLE CONDITION
008100*        ENDFILE  (7799-EXIT)
008101*        NOTFND   (7799-EXIT)
008102*    END-EXEC.
      *    MOVE '"$''I                  ! K #00014586' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'4B20233030303134353836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008103
008104
008105     
      * EXEC CICS READ
008106*        DATASET(ELMSTR-DSID)
008107*        SET    (ADDRESS OF CLAIM-MASTER)
008108*        RIDFLD (ELMSTR-KEY)
008109*        GENERIC
008110*        EQUAL
008111*        KEYLENGTH(ELMSTR-GENERIC-LENGTH)
008112*    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00014592' TO DFHEIV0
           MOVE X'26225320204B472020202045' &
                X'202020202020202020202820' &
                X'2020233030303134353932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 ELMSTR-GENERIC-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008113
008114     COMPUTE WS-ASSOC-CERT-TOTAL =
008115             CL-ASSOC-CERT-TOTAL + ONE-OR-MIN1.
008116
008117     GO TO 7799-EXIT.
008118
008119 7710-RESEQUENCE-CLAIMS.
008120
008121
008122     
      * EXEC CICS HANDLE CONDITION
008123*        ENDFILE  (7790-END-BROWSE)
008124*    END-EXEC.
      *    MOVE '"$''                   ! L #00014609' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'4C20233030303134363039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008125
008126
008127     
      * EXEC CICS STARTBR
008128*        DATASET    (ELMSTR-DSID)
008129*        RIDFLD     (ELMSTR-KEY)
008130*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00014614' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303134363134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008131
008132     ADD +1 TO WS-ASSOC-CERT-SEQU
008133               WS-READNEXT-SWITCH.
008134
008135 7720-READ-CLAIM-LOOP.
008136
008137
008138     
      * EXEC CICS READNEXT
008139*        DATASET   (ELMSTR-DSID)
008140*        SET       (ADDRESS OF CLAIM-MASTER)
008141*        RIDFLD    (ELMSTR-KEY)
008142*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00014625' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303134363235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
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
           
008143
008144     IF WS-READNEXT-SWITCH = 1
008145         ADD 1 TO WS-READNEXT-SWITCH
008146         GO TO 7720-READ-CLAIM-LOOP
008147     END-IF.
008148
008149 7730-END-BROWSE.
008150
008151
008152     
      * EXEC CICS ENDBR
008153*        DATASET   (ELMSTR-DSID)
008154*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014639' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134363339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008155
008156 7740-READ-CLAIM-UPDATE.
008157
008158     IF CL-COMPANY-CD NOT = WS-SAVE-COMPANY-CD OR
008159        CL-CARRIER    NOT = WS-SAVE-CARRIER    OR
008160        CL-CLAIM-NO   NOT = WS-SAVE-CLAIM-NO
008161          GO TO 7799-EXIT
008162     ELSE
008163          MOVE ZERO              TO WS-READNEXT-SWITCH
008164     END-IF.
008165
008166
008167     
      * EXEC CICS READ
008168*        DATASET(ELMSTR-DSID)
008169*        SET    (ADDRESS OF CLAIM-MASTER)
008170*        RIDFLD (ELMSTR-KEY)
008171*        UPDATE
008172*    END-EXEC.
      *    MOVE '&"S        EU         (   #00014654' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303134363534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008173
008174     MOVE WS-ASSOC-CERT-TOTAL    TO CL-ASSOC-CERT-TOTAL.
008175     MOVE WS-ASSOC-CERT-SEQU     TO CL-ASSOC-CERT-SEQU.
008176
008177
008178     
      * EXEC CICS REWRITE
008179*         DATASET     (ELMSTR-DSID)
008180*         FROM        (CLAIM-MASTER)
008181*    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00014665' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303134363635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008182
008183     GO TO 7710-RESEQUENCE-CLAIMS.
008184
008185 7790-END-BROWSE.
008186
008187
008188     
      * EXEC CICS ENDBR
008189*        DATASET   (ELMSTR-DSID)
008190*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014675' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134363735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008191
008192 7799-EXIT.
008193     EXIT.
008194
008195     EJECT
008196************************************************
008197*  I/O REQUESTS AGAINST ACTIVITY TRAILER FILE  *
008198************************************************
008199
008200 7900-READ-ACTV-UPDATE.
008201     MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
008202     MOVE MSTR-CARRIER           TO TRLR-CARRIER.
008203     MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
008204     MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
008205     MOVE +0                     TO TRLR-SEQ-NO.
008206     MOVE '1'                    TO TRLR-TYPE.
008207     GO TO 7900-READ-TRAILER-UPDATE.
008208
008209 7900-READ-NINETY-UPDATE.
008210     MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
008211     MOVE MSTR-CARRIER           TO TRLR-CARRIER.
008212     MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
008213     MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
008214     MOVE +90                    TO TRLR-SEQ-NO.
008215     MOVE '6'                    TO TRLR-TYPE.
008216
008217 7900-READ-TRAILER-UPDATE.
008218
008219     
      * EXEC CICS READ  UPDATE
008220*        DATASET    (ELTRLR-DSID)
008221*        SET        (ADDRESS OF ACTIVITY-TRAILERS)
008222*        RIDFLD     (ELTRLR-KEY)
008223*    END-EXEC.
      *    MOVE '&"S        EU         (   #00014706' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303134373036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008224
008225 7900-EXIT.
008226      EXIT.
008227
008228 7910-READ-ACTV.
008229     MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
008230     MOVE MSTR-CARRIER           TO TRLR-CARRIER.
008231     MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
008232     MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
008233     MOVE +0                     TO TRLR-SEQ-NO.
008234     MOVE '1'                    TO TRLR-TYPE.
008235     GO TO 7910-READ-TRAILER.
008236
008237 7910-READ-NINETY.
008238     MOVE MSTR-COMP-CD           TO TRLR-COMP-CD.
008239     MOVE MSTR-CARRIER           TO TRLR-CARRIER.
008240     MOVE MSTR-CLAIM-NO          TO TRLR-CLAIM-NO.
008241     MOVE MSTR-CERT-NO           TO TRLR-CERT-NO.
008242     MOVE +90                    TO TRLR-SEQ-NO.
008243     MOVE '6'                    TO TRLR-TYPE.
008244
008245 7910-READ-TRAILER.
008246     MOVE 'TRLR'                 TO FILE-SWITCH.
008247
008248
008249     
      * EXEC CICS READ
008250*        DATASET   (ELTRLR-DSID)
008251*        SET       (ADDRESS OF ACTIVITY-TRAILERS)
008252*        RIDFLD    (ELTRLR-KEY)
008253*    END-EXEC.
      *    MOVE '&"S        E          (   #00014736' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134373336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008254
008255 7910-EXIT.
008256      EXIT.
008257
008258 7915-REWRITE-TRAILER.
008259
008260     
      * EXEC CICS REWRITE
008261*        DATASET   (ELTRLR-DSID)
008262*        FROM      (ACTIVITY-TRAILERS)
008263*    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00014747' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303134373437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-DSID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008264
008265 7915-EXIT.
008266      EXIT.
008267
008268     EJECT
008269********************************************
008270*  I/O REQUESTS AGAINST CLAIM MASTER FILE  *
008271********************************************
008272
008273 7920-READ-CLAIM-UPDATE.
008274
008275
008276     
      * EXEC CICS READ
008277*        UPDATE
008278*        DATASET   (ELMSTR-DSID)
008279*        SET       (ADDRESS OF CLAIM-MASTER)
008280*        RIDFLD    (ELMSTR-KEY)
008281*    END-EXEC.
      *    MOVE '&"S        EU         (   #00014763' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303134373633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008282
008283 7920-EXIT.
008284      EXIT.
008285
008286 7930-READ-CERT-UPDATE.
008287
008288     
      * EXEC CICS READ
008289*        DATASET  (ELCERT-DSID)
008290*        SET      (ADDRESS OF CERTIFICATE-MASTER)
008291*        RIDFLD   (ELCERT-KEY)
008292*        UPDATE
008293*    END-EXEC.
      *    MOVE '&"S        EU         (   #00014775' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303134373735' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008294
008295 7930-EXIT.
008296      EXIT.
008297
008298 7940-READ-CERT.
008299
008300     
      * EXEC CICS READ
008301*        DATASET  (ELCERT-DSID)
008302*        SET      (ADDRESS OF CERTIFICATE-MASTER)
008303*        RIDFLD   (ELCERT-KEY)
008304*    END-EXEC.
      *    MOVE '&"S        E          (   #00014787' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134373837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008305
008306 7940-EXIT.
008307      EXIT.
008308
008309 7950-READ-CLAIM.
008310
008311     
      * EXEC CICS READ
008312*        DATASET   (ELMSTR-DSID)
008313*        SET       (ADDRESS OF CLAIM-MASTER)
008314*        RIDFLD    (ELMSTR-KEY)
008315*    END-EXEC.
      *    MOVE '&"S        E          (   #00014798' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134373938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008316
008317 7950-EXIT.
008318      EXIT.
008319
008320     EJECT
008321***************************************
008322*  I/O REQUESTS AGAINST CONTROL FILE  *
008323***************************************
008324
008325 7960-READ-CNTL-UPDATE.
008326
008327     
      * EXEC CICS READ
008328*        UPDATE
008329*        DATASET  (ELCNTL-DSID)
008330*        SET      (ADDRESS OF CONTROL-FILE)
008331*        RIDFLD   (ELCNTL-KEY)
008332*    END-EXEC.
      *    MOVE '&"S        EU         (   #00014814' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303134383134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008333
008334 7960-EXIT.
008335      EXIT.
008336
008337 7970-READ-CNTL.
008338
008339     
      * EXEC CICS READ
008340*        DATASET  (ELCNTL-DSID)
008341*        SET      (ADDRESS OF CONTROL-FILE)
008342*        RIDFLD   (ELCNTL-KEY)
008343*    END-EXEC.
      *    MOVE '&"S        E          (   #00014826' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134383236' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008344
008345 7970-EXIT.
008346      EXIT.
008347
008348 7975-READ-CNTL-GTEQ.
008349
008350     
      * EXEC CICS READ
008351*         DATASET   (ELCNTL-DSID)
008352*         SET       (ADDRESS OF CONTROL-FILE)
008353*         RIDFLD    (ELCNTL-KEY)
008354*         GTEQ
008355*     END-EXEC.
      *    MOVE '&"S        G          (   #00014837' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303134383337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008356
008357 7975-EXIT.
008358      EXIT.
008359
008360     EJECT
008361**************************************************
008362*  I/O REQUESTS AGAINST CERTIFICATE MASTER FILE  *
008363**************************************************
008364
008365 7980-READ-CERT5.
008366
008367      
      * EXEC CICS READ
008368*         DATASET  (ELCERT5-DSID)
008369*         SET      (ADDRESS OF CERTIFICATE-MASTER)
008370*         RIDFLD   (ELCERT-KEY-5)
008371*     END-EXEC.
      *    MOVE '&"S        E          (   #00014854' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134383534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT5-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY-5, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008372
008373 7980-EXIT.
008374      EXIT.
008375
008376 7990-get-lo-hi-acct-dates.
008377
008378     MOVE PI-COMPANY-CD       TO ACCT-COMP-CD
008379     MOVE PI-CARRIER          TO ACCT-CARRIER
008380     MOVE PI-GROUPING         TO ACCT-GROUPING
008381     MOVE PI-STATE            TO ACCT-STATE
008382     MOVE PI-ACCOUNT          TO ACCT-ACCOUNT
008383     MOVE low-values          TO ACCT-EXP-DT
008384     MOVE ERACCT-KEY          TO SAVE-ERACCT-KEY
008385
008386     move spaces              to ws-i-say-stop-ind
008387                                 ws-eracct-startbr-ind
008388                                 ws-acct-status
008389
008390     
      * EXEC CICS STARTBR
008391*         DATASET    ('ERACCT')
008392*         RIDFLD     (ERACCT-KEY)
008393*         GTEQ
008394*         resp       (ws-response)
008395*    END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00014877' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303134383737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
008396
008397     if ws-resp-normal
008398        set eracct-browse-started to true
008399     end-if
008400
008401     perform until i-say-stop
008402        
      * EXEC CICS READNEXT
008403*          DATASET ('ERACCT')
008404*          RIDFLD  (ERACCT-KEY)
008405*          SET     (ADDRESS OF ACCOUNT-MASTER)
008406*          resp    (WS-RESPONSE)
008407*       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00014889' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303134383839' TO DFHEIV0
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
008408
008409        IF WS-RESP-NORMAL
008410           AND save-eracct-key(1:20) =
008411                       AM-CONTROL-PRIMARY (1:20)
008412           if ws-lo-acct-dt = low-values
008413              move am-effective-dt
008414                                 to ws-lo-acct-dt
008415           end-if
008416           if am-expiration-dt > ws-hi-acct-dt
008417              move am-expiration-dt
008418                                 to ws-hi-acct-dt
008419           end-if
008420           move am-status        to ws-acct-status
008421        else
008422           set i-say-stop to true
008423        end-if
008424     end-perform
008425
008426     if eracct-browse-started
008427        
      * exec cics endbr
008428*          dataset('ERACCT')
008429*       end-exec
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00014914' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303134393134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
008430     end-if
008431
008432     .
008433 7990-exit.
008434     exit.
008435
008436**************************************************
008437*  I/O REQUESTS AGAINST BENEFICIARY MASTER FILE  *
008438**************************************************
008439
008440 7991-READ-BENE.
008441
008442     
      * EXEC CICS READ
008443*         DATASET   (ELBENE-DSID)
008444*         SET       (ADDRESS OF BENEFICIARY-MASTER)
008445*         RIDFLD    (ELBENE-KEY)
008446*     END-EXEC.
      *    MOVE '&"S        E          (   #00014929' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134393239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBENE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELBENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008447
008448 7991-EXIT.
008449      EXIT.
008450
008451     EJECT
008452**************************************************
008453*  I/O REQUESTS AGAINST REINSURANCE MASTER FILE  *
008454**************************************************
008455
008456 7992-READ-REIN.
008457
008458
008459     
      * EXEC CICS HANDLE CONDITION
008460*        NOTFND   (7992-NOT-FOUND)
008461*    END-EXEC.
      *    MOVE '"$I                   ! M #00014946' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'4D20233030303134393436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008462
008463
008464     
      * EXEC CICS READ
008465*        DATASET   (ERREIN-DSID)
008466*        SET       (ADDRESS OF REINSURANCE-RECORD)
008467*        RIDFLD    (ERREIN-KEY)
008468*    END-EXEC.
      *    MOVE '&"S        E          (   #00014951' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303134393531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREIN-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008469
008470     MOVE 'Y'                       TO  WS-REIN-REC-FOUND-SW.
008471
008472     GO TO 7992-EXIT.
008473
008474 7992-NOT-FOUND.
008475     MOVE 'N'                       TO  WS-REIN-REC-FOUND-SW.
008476
008477 7992-EXIT.
008478     EXIT.
008479
008480     EJECT
008481 8000-LOAD-ERROR-MESSAGES.
008482     MOVE SPACES                 TO ERRMSG1O  ERRMSG2O.
008483
008484     IF EMI-NO-ERRORS
008485         GO TO 8000-EXIT
008486     END-IF.
008487
008488     perform varying e1 from +1 by +1 until e1 > +3
008489        if emi-error-number (e1) = '1651' or '1652' or '1653'
008490                                or '1655' or '1660'
008491           move 'W'              to emi-severity (e1)
008492        end-if
008493     end-perform
008494
008495     IF EMI-NUMBER-OF-LINES = 1
008496         MOVE EMI-LINE1          TO ERRMSG1O
008497         GO TO 8000-EXIT
008498     END-IF.
008499
008500     IF EMI-NUMBER-OF-LINES = 2
008501          MOVE EMI-LINE1         TO ERRMSG1O
008502          MOVE EMI-LINE2         TO ERRMSG2O
008503          GO TO 8000-EXIT
008504     END-IF.
008505
008506     MOVE EMI-LINE1              TO ERRMSG1O.
008507
008508 8000-EXIT.
008509     EXIT.
008510
008511 8100-SEND-INITIAL-MAP.
008512     MOVE SAVE-DATE              TO RUNDTEO.
008513     MOVE EIBTIME                TO TIME-IN.
008514     MOVE TIME-OUT               TO RUNTIMEO.
008515     IF EIBAID = DFHPF11
008516        MOVE 'Y'                 TO EMI-ROLL-SWITCH
008517        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008518     end-if
008519
008520     IF RETURNED-FROM = XCTL-114
008521        CONTINUE
008522     ELSE
008523        MOVE -1                  TO MAINTL
008524     END-IF.
008525
008526*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL' AND 'LGX'
008527         MOVE SPACES             TO PRODO.
008528         MOVE AL-SANOF           TO PRODCDA.
008529*    END-IF.
008530
008531     MOVE PI-COMPANY-ID          TO COMPO.
008532     MOVE AL-PABOF               TO COMPA.
008533     MOVE PI-MEMBER-CAPTION      TO MEMCAPO.
008534     MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
008535     MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
008536
008537     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
008538
008539     IF NOT PI-NO-CARRIER-SECURITY
008540        MOVE PI-CARRIER-SECURITY TO CLMCARRO
008541        MOVE AL-SABOF            TO CLMCARRA
008542     END-IF.
008543
008544     IF PI-PROCESSOR-ID = 'PEMA'
008545        MOVE AL-UANON            TO CERTNOA
008546                                    SUFXA
008547     END-IF.
008548
008549*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'
008550*        NEXT SENTENCE
008551*        CONTINUE
008552*    ELSE
008553         MOVE AL-SANOF           TO LCVRATEA  ACVRATEA
008554*    END-IF.
008555
008556
008557     
      * EXEC CICS SEND
008558*        MAP     (MAP-NAME)
008559*        MAPSET  (MAPSET-NAME)
008560*        FROM    (EL130AO)
008561*        ERASE
008562*        CURSOR
008563*    END-EXEC.
           MOVE LENGTH OF
            EL130AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00015044' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303135303434' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL130AO, 
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
           
008564
008565     GO TO 9100-RETURN-TRAN.
008566
008567 8150-SEND-MAP-CURSOR.
008568     MOVE SAVE-DATE              TO RUNDTEO.
008569     MOVE EIBTIME                TO TIME-IN.
008570     MOVE TIME-OUT               TO RUNTIMEO.
008571     IF EIBAID = DFHPF11
008572        MOVE 'Y'                 TO EMI-ROLL-SWITCH
008573        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008574     end-if
008575
008576     MOVE PI-COMPANY-ID          TO COMPO.
008577     MOVE AL-PABOF               TO COMPA
008578     MOVE PI-MEMBER-CAPTION      TO MEMCAPO.
008579     MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
008580     MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
008581
008582*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL' AND 'LGX'
008583         MOVE SPACES             TO PRODO.
008584         MOVE AL-SANOF           TO PRODCDA.
008585*    END-IF.
008586
008587     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
008588
008589     IF NOT PI-NO-CARRIER-SECURITY
008590         MOVE PI-CARRIER-SECURITY TO CLMCARRO
008591         MOVE AL-SABOF            TO CLMCARRA
008592     END-IF.
008593
008594     IF PI-PROCESSOR-ID = 'PEMA'
008595        MOVE AL-UANON            TO CERTNOA
008596                                    SUFXA
008597     END-IF.
008598
008599*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'
008600*        NEXT SENTENCE
008601*        CONTINUE
008602*    ELSE
008603         MOVE AL-SANOF           TO LCVRATEA  ACVRATEA
008604*    END-IF.
008605
008606
008607     
      * EXEC CICS SEND
008608*        MAP      (MAP-NAME)
008609*        MAPSET   (MAPSET-NAME)
008610*        FROM     (EL130AO)
008611*        CURSOR   (PI-CURSOR)
008612*        ERASE
008613*    END-EXEC.
           MOVE LENGTH OF
            EL130AO
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00015094' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303135303934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL130AO, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 PI-CURSOR, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008614
008615     GO TO 9100-RETURN-TRAN.
008616
008617 8200-SEND-DATAONLY.
008618     PERFORM 8000-LOAD-ERROR-MESSAGES THRU 8000-EXIT.
008619
008620     IF EIBAID = DFHPF11
008621        MOVE 'Y'                 TO EMI-ROLL-SWITCH
008622        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008623        go to 8100-send-initial-map
008624     end-if
008625
008626*    IF EIBAID = DFHPF11
008627*       MOVE 'Y'                 TO EMI-ROLL-SWITCH
008628*       move pi-last-claim       to emi-claim-no
008629*       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
008630*       go to 8100-send-initial-map
008631*    END-IF.
008632     .
008633 8200-BYPASS-ERROR.
008634     MOVE SAVE-DATE              TO RUNDTEO.
008635     MOVE EIBTIME                TO TIME-IN.
008636     MOVE TIME-OUT               TO RUNTIMEO.
008637     MOVE PI-COMPANY-ID          TO COMPO.
008638     MOVE AL-PABOF               TO COMPA
008639     MOVE PI-MEMBER-CAPTION      TO MEMCAPO.
008640
008641*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL' AND 'LGX'
008642         MOVE SPACES             TO PRODO.
008643         MOVE AL-SANOF           TO PRODCDA.
008644*    END-IF.
008645
008646     MOVE PI-LIFE-OVERRIDE-L6    TO LCVDSCRO.
008647     MOVE PI-AH-OVERRIDE-L6      TO ACVDSCRO.
008648
008649     IF PI-PROCESSOR-ID = 'PEMA'
008650        MOVE AL-UANON            TO CERTNOA
008651                                    SUFXA
008652     END-IF.
008653
008654     IF NOT PI-NO-CARRIER-SECURITY
008655        MOVE PI-CARRIER-SECURITY TO CLMCARRO
008656        MOVE AL-SABOF            TO CLMCARRA
008657     END-IF.
008658
008659*    IF PI-COMPANY-ID  = 'CRI' OR 'LGX' OR 'NCL' OR 'HAN'
008660*        NEXT SENTENCE
008661*        CONTINUE
008662*    ELSE
008663         MOVE AL-SANOF           TO LCVRATEA  ACVRATEA
008664*    END-IF.
008665
008666*    IF PI-COMPANY-ID NOT = 'ACC' AND 'FDL'
008667         MOVE AL-SANOF           TO PRODCDA.
008668*    END-IF.
008669
008670
008671     
      * EXEC CICS SEND
008672*        MAP      (MAP-NAME)
008673*        MAPSET   (MAPSET-NAME)
008674*        FROM     (EL130AO)
008675*        DATAONLY
008676*        CURSOR
008677*    END-EXEC.
           MOVE LENGTH OF
            EL130AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00015158' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303135313538' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL130AO, 
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
           
008678
008679     GO TO 9100-RETURN-TRAN.
008680
008681 8300-SEND-TEXT.
008682
008683     
      * EXEC CICS SEND TEXT
008684*        FROM     (LOGOFF-TEXT)
008685*        LENGTH   (LOGOFF-LENGTH)
008686*        ERASE
008687*        FREEKB
008688*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00015170' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303135313730' TO DFHEIV0
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
           
008689
008690
008691     
      * EXEC CICS RETURN
008692*    END-EXEC.
      *    MOVE '.(                    ''   #00015178' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303135313738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008693
008694 8800-UNAUTHORIZED-ACCESS.
008695     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
008696     GO TO 8300-SEND-TEXT.
008697
008698 8810-PF23.
008699     MOVE EIBAID TO PI-ENTRY-CD-1.
008700     MOVE XCTL-005               TO PGM-NAME.
008701     GO TO 9300-XCTL.
008702
008703 8820-TERM-ERROR.
008704     MOVE ER-0412                TO EMI-ERROR.
008705     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
008706     MOVE -1                     TO MAINTL.
008707     GO TO 8200-SEND-DATAONLY.
008708
008709 8830-TRAN-ERROR.
008710     MOVE ER-0413                TO EMI-ERROR.
008711     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
008712     MOVE -1                     TO MAINTL.
008713     GO TO 8200-SEND-DATAONLY.
008714
008715 9000-RETURN-CICS.
008716
008717     
      * EXEC CICS RETURN
008718*    END-EXEC.
      *    MOVE '.(                    ''   #00015204' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303135323034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008719
008720 9100-RETURN-TRAN.
008721     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
008722     MOVE '130A'                 TO PI-CURRENT-SCREEN-NO.
008723
008724
008725     
      * EXEC CICS RETURN
008726*        TRANSID   (TRANS-ID)
008727*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
008728*        LENGTH    (PI-COMM-LENGTH)
008729*    END-EXEC.
      *    MOVE '.(CT                  ''   #00015212' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303135323132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008730
008731 9200-RETURN-MAIN-MENU.
008732     MOVE XCTL-126               TO PGM-NAME.
008733     GO TO 9300-XCTL.
008734
008735 9300-XCTL.
008736
008737     
      * EXEC CICS XCTL
008738*        PROGRAM   (PGM-NAME)
008739*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
008740*        LENGTH    (PI-COMM-LENGTH)
008741*    END-EXEC.
      *    MOVE '.$C                   %   #00015224' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303135323234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008742
008743 9400-CLEAR.
008744     MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
008745     GO TO 9300-XCTL.
008746
008747 9500-PF12.
008748     MOVE XCTL-010               TO PGM-NAME.
008749     GO TO 9300-XCTL.
008750
008751 9600-PGMID-ERROR.
008752
008753     
      * EXEC CICS HANDLE CONDITION
008754*        PGMIDERR  (8300-SEND-TEXT)
008755*    END-EXEC.
      *    MOVE '"$L                   ! N #00015240' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'4E20233030303135323430' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008756
008757     MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
008758     MOVE ' '                    TO PI-ENTRY-CD-1.
008759     MOVE XCTL-005               TO PGM-NAME.
008760     MOVE PGM-NAME               TO LOGOFF-PGM.
008761     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
008762     GO TO 9300-XCTL.
008763
008764 9700-LINK-DATE-CONVERT.
008765     MOVE LINK-ELDATCV           TO PGM-NAME.
008766
008767     
      * EXEC CICS LINK
008768*        PROGRAM    (PGM-NAME)
008769*        COMMAREA   (DATE-CONVERSION-DATA)
008770*        LENGTH     (DC-COMM-LENGTH)
008771*    END-EXEC.
      *    MOVE '."C                   (   #00015254' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135323534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008772
008773 9700-EXIT.
008774     EXIT.
008775
008776 9800-LINK-REM-TERM.
008777     MOVE LINK-ELRTRM            TO PGM-NAME.
008778
008779     
      * EXEC CICS LINK
008780*        PROGRAM    (PGM-NAME)
008781*        COMMAREA   (CALCULATION-PASS-AREA)
008782*        LENGTH     (CP-COMM-LENGTH)
008783*    END-EXEC.
      *    MOVE '."C                   (   #00015266' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135323636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008784
008785 9800-EXIT.
008786     EXIT.
008787
008788 9900-ERROR-FORMAT.
008789*    if emi-error = 1651 or 1652 or 1653 or 1655
008790*       display ' 9900 error ' emi-error
008791*       display emi-message-area (1)
008792*       display emi-message-area (2)
008793*       display emi-message-area (3)
008794*    end-if
008795
008796     IF NOT EMI-ERRORS-COMPLETE
008797         MOVE LINK-001           TO PGM-NAME
008798         
      * EXEC CICS LINK
008799*            PROGRAM   (PGM-NAME)
008800*            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
008801*            LENGTH    (EMI-COMM-LENGTH)
008802*        END-EXEC
      *    MOVE '."C                   (   #00015285' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135323835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
008803         if emi-error = 1651 or 1652 or 1653 or 1655 or 1660
008804            subtract 1 from emi-fatal-ctr
008805         end-if
008806     END-IF.
008807
008808     .
008809 9900-EXIT.
008810     EXIT.
008811
008812 9990-ABEND.
008813     MOVE -1                     TO MAINTL.
008814     MOVE LINK-004               TO PGM-NAME.
008815     MOVE DFHEIBLK               TO EMI-LINE1.
008816
008817     
      * EXEC CICS LINK
008818*        PROGRAM   (PGM-NAME)
008819*        COMMAREA  (EMI-LINE1)
008820*        LENGTH    (72)
008821*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00015304' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135333034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
008822
008823     MOVE EMI-LINE1              TO ERRMSG2O.
008824
008825     GO TO 8200-BYPASS-ERROR.
008826
008827
008828     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL130' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
008829
008830 9995-SECURITY-VIOLATION.
008831*             COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00015336' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303135333336' TO DFHEIV0
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
008832
008833 9995-EXIT.
008834     EXIT.
008835

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL130' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 0100-TEST-ENTRY,
                     8100-SEND-INITIAL-MAP,
                     9600-PGMID-ERROR,
                     8820-TERM-ERROR,
                     8830-TRAN-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0450-ADD-ELACTQ-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8820-TERM-ERROR,
                     8830-TRAN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0660-NOT-FOUND,
                     0670-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1025-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1025-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1025-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1025-SHOW-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 2050-DELETE-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 2010-DELETE-CL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 3010-TEST-FOR-ERRORS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 3008-DUP-ERROR,
                     3009-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 3030-BUILD-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 3033-ACCT-NOT-FOUND,
                     3033-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 3032-NOT-IN-DT-RANGE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 3070-BUILD-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 3065-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 3100-CARRIER-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 3110-COMPANY-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 3200-BUILD-ZERO-TRAILER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 9999-DFHBACK
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 3290-NO-INSURED-ADDRESS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 3410-DUP-KEY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 3690-CERT-WRITTEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 3850-CLAIM-MSTR-WRITTEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 3992-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 5050-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 5030-CERT-WRITTEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 5035-CLAIM-WRITTEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 9990-ABEND,
                     5799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 3100-CARRIER-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 6040-NO-PROCESSOR-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 34
               GO TO 6060-NO-BENEFICIARY-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 35
               GO TO 6499-EXIT,
                     6499-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 36
               GO TO 6600-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 37
               GO TO 6700-NOTOPEN-ERROR,
                     6700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 38
               GO TO 7120-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 39
               GO TO 7499-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 40
               GO TO 7500-EXIT,
                     7500-DUPLICATE-KEY,
                     7500-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 41
               GO TO 7500-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 42
               GO TO 7630-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 43
               GO TO 7799-EXIT,
                     7799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 44
               GO TO 7790-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 45
               GO TO 7992-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 46
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL130' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
