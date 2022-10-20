      *((program: EL652.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL652 .
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 09/14/94 07:57:28.
000007*                            VMOD=2.059
000008*
000009*AUTHOR.        LOGIC,INC.
000010*               DALLAS, TEXAS.
000011*
000012*REMARKS.
000013*        TRANSACTION - EXD4 - COMPENSATION MASTER MAINT
000014*
000015******************************************************************
000016*                   C H A N G E   L O G
000017*
000018* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000019*-----------------------------------------------------------------
000020*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000021* EFFECTIVE    NUMBER
000022*-----------------------------------------------------------------
000023* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
000024* 102202    2002032200002  SMVA  ADD PF8 KEY FOR COMP MSTR NOTES
000025* 111103    2003080800002  PEMA  ADD BANKFEE,CLPST,MAXFEE  & PF10
000026*                                FOR SECURE PAY
000027* 111204    2004110300005  PEMA  SPLIT AGENT COMMISSION CHANGES
000028* 033105    2005031100003  PEMA  ADD VALID TYPE OF 'B' (BANK)
000029* 042005    2005031100004  PEMA  ALLOW UPDATES TO PCONT FOR 'B'
000030* 042005                         AND 'G' RECORD TYPES.
000031* 092205    2005050300006  PEMA  ADD LEASE FEES
000032* 041106    2006022800001  AJRA  INIT CO-FIRST-WRITTEN-DT ON ADD
000033* 060506    2002061100007  PEMA  ADD CODES TO ERCOMP FILE
000034* 072406    2006022400001  PEMA  ADD REFUND ONLY EDIT
000035* 110706  CR2006071700002  PEMA  FIX NAME LOOK UP
000036* 043007  IR2007042600002  PEMA  TURN OFF YTDCOMM ATTR
000037* 102908  CR2007052100005  PEMA  UNPROT PCONT ATTRB
000038* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000039* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000040* 080612  CR2012042700005  PEMA  ADD OVER 120 DAYS FOR AHL
000041* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000042* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000043******************************************************************
000044
000045
000046 ENVIRONMENT DIVISION.
000047 DATA DIVISION.
000048 EJECT
000049 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000050 77  FILLER  PIC X(32)  VALUE '********************************'.
000051 77  FILLER  PIC X(32)  VALUE '*    EL652 WORKING STORAGE     *'.
000052 77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.059 *********'.
000053 77  WS-TOT-FEES                 PIC S9(3)V99 COMP-3 VALUE +0.
000054 77  S1                          PIC S999 VALUE +0 COMP-3.
000055
000056 01  WS-DATE-AREA.
000057     12  SAVE-DATE           PIC  X(8)       VALUE SPACES.
000058     12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.
000059
000060 01  STANDARD-AREAS.
000061     12  WS-RESPONSE             PIC S9(8)   COMP.
000062         88  RESP-NORMAL                  VALUE +00.
000063         88  RESP-NOTFND                  VALUE +13.
000064         88  RESP-NOTOPEN                 VALUE +19.
000065         88  RESP-ENDFILE                 VALUE +20.
000066     12  RETURNED-FROM       PIC X(8)        VALUE SPACES.
000067     12  QID-PI.
000068         16  QID-TERM        PIC X(4).
000069         16  FILLER          PIC X(4)        VALUE '652A'.
000070     12  QID-ITEM            PIC S9(4) COMP  VALUE +0001.
000071     12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
000072     12  MAP-NAME            PIC  X(8)       VALUE 'EL652A'.
000073     12  MAPSET-NAME         PIC  X(8)       VALUE 'EL652S'.
000074     12  TRANS-ID            PIC  X(4)       VALUE 'EXD4'.
000075     12  EL640-TRANS-ID      PIC  X(4)       VALUE 'EXC1'.
000076     12  EL642-TRANS-ID      PIC  X(4)       VALUE 'EXH7'.
000077     12  EL633-TRANS-ID      PIC  X(4)       VALUE 'EXB7'.
000078     12  EL633DMD-TRANS-ID   PIC  X(4)       VALUE 'EX1F'.
000079     12  EL635-TRANS-ID      PIC  X(4)       VALUE 'EXJ4'.
000080     12  EL6501-TRANS-ID     PIC  X(4)       VALUE 'EXC5'.
000081     12  EL6592-TRANS-ID     PIC  X(4)       VALUE 'EX66'.
000082     12  EL856-TRANS-ID      PIC  X(4)       VALUE 'EXJ8'.
000083     12  EM6508-TRANS-ID     PIC  X(4)       VALUE 'MXG8'.
000084     12  THIS-PGM            PIC  X(8)       VALUE 'EL652'.
000085     12  PGM-NAME            PIC  X(8).
000086     12  WS-COMP-CD-R.
000087         16  FILLER          PIC  X.
000088         16  WS-COMP-CD-X    PIC  X.
000089     12  WS-COMP-CD  REDEFINES
000090         WS-COMP-CD-R        PIC S9(4)                  COMP.
000091     12  TIME-IN             PIC S9(7).
000092     12  TIME-OUT-R  REDEFINES  TIME-IN.
000093         16  FILLER          PIC  X.
000094         16  TIME-OUT        PIC  99V99.
000095         16  FILLER          PIC  XX.
000096     12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
000097     12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
000098     12  XCTL-126            PIC  X(8)       VALUE 'EL126'.
000099     12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
000100     12  XCTL-633            PIC  X(8)       VALUE 'EL633'.
000101     12  XCTL-633DMD         PIC  X(8)       VALUE 'EL633DMD'.
000102     12  XCTL-635            PIC  X(8)       VALUE 'EL635'.
000103     12  XCTL-650            PIC  X(8)       VALUE 'EL650'.
000104     12  XCTL-6521           PIC  X(8)       VALUE 'EL6521'.
000105     12  XCTL-6522           PIC  X(8)       VALUE 'EL6522'.
000106     12  XCTL-6523           PIC  X(8)       VALUE 'EL6523'.
000107     12  XCTL-6524           PIC  X(8)       VALUE 'EL6524'.
000108     12  XCTL-689            PIC  X(8)       VALUE 'EL689'.
000109     12  XCTL-690            PIC  X(8)       VALUE 'EL690'.
000110     12  XCTL-856            PIC  X(8)       VALUE 'EL856'.
000111     12  XCTL-EM626          PIC  X(8)       VALUE 'EM626'.
000112     12  LINK-001            PIC  X(8)       VALUE 'EL001'.
000113     12  LINK-004            PIC  X(8)       VALUE 'EL004'.
000114     12  LINK-ELDATCV        PIC  X(8)       VALUE 'ELDATCV'.
000115     12  FILE-ID             PIC  X(8)       VALUE SPACES.
000116     12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.
000117     12  NAME-FILE-ID        PIC  X(8)       VALUE 'ERNAME'.
000118     12  CNTL-FILE-ID        PIC  X(8)       VALUE 'ELCNTL'.
000119     12  SUMM-FILE-ID        PIC  X(8)       VALUE 'ERSUMM'.
000120     12  RQST-FILE-ID        PIC  X(8)       VALUE 'ERRQST'.
000121     12  RQST-FILE-ID-3      PIC  X(8)       VALUE 'ERRQST3'.
000122
000123 01  MISC-WORK-AREAS.
000124     12  WS-SRCH-STATE       PIC X(30)  VALUE SPACES.
000125     12  STNDX               PIC S999   COMP-3 VALUE +0.
000126     12  W-APPL-SCRTY-NDX    PIC S9(04) COMP   VALUE +35.
000127     12  WS-PHONE-IN         PIC  9(10).
000128     12  WS-PHONE-IN-R  REDEFINES  WS-PHONE-IN.
000129         16  WSPI-AREA       PIC  X(3).
000130         16  WSPI-PFX        PIC  X(3).
000131         16  WSPI-SFX        PIC  X(4).
000132     12  WS-PHONE-OUT.
000133         16  WSPO-AREA       PIC  X(3).
000134         16  FILLER          PIC  X            VALUE '-'.
000135         16  WSPO-PFX        PIC  X(3).
000136         16  FILLER          PIC  X            VALUE '-'.
000137         16  WSPO-SFX        PIC  X(4).
000138     12  DEEDIT-FIELD        PIC  X(15).
000139     12  DEEDIT-FIELD-V0  REDEFINES
000140         DEEDIT-FIELD        PIC S9(15).
000141     12  DEEDIT-FIELD-V1  REDEFINES
000142         DEEDIT-FIELD        PIC S9(13)V99.
000143     12  SUB1                PIC S9(4)         VALUE +0   COMP.
000144     12  SUB2                PIC S9(4)         VALUE +0   COMP.
000145     12  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.
000146     12  ERCOMP-LENGTH       PIC S9(4)         VALUE +700 COMP.
000147     12  ERSUMM-LENGTH       PIC S9(4)         VALUE +150 COMP.
000148     12  ERNAME-LENGTH       PIC S9(4)         VALUE +160 COMP.
000149     12  SV-CLMTOL           PIC  9(3)V99      VALUE ZEROS.
000150     12  WK-DATE.
000151         16  WK-MO           PIC  99.
000152         16  WK-DA           PIC  99.
000153         16  WK-YR           PIC  99.
000154     12  DATE-TEST-AREA      PIC  9(6).
000155     12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.
000156         16  DATE-TEST-MM    PIC  99.
000157         16  DATE-TEST-DD    PIC  99.
000158         16  DATE-TEST-YY    PIC  99.
000159     12  DIVIDE-RESULT       PIC  99.
000160     12  DIVIDE-REMAINDER    PIC  9.
000161     12  WS-ZERO             PIC  X            VALUE '0'.
000162     12  WS-ONE              PIC  X            VALUE '1'.
000163     12  WS-TWO              PIC  X            VALUE '2'.
000164     12  WS-ACCESS.
000165         16  FILLER          PIC  X(3)         VALUE SPACES.
000166         16  WS-CARRIER      PIC  X.
000167     12  BIN-CURRENT-SAVE    PIC  XX.
000168     12  WS-BROWSE-SW        PIC  X.
000169         88  BROWSE-STARTED                    VALUE 'Y'.
000170     12  WS-SAVE-KEY         PIC  X(29)        VALUE SPACES.
000171     12  WS-SAVE-SUMM        PIC  X(6)         VALUE SPACES.
000172     12  WS-SUMM-FOR-RQST    PIC  X(6)         VALUE SPACES.
000173     12  WS-SAVE-RQST        PIC X(46)         VALUE SPACES.
000174     12  WS-SHOW-SAVE-TOTALS PIC  X            VALUE SPACE.
000175         88  SHOW-SAVE-TOTALS                  VALUE 'Y'.
000176     12  ELCNTL-KEY.
000177         16  CNTL-COMP-ID    PIC  X(3)         VALUE SPACES.
000178         16  CNTL-REC-TYPE   PIC  X            VALUE SPACES.
000179         16  CNTL-ACCESS     PIC  X(4)         VALUE SPACES.
000180         16  CNTL-SEQ-NO     PIC S9(4)         VALUE +0   COMP.
000181     12  ELCNTL-KEY2.
000182         16  CNTL-COMP-ID-2  PIC  X(3)         VALUE SPACES.
000183         16  CNTL-REC-TYPE-2 PIC  X            VALUE SPACES.
000184         16  CNTL-STATE-2    PIC  X(2)         VALUE SPACES.
000185         16  FILLER          PIC  X(2)         VALUE SPACES.
000186         16  CNTL-SEQ-NO-2   PIC S9(4)         VALUE +0   COMP.
000187     12  ERSUMM-KEY.
000188         16  SUMM-COMP-ID    PIC  X            VALUE SPACES.
000189         16  SUMM-SUMMARY    PIC  X(6)         VALUE SPACES.
000190         16  SUMM-CARRIER    PIC  X            VALUE SPACES.
000191         16  SUMM-GROUP      PIC  X(6)         VALUE SPACES.
000192         16  SUMM-FIN-RESP   PIC  X(10)        VALUE SPACES.
000193         16  SUMM-ACCT-AGENT PIC  X(10)        VALUE SPACES.
000194     12  ERRQST-KEY.
000195         16  RQST-COMP-ID-PC PIC  X            VALUE SPACES.
000196         16  RQST-BATCH-PC   PIC  X(6)         VALUE SPACES.
000197     12  ERRQST-KEY-3.
000198         16  RQST-COMP-ID    PIC  X            VALUE SPACES.
000199         16  RQST-CARRIER    PIC  X            VALUE SPACES.
000200         16  RQST-GROUP      PIC  X(6)         VALUE SPACES.
000201         16  RQST-FIN-RESP   PIC  X(10)        VALUE SPACES.
000202         16  RQST-ACCT-AGENT PIC  X(10)        VALUE SPACES.
000203         16  RQST-REFERENCE  PIC  X(12)        VALUE SPACES.
000204         16  RQST-BATCH      PIC  X(06)        VALUE SPACES.
000205     12  WS-DATE-MDY-8.
000206         16  WS-DMDY8-MM     PIC  XX.
000207         16  WS-DMDY8-SL1    PIC  X.
000208         16  WS-DMDY8-DD     PIC  XX.
000209         16  WS-DMDY8-SL2    PIC  X.
000210         16  WS-DMDY8-YY     PIC  XX.
000211     12  WS-DATE-MDY-6  REDEFINES  WS-DATE-MDY-8.
000212         16  WS-DMDY6-MM     PIC  XX.
000213         16  WS-DMDY6-DD     PIC  XX.
000214         16  WS-DMDY6-YY     PIC  XX.
000215         16  WS-DMDY6-BLNK   PIC  XX.
000216     12  WS-YMD-DATE.
000217         16  WS-YMD-YY       PIC  XX.
000218         16  WS-YMD-MM       PIC  XX.
000219         16  WS-YMD-DD       PIC  XX.
000220     12  WS-YMD-DATE-NUM  REDEFINES
000221         WS-YMD-DATE         PIC  9(6).
000222     12  WS-SAVE-SUMMARY     PIC X(6)          VALUE SPACES.
000223     12  WS-SAVE-NAME        PIC X(30)         VALUE SPACES.
000224     12  WS-SAVE-BALFWD      PIC S9(7)V99      VALUE +0   COMP-3.
000225     12  WS-SAVE-CURCOM      PIC S9(7)V99      VALUE +0   COMP-3.
000226     12  WS-SAVE-CURCHG      PIC S9(7)V99      VALUE +0   COMP-3.
000227     12  WS-SAVE-CURPMT      PIC S9(7)V99      VALUE +0   COMP-3.
000228     12  WS-SAVE-ENDBAL      PIC S9(7)V99      VALUE +0   COMP-3.
000229     12  WS-SAVE-YTDCOM      PIC S9(7)V99      VALUE +0   COMP-3.
000230     12  WS-SOC-SEC-WORK-FIELD.
000231         16  WS-SS-TYPE          PIC X.
000232         16  WS-SOC-SEC          PIC X(12).
000233
000234     12  WS-ZIP-CODE.
000235         16  WS-ZIP-1            PIC X.
000236             88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
000237         16  WS-ZIP-2-3          PIC XX.
000238         16  WS-ZIP-4            PIC X.
000239         16  WS-ZIP-5            PIC X.
000240         16  WS-ZIP-6            PIC X.
000241         16  FILLER              PIC X(4).
000242     12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
000243         16  WS-ZIP-AM-1-CODE    PIC X(5).
000244         16  WS-ZIP-AM-1-PLUS4   PIC X(4).
000245         16  FILLER              PIC X.
000246     12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
000247         16  WS-ZIP-AM-2-CODE    PIC X(5).
000248         16  WS-ZIP-AM-2-DASH    PIC X.
000249         16  WS-ZIP-AM-2-PLUS4   PIC X(4).
000250     12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
000251         16  WS-ZIP-CAN-1-POST1  PIC XXX.
000252         16  WS-ZIP-CAN-1-POST2  PIC XXX.
000253         16  FILLER              PIC X(4).
000254     12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
000255         16  WS-ZIP-CAN-2-POST1  PIC XXX.
000256         16  FILLER              PIC X.
000257         16  WS-ZIP-CAN-2-POST2  PIC XXX.
000258         16  FILLER              PIC XXX.
000259
000260 EJECT
000261 01  ERROR-NUMBERS.
000262     12  ER-0000             PIC  X(4)       VALUE '0000'.
000263     12  ER-0002             PIC  X(4)       VALUE '0002'.
000264     12  ER-0004             PIC  X(4)       VALUE '0004'.
000265     12  ER-0008             PIC  X(4)       VALUE '0008'.
000266     12  ER-0029             PIC  X(4)       VALUE '0029'.
000267     12  ER-0033             PIC  X(4)       VALUE '0033'.
000268     12  ER-0035             PIC  X(4)       VALUE '0035'.
000269     12  ER-0046             PIC  X(4)       VALUE '0046'.
000270     12  ER-0068             PIC  X(4)       VALUE '0068'.
000271     12  ER-0070             PIC  X(4)       VALUE '0070'.
000272     12  ER-0142             PIC  X(4)       VALUE '0142'.
000273     12  ER-0144             PIC  X(4)       VALUE '0144'.
000274     12  ER-0187             PIC  X(4)       VALUE '0187'.
000275     12  ER-0193             PIC  X(4)       VALUE '0193'.
000276     12  ER-0314             PIC  X(4)       VALUE '0314'.
000277     12  ER-0584             PIC  X(4)       VALUE '0584'.
000278     12  ER-0829             PIC  X(4)       VALUE '0829'.
000279     12  ER-1299             PIC  X(4)       VALUE '1299'.
000280     12  ER-1778             PIC  X(4)       VALUE '1778'.
000281     12  ER-1883             PIC  X(4)       VALUE '1883'.
000282     12  ER-2039             PIC  X(4)       VALUE '2039'.
000283     12  ER-2042             PIC  X(4)       VALUE '2042'.
000284     12  ER-2045             PIC  X(4)       VALUE '2045'.
000285     12  ER-2046             PIC  X(4)       VALUE '2046'.
000286     12  ER-2047             PIC  X(4)       VALUE '2047'.
000287     12  ER-2048             PIC  X(4)       VALUE '2048'.
000288     12  ER-2049             PIC  X(4)       VALUE '2049'.
000289     12  ER-2050             PIC  X(4)       VALUE '2050'.
000290     12  ER-2055             PIC  X(4)       VALUE '2055'.
000291     12  ER-2056             PIC  X(4)       VALUE '2056'.
000292     12  ER-2057             PIC  X(4)       VALUE '2057'.
000293     12  ER-2067             PIC  X(4)       VALUE '2067'.
000294     12  ER-2088             PIC  X(4)       VALUE '2088'.
000295     12  ER-2089             PIC  X(4)       VALUE '2089'.
000296     12  ER-2091             PIC  X(4)       VALUE '2091'.
000297     12  ER-2092             PIC  X(4)       VALUE '2092'.
000298     12  ER-2093             PIC  X(4)       VALUE '2093'.
000299     12  ER-2094             PIC  X(4)       VALUE '2094'.
000300     12  ER-2095             PIC  X(4)       VALUE '2095'.
000301     12  ER-2096             PIC  X(4)       VALUE '2096'.
000302     12  ER-2097             PIC  X(4)       VALUE '2097'.
000303     12  ER-2209             PIC  X(4)       VALUE '2209'.
000304     12  ER-2238             PIC  X(4)       VALUE '2238'.
000305     12  ER-2370             PIC  X(4)       VALUE '2370'.
000306     12  ER-2572             PIC  X(4)       VALUE '2572'.
000307     12  ER-2717             PIC  X(4)       VALUE '2717'.
000308     12  ER-2790             PIC  X(4)       VALUE '2790'.
000309     12  ER-2872             PIC  X(4)       VALUE '2872'.
000310     12  ER-3021             PIC  X(4)       VALUE '3021'.
000311     12  ER-3053             PIC  X(4)       VALUE '3053'.
000312     12  ER-3055             PIC  X(4)       VALUE '3055'.
000313     12  ER-3056             PIC  X(4)       VALUE '3056'.
000314     12  ER-3150             PIC  X(4)       VALUE '3150'.
000315     12  ER-3151             PIC  X(4)       VALUE '3151'.
000316     12  ER-3152             PIC  X(4)       VALUE '3152'.
000317     12  ER-3153             PIC  X(4)       VALUE '3153'.
000318     12  ER-3154             PIC  X(4)       VALUE '3154'.
000319     12  ER-3168             PIC  X(4)       VALUE '3168'.
000320     12  ER-3170             PIC  X(4)       VALUE '3170'.
000321     12  ER-3174             PIC  X(4)       VALUE '3174'.
000322     12  ER-3261             PIC  X(4)       VALUE '3261'.
000323     12  ER-9096             PIC  X(4)       VALUE '9096'.
000324     12  ER-9097             PIC  X(4)       VALUE '9097'.
000325     12  ER-9999             PIC  X(4)       VALUE '9999'.
000326 EJECT
000327*                          COPY ELCSCTM.
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
000328 EJECT
000329*                          COPY ELCSCRTY.
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
000330*                          COPY MPCSCRT.
      *>>((file: MPCSCRT))
000001******************************************************************
000002*                                                                *
000003*                            MPCSCRT                             *
000004*                            VMOD=1.001                          *
000005*                                                                *
000006*   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
000007*        ACQUIRED BY SIGN-ON PROGRAM EL125.                      *
000008*                                      (MP MORTGAGE PROTECTION)  *
000009*                                                                *
000010******************************************************************
000011*
000012 01  SECURITY-CONTROL-E.
000013     12  SC-COMM-LENGTH-E             PIC S9(04) VALUE +144 COMP.
000014     12  FILLER                       PIC  X(02) VALUE 'SC'.
000015     12  SC-QUID-KEY.
000016         16  SC-QUID-TERMINAL         PIC  X(04).
000017         16  SC-QUID-SYSTEM           PIC  X(04).
000018     12  SC-ITEM                      PIC S9(04) VALUE +1   COMP.
000019     12  SC-SECURITY-ACCESS-CODE      PIC  X(01).
000020     12  SC-PRODUCER-AUTHORIZED-SW    PIC  X(01).
000021         88 SC-PRODUCER-AUTHORIZED               VALUE ' '.
000022         88 SC-PRODUCER-NOT-AUTHORIZED           VALUE 'N'.
000023     12  SC-MP-CODES.
000024         16  SC-MP-AUTHORIZATION OCCURS 44 TIMES.
000025             20  SC-MP-DISPLAY        PIC  X(01).
000026             20  SC-MP-UPDATE         PIC  X(01).
000027     12  FILLER                       PIC  X(40).
      *<<((file: MPCSCRT))
000331 EJECT
000332 01  FILLER                  PIC X(16)  VALUE ALL 'A'.
000333*                          COPY ELCDATE.
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
000334 EJECT
000335*                          COPY ELCLOGOF.
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
000336 EJECT
000337*                          COPY ELCATTR.
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
000338 EJECT
000339*                          COPY ELCEMIB.
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
000340 EJECT
000341 01  FILLER                  PIC X(16)  VALUE ALL 'B'.
000342*                          COPY ELCINTF.
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
000343     12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
000344         16  PI-CHECK-MAINT-TYPE     PIC  X.
000345             88  VALID-MAINT-TYPE            VALUE 'S' 'A'
000346                                                   'C' 'D'.
000347             88  ADD-FUNCTION                VALUE 'A'.
000348             88  SHOW-FUNCTION               VALUE 'S'.
000349             88  DELETE-FUNCTION             VALUE 'D'.
000350             88  CHANGE-FUNCTION             VALUE 'C'.
000351         16  PI-CHECK-TYPE           PIC  X.
000352             88  VALID-TYPE                  VALUE 'A' 'B' 'C'
000353                                                           'G'.
000354         16  PI-CHECK-CARRY-BAL      PIC  X.
000355             88  VALID-CARRY-BAL             VALUE 'Y' 'N'.
000356         16  PI-FIRST-TIME-SW        PIC  X.
000357             88  FIRST-TIME                  VALUE 'Y'.
000358         16  PI-ERCOMP-EOF-SW        PIC  X.
000359             88  ERCOMP-EOF                  VALUE 'Y'.
000360         16  PI-SAVE-PHONE           PIC  X(10).
000361         16  PI-SAVE-PHONE-RED REDEFINES PI-SAVE-PHONE  PIC 9(10).
000362         16  PI-ERC-END-BAL          PIC S9(9)V99       COMP-3.
000363         16  PI-ERCOMP-KEY.
000364             20  PI-ERC-COMPANY-CD   PIC  X.
000365             20  PI-ERC-CARRIER      PIC  X.
000366             20  PI-ERC-GROUP        PIC  X(6).
000367             20  PI-ERC-RESP         PIC  X(10).
000368             20  PI-ERC-ACCT         PIC  X(10).
000369             20  PI-ERC-TYPE         PIC  X.
000370         16  PI-SAVE-ERCOMP-KEY      PIC  X(29).
000371         16  PI-SAVE-FAXNO           PIC  X(10).
000372         16  PI-SAVE-FAXNO-RED REDEFINES PI-SAVE-FAXNO  PIC 9(10).
000373         16  PI-SAVE-ADDR2           PIC X(30).
000374         16  PI-SAVE-CITYST.
000375             20  PI-SAVE-CITY        PIC X(28).
000376             20  PI-SAVE-STATE       PIC XX.
000377         16  PI-SAVE-ACCT-NAME       PIC X(30).
000378         16  PI-EL652-DEL-SW         PIC X.
000379         16  PI-UPDATE-SW            PIC X.
000380         16  FILLER                  PIC  X(457).
000381 EJECT
000382*                            COPY ELCJPFX.
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
000383                             PIC  X(750).
000384 EJECT
000385*                            COPY ELCAID.
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
000386
000387 01  FILLER  REDEFINES  DFHAID.
000388     12  FILLER              PIC  X(8).
000389     12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
000390 EJECT
000391*                            COPY EL652S.
      *>>((file: EL652S))
000001 01  EL652AI.
000002     05  FILLER            PIC  X(0012).
000003*    -------------------------------
000004     05  RUNDATEL PIC S9(0004) COMP.
000005     05  RUNDATEF PIC  X(0001).
000006     05  FILLER REDEFINES RUNDATEF.
000007         10  RUNDATEA PIC  X(0001).
000008     05  RUNDATEI PIC  X(0008).
000009*    -------------------------------
000010     05  RUNTIMEL PIC S9(0004) COMP.
000011     05  RUNTIMEF PIC  X(0001).
000012     05  FILLER REDEFINES RUNTIMEF.
000013         10  RUNTIMEA PIC  X(0001).
000014     05  RUNTIMEI PIC  X(0005).
000015*    -------------------------------
000016     05  CMPNYIDL PIC S9(0004) COMP.
000017     05  CMPNYIDF PIC  X(0001).
000018     05  FILLER REDEFINES CMPNYIDF.
000019         10  CMPNYIDA PIC  X(0001).
000020     05  CMPNYIDI PIC  X(0003).
000021*    -------------------------------
000022     05  USERIDL PIC S9(0004) COMP.
000023     05  USERIDF PIC  X(0001).
000024     05  FILLER REDEFINES USERIDF.
000025         10  USERIDA PIC  X(0001).
000026     05  USERIDI PIC  X(0004).
000027*    -------------------------------
000028     05  MAINTYPL PIC S9(0004) COMP.
000029     05  MAINTYPF PIC  X(0001).
000030     05  FILLER REDEFINES MAINTYPF.
000031         10  MAINTYPA PIC  X(0001).
000032     05  MAINTYPI PIC  X(0001).
000033*    -------------------------------
000034     05  SCDESCL PIC S9(0004) COMP.
000035     05  SCDESCF PIC  X(0001).
000036     05  FILLER REDEFINES SCDESCF.
000037         10  SCDESCA PIC  X(0001).
000038     05  SCDESCI PIC  X(0008).
000039*    -------------------------------
000040     05  FLITYPEL PIC S9(0004) COMP.
000041     05  FLITYPEF PIC  X(0001).
000042     05  FILLER REDEFINES FLITYPEF.
000043         10  FLITYPEA PIC  X(0001).
000044     05  FLITYPEI PIC  X(0004).
000045*    -------------------------------
000046     05  CARRIERL PIC S9(0004) COMP.
000047     05  CARRIERF PIC  X(0001).
000048     05  FILLER REDEFINES CARRIERF.
000049         10  CARRIERA PIC  X(0001).
000050     05  CARRIERI PIC  X(0001).
000051*    -------------------------------
000052     05  GROUPL PIC S9(0004) COMP.
000053     05  GROUPF PIC  X(0001).
000054     05  FILLER REDEFINES GROUPF.
000055         10  GROUPA PIC  X(0001).
000056     05  GROUPI PIC  X(0006).
000057*    -------------------------------
000058     05  TYPEL PIC S9(0004) COMP.
000059     05  TYPEF PIC  X(0001).
000060     05  FILLER REDEFINES TYPEF.
000061         10  TYPEA PIC  X(0001).
000062     05  TYPEI PIC  X(0001).
000063*    -------------------------------
000064     05  FINRESPL PIC S9(0004) COMP.
000065     05  FINRESPF PIC  X(0001).
000066     05  FILLER REDEFINES FINRESPF.
000067         10  FINRESPA PIC  X(0001).
000068     05  FINRESPI PIC  X(0010).
000069*    -------------------------------
000070     05  ACCTNOL PIC S9(0004) COMP.
000071     05  ACCTNOF PIC  X(0001).
000072     05  FILLER REDEFINES ACCTNOF.
000073         10  ACCTNOA PIC  X(0001).
000074     05  ACCTNOI PIC  X(0010).
000075*    -------------------------------
000076     05  SUMMNOL PIC S9(0004) COMP.
000077     05  SUMMNOF PIC  X(0001).
000078     05  FILLER REDEFINES SUMMNOF.
000079         10  SUMMNOA PIC  X(0001).
000080     05  SUMMNOI PIC  X(0006).
000081*    -------------------------------
000082     05  LSTMDTL PIC S9(0004) COMP.
000083     05  LSTMDTF PIC  X(0001).
000084     05  FILLER REDEFINES LSTMDTF.
000085         10  LSTMDTA PIC  X(0001).
000086     05  LSTMDTI PIC  X(0008).
000087*    -------------------------------
000088     05  FLITYPL PIC S9(0004) COMP.
000089     05  FLITYPF PIC  X(0001).
000090     05  FILLER REDEFINES FLITYPF.
000091         10  FLITYPA PIC  X(0001).
000092     05  FLITYPI PIC  X(0001).
000093*    -------------------------------
000094     05  PCONTL PIC S9(0004) COMP.
000095     05  PCONTF PIC  X(0001).
000096     05  FILLER REDEFINES PCONTF.
000097         10  PCONTA PIC  X(0001).
000098     05  PCONTI PIC  X(0030).
000099*    -------------------------------
000100     05  PNT1099L PIC S9(0004) COMP.
000101     05  PNT1099F PIC  X(0001).
000102     05  FILLER REDEFINES PNT1099F.
000103         10  PNT1099A PIC  X(0001).
000104     05  PNT1099I PIC  X(0001).
000105*    -------------------------------
000106     05  MAILNAML PIC S9(0004) COMP.
000107     05  MAILNAMF PIC  X(0001).
000108     05  FILLER REDEFINES MAILNAMF.
000109         10  MAILNAMA PIC  X(0001).
000110     05  MAILNAMI PIC  X(0030).
000111*    -------------------------------
000112     05  SSNL PIC S9(0004) COMP.
000113     05  SSNF PIC  X(0001).
000114     05  FILLER REDEFINES SSNF.
000115         10  SSNA PIC  X(0001).
000116     05  SSNI PIC  X(0013).
000117*    -------------------------------
000118     05  ACCTNAML PIC S9(0004) COMP.
000119     05  ACCTNAMF PIC  X(0001).
000120     05  FILLER REDEFINES ACCTNAMF.
000121         10  ACCTNAMA PIC  X(0001).
000122     05  ACCTNAMI PIC  X(0030).
000123*    -------------------------------
000124     05  PHONEL PIC S9(0004) COMP.
000125     05  PHONEF PIC  X(0001).
000126     05  FILLER REDEFINES PHONEF.
000127         10  PHONEA PIC  X(0001).
000128     05  PHONEI PIC  X(0012).
000129*    -------------------------------
000130     05  ADDR1L PIC S9(0004) COMP.
000131     05  ADDR1F PIC  X(0001).
000132     05  FILLER REDEFINES ADDR1F.
000133         10  ADDR1A PIC  X(0001).
000134     05  ADDR1I PIC  X(0030).
000135*    -------------------------------
000136     05  FAXDESCL PIC S9(0004) COMP.
000137     05  FAXDESCF PIC  X(0001).
000138     05  FILLER REDEFINES FAXDESCF.
000139         10  FAXDESCA PIC  X(0001).
000140     05  FAXDESCI PIC  X(0016).
000141*    -------------------------------
000142     05  FAXNOL PIC S9(0004) COMP.
000143     05  FAXNOF PIC  X(0001).
000144     05  FILLER REDEFINES FAXNOF.
000145         10  FAXNOA PIC  X(0001).
000146     05  FAXNOI PIC  X(0012).
000147*    -------------------------------
000148     05  ADDR2L PIC S9(0004) COMP.
000149     05  ADDR2F PIC  X(0001).
000150     05  FILLER REDEFINES ADDR2F.
000151         10  ADDR2A PIC  X(0001).
000152     05  ADDR2I PIC  X(0030).
000153*    -------------------------------
000154     05  CARBALL PIC S9(0004) COMP.
000155     05  CARBALF PIC  X(0001).
000156     05  FILLER REDEFINES CARBALF.
000157         10  CARBALA PIC  X(0001).
000158     05  CARBALI PIC  X(0001).
000159*    -------------------------------
000160     05  CITYL PIC S9(0004) COMP.
000161     05  CITYF PIC  X(0001).
000162     05  FILLER REDEFINES CITYF.
000163         10  CITYA PIC  X(0001).
000164     05  CITYI PIC  X(0028).
000165*    -------------------------------
000166     05  STATEL PIC S9(0004) COMP.
000167     05  STATEF PIC  X(0001).
000168     05  FILLER REDEFINES STATEF.
000169         10  STATEA PIC  X(0001).
000170     05  STATEI PIC  X(0002).
000171*    -------------------------------
000172     05  REFEHL PIC S9(0004) COMP.
000173     05  REFEHF PIC  X(0001).
000174     05  FILLER REDEFINES REFEHF.
000175         10  REFEHA PIC  X(0001).
000176     05  REFEHI PIC  X(0013).
000177*    -------------------------------
000178     05  REFEL PIC S9(0004) COMP.
000179     05  REFEF PIC  X(0001).
000180     05  FILLER REDEFINES REFEF.
000181         10  REFEA PIC  X(0001).
000182     05  REFEI PIC  X(0001).
000183*    -------------------------------
000184     05  ZIPCODEL PIC S9(0004) COMP.
000185     05  ZIPCODEF PIC  X(0001).
000186     05  FILLER REDEFINES ZIPCODEF.
000187         10  ZIPCODEA PIC  X(0001).
000188     05  ZIPCODEI PIC  X(0010).
000189*    -------------------------------
000190     05  NGDESCL PIC S9(0004) COMP.
000191     05  NGDESCF PIC  X(0001).
000192     05  FILLER REDEFINES NGDESCF.
000193         10  NGDESCA PIC  X(0001).
000194     05  NGDESCI PIC  X(0012).
000195*    -------------------------------
000196     05  NETGRSL PIC S9(0004) COMP.
000197     05  NETGRSF PIC  X(0001).
000198     05  FILLER REDEFINES NETGRSF.
000199         10  NETGRSA PIC  X(0001).
000200     05  NETGRSI PIC  X(0001).
000201*    -------------------------------
000202     05  AHL120L PIC S9(0004) COMP.
000203     05  AHL120F PIC  X(0001).
000204     05  FILLER REDEFINES AHL120F.
000205         10  AHL120A PIC  X(0001).
000206     05  AHL120I PIC  X(0016).
000207*    -------------------------------
000208     05  OVER120L PIC S9(0004) COMP.
000209     05  OVER120F PIC  X(0001).
000210     05  FILLER REDEFINES OVER120F.
000211         10  OVER120A PIC  X(0001).
000212     05  OVER120I PIC  X(0013).
000213*    -------------------------------
000214     05  CSRL PIC S9(0004) COMP.
000215     05  CSRF PIC  X(0001).
000216     05  FILLER REDEFINES CSRF.
000217         10  CSRA PIC  X(0001).
000218     05  CSRI PIC  X(0004).
000219*    -------------------------------
000220     05  RPTCDDL PIC S9(0004) COMP.
000221     05  RPTCDDF PIC  X(0001).
000222     05  FILLER REDEFINES RPTCDDF.
000223         10  RPTCDDA PIC  X(0001).
000224     05  RPTCDDI PIC  X(0010).
000225*    -------------------------------
000226     05  RPTCD2L PIC S9(0004) COMP.
000227     05  RPTCD2F PIC  X(0001).
000228     05  FILLER REDEFINES RPTCD2F.
000229         10  RPTCD2A PIC  X(0001).
000230     05  RPTCD2I PIC  X(0010).
000231*    -------------------------------
000232     05  CKDESCL PIC S9(0004) COMP.
000233     05  CKDESCF PIC  X(0001).
000234     05  FILLER REDEFINES CKDESCF.
000235         10  CKDESCA PIC  X(0001).
000236     05  CKDESCI PIC  X(0016).
000237*    -------------------------------
000238     05  CKPULLL PIC S9(0004) COMP.
000239     05  CKPULLF PIC  X(0001).
000240     05  FILLER REDEFINES CKPULLF.
000241         10  CKPULLA PIC  X(0001).
000242     05  CKPULLI PIC  X(0001).
000243*    -------------------------------
000244     05  BILLPRTL PIC S9(0004) COMP.
000245     05  BILLPRTF PIC  X(0001).
000246     05  FILLER REDEFINES BILLPRTF.
000247         10  BILLPRTA PIC  X(0001).
000248     05  BILLPRTI PIC  X(0001).
000249*    -------------------------------
000250     05  LETDESCL PIC S9(0004) COMP.
000251     05  LETDESCF PIC  X(0001).
000252     05  FILLER REDEFINES LETDESCF.
000253         10  LETDESCA PIC  X(0001).
000254     05  LETDESCI PIC  X(0011).
000255*    -------------------------------
000256     05  LETRCDL PIC S9(0004) COMP.
000257     05  LETRCDF PIC  X(0001).
000258     05  FILLER REDEFINES LETRCDF.
000259         10  LETRCDA PIC  X(0001).
000260     05  LETRCDI PIC  X(0001).
000261*    -------------------------------
000262     05  BALPRTL PIC S9(0004) COMP.
000263     05  BALPRTF PIC  X(0001).
000264     05  FILLER REDEFINES BALPRTF.
000265         10  BALPRTA PIC  X(0001).
000266     05  BALPRTI PIC  X(0011).
000267*    -------------------------------
000268     05  BALCDL PIC S9(0004) COMP.
000269     05  BALCDF PIC  X(0001).
000270     05  FILLER REDEFINES BALCDF.
000271         10  BALCDA PIC  X(0001).
000272     05  BALCDI PIC  X(0001).
000273*    -------------------------------
000274     05  SPPDDHL PIC S9(0004) COMP.
000275     05  SPPDDHF PIC  X(0001).
000276     05  FILLER REDEFINES SPPDDHF.
000277         10  SPPDDHA PIC  X(0001).
000278     05  SPPDDHI PIC  X(0012).
000279*    -------------------------------
000280     05  SPPDDL PIC S9(0004) COMP.
000281     05  SPPDDF PIC  X(0001).
000282     05  FILLER REDEFINES SPPDDF.
000283         10  SPPDDA PIC  X(0001).
000284     05  SPPDDI PIC  X(0001).
000285*    -------------------------------
000286     05  MFLABLL PIC S9(0004) COMP.
000287     05  MFLABLF PIC  X(0001).
000288     05  FILLER REDEFINES MFLABLF.
000289         10  MFLABLA PIC  X(0001).
000290     05  MFLABLI PIC  X(0016).
000291*    -------------------------------
000292     05  MAXFEEL PIC S9(0004) COMP.
000293     05  MAXFEEF PIC  X(0001).
000294     05  FILLER REDEFINES MAXFEEF.
000295         10  MAXFEEA PIC  X(0001).
000296     05  MAXFEEI PIC  S9(04)V99.
000297*    -------------------------------
000298     05  CSLABLL PIC S9(0004) COMP.
000299     05  CSLABLF PIC  X(0001).
000300     05  FILLER REDEFINES CSLABLF.
000301         10  CSLABLA PIC  X(0001).
000302     05  CSLABLI PIC  X(0012).
000303*    -------------------------------
000304     05  CLPSTL PIC S9(0004) COMP.
000305     05  CLPSTF PIC  X(0001).
000306     05  FILLER REDEFINES CLPSTF.
000307         10  CLPSTA PIC  X(0001).
000308     05  CLPSTI PIC  X(0002).
000309*    -------------------------------
000310     05  LFLABLL PIC S9(0004) COMP.
000311     05  LFLABLF PIC  X(0001).
000312     05  FILLER REDEFINES LFLABLF.
000313         10  LFLABLA PIC  X(0001).
000314     05  LFLABLI PIC  X(0016).
000315*    -------------------------------
000316     05  MAXLFL PIC S9(0004) COMP.
000317     05  MAXLFF PIC  X(0001).
000318     05  FILLER REDEFINES MAXLFF.
000319         10  MAXLFA PIC  X(0001).
000320     05  MAXLFI PIC  S9(04)V99.
000321*    -------------------------------
000322     05  BALFWDL PIC S9(0004) COMP.
000323     05  BALFWDF PIC  X(0001).
000324     05  FILLER REDEFINES BALFWDF.
000325         10  BALFWDA PIC  X(0001).
000326     05  BALFWDI PIC  S9(11)V99.
000327*    -------------------------------
000328     05  CURCOML PIC S9(0004) COMP.
000329     05  CURCOMF PIC  X(0001).
000330     05  FILLER REDEFINES CURCOMF.
000331         10  CURCOMA PIC  X(0001).
000332     05  CURCOMI PIC  S9(11)V99.
000333*    -------------------------------
000334     05  CURCHGL PIC S9(0004) COMP.
000335     05  CURCHGF PIC  X(0001).
000336     05  FILLER REDEFINES CURCHGF.
000337         10  CURCHGA PIC  X(0001).
000338     05  CURCHGI PIC  S9(11)V99.
000339*    -------------------------------
000340     05  CURPMTL PIC S9(0004) COMP.
000341     05  CURPMTF PIC  X(0001).
000342     05  FILLER REDEFINES CURPMTF.
000343         10  CURPMTA PIC  X(0001).
000344     05  CURPMTI PIC  S9(11)V99.
000345*    -------------------------------
000346     05  ENDBALL PIC S9(0004) COMP.
000347     05  ENDBALF PIC  X(0001).
000348     05  FILLER REDEFINES ENDBALF.
000349         10  ENDBALA PIC  X(0001).
000350     05  ENDBALI PIC  S9(11)V99.
000351*    -------------------------------
000352     05  CURRENTL PIC S9(0004) COMP.
000353     05  CURRENTF PIC  X(0001).
000354     05  FILLER REDEFINES CURRENTF.
000355         10  CURRENTA PIC  X(0001).
000356     05  CURRENTI PIC  X(0013).
000357*    -------------------------------
000358     05  OVER30L PIC S9(0004) COMP.
000359     05  OVER30F PIC  X(0001).
000360     05  FILLER REDEFINES OVER30F.
000361         10  OVER30A PIC  X(0001).
000362     05  OVER30I PIC  X(0013).
000363*    -------------------------------
000364     05  OVER60L PIC S9(0004) COMP.
000365     05  OVER60F PIC  X(0001).
000366     05  FILLER REDEFINES OVER60F.
000367         10  OVER60A PIC  X(0001).
000368     05  OVER60I PIC  X(0013).
000369*    -------------------------------
000370     05  OVER90L PIC S9(0004) COMP.
000371     05  OVER90F PIC  X(0001).
000372     05  FILLER REDEFINES OVER90F.
000373         10  OVER90A PIC  X(0001).
000374     05  OVER90I PIC  X(0013).
000375*    -------------------------------
000376     05  YTDCOML PIC S9(0004) COMP.
000377     05  YTDCOMF PIC  X(0001).
000378     05  FILLER REDEFINES YTDCOMF.
000379         10  YTDCOMA PIC  X(0001).
000380     05  YTDCOMI PIC  S9(11)V99.
000381*    -------------------------------
000382     05  LSTDTEL PIC S9(0004) COMP.
000383     05  LSTDTEF PIC  X(0001).
000384     05  FILLER REDEFINES LSTDTEF.
000385         10  LSTDTEA PIC  X(0001).
000386     05  LSTDTEI PIC  X(0008).
000387*    -------------------------------
000388     05  LSTTIMEL PIC S9(0004) COMP.
000389     05  LSTTIMEF PIC  X(0001).
000390     05  FILLER REDEFINES LSTTIMEF.
000391         10  LSTTIMEA PIC  X(0001).
000392     05  LSTTIMEI PIC  X(0005).
000393*    -------------------------------
000394     05  LSTUSRL PIC S9(0004) COMP.
000395     05  LSTUSRF PIC  X(0001).
000396     05  FILLER REDEFINES LSTUSRF.
000397         10  LSTUSRA PIC  X(0001).
000398     05  LSTUSRI PIC  X(0004).
000399*    -------------------------------
000400     05  ERRMSG1L PIC S9(0004) COMP.
000401     05  ERRMSG1F PIC  X(0001).
000402     05  FILLER REDEFINES ERRMSG1F.
000403         10  ERRMSG1A PIC  X(0001).
000404     05  ERRMSG1I PIC  X(0072).
000405*    -------------------------------
000406     05  PFKEYL PIC S9(0004) COMP.
000407     05  PFKEYF PIC  X(0001).
000408     05  FILLER REDEFINES PFKEYF.
000409         10  PFKEYA PIC  X(0001).
000410     05  PFKEYI PIC  99.
000411*    -------------------------------
000412     05  PFK5L PIC S9(0004) COMP.
000413     05  PFK5F PIC  X(0001).
000414     05  FILLER REDEFINES PFK5F.
000415         10  PFK5A PIC  X(0001).
000416     05  PFK5I PIC  X(0011).
000417*    -------------------------------
000418     05  PFK7L PIC S9(0004) COMP.
000419     05  PFK7F PIC  X(0001).
000420     05  FILLER REDEFINES PFK7F.
000421         10  PFK7A PIC  X(0001).
000422     05  PFK7I PIC  X(0012).
000423*    -------------------------------
000424     05  PFK9L PIC S9(0004) COMP.
000425     05  PFK9F PIC  X(0001).
000426     05  FILLER REDEFINES PFK9F.
000427         10  PFK9A PIC  X(0001).
000428     05  PFK9I PIC  X(0012).
000429*    -------------------------------
000430     05  PFK13L PIC S9(0004) COMP.
000431     05  PFK13F PIC  X(0001).
000432     05  FILLER REDEFINES PFK13F.
000433         10  PFK13A PIC  X(0001).
000434     05  PFK13I PIC  X(0011).
000435*    -------------------------------
000436     05  PFK4L PIC S9(0004) COMP.
000437     05  PFK4F PIC  X(0001).
000438     05  FILLER REDEFINES PFK4F.
000439         10  PFK4A PIC  X(0001).
000440     05  PFK4I PIC  X(0012).
000441*    -------------------------------
000442     05  PFK6L PIC S9(0004) COMP.
000443     05  PFK6F PIC  X(0001).
000444     05  FILLER REDEFINES PFK6F.
000445         10  PFK6A PIC  X(0001).
000446     05  PFK6I PIC  X(0011).
000447*    -------------------------------
000448     05  PFK10L PIC S9(0004) COMP.
000449     05  PFK10F PIC  X(0001).
000450     05  FILLER REDEFINES PFK10F.
000451         10  PFK10A PIC  X(0001).
000452     05  PFK10I PIC  X(0014).
000453 01  EL652AO REDEFINES EL652AI.
000454     05  FILLER            PIC  X(0012).
000455*    -------------------------------
000456     05  FILLER            PIC  X(0003).
000457     05  RUNDATEO PIC  X(0008).
000458*    -------------------------------
000459     05  FILLER            PIC  X(0003).
000460     05  RUNTIMEO PIC  99.99.
000461*    -------------------------------
000462     05  FILLER            PIC  X(0003).
000463     05  CMPNYIDO PIC  X(0003).
000464*    -------------------------------
000465     05  FILLER            PIC  X(0003).
000466     05  USERIDO PIC  X(0004).
000467*    -------------------------------
000468     05  FILLER            PIC  X(0003).
000469     05  MAINTYPO PIC  X(0001).
000470*    -------------------------------
000471     05  FILLER            PIC  X(0003).
000472     05  SCDESCO PIC  X(0008).
000473*    -------------------------------
000474     05  FILLER            PIC  X(0003).
000475     05  FLITYPEO PIC  X(0004).
000476*    -------------------------------
000477     05  FILLER            PIC  X(0003).
000478     05  CARRIERO PIC  X(0001).
000479*    -------------------------------
000480     05  FILLER            PIC  X(0003).
000481     05  GROUPO PIC  X(0006).
000482*    -------------------------------
000483     05  FILLER            PIC  X(0003).
000484     05  TYPEO PIC  X(0001).
000485*    -------------------------------
000486     05  FILLER            PIC  X(0003).
000487     05  FINRESPO PIC  X(0010).
000488*    -------------------------------
000489     05  FILLER            PIC  X(0003).
000490     05  ACCTNOO PIC  X(0010).
000491*    -------------------------------
000492     05  FILLER            PIC  X(0003).
000493     05  SUMMNOO PIC  X(0006).
000494*    -------------------------------
000495     05  FILLER            PIC  X(0003).
000496     05  LSTMDTO PIC  X(0008).
000497*    -------------------------------
000498     05  FILLER            PIC  X(0003).
000499     05  FLITYPO PIC  X(0001).
000500*    -------------------------------
000501     05  FILLER            PIC  X(0003).
000502     05  PCONTO PIC  X(0030).
000503*    -------------------------------
000504     05  FILLER            PIC  X(0003).
000505     05  PNT1099O PIC  X(0001).
000506*    -------------------------------
000507     05  FILLER            PIC  X(0003).
000508     05  MAILNAMO PIC  X(0030).
000509*    -------------------------------
000510     05  FILLER            PIC  X(0003).
000511     05  SSNO PIC  X(0013).
000512*    -------------------------------
000513     05  FILLER            PIC  X(0003).
000514     05  ACCTNAMO PIC  X(0030).
000515*    -------------------------------
000516     05  FILLER            PIC  X(0003).
000517     05  PHONEO PIC  X(0012).
000518*    -------------------------------
000519     05  FILLER            PIC  X(0003).
000520     05  ADDR1O PIC  X(0030).
000521*    -------------------------------
000522     05  FILLER            PIC  X(0003).
000523     05  FAXDESCO PIC  X(0016).
000524*    -------------------------------
000525     05  FILLER            PIC  X(0003).
000526     05  FAXNOO PIC  X(0012).
000527*    -------------------------------
000528     05  FILLER            PIC  X(0003).
000529     05  ADDR2O PIC  X(0030).
000530*    -------------------------------
000531     05  FILLER            PIC  X(0003).
000532     05  CARBALO PIC  X(0001).
000533*    -------------------------------
000534     05  FILLER            PIC  X(0003).
000535     05  CITYO PIC  X(0028).
000536*    -------------------------------
000537     05  FILLER            PIC  X(0003).
000538     05  STATEO PIC  X(0002).
000539*    -------------------------------
000540     05  FILLER            PIC  X(0003).
000541     05  REFEHO PIC  X(0013).
000542*    -------------------------------
000543     05  FILLER            PIC  X(0003).
000544     05  REFEO PIC  X(0001).
000545*    -------------------------------
000546     05  FILLER            PIC  X(0003).
000547     05  ZIPCODEO PIC  X(0010).
000548*    -------------------------------
000549     05  FILLER            PIC  X(0003).
000550     05  NGDESCO PIC  X(0012).
000551*    -------------------------------
000552     05  FILLER            PIC  X(0003).
000553     05  NETGRSO PIC  X(0001).
000554*    -------------------------------
000555     05  FILLER            PIC  X(0003).
000556     05  AHL120O PIC  X(0016).
000557*    -------------------------------
000558     05  FILLER            PIC  X(0003).
000559     05  OVER120O PIC  Z,ZZZ,ZZZ.99-.
000560*    -------------------------------
000561     05  FILLER            PIC  X(0003).
000562     05  CSRO PIC  X(0004).
000563*    -------------------------------
000564     05  FILLER            PIC  X(0003).
000565     05  RPTCDDO PIC  X(0010).
000566*    -------------------------------
000567     05  FILLER            PIC  X(0003).
000568     05  RPTCD2O PIC  X(0010).
000569*    -------------------------------
000570     05  FILLER            PIC  X(0003).
000571     05  CKDESCO PIC  X(0016).
000572*    -------------------------------
000573     05  FILLER            PIC  X(0003).
000574     05  CKPULLO PIC  X(0001).
000575*    -------------------------------
000576     05  FILLER            PIC  X(0003).
000577     05  BILLPRTO PIC  X(0001).
000578*    -------------------------------
000579     05  FILLER            PIC  X(0003).
000580     05  LETDESCO PIC  X(0011).
000581*    -------------------------------
000582     05  FILLER            PIC  X(0003).
000583     05  LETRCDO PIC  X(0001).
000584*    -------------------------------
000585     05  FILLER            PIC  X(0003).
000586     05  BALPRTO PIC  X(0011).
000587*    -------------------------------
000588     05  FILLER            PIC  X(0003).
000589     05  BALCDO PIC  X(0001).
000590*    -------------------------------
000591     05  FILLER            PIC  X(0003).
000592     05  SPPDDHO PIC  X(0012).
000593*    -------------------------------
000594     05  FILLER            PIC  X(0003).
000595     05  SPPDDO PIC  X(0001).
000596*    -------------------------------
000597     05  FILLER            PIC  X(0003).
000598     05  MFLABLO PIC  X(0016).
000599*    -------------------------------
000600     05  FILLER            PIC  X(0003).
000601     05  MAXFEEO PIC  Z99.99.
000602*    -------------------------------
000603     05  FILLER            PIC  X(0003).
000604     05  CSLABLO PIC  X(0012).
000605*    -------------------------------
000606     05  FILLER            PIC  X(0003).
000607     05  CLPSTO PIC  X(0002).
000608*    -------------------------------
000609     05  FILLER            PIC  X(0003).
000610     05  LFLABLO PIC  X(0016).
000611*    -------------------------------
000612     05  FILLER            PIC  X(0003).
000613     05  MAXLFO PIC  Z99.99.
000614*    -------------------------------
000615     05  FILLER            PIC  X(0003).
000616     05  BALFWDO PIC  Z,ZZZ,ZZZ.99-.
000617*    -------------------------------
000618     05  FILLER            PIC  X(0003).
000619     05  CURCOMO PIC  Z,ZZZ,ZZZ.99-.
000620*    -------------------------------
000621     05  FILLER            PIC  X(0003).
000622     05  CURCHGO PIC  Z,ZZZ,ZZZ.99-.
000623*    -------------------------------
000624     05  FILLER            PIC  X(0003).
000625     05  CURPMTO PIC  Z,ZZZ,ZZZ.99-.
000626*    -------------------------------
000627     05  FILLER            PIC  X(0003).
000628     05  ENDBALO PIC  Z,ZZZ,ZZZ.99-.
000629*    -------------------------------
000630     05  FILLER            PIC  X(0003).
000631     05  CURRENTO PIC  Z,ZZZ,ZZZ.99-.
000632*    -------------------------------
000633     05  FILLER            PIC  X(0003).
000634     05  OVER30O PIC  Z,ZZZ,ZZZ.99-.
000635*    -------------------------------
000636     05  FILLER            PIC  X(0003).
000637     05  OVER60O PIC  Z,ZZZ,ZZZ.99-.
000638*    -------------------------------
000639     05  FILLER            PIC  X(0003).
000640     05  OVER90O PIC  Z,ZZZ,ZZZ.99-.
000641*    -------------------------------
000642     05  FILLER            PIC  X(0003).
000643     05  YTDCOMO PIC  Z,ZZZ,ZZZ.99-.
000644*    -------------------------------
000645     05  FILLER            PIC  X(0003).
000646     05  LSTDTEO PIC  X(0008).
000647*    -------------------------------
000648     05  FILLER            PIC  X(0003).
000649     05  LSTTIMEO PIC  99.99.
000650*    -------------------------------
000651     05  FILLER            PIC  X(0003).
000652     05  LSTUSRO PIC  X(0004).
000653*    -------------------------------
000654     05  FILLER            PIC  X(0003).
000655     05  ERRMSG1O PIC  X(0072).
000656*    -------------------------------
000657     05  FILLER            PIC  X(0003).
000658     05  PFKEYO PIC  X(0002).
000659*    -------------------------------
000660     05  FILLER            PIC  X(0003).
000661     05  PFK5O PIC  X(0011).
000662*    -------------------------------
000663     05  FILLER            PIC  X(0003).
000664     05  PFK7O PIC  X(0012).
000665*    -------------------------------
000666     05  FILLER            PIC  X(0003).
000667     05  PFK9O PIC  X(0012).
000668*    -------------------------------
000669     05  FILLER            PIC  X(0003).
000670     05  PFK13O PIC  X(0011).
000671*    -------------------------------
000672     05  FILLER            PIC  X(0003).
000673     05  PFK4O PIC  X(0012).
000674*    -------------------------------
000675     05  FILLER            PIC  X(0003).
000676     05  PFK6O PIC  X(0011).
000677*    -------------------------------
000678     05  FILLER            PIC  X(0003).
000679     05  PFK10O PIC  X(0014).
000680*    -------------------------------
      *<<((file: EL652S))
000392 EJECT
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
000394
000395 01  DFHCOMMAREA             PIC  X(1024).
000396
000397*01 PARMLIST .
000398*    12  FILLER              PIC S9(8)                  COMP.
000399*    12  ERCOMP-POINTER      PIC S9(8)                  COMP.
000400*    12  ERNAME-POINTER      PIC S9(8)                  COMP.
000401*    12  ELCNTL-POINTER      PIC S9(8)                  COMP.
000402*    12  ERSUMM-POINTER      PIC S9(8)                  COMP.
000403*    12  ERRQST-POINTER      PIC S9(8)                  COMP.
000404 EJECT
000405*                            COPY ERCCOMP.
      *>>((file: ERCCOMP))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCOMP                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.019                          *
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = COMPENSATION MASTER                       *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 700   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
000016*       ALTERNATE PATH = NONE                                    *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021******************************************************************
000022*                   C H A N G E   L O G
000023*
000024* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000025*-----------------------------------------------------------------
000026*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000027* EFFECTIVE    NUMBER
000028*-----------------------------------------------------------------
000029* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
000030* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
000031* 092205    2005050300006  PEMA  ADD LEASE FEE
000032* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
000033* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
000034* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
000035* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000036* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
000037* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
000038******************************************************************
000039
000040 01  COMPENSATION-MASTER.
000041     12  CO-RECORD-ID                          PIC XX.
000042         88  VALID-CO-ID                          VALUE 'CO'.
000043
000044     12  CO-CONTROL-PRIMARY.
000045         16  CO-COMPANY-CD                     PIC X.
000046         16  CO-CONTROL.
000047             20  CO-CTL-1.
000048                 24  CO-CARR-GROUP.
000049                     28  CO-CARRIER            PIC X.
000050                     28  CO-GROUPING.
000051                         32  CO-GROUP-PREFIX   PIC XXX.
000052                         32  CO-GROUP-PRIME    PIC XXX.
000053                 24  CO-RESP-NO.
000054                     28  CO-RESP-PREFIX        PIC X(4).
000055                     28  CO-RESP-PRIME         PIC X(6).
000056             20  CO-CTL-2.
000057                 24  CO-ACCOUNT.
000058                     28  CO-ACCT-PREFIX        PIC X(4).
000059                     28  CO-ACCT-PRIME         PIC X(6).
000060         16  CO-TYPE                           PIC X.
000061             88  CO-COMPANY-TYPE                  VALUE 'C'.
000062             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
000063             88  CO-ACCOUNT-TYPE                  VALUE 'A'.
000064
000065     12  CO-MAINT-INFORMATION.
000066         16  CO-LAST-MAINT-DT                  PIC XX.
000067         16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
000068         16  CO-LAST-MAINT-USER                PIC X(4).
000069     12  FILLER                                PIC XX.
000070     12  CO-STMT-TYPE                          PIC XXX.
000071     12  CO-COMP-TYPE                          PIC X.
000072         88  CO-COMP-IS-SPPDD                    VALUE '1'.
000073     12  CO-STMT-OWNER                         PIC X(4).
000074     12  CO-BALANCE-CONTROL                    PIC X.
000075         88  CO-CARRY-BALANCE                     VALUE 'Y'.
000076         88  CO-NO-BALANCE                        VALUE 'N'.
000077
000078     12  CO-INTERNAL-CONTROL-1                 PIC X.
000079         88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
000080         88  CO-AUTO-GENERATED                    VALUE 'Y'.
000081         88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
000082
000083     12  CO-INTERNAL-CONTROL-2                 PIC X.
000084         88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
000085         88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
000086
000087     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
000088     12  CO-GA-DIRECT-DEP                      PIC X.
000089     12  CO-FUTURE-SPACE                       PIC X.
000090         88  CO-FUTURE-NOT-USED                   VALUE ' '.
000091
000092     12  CO-ACCT-NAME                          PIC X(30).
000093     12  CO-MAIL-NAME                          PIC X(30).
000094     12  CO-ADDR-1                             PIC X(30).
000095     12  CO-ADDR-2                             PIC X(30).
000096     12  CO-ADDR-3.
000097         16  CO-ADDR-CITY                      PIC X(27).
000098         16  CO-ADDR-STATE                     PIC XX.
000099     12  CO-CSO-1099                           PIC X.
000100     12  CO-ZIP.
000101         16  CO-ZIP-PRIME.
000102             20  CO-ZIP-PRI-1ST                PIC X.
000103                 88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
000104             20  FILLER                        PIC X(4).
000105         16  CO-ZIP-PLUS4                      PIC X(4).
000106     12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
000107         16  CO-CAN-POSTAL-1                   PIC XXX.
000108         16  CO-CAN-POSTAL-2                   PIC XXX.
000109         16  FILLER                            PIC XXX.
000110     12  CO-SOC-SEC                            PIC X(13).
000111     12  CO-TELEPHONE.
000112         16  CO-AREA-CODE                      PIC XXX.
000113         16  CO-PREFIX                         PIC XXX.
000114         16  CO-PHONE                          PIC X(4).
000115
000116     12  CO-ROLADEX-PRINT-DT                   PIC XX.
000117
000118     12  CO-AR-BAL-LEVEL                       PIC X.
000119         88  CO-AR-REF-LVL                        VALUE '1'.
000120         88  CO-AR-BILL-REF-LVL                   VALUE '1'.
000121         88  CO-AR-BILL-LVL                       VALUE '2'.
000122         88  CO-AR-AGT-LVL                        VALUE '3'.
000123         88  CO-AR-FR-LVL                         VALUE '4'.
000124
000125     12  CO-AR-NORMAL-PRINT                    PIC X.
000126         88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
000127         88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
000128
000129     12  CO-AR-SUMMARY-CODE                    PIC X(6).
000130
000131     12  CO-AR-REPORTING                       PIC X.
000132         88  CO-AR-NET-REPORT                     VALUE 'N'.
000133         88  CO-AR-GROSS-REPORT                   VALUE 'G'.
000134
000135     12  CO-AR-PULL-CHECK                      PIC X.
000136         88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
000137         88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
000138
000139     12  CO-AR-BALANCE-PRINT                   PIC X.
000140         88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
000141
000142     12  CO-AR-LAST-RUN-CODE                   PIC X.
000143         88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
000144         88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
000145         88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
000146
000147     12  CO-LAST-EOM-STMT-DT                   PIC XX.
000148
000149     12  CO-USER-CODE                          PIC X.
000150     12  CO-REPORT-GROUP-ID                    PIC X(12).
000151
000152******************************************************************
000153*    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
000154*    THE LAST MONTH END RUN.
000155******************************************************************
000156
000157     12  CO-LAST-ACTIVITY-DATE.
000158         16  CO-ACT-YEAR                       PIC 99.
000159         16  CO-ACT-MONTH                      PIC 99.
000160         16  CO-ACT-DAY                        PIC 99.
000161
000162     12  CO-LAST-STMT-DT.
000163         16  CO-LAST-STMT-YEAR                 PIC 99.
000164         16  CO-LAST-STMT-MONTH                PIC 99.
000165         16  CO-LAST-STMT-DAY                  PIC 99.
000166
000167     12  CO-MO-END-TOTALS.
000168         16  CO-MONTHLY-TOTALS.
000169             20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
000170             20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
000171             20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
000172             20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
000173             20  CO-END-BAL                PIC S9(7)V99   COMP-3.
000174
000175         16  CO-AGING-TOTALS.
000176             20  CO-CUR                    PIC S9(7)V99   COMP-3.
000177             20  CO-OV30                   PIC S9(7)V99   COMP-3.
000178             20  CO-OV60                   PIC S9(7)V99   COMP-3.
000179             20  CO-OV90                   PIC S9(7)V99   COMP-3.
000180
000181         16  CO-YTD-TOTALS.
000182             20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
000183             20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
000184
000185         16  CO-OVER-UNDER-TOTALS.
000186             20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
000187             20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
000188
000189     12  CO-MISCELLANEOUS-TOTALS.
000190         16  CO-FICA-TOTALS.
000191             20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
000192             20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
000193
000194         16  CO-CLAIM-TOTALS.
000195             20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
000196             20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
000197
000198******************************************************************
000199*    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
000200*    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
000201******************************************************************
000202
000203     12  CO-CURRENT-TOTALS.
000204         16  CO-CURRENT-LAST-STMT-DT.
000205             20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
000206             20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
000207             20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
000208
000209         16  CO-CURRENT-MONTHLY-TOTALS.
000210             20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
000211             20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
000212             20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
000213             20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
000214             20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
000215
000216         16  CO-CURRENT-AGING-TOTALS.
000217             20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
000218             20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
000219             20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
000220             20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
000221
000222         16  CO-CURRENT-YTD-TOTALS.
000223             20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
000224             20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
000225
000226     12  CO-PAID-COMM-TOTALS.
000227         16  CO-YTD-PAID-COMMS.
000228             20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
000229             20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
000230
000231     12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
000232         88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
000233         88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
000234
000235     12  CO-DELINQUENT-LETTER-CODE         PIC X.
000236         88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
000237         88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
000238         88  CO-AGENT-1ST-LETTER              VALUE 'B'.
000239         88  CO-AGENT-2ND-LETTER              VALUE 'G'.
000240         88  CO-OVERWRITE-LETTER              VALUE 'O'.
000241         88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
000242         88  CO-FINAL-LETTER                  VALUE 'F'.
000243         88  CO-RECONCILING                   VALUE 'R'.
000244         88  CO-PHONE-CALL                    VALUE 'P'.
000245         88  CO-LEGAL                         VALUE 'L'.
000246         88  CO-COLLECTION-AGENCY             VALUE 'C'.
000247         88  CO-WRITE-OFF                     VALUE 'W'.
000248         88  CO-NO-ACTION                     VALUE 'N' ' '.
000249
000250     12  CO-CSR-CODE                       PIC X(4).
000251
000252     12  CO-GA-STATUS-INFO.
000253         16  CO-GA-EFFECTIVE-DT            PIC XX.
000254         16  CO-GA-TERMINATION-DT          PIC XX.
000255         16  CO-GA-STATUS-CODE             PIC X.
000256             88  CO-GA-ACTIVE                 VALUE 'A'.
000257             88  CO-GA-INACTIVE               VALUE 'I'.
000258             88  CO-GA-PENDING                VALUE 'P'.
000259         16  CO-GA-COMMENTS.
000260             20  CO-GA-COMMENT-1           PIC X(40).
000261             20  CO-GA-COMMENT-2           PIC X(40).
000262             20  CO-GA-COMMENT-3           PIC X(40).
000263             20  CO-GA-COMMENT-4           PIC X(40).
000264
000265     12  CO-RPTCD2                         PIC X(10).
000266     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
000267         16  CO-OV120                      PIC S9(7)V99   COMP-3.
000268         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
000269
000270     12  CO-TYPE-AGENT                     PIC X(01).
000271         88  CO-CORPORATION                   VALUE 'C'.
000272         88  CO-PARTNERSHIP                   VALUE 'P'.
000273         88  CO-SOLE-PROPRIETOR               VALUE 'S'.
000274         88  CO-TRUST                         VALUE 'T'.
000275         88  CO-UNKNOWN                       VALUE ' ' 'X'.
000276
000277     12  CO-FAXNO.
000278         16  CO-FAX-AREA-CODE                  PIC XXX.
000279         16  CO-FAX-PREFIX                     PIC XXX.
000280         16  CO-FAX-PHONE                      PIC X(4).
000281
000282     12  CO-BANK-INFORMATION.
000283         16  CO-BANK-TRANSIT-NO                PIC X(8).
000284         16  CO-BANK-TRANSIT-NON REDEFINES
000285             CO-BANK-TRANSIT-NO                PIC 9(8).
000286
000287         16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
000288     12  CO-MISC-DEDUCT-INFO REDEFINES
000289                  CO-BANK-INFORMATION.
000290         16  CO-MD-GL-ACCT                     PIC X(10).
000291         16  CO-MD-DIV                         PIC XX.
000292         16  CO-MD-CENTER                      PIC X(4).
000293         16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
000294         16  CO-CREATE-AP-CHECK                PIC X.
000295         16  CO-DELIVER-CK-TO-MEL              PIC X.
000296         16  FILLER                            PIC XXX.
000297     12  CO-ACH-STATUS                         PIC X.
000298         88  CO-ACH-ACTIVE                         VALUE 'A'.
000299         88  CO-ACH-PENDING                        VALUE 'P'.
000300
000301     12  CO-BILL-SW                            PIC X.
000302     12  CO-CONTROL-NAME                       PIC X(30).
000303     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
000304     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
000305     12  CO-CLP-STATE                          PIC XX.
000306     12  CO-FIRST-WRITTEN-DT                   PIC XX.
000307     12  CO-SPP-REFUND-EDIT                    PIC X.
000308
000309******************************************************************
      *<<((file: ERCCOMP))
000406*                            COPY ERCAGTC.
      *>>((file: ERCAGTC))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCAGTC                             *
000005*                            VMOD=2.001                          *
000006*                                                                *
000007*   ONLINE CREDIT SYSTEM                                         *
000008*                                                                *
000009*   FILE DESCRIPTION = AGENT COMMISSIONS                         *
000010*                                                                *
000011*   FILE TYPE = VSAM,KSDS                                        *
000012*   RECORD SIZE = 450   RECFORM = FIXED                          *
000013*                                                                *
000014*   BASE CLUSTER NAME = ERAGTC                   RKP=2,LEN=21    *
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
000028* 111004    2004110300005  PEMA  ADD DISTRIBUTION OF ADDENDUM FEE
000029* 111004                         NEW FILE AND COPYBOOK
000030* 092205                   PEMA  ADD PROCESSING FOR SPP LEASE
000031******************************************************************
000032
000033 01  AGENT-COMMISSIONS.
000034     12  AG-RECORD-ID                          PIC XX.
000035         88  VALID-CO-ID                          VALUE 'AG'.
000036
000037     12  AG-CONTROL-PRIMARY.
000038         16  AG-COMPANY-CD                     PIC X.
000039         16  AG-CONTROL.
000040             20  AG-CTL-1.
000041                 24  AG-CARR-GROUP.
000042                     28  AG-CARRIER            PIC X.
000043                     28  AG-GROUPING           PIC X(6).
000044                 24  AG-BANK                   PIC X(10).
000045             20  AG-CTL-2.
000046                 24  AG-EXP-DT                 PIC XX.
000047         16  AG-TYPE                           PIC X.
000048             88  AG-GEN-AGENT-TYPE                VALUE 'G'.
000049             88  AG-ACCOUNT-TYPE                  VALUE 'A'.
000050
000051     12  AG-MAINT-INFORMATION.
000052         16  AG-LAST-MAINT-DT                  PIC XX.
000053         16  AG-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
000054         16  AG-LAST-MAINT-USER                PIC X(4).
000055         16  FILLER                            PIC X(10).
000056
000057     12  AG-EFF-DT                             PIC XX.
000058     12  AG-COMM-STRUCTURE.
000059         16  AG-AGT-COMMS OCCURS 10.
000060             20  AG-AGT                PIC X(10).
000061             20  AG-COM-TYP            PIC X.
000062             20  AG-SPP-FEES           PIC S9(5)V99   COMP-3.
000063             20  AG-RECALC-LV-INDIC    PIC X.
000064             20  AG-SPP-LFEES          PIC S9(5)V99   COMP-3.
000065             20  AG-LRCALC-LV-INDIC    PIC X.
000066             20  FILLER                PIC X(05).
000067
000068     12  FILLER                  PIC X(145).
000069******************************************************************
      *<<((file: ERCAGTC))
000407 EJECT
000408*                            COPY ERCNAME.
      *>>((file: ERCNAME))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCNAME                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003                          *
000007*                                                                *
000008*   CREDIT SYSTEM ACCOUNT MASTER NAME, COMPENSATION MASTER       *
000009*       NAME, REINSURANCE COMPANY NAME LOOKUP FILE.              *
000010*                                                                *
000011*   FILE DESCRIPTION = NAME LOOKUP FILE                          *
000012*                                                                *
000013*   FILE TYPE = VSAM,KSDS                                        *
000014*   RECORD SIZE = 160   RECFORM = FIX                            *
000015*                                                                *
000016*   BASE CLUSTER NAME = ERNAME                    RKP=2,LEN=61   *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021*                                                                *
000022******************************************************************
000023
000024 01  NAME-LOOKUP-MASTER.
000025     12  NL-RECORD-ID                PIC  X(02).
000026         88  VALID-NL-ID                         VALUE 'NL'.
000027
000028     12  NL-RECORD-KEY.
000029         16  NL-CONTROL-PRIMARY.
000030             20  NL-COMPANY-CD       PIC  X(01).
000031             20  NL-NAME             PIC  X(30).
000032             20  NL-RECORD-TYPE      PIC  X(01).
000033                 88  NL-ACCOUNT-TYPE             VALUE 'A'.
000034                 88  NL-COMPENSATION-TYPE        VALUE 'C'.
000035                 88  NL-REINSURANCE-TYPE         VALUE 'R'.
000036
000037         16  NL-ACCOUNT-MASTER.
000038             20  NL-AM-COMPANY-CD    PIC  X(01).
000039             20  NL-AM-CARRIER       PIC  X(01).
000040             20  NL-AM-GROUPING      PIC  X(06).
000041             20  NL-AM-STATE         PIC  X(02).
000042             20  NL-AM-ACCOUNT       PIC  X(10).
000043             20  FILLER              PIC  X(09).
000044
000045         16  NL-COMPENSATION-MASTER
000046                                 REDEFINES  NL-ACCOUNT-MASTER.
000047             20  NL-CO-COMPANY-CD    PIC  X(01).
000048             20  NL-CO-CARRIER       PIC  X(01).
000049             20  NL-CO-GROUPING      PIC  X(06).
000050             20  NL-CO-RESP-NO       PIC  X(10).
000051             20  NL-CO-ACCOUNT       PIC  X(10).
000052             20  NL-CO-TYPE          PIC  X(01).
000053
000054         16  NL-REINSURANCE-RECORD
000055                                 REDEFINES  NL-ACCOUNT-MASTER.
000056             20  NL-RE-COMPANY-CD    PIC  X(01).
000057             20  NL-RE-CODE          PIC  X(01).
000058             20  NL-RE-COMPANY.
000059                 24  NL-RE-COMP      PIC  X(03).
000060                 24  NL-RE-CO-SUB    PIC  X(03).
000061             20  NL-RE-TABLE         PIC  X(03).
000062             20  FILLER              PIC  X(18).
000063
000064     12  NL-MAINT-INFORMATION.
000065         16  NL-LAST-MAINT-DT        PIC  X(02).
000066         16  NL-LAST-MAINT-HHMMSS    PIC S9(07)  COMP-3.
000067         16  NL-LAST-MAINT-USER      PIC  X(04).
000068         16  FILLER                  PIC  X(10).
000069
000070     12  NL-RE-LEVELS  OCCURS  30  TIMES.
000071         16  NL-RE-LEVEL             PIC  9(02).
000072
000073     12  NL-CITY                     PIC  X(15).
000074     12  NL-ST                       PIC  XX.
000075
000076******************************************************************
      *<<((file: ERCNAME))
000409 EJECT
000410*                            COPY ELCCNTL.
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
000411 EJECT
000412*                            COPY ERCSUMM.
      *>>((file: ERCSUMM))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCSUMM                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = AR SUMMARY CROSS REFERENCE                *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 150           RECFORM = FIXED                  *
000014*                                                                *
000015*   BASE CLUSTER NAME = ERSUMM                   RKP=2,LEN=34    *
000016*                                                                *
000017*       ALTERNATE PATH1 = ERSUMM2  (BY CO SUMMARY CARR           *
000018*                                      GROUP F.R. AGENT)         *
000019*                                                 RKP=36 ,LEN=34 *
000020*                                                                *
000021*   LOG = NO                                                     *
000022*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000023*                                                                *
000024******************************************************************
000025
000026 01  SUMM-CROSS-REFERENCE.
000027     12  SX-RECORD-ID                PIC XX.
000028         88  VALID-SX-ID             VALUE 'SX'.
000029
000030     12  SX-CONTROL-PRIMARY.
000031         16  SX-COMPANY-CD           PIC X.
000032         16  SX-SUMMARY              PIC X(6).
000033         16  SX-CARRIER              PIC X.
000034         16  SX-GROUP                PIC X(6).
000035         16  SX-FIN-RESP             PIC X(10).
000036         16  SX-ACCT-AGENT           PIC X(10).
000037
000038     12  SX-CONTROL-A1.
000039         16  SX-COMPANY-A1           PIC X.
000040         16  SX-ACCT-AGENT-A1        PIC X(10).
000041         16  SX-SUMMARY-A1           PIC X(6).
000042         16  SX-CARR-A1              PIC X.
000043         16  SX-GROUP-A1             PIC X(6).
000044         16  SX-FIN-RESP-A1          PIC X(10).
000045
000046     12  SX-MAINT-INFORMATION.
000047         16  SX-LAST-MAINT-DT        PIC XX.
000048         16  SX-LAST-MAINT-BY        PIC X(4).
000049         16  SX-LAST-MAINT-HHMMSS    PIC S9(7)  COMP-3.
000050
000051     12  SX-SUMM-OR-AGT-NAME         PIC X(30).
000052
000053     12  FILLER                      PIC X(40).
000054
000055******************************************************************
      *<<((file: ERCSUMM))
000413 EJECT
000414*                            COPY ERCRQST.
      *>>((file: ERCRQST))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCRQST.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*   FILE DESCRIPTION = ACCOUNTS RECEIVABLE REQUEST RECORD        *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 200  RECFORM = FIXED                           *
000012*                                                                *
000013*   BASE CLUSTER = ERRQST                         RKP=2,LEN=7    *
000014*       ALTERNATE PATH1 = ERRQST2  (BY CO, CAR, GROUP, ST,       *
000015*                                   ACCOUNT, REF, BATCH)         *
000016*                                                RKP=9, LEN=38   *
000017*       ALTERNATE PATH2 = ERRQST3  (BY CO, CAR, GROUP, FIN  RESP *
000018*                                   ACCOUNT, REF, BATCH)         *
000019*                                                RKP=47, LEN=46  *
000020*       ALTERNATE PATH3 = ERRQST4  (BY CO, CAR, GROUP, AGENT,    *
000021*                                   BATCH)                       *
000022*                                                RKP=93, LEN=24  *
000023*       ALTERNATE PATH4 = ERRQST5  (BY CO, SUMMARY CODE, ACCT,   *
000024*                                   REF, BATCH)                  *
000025*                                                RKP=117, LEN=35 *
000026*   LOG = NO                                                     *
000027*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000028******************************************************************
000029
000030 01  AR-REQUEST-RECORD.
000031     12  RQ-RECORD-ID                     PIC XX.
000032         88  VALID-RQ-ID                        VALUE 'RQ'.
000033
000034     12  RQ-CONTROL-PRIMARY.
000035         16  RQ-COMPANY-CD                PIC X.
000036         16  RQ-ENTRY-BATCH               PIC X(6).
000037
000038     12  RQ-CONTROL-BY-ACCT-REF.
000039         16  RQ-COMPANY-CD-A1             PIC X.
000040         16  RQ-CARRIER-A1                PIC X.
000041         16  RQ-GROUPING-A1               PIC X(6).
000042         16  RQ-STATE-A1                  PIC XX.
000043         16  RQ-ACCOUNT-A1                PIC X(10).
000044         16  RQ-REFERENCE-A1              PIC X(12).
000045         16  RQ-BATCH-A1                  PIC X(6).
000046
000047     12  RQ-CONTROL-BY-FIN-RESP.
000048         16  RQ-COMPANY-CD-A2             PIC X.
000049         16  RQ-CARRIER-A2                PIC X.
000050         16  RQ-GROUPING-A2               PIC X(6).
000051         16  RQ-FIN-RESP-A2               PIC X(10).
000052         16  RQ-ACCT-AGENT-A2             PIC X(10).
000053         16  RQ-REFERENCE-A2              PIC X(12).
000054         16  RQ-BATCH-A2                  PIC X(6).
000055
000056     12  RQ-CONTROL-BY-ACCT-AGENT.
000057         16  RQ-COMPANY-CD-A3             PIC X.
000058         16  RQ-CARRIER-A3                PIC X.
000059         16  RQ-GROUPING-A3               PIC X(6).
000060         16  RQ-ACCT-AGENT-A3             PIC X(10).
000061         16  RQ-BATCH-A3                  PIC X(6).
000062
000063     12  RQ-CONTROL-BY-SUMMARY.
000064         16  RQ-COMPANY-CD-A4             PIC X.
000065         16  RQ-SUMMARY-CODE              PIC X(6).
000066         16  RQ-ACCOUNT-A4                PIC X(10).
000067         16  RQ-REFERENCE-A4              PIC X(12).
000068         16  RQ-BATCH-A4                  PIC X(6).
000069
000070     12  RQ-REQUEST-METHOD                PIC X.
000071         88 RQ-FIN-RESP-REQUEST               VALUE 'F'.
000072         88 RQ-ACCT-AGENT-REQUEST             VALUE 'A'.
000073         88 RQ-SUMMARY-REQUEST                VALUE 'S'.
000074         88 RQ-BATCH-REQUEST                  VALUE 'B'.
000075     12  FILLER                           PIC X.
000076     12  RQ-STATUS                        PIC X.
000077         88  RQ-REQUEST-ERROR                 VALUE 'E'.
000078         88  RQ-RESUBMIT                      VALUE 'R'.
000079     12  RQ-PROCESSOR-ID                  PIC X(4).
000080     12  RQ-ENTRY-DT                      PIC XX.
000081     12  RQ-MO-END-DT                     PIC XX.
000082     12  RQ-REQUEST-DT                    PIC XX.
000083     12  RQ-STMT-DT                       PIC XX.
000084     12  RQ-REVERSAL-DT                   PIC XX.
000085     12  RQ-CREDIT-SELECT-DT              PIC XX.
000086     12  RQ-CREDIT-ACCEPT-DT              PIC XX.
000087
000088     12  FILLER                           PIC X(27).
000089
000090******************************************************************
000091
      *<<((file: ERCRQST))
000415 EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL652' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000416 VCOBOL-DUMMY-PROCEDURE.
000417
000418     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
000419     MOVE '5'                    TO  DC-OPTION-CODE.
000420     PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
000421     MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
000422     MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
000423
000424     MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
000425     MOVE EIBTRMID               TO  QID-TERM.
000426
000427 1000-START.
000428     IF EIBCALEN = ZERO
000429         GO TO 8800-UNAUTHORIZED-ACCESS.
000430
000431     IF PI-RETURN-TO-PROGRAM = THIS-PGM
000432         MOVE PI-CALLING-PROGRAM        TO  RETURNED-FROM.
000433
000434     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000435         IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
000436             MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
000437             MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
000438             MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
000439             MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
000440             MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
000441             MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
000442             MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
000443             MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
000444             PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
000445         ELSE
000446             MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
000447             MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
000448             MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
000449             MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
000450             MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
000451             MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
000452             MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
000453             MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
000454
000455     IF  RETURNED-FROM = XCTL-633
000456             OR
000457         RETURNED-FROM = XCTL-633DMD
000458             OR
000459         RETURNED-FROM = XCTL-635
000460             OR
000461         RETURNED-FROM = XCTL-6521
000462             OR
000463         RETURNED-FROM = XCTL-6522
000464             OR
000465         RETURNED-FROM = XCTL-6523
000466             OR
000467         RETURNED-FROM = XCTL-6524
000468             OR
000469         RETURNED-FROM = XCTL-689
000470             OR
000471         RETURNED-FROM = XCTL-690
000472             OR
000473         RETURNED-FROM = XCTL-650
000474         GO TO 3475-RECOVER-TEMP-STOR-PI.
000475
000476     
      * EXEC CICS HANDLE CONDITION
000477*        NOTOPEN   (9990-ABEND)
000478*        NOTFND    (8880-NOT-FOUND)
000479*        PGMIDERR  (9600-PGMID-ERROR)
000480*        ERROR     (9990-ABEND)
000481*    END-EXEC.
      *    MOVE '"$JIL.                ! " #00004193' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' &
                X'202020202020202020202120' &
                X'2220233030303034313933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000482
000483     IF  EIBTRNID NOT = TRANS-ID
000484         MOVE LOW-VALUES         TO  EL652AI
000485         IF  EIBTRNID = EM6508-TRANS-ID
000486             MOVE DFHENTER       TO  EIBAID
000487             MOVE 'S'            TO  MAINTYPI
000488             MOVE PI-CARRIER     TO  CARRIERI
000489             MOVE PI-GROUPING    TO  GROUPI
000490             MOVE 'A'            TO  TYPEI
000491             MOVE PI-CR-FIN-RESP TO  FINRESPI
000492             MOVE PI-CR-ACCOUNT  TO  ACCTNOI
000493             MOVE 1              TO  CARRIERL TYPEL MAINTYPL
000494             MOVE 6              TO  GROUPL
000495             MOVE 10             TO  FINRESPL  ACCTNOL
000496             GO TO 4000-EDIT-MAINT
000497         ELSE
000498             IF  (EIBTRNID NOT = EL640-TRANS-ID AND
000499                                 EL633-TRANS-ID AND
000500                                 EL633DMD-TRANS-ID AND
000501                                 EL635-TRANS-ID AND
000502                                 EL642-TRANS-ID AND
000503                                 EL6501-TRANS-ID AND
000504                                 EL6592-TRANS-ID AND
000505                                 EL856-TRANS-ID)
000506                 OR PI-CR-FIN-RESP = SPACES
000507                 GO TO 8100-SEND-INITIAL-MAP
000508             ELSE
000509                 MOVE DFHENTER   TO  EIBAID
000510                 MOVE 'S'        TO  MAINTYPI
000511                 MOVE PI-CR-CARRIER  TO  CARRIERI
000512                 MOVE PI-CR-GROUPING TO  GROUPI
000513                 MOVE PI-CR-TYPE     TO  TYPEI
000514                 MOVE PI-CR-FIN-RESP TO  FINRESPI
000515                 MOVE PI-CR-ACCOUNT  TO  ACCTNOI
000516                 MOVE 1              TO  CARRIERL
000517                                         TYPEL
000518                                         MAINTYPL
000519                 MOVE 6              TO  GROUPL
000520                 MOVE 10             TO  FINRESPL
000521                                         ACCTNOL
000522                 GO TO 4000-EDIT-MAINT.
000523
000524     IF  EIBAID = DFHCLEAR
000525             OR
000526         NOT DISPLAY-CAP
000527         GO TO 9400-CLEAR.
000528
000529 EJECT
000530 2000-RECEIVE.
000531     MOVE LOW-VALUES             TO  EL652AI.
000532
000533     IF EIBAID = DFHPA1 OR  DFHPA2  OR  DFHPA3
000534         MOVE ER-0008            TO  EMI-ERROR
000535         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000536         MOVE -1                 TO  MAINTYPL
000537         GO TO 8200-SEND-DATAONLY.
000538
000539     
      * EXEC CICS RECEIVE
000540*        MAP     (MAP-NAME)
000541*        MAPSET  (MAPSET-NAME)
000542*        INTO    (EL652AI)
000543*    END-EXEC.
           MOVE LENGTH OF
            EL652AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004256' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034323536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000544
000545     IF PFKEYL = ZERO
000546         GO TO 3000-CHECK-PFKEYS.
000547
000548     IF EIBAID NOT = DFHENTER
000549         MOVE ER-0004            TO EMI-ERROR
000550         GO TO 3100-INPUT-ERROR.
000551
000552     IF (PFKEYI NUMERIC) AND (PFKEYI > 0 AND < 25)
000553         MOVE PF-VALUES (PFKEYI)  TO  EIBAID
000554     ELSE
000555         MOVE ER-0029             TO  EMI-ERROR
000556         GO TO 3100-INPUT-ERROR.
000557 EJECT
000558 3000-CHECK-PFKEYS.
000559     IF EIBAID = DFHPF23
000560         GO TO 8810-PF23.
000561
000562     IF EIBAID = DFHPF24
000563         GO TO 9200-RETURN-MAIN-MENU.
000564
000565     IF EIBAID = DFHPF12
000566         GO TO 9500-PF12.
000567
000568     if (maintypl not = zeros)
000569        and (maintypi not = 'D')
000570        move spaces              to pi-el652-del-sw
000571     end-if
000572
000573     IF EIBAID = DFHPF1
000574         IF PI-CHECK-MAINT-TYPE = 'C' OR 'D'
000575             MOVE 'S'            TO  MAINTYPO
000576             MOVE AL-UANON       TO  MAINTYPA
000577             MOVE 1              TO  MAINTYPL
000578             GO TO 7250-PAGE-FORWARD
000579         ELSE
000580             GO TO 7250-PAGE-FORWARD.
000581
000582     IF EIBAID = DFHPF2
000583         IF PI-CHECK-MAINT-TYPE = 'C' OR 'D'
000584             MOVE 'S'            TO  MAINTYPO
000585             MOVE AL-UANON       TO  MAINTYPA
000586             MOVE 1              TO  MAINTYPL
000587             GO TO 7300-PAGE-BACKWARD
000588         ELSE
000589             GO TO 7300-PAGE-BACKWARD.
000590
000591     IF EIBAID = DFHPF3
000592        IF PI-AR-PROCESSING
000593            IF PI-ERCOMP-KEY  NOT =       SPACES
000594                MOVE PI-ERC-CARRIER TO  PI-CR-CARRIER
000595                MOVE PI-ERC-GROUP   TO  PI-CR-GROUPING
000596                MOVE PI-ERC-RESP    TO  PI-CR-FIN-RESP
000597                MOVE PI-ERC-ACCT    TO  PI-CR-ACCOUNT
000598                MOVE PI-ERC-TYPE    TO  PI-CR-TYPE
000599                IF PI-CR-ACCOUNT =  LOW-VALUES
000600                   MOVE 'G'         TO  PI-CR-TYPE
000601                                        PI-ERC-TYPE
000602                   PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000603                   MOVE XCTL-635    TO PGM-NAME
000604                   GO TO 9300-XCTL
000605                ELSE
000606                   MOVE 'A'         TO  PI-CR-TYPE
000607                                        PI-ERC-TYPE
000608                   PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000609                   MOVE XCTL-635    TO PGM-NAME
000610                   GO TO 9300-XCTL
000611            ELSE
000612                MOVE ER-3021        TO  EMI-ERROR
000613                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000614                GO TO 8100-SEND-INITIAL-MAP
000615        ELSE
000616            IF PI-ERCOMP-KEY NOT = SPACES AND LOW-VALUES
000617                MOVE PI-ERC-CARRIER TO  PI-CR-CARRIER
000618                MOVE PI-ERC-GROUP   TO  PI-CR-GROUPING
000619                MOVE PI-ERC-RESP    TO  PI-CR-FIN-RESP
000620                MOVE PI-ERC-ACCT    TO  PI-CR-ACCOUNT
000621                MOVE PI-ERC-TYPE    TO  PI-CR-TYPE
000622                IF PI-CR-ACCOUNT =  LOW-VALUES
000623*                  MOVE 'G'         TO  PI-CR-TYPE
000624*                                       PI-ERC-TYPE
000625                   PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000626                   IF PI-COMPANY-ID = 'DMD'
000627                      MOVE XCTL-633DMD TO PGM-NAME
000628                      GO TO 9300-XCTL
000629                   ELSE
000630                      MOVE XCTL-633    TO PGM-NAME
000631                      GO TO 9300-XCTL
000632                ELSE
000633                   MOVE 'A'         TO  PI-CR-TYPE
000634                                        PI-ERC-TYPE
000635                   PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000636                   IF PI-COMPANY-ID = 'DMD'
000637                      MOVE XCTL-633DMD TO PGM-NAME
000638                      GO TO 9300-XCTL
000639                   ELSE
000640                      MOVE XCTL-633    TO PGM-NAME
000641                      GO TO 9300-XCTL
000642            ELSE
000643                MOVE ER-3021        TO  EMI-ERROR
000644                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000645                GO TO 8100-SEND-INITIAL-MAP.
000646
000647     IF EIBAID = DFHPF4
000648         IF PI-ERC-TYPE = 'G' OR 'A' OR 'B'
000649             PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000650             MOVE XCTL-6521      TO PGM-NAME
000651             GO TO 9300-XCTL
000652         ELSE
000653             MOVE ER-2872        TO  EMI-ERROR
000654             GO TO 3100-INPUT-ERROR.
000655
000656     IF  EIBAID = DFHPF5
000657         MOVE XCTL-689           TO PGM-NAME
000658         IF  PI-ERCOMP-KEY NOT = SPACES
000659             MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
000660             MOVE PI-ERC-GROUP   TO PI-CR-GROUPING
000661             MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP
000662             MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
000663             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
000664
000665             IF  PI-CR-ACCOUNT =  LOW-VALUES
000666*                MOVE 'G'        TO PI-CR-TYPE, PI-ERC-TYPE
000667                 PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000668                 MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA
000669                 GO TO 9300-XCTL
000670
000671             ELSE
000672                 MOVE 'A'        TO PI-CR-TYPE, PI-ERC-TYPE
000673                 PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000674                 MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA
000675                 GO TO 9300-XCTL
000676
000677         ELSE
000678             PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000679             MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA
000680             GO TO 9300-XCTL.
000681
000682     IF  EIBAID = DFHPF6
000683
000684         MOVE XCTL-690           TO PGM-NAME
000685
000686         IF  PI-ERCOMP-KEY NOT = SPACES
000687             MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
000688             MOVE PI-ERC-GROUP   TO PI-CR-GROUPING
000689             MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP
000690             MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
000691             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
000692
000693             IF  PI-CR-ACCOUNT =  LOW-VALUES
000694*                MOVE 'G'        TO PI-CR-TYPE, PI-ERC-TYPE
000695                 PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000696                 MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA
000697                 GO TO 9300-XCTL
000698
000699             ELSE
000700                 MOVE 'A'        TO PI-CR-TYPE, PI-ERC-TYPE
000701                 PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000702                 MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA
000703                 GO TO 9300-XCTL
000704
000705         ELSE
000706             PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
000707             MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA
000708             GO TO 9300-XCTL.
000709
000710     IF EIBAID = DFHPF7
000711         IF PI-ERC-TYPE = 'A'
000712            MOVE PI-ERC-CARRIER  TO PI-CR-CARRIER
000713            MOVE PI-ERC-GROUP    TO PI-CR-GROUPING
000714            MOVE PI-ERC-ACCT     TO PI-CR-ACCOUNT
000715*           PERFORM 3200-FIND-STATE
000716*                                THRU 3200-EXIT
000717            MOVE PI-SAVE-STATE   TO PI-CR-STATE
000718            PERFORM 3400-CREATE-TS-PI
000719                                 THRU 3400-EXIT
000720            MOVE XCTL-650        TO PGM-NAME
000721            GO TO 9300-XCTL
000722         ELSE
000723            MOVE ER-0035         TO EMI-ERROR
000724            GO TO 3100-INPUT-ERROR
000725         END-IF
000726     END-IF.
000727
000728     IF EIBAID = DFHPF8
000729         IF PI-ERCOMP-KEY NOT = SPACES
000730             MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
000731             MOVE PI-ERC-GROUP   TO PI-CR-GROUPING
000732             MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP
000733             MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
000734             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
000735             PERFORM 3400-CREATE-TS-PI  THRU 3400-EXIT
000736             MOVE XCTL-6522             TO PGM-NAME
000737             GO TO 9300-XCTL
000738         ELSE
000739            MOVE ER-0187                TO EMI-ERROR
000740            GO TO 3100-INPUT-ERROR
000741         END-IF
000742     END-IF.
000743
000744*****************************************************************
000745*  SPECIAL CODE - ENABLES DISPLAY OF PRIOR MONTH-END TOTALS*
000746*  PF9 AVAILABLE TO A/R USERS                                   *
000747*****************************************************************
000748
000749     IF PI-AR-PROCESSING OR PI-PROCESSOR-ID = 'LGXX'
000750         IF EIBAID = DFHPF9
000751             MOVE 'Y'            TO  WS-SHOW-SAVE-TOTALS
000752             GO TO 4000-EDIT-MAINT.
000753
000754     IF EIBAID = DFHPF10
000755        IF PI-ERC-TYPE = 'B'
000756         IF PI-ERCOMP-KEY NOT = SPACES
000757             MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
000758             MOVE PI-ERC-GROUP   TO PI-CR-GROUPING
000759             MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP
000760             MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
000761             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
000762             PERFORM 3400-CREATE-TS-PI  THRU 3400-EXIT
000763             MOVE XCTL-6523             TO PGM-NAME
000764             GO TO 9300-XCTL
000765         ELSE
000766            MOVE ER-0187                TO EMI-ERROR
000767            GO TO 3100-INPUT-ERROR
000768         END-IF
000769        ELSE
000770           MOVE ER-0035          TO EMI-ERROR
000771           GO TO 3100-INPUT-ERROR
000772        END-IF
000773     END-IF
000774
000775     IF EIBAID = DFHPF13
000776        IF PI-ERC-TYPE = 'B'
000777           IF PI-ERCOMP-KEY NOT = SPACES
000778              MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
000779              MOVE PI-ERC-GROUP  TO PI-CR-GROUPING
000780              MOVE PI-ERC-RESP   TO PI-CR-FIN-RESP
000781              MOVE PI-ERC-ACCT   TO PI-CR-ACCOUNT
000782              MOVE PI-ERC-TYPE   TO PI-CR-TYPE
000783              PERFORM 3400-CREATE-TS-PI
000784                                 THRU 3400-EXIT
000785              MOVE XCTL-6524     TO PGM-NAME
000786              GO TO 9300-XCTL
000787           ELSE
000788              MOVE ER-0187       TO EMI-ERROR
000789              GO TO 3100-INPUT-ERROR
000790           END-IF
000791        ELSE
000792           MOVE ER-0035          TO EMI-ERROR
000793           GO TO 3100-INPUT-ERROR
000794        END-IF
000795     END-IF
000796
000797     IF EIBAID = DFHENTER
000798         GO TO 4000-EDIT-MAINT.
000799
000800     MOVE ER-0029                TO  EMI-ERROR.
000801
000802 3100-INPUT-ERROR.
000803     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000804
000805     MOVE AL-UNBON               TO  PFKEYA.
000806     MOVE -1                     TO  PFKEYL.
000807
000808     GO TO 8200-SEND-DATAONLY.
000809 EJECT
000810 3200-FIND-STATE.
000811
000812     IF PI-SAVE-CITYST NOT = SPACES
000813        MOVE PI-SAVE-CITYST       TO WS-SRCH-STATE
000814     ELSE
000815        IF PI-SAVE-ADDR2 NOT = SPACES
000816           MOVE PI-SAVE-ADDR2     TO WS-SRCH-STATE
000817        END-IF
000818     END-IF
000819
000820     PERFORM VARYING STNDX FROM +29 BY -1 UNTIL
000821          (STNDX < +1) OR
000822          ((WS-SRCH-STATE (STNDX:2) ALPHABETIC) AND
000823          (WS-SRCH-STATE (STNDX:2) NOT = SPACES AND LOW-VALUES)
000824          AND
000825          (WS-SRCH-STATE (STNDX + 1:1) NOT = ' ' AND ',' AND
000826                   '.' AND LOW-VALUES) AND
000827          (WS-SRCH-STATE (STNDX:1) NOT = ' ' AND ',' AND
000828                   '.' AND LOW-VALUES))
000829     END-PERFORM
000830
000831     IF STNDX NOT < +1
000832        MOVE WS-SRCH-STATE (STNDX:2)
000833                                 TO PI-SAVE-STATE
000834     ELSE
000835        MOVE SPACES              TO PI-SAVE-STATE
000836     END-IF
000837
000838     .
000839 3200-EXIT.
000840     EXIT.
000841 3400-CREATE-TS-PI.
000842
000843     PERFORM 3450-DELETE-TEMP-STOR-PI THRU 3450-EXIT.
000844
000845     
      * EXEC CICS HANDLE CONDITION
000846*        QIDERR  (3400-EXIT)
000847*    END-EXEC.
      *    MOVE '"$N                   ! # #00004562' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303034353632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000848
000849     
      * EXEC CICS WRITEQ TS
000850*        QUEUE   (QID-PI)
000851*        FROM    (PROGRAM-INTERFACE-BLOCK)
000852*        LENGTH  (PI-COMM-LENGTH)
000853*        ITEM    (QID-ITEM)
000854*    END-EXEC.
      *    MOVE '*" I   L              ''   #00004566' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034353636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID-PI, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000855
000856 3400-EXIT.
000857      EXIT.
000858
000859 3450-DELETE-TEMP-STOR-PI.
000860
000861     
      * EXEC CICS HANDLE CONDITION
000862*        QIDERR  (3450-EXIT)
000863*    END-EXEC.
      *    MOVE '"$N                   ! $ #00004578' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303034353738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000864
000865     
      * EXEC CICS DELETEQ TS
000866*        QUEUE  (QID-PI)
000867*    END-EXEC.
      *    MOVE '*&                    #   #00004582' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303034353832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID-PI, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000868
000869     
      * EXEC CICS SYNCPOINT
000870*    END-EXEC.
      *    MOVE '6"                    !   #00004586' TO DFHEIV0
           MOVE X'362220202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303034353836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000871
000872 3450-EXIT.
000873      EXIT.
000874
000875 3475-RECOVER-TEMP-STOR-PI.
000876
000877     MOVE LOW-VALUES            TO  EL652AI.
000878
000879     
      * EXEC CICS HANDLE CONDITION
000880*        NOTOPEN   (9990-ABEND)
000881*        NOTFND    (8880-NOT-FOUND)
000882*        PGMIDERR  (9600-PGMID-ERROR)
000883*        ERROR     (9990-ABEND)
000884*    END-EXEC.
      *    MOVE '"$JIL.                ! % #00004596' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' &
                X'202020202020202020202120' &
                X'2520233030303034353936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000885
000886     
      * EXEC CICS HANDLE CONDITION
000887*        QIDERR  (3475-EXIT)
000888*    END-EXEC.
      *    MOVE '"$N                   ! & #00004603' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303034363033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000889
000890     
      * EXEC CICS READQ TS
000891*        QUEUE   (QID-PI)
000892*        ITEM    (1)
000893*        INTO    (PROGRAM-INTERFACE-BLOCK)
000894*        LENGTH  (PI-COMM-LENGTH)
000895*    END-EXEC.
           MOVE 1
             TO DFHEIV11
      *    MOVE '*$II   L              ''   #00004607' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303034363037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID-PI, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000896
000897     PERFORM 3450-DELETE-TEMP-STOR-PI THRU 3450-EXIT.
000898
000899     IF  PI-ERCOMP-KEY = SPACES OR LOW-VALUES
000900         GO TO 8100-SEND-INITIAL-MAP.
000901
000902     IF  PI-ERC-CARRIER > LOW-VALUES
000903         MOVE +1                 TO CARRIERL
000904         MOVE PI-ERC-CARRIER     TO CARRIERI.
000905
000906     IF  PI-ERC-GROUP > LOW-VALUES
000907         MOVE +6                 TO GROUPL
000908         MOVE PI-ERC-GROUP       TO GROUPI.
000909
000910     IF  PI-ERC-RESP > LOW-VALUES
000911         MOVE +10                TO FINRESPL
000912         MOVE PI-ERC-RESP        TO FINRESPI.
000913
000914     IF  PI-ERC-ACCT > LOW-VALUES
000915         MOVE +1                 TO ACCTNOL
000916         MOVE PI-ERC-ACCT        TO ACCTNOI.
000917
000918     IF  PI-ERC-TYPE > LOW-VALUES
000919         MOVE +1                 TO TYPEL
000920         MOVE PI-ERC-TYPE        TO TYPEI.
000921
000922     MOVE 'S'                    TO MAINTYPI.
000923     MOVE 1                      TO MAINTYPL.
000924     GO TO 4000-EDIT-MAINT.
000925
000926 3475-EXIT.
000927      EXIT.
000928 EJECT
000929 4000-EDIT-MAINT.
000930     IF MAINTYPL > ZERO
000931         MOVE MAINTYPI           TO  PI-CHECK-MAINT-TYPE
000932         IF VALID-MAINT-TYPE
000933             MOVE AL-UANON       TO  MAINTYPA
000934             IF MAINTYPI NOT = 'S'
000935                 MOVE SPACES     TO  WS-ACCESS
000936                 MOVE '1'        TO  CNTL-REC-TYPE
000937                 PERFORM 7400-READ-CONTROL-FILE  THRU  7499-EXIT
000938             ELSE
000939                 NEXT SENTENCE
000940         ELSE
000941             MOVE -1             TO  MAINTYPL
000942             MOVE AL-UABON       TO  MAINTYPA
000943             MOVE ER-2039        TO  EMI-ERROR
000944             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
000945     ELSE
000946         MOVE -1                 TO  MAINTYPL
000947         MOVE AL-UABON           TO  MAINTYPA
000948         MOVE ER-2039            TO  EMI-ERROR
000949         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
000950
000951     IF  NOT MODIFY-CAP
000952             AND
000953         NOT SHOW-FUNCTION
000954         MOVE 'UPDATE'           TO SM-READ
000955         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000956         IF  MORTGAGE-SESSION
000957             MOVE ER-9096        TO  EMI-ERROR
000958             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000959             GO TO 8100-SEND-INITIAL-MAP
000960         ELSE
000961             MOVE ER-0070        TO  EMI-ERROR
000962             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000963             GO TO 8100-SEND-INITIAL-MAP.
000964
000965     IF CARRIERL > ZERO
000966         IF PI-CARRIER-SECURITY > SPACES
000967             IF CARRIERI = PI-CARRIER-SECURITY
000968                 NEXT SENTENCE
000969             ELSE
000970                 MOVE -1            TO  CARRIERL
000971                 MOVE ER-2370       TO  EMI-ERROR
000972                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000973                 MOVE AL-UABON      TO  CARRIERA
000974                 GO TO 8200-SEND-DATAONLY.
000975
000976     IF CARRIERL > ZERO
000977         IF ADD-FUNCTION
000978             IF PI-ZERO-CARRIER
000979               OR PI-ZERO-CAR-GROUP
000980                 MOVE ZEROS      TO  PI-ERC-CARRIER
000981                                     CARRIERI
000982                 MOVE AL-UANON   TO  CARRIERA
000983             ELSE
000984                 MOVE CARRIERI   TO  WS-CARRIER
000985                 MOVE '6'        TO  CNTL-REC-TYPE
000986                 PERFORM 7400-READ-CONTROL-FILE  THRU  7499-EXIT
000987         ELSE
000988             IF PI-ZERO-CARRIER
000989               OR PI-ZERO-CAR-GROUP
000990                 MOVE ZEROS      TO  PI-ERC-CARRIER
000991                                     CARRIERI
000992                 MOVE AL-UANON   TO  CARRIERA
000993             ELSE
000994                 MOVE AL-UANON   TO  CARRIERA
000995                 MOVE CARRIERI   TO  PI-ERC-CARRIER
000996     ELSE
000997         IF ADD-FUNCTION
000998             IF PI-ZERO-CARRIER
000999               OR PI-ZERO-CAR-GROUP
001000                 MOVE ZEROS      TO  PI-ERC-CARRIER
001001                                     CARRIERI
001002                 MOVE AL-UANON   TO  CARRIERA
001003             ELSE
001004                 MOVE -1         TO  CARRIERL
001005                 MOVE AL-UABON   TO  CARRIERA
001006                 MOVE ER-0193    TO  EMI-ERROR
001007                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001008         ELSE
001009             MOVE -1             TO  CARRIERL
001010             MOVE AL-UABON       TO  CARRIERA
001011             MOVE ER-0193        TO  EMI-ERROR
001012             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001013
001014     IF GROUPL > ZERO
001015         IF PI-ZERO-GROUPING
001016           OR PI-ZERO-CAR-GROUP
001017             MOVE ZEROS          TO  PI-ERC-GROUP
001018                                     GROUPI
001019             MOVE AL-UANON       TO  GROUPA
001020         ELSE
001021             MOVE AL-UANON       TO  GROUPA
001022             MOVE GROUPI         TO  PI-ERC-GROUP
001023     ELSE
001024         IF ADD-FUNCTION
001025             IF PI-ZERO-GROUPING
001026               OR PI-ZERO-CAR-GROUP
001027                 MOVE ZEROS      TO  PI-ERC-GROUP
001028                                     GROUPI
001029                 MOVE AL-UANON   TO  GROUPA
001030             ELSE
001031                 MOVE LOW-VALUES  TO  PI-ERC-GROUP
001032         ELSE
001033             MOVE LOW-VALUES     TO  PI-ERC-GROUP.
001034
001035     IF TYPEL > ZERO
001036         MOVE TYPEI              TO  PI-CHECK-TYPE
001037         IF VALID-TYPE
001038             MOVE AL-UANON       TO  TYPEA
001039             MOVE TYPEI          TO  PI-ERC-TYPE
001040         ELSE
001041             MOVE -1             TO  TYPEL
001042             MOVE AL-UABON       TO  TYPEA
001043             MOVE ER-2042        TO  EMI-ERROR
001044             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001045     ELSE
001046         MOVE -1                 TO  TYPEL
001047         MOVE AL-UABON           TO  TYPEA
001048         MOVE ER-2042            TO  EMI-ERROR
001049         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001050
001051     IF FINRESPL >        ZERO
001052         MOVE AL-UANON           TO  FINRESPA
001053         IF FINRESPI = SPACES
001054             MOVE LOW-VALUES     TO  PI-ERC-RESP
001055         ELSE
001056             MOVE FINRESPI       TO  PI-ERC-RESP
001057     ELSE
001058         MOVE LOW-VALUES         TO  PI-ERC-RESP.
001059
001060     IF ACCTNOL > ZERO
001061         MOVE AL-UANON           TO  ACCTNOA
001062         IF ACCTNOI = SPACES
001063             MOVE LOW-VALUES     TO  PI-ERC-ACCT
001064         ELSE
001065             MOVE ACCTNOI        TO  PI-ERC-ACCT
001066     ELSE
001067         MOVE LOW-VALUES         TO  PI-ERC-ACCT.
001068
001069     IF ADDR2L > ZERO
001070        MOVE AL-UANON            TO ADDR2A
001071        IF ADDR2I = SPACES
001072           MOVE LOW-VALUES       TO PI-SAVE-ADDR2
001073        ELSE
001074           MOVE ADDR2I           TO PI-SAVE-ADDR2
001075        END-IF
001076     ELSE
001077        MOVE LOW-VALUES          TO PI-SAVE-ADDR2
001078     END-IF
001079
001080     IF CITYL > ZERO
001081        MOVE AL-UANON            TO CITYA
001082        IF CITYI = SPACES
001083           MOVE LOW-VALUES       TO PI-SAVE-CITY
001084        ELSE
001085           MOVE CITYI            TO PI-SAVE-CITY
001086        END-IF
001087     ELSE
001088        MOVE LOW-VALUES          TO PI-SAVE-CITY
001089     END-IF
001090
001091     IF STATEL > ZERO
001092        MOVE AL-UANON            TO STATEA
001093        IF STATEI = SPACES
001094           MOVE LOW-VALUES       TO PI-SAVE-STATE
001095        ELSE
001096           MOVE STATEI           TO PI-SAVE-STATE
001097        END-IF
001098     ELSE
001099        MOVE LOW-VALUES          TO PI-SAVE-STATE
001100     END-IF
001101
001102     IF NOT MODIFY-CAP
001103         IF SHOW-FUNCTION
001104             GO TO 5000-BUILD-INITIAL-SCREEN
001105         ELSE
001106             MOVE 'UPDATE'       TO SM-READ
001107             PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
001108             MOVE ER-0070        TO  EMI-ERROR
001109             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001110             GO TO 8100-SEND-INITIAL-MAP.
001111
001112     IF EMI-NO-ERRORS
001113         NEXT SENTENCE
001114     ELSE
001115        IF EIBTRNID NOT = TRANS-ID
001116            GO TO 8100-SEND-INITIAL-MAP
001117         ELSE
001118            GO TO 8200-SEND-DATAONLY.
001119
001120     IF CHANGE-FUNCTION
001121         GO TO 4400-CHANGE.
001122
001123     IF DELETE-FUNCTION
001124         GO TO 4600-DELETE.
001125
001126     IF SHOW-FUNCTION
001127         GO TO 5000-BUILD-INITIAL-SCREEN.
001128
001129     IF TYPEI = 'C'
001130         IF FINRESPI NOT = LOW-VALUES OR
001131            ACCTNOI  NOT = LOW-VALUES
001132               MOVE -1           TO  FINRESPL
001133               MOVE AL-UABON     TO  FINRESPA
001134                                     ACCTNOA
001135               MOVE ER-2088      TO  EMI-ERROR
001136               PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001137
001138     IF TYPEI = 'A'
001139         IF FINRESPI = LOW-VALUES OR
001140             ACCTNOI = LOW-VALUES
001141               MOVE -1           TO  FINRESPL
001142               MOVE AL-UABON     TO  FINRESPA
001143                                     ACCTNOA
001144               MOVE ER-2089      TO  EMI-ERROR
001145               PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001146
001147     IF TYPEI = 'G' OR 'B'
001148         IF ACCTNOI NOT = LOW-VALUES
001149             MOVE -1             TO  ACCTNOL
001150             MOVE AL-UABON       TO  ACCTNOA
001151             MOVE ER-2091        TO  EMI-ERROR
001152             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001153         ELSE
001154             IF FINRESPI = LOW-VALUES
001155                 MOVE -1         TO  FINRESPL
001156                 MOVE AL-UABON   TO  FINRESPA
001157                 MOVE ER-2097    TO  EMI-ERROR
001158                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001159
001160     IF TYPEI = 'G' OR 'B'
001161         IF SUMMNOI NOT = LOW-VALUES
001162             MOVE -1             TO  SUMMNOL
001163             MOVE AL-UABON       TO  SUMMNOA
001164             MOVE ER-3153        TO  EMI-ERROR
001165             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001166
001167     IF TYPEI = 'G' OR 'B'
001168         IF NETGRSI NOT = LOW-VALUES
001169             MOVE -1             TO  NETGRSL
001170             MOVE AL-UABON       TO  NETGRSA
001171             MOVE ER-3154        TO  EMI-ERROR
001172             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001173
001174     IF EMI-NO-ERRORS
001175         NEXT SENTENCE
001176     ELSE
001177         GO TO 8200-SEND-DATAONLY.
001178
001179     IF ADD-FUNCTION
001180         GO TO 4200-ADD.
001181
001182     MOVE -1                     TO  MAINTYPL.
001183     MOVE ER-2056                TO  EMI-ERROR.
001184     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001185
001186     GO TO 8200-SEND-DATAONLY.
001187
001188 4000-EXIT.
001189     EXIT.
001190 EJECT
001191 4200-ADD.
001192     IF PI-ERCOMP-KEY = PI-SAVE-ERCOMP-KEY
001193         MOVE ER-2057            TO  EMI-ERROR
001194         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001195         MOVE -1                 TO  MAINTYPL
001196         GO TO 8200-SEND-DATAONLY.
001197
001198     PERFORM 7000-EDIT  THRU  7000-EXIT.
001199
001200     IF EMI-NO-ERRORS
001201         NEXT SENTENCE
001202     ELSE
001203         GO TO 8200-SEND-DATAONLY.
001204
001205     
      * EXEC CICS HANDLE CONDITION
001206*        NOTOPEN  (9990-ABEND)
001207*        NOTFND   (4250-CONT)
001208*    END-EXEC.
      *    MOVE '"$JI                  ! '' #00004922' TO DFHEIV0
           MOVE X'22244A492020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303034393232' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001209
001210     PERFORM 7050-READ-ERCOMP  THRU  7050-EXIT.
001211
001212     MOVE ER-2057                TO  EMI-ERROR.
001213     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001214
001215     MOVE LOW-VALUES             TO  PI-SAVE-ERCOMP-KEY.
001216
001217     MOVE -1                     TO  MAINTYPL.
001218
001219     GO TO 8200-SEND-DATAONLY.
001220
001221 4250-CONT.
001222     PERFORM 7150-ERCOMP-GETMAIN  THRU  7150-EXIT.
001223
001224     MOVE SPACES                 TO  COMPENSATION-MASTER.
001225     MOVE ZEROS                  TO  CO-LF-CLM-AMT
001226                                     CO-AH-CLM-AMT
001227                                     CO-CUR-FICA
001228                                     CO-YTD-FICA
001229                                     CO-CUR-OVR-UNDR
001230                                     CO-YTD-OVR-UNDR
001231                                     CO-MAX-BANK-FEE-LEASE
001232                                     CO-MAX-BANK-FEE
001233                                     CO-BAL-FWD
001234                                     CO-CUR-COM
001235                                     CO-CUR-CHG
001236                                     CO-CUR-PMT
001237                                     CO-END-BAL
001238                                     CO-CUR
001239                                     CO-OV30
001240                                     CO-OV60
001241                                     CO-OV90
001242                                     co-ov120
001243                                     CO-YTD-COM
001244                                     CO-YTD-OV
001245                                     CO-YTD-PAID-COM
001246                                     CO-YTD-PAID-OV
001247                                     CO-CURRENT-BAL-FWD
001248                                     CO-CURRENT-CUR-COM
001249                                     CO-CURRENT-CUR-CHG
001250                                     CO-CURRENT-CUR-PMT
001251                                     CO-CURRENT-END-BAL
001252                                     CO-CURRENT-CUR
001253                                     CO-CURRENT-OV30
001254                                     CO-CURRENT-OV60
001255                                     CO-CURRENT-OV90
001256                                     co-current-ov120
001257                                     CO-CURRENT-YTD-COM
001258                                     CO-CURRENT-YTD-OV
001259                                     CO-ACT-YEAR
001260                                     CO-ACT-MONTH
001261                                     CO-ACT-DAY.
001262     MOVE LOW-VALUES             TO  CO-LAST-ACTIVITY-DATE
001263                                     CO-LAST-STMT-DT
001264                                     CO-CURRENT-LAST-STMT-DT
001265                                     CO-GA-EFFECTIVE-DT
001266                                     CO-GA-TERMINATION-DT.
001267     MOVE LOW-VALUES             TO  CO-FIRST-WRITTEN-DT.
001268
001269     MOVE 'N'                    TO  CO-INTERNAL-CONTROL-1
001270                                     CO-INTERNAL-CONTROL-2.
001271
001272     PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.
001273
001274     MOVE PI-PROCESSOR-ID        TO  CO-LAST-MAINT-USER.
001275     MOVE EIBTIME                TO  CO-LAST-MAINT-HHMMSS.
001276     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
001277     MOVE '5'                    TO  DC-OPTION-CODE.
001278     MOVE LINK-ELDATCV           TO  PGM-NAME.
001279
001280     
      * EXEC CICS LINK
001281*        PROGRAM   (PGM-NAME)
001282*        COMMAREA  (DATE-CONVERSION-DATA)
001283*        LENGTH    (DC-COMM-LENGTH)
001284*    END-EXEC.
      *    MOVE '."C                   (   #00004997' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303034393937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001285
001286     MOVE DC-BIN-DATE-1          TO  CO-LAST-MAINT-DT
001287                                     BIN-CURRENT-SAVE.
001288     MOVE PI-CR-MONTH-END-DT     TO  CO-ROLADEX-PRINT-DT.
001289     MOVE PI-CR-MONTH-END-DT     TO  DC-BIN-DATE-1
001290     MOVE SPACE                  TO  DC-OPTION-CODE.
001291
001292     
      * EXEC CICS LINK
001293*        PROGRAM   (PGM-NAME)
001294*        COMMAREA  (DATE-CONVERSION-DATA)
001295*        LENGTH    (DC-COMM-LENGTH)
001296*    END-EXEC.
      *    MOVE '."C                   (   #00005009' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035303039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001297
001298     MOVE PI-COMPANY-CD          TO  CO-COMPANY-CD.
001299     MOVE 'CO'                   TO  CO-RECORD-ID.
001300     MOVE 'A'                    TO  JP-RECORD-TYPE.
001301     MOVE COMP-FILE-ID           TO  FILE-ID.
001302     MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.
001303
001304     PERFORM 4300-ADD-COMPENSATION-NAME  THRU  4300-EXIT.
001305
001306     
      * EXEC CICS WRITE
001307*        DATASET  (COMP-FILE-ID)
001308*        FROM     (COMPENSATION-MASTER)
001309*        RIDFLD   (CO-CONTROL-PRIMARY)
001310*    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005023' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035303233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001311
001312     PERFORM 8400-LOG-JOURNAL-RECORD.
001313
001314     PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.
001315
001316     IF PI-AR-PROCESSING
001317         IF SUMMNOL > ZERO
001318             MOVE SUMMNOI        TO PI-AR-SUMMARY-CODE
001319             PERFORM 7500-UPDATE-SUMM  THRU  7599-EXIT
001320             MOVE PI-AR-SUMMARY-CODE
001321                                 TO  WS-SUMM-FOR-RQST
001322             PERFORM 6500-UPDATE-RQST  THRU  6599-EXIT.
001323
001324     MOVE ER-0000                TO  EMI-ERROR.
001325     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001326
001327     MOVE LOW-VALUES             TO  EL652AO.
001328     MOVE PI-ERC-CARRIER         TO  CARRIERO.
001329     MOVE PI-ERC-TYPE            TO  TYPEO.
001330     MOVE AL-UANON               TO  CARRIERA
001331                                     TYPEA.
001332
001333     IF PI-ERC-GROUP NOT = SPACES
001334         MOVE PI-ERC-GROUP       TO  GROUPO
001335         MOVE AL-UANON           TO  GROUPA.
001336
001337     IF PI-ERC-RESP NOT = SPACES
001338         MOVE PI-ERC-RESP        TO  FINRESPO
001339         MOVE AL-UANON           TO  FINRESPA.
001340
001341     IF PI-ERC-ACCT NOT = SPACES
001342         MOVE PI-ERC-ACCT        TO  ACCTNOO
001343         MOVE AL-UANON           TO  ACCTNOA.
001344
001345     GO TO 5000-BUILD-INITIAL-SCREEN.
001346
001347 4200-EXIT.
001348     EXIT.
001349 EJECT
001350 4300-ADD-COMPENSATION-NAME.
001351     
      * EXEC CICS HANDLE CONDITION
001352*         DUPREC   (4300-EXIT)
001353*    END-EXEC.
      *    MOVE '"$%                   ! ( #00005068' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303035303638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001354
001355     
      * EXEC CICS GETMAIN
001356*         LENGTH   (ERNAME-LENGTH)
001357*         SET      (ADDRESS OF NAME-LOOKUP-MASTER)
001358*         INITIMG  (GETMAIN-SPACE)
001359*    END-EXEC.
      *    MOVE ',"IL                  $   #00005072' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303035303732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERNAME-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001360
001361     MOVE SPACES                 TO  NAME-LOOKUP-MASTER.
001362     MOVE BIN-CURRENT-SAVE       TO  NL-LAST-MAINT-DT.
001363     MOVE PI-PROCESSOR-ID        TO  NL-LAST-MAINT-USER.
001364     MOVE EIBTIME                TO  NL-LAST-MAINT-HHMMSS.
001365     MOVE PI-COMPANY-CD          TO  NL-COMPANY-CD.
001366     MOVE CO-ACCT-NAME           TO  NL-NAME.
001367     IF NL-NAME (1:4) = 'THE ' OR 'The ' OR 'the '
001368        MOVE NL-NAME (5:26)      TO NL-NAME
001369     END-IF
001370     MOVE CO-ADDR-CITY           TO  NL-CITY.
001371     MOVE CO-ADDR-STATE          TO  NL-ST
001372     MOVE 'C'                    TO  NL-RECORD-TYPE.
001373     MOVE PI-COMPANY-CD          TO  NL-CO-COMPANY-CD.
001374     MOVE CO-CARRIER             TO  NL-CO-CARRIER.
001375     MOVE CO-GROUPING            TO  NL-CO-GROUPING.
001376     MOVE CO-RESP-NO             TO  NL-CO-RESP-NO.
001377     MOVE CO-ACCOUNT             TO  NL-CO-ACCOUNT.
001378     MOVE CO-TYPE                TO  NL-CO-TYPE.
001379
001380     
      * EXEC CICS WRITE
001381*        FROM      (NAME-LOOKUP-MASTER)
001382*        RIDFLD    (NL-CONTROL-PRIMARY)
001383*        DATASET   (NAME-FILE-ID)
001384*    END-EXEC.
           MOVE LENGTH OF
            NAME-LOOKUP-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005097' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303035303937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NAME-FILE-ID, 
                 NAME-LOOKUP-MASTER, 
                 DFHEIV11, 
                 NL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001385
001386 4300-EXIT.
001387     EXIT.
001388 EJECT
001389 4400-CHANGE.
001390     IF PI-ERCOMP-KEY = PI-SAVE-ERCOMP-KEY
001391         NEXT SENTENCE
001392     ELSE
001393**** MUST SHOW RECORD FIRST
001394         MOVE ER-2056            TO  EMI-ERROR
001395         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001396         MOVE -1                 TO  MAINTYPL
001397         GO TO 8200-SEND-DATAONLY.
001398
001399     PERFORM 7000-EDIT  THRU  7000-EXIT.
001400
001401     IF EMI-NO-ERRORS
001402         NEXT SENTENCE
001403     ELSE
001404         GO TO 8200-SEND-DATAONLY.
001405
001406     PERFORM 7200-READ-ERCOMP-UPDATE  THRU  7200-EXIT.
001407
001408     MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.
001409     MOVE CO-AR-SUMMARY-CODE     TO  WS-SAVE-SUMM.
001410
001411     PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.
001412
001413     IF (CO-LAST-MAINT-USER   = PI-UPDATE-BY)
001414        OR (CO-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
001415        CONTINUE
001416     ELSE
001417        
      * EXEC CICS UNLOCK
001418*            DATASET  (COMP-FILE-ID)
001419*       END-EXEC
      *    MOVE '&*                    #   #00005134' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035313334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001420        MOVE ER-0068             TO EMI-ERROR
001421        PERFORM 9900-ERROR-FORMAT
001422                                 THRU 9900-EXIT
001423        GO TO 8200-SEND-DATAONLY
001424     END-IF
001425
001426     MOVE PI-PROCESSOR-ID        TO  CO-LAST-MAINT-USER.
001427     MOVE EIBTIME                TO  CO-LAST-MAINT-HHMMSS.
001428     MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
001429     MOVE '5'                    TO  DC-OPTION-CODE.
001430     MOVE LINK-ELDATCV           TO  PGM-NAME.
001431
001432     
      * EXEC CICS LINK
001433*        PROGRAM   (PGM-NAME)
001434*        COMMAREA  (DATE-CONVERSION-DATA)
001435*        LENGTH    (DC-COMM-LENGTH)
001436*    END-EXEC.
      *    MOVE '."C                   (   #00005149' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035313439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001437
001438     MOVE DC-BIN-DATE-1          TO  CO-LAST-MAINT-DT
001439                                     BIN-CURRENT-SAVE.
001440     MOVE SPACE                  TO  DC-OPTION-CODE.
001441     MOVE PI-CR-MONTH-END-DT     TO  CO-ROLADEX-PRINT-DT.
001442     MOVE PI-CR-MONTH-END-DT     TO  DC-BIN-DATE-1
001443
001444     
      * EXEC CICS LINK
001445*        PROGRAM   (PGM-NAME)
001446*        COMMAREA  (DATE-CONVERSION-DATA)
001447*        LENGTH    (DC-COMM-LENGTH)
001448*    END-EXEC.
      *    MOVE '."C                   (   #00005161' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035313631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001449
001450     MOVE 'N'                    TO  CO-INTERNAL-CONTROL-1.
001451     MOVE 'B'                    TO  JP-RECORD-TYPE.
001452     MOVE COMP-FILE-ID           TO  FILE-ID.
001453
001454     PERFORM 8400-LOG-JOURNAL-RECORD.
001455
001456     MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.
001457
001458     PERFORM 4300-ADD-COMPENSATION-NAME  THRU  4300-EXIT.
001459
001460     
      * EXEC CICS REWRITE
001461*        DATASET  (COMP-FILE-ID)
001462*        FROM     (COMPENSATION-MASTER)
001463*    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005177' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303035313737' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001464
001465     MOVE 'C'                    TO  JP-RECORD-TYPE.
001466
001467     PERFORM 8400-LOG-JOURNAL-RECORD.
001468
001469     IF NOT PI-AR-PROCESSING
001470         GO TO 4400-CONT.
001471
001472     IF SUMMNOL NOT > ZERO
001473         GO TO 4400-CONT.
001474
001475     IF SUMMNOI  = WS-SAVE-SUMM
001476         GO TO 4400-CONT
001477     ELSE
001478         PERFORM 7600-DELETE-SUMM  THRU  7699-EXIT.
001479
001480     MOVE SUMMNOI                 TO WS-SUMM-FOR-RQST.
001481     PERFORM 6500-UPDATE-RQST  THRU  6599-EXIT.
001482
001483     IF SUMMNOI = SPACES
001484         GO TO 4400-CONT.
001485
001486     MOVE SUMMNOI                 TO PI-AR-SUMMARY-CODE.
001487     PERFORM 7500-UPDATE-SUMM  THRU  7599-EXIT.
001488
001489 4400-CONT.
001490     PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.
001491
001492     MOVE ER-0000                TO  EMI-ERROR.
001493     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001494
001495     MOVE LOW-VALUES             TO  EL652AO.
001496     MOVE PI-ERC-CARRIER         TO  CARRIERO.
001497     MOVE PI-ERC-TYPE            TO  TYPEO.
001498     MOVE AL-UANON               TO  CARRIERA
001499                                     TYPEA.
001500
001501     IF PI-ERC-GROUP NOT = SPACES
001502         MOVE PI-ERC-GROUP       TO  GROUPO
001503         MOVE AL-UANON           TO  GROUPA.
001504
001505     IF PI-ERC-RESP NOT = SPACES
001506         MOVE PI-ERC-RESP        TO  FINRESPO
001507         MOVE AL-UANON           TO  FINRESPA.
001508
001509     IF PI-ERC-ACCT NOT = SPACES
001510         MOVE PI-ERC-ACCT        TO  ACCTNOO
001511         MOVE AL-UANON           TO  ACCTNOA.
001512
001513     GO TO 5000-BUILD-INITIAL-SCREEN.
001514
001515 4400-EXIT.
001516     EXIT.
001517 EJECT
001518 4600-DELETE.
001519     IF PI-ERCOMP-KEY = PI-SAVE-ERCOMP-KEY
001520         NEXT SENTENCE
001521     ELSE
001522         MOVE ER-2056            TO  EMI-ERROR
001523         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001524         MOVE -1                 TO  MAINTYPL
001525         GO TO 8200-SEND-DATAONLY.
001526
001527     PERFORM 7200-READ-ERCOMP-UPDATE  THRU  7200-EXIT.
001528
001529     MOVE CO-AR-SUMMARY-CODE     TO  WS-SAVE-SUMM.
001530
001531     IF (CO-YTD-COM NOT = ZERO) OR
001532        (CO-END-BAL NOT = ZERO)
001533         
      * EXEC CICS UNLOCK
001534*            DATASET  (COMP-FILE-ID)
001535*        END-EXEC
      *    MOVE '&*                    #   #00005250' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035323530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001536         MOVE ER-2092            TO  EMI-ERROR
001537         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001538         MOVE -1                 TO  MAINTYPL
001539         GO TO 8200-SEND-DATAONLY.
001540
001541     MOVE 'D'                    TO  JP-RECORD-TYPE.
001542     MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.
001543     MOVE COMP-FILE-ID           TO  FILE-ID.
001544
001545     IF CO-LAST-MAINT-USER   = PI-UPDATE-BY OR
001546        CO-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
001547         continue
001548     ELSE
001549         
      * EXEC CICS UNLOCK
001550*            DATASET  (COMP-FILE-ID)
001551*        END-EXEC
      *    MOVE '&*                    #   #00005266' TO DFHEIV0
           MOVE X'262A20202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303035323636' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001552         MOVE ER-0068            TO  EMI-ERROR
001553         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001554         GO TO 8200-SEND-DATAONLY
001555     end-if
001556
001557     if pi-el652-del-sw = 'Y'
001558        continue
001559     else
001560        move 'Y'                 to pi-el652-del-sw
001561        move er-1299             to emi-error
001562        perform 9900-error-format thru 9900-exit
001563        move spaces              to maintypo
001564        MOVE -1                  TO MAINTypL
001565        GO TO 8200-send-dataonly
001566     end-if
001567
001568     
      * EXEC CICS DELETE
001569*        DATASET  (COMP-FILE-ID)
001570*    END-EXEC
      *    MOVE '&(                    &   #00005285' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303035323835' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 NL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001571
001572     PERFORM 8400-LOG-JOURNAL-RECORD
001573
001574     MOVE EIBDATE                TO  DC-JULIAN-YYDDD
001575     MOVE '5'                    TO  DC-OPTION-CODE
001576     MOVE LINK-ELDATCV           TO  PGM-NAME
001577
001578     
      * EXEC CICS LINK
001579*        PROGRAM   (PGM-NAME)
001580*        COMMAREA  (DATE-CONVERSION-DATA)
001581*        LENGTH    (DC-COMM-LENGTH)
001582*    END-EXEC
      *    MOVE '."C                   (   #00005295' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303035323935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001583
001584     MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE
001585
001586     PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT
001587
001588     MOVE ER-0000                TO  EMI-ERROR
001589     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
001590
001591     IF PI-AR-PROCESSING
001592         IF WS-SAVE-SUMM NOT = SPACES
001593             PERFORM 7600-DELETE-SUMM  THRU  7699-EXIT
001594             MOVE SPACES         TO  WS-SUMM-FOR-RQST
001595             PERFORM  6500-UPDATE-RQST  THRU  6599-EXIT
001596         end-if
001597     end-if
001598
001599     MOVE LOW-VALUES             TO  EL652AO
001600     MOVE PI-ERC-CARRIER         TO  CARRIERO
001601     MOVE PI-ERC-TYPE            TO  TYPEO
001602     MOVE AL-UANON               TO  CARRIERA
001603                                     TYPEA
001604
001605     IF PI-ERC-GROUP NOT = SPACES
001606         MOVE PI-ERC-GROUP       TO  GROUPO
001607         MOVE AL-UANON           TO  GROUPA
001608     end-if
001609
001610     IF PI-ERC-RESP NOT = SPACES
001611         MOVE PI-ERC-RESP        TO  FINRESPO
001612         MOVE AL-UANON           TO  FINRESPA
001613     end-if
001614
001615     IF PI-ERC-ACCT NOT = SPACES
001616         MOVE PI-ERC-ACCT        TO  ACCTNOO
001617         MOVE AL-UANON           TO  ACCTNOA
001618     end-if
001619
001620     MOVE LOW-VALUES             TO  PI-SAVE-ERCOMP-KEY
001621
001622     GO TO 8100-SEND-INITIAL-MAP
001623     .
001624 4600-EXIT.
001625     EXIT.
001626 EJECT
001627
001628 4700-CHECK-STATE.
001629     IF CLPSTI =  SPACES
001630         GO TO 4799-EXIT
001631     END-IF.
001632
001633     MOVE SPACES                 TO  ELCNTL-KEY2.
001634     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID-2.
001635     MOVE '3'                    TO  CNTL-REC-TYPE-2.
001636     MOVE CLPSTI                 TO  CNTL-STATE-2.
001637     MOVE +0                     TO  CNTL-SEQ-NO-2.
001638
001639     
      * EXEC CICS  HANDLE CONDITION
001640*        NOTFND  (4750-NO-STATE)
001641*    END-EXEC.
      *    MOVE '"$I                   ! ) #00005356' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303035333536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001642
001643     
      * EXEC CICS  READ
001644*        DATASET  (CNTL-FILE-ID)
001645*        SET      (ADDRESS OF CONTROL-FILE)
001646*        RIDFLD   (ELCNTL-KEY2)
001647*    END-EXEC.
      *    MOVE '&"S        E          (   #00005360' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303035333630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001648
001649     GO TO 4799-EXIT.
001650
001651 4750-NO-STATE.
001652     MOVE ER-0144                TO  EMI-ERROR.
001653     MOVE -1                     TO  CLPSTL.
001654     MOVE AL-UABON               TO  CLPSTA.
001655
001656     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
001657
001658 4799-EXIT.
001659     EXIT.
001660
001661 5000-BUILD-INITIAL-SCREEN.
001662     MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
001663
001664     PERFORM 7050-READ-ERCOMP  THRU  7050-EXIT.
001665
001666     MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.
001667     MOVE CO-LAST-MAINT-USER     TO  PI-UPDATE-BY.
001668
001669     IF CO-LAST-MAINT-HHMMSS NUMERIC
001670        MOVE CO-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS
001671        ELSE
001672        MOVE ZEROS                  TO  PI-UPDATE-HHMMSS.
001673
001674 5050-SET-UP-SCREEN.
001675     MOVE CO-CONTROL-NAME        TO  PCONTO.
001676     MOVE CO-MAIL-NAME           TO  MAILNAMO.
001677     MOVE CO-ACCT-NAME           TO  ACCTNAMO.
001678     MOVE CO-ACCT-NAME           TO  PI-SAVE-ACCT-NAME.
001679     MOVE CO-ADDR-1              TO  ADDR1O.
001680     MOVE CO-ADDR-2              TO  ADDR2O
001681                                     PI-SAVE-ADDR2
001682     MOVE CO-ADDR-CITY           TO  CITYO
001683                                     PI-SAVE-CITY
001684     MOVE CO-ADDR-STATE          TO  STATEO
001685                                     PI-SAVE-STATE
001686     MOVE SPACES                 TO  WS-ZIP-CODE.
001687
001688     IF CO-CANADIAN-POST-CODE
001689         MOVE CO-CAN-POSTAL-1    TO  WS-ZIP-CAN-2-POST1
001690         MOVE CO-CAN-POSTAL-2    TO  WS-ZIP-CAN-2-POST2
001691     ELSE
001692         MOVE CO-ZIP-PRIME       TO  WS-ZIP-AM-2-CODE
001693         IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
001694             MOVE '-'            TO  WS-ZIP-AM-2-DASH
001695             MOVE CO-ZIP-PLUS4   TO  WS-ZIP-AM-2-PLUS4.
001696
001697     MOVE WS-ZIP-CODE            TO  ZIPCODEO.
001698
001699     IF CO-BILL-SW = ' ' OR 'B' OR 'R' OR 'T' OR 'S'
001700        OR 'O' OR 'E' OR 'C'
001701         MOVE AL-UANON           TO  BILLPRTA
001702         MOVE CO-BILL-SW         TO  BILLPRTO
001703     END-IF.
001704
001705     MOVE CO-CSO-1099            TO  PNT1099O.
001706
001707     MOVE CO-CSR-CODE            TO  CSRO.
001708
001709     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
001710        AND (CO-TYPE = 'A')
001711        IF CO-COMP-TYPE = '1'
001712           MOVE 'Y'              TO SPPDDO
001713        ELSE
001714           MOVE 'N'              TO SPPDDO
001715        END-IF
001716     END-IF
001717     MOVE CO-SOC-SEC             TO  SSNO.
001718
001719     MOVE CO-BALANCE-CONTROL     TO  CARBALO.
001720
001721     IF PI-AR-PROCESSING
001722         IF CO-AR-SUMMARY-CODE > SPACES
001723             MOVE AL-UANON       TO  SUMMNOA
001724             MOVE CO-AR-SUMMARY-CODE
001725                                 TO  SUMMNOO
001726         ELSE
001727             MOVE LOW-VALUES     TO  SUMMNOO.
001728
001729     IF PI-AR-PROCESSING
001730        IF NOT CO-ACCOUNT-TYPE
001731           MOVE AL-SADOF         TO SCDESCA
001732           MOVE AL-SANOF         TO SUMMNOA
001733        ELSE
001734           MOVE AL-UANOF         TO SUMMNOA.
001735
001736     IF PI-AR-PROCESSING
001737*        MOVE CO-AR-BAL-LEVEL    TO  ARBALO
001738         MOVE CO-AR-PULL-CHECK   TO  CKPULLO.
001739
001740     IF PI-AR-PROCESSING
001741         IF CO-ACCOUNT-TYPE
001742             MOVE CO-AR-REPORTING
001743                                 TO  NETGRSO
001744         ELSE
001745             MOVE SPACES         TO  NETGRSO.
001746
001747     MOVE SPACES                 TO  FLITYPO.
001748
001749     IF CO-COMPANY-TYPE
001750         MOVE SPACES             TO  RPTCD2O
001751         MOVE AL-SADOF           TO  RPTCDDA.
001752
001753     MOVE SPACES                 TO  LETRCDO.
001754     MOVE AL-SADOF               TO  LETDESCA.
001755     MOVE SPACES                 TO  BALCDO.
001756     MOVE AL-SADOF               TO  BALPRTA.
001757     MOVE CO-CLP-STATE           TO  CLPSTO.
001758     MOVE CO-MAX-BANK-FEE        TO  MAXFEEO
001759     MOVE CO-SPP-REFUND-EDIT     TO  REFEO
001760     IF CO-MAX-BANK-FEE-LEASE NOT NUMERIC
001761        MOVE +0                  TO CO-MAX-BANK-FEE-LEASE
001762     END-IF
001763     MOVE CO-MAX-BANK-FEE-LEASE  TO  MAXLFO
001764     .
001765 5060-BUILD-TOTALS.
001766     IF SHOW-SAVE-TOTALS
001767         NEXT SENTENCE
001768     ELSE
001769         GO TO 5070-BUILD-CURRENT-TOTALS.
001770
001771     IF CO-LAST-STMT-DT = SPACES
001772         MOVE SPACES             TO  LSTMDTO
001773         GO TO 5060-LST-STMT-OK.
001774
001775     MOVE CO-LAST-STMT-MONTH     TO  WS-YMD-MM.
001776     MOVE CO-LAST-STMT-DAY       TO  WS-YMD-DD.
001777     MOVE CO-LAST-STMT-YEAR      TO  WS-YMD-YY.
001778
001779     IF WS-YMD-DATE-NUM NUMERIC
001780         NEXT SENTENCE
001781     ELSE
001782         MOVE SPACES             TO  LSTMDTO
001783         GO TO 5060-LST-STMT-OK.
001784
001785     MOVE WS-YMD-DATE            TO  DC-GREG-DATE-1-YMD.
001786     MOVE '3'                    TO  DC-OPTION-CODE.
001787
001788     PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
001789
001790     IF NO-CONVERSION-ERROR
001791         NEXT SENTENCE
001792     ELSE
001793         MOVE SPACES             TO  LSTMDTO
001794         GO TO 5060-LST-STMT-OK.
001795
001796     MOVE CO-LAST-STMT-MONTH     TO  WS-DMDY8-MM.
001797     MOVE CO-LAST-STMT-DAY       TO  WS-DMDY8-DD.
001798     MOVE CO-LAST-STMT-YEAR      TO  WS-DMDY8-YY.
001799     MOVE '/'                    TO  WS-DMDY8-SL1
001800                                     WS-DMDY8-SL2.
001801     MOVE WS-DATE-MDY-8          TO  LSTMDTO.
001802
001803 5060-LST-STMT-OK.
001804     MOVE CO-BAL-FWD             TO  BALFWDO.
001805     MOVE CO-CUR-COM             TO  CURCOMO.
001806     MOVE CO-CUR-PMT             TO  CURPMTO.
001807     MOVE CO-CUR-CHG             TO  CURCHGO.
001808     MOVE CO-END-BAL             TO  ENDBALO.
001809     MOVE CO-CUR                 TO  CURRENTO.
001810     MOVE CO-OV30                TO  OVER30O.
001811     MOVE CO-OV60                TO  OVER60O.
001812     MOVE CO-OV90                TO  OVER90O.
001813
001814     if pi-company-id = 'AHL'
001815        if co-ov120 not numeric
001816           move zeros            to co-ov120
001817        end-if
001818        move co-ov120            to over120o
001819     end-if
001820
001821     IF CO-ACCOUNT-TYPE
001822        MOVE CO-YTD-COM          TO YTDCOMO
001823     ELSE
001824        MOVE CO-YTD-OV           TO YTDCOMO
001825     END-IF
001826
001827     MOVE AL-SADOF               TO  PFK9A.
001828     MOVE 'PREV. END-OF-MONTH TOTALS DISPLAYED'
001829                                 TO  EMI-MESSAGE-AREA (1).
001830
001831     GO TO 5090-CONTD.
001832
001833 5070-BUILD-CURRENT-TOTALS.
001834     IF CO-CURRENT-LAST-STMT-DT = SPACES
001835         MOVE SPACES             TO  LSTMDTO
001836         GO TO 5070-LST-STMT-OK.
001837
001838     MOVE CO-CURRENT-LAST-STMT-MONTH
001839                                 TO WS-YMD-MM.
001840     MOVE CO-CURRENT-LAST-STMT-DAY
001841                                 TO  WS-YMD-DD.
001842     MOVE CO-CURRENT-LAST-STMT-YEAR
001843                                 TO  WS-YMD-YY.
001844
001845     IF WS-YMD-DATE-NUM NUMERIC
001846         NEXT SENTENCE
001847     ELSE
001848         MOVE SPACES             TO  LSTMDTO
001849         GO TO 5070-LST-STMT-OK.
001850
001851     MOVE WS-YMD-DATE            TO  DC-GREG-DATE-1-YMD.
001852     MOVE '3'                    TO  DC-OPTION-CODE.
001853
001854     PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
001855
001856     IF NO-CONVERSION-ERROR
001857         NEXT SENTENCE
001858     ELSE
001859         MOVE SPACES             TO  LSTMDTO
001860         GO TO 5070-LST-STMT-OK.
001861
001862     MOVE CO-CURRENT-LAST-STMT-MONTH
001863                                 TO  WS-DMDY8-MM.
001864     MOVE CO-CURRENT-LAST-STMT-DAY
001865                                 TO  WS-DMDY8-DD.
001866     MOVE CO-CURRENT-LAST-STMT-YEAR
001867                                 TO  WS-DMDY8-YY.
001868     MOVE '/'                    TO  WS-DMDY8-SL1
001869                                     WS-DMDY8-SL2.
001870     MOVE WS-DATE-MDY-8          TO  LSTMDTO.
001871
001872 5070-LST-STMT-OK.
001873     MOVE CO-CURRENT-BAL-FWD     TO  BALFWDO.
001874     MOVE CO-CURRENT-CUR-COM     TO  CURCOMO.
001875     MOVE CO-CURRENT-CUR-PMT     TO  CURPMTO.
001876     MOVE CO-CURRENT-CUR-CHG     TO  CURCHGO.
001877     MOVE CO-CURRENT-END-BAL     TO  ENDBALO
001878                                     PI-ERC-END-BAL.
001879     MOVE CO-CURRENT-CUR         TO  CURRENTO.
001880     MOVE CO-CURRENT-OV30        TO  OVER30O.
001881     MOVE CO-CURRENT-OV60        TO  OVER60O.
001882     MOVE CO-CURRENT-OV90        TO  OVER90O.
001883
001884     if pi-company-id = 'AHL'
001885        if co-current-ov120 not numeric
001886           move zeros            to co-current-ov120
001887        end-if
001888        move co-current-ov120    to over120o
001889     end-if
001890
001891     IF CO-ACCOUNT-TYPE
001892        MOVE CO-CURRENT-YTD-COM  TO YTDCOMO
001893     ELSE
001894        MOVE CO-CURRENT-YTD-OV   TO YTDCOMO
001895     END-IF
001896
001897     .
001898 5090-CONTD.
001899
001900     MOVE CO-LAST-MAINT-USER     TO LSTUSRO.
001901
001902     MOVE ' '              TO DC-OPTION-CODE.
001903     MOVE CO-LAST-MAINT-DT TO DC-BIN-DATE-1.
001904
001905     PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
001906
001907     MOVE DC-GREG-DATE-1-EDIT    TO LSTDTEO.
001908
001909     IF CO-LAST-MAINT-HHMMSS NUMERIC
001910         MOVE CO-LAST-MAINT-HHMMSS
001911                                 TO TIME-IN
001912         MOVE TIME-OUT           TO LSTTIMEO
001913     ELSE
001914         MOVE ZEROS              TO TIME-IN
001915         MOVE TIME-OUT           TO LSTTIMEO.
001916
001917     IF PI-COMPANY-ID =  'FLI'  OR  'FLU'  OR  'UCL'
001918         MOVE AL-SANOF           TO  FLITYPEA
001919         MOVE AL-UANON           TO  FLITYPA
001920     ELSE
001921         MOVE AL-SANON           TO  FLITYPA.
001922
001923     IF PI-COMPANY-ID = 'NCL'
001924         IF CO-ACCOUNT-TYPE
001925             MOVE AL-SANOF       TO  RPTCD2A
001926         ELSE
001927             IF CO-COMPANY-TYPE
001928                MOVE AL-SANOF    TO  RPTCD2A
001929                MOVE AL-SADOF    TO  RPTCDDA.
001930
001931     IF PI-COMPANY-ID = 'NCL'
001932         IF CO-GEN-AGENT-TYPE
001933            MOVE AL-UANON       TO  RPTCD2A.
001934
001935     IF PI-COMPANY-ID = 'NCL'
001936         MOVE AL-UANON           TO  LETRCDA
001937     ELSE
001938         MOVE AL-SANOF           TO  LETRCDA
001939         MOVE AL-SADOF           TO  LETDESCA.
001940
001941     IF PI-COMPANY-ID  NOT =  'DDB' AND 'ANT' AND 'ASL' AND
001942                              'AN1' AND 'TFS'
001943         MOVE AL-SANOF           TO  BALCDA
001944         MOVE AL-SADOF           TO  BALPRTA.
001945
001946     IF PI-AR-PROCESSING
001947         IF CO-ACCOUNT-TYPE
001948             MOVE AL-SANOF       TO  SCDESCA
001949             MOVE AL-SANOF       TO  NGDESCA
001950             MOVE AL-UANON       TO  NETGRSA
001951             IF PI-COMPANY-ID  =  'DDB' OR 'ANT' OR 'ASL' OR
001952                                  'AN1' OR 'TFS'
001953                 MOVE AL-SANOF   TO  BALPRTA
001954                 MOVE AL-UANON   TO  BALCDA.
001955
001956     IF PI-AR-PROCESSING
001957*            MOVE AL-SANOF       TO  BALDESCA
001958             move al-sanof       to  CKDESCA
001959*            MOVE AL-UANON       TO  ARBALA
001960             move al-uanon       to  CKPULLA.
001961
001962
001963     MOVE AL-UANON               TO  MAILNAMA
001964                                     ACCTNAMA
001965                                     ADDR1A
001966                                     ADDR2A
001967                                     CITYA
001968                                     STATEA
001969                                     SSNA
001970                                     ZIPCODEA
001971                                     BILLPRTA
001972                                     PNT1099A
001973                                     CARBALA.
001974
001975     IF NOT SHOW-SAVE-TOTALS
001976         IF PI-PROCESSOR-ID = 'LGXX'
001977             GO TO 5100-CONTD.
001978
001979     MOVE AL-SANOF               TO  BALFWDA
001980                                     CURCOMA
001981                                     CURPMTA
001982                                     CURCHGA
001983                                     ENDBALA.
001984
001985 5100-CONTD.
001986     IF CO-TELEPHONE NUMERIC
001987         MOVE CO-TELEPHONE       TO  WS-PHONE-IN
001988                                     PI-SAVE-PHONE
001989         MOVE WSPI-AREA          TO  WSPO-AREA
001990         MOVE WSPI-PFX           TO  WSPO-PFX
001991         MOVE WSPI-SFX           TO  WSPO-SFX
001992         MOVE WS-PHONE-OUT       TO  PHONEO
001993         MOVE AL-UNNON           TO  PHONEA
001994     ELSE
001995         MOVE SPACES             TO  PHONEO.
001996
001997     IF CO-FAXNO NUMERIC
001998         MOVE CO-FAXNO           TO  WS-PHONE-IN
001999                                     PI-SAVE-FAXNO
002000         MOVE WSPI-AREA          TO  WSPO-AREA
002001         MOVE WSPI-PFX           TO  WSPO-PFX
002002         MOVE WSPI-SFX           TO  WSPO-SFX
002003         MOVE WS-PHONE-OUT       TO  FAXNOO
002004         MOVE AL-UNNON           TO  FAXNOA
002005     ELSE
002006         MOVE SPACES             TO  FAXNOO.
002007
002008     MOVE 'S'                    TO  MAINTYPO
002009                                     PI-CHECK-MAINT-TYPE.
002010     MOVE AL-UANON               TO  MAINTYPA.
002011     MOVE -1                     TO  MAINTYPL.
002012
002013     GO TO 8100-SEND-INITIAL-MAP.
002014 EJECT
002015 6000-CHECK-FOR-UPDATE.
002016     IF CHANGE-FUNCTION
002017         GO TO 6010-CONT.
002018
002019     IF CARRIERL > ZERO
002020         MOVE CARRIERI           TO  CO-CARRIER.
002021
002022     IF GROUPL > ZERO
002023         MOVE GROUPI             TO  CO-GROUPING
002024     ELSE
002025         MOVE LOW-VALUES         TO  CO-GROUPING.
002026
002027     IF FINRESPL > ZERO
002028         MOVE FINRESPI           TO  CO-RESP-NO
002029     ELSE
002030         MOVE LOW-VALUES         TO  CO-RESP-NO.
002031
002032     IF ACCTNOL > ZERO
002033         MOVE ACCTNOI            TO  CO-ACCOUNT
002034     ELSE
002035         MOVE LOW-VALUES         TO  CO-ACCOUNT.
002036
002037     IF TYPEL > ZERO
002038         MOVE TYPEI              TO  CO-TYPE.
002039
002040 6010-CONT.
002041     IF PI-PROCESSOR-ID = 'LGXX'
002042         IF LSTMDTL > ZERO
002043             MOVE WS-YMD-YY      TO  CO-CURRENT-LAST-STMT-YEAR
002044                                     WS-DMDY8-YY
002045             MOVE WS-YMD-MM      TO  CO-CURRENT-LAST-STMT-MONTH
002046                                     WS-DMDY8-MM
002047             MOVE WS-YMD-DD      TO  CO-CURRENT-LAST-STMT-DAY
002048                                     WS-DMDY8-DD
002049             MOVE '/'            TO  WS-DMDY8-SL1  WS-DMDY8-SL2
002050             MOVE WS-DATE-MDY-8  TO  LSTMDTO.
002051
002052     IF PI-AR-PROCESSING
002053         IF TYPEI = 'A'
002054             IF SUMMNOL > ZERO
002055                 MOVE SUMMNOI   TO  CO-AR-SUMMARY-CODE
002056                                    WS-SAVE-SUMMARY.
002057
002058     IF PI-AR-PROCESSING
002059         IF TYPEI = 'A'
002060             IF NETGRSL > ZERO
002061                 MOVE NETGRSI   TO  CO-AR-REPORTING
002062             ELSE
002063                 IF ADD-FUNCTION
002064                     MOVE 'G'   TO  CO-AR-REPORTING.
002065
002066     IF PI-AR-PROCESSING
002067*        IF ARBALL > ZERO
002068*            MOVE ARBALI        TO  CO-AR-BAL-LEVEL
002069*        ELSE
002070             IF ADD-FUNCTION
002071                 MOVE '1'       TO  CO-AR-BAL-LEVEL.
002072
002073     IF PI-AR-PROCESSING
002074         IF CKPULLL > ZERO
002075             MOVE CKPULLI       TO  CO-AR-PULL-CHECK
002076         ELSE
002077             IF ADD-FUNCTION
002078                 MOVE 'Y'       TO  CO-AR-PULL-CHECK.
002079
002080     IF MAILNAML > ZERO
002081         MOVE MAILNAMI           TO  CO-MAIL-NAME
002082                                     WS-SAVE-NAME.
002083
002084     IF ACCTNAML > ZERO
002085         MOVE ACCTNAMI           TO  CO-ACCT-NAME.
002086
002087     IF ADDR1L > ZERO
002088         MOVE ADDR1I             TO  CO-ADDR-1.
002089
002090     IF ADDR2L > ZERO
002091         MOVE ADDR2I             TO  CO-ADDR-2.
002092
002093     IF CITYL > ZERO
002094         MOVE CITYI              TO  CO-ADDR-CITY.
002095     IF STATEL > ZERO
002096         MOVE STATEI             TO  CO-ADDR-STATE.
002097
002098     IF ZIPCODEL NOT > ZERO
002099         GO TO 6015-CONT.
002100
002101     MOVE ZIPCODEI               TO  WS-ZIP-CODE.
002102
002103     IF WS-CANADIAN-ZIP
002104         IF WS-ZIP-4 = SPACE  OR  '-'
002105             MOVE WS-ZIP-CAN-2-POST1     TO  CO-CAN-POSTAL-1
002106             MOVE WS-ZIP-CAN-2-POST2     TO  CO-CAN-POSTAL-2
002107         ELSE
002108             MOVE WS-ZIP-CAN-1-POST1     TO  CO-CAN-POSTAL-1
002109             MOVE WS-ZIP-CAN-1-POST2     TO  CO-CAN-POSTAL-2
002110     ELSE
002111         IF WS-ZIP-6 = SPACE  OR  '-'
002112             MOVE WS-ZIP-AM-2-CODE       TO  CO-ZIP-PRIME
002113             MOVE WS-ZIP-AM-2-PLUS4      TO  CO-ZIP-PLUS4
002114         ELSE
002115             MOVE WS-ZIP-AM-1-CODE       TO  CO-ZIP-PRIME
002116             MOVE WS-ZIP-AM-1-PLUS4      TO  CO-ZIP-PLUS4.
002117
002118 6015-CONT.
002119     IF BILLPRTL > ZERO
002120         IF BILLPRTI = ' ' OR 'B' OR 'R' OR 'T' OR 'S'
002121            OR 'O' OR 'E' OR 'C'
002122             MOVE  BILLPRTI          TO  CO-BILL-SW
002123         ELSE
002124             MOVE SPACES         TO  CO-BILL-SW
002125                                     BILLPRTO
002126         END-IF
002127     END-IF.
002128
002129     IF SPPDDL > ZERO
002130        IF SPPDDI = 'Y'
002131           MOVE '1'              TO CO-COMP-TYPE
002132        ELSE
002133           MOVE ' '              TO CO-COMP-TYPE
002134        END-IF
002135     END-IF
002136     IF PNT1099L > ZERO
002137         IF PNT1099I = 'N' OR 'Y'
002138             MOVE  PNT1099I      TO  CO-CSO-1099
002139         ELSE
002140             MOVE 'N'            TO  CO-CSO-1099
002141                                     PNT1099O
002142         END-IF
002143     END-IF.
002144
002145     IF CSRL > ZERO
002146         MOVE CSRI               TO  CO-CSR-CODE.
002147
002148     IF SSNL > ZERO
002149         MOVE SSNI               TO  CO-SOC-SEC.
002150
002151     IF PHONEL > ZERO
002152         MOVE PI-SAVE-PHONE      TO  CO-TELEPHONE
002153         MOVE ZEROS              TO  PI-SAVE-PHONE.
002154
002155     IF FAXNOL > ZERO
002156         MOVE PI-SAVE-FAXNO      TO  CO-FAXNO
002157         MOVE ZEROS              TO  PI-SAVE-FAXNO.
002158
002159     IF PCONTL > ZEROS
002160        MOVE PCONTI              TO CO-CONTROL-NAME
002161     END-IF
002162     IF CLPSTL > ZERO
002163         MOVE CLPSTI             TO  CO-CLP-STATE
002164     END-IF
002165
002166     IF REFEL > ZERO
002167        MOVE REFEI               TO CO-SPP-REFUND-EDIT
002168     END-IF
002169
002170     IF MAXFEEL > ZERO
002171         MOVE MAXFEEI            TO  CO-MAX-BANK-FEE
002172*        IF ADD-FUNCTION
002173*           MOVE CO-MAX-BANK-FEE TO  CO-BANK-FEE
002174*        ELSE
002175*           IF CHANGE-FUNCTION
002176*              PERFORM 6100-EDIT-ERAGTC
002177*                                THRU 6100-EXIT
002178*              IF WS-TOT-FEES > CO-MAX-BANK-FEE
002179*                 MOVE ER-2717   TO EMI-ERROR
002180*                 MOVE -1        TO MAXFEEL
002181*                 MOVE AL-UABON  TO MAXFEEA
002182*                 PERFORM 9900-ERROR-FORMAT
002183*                                THRU 9900-EXIT
002184*              ELSE
002185*                 COMPUTE CO-BANK-FEE = CO-MAX-BANK-FEE
002186*                    - WS-TOT-FEES
002187*              END-IF
002188*           END-IF
002189*        END-IF
002190     END-IF
002191
002192     IF MAXLFL > ZERO
002193         MOVE MAXLFI             TO CO-MAX-BANK-FEE-LEASE
002194     END-IF
002195
002196     IF CARBALL > ZERO
002197         MOVE CARBALI            TO  CO-BALANCE-CONTROL
002198         GO TO 6020-CONT.
002199
002200     IF NOT PI-AR-PROCESSING
002201         GO TO 6020-CONT.
002202
002203     IF ADD-FUNCTION
002204        AND (TYPEI = 'G' OR 'B')
002205         MOVE 'Y'                TO  CO-BALANCE-CONTROL
002206         GO TO 6020-CONT.
002207
002208     IF ADD-FUNCTION
002209         IF TYPEI = 'A'
002210             IF FINRESPI = ACCTNOI
002211                 MOVE 'Y'        TO  CO-BALANCE-CONTROL
002212             ELSE
002213                 MOVE 'N'        TO  CO-BALANCE-CONTROL.
002214
002215     IF ADD-FUNCTION AND TYPEI = 'C'
002216         MOVE 'N'                TO  CO-BALANCE-CONTROL.
002217
002218 6020-CONT.
002219     IF BALFWDL > ZERO
002220         MOVE WS-SAVE-BALFWD     TO  CO-CURRENT-BAL-FWD.
002221
002222     IF CURCOML > ZERO
002223         MOVE WS-SAVE-CURCOM     TO  CO-CURRENT-CUR-COM.
002224
002225     IF CURCHGL > ZERO
002226         MOVE WS-SAVE-CURCHG     TO  CO-CURRENT-CUR-CHG.
002227
002228     IF CURPMTL > ZERO
002229         MOVE WS-SAVE-CURPMT     TO  CO-CURRENT-CUR-PMT.
002230
002231     IF ENDBALL > ZERO
002232         MOVE WS-SAVE-ENDBAL     TO  CO-CURRENT-END-BAL.
002233
002234*    IF YTDCOML > ZERO
002235*       IF CO-ACCOUNT-TYPE
002236*          MOVE WS-SAVE-YTDCOM   TO CO-CURRENT-YTD-COM
002237*       ELSE
002238*          MOVE WS-SAVE-YTDCOM   TO CO-CURRENT-YTD-OV
002239*       END-IF
002240*    END-IF
002241
002242     .
002243 6099-EXIT.
002244     EXIT.
002245 EJECT
002246 6100-EDIT-ERAGTC.
002247
002248     
      * EXEC CICS READ
002249*         SET     (ADDRESS OF AGENT-COMMISSIONS)
002250*         DATASET ('ERAGTC')
002251*         RIDFLD  (PI-ERCOMP-KEY)
002252*         RESP    (WS-RESPONSE)
002253*    END-EXEC
           MOVE 'ERAGTC' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00005965' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303035393635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AGENT-COMMISSIONS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002254     MOVE +0                     TO WS-TOT-FEES
002255     IF RESP-NORMAL
002256        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
002257           (S1 > +10)
002258           COMPUTE WS-TOT-FEES = WS-TOT-FEES
002259              + AG-SPP-FEES (S1)
002260        END-PERFORM
002261     END-IF
002262
002263     .
002264 6100-EXIT.
002265     EXIT.
002266
002267 6500-UPDATE-RQST.
002268
002269     MOVE LOW-VALUES             TO  ERRQST-KEY-3.
002270     MOVE PI-ERC-COMPANY-CD      TO  RQST-COMP-ID.
002271     MOVE PI-ERC-CARRIER         TO  RQST-CARRIER.
002272     MOVE PI-ERC-GROUP           TO  RQST-GROUP.
002273     MOVE PI-ERC-RESP            TO  RQST-FIN-RESP.
002274     MOVE PI-ERC-ACCT            TO  RQST-ACCT-AGENT.
002275
002276     
      * EXEC CICS HANDLE CONDITION
002277*        ENDFILE  (6550-END-BROWSE)
002278*        NOTFND   (6550-END-BROWSE)
002279*    END-EXEC.
      *    MOVE '"$''I                  ! * #00005993' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303035393933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002280
002281     
      * EXEC CICS STARTBR
002282*        DATASET  (RQST-FILE-ID-3)
002283*        RIDFLD   (ERRQST-KEY-3)
002284*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005998' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303035393938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID-3, 
                 ERRQST-KEY-3, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002285
002286 6525-READ-LOOP.
002287     
      * EXEC CICS HANDLE CONDITION
002288*        ENDFILE  (6550-END-BROWSE)
002289*        NOTFND   (6550-END-BROWSE)
002290*    END-EXEC.
      *    MOVE '"$''I                  ! + #00006004' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303036303034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002291
002292     
      * EXEC CICS READNEXT
002293*        DATASET  (RQST-FILE-ID-3)
002294*        SET      (ADDRESS OF AR-REQUEST-RECORD)
002295*        RIDFLD   (ERRQST-KEY-3)
002296*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006009' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036303039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID-3, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRQST-KEY-3, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002297
002298     IF RQ-CONTROL-BY-FIN-RESP = WS-SAVE-RQST
002299         GO TO 6525-READ-LOOP.
002300
002301     IF RQ-COMPANY-CD-A2 =  PI-ERC-COMPANY-CD AND
002302        RQ-CARRIER-A2    =  PI-ERC-CARRIER    AND
002303        RQ-GROUPING-A2   =  PI-ERC-GROUP      AND
002304        RQ-FIN-RESP-A2   =  PI-ERC-RESP       AND
002305        RQ-ACCT-AGENT-A2 =  PI-ERC-ACCT
002306         NEXT SENTENCE
002307     ELSE
002308         GO TO 6550-END-BROWSE.
002309
002310     IF WS-SUMM-FOR-RQST = RQ-SUMMARY-CODE
002311         GO TO 6525-READ-LOOP.
002312
002313     MOVE RQ-CONTROL-BY-FIN-RESP TO  WS-SAVE-RQST.
002314     MOVE RQ-CONTROL-PRIMARY     TO  ERRQST-KEY.
002315
002316     
      * EXEC CICS ENDBR
002317*        DATASET  (RQST-FILE-ID-3)
002318*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006033' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036303333' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID-3, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002319
002320     
      * EXEC CICS HANDLE CONDITION
002321*        ENDFILE  (6599-EXIT)
002322*        NOTFND   (6599-EXIT)
002323*    END-EXEC.
      *    MOVE '"$''I                  ! , #00006037' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303036303337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002324
002325     
      * EXEC CICS READ
002326*        DATASET  (RQST-FILE-ID)
002327*        SET      (ADDRESS OF AR-REQUEST-RECORD)
002328*        RIDFLD   (ERRQST-KEY)
002329*        UPDATE
002330*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006042' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036303432' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002331
002332     MOVE WS-SUMM-FOR-RQST       TO  RQ-SUMMARY-CODE.
002333
002334     
      * EXEC CICS REWRITE
002335*        DATASET  (RQST-FILE-ID)
002336*        FROM     (AR-REQUEST-RECORD)
002337*    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006051' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036303531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002338
002339     GO TO 6500-UPDATE-RQST.
002340
002341 6550-END-BROWSE.
002342
002343     
      * EXEC CICS ENDBR
002344*        DATASET  (RQST-FILE-ID-3)
002345*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006060' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036303630' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID-3, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002346
002347 6599-EXIT.
002348     EXIT.
002349 EJECT
002350
002351 7000-EDIT.
002352     MOVE LSTMDTI                TO  WS-DATE-MDY-8.
002353
002354     IF LSTMDTL > ZERO
002355         NEXT SENTENCE
002356     ELSE
002357         GO TO 7100-EDIT-CONTD.
002358
002359     IF LSTMDTI = SPACES
002360         GO TO 7100-EDIT-CONTD.
002361
002362     IF WS-DMDY8-SL1 = '/'  AND
002363        WS-DMDY8-SL2 = '/'
002364         MOVE WS-DMDY8-MM        TO  WS-YMD-MM
002365         MOVE WS-DMDY8-DD        TO  WS-YMD-DD
002366         MOVE WS-DMDY8-YY        TO  WS-YMD-YY
002367     ELSE
002368         MOVE WS-DMDY6-MM        TO  WS-YMD-MM
002369         MOVE WS-DMDY6-DD        TO  WS-YMD-DD
002370         MOVE WS-DMDY6-YY        TO  WS-YMD-YY.
002371
002372     MOVE WS-YMD-DATE            TO  DC-GREG-DATE-1-YMD.
002373     MOVE '3'                    TO  DC-OPTION-CODE.
002374
002375     PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
002376
002377     IF NO-CONVERSION-ERROR
002378         NEXT SENTENCE
002379     ELSE
002380         GO TO 7090-DATE-ERROR.
002381
002382     IF DC-BIN-DATE-1 > PI-CR-MONTH-END-DT
002383         NEXT SENTENCE
002384     ELSE
002385         GO TO 7100-EDIT-CONTD.
002386
002387 7090-DATE-ERROR.
002388     MOVE -1                     TO  LSTMDTL.
002389     MOVE AL-UABON               TO  LSTMDTA.
002390     MOVE ER-0314                TO  EMI-ERROR.
002391
002392     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002393
002394 7100-EDIT-CONTD.
002395
002396     IF CSRL > ZEROS
002397         MOVE AL-UANON           TO  CSRA
002398     END-IF
002399
002400     IF MAILNAML > ZERO
002401         MOVE AL-UANON           TO  MAILNAMA.
002402
002403     IF SSNL > ZERO
002404         MOVE AL-UANON           TO  SSNA.
002405
002406     IF ACCTNAML > ZERO
002407         MOVE AL-UANON           TO  ACCTNAMA
002408     ELSE
002409         IF ADD-FUNCTION
002410             MOVE -1             TO  ACCTNAML
002411             MOVE AL-UABON       TO  ACCTNAMA
002412             MOVE ER-2045        TO  EMI-ERROR
002413             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002414
002415     IF PHONEL > ZERO
002416         MOVE PHONEI               TO  DEEDIT-FIELD
002417         PERFORM 7100-DEEDIT  THRU  7100-EXIT
002418         IF DEEDIT-FIELD-V0 = ZEROS
002419             MOVE SPACES               TO  PHONEO
002420                                           PI-SAVE-PHONE
002421         ELSE
002422             IF (DEEDIT-FIELD-V0 > 9999999999
002423               OR < 2000000000)
002424                 MOVE -1               TO  PHONEL
002425                 MOVE AL-UNBON         TO  PHONEA
002426                 MOVE ER-2046          TO  EMI-ERROR
002427                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002428             ELSE
002429                 MOVE DEEDIT-FIELD-V0  TO  WS-PHONE-IN
002430                                           PI-SAVE-PHONE-RED
002431                 MOVE WSPI-AREA        TO  WSPO-AREA
002432                 MOVE WSPI-PFX         TO  WSPO-PFX
002433                 MOVE WSPI-SFX         TO  WSPO-SFX
002434                 MOVE WS-PHONE-OUT     TO  PHONEO
002435                 MOVE AL-UNNON         TO  PHONEA.
002436
002437     IF FAXNOL > ZERO
002438         MOVE FAXNOI               TO  DEEDIT-FIELD
002439         PERFORM 7100-DEEDIT  THRU  7100-EXIT
002440         IF DEEDIT-FIELD-V0 = ZEROS
002441             MOVE SPACES               TO  FAXNOO
002442                                           PI-SAVE-FAXNO
002443         ELSE
002444             IF (DEEDIT-FIELD-V0 > 9999999999
002445               OR < 2000000000)
002446                 MOVE -1               TO  FAXNOL
002447                 MOVE AL-UNBON         TO  FAXNOA
002448                 MOVE ER-3055          TO  EMI-ERROR
002449                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002450             ELSE
002451                 MOVE DEEDIT-FIELD-V0  TO  WS-PHONE-IN
002452                                           PI-SAVE-FAXNO-RED
002453                 MOVE WSPI-AREA        TO  WSPO-AREA
002454                 MOVE WSPI-PFX         TO  WSPO-PFX
002455                 MOVE WSPI-SFX         TO  WSPO-SFX
002456                 MOVE WS-PHONE-OUT     TO  FAXNOO
002457                 MOVE AL-UNNON         TO  FAXNOA.
002458
002459     IF ADDR1L > ZERO
002460         MOVE AL-UANON           TO  ADDR1A
002461     ELSE
002462         IF ADD-FUNCTION
002463             MOVE -1             TO  ADDR1L
002464             MOVE AL-UABON       TO  ADDR1A
002465             MOVE ER-2047        TO  EMI-ERROR
002466             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002467
002468     IF ADDR2L > ZERO
002469         MOVE AL-UANON           TO  ADDR2A.
002470
002471     IF CARBALL > ZERO OR CHANGE-FUNCTION
002472         MOVE CARBALI            TO  PI-CHECK-CARRY-BAL
002473         IF VALID-CARRY-BAL
002474             PERFORM 7010-EDIT-CARBAL  THRU  7010-EXIT
002475         ELSE
002476             MOVE -1             TO  CARBALL
002477             MOVE AL-UABON       TO  CARBALA
002478             MOVE ER-2048        TO  EMI-ERROR
002479             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002480     ELSE
002481         IF ADD-FUNCTION AND NOT PI-AR-PROCESSING
002482             MOVE -1             TO  CARBALL
002483             MOVE AL-UABON       TO  CARBALA
002484             MOVE ER-2048        TO  EMI-ERROR
002485             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002486
002487     IF PI-AR-PROCESSING
002488         IF TYPEI = 'A'
002489            IF NETGRSL > ZERO OR CHANGE-FUNCTION
002490                 IF NETGRSI = 'N' OR 'G'
002491                    MOVE AL-UANON    TO  NETGRSA
002492                 ELSE
002493                     MOVE -1         TO  NETGRSL
002494                     MOVE AL-UABON   TO  NETGRSA
002495                     MOVE ER-3151    TO  EMI-ERROR
002496                     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002497
002498     IF CITYL > ZERO
002499         MOVE AL-UANON           TO  CITYA
002500     ELSE
002501         IF ADD-FUNCTION
002502             MOVE -1             TO  CITYL
002503             MOVE AL-UABON       TO  CITYA
002504             MOVE ER-2049        TO  EMI-ERROR
002505             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002506
002507     IF STATEL   > ZERO
002508        MOVE AL-UANON            TO STATEA
002509        MOVE SPACES              TO ELCNTL-KEY
002510        MOVE PI-COMPANY-ID       TO CNTL-COMP-ID
002511        MOVE '3'                 TO CNTL-REC-TYPE
002512        MOVE STATEI              TO CNTL-ACCESS
002513        MOVE +0                  TO CNTL-SEQ-NO
002514        
      * EXEC CICS READ
002515*          DATASET   (CNTL-FILE-ID)
002516*          SET       (ADDRESS OF CONTROL-FILE)
002517*          RIDFLD    (ELCNTL-KEY)
002518*          RESP      (WS-RESPONSE)
002519*       END-EXEC
      *    MOVE '&"S        E          (  N#00006231' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303036323331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002520        IF RESP-NORMAL
002521           CONTINUE
002522        ELSE
002523           MOVE ER-2209          TO EMI-ERROR
002524           MOVE -1               TO STATEL
002525           MOVE AL-UABON         TO STATEA
002526           PERFORM 9900-ERROR-FORMAT
002527                                 THRU 9900-EXIT
002528        END-IF
002529     ELSE
002530         IF ADD-FUNCTION
002531             MOVE -1             TO  STATEL
002532             MOVE AL-UABON       TO  STATEA
002533             MOVE ER-2049        TO  EMI-ERROR
002534             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002535
002536*     IF PI-AR-PROCESSING
002537*         IF ARBALL > ZERO OR CHANGE-FUNCTION
002538*             IF ARBALI = '1' OR '2' OR '3' OR '4'
002539*                 MOVE AL-UANON   TO  ARBALA
002540*             ELSE
002541*                 MOVE -1         TO  ARBALL
002542*                 MOVE AL-UABON   TO  ARBALA
002543*                 MOVE ER-3150    TO  EMI-ERROR
002544*                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002545
002546     IF PI-AR-PROCESSING
002547         IF CKPULLL > ZERO OR CHANGE-FUNCTION
002548             IF CKPULLI = 'Y' OR 'N'
002549                 MOVE AL-UANON   TO  CKPULLA
002550             ELSE
002551                 MOVE -1         TO  CKPULLL
002552                 MOVE AL-UABON   TO  CKPULLA
002553                 MOVE ER-3170    TO  EMI-ERROR
002554                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002555
002556     IF ZIPCODEL > ZERO
002557         MOVE AL-UANON           TO  ZIPCODEA.
002558
002559     IF PI-COMPANY-ID = 'CID' OR 'AHL' or 'FNL'
002560        CONTINUE
002561     ELSE
002562        IF PI-ERC-TYPE = 'B'
002563           IF CLPSTL > ZERO
002564              PERFORM 4700-CHECK-STATE       THRU 4799-EXIT
002565              IF CLPSTI = SPACES
002566                 MOVE -1         TO  CLPSTL
002567                 MOVE AL-UABON   TO  CLPSTA
002568                 MOVE ER-0144    TO  EMI-ERROR
002569                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002570              ELSE
002571                 MOVE AL-UANON   TO  CLPSTA
002572              END-IF
002573           ELSE
002574              IF ADD-FUNCTION
002575                 MOVE -1         TO  CLPSTL
002576                 MOVE AL-UABON   TO  CLPSTA
002577                 MOVE ER-0144    TO  EMI-ERROR
002578                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002579              END-IF
002580           END-IF
002581
002582           IF MAXFEEL  > ZERO
002583              
      * EXEC CICS BIF
002584*                  DEEDIT
002585*                  FIELD   (MAXFEEI)
002586*                  LENGTH  (6)
002587*             END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006300' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036333030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAXFEEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002588              IF MAXFEEI NUMERIC
002589                 MOVE AL-UNNON   TO  MAXFEEA
002590              ELSE
002591                 MOVE -1         TO  MAXFEEL
002592                 MOVE AL-UABON   TO  MAXFEEA
002593                 MOVE ER-3261    TO  EMI-ERROR
002594                 PERFORM 9900-ERROR-FORMAT
002595                                 THRU  9900-EXIT
002596              END-IF
002597           ELSE
002598              IF ADD-FUNCTION
002599                 MOVE -1         TO  MAXFEEL
002600                 MOVE AL-UABON   TO  MAXFEEA
002601                 MOVE ER-3261    TO  EMI-ERROR
002602                 PERFORM 9900-ERROR-FORMAT
002603                                 THRU  9900-EXIT
002604              END-IF
002605           END-IF
002606           IF MAXLFL  > ZERO
002607              
      * EXEC CICS BIF
002608*                  DEEDIT
002609*                  FIELD   (MAXLFI)
002610*                  LENGTH  (6)
002611*             END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006324' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036333234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAXLFI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002612              IF MAXLFI NUMERIC
002613                 MOVE AL-UNNON   TO  MAXLFA
002614              ELSE
002615                 MOVE -1         TO  MAXLFL
002616                 MOVE AL-UABON   TO  MAXLFA
002617                 MOVE ER-3261    TO  EMI-ERROR
002618                 PERFORM 9900-ERROR-FORMAT
002619                                 THRU  9900-EXIT
002620              END-IF
002621           ELSE
002622              IF ADD-FUNCTION
002623                 MOVE -1         TO  MAXLFL
002624                 MOVE AL-UABON   TO  MAXLFA
002625                 MOVE ER-3261    TO  EMI-ERROR
002626                 PERFORM 9900-ERROR-FORMAT
002627                                 THRU  9900-EXIT
002628              END-IF
002629           END-IF
002630           IF REFEL > ZERO
002631              IF REFEI = ' ' OR 'R' OR 'N' OR 'B'
002632                 MOVE AL-UANON   TO REFEA
002633              ELSE
002634                 MOVE -1         TO REFEL
002635                 MOVE AL-UABON   TO REFEA
002636                 MOVE ER-2790    TO EMI-ERROR
002637                 PERFORM 9900-ERROR-FORMAT
002638                                 THRU  9900-EXIT
002639              END-IF
002640           END-IF
002641        END-IF
002642     END-IF
002643
002644     IF BALFWDL > ZERO
002645         
      * EXEC CICS BIF
002646*            DEEDIT
002647*            FIELD   (BALFWDI)
002648*            LENGTH  (13)
002649*        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006362' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036333632' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALFWDI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002650         MOVE AL-UNNON           TO  BALFWDA
002651         MOVE BALFWDI            TO  WS-SAVE-BALFWD.
002652
002653     IF CURCOML >        ZERO
002654         
      * EXEC CICS BIF
002655*            DEEDIT
002656*            FIELD   (CURCOMI)
002657*            LENGTH  (13)
002658*        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006371' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036333731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CURCOMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002659         MOVE AL-UNNON           TO  CURCOMA
002660         MOVE CURCOMI            TO  WS-SAVE-CURCOM.
002661
002662     IF CURCHGL > ZERO
002663         
      * EXEC CICS BIF
002664*            DEEDIT
002665*            FIELD   (CURCHGI)
002666*            LENGTH  (13)
002667*        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006380' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036333830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CURCHGI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002668         MOVE AL-UNNON           TO  CURCHGA
002669         MOVE CURCHGI            TO  WS-SAVE-CURCHG.
002670
002671     IF CURPMTL > ZERO
002672         
      * EXEC CICS BIF
002673*            DEEDIT
002674*            FIELD   (CURPMTI)
002675*            LENGTH  (13)
002676*        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006389' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036333839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CURPMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002677         MOVE AL-UNNON           TO  CURPMTA
002678         MOVE CURPMTI            TO  WS-SAVE-CURPMT.
002679
002680     IF ENDBALL > ZERO
002681         
      * EXEC CICS BIF
002682*            DEEDIT
002683*            FIELD   (ENDBALI)
002684*            LENGTH  (13)
002685*        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006398' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036333938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ENDBALI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002686         MOVE AL-UNNON           TO  ENDBALA
002687         MOVE ENDBALI            TO  WS-SAVE-ENDBAL.
002688
002689*    IF YTDCOML > ZERO
002690*       EXEC CICS BIF
002691*            DEEDIT
002692*            FIELD   (YTDCOMI)
002693*            LENGTH  (13)
002694*       END-EXEC
002695*       MOVE AL-UNNON            TO YTDCOMA
002696*       MOVE YTDCOMI             TO WS-SAVE-YTDCOM
002697*    END-IF
002698
002699     .
002700 7000-EXIT.
002701     EXIT.
002702 EJECT
002703 7010-EDIT-CARBAL.
002704     IF TYPEI = 'G' OR 'B'
002705         IF CARBALI NOT = 'Y'
002706             MOVE -1             TO  CARBALL
002707             MOVE AL-UABON       TO  CARBALA
002708             MOVE ER-2093        TO  EMI-ERROR
002709             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002710             GO TO 7010-EXIT.
002711
002712     IF TYPEI = 'A'
002713         IF FINRESPI = ACCTNOI
002714             IF CARBALI NOT = 'Y'
002715                 MOVE -1         TO  CARBALL
002716                 MOVE AL-UABON   TO  CARBALA
002717                 MOVE ER-2094    TO  EMI-ERROR
002718                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002719                 GO TO 7010-EXIT.
002720
002721     IF PI-PROCESSOR-ID = 'E864'
002722         NEXT SENTENCE
002723     ELSE
002724        IF TYPEI = 'A'
002725           IF PI-AR-PROCESSING
002726               IF FINRESPI NOT = ACCTNOI
002727                    IF CARBALI = 'Y'
002728                       MOVE -1             TO  CARBALL
002729                       MOVE AL-UABON       TO  CARBALA
002730                       MOVE ER-3174        TO  EMI-ERROR
002731                       PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002732
002733     IF TYPEI = 'C'
002734         IF CARBALI NOT = 'N'
002735             MOVE -1             TO  CARBALL
002736             MOVE AL-UABON       TO  CARBALA
002737             MOVE ER-2096        TO  EMI-ERROR
002738             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002739             GO TO 7010-EXIT.
002740
002741     IF PI-PROCESSOR-ID = 'E864'
002742         NEXT SENTENCE
002743     ELSE
002744        IF CHANGE-FUNCTION
002745            IF PI-ERC-END-BAL NOT = ZERO
002746                IF CARBALI = 'N'
002747                    MOVE -1         TO  CARBALL
002748                    MOVE AL-UABON   TO  CARBALA
002749                    MOVE ER-2095    TO  EMI-ERROR
002750                    PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002751                    GO TO 7010-EXIT.
002752
002753     MOVE AL-UANON               TO  CARBALA.
002754
002755 7010-EXIT.
002756     EXIT.
002757 EJECT
002758 7050-READ-ERCOMP.
002759     MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
002760
002761     
      * EXEC CICS READ
002762*        DATASET  (COMP-FILE-ID)
002763*        SET      (ADDRESS OF COMPENSATION-MASTER)
002764*        RIDFLD   (PI-ERCOMP-KEY)
002765*    END-EXEC.
      *    MOVE '&"S        E          (   #00006478' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036343738' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002766
002767     MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.
002768
002769 7050-EXIT.
002770     EXIT.
002771 EJECT
002772 7100-DEEDIT.
002773     
      * EXEC CICS BIF
002774*        DEEDIT
002775*        FIELD   (DEEDIT-FIELD)
002776*        LENGTH  (15)
002777*    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006490' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303036343930' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002778
002779 7100-EXIT.
002780     EXIT.
002781 EJECT
002782 7150-ERCOMP-GETMAIN.
002783     
      * EXEC CICS GETMAIN
002784*        SET      (ADDRESS OF COMPENSATION-MASTER)
002785*        LENGTH   (ERCOMP-LENGTH)
002786*        INITIMG  (GETMAIN-SPACE)
002787*    END-EXEC.
      *    MOVE ',"IL                  $   #00006500' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036353030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCOMP-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002788
002789 7150-EXIT.
002790     EXIT.
002791 EJECT
002792 7200-READ-ERCOMP-UPDATE.
002793     MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
002794
002795     
      * EXEC CICS READ
002796*        DATASET  (COMP-FILE-ID)
002797*        SET      (ADDRESS OF COMPENSATION-MASTER)
002798*        RIDFLD   (PI-ERCOMP-KEY)
002799*        UPDATE
002800*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006512' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036353132' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002801
002802 7200-EXIT.
002803     EXIT.
002804 EJECT
002805 7250-PAGE-FORWARD.
002806     MOVE SPACES                 TO  PI-ERCOMP-EOF-SW.
002807
002808     IF MAINTYPL > ZERO
002809         MOVE GROUPI             TO  PI-ERC-GROUP
002810         MOVE CARRIERI           TO  PI-ERC-CARRIER
002811         MOVE FINRESPI           TO  PI-ERC-RESP
002812         MOVE ACCTNOI            TO  PI-ERC-ACCT
002813         MOVE TYPEI              TO  PI-ERC-TYPE
002814     ELSE
002815         MOVE LOW-VALUES         TO  PI-ERCOMP-KEY
002816         MOVE 'Y'                TO  PI-FIRST-TIME-SW.
002817
002818     MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
002819     MOVE PI-ERCOMP-KEY          TO  WS-SAVE-KEY.
002820
002821     
      * EXEC CICS HANDLE CONDITION
002822*        ENDFILE  (7250-ENDFILE)
002823*        NOTFND   (7250-ENDFILE)
002824*        ERROR    (9990-ABEND)
002825*    END-EXEC.
      *    MOVE '"$''I.                 ! - #00006538' TO DFHEIV0
           MOVE X'222427492E20202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303036353338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002826
002827     PERFORM 7850-START-BROWSE  THRU  7850-EXIT.
002828
002829 7250-READ-NEXT.
002830     
      * EXEC CICS HANDLE CONDITION
002831*        ENDFILE  (7250-ENDFILE)
002832*        NOTFND   (7275-NOTFOUND)
002833*    END-EXEC.
      *    MOVE '"$''I                  ! . #00006547' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303036353437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002834
002835     PERFORM 7900-READNEXT  THRU  7900-EXIT.
002836
002837     IF PI-CARRIER-SECURITY > SPACES
002838         IF PI-ERC-CARRIER = PI-CARRIER-SECURITY
002839             NEXT SENTENCE
002840         ELSE
002841             GO TO 7250-READ-NEXT.
002842
002843     IF ERCOMP-EOF
002844         IF FIRST-TIME
002845             MOVE LOW-VALUES     TO  EL652AO
002846             MOVE ER-0584        TO  EMI-ERROR
002847             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002848             GO TO 8100-SEND-INITIAL-MAP
002849         ELSE
002850             MOVE LOW-VALUES     TO  EL652AO
002851             MOVE ER-2067        TO  EMI-ERROR
002852             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002853             PERFORM 7950-END-BROWSE  THRU  7950-EXIT
002854             GO TO 7250-PAGE-FORWARD.
002855
002856     IF PI-ERCOMP-KEY = WS-SAVE-KEY
002857         GO TO 7250-READ-NEXT.
002858
002859     MOVE CO-LAST-MAINT-USER     TO  PI-UPDATE-BY.
002860     MOVE CO-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
002861     MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.
002862     MOVE LOW-VALUES             TO  EL652AO.
002863     MOVE CO-CARRIER             TO  CARRIERO.
002864     MOVE CO-GROUPING            TO  GROUPO.
002865     MOVE CO-RESP-NO             TO  FINRESPO.
002866     MOVE CO-ACCOUNT             TO  ACCTNOO.
002867     MOVE CO-TYPE                TO  TYPEO.
002868     MOVE AL-UANON               TO  CARRIERA
002869                                     GROUPA
002870                                     TYPEA
002871                                     FINRESPA
002872                                     ACCTNOA
002873                                     MAINTYPA.
002874     MOVE 'S'                    TO  MAINTYPO
002875                                     PI-CHECK-MAINT-TYPE.
002876
002877     GO TO 5050-SET-UP-SCREEN.
002878
002879 7250-ENDFILE.
002880     IF FIRST-TIME
002881         MOVE LOW-VALUES         TO  EL652AO
002882         MOVE ER-0584            TO  EMI-ERROR
002883         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002884         GO TO 8100-SEND-INITIAL-MAP.
002885
002886     IF BROWSE-STARTED
002887         PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
002888
002889     MOVE ER-2067                TO  EMI-ERROR.
002890     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002891
002892     MOVE LOW-VALUES             TO  EL652AO.
002893
002894     GO TO 7250-PAGE-FORWARD.
002895
002896 7275-NOTFOUND.
002897     IF BROWSE-STARTED
002898         PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
002899
002900     GO TO 8880-NOT-FOUND.
002901
002902 7299-EXIT.
002903     EXIT.
002904 EJECT
002905 7300-PAGE-BACKWARD.
002906     MOVE SPACES                 TO  PI-ERCOMP-EOF-SW.
002907
002908     IF MAINTYPL > ZERO
002909         MOVE GROUPI             TO  PI-ERC-GROUP
002910         MOVE CARRIERI           TO  PI-ERC-CARRIER
002911         MOVE FINRESPI           TO  PI-ERC-RESP
002912         MOVE ACCTNOI            TO  PI-ERC-ACCT
002913         MOVE TYPEI              TO  PI-ERC-TYPE
002914     ELSE
002915         MOVE LOW-VALUES         TO  PI-ERCOMP-KEY.
002916
002917     MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
002918
002919     
      * EXEC CICS HANDLE CONDITION
002920*        ENDFILE  (7375-ENDFILE)
002921*        NOTFND   (7375-ENDFILE)
002922*        ERROR    (9990-ABEND)
002923*    END-EXEC.
      *    MOVE '"$''I.                 ! / #00006636' TO DFHEIV0
           MOVE X'222427492E20202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303036363336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002924
002925     PERFORM 7850-START-BROWSE  THRU  7850-EXIT.
002926
002927     PERFORM 8050-READPREV  THRU  8050-EXIT.
002928
002929 7350-READ-PREV.
002930     
      * EXEC CICS HANDLE CONDITION
002931*        ENDFILE  (7375-ENDFILE)
002932*        NOTFND   (7275-NOTFOUND)
002933*    END-EXEC.
      *    MOVE '"$''I                  ! 0 #00006647' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303036363437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002934
002935     PERFORM 8050-READPREV  THRU  8050-EXIT.
002936
002937     IF PI-CARRIER-SECURITY > SPACES
002938         IF PI-ERC-CARRIER = PI-CARRIER-SECURITY
002939             NEXT SENTENCE
002940         ELSE
002941             GO TO 7350-READ-PREV.
002942
002943     IF PI-COMPANY-CD NOT = CO-COMPANY-CD
002944         GO TO 7375-ENDFILE.
002945
002946     MOVE CO-LAST-MAINT-USER     TO  PI-UPDATE-BY.
002947     MOVE CO-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
002948     MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.
002949     MOVE LOW-VALUES             TO  EL652AO.
002950     MOVE CO-CARRIER             TO  CARRIERO.
002951     MOVE CO-GROUPING            TO  GROUPO.
002952     MOVE CO-RESP-NO             TO  FINRESPO.
002953     MOVE CO-ACCOUNT             TO  ACCTNOO.
002954     MOVE CO-TYPE                TO  TYPEO.
002955     MOVE AL-UANON               TO  CARRIERA
002956                                     GROUPA
002957                                     TYPEA
002958                                     FINRESPA
002959                                     ACCTNOA
002960                                     MAINTYPA.
002961     MOVE 'S'                    TO  MAINTYPO
002962                                     PI-CHECK-MAINT-TYPE.
002963
002964     GO TO 5050-SET-UP-SCREEN.
002965
002966 7375-ENDFILE.
002967     IF BROWSE-STARTED
002968         PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
002969
002970     MOVE ER-2238                TO  EMI-ERROR.
002971     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002972
002973     MOVE -1                     TO  MAINTYPL.
002974
002975     GO TO 8200-SEND-DATAONLY.
002976
002977 EJECT
002978 7400-READ-CONTROL-FILE.
002979     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
002980     MOVE WS-ACCESS              TO  CNTL-ACCESS.
002981
002982     
      * EXEC CICS HANDLE CONDITION
002983*        NOTFND  (7490-NOT-FOUND)
002984*        ERROR   (9990-ABEND)
002985*    END-EXEC.
      *    MOVE '"$I.                  ! 1 #00006699' TO DFHEIV0
           MOVE X'2224492E2020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303036363939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002986
002987     
      * EXEC CICS READ
002988*        DATASET  (CNTL-FILE-ID)
002989*        SET      (ADDRESS OF CONTROL-FILE)
002990*        RIDFLD   (ELCNTL-KEY)
002991*    END-EXEC.
      *    MOVE '&"S        E          (   #00006704' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036373034' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002992
002993     IF CNTL-REC-TYPE = '6'
002994         MOVE AL-UANON           TO  CARRIERA
002995         MOVE CARRIERI           TO  PI-ERC-CARRIER
002996         GO TO 7499-EXIT.
002997
002998     IF CF-COMPENSATION-MSTR-MAINT-DT NOT = LOW-VALUES
002999         GO TO 7499-EXIT
003000     ELSE
003001         MOVE -1                 TO  MAINTYPL
003002         MOVE ER-2572            TO  EMI-ERROR
003003         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
003004         GO TO 7499-EXIT.
003005
003006 7490-NOT-FOUND.
003007     IF CNTL-REC-TYPE = '6'
003008         MOVE -1                 TO  CARRIERL
003009         MOVE AL-UABON           TO  CARRIERA
003010         MOVE ER-0193            TO  EMI-ERROR
003011     ELSE
003012         IF CNTL-REC-TYPE = '2'
003013             MOVE -1                TO  CSRL
003014             MOVE AL-UABON          TO  CSRA
003015             MOVE ER-1883           TO  EMI-ERROR
003016         ELSE
003017            MOVE ER-0002            TO  EMI-ERROR
003018            MOVE -1                 TO  PFKEYL.
003019
003020     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
003021
003022 7499-EXIT.
003023     EXIT.
003024 EJECT
003025
003026 7500-UPDATE-SUMM.
003027
003028     PERFORM 7700-ERSUMM-GETMAIN  THRU  7700-EXIT.
003029
003030     MOVE 'SX'                   TO SX-RECORD-ID.
003031     MOVE PI-COMPANY-CD          TO SX-COMPANY-CD.
003032     MOVE PI-AR-SUMMARY-CODE     TO SX-SUMMARY.
003033     MOVE PI-ERC-CARRIER         TO SX-CARRIER.
003034     MOVE PI-ERC-GROUP           TO SX-GROUP.
003035     MOVE PI-ERC-RESP            TO SX-FIN-RESP.
003036     MOVE PI-ERC-ACCT            TO SX-ACCT-AGENT.
003037     MOVE WS-SAVE-NAME           TO SX-SUMM-OR-AGT-NAME.
003038     MOVE PI-PROCESSOR-ID        TO SX-LAST-MAINT-BY.
003039     MOVE EIBTIME                TO SX-LAST-MAINT-HHMMSS.
003040     MOVE BIN-CURRENT-SAVE       TO SX-LAST-MAINT-DT.
003041     MOVE PI-COMPANY-CD          TO SX-COMPANY-A1.
003042     MOVE PI-ERC-ACCT            TO SX-ACCT-AGENT-A1.
003043     MOVE PI-AR-SUMMARY-CODE     TO SX-SUMMARY-A1.
003044     MOVE PI-ERC-CARRIER         TO SX-CARR-A1.
003045     MOVE PI-ERC-GROUP           TO SX-GROUP-A1.
003046     MOVE PI-ERC-RESP            TO SX-FIN-RESP-A1.
003047
003048     
      * EXEC CICS HANDLE CONDITION
003049*        DUPREC  (7550-DUP)
003050*    END-EXEC.
      *    MOVE '"$%                   ! 2 #00006765' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303036373635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003051
003052     
      * EXEC CICS WRITE
003053*        DATASET  (SUMM-FILE-ID)
003054*        FROM     (SUMM-CROSS-REFERENCE)
003055*        RIDFLD   (SX-CONTROL-PRIMARY)
003056*    END-EXEC.
           MOVE LENGTH OF
            SUMM-CROSS-REFERENCE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006769' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036373639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 SUMM-CROSS-REFERENCE, 
                 DFHEIV11, 
                 SX-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003057
003058     MOVE LOW-VALUES             TO ERSUMM-KEY.
003059     MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.
003060     MOVE PI-AR-SUMMARY-CODE     TO SUMM-SUMMARY.
003061
003062     
      * EXEC CICS HANDLE CONDITION
003063*        NOTFND  (7540-WRITE)
003064*    END-EXEC.
      *    MOVE '"$I                   ! 3 #00006779' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3320233030303036373739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003065
003066     
      * EXEC CICS READ
003067*        DATASET  (SUMM-FILE-ID)
003068*        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
003069*        RIDFLD   (ERSUMM-KEY)
003070*    END-EXEC.
      *    MOVE '&"S        E          (   #00006783' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303036373833' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003071     GO TO 7599-EXIT.
003072
003073 7540-WRITE.
003074
003075     MOVE ERSUMM-KEY             TO SX-CONTROL-PRIMARY.
003076     MOVE LOW-VALUES             TO SX-CONTROL-A1.
003077     MOVE PI-COMPANY-CD          TO SX-COMPANY-A1.
003078     MOVE PI-AR-SUMMARY-CODE     TO SX-SUMMARY-A1.
003079     MOVE SPACES                 TO SX-SUMM-OR-AGT-NAME.
003080     MOVE 'LGXX'                 TO SX-LAST-MAINT-BY.
003081
003082     
      * EXEC CICS WRITE
003083*        DATASET  (SUMM-FILE-ID)
003084*        FROM     (SUMM-CROSS-REFERENCE)
003085*        RIDFLD   (SX-CONTROL-PRIMARY)
003086*    END-EXEC.
           MOVE LENGTH OF
            SUMM-CROSS-REFERENCE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006799' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036373939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 SUMM-CROSS-REFERENCE, 
                 DFHEIV11, 
                 SX-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003087
003088     MOVE PI-ERC-CARRIER         TO PI-CR-CARRIER.
003089     MOVE PI-ERC-GROUP           TO PI-CR-GROUPING.
003090     MOVE PI-ERC-TYPE            TO PI-CR-TYPE.
003091     MOVE PI-ERC-RESP            TO PI-CR-FIN-RESP.
003092     MOVE PI-ERC-ACCT            TO PI-CR-ACCOUNT.
003093
003094     MOVE XCTL-856               TO PGM-NAME.
003095     GO TO 9300-XCTL.
003096
003097 7550-DUP.
003098
003099     MOVE ER-3152                TO EMI-ERROR.
003100     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
003101
003102     MOVE -1                     TO MAINTYPL.
003103
003104     GO TO 8200-SEND-DATAONLY.
003105
003106 7599-EXIT.
003107     EXIT.
003108 EJECT
003109
003110 7600-DELETE-SUMM.
003111
003112     MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.
003113     MOVE WS-SAVE-SUMM           TO SUMM-SUMMARY.
003114     MOVE PI-ERC-CARRIER         TO SUMM-CARRIER.
003115     MOVE PI-ERC-GROUP           TO SUMM-GROUP.
003116     MOVE PI-ERC-RESP            TO SUMM-FIN-RESP.
003117     MOVE PI-ERC-ACCT            TO SUMM-ACCT-AGENT.
003118
003119 7620-READ-ERSUMM.
003120     
      * EXEC CICS HANDLE CONDITION
003121*        NOTFND  (7699-EXIT)
003122*    END-EXEC.
      *    MOVE '"$I                   ! 4 #00006837' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3420233030303036383337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003123
003124     
      * EXEC CICS READ
003125*        DATASET  (SUMM-FILE-ID)
003126*        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
003127*        RIDFLD   (ERSUMM-KEY)
003128*        UPDATE
003129*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006841' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036383431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003130
003131     
      * EXEC CICS DELETE
003132*        DATASET  (SUMM-FILE-ID)
003133*    END-EXEC.
      *    MOVE '&(                    &   #00006848' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303036383438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003134
003135     MOVE LOW-VALUES             TO ERSUMM-KEY.
003136     MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.
003137     MOVE WS-SAVE-SUMM           TO SUMM-SUMMARY.
003138
003139     
      * EXEC CICS STARTBR
003140*        DATASET  (SUMM-FILE-ID)
003141*        RIDFLD   (ERSUMM-KEY)
003142*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006856' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303036383536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003143
003144     
      * EXEC CICS READNEXT
003145*        DATASET  (SUMM-FILE-ID)
003146*        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
003147*        RIDFLD   (ERSUMM-KEY)
003148*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006861' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036383631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003149
003150     IF SX-CARRIER = LOW-VALUES
003151         NEXT SENTENCE
003152     ELSE
003153         GO TO 7640-END-BROWSE.
003154
003155     
      * EXEC CICS HANDLE CONDITION
003156*        ENDFILE  (7630-DELETE)
003157*    END-EXEC.
      *    MOVE '"$''                   ! 5 #00006872' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'3520233030303036383732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003158
003159     
      * EXEC CICS READNEXT
003160*        DATASET  (SUMM-FILE-ID)
003161*        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
003162*        RIDFLD   (ERSUMM-KEY)
003163*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006876' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036383736' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003164
003165     IF SX-SUMMARY = WS-SAVE-SUMM
003166         GO TO 7640-END-BROWSE.
003167
003168 7630-DELETE.
003169     
      * EXEC CICS ENDBR
003170*        DATASET  (SUMM-FILE-ID)
003171*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006886' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036383836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003172
003173     MOVE LOW-VALUES             TO ERSUMM-KEY.
003174     MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.
003175     MOVE WS-SAVE-SUMM           TO SUMM-SUMMARY.
003176
003177     
      * EXEC CICS READ
003178*        DATASET  (SUMM-FILE-ID)
003179*        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
003180*        RIDFLD   (ERSUMM-KEY)
003181*        UPDATE
003182*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006894' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036383934' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003183
003184     
      * EXEC CICS DELETE
003185*        DATASET  (SUMM-FILE-ID)
003186*    END-EXEC.
      *    MOVE '&(                    &   #00006901' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303036393031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003187
003188     GO TO 7699-EXIT.
003189
003190 7640-END-BROWSE.
003191     
      * EXEC CICS ENDBR
003192*        DATASET  (SUMM-FILE-ID)
003193*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006908' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036393038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003194
003195 7699-EXIT.
003196     EXIT.
003197 EJECT
003198
003199 7700-ERSUMM-GETMAIN.
003200     
      * EXEC CICS GETMAIN
003201*        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
003202*        LENGTH   (ERSUMM-LENGTH)
003203*        INITIMG  (GETMAIN-SPACE)
003204*    END-EXEC.
      *    MOVE ',"IL                  $   #00006917' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036393137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERSUMM-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003205
003206 7700-EXIT.
003207     EXIT.
003208 EJECT
003209
003210 7850-START-BROWSE.
003211     
      * EXEC CICS STARTBR
003212*        DATASET  (COMP-FILE-ID)
003213*        RIDFLD   (PI-ERCOMP-KEY)
003214*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006928' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303036393238' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003215
003216     MOVE 'Y'                    TO  WS-BROWSE-SW.
003217
003218 7850-EXIT.
003219     EXIT.
003220 EJECT
003221 7900-READNEXT.
003222     
      * EXEC CICS READNEXT
003223*        DATASET  (COMP-FILE-ID)
003224*        SET      (ADDRESS OF COMPENSATION-MASTER)
003225*        RIDFLD   (PI-ERCOMP-KEY)
003226*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006939' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303036393339' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003227
003228     IF PI-COMPANY-CD NOT = CO-COMPANY-CD
003229         MOVE 'Y'                TO  PI-ERCOMP-EOF-SW
003230     ELSE
003231         MOVE SPACE              TO  PI-FIRST-TIME-SW.
003232
003233 7900-EXIT.
003234     EXIT.
003235 EJECT
003236 7950-END-BROWSE.
003237     
      * EXEC CICS ENDBR
003238*        DATASET  (COMP-FILE-ID)
003239*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006954' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036393534' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003240
003241     MOVE 'N'                    TO  WS-BROWSE-SW.
003242
003243 7950-EXIT.
003244     EXIT.
003245 EJECT
003246 8000-UPDATE-MAINT-DATE.
003247     MOVE SPACES                 TO  ELCNTL-KEY.
003248     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
003249     MOVE '1'                    TO  CNTL-REC-TYPE.
003250     MOVE +0                     TO  CNTL-SEQ-NO.
003251
003252     
      * EXEC CICS HANDLE CONDITION
003253*        NOTFND  (8000-EXIT)
003254*    END-EXEC.
      *    MOVE '"$I                   ! 6 #00006969' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3620233030303036393639' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003255
003256     
      * EXEC CICS READ
003257*        UPDATE
003258*        DATASET  (CNTL-FILE-ID)
003259*        SET      (ADDRESS OF CONTROL-FILE)
003260*        RIDFLD   (ELCNTL-KEY)
003261*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006973' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036393733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
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
003263     MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
003264     MOVE 'B'                    TO  JP-RECORD-TYPE.
003265     MOVE CNTL-FILE-ID           TO  FILE-ID.
003266
003267     PERFORM 8400-LOG-JOURNAL-RECORD.
003268
003269     MOVE BIN-CURRENT-SAVE  TO  CF-COMPENSATION-MSTR-MAINT-DT.
003270     MOVE CONTROL-FILE      TO  JP-RECORD-AREA.
003271     MOVE 'C'               TO  JP-RECORD-TYPE.
003272     MOVE CNTL-FILE-ID      TO  FILE-ID.
003273
003274     
      * EXEC CICS REWRITE
003275*        DATASET  (CNTL-FILE-ID)
003276*        FROM     (CONTROL-FILE)
003277*    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006991' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036393931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003278
003279     PERFORM 8400-LOG-JOURNAL-RECORD.
003280
003281 8000-EXIT.
003282      EXIT.
003283 EJECT
003284 8050-READPREV.
003285     
      * EXEC CICS READPREV
003286*        DATASET  (COMP-FILE-ID)
003287*        SET      (ADDRESS OF COMPENSATION-MASTER)
003288*        RIDFLD   (PI-ERCOMP-KEY)
003289*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00007002' TO DFHEIV0
           MOVE X'263053202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303037303032' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003290
003291 8050-EXIT.
003292     EXIT.
003293 EJECT
003294 8100-SEND-INITIAL-MAP.
003295
003296     IF  NOT CREDIT-SESSION
003297         MOVE AL-SADOF           TO PFK5A
003298                                    PFK6A.
003299
003300     IF PI-COMPANY-ID NOT = 'NCL'
003301         MOVE AL-SANOF           TO  LETRCDA
003302         MOVE AL-SADOF           TO  LETDESCA.
003303
003304     IF PI-COMPANY-ID  NOT = 'DDB' AND 'ANT' AND 'ASL' AND
003305                             'AN1' AND 'TFS'
003306         MOVE AL-SANOF           TO  BALCDA
003307         MOVE AL-SADOF           TO  BALPRTA.
003308
003309     IF PI-COMPANY-ID   = 'NCL'
003310        IF TYPEI =  'G' OR 'B'
003311           MOVE AL-UNNOF         TO  RPTCD2A
003312           MOVE AL-SANOF         TO  RPTCDDA.
003313
003314     IF PI-COMPANY-ID = 'DCC' or 'VPP'
003315        IF TYPEI = 'A'
003316           MOVE AL-SANOF         TO SPPDDHA
003317           MOVE AL-UANON         TO SPPDDA
003318        END-IF
003319     END-IF
003320     MOVE AL-SANON               TO  FLITYPA.
003321
003322     IF EIBTRNID NOT = TRANS-ID
003323        IF PI-AR-PROCESSING
003324             MOVE AL-SANOF       TO  SCDESCA
003325                                     NGDESCA
003326*                                    BALDESCA
003327                                     CKDESCA
003328             MOVE AL-UANON       TO  SUMMNOA
003329                                     NETGRSA
003330*                                    ARBALA
003331                                     CKPULLA.
003332
003333     IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'
003334         MOVE AL-UANOF           TO  LSTMDTA
003335         MOVE AL-SANOF           TO  PFK9A
003336     ELSE
003337         IF PI-AR-PROCESSING
003338             MOVE AL-SANOF       TO  PFK9A
003339         ELSE
003340             MOVE AL-SANOF       TO  LSTMDTA
003341             MOVE AL-SADOF       TO  PFK9A.
003342
003343*    IF (PI-ERC-TYPE NOT = 'G' AND SPACE)
003344*       OR (PI-COMPANY-ID NOT = 'DCC')
003345
003346     IF PI-ERC-TYPE = 'A'
003347        MOVE AL-SANOF            TO PCONTA
003348     END-IF
003349
003350     IF PI-ERC-TYPE NOT = 'B' AND SPACE
003351         MOVE AL-SADOF           TO  PFK10A
003352                                     PFK13A
003353         MOVE AL-SADOF           TO  CSLABLA
003354         MOVE AL-SADOF           TO  CLPSTA
003355         MOVE AL-SADOF           TO  MFLABLA
003356         MOVE AL-SADOF           TO  MAXFEEA
003357         MOVE AL-SADOF           TO  LFLABLA
003358         MOVE AL-SADOF           TO  MAXLFA
003359         MOVE AL-SADOF           TO  REFEHA
003360                                     REFEA
003361     END-IF
003362
003363     if pi-company-id = 'AHL'
003364        move al-sanof            to ahl120a
003365                                    over120a
003366     end-if
003367
003368     MOVE SAVE-DATE              TO  RUNDATEO.
003369     MOVE EIBTIME                TO  TIME-IN.
003370     MOVE TIME-OUT               TO  RUNTIMEO.
003371     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
003372     MOVE PI-PROCESSOR-ID        TO  USERIDO.
003373     MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
003374     MOVE -1                     TO  MAINTYPL.
003375
003376     
      * EXEC CICS SEND
003377*        MAP     (MAP-NAME)
003378*        MAPSET  (MAPSET-NAME)
003379*        FROM    (EL652AO)
003380*        ERASE
003381*        CURSOR
003382*    END-EXEC.
           MOVE LENGTH OF
            EL652AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007093' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303037303933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652AO, 
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
           
003383
003384     GO TO 9100-RETURN-TRAN.
003385
003386 8200-SEND-DATAONLY.
003387
003388     IF  NOT CREDIT-SESSION
003389         MOVE AL-SADOF           TO PFK5A
003390                                    PFK6A.
003391
003392     IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'
003393         MOVE AL-UANOF           TO  LSTMDTA
003394         MOVE AL-SANOF           TO  PFK9A
003395     ELSE
003396         IF PI-AR-PROCESSING
003397             MOVE AL-SANOF       TO  PFK9A
003398         ELSE
003399             MOVE AL-SANOF       TO  LSTMDTA
003400             MOVE AL-SADOF       TO  PFK9A.
003401
003402     IF PI-ERC-TYPE = 'A'
003403        MOVE AL-SANOF            TO PCONTA
003404     END-IF
003405
003406
003407     IF PI-COMPANY-ID = 'DCC' or 'VPP'
003408        IF TYPEI = 'A'
003409           MOVE AL-SANOF         TO SPPDDHA
003410           MOVE AL-UANON         TO SPPDDA
003411        END-IF
003412     END-IF
003413*    IF (PI-ERC-TYPE NOT = 'G')
003414*       OR (PI-COMPANY-ID NOT = 'DCC')
003415     IF PI-ERC-TYPE NOT = 'B'
003416         MOVE AL-SADOF           TO  PFK10A
003417                                     PFK13A
003418         MOVE AL-SADOF           TO  CSLABLA
003419         MOVE AL-SADOF           TO  CLPSTA
003420         MOVE AL-SADOF           TO  MFLABLA
003421         MOVE AL-SADOF           TO  MAXFEEA
003422         MOVE AL-SADOF           TO  LFLABLA
003423         MOVE AL-SADOF           TO  MAXLFA
003424     END-IF.
003425
003426     MOVE SAVE-DATE              TO  RUNDATEO.
003427     MOVE EIBTIME                TO  TIME-IN.
003428     MOVE TIME-OUT               TO  RUNTIMEO.
003429     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
003430     MOVE PI-PROCESSOR-ID        TO  USERIDO.
003431     MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
003432
003433     
      * EXEC CICS SEND
003434*        MAP     (MAP-NAME)
003435*        MAPSET  (MAPSET-NAME)
003436*        FROM    (EL652AO)
003437*        DATAONLY
003438*        CURSOR
003439*    END-EXEC.
           MOVE LENGTH OF
            EL652AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00007150' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303037313530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652AO, 
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
           
003440
003441     GO TO 9100-RETURN-TRAN.
003442
003443 8300-SEND-TEXT.
003444     
      * EXEC CICS SEND TEXT
003445*        FROM    (LOGOFF-TEXT)
003446*        LENGTH  (LOGOFF-LENGTH)
003447*        ERASE
003448*        FREEKB
003449*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007161' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303037313631' TO DFHEIV0
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
           
003450
003451     
      * EXEC CICS RETURN
003452*    END-EXEC.
      *    MOVE '.(                    ''   #00007168' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037313638' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003453
003454 8400-LOG-JOURNAL-RECORD.
003455     MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
003456     MOVE FILE-ID                TO  JP-FILE-ID.
003457     MOVE THIS-PGM               TO  JP-PROGRAM-ID.
003458
003459*    EXEC CICS JOURNAL
003460*        JFILEID  (PI-JOURNAL-FILE-ID)
003461*        JTYPEID  ('EL')
003462*        FROM     (JOURNAL-RECORD)
003463*        LENGTH   (473)
003464*    END-EXEC.
003465
003466 8800-UNAUTHORIZED-ACCESS.
003467     MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
003468
003469     GO TO 8300-SEND-TEXT.
003470
003471 8810-PF23.
003472     MOVE EIBAID                 TO  PI-ENTRY-CD-1.
003473     MOVE XCTL-005               TO  PGM-NAME.
003474
003475     GO TO 9300-XCTL.
003476
003477 8880-NOT-FOUND.
003478     MOVE ER-0142                TO  EMI-ERROR.
003479     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
003480
003481     MOVE SPACE                  TO  PI-ERC-TYPE.
003482     MOVE -1                     TO  MAINTYPL.
003483
003484     IF EIBTRNID NOT = TRANS-ID
003485         GO TO 8100-SEND-INITIAL-MAP.
003486
003487     GO TO 8200-SEND-DATAONLY.
003488
003489 9100-RETURN-TRAN.
003490     MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
003491     MOVE '652A'                 TO  PI-CURRENT-SCREEN-NO.
003492
003493     
      * EXEC CICS RETURN
003494*        TRANSID   (TRANS-ID)
003495*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
003496*        LENGTH    (PI-COMM-LENGTH)
003497*    END-EXEC.
      *    MOVE '.(CT                  ''   #00007210' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303037323130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003498
003499 9200-RETURN-MAIN-MENU.
003500
003501     IF  MORTGAGE-SESSION
003502         MOVE XCTL-EM626         TO PGM-NAME
003503     ELSE
003504         IF  CREDIT-SESSION
003505             MOVE XCTL-626       TO PGM-NAME
003506         ELSE
003507             MOVE XCTL-126       TO PGM-NAME.
003508
003509     GO TO 9300-XCTL.
003510
003511 9300-XCTL.
003512     
      * EXEC CICS XCTL
003513*        PROGRAM   (PGM-NAME)
003514*        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
003515*        LENGTH    (PI-COMM-LENGTH)
003516*    END-EXEC.
      *    MOVE '.$C                   %   #00007229' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037323239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003517
003518 9400-CLEAR.
003519     MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
003520
003521     GO TO 9300-XCTL.
003522
003523 9500-PF12.
003524     MOVE XCTL-010               TO  PGM-NAME.
003525
003526     GO TO 9300-XCTL.
003527
003528 9600-PGMID-ERROR.
003529     
      * EXEC CICS HANDLE CONDITION
003530*        PGMIDERR  (8300-SEND-TEXT)
003531*    END-EXEC.
      *    MOVE '"$L                   ! 7 #00007246' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'3720233030303037323436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003532
003533     MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
003534     MOVE ' '                    TO  PI-ENTRY-CD-1.
003535     MOVE XCTL-005               TO  PGM-NAME.
003536     MOVE PGM-NAME               TO  LOGOFF-PGM.
003537     MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
003538
003539     GO TO 9300-XCTL.
003540
003541 9700-LINK-DATE-CONVERT.
003542     
      * EXEC CICS LINK
003543*        PROGRAM   ('ELDATCV')
003544*        COMMAREA  (DATE-CONVERSION-DATA)
003545*        LENGTH    (DC-COMM-LENGTH)
003546*    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00007259' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037323539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003547
003548 9700-EXIT.
003549     EXIT.
003550
003551 9900-ERROR-FORMAT.
003552     IF NOT EMI-ERRORS-COMPLETE
003553         MOVE LINK-001           TO  PGM-NAME
003554         
      * EXEC CICS LINK
003555*            PROGRAM   (PGM-NAME)
003556*            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
003557*            LENGTH    (EMI-COMM-LENGTH)
003558*        END-EXEC.
      *    MOVE '."C                   (   #00007271' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037323731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003559
003560 9900-EXIT.
003561     EXIT.
003562
003563 9910-INITIALIZE-SECURITY.
003564******************************************************************
003565*                                                                *
003566*       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
003567*       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *
003568*       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
003569*       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
003570*       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
003571*       ERROR CONDITION AND EXITS THE PROGRAM.                   *
003572*                                                                *
003573******************************************************************
003574
003575     IF  PI-PROCESSOR-ID NOT = 'LGXX'
003576
003577         IF  MORTGAGE-SESSION
003578             MOVE '125E'             TO SC-QUID-SYSTEM
003579             MOVE EIBTRMID           TO SC-QUID-TERMINAL
003580
003581             
      * EXEC CICS READQ TS
003582*                QUEUE  (SC-QUID-KEY)
003583*                INTO   (SECURITY-CONTROL-E)
003584*                LENGTH (SC-COMM-LENGTH-E)
003585*                ITEM   (SC-ITEM)
003586*            END-EXEC
      *    MOVE '*$II   L              ''   #00007298' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037323938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SC-QUID-KEY, 
                 SECURITY-CONTROL-E, 
                 SC-COMM-LENGTH-E, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003587
003588             MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)
003589                                     TO PI-DISPLAY-CAP
003590             MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)
003591                                     TO PI-MODIFY-CAP
003592
003593             IF  NOT DISPLAY-CAP
003594                 MOVE 'READ'         TO SM-READ
003595                 PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
003596                 MOVE ER-9097        TO EMI-ERROR
003597                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003598                 GO TO 8100-SEND-INITIAL-MAP
003599             ELSE
003600                 GO TO 9910-EXIT
003601         ELSE
003602             
      * EXEC CICS  READQ TS
003603*                QUEUE   (PI-SECURITY-TEMP-STORE-ID)
003604*                INTO    (SECURITY-CONTROL)
003605*                LENGTH  (SC-COMM-LENGTH)
003606*                ITEM    (SC-ITEM-CL-CR)
003607*                END-EXEC
      *    MOVE '*$II   L              ''   #00007319' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303037333139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM-CL-CR, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003608
003609             MOVE SC-CREDIT-DISPLAY (05)
003610                                 TO PI-DISPLAY-CAP
003611             MOVE SC-CREDIT-UPDATE  (05)
003612                                 TO PI-MODIFY-CAP
003613
003614             IF  NOT DISPLAY-CAP
003615                 MOVE 'READ'     TO SM-READ
003616                 PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
003617                 MOVE ER-0070    TO  EMI-ERROR
003618                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003619                 GO TO 8100-SEND-INITIAL-MAP.
003620
003621 9910-EXIT.
003622     EXIT.
003623
003624 9990-ABEND.
003625     MOVE LINK-004               TO  PGM-NAME.
003626     MOVE DFHEIBLK               TO  EMI-LINE1.
003627
003628     
      * EXEC CICS LINK
003629*        PROGRAM   (PGM-NAME)
003630*        COMMAREA  (EMI-LINE1)
003631*        LENGTH    (72)
003632*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007345' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037333435' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003633
003634     GO TO 8200-SEND-DATAONLY.
003635
003636 9995-SECURITY-VIOLATION.
003637*           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00007372' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303037333732' TO DFHEIV0
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
003638 9995-EXIT.
003639      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL652' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 3400-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3450-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 9990-ABEND,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3475-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 9990-ABEND,
                     4250-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4300-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4750-NO-STATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6550-END-BROWSE,
                     6550-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 6550-END-BROWSE,
                     6550-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 6599-EXIT,
                     6599-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 7250-ENDFILE,
                     7250-ENDFILE,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 7250-ENDFILE,
                     7275-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7375-ENDFILE,
                     7375-ENDFILE,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 7375-ENDFILE,
                     7275-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 7490-NOT-FOUND,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 7550-DUP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 7540-WRITE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 7699-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 7630-DELETE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 8000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL652' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
