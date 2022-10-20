      *((program: EL6501.cl2))
000001 IDENTIFICATION DIVISION.
000002
000003 PROGRAM-ID. EL6501.
000004*              PROGRAM CONVERTED BY
000005*              COBOL CONVERSION AID PO 5785-ABJ
000006*              CONVERSION DATE 05/11/94 14:25:07.
000007*            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
000008*                            VMOD=2.060.
000009
000010*AUTHOR.        LOGIC,INC.
000011*               DALLAS, TEXAS.
000012
000013*DATE-COMPILED.
000014
000015*SECURITY.   *****************************************************
000016*            *                                                   *
000017*            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
000018*            *                                                   *
000019*            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
000020*            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
000021*            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
000022*            *                                                   *
000023*            *****************************************************
000024*
000025*REMARKS.
000026*        TRANSACTION - EXC5 - ACCOUNT MAINT (GENERAL)
000027******************************************************************
000028*                   C H A N G E   L O G
000029*
000030* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000031*-----------------------------------------------------------------
000032*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000033* EFFECTIVE    NUMBER
000034*-----------------------------------------------------------------
000035* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
000036*                                ADJUSTED MAP-R REDEFINES FILLER
000037* 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
000038*                          SMVA  ADD CLP TOL PCT CODE
000039* 102004                   PEMA  ADD STATUS OF CANCEL
000040* 092705  CR2005050300006  PEMA  ADD SPP LEASES
000041* 021506  CR2006021400002  PEMA  DARKEN DCC FIELDS FOR CID
000042* 052306  CR2006050800002  PEMA  ADD COMM TYPE J
000043* 111606  CR2002061800017  PEMA  FIX ERCOMP UPDATE ROUTINE
000044* 110706  CR2006071700002  PEMA  FIX NAME LOOK UP
000045* 031607                   PEMA  PER TRWA AND KLSC DEFAULT TO P
000046* 062707  CR2007010300002  PEMA  REMOVE LEV 5 RESTRICTION FOR DCC
000047* 071207                   PEMA  ADD NEW TYPE 'M'
000048* 080807  CR2006062900001  PEMA  AUTOMATE STEPS TO CANCEL ACCT
000049* 022808  CR2007083100002  PEMA  ADD 'F' ACCT STATUS
000050* 042809    2007070200004  PEMA  ADD EDIT FOR KY ACCOUNTS
000051* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
000052* 020210  CR2020010400006  PEMA  REMOVE CHANGES MADE ON 09/09
000053* 031811  CR2011012700001  PEMA  ADD 'S' ACCT STATUS
000054* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
000055* 080612  CR2012042700005  PEMA  ADD OVER 120 DAYS FOR AHL
000056* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
000057* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
000058* 070714    2014052800001  PEMA  correct read on erpdef for DCC
000059* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
000060* 021916  CR2014010900001  TANA  ADD ACCT STATUS D,L,R,P
000061* 102717  CR2017062000003  PEMA  Add comm cap edits
000062* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
000063******************************************************************
000064
000065 ENVIRONMENT DIVISION.
000066 DATA DIVISION.
000067 EJECT
000068 WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
000069 77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
000070 77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
000071                                   USAGE POINTER.
000072 77  FILLER  PIC X(32)  VALUE '********************************'.
000073 77  FILLER  PIC X(32)  VALUE '*    EL6501 WORKING STORAGE    *'.
000074 77  FILLER  PIC X(32)  VALUE '********* VMOD=2.060 ***********'.
000075 77  WS-LAST-DATE-RANGE-SW       PIC X    VALUE SPACES.
000076     88  ON-LAST-DATE-RANGE             VALUE 'Y'.
000077 77  WS-AUTO-CANCEL-SW           PIC X  VALUE ' '.
000078     88  AUTO-CANCEL-ACCOUNT            VALUE 'Y'.
000079 77  WS-DELAY-INT                PIC S9(7) VALUE +2.
000080 77  WS-CHANGE-ALL-CNT           PIC S999  VALUE +0.
000081
000082 01  WS-DEGUG-AREA.
000083     05  FILLER PIC X(20) VALUE '&&&&&& DEBUG &&&&&&&'.
000084     05  FILLER PIC X(20) VALUE '  PAGE NUMBER HERE  '.
000085     05  WS-PAGE-NUMBER PIC 99 VALUE ZEROS.
000086     05  FILLER PIC X(20) VALUE '  TOTAL LINES HERE  '.
000087     05  WS-TOTAL-LINES PIC 99 VALUE ZEROS.
000088     05  FILLER PIC X(20) VALUE ' LINE SELECTED HERE '.
000089     05  WS-LINE-SELECTED PIC 99 VALUE ZEROS.
000090     05  FILLER PIC X(20) VALUE '&&&&&& DEBUG &&&&&&&'.
000091
000092
000093 01  S1                          PIC S999   VALUE +0    COMP.
000094 01  WS-WORK-FIELD.
000095     12  WS-WORK-NUM                 PIC 9(5)    VALUE ZEROS.
000096 01  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.
000097 01  WS-DATE-AREA.
000098     12  SAVE-DATE               PIC X(8)    VALUE SPACES.
000099     12  SAVE-BIN-DATE           PIC XX      VALUE SPACES.
000100
000101***DMD CUSTOM CODE *****START
000102     12  WS-DMD-EXPR-DATE        PIC 9(8) COMP-3.
000103     12  WS-DMD-AGENT            PIC X(10).
000104     12  WS-YYYYMMDD             PIC X(8).
000105     12  WS-COMP-DATE REDEFINES WS-YYYYMMDD
000106                                 PIC 9(8).
000107***DMD CUSTOM CODE *****END
000108
000109 01  WS-ACCOUNT-NUM-AREA.
000110     12  WS-ACCOUNT-FILLER       PIC X(06).
000111     12  WS-ACCT-BRANCH-CD       PIC X(04).
000112
000113 01  WS-NAME-AREA.
000114     12  WS-NAME-1-4             PIC X(04).
000115     12  WS-NAME-FILLER          PIC X(26).
000116
000117 01  STANDARD-AREAS.
000118     12  WS-STATE-MAX-SW         PIC X       VALUE ' '.
000119         88  EXCEEDS-STATE-MAX      VALUE 'Y'.
000120     12  WS-RESPONSE             PIC S9(8)   COMP.
000121         88  RESP-NORMAL                  VALUE +00.
000122         88  RESP-NOTFND                  VALUE +13.
000123         88  RESP-DUPREC                  VALUE +14.
000124         88  RESP-DUPKEY                  VALUE +15.
000125         88  RESP-NOTOPEN                 VALUE +19.
000126         88  RESP-ENDFILE                 VALUE +20.
000127
000128     12  GETMAIN-SPACE           PIC X       VALUE SPACE.
000129     12  MAP-NAME                PIC X(8)    VALUE 'EL6501A'.
000130     12  MAPSET-NAME             PIC X(8)    VALUE 'EL6501S'.
000131     12  SCREEN-NUMBER           PIC X(4)    VALUE '650B'.
000132     12  TRANS-ID                PIC X(4)    VALUE 'EXC5'.
000133     12  THIS-PGM                PIC X(8)    VALUE 'EL6501'.
000134     12  PGM-NAME                PIC X(8).
000135     12  TIME-IN                 PIC S9(7).
000136     12  TIME-OUT-R REDEFINES TIME-IN.
000137         16  FILLER              PIC X.
000138         16  TIME-OUT            PIC 99V99.
000139         16  FILLER              PIC XX.
000140     12  XCTL-005                PIC X(8)    VALUE 'EL005'.
000141     12  XCTL-010                PIC X(8)    VALUE 'EL010'.
000142     12  XCTL-126                PIC X(8)    VALUE 'EL126'.
000143     12  XCTL-601                PIC X(8)    VALUE 'EL601'.
000144     12  XCTL-608                PIC X(8)    VALUE 'EL608'.
000145     12  XCTL-626                PIC X(8)    VALUE 'EL626'.
000146     12  XCTL-650                PIC X(8)    VALUE 'EL650'.
000147     12  XCTL-6502               PIC X(8)    VALUE 'EL6502'.
000148     12  XCTL-6503               PIC X(8)    VALUE 'EL6503'.
000149     12  XCTL-6504               PIC X(8)    VALUE 'EL6504'.
000150     12  XCTL-6505               PIC X(8)    VALUE 'EL6505'.
000151     12  XCTL-6506               PIC X(8)    VALUE 'EL6506'.
000152     12  XCTL-652                PIC X(8)    VALUE 'EL652'.
000153     12  XCTL-689                PIC X(8)    VALUE 'EL689'.
000154     12  XCTL-690                PIC X(8)    VALUE 'EL690'.
000155     12  XCTL-653                PIC X(8)    VALUE 'EL653'.
000156     12  LINK-001                PIC X(8)    VALUE 'EL001'.
000157     12  LINK-004                PIC X(8)    VALUE 'EL004'.
000158     12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
000159     12  FILE-ID                 PIC X(8).
000160     12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.
000161     12  ACCT-FILE-ID            PIC X(8)    VALUE 'ERACCT'.
000162     12  ERACNT-FILE-ID          PIC X(8)    VALUE 'ERACNT'.
000163     12  REIN-FILE-ID            PIC X(8)    VALUE 'ERREIN'.
000164     12  COMM-FILE-ID            PIC X(8)    VALUE 'ERCTBL'.
000165     12  COMP-FILE-ID            PIC X(8)    VALUE 'ERCOMP'.
000166     12  NAME-FILE-ID            PIC X(8)    VALUE 'ERNAME'.
000167     12  RQST-FILE-ID            PIC X(8)    VALUE 'ERRQST'.
000168     12  RQST3-FILE-ID           PIC X(8)    VALUE 'ERRQST3'.
000169     12  ERGXRF-FILE-ID          PIC X(8)    VALUE 'ERGXRF'.
000170     12  ACCT-REC-LEN            PIC S9(4)   VALUE +2000  COMP.
000171     12  ERACNT-REC-LEN          PIC S9(4)   VALUE +120   COMP.
000172     12  SC-ITEM                 PIC S9(4)   VALUE +1     COMP.
000173     12  COMP-REC-LEN            PIC S9(4)   VALUE +700   COMP.
000174     12  NAME-REC-LEN            PIC S9(4)   VALUE +160   COMP.
000175     12  ERGXRF-REC-LEN          PIC S9(4)   VALUE +0     COMP.
000176     12  ERGXRF-INC-LEN          PIC S9(4)   VALUE +31    COMP.
000177     12  ERGXRF-MIN-LEN          PIC S9(4)   VALUE +109   COMP.
000178     12  ERGXRF-MAX-LEN          PIC S9(8)   VALUE +31264 COMP.
000179     12  CNTL-REC-LEN            PIC S9(4)   VALUE +750   COMP.
000180     12  JRNL-REC-LEN            PIC S9(4)   VALUE ZERO   COMP.
000181
000182*                                copy ERCGXRF.
      *>>((file: ERCGXRF))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCGXRF                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = GENERAL AGENT CROSS REFERENCE             *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 62 - 32,062   RECFORM = VARIABLE
000014*                                                                *
000015*   BASE CLUSTER NAME = ERGXRF                   RKP=2,LEN=18    *
000016*       ALTERNATE PATH = NONE                                    *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021******************************************************************
000022
000023 01  AGENT-CROSS-REFERENCE.
000024     12  GX-RECORD-ID                PIC XX.
000025         88  VALID-GX-ID             VALUE 'GX'.
000026
000027     12  GX-CONTROL-PRIMARY.
000028         16  GX-COMPANY-CD           PIC X.
000029         16  GX-CARRIER              PIC X.
000030         16  GX-GROUPING             PIC X(6).
000031         16  GX-AGENT-NO             PIC X(10).
000032
000033     12  GX-MAINT-INFORMATION.
000034         16  GX-LAST-MAINT-DT        PIC XX.
000035         16  GX-LAST-MAINT-HHMMSS    PIC S9(7)  COMP-3.
000036         16  GX-LAST-MAINT-USER      PIC X(4).
000037         16  FILLER                  PIC X(9).
000038
000039     12  FILLER                      PIC X(37).
000040
000041     12  GX-AGENT-POINTER-CNT        PIC S9(4)  COMP.
000042
000043     12  GX-AGENT-POINTER   OCCURS 1 TO 1006 TIMES
000044                            DEPENDING ON GX-AGENT-POINTER-CNT.
000045         16  GX-AM-CARRIER           PIC X.
000046         16  GX-AM-GROUPING          PIC X(6).
000047         16  GX-AM-STATE             PIC XX.
000048         16  GX-AM-ACCOUNT           PIC X(10).
000049         16  GX-AM-EXPIRATION-DT     PIC XX.
000050         16  GX-AM-LEVEL-NO          PIC S9(4)     COMP.
000051         16  GX-LAST-BILL-DT         PIC XX.
000052         16  GX-AM-EFF-DT            PIC XX.
000053         16  FILLER                  PIC X(4).
000054
000055******************************************************************
      *<<((file: ERCGXRF))
000183
000184 01  ERPDEF-KEY-SAVE             PIC X(18).
000185 01  ERPDEF-KEY.
000186     12  ERPDEF-COMPANY-CD       PIC X.
000187     12  ERPDEF-STATE            PIC XX.
000188     12  ERPDEF-PROD-CD          PIC XXX.
000189     12  F                       PIC X(7).
000190     12  ERPDEF-BEN-TYPE         PIC X.
000191     12  ERPDEF-BEN-CODE         PIC XX.
000192     12  ERPDEF-EXP-DT           PIC XX.
000193
000194 01  WORK-AREAS-AND-SWITCHES.
000195     12  ws-comp-cd-dis          pic s9(4) comp value +0.
000196     12  filler redefines ws-comp-cd-dis.
000197         16  filler              pic x.
000198         16  ws-comp-cd          pic x.
000199     12  W-ONE                   PIC S9(04)  COMP   VALUE +1.
000200     12  WS-TABLE-MAX            PIC S9V9(4) COMP-3 VALUE +0.
000201
000202
000203     12  WS-ST-COMMISSION-CAPS.
000204         16  WS-ST-COMM-CAP-SL     PIC S9V9(4) COMP-3 VALUE +0.
000205         16  WS-ST-COMM-CAP-JL     PIC S9V9(4) COMP-3 VALUE +0.
000206         16  WS-ST-COMM-CAP-SA     PIC S9V9(4) COMP-3 VALUE +0.
000207         16  WS-ST-COMM-CAP-JA     PIC S9V9(4) COMP-3 VALUE +0.
000208     12  WS-ST-GA-COMMISSION-CAPS.
000209         16  WS-ST-GA-COMM-CAP-SL  PIC S9V9(4) COMP-3 VALUE +0.
000210         16  WS-ST-GA-COMM-CAP-JL  PIC S9V9(4) COMP-3 VALUE +0.
000211         16  WS-ST-GA-COMM-CAP-SA  PIC S9V9(4) COMP-3 VALUE +0.
000212         16  WS-ST-GA-COMM-CAP-JA  PIC S9V9(4) COMP-3 VALUE +0.
000213     12  WS-ST-TOT-COMMISSION-CAPS.
000214         16  WS-ST-TOT-COMM-CAP-SL PIC S9V9(4) COMP-3 VALUE +0.
000215         16  WS-ST-TOT-COMM-CAP-JL PIC S9V9(4) COMP-3 VALUE +0.
000216         16  WS-ST-TOT-COMM-CAP-SA PIC S9V9(4) COMP-3 VALUE +0.
000217         16  WS-ST-TOT-COMM-CAP-JA PIC S9V9(4) COMP-3 VALUE +0.
000218
000219
000220     12  WS-DCC-MAX-MARKET-FEE   PIC S9(5)   COMP-3 VALUE +100.
000221     12  WS-COMM-CAP-LIMIT-TO    PIC X.
000222         88  WS-EXCLUDE-ACCOUNT              VALUE 'A'.
000223         88  WS-EXCLUDE-GA                   VALUE 'G'.
000224         88  WS-EXCLUDE-BOTH                 VALUE 'B'.
000225
000226     12  COMMISSION-ACCUMS.
000227         16  COMM-SL-ACCUM-AL    PIC S9V9(4) COMP-3 VALUE ZERO.
000228         16  COMM-JL-ACCUM-AL    PIC S9V9(4) COMP-3 VALUE ZERO.
000229         16  COMM-AH-ACCUM-AL    PIC S9V9(4) COMP-3 VALUE ZERO.
000230         16  COMM-SL-ACCUM-GL    PIC S9V9(4) COMP-3 VALUE ZERO.
000231         16  COMM-JL-ACCUM-GL    PIC S9V9(4) COMP-3 VALUE ZERO.
000232         16  COMM-AH-ACCUM-GL    PIC S9V9(4) COMP-3 VALUE ZERO.
000233         16  COMM-SL-ACCUM       PIC S9V9(4) COMP-3 VALUE ZERO.
000234         16  COMM-JL-ACCUM       PIC S9V9(4) COMP-3 VALUE ZERO.
000235         16  COMM-AH-ACCUM       PIC S9V9(4) COMP-3 VALUE ZERO.
000236         16  COMM-MFEE-ACCUM     PIC S9(5)   COMP-3 VALUE ZERO.
000237
000238     12  SAVE-FIN-RESP           PIC X(10)   VALUE SPACES.
000239     12  SAVE-ACCT-AGENT         PIC X(10)   VALUE SPACES.
000240     12  SAVE-NEW-ACCT-AGENT     PIC X(10)   VALUE SPACES.
000241     12  SUB1                    PIC S9(4)   VALUE +0 COMP.
000242     12  WS-NDX                  PIC S9(5)   VALUE +0 COMP-3.
000243     12  CURRENT-SAVE            PIC 9(8).
000244     12  BIN-CURRENT-SAVE        PIC XX.
000245     12  CYMD-CURRENT-SAVE       PIC 9(8).
000246     12  SUB                     PIC 99.
000247     12  AGT-SUB                 PIC 99      VALUE ZEROS.
000248     12  VALID-AGT-SW            PIC X       VALUE 'N'.
000249     12  CHANGE-WAS-MADE-SW      PIC X       VALUE 'N'.
000250         88  CHANGE-WAS-MADE                 VALUE 'Y'.
000251
000252     12  WS-EDIT-BEN-CODE        PIC XX.
000253         88  INVALID-BENEFIT-CODE   VALUE '  ' '00'
000254                                          '90' THRU '99'.
000255
000256     12  WS-KEY-SAVE.
000257           18  WS-ACCT-CCGSA-KEY.
000258             20  WS-ACCT-CO             PIC X.
000259             20  WS-ACCT-CARRIER        PIC X.
000260             20  WS-ACCT-GROUPING       PIC X(6).
000261             20  WS-ACCT-STATE          PIC XX.
000262             20  WS-ACCT-ACCOUNT        PIC X(10).
000263           18  WS-ACCT-EXP-DT           PIC XX.
000264           18  WS-ACCT-REST-OF-EXP      PIC X(4).
000265
000266     12  DEEDIT-FIELD                   PIC X(15).
000267     12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
000268     12  DEEDIT-FIELD-V1 REDEFINES DEEDIT-FIELD   PIC S9(14)V9.
000269     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.
000270     12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD   PIC S9(10)V9(5).
000271     12  DEEDIT-FIELD-V6 REDEFINES DEEDIT-FIELD   PIC S9(9)V9(6).
000272
000273     12  ELCNTL-KEY.
000274         16  CNTL-COMP-ID        PIC XXX.
000275         16  CNTL-REC-TYPE       PIC X.
000276         16  CNTL-ACCESS.
000277             20  FILLER          PIC X(4).
000278         16  CNTL-BUS REDEFINES CNTL-ACCESS.
000279             20  FILLER          PIC XX.
000280             20  CNTL-BUS-TYPE   PIC XX.
000281         16  CNTL-BEN REDEFINES CNTL-ACCESS.
000282             20  FILLER          PIC XX.
000283             20  CNTL-BEN-TYPE   PIC XX.
000284         16  CNTL-SEQ-NO     PIC S9(4)    COMP.
000285
000286     12  REIN-KEY.
000287         16  REIN-COMP-CD        PIC X.
000288         16  REIN-CODE           PIC X.
000289         16  REIN-TABLE          PIC X(3).
000290         16  FILLER              PIC X(3).
000291
000292     12  WS-HOLD-COMM-KEY        PIC X(7) VALUE LOW-VALUES.
000293     12  COMM-KEY.
000294         16  COMM-COMP-CD        PIC X.
000295         16  COMM-TABLE          PIC XXX.
000296         16  COMM-LF-AH          PIC X.
000297         16  COMM-FILLER         PIC XX    VALUE LOW-VALUES.
000298
000299     12  RQST-KEY.
000300         16  RQST-COMPANY-CD     PIC X     VALUE SPACES.
000301         16  RQST-ENTRY-BATCH    PIC X(6)  VALUE SPACES.
000302
000303     12  RQST3-KEY.
000304         16  RQST3-COMPANY-CD    PIC X     VALUE SPACES.
000305         16  RQST3-CARRIER       PIC X     VALUE SPACES.
000306         16  RQST3-GROUPING      PIC X(6)  VALUE SPACES.
000307         16  RQST3-FIN-RESP      PIC X(10) VALUE SPACES.
000308         16  RQST3-ACCT-AGENT    PIC X(10) VALUE SPACES.
000309         16  RQST3-REFERENCE     PIC X(12) VALUE SPACES.
000310         16  RQST3-BATCH         PIC X(6)  VALUE SPACES.
000311
000312     12  SAVE-RQST3-KEY.
000313         16  FILLER              PIC X     VALUE SPACES.
000314         16  FILLER              PIC X     VALUE SPACES.
000315         16  FILLER              PIC X(6)  VALUE SPACES.
000316         16  FILLER              PIC X(10) VALUE SPACES.
000317         16  FILLER              PIC X(10) VALUE SPACES.
000318         16  FILLER              PIC X(12) VALUE SPACES.
000319         16  FILLER              PIC X(6)  VALUE SPACES.
000320
000321     12  COMP-KEY.
000322         16  COMP-COMPANY-CD     PIC X.
000323         16  COMP-CARRIER        PIC X.
000324         16  COMP-GROUPING       PIC X(6).
000325         16  COMP-RESP-NO        PIC X(10).
000326         16  COMP-ACCOUNT        PIC X(10).
000327         16  COMP-TYPE           PIC X.
000328
000329     12  WS-AXRF-KEY.
000330         16  WS-AXRF-COMPANY-CD  PIC X.
000331         16  WS-AXRF-CARRIER     PIC X.
000332         16  WS-AXRF-GROUPING    PIC X(6).
000333         16  WS-AXRF-AGENT-NO    PIC X(10).
000334
000335     12  GX-AM-CONTROL-PRIMARY.
000336         16  GX-AM-COMPANY-CD    PIC X.
000337         16  GX-REST-OF-KEY      PIC X(20).
000338
000339     12  QID.
000340         16  QID-TERM            PIC X(4).
000341         16  FILLER              PIC X(4)    VALUE '6501'.
000342
000343     12  BROWSE-STARTED-SW       PIC X       VALUE ' '.
000344         88  BROWSE-STARTED         VALUE 'Y'.
000345     12  FIRST-TIME-SW           PIC X       VALUE ' '.
000346         88  FIRST-TIME             VALUE 'Y'.
000347
000348     12  AXRF-SUB                PIC S9(4)  COMP VALUE ZEROS.
000349     12  GX-AGENT-SUB            PIC S9(4)  COMP VALUE ZEROS.
000350     12  COMM-WORK-SUB           PIC S9(4)  COMP VALUE ZEROS.
000351     12  FIND-AGENT-SUB1         PIC S9(4)  COMP VALUE ZEROS.
000352     12  FIND-AGENT-SUB2         PIC S9(4)  COMP VALUE ZEROS.
000353     12  FIND-AGENT-SUB3         PIC S9(4)  COMP VALUE ZEROS.
000354     12  GETMAIN-SW              PIC X       VALUE SPACES.
000355     12  COMMISSION-GETMAIN-SW   PIC X       VALUE SPACES.
000356         88  GETMAIN-ACQUIRED                VALUE 'X'.
000357
000358     12  ER-0000                 PIC X(4)    VALUE '0000'.
000359     12  ER-0002                 PIC X(4)    VALUE '0002'.
000360     12  ER-0004                 PIC X(4)    VALUE '0004'.
000361     12  ER-0008                 PIC X(4)    VALUE '0008'.
000362     12  ER-0023                 PIC X(4)    VALUE '0023'.
000363     12  ER-0029                 PIC X(4)    VALUE '0029'.
000364     12  ER-0033                 PIC X(4)    VALUE '0033'.
000365     12  ER-0144                 PIC X(4)    VALUE '0144'.
000366     12  ER-0070                 PIC X(4)    VALUE '0070'.
000367     12  ER-0250                 PIC X(4)    VALUE '0250'.
000368     12  ER-0348                 PIC X(4)    VALUE '0348'.
000369     12  ER-0454                 PIC X(4)    VALUE '0454'.
000370     12  ER-0625                 PIC X(4)    VALUE '0625'.
000371     12  ER-0759                 PIC X(4)    VALUE '0759'.
000372     12  ER-0788                 PIC X(4)    VALUE '0788'.
000373     12  ER-0852                 PIC X(4)    VALUE '0852'.
000374     12  ER-0899                 PIC X(4)    VALUE '0899'.
000375     12  ER-1778                 PIC X(4)    VALUE '1778'.
000376     12  ER-1817                 PIC X(4)    VALUE '1817'.
000377     12  ER-1883                 PIC X(4)    VALUE '1883'.
000378     12  ER-1884                 PIC X(4)    VALUE '1884'.
000379     12  ER-1885                 PIC X(4)    VALUE '1885'.
000380     12  ER-1886                 PIC X(4)    VALUE '1886'.
000381     12  ER-1887                 PIC X(4)    VALUE '1887'.
000382     12  ER-1955                 PIC X(4)    VALUE '1955'.
000383     12  ER-1956                 PIC X(4)    VALUE '1956'.
000384     12  ER-1957                 PIC X(4)    VALUE '1957'.
000385     12  ER-1958                 PIC X(4)    VALUE '1958'.
000386     12  ER-1959                 PIC X(4)    VALUE '1959'.
000387     12  ER-1960                 PIC X(4)    VALUE '1960'.
000388     12  ER-1961                 PIC X(4)    VALUE '1961'.
000389     12  ER-1962                 PIC X(4)    VALUE '1962'.
000390     12  ER-1963                 PIC X(4)    VALUE '1963'.
000391     12  ER-2045                 PIC X(4)    VALUE '2045'.
000392     12  ER-2071                 PIC X(4)    VALUE '2071'.
000393     12  ER-2131                 PIC X(4)    VALUE '2131'.
000394*    12  ER-3057                 PIC X(4)    VALUE '3057'.
000395     12  ER-2151                 PIC X(4)    VALUE '2151'.
000396     12  ER-2152                 PIC X(4)    VALUE '2152'.
000397     12  ER-2153                 PIC X(4)    VALUE '2153'.
000398     12  ER-2154                 PIC X(4)    VALUE '2154'.
000399     12  ER-2155                 PIC X(4)    VALUE '2155'.
000400     12  ER-2156                 PIC X(4)    VALUE '2156'.
000401     12  ER-2157                 PIC X(4)    VALUE '2157'.
000402     12  ER-2158                 PIC X(4)    VALUE '2158'.
000403     12  ER-2159                 PIC X(4)    VALUE '2159'.
000404     12  ER-2160                 PIC X(4)    VALUE '2160'.
000405     12  ER-2165                 PIC X(4)    VALUE '2165'.
000406     12  ER-2168                 PIC X(4)    VALUE '2168'.
000407     12  ER-2169                 PIC X(4)    VALUE '2169'.
000408     12  ER-2170                 PIC X(4)    VALUE '2170'.
000409     12  ER-2172                 PIC X(4)    VALUE '2172'.
000410     12  ER-2173                 PIC X(4)    VALUE '2173'.
000411     12  ER-2174                 PIC X(4)    VALUE '2174'.
000412     12  ER-2175                 PIC X(4)    VALUE '2175'.
000413     12  ER-2176                 PIC X(4)    VALUE '2176'.
000414     12  ER-2177                 PIC X(4)    VALUE '2177'.
000415     12  ER-2178                 PIC X(4)    VALUE '2178'.
000416     12  ER-2179                 PIC X(4)    VALUE '2179'.
000417     12  ER-2180                 PIC X(4)    VALUE '2180'.
000418     12  ER-2181                 PIC X(4)    VALUE '2181'.
000419     12  ER-2182                 PIC X(4)    VALUE '2182'.
000420     12  ER-2183                 PIC X(4)    VALUE '2183'.
000421     12  ER-2189                 PIC X(4)    VALUE '2189'.
000422     12  ER-2572                 PIC X(4)    VALUE '2572'.
000423     12  ER-2615                 PIC X(4)    VALUE '2615'.
000424     12  ER-2949                 PIC X(4)    VALUE '2949'.
000425     12  ER-2970                 PIC X(4)    VALUE '2970'.
000426     12  ER-3065                 PIC X(4)    VALUE '3065'.
000427     12  ER-3066                 PIC X(4)    VALUE '3066'.
000428     12  ER-4009                 PIC X(4)    VALUE '4009'.
000429     12  ER-4010                 PIC X(4)    VALUE '4010'.
000430     12  ER-4011                 PIC X(4)    VALUE '4011'.
000431     12  ER-4012                 PIC X(4)    VALUE '4012'.
000432     12  ER-7717                 PIC X(4)    VALUE '7717'.
000433     12  er-8156                 pic x(4)    value '8156'.
000434     12  ER-9999                 PIC X(4)    VALUE '9999'.
000435     12  ER-XXXX                 PIC X(4)    VALUE 'XXXX'.
000436
000437     12  WSS-LINE-LIAB-LIMITS.
000438         16  WSS-LF-LIMITS.
000439             20  WSS-L-TYPE          PIC  XX    VALUE SPACES.
000440             20  WSS-LF-ATT-AGE      PIC S99    VALUE +0 COMP-3.
000441             20  WSS-L-LIMITS        OCCURS 4 TIMES.
000442                 24  WSS-LF-LM-AGE   PIC S99     COMP-3.
000443                 24  WSS-LF-LM-DUR   PIC S999    COMP-3.
000444                 24  WSS-LF-LM-AMT   PIC S9(6)   COMP-3.
000445         16  WSS-AH-LIMITS.
000446             20  WSS-A-TYPE          PIC  XX    VALUE SPACES.
000447             20  WSS-AH-ATT-AGE      PIC S99    VALUE +0 COMP-3.
000448             20  WSS-A-LIMITS        OCCURS 4 TIMES.
000449                 24  WSS-AH-LM-AGE   PIC S99     COMP-3.
000450                 24  WSS-AH-LM-DUR   PIC S999    COMP-3.
000451                 24  WSS-AH-LM-MOA   PIC S9(4)   COMP-3.
000452                 24  WSS-AH-LM-AMT   PIC S9(6)   COMP-3.
000453
000454     12  WSS-COMM-STRUCTURE.
000455         16  WSS-DEFN-1.
000456             20  WSS-AGT-COMMS.
000457                 24  WSS-AGT     PIC  X(10)   VALUE SPACES.
000458                 24  WSS-COM-TYP PIC  X       VALUE SPACES.
000459                 24  WSS-L-COM   PIC SV9(5)   VALUE ZEROS COMP-3.
000460                 24  WSS-J-COM   PIC SV9(5)   VALUE ZEROS COMP-3.
000461                 24  WSS-A-COM   PIC SV9(5)   VALUE ZEROS COMP-3.
000462                 24  FILLER      PIC X(6)     VALUE SPACES.
000463
000464     12  WS-PHONE.
000465         16  WS-PH1              PIC XXX.
000466         16  WS-PH2              PIC XXX.
000467         16  WS-PH3              PIC XXXX.
000468     12  WS-PHONE-NUM REDEFINES WS-PHONE PIC 9(10).
000469
000470     12  WS-LIFE-PSI             PIC S9V9(6)  VALUE +0    COMP-3.
000471     12  WS-AH-PSI               PIC S9V9(6)  VALUE +0    COMP-3.
000472     12  WS-ACCOUNT-CTR-C        PIC 99       VALUE ZEROS COMP-3.
000473     12  WS-ACCOUNT-CTR-R        PIC 99       VALUE ZEROS COMP-3.
000474     12  WS-ACCOUNT-CTR-D        PIC 99       VALUE ZEROS COMP-3.
000475     12  WS-BUS-TYPE             PIC 99.
000476     12  WS-BUS-ENTRY            PIC 99.
000477     12  WS-ANVR-DT              PIC 9(11)    VALUE ZEROS.
000478     12  WS-PEFF-DT              PIC 9(11)    VALUE ZEROS.
000479     12  WS-PEXP-DT              PIC 9(11)    VALUE ZEROS.
000480     12  WS-TOL-PREM             PIC S999V99  VALUE ZEROS.
000481     12  WS-TOL-REF              PIC S999V99  VALUE ZEROS.
000482     12  WS-TOL-CLM              PIC S999V99  VALUE ZEROS.
000483     12  WS-COMM-ERROR-SW        PIC 9   COMP-3.
000484         88  COMMISSION-ERROR    VALUE 1.
000485     12  WS-JTYPEID              PIC XX       VALUE SPACES.
000486     12  AGENT-FIND              PIC X        VALUE SPACES.
000487         88  AGENT-RECORD-NOT-FOUND           VALUE 'X'.
000488         88  ACCT-RECORD-NOT-FOUND            VALUE 'Y'.
000489         88  ACCT-LEVEL-DIFFERENT             VALUE 'Z'.
000490
000491     12  WS-ZIP-CODE.
000492         16  WS-ZIP-1            PIC X.
000493             88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
000494         16  WS-ZIP-2-3          PIC XX.
000495         16  WS-ZIP-4            PIC X.
000496         16  WS-ZIP-5            PIC X.
000497         16  WS-ZIP-6            PIC X.
000498         16  FILLER              PIC X(4).
000499     12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
000500         16  WS-ZIP-AM-1-CODE    PIC X(5).
000501         16  WS-ZIP-AM-1-PLUS4   PIC X(4).
000502         16  FILLER              PIC X.
000503     12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
000504         16  WS-ZIP-AM-2-CODE    PIC X(5).
000505         16  WS-ZIP-AM-2-DASH    PIC X.
000506         16  WS-ZIP-AM-2-PLUS4   PIC X(4).
000507     12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
000508         16  WS-ZIP-CAN-1-POST1  PIC XXX.
000509         16  WS-ZIP-CAN-1-POST2  PIC XXX.
000510         16  FILLER              PIC X(4).
000511     12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
000512         16  WS-ZIP-CAN-2-POST1  PIC XXX.
000513         16  FILLER              PIC X.
000514         16  WS-ZIP-CAN-2-POST2  PIC XXX.
000515         16  FILLER              PIC XXX.
000516     12  WS-SOC-SEC-WORK.
000517         16  WS-SS-TYPE          PIC X.
000518         16  WS-SOC-SEC          PIC X(12).
000519 EJECT
000520*    COPY ELCSCTM.
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
000521 EJECT
000522*    COPY ELCSCRTY.
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
000523 EJECT
000524*    COPY ELCLOGOF.
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
000525 EJECT
000526*    COPY ELCDATE.
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
000527 EJECT
000528*    COPY ELCATTR.
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
000529 EJECT
000530*    COPY ELCEMIB.
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
000531 EJECT
000532*    COPY ELCINTF.
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
000533*    COPY ELC650PI.
      *>>((file: ELC650PI))
000001******************************************************************
000002*
000003*                            ELC650PI.
000004*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000005*                            VMOD=2.007
000006*  ***  NOTE  ***   IF ANY CHANGES ARE MADE TO THIS COPYBOOK
000007*   YOU MUST CONSIDER ALL PROGRAMS THAT USE THIS COPYBOOK AND
000008* PROGRAM EL6565.  ALSO, CONSIDER EL106 AND EL1061
000009*
000010******************************************************************
000011******************************************************************
000012*                   C H A N G E   L O G
000013*
000014* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000015*-----------------------------------------------------------------
000016*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000017* EFFECTIVE    NUMBER
000018*-----------------------------------------------------------------
000019* 101916  IR2016101900001  PEMA  Inc tot line to 3 bytes
000020
000021     12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
000022         16  PI-MAINT                   PIC X.
000023         16  PI-PREV-ACCOUNT            PIC X(20).
000024         16  PI-PREV-VG-ACCOUNT         PIC X(20).
000025         16  PI-ACCT-KEY.
000026             20  PI-ACCT-CCGSA-KEY.
000027                 24  PI-ACCT-CO             PIC X.
000028                 24  PI-ACCT-CARRIER        PIC X.
000029                 24  PI-ACCT-GROUPING       PIC X(6).
000030                 24  PI-ACCT-STATE          PIC XX.
000031                 24  PI-ACCT-ACCOUNT        PIC X(10).
000032             20  PI-ACCT-EXP-DT           PIC XX.
000033             20  PI-ACCT-REST-OF-EXP      PIC X(4).
000034         16  PI-ACCT-ID                 PIC X(8).
000035         16  PI-PLAN-KEY.
000036             20  PI-PLAN-ACCT-KEY.
000037                 24  PI-PLAN-COMPANY-CD PIC X.
000038                 24  PI-PLAN-CARRIER    PIC X.
000039                 24  PI-PLAN-GROUP      PIC X(6).
000040                 24  PI-PLAN-STATE      PIC X(2).
000041                 24  PI-PLAN-ACCOUNT    PIC X(10).
000042             20  PI-PLAN-BEN-TYPE       PIC X.
000043             20  PI-PLAN-BEN            PIC XX.
000044             20  PI-PLAN-REVISION       PIC X(3).
000045         16  PI-WS-STATE                PIC XX.
000046         16  PI-WS-CLASS                PIC XX.
000047         16  PI-WS-DEV                  PIC X(3).
000048         16  PI-WS-TYPE                 PIC X.
000049         16  PI-WS-PLAN                 PIC XX.
000050
000051         16  PI-ERPNDB-ALT-KEY.
000052             20  PI-PB-COMPANY-CD-A1    PIC X.
000053             20  PI-PB-CARRIER          PIC X.
000054             20  PI-PB-GROUPING         PIC X(6).
000055             20  PI-PB-STATE            PIC XX.
000056             20  PI-PB-ACCOUNT          PIC X(10).
000057             20  PI-PB-CERT-EFF-DT      PIC XX.
000058             20  PI-PB-CERT-NO          PIC X(10).
000059             20  PI-PB-ALT-CHG-SEQ-NO   PIC S9(4)      COMP.
000060             20  PI-PB-RECORD-TYPE      PIC X.
000061
000062         16  PI-DATE-RANGE-TABLE.
000063             20  PI-TABLE-ENT OCCURS 32 TIMES
000064                            INDEXED BY T-INDEX.
000065                 24  PI-BIN-EFF-DT          PIC XX.
000066                 24  PI-BIN-EXP-DT          PIC XX.
000067                 24  PI-BIN-MAINT-DT        PIC XX.
000068                 24  PI-BIN-LO-CERT         PIC XX.
000069                 24  PI-BIN-AR-HI-CERT      PIC XX.
000070                 24  PI-BIN-HI-CERT         PIC XX.
000071         16  PI-PAGE-NUMBER             PIC S9.
000072             88  PI-FST-PAGE               VALUE +1.
000073             88  PI-2ND-PAGE               VALUE +2.
000074             88  PI-3RD-PAGE               VALUE +3.
000075             88  PI-LST-PAGE               VALUE +4.
000076         16  PI-TOTAL-LINES             PIC S999.
000077         16  PI-LINE-SELECTED    PIC S9.
000078***  Y2K PROJ 7744
000079         16  EFFCHG-SAVE         PIC 9(11)   COMP-3.
000080         16  BIN-EFFCHG-SAVE     PIC XX.
000081         16  EXPCHG-SAVE         PIC 9(11)   COMP-3.
000082***  Y2K PROJ 7744
000083         16  BIN-EXPCHG-SAVE     PIC XX.
000084         16  PI-RECORD-ADDED-SW  PIC X.
000085             88  PI-RECORD-ADDED            VALUE '1'.
000086             88  PI-RECORD-NOT-CREATED      VALUE SPACE.
000087         16  PI-ACCNAME          PIC X(30).
000088         16  PI-COMM-POINTER     PIC S9(8)   COMP.
000089         16  PI-SV-MAINT         PIC X.
000090         16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.
000091         16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.
000092         16  PI-UPDATE-SW        PIC X.
000093             88  PI-CHANGES-MADE             VALUE '1'.
000094         16  PI-NOTE-TYPE        PIC X.
000095             88  PI-ACCT-NOTE                VALUE '1'.
000096         16  PI-DMD-FILE-SW      PIC X.
000097             88  END-OF-FILE                 VALUE 'E'.
000098             88  INTO-NEXT-BENEFITS          VALUE 'I'.
000099             88  FIRST-OCCURS                VALUE 'F'.
000100         16  PI-DMD-OCCURS       PIC S999.
000101         16  PI-DMD-SCREEN       PIC X.
000102             88  SCREEN-1-DISPLAYED  VALUE '1'.
000103             88  SCREEN-2-DISPLAYED  VALUE '2'.
000104             88  SCREEN-3-DISPLAYED  VALUE '3'.
000105         16  PI-NAMEFLG          PIC X.
000106         16  PI-EL650-DEL-SW     PIC X.
000107         16  PI-MAX-MFEE         PIC S9(5) COMP-3.
000108         16  PI-DCC-PROD-CODE    PIC XXX.
000109         16  FILLER              PIC X(34).
000110     EJECT
      *<<((file: ELC650PI))
000534
000535*    COPY ELCJPFX.
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
000536         PIC X(2000).
000537 EJECT
000538*    COPY ELCAID.
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
000539 01  FILLER    REDEFINES DFHAID.
000540     12  FILLER                  PIC X(8).
000541     12  PF-VALUES               PIC X       OCCURS 24 TIMES.
000542 EJECT
000543*    COPY EL6501S.
      *>>((file: EL6501S))
000001 01  EL6501AI.
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
000028     05  MAINTL PIC S9(0004) COMP.
000029     05  MAINTF PIC  X(0001).
000030     05  FILLER REDEFINES MAINTF.
000031         10  MAINTA PIC  X(0001).
000032     05  MAINTI PIC  X(0001).
000033*    -------------------------------
000034     05  CARRL PIC S9(0004) COMP.
000035     05  CARRF PIC  X(0001).
000036     05  FILLER REDEFINES CARRF.
000037         10  CARRA PIC  X(0001).
000038     05  CARRI PIC  X(0001).
000039*    -------------------------------
000040     05  GROUPINL PIC S9(0004) COMP.
000041     05  GROUPINF PIC  X(0001).
000042     05  FILLER REDEFINES GROUPINF.
000043         10  GROUPINA PIC  X(0001).
000044     05  GROUPINI PIC  X(0006).
000045*    -------------------------------
000046     05  STATEL PIC S9(0004) COMP.
000047     05  STATEF PIC  X(0001).
000048     05  FILLER REDEFINES STATEF.
000049         10  STATEA PIC  X(0001).
000050     05  STATEI PIC  X(0002).
000051*    -------------------------------
000052     05  ACCOUNTL PIC S9(0004) COMP.
000053     05  ACCOUNTF PIC  X(0001).
000054     05  FILLER REDEFINES ACCOUNTF.
000055         10  ACCOUNTA PIC  X(0001).
000056     05  ACCOUNTI PIC  X(0010).
000057*    -------------------------------
000058     05  EFFDTEL PIC S9(0004) COMP.
000059     05  EFFDTEF PIC  X(0001).
000060     05  FILLER REDEFINES EFFDTEF.
000061         10  EFFDTEA PIC  X(0001).
000062     05  EFFDTEI PIC  X(0008).
000063*    -------------------------------
000064     05  EXPDTEL PIC S9(0004) COMP.
000065     05  EXPDTEF PIC  X(0001).
000066     05  FILLER REDEFINES EXPDTEF.
000067         10  EXPDTEA PIC  X(0001).
000068     05  EXPDTEI PIC  X(0008).
000069*    -------------------------------
000070     05  MAINTBYL PIC S9(0004) COMP.
000071     05  MAINTBYF PIC  X(0001).
000072     05  FILLER REDEFINES MAINTBYF.
000073         10  MAINTBYA PIC  X(0001).
000074     05  MAINTBYI PIC  X(0004).
000075*    -------------------------------
000076     05  PCONTL PIC S9(0004) COMP.
000077     05  PCONTF PIC  X(0001).
000078     05  FILLER REDEFINES PCONTF.
000079         10  PCONTA PIC  X(0001).
000080     05  PCONTI PIC  X(0030).
000081*    -------------------------------
000082     05  CSRL PIC S9(0004) COMP.
000083     05  CSRF PIC  X(0001).
000084     05  FILLER REDEFINES CSRF.
000085         10  CSRA PIC  X(0001).
000086     05  CSRI PIC  X(0004).
000087*    -------------------------------
000088     05  PDSPLYL PIC S9(0004) COMP.
000089     05  PDSPLYF PIC  X(0001).
000090     05  FILLER REDEFINES PDSPLYF.
000091         10  PDSPLYA PIC  X(0001).
000092     05  PDSPLYI PIC  X(0006).
000093*    -------------------------------
000094     05  PEFFDTEL PIC S9(0004) COMP.
000095     05  PEFFDTEF PIC  X(0001).
000096     05  FILLER REDEFINES PEFFDTEF.
000097         10  PEFFDTEA PIC  X(0001).
000098     05  PEFFDTEI PIC  X(0008).
000099*    -------------------------------
000100     05  PEXPDTEL PIC S9(0004) COMP.
000101     05  PEXPDTEF PIC  X(0001).
000102     05  FILLER REDEFINES PEXPDTEF.
000103         10  PEXPDTEA PIC  X(0001).
000104     05  PEXPDTEI PIC  X(0008).
000105*    -------------------------------
000106     05  NAMEL PIC S9(0004) COMP.
000107     05  NAMEF PIC  X(0001).
000108     05  FILLER REDEFINES NAMEF.
000109         10  NAMEA PIC  X(0001).
000110     05  NAMEI PIC  X(0030).
000111*    -------------------------------
000112     05  TAXNOL PIC S9(0004) COMP.
000113     05  TAXNOF PIC  X(0001).
000114     05  FILLER REDEFINES TAXNOF.
000115         10  TAXNOA PIC  X(0001).
000116     05  TAXNOI PIC  X(0011).
000117*    -------------------------------
000118     05  STATUSL PIC S9(0004) COMP.
000119     05  STATUSF PIC  X(0001).
000120     05  FILLER REDEFINES STATUSF.
000121         10  STATUSA PIC  X(0001).
000122     05  STATUSI PIC  X(0001).
000123*    -------------------------------
000124     05  INCAREL PIC S9(0004) COMP.
000125     05  INCAREF PIC  X(0001).
000126     05  FILLER REDEFINES INCAREF.
000127         10  INCAREA PIC  X(0001).
000128     05  INCAREI PIC  X(0030).
000129*    -------------------------------
000130     05  PHONEL PIC S9(0004) COMP.
000131     05  PHONEF PIC  X(0001).
000132     05  FILLER REDEFINES PHONEF.
000133         10  PHONEA PIC  X(0001).
000134     05  PHONEI PIC  X(0012).
000135*    -------------------------------
000136     05  INDGRPL PIC S9(0004) COMP.
000137     05  INDGRPF PIC  X(0001).
000138     05  FILLER REDEFINES INDGRPF.
000139         10  INDGRPA PIC  X(0001).
000140     05  INDGRPI PIC  X(0001).
000141*    -------------------------------
000142     05  ADDR1L PIC S9(0004) COMP.
000143     05  ADDR1F PIC  X(0001).
000144     05  FILLER REDEFINES ADDR1F.
000145         10  ADDR1A PIC  X(0001).
000146     05  ADDR1I PIC  X(0030).
000147*    -------------------------------
000148     05  CONTRL PIC S9(0004) COMP.
000149     05  CONTRF PIC  X(0001).
000150     05  FILLER REDEFINES CONTRF.
000151         10  CONTRA PIC  X(0001).
000152     05  CONTRI PIC  X(0008).
000153*    -------------------------------
000154     05  TYPEL PIC S9(0004) COMP.
000155     05  TYPEF PIC  X(0001).
000156     05  FILLER REDEFINES TYPEF.
000157         10  TYPEA PIC  X(0001).
000158     05  TYPEI PIC  99.
000159*    -------------------------------
000160     05  ACITYL PIC S9(0004) COMP.
000161     05  ACITYF PIC  X(0001).
000162     05  FILLER REDEFINES ACITYF.
000163         10  ACITYA PIC  X(0001).
000164     05  ACITYI PIC  X(0028).
000165*    -------------------------------
000166     05  ASTATEL PIC S9(0004) COMP.
000167     05  ASTATEF PIC  X(0001).
000168     05  FILLER REDEFINES ASTATEF.
000169         10  ASTATEA PIC  X(0001).
000170     05  ASTATEI PIC  X(0002).
000171*    -------------------------------
000172     05  PRODATEL PIC S9(0004) COMP.
000173     05  PRODATEF PIC  X(0001).
000174     05  FILLER REDEFINES PRODATEF.
000175         10  PRODATEA PIC  X(0001).
000176     05  PRODATEI PIC  X(0008).
000177*    -------------------------------
000178     05  STDBENL PIC S9(0004) COMP.
000179     05  STDBENF PIC  X(0001).
000180     05  FILLER REDEFINES STDBENF.
000181         10  STDBENA PIC  X(0001).
000182     05  STDBENI PIC  X(0002).
000183*    -------------------------------
000184     05  ZIPL PIC S9(0004) COMP.
000185     05  ZIPF PIC  X(0001).
000186     05  FILLER REDEFINES ZIPF.
000187         10  ZIPA PIC  X(0001).
000188     05  ZIPI PIC  X(0010).
000189*    -------------------------------
000190     05  CTPLABLL PIC S9(0004) COMP.
000191     05  CTPLABLF PIC  X(0001).
000192     05  FILLER REDEFINES CTPLABLF.
000193         10  CTPLABLA PIC  X(0001).
000194     05  CTPLABLI PIC  X(0010).
000195*    -------------------------------
000196     05  CLPTOLPL PIC S9(0004) COMP.
000197     05  CLPTOLPF PIC  X(0001).
000198     05  FILLER REDEFINES CLPTOLPF.
000199         10  CLPTOLPA PIC  X(0001).
000200     05  CLPTOLPI PIC  S9(02)V9999.
000201*    -------------------------------
000202     05  RETROCDL PIC S9(0004) COMP.
000203     05  RETROCDF PIC  X(0001).
000204     05  FILLER REDEFINES RETROCDF.
000205         10  RETROCDA PIC  X(0001).
000206     05  RETROCDI PIC  X(0003).
000207*    -------------------------------
000208     05  BILLCDL PIC S9(0004) COMP.
000209     05  BILLCDF PIC  X(0001).
000210     05  FILLER REDEFINES BILLCDF.
000211         10  BILLCDA PIC  X(0001).
000212     05  BILLCDI PIC  X(0003).
000213*    -------------------------------
000214     05  LCLABLL PIC S9(0004) COMP.
000215     05  LCLABLF PIC  X(0001).
000216     05  FILLER REDEFINES LCLABLF.
000217         10  LCLABLA PIC  X(0001).
000218     05  LCLABLI PIC  X(0011).
000219*    -------------------------------
000220     05  LCOMML PIC S9(0004) COMP.
000221     05  LCOMMF PIC  X(0001).
000222     05  FILLER REDEFINES LCOMMF.
000223         10  LCOMMA PIC  X(0001).
000224     05  LCOMMI PIC  S9(6)V9(2).
000225*    -------------------------------
000226     05  MAXFEEHL PIC S9(0004) COMP.
000227     05  MAXFEEHF PIC  X(0001).
000228     05  FILLER REDEFINES MAXFEEHF.
000229         10  MAXFEEHA PIC  X(0001).
000230     05  MAXFEEHI PIC  X(0013).
000231*    -------------------------------
000232     05  MAXMFEEL PIC S9(0004) COMP.
000233     05  MAXMFEEF PIC  X(0001).
000234     05  FILLER REDEFINES MAXMFEEF.
000235         10  MAXMFEEA PIC  X(0001).
000236     05  MAXMFEEI PIC  X(0005).
000237*    -------------------------------
000238     05  CLPSTHL PIC S9(0004) COMP.
000239     05  CLPSTHF PIC  X(0001).
000240     05  FILLER REDEFINES CLPSTHF.
000241         10  CLPSTHA PIC  X(0001).
000242     05  CLPSTHI PIC  X(0010).
000243*    -------------------------------
000244     05  CLPSTL PIC S9(0004) COMP.
000245     05  CLPSTF PIC  X(0001).
000246     05  FILLER REDEFINES CLPSTF.
000247         10  CLPSTA PIC  X(0001).
000248     05  CLPSTI PIC  X(0002).
000249*    -------------------------------
000250     05  PRODCDHL PIC S9(0004) COMP.
000251     05  PRODCDHF PIC  X(0001).
000252     05  FILLER REDEFINES PRODCDHF.
000253         10  PRODCDHA PIC  X(0001).
000254     05  PRODCDHI PIC  X(0012).
000255*    -------------------------------
000256     05  PRODCDL PIC S9(0004) COMP.
000257     05  PRODCDF PIC  X(0001).
000258     05  FILLER REDEFINES PRODCDF.
000259         10  PRODCDA PIC  X(0001).
000260     05  PRODCDI PIC  X(0003).
000261*    -------------------------------
000262     05  REMITL PIC S9(0004) COMP.
000263     05  REMITF PIC  X(0001).
000264     05  FILLER REDEFINES REMITF.
000265         10  REMITA PIC  X(0001).
000266     05  REMITI PIC  99.
000267*    -------------------------------
000268     05  COMMCALL PIC S9(0004) COMP.
000269     05  COMMCALF PIC  X(0001).
000270     05  FILLER REDEFINES COMMCALF.
000271         10  COMMCALA PIC  X(0001).
000272     05  COMMCALI PIC  X(0001).
000273*    -------------------------------
000274     05  REINTABL PIC S9(0004) COMP.
000275     05  REINTABF PIC  X(0001).
000276     05  FILLER REDEFINES REINTABF.
000277         10  REINTABA PIC  X(0001).
000278     05  REINTABI PIC  X(0003).
000279*    -------------------------------
000280     05  REINCALL PIC S9(0004) COMP.
000281     05  REINCALF PIC  X(0001).
000282     05  FILLER REDEFINES REINCALF.
000283         10  REINCALA PIC  X(0001).
000284     05  REINCALI PIC  X(0001).
000285*    -------------------------------
000286     05  AHHEADL PIC S9(0004) COMP.
000287     05  AHHEADF PIC  X(0001).
000288     05  FILLER REDEFINES AHHEADF.
000289         10  AHHEADA PIC  X(0001).
000290     05  AHHEADI PIC  X(0006).
000291*    -------------------------------
000292     05  AGNT1L PIC S9(0004) COMP.
000293     05  AGNT1F PIC  X(0001).
000294     05  FILLER REDEFINES AGNT1F.
000295         10  AGNT1A PIC  X(0001).
000296     05  AGNT1I PIC  X(0010).
000297*    -------------------------------
000298     05  TYPE1L PIC S9(0004) COMP.
000299     05  TYPE1F PIC  X(0001).
000300     05  FILLER REDEFINES TYPE1F.
000301         10  TYPE1A PIC  X(0001).
000302     05  TYPE1I PIC  X(0001).
000303*    -------------------------------
000304     05  SINRT1L PIC S9(0004) COMP.
000305     05  SINRT1F PIC  X(0001).
000306     05  FILLER REDEFINES SINRT1F.
000307         10  SINRT1A PIC  X(0001).
000308     05  SINRT1I PIC  X(0006).
000309*    -------------------------------
000310     05  JNTRT1L PIC S9(0004) COMP.
000311     05  JNTRT1F PIC  X(0001).
000312     05  FILLER REDEFINES JNTRT1F.
000313         10  JNTRT1A PIC  X(0001).
000314     05  JNTRT1I PIC  X(0006).
000315*    -------------------------------
000316     05  AHRT1L PIC S9(0004) COMP.
000317     05  AHRT1F PIC  X(0001).
000318     05  FILLER REDEFINES AHRT1F.
000319         10  AHRT1A PIC  X(0001).
000320     05  AHRT1I PIC  X(0006).
000321*    -------------------------------
000322     05  RECAL1L PIC S9(0004) COMP.
000323     05  RECAL1F PIC  X(0001).
000324     05  FILLER REDEFINES RECAL1F.
000325         10  RECAL1A PIC  X(0001).
000326     05  RECAL1I PIC  X(0001).
000327*    -------------------------------
000328     05  COMM1L PIC S9(0004) COMP.
000329     05  COMM1F PIC  X(0001).
000330     05  FILLER REDEFINES COMM1F.
000331         10  COMM1A PIC  X(0001).
000332     05  COMM1I PIC  X(0001).
000333*    -------------------------------
000334     05  CHGBK1L PIC S9(0004) COMP.
000335     05  CHGBK1F PIC  X(0001).
000336     05  FILLER REDEFINES CHGBK1F.
000337         10  CHGBK1A PIC  X(0001).
000338     05  CHGBK1I PIC  X(0002).
000339*    -------------------------------
000340     05  CCE01L PIC S9(0004) COMP.
000341     05  CCE01F PIC  X(0001).
000342     05  FILLER REDEFINES CCE01F.
000343         10  CCE01A PIC  X(0001).
000344     05  CCE01I PIC  X(0001).
000345*    -------------------------------
000346     05  AGNT2L PIC S9(0004) COMP.
000347     05  AGNT2F PIC  X(0001).
000348     05  FILLER REDEFINES AGNT2F.
000349         10  AGNT2A PIC  X(0001).
000350     05  AGNT2I PIC  X(0010).
000351*    -------------------------------
000352     05  TYPE2L PIC S9(0004) COMP.
000353     05  TYPE2F PIC  X(0001).
000354     05  FILLER REDEFINES TYPE2F.
000355         10  TYPE2A PIC  X(0001).
000356     05  TYPE2I PIC  X(0001).
000357*    -------------------------------
000358     05  SINRT2L PIC S9(0004) COMP.
000359     05  SINRT2F PIC  X(0001).
000360     05  FILLER REDEFINES SINRT2F.
000361         10  SINRT2A PIC  X(0001).
000362     05  SINRT2I PIC  X(0006).
000363*    -------------------------------
000364     05  JNTRT2L PIC S9(0004) COMP.
000365     05  JNTRT2F PIC  X(0001).
000366     05  FILLER REDEFINES JNTRT2F.
000367         10  JNTRT2A PIC  X(0001).
000368     05  JNTRT2I PIC  X(0006).
000369*    -------------------------------
000370     05  AHRT2L PIC S9(0004) COMP.
000371     05  AHRT2F PIC  X(0001).
000372     05  FILLER REDEFINES AHRT2F.
000373         10  AHRT2A PIC  X(0001).
000374     05  AHRT2I PIC  X(0006).
000375*    -------------------------------
000376     05  RECAL2L PIC S9(0004) COMP.
000377     05  RECAL2F PIC  X(0001).
000378     05  FILLER REDEFINES RECAL2F.
000379         10  RECAL2A PIC  X(0001).
000380     05  RECAL2I PIC  X(0001).
000381*    -------------------------------
000382     05  COMM2L PIC S9(0004) COMP.
000383     05  COMM2F PIC  X(0001).
000384     05  FILLER REDEFINES COMM2F.
000385         10  COMM2A PIC  X(0001).
000386     05  COMM2I PIC  X(0001).
000387*    -------------------------------
000388     05  CHGBK2L PIC S9(0004) COMP.
000389     05  CHGBK2F PIC  X(0001).
000390     05  FILLER REDEFINES CHGBK2F.
000391         10  CHGBK2A PIC  X(0001).
000392     05  CHGBK2I PIC  X(0002).
000393*    -------------------------------
000394     05  CCE02L PIC S9(0004) COMP.
000395     05  CCE02F PIC  X(0001).
000396     05  FILLER REDEFINES CCE02F.
000397         10  CCE02A PIC  X(0001).
000398     05  CCE02I PIC  X(0001).
000399*    -------------------------------
000400     05  AGNT3L PIC S9(0004) COMP.
000401     05  AGNT3F PIC  X(0001).
000402     05  FILLER REDEFINES AGNT3F.
000403         10  AGNT3A PIC  X(0001).
000404     05  AGNT3I PIC  X(0010).
000405*    -------------------------------
000406     05  TYPE3L PIC S9(0004) COMP.
000407     05  TYPE3F PIC  X(0001).
000408     05  FILLER REDEFINES TYPE3F.
000409         10  TYPE3A PIC  X(0001).
000410     05  TYPE3I PIC  X(0001).
000411*    -------------------------------
000412     05  SINRT3L PIC S9(0004) COMP.
000413     05  SINRT3F PIC  X(0001).
000414     05  FILLER REDEFINES SINRT3F.
000415         10  SINRT3A PIC  X(0001).
000416     05  SINRT3I PIC  X(0006).
000417*    -------------------------------
000418     05  JNTRT3L PIC S9(0004) COMP.
000419     05  JNTRT3F PIC  X(0001).
000420     05  FILLER REDEFINES JNTRT3F.
000421         10  JNTRT3A PIC  X(0001).
000422     05  JNTRT3I PIC  X(0006).
000423*    -------------------------------
000424     05  AHRT3L PIC S9(0004) COMP.
000425     05  AHRT3F PIC  X(0001).
000426     05  FILLER REDEFINES AHRT3F.
000427         10  AHRT3A PIC  X(0001).
000428     05  AHRT3I PIC  X(0006).
000429*    -------------------------------
000430     05  RECAL3L PIC S9(0004) COMP.
000431     05  RECAL3F PIC  X(0001).
000432     05  FILLER REDEFINES RECAL3F.
000433         10  RECAL3A PIC  X(0001).
000434     05  RECAL3I PIC  X(0001).
000435*    -------------------------------
000436     05  COMM3L PIC S9(0004) COMP.
000437     05  COMM3F PIC  X(0001).
000438     05  FILLER REDEFINES COMM3F.
000439         10  COMM3A PIC  X(0001).
000440     05  COMM3I PIC  X(0001).
000441*    -------------------------------
000442     05  CHGBK3L PIC S9(0004) COMP.
000443     05  CHGBK3F PIC  X(0001).
000444     05  FILLER REDEFINES CHGBK3F.
000445         10  CHGBK3A PIC  X(0001).
000446     05  CHGBK3I PIC  X(0002).
000447*    -------------------------------
000448     05  CCE03L PIC S9(0004) COMP.
000449     05  CCE03F PIC  X(0001).
000450     05  FILLER REDEFINES CCE03F.
000451         10  CCE03A PIC  X(0001).
000452     05  CCE03I PIC  X(0001).
000453*    -------------------------------
000454     05  AGNT4L PIC S9(0004) COMP.
000455     05  AGNT4F PIC  X(0001).
000456     05  FILLER REDEFINES AGNT4F.
000457         10  AGNT4A PIC  X(0001).
000458     05  AGNT4I PIC  X(0010).
000459*    -------------------------------
000460     05  TYPE4L PIC S9(0004) COMP.
000461     05  TYPE4F PIC  X(0001).
000462     05  FILLER REDEFINES TYPE4F.
000463         10  TYPE4A PIC  X(0001).
000464     05  TYPE4I PIC  X(0001).
000465*    -------------------------------
000466     05  SINRT4L PIC S9(0004) COMP.
000467     05  SINRT4F PIC  X(0001).
000468     05  FILLER REDEFINES SINRT4F.
000469         10  SINRT4A PIC  X(0001).
000470     05  SINRT4I PIC  X(0006).
000471*    -------------------------------
000472     05  JNTRT4L PIC S9(0004) COMP.
000473     05  JNTRT4F PIC  X(0001).
000474     05  FILLER REDEFINES JNTRT4F.
000475         10  JNTRT4A PIC  X(0001).
000476     05  JNTRT4I PIC  X(0006).
000477*    -------------------------------
000478     05  AHRT4L PIC S9(0004) COMP.
000479     05  AHRT4F PIC  X(0001).
000480     05  FILLER REDEFINES AHRT4F.
000481         10  AHRT4A PIC  X(0001).
000482     05  AHRT4I PIC  X(0006).
000483*    -------------------------------
000484     05  RECAL4L PIC S9(0004) COMP.
000485     05  RECAL4F PIC  X(0001).
000486     05  FILLER REDEFINES RECAL4F.
000487         10  RECAL4A PIC  X(0001).
000488     05  RECAL4I PIC  X(0001).
000489*    -------------------------------
000490     05  COMM4L PIC S9(0004) COMP.
000491     05  COMM4F PIC  X(0001).
000492     05  FILLER REDEFINES COMM4F.
000493         10  COMM4A PIC  X(0001).
000494     05  COMM4I PIC  X(0001).
000495*    -------------------------------
000496     05  CHGBK4L PIC S9(0004) COMP.
000497     05  CHGBK4F PIC  X(0001).
000498     05  FILLER REDEFINES CHGBK4F.
000499         10  CHGBK4A PIC  X(0001).
000500     05  CHGBK4I PIC  X(0002).
000501*    -------------------------------
000502     05  CCE04L PIC S9(0004) COMP.
000503     05  CCE04F PIC  X(0001).
000504     05  FILLER REDEFINES CCE04F.
000505         10  CCE04A PIC  X(0001).
000506     05  CCE04I PIC  X(0001).
000507*    -------------------------------
000508     05  AGNT5L PIC S9(0004) COMP.
000509     05  AGNT5F PIC  X(0001).
000510     05  FILLER REDEFINES AGNT5F.
000511         10  AGNT5A PIC  X(0001).
000512     05  AGNT5I PIC  X(0010).
000513*    -------------------------------
000514     05  TYPE5L PIC S9(0004) COMP.
000515     05  TYPE5F PIC  X(0001).
000516     05  FILLER REDEFINES TYPE5F.
000517         10  TYPE5A PIC  X(0001).
000518     05  TYPE5I PIC  X(0001).
000519*    -------------------------------
000520     05  SINRT5L PIC S9(0004) COMP.
000521     05  SINRT5F PIC  X(0001).
000522     05  FILLER REDEFINES SINRT5F.
000523         10  SINRT5A PIC  X(0001).
000524     05  SINRT5I PIC  X(0006).
000525*    -------------------------------
000526     05  JNTRT5L PIC S9(0004) COMP.
000527     05  JNTRT5F PIC  X(0001).
000528     05  FILLER REDEFINES JNTRT5F.
000529         10  JNTRT5A PIC  X(0001).
000530     05  JNTRT5I PIC  X(0006).
000531*    -------------------------------
000532     05  AHRT5L PIC S9(0004) COMP.
000533     05  AHRT5F PIC  X(0001).
000534     05  FILLER REDEFINES AHRT5F.
000535         10  AHRT5A PIC  X(0001).
000536     05  AHRT5I PIC  X(0006).
000537*    -------------------------------
000538     05  RECAL5L PIC S9(0004) COMP.
000539     05  RECAL5F PIC  X(0001).
000540     05  FILLER REDEFINES RECAL5F.
000541         10  RECAL5A PIC  X(0001).
000542     05  RECAL5I PIC  X(0001).
000543*    -------------------------------
000544     05  COMM5L PIC S9(0004) COMP.
000545     05  COMM5F PIC  X(0001).
000546     05  FILLER REDEFINES COMM5F.
000547         10  COMM5A PIC  X(0001).
000548     05  COMM5I PIC  X(0001).
000549*    -------------------------------
000550     05  CHGBK5L PIC S9(0004) COMP.
000551     05  CHGBK5F PIC  X(0001).
000552     05  FILLER REDEFINES CHGBK5F.
000553         10  CHGBK5A PIC  X(0001).
000554     05  CHGBK5I PIC  X(0002).
000555*    -------------------------------
000556     05  CCE05L PIC S9(0004) COMP.
000557     05  CCE05F PIC  X(0001).
000558     05  FILLER REDEFINES CCE05F.
000559         10  CCE05A PIC  X(0001).
000560     05  CCE05I PIC  X(0001).
000561*    -------------------------------
000562     05  AGNT6L PIC S9(0004) COMP.
000563     05  AGNT6F PIC  X(0001).
000564     05  FILLER REDEFINES AGNT6F.
000565         10  AGNT6A PIC  X(0001).
000566     05  AGNT6I PIC  X(0010).
000567*    -------------------------------
000568     05  TYPE6L PIC S9(0004) COMP.
000569     05  TYPE6F PIC  X(0001).
000570     05  FILLER REDEFINES TYPE6F.
000571         10  TYPE6A PIC  X(0001).
000572     05  TYPE6I PIC  X(0001).
000573*    -------------------------------
000574     05  SINRT6L PIC S9(0004) COMP.
000575     05  SINRT6F PIC  X(0001).
000576     05  FILLER REDEFINES SINRT6F.
000577         10  SINRT6A PIC  X(0001).
000578     05  SINRT6I PIC  X(0006).
000579*    -------------------------------
000580     05  JNTRT6L PIC S9(0004) COMP.
000581     05  JNTRT6F PIC  X(0001).
000582     05  FILLER REDEFINES JNTRT6F.
000583         10  JNTRT6A PIC  X(0001).
000584     05  JNTRT6I PIC  X(0006).
000585*    -------------------------------
000586     05  AHRT6L PIC S9(0004) COMP.
000587     05  AHRT6F PIC  X(0001).
000588     05  FILLER REDEFINES AHRT6F.
000589         10  AHRT6A PIC  X(0001).
000590     05  AHRT6I PIC  X(0006).
000591*    -------------------------------
000592     05  RECAL6L PIC S9(0004) COMP.
000593     05  RECAL6F PIC  X(0001).
000594     05  FILLER REDEFINES RECAL6F.
000595         10  RECAL6A PIC  X(0001).
000596     05  RECAL6I PIC  X(0001).
000597*    -------------------------------
000598     05  COMM6L PIC S9(0004) COMP.
000599     05  COMM6F PIC  X(0001).
000600     05  FILLER REDEFINES COMM6F.
000601         10  COMM6A PIC  X(0001).
000602     05  COMM6I PIC  X(0001).
000603*    -------------------------------
000604     05  CHGBK6L PIC S9(0004) COMP.
000605     05  CHGBK6F PIC  X(0001).
000606     05  FILLER REDEFINES CHGBK6F.
000607         10  CHGBK6A PIC  X(0001).
000608     05  CHGBK6I PIC  X(0002).
000609*    -------------------------------
000610     05  CCE06L PIC S9(0004) COMP.
000611     05  CCE06F PIC  X(0001).
000612     05  FILLER REDEFINES CCE06F.
000613         10  CCE06A PIC  X(0001).
000614     05  CCE06I PIC  X(0001).
000615*    -------------------------------
000616     05  AGNT7L PIC S9(0004) COMP.
000617     05  AGNT7F PIC  X(0001).
000618     05  FILLER REDEFINES AGNT7F.
000619         10  AGNT7A PIC  X(0001).
000620     05  AGNT7I PIC  X(0010).
000621*    -------------------------------
000622     05  TYPE7L PIC S9(0004) COMP.
000623     05  TYPE7F PIC  X(0001).
000624     05  FILLER REDEFINES TYPE7F.
000625         10  TYPE7A PIC  X(0001).
000626     05  TYPE7I PIC  X(0001).
000627*    -------------------------------
000628     05  SINRT7L PIC S9(0004) COMP.
000629     05  SINRT7F PIC  X(0001).
000630     05  FILLER REDEFINES SINRT7F.
000631         10  SINRT7A PIC  X(0001).
000632     05  SINRT7I PIC  X(0006).
000633*    -------------------------------
000634     05  JNTRT7L PIC S9(0004) COMP.
000635     05  JNTRT7F PIC  X(0001).
000636     05  FILLER REDEFINES JNTRT7F.
000637         10  JNTRT7A PIC  X(0001).
000638     05  JNTRT7I PIC  X(0006).
000639*    -------------------------------
000640     05  AHRT7L PIC S9(0004) COMP.
000641     05  AHRT7F PIC  X(0001).
000642     05  FILLER REDEFINES AHRT7F.
000643         10  AHRT7A PIC  X(0001).
000644     05  AHRT7I PIC  X(0006).
000645*    -------------------------------
000646     05  RECAL7L PIC S9(0004) COMP.
000647     05  RECAL7F PIC  X(0001).
000648     05  FILLER REDEFINES RECAL7F.
000649         10  RECAL7A PIC  X(0001).
000650     05  RECAL7I PIC  X(0001).
000651*    -------------------------------
000652     05  COMM7L PIC S9(0004) COMP.
000653     05  COMM7F PIC  X(0001).
000654     05  FILLER REDEFINES COMM7F.
000655         10  COMM7A PIC  X(0001).
000656     05  COMM7I PIC  X(0001).
000657*    -------------------------------
000658     05  CHGBK7L PIC S9(0004) COMP.
000659     05  CHGBK7F PIC  X(0001).
000660     05  FILLER REDEFINES CHGBK7F.
000661         10  CHGBK7A PIC  X(0001).
000662     05  CHGBK7I PIC  X(0002).
000663*    -------------------------------
000664     05  CCE07L PIC S9(0004) COMP.
000665     05  CCE07F PIC  X(0001).
000666     05  FILLER REDEFINES CCE07F.
000667         10  CCE07A PIC  X(0001).
000668     05  CCE07I PIC  X(0001).
000669*    -------------------------------
000670     05  AGNT8L PIC S9(0004) COMP.
000671     05  AGNT8F PIC  X(0001).
000672     05  FILLER REDEFINES AGNT8F.
000673         10  AGNT8A PIC  X(0001).
000674     05  AGNT8I PIC  X(0010).
000675*    -------------------------------
000676     05  TYPE8L PIC S9(0004) COMP.
000677     05  TYPE8F PIC  X(0001).
000678     05  FILLER REDEFINES TYPE8F.
000679         10  TYPE8A PIC  X(0001).
000680     05  TYPE8I PIC  X(0001).
000681*    -------------------------------
000682     05  SINRT8L PIC S9(0004) COMP.
000683     05  SINRT8F PIC  X(0001).
000684     05  FILLER REDEFINES SINRT8F.
000685         10  SINRT8A PIC  X(0001).
000686     05  SINRT8I PIC  X(0006).
000687*    -------------------------------
000688     05  JNTRT8L PIC S9(0004) COMP.
000689     05  JNTRT8F PIC  X(0001).
000690     05  FILLER REDEFINES JNTRT8F.
000691         10  JNTRT8A PIC  X(0001).
000692     05  JNTRT8I PIC  X(0006).
000693*    -------------------------------
000694     05  AHRT8L PIC S9(0004) COMP.
000695     05  AHRT8F PIC  X(0001).
000696     05  FILLER REDEFINES AHRT8F.
000697         10  AHRT8A PIC  X(0001).
000698     05  AHRT8I PIC  X(0006).
000699*    -------------------------------
000700     05  RECAL8L PIC S9(0004) COMP.
000701     05  RECAL8F PIC  X(0001).
000702     05  FILLER REDEFINES RECAL8F.
000703         10  RECAL8A PIC  X(0001).
000704     05  RECAL8I PIC  X(0001).
000705*    -------------------------------
000706     05  COMM8L PIC S9(0004) COMP.
000707     05  COMM8F PIC  X(0001).
000708     05  FILLER REDEFINES COMM8F.
000709         10  COMM8A PIC  X(0001).
000710     05  COMM8I PIC  X(0001).
000711*    -------------------------------
000712     05  CHGBK8L PIC S9(0004) COMP.
000713     05  CHGBK8F PIC  X(0001).
000714     05  FILLER REDEFINES CHGBK8F.
000715         10  CHGBK8A PIC  X(0001).
000716     05  CHGBK8I PIC  X(0002).
000717*    -------------------------------
000718     05  CCE08L PIC S9(0004) COMP.
000719     05  CCE08F PIC  X(0001).
000720     05  FILLER REDEFINES CCE08F.
000721         10  CCE08A PIC  X(0001).
000722     05  CCE08I PIC  X(0001).
000723*    -------------------------------
000724     05  AGNT9L PIC S9(0004) COMP.
000725     05  AGNT9F PIC  X(0001).
000726     05  FILLER REDEFINES AGNT9F.
000727         10  AGNT9A PIC  X(0001).
000728     05  AGNT9I PIC  X(0010).
000729*    -------------------------------
000730     05  TYPE9L PIC S9(0004) COMP.
000731     05  TYPE9F PIC  X(0001).
000732     05  FILLER REDEFINES TYPE9F.
000733         10  TYPE9A PIC  X(0001).
000734     05  TYPE9I PIC  X(0001).
000735*    -------------------------------
000736     05  SINRT9L PIC S9(0004) COMP.
000737     05  SINRT9F PIC  X(0001).
000738     05  FILLER REDEFINES SINRT9F.
000739         10  SINRT9A PIC  X(0001).
000740     05  SINRT9I PIC  X(0006).
000741*    -------------------------------
000742     05  JNTRT9L PIC S9(0004) COMP.
000743     05  JNTRT9F PIC  X(0001).
000744     05  FILLER REDEFINES JNTRT9F.
000745         10  JNTRT9A PIC  X(0001).
000746     05  JNTRT9I PIC  X(0006).
000747*    -------------------------------
000748     05  AHRT9L PIC S9(0004) COMP.
000749     05  AHRT9F PIC  X(0001).
000750     05  FILLER REDEFINES AHRT9F.
000751         10  AHRT9A PIC  X(0001).
000752     05  AHRT9I PIC  X(0006).
000753*    -------------------------------
000754     05  RECAL9L PIC S9(0004) COMP.
000755     05  RECAL9F PIC  X(0001).
000756     05  FILLER REDEFINES RECAL9F.
000757         10  RECAL9A PIC  X(0001).
000758     05  RECAL9I PIC  X(0001).
000759*    -------------------------------
000760     05  COMM9L PIC S9(0004) COMP.
000761     05  COMM9F PIC  X(0001).
000762     05  FILLER REDEFINES COMM9F.
000763         10  COMM9A PIC  X(0001).
000764     05  COMM9I PIC  X(0001).
000765*    -------------------------------
000766     05  CHGBK9L PIC S9(0004) COMP.
000767     05  CHGBK9F PIC  X(0001).
000768     05  FILLER REDEFINES CHGBK9F.
000769         10  CHGBK9A PIC  X(0001).
000770     05  CHGBK9I PIC  X(0002).
000771*    -------------------------------
000772     05  CCE09L PIC S9(0004) COMP.
000773     05  CCE09F PIC  X(0001).
000774     05  FILLER REDEFINES CCE09F.
000775         10  CCE09A PIC  X(0001).
000776     05  CCE09I PIC  X(0001).
000777*    -------------------------------
000778     05  AGNT10L PIC S9(0004) COMP.
000779     05  AGNT10F PIC  X(0001).
000780     05  FILLER REDEFINES AGNT10F.
000781         10  AGNT10A PIC  X(0001).
000782     05  AGNT10I PIC  X(0010).
000783*    -------------------------------
000784     05  TYPE10L PIC S9(0004) COMP.
000785     05  TYPE10F PIC  X(0001).
000786     05  FILLER REDEFINES TYPE10F.
000787         10  TYPE10A PIC  X(0001).
000788     05  TYPE10I PIC  X(0001).
000789*    -------------------------------
000790     05  SINRT10L PIC S9(0004) COMP.
000791     05  SINRT10F PIC  X(0001).
000792     05  FILLER REDEFINES SINRT10F.
000793         10  SINRT10A PIC  X(0001).
000794     05  SINRT10I PIC  X(0006).
000795*    -------------------------------
000796     05  JNTRT10L PIC S9(0004) COMP.
000797     05  JNTRT10F PIC  X(0001).
000798     05  FILLER REDEFINES JNTRT10F.
000799         10  JNTRT10A PIC  X(0001).
000800     05  JNTRT10I PIC  X(0006).
000801*    -------------------------------
000802     05  AHRT10L PIC S9(0004) COMP.
000803     05  AHRT10F PIC  X(0001).
000804     05  FILLER REDEFINES AHRT10F.
000805         10  AHRT10A PIC  X(0001).
000806     05  AHRT10I PIC  X(0006).
000807*    -------------------------------
000808     05  RECAL10L PIC S9(0004) COMP.
000809     05  RECAL10F PIC  X(0001).
000810     05  FILLER REDEFINES RECAL10F.
000811         10  RECAL10A PIC  X(0001).
000812     05  RECAL10I PIC  X(0001).
000813*    -------------------------------
000814     05  COMM10L PIC S9(0004) COMP.
000815     05  COMM10F PIC  X(0001).
000816     05  FILLER REDEFINES COMM10F.
000817         10  COMM10A PIC  X(0001).
000818     05  COMM10I PIC  X(0001).
000819*    -------------------------------
000820     05  CHGBK10L PIC S9(0004) COMP.
000821     05  CHGBK10F PIC  X(0001).
000822     05  FILLER REDEFINES CHGBK10F.
000823         10  CHGBK10A PIC  X(0001).
000824     05  CHGBK10I PIC  X(0002).
000825*    -------------------------------
000826     05  CCE10L PIC S9(0004) COMP.
000827     05  CCE10F PIC  X(0001).
000828     05  FILLER REDEFINES CCE10F.
000829         10  CCE10A PIC  X(0001).
000830     05  CCE10I PIC  X(0001).
000831*    -------------------------------
000832     05  ERRMSG1L PIC S9(0004) COMP.
000833     05  ERRMSG1F PIC  X(0001).
000834     05  FILLER REDEFINES ERRMSG1F.
000835         10  ERRMSG1A PIC  X(0001).
000836     05  ERRMSG1I PIC  X(0075).
000837*    -------------------------------
000838     05  PFENTERL PIC S9(0004) COMP.
000839     05  PFENTERF PIC  X(0001).
000840     05  FILLER REDEFINES PFENTERF.
000841         10  PFENTERA PIC  X(0001).
000842     05  PFENTERI PIC  9(2).
000843*    -------------------------------
000844     05  PF1PF2L PIC S9(0004) COMP.
000845     05  PF1PF2F PIC  X(0001).
000846     05  FILLER REDEFINES PF1PF2F.
000847         10  PF1PF2A PIC  X(0001).
000848     05  PF1PF2I PIC  X(0035).
000849 01  EL6501AO REDEFINES EL6501AI.
000850     05  FILLER            PIC  X(0012).
000851*    -------------------------------
000852     05  FILLER            PIC  X(0003).
000853     05  DATEO PIC  X(0008).
000854*    -------------------------------
000855     05  FILLER            PIC  X(0003).
000856     05  TIMEO PIC  99.99.
000857*    -------------------------------
000858     05  FILLER            PIC  X(0003).
000859     05  CMPNYIDO PIC  X(0003).
000860*    -------------------------------
000861     05  FILLER            PIC  X(0003).
000862     05  USERIDO PIC  X(0004).
000863*    -------------------------------
000864     05  FILLER            PIC  X(0003).
000865     05  MAINTO PIC  X(0001).
000866*    -------------------------------
000867     05  FILLER            PIC  X(0003).
000868     05  CARRO PIC  X(0001).
000869*    -------------------------------
000870     05  FILLER            PIC  X(0003).
000871     05  GROUPINO PIC  X(0006).
000872*    -------------------------------
000873     05  FILLER            PIC  X(0003).
000874     05  STATEO PIC  X(0002).
000875*    -------------------------------
000876     05  FILLER            PIC  X(0003).
000877     05  ACCOUNTO PIC  X(0010).
000878*    -------------------------------
000879     05  FILLER            PIC  X(0003).
000880     05  EFFDTEO PIC  X(0008).
000881*    -------------------------------
000882     05  FILLER            PIC  X(0003).
000883     05  EXPDTEO PIC  99B99B99.
000884*    -------------------------------
000885     05  FILLER            PIC  X(0003).
000886     05  MAINTBYO PIC  X(0004).
000887*    -------------------------------
000888     05  FILLER            PIC  X(0003).
000889     05  PCONTO PIC  X(0030).
000890*    -------------------------------
000891     05  FILLER            PIC  X(0003).
000892     05  CSRO PIC  X(0004).
000893*    -------------------------------
000894     05  FILLER            PIC  X(0003).
000895     05  PDSPLYO PIC  X(0006).
000896*    -------------------------------
000897     05  FILLER            PIC  X(0003).
000898     05  PEFFDTEO PIC  X(0008).
000899*    -------------------------------
000900     05  FILLER            PIC  X(0003).
000901     05  PEXPDTEO PIC  X(0008).
000902*    -------------------------------
000903     05  FILLER            PIC  X(0003).
000904     05  NAMEO PIC  X(0030).
000905*    -------------------------------
000906     05  FILLER            PIC  X(0003).
000907     05  TAXNOO PIC  X(0011).
000908*    -------------------------------
000909     05  FILLER            PIC  X(0003).
000910     05  STATUSO PIC  X(0001).
000911*    -------------------------------
000912     05  FILLER            PIC  X(0003).
000913     05  INCAREO PIC  X(0030).
000914*    -------------------------------
000915     05  FILLER            PIC  X(0003).
000916     05  PHONEO PIC  999B999B9999.
000917*    -------------------------------
000918     05  FILLER            PIC  X(0003).
000919     05  INDGRPO PIC  X(0001).
000920*    -------------------------------
000921     05  FILLER            PIC  X(0003).
000922     05  ADDR1O PIC  X(0030).
000923*    -------------------------------
000924     05  FILLER            PIC  X(0003).
000925     05  CONTRO PIC  X(0008).
000926*    -------------------------------
000927     05  FILLER            PIC  X(0003).
000928     05  TYPEO PIC  X(0002).
000929*    -------------------------------
000930     05  FILLER            PIC  X(0003).
000931     05  ACITYO PIC  X(0028).
000932*    -------------------------------
000933     05  FILLER            PIC  X(0003).
000934     05  ASTATEO PIC  X(0002).
000935*    -------------------------------
000936     05  FILLER            PIC  X(0003).
000937     05  PRODATEO PIC  X(0008).
000938*    -------------------------------
000939     05  FILLER            PIC  X(0003).
000940     05  STDBENO PIC  X(0002).
000941*    -------------------------------
000942     05  FILLER            PIC  X(0003).
000943     05  ZIPO PIC  X(0010).
000944*    -------------------------------
000945     05  FILLER            PIC  X(0003).
000946     05  CTPLABLO PIC  X(0010).
000947*    -------------------------------
000948     05  FILLER            PIC  X(0003).
000949     05  CLPTOLPO PIC  Z.9999.
000950*    -------------------------------
000951     05  FILLER            PIC  X(0003).
000952     05  RETROCDO PIC  X(0003).
000953*    -------------------------------
000954     05  FILLER            PIC  X(0003).
000955     05  BILLCDO PIC  X(0003).
000956*    -------------------------------
000957     05  FILLER            PIC  X(0003).
000958     05  LCLABLO PIC  X(0011).
000959*    -------------------------------
000960     05  FILLER            PIC  X(0003).
000961     05  LCOMMO PIC  ZZ99.99-.
000962*    -------------------------------
000963     05  FILLER            PIC  X(0003).
000964     05  MAXFEEHO PIC  X(0013).
000965*    -------------------------------
000966     05  FILLER            PIC  X(0003).
000967     05  MAXMFEEO PIC  Z,Z99.
000968*    -------------------------------
000969     05  FILLER            PIC  X(0003).
000970     05  CLPSTHO PIC  X(0010).
000971*    -------------------------------
000972     05  FILLER            PIC  X(0003).
000973     05  CLPSTO PIC  X(0002).
000974*    -------------------------------
000975     05  FILLER            PIC  X(0003).
000976     05  PRODCDHO PIC  X(0012).
000977*    -------------------------------
000978     05  FILLER            PIC  X(0003).
000979     05  PRODCDO PIC  X(0003).
000980*    -------------------------------
000981     05  FILLER            PIC  X(0003).
000982     05  REMITO PIC  X(0002).
000983*    -------------------------------
000984     05  FILLER            PIC  X(0003).
000985     05  COMMCALO PIC  X(0001).
000986*    -------------------------------
000987     05  FILLER            PIC  X(0003).
000988     05  REINTABO PIC  X(0003).
000989*    -------------------------------
000990     05  FILLER            PIC  X(0003).
000991     05  REINCALO PIC  X(0001).
000992*    -------------------------------
000993     05  FILLER            PIC  X(0003).
000994     05  AHHEADO PIC  X(0006).
000995*    -------------------------------
000996     05  FILLER            PIC  X(0003).
000997     05  AGNT1O PIC  X(0010).
000998*    -------------------------------
000999     05  FILLER            PIC  X(0003).
001000     05  TYPE1O PIC  X(0001).
001001*    -------------------------------
001002     05  FILLER            PIC  X(0003).
001003     05  SINRT1O PIC  X(0006).
001004*    -------------------------------
001005     05  FILLER            PIC  X(0003).
001006     05  JNTRT1O PIC  X(0006).
001007*    -------------------------------
001008     05  FILLER            PIC  X(0003).
001009     05  AHRT1O PIC  X(0006).
001010*    -------------------------------
001011     05  FILLER            PIC  X(0003).
001012     05  RECAL1O PIC  X(0001).
001013*    -------------------------------
001014     05  FILLER            PIC  X(0003).
001015     05  COMM1O PIC  X(0001).
001016*    -------------------------------
001017     05  FILLER            PIC  X(0003).
001018     05  CHGBK1O PIC  X(0002).
001019*    -------------------------------
001020     05  FILLER            PIC  X(0003).
001021     05  CCE01O PIC  X(0001).
001022*    -------------------------------
001023     05  FILLER            PIC  X(0003).
001024     05  AGNT2O PIC  X(0010).
001025*    -------------------------------
001026     05  FILLER            PIC  X(0003).
001027     05  TYPE2O PIC  X(0001).
001028*    -------------------------------
001029     05  FILLER            PIC  X(0003).
001030     05  SINRT2O PIC  X(0006).
001031*    -------------------------------
001032     05  FILLER            PIC  X(0003).
001033     05  JNTRT2O PIC  X(0006).
001034*    -------------------------------
001035     05  FILLER            PIC  X(0003).
001036     05  AHRT2O PIC  X(0006).
001037*    -------------------------------
001038     05  FILLER            PIC  X(0003).
001039     05  RECAL2O PIC  X(0001).
001040*    -------------------------------
001041     05  FILLER            PIC  X(0003).
001042     05  COMM2O PIC  X(0001).
001043*    -------------------------------
001044     05  FILLER            PIC  X(0003).
001045     05  CHGBK2O PIC  X(0002).
001046*    -------------------------------
001047     05  FILLER            PIC  X(0003).
001048     05  CCE02O PIC  X(0001).
001049*    -------------------------------
001050     05  FILLER            PIC  X(0003).
001051     05  AGNT3O PIC  X(0010).
001052*    -------------------------------
001053     05  FILLER            PIC  X(0003).
001054     05  TYPE3O PIC  X(0001).
001055*    -------------------------------
001056     05  FILLER            PIC  X(0003).
001057     05  SINRT3O PIC  X(0006).
001058*    -------------------------------
001059     05  FILLER            PIC  X(0003).
001060     05  JNTRT3O PIC  X(0006).
001061*    -------------------------------
001062     05  FILLER            PIC  X(0003).
001063     05  AHRT3O PIC  X(0006).
001064*    -------------------------------
001065     05  FILLER            PIC  X(0003).
001066     05  RECAL3O PIC  X(0001).
001067*    -------------------------------
001068     05  FILLER            PIC  X(0003).
001069     05  COMM3O PIC  X(0001).
001070*    -------------------------------
001071     05  FILLER            PIC  X(0003).
001072     05  CHGBK3O PIC  X(0002).
001073*    -------------------------------
001074     05  FILLER            PIC  X(0003).
001075     05  CCE03O PIC  X(0001).
001076*    -------------------------------
001077     05  FILLER            PIC  X(0003).
001078     05  AGNT4O PIC  X(0010).
001079*    -------------------------------
001080     05  FILLER            PIC  X(0003).
001081     05  TYPE4O PIC  X(0001).
001082*    -------------------------------
001083     05  FILLER            PIC  X(0003).
001084     05  SINRT4O PIC  X(0006).
001085*    -------------------------------
001086     05  FILLER            PIC  X(0003).
001087     05  JNTRT4O PIC  X(0006).
001088*    -------------------------------
001089     05  FILLER            PIC  X(0003).
001090     05  AHRT4O PIC  X(0006).
001091*    -------------------------------
001092     05  FILLER            PIC  X(0003).
001093     05  RECAL4O PIC  X(0001).
001094*    -------------------------------
001095     05  FILLER            PIC  X(0003).
001096     05  COMM4O PIC  X(0001).
001097*    -------------------------------
001098     05  FILLER            PIC  X(0003).
001099     05  CHGBK4O PIC  X(0002).
001100*    -------------------------------
001101     05  FILLER            PIC  X(0003).
001102     05  CCE04O PIC  X(0001).
001103*    -------------------------------
001104     05  FILLER            PIC  X(0003).
001105     05  AGNT5O PIC  X(0010).
001106*    -------------------------------
001107     05  FILLER            PIC  X(0003).
001108     05  TYPE5O PIC  X(0001).
001109*    -------------------------------
001110     05  FILLER            PIC  X(0003).
001111     05  SINRT5O PIC  X(0006).
001112*    -------------------------------
001113     05  FILLER            PIC  X(0003).
001114     05  JNTRT5O PIC  X(0006).
001115*    -------------------------------
001116     05  FILLER            PIC  X(0003).
001117     05  AHRT5O PIC  X(0006).
001118*    -------------------------------
001119     05  FILLER            PIC  X(0003).
001120     05  RECAL5O PIC  X(0001).
001121*    -------------------------------
001122     05  FILLER            PIC  X(0003).
001123     05  COMM5O PIC  X(0001).
001124*    -------------------------------
001125     05  FILLER            PIC  X(0003).
001126     05  CHGBK5O PIC  X(0002).
001127*    -------------------------------
001128     05  FILLER            PIC  X(0003).
001129     05  CCE05O PIC  X(0001).
001130*    -------------------------------
001131     05  FILLER            PIC  X(0003).
001132     05  AGNT6O PIC  X(0010).
001133*    -------------------------------
001134     05  FILLER            PIC  X(0003).
001135     05  TYPE6O PIC  X(0001).
001136*    -------------------------------
001137     05  FILLER            PIC  X(0003).
001138     05  SINRT6O PIC  X(0006).
001139*    -------------------------------
001140     05  FILLER            PIC  X(0003).
001141     05  JNTRT6O PIC  X(0006).
001142*    -------------------------------
001143     05  FILLER            PIC  X(0003).
001144     05  AHRT6O PIC  X(0006).
001145*    -------------------------------
001146     05  FILLER            PIC  X(0003).
001147     05  RECAL6O PIC  X(0001).
001148*    -------------------------------
001149     05  FILLER            PIC  X(0003).
001150     05  COMM6O PIC  X(0001).
001151*    -------------------------------
001152     05  FILLER            PIC  X(0003).
001153     05  CHGBK6O PIC  X(0002).
001154*    -------------------------------
001155     05  FILLER            PIC  X(0003).
001156     05  CCE06O PIC  X(0001).
001157*    -------------------------------
001158     05  FILLER            PIC  X(0003).
001159     05  AGNT7O PIC  X(0010).
001160*    -------------------------------
001161     05  FILLER            PIC  X(0003).
001162     05  TYPE7O PIC  X(0001).
001163*    -------------------------------
001164     05  FILLER            PIC  X(0003).
001165     05  SINRT7O PIC  X(0006).
001166*    -------------------------------
001167     05  FILLER            PIC  X(0003).
001168     05  JNTRT7O PIC  X(0006).
001169*    -------------------------------
001170     05  FILLER            PIC  X(0003).
001171     05  AHRT7O PIC  X(0006).
001172*    -------------------------------
001173     05  FILLER            PIC  X(0003).
001174     05  RECAL7O PIC  X(0001).
001175*    -------------------------------
001176     05  FILLER            PIC  X(0003).
001177     05  COMM7O PIC  X(0001).
001178*    -------------------------------
001179     05  FILLER            PIC  X(0003).
001180     05  CHGBK7O PIC  X(0002).
001181*    -------------------------------
001182     05  FILLER            PIC  X(0003).
001183     05  CCE07O PIC  X(0001).
001184*    -------------------------------
001185     05  FILLER            PIC  X(0003).
001186     05  AGNT8O PIC  X(0010).
001187*    -------------------------------
001188     05  FILLER            PIC  X(0003).
001189     05  TYPE8O PIC  X(0001).
001190*    -------------------------------
001191     05  FILLER            PIC  X(0003).
001192     05  SINRT8O PIC  X(0006).
001193*    -------------------------------
001194     05  FILLER            PIC  X(0003).
001195     05  JNTRT8O PIC  X(0006).
001196*    -------------------------------
001197     05  FILLER            PIC  X(0003).
001198     05  AHRT8O PIC  X(0006).
001199*    -------------------------------
001200     05  FILLER            PIC  X(0003).
001201     05  RECAL8O PIC  X(0001).
001202*    -------------------------------
001203     05  FILLER            PIC  X(0003).
001204     05  COMM8O PIC  X(0001).
001205*    -------------------------------
001206     05  FILLER            PIC  X(0003).
001207     05  CHGBK8O PIC  X(0002).
001208*    -------------------------------
001209     05  FILLER            PIC  X(0003).
001210     05  CCE08O PIC  X(0001).
001211*    -------------------------------
001212     05  FILLER            PIC  X(0003).
001213     05  AGNT9O PIC  X(0010).
001214*    -------------------------------
001215     05  FILLER            PIC  X(0003).
001216     05  TYPE9O PIC  X(0001).
001217*    -------------------------------
001218     05  FILLER            PIC  X(0003).
001219     05  SINRT9O PIC  X(0006).
001220*    -------------------------------
001221     05  FILLER            PIC  X(0003).
001222     05  JNTRT9O PIC  X(0006).
001223*    -------------------------------
001224     05  FILLER            PIC  X(0003).
001225     05  AHRT9O PIC  X(0006).
001226*    -------------------------------
001227     05  FILLER            PIC  X(0003).
001228     05  RECAL9O PIC  X(0001).
001229*    -------------------------------
001230     05  FILLER            PIC  X(0003).
001231     05  COMM9O PIC  X(0001).
001232*    -------------------------------
001233     05  FILLER            PIC  X(0003).
001234     05  CHGBK9O PIC  X(0002).
001235*    -------------------------------
001236     05  FILLER            PIC  X(0003).
001237     05  CCE09O PIC  X(0001).
001238*    -------------------------------
001239     05  FILLER            PIC  X(0003).
001240     05  AGNT10O PIC  X(0010).
001241*    -------------------------------
001242     05  FILLER            PIC  X(0003).
001243     05  TYPE10O PIC  X(0001).
001244*    -------------------------------
001245     05  FILLER            PIC  X(0003).
001246     05  SINRT10O PIC  X(0006).
001247*    -------------------------------
001248     05  FILLER            PIC  X(0003).
001249     05  JNTRT10O PIC  X(0006).
001250*    -------------------------------
001251     05  FILLER            PIC  X(0003).
001252     05  AHRT10O PIC  X(0006).
001253*    -------------------------------
001254     05  FILLER            PIC  X(0003).
001255     05  RECAL10O PIC  X(0001).
001256*    -------------------------------
001257     05  FILLER            PIC  X(0003).
001258     05  COMM10O PIC  X(0001).
001259*    -------------------------------
001260     05  FILLER            PIC  X(0003).
001261     05  CHGBK10O PIC  X(0002).
001262*    -------------------------------
001263     05  FILLER            PIC  X(0003).
001264     05  CCE10O PIC  X(0001).
001265*    -------------------------------
001266     05  FILLER            PIC  X(0003).
001267     05  ERRMSG1O PIC  X(0075).
001268*    -------------------------------
001269     05  FILLER            PIC  X(0003).
001270     05  PFENTERO PIC  X(0002).
001271*    -------------------------------
001272     05  FILLER            PIC  X(0003).
001273     05  PF1PF2O PIC  X(0035).
001274*    -------------------------------
      *<<((file: EL6501S))
000544 01  MAP-R REDEFINES EL6501AI.
000545*    12  FILLER                  PIC X(518).
000546*    12  FILLER                  PIC X(543).
000547     12  FILLER                  PIC X(546).
000548     12  MAP-AGENT-AREA.
000549         16  AGENT-AREA OCCURS 10 TIMES INDEXED BY M-INDEX.
000550             20  AGENTL          PIC S9(4) COMP.
000551             20  AGENTA          PIC X.
000552             20  AGENT           PIC X(10).
000553             20  ATYPEL          PIC S9(4) COMP.
000554             20  ATYPEA          PIC X.
000555             20  ATYPE           PIC X.
000556             20  SINGLEL         PIC S9(4) COMP.
000557             20  SINGLEA         PIC X.
000558             20  SINGLE-COMM-T   PIC X(6).
000559             20  SINGLE-COMM-N REDEFINES SINGLE-COMM-T.
000560                 24  SINGLE-COMM-R PIC V9(5).
000561                 24  SINGLE-DASH   PIC X.
000562             20  SINGLE-COMM-O REDEFINES SINGLE-COMM-T
000563                                 PIC V9(5)-.
000564             20  JOINTL          PIC S9(4) COMP.
000565             20  JOINTA          PIC X.
000566             20  JOINT-COMM-T    PIC X(6).
000567             20  JOINT-COMM-N REDEFINES JOINT-COMM-T.
000568                 24  JOINT-COMM-R PIC V9(5).
000569                 24  JOINT-DASH   PIC X.
000570             20  JOINT-COMM-O REDEFINES JOINT-COMM-T
000571                                 PIC V9(5)-.
000572             20  A-HL            PIC S9(4) COMP.
000573             20  A-HA            PIC X.
000574             20  A-H-COMM-T      PIC X(6).
000575             20  A-H-COMM-N  REDEFINES A-H-COMM-T.
000576                 24  A-H-COMM-R  PIC V9(5).
000577                 24  A-H-DASH    PIC X.
000578             20  A-H-COMM-O  REDEFINES A-H-COMM-T
000579                                 PIC V9(5)-.
000580             20  RECALL          PIC S9(4) COMP.
000581             20  RECALA          PIC X.
000582             20  RECAL           PIC X.
000583             20  RCOMML          PIC S9(4) COMP.
000584             20  RCOMMA          PIC X.
000585             20  RCOMM           PIC X.
000586             20  CHGBCKL         PIC S9(4) COMP.
000587             20  CHGBCKA         PIC X.
000588             20  CHGBCK          PIC X(02).
000589             20  CCEL            PIC S9(4) COMP.
000590             20  CCEA            PIC X.
000591             20  CCEIND          PIC X.
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
000593 01  DFHCOMMAREA                 PIC X(1500).
000594 EJECT
000595*01 PARMLIST .
000596*    02  FILLER                  PIC S9(8)   COMP.
000597*    02  ERACCT-POINTER          PIC S9(8)   COMP.
000598*    02  ELCNTL-POINTER          PIC S9(8)   COMP.
000599*    02  REIN-POINTER            PIC S9(8)   COMP.
000600*    02  CTBL-POINTER            PIC S9(8)   COMP.
000601*    02  COMM-POINTER            PIC S9(8)   COMP.
000602*    02  COMP-POINTER            PIC S9(8)   COMP.
000603*    02  NAME-POINTER            PIC S9(8)   COMP.
000604*    02  RQST-POINTER            PIC S9(8)   COMP.
000605*    02  AXRF-POINTER            PIC S9(8)   COMP.
000606*    02  AXRF2-POINTER           PIC S9(8)   COMP.
000607*    02  AXRF3-POINTER           PIC S9(8)   COMP.
000608*    02  AXRF4-POINTER           PIC S9(8)   COMP.
000609*    02  AXRF5-POINTER           PIC S9(8)   COMP.
000610*    02  AXRF6-POINTER           PIC S9(8)   COMP.
000611*    02  AXRF7-POINTER           PIC S9(8)   COMP.
000612*    02  AXRF8-POINTER           PIC S9(8)   COMP.
000613
000614*    COPY ERCACCT.
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
000615*    copy ERCPDEF.
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
000616*    COPY ELCCNTL.
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
000617
000618*    COPY ERCREIN.
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
000619
000620*    COPY ERCCTBL.
      *>>((file: ERCCTBL))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCCTBL                             *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.003
000007*                                                                *
000008*   ONLINE CREDIT SYSTEM                                         *
000009*                                                                *
000010*   FILE DESCRIPTION = COMPENSATION TABLE                        *
000011*                                                                *
000012*   FILE TYPE = VSAM,KSDS                                        *
000013*   RECORD SIZE = 200   RECFORM = FIXED                          *
000014*                                                                *
000015*   BASE CLUSTER NAME = ERCTBL                   RKP=2,LEN=7     *
000016*       ALTERNATE PATH = NONE                                    *
000017*                                                                *
000018*   LOG = NO                                                     *
000019*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000020*                                                                *
000021*                                                                *
000022******************************************************************
000023
000024 01  COMM-TABLE-RECORD.
000025     12  CT-RECORD-ID                      PIC XX.
000026         88  VALID-CT-ID                      VALUE 'CT'.
000027
000028     12  CT-CONTROL-PRIMARY.
000029         16  CT-COMPANY-CD                 PIC X.
000030         16  CT-TABLE                      PIC XXX.
000031         16  CT-CNTRL-2.
000032             20  CT-BEN-TYPE               PIC X.
000033             20  CT-BEN-CODE               PIC XX.
000034
000035     12  CT-MAINT-INFORMATION.
000036         16  CT-LAST-MAINT-DT              PIC XX.
000037         16  CT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
000038         16  CT-LAST-MAINT-USER            PIC X(4).
000039         16  FILLER                        PIC X(31).
000040
000041     12  CT-LIMITS.
000042         16  CT-TBF OCCURS 3 TIMES         PIC S9(7)V99   COMP-3.
000043
000044         16  CT-AGE OCCURS 3 TIMES         PIC S99        COMP-3.
000045
000046         16  CT-TRM OCCURS 3 TIMES         PIC S999       COMP-3.
000047
000048     12  CT-RATES.
000049         16  CT-RTX          OCCURS 27 TIMES.
000050             20  CT-RT                     PIC SV9(5)     COMP-3.
000051             20  CT-RT-R   REDEFINES
000052                 CT-RT                     PIC XXX.
000053
000054     12  FILLER                            PIC  X(42).
000055
000056******************************************************************
      *<<((file: ERCCTBL))
000621
000622*                                COPY ERCACNT.
      *>>((file: ERCACNT))
000001******************************************************************
000002*                                                                *
000003*                                                                *
000004*                            ERCACNT.                            *
000005*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000006*                            VMOD=2.002                          *
000007*                                                                *
000008*   FILE DESCRIPTION = NOTE FILE FOR RECORDING OF ACCOUNT NOTES  *
000009*                                                                *
000010*   FILE TYPE = VSAM,KSDS                                        *
000011*   RECORD SIZE = 120   RECFORM = FIXED                          *
000012*                                                                *
000013*   BASE CLUSTER NAME = ERACNT             RKP=2,LEN=23          *
000014*       ALTERNATE INDEX = NONE                                   *
000015*                                                                *
000016*                                                                *
000017*   LOG = YES                                                    *
000018*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000019******************************************************************
000020*                   C H A N G E   L O G
000021*
000022* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
000023*-----------------------------------------------------------------
000024*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
000025* EFFECTIVE    NUMBER
000026*-----------------------------------------------------------------
000027* 110706  CR2006071700004  PEMA  ADD BRANCH LOCATIONS
000028*           AND SHIPPING ADDRESS TO ACCOUNT NOTES FILE
000029******************************************************************
000030 01  NOTE-FILE.
000031     12  NT-FILE-ID                  PIC XX.
000032         88  VALID-NOTE-ID              VALUE 'NT'.
000033
000034     12  NT-CONTROL-PRIMARY.
000035         16  NT-COMPANY-CD           PIC X.
000036         16  NT-ACCT-NOTE-KEY.
000037             18  NT-CARRIER              PIC X.
000038             18  NT-GROUPING             PIC X(06).
000039             18  NT-STATE                PIC XX.
000040             18  NT-ACCOUNT              PIC X(10).
000041         16  NT-RECORD-TYPE          PIC X.
000042              88  ACCT-NOTE          VALUE '1'.
000043              88  ACCT-BRANCH-LOC    VALUE '2'.
000044              88  ACCT-SHIPPING-ADDR VALUE '3'.
000045         16  NT-LINE-SEQUENCE        PIC S9(4)     COMP.
000046
000047     12  NT-LAST-MAINT-DT            PIC XX.
000048     12  NT-LAST-MAINT-BY            PIC X(4).
000049     12  NT-LAST-MAINT-HHMMSS        PIC S9(7) COMP-3.
000050
000051*  ALL NOTE LINES ARE RECORD TYPE '1' WITH ALMOST UNLIMITED
000052*     SEQUENCE NUMBERS
000053     12  NT-NOTE-INFORMATION.
000054         16  NT-NOTE-LINE            PIC X(60).
000055         16  FILLER                  PIC X(25).
000056*  BOTH BRANCH LOCATION LINES ARE RECORD TYPE '2' SEQ 1 AND 2
000057     12  NT-LOCATION-INFORMATION REDEFINES
000058                         NT-NOTE-INFORMATION.
000059         16  NT-BRANCH-LOC-LINE      PIC X(60).
000060         16  FILLER                  PIC X(25).
000061* Account special indicator is record type '2', sequence 3
000062     12  filler REDEFINES NT-NOTE-INFORMATION.
000063         16  nt-account-special      PIC X.
000064         16  FILLER                  PIC X(84).
000065*  ALL SHIPPING ADDRESS LINES ARE RECORD TYPE '3'AND
000066*     SEQUENCE NUMBER 1 IS NAME LINE 1
000067*     SEQUENCE NUMBER 2 IS NAME LINE 2
000068*     SEQUENCE NUMBER 3 IS ADDR LINE 1
000069*     SEQUENCE NUMBER 4 IS ADDR LINE 2
000070*     SEQUENCE NUMBER 5 IS ADDR LINE 3
000071*     SEQUENCE NUMBER 6 IS CITY, ST AND ZIP
000072     12  NT-SHIPPING-INFORMATION REDEFINES
000073                         NT-NOTE-INFORMATION.
000074         16  NT-SHIPPING-LINE        PIC X(60).
000075         16  NT-SHIP-STATE           PIC XX.
000076         16  NT-SHIP-ZIP             PIC X(10).
000077         16  FILLER                  PIC X(13).
000078*****************************************************************
      *<<((file: ERCACNT))
000623
000624 01  COMMISSION-WORK-AREA.
000625     05  WK-AGT-COMMS   OCCURS 10 TIMES.
000626         10  WK-AM-AGT           PIC X(10).
000627         10  WK-AM-TYPE          PIC X.
000628         10  WK-FILLER           PIC X(13).
000629         10  WK-GA-BILL-DT       PIC XX.
000630 EJECT
000631*    COPY ERCCOMP.
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
000632 EJECT
000633*    COPY ERCNAME.
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
000634 EJECT
000635*    COPY ERCRQST.
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
000636*EJECT
000637*    COPY ERCGXRF.
000638 EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE FUNCTION WHEN-COMPILED TO DFHEIVL0(1:21).
           MOVE '9#                    %   ' TO DFHEIV0.
           MOVE 'EL6501' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1
                DFHEIVL0.
000639 VCOBOL-DUMMY-PROCEDURE.
000640     MOVE EIBTRMID              TO QID-TERM.
000641
000642     MOVE EIBDATE               TO DC-JULIAN-YYDDD.
000643     MOVE '5'                   TO DC-OPTION-CODE.
000644     PERFORM 9700-DATE-LINK.
000645     MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
000646     MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
000647
000648***  Y2K PROJ 7744
000649     MOVE DC-GREG-DATE-1-YMD    TO  CYMD-CURRENT-SAVE
000650                                    CURRENT-SAVE
000651     MOVE DC-ALPHA-CENTURY      TO  CYMD-CURRENT-SAVE(1:2)
000652                                    CURRENT-SAVE(1:2)
000653***  Y2K PROJ 7744
000654
000655     MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
000656     IF EIBCALEN = 0
000657         GO TO 8800-UNAUTHORIZED-ACCESS.
000658
000659     MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN.
000660     MOVE LOW-VALUES             TO EL6501AI.
000661
000662     
      * EXEC CICS HANDLE CONDITION
000663*        PGMIDERR  (9600-PGMID-ERROR)
000664*        ERROR     (9990-ABEND)
000665*        QIDERR    (0500-CHECK-MAINT-TYPE)
000666*    END-EXEC.
      *    MOVE '"$L.N                 ! " #00005937' TO DFHEIV0
           MOVE X'22244C2E4E20202020202020' &
                X'202020202020202020202120' &
                X'2220233030303035393337' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000667
000668     IF PI-CALLING-PROGRAM NOT = THIS-PGM
000669         IF PI-CALLING-PROGRAM = XCTL-650
000670             MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
000671             MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
000672             MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
000673             MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
000674             MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
000675             MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
000676             MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
000677             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000678             PERFORM 5200-DELETE-TS THRU 5299-EXIT
000679             GO TO 0500-CHECK-MAINT-TYPE
000680         ELSE
000681             PERFORM 5200-RECOVER-TS  THRU  5299-EXIT
000682             MOVE THIS-PGM             TO PI-CALLING-PROGRAM
000683             GO TO 0600-DISPLAY-RECORD.
000684
000685     IF EIBAID = DFHCLEAR
000686         GO TO 9400-CLEAR.
000687
000688 EJECT
000689 0200-RECEIVE.
000690     IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
000691         MOVE ER-0008            TO EMI-ERROR
000692         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000693         MOVE -1                 TO PFENTERL
000694         GO TO 8200-SEND-DATAONLY.
000695
000696     
      * EXEC CICS RECEIVE
000697*        MAP      (MAP-NAME)
000698*        MAPSET   (MAPSET-NAME)
000699*        INTO     (EL6501AI)
000700*    END-EXEC.
           MOVE LENGTH OF
            EL6501AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005971' TO DFHEIV0
           MOVE X'382254204920204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303035393731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6501AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000701
000702     IF PFENTERL = 0
000703         GO TO 0300-CHECK-PFKEYS.
000704     IF EIBAID NOT = DFHENTER
000705         MOVE ER-0004            TO EMI-ERROR
000706         GO TO 0320-INPUT-ERROR.
000707     IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)
000708         MOVE PF-VALUES (PFENTERI) TO EIBAID
000709     ELSE
000710         MOVE ER-0029            TO EMI-ERROR
000711         GO TO 0320-INPUT-ERROR.
000712
000713 0300-CHECK-PFKEYS.
000714     IF EIBAID = DFHPF23
000715         GO TO 8810-PF23.
000716     IF EIBAID = DFHPF24
000717         GO TO 9200-RETURN-MAIN-MENU.
000718     IF EIBAID = DFHPF12
000719         GO TO 9500-PF12.
000720
000721* CHECK PF10 KEY FOR A CALL TO EL608 TO BROWSE RESIDENT STATE TAX
000722*             MASTER(ERRESC)
000723*
000724*    IF  EIBAID = DFHPF10
000725*        IF PI-COMPANY-ID = 'DMD'
000726*           PERFORM 0700-FORMAT-KEYDATA  THRU 0700-EXIT
000727*           MOVE LOW-VALUES TO WS-DMD-AGENT
000728*           PERFORM 6500-READ-ACCT       THRU 6599-EXIT
000729*           PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT
000730*           PERFORM 6000-MOVE-TO-SCREEN  THRU 6099-EXIT
000731*           MOVE EIBAID       TO PI-ENTRY-CD-1
000732*           MOVE WS-COMP-DATE TO PI-PREV-VG-ACCOUNT (11:5)
000733*           MOVE WS-DMD-AGENT TO PI-PREV-VG-ACCOUNT (1:10)
000734*           MOVE XCTL-608           TO PGM-NAME
000735*                                      PI-RETURN-TO-PROGRAM
000736*                                      PI-CALLING-PROGRAM
000737*           MOVE '106E'             TO PI-CURRENT-SCREEN-NO
000738*           GO TO 9300-XCTL.
000739
000740     IF (EIBAID = DFHPF1  OR  DFHPF2  OR DFHPF3 OR
000741                  DFHPF4  OR  DFHPF5  OR DFHPF7 OR
000742                  DFHPF8  OR  DFHPF9 OR DFHPF10)
000743         IF PI-COMPANY-ID = 'AIG' OR 'AUK'
000744             MOVE ER-0029        TO  EMI-ERROR
000745             GO TO 0320-INPUT-ERROR
000746         ELSE
000747             GO TO 0800-CHECK-MAINT.
000748
000749     IF EIBAID = DFHENTER
000750         GO TO 0330-EDIT-DATA.
000751
000752     MOVE ER-0029                TO EMI-ERROR.
000753
000754 0320-INPUT-ERROR.
000755     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000756     MOVE AL-UNBON               TO PFENTERA.
000757     MOVE -1                     TO PFENTERL.
000758     GO TO 8200-SEND-DATAONLY.
000759 EJECT
000760 0330-EDIT-DATA.
000761     IF MAINTI  = 'S'
000762        PERFORM 0700-FORMAT-KEYDATA  THRU 0700-EXIT
000763        PERFORM 6500-READ-ACCT       THRU 6599-EXIT
000764        PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT
000765        PERFORM 6000-MOVE-TO-SCREEN  THRU 6099-EXIT
000766        MOVE -1                  TO MAINTL
000767        GO TO 8100-SEND-INITIAL-MAP.
000768
000769     PERFORM 8000-STATE-REC-READ   THRU 8010-EXIT.
000770     PERFORM 6800-COMPANY-REC-READ THRU 6899-EXIT.
000771
000772     IF EMI-ERROR NOT = ZEROS
000773         MOVE -1                 TO MAINTL
000774         GO TO 8200-SEND-DATAONLY.
000775
000776     IF NOT MODIFY-CAP
000777         MOVE 'UPDATE'           TO SM-READ
000778         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
000779         MOVE ER-0070            TO EMI-ERROR
000780         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000781         GO TO 8100-SEND-INITIAL-MAP.
000782
000783     MOVE SAVE-BIN-DATE          TO BIN-CURRENT-SAVE.
000784
000785     IF MAINTI = 'A'
000786        GO TO 1000-ADD-A-RECORD.
000787
000788     IF MAINTI = 'C'
000789        IF PI-SV-MAINT  =  'L'
000790            MOVE -1             TO MAINTL
000791            MOVE AL-UABON       TO MAINTA
000792            MOVE ER-0899        TO EMI-ERROR
000793            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000794            GO TO 8200-SEND-DATAONLY
000795        ELSE
000796            GO TO 2000-CHANGE-A-RECORD.
000797
000798     IF MAINTI = 'L'
000799        MOVE 'L'                TO PI-SV-MAINT
000800        GO TO 3000-CHANGE-ALL-RECORDS.
000801
000802     MOVE ER-0023                TO EMI-ERROR.
000803     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000804     MOVE -1                     TO MAINTL.
000805     MOVE AL-UABON               TO MAINTA.
000806     GO TO 8200-SEND-DATAONLY.
000807 EJECT
000808 0500-CHECK-MAINT-TYPE.
000809     IF PI-MAINT = 'A'
000810        PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT
000811        PERFORM 0700-FORMAT-KEYDATA THRU 0700-EXIT
000812        MOVE -1                  TO NAMEL
000813        GO TO 8100-SEND-INITIAL-MAP.
000814
000815 0600-DISPLAY-RECORD.
000816     PERFORM 0700-FORMAT-KEYDATA  THRU 0700-EXIT.
000817     PERFORM 6500-READ-ACCT       THRU 6599-EXIT.
000818     PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT.
000819     PERFORM 6000-MOVE-TO-SCREEN  THRU 6099-EXIT.
000820
000821     IF PI-MAINT = 'A'
000822        MOVE -1                  TO NAMEL
000823     ELSE
000824        MOVE -1                  TO MAINTL.
000825
000826     GO TO 8100-SEND-INITIAL-MAP.
000827
000828 0700-FORMAT-KEYDATA.
000829     MOVE PI-MAINT               TO MAINTI.
000830
000831     IF PI-MAINT = 'A'
000832        MOVE AL-PANON            TO MAINTA
000833        SET T-INDEX TO 1
000834     ELSE
000835        SET T-INDEX              TO PI-LINE-SELECTED
000836        MOVE AL-UANON            TO MAINTA.
000837
000838     IF PI-2ND-PAGE
000839         SET T-INDEX UP BY 8
000840     ELSE
000841     IF PI-3RD-PAGE
000842         SET T-INDEX UP BY 16
000843     ELSE
000844     IF PI-LST-PAGE
000845         SET T-INDEX UP BY 24.
000846
000847     MOVE PI-ACCT-CARRIER        TO CARRI.
000848     MOVE PI-ACCT-GROUPING       TO GROUPINI.
000849     MOVE PI-ACCT-STATE          TO STATEI.
000850     MOVE PI-ACCT-ACCOUNT        TO ACCOUNTI.
000851
000852     IF PI-ACCT-EXP-DT = LOW-VALUES
000853        MOVE SPACES              TO EXPDTEI
000854     ELSE
000855        IF PI-ACCT-EXP-DT NOT = HIGH-VALUES
000856           MOVE PI-ACCT-EXP-DT      TO DC-BIN-DATE-1
000857           MOVE SPACE               TO DC-OPTION-CODE
000858           PERFORM 9700-DATE-LINK
000859           MOVE DC-GREG-DATE-1-EDIT TO EXPDTEI
000860        ELSE
000861           MOVE 999999              TO EXPDTEO
000862           INSPECT EXPDTEI CONVERTING SPACES TO '/'.
000863
000864***DMD CUSTOM CODE *****START
000865     MOVE EXPDTEI (1:2)             TO WS-YYYYMMDD (5:2).
000866     MOVE EXPDTEI (4:2)             TO WS-YYYYMMDD (7:2).
000867     MOVE EXPDTEI (6:2)             TO WS-YYYYMMDD (3:2).
000868     IF EXPDTEI (7:1) IS NOT EQUAL TO '9'
000869        MOVE '20'                 TO WS-YYYYMMDD (1:2)
000870     ELSE
000871        MOVE '19'                 TO WS-YYYYMMDD (1:2).
000872***DMD CUSTOM CODE *****END
000873
000874     MOVE PI-BIN-EFF-DT (T-INDEX) TO DC-BIN-DATE-1.
000875     MOVE SPACE                   TO DC-OPTION-CODE.
000876     PERFORM 9700-DATE-LINK.
000877     MOVE DC-GREG-DATE-1-EDIT     TO EFFDTEI.
000878
000879 0700-EXIT.
000880      EXIT.
000881
000882 0800-CHECK-MAINT.
000883     IF PI-MAINT = 'A'
000884        IF PI-RECORD-NOT-CREATED
000885           MOVE ER-2071          TO EMI-ERROR
000886           MOVE  -1              TO NAMEL
000887           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
000888           GO TO 8200-SEND-DATAONLY.
000889
000890     IF  EIBAID = DFHPF1
000891             AND
000892         CREDIT-SESSION
000893         PERFORM 5100-WRITE-TS THRU 5199-EXIT
000894         MOVE PI-ACCT-CARRIER    TO PI-CARRIER
000895         MOVE PI-ACCT-GROUPING   TO PI-GROUPING
000896         MOVE PI-ACCT-STATE      TO PI-STATE
000897         MOVE PI-ACCT-ACCOUNT    TO PI-ACCOUNT
000898         MOVE PI-ACCT-EXP-DT     TO PI-CERT-EFF-DT
000899         MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA
000900         MOVE XCTL-689           TO PGM-NAME
000901         GO TO 9300-XCTL.
000902
000903     IF  EIBAID = DFHPF2 AND CREDIT-SESSION
000904         PERFORM 5100-WRITE-TS THRU 5199-EXIT
000905         MOVE PI-ACCT-CARRIER    TO PI-CARRIER
000906         MOVE PI-ACCT-GROUPING   TO PI-GROUPING
000907         MOVE PI-ACCT-STATE      TO PI-STATE
000908         MOVE PI-ACCT-ACCOUNT    TO PI-ACCOUNT
000909         MOVE LOW-VALUES         TO PI-CERT-EFF-DT
000910         MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA
000911         MOVE XCTL-690           TO PGM-NAME
000912         GO TO 9300-XCTL
000913     END-IF.
000914
000915     IF EIBAID = DFHPF3
000916             AND
000917         CREDIT-SESSION
000918         PERFORM 5100-WRITE-TS THRU 5199-EXIT
000919         MOVE PI-ACCT-CARRIER    TO PI-CARRIER
000920         MOVE PI-ACCT-GROUPING   TO PI-GROUPING
000921         MOVE PI-ACCT-STATE      TO PI-STATE
000922         MOVE PI-ACCT-ACCOUNT    TO PI-ACCOUNT
000923         MOVE LOW-VALUES         TO PI-CERT-EFF-DT
000924         MOVE XCTL-653           TO PGM-NAME
000925         GO TO 9300-XCTL.
000926
000927     IF EIBAID = DFHPF4
000928         PERFORM 5100-WRITE-TS  THRU  5199-EXIT
000929         MOVE XCTL-652           TO PGM-NAME
000930         GO TO 9300-XCTL.
000931
000932     IF EIBAID = DFHPF5
000933         MOVE XCTL-6502          TO PGM-NAME
000934         GO TO 9300-XCTL.
000935
000936     IF EIBAID = DFHPF7
000937         MOVE XCTL-6504          TO PGM-NAME
000938         GO TO 9300-XCTL.
000939
000940     IF EIBAID = DFHPF8
000941         MOVE XCTL-6506          TO PGM-NAME
000942         GO TO 9300-XCTL.
000943
000944     IF EIBAID = DFHPF9
000945        MOVE XCTL-6505           TO PGM-NAME
000946        GO TO 9300-XCTL.
000947
000948     IF EIBAID = DFHPF10
000949        IF PI-COMPANY-ID = 'DMD'
000950           PERFORM 5100-WRITE-TS THRU 5199-EXIT
000951           MOVE XCTL-608         TO PGM-NAME
000952           GO TO 9300-XCTL.
000953
000954     MOVE ER-0029                TO EMI-ERROR.
000955     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000956     MOVE AL-UNBON               TO PFENTERA.
000957     MOVE -1                     TO PFENTERL.
000958     GO TO 8200-SEND-DATAONLY.
000959 EJECT
000960 1000-ADD-A-RECORD.
000961     IF PI-MAINT = 'A'
000962        PERFORM 4000-EDITS THRU 4900-EXIT
000963     ELSE
000964        MOVE ER-2180             TO EMI-ERROR
000965        MOVE -1                  TO MAINTL
000966        MOVE AL-UABON            TO MAINTA
000967        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
000968
000969     IF EMI-FORCABLE-CTR GREATER ZERO  OR
000970        EMI-FATAL-CTR    GREATER ZERO
000971         GO TO 8200-SEND-DATAONLY.
000972
000973     
      * EXEC CICS GETMAIN
000974*         LENGTH   (ACCT-REC-LEN)
000975*         SET      (ADDRESS OF ACCOUNT-MASTER)
000976*         INITIMG  (GETMAIN-SPACE)
000977*    END-EXEC.
      *    MOVE ',"IL                  $   #00006248' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036323438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACCT-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
000978
000979     MOVE ZEROS          TO WSS-LF-LM-AGE (1) WSS-LF-LM-AGE (2)
000980                            WSS-LF-LM-AGE (3) WSS-LF-LM-AGE (4)
000981                            WSS-LF-LM-DUR (1) WSS-LF-LM-DUR (2)
000982                            WSS-LF-LM-DUR (3) WSS-LF-LM-DUR (4)
000983                            WSS-LF-LM-AMT (1) WSS-LF-LM-AMT (2)
000984                            WSS-LF-LM-AMT (3) WSS-LF-LM-AMT (4).
000985
000986     MOVE ZEROS          TO WSS-AH-LM-AGE (1) WSS-LF-LM-AGE (2)
000987                            WSS-AH-LM-AGE (3) WSS-LF-LM-AGE (4)
000988                            WSS-AH-LM-DUR (1) WSS-LF-LM-DUR (2)
000989                            WSS-AH-LM-DUR (3) WSS-LF-LM-DUR (4)
000990                            WSS-AH-LM-AMT (1) WSS-LF-LM-AMT (2)
000991                            WSS-AH-LM-AMT (3) WSS-LF-LM-AMT (4)
000992                            WSS-AH-LM-MOA (1) WSS-AH-LM-MOA (2)
000993                            WSS-AH-LM-MOA (3) WSS-AH-LM-MOA (4).
000994
000995     MOVE WSS-AGT-COMMS          TO AM-AGT-COMMS (1)
000996                                    AM-AGT-COMMS (2)
000997                                    AM-AGT-COMMS (3)
000998                                    AM-AGT-COMMS (4)
000999                                    AM-AGT-COMMS (5)
001000                                    AM-AGT-COMMS (6)
001001                                    AM-AGT-COMMS (7)
001002                                    AM-AGT-COMMS (8)
001003                                    AM-AGT-COMMS (9)
001004                                    AM-AGT-COMMS (10).
001005
001006     IF PI-COMPANY-ID = 'FLI'
001007         MOVE ZEROS              TO AM-FLI-BANK-BALANCE
001008                                    AM-FLI-BANK-1ST-6-PREM
001009                                    AM-FLI-BANK-CAP-AMT
001010                                    AM-FLI-AGT-SHARE-PCT (1)
001011                                    AM-FLI-AGT-SHARE-PCT (2)
001012                                    AM-FLI-AGT-SHARE-PCT (3)
001013                                    AM-FLI-AGT-SHARE-PCT (4)
001014                                    AM-FLI-AGT-SHARE-PCT (5)
001015                                    AM-FLI-AGT-SHARE-PCT (6)
001016                                    AM-FLI-AGT-SHARE-PCT (7)
001017                                    AM-FLI-AGT-SHARE-PCT (8)
001018                                    AM-FLI-AGT-SHARE-PCT (9)
001019                                    AM-FLI-AGT-SHARE-PCT (10).
001020
001021     MOVE ZEROS                  TO AM-GPCD
001022                                    AM-LF-RET
001023                                    AM-AH-RET
001024                                    AM-LF-OB-RATE
001025                                    AM-LF-OB-RATE-JNT
001026                                    AM-AH-OB-RATE
001027                                    AM-AH-OB-RATE-JNT
001028                                    AM-REI-FEE-LF
001029                                    AM-REI-FEE-AH
001030                                    AM-REI-LF-TAX
001031                                    AM-REI-PR-PCT
001032                                    AM-REI-78-PCT
001033                                    AM-REI-AH-TAX
001034                                    AM-TOL-PREM
001035                                    AM-TOL-REF
001036                                    AM-CLP-TOL-PCT
001037                                    AM-TOL-CLM
001038                                    AM-CERTS-PURGED-DATE
001039                                    AM-HI-CERT-DATE
001040                                    AM-LO-CERT-DATE
001041                                    AM-1ST-PROD-DATE
001042                                    AM-GPCD
001043                                    AM-CAL-TABLE
001044                                    AM-LF-DEVIATION
001045                                    AM-LF-DEVIATION-PCT
001046                                    AM-AH-DEVIATION
001047                                    AM-AH-DEVIATION-PCT
001048                                    AM-ANNIVERSARY-DATE
001049                                    AM-TEL-NO
001050                                    AM-ENTRY-DATE
001051                                    AM-INACTIVE-DATE
001052                                    AM-RET-MIN-LOSS-L
001053                                    AM-RET-MIN-LOSS-A
001054                                    AM-LF-RPT021-EXP-PCT
001055                                    AM-AH-RPT021-EXP-PCT
001056                                    AM-MAX-MON-BEN
001057                                    AM-MAX-TOT-BEN
001058                                    AM-DCC-MAX-MARKETING-FEE
001059                                    AM-SPP-LEASE-COMM
001060                                    AM-RETRO-QUALIFY-LIMIT
001061                                    AM-RETRO-RET-PCT-LF (1)
001062                                    AM-RETRO-RET-PCT-LF (2)
001063                                    AM-RETRO-RET-PCT-LF (3)
001064                                    AM-RETRO-RET-THRU-LF (1)
001065                                    AM-RETRO-RET-THRU-LF (2)
001066                                    AM-RETRO-RET-THRU-LF (3)
001067                                    AM-RETRO-RET-PCT-AH (1)
001068                                    AM-RETRO-RET-PCT-AH (2)
001069                                    AM-RETRO-RET-PCT-AH (3)
001070                                    AM-RETRO-RET-THRU-AH (1)
001071                                    AM-RETRO-RET-THRU-AH (2)
001072                                    AM-RETRO-RET-THRU-AH (3).
001073
001074     IF PI-COMPANY-ID = 'DCC' or 'VPP'
001075        MOVE +100                TO AM-DCC-MAX-MARKETING-FEE
001076     END-IF
001077
001078     MOVE LOW-VALUES             TO PI-ACCT-REST-OF-EXP
001079                                    AM-AR-HI-CERT-DATE.
001080
001081     MOVE 'AM'                   TO AM-RECORD-ID.
001082     MOVE PI-ACCT-KEY            TO AM-CONTROL-PRIMARY
001083                                    AM-CONTROL-BY-VAR-GRP.
001084
001085     IF ST-ACCNT-CNTL
001086        MOVE SPACES              TO AM-VG-CARRIER
001087                                    AM-VG-GROUPING.
001088     IF CARR-ST-ACCNT-CNTL
001089        MOVE SPACES              TO AM-VG-GROUPING.
001090
001091     IF ACCNT-CNTL
001092        MOVE SPACES              TO AM-VG-CARRIER
001093                                    AM-VG-STATE
001094                                    AM-VG-GROUPING.
001095
001096     IF CARR-ACCNT-CNTL
001097        MOVE SPACES              TO AM-VG-STATE
001098                                    AM-VG-GROUPING.
001099
001100     MOVE CURRENT-SAVE           TO AM-ENTRY-DATE.
001101     MOVE BIN-EXPCHG-SAVE        TO AM-VG-EXPIRATION-DT.
001102     MOVE PI-BIN-EFF-DT (1)      TO AM-EFFECTIVE-DT.
001103
001104     MOVE BIN-CURRENT-SAVE       TO AM-LAST-MAINT-DT.
001105     MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.
001106     MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.
001107
001108     PERFORM 5000-MOVE-TO-RECORD THRU 5000-EXIT.
001109
001110     IF PI-COMPANY-ID = 'NCL'
001111         MOVE 'N'                TO AM-AH-ONLY-INDICATOR.
001112
001113
001114     IF EMI-FORCABLE-CTR GREATER ZERO  OR
001115        EMI-FATAL-CTR    GREATER ZERO
001116         GO TO 8200-SEND-DATAONLY.
001117
001118     IF PI-COMPANY-ID = 'DCC' or 'VPP'
001119        IF CLPSTL <= ZEROS
001120           MOVE ER-0144          TO  EMI-ERROR
001121           MOVE -1               TO  CLPSTL
001122           MOVE AL-UABON         TO  CLPSTA
001123           PERFORM 9900-ERROR-FORMAT
001124                                 THRU  9900-EXIT
001125           GO TO 8200-SEND-DATAONLY
001126        END-IF
001127     END-IF
001128
001129     MOVE AM-COMM-STRUCTURE      TO AM-COMM-STRUCTURE-SAVED.
001130     MOVE SPACE                  TO AM-COMM-CHANGE-STATUS.
001131
001132     MOVE +1 TO AXRF-SUB
001133                COMM-WORK-SUB.
001134
001135     PERFORM 6700-BUILD-COMM-WORK     THRU 6799-EXIT.
001136     PERFORM 6900-READ-AND-CHECK-AXRF THRU 6999-EXIT.
001137
001138     PERFORM 2100-ADD-ACCOUNT-NAME THRU 2100-EXIT.
001139
001140     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
001141         NEXT SENTENCE
001142     ELSE
001143         PERFORM 1500-ADD-COMP     THRU 1500-EXIT.
001144
001145     
      * EXEC CICS WRITE
001146*        FROM      (ACCOUNT-MASTER)
001147*        RIDFLD    (AM-CONTROL-PRIMARY)
001148*        DATASET   (ACCT-FILE-ID)
001149*    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006420' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036343230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001150
001151     PERFORM 6600-UPDATE-MAINT-DT THRU 6699-EXIT.
001152
001153     MOVE PI-MAINT               TO PI-SV-MAINT.
001154     MOVE 'C'                    TO PI-MAINT.
001155     MOVE LOW-VALUES             TO EL6501AI.
001156
001157     PERFORM 0700-FORMAT-KEYDATA THRU 0700-EXIT.
001158     PERFORM 6500-READ-ACCT      THRU 6599-EXIT.
001159     PERFORM 6000-MOVE-TO-SCREEN THRU 6099-EXIT.
001160
001161     IF PI-COMPANY-ID = 'NCL'
001162         MOVE ER-4011            TO EMI-ERROR
001163      ELSE
001164         MOVE ER-0000            TO EMI-ERROR.
001165
001166     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001167
001168     MOVE -1                     TO MAINTL.
001169     MOVE AL-UANON               TO MAINTA.
001170     MOVE '1'                    TO PI-RECORD-ADDED-SW.
001171     GO TO 8100-SEND-INITIAL-MAP.
001172
001173     EJECT
001174 1500-ADD-COMP.
001175     
      * EXEC CICS GETMAIN
001176*         LENGTH   (COMP-REC-LEN)
001177*         SET      (ADDRESS OF COMPENSATION-MASTER)
001178*         INITIMG  (GETMAIN-SPACE)
001179*    END-EXEC.
      *    MOVE ',"IL                  $   #00006450' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036343530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 COMP-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001180
001181     MOVE SPACES                 TO COMPENSATION-MASTER.
001182     MOVE ZEROS TO CO-BAL-FWD      CO-CUR-COM   CO-CUR-CHG
001183                   CO-CUR-PMT      CO-END-BAL   CO-CUR
001184                   CO-OV30         CO-OV60      CO-OV90
001185                   CO-YTD-COM      CO-YTD-OV    CO-CUR-OVR-UNDR
001186                   CO-YTD-OVR-UNDR CO-CUR-FICA CO-YTD-FICA
001187                   CO-LF-CLM-AMT   CO-AH-CLM-AMT
001188                   CO-CURRENT-BAL-FWD CO-CURRENT-CUR-COM
001189                   CO-CURRENT-CUR-CHG CO-CURRENT-END-BAL
001190                   CO-CURRENT-CUR     CO-CURRENT-OV30
001191                   CO-CURRENT-OV60    CO-CURRENT-OV90
001192                   CO-CURRENT-CUR-PMT
001193                   CO-CURRENT-YTD-COM CO-CURRENT-YTD-OV
001194                   CO-ACT-YEAR        CO-ACT-MONTH
001195                   CO-ACT-DAY         CO-LAST-STMT-YEAR
001196                   CO-LAST-STMT-MONTH CO-LAST-STMT-DAY
001197                   CO-CURRENT-LAST-STMT-YEAR
001198                   CO-CURRENT-LAST-STMT-MONTH
001199                   CO-CURRENT-LAST-STMT-DAY
001200                   CO-YTD-PAID-COM
001201                   CO-YTD-PAID-OV
001202                   co-ov120 co-current-ov120
001203
001204     MOVE BIN-CURRENT-SAVE       TO CO-LAST-MAINT-DT.
001205     MOVE PI-PROCESSOR-ID        TO CO-LAST-MAINT-USER.
001206     MOVE EIBTIME                TO CO-LAST-MAINT-HHMMSS.
001207     MOVE PI-CR-MONTH-END-DT     TO CO-ROLADEX-PRINT-DT.
001208     MOVE PI-COMPANY-CD          TO CO-COMPANY-CD.
001209
001210     IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
001211         MOVE ZEROS              TO CO-CARRIER
001212     ELSE
001213         MOVE AM-CARRIER         TO CO-CARRIER.
001214
001215     IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
001216         MOVE ZEROS              TO CO-GROUPING
001217     ELSE
001218         MOVE AM-GROUPING        TO CO-GROUPING.
001219
001220     MOVE 'CO'                   TO CO-RECORD-ID
001221     MOVE 'A'                    TO CO-TYPE.
001222     MOVE AM-NAME                TO CO-ACCT-NAME.
001223     MOVE AM-PERSON              TO CO-MAIL-NAME.
001224     MOVE AM-ADDRS               TO CO-ADDR-1.
001225*    MOVE AM-CITY                TO CO-ADDR-3.
001226     MOVE AM-ADDR-CITY           TO CO-ADDR-CITY
001227     MOVE AM-ADDR-STATE          TO CO-ADDR-STATE
001228     MOVE AM-ZIP                 TO CO-ZIP.
001229     MOVE AM-CSR-CODE            TO CO-CSR-CODE.
001230     MOVE AM-TEL-NO              TO CO-TELEPHONE.
001231
001232     IF PI-COMPANY-ID = 'NCL'
001233         MOVE AM-ID-NO           TO  WS-SOC-SEC-WORK
001234         IF WS-SS-TYPE = 'C' OR 'P' OR 'S' OR 'T' OR
001235                         'X' OR 'I'
001236             MOVE WS-SS-TYPE     TO  CO-TYPE-AGENT
001237             MOVE WS-SOC-SEC     TO  CO-SOC-SEC
001238         ELSE
001239             MOVE SPACE          TO  CO-TYPE-AGENT
001240             MOVE AM-ID-NO       TO  CO-SOC-SEC
001241     ELSE
001242         MOVE AM-ID-NO           TO CO-SOC-SEC.
001243
001244     IF AM-REMIT-TO NOT = ZERO
001245         MOVE AM-AGT (AM-REMIT-TO) TO CO-RESP-NO
001246       ELSE
001247         MOVE AM-ACCOUNT           TO CO-RESP-NO.
001248
001249     MOVE +0                      TO AGT-SUB.
001250
001251     PERFORM 1600-FIND-C-AGT THRU 1600-EXIT.
001252
001253     IF VALID-AGT-SW = 'Y'
001254         NEXT SENTENCE
001255     ELSE
001256         GO TO 1500-EXIT.
001257
001258     IF CO-RESP-NO = CO-ACCOUNT
001259         MOVE 'Y'                  TO CO-BALANCE-CONTROL
001260     ELSE
001261         MOVE 'N'                  TO CO-BALANCE-CONTROL.
001262
001263     IF PI-AR-PROCESSING
001264        MOVE 'G'                   TO CO-AR-REPORTING
001265        MOVE '1'                   TO CO-AR-BAL-LEVEL
001266        MOVE 'Y'                   TO CO-AR-PULL-CHECK
001267        MOVE 'Y'                   TO CO-AR-NORMAL-PRINT.
001268
001269     IF PI-COMPANY-ID = 'CRI' OR 'HER' OR 'HSL'
001270        MOVE 'N'                   TO CO-AR-REPORTING.
001271
001272     IF PI-COMPANY-ID = 'HER' OR 'HSL'
001273        MOVE '999999'              TO CO-AR-SUMMARY-CODE.
001274
001275     PERFORM 2200-ADD-COMPENSATION-NAME THRU 2200-EXIT.
001276
001277     
      * EXEC CICS HANDLE CONDITION
001278*         DUPREC   (1500-EXIT)
001279*    END-EXEC.
      *    MOVE '"$%                   ! # #00006552' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2320233030303036353532' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001280
001281     
      * EXEC CICS WRITE
001282*        FROM      (COMPENSATION-MASTER)
001283*        RIDFLD    (CO-CONTROL-PRIMARY)
001284*        DATASET   (COMP-FILE-ID)
001285*    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006556' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036353536' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001286
001287 1500-EXIT.
001288      EXIT.
001289
001290 1600-FIND-C-AGT.
001291     ADD 1 TO  AGT-SUB.
001292     IF AGT-SUB GREATER 10
001293         MOVE 'N'                TO  VALID-AGT-SW
001294         GO TO 1600-EXIT.
001295
001296************************************************************
001297**  MODIFICATION MADE TO ADD SERVICE FEES                 **
001298************************************************************
001299
001300     IF AM-COM-TYP (AGT-SUB) = 'C' OR 'D' OR 'F'
001301         MOVE AM-AGT (AGT-SUB)   TO  CO-ACCOUNT
001302         MOVE 'Y'                TO  VALID-AGT-SW
001303         GO TO 1600-EXIT
001304     ELSE
001305         GO TO 1600-FIND-C-AGT.
001306
001307 1600-EXIT.
001308     EXIT.
001309 EJECT
001310 1700-CHANGE-COMP.
001311     MOVE 'N'                    TO CHANGE-WAS-MADE-SW.
001312     MOVE PI-COMPANY-CD          TO COMP-COMPANY-CD.
001313
001314     IF PI-ZERO-CARRIER
001315       OR PI-ZERO-CAR-GROUP
001316         MOVE ZEROS              TO COMP-CARRIER
001317     ELSE
001318         MOVE AM-CARRIER         TO COMP-CARRIER.
001319
001320     IF PI-ZERO-GROUPING
001321       OR PI-ZERO-CAR-GROUP
001322         MOVE ZEROS              TO COMP-GROUPING
001323     ELSE
001324         MOVE AM-GROUPING        TO COMP-GROUPING.
001325
001326     IF AM-REMIT-TO NOT = ZERO
001327         MOVE AM-AGT (AM-REMIT-TO)
001328                                 TO COMP-RESP-NO
001329     ELSE
001330         MOVE AM-ACCOUNT         TO COMP-RESP-NO.
001331
001332     MOVE ZERO                   TO AGT-SUB.
001333
001334     PERFORM 1800-FIND-C-AGT THRU 1899-EXIT.
001335
001336     IF VALID-AGT-SW = 'Y'
001337         NEXT SENTENCE
001338     ELSE
001339         GO TO 1799-EXIT.
001340
001341     MOVE 'A'                    TO COMP-TYPE.
001342
001343     
      * EXEC CICS HANDLE CONDITION
001344*         ENDFILE     (1799-EXIT)
001345*         NOTFND      (1799-EXIT)
001346*    END-EXEC.
      *    MOVE '"$''I                  ! $ #00006618' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'2420233030303036363138' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001347
001348     
      * EXEC CICS READ
001349*        UPDATE
001350*        DATASET   (COMP-FILE-ID)
001351*        SET       (ADDRESS OF COMPENSATION-MASTER)
001352*        RIDFLD    (COMP-KEY)
001353*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006623' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036363233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001354
001355     IF (ON-LAST-DATE-RANGE)
001356        IF NAMEL GREATER ZEROS
001357           MOVE 'Y'              TO CHANGE-WAS-MADE-SW
001358           MOVE AM-NAME          TO CO-ACCT-NAME
001359        END-IF
001360
001361        IF INCAREL GREATER ZEROS
001362           MOVE 'Y'              TO CHANGE-WAS-MADE-SW
001363           MOVE AM-PERSON        TO CO-MAIL-NAME
001364        END-IF
001365
001366        IF ADDR1L GREATER ZEROS
001367           MOVE 'Y'              TO CHANGE-WAS-MADE-SW
001368           MOVE AM-ADDRS         TO CO-ADDR-1
001369        END-IF
001370
001371        IF ACITYL  GREATER ZEROS
001372           MOVE 'Y'              TO CHANGE-WAS-MADE-SW
001373           MOVE AM-ADDR-CITY     TO CO-ADDR-CITY
001374        END-IF
001375
001376        IF ASTATEL  GREATER ZEROS
001377           MOVE 'Y'              TO CHANGE-WAS-MADE-SW
001378           MOVE AM-ADDR-STATE    TO CO-ADDR-STATE
001379        END-IF
001380
001381        IF ZIPL GREATER ZEROS
001382           MOVE 'Y'              TO CHANGE-WAS-MADE-SW
001383           MOVE AM-ZIP           TO CO-ZIP
001384        END-IF
001385
001386        IF CSRL GREATER ZEROS
001387           MOVE 'Y'              TO CHANGE-WAS-MADE-SW
001388           MOVE AM-CSR-CODE      TO CO-CSR-CODE
001389        END-IF
001390
001391        IF PHONEL GREATER ZEROS
001392           MOVE 'Y'              TO CHANGE-WAS-MADE-SW
001393           MOVE AM-TEL-NO        TO CO-TELEPHONE
001394        END-IF
001395
001396        IF TAXNOL GREATER ZEROS
001397           MOVE 'Y'              TO CHANGE-WAS-MADE-SW
001398           MOVE AM-ID-NO         TO CO-SOC-SEC
001399        END-IF
001400        IF AUTO-CANCEL-ACCOUNT
001401           IF CO-BILL-SW = ' '
001402              MOVE 'B'           TO CO-BILL-SW
001403              MOVE 'Y'           TO CHANGE-WAS-MADE-SW
001404           END-IF
001405        END-IF
001406     END-IF
001407
001408     IF PCONTL > 0
001409         MOVE 'Y'                TO CHANGE-WAS-MADE-SW
001410         MOVE AM-CONTROL-NAME    TO CO-CONTROL-NAME
001411     END-IF
001412
001413     IF CHANGE-WAS-MADE
001414         MOVE BIN-CURRENT-SAVE   TO CO-LAST-MAINT-DT
001415         MOVE PI-PROCESSOR-ID    TO CO-LAST-MAINT-USER
001416         MOVE EIBTIME            TO CO-LAST-MAINT-HHMMSS.
001417
001418     IF NAMEL GREATER THAN ZERO
001419         PERFORM 2200-ADD-COMPENSATION-NAME THRU 2200-EXIT.
001420
001421     
      * EXEC CICS REWRITE
001422*        FROM      (COMPENSATION-MASTER)
001423*        DATASET   (COMP-FILE-ID)
001424*    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006696' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036363936' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001425
001426 1799-EXIT.
001427     EXIT.
001428 EJECT
001429 1800-FIND-C-AGT.
001430     ADD 1                       TO  AGT-SUB.
001431
001432     IF AGT-SUB GREATER 10
001433         MOVE 'N'                TO  VALID-AGT-SW
001434         GO TO 1899-EXIT.
001435
001436************************************************************
001437**  MODIFICATION MADE TO ADD SERVICE FEES                 **
001438************************************************************
001439
001440     IF AM-COM-TYP (AGT-SUB) = 'C' OR 'D' OR 'F'
001441         MOVE AM-AGT (AGT-SUB)   TO  COMP-ACCOUNT
001442         MOVE 'Y'                TO  VALID-AGT-SW
001443         GO TO 1899-EXIT
001444     ELSE
001445         GO TO 1800-FIND-C-AGT.
001446
001447 1899-EXIT.
001448     EXIT.
001449 EJECT
001450 2000-CHANGE-A-RECORD.
001451     PERFORM 4000-EDITS THRU 4900-EXIT.
001452
001453     IF EMI-FORCABLE-CTR GREATER ZERO  OR
001454        EMI-FATAL-CTR    GREATER ZERO
001455         GO TO 8200-SEND-DATAONLY.
001456
001457     
      * EXEC CICS READ
001458*        UPDATE
001459*        DATASET   (ACCT-FILE-ID)
001460*        SET       (ADDRESS OF ACCOUNT-MASTER)
001461*        RIDFLD    (PI-ACCT-KEY)
001462*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006732' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036373332' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001463
001464     PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT.
001465
001466     IF NOT PI-AR-PROCESSING
001467        GO TO 2075-PROCESS-CHANGE.
001468
001469     MOVE AM-AGT (AM-REMIT-TO)   TO SAVE-FIN-RESP.
001470     MOVE +0                     TO SUB1.
001471
001472 2075-PROCESS-CHANGE.
001473     PERFORM 5000-MOVE-TO-RECORD THRU 5000-EXIT.
001474
001475     IF PI-AR-PROCESSING
001476        PERFORM 5300-UPDATE-REQUEST-FILE THRU 5399-EXIT.
001477
001478     IF PI-COMPANY-ID = 'DMD'
001479     IF AM-ANNIVERSARY-DATE = ZEROS
001480           MOVE ER-0852             TO EMI-ERROR
001481           MOVE -1                  TO CONTRL
001482           MOVE AL-UABON            TO CONTRA
001483           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001484
001485     IF EMI-FORCABLE-CTR GREATER ZERO  OR
001486        EMI-FATAL-CTR    GREATER ZERO
001487         GO TO 8200-SEND-DATAONLY.
001488
001489     MOVE +1                     TO AXRF-SUB
001490                                    COMM-WORK-SUB.
001491     PERFORM 6900-READ-AND-CHECK-AXRF THRU 6999-EXIT.
001492
001493     MOVE BIN-CURRENT-SAVE       TO AM-LAST-MAINT-DT.
001494     MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.
001495     MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.
001496
001497     IF ((PI-FST-PAGE)
001498        AND (PI-TOTAL-LINES = PI-LINE-SELECTED))
001499                      OR
001500        ((PI-2ND-PAGE)
001501        AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 8))
001502                      OR
001503        ((PI-3RD-PAGE)
001504        AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 16))
001505                      OR
001506        ((PI-LST-PAGE)
001507        AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 24))
001508        SET ON-LAST-DATE-RANGE   TO TRUE
001509     END-IF
001510
001511     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
001512         NEXT SENTENCE
001513     ELSE
001514         IF (ON-LAST-DATE-RANGE)
001515                          OR
001516            ((PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL') AND
001517             (PCONTL > ZERO))
001518             PERFORM 1700-CHANGE-COMP THRU 1799-EXIT.
001519
001520     IF NAMEL GREATER THAN ZERO
001521         PERFORM 2100-ADD-ACCOUNT-NAME THRU 2100-EXIT.
001522
001523     
      * EXEC CICS REWRITE
001524*        FROM      (ACCOUNT-MASTER)
001525*        DATASET   (ACCT-FILE-ID)
001526*    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006798' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303036373938' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001527
001528     PERFORM 6600-UPDATE-MAINT-DT THRU 6699-EXIT.
001529
001530     IF CSRL GREATER ZEROS
001531         GO TO 3500-CHANGE-ALL-CSR.
001532
001533     MOVE ER-0000                TO EMI-ERROR.
001534     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001535     MOVE LOW-VALUES             TO EL6501AI.
001536
001537     PERFORM 0700-FORMAT-KEYDATA THRU 0700-EXIT.
001538     PERFORM 6500-READ-ACCT      THRU 6599-EXIT.
001539     PERFORM 6000-MOVE-TO-SCREEN THRU 6099-EXIT.
001540
001541     MOVE ER-0000                TO EMI-ERROR.
001542     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001543
001544     MOVE -1                     TO MAINTL.
001545     MOVE AL-UANON               TO MAINTA.
001546     GO TO 8100-SEND-INITIAL-MAP.
001547
001548 EJECT
001549 2100-ADD-ACCOUNT-NAME.
001550     
      * EXEC CICS HANDLE CONDITION
001551*         DUPREC   (2100-EXIT)
001552*    END-EXEC.
      *    MOVE '"$%                   ! % #00006825' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2520233030303036383235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001553
001554     
      * EXEC CICS GETMAIN
001555*         LENGTH   (NAME-REC-LEN)
001556*         SET      (ADDRESS OF NAME-LOOKUP-MASTER)
001557*         INITIMG  (GETMAIN-SPACE)
001558*    END-EXEC.
      *    MOVE ',"IL                  $   #00006829' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036383239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 NAME-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001559
001560     MOVE SPACES                 TO  NAME-LOOKUP-MASTER.
001561     MOVE BIN-CURRENT-SAVE       TO  NL-LAST-MAINT-DT.
001562     MOVE PI-PROCESSOR-ID        TO  NL-LAST-MAINT-USER.
001563     MOVE EIBTIME                TO  NL-LAST-MAINT-HHMMSS.
001564     MOVE PI-COMPANY-CD          TO  NL-COMPANY-CD.
001565     MOVE AM-NAME                TO  NL-NAME.
001566     IF NL-NAME (1:4) = 'THE ' OR 'The ' OR 'the '
001567        MOVE NL-NAME (5:26)      TO NL-NAME
001568     END-IF
001569     MOVE AM-ADDR-CITY           TO  NL-CITY.
001570     MOVE AM-ADDR-STATE          TO  NL-ST.
001571     MOVE 'A'                    TO  NL-RECORD-TYPE.
001572     MOVE PI-COMPANY-CD          TO  NL-AM-COMPANY-CD.
001573     MOVE AM-CARRIER             TO  NL-AM-CARRIER.
001574     MOVE AM-GROUPING            TO  NL-AM-GROUPING.
001575     MOVE AM-STATE               TO  NL-AM-STATE.
001576     MOVE AM-ACCOUNT             TO  NL-AM-ACCOUNT.
001577
001578     
      * EXEC CICS WRITE
001579*        FROM      (NAME-LOOKUP-MASTER)
001580*        RIDFLD    (NL-CONTROL-PRIMARY)
001581*        DATASET   (NAME-FILE-ID)
001582*    END-EXEC.
           MOVE LENGTH OF
            NAME-LOOKUP-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006853' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036383533' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NAME-FILE-ID, 
                 NAME-LOOKUP-MASTER, 
                 DFHEIV11, 
                 NL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001583
001584 2100-EXIT.
001585     EXIT.
001586 EJECT
001587 2200-ADD-COMPENSATION-NAME.
001588     
      * EXEC CICS HANDLE CONDITION
001589*         DUPREC   (2200-EXIT)
001590*    END-EXEC.
      *    MOVE '"$%                   ! & #00006863' TO DFHEIV0
           MOVE X'222425202020202020202020' &
                X'202020202020202020202120' &
                X'2620233030303036383633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001591
001592     
      * EXEC CICS GETMAIN
001593*         LENGTH   (NAME-REC-LEN)
001594*         SET      (ADDRESS OF NAME-LOOKUP-MASTER)
001595*         INITIMG  (GETMAIN-SPACE)
001596*    END-EXEC.
      *    MOVE ',"IL                  $   #00006867' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036383637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 NAME-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001597
001598     MOVE SPACES                 TO  NAME-LOOKUP-MASTER.
001599     MOVE BIN-CURRENT-SAVE       TO  NL-LAST-MAINT-DT.
001600     MOVE PI-PROCESSOR-ID        TO  NL-LAST-MAINT-USER.
001601     MOVE EIBTIME                TO  NL-LAST-MAINT-HHMMSS.
001602     MOVE PI-COMPANY-CD          TO  NL-COMPANY-CD.
001603     MOVE CO-ACCT-NAME           TO  NL-NAME.
001604     IF NL-NAME (1:4) = 'THE ' OR 'The ' OR 'the '
001605        MOVE NL-NAME (5:26)      TO NL-NAME
001606     END-IF
001607     MOVE CO-ADDR-CITY           TO  NL-CITY.
001608     MOVE CO-ADDR-STATE          TO  NL-ST
001609     MOVE 'C'                    TO  NL-RECORD-TYPE.
001610     MOVE PI-COMPANY-CD          TO  NL-CO-COMPANY-CD.
001611     MOVE CO-CARRIER             TO  NL-CO-CARRIER.
001612     MOVE CO-GROUPING            TO  NL-CO-GROUPING.
001613     MOVE CO-RESP-NO             TO  NL-CO-RESP-NO.
001614     MOVE CO-ACCOUNT             TO  NL-CO-ACCOUNT.
001615     MOVE CO-TYPE                TO  NL-CO-TYPE.
001616
001617     
      * EXEC CICS WRITE
001618*        FROM      (NAME-LOOKUP-MASTER)
001619*        RIDFLD    (NL-CONTROL-PRIMARY)
001620*        DATASET   (NAME-FILE-ID)
001621*    END-EXEC.
           MOVE LENGTH OF
            NAME-LOOKUP-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006892' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303036383932' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NAME-FILE-ID, 
                 NAME-LOOKUP-MASTER, 
                 DFHEIV11, 
                 NL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001622
001623 2200-EXIT.
001624     EXIT.
001625 EJECT
001626 3000-CHANGE-ALL-RECORDS.
001627     MOVE LOW-VALUES             TO PI-ACCT-REST-OF-EXP.
001628     MOVE PI-ACCT-KEY            TO WS-KEY-SAVE.
001629
001630     
      * EXEC CICS HANDLE CONDITION
001631*         ENDFILE     (3030-END-FILE)
001632*    END-EXEC.
      *    MOVE '"$''                   ! '' #00006905' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'2720233030303036393035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001633
001634     PERFORM 6300-STARTBR THRU 6300-EXIT.
001635
001636 3010-READ-NEXT.
001637     PERFORM 6310-READNEXT THRU 6310-EXIT.
001638
001639     IF PI-ACCT-CCGSA-KEY NOT = WS-ACCT-CCGSA-KEY
001640        GO TO 3030-END-FILE.
001641
001642     
      * EXEC CICS ENDBR
001643*        DATASET   (ACCT-FILE-ID)
001644*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006917' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303036393137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001645
001646     ADD +1                      TO WS-CHANGE-ALL-CNT
001647
001648     
      * EXEC CICS READ
001649*        UPDATE
001650*        DATASET   (ACCT-FILE-ID)
001651*        SET       (ADDRESS OF ACCOUNT-MASTER)
001652*        RIDFLD    (WS-KEY-SAVE)
001653*    END-EXEC.
      *    MOVE '&"S        EU         (   #00006923' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303036393233' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-KEY-SAVE, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001654
001655     IF NAMEL    GREATER THAN ZERO  OR
001656        INCAREL  GREATER THAN ZERO  OR
001657        ADDR1L   GREATER THAN ZERO  OR
001658        ACITYL   GREATER THAN ZERO  OR
001659        ASTATEL  GREATER THAN ZERO  OR
001660        ZIPL     GREATER THAN ZERO  OR
001661        CLPTOLPL GREATER THAN ZERO  OR
001662        PHONEL   GREATER THAN ZERO
001663         MOVE '*'                TO AM-PROFILE-CHANGE-SWITCH.
001664
001665     IF STATUSL > ZERO
001666        EVALUATE TRUE
001667           WHEN STATUSI = 'A'
001668              MOVE '0'           TO AM-STATUS
001669           WHEN STATUSI = 'I'
001670              MOVE '1'           TO AM-STATUS
001671           WHEN (STATUSI = 'C')
001672              AND (AM-STATUS NOT = '3' AND 'C')
001673              MOVE '3'           TO AM-STATUS
001674              SET AUTO-CANCEL-ACCOUNT
001675                                 TO TRUE
001676           WHEN (STATUSI = 'C')
001677              MOVE '3'           TO AM-STATUS
001678           WHEN STATUSI = 'F'
001679              MOVE '4'           TO AM-STATUS
001680           WHEN STATUSI = 'S'
001681              MOVE '5'           TO AM-STATUS
001682           WHEN STATUSI = 'D'
001683              MOVE '6'           TO AM-STATUS
001684           WHEN STATUSI = 'L'
001685              MOVE '7'           TO AM-STATUS
001686           WHEN STATUSI = 'R'
001687              MOVE '8'           TO AM-STATUS
001688           WHEN STATUSI = 'P'
001689              MOVE '9'           TO AM-STATUS
001690
001691           WHEN OTHER
001692              MOVE -1            TO STATUSL
001693              MOVE AL-UABON      TO STATUSA
001694              MOVE ER-2153       TO EMI-ERROR
001695              PERFORM 9900-ERROR-FORMAT
001696                                 THRU 9900-EXIT
001697        END-EVALUATE
001698     END-IF
001699
001700*    IF STATUSL > ZERO
001701*       IF STATUSI = 'A'
001702*          MOVE '0'              TO AM-STATUS
001703*       ELSE
001704*          IF STATUSI = 'I'
001705*             MOVE '1'           TO AM-STATUS
001706*          ELSE
001707*             IF STATUSI = 'C'
001708*                IF AM-STATUS NOT = '3' AND 'C'
001709*                   SET AUTO-CANCEL-ACCOUNT
001710*                                TO TRUE
001711*                END-IF
001712*                MOVE '3'        TO AM-STATUS
001713*             ELSE
001714*                IF STATUSI = 'F'
001715*                   MOVE '4'     TO AM-STATUS
001716*                ELSE
001717*                   MOVE -1         TO STATUSL
001718*                   MOVE AL-UABON   TO STATUSA
001719*                   MOVE ER-2153    TO EMI-ERROR
001720*                   PERFORM 9900-ERROR-FORMAT
001721*                                THRU 9900-EXIT
001722*                END-IF
001723*             END-IF
001724*          END-IF
001725*       END-IF
001726*    END-IF
001727
001728     IF CSRL GREATER ZERO
001729        MOVE CSRI                TO AM-CSR-CODE.
001730     IF NAMEL GREATER THAN ZERO
001731        MOVE NAMEI               TO AM-NAME.
001732     IF PCONTL GREATER THAN ZERO
001733        MOVE PCONTI              TO AM-CONTROL-NAME
001734     END-IF
001735     IF INCAREL GREATER ZEROS
001736        MOVE INCAREI             TO AM-PERSON.
001737     IF ADDR1L GREATER ZEROS
001738        MOVE ADDR1I              TO AM-ADDRS.
001739     IF ACITYL GREATER ZEROS
001740        MOVE ACITYI             TO AM-ADDR-CITY.
001741     IF ASTATEL GREATER ZEROS
001742        MOVE ASTATEI            TO AM-ADDR-STATE.
001743     IF ZIPL NOT GREATER ZEROS
001744         GO TO 3015-CONT-CHANGING.
001745
001746     MOVE ZIPI                   TO WS-ZIP-CODE.
001747
001748     IF WS-CANADIAN-ZIP
001749         IF WS-ZIP-4 = SPACE  OR  '-'
001750             MOVE WS-ZIP-CAN-2-POST1  TO AM-CAN-POSTAL-1
001751             MOVE WS-ZIP-CAN-2-POST2  TO AM-CAN-POSTAL-2
001752         ELSE
001753             MOVE WS-ZIP-CAN-1-POST1  TO AM-CAN-POSTAL-1
001754             MOVE WS-ZIP-CAN-1-POST2  TO AM-CAN-POSTAL-2
001755     ELSE
001756         IF WS-ZIP-6 = SPACE  OR  '-'
001757             MOVE WS-ZIP-AM-2-CODE    TO AM-ZIP-PRIME
001758             MOVE WS-ZIP-AM-2-PLUS4   TO AM-ZIP-PLUS4
001759         ELSE
001760             MOVE WS-ZIP-AM-1-CODE    TO AM-ZIP-PRIME
001761             MOVE WS-ZIP-AM-1-PLUS4   TO AM-ZIP-PLUS4.
001762
001763 3015-CONT-CHANGING.
001764
001765     IF CLPTOLPL GREATER ZEROS
001766         
      * EXEC CICS BIF
001767*            DEEDIT
001768*            FIELD    (CLPTOLPI)
001769*            LENGTH   (6)
001770*        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007041' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037303431' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLPTOLPI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001771         IF CLPTOLPI NUMERIC
001772             MOVE CLPTOLPI            TO AM-CLP-TOL-PCT
001773         ELSE
001774             MOVE -1                  TO CLPTOLPL
001775             MOVE AL-UNBON            TO CLPTOLPA
001776             MOVE ER-1778             TO EMI-ERROR
001777             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
001778         END-IF
001779     END-IF
001780
001781     IF LCOMML > ZEROS
001782        
      * EXEC CICS BIF
001783*            DEEDIT
001784*            FIELD    (LCOMMI)
001785*            LENGTH   (8)
001786*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007057' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037303537' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCOMMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001787         IF LCOMMI NUMERIC
001788            MOVE LCOMMI          TO AM-SPP-LEASE-COMM
001789         ELSE
001790            MOVE -1              TO LCOMML
001791            MOVE AL-UNBON        TO LCOMMA
001792            MOVE ER-1778         TO EMI-ERROR
001793            PERFORM 9900-ERROR-FORMAT
001794                                 THRU 9900-EXIT
001795         END-IF
001796     END-IF
001797
001798     IF PHONEL GREATER ZEROS
001799        MOVE PHONEI              TO DEEDIT-FIELD
001800        PERFORM 8600-DEEDIT
001801        MOVE DEEDIT-FIELD-V0     TO WS-PHONE-NUM
001802        MOVE WS-PH1              TO AM-AREA-CODE
001803        MOVE WS-PH2              TO AM-TEL-PRE
001804        MOVE WS-PH3              TO AM-TEL-NBR.
001805
001806*    IF PSILFFL GREATER +0
001807*       MOVE PSILFFI             TO DEEDIT-FIELD
001808*       PERFORM 8600-DEEDIT
001809*       IF DEEDIT-FIELD NUMERIC
001810*          MOVE DEEDIT-FIELD-V5  TO AM-LF-PSI-FACTOR.
001811*
001812*    IF PSIAHFL GREATER +0
001813*       MOVE PSIAHFI             TO DEEDIT-FIELD
001814*       PERFORM 8600-DEEDIT
001815*       IF DEEDIT-FIELD NUMERIC
001816*          MOVE DEEDIT-FIELD-V5  TO AM-AH-PSI-FACTOR.
001817
001818     MOVE BIN-CURRENT-SAVE       TO AM-LAST-MAINT-DT.
001819     MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.
001820     MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.
001821
001822     IF ((PI-FST-PAGE)
001823        AND (PI-TOTAL-LINES = WS-CHANGE-ALL-CNT))
001824                      OR
001825        ((PI-2ND-PAGE)
001826        AND (PI-TOTAL-LINES = WS-CHANGE-ALL-CNT + 8))
001827                      OR
001828        ((PI-3RD-PAGE)
001829        AND (PI-TOTAL-LINES = WS-CHANGE-ALL-CNT + 16))
001830                      OR
001831        ((PI-LST-PAGE)
001832        AND (PI-TOTAL-LINES = WS-CHANGE-ALL-CNT + 24))
001833        SET ON-LAST-DATE-RANGE   TO TRUE
001834     END-IF
001835
001836*    IF ((PI-FST-PAGE)
001837*       AND (PI-TOTAL-LINES = PI-LINE-SELECTED))
001838*                     OR
001839*       ((PI-2ND-PAGE)
001840*       AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 8))
001841*                     OR
001842*       ((PI-3RD-PAGE)
001843*       AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 16))
001844*                     OR
001845*       ((PI-LST-PAGE)
001846*       AND (PI-TOTAL-LINES = PI-LINE-SELECTED + 24))
001847*       SET ON-LAST-DATE-RANGE   TO TRUE
001848*    END-IF
001849
001850     MOVE PI-PAGE-NUMBER TO WS-PAGE-NUMBER
001851     MOVE PI-TOTAL-LINES TO WS-TOTAL-LINES
001852     MOVE PI-LINE-SELECTED TO WS-LINE-SELECTED
001853
001854*    EXEC CICS DELAY
001855*       INTERVAL (WS-DELAY-INT)
001856*    END-EXEC
001857
001858     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
001859        CONTINUE
001860     ELSE
001861        IF (ON-LAST-DATE-RANGE)
001862                   OR
001863           ((PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL') AND
001864           (PCONTL > ZERO))
001865           PERFORM 1700-CHANGE-COMP
001866                                 THRU 1799-EXIT
001867        END-IF
001868     END-IF
001869
001870     IF NAMEL GREATER THAN ZERO
001871         PERFORM 2100-ADD-ACCOUNT-NAME THRU 2100-EXIT.
001872
001873     
      * EXEC CICS REWRITE
001874*        FROM      (ACCOUNT-MASTER)
001875*        DATASET   (ACCT-FILE-ID)
001876*    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007148' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037313438' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001877
001878     PERFORM 6300-STARTBR  THRU 6300-EXIT.
001879     PERFORM 6310-READNEXT THRU 6310-EXIT.
001880
001881     GO TO 3010-READ-NEXT.
001882
001883 3030-END-FILE.
001884     IF BROWSE-STARTED
001885        PERFORM 6320-ENDBR THRU 6320-EXIT.
001886
001887     PERFORM 6600-UPDATE-MAINT-DT THRU 6699-EXIT.
001888
001889     IF AUTO-CANCEL-ACCOUNT
001890        
      * EXEC CICS GETMAIN
001891*          LENGTH   (ERACNT-REC-LEN)
001892*          SET      (ADDRESS OF NOTE-FILE)
001893*          INITIMG  (GETMAIN-SPACE)
001894*          RESP     (WS-RESPONSE)
001895*       END-EXEC
      *    MOVE ',"IL                  $  N#00007165' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'204E233030303037313635' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERACNT-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF NOTE-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001896
001897        MOVE PI-ACCT-CCGSA-KEY   TO NT-CONTROL-PRIMARY
001898        MOVE '1'                 TO NT-RECORD-TYPE
001899        MOVE +1                  TO NT-LINE-SEQUENCE
001900        MOVE SAVE-BIN-DATE       TO NT-LAST-MAINT-DT
001901        MOVE PI-PROCESSOR-ID     TO NT-LAST-MAINT-BY
001902        MOVE EIBTIME             TO NT-LAST-MAINT-HHMMSS
001903        MOVE 'CANCELLED DUE TO LOW PRODUCTION'
001904                                 TO NT-NOTE-LINE
001905
001906        PERFORM WITH TEST AFTER UNTIL
001907           (NOT RESP-DUPKEY)
001908           AND (NOT RESP-DUPREC)
001909           
      * EXEC CICS WRITE
001910*             FROM      (NOTE-FILE)
001911*             RIDFLD    (NT-CONTROL-PRIMARY)
001912*             DATASET   (ERACNT-FILE-ID)
001913*             RESP      (WS-RESPONSE)
001914*          END-EXEC
           MOVE LENGTH OF
            NOTE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00007184' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'204E233030303037313834' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACNT-FILE-ID, 
                 NOTE-FILE, 
                 DFHEIV11, 
                 NT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
001915           IF RESP-DUPKEY OR RESP-DUPREC
001916              ADD +1             TO NT-LINE-SEQUENCE
001917           END-IF
001918        END-PERFORM
001919     END-IF
001920
001921     IF NOT EMI-NO-ERRORS
001922         GO TO 8200-SEND-DATAONLY.
001923
001924     MOVE -1                     TO NAMEL.
001925     MOVE ER-0000                TO EMI-ERROR.
001926     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001927     GO TO 8200-SEND-DATAONLY.
001928 EJECT
001929 3500-CHANGE-ALL-CSR.
001930     MOVE PI-ACCT-KEY            TO WS-KEY-SAVE.
001931     MOVE LOW-VALUES             TO WS-ACCT-EXP-DT
001932                                    WS-ACCT-REST-OF-EXP.
001933
001934     
      * EXEC CICS HANDLE CONDITION
001935*         ENDFILE     (3530-END-FILE)
001936*    END-EXEC.
      *    MOVE '"$''                   ! ( #00007209' TO DFHEIV0
           MOVE X'222427202020202020202020' &
                X'202020202020202020202120' &
                X'2820233030303037323039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001937
001938     PERFORM 6300-STARTBR THRU 6300-EXIT.
001939
001940 3510-READ-NEXT.
001941     PERFORM 6310-READNEXT THRU 6310-EXIT.
001942
001943     IF PI-ACCT-CCGSA-KEY NOT = WS-ACCT-CCGSA-KEY
001944        GO TO 3530-END-FILE.
001945
001946     
      * EXEC CICS ENDBR
001947*        DATASET   (ACCT-FILE-ID)
001948*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007221' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303037323231' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001949
001950     
      * EXEC CICS READ
001951*        UPDATE
001952*        DATASET   (ACCT-FILE-ID)
001953*        SET       (ADDRESS OF ACCOUNT-MASTER)
001954*        RIDFLD    (WS-KEY-SAVE)
001955*    END-EXEC.
      *    MOVE '&"S        EU         (   #00007225' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303037323235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-KEY-SAVE, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001956
001957     MOVE BIN-CURRENT-SAVE       TO AM-LAST-MAINT-DT.
001958     MOVE PI-PROCESSOR-ID        TO AM-LAST-MAINT-USER.
001959     MOVE EIBTIME                TO AM-LAST-MAINT-HHMMSS.
001960
001961     MOVE CSRI                   TO AM-CSR-CODE.
001962
001963     
      * EXEC CICS REWRITE
001964*        FROM      (ACCOUNT-MASTER)
001965*        DATASET   (ACCT-FILE-ID)
001966*    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007238' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303037323338' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
001967
001968     PERFORM 6300-STARTBR  THRU 6300-EXIT.
001969     PERFORM 6310-READNEXT THRU 6310-EXIT.
001970
001971     GO TO 3510-READ-NEXT.
001972
001973 3530-END-FILE.
001974     IF BROWSE-STARTED
001975        PERFORM 6320-ENDBR THRU 6320-EXIT.
001976
001977     MOVE ER-4009                TO EMI-ERROR.
001978     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
001979     MOVE LOW-VALUES             TO EL6501AI.
001980
001981     PERFORM 0700-FORMAT-KEYDATA THRU 0700-EXIT.
001982     PERFORM 6500-READ-ACCT      THRU 6599-EXIT.
001983     PERFORM 6000-MOVE-TO-SCREEN THRU 6099-EXIT.
001984
001985     PERFORM 6600-UPDATE-MAINT-DT THRU 6699-EXIT.
001986
001987     MOVE -1                     TO MAINTL.
001988     MOVE AL-UANON               TO MAINTA.
001989     GO TO 8100-SEND-INITIAL-MAP.
001990
001991 EJECT
001992 4000-EDITS.
001993     IF PI-COMPANY-ID = 'NCL'
001994      IF MAINTI = 'A' OR 'C'
001995        IF CSRL GREATER ZEROS
001996            PERFORM 8050-USER-REC-READ THRU 8060-EXIT
001997          ELSE
001998            IF MAINTI = 'A'
001999                MOVE ER-1883         TO EMI-ERROR
002000                MOVE -1              TO CSRL
002001                MOVE AL-UABON        TO CSRA
002002                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002003
002004     IF PI-COMPANY-ID  =  'AIG' OR 'AUK'
002005        IF NAMEL NOT GREATER ZEROS
002006           IF MAINTI = 'C'
002007              GO TO 4070-CHECK-STATUS
002008           ELSE
002009              MOVE -1               TO  NAMEL
002010              MOVE AL-UABON         TO  NAMEA
002011              MOVE ER-2045          TO  EMI-ERROR
002012              PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
002013        ELSE
002014            GO TO 4065-CHECK-ASSOCIATES.
002015
002016     IF TYPEL NOT GREATER ZEROS
002017        IF MAINTI = 'C'
002018           GO TO 4050-CONTINUE
002019        ELSE
002020           GO TO 4020-BUS-TYPE-ERROR.
002021
002022     IF TYPEI NOT NUMERIC
002023        GO TO 4020-BUS-TYPE-ERROR
002024     ELSE
002025        IF TYPEI LESS 1 OR GREATER 99
002026           GO TO 4020-BUS-TYPE-ERROR.
002027
002028     MOVE SPACES                 TO ELCNTL-KEY.
002029
002030     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
002031     MOVE TYPEI                  TO CNTL-BUS-TYPE.
002032     MOVE '8'                    TO CNTL-REC-TYPE.
002033     MOVE +0                     TO CNTL-SEQ-NO.
002034
002035     
      * EXEC CICS HANDLE CONDITION
002036*        NOTFND   (4020-BUS-TYPE-ERROR)
002037*    END-EXEC.
      *    MOVE '"$I                   ! ) #00007310' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2920233030303037333130' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002038
002039     
      * EXEC CICS READ
002040*        GTEQ
002041*        DATASET   (CNTL-FILE-ID)
002042*        SET       (ADDRESS OF CONTROL-FILE)
002043*        RIDFLD    (ELCNTL-KEY)
002044*    END-EXEC.
      *    MOVE '&"S        G          (   #00007314' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303037333134' TO DFHEIV0
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
           
002045
002046     IF TYPEI LESS 21
002047        MOVE TYPEI               TO WS-BUS-ENTRY
002048     ELSE
002049        DIVIDE TYPEI BY 20
002050            GIVING WS-BUS-TYPE REMAINDER WS-BUS-ENTRY
002051        IF WS-BUS-ENTRY = ZEROS
002052           MOVE 20               TO WS-BUS-ENTRY.
002053
002054     IF CF-BUSINESS-TITLE (WS-BUS-ENTRY) NOT = SPACES
002055        GO TO 4050-CONTINUE.
002056
002057 4020-BUS-TYPE-ERROR.
002058     MOVE -1                     TO TYPEL.
002059     MOVE AL-UNBON               TO TYPEA.
002060     MOVE ER-2178                TO EMI-ERROR.
002061     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002062
002063 4050-CONTINUE.
002064
002065     IF ASTATEL > 0
002066        MOVE SPACES                 TO  ELCNTL-KEY
002067        MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID
002068        MOVE '3'                    TO  CNTL-REC-TYPE
002069        MOVE ASTATEI                TO  CNTL-ACCESS (1:2)
002070        MOVE +0                     TO  CNTL-SEQ-NO
002071
002072        
      * EXEC CICS  READ
002073*           DATASET  ('ELCNTL')
002074*           SET      (ADDRESS OF CONTROL-FILE)
002075*           RIDFLD   (ELCNTL-KEY)
002076*           RESP     (WS-RESPONSE)
002077*       END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00007347' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037333437' TO DFHEIV0
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002078
002079        IF NOT RESP-NORMAL
002080           MOVE ER-0144                TO  EMI-ERROR
002081           MOVE -1                     TO  ASTATEL
002082           MOVE AL-UABON               TO  ASTATEA
002083           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002084        END-IF
002085     ELSE
002086        IF MAINTI = 'A'
002087           MOVE ER-0144                TO  EMI-ERROR
002088           MOVE -1                     TO  ASTATEL
002089           MOVE AL-UABON               TO  ASTATEA
002090           PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
002091        END-IF
002092     END-IF
002093
002094     IF INDGRPL NOT GREATER ZEROS
002095        IF MAINTI = 'C'
002096           GO TO 4070-CHECK-STATUS
002097        ELSE
002098           GO TO 4060-IG-ERROR.
002099
002100     IF INDGRPI = 'I' OR 'G'
002101        GO TO 4070-CHECK-STATUS.
002102
002103 4060-IG-ERROR.
002104     MOVE -1                     TO INDGRPL.
002105     MOVE AL-UABON               TO INDGRPA.
002106     MOVE ER-2152                TO EMI-ERROR.
002107     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002108
002109 4065-CHECK-ASSOCIATES.
002110     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
002111        MOVE  PI-ACCT-ACCOUNT    TO  WS-ACCOUNT-NUM-AREA
002112        MOVE  NAMEI              TO  WS-NAME-AREA
002113     ELSE
002114        GO TO 4070-CHECK-STATUS.
002115
002116     IF WS-NAME-1-4  =  WS-ACCT-BRANCH-CD
002117        NEXT SENTENCE
002118     ELSE
002119        MOVE -1                  TO  NAMEL
002120        MOVE AL-UABON            TO  NAMEA
002121        MOVE ER-0788             TO  EMI-ERROR
002122        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
002123
002124 4070-CHECK-STATUS.
002125     IF STATUSL NOT GREATER ZEROS
002126        IF MAINTI = 'C'
002127           GO TO 4090-CHECK-STD-AH-TYPE
002128        ELSE
002129           MOVE 'A'              TO STATUSI
002130           MOVE AL-UANON         TO STATUSA
002131           GO TO 4090-CHECK-STD-AH-TYPE.
002132
002133*    IF STATUSI = 'A' OR 'I' OR 'C' OR 'F' OR 'S'
002134     IF STATUSI = 'A' OR 'I' OR 'C' OR 'F' OR 'S' OR 'D' OR 'L'
002135                      OR 'R' OR 'P'
002136        GO TO 4090-CHECK-STD-AH-TYPE.
002137
002138 4080-STATUS-ERROR.
002139     MOVE -1                     TO STATUSL.
002140     MOVE AL-UABON               TO STATUSA.
002141     MOVE ER-2153                TO EMI-ERROR.
002142     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002143
002144 4090-CHECK-STD-AH-TYPE.
002145
002146     IF PI-COMPANY-ID  = 'AIG' OR 'AUK'
002147         GO TO 4900-EXIT.
002148
002149     IF STDBENL NOT GREATER ZEROS
002150        IF MAINTI = 'C'
002151*          GO TO 4110-CHECK-REMIT
002152           GO TO 4100-CHECK-CLPTOLP
002153        ELSE
002154           MOVE ZEROS            TO STDBENI
002155           MOVE AL-UANON         TO STDBENA
002156*          GO TO 4110-CHECK-REMIT.
002157           GO TO 4100-CHECK-CLPTOLP.
002158
002159     IF STDBENI = SPACES
002160         MOVE ZEROS               TO STDBENI.
002161     IF STDBENI = ZEROS
002162*        GO TO 4110-CHECK-REMIT.
002163         GO TO 4100-CHECK-CLPTOLP.
002164
002165     MOVE STDBENI                TO WS-EDIT-BEN-CODE.
002166     IF INVALID-BENEFIT-CODE
002167        GO TO 4099-STD-BEN-ERROR.
002168
002169     MOVE SPACES                 TO ELCNTL-KEY.
002170
002171     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
002172     MOVE STDBENI                TO CNTL-BEN-TYPE.
002173     MOVE '5'                    TO CNTL-REC-TYPE.
002174     MOVE +0                     TO CNTL-SEQ-NO.
002175
002176     
      * EXEC CICS HANDLE CONDITION
002177*        NOTFND   (4099-STD-BEN-ERROR)
002178*    END-EXEC.
      *    MOVE '"$I                   ! * #00007451' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2A20233030303037343531' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002179
002180     
      * EXEC CICS READ
002181*        GTEQ
002182*        DATASET   (CNTL-FILE-ID)
002183*        SET       (ADDRESS OF CONTROL-FILE)
002184*        RIDFLD    (ELCNTL-KEY)
002185*    END-EXEC.
      *    MOVE '&"S        G          (   #00007455' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303037343535' TO DFHEIV0
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
           
002186
002187     IF CNTL-COMP-ID  = CF-COMPANY-ID  AND
002188        CNTL-REC-TYPE = CF-RECORD-TYPE
002189         MOVE 1                  TO SUB
002190     ELSE
002191         GO TO 4099-STD-BEN-ERROR.
002192
002193 4095-CHECK-BEN-TYPE.
002194     IF STDBENI = CF-BENEFIT-CODE (SUB)
002195*       GO TO 4110-CHECK-REMIT.
002196        GO TO 4100-CHECK-CLPTOLP.
002197
002198     IF SUB LESS 8
002199         ADD 1 TO SUB
002200         GO TO 4095-CHECK-BEN-TYPE.
002201
002202 4099-STD-BEN-ERROR.
002203     MOVE -1                     TO STDBENL.
002204     MOVE AL-UABON               TO STDBENA.
002205     MOVE ER-0250                TO EMI-ERROR.
002206     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002207
002208 4100-CHECK-CLPTOLP.
002209
002210     IF CLPTOLPL > ZERO
002211         
      * EXEC CICS BIF
002212*            DEEDIT
002213*            FIELD    (CLPTOLPI)
002214*            LENGTH   (6)
002215*        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007486' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037343836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLPTOLPI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002216         IF CLPTOLPI NOT NUMERIC
002217             MOVE ER-1778        TO EMI-ERROR
002218             MOVE -1             TO CLPTOLPL
002219             MOVE AL-UNBON       TO CLPTOLPA
002220             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002221         END-IF
002222     END-IF.
002223
002224     IF LCOMML > ZERO
002225         
      * EXEC CICS BIF
002226*            DEEDIT
002227*            FIELD    (LCOMMI)
002228*            LENGTH   (8)
002229*        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007500' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037353030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCOMMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002230         IF LCOMMI NOT NUMERIC
002231             MOVE ER-1778        TO EMI-ERROR
002232             MOVE -1             TO LCOMML
002233             MOVE AL-UNBON       TO LCOMMA
002234             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002235         END-IF
002236     END-IF
002237
002238     IF MAXMFEEL > ZEROS
002239        
      * EXEC CICS BIF DEEDIT
002240*          FIELD   (MAXMFEEI)
002241*          LENGTH  (5)
002242*       END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007514' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303037353134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAXMFEEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002243        IF MAXMFEEI NUMERIC
002244           MOVE MAXMFEEI         TO WS-WORK-FIELD
002245           MOVE WS-WORK-NUM      TO MAXMFEEO
002246        ELSE
002247           MOVE -1               TO MAXMFEEL
002248           MOVE AL-UABON         TO MAXMFEEA
002249           MOVE ER-7717          TO EMI-ERROR
002250           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002251        END-IF
002252     END-IF
002253
002254     IF (CLPSTL > 0)
002255         MOVE AL-UANON           TO CLPSTA
002256        IF (CLPSTI NOT = SPACES)
002257           MOVE SPACES           TO ELCNTL-KEY
002258           MOVE PI-COMPANY-ID    TO CNTL-COMP-ID
002259           MOVE '3'              TO CNTL-REC-TYPE
002260           MOVE CLPSTI           TO CNTL-ACCESS (1:2)
002261           MOVE +0               TO CNTL-SEQ-NO
002262
002263           
      * EXEC CICS  READ
002264*              DATASET  ('ELCNTL')
002265*              SET      (ADDRESS OF CONTROL-FILE)
002266*              RIDFLD   (ELCNTL-KEY)
002267*              RESP     (WS-RESPONSE)
002268*          END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00007538' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'204E233030303037353338' TO DFHEIV0
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002269
002270           IF NOT RESP-NORMAL
002271              MOVE ER-0144       TO EMI-ERROR
002272              MOVE -1            TO CLPSTL
002273              MOVE AL-UABON      TO CLPSTA
002274              PERFORM 9900-ERROR-FORMAT
002275                                 THRU  9900-EXIT
002276           END-IF
002277        ELSE
002278           MOVE ER-0144          TO EMI-ERROR
002279           MOVE -1               TO CLPSTL
002280           MOVE AL-UABON         TO CLPSTA
002281           PERFORM 9900-ERROR-FORMAT
002282                                 THRU  9900-EXIT
002283        END-IF
002284     END-IF
002285
002286     IF (PRODCDL > ZEROS)
002287        and (emi-error not = er-0144)
002288        if prodcdi = spaces
002289           continue
002290        else
002291           move low-values       to erpdef-key
002292           move pi-company-cd    to erpdef-company-cd
002293           move clpsti           to erpdef-state
002294           move prodcdi          to erpdef-prod-cd
002295           move erpdef-key       to erpdef-key-save
002296           
      * exec cics read
002297*             dataset ('ERPDEF')
002298*             set     (address of product-master)
002299*             ridfld  (erpdef-key)
002300*             length  (6)
002301*             gteq
002302*             resp    (ws-response)
002303*          end-exec
           MOVE 'ERPDEF' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00007571' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'204E233030303037353731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 erpdef-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF product-master TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
002304           if (resp-normal)
002305              and (pd-control-primary (1:6) =
002306                           erpdef-key-save (1:6))
002307              continue
002308           else
002309              move er-8156       to emi-error
002310              MOVE -1            TO PRODCDL
002311              MOVE AL-UABON      TO PRODCDA
002312              PERFORM 9900-ERROR-FORMAT
002313                                 THRU  9900-EXIT
002314           end-if
002315        end-if
002316     end-if
002317
002318     .
002319 4110-CHECK-REMIT.
002320     IF REMITL NOT GREATER ZEROS
002321        MOVE 1                   TO REMITI
002322        MOVE AL-UNNON            TO REMITA
002323        GO TO 4120-REMIT-ERROR.
002324
002325     IF (REMITI NOT NUMERIC)  OR
002326        (REMITI LESS 1 OR GREATER 10)
002327           GO TO 4120-REMIT-ERROR.
002328
002329 4115-CHECK-AGENT.
002330     SET M-INDEX                 TO REMITI.
002331
002332     IF AGENTL (M-INDEX)  NOT GREATER ZEROS
002333        MOVE ER-2156             TO EMI-ERROR
002334        GO TO 4120-R-ERROR.
002335
002336     GO TO 4130-CHECK-CONTRACT.
002337
002338 4120-REMIT-ERROR.
002339     MOVE ER-2155                TO EMI-ERROR.
002340
002341 4120-R-ERROR.
002342     MOVE -1                     TO REMITL.
002343     MOVE AL-UABON               TO REMITA.
002344     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002345
002346 4130-CHECK-CONTRACT.
002347     IF CONTRL NOT = ZEROS AND CONTRI NOT = SPACES
002348        MOVE CONTRI                TO DEEDIT-FIELD
002349        PERFORM 8600-DEEDIT
002350        MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY
002351        MOVE '4'                    TO DC-OPTION-CODE
002352        PERFORM 9700-DATE-LINK
002353        IF DATE-CONVERSION-ERROR
002354           MOVE ER-2157             TO EMI-ERROR
002355           MOVE -1                  TO CONTRL
002356           MOVE AL-UABON            TO CONTRA
002357           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002358        ELSE
002359***  Y2K PROJ 7744
002360           MOVE DC-GREG-DATE-1-EDIT TO CONTRI
002361           MOVE AL-UANON            TO CONTRA
002362           MOVE DC-GREG-DATE-1-YMD  TO WS-ANVR-DT
002363           MOVE DC-ALPHA-CENTURY    TO WS-ANVR-DT(4:2)
002364        END-IF
002365     END-IF.
002366***  Y2K PROJ 7744
002367
002368     IF PI-COMPANY-ID = 'DMD'
002369       IF PI-MAINT = 'A'
002370        IF CONTRL = ZERO
002371           MOVE ER-0852             TO EMI-ERROR
002372           MOVE -1                  TO CONTRL
002373           MOVE AL-UABON            TO CONTRA
002374           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002375
002376*4131-CHECK-PSI-FACTOR.
002377*    IF PSILFFL GREATER +0
002378*       MOVE PSILFFI             TO DEEDIT-FIELD
002379*       PERFORM 8600-DEEDIT
002380*       IF DEEDIT-FIELD NUMERIC
002381*          MOVE DEEDIT-FIELD-V5  TO WS-LIFE-PSI
002382*                                   PSILFFO
002383*       ELSE
002384*          MOVE ER-9999          TO EMI-ERROR
002385*          MOVE -1               TO PSILFFL
002386*          MOVE AL-UABON         TO PSILFFA
002387*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002388*
002389*    IF PSIAHFL GREATER +0
002390*       MOVE PSIAHFI             TO DEEDIT-FIELD
002391*       PERFORM 8600-DEEDIT
002392*       IF DEEDIT-FIELD NUMERIC
002393*          MOVE DEEDIT-FIELD-V5  TO WS-AH-PSI
002394*                                   PSIAHFO
002395*       ELSE
002396*          MOVE ER-9999          TO EMI-ERROR
002397*          MOVE -1               TO PSIAHFL
002398*          MOVE AL-UABON         TO PSIAHFA
002399*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002400
002401 4135-CHECK-PREVIOUS-DATES.
002402     IF PEFFDTEL NOT = ZEROS AND PEFFDTEI NOT = SPACES
002403        MOVE PEFFDTEI              TO DEEDIT-FIELD
002404        PERFORM 8600-DEEDIT
002405        MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY
002406        MOVE '4'                    TO DC-OPTION-CODE
002407        PERFORM 9700-DATE-LINK
002408        IF DATE-CONVERSION-ERROR
002409           MOVE ER-0348             TO EMI-ERROR
002410           MOVE -1                  TO PEFFDTEL
002411           MOVE AL-UABON            TO PEFFDTEA
002412           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002413        ELSE
002414***  Y2K PROJ 7744
002415           MOVE DC-GREG-DATE-1-EDIT TO PEFFDTEI
002416           MOVE AL-UANON            TO PEFFDTEA
002417           MOVE DC-GREG-DATE-1-YMD  TO WS-PEFF-DT
002418           MOVE DC-ALPHA-CENTURY    TO WS-PEFF-DT(4:2)
002419        END-IF
002420     END-IF.
002421
002422     IF PEXPDTEL NOT = ZEROS AND PEXPDTEI NOT = SPACES
002423        MOVE PEXPDTEI              TO DEEDIT-FIELD
002424        PERFORM 8600-DEEDIT
002425        IF DEEDIT-FIELD-V0 NOT LESS 999999
002426           MOVE '99/99/99'             TO PEXPDTEO
002427           MOVE 99999999999            TO WS-PEXP-DT
002428           MOVE AL-UANON               TO PEXPDTEA
002429        ELSE
002430           MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY
002431           MOVE '4'                    TO DC-OPTION-CODE
002432           PERFORM 9700-DATE-LINK
002433           IF DATE-CONVERSION-ERROR
002434              MOVE ER-0454             TO EMI-ERROR
002435              MOVE -1                  TO PEXPDTEL
002436              MOVE AL-UABON            TO PEXPDTEA
002437              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002438           ELSE
002439              MOVE DC-GREG-DATE-1-EDIT TO PEXPDTEI
002440              MOVE AL-UANON            TO PEXPDTEA
002441              MOVE DC-GREG-DATE-1-YMD  TO WS-PEXP-DT
002442              MOVE DC-ALPHA-CENTURY    TO WS-PEXP-DT(4:2)
002443           END-IF
002444        END-IF
002445     END-IF.
002446***  Y2K PROJ 7744
002447
002448     IF COMMCALL NOT GREATER ZEROS
002449         GO TO 4150-CHECK-REINCAL.
002450
002451     IF (PI-COMPANY-ID = 'ADL' OR 'DEF') AND
002452         (EFFCHG-SAVE LESS 841101)       AND
002453         (COMMCALI = 'Y' OR '1')         AND
002454         (NOT MODIFY-CAP)
002455           MOVE -1               TO COMMCALL
002456           MOVE AL-UABON         TO COMMCALA
002457           MOVE ER-2189          TO EMI-ERROR
002458           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002459
002460     IF COMMCALI = 'Y' OR 'N' OR '1' OR ' '
002461         GO TO 4150-CHECK-REINCAL.
002462
002463 4140-COMMCAL-ERROR.
002464     MOVE -1                     TO COMMCALL.
002465     MOVE AL-UABON               TO COMMCALA.
002466     MOVE ER-2158                TO EMI-ERROR.
002467     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002468
002469 4150-CHECK-REINCAL.
002470     IF REINCALL NOT GREATER ZEROS
002471         GO TO 4170-CHECK-REINTAB.
002472
002473     IF REINCALI = 'Y' OR 'N'
002474        GO TO 4170-CHECK-REINTAB.
002475
002476 4160-REINCAL-ERROR.
002477     MOVE -1                     TO REINCALL.
002478     MOVE AL-UABON               TO REINCALA.
002479     MOVE ER-2179                TO EMI-ERROR.
002480     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002481
002482 4170-CHECK-REINTAB.
002483     IF REINTABL NOT GREATER ZEROS
002484        IF PI-COMPANY-ID = 'NCL' AND
002485           MAINTI = 'A'
002486                MOVE ER-4010         TO EMI-ERROR
002487                MOVE -1              TO REINTABL
002488                MOVE AL-UABON        TO REINTABA
002489                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002490
002491     IF REINTABL NOT GREATER ZEROS OR REINTABI = SPACES
002492         GO TO 4410-CHECK-AGENTS.
002493
002494     IF PI-COMPANY-ID = 'CID'
002495        IF PI-ACCT-STATE = 'KY'
002496           IF REINTABI NOT = 'K98' AND 'K99' AND 'KYA'
002497              MOVE -1            TO REINTABL
002498              MOVE AL-UABON      TO REINTABA
002499              MOVE ER-2159       TO EMI-ERROR
002500              PERFORM 9900-ERROR-FORMAT
002501                                 THRU 9900-EXIT
002502           END-IF
002503        END-IF
002504     END-IF
002505
002506     MOVE LOW-VALUES             TO REIN-KEY.
002507
002508     MOVE PI-COMPANY-CD          TO REIN-COMP-CD.
002509     MOVE 'A'                    TO REIN-CODE.
002510     MOVE REINTABI               TO REIN-TABLE.
002511
002512     
      * EXEC CICS HANDLE CONDITION
002513*        NOTFND   (4180-REIN-ERROR)
002514*    END-EXEC.
      *    MOVE '"$I                   ! + #00007787' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2B20233030303037373837' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002515
002516     
      * EXEC CICS READ
002517*        DATASET   (REIN-FILE-ID)
002518*        SET       (ADDRESS OF REINSURANCE-RECORD)
002519*        RIDFLD    (REIN-KEY)
002520*    END-EXEC.
      *    MOVE '&"S        E          (   #00007791' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303037373931' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 REIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
002521
002522     GO TO 4410-CHECK-AGENTS.
002523
002524 4180-REIN-ERROR.
002525     MOVE -1                     TO REINTABL.
002526     MOVE AL-UABON               TO REINTABA.
002527     MOVE ER-2615                TO EMI-ERROR.
002528     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002529
002530 EJECT
002531 4410-CHECK-AGENTS.
002532     MOVE PI-COMPANY-CD          TO COMM-COMP-CD.
002533     SET M-INDEX TO 1.
002534
002535     IF PI-COMPANY-ID = 'NCL'
002536       IF AGENTL (M-INDEX) GREATER ZEROS
002537         MOVE PI-ACCT-ACCOUNT    TO AGENT  (M-INDEX).
002538
002539 4420-LOOP.
002540
002541     IF AGENTL (M-INDEX) NOT GREATER ZEROS
002542        IF (ATYPEL (M-INDEX)  GREATER ZEROS AND
002543            ATYPE  (M-INDEX) NOT = SPACES)  OR
002544           SINGLEL (M-INDEX) GREATER ZEROS  OR
002545           JOINTL  (M-INDEX) GREATER ZEROS  OR
002546           A-HL    (M-INDEX) GREATER ZEROS  OR
002547           RECALL  (M-INDEX) GREATER ZEROS  OR
002548           RCOMML  (M-INDEX) GREATER ZEROS  OR
002549           CHGBCKL (M-INDEX) GREATER ZEROS  OR
002550           CCEL    (M-INDEX) GREATER ZEROS
002551           MOVE ER-2172          TO EMI-ERROR
002552           MOVE -1               TO AGENTL (M-INDEX)
002553           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002554           GO TO 4450-BUMP-INDEX
002555        else
002556           go to 4430-check-single
002557*       ELSE
002558*          GO TO 4450-BUMP-INDEX.
002559        end-if
002560     end-if
002561
002562     IF AGENT (M-INDEX) = ZEROS
002563         MOVE ER-2970            TO  EMI-ERROR
002564         MOVE -1                 TO  AGENTL (M-INDEX)
002565         MOVE AL-UABON           TO  AGENTA (M-INDEX)
002566         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002567         GO TO 4450-BUMP-INDEX.
002568
002569     IF ATYPEL (M-INDEX) = ZEROS
002570        MOVE -1                  TO ATYPEL (M-INDEX)
002571        MOVE ER-2173             TO EMI-ERROR
002572        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002573        GO TO 4430-CHECK-SINGLE.
002574
002575     IF ATYPE (M-INDEX) = 'C'
002576        ADD 1                    TO WS-ACCOUNT-CTR-C
002577       ELSE
002578        IF ATYPE (M-INDEX) = 'R'
002579           ADD 1                 TO WS-ACCOUNT-CTR-R
002580          ELSE
002581           IF ATYPE (M-INDEX) = 'D'
002582              ADD 1                 TO WS-ACCOUNT-CTR-D
002583             ELSE
002584              IF ATYPE (M-INDEX) = 'O' OR 'P' OR 'T' OR 'W'
002585                                OR 'B' OR 'I' OR 'K' OR 'L'
002586                                OR 'J' OR 'M' OR 'A' OR 'N'
002587                                OR 'S'
002588                 NEXT SENTENCE
002589                ELSE
002590                 IF ATYPE (M-INDEX) = SPACES
002591                    NEXT SENTENCE
002592                   ELSE
002593                    MOVE ER-2175       TO EMI-ERROR
002594                    MOVE -1            TO ATYPEL (M-INDEX)
002595                    MOVE AL-UABON      TO ATYPEA (M-INDEX)
002596                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002597
002598 4430-CHECK-SINGLE.
002599
002600     IF SINGLEL (M-INDEX) GREATER ZEROS
002601        IF SINGLE-COMM-R (M-INDEX) NOT NUMERIC
002602           MOVE SINGLE-COMM-T (M-INDEX)    TO COMM-TABLE
002603           MOVE PI-LIFE-OVERRIDE-L1        TO COMM-LF-AH
002604           MOVE LOW-VALUES                 TO COMM-FILLER
002605           PERFORM 6400-READ-COMMISSION THRU 6459-EXIT
002606           IF COMMISSION-ERROR
002607              MOVE AL-UABON      TO SINGLEA (M-INDEX)
002608              MOVE -1            TO SINGLEL (M-INDEX)
002609           ELSE
002610              if cceind(m-index) <> 'Y'
002611                 add ws-table-max to COMM-SL-ACCUM
002612                 if atype (m-index) = 'C' or 'D'
002613                    add ws-table-max to comm-sl-accum-al
002614                 else
002615                    if atype (m-index) = 'O' OR 'P'
002616                       add ws-table-max to comm-sl-accum-gl
002617                    end-if
002618                 end-if
002619              end-if
002620           END-IF
002621        END-IF
002622     END-IF
002623
002624     IF JOINTL (M-INDEX) GREATER ZEROS
002625        IF JOINT-COMM-R (M-INDEX) NOT NUMERIC
002626           MOVE JOINT-COMM-T (M-INDEX) TO COMM-TABLE
002627           MOVE PI-LIFE-OVERRIDE-L1    TO COMM-LF-AH
002628           MOVE LOW-VALUES             TO COMM-FILLER
002629           PERFORM 6400-READ-COMMISSION THRU 6459-EXIT
002630           IF COMMISSION-ERROR
002631              MOVE AL-UABON      TO JOINTA (M-INDEX)
002632              MOVE -1            TO JOINTL (M-INDEX)
002633           ELSE
002634              if cceind(m-index) <> 'Y'
002635                 add ws-table-max to COMM-JL-ACCUM
002636                 if atype (m-index) = 'C' or 'D'
002637                    add ws-table-max to comm-jl-accum-al
002638                 else
002639                    if atype (m-index) = 'O' OR 'P'
002640                       add ws-table-max to comm-jl-accum-gl
002641                 end-if
002642              end-if
002643           END-IF
002644        END-IF
002645     END-IF
002646
002647     IF A-HL (M-INDEX) GREATER ZEROS
002648        IF A-H-COMM-R (M-INDEX)  NOT NUMERIC
002649           MOVE A-H-COMM-T (M-INDEX)      TO COMM-TABLE
002650           MOVE PI-AH-OVERRIDE-L1         TO COMM-LF-AH
002651           MOVE LOW-VALUES                 TO COMM-FILLER
002652           PERFORM 6400-READ-COMMISSION THRU 6459-EXIT
002653           IF COMMISSION-ERROR
002654              MOVE AL-UABON      TO A-HA (M-INDEX)
002655              MOVE -1            TO A-HL (M-INDEX)
002656           ELSE
002657              if cceind(m-index) <> 'Y'
002658                 add ws-table-max to COMM-ah-ACCUM
002659                 if atype (m-index) = 'C' or 'D'
002660                    add ws-table-max to comm-ah-accum-al
002661                 else
002662                    if atype (m-index) = 'O' OR 'P'
002663                       add ws-table-max to comm-ah-accum-gl
002664                 end-if
002665              end-if
002666           END-IF
002667        END-IF
002668     END-IF
002669
002670     IF RECALL (M-INDEX) GREATER ZEROS
002671         IF RECAL (M-INDEX) = 'Y' OR 'N' OR '1' OR SPACE
002672             MOVE AL-UANON       TO RECALA (M-INDEX)
002673         ELSE
002674             MOVE -1             TO RECALL (M-INDEX)
002675             MOVE AL-UABON       TO RECALA (M-INDEX)
002676             MOVE ER-2158        TO EMI-ERROR
002677             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002678
002679     IF RCOMML (M-INDEX) GREATER ZEROS
002680         IF RCOMM (M-INDEX) = 'Y' OR 'N' OR 'A' OR SPACE
002681             MOVE AL-UANON       TO RCOMMA (M-INDEX)
002682         ELSE
002683             MOVE -1             TO RCOMML (M-INDEX)
002684             MOVE AL-UABON       TO RCOMMA (M-INDEX)
002685             MOVE ER-0625        TO EMI-ERROR
002686             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002687
002688     IF  CHGBCKL (M-INDEX)  GREATER THAN  ZEROS
002689         IF  CHGBCK (M-INDEX)    NUMERIC
002690             MOVE  AL-UNNON        TO  CHGBCKA (M-INDEX)
002691         ELSE
002692             MOVE  -1              TO  CHGBCKL (M-INDEX)
002693             MOVE  AL-UNBON        TO  CHGBCKA (M-INDEX)
002694             MOVE  ER-0759         TO  EMI-ERROR
002695             PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT.
002696
002697     IF CCEL (M-INDEX) <> ZEROS
002698        IF CCEIND(M-INDEX) = 'Y'
002699           MOVE AL-UANON         TO CCEA(M-INDEX)
002700           IF ATYPE(M-INDEX) = 'C' OR 'D'
002701              IF WS-COMM-CAP-LIMIT-TO <> 'A' AND 'B'
002702                 MOVE  -1        TO CCEL (M-INDEX)
002703                 MOVE  AL-UABON  TO CCEA (M-INDEX)
002704                 MOVE  ER-3065   TO EMI-ERROR
002705                 PERFORM  9900-ERROR-FORMAT
002706                                 THRU 9900-EXIT
002707              END-IF
002708           ELSE
002709              IF ATYPE(M-INDEX) = 'O' OR 'P'
002710                 IF WS-COMM-CAP-LIMIT-TO <> 'G' AND 'B'
002711                    MOVE  -1        TO CCEL (M-INDEX)
002712                    MOVE  AL-UABON  TO CCEA (M-INDEX)
002713                    MOVE  ER-3065   TO EMI-ERROR
002714                    PERFORM  9900-ERROR-FORMAT
002715                                    THRU 9900-EXIT
002716                 END-IF
002717              END-IF
002718           END-IF
002719        ELSE
002720           IF CCEIND (M-INDEX) = 'N' OR ' '
002721              MOVE  AL-UANON        TO CCEA (M-INDEX)
002722           ELSE
002723              MOVE  -1              TO CCEL (M-INDEX)
002724              MOVE  AL-UABON        TO CCEA (M-INDEX)
002725              MOVE  ER-3066         TO EMI-ERROR
002726              PERFORM  9900-ERROR-FORMAT
002727                                 THRU 9900-EXIT
002728           END-IF
002729        END-IF
002730     END-IF
002731
002732     IF ATYPE (M-INDEX) NOT = 'C' AND 'D' AND 'O' AND 'P'
002733        AND 'J'
002734         GO TO 4450-BUMP-INDEX.
002735
002736     IF SINGLE-COMM-R (M-INDEX) NUMERIC
002737        if cceind(m-index) <> 'Y'
002738           ADD SINGLE-COMM-R (M-INDEX) TO COMM-SL-ACCUM
002739           if atype (m-index) = 'C' or 'D'
002740              add SINGLE-COMM-R (M-INDEX) to comm-sl-accum-al
002741           else
002742              if atype (m-index) = 'O' or 'P'
002743                 add single-comm-r (m-index) to comm-sl-accum-gl
002744              end-if
002745           end-if
002746        end-if
002747     else
002748        display ' single comm r not numeric '
002749     end-if
002750
002751     IF JOINT-COMM-R (M-INDEX) NUMERIC
002752        if cceind(m-index) <> 'Y'
002753           ADD JOINT-COMM-R (M-INDEX)  TO COMM-JL-ACCUM
002754           if atype (m-index) = 'C' or 'D'
002755              add JOINT-COMM-R (M-INDEX) to comm-jl-accum-al
002756           else
002757              if atype (m-index) = 'O' or 'P'
002758                 add joint-comm-r (m-index) to comm-jl-accum-gl
002759              end-if
002760           end-if
002761        end-if
002762     end-if
002763
002764     IF A-H-COMM-R (M-INDEX) NUMERIC
002765        if cceind(m-index) <> 'Y'
002766           ADD A-H-COMM-R (M-INDEX)    TO COMM-AH-ACCUM
002767           if atype (m-index) = 'C' or 'D'
002768              add A-H-COMM-R (M-INDEX) to comm-ah-accum-al
002769           else
002770              if atype (m-index) = 'O' or 'P'
002771                 add a-h-comm-r (m-index) to comm-ah-accum-gl
002772              end-if
002773           end-if
002774        end-if
002775     END-IF
002776
002777     .
002778 4450-BUMP-INDEX.
002779     SET M-INDEX UP BY 1.
002780
002781     IF M-INDEX NOT = 11
002782        GO TO 4420-LOOP.
002783
002784     IF WS-ACCOUNT-CTR-C = ZEROS AND
002785        WS-ACCOUNT-CTR-D = ZEROS AND
002786        WS-ACCOUNT-CTR-R = ZEROS
002787          MOVE ER-2177             TO EMI-ERROR
002788          MOVE -1                  TO AGENTL (1)
002789          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002790          GO TO 4900-EXIT.
002791
002792     IF (WS-ACCOUNT-CTR-C  GREATER 0 AND
002793         WS-ACCOUNT-CTR-D  GREATER 0) OR
002794        (WS-ACCOUNT-CTR-R  GREATER 0 AND
002795         WS-ACCOUNT-CTR-D  GREATER 0)
002796           MOVE ER-2174             TO EMI-ERROR
002797           MOVE -1                  TO AGENTL (1)
002798           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002799           GO TO 4900-EXIT.
002800
002801     IF WS-ACCOUNT-CTR-C GREATER 1 OR
002802        WS-ACCOUNT-CTR-R GREATER 1 OR
002803        WS-ACCOUNT-CTR-D GREATER 1
002804          MOVE ER-2182             TO EMI-ERROR
002805          MOVE -1                  TO AGENTL (1)
002806          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
002807          GO TO 4900-EXIT.
002808
002809
002810     IF WS-ST-COMM-CAP-SL NOT NUMERIC
002811        MOVE ZEROS               TO WS-ST-COMM-CAP-SL
002812     end-if
002813     IF WS-ST-COMM-CAP-JL NOT NUMERIC
002814        MOVE ZEROS               TO WS-ST-COMM-CAP-JL
002815     END-IF
002816     IF WS-ST-COMM-CAP-SA NOT NUMERIC
002817        MOVE ZEROS               TO WS-ST-COMM-CAP-SA
002818     END-IF
002819
002820     if ws-st-comm-cap-sl = zeros
002821        continue
002822     else
002823        if comm-sl-accum-al numeric
002824           if (comm-sl-accum-al > ws-st-comm-cap-sl)
002825              MOVE -1         TO MAINTL
002826              MOVE ER-1955    TO EMI-ERROR
002827              PERFORM 9900-ERROR-FORMAT
002828                              THRU 9900-EXIT
002829           end-if
002830        end-if
002831     end-if
002832
002833     if ws-st-comm-cap-jl = zeros
002834        continue
002835     else
002836        if comm-jl-accum-al numeric
002837           if (comm-jl-accum-al > ws-st-comm-cap-jl)
002838              MOVE -1         TO MAINTL
002839              MOVE ER-1956    TO EMI-ERROR
002840              PERFORM 9900-ERROR-FORMAT
002841                              THRU 9900-EXIT
002842           end-if
002843        end-if
002844     end-if
002845
002846     if ws-st-comm-cap-sa = zeros
002847        continue
002848     else
002849        if comm-ah-accum-al numeric
002850           if (comm-ah-accum-al > ws-st-comm-cap-sa)
002851              MOVE -1         TO MAINTL
002852              MOVE ER-1957    TO EMI-ERROR
002853              PERFORM 9900-ERROR-FORMAT
002854                              THRU 9900-EXIT
002855           end-if
002856        end-if
002857     end-if
002858
002859     if ws-st-ga-comm-cap-sl = zeros
002860        continue
002861     else
002862        if comm-sl-accum-gl numeric
002863           if (comm-sl-accum-gl > ws-st-ga-comm-cap-sl)
002864              MOVE -1         TO MAINTL
002865              MOVE ER-1958    TO EMI-ERROR
002866              PERFORM 9900-ERROR-FORMAT
002867                              THRU 9900-EXIT
002868           end-if
002869        end-if
002870     end-if
002871
002872     if ws-st-ga-comm-cap-jl = zeros
002873        continue
002874     else
002875        if comm-jl-accum-gl numeric
002876           if (comm-jl-accum-gl > ws-st-ga-comm-cap-jl)
002877              MOVE -1         TO MAINTL
002878              MOVE ER-1959    TO EMI-ERROR
002879              PERFORM 9900-ERROR-FORMAT
002880                              THRU 9900-EXIT
002881           end-if
002882        end-if
002883     end-if
002884
002885     if ws-st-ga-comm-cap-sa = zeros
002886        continue
002887     else
002888        if comm-ah-accum-gl numeric
002889           if (comm-ah-accum-gl > ws-st-ga-comm-cap-sa)
002890              MOVE -1         TO MAINTL
002891              MOVE ER-1960    TO EMI-ERROR
002892              PERFORM 9900-ERROR-FORMAT
002893                              THRU 9900-EXIT
002894           end-if
002895        end-if
002896     end-if
002897
002898
002899     if ws-st-tot-comm-cap-sl = zeros
002900        continue
002901     else
002902        if comm-sl-accum numeric
002903           if (comm-sl-accum > ws-st-tot-comm-cap-sl)
002904              MOVE -1         TO MAINTL
002905              MOVE ER-1961    TO EMI-ERROR
002906              PERFORM 9900-ERROR-FORMAT
002907                              THRU 9900-EXIT
002908           end-if
002909        end-if
002910     end-if
002911
002912     if ws-st-tot-comm-cap-jl = zeros
002913        continue
002914     else
002915        if comm-jl-accum numeric
002916           if (comm-jl-accum > ws-st-tot-comm-cap-jl)
002917              MOVE -1         TO MAINTL
002918              MOVE ER-1962    TO EMI-ERROR
002919              PERFORM 9900-ERROR-FORMAT
002920                              THRU 9900-EXIT
002921           end-if
002922        end-if
002923     end-if
002924
002925     if ws-st-tot-comm-cap-sa = zeros
002926        continue
002927     else
002928        if comm-ah-accum numeric
002929           if (comm-ah-accum > ws-st-tot-comm-cap-sa)
002930              MOVE -1         TO MAINTL
002931              MOVE ER-1963    TO EMI-ERROR
002932              PERFORM 9900-ERROR-FORMAT
002933                              THRU 9900-EXIT
002934           end-if
002935        end-if
002936     end-if
002937
002938     IF MAINTI NOT = 'C'
002939        GO TO 4900-EXIT.
002940
002941     IF NOT PI-GA-BILLING
002942        GO TO 4900-EXIT.
002943
002944 4460-CHECK-IF-GA-BILLED.
002945     PERFORM 6700-BUILD-COMM-WORK THRU 6799-EXIT.
002946
002947     MOVE PI-COMM-POINTER        TO LCP-WS-ADDR-COMP
002948     SET ADDRESS OF COMMISSION-WORK-AREA TO LCP-WS-ADDR-PNTR.
002949     SET M-INDEX TO +1.
002950     MOVE +1 TO AXRF-SUB.
002951
002952 4470-GA-LOOP.
002953     IF ((AGENT (M-INDEX) NOT = WK-AM-AGT (AXRF-SUB)  OR
002954        ATYPE  (M-INDEX) NOT = WK-AM-TYPE (AXRF-SUB)) OR
002955        SINGLEL (M-INDEX) GREATER ZERO                OR
002956        JOINTL  (M-INDEX) GREATER ZERO                OR
002957        A-HL    (M-INDEX) GREATER ZERO                OR
002958        RCOMML  (M-INDEX) GREATER ZERO                OR
002959        CHGBCKL (M-INDEX) GREATER ZERO                OR
002960        CCEL    (M-INDEX) GREATER ZERO)               AND
002961        (WK-GA-BILL-DT (AXRF-SUB) NOT = LOW-VALUES AND SPACES)
002962           MOVE ER-2131   TO EMI-ERROR
002963           MOVE -1        TO AGENTL (M-INDEX)
002964           MOVE AL-UABON  TO AGENTA (M-INDEX)
002965           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
002966
002967     ADD +1 TO AXRF-SUB.
002968     SET M-INDEX UP BY +1.
002969
002970     IF AXRF-SUB NOT = +11
002971        GO TO 4470-GA-LOOP.
002972
002973 4900-EXIT.
002974      EXIT.
002975 EJECT
002976 5000-MOVE-TO-RECORD.
002977
002978     IF NAMEL   GREATER THAN ZERO  OR
002979        INCAREL GREATER THAN ZERO  OR
002980        ADDR1L  GREATER THAN ZERO  OR
002981        ACITYL  GREATER THAN ZERO  OR
002982        ASTATEL GREATER THAN ZERO  OR
002983        ZIPL    GREATER THAN ZERO  OR
002984        PHONEL  GREATER THAN ZERO  OR
002985        TAXNOL  GREATER THAN ZERO
002986         MOVE '*'                TO AM-PROFILE-CHANGE-SWITCH.
002987
002988     IF CSRL GREATER ZEROS
002989        MOVE CSRI                TO AM-CSR-CODE.
002990     IF NAMEL GREATER ZEROS
002991        MOVE NAMEI               TO AM-NAME.
002992     IF PCONTL GREATER ZEROS
002993        MOVE PCONTI              TO AM-CONTROL-NAME
002994     END-IF
002995     IF INCAREL GREATER ZEROS
002996        MOVE INCAREI             TO AM-PERSON.
002997     IF ADDR1L GREATER ZEROS
002998        MOVE ADDR1I              TO AM-ADDRS.
002999     IF TAXNOL GREATER ZEROS
003000        MOVE TAXNOI              TO AM-ID-NO.
003001     IF PHONEL GREATER ZEROS
003002        MOVE PHONEI              TO DEEDIT-FIELD
003003        PERFORM 8600-DEEDIT
003004        MOVE DEEDIT-FIELD-V0     TO WS-PHONE-NUM
003005        MOVE WS-PH1              TO AM-AREA-CODE
003006        MOVE WS-PH2              TO AM-TEL-PRE
003007        MOVE WS-PH3              TO AM-TEL-NBR.
003008
003009     IF ACITYL GREATER ZEROS
003010        MOVE ACITYI             TO AM-ADDR-CITY.
003011     IF ASTATEL GREATER ZEROS
003012        MOVE ASTATEI            TO AM-ADDR-STATE.
003013
003014     IF ZIPL NOT GREATER ZEROS
003015         GO TO 5010-CONT-MOVING.
003016
003017     MOVE ZIPI                   TO WS-ZIP-CODE.
003018
003019     IF WS-CANADIAN-ZIP
003020         IF WS-ZIP-4 = SPACE  OR  '-'
003021             MOVE WS-ZIP-CAN-2-POST1  TO AM-CAN-POSTAL-1
003022             MOVE WS-ZIP-CAN-2-POST2  TO AM-CAN-POSTAL-2
003023         ELSE
003024             MOVE WS-ZIP-CAN-1-POST1  TO AM-CAN-POSTAL-1
003025             MOVE WS-ZIP-CAN-1-POST2  TO AM-CAN-POSTAL-2
003026     ELSE
003027         IF WS-ZIP-6 = SPACE  OR  '-'
003028             MOVE WS-ZIP-AM-2-CODE    TO AM-ZIP-PRIME
003029             MOVE WS-ZIP-AM-2-PLUS4   TO AM-ZIP-PLUS4
003030         ELSE
003031             MOVE WS-ZIP-AM-1-CODE    TO AM-ZIP-PRIME
003032             MOVE WS-ZIP-AM-1-PLUS4   TO AM-ZIP-PLUS4.
003033
003034 5010-CONT-MOVING.
003035     IF TYPEL GREATER ZEROS
003036        MOVE TYPEI               TO AM-GPCD.
003037
003038     IF STDBENL GREATER ZEROS
003039        MOVE STDBENI             TO AM-STD-AH-TYPE.
003040
003041     IF INDGRPL GREATER ZEROS
003042        MOVE INDGRPI             TO AM-IG
003043        INSPECT AM-IG CONVERTING 'IG' TO '12'.
003044
003045     IF STATUSL GREATER ZEROS
003046        MOVE STATUSI             TO AM-STATUS
003047*       INSPECT AM-STATUS CONVERTING 'SFCTIA' TO '543210'
003048        INSPECT AM-STATUS CONVERTING 'PRLDSFCTIA' TO '9876543210'
003049     END-IF
003050
003051     IF CLPTOLPL GREATER ZEROS
003052        MOVE CLPTOLPI            TO AM-CLP-TOL-PCT
003053     END-IF.
003054
003055
003056     IF PRODCDL > 0
003057        MOVE PRODCDI             TO AM-DCC-PRODUCT-CODE
003058     END-IF
003059
003060     IF CLPSTL > 0
003061        MOVE CLPSTI              TO AM-DCC-CLP-STATE
003062     END-IF
003063
003064     IF MAXMFEEL > ZEROS
003065        MOVE WS-WORK-NUM         TO AM-DCC-MAX-MARKETING-FEE
003066     END-IF
003067
003068     IF LCOMML > ZEROS
003069        MOVE LCOMMI              TO AM-SPP-LEASE-COMM
003070     END-IF
003071
003072     IF REMITL GREATER ZEROS
003073        MOVE REMITI              TO AM-REMIT-TO.
003074     IF CONTRL GREATER ZEROS
003075        MOVE WS-ANVR-DT          TO AM-ANNIVERSARY-DATE.
003076*    IF PSILFFL GREATER +0
003077*       MOVE WS-LIFE-PSI         TO AM-LF-PSI-FACTOR.
003078*    IF PSIAHFL GREATER +0
003079*       MOVE WS-AH-PSI           TO AM-AH-PSI-FACTOR.
003080     IF PEFFDTEL GREATER ZEROS
003081        MOVE WS-PEFF-DT          TO AM-PREV-EFF-DT.
003082     IF PEXPDTEL GREATER ZEROS
003083        MOVE WS-PEXP-DT          TO AM-PREV-EXP-DT.
003084     IF COMMCALL GREATER ZEROS
003085        MOVE COMMCALI            TO AM-RECALC-COMM
003086        MOVE '*'                 TO AM-COMM-CHANGE-STATUS
003087     ELSE
003088         IF MAINTI = 'A'
003089             MOVE 'N'            TO AM-RECALC-COMM.
003090
003091     IF REINCALL GREATER ZEROS
003092        MOVE REINCALI            TO AM-RECALC-REIN
003093     ELSE
003094         IF MAINTI = 'A'
003095             MOVE 'N'            TO AM-RECALC-REIN.
003096
003097     IF REINTABL GREATER ZEROS
003098        MOVE REINTABI            TO AM-REI-TABLE.
003099
003100     IF PI-MAINT = 'A'
003101        MOVE 'P'                 TO AM-RET-ST-TAX-USE
003102     END-IF
003103
003104     MOVE 1                      TO SUB.
003105     SET M-INDEX                 TO 1.
003106
003107     
      * EXEC CICS
003108*         ASKTIME
003109*    END-EXEC.
      *    MOVE '0"                    "   #00008382' TO DFHEIV0
           MOVE X'302220202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303038333832' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003110
003111 5000-LOOP.
003112
003113*    IF PI-COMPANY-ID = 'DCC'
003114*       IF (M-INDEX = +5)
003115*          AND ((AGENTL (5) > ZEROS)
003116*              OR (ATYPEL (5) > ZEROS))
003117*             MOVE ER-3057         TO EMI-ERROR
003118*             MOVE -1              TO AGENTL (5)
003119*             MOVE AL-UABON        TO AGENTA (5)
003120*             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003121*             GO TO 5000-EXIT
003122*       END-IF
003123*    END-IF
003124
003125     IF MAINTI = 'C'
003126        IF ((AM-AGT (SUB) NOT = ZEROS AND AGENT (M-INDEX)) AND
003127           (COMMCALI NOT = 'Y' AND '1')                    AND
003128            AM-HI-CERT-DATE NOT = ZEROS)                    OR
003129           ((AM-AGT (SUB) = ZEROS AND
003130             (AGENT (M-INDEX)) NOT = LOW-VALUES AND SPACES AND
003131                                     ZEROS) AND
003132           (COMMCALI NOT = 'Y' AND '1')  AND
003133            AM-HI-CERT-DATE NOT = ZEROS)
003134              MOVE ER-2181         TO EMI-ERROR
003135              MOVE -1              TO COMMCALL
003136              MOVE AL-UABON        TO COMMCALA
003137              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003138              GO TO 5000-EXIT.
003139
003140     IF MAINTI = 'C'
003141        IF ((AM-COM-TYP (SUB) NOT = SPACES AND
003142             ATYPE (M-INDEX)) AND
003143           (COMMCALI NOT = 'Y' AND '1') AND
003144            AM-HI-CERT-DATE NOT = ZEROS)
003145            OR
003146           ((AM-COM-TYP (SUB) = SPACES AND
003147             (ATYPE (M-INDEX)) NOT = LOW-VALUES AND SPACES) AND
003148           (COMMCALI NOT = 'Y' AND '1')  AND
003149            AM-HI-CERT-DATE NOT = ZEROS)
003150              MOVE ER-2181         TO EMI-ERROR
003151              MOVE -1              TO COMMCALL
003152              MOVE AL-UABON        TO COMMCALA
003153              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003154              GO TO 5000-EXIT.
003155
003156     IF AM-ACCOUNT-BILLED
003157        IF COMMCALI = 'Y' OR '1'
003158            MOVE ER-2183         TO EMI-ERROR
003159            MOVE -1              TO COMMCALL
003160            MOVE AL-UABON        TO COMMCALA
003161            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003162            GO TO 5000-EXIT.
003163
003164     IF (AGENTL (M-INDEX) NOT GREATER ZEROS AND
003165        ATYPEL  (M-INDEX) NOT GREATER ZEROS)
003166        OR
003167        ((AGENT (M-INDEX) = ZEROS OR SPACES) AND
003168         ATYPE  (M-INDEX) = SPACES)
003169           MOVE ZEROS              TO AM-AGT   (SUB)
003170                                      AM-L-COM (SUB)
003171                                      AM-J-COM (SUB)
003172                                      AM-A-COM (SUB)
003173           MOVE SPACE TO AM-COM-TYP (SUB)
003174           SET M-INDEX UP BY 1
003175           ADD 1 TO SUB
003176           IF SUB = 11
003177              GO TO 5000-CHECK-MAX-FEES
003178            ELSE
003179              GO TO 5000-LOOP.
003180
003181     MOVE AGENT (M-INDEX)        TO AM-AGT (SUB).
003182     MOVE ATYPE (M-INDEX)        TO AM-COM-TYP (SUB).
003183
003184     IF SINGLEL (M-INDEX) GREATER ZEROS
003185        MOVE '*'                 TO AM-COMM-CHANGE-STATUS
003186        IF SINGLE-COMM-R (M-INDEX) NOT NUMERIC
003187           MOVE SINGLE-COMM-T (M-INDEX) TO AM-L-COMA (SUB)
003188          ELSE
003189           MOVE SINGLE-COMM-R (M-INDEX) TO AM-L-COM  (SUB)
003190           IF SINGLE-DASH (M-INDEX) = '-'
003191               MULTIPLY AM-L-COM (SUB) BY -1
003192                     GIVING AM-L-COM (SUB).
003193
003194     IF JOINTL (M-INDEX) GREATER ZEROS
003195        MOVE '*'                 TO AM-COMM-CHANGE-STATUS
003196        IF JOINT-COMM-R (M-INDEX) NOT NUMERIC
003197           MOVE JOINT-COMM-T (M-INDEX)  TO AM-J-COMA (SUB)
003198          ELSE
003199           MOVE JOINT-COMM-R (M-INDEX)  TO AM-J-COM (SUB)
003200           IF JOINT-DASH (M-INDEX) = '-'
003201               MULTIPLY AM-J-COM (SUB) BY -1
003202                     GIVING AM-J-COM (SUB).
003203
003204     IF A-HL (M-INDEX) GREATER ZEROS
003205        MOVE '*'                 TO AM-COMM-CHANGE-STATUS
003206        IF A-H-COMM-R (M-INDEX) NOT NUMERIC
003207           MOVE A-H-COMM-T (M-INDEX)  TO AM-A-COMA (SUB)
003208          ELSE
003209           MOVE A-H-COMM-R (M-INDEX)  TO AM-A-COM (SUB)
003210           IF A-H-DASH (M-INDEX) = '-'
003211               MULTIPLY AM-A-COM (SUB) BY -1
003212                     GIVING AM-A-COM (SUB).
003213
003214     IF RECALL (M-INDEX) GREATER ZEROS
003215         MOVE RECAL  (M-INDEX)   TO AM-RECALC-LV-INDIC (SUB).
003216
003217     IF RCOMML (M-INDEX) GREATER ZEROS
003218         MOVE RCOMM  (M-INDEX)   TO AM-RETRO-LV-INDIC (SUB).
003219
003220     IF CHGBCKL (M-INDEX) GREATER ZEROS
003221         MOVE CHGBCK (M-INDEX)   TO AM-COMM-CHARGEBACK (SUB).
003222
003223     IF CCEL (M-INDEX) GREATER ZEROS
003224         MOVE CCEIND (M-INDEX)   TO AM-GL-CODES (SUB).
003225
003226     SET M-INDEX UP BY 1.
003227     ADD 1 TO SUB.
003228     IF M-INDEX NOT = 11
003229        GO TO 5000-LOOP.
003230
003231 5000-CHECK-MAX-FEES.
003232
003233     IF (AM-DCC-PRODUCT-CODE = 'DDF')
003234        MOVE +1                     TO S1
003235        PERFORM UNTIL S1 > +10
003236           IF (AM-A-COM (S1) NUMERIC)
003237              AND (AM-A-COMA (S1) (3:1) NOT = 'L' AND 'M'
003238                     AND 'O')
003239              AND (AM-COM-TYP (S1) = 'J')
003240                 COMPUTE COMM-MFEE-ACCUM = COMM-MFEE-ACCUM
003241                    + (AM-A-COM (S1) * +1000)
003242           END-IF
003243           ADD +1             TO S1
003244        END-PERFORM
003245
003246        IF COMM-MFEE-ACCUM > AM-DCC-MAX-MARKETING-FEE
003247           MOVE -1               TO MAXMFEEL
003248           MOVE AL-UABON         TO MAXMFEEA
003249           MOVE ER-1817          TO EMI-ERROR
003250           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003251        END-IF
003252     END-IF
003253
003254     .
003255 5000-EXIT.
003256      EXIT.
003257 EJECT
003258
003259 5100-WRITE-TS.
003260     PERFORM 6500-READ-ACCT THRU 6599-EXIT.
003261
003262     IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
003263         MOVE ZEROS              TO PI-CR-CARRIER
003264     ELSE
003265         MOVE AM-CARRIER         TO PI-CR-CARRIER.
003266
003267     IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
003268         MOVE ZEROS              TO PI-CR-GROUPING
003269     ELSE
003270         MOVE AM-GROUPING        TO PI-CR-GROUPING.
003271
003272     IF AM-REMIT-TO NOT = ZERO
003273         MOVE AM-AGT (AM-REMIT-TO)
003274                                 TO PI-CR-FIN-RESP
003275       ELSE
003276         MOVE AM-ACCOUNT         TO PI-CR-FIN-RESP.
003277
003278     MOVE 'A'                    TO PI-CR-TYPE.
003279 5110-AGT-LOOP.
003280     ADD 1                       TO  AGT-SUB.
003281     IF AGT-SUB GREATER 10
003282         MOVE LOW-VALUES         TO PI-CR-CONTROL-IN-PROGRESS
003283         GO TO 5120-WRITE.
003284
003285************************************************************
003286**  MODIFICATION MADE TO ADD SERVICE FEES                 **
003287************************************************************
003288
003289     IF AM-COM-TYP (AGT-SUB) = 'C' OR 'D' OR 'R'
003290                  OR 'F' OR 'U'
003291         MOVE AM-AGT (AGT-SUB)   TO  PI-CR-ACCOUNT
003292         GO TO 5120-WRITE
003293     ELSE
003294         GO TO 5110-AGT-LOOP.
003295
003296 5120-WRITE.
003297     
      * EXEC CICS WRITEQ TS
003298*         QUEUE    (QID)
003299*         FROM     (PROGRAM-INTERFACE-BLOCK)
003300*         LENGTH   (WS-COMM-LENGTH)
003301*         ITEM     (W-ONE)
003302*    END-EXEC.
      *    MOVE '*" I   L              ''   #00008572' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303038353732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 W-ONE, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003303
003304 5199-EXIT.
003305     EXIT.
003306
003307 EJECT
003308
003309 5200-RECOVER-TS.
003310
003311     IF PI-CALLING-PROGRAM NOT = XCTL-652 AND
003312                                 XCTL-653 AND
003313                                 XCTL-689 AND
003314                                 XCTL-608 AND
003315                                 XCTL-690
003316         GO TO 5299-EXIT.
003317
003318     
      * EXEC CICS HANDLE CONDITION
003319*        QIDERR  (5250-QID-ERROR)
003320*    END-EXEC.
      *    MOVE '"$N                   ! , #00008593' TO DFHEIV0
           MOVE X'22244E202020202020202020' &
                X'202020202020202020202120' &
                X'2C20233030303038353933' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003321
003322     
      * EXEC CICS READQ TS
003323*        QUEUE    (QID)
003324*        INTO     (PROGRAM-INTERFACE-BLOCK)
003325*        LENGTH   (WS-COMM-LENGTH)
003326*        ITEM     (W-ONE)
003327*    END-EXEC.
      *    MOVE '*$II   L              ''   #00008597' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' &
                X'202020202020202020202720' &
                X'2020233030303038353937' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 W-ONE, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003328
003329 5200-DELETE-TS.
003330
003331     
      * EXEC CICS DELETEQ TS
003332*        QUEUE    (QID)
003333*    END-EXEC.
      *    MOVE '*&                    #   #00008606' TO DFHEIV0
           MOVE X'2A2620202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303038363036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003334
003335     GO TO 5299-EXIT.
003336
003337 5250-QID-ERROR.
003338     MOVE ER-0033                TO  EMI-ERROR.
003339     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
003340
003341 5299-EXIT.
003342     EXIT.
003343
003344     EJECT
003345
003346 5300-UPDATE-REQUEST-FILE.
003347     MOVE +0                     TO SUB1.
003348
003349 5300-FIND-ACCOUNT-AGENT.
003350     ADD +1                      TO SUB1.
003351
003352     IF SUB1 GREATER THAN +10
003353        MOVE ER-2949             TO EMI-ERROR
003354        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
003355        MOVE -1                  TO AGENTL (1)
003356        
      * EXEC CICS SYNCPOINT ROLLBACK
003357*       END-EXEC
      *    MOVE '6"R                   !   #00008631' TO DFHEIV0
           MOVE X'362252202020202020202020' &
                X'202020202020202020202120' &
                X'2020233030303038363331' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003358        GO TO 8200-SEND-DATAONLY.
003359
003360     MOVE SAVE-ACCT-AGENT      TO SAVE-NEW-ACCT-AGENT.
003361
003362************************************************************
003363**  MODIFICATION MADE TO ADD SERVICE FEES                 **
003364************************************************************
003365
003366     IF AM-COM-TYP  (SUB1) = 'C' OR 'D' OR 'F'
003367        IF AM-AGT   (SUB1) NOT = SAVE-ACCT-AGENT
003368           MOVE AM-AGT (SUB1)  TO SAVE-NEW-ACCT-AGENT.
003369
003370     IF AM-AGT (AM-REMIT-TO) NOT = SAVE-FIN-RESP
003371        GO TO 5325-PROCESS-RQST-FILE.
003372
003373     IF SAVE-ACCT-AGENT NOT = SAVE-NEW-ACCT-AGENT
003374        GO TO 5325-PROCESS-RQST-FILE.
003375
003376     GO TO 5399-EXIT.
003377
003378 5325-PROCESS-RQST-FILE.
003379     MOVE LOW-VALUES             TO RQST3-KEY.
003380     MOVE AM-COMPANY-CD          TO RQST3-COMPANY-CD.
003381     MOVE AM-CARRIER             TO RQST3-CARRIER.
003382     MOVE AM-GROUPING            TO RQST3-GROUPING.
003383     MOVE SAVE-FIN-RESP          TO RQST3-FIN-RESP.
003384     MOVE SAVE-ACCT-AGENT        TO RQST3-ACCT-AGENT.
003385
003386     
      * EXEC CICS HANDLE CONDITION
003387*        NOTFND(5399-EXIT)
003388*        ENDFILE(5399-EXIT)
003389*    END-EXEC.
      *    MOVE '"$I''                  ! - #00008661' TO DFHEIV0
           MOVE X'222449272020202020202020' &
                X'202020202020202020202120' &
                X'2D20233030303038363631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003390
003391 5325-PROCESS-REQUEST-FILE.
003392     
      * EXEC CICS STARTBR
003393*         DATASET   (RQST3-FILE-ID)
003394*         RIDFLD    (RQST3-KEY)
003395*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008667' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303038363637' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST3-FILE-ID, 
                 RQST3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003396
003397 5330-READNEXT-REQUEST.
003398     
      * EXEC CICS READNEXT
003399*        SET    (ADDRESS OF AR-REQUEST-RECORD)
003400*        DATASET(RQST3-FILE-ID)
003401*        RIDFLD (RQST3-KEY)
003402*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008673' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303038363733' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST3-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 RQST3-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003403
003404     IF RQST3-KEY = SAVE-RQST3-KEY
003405        GO TO 5330-READNEXT-REQUEST.
003406
003407     MOVE RQST3-KEY              TO SAVE-RQST3-KEY.
003408
003409     IF RQ-COMPANY-CD-A2 NOT = PI-COMPANY-CD
003410        GO TO 5350-REQUESTS-FINISHED.
003411
003412     IF RQ-FIN-RESP-A2 NOT = SAVE-FIN-RESP
003413        GO TO 5350-REQUESTS-FINISHED.
003414
003415     IF RQ-ACCT-AGENT-A2 NOT = SAVE-ACCT-AGENT
003416        GO TO 5350-REQUESTS-FINISHED.
003417
003418     MOVE RQ-CONTROL-PRIMARY     TO RQST-KEY.
003419
003420     
      * EXEC CICS ENDBR
003421*        DATASET(RQST3-FILE-ID)
003422*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008695' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038363935' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST3-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003423
003424     
      * EXEC CICS READ
003425*        SET    (ADDRESS OF AR-REQUEST-RECORD)
003426*        DATASET(RQST-FILE-ID)
003427*        RIDFLD (RQST-KEY)
003428*        UPDATE
003429*    END-EXEC.
      *    MOVE '&"S        EU         (   #00008699' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303038363939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 RQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003430
003431     MOVE AM-AGT (AM-REMIT-TO)   TO RQ-FIN-RESP-A2.
003432     MOVE SAVE-NEW-ACCT-AGENT    TO RQ-ACCT-AGENT-A2.
003433
003434     
      * EXEC CICS REWRITE
003435*        DATASET   (RQST-FILE-ID)
003436*        FROM      (AR-REQUEST-RECORD)
003437*    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008709' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303038373039' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003438
003439     GO TO  5325-PROCESS-REQUEST-FILE.
003440
003441 5350-REQUESTS-FINISHED.
003442     
      * EXEC CICS ENDBR
003443*        DATASET(RQST3-FILE-ID)
003444*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008717' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303038373137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST3-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003445
003446 5399-EXIT.
003447     EXIT.
003448
003449
003450 EJECT
003451
003452 6000-MOVE-TO-SCREEN.
003453     PERFORM 8000-STATE-REC-READ   THRU 8010-EXIT.
003454
003455     MOVE AM-LAST-MAINT-USER     TO MAINTBYO.
003456     MOVE AM-NAME                TO NAMEO.
003457     MOVE AM-CONTROL-NAME        TO PCONTO.
003458     MOVE AM-CSR-CODE            TO CSRO.
003459     MOVE AM-PERSON              TO INCAREO.
003460     MOVE AM-ADDRS               TO ADDR1O.
003461     MOVE AM-ID-NO               TO TAXNOO.
003462     MOVE AM-TEL-NO              TO WS-PHONE.
003463     MOVE WS-PHONE-NUM           TO PHONEO.
003464     INSPECT PHONEI CONVERTING SPACES TO '-'.
003465     MOVE AM-ADDR-CITY           TO ACITYO
003466     MOVE AM-ADDR-STATE          TO ASTATEO
003467
003468     MOVE SPACES                 TO WS-ZIP-CODE.
003469     IF AM-CANADIAN-POST-CODE
003470         MOVE AM-CAN-POSTAL-1    TO WS-ZIP-CAN-2-POST1
003471         MOVE AM-CAN-POSTAL-2    TO WS-ZIP-CAN-2-POST2
003472     ELSE
003473         MOVE AM-ZIP-PRIME       TO WS-ZIP-AM-2-CODE
003474         IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
003475             MOVE '-'            TO WS-ZIP-AM-2-DASH
003476             MOVE AM-ZIP-PLUS4   TO WS-ZIP-AM-2-PLUS4.
003477
003478     MOVE WS-ZIP-CODE            TO ZIPO.
003479
003480     MOVE AM-GPCD                TO TYPEO.
003481     MOVE AM-STD-AH-TYPE         TO STDBENO.
003482
003483     MOVE AM-IG                  TO INDGRPO.
003484     INSPECT INDGRPO CONVERTING '12' TO 'IG'.
003485
003486     MOVE AM-STATUS              TO STATUSO.
003487*    INSPECT STATUSO CONVERTING '543210' TO 'SFCTIA'
003488     INSPECT STATUSO CONVERTING '9876543210' TO 'PRLDSFCTIA'
003489
003490     MOVE AM-REMIT-TO            TO REMITO.
003491
003492     IF PI-AR-PROCESSING
003493        IF AM-AR-HI-CERT-DATE = LOW-VALUES
003494           MOVE AL-UNNON         TO REMITA
003495        ELSE
003496           MOVE AL-SANON         TO REMITA
003497     ELSE
003498        MOVE AL-UNNON            TO REMITA.
003499
003500***  Y2K PROJ 7744
003501     MOVE AM-ANNIVERSARY-DATE    TO DC-GREG-DATE-1-YMD.
003502     MOVE '3'                    TO DC-OPTION-CODE.
003503     PERFORM 9700-DATE-LINK.
003504     IF DATE-CONVERSION-ERROR
003505        MOVE SPACES              TO CONTRO
003506     ELSE
003507        MOVE DC-GREG-DATE-1-EDIT TO CONTRO
003508     END-IF.
003509***  Y2K PROJ 7744
003510
003511*    MOVE PI-LIFE-OVERRIDE-L6    TO PSILFTO
003512*    MOVE PI-AH-OVERRIDE-L6      TO PSIAHTO.
003513
003514*    IF AM-LF-PSI-FACTOR NUMERIC
003515*       MOVE AM-LF-PSI-FACTOR    TO PSILFFO.
003516*
003517*    IF AM-AH-PSI-FACTOR NUMERIC
003518*       MOVE AM-AH-PSI-FACTOR    TO PSIAHFO.
003519
003520     MOVE AM-RECALC-COMM         TO COMMCALO.
003521     MOVE AL-UANON               TO COMMCALA.
003522     MOVE AM-RECALC-REIN         TO REINCALO.
003523     MOVE AM-REI-TABLE           TO REINTABO.
003524     MOVE AM-1ST-PROD-DATE       TO DC-GREG-DATE-1-YMD.
003525     MOVE '3'                    TO DC-OPTION-CODE.
003526     PERFORM 9700-DATE-LINK.
003527     IF DATE-CONVERSION-ERROR
003528        MOVE SPACES              TO PRODATEO
003529     ELSE
003530        MOVE DC-GREG-DATE-1-EDIT TO PRODATEO.
003531
003532     IF PI-COMPANY-ID = 'DCC' or 'VPP'
003533        IF AM-CLP-TOL-PCT NOT NUMERIC
003534           MOVE ZEROS            TO AM-CLP-TOL-PCT
003535        END-IF
003536        IF AM-SPP-LEASE-COMM NOT NUMERIC
003537           MOVE ZEROS            TO AM-SPP-LEASE-COMM
003538        END-IF
003539        MOVE AM-CLP-TOL-PCT      TO CLPTOLPO
003540        MOVE AM-SPP-LEASE-COMM   TO LCOMMO
003541        MOVE AM-DCC-PRODUCT-CODE TO PRODCDO
003542        MOVE AM-DCC-CLP-STATE    TO CLPSTO
003543        MOVE AL-UANON            TO CLPSTA
003544
003545        IF AM-DCC-MAX-MARKETING-FEE NOT NUMERIC
003546           MOVE ZEROS            TO AM-DCC-MAX-MARKETING-FEE
003547        END-IF
003548        MOVE AM-DCC-MAX-MARKETING-FEE
003549                                 TO MAXMFEEO
003550     ELSE
003551        MOVE AL-SADOF            TO CTPLABLA
003552                                    CLPTOLPA
003553                                    LCLABLA
003554                                    LCOMMA
003555     END-IF
003556
003557     IF AM-RET-Y-N = ' '  OR  'N'
003558         MOVE 'NO '              TO RETROCDO
003559     ELSE
003560         MOVE 'YES'              TO RETROCDO.
003561
003562     IF AM-ACCOUNT-BILLED
003563         MOVE 'YES'              TO BILLCDO
003564     ELSE
003565         MOVE 'NO '              TO BILLCDO.
003566
003567     MOVE AL-UANON               TO RETROCDA.
003568     MOVE AL-SANON               TO PRODATEA
003569                                    BILLCDA.
003570     MOVE AM-PREV-EFF-DT TO DC-GREG-DATE-1-YMD.
003571     MOVE '3' TO DC-OPTION-CODE.
003572     PERFORM 9700-DATE-LINK.
003573     IF DATE-CONVERSION-ERROR
003574        MOVE SPACES TO PEFFDTEO
003575     ELSE
003576        MOVE DC-GREG-DATE-1-EDIT TO PEFFDTEO.
003577
003578     IF AM-PREV-EXP-DT NOT = 99999999999
003579        MOVE AM-PREV-EXP-DT TO DC-GREG-DATE-1-YMD
003580        MOVE '3' TO DC-OPTION-CODE
003581        PERFORM 9700-DATE-LINK
003582        IF DATE-CONVERSION-ERROR
003583           MOVE SPACES TO PEXPDTEO
003584        ELSE
003585           MOVE DC-GREG-DATE-1-EDIT TO PEXPDTEO
003586     ELSE
003587        MOVE '99/99/99' TO PEXPDTEO.
003588
003589     IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'
003590        MOVE AL-UANON            TO PEXPDTEA
003591                                    PEFFDTEA
003592        MOVE AL-SANOF TO            PDSPLYA.
003593
003594     MOVE LOW-VALUES             TO MAP-AGENT-AREA.
003595     MOVE PI-COMM-POINTER        TO LCP-WS-ADDR-COMP
003596     SET ADDRESS OF COMMISSION-WORK-AREA TO LCP-WS-ADDR-PNTR.
003597
003598     MOVE 1  TO SUB.
003599     SET M-INDEX TO 1.
003600
003601 6005-LOOP.
003602     IF AM-AGT (SUB) = SPACES  OR ZEROS
003603        IF PI-AR-PROCESSING
003604           IF AM-AR-HI-CERT-DATE GREATER THAN LOW-VALUES
003605              MOVE AL-SANOF     TO AGENTA  (M-INDEX)
003606                                   ATYPEA (M-INDEX)
003607                                   SINGLEA (M-INDEX)
003608                                   JOINTA (M-INDEX)
003609                                   A-HA    (M-INDEX)
003610              GO TO 6010-BUMP-SUB
003611           ELSE
003612              GO TO 6010-BUMP-SUB
003613        ELSE
003614           GO TO 6010-BUMP-SUB.
003615
003616     MOVE AM-AGT (SUB)           TO AGENT (M-INDEX).
003617
003618***DMD CUSTOM CODE - CAPTURE THE FIRST AGENT TO BE DISPLAYED
003619     IF WS-DMD-AGENT = LOW-VALUES
003620        MOVE AM-AGT (SUB)        TO WS-DMD-AGENT.
003621***DMD ******************************************************
003622
003623     IF PI-GA-BILLING
003624        MOVE AM-AGT (SUB)        TO WK-AM-AGT (SUB).
003625     MOVE AL-UANON               TO AGENTA (M-INDEX).
003626
003627     MOVE AM-COM-TYP (SUB)       TO ATYPE (M-INDEX).
003628     IF PI-GA-BILLING
003629        MOVE AM-COM-TYP (SUB)    TO WK-AM-TYPE (SUB).
003630     MOVE AL-UANON               TO ATYPEA (M-INDEX).
003631
003632     IF (AM-L-COM (SUB) NOT NUMERIC)
003633        OR (AM-L-COMA (SUB) (3:1) = 'L' OR 'M' OR 'O')
003634        MOVE AM-L-COMA (SUB)     TO SINGLE-COMM-T (M-INDEX)
003635     ELSE
003636        IF AM-L-COM (SUB) NOT = ZEROS
003637           MOVE AM-L-COM  (SUB)  TO SINGLE-COMM-O (M-INDEX).
003638
003639     move al-uanon to singlea (m-index)
003640
003641     IF (AM-J-COM (SUB) NOT NUMERIC)
003642        OR (AM-J-COMA (SUB) (3:1) = 'L' OR 'M' OR 'O')
003643        MOVE AM-J-COMA (SUB)     TO JOINT-COMM-T (M-INDEX)
003644     ELSE
003645        IF AM-J-COM (SUB) NOT = ZEROS
003646           MOVE AM-J-COM  (SUB)  TO JOINT-COMM-O (M-INDEX).
003647     move al-uanon to jointa (m-index)
003648
003649     IF (AM-A-COM (SUB) NOT NUMERIC)
003650        OR (AM-A-COMA (SUB) (3:1) = 'L' OR 'M' OR 'O')
003651        MOVE AM-A-COMA (SUB)     TO A-H-COMM-T (M-INDEX)
003652     ELSE
003653        IF AM-A-COM (SUB) NOT = ZEROS
003654           MOVE AM-A-COM  (SUB)  TO A-H-COMM-O (M-INDEX).
003655     move al-uanon to a-ha (m-index)
003656
003657************************************************************
003658**  MODIFICATION MADE TO ADD SERVICE FEES                 **
003659************************************************************
003660
003661     IF (AM-COM-TYP (SUB) NOT = 'C' AND 'D' AND 'O' AND 'P'
003662            AND 'F' AND 'S' AND 'G' AND 'B' AND 'I'
003663            AND 'K' AND 'L' AND 'J' AND 'M' AND 'A' AND 'N')
003664         GO TO 6006-CONTINUE.
003665
003666     .
003667 6006-CONTINUE.
003668
003669     IF WS-ST-COMM-CAP-SL NOT NUMERIC
003670        MOVE ZEROS               TO WS-ST-COMM-CAP-SL
003671     end-if
003672
003673     IF WS-ST-COMM-CAP-JL NOT NUMERIC
003674        MOVE ZEROS               TO WS-ST-COMM-CAP-JL
003675     end-if
003676
003677     IF WS-ST-COMM-CAP-SA NOT NUMERIC
003678        MOVE ZEROS               TO WS-ST-COMM-CAP-SA
003679     end-if
003680
003681     IF (AM-L-COM (SUB) NUMERIC)
003682        AND (AM-L-COMA (SUB) (3:1) NOT = 'L' AND 'M' AND 'O')
003683        if am-gl-codes(sub) <> 'Y'
003684           ADD AM-L-COM (SUB)    TO COMM-SL-ACCUM
003685           if am-com-typ (sub) = 'C' or 'D'
003686              ADD AM-L-COM (SUB) TO COMM-SL-ACCUM-al
003687           else
003688              if am-com-typ (sub) = 'O' or 'P'
003689                 add am-l-com (sub) to comm-sl-accum-gl
003690              end-if
003691           end-if
003692        end-if
003693     ELSE
003694        MOVE PI-COMPANY-CD    TO COMM-COMP-CD
003695        MOVE AM-L-COMA (SUB)  TO COMM-TABLE
003696        MOVE PI-LIFE-OVERRIDE-L1
003697                              TO COMM-LF-AH
003698        MOVE LOW-VALUES       TO COMM-FILLER
003699        PERFORM 6400-READ-COMMISSION
003700                              THRU 6459-EXIT
003701        if am-gl-codes(sub) <> 'Y'
003702           add ws-table-max         to COMM-SL-ACCUM
003703           if atype (m-index) = 'C' or 'D'
003704              add ws-table-max to comm-sl-accum-al
003705           else
003706              if atype (m-index) = 'O' or 'P'
003707                 add ws-table-max to comm-sl-accum-gl
003708              end-if
003709           end-if
003710        end-if
003711     END-IF
003712
003713     IF (AM-J-COM (SUB) NUMERIC)
003714        AND (AM-J-COMA (SUB) (3:1) NOT = 'L' AND 'M' AND 'O')
003715        if am-gl-codes(sub) <> 'Y'
003716           ADD AM-J-COM (SUB)    TO COMM-JL-ACCUM
003717           if am-com-typ (sub) = 'C' or 'D'
003718              ADD AM-J-COM (SUB) TO COMM-JL-ACCUM-al
003719           else
003720              if am-com-typ (sub) = 'O' or 'P'
003721                 add am-J-com (sub) to comm-Jl-accum-gl
003722              end-if
003723           end-if
003724        end-if
003725     ELSE
003726        MOVE PI-COMPANY-CD    TO COMM-COMP-CD
003727        MOVE AM-J-COMA (SUB)  TO COMM-TABLE
003728        MOVE PI-LIFE-OVERRIDE-L1
003729                              TO COMM-LF-AH
003730        MOVE LOW-VALUES       TO COMM-FILLER
003731        PERFORM 6400-READ-COMMISSION
003732                              THRU 6459-EXIT
003733        if am-gl-codes(sub) <> 'Y'
003734           add ws-table-max to COMM-JL-ACCUM
003735           if atype (m-index) = 'C' or 'D'
003736              add ws-table-max to comm-jl-accum-al
003737           else
003738              if atype (m-index) = 'O' or 'P'
003739                 add ws-table-max to comm-jl-accum-gl
003740              end-if
003741           end-if
003742        end-if
003743     END-IF
003744
003745     IF (AM-A-COM (SUB) NUMERIC)
003746        AND (AM-A-COMA (SUB) (3:1) NOT = 'L' AND 'M' AND 'O')
003747        if am-gl-codes(sub) <> 'Y'
003748           ADD AM-A-COM (SUB)    TO COMM-AH-ACCUM
003749           if am-com-typ (sub) = 'C' or 'D'
003750              ADD AM-A-COM (SUB) TO COMM-AH-ACCUM-al
003751           else
003752              if am-com-typ (sub) = 'O' or 'P'
003753                 add am-a-com (sub) to comm-AH-accum-gl
003754              end-if
003755           end-if
003756        end-if
003757     ELSE
003758        MOVE PI-COMPANY-CD    TO COMM-COMP-CD
003759        MOVE AM-A-COMA (SUB)  TO COMM-TABLE
003760        MOVE PI-AH-OVERRIDE-L1
003761                              TO COMM-LF-AH
003762        MOVE LOW-VALUES       TO COMM-FILLER
003763        PERFORM 6400-READ-COMMISSION
003764                              THRU 6459-EXIT
003765        if am-gl-codes(sub) <> 'Y'
003766           add ws-table-max to COMM-AH-ACCUM
003767           if atype (m-index) = 'C' or 'D'
003768              add ws-table-max to comm-AH-accum-al
003769           else
003770              if atype (m-index) = 'O' or 'P'
003771                 add ws-table-max to comm-ah-accum-gl
003772              end-if
003773           end-if
003774        end-if
003775     END-IF
003776
003777
003778
003779     IF AM-RECALC-LV-INDIC (SUB) NOT = SPACE
003780         MOVE AM-RECALC-LV-INDIC (SUB) TO RECAL (M-INDEX).
003781
003782     IF AM-RETRO-LV-INDIC (SUB) NOT = SPACE
003783         MOVE AM-RETRO-LV-INDIC (SUB) TO RCOMM (M-INDEX).
003784
003785     IF AM-COMM-CHARGEBACK (SUB)  NUMERIC
003786        NEXT SENTENCE
003787     ELSE
003788        MOVE ZEROS              TO  AM-COMM-CHARGEBACK (SUB).
003789
003790     IF AM-COMM-CHARGEBACK (SUB)   NOT  =    ZEROS
003791         MOVE AM-COMM-CHARGEBACK (SUB) TO CHGBCK (M-INDEX).
003792
003793     IF AM-GL-CODES (SUB) = 'Y'
003794        MOVE AM-GL-CODES (SUB)   TO CCEIND (M-INDEX)
003795        MOVE AL-UANON            TO CCEA   (M-INDEX)
003796     ELSE
003797        MOVE 'N'                 TO CCEIND (M-INDEX)
003798     END-IF
003799
003800     if am-gl-codes(sub) = 'Y'
003801        if ((am-com-typ(sub) = 'C' OR 'D')
003802           and (WS-COMM-CAP-LIMIT-TO not = 'A' AND 'B'))
003803                            or
003804           ((am-com-typ(sub) = 'O' or 'P')
003805           and (ws-comm-cap-limit-to not = 'G' AND 'B'))
003806           move er-3065       to emi-error
003807           move -1            to maintl
003808           perform 9900-error-format
003809                                 thru 9900-exit
003810        end-if
003811     end-if
003812
003813     IF AM-GL-CODES (SUB)   NOT  =    ZEROS
003814         MOVE AM-GL-CODES (SUB)  TO  CCEIND (M-INDEX).
003815
003816     IF NOT PI-AR-PROCESSING
003817        GO TO 6010-BUMP-SUB.
003818
003819     IF AM-AR-HI-CERT-DATE = LOW-VALUES
003820        GO TO 6010-BUMP-SUB.
003821
003822     MOVE AL-SANON              TO AGENTA  (M-INDEX)
003823                                   ATYPEA  (M-INDEX)
003824                                   SINGLEA (M-INDEX)
003825                                   JOINTA  (M-INDEX)
003826                                   A-HA    (M-INDEX).
003827
003828 6010-BUMP-SUB.
003829     ADD 1  TO SUB.
003830     SET M-INDEX UP BY 1.
003831     IF SUB LESS 11
003832        GO TO 6005-LOOP.
003833
003834     if ws-st-comm-cap-sl = zeros
003835        continue
003836     else
003837        if comm-sl-accum-al > ws-st-comm-cap-sl
003838           MOVE -1               TO MAINTL
003839           MOVE ER-1955          TO EMI-ERROR
003840           PERFORM 9900-ERROR-FORMAT
003841                                 THRU 9900-EXIT
003842        end-if
003843     end-if
003844
003845     if ws-st-comm-cap-jl = zeros
003846        continue
003847     else
003848        if comm-jl-accum-al > ws-st-comm-cap-jl
003849           MOVE -1               TO MAINTL
003850           MOVE ER-1956          TO EMI-ERROR
003851           PERFORM 9900-ERROR-FORMAT
003852                                 THRU 9900-EXIT
003853        end-if
003854     end-if
003855
003856     if ws-st-comm-cap-sa = zeros
003857        continue
003858     else
003859        if comm-ah-accum-al > ws-st-comm-cap-sa
003860           MOVE -1               TO MAINTL
003861           MOVE ER-1957          TO EMI-ERROR
003862           PERFORM 9900-ERROR-FORMAT
003863                                 THRU 9900-EXIT
003864        end-if
003865     end-if
003866
003867
003868     if ws-st-ga-comm-cap-sl = zeros
003869        continue
003870     else
003871        if comm-sl-accum-gl > ws-st-ga-comm-cap-sl
003872           MOVE -1               TO MAINTL
003873           MOVE ER-1958          TO EMI-ERROR
003874           PERFORM 9900-ERROR-FORMAT
003875                                 THRU 9900-EXIT
003876        end-if
003877     end-if
003878
003879     if ws-st-ga-comm-cap-jl = zeros
003880        continue
003881     else
003882        if comm-jl-accum-gl > ws-st-ga-comm-cap-jl
003883           MOVE -1               TO MAINTL
003884           MOVE ER-1959          TO EMI-ERROR
003885           PERFORM 9900-ERROR-FORMAT
003886                                 THRU 9900-EXIT
003887        end-if
003888     end-if
003889
003890     if ws-st-ga-comm-cap-sa = zeros
003891        continue
003892     else
003893        if comm-ah-accum-gl > ws-st-ga-comm-cap-sa
003894           MOVE -1               TO MAINTL
003895           MOVE ER-1960          TO EMI-ERROR
003896           PERFORM 9900-ERROR-FORMAT
003897                                 THRU 9900-EXIT
003898        end-if
003899     end-if
003900
003901     if ws-st-tot-comm-cap-sl = zeros
003902        continue
003903     else
003904        if comm-sl-accum > ws-st-tot-comm-cap-sl
003905           MOVE -1                  TO MAINTL
003906           MOVE ER-1961             TO EMI-ERROR
003907           PERFORM 9900-ERROR-FORMAT
003908                                    THRU 9900-EXIT
003909        end-if
003910     end-if
003911
003912     if ws-st-tot-comm-cap-jl = zeros
003913        continue
003914     else
003915        if comm-jl-accum > ws-st-tot-comm-cap-jl
003916           MOVE -1                  TO MAINTL
003917           MOVE ER-1962             TO EMI-ERROR
003918           PERFORM 9900-ERROR-FORMAT
003919                                    THRU 9900-EXIT
003920        end-if
003921     end-if
003922
003923     if ws-st-tot-comm-cap-sa = zeros
003924        continue
003925     else
003926        if comm-ah-accum > ws-st-tot-comm-cap-sa
003927           MOVE -1                  TO MAINTL
003928           MOVE ER-1963             TO EMI-ERROR
003929           PERFORM 9900-ERROR-FORMAT
003930                                    THRU 9900-EXIT
003931        end-if
003932     end-if
003933
003934     .
003935 6099-EXIT.
003936      EXIT.
003937 EJECT
003938 6300-STARTBR.
003939     
      * EXEC CICS STARTBR
003940*         DATASET   (ACCT-FILE-ID)
003941*         RIDFLD    (WS-KEY-SAVE)
003942*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009214' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'2020233030303039323134' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 WS-KEY-SAVE, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003943
003944     MOVE 'Y'                    TO BROWSE-STARTED-SW.
003945
003946 6300-EXIT.
003947      EXIT.
003948
003949 6310-READNEXT.
003950     
      * EXEC CICS READNEXT
003951*         DATASET    (ACCT-FILE-ID)
003952*         SET        (ADDRESS OF ACCOUNT-MASTER)
003953*         RIDFLD     (WS-KEY-SAVE)
003954*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009225' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'2020233030303039323235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-KEY-SAVE, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003955
003956 6310-EXIT.
003957      EXIT.
003958
003959 6320-ENDBR.
003960     MOVE SPACE               TO BROWSE-STARTED-SW.
003961     
      * EXEC CICS ENDBR
003962*        DATASET(ACCT-FILE-ID)
003963*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009236' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039323336' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
003964
003965 6320-EXIT.
003966      EXIT.
003967 EJECT
003968 6400-READ-COMMISSION.
003969
003970     MOVE ZEROS                  TO WS-COMM-ERROR-SW
003971     
      * EXEC CICS HANDLE CONDITION
003972*         NOTFND   (6450-NOT-FOUND)
003973*    END-EXEC
      *    MOVE '"$I                   ! . #00009246' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2E20233030303039323436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003974
003975     
      * EXEC CICS READ
003976*        GTEQ
003977*        DATASET   (COMM-FILE-ID)
003978*        SET       (ADDRESS OF COMM-TABLE-RECORD)
003979*        RIDFLD    (COMM-KEY)
003980*    END-EXEC
      *    MOVE '&"S        G          (   #00009250' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303039323530' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 COMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
003981
003982     IF CT-COMPANY-CD = PI-COMPANY-CD AND
003983        CT-TABLE      = COMM-TABLE    AND
003984        CT-BEN-TYPE   = COMM-LF-AH
003985        PERFORM 6460-CHECK-MAX THRU 6460-EXIT
003986        GO TO 6459-EXIT
003987     END-IF
003988     .
003989 6450-NOT-FOUND.
003990     MOVE ER-2176                TO EMI-ERROR.
003991     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
003992     MOVE 1  TO WS-COMM-ERROR-SW.
003993
003994 6459-EXIT.
003995      EXIT.
003996
003997 6460-CHECK-MAX.
003998
003999     
      * EXEC CICS STARTBR
004000*         DATASET   (COMM-FILE-ID)
004001*         RIDFLD    (COMM-KEY)
004002*         RESP      (WS-RESPONSE)
004003*    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00009274' TO DFHEIV0
           MOVE X'262C20202020202020202047' &
                X'202020202020202020202620' &
                X'204E233030303039323734' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-FILE-ID, 
                 COMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004004
004005     MOVE SPACES          TO WS-STATE-MAX-SW
004006
004007     IF RESP-NORMAL
004008        MOVE LOW-VALUES          TO COMM-FILLER
004009        MOVE COMM-KEY            TO WS-HOLD-COMM-KEY
004010        MOVE ZEROS               TO WS-TABLE-MAX
004011        PERFORM UNTIL
004012              (NOT RESP-NORMAL) OR
004013              (COMM-KEY (1:5) NOT = WS-HOLD-COMM-KEY (1:5))
004014            
      * EXEC CICS READNEXT
004015*             DATASET   (COMM-FILE-ID)
004016*             SET       (ADDRESS OF COMM-TABLE-RECORD)
004017*             RIDFLD    (COMM-KEY)
004018*             RESP      (WS-RESPONSE)
004019*           END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00009289' TO DFHEIV0
           MOVE X'262E53202020202020202020' &
                X'202020202020202020202920' &
                X'204E233030303039323839' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 COMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-TABLE-RECORD TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004020            IF (RESP-NORMAL) AND
004021               (COMM-KEY (1:5) = WS-HOLD-COMM-KEY (1:5))
004022               PERFORM VARYING WS-NDX FROM +1 BY +1 UNTIL
004023                   (WS-NDX > +27)
004024                   IF CT-RT (WS-NDX) > WS-TABLE-MAX
004025                      MOVE CT-RT (WS-NDX)
004026                                 TO WS-TABLE-MAX
004027                   END-IF
004028               END-PERFORM
004029            END-IF
004030        END-PERFORM
004031        
      * EXEC CICS ENDBR
004032*            DATASET   (COMM-FILE-ID)
004033*       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009306' TO DFHEIV0
           MOVE X'263220202020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039333036' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMM-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004034     END-IF
004035
004036     .
004037 6460-EXIT.
004038      EXIT.
004039
004040
004041 6500-READ-ACCT.
004042     
      * EXEC CICS READ
004043*        GTEQ
004044*        DATASET   (ACCT-FILE-ID)
004045*        SET       (ADDRESS OF ACCOUNT-MASTER)
004046*        RIDFLD    (PI-ACCT-KEY)
004047*    END-EXEC.
      *    MOVE '&"S        G          (   #00009317' TO DFHEIV0
           MOVE X'262253202020202020202047' &
                X'202020202020202020202820' &
                X'2020233030303039333137' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004048
004049 6599-EXIT.
004050      EXIT.
004051 EJECT
004052 6600-UPDATE-MAINT-DT.
004053     MOVE SPACES                 TO ELCNTL-KEY.
004054
004055     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
004056     MOVE '1'                    TO CNTL-REC-TYPE.
004057     MOVE +0                     TO CNTL-SEQ-NO.
004058
004059     
      * EXEC CICS HANDLE CONDITION
004060*        NOTFND   (6699-EXIT)
004061*    END-EXEC.
      *    MOVE '"$I                   ! / #00009334' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'2F20233030303039333334' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004062
004063     
      * EXEC CICS READ
004064*        UPDATE
004065*        DATASET   (CNTL-FILE-ID)
004066*        SET       (ADDRESS OF CONTROL-FILE)
004067*        RIDFLD    (ELCNTL-KEY)
004068*    END-EXEC.
      *    MOVE '&"S        EU         (   #00009338' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303039333338' TO DFHEIV0
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
           
004069
004070     MOVE BIN-CURRENT-SAVE       TO CF-ACCOUNT-MSTR-MAINT-DT.
004071
004072     
      * EXEC CICS REWRITE
004073*        DATASET   (CNTL-FILE-ID)
004074*        FROM      (CONTROL-FILE)
004075*    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00009347' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039333437' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004076
004077 6699-EXIT.
004078      EXIT.
004079 EJECT
004080 6700-BUILD-COMM-WORK.
004081     IF NOT PI-GA-BILLING
004082         GO TO 6799-EXIT.
004083
004084     
      * EXEC CICS GETMAIN
004085*         LENGTH   (260)
004086*         SET      (ADDRESS OF COMMISSION-WORK-AREA)
004087*         INITIMG  (GETMAIN-SPACE)
004088*     END-EXEC.
           MOVE 260
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00009359' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' &
                X'202020202020202020202420' &
                X'2020233030303039333539' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMMISSION-WORK-AREA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004089     SET LCP-WS-ADDR-PNTR TO ADDRESS OF COMMISSION-WORK-AREA
004090
004091     MOVE LCP-WS-ADDR-COMP TO PI-COMM-POINTER.
004092     MOVE SPACES             TO COMMISSION-WORK-AREA.
004093     MOVE 'X'                TO COMMISSION-GETMAIN-SW.
004094
004095     IF PI-MAINT = 'A'
004096         GO TO 6799-EXIT.
004097
004098     MOVE AM-COMM-STRUCTURE TO COMMISSION-WORK-AREA.
004099     MOVE +1                TO AXRF-SUB.
004100
004101 6710-AXRF-LOOP.
004102     MOVE SPACES         TO AGENT-FIND.
004103     MOVE ERGXRF-MAX-LEN TO ERGXRF-REC-LEN.
004104
004105     PERFORM 6920-READ-AXRF-FILE THRU 6929-EXIT.
004106
004107     IF AGENT-FIND = SPACES
004108        MOVE GX-LAST-BILL-DT (GX-AGENT-SUB) TO
004109             WK-GA-BILL-DT (AXRF-SUB)
004110     ELSE
004111        MOVE LOW-VALUES TO WK-GA-BILL-DT (AXRF-SUB).
004112
004113     ADD +1 TO AXRF-SUB.
004114
004115     IF AXRF-SUB NOT = +11
004116        GO TO 6710-AXRF-LOOP.
004117
004118 6799-EXIT.
004119     EXIT.
004120 EJECT
004121 6800-COMPANY-REC-READ.
004122     MOVE SPACES                 TO ELCNTL-KEY.
004123     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
004124     MOVE '1'                    TO CNTL-REC-TYPE.
004125     MOVE +0                     TO CNTL-SEQ-NO.
004126     
      * EXEC CICS HANDLE CONDITION
004127*        NOTFND   (6880-NO-COMP)
004128*    END-EXEC.
      *    MOVE '"$I                   ! 0 #00009401' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3020233030303039343031' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004129
004130     
      * EXEC CICS READ
004131*        DATASET   (CNTL-FILE-ID)
004132*        SET       (ADDRESS OF CONTROL-FILE)
004133*        RIDFLD    (ELCNTL-KEY)
004134*    END-EXEC.
      *    MOVE '&"S        E          (   #00009405' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039343035' TO DFHEIV0
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
           
004135
004136     IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES
004137         MOVE ER-2572               TO EMI-ERROR
004138         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004139
004140     GO TO 6899-EXIT.
004141
004142 6880-NO-COMP.
004143     MOVE ER-0002                TO EMI-ERROR.
004144     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004145
004146 6899-EXIT.
004147     EXIT.
004148 EJECT
004149 6900-READ-AND-CHECK-AXRF.
004150     IF NOT PI-GA-BILLING
004151        GO TO 6999-EXIT.
004152
004153     MOVE PI-COMM-POINTER TO LCP-WS-ADDR-COMP
004154     SET ADDRESS OF COMMISSION-WORK-AREA TO LCP-WS-ADDR-PNTR.
004155
004156 6905-AGENT-LEVEL-LOOP.
004157     MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN.
004158     MOVE SPACES TO AGENT-FIND.
004159     IF ((AM-AGT (AXRF-SUB) NOT = WK-AM-AGT (AXRF-SUB))
004160      OR
004161        (AM-COM-TYP (AXRF-SUB) NOT = WK-AM-TYPE (AXRF-SUB)))
004162      AND
004163        ((AM-COM-TYP (AXRF-SUB) = 'O' OR 'P' OR 'G'
004164                             OR 'B' OR 'I'
004165                             OR 'K' OR 'L' OR 'J' OR 'M'
004166                             OR 'S' OR 'A' OR 'N')
004167                            OR
004168        (WK-AM-TYPE (AXRF-SUB) = 'O' OR 'P' OR 'G' OR
004169                'B' OR 'I' OR 'K' OR 'L' OR 'J' OR 'M'
004170                                  OR 'S' OR 'A' OR 'N'))
004171            PERFORM 6920-READ-AXRF-FILE THRU 6929-EXIT
004172            IF AGENT-RECORD-NOT-FOUND
004173               PERFORM 6950-ADD-AGENT-RECORD THRU 6959-EXIT
004174           ELSE
004175            IF ACCT-RECORD-NOT-FOUND OR ACCT-LEVEL-DIFFERENT
004176               PERFORM 6970-REWRITE-AGENT-RECORD THRU 6979-EXIT
004177           ELSE
004178        IF (AM-COM-TYP (AXRF-SUB) NOT = 'O' AND 'P' AND 'G'
004179              AND 'B' AND 'I' AND 'S'
004180              AND 'K' AND 'L' AND 'J' AND 'M' AND 'A' AND 'N') OR
004181           (AM-AGT (AXRF-SUB) NOT =
004182                   WK-AM-AGT (AXRF-SUB))
004183           PERFORM 7000-DELETE-AXRF-REC THRU 7999-EXIT
004184           GO TO 6910-INC-SUB.
004185
004186     MOVE SPACES TO AGENT-FIND.
004187     IF (AM-AGT (AXRF-SUB) NOT = WK-AM-AGT (AXRF-SUB)) AND
004188        (WK-AM-AGT (AXRF-SUB) NOT = LOW-VALUES AND
004189                               SPACES AND ZEROS)
004190          MOVE WK-AM-AGT (AXRF-SUB)   TO WS-AXRF-AGENT-NO
004191          MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN
004192          PERFORM 6921-READ-AXRF THRU 6929-EXIT
004193          IF AGENT-FIND = SPACES
004194              PERFORM 7000-DELETE-AXRF-REC THRU 7999-EXIT.
004195
004196 6910-INC-SUB.
004197     ADD +1                      TO AXRF-SUB.
004198     IF AXRF-SUB NOT = +11
004199        GO TO 6905-AGENT-LEVEL-LOOP.
004200
004201     GO TO 6999-EXIT.
004202
004203 6920-READ-AXRF-FILE.
004204*    IF GETMAIN-SW = 'X'
004205*       GO TO 6920-BUILD-KEY.
004206*
004207*    EXEC CICS GETMAIN
004208*         LENGTH   (ERGXRF-REC-LEN)
004209*         SET      (ADDRESS OF AGENT-CROSS-REFERENCE)
004210*         INITIMG  (GETMAIN-SPACE)
004211*    END-EXEC.
004212*
004213*    MOVE 'X' TO GETMAIN-SW.
004214
004215 6920-BUILD-KEY.
004216     MOVE PI-COMPANY-CD          TO WS-AXRF-COMPANY-CD.
004217
004218     IF PI-ZERO-CARRIER
004219       OR PI-ZERO-CAR-GROUP
004220         MOVE ZERO               TO WS-AXRF-CARRIER
004221     ELSE
004222         MOVE AM-CARRIER         TO WS-AXRF-CARRIER.
004223
004224     IF PI-ZERO-GROUPING
004225       OR PI-ZERO-CAR-GROUP
004226         MOVE ZEROS              TO WS-AXRF-GROUPING
004227     ELSE
004228         MOVE AM-GROUPING        TO WS-AXRF-GROUPING.
004229
004230     IF AM-AGT (AXRF-SUB) NOT = SPACES AND ZEROS AND
004231                                              LOW-VALUES
004232        MOVE AM-AGT (AXRF-SUB) TO WS-AXRF-AGENT-NO
004233     ELSE
004234     IF WK-AM-AGT (AXRF-SUB) NOT = SPACES AND ZEROS AND
004235                                              LOW-VALUES
004236        MOVE WK-AM-AGT (AXRF-SUB) TO WS-AXRF-AGENT-NO
004237     ELSE
004238        MOVE 'X' TO AGENT-FIND
004239        GO TO 6929-EXIT.
004240
004241     MOVE +1 TO GX-AGENT-SUB.
004242
004243 6921-READ-AXRF.
004244     
      * EXEC CICS HANDLE CONDITION
004245*         ENDFILE     (6925-NOT-FIND)
004246*         NOTFND      (6925-NOT-FIND)
004247*    END-EXEC.
      *    MOVE '"$''I                  ! 1 #00009519' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'3120233030303039353139' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004248
004249     
      * EXEC CICS READ
004250*        DATASET   (ERGXRF-FILE-ID)
004251*        LENGTH    (ERGXRF-REC-LEN)
004252*        INTO      (AGENT-CROSS-REFERENCE)
004253*        RIDFLD    (WS-AXRF-KEY)
004254*        EQUAL
004255*    END-EXEC.
      *    MOVE '&"IL       E          (   #00009524' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039353234' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 WS-AXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004256
004257     MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
004258
004259     PERFORM 6921-FIND-ACCT-LOOP VARYING GX-AGENT-SUB
004260             FROM +1 BY +1 UNTIL
004261         (GX-AGENT-SUB GREATER THAN GX-AGENT-POINTER-CNT) OR
004262         (GX-AM-ACCOUNT (GX-AGENT-SUB) = AM-ACCOUNT      AND
004263         GX-AM-EXPIRATION-DT (GX-AGENT-SUB) = AM-EXPIRATION-DT
004264         AND
004265         GX-AM-EFF-DT (GX-AGENT-SUB)   = AM-EFFECTIVE-DT AND
004266         GX-AM-CARRIER (GX-AGENT-SUB)  = AM-CARRIER      AND
004267         GX-AM-GROUPING (GX-AGENT-SUB) = AM-GROUPING     AND
004268         GX-AM-STATE (GX-AGENT-SUB)    = AM-STATE).
004269
004270     IF GX-AGENT-SUB GREATER GX-AGENT-POINTER-CNT
004271        MOVE 'Y' TO AGENT-FIND
004272        GO TO 6929-EXIT.
004273
004274     IF GX-AM-LEVEL-NO (GX-AGENT-SUB) NOT = AXRF-SUB
004275        MOVE 'Z' TO AGENT-FIND.
004276
004277     GO TO 6929-EXIT.
004278
004279 6921-FIND-ACCT-LOOP.
004280***  DUMMY PARAGRAPH  ***
004281
004282 6925-NOT-FIND.
004283     MOVE 'X' TO AGENT-FIND.
004284
004285 6929-EXIT.
004286     EXIT.
004287 EJECT
004288 6950-ADD-AGENT-RECORD.
004289     IF AM-AGT (AXRF-SUB) = SPACES OR ZEROS
004290        GO TO 6959-EXIT.
004291
004292     MOVE +109                   TO ERGXRF-REC-LEN.
004293
004294     MOVE +1                     TO GX-AGENT-POINTER-CNT.
004295     MOVE 'GX'                   TO GX-RECORD-ID.
004296     MOVE AM-COMPANY-CD          TO GX-COMPANY-CD.
004297
004298     IF PI-ZERO-CARRIER
004299       OR PI-ZERO-CAR-GROUP
004300         MOVE ZERO               TO GX-CARRIER
004301     ELSE
004302         MOVE AM-CARRIER         TO GX-CARRIER.
004303
004304     IF PI-ZERO-GROUPING
004305       OR PI-ZERO-CAR-GROUP
004306         MOVE ZERO               TO GX-GROUPING
004307     ELSE
004308         MOVE AM-GROUPING        TO GX-GROUPING.
004309
004310     MOVE AM-AGT (AXRF-SUB)      TO GX-AGENT-NO.
004311
004312     MOVE AM-CARRIER             TO GX-AM-CARRIER (1).
004313     MOVE AM-GROUPING            TO GX-AM-GROUPING (1).
004314     MOVE AM-STATE               TO GX-AM-STATE (1).
004315     MOVE AM-ACCOUNT             TO GX-AM-ACCOUNT (1).
004316     MOVE AM-EXPIRATION-DT       TO GX-AM-EXPIRATION-DT (1).
004317     MOVE AM-EFFECTIVE-DT        TO GX-AM-EFF-DT (1).
004318     MOVE AXRF-SUB               TO GX-AM-LEVEL-NO (1).
004319     MOVE LOW-VALUES             TO GX-LAST-BILL-DT (1).
004320
004321     MOVE BIN-CURRENT-SAVE       TO GX-LAST-MAINT-DT.
004322     MOVE EIBTIME                TO GX-LAST-MAINT-HHMMSS.
004323     MOVE PI-PROCESSOR-ID        TO GX-LAST-MAINT-USER.
004324
004325     
      * EXEC CICS WRITE
004326*        FROM      (AGENT-CROSS-REFERENCE)
004327*        LENGTH    (ERGXRF-REC-LEN)
004328*        RIDFLD    (GX-CONTROL-PRIMARY)
004329*        DATASET   (ERGXRF-FILE-ID)
004330*    END-EXEC.
      *    MOVE '&$ L                  ''   #00009600' TO DFHEIV0
           MOVE X'2624204C2020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039363030' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 GX-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004331
004332 6959-EXIT.
004333      EXIT.
004334 EJECT
004335 6970-REWRITE-AGENT-RECORD.
004336     MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN.
004337
004338     
      * EXEC CICS READ
004339*        DATASET   (ERGXRF-FILE-ID)
004340*        INTO      (AGENT-CROSS-REFERENCE)
004341*        LENGTH    (ERGXRF-REC-LEN)
004342*        RIDFLD    (WS-AXRF-KEY)
004343*        EQUAL
004344*        UPDATE
004345*    END-EXEC.
      *    MOVE '&"IL       EU         (   #00009613' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303039363133' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 WS-AXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004346
004347     MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
004348
004349     IF ACCT-LEVEL-DIFFERENT
004350        GO TO 6975-UPDATE-LEVEL-NO.
004351
004352     IF GX-AGENT-POINTER-CNT = +1006
004353         GO TO 6979-EXIT.
004354
004355     MOVE +1 TO FIND-AGENT-SUB1.
004356
004357 6972-FIND-REC-LOCATION.
004358     MOVE GX-AGENT-POINTER (FIND-AGENT-SUB1) TO GX-REST-OF-KEY.
004359     MOVE AM-COMPANY-CD TO GX-AM-COMPANY-CD.
004360
004361     IF (GX-AM-CONTROL-PRIMARY GREATER AM-CONTROL-PRIMARY)  OR
004362        (FIND-AGENT-SUB1 GREATER GX-AGENT-POINTER-CNT)
004363        NEXT SENTENCE
004364     ELSE
004365        ADD +1 TO FIND-AGENT-SUB1
004366        GO TO 6972-FIND-REC-LOCATION.
004367
004368     MOVE GX-AGENT-POINTER-CNT TO FIND-AGENT-SUB2
004369                                  FIND-AGENT-SUB3.
004370     ADD +1 TO FIND-AGENT-SUB3.
004371
004372     IF FIND-AGENT-SUB1 GREATER GX-AGENT-POINTER-CNT
004373        NEXT SENTENCE
004374     ELSE
004375        PERFORM 6977-BUMP-AXRF-REC VARYING FIND-AGENT-SUB2
004376        FROM FIND-AGENT-SUB2 BY -1 UNTIL
004377        FIND-AGENT-SUB3 = FIND-AGENT-SUB1.
004378
004379     ADD ERGXRF-INC-LEN          TO ERGXRF-REC-LEN.
004380     ADD  +1                     TO GX-AGENT-POINTER-CNT.
004381
004382     MOVE AM-CARRIER  TO GX-AM-CARRIER (FIND-AGENT-SUB3).
004383     MOVE AM-GROUPING TO GX-AM-GROUPING (FIND-AGENT-SUB3).
004384
004385     MOVE AM-STATE    TO GX-AM-STATE (FIND-AGENT-SUB3).
004386     MOVE AM-ACCOUNT  TO GX-AM-ACCOUNT (FIND-AGENT-SUB3).
004387     MOVE AM-EXPIRATION-DT TO
004388                  GX-AM-EXPIRATION-DT (FIND-AGENT-SUB3).
004389     MOVE AM-EFFECTIVE-DT  TO
004390                  GX-AM-EFF-DT (FIND-AGENT-SUB3).
004391     MOVE AXRF-SUB    TO GX-AM-LEVEL-NO (FIND-AGENT-SUB3).
004392     MOVE LOW-VALUES  TO GX-LAST-BILL-DT (FIND-AGENT-SUB3).
004393
004394 6975-UPDATE-LEVEL-NO.
004395     MOVE AXRF-SUB TO GX-AM-LEVEL-NO (GX-AGENT-SUB).
004396     MOVE BIN-CURRENT-SAVE       TO GX-LAST-MAINT-DT.
004397     MOVE EIBTIME                TO GX-LAST-MAINT-HHMMSS.
004398     MOVE PI-PROCESSOR-ID        TO GX-LAST-MAINT-USER.
004399     compute ergxrf-rec-len = (gx-agent-pointer-cnt * +31) + +78
004400     if ergxrf-rec-len > +31264
004401        move +31264               to ergxrf-rec-len
004402        move +1006                to gx-agent-pointer-cnt
004403     end-if
004404
004405     
      * EXEC CICS REWRITE
004406*        FROM      (AGENT-CROSS-REFERENCE)
004407*        LENGTH    (ERGXRF-REC-LEN)
004408*        DATASET   (ERGXRF-FILE-ID)
004409*    END-EXEC.
      *    MOVE '&& L                  %   #00009680' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039363830' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004410
004411     GO TO 6979-EXIT.
004412
004413 6977-BUMP-AXRF-REC.
004414     MOVE GX-AGENT-POINTER (FIND-AGENT-SUB2) TO
004415          GX-AGENT-POINTER (FIND-AGENT-SUB3).
004416
004417     SUBTRACT +1 FROM FIND-AGENT-SUB3.
004418
004419 6979-EXIT.
004420     EXIT.
004421
004422 6999-EXIT.
004423     EXIT.
004424 EJECT
004425 7000-DELETE-AXRF-REC.
004426     MOVE ERGXRF-MAX-LEN         TO ERGXRF-REC-LEN.
004427
004428     
      * EXEC CICS HANDLE CONDITION
004429*         ENDFILE     (7999-EXIT)
004430*         NOTFND      (7999-EXIT)
004431*    END-EXEC.
      *    MOVE '"$''I                  ! 2 #00009703' TO DFHEIV0
           MOVE X'222427492020202020202020' &
                X'202020202020202020202120' &
                X'3220233030303039373033' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004432
004433     
      * EXEC CICS READ
004434*        DATASET   (ERGXRF-FILE-ID)
004435*        INTO      (AGENT-CROSS-REFERENCE)
004436*        LENGTH    (ERGXRF-REC-LEN)
004437*        RIDFLD    (WS-AXRF-KEY)
004438*        EQUAL
004439*        UPDATE
004440*    END-EXEC.
      *    MOVE '&"IL       EU         (   #00009708' TO DFHEIV0
           MOVE X'2622494C2020202020202045' &
                X'552020202020202020202820' &
                X'2020233030303039373038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 WS-AXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004441
004442     MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
004443
004444     IF GX-AGENT-SUB = +0
004445        PERFORM 6921-FIND-ACCT-LOOP VARYING GX-AGENT-SUB
004446        FROM +1 BY +1 UNTIL
004447         (GX-AGENT-SUB GREATER GX-AGENT-POINTER-CNT) OR
004448         (GX-AM-ACCOUNT (GX-AGENT-SUB) = AM-ACCOUNT  AND
004449         GX-AM-EXPIRATION-DT (GX-AGENT-SUB) = AM-EXPIRATION-DT
004450         AND
004451         GX-AM-EFF-DT (GX-AGENT-SUB)   = AM-EFFECTIVE-DT AND
004452         GX-AM-CARRIER (GX-AGENT-SUB)  = AM-CARRIER      AND
004453         GX-AM-GROUPING (GX-AGENT-SUB) = AM-GROUPING     AND
004454         GX-AM-STATE (GX-AGENT-SUB)    = AM-STATE).
004455
004456     IF GX-AGENT-SUB GREATER GX-AGENT-POINTER-CNT
004457        GO TO 7999-EXIT.
004458
004459     MOVE GX-AGENT-SUB TO FIND-AGENT-SUB1.
004460     PERFORM 7010-BUMP-RECORDS VARYING GX-AGENT-SUB
004461                                  FROM GX-AGENT-SUB BY +1 UNTIL
004462                            GX-AGENT-SUB = GX-AGENT-POINTER-CNT.
004463     SUBTRACT ERGXRF-INC-LEN FROM ERGXRF-REC-LEN.
004464     SUBTRACT +1             FROM GX-AGENT-POINTER-CNT.
004465     GO TO 7020-REWRITE-AXRF-RECORD.
004466
004467 7010-BUMP-RECORDS.
004468     ADD +1 TO FIND-AGENT-SUB1.
004469     MOVE GX-AGENT-POINTER (FIND-AGENT-SUB1) TO
004470          GX-AGENT-POINTER (GX-AGENT-SUB).
004471
004472 7020-REWRITE-AXRF-RECORD.
004473     IF GX-AGENT-POINTER-CNT = +0
004474        
      * EXEC CICS DELETE
004475*            DATASET   (ERGXRF-FILE-ID)
004476*       END-EXEC
      *    MOVE '&(                    &   #00009749' TO DFHEIV0
           MOVE X'262820202020202020202020' &
                X'202020202020202020202620' &
                X'2020233030303039373439' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 WS-AXRF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
004477     ELSE
004478        MOVE BIN-CURRENT-SAVE       TO GX-LAST-MAINT-DT
004479        MOVE EIBTIME                TO GX-LAST-MAINT-HHMMSS
004480        MOVE PI-PROCESSOR-ID        TO GX-LAST-MAINT-USER
004481        compute ergxrf-rec-len =
004482            (gx-agent-pointer-cnt * +31) + +78
004483        if ergxrf-rec-len > +31264
004484           move +31264               to ergxrf-rec-len
004485           move +1006                to gx-agent-pointer-cnt
004486        end-if
004487
004488        
      * EXEC CICS REWRITE
004489*            FROM      (AGENT-CROSS-REFERENCE)
004490*            LENGTH    (ERGXRF-REC-LEN)
004491*            DATASET   (ERGXRF-FILE-ID)
004492*       END-EXEC.
      *    MOVE '&& L                  %   #00009763' TO DFHEIV0
           MOVE X'2626204C2020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039373633' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERGXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 ERGXRF-REC-LEN, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004493
004494 7999-EXIT.
004495     EXIT.
004496
004497*************************************
004498 8000-STATE-REC-READ.
004499     MOVE SPACES                 TO ELCNTL-KEY.
004500     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
004501     MOVE '3'                    TO CNTL-REC-TYPE.
004502     MOVE PI-ACCT-STATE          TO CNTL-ACCESS.
004503     MOVE +0                     TO CNTL-SEQ-NO.
004504     
      * EXEC CICS HANDLE CONDITION
004505*        NOTFND  (8010-EXIT)
004506*    END-EXEC.
      *    MOVE '"$I                   ! 3 #00009779' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3320233030303039373739' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004507
004508     
      * EXEC CICS READ
004509*        DATASET   (CNTL-FILE-ID)
004510*        SET       (ADDRESS OF CONTROL-FILE)
004511*        RIDFLD    (ELCNTL-KEY)
004512*    END-EXEC.
      *    MOVE '&"S        E          (   #00009783' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039373833' TO DFHEIV0
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
           
004513
004514     MOVE CF-ST-COMMISSION-CAPS   TO WS-ST-COMMISSION-CAPS
004515     MOVE CF-ST-GA-COMMISSION-CAPS
004516                                  TO WS-ST-GA-COMMISSION-CAPS
004517     MOVE CF-ST-TOT-COMMISSION-CAPS
004518                                  TO WS-ST-TOT-COMMISSION-CAPS
004519     MOVE CF-COMM-CAP-LIMIT-TO    TO WS-COMM-CAP-LIMIT-TO.
004520
004521 8010-EXIT.
004522      EXIT.
004523
004524 8050-USER-REC-READ.
004525     MOVE SPACES                 TO ELCNTL-KEY.
004526     MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
004527     MOVE '2'                    TO CNTL-REC-TYPE.
004528     MOVE CSRI                   TO CNTL-ACCESS.
004529     MOVE +0                     TO CNTL-SEQ-NO.
004530     
      * EXEC CICS HANDLE CONDITION
004531*        NOTFND  (8055-NOTFND)
004532*    END-EXEC.
      *    MOVE '"$I                   ! 4 #00009805' TO DFHEIV0
           MOVE X'222449202020202020202020' &
                X'202020202020202020202120' &
                X'3420233030303039383035' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004533
004534     
      * EXEC CICS READ
004535*        DATASET   (CNTL-FILE-ID)
004536*        SET       (ADDRESS OF CONTROL-FILE)
004537*        RIDFLD    (ELCNTL-KEY)
004538*    END-EXEC.
      *    MOVE '&"S        E          (   #00009809' TO DFHEIV0
           MOVE X'262253202020202020202045' &
                X'202020202020202020202820' &
                X'2020233030303039383039' TO DFHEIV0
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
           
004539
004540     GO TO 8060-EXIT.
004541
004542 8055-NOTFND.
004543     MOVE ER-1883             TO EMI-ERROR.
004544     MOVE -1                  TO CSRL.
004545     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
004546
004547 8060-EXIT.
004548      EXIT.
004549
004550*************************************
004551
004552 8100-SEND-INITIAL-MAP.
004553     MOVE SAVE-DATE              TO DATEO.
004554     MOVE EIBTIME                TO TIME-IN.
004555     MOVE TIME-OUT               TO TIMEO.
004556     MOVE PI-COMPANY-ID          TO CMPNYIDO.
004557     MOVE PI-PROCESSOR-ID        TO USERIDO.
004558     MOVE -1                     TO PFENTERL.
004559     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
004560
004561     MOVE PI-AH-OVERRIDE-L6      TO AHHEADO.
004562
004563*    IF PI-COMPANY-ID = 'HAN' OR 'LGX' OR 'BNE'
004564*       NEXT SENTENCE
004565*    ELSE
004566*       MOVE AL-SADOF            TO PSIHEADA
004567*                                   PSILFTA
004568*                                   PSILFCA
004569*                                   PSIAHTA
004570*                                   PSIAHCA
004571*                                   PSILFFA
004572*                                   PSIAHFA.
004573
004574*    IF PI-COMPANY-ID = 'DMD'
004575*         MOVE AL-SANOF          TO KEY10A.
004576
004577     IF PI-COMPANY-ID = 'DCC' or 'VPP'
004578        CONTINUE
004579     ELSE
004580        MOVE AL-SADOF            TO CTPLABLA
004581                                    CLPTOLPA
004582                                    LCLABLA
004583                                    LCOMMA
004584                                    MAXFEEHA
004585                                    CLPSTHA
004586                                    PRODCDHA
004587     END-IF
004588
004589     IF PI-COMPANY-ID = 'NCL' OR 'LGX'
004590        MOVE -1                  TO CSRL.
004591
004592     IF  CLAIM-SESSION
004593         MOVE SPACES             TO PF1PF2O
004594         MOVE AL-SANOF           TO PF1PF2A.
004595
004596     
      * EXEC CICS SEND
004597*        MAP      (MAP-NAME)
004598*        MAPSET   (MAPSET-NAME)
004599*        FROM     (EL6501AO)
004600*        ERASE
004601*        CURSOR
004602*    END-EXEC.
           MOVE LENGTH OF
            EL6501AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009871' TO DFHEIV0
           MOVE X'382420202020204354202045' &
                X'2020202048204C2046202C20' &
                X'2020233030303039383731' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6501AO, 
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
           
004603
004604     GO TO 9100-RETURN-TRAN.
004605
004606 8200-SEND-DATAONLY.
004607     MOVE SAVE-DATE              TO DATEO.
004608     MOVE EIBTIME                TO TIME-IN.
004609     MOVE TIME-OUT               TO TIMEO.
004610     MOVE PI-COMPANY-ID          TO CMPNYIDO.
004611     MOVE PI-PROCESSOR-ID        TO USERIDO.
004612     MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
004613
004614     MOVE PI-AH-OVERRIDE-L6      TO AHHEADO.
004615
004616     IF PI-COMPANY-ID = 'DCC' or 'VPP'
004617        CONTINUE
004618     ELSE
004619        MOVE AL-SADOF            TO CTPLABLA
004620                                    CLPTOLPA
004621                                    LCLABLA
004622                                    LCOMMA
004623                                    MAXFEEHA
004624                                    CLPSTHA
004625                                    PRODCDHA
004626     END-IF
004627
004628     IF  CLAIM-SESSION
004629         MOVE SPACES             TO PF1PF2O
004630         MOVE AL-SANOF           TO PF1PF2A.
004631
004632     
      * EXEC CICS SEND
004633*        MAP      (MAP-NAME)
004634*        MAPSET   (MAPSET-NAME)
004635*        FROM     (EL6501AO)
004636*        DATAONLY
004637*        CURSOR
004638*    END-EXEC.
           MOVE LENGTH OF
            EL6501AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009907' TO DFHEIV0
           MOVE X'382444202020204354202020' &
                X'2020202048204C2046202C20' &
                X'2020233030303039393037' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6501AO, 
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
           
004639
004640     GO TO 9100-RETURN-TRAN.
004641
004642 8300-SEND-TEXT.
004643     
      * EXEC CICS SEND TEXT
004644*        FROM     (LOGOFF-TEXT)
004645*        LENGTH   (LOGOFF-LENGTH)
004646*        ERASE
004647*        FREEKB
004648*    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009918' TO DFHEIV0
           MOVE X'382620202020202054202045' &
                X'204620204820202046202D20' &
                X'2020233030303039393138' TO DFHEIV0
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
           
004649
004650     
      * EXEC CICS RETURN
004651*    END-EXEC.
      *    MOVE '.(                    ''   #00009925' TO DFHEIV0
           MOVE X'2E2820202020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039393235' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004652
004653 8600-DEEDIT.
004654     
      * EXEC CICS BIF DEEDIT
004655*         FIELD(DEEDIT-FIELD)
004656*         LENGTH(15)
004657*     END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009929' TO DFHEIV0
           MOVE X'40224C202020202020202020' &
                X'202020202020202020202320' &
                X'2020233030303039393239' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004658
004659 8800-UNAUTHORIZED-ACCESS.
004660     MOVE UNACCESS-MSG           TO LOGOFF-MSG.
004661     GO TO 8300-SEND-TEXT.
004662
004663 8810-PF23.
004664     MOVE EIBAID                 TO PI-ENTRY-CD-1.
004665     MOVE XCTL-005               TO PGM-NAME.
004666     GO TO 9300-XCTL.
004667
004668 9100-RETURN-TRAN.
004669     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
004670     MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
004671     
      * EXEC CICS RETURN
004672*        TRANSID    (TRANS-ID)
004673*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
004674*        LENGTH     (WS-COMM-LENGTH)
004675*    END-EXEC.
      *    MOVE '.(CT                  ''   #00009946' TO DFHEIV0
           MOVE X'2E2843542020202020202020' &
                X'202020202020202020202720' &
                X'2020233030303039393436' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004676
004677 9200-RETURN-MAIN-MENU.
004678     IF PI-COMPANY-ID = 'AIG' OR 'AUK'
004679         MOVE XCTL-126           TO  PGM-NAME
004680     ELSE
004681         MOVE XCTL-626           TO  PGM-NAME.
004682
004683     GO TO 9300-XCTL.
004684
004685 9300-XCTL.
004686     
      * EXEC CICS XCTL
004687*        PROGRAM    (PGM-NAME)
004688*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
004689*        LENGTH     (WS-COMM-LENGTH)
004690*    END-EXEC.
      *    MOVE '.$C                   %   #00009961' TO DFHEIV0
           MOVE X'2E2443202020202020202020' &
                X'202020202020202020202520' &
                X'2020233030303039393631' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004691
004692 9400-CLEAR.
004693     IF PI-GA-BILLING
004694        IF GETMAIN-ACQUIRED
004695           MOVE PI-COMM-POINTER TO LCP-WS-ADDR-COMP
004696           SET ADDRESS OF COMMISSION-WORK-AREA TO LCP-WS-ADDR-PNTR
004697           
      * EXEC CICS FREEMAIN
004698*               DATA     (COMMISSION-WORK-AREA)
004699*          END-EXEC.
      *    MOVE ',$D                   "   #00009972' TO DFHEIV0
           MOVE X'2C2444202020202020202020' &
                X'202020202020202020202220' &
                X'2020233030303039393732' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMMISSION-WORK-AREA
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004700
004701     MOVE SPACES                 TO PI-SV-MAINT.
004702     MOVE XCTL-650               TO PGM-NAME.
004703     MOVE XCTL-650               TO PI-RETURN-TO-PROGRAM.
004704     GO TO 9300-XCTL.
004705
004706 9500-PF12.
004707     MOVE XCTL-010               TO PGM-NAME.
004708     GO TO 9300-XCTL.
004709
004710 9600-PGMID-ERROR.
004711     
      * EXEC CICS HANDLE CONDITION
004712*        PGMIDERR    (8300-SEND-TEXT)
004713*    END-EXEC.
      *    MOVE '"$L                   ! 5 #00009986' TO DFHEIV0
           MOVE X'22244C202020202020202020' &
                X'202020202020202020202120' &
                X'3520233030303039393836' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004714
004715     MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
004716     MOVE ' '                    TO PI-ENTRY-CD-1.
004717     MOVE XCTL-005               TO PGM-NAME.
004718     MOVE PGM-NAME               TO LOGOFF-PGM.
004719     MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
004720     GO TO 9300-XCTL.
004721
004722 9700-DATE-LINK.
004723     MOVE LINK-ELDATCV           TO PGM-NAME.
004724     
      * EXEC CICS LINK
004725*        PROGRAM    (PGM-NAME)
004726*        COMMAREA   (DATE-CONVERSION-DATA)
004727*        LENGTH     (DC-COMM-LENGTH)
004728*    END-EXEC.
      *    MOVE '."C                   (   #00009999' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303039393939' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004729
004730 9900-ERROR-FORMAT.
004731     IF NOT EMI-ERRORS-COMPLETE
004732         MOVE LINK-001           TO PGM-NAME
004733         
      * EXEC CICS LINK
004734*            PROGRAM    (PGM-NAME)
004735*            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
004736*            LENGTH     (EMI-COMM-LENGTH)
004737*        END-EXEC.
      *    MOVE '."C                   (   #00010008' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303130303038' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004738
004739 9900-EXIT.
004740     EXIT.
004741
004742 9990-ABEND.
004743     MOVE LINK-004               TO PGM-NAME.
004744     MOVE DFHEIBLK               TO EMI-LINE1.
004745     
      * EXEC CICS LINK
004746*        PROGRAM   (PGM-NAME)
004747*        COMMAREA  (EMI-LINE1)
004748*        LENGTH    (72)
004749*    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00010020' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303130303230' TO DFHEIV0
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
004750
004751     MOVE -1                     TO PFENTERL.
004752     GO TO 8200-SEND-DATAONLY.
004753
004754     
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6501' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
004755
004756 9995-SECURITY-VIOLATION.
004757*                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00010050' TO DFHEIV0
           MOVE X'2E2243202020202020202020' &
                X'202020202020202020202820' &
                X'2020233030303130303530' TO DFHEIV0
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
004758
004759 9995-EXIT.
004760     EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6501' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND,
                     0500-CHECK-MAINT-TYPE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1500-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1799-EXIT,
                     1799-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2100-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 2200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3030-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 3530-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4020-BUS-TYPE-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4099-STD-BEN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4180-REIN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5250-QID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 5399-EXIT,
                     5399-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6450-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 6699-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 6880-NO-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 6925-NOT-FIND,
                     6925-NOT-FIND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 7999-EXIT,
                     7999-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8010-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 8055-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6501' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
