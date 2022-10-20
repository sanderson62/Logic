00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL652 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 09/14/94 07:57:28.
00007 *                            VMOD=2.059
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS, TEXAS.
00024 *
00025 *REMARKS.
00026 *        TRANSACTION - EXD4 - COMPENSATION MASTER MAINT
00024 *
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
102202* 102202    2002032200002  SMVA  ADD PF8 KEY FOR COMP MSTR NOTES
111103* 111103    2003080800002  PEMA  ADD BANKFEE,CLPST,MAXFEE  & PF10
111103*                                FOR SECURE PAY
111204* 111204    2004110300005  PEMA  SPLIT AGENT COMMISSION CHANGES
033105* 033105    2005031100003  PEMA  ADD VALID TYPE OF 'B' (BANK)
042005* 042005    2005031100004  PEMA  ALLOW UPDATES TO PCONT FOR 'B'
042005* 042005                         AND 'G' RECORD TYPES.
092205* 092205    2005050300006  PEMA  ADD LEASE FEES
041106* 041106    2006022800001  AJRA  INIT CO-FIRST-WRITTEN-DT ON ADD
060506* 060506    2002061100007  PEMA  ADD CODES TO ERCOMP FILE
072406* 072406    2006022400001  PEMA  ADD REFUND ONLY EDIT
012407* 110706  CR2006071700002  PEMA  FIX NAME LOOK UP
043007* 043007  IR2007042600002  PEMA  TURN OFF YTDCOMM ATTR
102908* 102908  CR2007052100005  PEMA  UNPROT PCONT ATTRB
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
080612* 080612  CR2012042700005  PEMA  ADD OVER 120 DAYS FOR AHL
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
101101******************************************************************
00027
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  EJECT
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL652 WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.059 *********'.
       77  WS-TOT-FEES                 PIC S9(3)V99 COMP-3 VALUE +0.
       77  S1                          PIC S999 VALUE +0 COMP-3.
00035
00036  01  WS-DATE-AREA.
00037      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.
00038      12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.
00039
00040  01  STANDARD-AREAS.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
00041      12  RETURNED-FROM       PIC X(8)        VALUE SPACES.
00042      12  QID-PI.
00043          16  QID-TERM        PIC X(4).
00044          16  FILLER          PIC X(4)        VALUE '652A'.
00045      12  QID-ITEM            PIC S9(4) COMP  VALUE +0001.
00046      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
00047      12  MAP-NAME            PIC  X(8)       VALUE 'EL652A'.
00048      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL652S'.
00049      12  TRANS-ID            PIC  X(4)       VALUE 'EXD4'.
00050      12  EL640-TRANS-ID      PIC  X(4)       VALUE 'EXC1'.
00051      12  EL642-TRANS-ID      PIC  X(4)       VALUE 'EXH7'.
00052      12  EL633-TRANS-ID      PIC  X(4)       VALUE 'EXB7'.
00053      12  EL633DMD-TRANS-ID   PIC  X(4)       VALUE 'EX1F'.
00054      12  EL635-TRANS-ID      PIC  X(4)       VALUE 'EXJ4'.
00055      12  EL6501-TRANS-ID     PIC  X(4)       VALUE 'EXC5'.
00056      12  EL6592-TRANS-ID     PIC  X(4)       VALUE 'EX66'.
00057      12  EL856-TRANS-ID      PIC  X(4)       VALUE 'EXJ8'.
00058      12  EM6508-TRANS-ID     PIC  X(4)       VALUE 'MXG8'.
00059      12  THIS-PGM            PIC  X(8)       VALUE 'EL652'.
00060      12  PGM-NAME            PIC  X(8).
00061      12  WS-COMP-CD-R.
00062          16  FILLER          PIC  X.
00063          16  WS-COMP-CD-X    PIC  X.
00064      12  WS-COMP-CD  REDEFINES
00065          WS-COMP-CD-R        PIC S9(4)                  COMP.
00066      12  TIME-IN             PIC S9(7).
00067      12  TIME-OUT-R  REDEFINES  TIME-IN.
00068          16  FILLER          PIC  X.
00069          16  TIME-OUT        PIC  99V99.
00070          16  FILLER          PIC  XX.
00071      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00072      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00073      12  XCTL-126            PIC  X(8)       VALUE 'EL126'.
00074      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
00075      12  XCTL-633            PIC  X(8)       VALUE 'EL633'.
00076      12  XCTL-633DMD         PIC  X(8)       VALUE 'EL633DMD'.
00077      12  XCTL-635            PIC  X(8)       VALUE 'EL635'.
00077      12  XCTL-650            PIC  X(8)       VALUE 'EL650'.
00078      12  XCTL-6521           PIC  X(8)       VALUE 'EL6521'.
102202     12  XCTL-6522           PIC  X(8)       VALUE 'EL6522'.
111103     12  XCTL-6523           PIC  X(8)       VALUE 'EL6523'.
111204     12  XCTL-6524           PIC  X(8)       VALUE 'EL6524'.
00079      12  XCTL-689            PIC  X(8)       VALUE 'EL689'.
00080      12  XCTL-690            PIC  X(8)       VALUE 'EL690'.
00081      12  XCTL-856            PIC  X(8)       VALUE 'EL856'.
00082      12  XCTL-EM626          PIC  X(8)       VALUE 'EM626'.
00083      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00084      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00085      12  LINK-ELDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00086      12  FILE-ID             PIC  X(8)       VALUE SPACES.
00087      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.
00088      12  NAME-FILE-ID        PIC  X(8)       VALUE 'ERNAME'.
00089      12  CNTL-FILE-ID        PIC  X(8)       VALUE 'ELCNTL'.
00090      12  SUMM-FILE-ID        PIC  X(8)       VALUE 'ERSUMM'.
00091      12  RQST-FILE-ID        PIC  X(8)       VALUE 'ERRQST'.
00092      12  RQST-FILE-ID-3      PIC  X(8)       VALUE 'ERRQST3'.
00093
00094  01  MISC-WORK-AREAS.
CIDMOD     12  WS-SRCH-STATE       PIC X(30)  VALUE SPACES.
CIDMOD     12  STNDX               PIC S999   COMP-3 VALUE +0.
00095      12  W-APPL-SCRTY-NDX    PIC S9(04) COMP   VALUE +35.
00096      12  WS-PHONE-IN         PIC  9(10).
00097      12  WS-PHONE-IN-R  REDEFINES  WS-PHONE-IN.
00098          16  WSPI-AREA       PIC  X(3).
00099          16  WSPI-PFX        PIC  X(3).
00100          16  WSPI-SFX        PIC  X(4).
00101      12  WS-PHONE-OUT.
00102          16  WSPO-AREA       PIC  X(3).
00103          16  FILLER          PIC  X            VALUE '-'.
00104          16  WSPO-PFX        PIC  X(3).
00105          16  FILLER          PIC  X            VALUE '-'.
00106          16  WSPO-SFX        PIC  X(4).
00107      12  DEEDIT-FIELD        PIC  X(15).
00108      12  DEEDIT-FIELD-V0  REDEFINES
00109          DEEDIT-FIELD        PIC S9(15).
00110      12  DEEDIT-FIELD-V1  REDEFINES
00111          DEEDIT-FIELD        PIC S9(13)V99.
00112      12  SUB1                PIC S9(4)         VALUE +0   COMP.
00113      12  SUB2                PIC S9(4)         VALUE +0   COMP.
00114      12  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.
00115      12  ERCOMP-LENGTH       PIC S9(4)         VALUE +700 COMP.
00116      12  ERSUMM-LENGTH       PIC S9(4)         VALUE +150 COMP.
00117      12  ERNAME-LENGTH       PIC S9(4)         VALUE +160 COMP.
00118      12  SV-CLMTOL           PIC  9(3)V99      VALUE ZEROS.
00119      12  WK-DATE.
00120          16  WK-MO           PIC  99.
00121          16  WK-DA           PIC  99.
00122          16  WK-YR           PIC  99.
00123      12  DATE-TEST-AREA      PIC  9(6).
00124      12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.
00125          16  DATE-TEST-MM    PIC  99.
00126          16  DATE-TEST-DD    PIC  99.
00127          16  DATE-TEST-YY    PIC  99.
00128      12  DIVIDE-RESULT       PIC  99.
00129      12  DIVIDE-REMAINDER    PIC  9.
00130      12  WS-ZERO             PIC  X            VALUE '0'.
00131      12  WS-ONE              PIC  X            VALUE '1'.
00132      12  WS-TWO              PIC  X            VALUE '2'.
00133      12  WS-ACCESS.
00134          16  FILLER          PIC  X(3)         VALUE SPACES.
00135          16  WS-CARRIER      PIC  X.
00136      12  BIN-CURRENT-SAVE    PIC  XX.
00137      12  WS-BROWSE-SW        PIC  X.
00138          88  BROWSE-STARTED                    VALUE 'Y'.
00139      12  WS-SAVE-KEY         PIC  X(29)        VALUE SPACES.
00140      12  WS-SAVE-SUMM        PIC  X(6)         VALUE SPACES.
00141      12  WS-SUMM-FOR-RQST    PIC  X(6)         VALUE SPACES.
00142      12  WS-SAVE-RQST        PIC X(46)         VALUE SPACES.
00143      12  WS-SHOW-SAVE-TOTALS PIC  X            VALUE SPACE.
00144          88  SHOW-SAVE-TOTALS                  VALUE 'Y'.
00145      12  ELCNTL-KEY.
00146          16  CNTL-COMP-ID    PIC  X(3)         VALUE SPACES.
00147          16  CNTL-REC-TYPE   PIC  X            VALUE SPACES.
00148          16  CNTL-ACCESS     PIC  X(4)         VALUE SPACES.
00149          16  CNTL-SEQ-NO     PIC S9(4)         VALUE +0   COMP.
111103     12  ELCNTL-KEY2.
111103         16  CNTL-COMP-ID-2  PIC  X(3)         VALUE SPACES.
111103         16  CNTL-REC-TYPE-2 PIC  X            VALUE SPACES.
111103         16  CNTL-STATE-2    PIC  X(2)         VALUE SPACES.
111103         16  FILLER          PIC  X(2)         VALUE SPACES.
111103         16  CNTL-SEQ-NO-2   PIC S9(4)         VALUE +0   COMP.
00150      12  ERSUMM-KEY.
00151          16  SUMM-COMP-ID    PIC  X            VALUE SPACES.
00152          16  SUMM-SUMMARY    PIC  X(6)         VALUE SPACES.
00153          16  SUMM-CARRIER    PIC  X            VALUE SPACES.
00154          16  SUMM-GROUP      PIC  X(6)         VALUE SPACES.
00155          16  SUMM-FIN-RESP   PIC  X(10)        VALUE SPACES.
00156          16  SUMM-ACCT-AGENT PIC  X(10)        VALUE SPACES.
00157      12  ERRQST-KEY.
00158          16  RQST-COMP-ID-PC PIC  X            VALUE SPACES.
00159          16  RQST-BATCH-PC   PIC  X(6)         VALUE SPACES.
00160      12  ERRQST-KEY-3.
00161          16  RQST-COMP-ID    PIC  X            VALUE SPACES.
00162          16  RQST-CARRIER    PIC  X            VALUE SPACES.
00163          16  RQST-GROUP      PIC  X(6)         VALUE SPACES.
00164          16  RQST-FIN-RESP   PIC  X(10)        VALUE SPACES.
00165          16  RQST-ACCT-AGENT PIC  X(10)        VALUE SPACES.
00166          16  RQST-REFERENCE  PIC  X(12)        VALUE SPACES.
00167          16  RQST-BATCH      PIC  X(06)        VALUE SPACES.
00168      12  WS-DATE-MDY-8.
00169          16  WS-DMDY8-MM     PIC  XX.
00170          16  WS-DMDY8-SL1    PIC  X.
00171          16  WS-DMDY8-DD     PIC  XX.
00172          16  WS-DMDY8-SL2    PIC  X.
00173          16  WS-DMDY8-YY     PIC  XX.
00174      12  WS-DATE-MDY-6  REDEFINES  WS-DATE-MDY-8.
00175          16  WS-DMDY6-MM     PIC  XX.
00176          16  WS-DMDY6-DD     PIC  XX.
00177          16  WS-DMDY6-YY     PIC  XX.
00178          16  WS-DMDY6-BLNK   PIC  XX.
00179      12  WS-YMD-DATE.
00180          16  WS-YMD-YY       PIC  XX.
00181          16  WS-YMD-MM       PIC  XX.
00182          16  WS-YMD-DD       PIC  XX.
00183      12  WS-YMD-DATE-NUM  REDEFINES
00184          WS-YMD-DATE         PIC  9(6).
00185      12  WS-SAVE-SUMMARY     PIC X(6)          VALUE SPACES.
00186      12  WS-SAVE-NAME        PIC X(30)         VALUE SPACES.
00187      12  WS-SAVE-BALFWD      PIC S9(7)V99      VALUE +0   COMP-3.
00188      12  WS-SAVE-CURCOM      PIC S9(7)V99      VALUE +0   COMP-3.
00189      12  WS-SAVE-CURCHG      PIC S9(7)V99      VALUE +0   COMP-3.
00190      12  WS-SAVE-CURPMT      PIC S9(7)V99      VALUE +0   COMP-3.
00191      12  WS-SAVE-ENDBAL      PIC S9(7)V99      VALUE +0   COMP-3.
00192      12  WS-SAVE-YTDCOM      PIC S9(7)V99      VALUE +0   COMP-3.
00193      12  WS-SOC-SEC-WORK-FIELD.
00194          16  WS-SS-TYPE          PIC X.
00195          16  WS-SOC-SEC          PIC X(12).
00196
00197      12  WS-ZIP-CODE.
00198          16  WS-ZIP-1            PIC X.
00199              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
00200          16  WS-ZIP-2-3          PIC XX.
00201          16  WS-ZIP-4            PIC X.
00202          16  WS-ZIP-5            PIC X.
00203          16  WS-ZIP-6            PIC X.
00204          16  FILLER              PIC X(4).
00205      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
00206          16  WS-ZIP-AM-1-CODE    PIC X(5).
00207          16  WS-ZIP-AM-1-PLUS4   PIC X(4).
00208          16  FILLER              PIC X.
00209      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
00210          16  WS-ZIP-AM-2-CODE    PIC X(5).
00211          16  WS-ZIP-AM-2-DASH    PIC X.
00212          16  WS-ZIP-AM-2-PLUS4   PIC X(4).
00213      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
00214          16  WS-ZIP-CAN-1-POST1  PIC XXX.
00215          16  WS-ZIP-CAN-1-POST2  PIC XXX.
00216          16  FILLER              PIC X(4).
00217      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
00218          16  WS-ZIP-CAN-2-POST1  PIC XXX.
00219          16  FILLER              PIC X.
00220          16  WS-ZIP-CAN-2-POST2  PIC XXX.
00221          16  FILLER              PIC XXX.
00222
00223  EJECT
00224  01  ERROR-NUMBERS.
00225      12  ER-0000             PIC  X(4)       VALUE '0000'.
00226      12  ER-0002             PIC  X(4)       VALUE '0002'.
00227      12  ER-0004             PIC  X(4)       VALUE '0004'.
00228      12  ER-0008             PIC  X(4)       VALUE '0008'.
00229      12  ER-0029             PIC  X(4)       VALUE '0029'.
00230      12  ER-0033             PIC  X(4)       VALUE '0033'.
CIDMOD     12  ER-0035             PIC  X(4)       VALUE '0035'.
00231      12  ER-0046             PIC  X(4)       VALUE '0046'.
00232      12  ER-0068             PIC  X(4)       VALUE '0068'.
00233      12  ER-0070             PIC  X(4)       VALUE '0070'.
00234      12  ER-0142             PIC  X(4)       VALUE '0142'.
111103     12  ER-0144             PIC  X(4)       VALUE '0144'.
111103     12  ER-0187             PIC  X(4)       VALUE '0187'.
00235      12  ER-0193             PIC  X(4)       VALUE '0193'.
00236      12  ER-0314             PIC  X(4)       VALUE '0314'.
00237      12  ER-0584             PIC  X(4)       VALUE '0584'.
00238      12  ER-0829             PIC  X(4)       VALUE '0829'.
00240      12  ER-1299             PIC  X(4)       VALUE '1299'.
111103     12  ER-1778             PIC  X(4)       VALUE '1778'.
00239      12  ER-1883             PIC  X(4)       VALUE '1883'.
00240      12  ER-2039             PIC  X(4)       VALUE '2039'.
00241      12  ER-2042             PIC  X(4)       VALUE '2042'.
00242      12  ER-2045             PIC  X(4)       VALUE '2045'.
00243      12  ER-2046             PIC  X(4)       VALUE '2046'.
00244      12  ER-2047             PIC  X(4)       VALUE '2047'.
00245      12  ER-2048             PIC  X(4)       VALUE '2048'.
00246      12  ER-2049             PIC  X(4)       VALUE '2049'.
00247      12  ER-2050             PIC  X(4)       VALUE '2050'.
00248      12  ER-2055             PIC  X(4)       VALUE '2055'.
00249      12  ER-2056             PIC  X(4)       VALUE '2056'.
00250      12  ER-2057             PIC  X(4)       VALUE '2057'.
00251      12  ER-2067             PIC  X(4)       VALUE '2067'.
00252      12  ER-2088             PIC  X(4)       VALUE '2088'.
00253      12  ER-2089             PIC  X(4)       VALUE '2089'.
00254      12  ER-2091             PIC  X(4)       VALUE '2091'.
00255      12  ER-2092             PIC  X(4)       VALUE '2092'.
00256      12  ER-2093             PIC  X(4)       VALUE '2093'.
00257      12  ER-2094             PIC  X(4)       VALUE '2094'.
00258      12  ER-2095             PIC  X(4)       VALUE '2095'.
00259      12  ER-2096             PIC  X(4)       VALUE '2096'.
00260      12  ER-2097             PIC  X(4)       VALUE '2097'.
           12  ER-2209             PIC  X(4)       VALUE '2209'.
00261      12  ER-2238             PIC  X(4)       VALUE '2238'.
00262      12  ER-2370             PIC  X(4)       VALUE '2370'.
00263      12  ER-2572             PIC  X(4)       VALUE '2572'.
00263      12  ER-2717             PIC  X(4)       VALUE '2717'.
072406     12  ER-2790             PIC  X(4)       VALUE '2790'.
00264      12  ER-2872             PIC  X(4)       VALUE '2872'.
00265      12  ER-3021             PIC  X(4)       VALUE '3021'.
00266      12  ER-3053             PIC  X(4)       VALUE '3053'.
00267      12  ER-3055             PIC  X(4)       VALUE '3055'.
00268      12  ER-3056             PIC  X(4)       VALUE '3056'.
00269      12  ER-3150             PIC  X(4)       VALUE '3150'.
00270      12  ER-3151             PIC  X(4)       VALUE '3151'.
00271      12  ER-3152             PIC  X(4)       VALUE '3152'.
00272      12  ER-3153             PIC  X(4)       VALUE '3153'.
00273      12  ER-3154             PIC  X(4)       VALUE '3154'.
00274      12  ER-3168             PIC  X(4)       VALUE '3168'.
00275      12  ER-3170             PIC  X(4)       VALUE '3170'.
00276      12  ER-3174             PIC  X(4)       VALUE '3174'.
111103     12  ER-3261             PIC  X(4)       VALUE '3261'.
00277      12  ER-9096             PIC  X(4)       VALUE '9096'.
00278      12  ER-9097             PIC  X(4)       VALUE '9097'.
           12  ER-9999             PIC  X(4)       VALUE '9999'.
00279  EJECT
00280 *                          COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00281  EJECT
00282 *                          COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
00283 *                          COPY MPCSCRT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCSCRT                             *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        ACQUIRED BY SIGN-ON PROGRAM EL125.                      *
00008 *                                      (MP MORTGAGE PROTECTION)  *
00009 *                                                                *
00010 ******************************************************************
00011 *
00012  01  SECURITY-CONTROL-E.
00013      12  SC-COMM-LENGTH-E             PIC S9(04) VALUE +144 COMP.
00014      12  FILLER                       PIC  X(02) VALUE 'SC'.
00015      12  SC-QUID-KEY.
00016          16  SC-QUID-TERMINAL         PIC  X(04).
00017          16  SC-QUID-SYSTEM           PIC  X(04).
00018      12  SC-ITEM                      PIC S9(04) VALUE +1   COMP.
00019      12  SC-SECURITY-ACCESS-CODE      PIC  X(01).
00020      12  SC-PRODUCER-AUTHORIZED-SW    PIC  X(01).
00021          88 SC-PRODUCER-AUTHORIZED               VALUE ' '.
00022          88 SC-PRODUCER-NOT-AUTHORIZED           VALUE 'N'.
00023      12  SC-MP-CODES.
00024          16  SC-MP-AUTHORIZATION OCCURS 44 TIMES.
00025              20  SC-MP-DISPLAY        PIC  X(01).
00026              20  SC-MP-UPDATE         PIC  X(01).
00027      12  FILLER                       PIC  X(40).
00284  EJECT
00285  01  FILLER                  PIC X(16)  VALUE ALL 'A'.
00286 *                          COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00287  EJECT
00288 *                          COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00289  EJECT
00290 *                          COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00291  EJECT
00292 *                          COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00293  EJECT
00294  01  FILLER                  PIC X(16)  VALUE ALL 'B'.
00295 *                          COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
00296      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
00297          16  PI-CHECK-MAINT-TYPE     PIC  X.
00298              88  VALID-MAINT-TYPE            VALUE 'S' 'A'
00299                                                    'C' 'D'.
00300              88  ADD-FUNCTION                VALUE 'A'.
00301              88  SHOW-FUNCTION               VALUE 'S'.
00302              88  DELETE-FUNCTION             VALUE 'D'.
00303              88  CHANGE-FUNCTION             VALUE 'C'.
00304          16  PI-CHECK-TYPE           PIC  X.
033105             88  VALID-TYPE                  VALUE 'A' 'B' 'C'
00306                                                            'G'.
00307          16  PI-CHECK-CARRY-BAL      PIC  X.
00308              88  VALID-CARRY-BAL             VALUE 'Y' 'N'.
00309          16  PI-FIRST-TIME-SW        PIC  X.
00310              88  FIRST-TIME                  VALUE 'Y'.
00311          16  PI-ERCOMP-EOF-SW        PIC  X.
00312              88  ERCOMP-EOF                  VALUE 'Y'.
00313          16  PI-SAVE-PHONE           PIC  X(10).
00314          16  PI-SAVE-PHONE-RED REDEFINES PI-SAVE-PHONE  PIC 9(10).
00315          16  PI-ERC-END-BAL          PIC S9(9)V99       COMP-3.
00316          16  PI-ERCOMP-KEY.
00317              20  PI-ERC-COMPANY-CD   PIC  X.
00318              20  PI-ERC-CARRIER      PIC  X.
00319              20  PI-ERC-GROUP        PIC  X(6).
00320              20  PI-ERC-RESP         PIC  X(10).
00321              20  PI-ERC-ACCT         PIC  X(10).
00322              20  PI-ERC-TYPE         PIC  X.
00323          16  PI-SAVE-ERCOMP-KEY      PIC  X(29).
00324          16  PI-SAVE-FAXNO           PIC  X(10).
00325          16  PI-SAVE-FAXNO-RED REDEFINES PI-SAVE-FAXNO  PIC 9(10).
CIDMOD         16  PI-SAVE-ADDR2           PIC X(30).
CIDMOD         16  PI-SAVE-CITYST.
                   20  PI-SAVE-CITY        PIC X(28).
CIDMOD             20  PI-SAVE-STATE       PIC XX.
102202         16  PI-SAVE-ACCT-NAME       PIC X(30).
pemtst         16  PI-EL652-DEL-SW         PIC X.
               16  PI-UPDATE-SW            PIC X.
102202         16  FILLER                  PIC  X(457).
00327  EJECT
00328 *                            COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00329                              PIC  X(750).
00330  EJECT
00331 *                            COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00332
00333  01  FILLER  REDEFINES  DFHAID.
00334      12  FILLER              PIC  X(8).
00335      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
00336  EJECT
00337 *                            COPY EL652S.
       01  EL652AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDATEL PIC S9(0004) COMP.
           05  RUNDATEF PIC  X(0001).
           05  FILLER REDEFINES RUNDATEF.
               10  RUNDATEA PIC  X(0001).
           05  RUNDATEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  MAINTYPL PIC S9(0004) COMP.
           05  MAINTYPF PIC  X(0001).
           05  FILLER REDEFINES MAINTYPF.
               10  MAINTYPA PIC  X(0001).
           05  MAINTYPI PIC  X(0001).
      *    -------------------------------
           05  SCDESCL PIC S9(0004) COMP.
           05  SCDESCF PIC  X(0001).
           05  FILLER REDEFINES SCDESCF.
               10  SCDESCA PIC  X(0001).
           05  SCDESCI PIC  X(0008).
      *    -------------------------------
           05  FLITYPEL PIC S9(0004) COMP.
           05  FLITYPEF PIC  X(0001).
           05  FILLER REDEFINES FLITYPEF.
               10  FLITYPEA PIC  X(0001).
           05  FLITYPEI PIC  X(0004).
      *    -------------------------------
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  FINRESPL PIC S9(0004) COMP.
           05  FINRESPF PIC  X(0001).
           05  FILLER REDEFINES FINRESPF.
               10  FINRESPA PIC  X(0001).
           05  FINRESPI PIC  X(0010).
      *    -------------------------------
           05  ACCTNOL PIC S9(0004) COMP.
           05  ACCTNOF PIC  X(0001).
           05  FILLER REDEFINES ACCTNOF.
               10  ACCTNOA PIC  X(0001).
           05  ACCTNOI PIC  X(0010).
      *    -------------------------------
           05  SUMMNOL PIC S9(0004) COMP.
           05  SUMMNOF PIC  X(0001).
           05  FILLER REDEFINES SUMMNOF.
               10  SUMMNOA PIC  X(0001).
           05  SUMMNOI PIC  X(0006).
      *    -------------------------------
           05  LSTMDTL PIC S9(0004) COMP.
           05  LSTMDTF PIC  X(0001).
           05  FILLER REDEFINES LSTMDTF.
               10  LSTMDTA PIC  X(0001).
           05  LSTMDTI PIC  X(0008).
      *    -------------------------------
           05  FLITYPL PIC S9(0004) COMP.
           05  FLITYPF PIC  X(0001).
           05  FILLER REDEFINES FLITYPF.
               10  FLITYPA PIC  X(0001).
           05  FLITYPI PIC  X(0001).
      *    -------------------------------
           05  PCONTL PIC S9(0004) COMP.
           05  PCONTF PIC  X(0001).
           05  FILLER REDEFINES PCONTF.
               10  PCONTA PIC  X(0001).
           05  PCONTI PIC  X(0030).
      *    -------------------------------
           05  PNT1099L PIC S9(0004) COMP.
           05  PNT1099F PIC  X(0001).
           05  FILLER REDEFINES PNT1099F.
               10  PNT1099A PIC  X(0001).
           05  PNT1099I PIC  X(0001).
      *    -------------------------------
           05  MAILNAML PIC S9(0004) COMP.
           05  MAILNAMF PIC  X(0001).
           05  FILLER REDEFINES MAILNAMF.
               10  MAILNAMA PIC  X(0001).
           05  MAILNAMI PIC  X(0030).
      *    -------------------------------
           05  SSNL PIC S9(0004) COMP.
           05  SSNF PIC  X(0001).
           05  FILLER REDEFINES SSNF.
               10  SSNA PIC  X(0001).
           05  SSNI PIC  X(0013).
      *    -------------------------------
           05  ACCTNAML PIC S9(0004) COMP.
           05  ACCTNAMF PIC  X(0001).
           05  FILLER REDEFINES ACCTNAMF.
               10  ACCTNAMA PIC  X(0001).
           05  ACCTNAMI PIC  X(0030).
      *    -------------------------------
           05  PHONEL PIC S9(0004) COMP.
           05  PHONEF PIC  X(0001).
           05  FILLER REDEFINES PHONEF.
               10  PHONEA PIC  X(0001).
           05  PHONEI PIC  X(0012).
      *    -------------------------------
           05  ADDR1L PIC S9(0004) COMP.
           05  ADDR1F PIC  X(0001).
           05  FILLER REDEFINES ADDR1F.
               10  ADDR1A PIC  X(0001).
           05  ADDR1I PIC  X(0030).
      *    -------------------------------
           05  FAXDESCL PIC S9(0004) COMP.
           05  FAXDESCF PIC  X(0001).
           05  FILLER REDEFINES FAXDESCF.
               10  FAXDESCA PIC  X(0001).
           05  FAXDESCI PIC  X(0016).
      *    -------------------------------
           05  FAXNOL PIC S9(0004) COMP.
           05  FAXNOF PIC  X(0001).
           05  FILLER REDEFINES FAXNOF.
               10  FAXNOA PIC  X(0001).
           05  FAXNOI PIC  X(0012).
      *    -------------------------------
           05  ADDR2L PIC S9(0004) COMP.
           05  ADDR2F PIC  X(0001).
           05  FILLER REDEFINES ADDR2F.
               10  ADDR2A PIC  X(0001).
           05  ADDR2I PIC  X(0030).
      *    -------------------------------
           05  CARBALL PIC S9(0004) COMP.
           05  CARBALF PIC  X(0001).
           05  FILLER REDEFINES CARBALF.
               10  CARBALA PIC  X(0001).
           05  CARBALI PIC  X(0001).
      *    -------------------------------
           05  CITYL PIC S9(0004) COMP.
           05  CITYF PIC  X(0001).
           05  FILLER REDEFINES CITYF.
               10  CITYA PIC  X(0001).
           05  CITYI PIC  X(0028).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  REFEHL PIC S9(0004) COMP.
           05  REFEHF PIC  X(0001).
           05  FILLER REDEFINES REFEHF.
               10  REFEHA PIC  X(0001).
           05  REFEHI PIC  X(0013).
      *    -------------------------------
           05  REFEL PIC S9(0004) COMP.
           05  REFEF PIC  X(0001).
           05  FILLER REDEFINES REFEF.
               10  REFEA PIC  X(0001).
           05  REFEI PIC  X(0001).
      *    -------------------------------
           05  ZIPCODEL PIC S9(0004) COMP.
           05  ZIPCODEF PIC  X(0001).
           05  FILLER REDEFINES ZIPCODEF.
               10  ZIPCODEA PIC  X(0001).
           05  ZIPCODEI PIC  X(0010).
      *    -------------------------------
           05  NGDESCL PIC S9(0004) COMP.
           05  NGDESCF PIC  X(0001).
           05  FILLER REDEFINES NGDESCF.
               10  NGDESCA PIC  X(0001).
           05  NGDESCI PIC  X(0012).
      *    -------------------------------
           05  NETGRSL PIC S9(0004) COMP.
           05  NETGRSF PIC  X(0001).
           05  FILLER REDEFINES NETGRSF.
               10  NETGRSA PIC  X(0001).
           05  NETGRSI PIC  X(0001).
      *    -------------------------------
           05  AHL120L PIC S9(0004) COMP.
           05  AHL120F PIC  X(0001).
           05  FILLER REDEFINES AHL120F.
               10  AHL120A PIC  X(0001).
           05  AHL120I PIC  X(0016).
      *    -------------------------------
           05  OVER120L PIC S9(0004) COMP.
           05  OVER120F PIC  X(0001).
           05  FILLER REDEFINES OVER120F.
               10  OVER120A PIC  X(0001).
           05  OVER120I PIC  X(0013).
      *    -------------------------------
           05  CSRL PIC S9(0004) COMP.
           05  CSRF PIC  X(0001).
           05  FILLER REDEFINES CSRF.
               10  CSRA PIC  X(0001).
           05  CSRI PIC  X(0004).
      *    -------------------------------
           05  RPTCDDL PIC S9(0004) COMP.
           05  RPTCDDF PIC  X(0001).
           05  FILLER REDEFINES RPTCDDF.
               10  RPTCDDA PIC  X(0001).
           05  RPTCDDI PIC  X(0010).
      *    -------------------------------
           05  RPTCD2L PIC S9(0004) COMP.
           05  RPTCD2F PIC  X(0001).
           05  FILLER REDEFINES RPTCD2F.
               10  RPTCD2A PIC  X(0001).
           05  RPTCD2I PIC  X(0010).
      *    -------------------------------
           05  CKDESCL PIC S9(0004) COMP.
           05  CKDESCF PIC  X(0001).
           05  FILLER REDEFINES CKDESCF.
               10  CKDESCA PIC  X(0001).
           05  CKDESCI PIC  X(0016).
      *    -------------------------------
           05  CKPULLL PIC S9(0004) COMP.
           05  CKPULLF PIC  X(0001).
           05  FILLER REDEFINES CKPULLF.
               10  CKPULLA PIC  X(0001).
           05  CKPULLI PIC  X(0001).
      *    -------------------------------
           05  BILLPRTL PIC S9(0004) COMP.
           05  BILLPRTF PIC  X(0001).
           05  FILLER REDEFINES BILLPRTF.
               10  BILLPRTA PIC  X(0001).
           05  BILLPRTI PIC  X(0001).
      *    -------------------------------
           05  LETDESCL PIC S9(0004) COMP.
           05  LETDESCF PIC  X(0001).
           05  FILLER REDEFINES LETDESCF.
               10  LETDESCA PIC  X(0001).
           05  LETDESCI PIC  X(0011).
      *    -------------------------------
           05  LETRCDL PIC S9(0004) COMP.
           05  LETRCDF PIC  X(0001).
           05  FILLER REDEFINES LETRCDF.
               10  LETRCDA PIC  X(0001).
           05  LETRCDI PIC  X(0001).
      *    -------------------------------
           05  BALPRTL PIC S9(0004) COMP.
           05  BALPRTF PIC  X(0001).
           05  FILLER REDEFINES BALPRTF.
               10  BALPRTA PIC  X(0001).
           05  BALPRTI PIC  X(0011).
      *    -------------------------------
           05  BALCDL PIC S9(0004) COMP.
           05  BALCDF PIC  X(0001).
           05  FILLER REDEFINES BALCDF.
               10  BALCDA PIC  X(0001).
           05  BALCDI PIC  X(0001).
      *    -------------------------------
           05  SPPDDHL PIC S9(0004) COMP.
           05  SPPDDHF PIC  X(0001).
           05  FILLER REDEFINES SPPDDHF.
               10  SPPDDHA PIC  X(0001).
           05  SPPDDHI PIC  X(0012).
      *    -------------------------------
           05  SPPDDL PIC S9(0004) COMP.
           05  SPPDDF PIC  X(0001).
           05  FILLER REDEFINES SPPDDF.
               10  SPPDDA PIC  X(0001).
           05  SPPDDI PIC  X(0001).
      *    -------------------------------
           05  MFLABLL PIC S9(0004) COMP.
           05  MFLABLF PIC  X(0001).
           05  FILLER REDEFINES MFLABLF.
               10  MFLABLA PIC  X(0001).
           05  MFLABLI PIC  X(0016).
      *    -------------------------------
           05  MAXFEEL PIC S9(0004) COMP.
           05  MAXFEEF PIC  X(0001).
           05  FILLER REDEFINES MAXFEEF.
               10  MAXFEEA PIC  X(0001).
           05  MAXFEEI PIC  S9(04)V99.
      *    -------------------------------
           05  CSLABLL PIC S9(0004) COMP.
           05  CSLABLF PIC  X(0001).
           05  FILLER REDEFINES CSLABLF.
               10  CSLABLA PIC  X(0001).
           05  CSLABLI PIC  X(0012).
      *    -------------------------------
           05  CLPSTL PIC S9(0004) COMP.
           05  CLPSTF PIC  X(0001).
           05  FILLER REDEFINES CLPSTF.
               10  CLPSTA PIC  X(0001).
           05  CLPSTI PIC  X(0002).
      *    -------------------------------
           05  LFLABLL PIC S9(0004) COMP.
           05  LFLABLF PIC  X(0001).
           05  FILLER REDEFINES LFLABLF.
               10  LFLABLA PIC  X(0001).
           05  LFLABLI PIC  X(0016).
      *    -------------------------------
           05  MAXLFL PIC S9(0004) COMP.
           05  MAXLFF PIC  X(0001).
           05  FILLER REDEFINES MAXLFF.
               10  MAXLFA PIC  X(0001).
           05  MAXLFI PIC  S9(04)V99.
      *    -------------------------------
           05  BALFWDL PIC S9(0004) COMP.
           05  BALFWDF PIC  X(0001).
           05  FILLER REDEFINES BALFWDF.
               10  BALFWDA PIC  X(0001).
           05  BALFWDI PIC  S9(11)V99.
      *    -------------------------------
           05  CURCOML PIC S9(0004) COMP.
           05  CURCOMF PIC  X(0001).
           05  FILLER REDEFINES CURCOMF.
               10  CURCOMA PIC  X(0001).
           05  CURCOMI PIC  S9(11)V99.
      *    -------------------------------
           05  CURCHGL PIC S9(0004) COMP.
           05  CURCHGF PIC  X(0001).
           05  FILLER REDEFINES CURCHGF.
               10  CURCHGA PIC  X(0001).
           05  CURCHGI PIC  S9(11)V99.
      *    -------------------------------
           05  CURPMTL PIC S9(0004) COMP.
           05  CURPMTF PIC  X(0001).
           05  FILLER REDEFINES CURPMTF.
               10  CURPMTA PIC  X(0001).
           05  CURPMTI PIC  S9(11)V99.
      *    -------------------------------
           05  ENDBALL PIC S9(0004) COMP.
           05  ENDBALF PIC  X(0001).
           05  FILLER REDEFINES ENDBALF.
               10  ENDBALA PIC  X(0001).
           05  ENDBALI PIC  S9(11)V99.
      *    -------------------------------
           05  CURRENTL PIC S9(0004) COMP.
           05  CURRENTF PIC  X(0001).
           05  FILLER REDEFINES CURRENTF.
               10  CURRENTA PIC  X(0001).
           05  CURRENTI PIC  X(0013).
      *    -------------------------------
           05  OVER30L PIC S9(0004) COMP.
           05  OVER30F PIC  X(0001).
           05  FILLER REDEFINES OVER30F.
               10  OVER30A PIC  X(0001).
           05  OVER30I PIC  X(0013).
      *    -------------------------------
           05  OVER60L PIC S9(0004) COMP.
           05  OVER60F PIC  X(0001).
           05  FILLER REDEFINES OVER60F.
               10  OVER60A PIC  X(0001).
           05  OVER60I PIC  X(0013).
      *    -------------------------------
           05  OVER90L PIC S9(0004) COMP.
           05  OVER90F PIC  X(0001).
           05  FILLER REDEFINES OVER90F.
               10  OVER90A PIC  X(0001).
           05  OVER90I PIC  X(0013).
      *    -------------------------------
           05  YTDCOML PIC S9(0004) COMP.
           05  YTDCOMF PIC  X(0001).
           05  FILLER REDEFINES YTDCOMF.
               10  YTDCOMA PIC  X(0001).
           05  YTDCOMI PIC  S9(11)V99.
      *    -------------------------------
           05  LSTDTEL PIC S9(0004) COMP.
           05  LSTDTEF PIC  X(0001).
           05  FILLER REDEFINES LSTDTEF.
               10  LSTDTEA PIC  X(0001).
           05  LSTDTEI PIC  X(0008).
      *    -------------------------------
           05  LSTTIMEL PIC S9(0004) COMP.
           05  LSTTIMEF PIC  X(0001).
           05  FILLER REDEFINES LSTTIMEF.
               10  LSTTIMEA PIC  X(0001).
           05  LSTTIMEI PIC  X(0005).
      *    -------------------------------
           05  LSTUSRL PIC S9(0004) COMP.
           05  LSTUSRF PIC  X(0001).
           05  FILLER REDEFINES LSTUSRF.
               10  LSTUSRA PIC  X(0001).
           05  LSTUSRI PIC  X(0004).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0072).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  99.
      *    -------------------------------
           05  PFK5L PIC S9(0004) COMP.
           05  PFK5F PIC  X(0001).
           05  FILLER REDEFINES PFK5F.
               10  PFK5A PIC  X(0001).
           05  PFK5I PIC  X(0011).
      *    -------------------------------
           05  PFK7L PIC S9(0004) COMP.
           05  PFK7F PIC  X(0001).
           05  FILLER REDEFINES PFK7F.
               10  PFK7A PIC  X(0001).
           05  PFK7I PIC  X(0012).
      *    -------------------------------
           05  PFK9L PIC S9(0004) COMP.
           05  PFK9F PIC  X(0001).
           05  FILLER REDEFINES PFK9F.
               10  PFK9A PIC  X(0001).
           05  PFK9I PIC  X(0012).
      *    -------------------------------
           05  PFK13L PIC S9(0004) COMP.
           05  PFK13F PIC  X(0001).
           05  FILLER REDEFINES PFK13F.
               10  PFK13A PIC  X(0001).
           05  PFK13I PIC  X(0011).
      *    -------------------------------
           05  PFK4L PIC S9(0004) COMP.
           05  PFK4F PIC  X(0001).
           05  FILLER REDEFINES PFK4F.
               10  PFK4A PIC  X(0001).
           05  PFK4I PIC  X(0012).
      *    -------------------------------
           05  PFK6L PIC S9(0004) COMP.
           05  PFK6F PIC  X(0001).
           05  FILLER REDEFINES PFK6F.
               10  PFK6A PIC  X(0001).
           05  PFK6I PIC  X(0011).
      *    -------------------------------
           05  PFK10L PIC S9(0004) COMP.
           05  PFK10F PIC  X(0001).
           05  FILLER REDEFINES PFK10F.
               10  PFK10A PIC  X(0001).
           05  PFK10I PIC  X(0014).
       01  EL652AO REDEFINES EL652AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SCDESCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FLITYPEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRESPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUMMNOO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTMDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FLITYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PCONTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PNT1099O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAILNAMO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SSNO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTNAMO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PHONEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FAXDESCO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FAXNOO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARBALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFEHO PIC  X(0013).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPCODEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NGDESCO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NETGRSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHL120O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVER120O PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCDDO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKDESCO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKPULLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BILLPRTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LETDESCO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LETRCDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALPRTO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALCDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SPPDDHO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SPPDDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MFLABLO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXFEEO PIC  Z99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSLABLO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLPSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFLABLO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXLFO PIC  Z99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALFWDO PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CURCOMO PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CURCHGO PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CURPMTO PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENDBALO PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CURRENTO PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVER30O PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVER60O PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVER90O PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YTDCOMO PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTUSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFK5O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFK7O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFK9O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFK13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFK4O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFK6O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFK10O PIC  X(0014).
      *    -------------------------------
00338  EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
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
      * Copyright (c) 2007-2013 Dell Inc.                             *
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
00340
00341  01  DFHCOMMAREA             PIC  X(1024).
00342
00343 *01 PARMLIST .
00344 *    12  FILLER              PIC S9(8)                  COMP.
00345 *    12  ERCOMP-POINTER      PIC S9(8)                  COMP.
00346 *    12  ERNAME-POINTER      PIC S9(8)                  COMP.
00347 *    12  ELCNTL-POINTER      PIC S9(8)                  COMP.
00348 *    12  ERSUMM-POINTER      PIC S9(8)                  COMP.
00349 *    12  ERRQST-POINTER      PIC S9(8)                  COMP.
00350  EJECT
00351 *                            COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
071712* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
071712     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
071712         16  CO-OV120                      PIC S9(7)V99   COMP-3.
071712         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
      *                            COPY ERCAGTC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCAGTC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   ONLINE CREDIT SYSTEM                                         *
      *                                                                *
      *   FILE DESCRIPTION = AGENT COMMISSIONS                         *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 450   RECFORM = FIXED                          *
      *                                                                *
      *   BASE CLUSTER NAME = ERAGTC                   RKP=2,LEN=21    *
      *       ALTERNATE PATH = NONE                                    *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 111004    2004110300005  PEMA  ADD DISTRIBUTION OF ADDENDUM FEE
      * 111004                         NEW FILE AND COPYBOOK
092205* 092205                   PEMA  ADD PROCESSING FOR SPP LEASE
      ******************************************************************
       01  AGENT-COMMISSIONS.
           12  AG-RECORD-ID                          PIC XX.
               88  VALID-CO-ID                          VALUE 'AG'.
           12  AG-CONTROL-PRIMARY.
               16  AG-COMPANY-CD                     PIC X.
               16  AG-CONTROL.
                   20  AG-CTL-1.
                       24  AG-CARR-GROUP.
                           28  AG-CARRIER            PIC X.
                           28  AG-GROUPING           PIC X(6).
                       24  AG-BANK                   PIC X(10).
                   20  AG-CTL-2.
                       24  AG-EXP-DT                 PIC XX.
               16  AG-TYPE                           PIC X.
                   88  AG-GEN-AGENT-TYPE                VALUE 'G'.
                   88  AG-ACCOUNT-TYPE                  VALUE 'A'.
           12  AG-MAINT-INFORMATION.
               16  AG-LAST-MAINT-DT                  PIC XX.
               16  AG-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
               16  AG-LAST-MAINT-USER                PIC X(4).
               16  FILLER                            PIC X(10).
           12  AG-EFF-DT                             PIC XX.
           12  AG-COMM-STRUCTURE.
               16  AG-AGT-COMMS OCCURS 10.
                   20  AG-AGT                PIC X(10).
                   20  AG-COM-TYP            PIC X.
                   20  AG-SPP-FEES           PIC S9(5)V99   COMP-3.
                   20  AG-RECALC-LV-INDIC    PIC X.
092205             20  AG-SPP-LFEES          PIC S9(5)V99   COMP-3.
092205             20  AG-LRCALC-LV-INDIC    PIC X.
092205             20  FILLER                PIC X(05).
           12  FILLER                  PIC X(145).
      ******************************************************************
00352  EJECT
00353 *                            COPY ERCNAME.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCNAME                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER NAME, COMPENSATION MASTER       *
00008 *       NAME, REINSURANCE COMPANY NAME LOOKUP FILE.              *
00009 *                                                                *
00010 *   FILE DESCRIPTION = NAME LOOKUP FILE                          *
00011 *                                                                *
00012 *   FILE TYPE = VSAM,KSDS                                        *
00013 *   RECORD SIZE = 160   RECFORM = FIX                            *
00014 *                                                                *
00015 *   BASE CLUSTER NAME = ERNAME                    RKP=2,LEN=61   *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  NAME-LOOKUP-MASTER.
00024      12  NL-RECORD-ID                PIC  X(02).
00025          88  VALID-NL-ID                         VALUE 'NL'.
00026
00027      12  NL-RECORD-KEY.
00028          16  NL-CONTROL-PRIMARY.
00029              20  NL-COMPANY-CD       PIC  X(01).
00030              20  NL-NAME             PIC  X(30).
00031              20  NL-RECORD-TYPE      PIC  X(01).
00032                  88  NL-ACCOUNT-TYPE             VALUE 'A'.
00033                  88  NL-COMPENSATION-TYPE        VALUE 'C'.
00034                  88  NL-REINSURANCE-TYPE         VALUE 'R'.
00035
00036          16  NL-ACCOUNT-MASTER.
00037              20  NL-AM-COMPANY-CD    PIC  X(01).
00038              20  NL-AM-CARRIER       PIC  X(01).
00039              20  NL-AM-GROUPING      PIC  X(06).
00040              20  NL-AM-STATE         PIC  X(02).
00041              20  NL-AM-ACCOUNT       PIC  X(10).
00042              20  FILLER              PIC  X(09).
00043
00044          16  NL-COMPENSATION-MASTER
00045                                  REDEFINES  NL-ACCOUNT-MASTER.
00046              20  NL-CO-COMPANY-CD    PIC  X(01).
00047              20  NL-CO-CARRIER       PIC  X(01).
00048              20  NL-CO-GROUPING      PIC  X(06).
00049              20  NL-CO-RESP-NO       PIC  X(10).
00050              20  NL-CO-ACCOUNT       PIC  X(10).
00051              20  NL-CO-TYPE          PIC  X(01).
00052
00053          16  NL-REINSURANCE-RECORD
00054                                  REDEFINES  NL-ACCOUNT-MASTER.
00055              20  NL-RE-COMPANY-CD    PIC  X(01).
00056              20  NL-RE-CODE          PIC  X(01).
00057              20  NL-RE-COMPANY.
00058                  24  NL-RE-COMP      PIC  X(03).
00059                  24  NL-RE-CO-SUB    PIC  X(03).
00060              20  NL-RE-TABLE         PIC  X(03).
00061              20  FILLER              PIC  X(18).
00062
00063      12  NL-MAINT-INFORMATION.
00064          16  NL-LAST-MAINT-DT        PIC  X(02).
00065          16  NL-LAST-MAINT-HHMMSS    PIC S9(07)  COMP-3.
00066          16  NL-LAST-MAINT-USER      PIC  X(04).
00067          16  FILLER                  PIC  X(10).
00068
00069      12  NL-RE-LEVELS  OCCURS  30  TIMES.
00070          16  NL-RE-LEVEL             PIC  9(02).
00071
00072      12  NL-CITY                     PIC  X(15).
00073      12  NL-ST                       PIC  XX.
00074
00075 ******************************************************************
00354  EJECT
00355 *                            COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
00356  EJECT
00357 *                            COPY ERCSUMM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCSUMM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = AR SUMMARY CROSS REFERENCE                *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 150           RECFORM = FIXED                  *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERSUMM                   RKP=2,LEN=34    *
00015 *                                                                *
00016 *       ALTERNATE PATH1 = ERSUMM2  (BY CO SUMMARY CARR           *
00017 *                                      GROUP F.R. AGENT)         *
00018 *                                                 RKP=36 ,LEN=34 *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 ******************************************************************
00024
00025  01  SUMM-CROSS-REFERENCE.
00026      12  SX-RECORD-ID                PIC XX.
00027          88  VALID-SX-ID             VALUE 'SX'.
00028
00029      12  SX-CONTROL-PRIMARY.
00030          16  SX-COMPANY-CD           PIC X.
00031          16  SX-SUMMARY              PIC X(6).
00032          16  SX-CARRIER              PIC X.
00033          16  SX-GROUP                PIC X(6).
00034          16  SX-FIN-RESP             PIC X(10).
00035          16  SX-ACCT-AGENT           PIC X(10).
00036
00037      12  SX-CONTROL-A1.
00038          16  SX-COMPANY-A1           PIC X.
00039          16  SX-ACCT-AGENT-A1        PIC X(10).
00040          16  SX-SUMMARY-A1           PIC X(6).
00041          16  SX-CARR-A1              PIC X.
00042          16  SX-GROUP-A1             PIC X(6).
00043          16  SX-FIN-RESP-A1          PIC X(10).
00044
00045      12  SX-MAINT-INFORMATION.
00046          16  SX-LAST-MAINT-DT        PIC XX.
00047          16  SX-LAST-MAINT-BY        PIC X(4).
00048          16  SX-LAST-MAINT-HHMMSS    PIC S9(7)  COMP-3.
00049
00050      12  SX-SUMM-OR-AGT-NAME         PIC X(30).
00051
00052      12  FILLER                      PIC X(40).
00053
00054 ******************************************************************
00358  EJECT
00359 *                            COPY ERCRQST.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCRQST.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACCOUNTS RECEIVABLE REQUEST RECORD        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERRQST                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH1 = ERRQST2  (BY CO, CAR, GROUP, ST,       *
00014 *                                   ACCOUNT, REF, BATCH)         *
00015 *                                                RKP=9, LEN=38   *
00016 *       ALTERNATE PATH2 = ERRQST3  (BY CO, CAR, GROUP, FIN  RESP *
00017 *                                   ACCOUNT, REF, BATCH)         *
00018 *                                                RKP=47, LEN=46  *
00019 *       ALTERNATE PATH3 = ERRQST4  (BY CO, CAR, GROUP, AGENT,    *
00020 *                                   BATCH)                       *
00021 *                                                RKP=93, LEN=24  *
00022 *       ALTERNATE PATH4 = ERRQST5  (BY CO, SUMMARY CODE, ACCT,   *
00023 *                                   REF, BATCH)                  *
00024 *                                                RKP=117, LEN=35 *
00025 *   LOG = NO                                                     *
00026 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00027 ******************************************************************
00028
00029  01  AR-REQUEST-RECORD.
00030      12  RQ-RECORD-ID                     PIC XX.
00031          88  VALID-RQ-ID                        VALUE 'RQ'.
00032
00033      12  RQ-CONTROL-PRIMARY.
00034          16  RQ-COMPANY-CD                PIC X.
00035          16  RQ-ENTRY-BATCH               PIC X(6).
00036
00037      12  RQ-CONTROL-BY-ACCT-REF.
00038          16  RQ-COMPANY-CD-A1             PIC X.
00039          16  RQ-CARRIER-A1                PIC X.
00040          16  RQ-GROUPING-A1               PIC X(6).
00041          16  RQ-STATE-A1                  PIC XX.
00042          16  RQ-ACCOUNT-A1                PIC X(10).
00043          16  RQ-REFERENCE-A1              PIC X(12).
00044          16  RQ-BATCH-A1                  PIC X(6).
00045
00046      12  RQ-CONTROL-BY-FIN-RESP.
00047          16  RQ-COMPANY-CD-A2             PIC X.
00048          16  RQ-CARRIER-A2                PIC X.
00049          16  RQ-GROUPING-A2               PIC X(6).
00050          16  RQ-FIN-RESP-A2               PIC X(10).
00051          16  RQ-ACCT-AGENT-A2             PIC X(10).
00052          16  RQ-REFERENCE-A2              PIC X(12).
00053          16  RQ-BATCH-A2                  PIC X(6).
00054
00055      12  RQ-CONTROL-BY-ACCT-AGENT.
00056          16  RQ-COMPANY-CD-A3             PIC X.
00057          16  RQ-CARRIER-A3                PIC X.
00058          16  RQ-GROUPING-A3               PIC X(6).
00059          16  RQ-ACCT-AGENT-A3             PIC X(10).
00060          16  RQ-BATCH-A3                  PIC X(6).
00061
00062      12  RQ-CONTROL-BY-SUMMARY.
00063          16  RQ-COMPANY-CD-A4             PIC X.
00064          16  RQ-SUMMARY-CODE              PIC X(6).
00065          16  RQ-ACCOUNT-A4                PIC X(10).
00066          16  RQ-REFERENCE-A4              PIC X(12).
00067          16  RQ-BATCH-A4                  PIC X(6).
00068
00069      12  RQ-REQUEST-METHOD                PIC X.
00070          88 RQ-FIN-RESP-REQUEST               VALUE 'F'.
00071          88 RQ-ACCT-AGENT-REQUEST             VALUE 'A'.
00072          88 RQ-SUMMARY-REQUEST                VALUE 'S'.
00073          88 RQ-BATCH-REQUEST                  VALUE 'B'.
00074      12  FILLER                           PIC X.
00075      12  RQ-STATUS                        PIC X.
00076          88  RQ-REQUEST-ERROR                 VALUE 'E'.
00077          88  RQ-RESUBMIT                      VALUE 'R'.
00078      12  RQ-PROCESSOR-ID                  PIC X(4).
00079      12  RQ-ENTRY-DT                      PIC XX.
00080      12  RQ-MO-END-DT                     PIC XX.
00081      12  RQ-REQUEST-DT                    PIC XX.
00082      12  RQ-STMT-DT                       PIC XX.
00083      12  RQ-REVERSAL-DT                   PIC XX.
00084      12  RQ-CREDIT-SELECT-DT              PIC XX.
00085      12  RQ-CREDIT-ACCEPT-DT              PIC XX.
00086
00087      12  FILLER                           PIC X(27).
00088
00089 ******************************************************************
00090
00360  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                COMPENSATION-MASTER AGENT-COMMISSIONS
                                NAME-LOOKUP-MASTER CONTROL-FILE
                                SUMM-CROSS-REFERENCE
                                AR-REQUEST-RECORD.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL652' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00362
00363      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00364      MOVE '5'                    TO  DC-OPTION-CODE.
00365      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
00366      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00367      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00368
00369      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00370      MOVE EIBTRMID               TO  QID-TERM.
00371
00372  1000-START.
00373      IF EIBCALEN = ZERO
00374          GO TO 8800-UNAUTHORIZED-ACCESS.
00375
00376      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00377          MOVE PI-CALLING-PROGRAM        TO  RETURNED-FROM.
00378
00379      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00380          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00381              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00382              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00383              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00384              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00385              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00386              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00387              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00388              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00389              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
00390          ELSE
00391              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00392              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00393              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00394              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00395              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00396              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00397              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00398              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00399
00400      IF  RETURNED-FROM = XCTL-633
00401              OR
00402          RETURNED-FROM = XCTL-633DMD
00403              OR
00404          RETURNED-FROM = XCTL-635
00405              OR
00406          RETURNED-FROM = XCTL-6521
00407              OR
102202         RETURNED-FROM = XCTL-6522
102202             OR
111103         RETURNED-FROM = XCTL-6523
111103             OR
111204         RETURNED-FROM = XCTL-6524
111204             OR
00408          RETURNED-FROM = XCTL-689
00409              OR
00410          RETURNED-FROM = XCTL-690
CIDMOD             OR
CIDMOD         RETURNED-FROM = XCTL-650
00411          GO TO 3475-RECOVER-TEMP-STOR-PI.
00412
00413      
      * EXEC CICS HANDLE CONDITION
00414 *        NOTOPEN   (9990-ABEND)
00415 *        NOTFND    (8880-NOT-FOUND)
00416 *        PGMIDERR  (9600-PGMID-ERROR)
00417 *        ERROR     (9990-ABEND)
00418 *    END-EXEC.
      *    MOVE '"$JIL.                ! " #00004155' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034313535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00419
00420      IF  EIBTRNID NOT = TRANS-ID
00421          MOVE LOW-VALUES         TO  EL652AI
00422          IF  EIBTRNID = EM6508-TRANS-ID
00423              MOVE DFHENTER       TO  EIBAID
00424              MOVE 'S'            TO  MAINTYPI
00425              MOVE PI-CARRIER     TO  CARRIERI
00426              MOVE PI-GROUPING    TO  GROUPI
00427              MOVE 'A'            TO  TYPEI
00428              MOVE PI-CR-FIN-RESP TO  FINRESPI
00429              MOVE PI-CR-ACCOUNT  TO  ACCTNOI
00430              MOVE 1              TO  CARRIERL TYPEL MAINTYPL
00431              MOVE 6              TO  GROUPL
00432              MOVE 10             TO  FINRESPL  ACCTNOL
00433              GO TO 4000-EDIT-MAINT
00434          ELSE
00435              IF  (EIBTRNID NOT = EL640-TRANS-ID AND
00436                                  EL633-TRANS-ID AND
00437                                  EL633DMD-TRANS-ID AND
00438                                  EL635-TRANS-ID AND
00439                                  EL642-TRANS-ID AND
00440                                  EL6501-TRANS-ID AND
00441                                  EL6592-TRANS-ID AND
00442                                  EL856-TRANS-ID)
00443                  OR PI-CR-FIN-RESP = SPACES
00444                  GO TO 8100-SEND-INITIAL-MAP
00445              ELSE
00446                  MOVE DFHENTER   TO  EIBAID
00447                  MOVE 'S'        TO  MAINTYPI
00448                  MOVE PI-CR-CARRIER  TO  CARRIERI
00449                  MOVE PI-CR-GROUPING TO  GROUPI
00450                  MOVE PI-CR-TYPE     TO  TYPEI
00451                  MOVE PI-CR-FIN-RESP TO  FINRESPI
00452                  MOVE PI-CR-ACCOUNT  TO  ACCTNOI
00453                  MOVE 1              TO  CARRIERL
00454                                          TYPEL
00455                                          MAINTYPL
00456                  MOVE 6              TO  GROUPL
00457                  MOVE 10             TO  FINRESPL
00458                                          ACCTNOL
00459                  GO TO 4000-EDIT-MAINT.
00460
00461      IF  EIBAID = DFHCLEAR
00462              OR
00463          NOT DISPLAY-CAP
00464          GO TO 9400-CLEAR.
00465
00466  EJECT
00467  2000-RECEIVE.
00468      MOVE LOW-VALUES             TO  EL652AI.
00469
00470      IF EIBAID = DFHPA1 OR  DFHPA2  OR  DFHPA3
00471          MOVE ER-0008            TO  EMI-ERROR
00472          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00473          MOVE -1                 TO  MAINTYPL
00474          GO TO 8200-SEND-DATAONLY.
00475
00476      
      * EXEC CICS RECEIVE
00477 *        MAP     (MAP-NAME)
00478 *        MAPSET  (MAPSET-NAME)
00479 *        INTO    (EL652AI)
00480 *    END-EXEC.
           MOVE LENGTH OF
            EL652AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004218' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL652AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00481
00482      IF PFKEYL = ZERO
00483          GO TO 3000-CHECK-PFKEYS.
00484
00485      IF EIBAID NOT = DFHENTER
00486          MOVE ER-0004            TO EMI-ERROR
00487          GO TO 3100-INPUT-ERROR.
00488
00489      IF (PFKEYI NUMERIC) AND (PFKEYI > 0 AND < 25)
00490          MOVE PF-VALUES (PFKEYI)  TO  EIBAID
00491      ELSE
00492          MOVE ER-0029             TO  EMI-ERROR
00493          GO TO 3100-INPUT-ERROR.
00494  EJECT
00495  3000-CHECK-PFKEYS.
00496      IF EIBAID = DFHPF23
00497          GO TO 8810-PF23.
00498
00499      IF EIBAID = DFHPF24
00500          GO TO 9200-RETURN-MAIN-MENU.
00501
00502      IF EIBAID = DFHPF12
00503          GO TO 9500-PF12.
00504
pemtst     if (maintypl not = zeros)
pemtst        and (maintypi not = 'D')
pemtst        move spaces              to pi-el652-del-sw
pemtst     end-if
00505      IF EIBAID = DFHPF1
00506          IF PI-CHECK-MAINT-TYPE = 'C' OR 'D'
00507              MOVE 'S'            TO  MAINTYPO
00508              MOVE AL-UANON       TO  MAINTYPA
00509              MOVE 1              TO  MAINTYPL
00510              GO TO 7250-PAGE-FORWARD
00511          ELSE
00512              GO TO 7250-PAGE-FORWARD.
00513
00514      IF EIBAID = DFHPF2
00515          IF PI-CHECK-MAINT-TYPE = 'C' OR 'D'
00516              MOVE 'S'            TO  MAINTYPO
00517              MOVE AL-UANON       TO  MAINTYPA
00518              MOVE 1              TO  MAINTYPL
00519              GO TO 7300-PAGE-BACKWARD
00520          ELSE
00521              GO TO 7300-PAGE-BACKWARD.
00522
00523      IF EIBAID = DFHPF3
00524         IF PI-AR-PROCESSING
00525             IF PI-ERCOMP-KEY  NOT =       SPACES
00526                 MOVE PI-ERC-CARRIER TO  PI-CR-CARRIER
00527                 MOVE PI-ERC-GROUP   TO  PI-CR-GROUPING
00528                 MOVE PI-ERC-RESP    TO  PI-CR-FIN-RESP
00529                 MOVE PI-ERC-ACCT    TO  PI-CR-ACCOUNT
00530                 MOVE PI-ERC-TYPE    TO  PI-CR-TYPE
00531                 IF PI-CR-ACCOUNT =  LOW-VALUES
00532                    MOVE 'G'         TO  PI-CR-TYPE
00533                                         PI-ERC-TYPE
00534                    PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00535                    MOVE XCTL-635    TO PGM-NAME
00536                    GO TO 9300-XCTL
00537                 ELSE
00538                    MOVE 'A'         TO  PI-CR-TYPE
00539                                         PI-ERC-TYPE
00540                    PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00541                    MOVE XCTL-635    TO PGM-NAME
00542                    GO TO 9300-XCTL
00543             ELSE
00544                 MOVE ER-3021        TO  EMI-ERROR
00545                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00546                 GO TO 8100-SEND-INITIAL-MAP
00547         ELSE
00548             IF PI-ERCOMP-KEY NOT = SPACES AND LOW-VALUES
00549                 MOVE PI-ERC-CARRIER TO  PI-CR-CARRIER
00550                 MOVE PI-ERC-GROUP   TO  PI-CR-GROUPING
00551                 MOVE PI-ERC-RESP    TO  PI-CR-FIN-RESP
00552                 MOVE PI-ERC-ACCT    TO  PI-CR-ACCOUNT
033105                MOVE PI-ERC-TYPE    TO  PI-CR-TYPE
00553                 IF PI-CR-ACCOUNT =  LOW-VALUES
00554 *                  MOVE 'G'         TO  PI-CR-TYPE
00555 *                                       PI-ERC-TYPE
00556                    PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00557                    IF PI-COMPANY-ID = 'DMD'
00558                       MOVE XCTL-633DMD TO PGM-NAME
00559                       GO TO 9300-XCTL
00560                    ELSE
00561                       MOVE XCTL-633    TO PGM-NAME
00562                       GO TO 9300-XCTL
00563                 ELSE
00564                    MOVE 'A'         TO  PI-CR-TYPE
00565                                         PI-ERC-TYPE
00566                    PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00567                    IF PI-COMPANY-ID = 'DMD'
00568                       MOVE XCTL-633DMD TO PGM-NAME
00569                       GO TO 9300-XCTL
00570                    ELSE
00571                       MOVE XCTL-633    TO PGM-NAME
00572                       GO TO 9300-XCTL
00573             ELSE
00574                 MOVE ER-3021        TO  EMI-ERROR
00575                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00576                 GO TO 8100-SEND-INITIAL-MAP.
00577
00578      IF EIBAID = DFHPF4
033105         IF PI-ERC-TYPE = 'G' OR 'A' OR 'B'
00580              PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00581              MOVE XCTL-6521      TO PGM-NAME
00582              GO TO 9300-XCTL
00583          ELSE
00584              MOVE ER-2872        TO  EMI-ERROR
00585              GO TO 3100-INPUT-ERROR.
00586
00587      IF  EIBAID = DFHPF5
00588          MOVE XCTL-689           TO PGM-NAME
00589          IF  PI-ERCOMP-KEY NOT = SPACES
00590              MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
00591              MOVE PI-ERC-GROUP   TO PI-CR-GROUPING
00592              MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP
00593              MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
033105             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
00594
00595              IF  PI-CR-ACCOUNT =  LOW-VALUES
00596 *                MOVE 'G'        TO PI-CR-TYPE, PI-ERC-TYPE
00597                  PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00598                  MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA
00599                  GO TO 9300-XCTL
00600
00601              ELSE
00602                  MOVE 'A'        TO PI-CR-TYPE, PI-ERC-TYPE
00603                  PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00604                  MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA
00605                  GO TO 9300-XCTL
00606
00607          ELSE
00608              PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00609              MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA
00610              GO TO 9300-XCTL.
00611
00612      IF  EIBAID = DFHPF6
00613
00614          MOVE XCTL-690           TO PGM-NAME
00615
00616          IF  PI-ERCOMP-KEY NOT = SPACES
00617              MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
00618              MOVE PI-ERC-GROUP   TO PI-CR-GROUPING
00619              MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP
00620              MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
033105             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
00621
00622              IF  PI-CR-ACCOUNT =  LOW-VALUES
00623 *                MOVE 'G'        TO PI-CR-TYPE, PI-ERC-TYPE
00624                  PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00625                  MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA
00626                  GO TO 9300-XCTL
00627
00628              ELSE
00629                  MOVE 'A'        TO PI-CR-TYPE, PI-ERC-TYPE
00630                  PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00631                  MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA
00632                  GO TO 9300-XCTL
00633
00634          ELSE
00635              PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT
00636              MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA
00637              GO TO 9300-XCTL.
00638
CIDMOD     IF EIBAID = DFHPF7
CIDMOD         IF PI-ERC-TYPE = 'A'
CIDMOD            MOVE PI-ERC-CARRIER  TO PI-CR-CARRIER
CIDMOD            MOVE PI-ERC-GROUP    TO PI-CR-GROUPING
CIDMOD            MOVE PI-ERC-ACCT     TO PI-CR-ACCOUNT
CIDMOD*           PERFORM 3200-FIND-STATE
CIDMOD*                                THRU 3200-EXIT
CIDMOD            MOVE PI-SAVE-STATE   TO PI-CR-STATE
CIDMOD            PERFORM 3400-CREATE-TS-PI
CIDMOD                                 THRU 3400-EXIT
CIDMOD            MOVE XCTL-650        TO PGM-NAME
CIDMOD            GO TO 9300-XCTL
CIDMOD         ELSE
CIDMOD            MOVE ER-0035         TO EMI-ERROR
CIDMOD            GO TO 3100-INPUT-ERROR
CIDMOD         END-IF
CIDMOD     END-IF.
00586
102202     IF EIBAID = DFHPF8
102202         IF PI-ERCOMP-KEY NOT = SPACES
102202             MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
102202             MOVE PI-ERC-GROUP   TO PI-CR-GROUPING
102202             MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP
102202             MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
102202             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
102202             PERFORM 3400-CREATE-TS-PI  THRU 3400-EXIT
102202             MOVE XCTL-6522             TO PGM-NAME
102202             GO TO 9300-XCTL
102202         ELSE
111103            MOVE ER-0187                TO EMI-ERROR
102202            GO TO 3100-INPUT-ERROR
102202         END-IF
102202     END-IF.
102202
00639 *****************************************************************
00640 *  SPECIAL CODE - ENABLES DISPLAY OF PRIOR MONTH-END TOTALS*
00641 *  PF9 AVAILABLE TO A/R USERS                                   *
00642 *****************************************************************
00643
00644      IF PI-AR-PROCESSING OR PI-PROCESSOR-ID = 'LGXX'
00645          IF EIBAID = DFHPF9
00646              MOVE 'Y'            TO  WS-SHOW-SAVE-TOTALS
00647              GO TO 4000-EDIT-MAINT.
00648
111103     IF EIBAID = DFHPF10
033105        IF PI-ERC-TYPE = 'B'
111103         IF PI-ERCOMP-KEY NOT = SPACES
111103             MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
111103             MOVE PI-ERC-GROUP   TO PI-CR-GROUPING
111103             MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP
111103             MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
111103             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
111103             PERFORM 3400-CREATE-TS-PI  THRU 3400-EXIT
111103             MOVE XCTL-6523             TO PGM-NAME
111103             GO TO 9300-XCTL
111103         ELSE
111103            MOVE ER-0187                TO EMI-ERROR
111103            GO TO 3100-INPUT-ERROR
111103         END-IF
033105        ELSE
CIDMOD           MOVE ER-0035          TO EMI-ERROR
CIDMOD           GO TO 3100-INPUT-ERROR
033105        END-IF
111103     END-IF
111204     IF EIBAID = DFHPF13
033105        IF PI-ERC-TYPE = 'B'
111204           IF PI-ERCOMP-KEY NOT = SPACES
111204              MOVE PI-ERC-CARRIER TO PI-CR-CARRIER
111204              MOVE PI-ERC-GROUP  TO PI-CR-GROUPING
111204              MOVE PI-ERC-RESP   TO PI-CR-FIN-RESP
111204              MOVE PI-ERC-ACCT   TO PI-CR-ACCOUNT
111204              MOVE PI-ERC-TYPE   TO PI-CR-TYPE
111204              PERFORM 3400-CREATE-TS-PI
111204                                 THRU 3400-EXIT
111204              MOVE XCTL-6524     TO PGM-NAME
111204              GO TO 9300-XCTL
111204           ELSE
111204              MOVE ER-0187       TO EMI-ERROR
111204              GO TO 3100-INPUT-ERROR
111204           END-IF
033105        ELSE
CIDMOD           MOVE ER-0035          TO EMI-ERROR
CIDMOD           GO TO 3100-INPUT-ERROR
033105        END-IF
111204     END-IF
00649      IF EIBAID = DFHENTER
00650          GO TO 4000-EDIT-MAINT.
00651
00652      MOVE ER-0029                TO  EMI-ERROR.
00653
00654  3100-INPUT-ERROR.
00655      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00656
00657      MOVE AL-UNBON               TO  PFKEYA.
00658      MOVE -1                     TO  PFKEYL.
00659
00660      GO TO 8200-SEND-DATAONLY.
00661  EJECT
CIDMOD 3200-FIND-STATE.
CIDMOD
CIDMOD     IF PI-SAVE-CITYST NOT = SPACES
CIDMOD        MOVE PI-SAVE-CITYST       TO WS-SRCH-STATE
CIDMOD     ELSE
CIDMOD        IF PI-SAVE-ADDR2 NOT = SPACES
CIDMOD           MOVE PI-SAVE-ADDR2     TO WS-SRCH-STATE
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
CIDMOD     PERFORM VARYING STNDX FROM +29 BY -1 UNTIL
CIDMOD          (STNDX < +1) OR
CIDMOD          ((WS-SRCH-STATE (STNDX:2) ALPHABETIC) AND
CIDMOD          (WS-SRCH-STATE (STNDX:2) NOT = SPACES AND LOW-VALUES)
CIDMOD          AND
CIDMOD          (WS-SRCH-STATE (STNDX + 1:1) NOT = ' ' AND ',' AND
CIDMOD                   '.' AND LOW-VALUES) AND
CIDMOD          (WS-SRCH-STATE (STNDX:1) NOT = ' ' AND ',' AND
CIDMOD                   '.' AND LOW-VALUES))
CIDMOD     END-PERFORM
CIDMOD
CIDMOD     IF STNDX NOT < +1
CIDMOD        MOVE WS-SRCH-STATE (STNDX:2)
CIDMOD                                 TO PI-SAVE-STATE
CIDMOD     ELSE
CIDMOD        MOVE SPACES              TO PI-SAVE-STATE
CIDMOD     END-IF
CIDMOD
CIDMOD     .
CIDMOD 3200-EXIT.
CIDMOD     EXIT.
00662  3400-CREATE-TS-PI.
00663
00664      PERFORM 3450-DELETE-TEMP-STOR-PI THRU 3450-EXIT.
00665
00666      
      * EXEC CICS HANDLE CONDITION
00667 *        QIDERR  (3400-EXIT)
00668 *    END-EXEC.
      *    MOVE '"$N                   ! # #00004521' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034353231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00669
00670      
      * EXEC CICS WRITEQ TS
00671 *        QUEUE   (QID-PI)
00672 *        FROM    (PROGRAM-INTERFACE-BLOCK)
00673 *        LENGTH  (PI-COMM-LENGTH)
00674 *        ITEM    (QID-ITEM)
00675 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00004525' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID-PI, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00676
00677  3400-EXIT.
00678       EXIT.
00679
00680  3450-DELETE-TEMP-STOR-PI.
00681
00682      
      * EXEC CICS HANDLE CONDITION
00683 *        QIDERR  (3450-EXIT)
00684 *    END-EXEC.
      *    MOVE '"$N                   ! $ #00004537' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034353337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00685
00686      
      * EXEC CICS DELETEQ TS
00687 *        QUEUE  (QID-PI)
00688 *    END-EXEC.
      *    MOVE '*&                    #   #00004541' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID-PI, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00689
00690      
      * EXEC CICS SYNCPOINT
00691 *    END-EXEC.
      *    MOVE '6"                    !   #00004545' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00692
00693  3450-EXIT.
00694       EXIT.
00695
00696  3475-RECOVER-TEMP-STOR-PI.
00697
00698      MOVE LOW-VALUES            TO  EL652AI.
00699
00700      
      * EXEC CICS HANDLE CONDITION
00701 *        NOTOPEN   (9990-ABEND)
00702 *        NOTFND    (8880-NOT-FOUND)
00703 *        PGMIDERR  (9600-PGMID-ERROR)
00704 *        ERROR     (9990-ABEND)
00705 *    END-EXEC.
      *    MOVE '"$JIL.                ! % #00004555' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034353535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00706
00707      
      * EXEC CICS HANDLE CONDITION
00708 *        QIDERR  (3475-EXIT)
00709 *    END-EXEC.
      *    MOVE '"$N                   ! & #00004562' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034353632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00710
00711      
      * EXEC CICS READQ TS
00712 *        QUEUE   (QID-PI)
00713 *        ITEM    (1)
00714 *        INTO    (PROGRAM-INTERFACE-BLOCK)
00715 *        LENGTH  (PI-COMM-LENGTH)
00716 *    END-EXEC.
           MOVE 1
             TO DFHEIV11
      *    MOVE '*$II   L              ''   #00004566' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID-PI, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00717
00718      PERFORM 3450-DELETE-TEMP-STOR-PI THRU 3450-EXIT.
00719
00720      IF  PI-ERCOMP-KEY = SPACES OR LOW-VALUES
00721          GO TO 8100-SEND-INITIAL-MAP.
00722
00723      IF  PI-ERC-CARRIER > LOW-VALUES
00724          MOVE +1                 TO CARRIERL
00725          MOVE PI-ERC-CARRIER     TO CARRIERI.
00726
00727      IF  PI-ERC-GROUP > LOW-VALUES
00728          MOVE +6                 TO GROUPL
00729          MOVE PI-ERC-GROUP       TO GROUPI.
00730
00731      IF  PI-ERC-RESP > LOW-VALUES
00732          MOVE +10                TO FINRESPL
00733          MOVE PI-ERC-RESP        TO FINRESPI.
00734
00735      IF  PI-ERC-ACCT > LOW-VALUES
00736          MOVE +1                 TO ACCTNOL
00737          MOVE PI-ERC-ACCT        TO ACCTNOI.
00738
00739      IF  PI-ERC-TYPE > LOW-VALUES
00740          MOVE +1                 TO TYPEL
00741          MOVE PI-ERC-TYPE        TO TYPEI.
00742
00743      MOVE 'S'                    TO MAINTYPI.
00744      MOVE 1                      TO MAINTYPL.
00745      GO TO 4000-EDIT-MAINT.
00746
00747  3475-EXIT.
00748       EXIT.
00749  EJECT
00750  4000-EDIT-MAINT.
00751      IF MAINTYPL > ZERO
00752          MOVE MAINTYPI           TO  PI-CHECK-MAINT-TYPE
00753          IF VALID-MAINT-TYPE
00754              MOVE AL-UANON       TO  MAINTYPA
00755              IF MAINTYPI NOT = 'S'
00756                  MOVE SPACES     TO  WS-ACCESS
00757                  MOVE '1'        TO  CNTL-REC-TYPE
00758                  PERFORM 7400-READ-CONTROL-FILE  THRU  7499-EXIT
00759              ELSE
00760                  NEXT SENTENCE
00761          ELSE
00762              MOVE -1             TO  MAINTYPL
00763              MOVE AL-UABON       TO  MAINTYPA
00764              MOVE ER-2039        TO  EMI-ERROR
00765              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00766      ELSE
00767          MOVE -1                 TO  MAINTYPL
00768          MOVE AL-UABON           TO  MAINTYPA
00769          MOVE ER-2039            TO  EMI-ERROR
00770          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00771
00772      IF  NOT MODIFY-CAP
00773              AND
00774          NOT SHOW-FUNCTION
00775          MOVE 'UPDATE'           TO SM-READ
00776          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00777          IF  MORTGAGE-SESSION
00778              MOVE ER-9096        TO  EMI-ERROR
00779              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00780              GO TO 8100-SEND-INITIAL-MAP
00781          ELSE
00782              MOVE ER-0070        TO  EMI-ERROR
00783              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00784              GO TO 8100-SEND-INITIAL-MAP.
00785
00786      IF CARRIERL > ZERO
00787          IF PI-CARRIER-SECURITY > SPACES
00788              IF CARRIERI = PI-CARRIER-SECURITY
00789                  NEXT SENTENCE
00790              ELSE
00791                  MOVE -1            TO  CARRIERL
00792                  MOVE ER-2370       TO  EMI-ERROR
00793                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00794                  MOVE AL-UABON      TO  CARRIERA
00795                  GO TO 8200-SEND-DATAONLY.
00796
00797      IF CARRIERL > ZERO
00798          IF ADD-FUNCTION
00799              IF PI-ZERO-CARRIER
00800                OR PI-ZERO-CAR-GROUP
00801                  MOVE ZEROS      TO  PI-ERC-CARRIER
00802                                      CARRIERI
00803                  MOVE AL-UANON   TO  CARRIERA
00804              ELSE
00805                  MOVE CARRIERI   TO  WS-CARRIER
00806                  MOVE '6'        TO  CNTL-REC-TYPE
00807                  PERFORM 7400-READ-CONTROL-FILE  THRU  7499-EXIT
00808          ELSE
00809              IF PI-ZERO-CARRIER
00810                OR PI-ZERO-CAR-GROUP
00811                  MOVE ZEROS      TO  PI-ERC-CARRIER
00812                                      CARRIERI
00813                  MOVE AL-UANON   TO  CARRIERA
00814              ELSE
00815                  MOVE AL-UANON   TO  CARRIERA
00816                  MOVE CARRIERI   TO  PI-ERC-CARRIER
00817      ELSE
00818          IF ADD-FUNCTION
00819              IF PI-ZERO-CARRIER
00820                OR PI-ZERO-CAR-GROUP
00821                  MOVE ZEROS      TO  PI-ERC-CARRIER
00822                                      CARRIERI
00823                  MOVE AL-UANON   TO  CARRIERA
00824              ELSE
00825                  MOVE -1         TO  CARRIERL
00826                  MOVE AL-UABON   TO  CARRIERA
00827                  MOVE ER-0193    TO  EMI-ERROR
00828                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00829          ELSE
00830              MOVE -1             TO  CARRIERL
00831              MOVE AL-UABON       TO  CARRIERA
00832              MOVE ER-0193        TO  EMI-ERROR
00833              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00834
00835      IF GROUPL > ZERO
00836          IF PI-ZERO-GROUPING
00837            OR PI-ZERO-CAR-GROUP
00838              MOVE ZEROS          TO  PI-ERC-GROUP
00839                                      GROUPI
00840              MOVE AL-UANON       TO  GROUPA
00841          ELSE
00842              MOVE AL-UANON       TO  GROUPA
00843              MOVE GROUPI         TO  PI-ERC-GROUP
00844      ELSE
00845          IF ADD-FUNCTION
00846              IF PI-ZERO-GROUPING
00847                OR PI-ZERO-CAR-GROUP
00848                  MOVE ZEROS      TO  PI-ERC-GROUP
00849                                      GROUPI
00850                  MOVE AL-UANON   TO  GROUPA
00851              ELSE
00852                  MOVE LOW-VALUES  TO  PI-ERC-GROUP
00853          ELSE
00854              MOVE LOW-VALUES     TO  PI-ERC-GROUP.
00855
00856      IF TYPEL > ZERO
00857          MOVE TYPEI              TO  PI-CHECK-TYPE
00858          IF VALID-TYPE
00859              MOVE AL-UANON       TO  TYPEA
00860              MOVE TYPEI          TO  PI-ERC-TYPE
00861          ELSE
00862              MOVE -1             TO  TYPEL
00863              MOVE AL-UABON       TO  TYPEA
00864              MOVE ER-2042        TO  EMI-ERROR
00865              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00866      ELSE
00867          MOVE -1                 TO  TYPEL
00868          MOVE AL-UABON           TO  TYPEA
00869          MOVE ER-2042            TO  EMI-ERROR
00870          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00871
00872      IF FINRESPL >        ZERO
00873          MOVE AL-UANON           TO  FINRESPA
00874          IF FINRESPI = SPACES
00875              MOVE LOW-VALUES     TO  PI-ERC-RESP
00876          ELSE
00877              MOVE FINRESPI       TO  PI-ERC-RESP
00878      ELSE
00879          MOVE LOW-VALUES         TO  PI-ERC-RESP.
00880
00881      IF ACCTNOL > ZERO
00882          MOVE AL-UANON           TO  ACCTNOA
00883          IF ACCTNOI = SPACES
00884              MOVE LOW-VALUES     TO  PI-ERC-ACCT
00885          ELSE
00886              MOVE ACCTNOI        TO  PI-ERC-ACCT
00887      ELSE
00888          MOVE LOW-VALUES         TO  PI-ERC-ACCT.
00889
CIDMOD     IF ADDR2L > ZERO
CIDMOD        MOVE AL-UANON            TO ADDR2A
CIDMOD        IF ADDR2I = SPACES
CIDMOD           MOVE LOW-VALUES       TO PI-SAVE-ADDR2
CIDMOD        ELSE
CIDMOD           MOVE ADDR2I           TO PI-SAVE-ADDR2
CIDMOD        END-IF
CIDMOD     ELSE
CIDMOD        MOVE LOW-VALUES          TO PI-SAVE-ADDR2
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CITYL > ZERO
CIDMOD        MOVE AL-UANON            TO CITYA
CIDMOD        IF CITYI = SPACES
CIDMOD           MOVE LOW-VALUES       TO PI-SAVE-CITY
CIDMOD        ELSE
CIDMOD           MOVE CITYI            TO PI-SAVE-CITY
CIDMOD        END-IF
CIDMOD     ELSE
CIDMOD        MOVE LOW-VALUES          TO PI-SAVE-CITY
CIDMOD     END-IF
CIDMOD
CIDMOD     IF STATEL > ZERO
CIDMOD        MOVE AL-UANON            TO STATEA
CIDMOD        IF STATEI = SPACES
CIDMOD           MOVE LOW-VALUES       TO PI-SAVE-STATE
CIDMOD        ELSE
CIDMOD           MOVE STATEI           TO PI-SAVE-STATE
CIDMOD        END-IF
CIDMOD     ELSE
CIDMOD        MOVE LOW-VALUES          TO PI-SAVE-STATE
CIDMOD     END-IF
CIDMOD
00890      IF NOT MODIFY-CAP
00891          IF SHOW-FUNCTION
00892              GO TO 5000-BUILD-INITIAL-SCREEN
00893          ELSE
00894              MOVE 'UPDATE'       TO SM-READ
00895              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00896              MOVE ER-0070        TO  EMI-ERROR
00897              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00898              GO TO 8100-SEND-INITIAL-MAP.
00899
00900      IF EMI-NO-ERRORS
00901          NEXT SENTENCE
00902      ELSE
00903         IF EIBTRNID NOT = TRANS-ID
00904             GO TO 8100-SEND-INITIAL-MAP
00905          ELSE
00906             GO TO 8200-SEND-DATAONLY.
00907
00908      IF CHANGE-FUNCTION
00909          GO TO 4400-CHANGE.
00910
00911      IF DELETE-FUNCTION
00912          GO TO 4600-DELETE.
00913
00914      IF SHOW-FUNCTION
00915          GO TO 5000-BUILD-INITIAL-SCREEN.
00916
00917      IF TYPEI = 'C'
00918          IF FINRESPI NOT = LOW-VALUES OR
00919             ACCTNOI  NOT = LOW-VALUES
00920                MOVE -1           TO  FINRESPL
00921                MOVE AL-UABON     TO  FINRESPA
00922                                      ACCTNOA
00923                MOVE ER-2088      TO  EMI-ERROR
00924                PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00925
00926      IF TYPEI = 'A'
00927          IF FINRESPI = LOW-VALUES OR
00928              ACCTNOI = LOW-VALUES
00929                MOVE -1           TO  FINRESPL
00930                MOVE AL-UABON     TO  FINRESPA
00931                                      ACCTNOA
00932                MOVE ER-2089      TO  EMI-ERROR
00933                PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00934
033105     IF TYPEI = 'G' OR 'B'
00936          IF ACCTNOI NOT = LOW-VALUES
00937              MOVE -1             TO  ACCTNOL
00938              MOVE AL-UABON       TO  ACCTNOA
00939              MOVE ER-2091        TO  EMI-ERROR
00940              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00941          ELSE
00942              IF FINRESPI = LOW-VALUES
00943                  MOVE -1         TO  FINRESPL
00944                  MOVE AL-UABON   TO  FINRESPA
00945                  MOVE ER-2097    TO  EMI-ERROR
00946                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00947
033105     IF TYPEI = 'G' OR 'B'
00949          IF SUMMNOI NOT = LOW-VALUES
00950              MOVE -1             TO  SUMMNOL
00951              MOVE AL-UABON       TO  SUMMNOA
00952              MOVE ER-3153        TO  EMI-ERROR
00953              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00954
033105     IF TYPEI = 'G' OR 'B'
00956          IF NETGRSI NOT = LOW-VALUES
00957              MOVE -1             TO  NETGRSL
00958              MOVE AL-UABON       TO  NETGRSA
00959              MOVE ER-3154        TO  EMI-ERROR
00960              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00961
00962      IF EMI-NO-ERRORS
00963          NEXT SENTENCE
00964      ELSE
00965          GO TO 8200-SEND-DATAONLY.
00966
00967      IF ADD-FUNCTION
00968          GO TO 4200-ADD.
00969
00970      MOVE -1                     TO  MAINTYPL.
00971      MOVE ER-2056                TO  EMI-ERROR.
00972      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00973
00974      GO TO 8200-SEND-DATAONLY.
00975
00976  4000-EXIT.
00977      EXIT.
00978  EJECT
00979  4200-ADD.
00980      IF PI-ERCOMP-KEY = PI-SAVE-ERCOMP-KEY
00981          MOVE ER-2057            TO  EMI-ERROR
00982          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00983          MOVE -1                 TO  MAINTYPL
00984          GO TO 8200-SEND-DATAONLY.
00985
00986      PERFORM 7000-EDIT  THRU  7000-EXIT.
00987
00988      IF EMI-NO-ERRORS
00989          NEXT SENTENCE
00990      ELSE
00991          GO TO 8200-SEND-DATAONLY.
00992
00993      
      * EXEC CICS HANDLE CONDITION
00994 *        NOTOPEN  (9990-ABEND)
00995 *        NOTFND   (4250-CONT)
00996 *    END-EXEC.
      *    MOVE '"$JI                  ! '' #00004881' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034383831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00997
00998      PERFORM 7050-READ-ERCOMP  THRU  7050-EXIT.
00999
01000      MOVE ER-2057                TO  EMI-ERROR.
01001      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01002
01003      MOVE LOW-VALUES             TO  PI-SAVE-ERCOMP-KEY.
01004
01005      MOVE -1                     TO  MAINTYPL.
01006
01007      GO TO 8200-SEND-DATAONLY.
01008
01009  4250-CONT.
01010      PERFORM 7150-ERCOMP-GETMAIN  THRU  7150-EXIT.
01011
01012      MOVE SPACES                 TO  COMPENSATION-MASTER.
01013      MOVE ZEROS                  TO  CO-LF-CLM-AMT
01014                                      CO-AH-CLM-AMT
01015                                      CO-CUR-FICA
01016                                      CO-YTD-FICA
01017                                      CO-CUR-OVR-UNDR
01018                                      CO-YTD-OVR-UNDR
092205                                     CO-MAX-BANK-FEE-LEASE
111103                                     CO-MAX-BANK-FEE
01019                                      CO-BAL-FWD
01020                                      CO-CUR-COM
01021                                      CO-CUR-CHG
01022                                      CO-CUR-PMT
01023                                      CO-END-BAL
01024                                      CO-CUR
01025                                      CO-OV30
01026                                      CO-OV60
01027                                      CO-OV90
080612                                     co-ov120
01028                                      CO-YTD-COM
01029                                      CO-YTD-OV
01030                                      CO-YTD-PAID-COM
01031                                      CO-YTD-PAID-OV
01032                                      CO-CURRENT-BAL-FWD
01033                                      CO-CURRENT-CUR-COM
01034                                      CO-CURRENT-CUR-CHG
01035                                      CO-CURRENT-CUR-PMT
01036                                      CO-CURRENT-END-BAL
01037                                      CO-CURRENT-CUR
01038                                      CO-CURRENT-OV30
01039                                      CO-CURRENT-OV60
01040                                      CO-CURRENT-OV90
080612                                     co-current-ov120
01041                                      CO-CURRENT-YTD-COM
01042                                      CO-CURRENT-YTD-OV
01043                                      CO-ACT-YEAR
01044                                      CO-ACT-MONTH
01045                                      CO-ACT-DAY.
01046      MOVE LOW-VALUES             TO  CO-LAST-ACTIVITY-DATE
01047                                      CO-LAST-STMT-DT
01048                                      CO-CURRENT-LAST-STMT-DT
01049                                      CO-GA-EFFECTIVE-DT
01050                                      CO-GA-TERMINATION-DT.
041106     MOVE LOW-VALUES             TO  CO-FIRST-WRITTEN-DT.
01051
01052      MOVE 'N'                    TO  CO-INTERNAL-CONTROL-1
01053                                      CO-INTERNAL-CONTROL-2.
01055      PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.
01056
01057      MOVE PI-PROCESSOR-ID        TO  CO-LAST-MAINT-USER.
01058      MOVE EIBTIME                TO  CO-LAST-MAINT-HHMMSS.
01059      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01060      MOVE '5'                    TO  DC-OPTION-CODE.
01061      MOVE LINK-ELDATCV           TO  PGM-NAME.
01062
01063      
      * EXEC CICS LINK
01064 *        PROGRAM   (PGM-NAME)
01065 *        COMMAREA  (DATE-CONVERSION-DATA)
01066 *        LENGTH    (DC-COMM-LENGTH)
01067 *    END-EXEC.
      *    MOVE '."C                   (   #00004955' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01068
01069      MOVE DC-BIN-DATE-1          TO  CO-LAST-MAINT-DT
01070                                      BIN-CURRENT-SAVE.
01071      MOVE PI-CR-MONTH-END-DT     TO  CO-ROLADEX-PRINT-DT.
01072      MOVE PI-CR-MONTH-END-DT     TO  DC-BIN-DATE-1
01073      MOVE SPACE                  TO  DC-OPTION-CODE.
01074
01075      
      * EXEC CICS LINK
01076 *        PROGRAM   (PGM-NAME)
01077 *        COMMAREA  (DATE-CONVERSION-DATA)
01078 *        LENGTH    (DC-COMM-LENGTH)
01079 *    END-EXEC.
      *    MOVE '."C                   (   #00004967' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01080
01081      MOVE PI-COMPANY-CD          TO  CO-COMPANY-CD.
01082      MOVE 'CO'                   TO  CO-RECORD-ID.
01083      MOVE 'A'                    TO  JP-RECORD-TYPE.
01084      MOVE COMP-FILE-ID           TO  FILE-ID.
01085      MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.
01086
01087      PERFORM 4300-ADD-COMPENSATION-NAME  THRU  4300-EXIT.
01088
01089      
      * EXEC CICS WRITE
01090 *        DATASET  (COMP-FILE-ID)
01091 *        FROM     (COMPENSATION-MASTER)
01092 *        RIDFLD   (CO-CONTROL-PRIMARY)
01093 *    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004981' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01094
01095      PERFORM 8400-LOG-JOURNAL-RECORD.
01096
01097      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.
01098
01099      IF PI-AR-PROCESSING
01100          IF SUMMNOL > ZERO
01101              MOVE SUMMNOI        TO PI-AR-SUMMARY-CODE
01102              PERFORM 7500-UPDATE-SUMM  THRU  7599-EXIT
01103              MOVE PI-AR-SUMMARY-CODE
01104                                  TO  WS-SUMM-FOR-RQST
01105              PERFORM 6500-UPDATE-RQST  THRU  6599-EXIT.
01106
01107      MOVE ER-0000                TO  EMI-ERROR.
01108      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01109
01110      MOVE LOW-VALUES             TO  EL652AO.
01111      MOVE PI-ERC-CARRIER         TO  CARRIERO.
01112      MOVE PI-ERC-TYPE            TO  TYPEO.
01113      MOVE AL-UANON               TO  CARRIERA
01114                                      TYPEA.
01115
01116      IF PI-ERC-GROUP NOT = SPACES
01117          MOVE PI-ERC-GROUP       TO  GROUPO
01118          MOVE AL-UANON           TO  GROUPA.
01119
01120      IF PI-ERC-RESP NOT = SPACES
01121          MOVE PI-ERC-RESP        TO  FINRESPO
01122          MOVE AL-UANON           TO  FINRESPA.
01123
01124      IF PI-ERC-ACCT NOT = SPACES
01125          MOVE PI-ERC-ACCT        TO  ACCTNOO
01126          MOVE AL-UANON           TO  ACCTNOA.
01127
01128      GO TO 5000-BUILD-INITIAL-SCREEN.
01129
01130  4200-EXIT.
01131      EXIT.
01132  EJECT
01133  4300-ADD-COMPENSATION-NAME.
01134      
      * EXEC CICS HANDLE CONDITION
01135 *         DUPREC   (4300-EXIT)
01136 *    END-EXEC.
      *    MOVE '"$%                   ! ( #00005026' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01137
01138      
      * EXEC CICS GETMAIN
01139 *         LENGTH   (ERNAME-LENGTH)
01140 *         SET      (ADDRESS OF NAME-LOOKUP-MASTER)
01141 *         INITIMG  (GETMAIN-SPACE)
01142 *    END-EXEC.
      *    MOVE ',"IL                  $   #00005030' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERNAME-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01143
01144      MOVE SPACES                 TO  NAME-LOOKUP-MASTER.
01145      MOVE BIN-CURRENT-SAVE       TO  NL-LAST-MAINT-DT.
01146      MOVE PI-PROCESSOR-ID        TO  NL-LAST-MAINT-USER.
01147      MOVE EIBTIME                TO  NL-LAST-MAINT-HHMMSS.
01148      MOVE PI-COMPANY-CD          TO  NL-COMPANY-CD.
01149      MOVE CO-ACCT-NAME           TO  NL-NAME.
012407     IF NL-NAME (1:4) = 'THE ' OR 'The ' OR 'the '
012407        MOVE NL-NAME (5:26)      TO NL-NAME
012407     END-IF
01150      MOVE CO-ADDR-CITY           TO  NL-CITY.
           MOVE CO-ADDR-STATE          TO  NL-ST
01151      MOVE 'C'                    TO  NL-RECORD-TYPE.
01152      MOVE PI-COMPANY-CD          TO  NL-CO-COMPANY-CD.
01153      MOVE CO-CARRIER             TO  NL-CO-CARRIER.
01154      MOVE CO-GROUPING            TO  NL-CO-GROUPING.
01155      MOVE CO-RESP-NO             TO  NL-CO-RESP-NO.
01156      MOVE CO-ACCOUNT             TO  NL-CO-ACCOUNT.
01157      MOVE CO-TYPE                TO  NL-CO-TYPE.
01158
01159      
      * EXEC CICS WRITE
01160 *        FROM      (NAME-LOOKUP-MASTER)
01161 *        RIDFLD    (NL-CONTROL-PRIMARY)
01162 *        DATASET   (NAME-FILE-ID)
01163 *    END-EXEC.
           MOVE LENGTH OF
            NAME-LOOKUP-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005055' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NAME-FILE-ID, 
                 NAME-LOOKUP-MASTER, 
                 DFHEIV11, 
                 NL-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01164
01165  4300-EXIT.
01166      EXIT.
01167  EJECT
01168  4400-CHANGE.
01169      IF PI-ERCOMP-KEY = PI-SAVE-ERCOMP-KEY
01170          NEXT SENTENCE
01171      ELSE
111103**** MUST SHOW RECORD FIRST
01172          MOVE ER-2056            TO  EMI-ERROR
01173          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01174          MOVE -1                 TO  MAINTYPL
01175          GO TO 8200-SEND-DATAONLY.
01176
01177      PERFORM 7000-EDIT  THRU  7000-EXIT.
01178
01179      IF EMI-NO-ERRORS
01180          NEXT SENTENCE
01181      ELSE
01182          GO TO 8200-SEND-DATAONLY.
01183
01184      PERFORM 7200-READ-ERCOMP-UPDATE  THRU  7200-EXIT.
01185
01186      MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.
01187      MOVE CO-AR-SUMMARY-CODE     TO  WS-SAVE-SUMM.
01188
01189      PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.
01190
01191      IF (CO-LAST-MAINT-USER   = PI-UPDATE-BY)
01192         OR (CO-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
01193         CONTINUE
01194      ELSE
01195         
      * EXEC CICS UNLOCK
01196 *            DATASET  (COMP-FILE-ID)
01197 *       END-EXEC
      *    MOVE '&*                    #   #00005092' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01198         MOVE ER-0068             TO EMI-ERROR
01199         PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
01200         GO TO 8200-SEND-DATAONLY
           END-IF
01201
01202      MOVE PI-PROCESSOR-ID        TO  CO-LAST-MAINT-USER.
01203      MOVE EIBTIME                TO  CO-LAST-MAINT-HHMMSS.
01204      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01205      MOVE '5'                    TO  DC-OPTION-CODE.
01206      MOVE LINK-ELDATCV           TO  PGM-NAME.
01207
01208      
      * EXEC CICS LINK
01209 *        PROGRAM   (PGM-NAME)
01210 *        COMMAREA  (DATE-CONVERSION-DATA)
01211 *        LENGTH    (DC-COMM-LENGTH)
01212 *    END-EXEC.
      *    MOVE '."C                   (   #00005107' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01213
01214      MOVE DC-BIN-DATE-1          TO  CO-LAST-MAINT-DT
01215                                      BIN-CURRENT-SAVE.
01216      MOVE SPACE                  TO  DC-OPTION-CODE.
01217      MOVE PI-CR-MONTH-END-DT     TO  CO-ROLADEX-PRINT-DT.
01218      MOVE PI-CR-MONTH-END-DT     TO  DC-BIN-DATE-1
01219
01220      
      * EXEC CICS LINK
01221 *        PROGRAM   (PGM-NAME)
01222 *        COMMAREA  (DATE-CONVERSION-DATA)
01223 *        LENGTH    (DC-COMM-LENGTH)
01224 *    END-EXEC.
      *    MOVE '."C                   (   #00005119' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01225
01226      MOVE 'N'                    TO  CO-INTERNAL-CONTROL-1.
01227      MOVE 'B'                    TO  JP-RECORD-TYPE.
01228      MOVE COMP-FILE-ID           TO  FILE-ID.
01229
01230      PERFORM 8400-LOG-JOURNAL-RECORD.
01231
01232      MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.
01233
01234      PERFORM 4300-ADD-COMPENSATION-NAME  THRU  4300-EXIT.
01235
01236      
      * EXEC CICS REWRITE
01237 *        DATASET  (COMP-FILE-ID)
01238 *        FROM     (COMPENSATION-MASTER)
01239 *    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005135' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01240
01241      MOVE 'C'                    TO  JP-RECORD-TYPE.
01242
01243      PERFORM 8400-LOG-JOURNAL-RECORD.
01244
01245      IF NOT PI-AR-PROCESSING
01246          GO TO 4400-CONT.
01247
01248      IF SUMMNOL NOT > ZERO
01249          GO TO 4400-CONT.
01250
01251      IF SUMMNOI  = WS-SAVE-SUMM
01252          GO TO 4400-CONT
01253      ELSE
01254          PERFORM 7600-DELETE-SUMM  THRU  7699-EXIT.
01255
01256      MOVE SUMMNOI                 TO WS-SUMM-FOR-RQST.
01257      PERFORM 6500-UPDATE-RQST  THRU  6599-EXIT.
01258
01259      IF SUMMNOI = SPACES
01260          GO TO 4400-CONT.
01261
01262      MOVE SUMMNOI                 TO PI-AR-SUMMARY-CODE.
01263      PERFORM 7500-UPDATE-SUMM  THRU  7599-EXIT.
01264
01265  4400-CONT.
01266      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.
01267
01268      MOVE ER-0000                TO  EMI-ERROR.
01269      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01270
01271      MOVE LOW-VALUES             TO  EL652AO.
01272      MOVE PI-ERC-CARRIER         TO  CARRIERO.
01273      MOVE PI-ERC-TYPE            TO  TYPEO.
01274      MOVE AL-UANON               TO  CARRIERA
01275                                      TYPEA.
01276
01277      IF PI-ERC-GROUP NOT = SPACES
01278          MOVE PI-ERC-GROUP       TO  GROUPO
01279          MOVE AL-UANON           TO  GROUPA.
01280
01281      IF PI-ERC-RESP NOT = SPACES
01282          MOVE PI-ERC-RESP        TO  FINRESPO
01283          MOVE AL-UANON           TO  FINRESPA.
01284
01285      IF PI-ERC-ACCT NOT = SPACES
01286          MOVE PI-ERC-ACCT        TO  ACCTNOO
01287          MOVE AL-UANON           TO  ACCTNOA.
01288
01289      GO TO 5000-BUILD-INITIAL-SCREEN.
01290
01291  4400-EXIT.
01292      EXIT.
01293  EJECT
01294  4600-DELETE.
01295      IF PI-ERCOMP-KEY = PI-SAVE-ERCOMP-KEY
01296          NEXT SENTENCE
01297      ELSE
01298          MOVE ER-2056            TO  EMI-ERROR
01299          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01300          MOVE -1                 TO  MAINTYPL
01301          GO TO 8200-SEND-DATAONLY.
01302
01303      PERFORM 7200-READ-ERCOMP-UPDATE  THRU  7200-EXIT.
01304
01305      MOVE CO-AR-SUMMARY-CODE     TO  WS-SAVE-SUMM.
01306
01307      IF (CO-YTD-COM NOT = ZERO) OR
01308         (CO-END-BAL NOT = ZERO)
01309          
      * EXEC CICS UNLOCK
01310 *            DATASET  (COMP-FILE-ID)
01311 *        END-EXEC
      *    MOVE '&*                    #   #00005208' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01312          MOVE ER-2092            TO  EMI-ERROR
01313          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01314          MOVE -1                 TO  MAINTYPL
01315          GO TO 8200-SEND-DATAONLY.
01316
01317      MOVE 'D'                    TO  JP-RECORD-TYPE.
01318      MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.
01319      MOVE COMP-FILE-ID           TO  FILE-ID.
01320
01321      IF CO-LAST-MAINT-USER   = PI-UPDATE-BY OR
01322         CO-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
01323          continue
01324      ELSE
01325          
      * EXEC CICS UNLOCK
01326 *            DATASET  (COMP-FILE-ID)
01327 *        END-EXEC
      *    MOVE '&*                    #   #00005224' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01328          MOVE ER-0068            TO  EMI-ERROR
01329          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01330          GO TO 8200-SEND-DATAONLY
01331      end-if
pemtst     if pi-el652-del-sw = 'Y'
              continue
pemtst     else
pemtst        move 'Y'                 to pi-el652-del-sw
pemtst        move er-1299             to emi-error
pemtst        perform 9900-error-format thru 9900-exit
pemtst        move spaces              to maintypo
pemtst        MOVE -1                  TO MAINTypL
pemtst        GO TO 8200-send-dataonly
01337      end-if
01332      
      * EXEC CICS DELETE
01333 *        DATASET  (COMP-FILE-ID)
01334 *    END-EXEC
      *    MOVE '&(                    &   #00005241' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01335
01336      PERFORM 8400-LOG-JOURNAL-RECORD
01338      MOVE EIBDATE                TO  DC-JULIAN-YYDDD
01339      MOVE '5'                    TO  DC-OPTION-CODE
01340      MOVE LINK-ELDATCV           TO  PGM-NAME
01341
01342      
      * EXEC CICS LINK
01343 *        PROGRAM   (PGM-NAME)
01344 *        COMMAREA  (DATE-CONVERSION-DATA)
01345 *        LENGTH    (DC-COMM-LENGTH)
01346 *    END-EXEC
      *    MOVE '."C                   (   #00005250' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01347
01348      MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE
01349
01350      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT
01351
01352      MOVE ER-0000                TO  EMI-ERROR
01353      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01354
01355      IF PI-AR-PROCESSING
01356          IF WS-SAVE-SUMM NOT = SPACES
01357              PERFORM 7600-DELETE-SUMM  THRU  7699-EXIT
01358              MOVE SPACES         TO  WS-SUMM-FOR-RQST
01359              PERFORM  6500-UPDATE-RQST  THRU  6599-EXIT
               end-if
           end-if
01360
01361      MOVE LOW-VALUES             TO  EL652AO
01362      MOVE PI-ERC-CARRIER         TO  CARRIERO
01363      MOVE PI-ERC-TYPE            TO  TYPEO
01364      MOVE AL-UANON               TO  CARRIERA
01365                                      TYPEA
01366
01367      IF PI-ERC-GROUP NOT = SPACES
01368          MOVE PI-ERC-GROUP       TO  GROUPO
01369          MOVE AL-UANON           TO  GROUPA
           end-if
01370
01371      IF PI-ERC-RESP NOT = SPACES
01372          MOVE PI-ERC-RESP        TO  FINRESPO
01373          MOVE AL-UANON           TO  FINRESPA
           end-if
01374
01375      IF PI-ERC-ACCT NOT = SPACES
01376          MOVE PI-ERC-ACCT        TO  ACCTNOO
01377          MOVE AL-UANON           TO  ACCTNOA
           end-if
01378
01379      MOVE LOW-VALUES             TO  PI-SAVE-ERCOMP-KEY
01380
01381      GO TO 8100-SEND-INITIAL-MAP
01382      .
01383  4600-EXIT.
01384      EXIT.
01385  EJECT
111103
111103 4700-CHECK-STATE.
111103     IF CLPSTI =  SPACES
111103         GO TO 4799-EXIT
111103     END-IF.
111103
111103     MOVE SPACES                 TO  ELCNTL-KEY2.
111103     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID-2.
111103     MOVE '3'                    TO  CNTL-REC-TYPE-2.
111103     MOVE CLPSTI                 TO  CNTL-STATE-2.
111103     MOVE +0                     TO  CNTL-SEQ-NO-2.
111103
111103     
      * EXEC CICS  HANDLE CONDITION
111103*        NOTFND  (4750-NO-STATE)
111103*    END-EXEC.
      *    MOVE '"$I                   ! ) #00005311' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035333131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
111103
111103     
      * EXEC CICS  READ
111103*        DATASET  (CNTL-FILE-ID)
111103*        SET      (ADDRESS OF CONTROL-FILE)
111103*        RIDFLD   (ELCNTL-KEY2)
111103*    END-EXEC.
      *    MOVE '&"S        E          (   #00005315' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333135' TO DFHEIV0(25:11)
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
           
111103
111103     GO TO 4799-EXIT.
111103
111103 4750-NO-STATE.
111103     MOVE ER-0144                TO  EMI-ERROR.
111103     MOVE -1                     TO  CLPSTL.
111103     MOVE AL-UABON               TO  CLPSTA.
111103
111103     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
111103
111103 4799-EXIT.
111103     EXIT.
111103
01386  5000-BUILD-INITIAL-SCREEN.
01387      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
01388
01389      PERFORM 7050-READ-ERCOMP  THRU  7050-EXIT.
01390
01391      MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.
01392      MOVE CO-LAST-MAINT-USER     TO  PI-UPDATE-BY.
01393
01394      IF CO-LAST-MAINT-HHMMSS NUMERIC
01395         MOVE CO-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS
01396         ELSE
01397         MOVE ZEROS                  TO  PI-UPDATE-HHMMSS.
01398
01399  5050-SET-UP-SCREEN.
CIDMOD     MOVE CO-CONTROL-NAME        TO  PCONTO.
01400      MOVE CO-MAIL-NAME           TO  MAILNAMO.
01401      MOVE CO-ACCT-NAME           TO  ACCTNAMO.
102202     MOVE CO-ACCT-NAME           TO  PI-SAVE-ACCT-NAME.
01402      MOVE CO-ADDR-1              TO  ADDR1O.
01403      MOVE CO-ADDR-2              TO  ADDR2O
CIDMOD                                     PI-SAVE-ADDR2
01404      MOVE CO-ADDR-CITY           TO  CITYO
CIDMOD                                     PI-SAVE-CITY
01404      MOVE CO-ADDR-STATE          TO  STATEO
CIDMOD                                     PI-SAVE-STATE
01405      MOVE SPACES                 TO  WS-ZIP-CODE.
01406
01407      IF CO-CANADIAN-POST-CODE
01408          MOVE CO-CAN-POSTAL-1    TO  WS-ZIP-CAN-2-POST1
01409          MOVE CO-CAN-POSTAL-2    TO  WS-ZIP-CAN-2-POST2
01410      ELSE
01411          MOVE CO-ZIP-PRIME       TO  WS-ZIP-AM-2-CODE
01412          IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
01413              MOVE '-'            TO  WS-ZIP-AM-2-DASH
01414              MOVE CO-ZIP-PLUS4   TO  WS-ZIP-AM-2-PLUS4.
01414
01415      MOVE WS-ZIP-CODE            TO  ZIPCODEO.
CIDMOD
CIDMOD     IF CO-BILL-SW = ' ' OR 'B' OR 'R' OR 'T' OR 'S'
060506        OR 'O' OR 'E' OR 'C'
CIDMOD         MOVE AL-UANON           TO  BILLPRTA
CIDMOD         MOVE CO-BILL-SW         TO  BILLPRTO
CIDMOD     END-IF.
CIDMOD
LGC186     MOVE CO-CSO-1099            TO  PNT1099O.
LGC186
01417      MOVE CO-CSR-CODE            TO  CSRO.
01418
020816     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
011410        AND (CO-TYPE = 'A')
011410        IF CO-COMP-TYPE = '1'
011410           MOVE 'Y'              TO SPPDDO
011410        ELSE
011410           MOVE 'N'              TO SPPDDO
011410        END-IF
011410     END-IF
01428      MOVE CO-SOC-SEC             TO  SSNO.
01429
01430      MOVE CO-BALANCE-CONTROL     TO  CARBALO.
01431
01432      IF PI-AR-PROCESSING
01433          IF CO-AR-SUMMARY-CODE > SPACES
01434              MOVE AL-UANON       TO  SUMMNOA
01435              MOVE CO-AR-SUMMARY-CODE
01436                                  TO  SUMMNOO
01437          ELSE
01438              MOVE LOW-VALUES     TO  SUMMNOO.
01439
01440      IF PI-AR-PROCESSING
01441         IF NOT CO-ACCOUNT-TYPE
01442            MOVE AL-SADOF         TO SCDESCA
01443            MOVE AL-SANOF         TO SUMMNOA
01444         ELSE
01445            MOVE AL-UANOF         TO SUMMNOA.
01446
01447      IF PI-AR-PROCESSING
071712*        MOVE CO-AR-BAL-LEVEL    TO  ARBALO
01449          MOVE CO-AR-PULL-CHECK   TO  CKPULLO.
01450
01451      IF PI-AR-PROCESSING
01452          IF CO-ACCOUNT-TYPE
01453              MOVE CO-AR-REPORTING
01454                                  TO  NETGRSO
01455          ELSE
01456              MOVE SPACES         TO  NETGRSO.
01457
01461      MOVE SPACES                 TO  FLITYPO.
01462
01469      IF CO-COMPANY-TYPE
01470          MOVE SPACES             TO  RPTCD2O
01471          MOVE AL-SADOF           TO  RPTCDDA.
01472
01482      MOVE SPACES                 TO  LETRCDO.
01483      MOVE AL-SADOF               TO  LETDESCA.
01494      MOVE SPACES                 TO  BALCDO.
01495      MOVE AL-SADOF               TO  BALPRTA.
111103     MOVE CO-CLP-STATE           TO  CLPSTO.
111103     MOVE CO-MAX-BANK-FEE        TO  MAXFEEO
072406     MOVE CO-SPP-REFUND-EDIT     TO  REFEO
092205     IF CO-MAX-BANK-FEE-LEASE NOT NUMERIC
092205        MOVE +0                  TO CO-MAX-BANK-FEE-LEASE
092205     END-IF
092205     MOVE CO-MAX-BANK-FEE-LEASE  TO  MAXLFO
           .
01497  5060-BUILD-TOTALS.
01498      IF SHOW-SAVE-TOTALS
01499          NEXT SENTENCE
01500      ELSE
01501          GO TO 5070-BUILD-CURRENT-TOTALS.
01502
01503      IF CO-LAST-STMT-DT = SPACES
01504          MOVE SPACES             TO  LSTMDTO
01505          GO TO 5060-LST-STMT-OK.
01506
01507      MOVE CO-LAST-STMT-MONTH     TO  WS-YMD-MM.
01508      MOVE CO-LAST-STMT-DAY       TO  WS-YMD-DD.
01509      MOVE CO-LAST-STMT-YEAR      TO  WS-YMD-YY.
01510
01511      IF WS-YMD-DATE-NUM NUMERIC
01512          NEXT SENTENCE
01513      ELSE
01514          MOVE SPACES             TO  LSTMDTO
01515          GO TO 5060-LST-STMT-OK.
01516
01517      MOVE WS-YMD-DATE            TO  DC-GREG-DATE-1-YMD.
01518      MOVE '3'                    TO  DC-OPTION-CODE.
01519
01520      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
01521
01522      IF NO-CONVERSION-ERROR
01523          NEXT SENTENCE
01524      ELSE
01525          MOVE SPACES             TO  LSTMDTO
01526          GO TO 5060-LST-STMT-OK.
01527
01528      MOVE CO-LAST-STMT-MONTH     TO  WS-DMDY8-MM.
01529      MOVE CO-LAST-STMT-DAY       TO  WS-DMDY8-DD.
01530      MOVE CO-LAST-STMT-YEAR      TO  WS-DMDY8-YY.
01531      MOVE '/'                    TO  WS-DMDY8-SL1
01532                                      WS-DMDY8-SL2.
01533      MOVE WS-DATE-MDY-8          TO  LSTMDTO.
01534
01535  5060-LST-STMT-OK.
01536      MOVE CO-BAL-FWD             TO  BALFWDO.
01537      MOVE CO-CUR-COM             TO  CURCOMO.
01538      MOVE CO-CUR-PMT             TO  CURPMTO.
01539      MOVE CO-CUR-CHG             TO  CURCHGO.
01540      MOVE CO-END-BAL             TO  ENDBALO.
01541      MOVE CO-CUR                 TO  CURRENTO.
01542      MOVE CO-OV30                TO  OVER30O.
01543      MOVE CO-OV60                TO  OVER60O.
01544      MOVE CO-OV90                TO  OVER90O.
080612     if pi-company-id = 'AHL'
080612        if co-ov120 not numeric
080612           move zeros            to co-ov120
080612        end-if
080612        move co-ov120            to over120o
080612     end-if
01546      IF CO-ACCOUNT-TYPE
01547         MOVE CO-YTD-COM          TO YTDCOMO
01548      ELSE
01549         MOVE CO-YTD-OV           TO YTDCOMO
           END-IF
01550
01551      MOVE AL-SADOF               TO  PFK9A.
01552      MOVE 'PREV. END-OF-MONTH TOTALS DISPLAYED'
01553                                  TO  EMI-MESSAGE-AREA (1).
01554
01555      GO TO 5090-CONTD.
01556
01557  5070-BUILD-CURRENT-TOTALS.
01558      IF CO-CURRENT-LAST-STMT-DT = SPACES
01559          MOVE SPACES             TO  LSTMDTO
01560          GO TO 5070-LST-STMT-OK.
01561
01562      MOVE CO-CURRENT-LAST-STMT-MONTH
01563                                  TO WS-YMD-MM.
01564      MOVE CO-CURRENT-LAST-STMT-DAY
01565                                  TO  WS-YMD-DD.
01566      MOVE CO-CURRENT-LAST-STMT-YEAR
01567                                  TO  WS-YMD-YY.
01568
01569      IF WS-YMD-DATE-NUM NUMERIC
01570          NEXT SENTENCE
01571      ELSE
01572          MOVE SPACES             TO  LSTMDTO
01573          GO TO 5070-LST-STMT-OK.
01574
01575      MOVE WS-YMD-DATE            TO  DC-GREG-DATE-1-YMD.
01576      MOVE '3'                    TO  DC-OPTION-CODE.
01577
01578      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
01579
01580      IF NO-CONVERSION-ERROR
01581          NEXT SENTENCE
01582      ELSE
01583          MOVE SPACES             TO  LSTMDTO
01584          GO TO 5070-LST-STMT-OK.
01585
01586      MOVE CO-CURRENT-LAST-STMT-MONTH
01587                                  TO  WS-DMDY8-MM.
01588      MOVE CO-CURRENT-LAST-STMT-DAY
01589                                  TO  WS-DMDY8-DD.
01590      MOVE CO-CURRENT-LAST-STMT-YEAR
01591                                  TO  WS-DMDY8-YY.
01592      MOVE '/'                    TO  WS-DMDY8-SL1
01593                                      WS-DMDY8-SL2.
01594      MOVE WS-DATE-MDY-8          TO  LSTMDTO.
01595
01596  5070-LST-STMT-OK.
01597      MOVE CO-CURRENT-BAL-FWD     TO  BALFWDO.
01598      MOVE CO-CURRENT-CUR-COM     TO  CURCOMO.
01599      MOVE CO-CURRENT-CUR-PMT     TO  CURPMTO.
01600      MOVE CO-CURRENT-CUR-CHG     TO  CURCHGO.
01601      MOVE CO-CURRENT-END-BAL     TO  ENDBALO
01602                                      PI-ERC-END-BAL.
01603      MOVE CO-CURRENT-CUR         TO  CURRENTO.
01604      MOVE CO-CURRENT-OV30        TO  OVER30O.
01605      MOVE CO-CURRENT-OV60        TO  OVER60O.
01606      MOVE CO-CURRENT-OV90        TO  OVER90O.
080612     if pi-company-id = 'AHL'
080612        if co-current-ov120 not numeric
080612           move zeros            to co-current-ov120
080612        end-if
080612        move co-current-ov120    to over120o
080612     end-if
01608      IF CO-ACCOUNT-TYPE
01609         MOVE CO-CURRENT-YTD-COM  TO YTDCOMO
01610      ELSE
01611         MOVE CO-CURRENT-YTD-OV   TO YTDCOMO
           END-IF
           .
01613  5090-CONTD.
01614
01615      MOVE CO-LAST-MAINT-USER     TO LSTUSRO.
01616
01617      MOVE ' '              TO DC-OPTION-CODE.
01618      MOVE CO-LAST-MAINT-DT TO DC-BIN-DATE-1.
01619
01620      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
01621
01622      MOVE DC-GREG-DATE-1-EDIT    TO LSTDTEO.
01623
01624      IF CO-LAST-MAINT-HHMMSS NUMERIC
01625          MOVE CO-LAST-MAINT-HHMMSS
01626                                  TO TIME-IN
01627          MOVE TIME-OUT           TO LSTTIMEO
01628      ELSE
01629          MOVE ZEROS              TO TIME-IN
01630          MOVE TIME-OUT           TO LSTTIMEO.
01631
01632      IF PI-COMPANY-ID =  'FLI'  OR  'FLU'  OR  'UCL'
01633          MOVE AL-SANOF           TO  FLITYPEA
01634          MOVE AL-UANON           TO  FLITYPA
01635      ELSE
01636          MOVE AL-SANON           TO  FLITYPA.
01637
01638      IF PI-COMPANY-ID = 'NCL'
01639          IF CO-ACCOUNT-TYPE
01640              MOVE AL-SANOF       TO  RPTCD2A
01641          ELSE
01642              IF CO-COMPANY-TYPE
01643                 MOVE AL-SANOF    TO  RPTCD2A
01644                 MOVE AL-SADOF    TO  RPTCDDA.
01645
01646      IF PI-COMPANY-ID = 'NCL'
01647          IF CO-GEN-AGENT-TYPE
01648             MOVE AL-UANON       TO  RPTCD2A.
01649
01650      IF PI-COMPANY-ID = 'NCL'
01651          MOVE AL-UANON           TO  LETRCDA
01652      ELSE
01653          MOVE AL-SANOF           TO  LETRCDA
01654          MOVE AL-SADOF           TO  LETDESCA.
01655
01656      IF PI-COMPANY-ID  NOT =  'DDB' AND 'ANT' AND 'ASL' AND
01657                               'AN1' AND 'TFS'
01658          MOVE AL-SANOF           TO  BALCDA
01659          MOVE AL-SADOF           TO  BALPRTA.
01660
01661      IF PI-AR-PROCESSING
01662          IF CO-ACCOUNT-TYPE
01663              MOVE AL-SANOF       TO  SCDESCA
01664              MOVE AL-SANOF       TO  NGDESCA
01665              MOVE AL-UANON       TO  NETGRSA
01666              IF PI-COMPANY-ID  =  'DDB' OR 'ANT' OR 'ASL' OR
01667                                   'AN1' OR 'TFS'
01668                  MOVE AL-SANOF   TO  BALPRTA
01669                  MOVE AL-UANON   TO  BALCDA.
01670
01671      IF PI-AR-PROCESSING
071712*            MOVE AL-SANOF       TO  BALDESCA
071712             move al-sanof       to  CKDESCA
071712*            MOVE AL-UANON       TO  ARBALA
071712             move al-uanon       to  CKPULLA.
01676
01677
01678      MOVE AL-UANON               TO  MAILNAMA
01679                                      ACCTNAMA
01680                                      ADDR1A
01681                                      ADDR2A
01682                                      CITYA
                                           STATEA
01683                                      SSNA
01684                                      ZIPCODEA
CIDMOD                                     BILLPRTA
LGC186                                     PNT1099A
01685                                      CARBALA.
01686
01687      IF NOT SHOW-SAVE-TOTALS
01688          IF PI-PROCESSOR-ID = 'LGXX'
01689              GO TO 5100-CONTD.
01690
01691      MOVE AL-SANOF               TO  BALFWDA
01692                                      CURCOMA
01693                                      CURPMTA
01694                                      CURCHGA
01695                                      ENDBALA.
01696
01697  5100-CONTD.
01698      IF CO-TELEPHONE NUMERIC
01699          MOVE CO-TELEPHONE       TO  WS-PHONE-IN
01700                                      PI-SAVE-PHONE
01701          MOVE WSPI-AREA          TO  WSPO-AREA
01702          MOVE WSPI-PFX           TO  WSPO-PFX
01703          MOVE WSPI-SFX           TO  WSPO-SFX
01704          MOVE WS-PHONE-OUT       TO  PHONEO
01705          MOVE AL-UNNON           TO  PHONEA
01706      ELSE
01707          MOVE SPACES             TO  PHONEO.
01708
01709      IF CO-FAXNO NUMERIC
01710          MOVE CO-FAXNO           TO  WS-PHONE-IN
01711                                      PI-SAVE-FAXNO
01712          MOVE WSPI-AREA          TO  WSPO-AREA
01713          MOVE WSPI-PFX           TO  WSPO-PFX
01714          MOVE WSPI-SFX           TO  WSPO-SFX
01715          MOVE WS-PHONE-OUT       TO  FAXNOO
01716          MOVE AL-UNNON           TO  FAXNOA
01717      ELSE
01718          MOVE SPACES             TO  FAXNOO.
01719
01720      MOVE 'S'                    TO  MAINTYPO
01721                                      PI-CHECK-MAINT-TYPE.
01722      MOVE AL-UANON               TO  MAINTYPA.
01723      MOVE -1                     TO  MAINTYPL.
01724
01725      GO TO 8100-SEND-INITIAL-MAP.
01726  EJECT
01727  6000-CHECK-FOR-UPDATE.
01728      IF CHANGE-FUNCTION
01729          GO TO 6010-CONT.
01730
01731      IF CARRIERL > ZERO
01732          MOVE CARRIERI           TO  CO-CARRIER.
01733
01734      IF GROUPL > ZERO
01735          MOVE GROUPI             TO  CO-GROUPING
01736      ELSE
01737          MOVE LOW-VALUES         TO  CO-GROUPING.
01738
01739      IF FINRESPL > ZERO
01740          MOVE FINRESPI           TO  CO-RESP-NO
01741      ELSE
01742          MOVE LOW-VALUES         TO  CO-RESP-NO.
01743
01744      IF ACCTNOL > ZERO
01745          MOVE ACCTNOI            TO  CO-ACCOUNT
01746      ELSE
01747          MOVE LOW-VALUES         TO  CO-ACCOUNT.
01748
01749      IF TYPEL > ZERO
01750          MOVE TYPEI              TO  CO-TYPE.
01751
01752  6010-CONT.
01753      IF PI-PROCESSOR-ID = 'LGXX'
01754          IF LSTMDTL > ZERO
01755              MOVE WS-YMD-YY      TO  CO-CURRENT-LAST-STMT-YEAR
01756                                      WS-DMDY8-YY
01757              MOVE WS-YMD-MM      TO  CO-CURRENT-LAST-STMT-MONTH
01758                                      WS-DMDY8-MM
01759              MOVE WS-YMD-DD      TO  CO-CURRENT-LAST-STMT-DAY
01760                                      WS-DMDY8-DD
01761              MOVE '/'            TO  WS-DMDY8-SL1  WS-DMDY8-SL2
01762              MOVE WS-DATE-MDY-8  TO  LSTMDTO.
01763
01764      IF PI-AR-PROCESSING
01765          IF TYPEI = 'A'
01766              IF SUMMNOL > ZERO
01767                  MOVE SUMMNOI   TO  CO-AR-SUMMARY-CODE
01768                                     WS-SAVE-SUMMARY.
01778
01779      IF PI-AR-PROCESSING
01780          IF TYPEI = 'A'
01781              IF NETGRSL > ZERO
01782                  MOVE NETGRSI   TO  CO-AR-REPORTING
01783              ELSE
01784                  IF ADD-FUNCTION
01788                      MOVE 'G'   TO  CO-AR-REPORTING.
01790
01791      IF PI-AR-PROCESSING
071712*        IF ARBALL > ZERO
071712*            MOVE ARBALI        TO  CO-AR-BAL-LEVEL
071712*        ELSE
01795              IF ADD-FUNCTION
01796                  MOVE '1'       TO  CO-AR-BAL-LEVEL.
01797
01798      IF PI-AR-PROCESSING
01799          IF CKPULLL > ZERO
01800              MOVE CKPULLI       TO  CO-AR-PULL-CHECK
01801          ELSE
01802              IF ADD-FUNCTION
01803                  MOVE 'Y'       TO  CO-AR-PULL-CHECK.
01804
01818      IF MAILNAML > ZERO
01819          MOVE MAILNAMI           TO  CO-MAIL-NAME
01820                                      WS-SAVE-NAME.
01821
01822      IF ACCTNAML > ZERO
01823          MOVE ACCTNAMI           TO  CO-ACCT-NAME.
01824
01825      IF ADDR1L > ZERO
01826          MOVE ADDR1I             TO  CO-ADDR-1.
01827
01828      IF ADDR2L > ZERO
01829          MOVE ADDR2I             TO  CO-ADDR-2.
01830
01831      IF CITYL > ZERO
01832          MOVE CITYI              TO  CO-ADDR-CITY.
01831      IF STATEL > ZERO
01832          MOVE STATEI             TO  CO-ADDR-STATE.
01833
01834      IF ZIPCODEL NOT > ZERO
01835          GO TO 6015-CONT.
01836
01837      MOVE ZIPCODEI               TO  WS-ZIP-CODE.
01838
01839      IF WS-CANADIAN-ZIP
01840          IF WS-ZIP-4 = SPACE  OR  '-'
01841              MOVE WS-ZIP-CAN-2-POST1     TO  CO-CAN-POSTAL-1
01842              MOVE WS-ZIP-CAN-2-POST2     TO  CO-CAN-POSTAL-2
01843          ELSE
01844              MOVE WS-ZIP-CAN-1-POST1     TO  CO-CAN-POSTAL-1
01845              MOVE WS-ZIP-CAN-1-POST2     TO  CO-CAN-POSTAL-2
01846      ELSE
01847          IF WS-ZIP-6 = SPACE  OR  '-'
01848              MOVE WS-ZIP-AM-2-CODE       TO  CO-ZIP-PRIME
01849              MOVE WS-ZIP-AM-2-PLUS4      TO  CO-ZIP-PLUS4
01850          ELSE
01851              MOVE WS-ZIP-AM-1-CODE       TO  CO-ZIP-PRIME
01852              MOVE WS-ZIP-AM-1-PLUS4      TO  CO-ZIP-PLUS4.
01853
01854  6015-CONT.
CIDMOD     IF BILLPRTL > ZERO
CIDMOD         IF BILLPRTI = ' ' OR 'B' OR 'R' OR 'T' OR 'S'
060506            OR 'O' OR 'E' OR 'C'
CIDMOD             MOVE  BILLPRTI          TO  CO-BILL-SW
CIDMOD         ELSE
CIDMOD             MOVE SPACES         TO  CO-BILL-SW
CIDMOD                                     BILLPRTO
CIDMOD         END-IF
CIDMOD     END-IF.
011410     IF SPPDDL > ZERO
011410        IF SPPDDI = 'Y'
011410           MOVE '1'              TO CO-COMP-TYPE
011410        ELSE
011410           MOVE ' '              TO CO-COMP-TYPE
011410        END-IF
011410     END-IF
LGC186     IF PNT1099L > ZERO
LGC186         IF PNT1099I = 'N' OR 'Y'
LGC186             MOVE  PNT1099I      TO  CO-CSO-1099
LGC186         ELSE
LGC186             MOVE 'N'            TO  CO-CSO-1099
LGC186                                     PNT1099O
LGC186         END-IF
LGC186     END-IF.
LGC186
01856      IF CSRL > ZERO
01857          MOVE CSRI               TO  CO-CSR-CODE.
01858
01876      IF SSNL > ZERO
01887          MOVE SSNI               TO  CO-SOC-SEC.
01888
01889      IF PHONEL > ZERO
01890          MOVE PI-SAVE-PHONE      TO  CO-TELEPHONE
01891          MOVE ZEROS              TO  PI-SAVE-PHONE.
01892
01893      IF FAXNOL > ZERO
01894          MOVE PI-SAVE-FAXNO      TO  CO-FAXNO
01895          MOVE ZEROS              TO  PI-SAVE-FAXNO.
01896
042005     IF PCONTL > ZEROS
042005        MOVE PCONTI              TO CO-CONTROL-NAME
042005     END-IF
111103     IF CLPSTL > ZERO
111103         MOVE CLPSTI             TO  CO-CLP-STATE
111103     END-IF
072406     IF REFEL > ZERO
              MOVE REFEI               TO CO-SPP-REFUND-EDIT
           END-IF
111103     IF MAXFEEL > ZERO
111103         MOVE MAXFEEI            TO  CO-MAX-BANK-FEE
      *        IF ADD-FUNCTION
      *           MOVE CO-MAX-BANK-FEE TO  CO-BANK-FEE
      *        ELSE
      *           IF CHANGE-FUNCTION
      *              PERFORM 6100-EDIT-ERAGTC
      *                                THRU 6100-EXIT
      *              IF WS-TOT-FEES > CO-MAX-BANK-FEE
      *                 MOVE ER-2717   TO EMI-ERROR
      *                 MOVE -1        TO MAXFEEL
      *                 MOVE AL-UABON  TO MAXFEEA
      *                 PERFORM 9900-ERROR-FORMAT
      *                                THRU 9900-EXIT
      *              ELSE
      *                 COMPUTE CO-BANK-FEE = CO-MAX-BANK-FEE
      *                    - WS-TOT-FEES
      *              END-IF
      *           END-IF
      *        END-IF
111103     END-IF
111103
092205     IF MAXLFL > ZERO
111103         MOVE MAXLFI             TO CO-MAX-BANK-FEE-LEASE
092205     END-IF
01897      IF CARBALL > ZERO
01898          MOVE CARBALI            TO  CO-BALANCE-CONTROL
01899          GO TO 6020-CONT.
01900
01901      IF NOT PI-AR-PROCESSING
01902          GO TO 6020-CONT.
01903
033105     IF ADD-FUNCTION
033105        AND (TYPEI = 'G' OR 'B')
01905          MOVE 'Y'                TO  CO-BALANCE-CONTROL
01906          GO TO 6020-CONT.
01907
01908      IF ADD-FUNCTION
01909          IF TYPEI = 'A'
01910              IF FINRESPI = ACCTNOI
01911                  MOVE 'Y'        TO  CO-BALANCE-CONTROL
01912              ELSE
01913                  MOVE 'N'        TO  CO-BALANCE-CONTROL.
01914
01915      IF ADD-FUNCTION AND TYPEI = 'C'
01916          MOVE 'N'                TO  CO-BALANCE-CONTROL.
01917
01918  6020-CONT.
01919      IF BALFWDL > ZERO
01920          MOVE WS-SAVE-BALFWD     TO  CO-CURRENT-BAL-FWD.
01921
01922      IF CURCOML > ZERO
01923          MOVE WS-SAVE-CURCOM     TO  CO-CURRENT-CUR-COM.
01924
01925      IF CURCHGL > ZERO
01926          MOVE WS-SAVE-CURCHG     TO  CO-CURRENT-CUR-CHG.
01927
01928      IF CURPMTL > ZERO
01929          MOVE WS-SAVE-CURPMT     TO  CO-CURRENT-CUR-PMT.
01930
01931      IF ENDBALL > ZERO
01932          MOVE WS-SAVE-ENDBAL     TO  CO-CURRENT-END-BAL.
01933
043007*    IF YTDCOML > ZERO
043007*       IF CO-ACCOUNT-TYPE
043007*          MOVE WS-SAVE-YTDCOM   TO CO-CURRENT-YTD-COM
043007*       ELSE
043007*          MOVE WS-SAVE-YTDCOM   TO CO-CURRENT-YTD-OV
043007*       END-IF
043007*    END-IF
           .
01940  6099-EXIT.
01941      EXIT.
01942  EJECT
       6100-EDIT-ERAGTC.
           
      * EXEC CICS READ
      *         SET     (ADDRESS OF AGENT-COMMISSIONS)
      *         DATASET ('ERAGTC')
      *         RIDFLD  (PI-ERCOMP-KEY)
      *         RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERAGTC' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00005909' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035393039' TO DFHEIV0(25:11)
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
           MOVE +0                     TO WS-TOT-FEES
           IF RESP-NORMAL
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (S1 > +10)
                 COMPUTE WS-TOT-FEES = WS-TOT-FEES
                    + AG-SPP-FEES (S1)
              END-PERFORM
           END-IF
           .
       6100-EXIT.
           EXIT.
01943  6500-UPDATE-RQST.
01944
01945      MOVE LOW-VALUES             TO  ERRQST-KEY-3.
01946      MOVE PI-ERC-COMPANY-CD      TO  RQST-COMP-ID.
01947      MOVE PI-ERC-CARRIER         TO  RQST-CARRIER.
01948      MOVE PI-ERC-GROUP           TO  RQST-GROUP.
01949      MOVE PI-ERC-RESP            TO  RQST-FIN-RESP.
01950      MOVE PI-ERC-ACCT            TO  RQST-ACCT-AGENT.
01951
01952      
      * EXEC CICS HANDLE CONDITION
01953 *        ENDFILE  (6550-END-BROWSE)
01954 *        NOTFND   (6550-END-BROWSE)
01955 *    END-EXEC.
      *    MOVE '"$''I                  ! * #00005935' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035393335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01956
01957      
      * EXEC CICS STARTBR
01958 *        DATASET  (RQST-FILE-ID-3)
01959 *        RIDFLD   (ERRQST-KEY-3)
01960 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005940' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID-3, 
                 ERRQST-KEY-3, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01961
01962  6525-READ-LOOP.
01963      
      * EXEC CICS HANDLE CONDITION
01964 *        ENDFILE  (6550-END-BROWSE)
01965 *        NOTFND   (6550-END-BROWSE)
01966 *    END-EXEC.
      *    MOVE '"$''I                  ! + #00005946' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035393436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01967
01968      
      * EXEC CICS READNEXT
01969 *        DATASET  (RQST-FILE-ID-3)
01970 *        SET      (ADDRESS OF AR-REQUEST-RECORD)
01971 *        RIDFLD   (ERRQST-KEY-3)
01972 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005951' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393531' TO DFHEIV0(25:11)
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
           
01973
01974      IF RQ-CONTROL-BY-FIN-RESP = WS-SAVE-RQST
01975          GO TO 6525-READ-LOOP.
01976
01977      IF RQ-COMPANY-CD-A2 =  PI-ERC-COMPANY-CD AND
01978         RQ-CARRIER-A2    =  PI-ERC-CARRIER    AND
01979         RQ-GROUPING-A2   =  PI-ERC-GROUP      AND
01980         RQ-FIN-RESP-A2   =  PI-ERC-RESP       AND
01981         RQ-ACCT-AGENT-A2 =  PI-ERC-ACCT
01982          NEXT SENTENCE
01983      ELSE
01984          GO TO 6550-END-BROWSE.
01985
01986      IF WS-SUMM-FOR-RQST = RQ-SUMMARY-CODE
01987          GO TO 6525-READ-LOOP.
01988
01989      MOVE RQ-CONTROL-BY-FIN-RESP TO  WS-SAVE-RQST.
01990      MOVE RQ-CONTROL-PRIMARY     TO  ERRQST-KEY.
01991
01992      
      * EXEC CICS ENDBR
01993 *        DATASET  (RQST-FILE-ID-3)
01994 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005975' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID-3, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01995
01996      
      * EXEC CICS HANDLE CONDITION
01997 *        ENDFILE  (6599-EXIT)
01998 *        NOTFND   (6599-EXIT)
01999 *    END-EXEC.
      *    MOVE '"$''I                  ! , #00005979' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035393739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02000
02001      
      * EXEC CICS READ
02002 *        DATASET  (RQST-FILE-ID)
02003 *        SET      (ADDRESS OF AR-REQUEST-RECORD)
02004 *        RIDFLD   (ERRQST-KEY)
02005 *        UPDATE
02006 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005984' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393834' TO DFHEIV0(25:11)
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
           
02007
02008      MOVE WS-SUMM-FOR-RQST       TO  RQ-SUMMARY-CODE.
02009
02010      
      * EXEC CICS REWRITE
02011 *        DATASET  (RQST-FILE-ID)
02012 *        FROM     (AR-REQUEST-RECORD)
02013 *    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005993' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02014
02015      GO TO 6500-UPDATE-RQST.
02016
02017  6550-END-BROWSE.
02018
02019      
      * EXEC CICS ENDBR
02020 *        DATASET  (RQST-FILE-ID-3)
02021 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006002' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RQST-FILE-ID-3, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02022
02023  6599-EXIT.
02024      EXIT.
02025  EJECT
02026
02027  7000-EDIT.
02028      MOVE LSTMDTI                TO  WS-DATE-MDY-8.
02029
02030      IF LSTMDTL > ZERO
02031          NEXT SENTENCE
02032      ELSE
02033          GO TO 7100-EDIT-CONTD.
02034
02035      IF LSTMDTI = SPACES
02036          GO TO 7100-EDIT-CONTD.
02037
02038      IF WS-DMDY8-SL1 = '/'  AND
02039         WS-DMDY8-SL2 = '/'
02040          MOVE WS-DMDY8-MM        TO  WS-YMD-MM
02041          MOVE WS-DMDY8-DD        TO  WS-YMD-DD
02042          MOVE WS-DMDY8-YY        TO  WS-YMD-YY
02043      ELSE
02044          MOVE WS-DMDY6-MM        TO  WS-YMD-MM
02045          MOVE WS-DMDY6-DD        TO  WS-YMD-DD
02046          MOVE WS-DMDY6-YY        TO  WS-YMD-YY.
02047
02048      MOVE WS-YMD-DATE            TO  DC-GREG-DATE-1-YMD.
02049      MOVE '3'                    TO  DC-OPTION-CODE.
02050
02051      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
02052
02053      IF NO-CONVERSION-ERROR
02054          NEXT SENTENCE
02055      ELSE
02056          GO TO 7090-DATE-ERROR.
02057
02058      IF DC-BIN-DATE-1 > PI-CR-MONTH-END-DT
02059          NEXT SENTENCE
02060      ELSE
02061          GO TO 7100-EDIT-CONTD.
02062
02063  7090-DATE-ERROR.
02064      MOVE -1                     TO  LSTMDTL.
02065      MOVE AL-UABON               TO  LSTMDTA.
02066      MOVE ER-0314                TO  EMI-ERROR.
02067
02068      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02069
02070  7100-EDIT-CONTD.
02104
02105      IF CSRL > ZEROS
02106          MOVE AL-UANON           TO  CSRA
111103     END-IF
02130
02131      IF MAILNAML > ZERO
02132          MOVE AL-UANON           TO  MAILNAMA.
02133
02134      IF SSNL > ZERO
02135          MOVE AL-UANON           TO  SSNA.
02136
02137      IF ACCTNAML > ZERO
02138          MOVE AL-UANON           TO  ACCTNAMA
02139      ELSE
02140          IF ADD-FUNCTION
02141              MOVE -1             TO  ACCTNAML
02142              MOVE AL-UABON       TO  ACCTNAMA
02143              MOVE ER-2045        TO  EMI-ERROR
02144              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02145
02146      IF PHONEL > ZERO
02147          MOVE PHONEI               TO  DEEDIT-FIELD
02148          PERFORM 7100-DEEDIT  THRU  7100-EXIT
02149          IF DEEDIT-FIELD-V0 = ZEROS
02150              MOVE SPACES               TO  PHONEO
02151                                            PI-SAVE-PHONE
02152          ELSE
02153              IF (DEEDIT-FIELD-V0 > 9999999999
02154                OR < 2000000000)
02155                  MOVE -1               TO  PHONEL
02156                  MOVE AL-UNBON         TO  PHONEA
02157                  MOVE ER-2046          TO  EMI-ERROR
02158                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02159              ELSE
02160                  MOVE DEEDIT-FIELD-V0  TO  WS-PHONE-IN
02161                                            PI-SAVE-PHONE-RED
02162                  MOVE WSPI-AREA        TO  WSPO-AREA
02163                  MOVE WSPI-PFX         TO  WSPO-PFX
02164                  MOVE WSPI-SFX         TO  WSPO-SFX
02165                  MOVE WS-PHONE-OUT     TO  PHONEO
02166                  MOVE AL-UNNON         TO  PHONEA.
02167
02168      IF FAXNOL > ZERO
02169          MOVE FAXNOI               TO  DEEDIT-FIELD
02170          PERFORM 7100-DEEDIT  THRU  7100-EXIT
02171          IF DEEDIT-FIELD-V0 = ZEROS
02172              MOVE SPACES               TO  FAXNOO
02173                                            PI-SAVE-FAXNO
02174          ELSE
02175              IF (DEEDIT-FIELD-V0 > 9999999999
02176                OR < 2000000000)
02177                  MOVE -1               TO  FAXNOL
02178                  MOVE AL-UNBON         TO  FAXNOA
02179                  MOVE ER-3055          TO  EMI-ERROR
02180                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02181              ELSE
02182                  MOVE DEEDIT-FIELD-V0  TO  WS-PHONE-IN
02183                                            PI-SAVE-FAXNO-RED
02184                  MOVE WSPI-AREA        TO  WSPO-AREA
02185                  MOVE WSPI-PFX         TO  WSPO-PFX
02186                  MOVE WSPI-SFX         TO  WSPO-SFX
02187                  MOVE WS-PHONE-OUT     TO  FAXNOO
02188                  MOVE AL-UNNON         TO  FAXNOA.
02189
02190      IF ADDR1L > ZERO
02191          MOVE AL-UANON           TO  ADDR1A
02192      ELSE
02193          IF ADD-FUNCTION
02194              MOVE -1             TO  ADDR1L
02195              MOVE AL-UABON       TO  ADDR1A
02196              MOVE ER-2047        TO  EMI-ERROR
02197              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02198
02199      IF ADDR2L > ZERO
02200          MOVE AL-UANON           TO  ADDR2A.
02201
02202      IF CARBALL > ZERO OR CHANGE-FUNCTION
02203          MOVE CARBALI            TO  PI-CHECK-CARRY-BAL
02204          IF VALID-CARRY-BAL
02205              PERFORM 7010-EDIT-CARBAL  THRU  7010-EXIT
02206          ELSE
02207              MOVE -1             TO  CARBALL
02208              MOVE AL-UABON       TO  CARBALA
02209              MOVE ER-2048        TO  EMI-ERROR
02210              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02211      ELSE
02212          IF ADD-FUNCTION AND NOT PI-AR-PROCESSING
02213              MOVE -1             TO  CARBALL
02214              MOVE AL-UABON       TO  CARBALA
02215              MOVE ER-2048        TO  EMI-ERROR
02216              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02217
02218      IF PI-AR-PROCESSING
02219          IF TYPEI = 'A'
02220             IF NETGRSL > ZERO OR CHANGE-FUNCTION
02221                  IF NETGRSI = 'N' OR 'G'
02222                     MOVE AL-UANON    TO  NETGRSA
02223                  ELSE
02224                      MOVE -1         TO  NETGRSL
02225                      MOVE AL-UABON   TO  NETGRSA
02226                      MOVE ER-3151    TO  EMI-ERROR
02227                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02228
02229      IF CITYL > ZERO
02230          MOVE AL-UANON           TO  CITYA
02231      ELSE
02232          IF ADD-FUNCTION
02233              MOVE -1             TO  CITYL
02234              MOVE AL-UABON       TO  CITYA
02235              MOVE ER-2049        TO  EMI-ERROR
02236              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02237
02229      IF STATEL   > ZERO
02230         MOVE AL-UANON            TO STATEA
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO CNTL-COMP-ID
              MOVE '3'                 TO CNTL-REC-TYPE
              MOVE STATEI              TO CNTL-ACCESS
              MOVE +0                  TO CNTL-SEQ-NO
              
      * EXEC CICS READ
      *          DATASET   (CNTL-FILE-ID)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        E          (  N#00006173' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036313733' TO DFHEIV0(25:11)
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
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO STATEL
                 MOVE AL-UABON         TO STATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
02231      ELSE
02232          IF ADD-FUNCTION
02233              MOVE -1             TO  STATEL
02234              MOVE AL-UABON       TO  STATEA
02235              MOVE ER-2049        TO  EMI-ERROR
02236              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02237
071712*     IF PI-AR-PROCESSING
071712*         IF ARBALL > ZERO OR CHANGE-FUNCTION
071712*             IF ARBALI = '1' OR '2' OR '3' OR '4'
071712*                 MOVE AL-UANON   TO  ARBALA
071712*             ELSE
071712*                 MOVE -1         TO  ARBALL
071712*                 MOVE AL-UABON   TO  ARBALA
071712*                 MOVE ER-3150    TO  EMI-ERROR
071712*                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02247
02284      IF PI-AR-PROCESSING
02285          IF CKPULLL > ZERO OR CHANGE-FUNCTION
02286              IF CKPULLI = 'Y' OR 'N'
02287                  MOVE AL-UANON   TO  CKPULLA
02288              ELSE
02289                  MOVE -1         TO  CKPULLL
02290                  MOVE AL-UABON   TO  CKPULLA
02291                  MOVE ER-3170    TO  EMI-ERROR
02292                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02293
02294      IF ZIPCODEL > ZERO
02295          MOVE AL-UANON           TO  ZIPCODEA.
111103
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' or 'FNL'
111103        CONTINUE
111103     ELSE
111103        IF PI-ERC-TYPE = 'B'
111103           IF CLPSTL > ZERO
111103              PERFORM 4700-CHECK-STATE       THRU 4799-EXIT
111103              IF CLPSTI = SPACES
111103                 MOVE -1         TO  CLPSTL
111103                 MOVE AL-UABON   TO  CLPSTA
111103                 MOVE ER-0144    TO  EMI-ERROR
111103                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
111103              ELSE
111103                 MOVE AL-UANON   TO  CLPSTA
111103              END-IF
052005           ELSE
052005              IF ADD-FUNCTION
052005                 MOVE -1         TO  CLPSTL
052005                 MOVE AL-UABON   TO  CLPSTA
052005                 MOVE ER-0144    TO  EMI-ERROR
052005                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
052005              END-IF
111103           END-IF
111103
111103           IF MAXFEEL  > ZERO
111103              
      * EXEC CICS BIF
111103*                  DEEDIT
111103*                  FIELD   (MAXFEEI)
111103*                  LENGTH  (6)
111103*             END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006242' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAXFEEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
111103              IF MAXFEEI NUMERIC
111103                 MOVE AL-UNNON   TO  MAXFEEA
111103              ELSE
111103                 MOVE -1         TO  MAXFEEL
111103                 MOVE AL-UABON   TO  MAXFEEA
111103                 MOVE ER-3261    TO  EMI-ERROR
111103                 PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
                    END-IF
                 ELSE
                    IF ADD-FUNCTION
111103                 MOVE -1         TO  MAXFEEL
111103                 MOVE AL-UABON   TO  MAXFEEA
111103                 MOVE ER-3261    TO  EMI-ERROR
111103                 PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
                    END-IF
                 END-IF
092205           IF MAXLFL  > ZERO
092205              
      * EXEC CICS BIF
092205*                  DEEDIT
092205*                  FIELD   (MAXLFI)
092205*                  LENGTH  (6)
092205*             END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006266' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAXLFI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
092205              IF MAXLFI NUMERIC
092205                 MOVE AL-UNNON   TO  MAXLFA
092205              ELSE
092205                 MOVE -1         TO  MAXLFL
092205                 MOVE AL-UABON   TO  MAXLFA
092205                 MOVE ER-3261    TO  EMI-ERROR
092205                 PERFORM 9900-ERROR-FORMAT
092205                                 THRU  9900-EXIT
092205              END-IF
092205           ELSE
092205              IF ADD-FUNCTION
092205                 MOVE -1         TO  MAXLFL
092205                 MOVE AL-UABON   TO  MAXLFA
092205                 MOVE ER-3261    TO  EMI-ERROR
092205                 PERFORM 9900-ERROR-FORMAT
092205                                 THRU  9900-EXIT
092205              END-IF
092205           END-IF
072406           IF REFEL > ZERO
072406              IF REFEI = ' ' OR 'R' OR 'N' OR 'B'
072406                 MOVE AL-UANON   TO REFEA
                    ELSE
                       MOVE -1         TO REFEL
                       MOVE AL-UABON   TO REFEA
                       MOVE ER-2790    TO EMI-ERROR
                       PERFORM 9900-ERROR-FORMAT
                                       THRU  9900-EXIT
                    END-IF
                 END-IF
111103        END-IF
111103     END-IF
02296
02297      IF BALFWDL > ZERO
02298          
      * EXEC CICS BIF
02299 *            DEEDIT
02300 *            FIELD   (BALFWDI)
02301 *            LENGTH  (13)
02302 *        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006304' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALFWDI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02303          MOVE AL-UNNON           TO  BALFWDA
02304          MOVE BALFWDI            TO  WS-SAVE-BALFWD.
02305
02306      IF CURCOML >        ZERO
02307          
      * EXEC CICS BIF
02308 *            DEEDIT
02309 *            FIELD   (CURCOMI)
02310 *            LENGTH  (13)
02311 *        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006313' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CURCOMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02312          MOVE AL-UNNON           TO  CURCOMA
02313          MOVE CURCOMI            TO  WS-SAVE-CURCOM.
02314
02315      IF CURCHGL > ZERO
02316          
      * EXEC CICS BIF
02317 *            DEEDIT
02318 *            FIELD   (CURCHGI)
02319 *            LENGTH  (13)
02320 *        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006322' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CURCHGI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02321          MOVE AL-UNNON           TO  CURCHGA
02322          MOVE CURCHGI            TO  WS-SAVE-CURCHG.
02323
02324      IF CURPMTL > ZERO
02325          
      * EXEC CICS BIF
02326 *            DEEDIT
02327 *            FIELD   (CURPMTI)
02328 *            LENGTH  (13)
02329 *        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006331' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CURPMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02330          MOVE AL-UNNON           TO  CURPMTA
02331          MOVE CURPMTI            TO  WS-SAVE-CURPMT.
02332
02333      IF ENDBALL > ZERO
02334          
      * EXEC CICS BIF
02335 *            DEEDIT
02336 *            FIELD   (ENDBALI)
02337 *            LENGTH  (13)
02338 *        END-EXEC
           MOVE 13
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006340' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ENDBALI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02339          MOVE AL-UNNON           TO  ENDBALA
02340          MOVE ENDBALI            TO  WS-SAVE-ENDBAL.
02341
043007*    IF YTDCOML > ZERO
043007*       EXEC CICS BIF
043007*            DEEDIT
043007*            FIELD   (YTDCOMI)
043007*            LENGTH  (13)
043007*       END-EXEC
043007*       MOVE AL-UNNON            TO YTDCOMA
043007*       MOVE YTDCOMI             TO WS-SAVE-YTDCOM
043007*    END-IF
           .
02351  7000-EXIT.
02352      EXIT.
02353  EJECT
02354  7010-EDIT-CARBAL.
033105     IF TYPEI = 'G' OR 'B'
02356          IF CARBALI NOT = 'Y'
02357              MOVE -1             TO  CARBALL
02358              MOVE AL-UABON       TO  CARBALA
02359              MOVE ER-2093        TO  EMI-ERROR
02360              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02361              GO TO 7010-EXIT.
02362
02363      IF TYPEI = 'A'
02367          IF FINRESPI = ACCTNOI
02368              IF CARBALI NOT = 'Y'
02369                  MOVE -1         TO  CARBALL
02370                  MOVE AL-UABON   TO  CARBALA
02371                  MOVE ER-2094    TO  EMI-ERROR
02372                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02373                  GO TO 7010-EXIT.
02374
02375      IF PI-PROCESSOR-ID = 'E864'
02376          NEXT SENTENCE
02377      ELSE
02378         IF TYPEI = 'A'
02379            IF PI-AR-PROCESSING
02380                IF FINRESPI NOT = ACCTNOI
02381                     IF CARBALI = 'Y'
02382                        MOVE -1             TO  CARBALL
02383                        MOVE AL-UABON       TO  CARBALA
02384                        MOVE ER-3174        TO  EMI-ERROR
02385                        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02386
02387      IF TYPEI = 'C'
02388          IF CARBALI NOT = 'N'
02389              MOVE -1             TO  CARBALL
02390              MOVE AL-UABON       TO  CARBALA
02391              MOVE ER-2096        TO  EMI-ERROR
02392              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02393              GO TO 7010-EXIT.
02394
02395      IF PI-PROCESSOR-ID = 'E864'
02396          NEXT SENTENCE
02397      ELSE
02398         IF CHANGE-FUNCTION
02402             IF PI-ERC-END-BAL NOT = ZERO
02403                 IF CARBALI = 'N'
02404                     MOVE -1         TO  CARBALL
02405                     MOVE AL-UABON   TO  CARBALA
02406                     MOVE ER-2095    TO  EMI-ERROR
02407                     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02408                     GO TO 7010-EXIT.
02409
02410      MOVE AL-UANON               TO  CARBALA.
02411
02412  7010-EXIT.
02413      EXIT.
02414  EJECT
02415  7050-READ-ERCOMP.
02416      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
02417
02418      
      * EXEC CICS READ
02419 *        DATASET  (COMP-FILE-ID)
02420 *        SET      (ADDRESS OF COMPENSATION-MASTER)
02421 *        RIDFLD   (PI-ERCOMP-KEY)
02422 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006419' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343139' TO DFHEIV0(25:11)
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
           
02423
02424      MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.
02425
02426  7050-EXIT.
02427      EXIT.
02428  EJECT
02429  7100-DEEDIT.
02430      
      * EXEC CICS BIF
02431 *        DEEDIT
02432 *        FIELD   (DEEDIT-FIELD)
02433 *        LENGTH  (15)
02434 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006431' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02435
02436  7100-EXIT.
02437      EXIT.
02438  EJECT
02439  7150-ERCOMP-GETMAIN.
02440      
      * EXEC CICS GETMAIN
02441 *        SET      (ADDRESS OF COMPENSATION-MASTER)
02442 *        LENGTH   (ERCOMP-LENGTH)
02443 *        INITIMG  (GETMAIN-SPACE)
02444 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006441' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCOMP-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02445
02446  7150-EXIT.
02447      EXIT.
02448  EJECT
02449  7200-READ-ERCOMP-UPDATE.
02450      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
02451
02452      
      * EXEC CICS READ
02453 *        DATASET  (COMP-FILE-ID)
02454 *        SET      (ADDRESS OF COMPENSATION-MASTER)
02455 *        RIDFLD   (PI-ERCOMP-KEY)
02456 *        UPDATE
02457 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006453' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343533' TO DFHEIV0(25:11)
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
           
02458
02459  7200-EXIT.
02460      EXIT.
02461  EJECT
02462  7250-PAGE-FORWARD.
02463      MOVE SPACES                 TO  PI-ERCOMP-EOF-SW.
02464
02465      IF MAINTYPL > ZERO
02466          MOVE GROUPI             TO  PI-ERC-GROUP
02467          MOVE CARRIERI           TO  PI-ERC-CARRIER
02468          MOVE FINRESPI           TO  PI-ERC-RESP
02469          MOVE ACCTNOI            TO  PI-ERC-ACCT
02470          MOVE TYPEI              TO  PI-ERC-TYPE
02471      ELSE
02472          MOVE LOW-VALUES         TO  PI-ERCOMP-KEY
02473          MOVE 'Y'                TO  PI-FIRST-TIME-SW.
02474
02475      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
02476      MOVE PI-ERCOMP-KEY          TO  WS-SAVE-KEY.
02477
02478      
      * EXEC CICS HANDLE CONDITION
02479 *        ENDFILE  (7250-ENDFILE)
02480 *        NOTFND   (7250-ENDFILE)
02481 *        ERROR    (9990-ABEND)
02482 *    END-EXEC.
      *    MOVE '"$''I.                 ! - #00006479' TO DFHEIV0
           MOVE X'222427492E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303036343739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02483
02484      PERFORM 7850-START-BROWSE  THRU  7850-EXIT.
02485
02486  7250-READ-NEXT.
02487      
      * EXEC CICS HANDLE CONDITION
02488 *        ENDFILE  (7250-ENDFILE)
02489 *        NOTFND   (7275-NOTFOUND)
02490 *    END-EXEC.
      *    MOVE '"$''I                  ! . #00006488' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303036343838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02491
02492      PERFORM 7900-READNEXT  THRU  7900-EXIT.
02493
02494      IF PI-CARRIER-SECURITY > SPACES
02495          IF PI-ERC-CARRIER = PI-CARRIER-SECURITY
02496              NEXT SENTENCE
02497          ELSE
02498              GO TO 7250-READ-NEXT.
02499
02500      IF ERCOMP-EOF
02501          IF FIRST-TIME
02502              MOVE LOW-VALUES     TO  EL652AO
02503              MOVE ER-0584        TO  EMI-ERROR
02504              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02505              GO TO 8100-SEND-INITIAL-MAP
02506          ELSE
02507              MOVE LOW-VALUES     TO  EL652AO
02508              MOVE ER-2067        TO  EMI-ERROR
02509              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02510              PERFORM 7950-END-BROWSE  THRU  7950-EXIT
02511              GO TO 7250-PAGE-FORWARD.
02512
02513      IF PI-ERCOMP-KEY = WS-SAVE-KEY
02514          GO TO 7250-READ-NEXT.
02515
02516      MOVE CO-LAST-MAINT-USER     TO  PI-UPDATE-BY.
02517      MOVE CO-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
02518      MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.
02519      MOVE LOW-VALUES             TO  EL652AO.
02520      MOVE CO-CARRIER             TO  CARRIERO.
02521      MOVE CO-GROUPING            TO  GROUPO.
02522      MOVE CO-RESP-NO             TO  FINRESPO.
02523      MOVE CO-ACCOUNT             TO  ACCTNOO.
02524      MOVE CO-TYPE                TO  TYPEO.
02525      MOVE AL-UANON               TO  CARRIERA
02526                                      GROUPA
02527                                      TYPEA
02528                                      FINRESPA
02529                                      ACCTNOA
02530                                      MAINTYPA.
02531      MOVE 'S'                    TO  MAINTYPO
02532                                      PI-CHECK-MAINT-TYPE.
02533
02534      GO TO 5050-SET-UP-SCREEN.
02535
02536  7250-ENDFILE.
02537      IF FIRST-TIME
02538          MOVE LOW-VALUES         TO  EL652AO
02539          MOVE ER-0584            TO  EMI-ERROR
02540          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02541          GO TO 8100-SEND-INITIAL-MAP.
02542
02543      IF BROWSE-STARTED
02544          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
02545
02546      MOVE ER-2067                TO  EMI-ERROR.
02547      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02548
02549      MOVE LOW-VALUES             TO  EL652AO.
02550
02551      GO TO 7250-PAGE-FORWARD.
02552
02553  7275-NOTFOUND.
02554      IF BROWSE-STARTED
02555          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
02556
02557      GO TO 8880-NOT-FOUND.
02558
02559  7299-EXIT.
02560      EXIT.
02561  EJECT
02562  7300-PAGE-BACKWARD.
02563      MOVE SPACES                 TO  PI-ERCOMP-EOF-SW.
02564
02565      IF MAINTYPL > ZERO
02566          MOVE GROUPI             TO  PI-ERC-GROUP
02567          MOVE CARRIERI           TO  PI-ERC-CARRIER
02568          MOVE FINRESPI           TO  PI-ERC-RESP
02569          MOVE ACCTNOI            TO  PI-ERC-ACCT
02570          MOVE TYPEI              TO  PI-ERC-TYPE
02571      ELSE
02572          MOVE LOW-VALUES         TO  PI-ERCOMP-KEY.
02573
02574      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.
02575
02576      
      * EXEC CICS HANDLE CONDITION
02577 *        ENDFILE  (7375-ENDFILE)
02578 *        NOTFND   (7375-ENDFILE)
02579 *        ERROR    (9990-ABEND)
02580 *    END-EXEC.
      *    MOVE '"$''I.                 ! / #00006577' TO DFHEIV0
           MOVE X'222427492E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303036353737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02581
02582      PERFORM 7850-START-BROWSE  THRU  7850-EXIT.
02583
02584      PERFORM 8050-READPREV  THRU  8050-EXIT.
02585
02586  7350-READ-PREV.
02587      
      * EXEC CICS HANDLE CONDITION
02588 *        ENDFILE  (7375-ENDFILE)
02589 *        NOTFND   (7275-NOTFOUND)
02590 *    END-EXEC.
      *    MOVE '"$''I                  ! 0 #00006588' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303036353838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02591
02592      PERFORM 8050-READPREV  THRU  8050-EXIT.
02593
02594      IF PI-CARRIER-SECURITY > SPACES
02595          IF PI-ERC-CARRIER = PI-CARRIER-SECURITY
02596              NEXT SENTENCE
02597          ELSE
02598              GO TO 7350-READ-PREV.
02599
02600      IF PI-COMPANY-CD NOT = CO-COMPANY-CD
02601          GO TO 7375-ENDFILE.
02602
02603      MOVE CO-LAST-MAINT-USER     TO  PI-UPDATE-BY.
02604      MOVE CO-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
02605      MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.
02606      MOVE LOW-VALUES             TO  EL652AO.
02607      MOVE CO-CARRIER             TO  CARRIERO.
02608      MOVE CO-GROUPING            TO  GROUPO.
02609      MOVE CO-RESP-NO             TO  FINRESPO.
02610      MOVE CO-ACCOUNT             TO  ACCTNOO.
02611      MOVE CO-TYPE                TO  TYPEO.
02612      MOVE AL-UANON               TO  CARRIERA
02613                                      GROUPA
02614                                      TYPEA
02615                                      FINRESPA
02616                                      ACCTNOA
02617                                      MAINTYPA.
02618      MOVE 'S'                    TO  MAINTYPO
02619                                      PI-CHECK-MAINT-TYPE.
02620
02621      GO TO 5050-SET-UP-SCREEN.
02622
02623  7375-ENDFILE.
02624      IF BROWSE-STARTED
02625          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.
02626
02627      MOVE ER-2238                TO  EMI-ERROR.
02628      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02629
02630      MOVE -1                     TO  MAINTYPL.
02631
02632      GO TO 8200-SEND-DATAONLY.
02633
02634  EJECT
02635  7400-READ-CONTROL-FILE.
02636      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
02637      MOVE WS-ACCESS              TO  CNTL-ACCESS.
02638
02639      
      * EXEC CICS HANDLE CONDITION
02640 *        NOTFND  (7490-NOT-FOUND)
02641 *        ERROR   (9990-ABEND)
02642 *    END-EXEC.
      *    MOVE '"$I.                  ! 1 #00006640' TO DFHEIV0
           MOVE X'2224492E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303036363430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02643
02644      
      * EXEC CICS READ
02645 *        DATASET  (CNTL-FILE-ID)
02646 *        SET      (ADDRESS OF CONTROL-FILE)
02647 *        RIDFLD   (ELCNTL-KEY)
02648 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006645' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363435' TO DFHEIV0(25:11)
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
           
02649
02650      IF CNTL-REC-TYPE = '6'
02651          MOVE AL-UANON           TO  CARRIERA
02652          MOVE CARRIERI           TO  PI-ERC-CARRIER
02653          GO TO 7499-EXIT.
02654
02655      IF CF-COMPENSATION-MSTR-MAINT-DT NOT = LOW-VALUES
02656          GO TO 7499-EXIT
02657      ELSE
02658          MOVE -1                 TO  MAINTYPL
02659          MOVE ER-2572            TO  EMI-ERROR
02660          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02661          GO TO 7499-EXIT.
02662
02663  7490-NOT-FOUND.
02664      IF CNTL-REC-TYPE = '6'
02665          MOVE -1                 TO  CARRIERL
02666          MOVE AL-UABON           TO  CARRIERA
02667          MOVE ER-0193            TO  EMI-ERROR
02668      ELSE
02669          IF CNTL-REC-TYPE = '2'
02670              MOVE -1                TO  CSRL
02671              MOVE AL-UABON          TO  CSRA
02672              MOVE ER-1883           TO  EMI-ERROR
02673          ELSE
02674             MOVE ER-0002            TO  EMI-ERROR
02675             MOVE -1                 TO  PFKEYL.
02676
02677      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02678
02679  7499-EXIT.
02680      EXIT.
02681  EJECT
02682
02683  7500-UPDATE-SUMM.
02684
02685      PERFORM 7700-ERSUMM-GETMAIN  THRU  7700-EXIT.
02686
02687      MOVE 'SX'                   TO SX-RECORD-ID.
02688      MOVE PI-COMPANY-CD          TO SX-COMPANY-CD.
02689      MOVE PI-AR-SUMMARY-CODE     TO SX-SUMMARY.
02690      MOVE PI-ERC-CARRIER         TO SX-CARRIER.
02691      MOVE PI-ERC-GROUP           TO SX-GROUP.
02692      MOVE PI-ERC-RESP            TO SX-FIN-RESP.
02693      MOVE PI-ERC-ACCT            TO SX-ACCT-AGENT.
02694      MOVE WS-SAVE-NAME           TO SX-SUMM-OR-AGT-NAME.
02695      MOVE PI-PROCESSOR-ID        TO SX-LAST-MAINT-BY.
02696      MOVE EIBTIME                TO SX-LAST-MAINT-HHMMSS.
02697      MOVE BIN-CURRENT-SAVE       TO SX-LAST-MAINT-DT.
02698      MOVE PI-COMPANY-CD          TO SX-COMPANY-A1.
02699      MOVE PI-ERC-ACCT            TO SX-ACCT-AGENT-A1.
02700      MOVE PI-AR-SUMMARY-CODE     TO SX-SUMMARY-A1.
02701      MOVE PI-ERC-CARRIER         TO SX-CARR-A1.
02702      MOVE PI-ERC-GROUP           TO SX-GROUP-A1.
02703      MOVE PI-ERC-RESP            TO SX-FIN-RESP-A1.
02704
02705      
      * EXEC CICS HANDLE CONDITION
02706 *        DUPREC  (7550-DUP)
02707 *    END-EXEC.
      *    MOVE '"$%                   ! 2 #00006706' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303036373036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02708
02709      
      * EXEC CICS WRITE
02710 *        DATASET  (SUMM-FILE-ID)
02711 *        FROM     (SUMM-CROSS-REFERENCE)
02712 *        RIDFLD   (SX-CONTROL-PRIMARY)
02713 *    END-EXEC.
           MOVE LENGTH OF
            SUMM-CROSS-REFERENCE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006710' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 SUMM-CROSS-REFERENCE, 
                 DFHEIV11, 
                 SX-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02714
02715      MOVE LOW-VALUES             TO ERSUMM-KEY.
02716      MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.
02717      MOVE PI-AR-SUMMARY-CODE     TO SUMM-SUMMARY.
02718
02719      
      * EXEC CICS HANDLE CONDITION
02720 *        NOTFND  (7540-WRITE)
02721 *    END-EXEC.
      *    MOVE '"$I                   ! 3 #00006720' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303036373230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02722
02723      
      * EXEC CICS READ
02724 *        DATASET  (SUMM-FILE-ID)
02725 *        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
02726 *        RIDFLD   (ERSUMM-KEY)
02727 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006724' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373234' TO DFHEIV0(25:11)
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
           
02728      GO TO 7599-EXIT.
02729
02730  7540-WRITE.
02731
02732      MOVE ERSUMM-KEY             TO SX-CONTROL-PRIMARY.
02733      MOVE LOW-VALUES             TO SX-CONTROL-A1.
02734      MOVE PI-COMPANY-CD          TO SX-COMPANY-A1.
02735      MOVE PI-AR-SUMMARY-CODE     TO SX-SUMMARY-A1.
02736      MOVE SPACES                 TO SX-SUMM-OR-AGT-NAME.
02737      MOVE 'LGXX'                 TO SX-LAST-MAINT-BY.
02738
02739      
      * EXEC CICS WRITE
02740 *        DATASET  (SUMM-FILE-ID)
02741 *        FROM     (SUMM-CROSS-REFERENCE)
02742 *        RIDFLD   (SX-CONTROL-PRIMARY)
02743 *    END-EXEC.
           MOVE LENGTH OF
            SUMM-CROSS-REFERENCE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006740' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 SUMM-CROSS-REFERENCE, 
                 DFHEIV11, 
                 SX-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02744
02745      MOVE PI-ERC-CARRIER         TO PI-CR-CARRIER.
02746      MOVE PI-ERC-GROUP           TO PI-CR-GROUPING.
02747      MOVE PI-ERC-TYPE            TO PI-CR-TYPE.
02748      MOVE PI-ERC-RESP            TO PI-CR-FIN-RESP.
02749      MOVE PI-ERC-ACCT            TO PI-CR-ACCOUNT.
02750
02751      MOVE XCTL-856               TO PGM-NAME.
02752      GO TO 9300-XCTL.
02753
02754  7550-DUP.
02755
02756      MOVE ER-3152                TO EMI-ERROR.
02757      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02758
02759      MOVE -1                     TO MAINTYPL.
02760
02761      GO TO 8200-SEND-DATAONLY.
02762
02763  7599-EXIT.
02764      EXIT.
02765  EJECT
02766
02767  7600-DELETE-SUMM.
02768
02769      MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.
02770      MOVE WS-SAVE-SUMM           TO SUMM-SUMMARY.
02771      MOVE PI-ERC-CARRIER         TO SUMM-CARRIER.
02772      MOVE PI-ERC-GROUP           TO SUMM-GROUP.
02773      MOVE PI-ERC-RESP            TO SUMM-FIN-RESP.
02774      MOVE PI-ERC-ACCT            TO SUMM-ACCT-AGENT.
02775
02776  7620-READ-ERSUMM.
02777      
      * EXEC CICS HANDLE CONDITION
02778 *        NOTFND  (7699-EXIT)
02779 *    END-EXEC.
      *    MOVE '"$I                   ! 4 #00006778' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303036373738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02780
02781      
      * EXEC CICS READ
02782 *        DATASET  (SUMM-FILE-ID)
02783 *        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
02784 *        RIDFLD   (ERSUMM-KEY)
02785 *        UPDATE
02786 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006782' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373832' TO DFHEIV0(25:11)
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
           
02787
02788      
      * EXEC CICS DELETE
02789 *        DATASET  (SUMM-FILE-ID)
02790 *    END-EXEC.
      *    MOVE '&(                    &   #00006789' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02791
02792      MOVE LOW-VALUES             TO ERSUMM-KEY.
02793      MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.
02794      MOVE WS-SAVE-SUMM           TO SUMM-SUMMARY.
02795
02796      
      * EXEC CICS STARTBR
02797 *        DATASET  (SUMM-FILE-ID)
02798 *        RIDFLD   (ERSUMM-KEY)
02799 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006797' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02800
02801      
      * EXEC CICS READNEXT
02802 *        DATASET  (SUMM-FILE-ID)
02803 *        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
02804 *        RIDFLD   (ERSUMM-KEY)
02805 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006802' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383032' TO DFHEIV0(25:11)
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
           
02806
02807      IF SX-CARRIER = LOW-VALUES
02808          NEXT SENTENCE
02809      ELSE
02810          GO TO 7640-END-BROWSE.
02811
02812      
      * EXEC CICS HANDLE CONDITION
02813 *        ENDFILE  (7630-DELETE)
02814 *    END-EXEC.
      *    MOVE '"$''                   ! 5 #00006813' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303036383133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02815
02816      
      * EXEC CICS READNEXT
02817 *        DATASET  (SUMM-FILE-ID)
02818 *        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
02819 *        RIDFLD   (ERSUMM-KEY)
02820 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006817' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383137' TO DFHEIV0(25:11)
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
           
02821
02822      IF SX-SUMMARY = WS-SAVE-SUMM
02823          GO TO 7640-END-BROWSE.
02824
02825  7630-DELETE.
02826      
      * EXEC CICS ENDBR
02827 *        DATASET  (SUMM-FILE-ID)
02828 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006827' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02829
02830      MOVE LOW-VALUES             TO ERSUMM-KEY.
02831      MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.
02832      MOVE WS-SAVE-SUMM           TO SUMM-SUMMARY.
02833
02834      
      * EXEC CICS READ
02835 *        DATASET  (SUMM-FILE-ID)
02836 *        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
02837 *        RIDFLD   (ERSUMM-KEY)
02838 *        UPDATE
02839 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006835' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383335' TO DFHEIV0(25:11)
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
           
02840
02841      
      * EXEC CICS DELETE
02842 *        DATASET  (SUMM-FILE-ID)
02843 *    END-EXEC.
      *    MOVE '&(                    &   #00006842' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02844
02845      GO TO 7699-EXIT.
02846
02847  7640-END-BROWSE.
02848      
      * EXEC CICS ENDBR
02849 *        DATASET  (SUMM-FILE-ID)
02850 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006849' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SUMM-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02851
02852  7699-EXIT.
02853      EXIT.
02854  EJECT
02855
02856  7700-ERSUMM-GETMAIN.
02857      
      * EXEC CICS GETMAIN
02858 *        SET      (ADDRESS OF SUMM-CROSS-REFERENCE)
02859 *        LENGTH   (ERSUMM-LENGTH)
02860 *        INITIMG  (GETMAIN-SPACE)
02861 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006858' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERSUMM-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02862
02863  7700-EXIT.
02864      EXIT.
02865  EJECT
02866
02867  7850-START-BROWSE.
02868      
      * EXEC CICS STARTBR
02869 *        DATASET  (COMP-FILE-ID)
02870 *        RIDFLD   (PI-ERCOMP-KEY)
02871 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006869' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02872
02873      MOVE 'Y'                    TO  WS-BROWSE-SW.
02874
02875  7850-EXIT.
02876      EXIT.
02877  EJECT
02878  7900-READNEXT.
02879      
      * EXEC CICS READNEXT
02880 *        DATASET  (COMP-FILE-ID)
02881 *        SET      (ADDRESS OF COMPENSATION-MASTER)
02882 *        RIDFLD   (PI-ERCOMP-KEY)
02883 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006880' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383830' TO DFHEIV0(25:11)
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
           
02884
02885      IF PI-COMPANY-CD NOT = CO-COMPANY-CD
02886          MOVE 'Y'                TO  PI-ERCOMP-EOF-SW
02887      ELSE
02888          MOVE SPACE              TO  PI-FIRST-TIME-SW.
02889
02890  7900-EXIT.
02891      EXIT.
02892  EJECT
02893  7950-END-BROWSE.
02894      
      * EXEC CICS ENDBR
02895 *        DATASET  (COMP-FILE-ID)
02896 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006895' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02897
02898      MOVE 'N'                    TO  WS-BROWSE-SW.
02899
02900  7950-EXIT.
02901      EXIT.
02902  EJECT
02903  8000-UPDATE-MAINT-DATE.
02904      MOVE SPACES                 TO  ELCNTL-KEY.
02905      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
02906      MOVE '1'                    TO  CNTL-REC-TYPE.
02907      MOVE +0                     TO  CNTL-SEQ-NO.
02908
02909      
      * EXEC CICS HANDLE CONDITION
02910 *        NOTFND  (8000-EXIT)
02911 *    END-EXEC.
      *    MOVE '"$I                   ! 6 #00006910' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303036393130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02912
02913      
      * EXEC CICS READ
02914 *        UPDATE
02915 *        DATASET  (CNTL-FILE-ID)
02916 *        SET      (ADDRESS OF CONTROL-FILE)
02917 *        RIDFLD   (ELCNTL-KEY)
02918 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006914' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393134' TO DFHEIV0(25:11)
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
           
02919
02920      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
02921      MOVE 'B'                    TO  JP-RECORD-TYPE.
02922      MOVE CNTL-FILE-ID           TO  FILE-ID.
02923
02924      PERFORM 8400-LOG-JOURNAL-RECORD.
02925
02926      MOVE BIN-CURRENT-SAVE  TO  CF-COMPENSATION-MSTR-MAINT-DT.
02927      MOVE CONTROL-FILE      TO  JP-RECORD-AREA.
02928      MOVE 'C'               TO  JP-RECORD-TYPE.
02929      MOVE CNTL-FILE-ID      TO  FILE-ID.
02930
02931      
      * EXEC CICS REWRITE
02932 *        DATASET  (CNTL-FILE-ID)
02933 *        FROM     (CONTROL-FILE)
02934 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006932' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02935
02936      PERFORM 8400-LOG-JOURNAL-RECORD.
02937
02938  8000-EXIT.
02939       EXIT.
02940  EJECT
02941  8050-READPREV.
02942      
      * EXEC CICS READPREV
02943 *        DATASET  (COMP-FILE-ID)
02944 *        SET      (ADDRESS OF COMPENSATION-MASTER)
02945 *        RIDFLD   (PI-ERCOMP-KEY)
02946 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006943' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393433' TO DFHEIV0(25:11)
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
           
02947
02948  8050-EXIT.
02949      EXIT.
02950  EJECT
02951  8100-SEND-INITIAL-MAP.
02952
02953      IF  NOT CREDIT-SESSION
02954          MOVE AL-SADOF           TO PFK5A
02955                                     PFK6A.
02956
02957      IF PI-COMPANY-ID NOT = 'NCL'
02958          MOVE AL-SANOF           TO  LETRCDA
02959          MOVE AL-SADOF           TO  LETDESCA.
02960
02961      IF PI-COMPANY-ID  NOT = 'DDB' AND 'ANT' AND 'ASL' AND
02962                              'AN1' AND 'TFS'
02963          MOVE AL-SANOF           TO  BALCDA
02964          MOVE AL-SADOF           TO  BALPRTA.
02965
02966      IF PI-COMPANY-ID   = 'NCL'
033105        IF TYPEI =  'G' OR 'B'
02968            MOVE AL-UNNOF         TO  RPTCD2A
02969            MOVE AL-SANOF         TO  RPTCDDA.
02970
020816     IF PI-COMPANY-ID = 'DCC' or 'VPP'
011410        IF TYPEI = 'A'
011410           MOVE AL-SANOF         TO SPPDDHA
011410           MOVE AL-UANON         TO SPPDDA
011410        END-IF
011410     END-IF
02975      MOVE AL-SANON               TO  FLITYPA.
02976
02980      IF EIBTRNID NOT = TRANS-ID
02981         IF PI-AR-PROCESSING
02982              MOVE AL-SANOF       TO  SCDESCA
02983                                      NGDESCA
071712*                                    BALDESCA
02985                                      CKDESCA
02986              MOVE AL-UANON       TO  SUMMNOA
02987                                      NETGRSA
071712*                                    ARBALA
02989                                      CKPULLA.
02990
02991      IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'
02992          MOVE AL-UANOF           TO  LSTMDTA
02993          MOVE AL-SANOF           TO  PFK9A
02994      ELSE
02995          IF PI-AR-PROCESSING
02996              MOVE AL-SANOF       TO  PFK9A
02997          ELSE
02998              MOVE AL-SANOF       TO  LSTMDTA
02999              MOVE AL-SADOF       TO  PFK9A.
03000
111103*    IF (PI-ERC-TYPE NOT = 'G' AND SPACE)
111103*       OR (PI-COMPANY-ID NOT = 'DCC')
102908     IF PI-ERC-TYPE = 'A'
102908        MOVE AL-SANOF            TO PCONTA
102908     END-IF
033105     IF PI-ERC-TYPE NOT = 'B' AND SPACE
111103         MOVE AL-SADOF           TO  PFK10A
111204                                     PFK13A
111103         MOVE AL-SADOF           TO  CSLABLA
111103         MOVE AL-SADOF           TO  CLPSTA
111103         MOVE AL-SADOF           TO  MFLABLA
111103         MOVE AL-SADOF           TO  MAXFEEA
092205         MOVE AL-SADOF           TO  LFLABLA
092205         MOVE AL-SADOF           TO  MAXLFA
               MOVE AL-SADOF           TO  REFEHA
                                           REFEA
111103     END-IF
           if pi-company-id = 'AHL'
              move al-sanof            to ahl120a
                                          over120a
           end-if
03006      MOVE SAVE-DATE              TO  RUNDATEO.
03007      MOVE EIBTIME                TO  TIME-IN.
03008      MOVE TIME-OUT               TO  RUNTIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
03009      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
03010      MOVE -1                     TO  MAINTYPL.
03011
03012      
      * EXEC CICS SEND
03013 *        MAP     (MAP-NAME)
03014 *        MAPSET  (MAPSET-NAME)
03015 *        FROM    (EL652AO)
03016 *        ERASE
03017 *        CURSOR
03018 *    END-EXEC.
           MOVE LENGTH OF
            EL652AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007030' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303330' TO DFHEIV0(25:11)
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
           
03019
03020      GO TO 9100-RETURN-TRAN.
03021
03022  8200-SEND-DATAONLY.
03023
03024      IF  NOT CREDIT-SESSION
03025          MOVE AL-SADOF           TO PFK5A
03026                                     PFK6A.
03027
03028      IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'
03029          MOVE AL-UANOF           TO  LSTMDTA
03030          MOVE AL-SANOF           TO  PFK9A
03031      ELSE
03032          IF PI-AR-PROCESSING
03033              MOVE AL-SANOF       TO  PFK9A
03034          ELSE
03035              MOVE AL-SANOF       TO  LSTMDTA
03036              MOVE AL-SADOF       TO  PFK9A.
102908     IF PI-ERC-TYPE = 'A'
102908        MOVE AL-SANOF            TO PCONTA
102908     END-IF
111103
020816     IF PI-COMPANY-ID = 'DCC' or 'VPP'
011410        IF TYPEI = 'A'
011410           MOVE AL-SANOF         TO SPPDDHA
011410           MOVE AL-UANON         TO SPPDDA
011410        END-IF
011410     END-IF
111103*    IF (PI-ERC-TYPE NOT = 'G')
111103*       OR (PI-COMPANY-ID NOT = 'DCC')
111103     IF PI-ERC-TYPE NOT = 'B'
111103         MOVE AL-SADOF           TO  PFK10A
111204                                     PFK13A
111103         MOVE AL-SADOF           TO  CSLABLA
111103         MOVE AL-SADOF           TO  CLPSTA
111103         MOVE AL-SADOF           TO  MFLABLA
111103         MOVE AL-SADOF           TO  MAXFEEA
092205         MOVE AL-SADOF           TO  LFLABLA
092205         MOVE AL-SADOF           TO  MAXLFA
111103     END-IF.
03037
03038      MOVE SAVE-DATE              TO  RUNDATEO.
03039      MOVE EIBTIME                TO  TIME-IN.
03040      MOVE TIME-OUT               TO  RUNTIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
03041      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
03042
03043      
      * EXEC CICS SEND
03044 *        MAP     (MAP-NAME)
03045 *        MAPSET  (MAPSET-NAME)
03046 *        FROM    (EL652AO)
03047 *        DATAONLY
03048 *        CURSOR
03049 *    END-EXEC.
           MOVE LENGTH OF
            EL652AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00007085' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303835' TO DFHEIV0(25:11)
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
           
03050
03051      GO TO 9100-RETURN-TRAN.
03052
03053  8300-SEND-TEXT.
03054      
      * EXEC CICS SEND TEXT
03055 *        FROM    (LOGOFF-TEXT)
03056 *        LENGTH  (LOGOFF-LENGTH)
03057 *        ERASE
03058 *        FREEKB
03059 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007096' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303936' TO DFHEIV0(25:11)
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
           
03060
03061      
      * EXEC CICS RETURN
03062 *    END-EXEC.
      *    MOVE '.(                    ''   #00007103' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03063
03064  8400-LOG-JOURNAL-RECORD.
03065      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
03066      MOVE FILE-ID                TO  JP-FILE-ID.
03067      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
03068
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID  (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID  ('EL')
pemuni*        FROM     (JOURNAL-RECORD)
pemuni*        LENGTH   (473)
pemuni*    END-EXEC.
03075
03076  8800-UNAUTHORIZED-ACCESS.
03077      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
03078
03079      GO TO 8300-SEND-TEXT.
03080
03081  8810-PF23.
03082      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
03083      MOVE XCTL-005               TO  PGM-NAME.
03084
03085      GO TO 9300-XCTL.
03086
03087  8880-NOT-FOUND.
03088      MOVE ER-0142                TO  EMI-ERROR.
03089      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03090
03091      MOVE SPACE                  TO  PI-ERC-TYPE.
03092      MOVE -1                     TO  MAINTYPL.
03093
03094      IF EIBTRNID NOT = TRANS-ID
03095          GO TO 8100-SEND-INITIAL-MAP.
03096
03097      GO TO 8200-SEND-DATAONLY.
03098
03099  9100-RETURN-TRAN.
03100      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
03101      MOVE '652A'                 TO  PI-CURRENT-SCREEN-NO.
03102
03103      
      * EXEC CICS RETURN
03104 *        TRANSID   (TRANS-ID)
03105 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
03106 *        LENGTH    (PI-COMM-LENGTH)
03107 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00007145' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03108
03109  9200-RETURN-MAIN-MENU.
03110
03111      IF  MORTGAGE-SESSION
03112          MOVE XCTL-EM626         TO PGM-NAME
03113      ELSE
03114          IF  CREDIT-SESSION
03115              MOVE XCTL-626       TO PGM-NAME
03116          ELSE
03117              MOVE XCTL-126       TO PGM-NAME.
03118
03119      GO TO 9300-XCTL.
03120
03121  9300-XCTL.
03122      
      * EXEC CICS XCTL
03123 *        PROGRAM   (PGM-NAME)
03124 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
03125 *        LENGTH    (PI-COMM-LENGTH)
03126 *    END-EXEC.
      *    MOVE '.$C                   %   #00007164' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03127
03128  9400-CLEAR.
03129      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
03130
03131      GO TO 9300-XCTL.
03132
03133  9500-PF12.
03134      MOVE XCTL-010               TO  PGM-NAME.
03135
03136      GO TO 9300-XCTL.
03137
03138  9600-PGMID-ERROR.
03139      
      * EXEC CICS HANDLE CONDITION
03140 *        PGMIDERR  (8300-SEND-TEXT)
03141 *    END-EXEC.
      *    MOVE '"$L                   ! 7 #00007181' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303037313831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03142
03143      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
03144      MOVE ' '                    TO  PI-ENTRY-CD-1.
03145      MOVE XCTL-005               TO  PGM-NAME.
03146      MOVE PGM-NAME               TO  LOGOFF-PGM.
03147      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
03148
03149      GO TO 9300-XCTL.
03150
03151  9700-LINK-DATE-CONVERT.
03152      
      * EXEC CICS LINK
03153 *        PROGRAM   ('ELDATCV')
03154 *        COMMAREA  (DATE-CONVERSION-DATA)
03155 *        LENGTH    (DC-COMM-LENGTH)
03156 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00007194' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03157
03158  9700-EXIT.
03159      EXIT.
03160
03161  9900-ERROR-FORMAT.
03162      IF NOT EMI-ERRORS-COMPLETE
03163          MOVE LINK-001           TO  PGM-NAME
03164          
      * EXEC CICS LINK
03165 *            PROGRAM   (PGM-NAME)
03166 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
03167 *            LENGTH    (EMI-COMM-LENGTH)
03168 *        END-EXEC.
      *    MOVE '."C                   (   #00007206' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03169
03170  9900-EXIT.
03171      EXIT.
03172
03173  9910-INITIALIZE-SECURITY.
03174 ******************************************************************
03175 *                                                                *
03176 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
03177 *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *
03178 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
03179 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
03180 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
03181 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
03182 *                                                                *
03183 ******************************************************************
03184
03185      IF  PI-PROCESSOR-ID NOT = 'LGXX'
03186
03187          IF  MORTGAGE-SESSION
03188              MOVE '125E'             TO SC-QUID-SYSTEM
03189              MOVE EIBTRMID           TO SC-QUID-TERMINAL
03190
03191              
      * EXEC CICS READQ TS
03192 *                QUEUE  (SC-QUID-KEY)
03193 *                INTO   (SECURITY-CONTROL-E)
03194 *                LENGTH (SC-COMM-LENGTH-E)
03195 *                ITEM   (SC-ITEM)
03196 *            END-EXEC
      *    MOVE '*$II   L              ''   #00007233' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SC-QUID-KEY, 
                 SECURITY-CONTROL-E, 
                 SC-COMM-LENGTH-E, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03197
03198              MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)
03199                                      TO PI-DISPLAY-CAP
03200              MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)
03201                                      TO PI-MODIFY-CAP
03202
03203              IF  NOT DISPLAY-CAP
03204                  MOVE 'READ'         TO SM-READ
03205                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
03206                  MOVE ER-9097        TO EMI-ERROR
03207                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03208                  GO TO 8100-SEND-INITIAL-MAP
03209              ELSE
03210                  GO TO 9910-EXIT
03211          ELSE
03212              
      * EXEC CICS  READQ TS
03213 *                QUEUE   (PI-SECURITY-TEMP-STORE-ID)
03214 *                INTO    (SECURITY-CONTROL)
03215 *                LENGTH  (SC-COMM-LENGTH)
03216 *                ITEM    (SC-ITEM-CL-CR)
03217 *                END-EXEC
      *    MOVE '*$II   L              ''   #00007254' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM-CL-CR, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03218
03219              MOVE SC-CREDIT-DISPLAY (05)
03220                                  TO PI-DISPLAY-CAP
03221              MOVE SC-CREDIT-UPDATE  (05)
03222                                  TO PI-MODIFY-CAP
03223
03224              IF  NOT DISPLAY-CAP
03225                  MOVE 'READ'     TO SM-READ
03226                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
03227                  MOVE ER-0070    TO  EMI-ERROR
03228                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03229                  GO TO 8100-SEND-INITIAL-MAP.
03230
03231  9910-EXIT.
03232      EXIT.
03233
03234  9990-ABEND.
03235      MOVE LINK-004               TO  PGM-NAME.
03236      MOVE DFHEIBLK               TO  EMI-LINE1.
03237
03238      
      * EXEC CICS LINK
03239 *        PROGRAM   (PGM-NAME)
03240 *        COMMAREA  (EMI-LINE1)
03241 *        LENGTH    (72)
03242 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007280' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03243
03244      GO TO 8200-SEND-DATAONLY.
03245
03246  9995-SECURITY-VIOLATION.
03247 *           COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00007306' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
03248  9995-EXIT.
03249       EXIT.

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
