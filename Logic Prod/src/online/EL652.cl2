00001  IDENTIFICATION DIVISION.                                         10/18/96
00002                                                                   EL652
00003  PROGRAM-ID.                 EL652 .                                 LV059
00004 *              PROGRAM CONVERTED BY                                  CL*55
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*55
00006 *              CONVERSION DATE 09/14/94 07:57:28.                    CL*55
00007 *                            VMOD=2.059                              CL*59
00008 *                                                                 EL652
00009 *AUTHOR.        LOGIC,INC.                                           CL*55
00010 *               DALLAS, TEXAS.                                       CL*55
00024 *                                                                 EL652
00025 *REMARKS.                                                            CL**5
00026 *        TRANSACTION - EXD4 - COMPENSATION MASTER MAINT              CL*46
00024 *                                                                 EL652
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

00027                                                                   EL652
00028  ENVIRONMENT DIVISION.                                            EL652
00029  DATA DIVISION.                                                   EL652
00030  EJECT                                                            EL652
00031  WORKING-STORAGE SECTION.                                         EL652
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL652
00033  77  FILLER  PIC X(32)  VALUE '*    EL652 WORKING STORAGE     *'. EL652
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.059 *********'.    CL*59
       77  WS-TOT-FEES                 PIC S9(3)V99 COMP-3 VALUE +0.
       77  S1                          PIC S999 VALUE +0 COMP-3.
00035                                                                   EL652
00036  01  WS-DATE-AREA.                                                EL652
00037      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.        EL652
00038      12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.        EL652
00039                                                                   EL652
00040  01  STANDARD-AREAS.                                              EL652
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
00041      12  RETURNED-FROM       PIC X(8)        VALUE SPACES.           CL*36
00042      12  QID-PI.                                                     CL*49
00043          16  QID-TERM        PIC X(4).                               CL*36
00044          16  FILLER          PIC X(4)        VALUE '652A'.           CL*36
00045      12  QID-ITEM            PIC S9(4) COMP  VALUE +0001.            CL*44
00046      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.         EL652
00047      12  MAP-NAME            PIC  X(8)       VALUE 'EL652A'.      EL652
00048      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL652S'.      EL652
00049      12  TRANS-ID            PIC  X(4)       VALUE 'EXD4'.        EL652
00050      12  EL640-TRANS-ID      PIC  X(4)       VALUE 'EXC1'.        EL652
00051      12  EL642-TRANS-ID      PIC  X(4)       VALUE 'EXH7'.        EL652
00052      12  EL633-TRANS-ID      PIC  X(4)       VALUE 'EXB7'.        EL652
00053      12  EL633DMD-TRANS-ID   PIC  X(4)       VALUE 'EX1F'.           CL*56
00054      12  EL635-TRANS-ID      PIC  X(4)       VALUE 'EXJ4'.           CL**6
00055      12  EL6501-TRANS-ID     PIC  X(4)       VALUE 'EXC5'.           CL**6
00056      12  EL6592-TRANS-ID     PIC  X(4)       VALUE 'EX66'.           CL*21
00057      12  EL856-TRANS-ID      PIC  X(4)       VALUE 'EXJ8'.           CL**6
00058      12  EM6508-TRANS-ID     PIC  X(4)       VALUE 'MXG8'.           CL*22
00059      12  THIS-PGM            PIC  X(8)       VALUE 'EL652'.       EL652
00060      12  PGM-NAME            PIC  X(8).                           EL652
00061      12  WS-COMP-CD-R.                                            EL652
00062          16  FILLER          PIC  X.                              EL652
00063          16  WS-COMP-CD-X    PIC  X.                              EL652
00064      12  WS-COMP-CD  REDEFINES                                    EL652
00065          WS-COMP-CD-R        PIC S9(4)                  COMP.     EL652
00066      12  TIME-IN             PIC S9(7).                           EL652
00067      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL652
00068          16  FILLER          PIC  X.                              EL652
00069          16  TIME-OUT        PIC  99V99.                          EL652
00070          16  FILLER          PIC  XX.                             EL652
00071      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL652
00072      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL652
00073      12  XCTL-126            PIC  X(8)       VALUE 'EL126'.          CL*19
00074      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.          CL*19
00075      12  XCTL-633            PIC  X(8)       VALUE 'EL633'.          CL*36
00076      12  XCTL-633DMD         PIC  X(8)       VALUE 'EL633DMD'.       CL*56
00077      12  XCTL-635            PIC  X(8)       VALUE 'EL635'.          CL*44
00077      12  XCTL-650            PIC  X(8)       VALUE 'EL650'.          CL*44
00078      12  XCTL-6521           PIC  X(8)       VALUE 'EL6521'.         CL*40
102202     12  XCTL-6522           PIC  X(8)       VALUE 'EL6522'.         CL*40
111103     12  XCTL-6523           PIC  X(8)       VALUE 'EL6523'.         CL*40
111204     12  XCTL-6524           PIC  X(8)       VALUE 'EL6524'.         CL*40
00079      12  XCTL-689            PIC  X(8)       VALUE 'EL689'.          CL*46
00080      12  XCTL-690            PIC  X(8)       VALUE 'EL690'.          CL*46
00081      12  XCTL-856            PIC  X(8)       VALUE 'EL856'.          CL**6
00082      12  XCTL-EM626          PIC  X(8)       VALUE 'EM626'.          CL*46
00083      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL652
00084      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL652
00085      12  LINK-ELDATCV        PIC  X(8)       VALUE 'ELDATCV'.        CL*36
00086      12  FILE-ID             PIC  X(8)       VALUE SPACES.        EL652
00087      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.      EL652
00088      12  NAME-FILE-ID        PIC  X(8)       VALUE 'ERNAME'.         CL*21
00089      12  CNTL-FILE-ID        PIC  X(8)       VALUE 'ELCNTL'.      EL652
00090      12  SUMM-FILE-ID        PIC  X(8)       VALUE 'ERSUMM'.         CL**6
00091      12  RQST-FILE-ID        PIC  X(8)       VALUE 'ERRQST'.         CL*34
00092      12  RQST-FILE-ID-3      PIC  X(8)       VALUE 'ERRQST3'.        CL*34
00093                                                                   EL652
00094  01  MISC-WORK-AREAS.                                             EL652
CIDMOD     12  WS-SRCH-STATE       PIC X(30)  VALUE SPACES.
CIDMOD     12  STNDX               PIC S999   COMP-3 VALUE +0.
00095      12  W-APPL-SCRTY-NDX    PIC S9(04) COMP   VALUE +35.            CL*19
00096      12  WS-PHONE-IN         PIC  9(10).                          EL652
00097      12  WS-PHONE-IN-R  REDEFINES  WS-PHONE-IN.                   EL652
00098          16  WSPI-AREA       PIC  X(3).                           EL652
00099          16  WSPI-PFX        PIC  X(3).                           EL652
00100          16  WSPI-SFX        PIC  X(4).                           EL652
00101      12  WS-PHONE-OUT.                                            EL652
00102          16  WSPO-AREA       PIC  X(3).                           EL652
00103          16  FILLER          PIC  X            VALUE '-'.         EL652
00104          16  WSPO-PFX        PIC  X(3).                           EL652
00105          16  FILLER          PIC  X            VALUE '-'.         EL652
00106          16  WSPO-SFX        PIC  X(4).                           EL652
00107      12  DEEDIT-FIELD        PIC  X(15).                          EL652
00108      12  DEEDIT-FIELD-V0  REDEFINES                               EL652
00109          DEEDIT-FIELD        PIC S9(15).                          EL652
00110      12  DEEDIT-FIELD-V1  REDEFINES                               EL652
00111          DEEDIT-FIELD        PIC S9(13)V99.                       EL652
00112      12  SUB1                PIC S9(4)         VALUE +0   COMP.   EL652
00113      12  SUB2                PIC S9(4)         VALUE +0   COMP.   EL652
00114      12  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.      CL*19
00115      12  ERCOMP-LENGTH       PIC S9(4)         VALUE +700 COMP.      CL*38
00116      12  ERSUMM-LENGTH       PIC S9(4)         VALUE +150 COMP.      CL**6
00117      12  ERNAME-LENGTH       PIC S9(4)         VALUE +160 COMP.      CL*21
00118      12  SV-CLMTOL           PIC  9(3)V99      VALUE ZEROS.       EL652
00119      12  WK-DATE.                                                 EL652
00120          16  WK-MO           PIC  99.                             EL652
00121          16  WK-DA           PIC  99.                             EL652
00122          16  WK-YR           PIC  99.                             EL652
00123      12  DATE-TEST-AREA      PIC  9(6).                           EL652
00124      12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.             EL652
00125          16  DATE-TEST-MM    PIC  99.                             EL652
00126          16  DATE-TEST-DD    PIC  99.                             EL652
00127          16  DATE-TEST-YY    PIC  99.                             EL652
00128      12  DIVIDE-RESULT       PIC  99.                             EL652
00129      12  DIVIDE-REMAINDER    PIC  9.                              EL652
00130      12  WS-ZERO             PIC  X            VALUE '0'.         EL652
00131      12  WS-ONE              PIC  X            VALUE '1'.         EL652
00132      12  WS-TWO              PIC  X            VALUE '2'.         EL652
00133      12  WS-ACCESS.                                               EL652
00134          16  FILLER          PIC  X(3)         VALUE SPACES.      EL652
00135          16  WS-CARRIER      PIC  X.                              EL652
00136      12  BIN-CURRENT-SAVE    PIC  XX.                             EL652
00137      12  WS-BROWSE-SW        PIC  X.                              EL652
00138          88  BROWSE-STARTED                    VALUE 'Y'.         EL652
00139      12  WS-SAVE-KEY         PIC  X(29)        VALUE SPACES.      EL652
00140      12  WS-SAVE-SUMM        PIC  X(6)         VALUE SPACES.         CL**6
00141      12  WS-SUMM-FOR-RQST    PIC  X(6)         VALUE SPACES.         CL*34
00142      12  WS-SAVE-RQST        PIC X(46)         VALUE SPACES.         CL*34
00143      12  WS-SHOW-SAVE-TOTALS PIC  X            VALUE SPACE.       EL652
00144          88  SHOW-SAVE-TOTALS                  VALUE 'Y'.         EL652
00145      12  ELCNTL-KEY.                                              EL652
00146          16  CNTL-COMP-ID    PIC  X(3)         VALUE SPACES.      EL652
00147          16  CNTL-REC-TYPE   PIC  X            VALUE SPACES.      EL652
00148          16  CNTL-ACCESS     PIC  X(4)         VALUE SPACES.      EL652
00149          16  CNTL-SEQ-NO     PIC S9(4)         VALUE +0   COMP.   EL652
111103     12  ELCNTL-KEY2. 
111103         16  CNTL-COMP-ID-2  PIC  X(3)         VALUE SPACES.      EL652
111103         16  CNTL-REC-TYPE-2 PIC  X            VALUE SPACES.      EL652
111103         16  CNTL-STATE-2    PIC  X(2)         VALUE SPACES.      EL652
111103         16  FILLER          PIC  X(2)         VALUE SPACES.
111103         16  CNTL-SEQ-NO-2   PIC S9(4)         VALUE +0   COMP.   EL652
00150      12  ERSUMM-KEY.                                                 CL**6
00151          16  SUMM-COMP-ID    PIC  X            VALUE SPACES.         CL**6
00152          16  SUMM-SUMMARY    PIC  X(6)         VALUE SPACES.         CL**6
00153          16  SUMM-CARRIER    PIC  X            VALUE SPACES.         CL**6
00154          16  SUMM-GROUP      PIC  X(6)         VALUE SPACES.         CL**6
00155          16  SUMM-FIN-RESP   PIC  X(10)        VALUE SPACES.         CL**6
00156          16  SUMM-ACCT-AGENT PIC  X(10)        VALUE SPACES.         CL**6
00157      12  ERRQST-KEY.                                                 CL*34
00158          16  RQST-COMP-ID-PC PIC  X            VALUE SPACES.         CL*34
00159          16  RQST-BATCH-PC   PIC  X(6)         VALUE SPACES.         CL*34
00160      12  ERRQST-KEY-3.                                               CL*34
00161          16  RQST-COMP-ID    PIC  X            VALUE SPACES.         CL*34
00162          16  RQST-CARRIER    PIC  X            VALUE SPACES.         CL*34
00163          16  RQST-GROUP      PIC  X(6)         VALUE SPACES.         CL*34
00164          16  RQST-FIN-RESP   PIC  X(10)        VALUE SPACES.         CL*34
00165          16  RQST-ACCT-AGENT PIC  X(10)        VALUE SPACES.         CL*34
00166          16  RQST-REFERENCE  PIC  X(12)        VALUE SPACES.         CL*34
00167          16  RQST-BATCH      PIC  X(06)        VALUE SPACES.         CL*34
00168      12  WS-DATE-MDY-8.                                           EL652
00169          16  WS-DMDY8-MM     PIC  XX.                             EL652
00170          16  WS-DMDY8-SL1    PIC  X.                              EL652
00171          16  WS-DMDY8-DD     PIC  XX.                             EL652
00172          16  WS-DMDY8-SL2    PIC  X.                              EL652
00173          16  WS-DMDY8-YY     PIC  XX.                             EL652
00174      12  WS-DATE-MDY-6  REDEFINES  WS-DATE-MDY-8.                 EL652
00175          16  WS-DMDY6-MM     PIC  XX.                             EL652
00176          16  WS-DMDY6-DD     PIC  XX.                             EL652
00177          16  WS-DMDY6-YY     PIC  XX.                             EL652
00178          16  WS-DMDY6-BLNK   PIC  XX.                             EL652
00179      12  WS-YMD-DATE.                                             EL652
00180          16  WS-YMD-YY       PIC  XX.                             EL652
00181          16  WS-YMD-MM       PIC  XX.                             EL652
00182          16  WS-YMD-DD       PIC  XX.                             EL652
00183      12  WS-YMD-DATE-NUM  REDEFINES                               EL652
00184          WS-YMD-DATE         PIC  9(6).                           EL652
00185      12  WS-SAVE-SUMMARY     PIC X(6)          VALUE SPACES.         CL**6
00186      12  WS-SAVE-NAME        PIC X(30)         VALUE SPACES.         CL**6
00187      12  WS-SAVE-BALFWD      PIC S9(7)V99      VALUE +0   COMP-3. EL652
00188      12  WS-SAVE-CURCOM      PIC S9(7)V99      VALUE +0   COMP-3. EL652
00189      12  WS-SAVE-CURCHG      PIC S9(7)V99      VALUE +0   COMP-3. EL652
00190      12  WS-SAVE-CURPMT      PIC S9(7)V99      VALUE +0   COMP-3. EL652
00191      12  WS-SAVE-ENDBAL      PIC S9(7)V99      VALUE +0   COMP-3. EL652
00192      12  WS-SAVE-YTDCOM      PIC S9(7)V99      VALUE +0   COMP-3. EL652
00193      12  WS-SOC-SEC-WORK-FIELD.                                      CL*41
00194          16  WS-SS-TYPE          PIC X.                              CL*41
00195          16  WS-SOC-SEC          PIC X(12).                          CL*41
00196                                                                      CL*28
00197      12  WS-ZIP-CODE.                                                CL*28
00198          16  WS-ZIP-1            PIC X.                              CL*28
00199              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.              CL*28
00200          16  WS-ZIP-2-3          PIC XX.                             CL*28
00201          16  WS-ZIP-4            PIC X.                              CL*28
00202          16  WS-ZIP-5            PIC X.                              CL*28
00203          16  WS-ZIP-6            PIC X.                              CL*28
00204          16  FILLER              PIC X(4).                           CL*28
00205      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.                        CL*28
00206          16  WS-ZIP-AM-1-CODE    PIC X(5).                           CL*28
00207          16  WS-ZIP-AM-1-PLUS4   PIC X(4).                           CL*28
00208          16  FILLER              PIC X.                              CL*28
00209      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.                        CL*28
00210          16  WS-ZIP-AM-2-CODE    PIC X(5).                           CL*28
00211          16  WS-ZIP-AM-2-DASH    PIC X.                              CL*28
00212          16  WS-ZIP-AM-2-PLUS4   PIC X(4).                           CL*28
00213      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.                       CL*28
00214          16  WS-ZIP-CAN-1-POST1  PIC XXX.                            CL*28
00215          16  WS-ZIP-CAN-1-POST2  PIC XXX.                            CL*28
00216          16  FILLER              PIC X(4).                           CL*28
00217      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.                       CL*28
00218          16  WS-ZIP-CAN-2-POST1  PIC XXX.                            CL*28
00219          16  FILLER              PIC X.                              CL*28
00220          16  WS-ZIP-CAN-2-POST2  PIC XXX.                            CL*28
00221          16  FILLER              PIC XXX.                            CL*28
00222                                                                      CL*28
00223  EJECT                                                            EL652
00224  01  ERROR-NUMBERS.                                               EL652
00225      12  ER-0000             PIC  X(4)       VALUE '0000'.        EL652
00226      12  ER-0002             PIC  X(4)       VALUE '0002'.        EL652
00227      12  ER-0004             PIC  X(4)       VALUE '0004'.        EL652
00228      12  ER-0008             PIC  X(4)       VALUE '0008'.        EL652
00229      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL652
00230      12  ER-0033             PIC  X(4)       VALUE '0033'.           CL**6
CIDMOD     12  ER-0035             PIC  X(4)       VALUE '0035'.           CL**6
00231      12  ER-0046             PIC  X(4)       VALUE '0046'.           CL*51
00232      12  ER-0068             PIC  X(4)       VALUE '0068'.        EL652
00233      12  ER-0070             PIC  X(4)       VALUE '0070'.        EL652
00234      12  ER-0142             PIC  X(4)       VALUE '0142'.        EL652
111103     12  ER-0144             PIC  X(4)       VALUE '0144'.
111103     12  ER-0187             PIC  X(4)       VALUE '0187'.
00235      12  ER-0193             PIC  X(4)       VALUE '0193'.        EL652
00236      12  ER-0314             PIC  X(4)       VALUE '0314'.        EL652
00237      12  ER-0584             PIC  X(4)       VALUE '0584'.        EL652
00238      12  ER-0829             PIC  X(4)       VALUE '0829'.           CL*43
00240      12  ER-1299             PIC  X(4)       VALUE '1299'.        EL652
111103     12  ER-1778             PIC  X(4)       VALUE '1778'.        EL652
00239      12  ER-1883             PIC  X(4)       VALUE '1883'.           CL*31
00240      12  ER-2039             PIC  X(4)       VALUE '2039'.        EL652
00241      12  ER-2042             PIC  X(4)       VALUE '2042'.        EL652
00242      12  ER-2045             PIC  X(4)       VALUE '2045'.        EL652
00243      12  ER-2046             PIC  X(4)       VALUE '2046'.        EL652
00244      12  ER-2047             PIC  X(4)       VALUE '2047'.        EL652
00245      12  ER-2048             PIC  X(4)       VALUE '2048'.        EL652
00246      12  ER-2049             PIC  X(4)       VALUE '2049'.        EL652
00247      12  ER-2050             PIC  X(4)       VALUE '2050'.        EL652
00248      12  ER-2055             PIC  X(4)       VALUE '2055'.        EL652
00249      12  ER-2056             PIC  X(4)       VALUE '2056'.        EL652
00250      12  ER-2057             PIC  X(4)       VALUE '2057'.        EL652
00251      12  ER-2067             PIC  X(4)       VALUE '2067'.        EL652
00252      12  ER-2088             PIC  X(4)       VALUE '2088'.        EL652
00253      12  ER-2089             PIC  X(4)       VALUE '2089'.        EL652
00254      12  ER-2091             PIC  X(4)       VALUE '2091'.        EL652
00255      12  ER-2092             PIC  X(4)       VALUE '2092'.        EL652
00256      12  ER-2093             PIC  X(4)       VALUE '2093'.        EL652
00257      12  ER-2094             PIC  X(4)       VALUE '2094'.        EL652
00258      12  ER-2095             PIC  X(4)       VALUE '2095'.        EL652
00259      12  ER-2096             PIC  X(4)       VALUE '2096'.        EL652
00260      12  ER-2097             PIC  X(4)       VALUE '2097'.        EL652
           12  ER-2209             PIC  X(4)       VALUE '2209'.
00261      12  ER-2238             PIC  X(4)       VALUE '2238'.           CL*20
00262      12  ER-2370             PIC  X(4)       VALUE '2370'.           CL**2
00263      12  ER-2572             PIC  X(4)       VALUE '2572'.        EL652
00263      12  ER-2717             PIC  X(4)       VALUE '2717'.
072406     12  ER-2790             PIC  X(4)       VALUE '2790'.
00264      12  ER-2872             PIC  X(4)       VALUE '2872'.           CL*40
00265      12  ER-3021             PIC  X(4)       VALUE '3021'.           CL*50
00266      12  ER-3053             PIC  X(4)       VALUE '3053'.           CL*42
00267      12  ER-3055             PIC  X(4)       VALUE '3055'.           CL*48
00268      12  ER-3056             PIC  X(4)       VALUE '3056'.           CL*52
00269      12  ER-3150             PIC  X(4)       VALUE '3150'.           CL**6
00270      12  ER-3151             PIC  X(4)       VALUE '3151'.           CL**6
00271      12  ER-3152             PIC  X(4)       VALUE '3152'.           CL**6
00272      12  ER-3153             PIC  X(4)       VALUE '3153'.           CL**6
00273      12  ER-3154             PIC  X(4)       VALUE '3154'.           CL**6
00274      12  ER-3168             PIC  X(4)       VALUE '3168'.           CL*10
00275      12  ER-3170             PIC  X(4)       VALUE '3170'.           CL*12
00276      12  ER-3174             PIC  X(4)       VALUE '3174'.           CL*14
111103     12  ER-3261             PIC  X(4)       VALUE '3261'.           CL*14
00277      12  ER-9096             PIC  X(4)       VALUE '9096'.           CL*19
00278      12  ER-9097             PIC  X(4)       VALUE '9097'.
           12  ER-9999             PIC  X(4)       VALUE '9999'.
00279  EJECT                                                            EL652
00280                            COPY ELCSCTM.                             CL*31
00281  EJECT                                                            EL652
00282                            COPY ELCSCRTY.                            CL*31
00283                            COPY MPCSCRT.                             CL*31
00284  EJECT                                                            EL652
00285  01  FILLER                  PIC X(16)  VALUE ALL 'A'.               CL*35
00286                            COPY ELCDATE.                             CL*31
00287  EJECT                                                            EL652
00288                            COPY ELCLOGOF.                            CL*31
00289  EJECT                                                            EL652
00290                            COPY ELCATTR.                             CL*31
00291  EJECT                                                            EL652
00292                            COPY ELCEMIB.                             CL*31
00293  EJECT                                                            EL652
00294  01  FILLER                  PIC X(16)  VALUE ALL 'B'.               CL*35
00295                            COPY ELCINTF.                             CL*31
00296      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.           EL652
00297          16  PI-CHECK-MAINT-TYPE     PIC  X.                      EL652
00298              88  VALID-MAINT-TYPE            VALUE 'S' 'A'        EL652
00299                                                    'C' 'D'.       EL652
00300              88  ADD-FUNCTION                VALUE 'A'.           EL652
00301              88  SHOW-FUNCTION               VALUE 'S'.           EL652
00302              88  DELETE-FUNCTION             VALUE 'D'.           EL652
00303              88  CHANGE-FUNCTION             VALUE 'C'.           EL652
00304          16  PI-CHECK-TYPE           PIC  X.                      EL652
033105             88  VALID-TYPE                  VALUE 'A' 'B' 'C'
00306                                                            'G'.   EL652
00307          16  PI-CHECK-CARRY-BAL      PIC  X.                      EL652
00308              88  VALID-CARRY-BAL             VALUE 'Y' 'N'.       EL652
00309          16  PI-FIRST-TIME-SW        PIC  X.                      EL652
00310              88  FIRST-TIME                  VALUE 'Y'.           EL652
00311          16  PI-ERCOMP-EOF-SW        PIC  X.                      EL652
00312              88  ERCOMP-EOF                  VALUE 'Y'.           EL652
00313          16  PI-SAVE-PHONE           PIC  X(10).                  EL652
00314          16  PI-SAVE-PHONE-RED REDEFINES PI-SAVE-PHONE  PIC 9(10).EL652
00315          16  PI-ERC-END-BAL          PIC S9(9)V99       COMP-3.      CL*44
00316          16  PI-ERCOMP-KEY.                                       EL652
00317              20  PI-ERC-COMPANY-CD   PIC  X.                         CL*50
00318              20  PI-ERC-CARRIER      PIC  X.                      EL652
00319              20  PI-ERC-GROUP        PIC  X(6).                      CL**6
00320              20  PI-ERC-RESP         PIC  X(10).                  EL652
00321              20  PI-ERC-ACCT         PIC  X(10).                  EL652
00322              20  PI-ERC-TYPE         PIC  X.                      EL652
00323          16  PI-SAVE-ERCOMP-KEY      PIC  X(29).                  EL652
00324          16  PI-SAVE-FAXNO           PIC  X(10).                     CL*48
00325          16  PI-SAVE-FAXNO-RED REDEFINES PI-SAVE-FAXNO  PIC 9(10).   CL*48
CIDMOD         16  PI-SAVE-ADDR2           PIC X(30).
CIDMOD         16  PI-SAVE-CITYST.
                   20  PI-SAVE-CITY        PIC X(28).
CIDMOD             20  PI-SAVE-STATE       PIC XX.
102202         16  PI-SAVE-ACCT-NAME       PIC X(30).
pemtst         16  PI-EL652-DEL-SW         PIC X.
               16  PI-UPDATE-SW            PIC X.
102202         16  FILLER                  PIC  X(457).                    CL*55
00327  EJECT                                                            EL652
00328                              COPY ELCJPFX.                           CL*31
00329                              PIC  X(750).                            CL*30
00330  EJECT                                                            EL652
00331                              COPY ELCAID.                            CL*31
00332                                                                   EL652
00333  01  FILLER  REDEFINES  DFHAID.                                   EL652
00334      12  FILLER              PIC  X(8).                           EL652
00335      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.      EL652
00336  EJECT                                                            EL652
00337                              COPY EL652S.
00338  EJECT                                                            EL652
00339  LINKAGE SECTION.                                                 EL652
00340                                                                   EL652
00341  01  DFHCOMMAREA             PIC  X(1024).                        EL652
00342                                                                   EL652
00343 *01 PARMLIST .                                                       CL*55
00344 *    12  FILLER              PIC S9(8)                  COMP.        CL*55
00345 *    12  ERCOMP-POINTER      PIC S9(8)                  COMP.        CL*55
00346 *    12  ERNAME-POINTER      PIC S9(8)                  COMP.        CL*55
00347 *    12  ELCNTL-POINTER      PIC S9(8)                  COMP.        CL*55
00348 *    12  ERSUMM-POINTER      PIC S9(8)                  COMP.        CL*55
00349 *    12  ERRQST-POINTER      PIC S9(8)                  COMP.        CL*55
00350  EJECT                                                            EL652
00351                              COPY ERCCOMP.                           CL*31
                                   COPY ERCAGTC.
00352  EJECT                                                               CL*21
00353                              COPY ERCNAME.                           CL*31
00354  EJECT                                                            EL652
00355                              COPY ELCCNTL.                           CL*31
00356  EJECT                                                               CL**6
00357                              COPY ERCSUMM.                           CL*31
00358  EJECT                                                               CL*34
00359                              COPY ERCRQST.                           CL*34
00360  EJECT                                                            EL652
00361  PROCEDURE DIVISION.                                              EL652
00362                                                                   EL652
00363      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL652
00364      MOVE '5'                    TO  DC-OPTION-CODE.              EL652
00365      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.             EL652
00366      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL652
00367      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL652
00368                                                                   EL652
00369      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL652
00370      MOVE EIBTRMID               TO  QID-TERM.                       CL*36
00371                                                                   EL652
00372  1000-START.                                                      EL652
00373      IF EIBCALEN = ZERO                                           EL652
00374          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL652
00375                                                                      CL*36
00376      IF PI-RETURN-TO-PROGRAM = THIS-PGM                              CL*50
00377          MOVE PI-CALLING-PROGRAM        TO  RETURNED-FROM.           CL*50
00378                                                                   EL652
00379      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL652
00380          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL652
00381              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL652
00382              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL652
00383              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL652
00384              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL652
00385              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL652
00386              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL652
00387              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL652
00388              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL652
00389              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT         CL*19
00390          ELSE                                                     EL652
00391              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL652
00392              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL652
00393              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL652
00394              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL652
00395              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL652
00396              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL652
00397              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL652
00398              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   EL652
00399                                                                   EL652
00400      IF  RETURNED-FROM = XCTL-633                                    CL*46
00401              OR                                                      CL*50
00402          RETURNED-FROM = XCTL-633DMD                                 CL*56
00403              OR                                                      CL*56
00404          RETURNED-FROM = XCTL-635                                    CL*50
00405              OR                                                      CL*46
00406          RETURNED-FROM = XCTL-6521                                   CL*46
00407              OR                                                      CL*46
102202         RETURNED-FROM = XCTL-6522                                   CL*46
102202             OR                                                      CL*46
111103         RETURNED-FROM = XCTL-6523                                   CL*46
111103             OR                                                      CL*46
111204         RETURNED-FROM = XCTL-6524
111204             OR
00408          RETURNED-FROM = XCTL-689                                    CL*46
00409              OR                                                      CL*46
00410          RETURNED-FROM = XCTL-690                                    CL*46
CIDMOD             OR                                                      CL*46
CIDMOD         RETURNED-FROM = XCTL-650                                    CL*46
00411          GO TO 3475-RECOVER-TEMP-STOR-PI.                            CL*46
00412                                                                      CL*36
00413      EXEC CICS HANDLE CONDITION                                   EL652
00414          NOTOPEN   (9990-ABEND)                                   EL652
00415          NOTFND    (8880-NOT-FOUND)                               EL652
00416          PGMIDERR  (9600-PGMID-ERROR)                             EL652
00417          ERROR     (9990-ABEND)                                   EL652
00418      END-EXEC.                                                    EL652
00419                                                                   EL652
00420      IF  EIBTRNID NOT = TRANS-ID                                     CL*22
00421          MOVE LOW-VALUES         TO  EL652AI                      EL652
00422          IF  EIBTRNID = EM6508-TRANS-ID                              CL*22
00423              MOVE DFHENTER       TO  EIBAID                          CL*22
00424              MOVE 'S'            TO  MAINTYPI                        CL*22
00425              MOVE PI-CARRIER     TO  CARRIERI                        CL*22
00426              MOVE PI-GROUPING    TO  GROUPI                          CL*22
00427              MOVE 'A'            TO  TYPEI                           CL*22
00428              MOVE PI-CR-FIN-RESP TO  FINRESPI                        CL*22
00429              MOVE PI-CR-ACCOUNT  TO  ACCTNOI                         CL*22
00430              MOVE 1              TO  CARRIERL TYPEL MAINTYPL         CL*22
00431              MOVE 6              TO  GROUPL                          CL*22
00432              MOVE 10             TO  FINRESPL  ACCTNOL               CL*22
00433              GO TO 4000-EDIT-MAINT                                   CL*22
00434          ELSE                                                        CL*22
00435              IF  (EIBTRNID NOT = EL640-TRANS-ID AND                  CL*22
00436                                  EL633-TRANS-ID AND                  CL*22
00437                                  EL633DMD-TRANS-ID AND               CL*56
00438                                  EL635-TRANS-ID AND                  CL*22
00439                                  EL642-TRANS-ID AND                  CL*22
00440                                  EL6501-TRANS-ID AND                 CL*22
00441                                  EL6592-TRANS-ID AND                 CL*22
00442                                  EL856-TRANS-ID)                     CL*22
00443                  OR PI-CR-FIN-RESP = SPACES                          CL*22
00444                  GO TO 8100-SEND-INITIAL-MAP                         CL**6
00445              ELSE                                                    CL*22
00446                  MOVE DFHENTER   TO  EIBAID                          CL*22
00447                  MOVE 'S'        TO  MAINTYPI                        CL*22
00448                  MOVE PI-CR-CARRIER  TO  CARRIERI                    CL*22
00449                  MOVE PI-CR-GROUPING TO  GROUPI                      CL*22
00450                  MOVE PI-CR-TYPE     TO  TYPEI                       CL*22
00451                  MOVE PI-CR-FIN-RESP TO  FINRESPI                    CL*22
00452                  MOVE PI-CR-ACCOUNT  TO  ACCTNOI                     CL*22
00453                  MOVE 1              TO  CARRIERL                    CL*36
00454                                          TYPEL                       CL*36
00455                                          MAINTYPL                    CL*36
00456                  MOVE 6              TO  GROUPL                      CL*22
00457                  MOVE 10             TO  FINRESPL                    CL*36
00458                                          ACCTNOL                     CL*36
00459                  GO TO 4000-EDIT-MAINT.                              CL*22
00460                                                                   EL652
00461      IF  EIBAID = DFHCLEAR                                           CL*19
00462              OR                                                      CL*19
00463          NOT DISPLAY-CAP                                             CL*19
00464          GO TO 9400-CLEAR.                                        EL652
00465                                                                   EL652
00466  EJECT                                                            EL652
00467  2000-RECEIVE.                                                    EL652
00468      MOVE LOW-VALUES             TO  EL652AI.                     EL652
00469                                                                   EL652
00470      IF EIBAID = DFHPA1 OR  DFHPA2  OR  DFHPA3                    EL652
00471          MOVE ER-0008            TO  EMI-ERROR                    EL652
00472          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL652
00473          MOVE -1                 TO  MAINTYPL                     EL652
00474          GO TO 8200-SEND-DATAONLY.                                EL652
00475                                                                   EL652
00476      EXEC CICS RECEIVE                                            EL652
00477          MAP     (MAP-NAME)                                       EL652
00478          MAPSET  (MAPSET-NAME)                                    EL652
00479          INTO    (EL652AI)                                        EL652
00480      END-EXEC.                                                    EL652
00481                                                                   EL652
00482      IF PFKEYL = ZERO                                             EL652
00483          GO TO 3000-CHECK-PFKEYS.                                 EL652
00484                                                                   EL652
00485      IF EIBAID NOT = DFHENTER                                     EL652
00486          MOVE ER-0004            TO EMI-ERROR                     EL652
00487          GO TO 3100-INPUT-ERROR.                                  EL652
00488                                                                   EL652
00489      IF (PFKEYI NUMERIC) AND (PFKEYI > 0 AND < 25)                   CL**6
00490          MOVE PF-VALUES (PFKEYI)  TO  EIBAID                      EL652
00491      ELSE                                                         EL652
00492          MOVE ER-0029             TO  EMI-ERROR                   EL652
00493          GO TO 3100-INPUT-ERROR.                                  EL652
00494  EJECT                                                            EL652
00495  3000-CHECK-PFKEYS.                                               EL652
00496      IF EIBAID = DFHPF23                                          EL652
00497          GO TO 8810-PF23.                                         EL652
00498                                                                   EL652
00499      IF EIBAID = DFHPF24                                          EL652
00500          GO TO 9200-RETURN-MAIN-MENU.                             EL652
00501                                                                   EL652
00502      IF EIBAID = DFHPF12                                          EL652
00503          GO TO 9500-PF12.                                         EL652
00504                                                                   EL652
pemtst     if (maintypl not = zeros)
pemtst        and (maintypi not = 'D')
pemtst        move spaces              to pi-el652-del-sw
pemtst     end-if

00505      IF EIBAID = DFHPF1                                           EL652
00506          IF PI-CHECK-MAINT-TYPE = 'C' OR 'D'                      EL652
00507              MOVE 'S'            TO  MAINTYPO                     EL652
00508              MOVE AL-UANON       TO  MAINTYPA                     EL652
00509              MOVE 1              TO  MAINTYPL                     EL652
00510              GO TO 7250-PAGE-FORWARD                              EL652
00511          ELSE                                                     EL652
00512              GO TO 7250-PAGE-FORWARD.                             EL652
00513                                                                      CL*20
00514      IF EIBAID = DFHPF2                                              CL*20
00515          IF PI-CHECK-MAINT-TYPE = 'C' OR 'D'                         CL*20
00516              MOVE 'S'            TO  MAINTYPO                        CL*20
00517              MOVE AL-UANON       TO  MAINTYPA                        CL*20
00518              MOVE 1              TO  MAINTYPL                        CL*20
00519              GO TO 7300-PAGE-BACKWARD                                CL*20
00520          ELSE                                                        CL*20
00521              GO TO 7300-PAGE-BACKWARD.                               CL*20
00522                                                                   EL652
00523      IF EIBAID = DFHPF3                                              CL*36
00524         IF PI-AR-PROCESSING                                          CL*44
00525             IF PI-ERCOMP-KEY  NOT =       SPACES                     CL*44
00526                 MOVE PI-ERC-CARRIER TO  PI-CR-CARRIER                CL*44
00527                 MOVE PI-ERC-GROUP   TO  PI-CR-GROUPING               CL*44
00528                 MOVE PI-ERC-RESP    TO  PI-CR-FIN-RESP               CL*44
00529                 MOVE PI-ERC-ACCT    TO  PI-CR-ACCOUNT                CL*44
00530                 MOVE PI-ERC-TYPE    TO  PI-CR-TYPE                   CL*50
00531                 IF PI-CR-ACCOUNT =  LOW-VALUES                       CL*44
00532                    MOVE 'G'         TO  PI-CR-TYPE                   CL*44
00533                                         PI-ERC-TYPE                  CL*50
00534                    PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT          CL*50
00535                    MOVE XCTL-635    TO PGM-NAME                      CL*50
00536                    GO TO 9300-XCTL                                   CL*44
00537                 ELSE                                                 CL*44
00538                    MOVE 'A'         TO  PI-CR-TYPE                   CL*44
00539                                         PI-ERC-TYPE                  CL*50
00540                    PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT          CL*50
00541                    MOVE XCTL-635    TO PGM-NAME                      CL*50
00542                    GO TO 9300-XCTL                                   CL*44
00543             ELSE                                                     CL*44
00544                 MOVE ER-3021        TO  EMI-ERROR                    CL*50
00545                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             CL*50
00546                 GO TO 8100-SEND-INITIAL-MAP                          CL*50
00547         ELSE                                                         CL*44
00548             IF PI-ERCOMP-KEY NOT = SPACES AND LOW-VALUES             CL*50
00549                 MOVE PI-ERC-CARRIER TO  PI-CR-CARRIER                CL*44
00550                 MOVE PI-ERC-GROUP   TO  PI-CR-GROUPING               CL*44
00551                 MOVE PI-ERC-RESP    TO  PI-CR-FIN-RESP               CL*44
00552                 MOVE PI-ERC-ACCT    TO  PI-CR-ACCOUNT                CL*44
033105                MOVE PI-ERC-TYPE    TO  PI-CR-TYPE
00553                 IF PI-CR-ACCOUNT =  LOW-VALUES                       CL*44
00554 *                  MOVE 'G'         TO  PI-CR-TYPE                   CL*44
00555 *                                       PI-ERC-TYPE                  CL*50
00556                    PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT          CL*50
00557                    IF PI-COMPANY-ID = 'DMD'                          CL*56
00558                       MOVE XCTL-633DMD TO PGM-NAME                   CL*56
00559                       GO TO 9300-XCTL                                CL*56
00560                    ELSE                                              CL*56
00561                       MOVE XCTL-633    TO PGM-NAME                   CL*56
00562                       GO TO 9300-XCTL                                CL*56
00563                 ELSE                                                 CL*44
00564                    MOVE 'A'         TO  PI-CR-TYPE                   CL*44
00565                                         PI-ERC-TYPE                  CL*50
00566                    PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT          CL*50
00567                    IF PI-COMPANY-ID = 'DMD'                          CL*56
00568                       MOVE XCTL-633DMD TO PGM-NAME                   CL*56
00569                       GO TO 9300-XCTL                                CL*56
00570                    ELSE                                              CL*56
00571                       MOVE XCTL-633    TO PGM-NAME                   CL*56
00572                       GO TO 9300-XCTL                                CL*56
00573             ELSE                                                     CL*44
00574                 MOVE ER-3021        TO  EMI-ERROR                    CL*50
00575                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             CL*50
00576                 GO TO 8100-SEND-INITIAL-MAP.                         CL*50
00577                                                                      CL*40
00578      IF EIBAID = DFHPF4                                              CL*40
033105         IF PI-ERC-TYPE = 'G' OR 'A' OR 'B'
00580              PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT                CL*46
00581              MOVE XCTL-6521      TO PGM-NAME                         CL*40
00582              GO TO 9300-XCTL                                         CL*40
00583          ELSE                                                        CL*50
00584              MOVE ER-2872        TO  EMI-ERROR                       CL*40
00585              GO TO 3100-INPUT-ERROR.                                 CL*50
00586                                                                      CL*46
00587      IF  EIBAID = DFHPF5                                             CL*46
00588          MOVE XCTL-689           TO PGM-NAME                         CL*46
00589          IF  PI-ERCOMP-KEY NOT = SPACES                              CL*46
00590              MOVE PI-ERC-CARRIER TO PI-CR-CARRIER                    CL*46
00591              MOVE PI-ERC-GROUP   TO PI-CR-GROUPING                   CL*46
00592              MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP                   CL*46
00593              MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
033105             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
00594                                                                      CL*46
00595              IF  PI-CR-ACCOUNT =  LOW-VALUES                         CL*46
00596 *                MOVE 'G'        TO PI-CR-TYPE, PI-ERC-TYPE          CL*50
00597                  PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT            CL*50
00598                  MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA             CL*47
00599                  GO TO 9300-XCTL                                     CL*46
00600                                                                      CL*46
00601              ELSE                                                    CL*46
00602                  MOVE 'A'        TO PI-CR-TYPE, PI-ERC-TYPE          CL*50
00603                  PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT            CL*50
00604                  MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA             CL*47
00605                  GO TO 9300-XCTL                                     CL*46
00606                                                                      CL*46
00607          ELSE                                                        CL*46
00608              PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT                CL*50
00609              MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA             CL*47
00610              GO TO 9300-XCTL.                                        CL*46
00611                                                                      CL*46
00612      IF  EIBAID = DFHPF6                                             CL*46
00613                                                                      CL*46
00614          MOVE XCTL-690           TO PGM-NAME                         CL*46
00615                                                                      CL*46
00616          IF  PI-ERCOMP-KEY NOT = SPACES                              CL*46
00617              MOVE PI-ERC-CARRIER TO PI-CR-CARRIER                    CL*46
00618              MOVE PI-ERC-GROUP   TO PI-CR-GROUPING                   CL*46
00619              MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP                   CL*46
00620              MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT
033105             MOVE PI-ERC-TYPE    TO PI-CR-TYPE
00621                                                                      CL*46
00622              IF  PI-CR-ACCOUNT =  LOW-VALUES                         CL*46
00623 *                MOVE 'G'        TO PI-CR-TYPE, PI-ERC-TYPE          CL*50
00624                  PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT            CL*50
00625                  MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA             CL*47
00626                  GO TO 9300-XCTL                                     CL*46
00627                                                                      CL*46
00628              ELSE                                                    CL*46
00629                  MOVE 'A'        TO PI-CR-TYPE, PI-ERC-TYPE          CL*50
00630                  PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT            CL*50
00631                  MOVE LOW-VALUES TO PI-PROGRAM-WORK-AREA             CL*47
00632                  GO TO 9300-XCTL                                     CL*46
00633                                                                      CL*46
00634          ELSE                                                        CL*46
00635              PERFORM 3400-CREATE-TS-PI THRU 3400-EXIT                CL*50
00636              MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA             CL*47
00637              GO TO 9300-XCTL.                                        CL*46
00638                                                                      CL*36
CIDMOD     IF EIBAID = DFHPF7                                              CL*40
CIDMOD         IF PI-ERC-TYPE = 'A'                                        CL*59
CIDMOD            MOVE PI-ERC-CARRIER  TO PI-CR-CARRIER                    CL*46
CIDMOD            MOVE PI-ERC-GROUP    TO PI-CR-GROUPING                   CL*46
CIDMOD            MOVE PI-ERC-ACCT     TO PI-CR-ACCOUNT                    CL*46
CIDMOD*           PERFORM 3200-FIND-STATE
CIDMOD*                                THRU 3200-EXIT
CIDMOD            MOVE PI-SAVE-STATE   TO PI-CR-STATE
CIDMOD            PERFORM 3400-CREATE-TS-PI                                CL*46
CIDMOD                                 THRU 3400-EXIT                      CL*46
CIDMOD            MOVE XCTL-650        TO PGM-NAME                         CL*40
CIDMOD            GO TO 9300-XCTL                                          CL*40
CIDMOD         ELSE                                                        CL*50
CIDMOD            MOVE ER-0035         TO EMI-ERROR                        CL*40
CIDMOD            GO TO 3100-INPUT-ERROR                                   CL*50
CIDMOD         END-IF
CIDMOD     END-IF.
00586                                                                      CL*46
102202     IF EIBAID = DFHPF8                                              CL*40
102202         IF PI-ERCOMP-KEY NOT = SPACES 
102202             MOVE PI-ERC-CARRIER TO PI-CR-CARRIER                    CL*46
102202             MOVE PI-ERC-GROUP   TO PI-CR-GROUPING                   CL*46
102202             MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP                   CL*46
102202             MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT                    CL*46
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
00639 *****************************************************************    CL*53
00640 *  SPECIAL CODE - ENABLES DISPLAY OF PRIOR MONTH-END TOTALS*         CL*53
00641 *  PF9 AVAILABLE TO A/R USERS                                   *    CL*53
00642 *****************************************************************    CL*53
00643                                                                   EL652
00644      IF PI-AR-PROCESSING OR PI-PROCESSOR-ID = 'LGXX'                 CL*53
00645          IF EIBAID = DFHPF9                                       EL652
00646              MOVE 'Y'            TO  WS-SHOW-SAVE-TOTALS          EL652
00647              GO TO 4000-EDIT-MAINT.                               EL652
00648                                                                   EL652
111103     IF EIBAID = DFHPF10
033105        IF PI-ERC-TYPE = 'B'
111103         IF PI-ERCOMP-KEY NOT = SPACES 
111103             MOVE PI-ERC-CARRIER TO PI-CR-CARRIER                    CL*46
111103             MOVE PI-ERC-GROUP   TO PI-CR-GROUPING                   CL*46
111103             MOVE PI-ERC-RESP    TO PI-CR-FIN-RESP                   CL*46
111103             MOVE PI-ERC-ACCT    TO PI-CR-ACCOUNT                    CL*46
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

00649      IF EIBAID = DFHENTER                                         EL652
00650          GO TO 4000-EDIT-MAINT.                                   EL652
00651                                                                   EL652
00652      MOVE ER-0029                TO  EMI-ERROR.                   EL652
00653                                                                   EL652
00654  3100-INPUT-ERROR.                                                EL652
00655      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL652
00656                                                                   EL652
00657      MOVE AL-UNBON               TO  PFKEYA.                      EL652
00658      MOVE -1                     TO  PFKEYL.                      EL652
00659                                                                   EL652
00660      GO TO 8200-SEND-DATAONLY.                                    EL652
00661  EJECT                                                               CL*36
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
00662  3400-CREATE-TS-PI.                                                  CL*46
00663                                                                      CL*46
00664      PERFORM 3450-DELETE-TEMP-STOR-PI THRU 3450-EXIT.                CL*46
00665                                                                      CL*36
00666      EXEC CICS HANDLE CONDITION                                      CL*37
00667          QIDERR  (3400-EXIT)                                         CL*46
00668      END-EXEC.                                                       CL*37
00669                                                                      CL*37
00670      EXEC CICS WRITEQ TS                                             CL*36
00671          QUEUE   (QID-PI)                                            CL*46
00672          FROM    (PROGRAM-INTERFACE-BLOCK)                           CL*46
00673          LENGTH  (PI-COMM-LENGTH)                                    CL*46
00674          ITEM    (QID-ITEM)                                          CL*44
00675      END-EXEC.                                                       CL*36
00676                                                                      CL*36
00677  3400-EXIT.                                                          CL*46
00678       EXIT.                                                          CL*36
00679                                                                      CL*36
00680  3450-DELETE-TEMP-STOR-PI.                                           CL*46
00681                                                                      CL*46
00682      EXEC CICS HANDLE CONDITION                                      CL*36
00683          QIDERR  (3450-EXIT)                                         CL*46
00684      END-EXEC.                                                       CL*36
00685                                                                      CL*36
00686      EXEC CICS DELETEQ TS                                            CL*36
00687          QUEUE  (QID-PI)                                             CL*46
00688      END-EXEC.                                                       CL*49
00689                                                                      CL*49
00690      EXEC CICS SYNCPOINT                                             CL*49
00691      END-EXEC.                                                       CL*36
00692                                                                      CL*36
00693  3450-EXIT.                                                          CL*46
00694       EXIT.                                                          CL*36
00695                                                                      CL*36
00696  3475-RECOVER-TEMP-STOR-PI.                                          CL*46
00697                                                                      CL*55
00698      MOVE LOW-VALUES            TO  EL652AI.                         CL*55
00699                                                                      CL*46
00700      EXEC CICS HANDLE CONDITION                                      CL*37
00701          NOTOPEN   (9990-ABEND)                                      CL*50
00702          NOTFND    (8880-NOT-FOUND)                                  CL*50
00703          PGMIDERR  (9600-PGMID-ERROR)                                CL*50
00704          ERROR     (9990-ABEND)                                      CL*50
00705      END-EXEC.                                                       CL*50
00706                                                                      CL*50
00707      EXEC CICS HANDLE CONDITION                                      CL*50
00708          QIDERR  (3475-EXIT)                                         CL*46
00709      END-EXEC.                                                       CL*37
00710                                                                      CL*37
00711      EXEC CICS READQ TS                                              CL*36
00712          QUEUE   (QID-PI)                                            CL*46
00713          ITEM    (1)                                                 CL*46
00714          INTO    (PROGRAM-INTERFACE-BLOCK)                           CL*46
00715          LENGTH  (PI-COMM-LENGTH)                                    CL*46
00716      END-EXEC.                                                       CL*36
00717                                                                      CL*36
00718      PERFORM 3450-DELETE-TEMP-STOR-PI THRU 3450-EXIT.                CL*46
00719                                                                      CL*36
00720      IF  PI-ERCOMP-KEY = SPACES OR LOW-VALUES                        CL*50
00721          GO TO 8100-SEND-INITIAL-MAP.                                CL*46
00722                                                                      CL*46
00723      IF  PI-ERC-CARRIER > LOW-VALUES                                 CL*46
00724          MOVE +1                 TO CARRIERL                         CL*46
00725          MOVE PI-ERC-CARRIER     TO CARRIERI.                        CL*46
00726                                                                      CL*46
00727      IF  PI-ERC-GROUP > LOW-VALUES                                   CL*46
00728          MOVE +6                 TO GROUPL                           CL*46
00729          MOVE PI-ERC-GROUP       TO GROUPI.                          CL*46
00730                                                                      CL*46
00731      IF  PI-ERC-RESP > LOW-VALUES                                    CL*46
00732          MOVE +10                TO FINRESPL                         CL*46
00733          MOVE PI-ERC-RESP        TO FINRESPI.                        CL*46
00734                                                                      CL*46
00735      IF  PI-ERC-ACCT > LOW-VALUES                                    CL*46
00736          MOVE +1                 TO ACCTNOL                          CL*46
00737          MOVE PI-ERC-ACCT        TO ACCTNOI.                         CL*46
00738                                                                      CL*46
00739      IF  PI-ERC-TYPE > LOW-VALUES                                    CL*50
00740          MOVE +1                 TO TYPEL                            CL*46
00741          MOVE PI-ERC-TYPE        TO TYPEI.                           CL*46
00742                                                                      CL*46
00743      MOVE 'S'                    TO MAINTYPI.                        CL*46
00744      MOVE 1                      TO MAINTYPL.                        CL*46
00745      GO TO 4000-EDIT-MAINT.                                          CL*46
00746                                                                      CL*46
00747  3475-EXIT.                                                          CL*46
00748       EXIT.                                                          CL*36
00749  EJECT                                                            EL652
00750  4000-EDIT-MAINT.                                                 EL652
00751      IF MAINTYPL > ZERO                                           EL652
00752          MOVE MAINTYPI           TO  PI-CHECK-MAINT-TYPE          EL652
00753          IF VALID-MAINT-TYPE                                      EL652
00754              MOVE AL-UANON       TO  MAINTYPA                     EL652
00755              IF MAINTYPI NOT = 'S'                                EL652
00756                  MOVE SPACES     TO  WS-ACCESS                    EL652
00757                  MOVE '1'        TO  CNTL-REC-TYPE                EL652
00758                  PERFORM 7400-READ-CONTROL-FILE  THRU  7499-EXIT  EL652
00759              ELSE                                                 EL652
00760                  NEXT SENTENCE                                    EL652
00761          ELSE                                                     EL652
00762              MOVE -1             TO  MAINTYPL                     EL652
00763              MOVE AL-UABON       TO  MAINTYPA                     EL652
00764              MOVE ER-2039        TO  EMI-ERROR                    EL652
00765              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL652
00766      ELSE                                                         EL652
00767          MOVE -1                 TO  MAINTYPL                     EL652
00768          MOVE AL-UABON           TO  MAINTYPA                     EL652
00769          MOVE ER-2039            TO  EMI-ERROR                    EL652
00770          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL652
00771                                                                   EL652
00772      IF  NOT MODIFY-CAP                                              CL*19
00773              AND                                                     CL*19
00774          NOT SHOW-FUNCTION                                           CL*19
00775          MOVE 'UPDATE'           TO SM-READ                          CL*19
00776          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT              CL*19
00777          IF  MORTGAGE-SESSION                                        CL*19
00778              MOVE ER-9096        TO  EMI-ERROR                       CL*19
00779              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*19
00780              GO TO 8100-SEND-INITIAL-MAP                             CL*19
00781          ELSE                                                     EL652
00782              MOVE ER-0070        TO  EMI-ERROR                    EL652
00783              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL652
00784              GO TO 8100-SEND-INITIAL-MAP.                         EL652
00785                                                                      CL**2
00786      IF CARRIERL > ZERO                                              CL**2
00787          IF PI-CARRIER-SECURITY > SPACES                             CL**2
00788              IF CARRIERI = PI-CARRIER-SECURITY                       CL**2
00789                  NEXT SENTENCE                                       CL**2
00790              ELSE                                                    CL**2
00791                  MOVE -1            TO  CARRIERL                     CL**2
00792                  MOVE ER-2370       TO  EMI-ERROR                    CL**2
00793                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**2
00794                  MOVE AL-UABON      TO  CARRIERA                     CL**2
00795                  GO TO 8200-SEND-DATAONLY.                           CL**2
00796                                                                   EL652
00797      IF CARRIERL > ZERO                                           EL652
00798          IF ADD-FUNCTION                                          EL652
00799              IF PI-ZERO-CARRIER                                   EL652
00800                OR PI-ZERO-CAR-GROUP                               EL652
00801                  MOVE ZEROS      TO  PI-ERC-CARRIER               EL652
00802                                      CARRIERI                     EL652
00803                  MOVE AL-UANON   TO  CARRIERA                     EL652
00804              ELSE                                                 EL652
00805                  MOVE CARRIERI   TO  WS-CARRIER                   EL652
00806                  MOVE '6'        TO  CNTL-REC-TYPE                EL652
00807                  PERFORM 7400-READ-CONTROL-FILE  THRU  7499-EXIT  EL652
00808          ELSE                                                     EL652
00809              IF PI-ZERO-CARRIER                                   EL652
00810                OR PI-ZERO-CAR-GROUP                               EL652
00811                  MOVE ZEROS      TO  PI-ERC-CARRIER               EL652
00812                                      CARRIERI                     EL652
00813                  MOVE AL-UANON   TO  CARRIERA                     EL652
00814              ELSE                                                 EL652
00815                  MOVE AL-UANON   TO  CARRIERA                     EL652
00816                  MOVE CARRIERI   TO  PI-ERC-CARRIER               EL652
00817      ELSE                                                         EL652
00818          IF ADD-FUNCTION                                          EL652
00819              IF PI-ZERO-CARRIER                                   EL652
00820                OR PI-ZERO-CAR-GROUP                               EL652
00821                  MOVE ZEROS      TO  PI-ERC-CARRIER               EL652
00822                                      CARRIERI                     EL652
00823                  MOVE AL-UANON   TO  CARRIERA                     EL652
00824              ELSE                                                 EL652
00825                  MOVE -1         TO  CARRIERL                     EL652
00826                  MOVE AL-UABON   TO  CARRIERA                     EL652
00827                  MOVE ER-0193    TO  EMI-ERROR                    EL652
00828                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL652
00829          ELSE                                                     EL652
00830              MOVE -1             TO  CARRIERL                     EL652
00831              MOVE AL-UABON       TO  CARRIERA                     EL652
00832              MOVE ER-0193        TO  EMI-ERROR                    EL652
00833              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL652
00834                                                                   EL652
00835      IF GROUPL > ZERO                                             EL652
00836          IF PI-ZERO-GROUPING                                      EL652
00837            OR PI-ZERO-CAR-GROUP                                   EL652
00838              MOVE ZEROS          TO  PI-ERC-GROUP                    CL**6
00839                                      GROUPI                       EL652
00840              MOVE AL-UANON       TO  GROUPA                       EL652
00841          ELSE                                                     EL652
00842              MOVE AL-UANON       TO  GROUPA                       EL652
00843              MOVE GROUPI         TO  PI-ERC-GROUP                    CL**6
00844      ELSE                                                         EL652
00845          IF ADD-FUNCTION                                          EL652
00846              IF PI-ZERO-GROUPING                                  EL652
00847                OR PI-ZERO-CAR-GROUP                               EL652
00848                  MOVE ZEROS      TO  PI-ERC-GROUP                    CL**6
00849                                      GROUPI                       EL652
00850                  MOVE AL-UANON   TO  GROUPA                       EL652
00851              ELSE                                                 EL652
00852                  MOVE LOW-VALUES  TO  PI-ERC-GROUP                   CL**6
00853          ELSE                                                     EL652
00854              MOVE LOW-VALUES     TO  PI-ERC-GROUP.                   CL**6
00855                                                                   EL652
00856      IF TYPEL > ZERO                                              EL652
00857          MOVE TYPEI              TO  PI-CHECK-TYPE                EL652
00858          IF VALID-TYPE                                            EL652
00859              MOVE AL-UANON       TO  TYPEA                        EL652
00860              MOVE TYPEI          TO  PI-ERC-TYPE                  EL652
00861          ELSE                                                     EL652
00862              MOVE -1             TO  TYPEL                        EL652
00863              MOVE AL-UABON       TO  TYPEA                        EL652
00864              MOVE ER-2042        TO  EMI-ERROR                    EL652
00865              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL652
00866      ELSE                                                         EL652
00867          MOVE -1                 TO  TYPEL                        EL652
00868          MOVE AL-UABON           TO  TYPEA                        EL652
00869          MOVE ER-2042            TO  EMI-ERROR                    EL652
00870          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL652
00871                                                                   EL652
00872      IF FINRESPL >        ZERO                                    EL652
00873          MOVE AL-UANON           TO  FINRESPA                     EL652
00874          IF FINRESPI = SPACES                                     EL652
00875              MOVE LOW-VALUES     TO  PI-ERC-RESP                  EL652
00876          ELSE                                                     EL652
00877              MOVE FINRESPI       TO  PI-ERC-RESP                  EL652
00878      ELSE                                                         EL652
00879          MOVE LOW-VALUES         TO  PI-ERC-RESP.                 EL652
00880                                                                   EL652
00881      IF ACCTNOL > ZERO                                            EL652
00882          MOVE AL-UANON           TO  ACCTNOA                      EL652
00883          IF ACCTNOI = SPACES                                      EL652
00884              MOVE LOW-VALUES     TO  PI-ERC-ACCT                  EL652
00885          ELSE                                                     EL652
00886              MOVE ACCTNOI        TO  PI-ERC-ACCT                  EL652
00887      ELSE                                                         EL652
00888          MOVE LOW-VALUES         TO  PI-ERC-ACCT.                 EL652
00889                                                                   EL652
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
00890      IF NOT MODIFY-CAP                                            EL652
00891          IF SHOW-FUNCTION                                         EL652
00892              GO TO 5000-BUILD-INITIAL-SCREEN                      EL652
00893          ELSE                                                     EL652
00894              MOVE 'UPDATE'       TO SM-READ                       EL652
00895              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT       EL652
00896              MOVE ER-0070        TO  EMI-ERROR                    EL652
00897              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL652
00898              GO TO 8100-SEND-INITIAL-MAP.                         EL652
00899                                                                   EL652
00900      IF EMI-NO-ERRORS                                             EL652
00901          NEXT SENTENCE                                            EL652
00902      ELSE                                                         EL652
00903         IF EIBTRNID NOT = TRANS-ID                                   CL*50
00904             GO TO 8100-SEND-INITIAL-MAP                              CL*50
00905          ELSE                                                        CL*50
00906             GO TO 8200-SEND-DATAONLY.                                CL*50
00907                                                                   EL652
00908      IF CHANGE-FUNCTION                                           EL652
00909          GO TO 4400-CHANGE.                                       EL652
00910                                                                   EL652
00911      IF DELETE-FUNCTION                                           EL652
00912          GO TO 4600-DELETE.                                       EL652
00913                                                                   EL652
00914      IF SHOW-FUNCTION                                             EL652
00915          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL652
00916                                                                   EL652
00917      IF TYPEI = 'C'                                               EL652
00918          IF FINRESPI NOT = LOW-VALUES OR                          EL652
00919             ACCTNOI  NOT = LOW-VALUES                             EL652
00920                MOVE -1           TO  FINRESPL                     EL652
00921                MOVE AL-UABON     TO  FINRESPA                     EL652
00922                                      ACCTNOA                      EL652
00923                MOVE ER-2088      TO  EMI-ERROR                    EL652
00924                PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.        EL652
00925                                                                   EL652
00926      IF TYPEI = 'A'                                               EL652
00927          IF FINRESPI = LOW-VALUES OR                              EL652
00928              ACCTNOI = LOW-VALUES                                 EL652
00929                MOVE -1           TO  FINRESPL                     EL652
00930                MOVE AL-UABON     TO  FINRESPA                     EL652
00931                                      ACCTNOA                      EL652
00932                MOVE ER-2089      TO  EMI-ERROR                    EL652
00933                PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.        EL652
00934                                                                   EL652
033105     IF TYPEI = 'G' OR 'B'
00936          IF ACCTNOI NOT = LOW-VALUES                              EL652
00937              MOVE -1             TO  ACCTNOL                      EL652
00938              MOVE AL-UABON       TO  ACCTNOA                      EL652
00939              MOVE ER-2091        TO  EMI-ERROR                    EL652
00940              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL652
00941          ELSE                                                     EL652
00942              IF FINRESPI = LOW-VALUES                             EL652
00943                  MOVE -1         TO  FINRESPL                     EL652
00944                  MOVE AL-UABON   TO  FINRESPA                     EL652
00945                  MOVE ER-2097    TO  EMI-ERROR                    EL652
00946                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.      EL652
00947                                                                      CL**6
033105     IF TYPEI = 'G' OR 'B'
00949          IF SUMMNOI NOT = LOW-VALUES                                 CL**6
00950              MOVE -1             TO  SUMMNOL                         CL**6
00951              MOVE AL-UABON       TO  SUMMNOA                         CL**6
00952              MOVE ER-3153        TO  EMI-ERROR                       CL**6
00953              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.             CL**6
00954                                                                      CL**6
033105     IF TYPEI = 'G' OR 'B'
00956          IF NETGRSI NOT = LOW-VALUES                                 CL**6
00957              MOVE -1             TO  NETGRSL                         CL**6
00958              MOVE AL-UABON       TO  NETGRSA                         CL**6
00959              MOVE ER-3154        TO  EMI-ERROR                       CL**6
00960              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.             CL**6
00961                                                                   EL652
00962      IF EMI-NO-ERRORS                                             EL652
00963          NEXT SENTENCE                                            EL652
00964      ELSE                                                         EL652
00965          GO TO 8200-SEND-DATAONLY.                                EL652
00966                                                                   EL652
00967      IF ADD-FUNCTION                                              EL652
00968          GO TO 4200-ADD.                                          EL652
00969                                                                   EL652
00970      MOVE -1                     TO  MAINTYPL.                    EL652
00971      MOVE ER-2056                TO  EMI-ERROR.                   EL652
00972      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL652
00973                                                                   EL652
00974      GO TO 8200-SEND-DATAONLY.                                    EL652
00975                                                                   EL652
00976  4000-EXIT.                                                       EL652
00977      EXIT.                                                        EL652
00978  EJECT                                                            EL652
00979  4200-ADD.                                                        EL652
00980      IF PI-ERCOMP-KEY = PI-SAVE-ERCOMP-KEY                        EL652
00981          MOVE ER-2057            TO  EMI-ERROR                    EL652
00982          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL652
00983          MOVE -1                 TO  MAINTYPL                     EL652
00984          GO TO 8200-SEND-DATAONLY.                                EL652
00985                                                                   EL652
00986      PERFORM 7000-EDIT  THRU  7000-EXIT.                          EL652
00987                                                                   EL652
00988      IF EMI-NO-ERRORS                                             EL652
00989          NEXT SENTENCE                                            EL652
00990      ELSE                                                         EL652
00991          GO TO 8200-SEND-DATAONLY.                                EL652
00992                                                                   EL652
00993      EXEC CICS HANDLE CONDITION                                   EL652
00994          NOTOPEN  (9990-ABEND)                                    EL652
00995          NOTFND   (4250-CONT)                                     EL652
00996      END-EXEC.                                                    EL652
00997                                                                   EL652
00998      PERFORM 7050-READ-ERCOMP  THRU  7050-EXIT.                   EL652
00999                                                                   EL652
01000      MOVE ER-2057                TO  EMI-ERROR.                   EL652
01001      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL652
01002                                                                      CL**5
01003      MOVE LOW-VALUES             TO  PI-SAVE-ERCOMP-KEY.             CL**5
01004                                                                   EL652
01005      MOVE -1                     TO  MAINTYPL.                    EL652
01006                                                                   EL652
01007      GO TO 8200-SEND-DATAONLY.                                    EL652
01008                                                                   EL652
01009  4250-CONT.                                                       EL652
01010      PERFORM 7150-ERCOMP-GETMAIN  THRU  7150-EXIT.                EL652
01011                                                                   EL652
01012      MOVE SPACES                 TO  COMPENSATION-MASTER.         EL652
01013      MOVE ZEROS                  TO  CO-LF-CLM-AMT                EL652
01014                                      CO-AH-CLM-AMT                EL652
01015                                      CO-CUR-FICA                  EL652
01016                                      CO-YTD-FICA                  EL652
01017                                      CO-CUR-OVR-UNDR              EL652
01018                                      CO-YTD-OVR-UNDR              EL652
092205                                     CO-MAX-BANK-FEE-LEASE
111103                                     CO-MAX-BANK-FEE
01019                                      CO-BAL-FWD                   EL652
01020                                      CO-CUR-COM                   EL652
01021                                      CO-CUR-CHG                   EL652
01022                                      CO-CUR-PMT                   EL652
01023                                      CO-END-BAL                   EL652
01024                                      CO-CUR                       EL652
01025                                      CO-OV30                      EL652
01026                                      CO-OV60                      EL652
01027                                      CO-OV90                      EL652
080612                                     co-ov120
01028                                      CO-YTD-COM                   EL652
01029                                      CO-YTD-OV                    EL652
01030                                      CO-YTD-PAID-COM                 CL**8
01031                                      CO-YTD-PAID-OV                  CL**8
01032                                      CO-CURRENT-BAL-FWD           EL652
01033                                      CO-CURRENT-CUR-COM           EL652
01034                                      CO-CURRENT-CUR-CHG           EL652
01035                                      CO-CURRENT-CUR-PMT           EL652
01036                                      CO-CURRENT-END-BAL           EL652
01037                                      CO-CURRENT-CUR               EL652
01038                                      CO-CURRENT-OV30              EL652
01039                                      CO-CURRENT-OV60              EL652
01040                                      CO-CURRENT-OV90              EL652
080612                                     co-current-ov120
01041                                      CO-CURRENT-YTD-COM           EL652
01042                                      CO-CURRENT-YTD-OV               CL*29
01043                                      CO-ACT-YEAR                     CL*29
01044                                      CO-ACT-MONTH                    CL*29
01045                                      CO-ACT-DAY.                     CL*50
01046      MOVE LOW-VALUES             TO  CO-LAST-ACTIVITY-DATE           CL*50
01047                                      CO-LAST-STMT-DT                 CL*50
01048                                      CO-CURRENT-LAST-STMT-DT         CL*50
01049                                      CO-GA-EFFECTIVE-DT              CL*50
01050                                      CO-GA-TERMINATION-DT.           CL*50
041106     MOVE LOW-VALUES             TO  CO-FIRST-WRITTEN-DT.
01051                                                                      CL*29
01052      MOVE 'N'                    TO  CO-INTERNAL-CONTROL-1        EL652
01053                                      CO-INTERNAL-CONTROL-2.       EL652

01055      PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.              EL652
01056                                                                   EL652
01057      MOVE PI-PROCESSOR-ID        TO  CO-LAST-MAINT-USER.          EL652
01058      MOVE EIBTIME                TO  CO-LAST-MAINT-HHMMSS.        EL652
01059      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL652
01060      MOVE '5'                    TO  DC-OPTION-CODE.              EL652
01061      MOVE LINK-ELDATCV           TO  PGM-NAME.                       CL*36
01062                                                                   EL652
01063      EXEC CICS LINK                                               EL652
01064          PROGRAM   (PGM-NAME)                                     EL652
01065          COMMAREA  (DATE-CONVERSION-DATA)                         EL652
01066          LENGTH    (DC-COMM-LENGTH)                               EL652
01067      END-EXEC.                                                    EL652
01068                                                                   EL652
01069      MOVE DC-BIN-DATE-1          TO  CO-LAST-MAINT-DT             EL652
01070                                      BIN-CURRENT-SAVE.            EL652
01071      MOVE PI-CR-MONTH-END-DT     TO  CO-ROLADEX-PRINT-DT.            CL**3
01072      MOVE PI-CR-MONTH-END-DT     TO  DC-BIN-DATE-1                EL652
01073      MOVE SPACE                  TO  DC-OPTION-CODE.              EL652
01074                                                                   EL652
01075      EXEC CICS LINK                                               EL652
01076          PROGRAM   (PGM-NAME)                                     EL652
01077          COMMAREA  (DATE-CONVERSION-DATA)                         EL652
01078          LENGTH    (DC-COMM-LENGTH)                               EL652
01079      END-EXEC.                                                    EL652
01080                                                                   EL652
01081      MOVE PI-COMPANY-CD          TO  CO-COMPANY-CD.               EL652
01082      MOVE 'CO'                   TO  CO-RECORD-ID.                EL652
01083      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL652
01084      MOVE COMP-FILE-ID           TO  FILE-ID.                     EL652
01085      MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.              EL652
01086                                                                   EL652
01087      PERFORM 4300-ADD-COMPENSATION-NAME  THRU  4300-EXIT.            CL*28
01088                                                                      CL*28
01089      EXEC CICS WRITE                                              EL652
01090          DATASET  (COMP-FILE-ID)                                  EL652
01091          FROM     (COMPENSATION-MASTER)                           EL652
01092          RIDFLD   (CO-CONTROL-PRIMARY)                            EL652
01093      END-EXEC.                                                    EL652
01094                                                                      CL*21
01095      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL652
01096                                                                   EL652
01097      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.             EL652
01098                                                                   EL652
01099      IF PI-AR-PROCESSING                                             CL**7
01100          IF SUMMNOL > ZERO                                           CL**7
01101              MOVE SUMMNOI        TO PI-AR-SUMMARY-CODE               CL**7
01102              PERFORM 7500-UPDATE-SUMM  THRU  7599-EXIT               CL*34
01103              MOVE PI-AR-SUMMARY-CODE                                 CL*34
01104                                  TO  WS-SUMM-FOR-RQST                CL*34
01105              PERFORM 6500-UPDATE-RQST  THRU  6599-EXIT.              CL*34
01106                                                                      CL**6
01107      MOVE ER-0000                TO  EMI-ERROR.                   EL652
01108      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL652
01109                                                                   EL652
01110      MOVE LOW-VALUES             TO  EL652AO.                     EL652
01111      MOVE PI-ERC-CARRIER         TO  CARRIERO.                    EL652
01112      MOVE PI-ERC-TYPE            TO  TYPEO.                       EL652
01113      MOVE AL-UANON               TO  CARRIERA                     EL652
01114                                      TYPEA.                       EL652
01115                                                                   EL652
01116      IF PI-ERC-GROUP NOT = SPACES                                    CL**6
01117          MOVE PI-ERC-GROUP       TO  GROUPO                          CL**6
01118          MOVE AL-UANON           TO  GROUPA.                      EL652
01119                                                                   EL652
01120      IF PI-ERC-RESP NOT = SPACES                                  EL652
01121          MOVE PI-ERC-RESP        TO  FINRESPO                     EL652
01122          MOVE AL-UANON           TO  FINRESPA.                    EL652
01123                                                                   EL652
01124      IF PI-ERC-ACCT NOT = SPACES                                  EL652
01125          MOVE PI-ERC-ACCT        TO  ACCTNOO                      EL652
01126          MOVE AL-UANON           TO  ACCTNOA.                     EL652
01127                                                                   EL652
01128      GO TO 5000-BUILD-INITIAL-SCREEN.                                CL**4
01129                                                                   EL652
01130  4200-EXIT.                                                       EL652
01131      EXIT.                                                           CL*21
01132  EJECT                                                               CL*21
01133  4300-ADD-COMPENSATION-NAME.                                         CL*21
01134      EXEC CICS HANDLE CONDITION                                      CL*21
01135           DUPREC   (4300-EXIT)                                       CL*21
01136      END-EXEC.                                                       CL*21
01137                                                                      CL*21
01138      EXEC CICS GETMAIN                                               CL*21
01139           LENGTH   (ERNAME-LENGTH)                                   CL*21
01140           SET      (ADDRESS OF NAME-LOOKUP-MASTER)                   CL*55
01141           INITIMG  (GETMAIN-SPACE)                                   CL*21
01142      END-EXEC.                                                       CL*21
01143                                                                      CL*21
01144      MOVE SPACES                 TO  NAME-LOOKUP-MASTER.             CL*21
01145      MOVE BIN-CURRENT-SAVE       TO  NL-LAST-MAINT-DT.               CL*21
01146      MOVE PI-PROCESSOR-ID        TO  NL-LAST-MAINT-USER.             CL*21
01147      MOVE EIBTIME                TO  NL-LAST-MAINT-HHMMSS.           CL*21
01148      MOVE PI-COMPANY-CD          TO  NL-COMPANY-CD.                  CL*21
01149      MOVE CO-ACCT-NAME           TO  NL-NAME.                        CL*21
012407     IF NL-NAME (1:4) = 'THE ' OR 'The ' OR 'the '
012407        MOVE NL-NAME (5:26)      TO NL-NAME
012407     END-IF
01150      MOVE CO-ADDR-CITY           TO  NL-CITY.                        CL*39
           MOVE CO-ADDR-STATE          TO  NL-ST
01151      MOVE 'C'                    TO  NL-RECORD-TYPE.                 CL*21
01152      MOVE PI-COMPANY-CD          TO  NL-CO-COMPANY-CD.               CL*21
01153      MOVE CO-CARRIER             TO  NL-CO-CARRIER.                  CL*21
01154      MOVE CO-GROUPING            TO  NL-CO-GROUPING.                 CL*21
01155      MOVE CO-RESP-NO             TO  NL-CO-RESP-NO.                  CL*21
01156      MOVE CO-ACCOUNT             TO  NL-CO-ACCOUNT.                  CL*21
01157      MOVE CO-TYPE                TO  NL-CO-TYPE.                     CL*21
01158                                                                      CL*21
01159      EXEC CICS WRITE                                                 CL*21
01160          FROM      (NAME-LOOKUP-MASTER)                              CL*21
01161          RIDFLD    (NL-CONTROL-PRIMARY)                              CL*21
01162          DATASET   (NAME-FILE-ID)                                    CL*21
01163      END-EXEC.                                                       CL*21
01164                                                                      CL*21
01165  4300-EXIT.                                                          CL*21
01166      EXIT.                                                        EL652
01167  EJECT                                                            EL652
01168  4400-CHANGE.                                                     EL652
01169      IF PI-ERCOMP-KEY = PI-SAVE-ERCOMP-KEY                        EL652
01170          NEXT SENTENCE                                            EL652
01171      ELSE                                                         EL652
111103**** MUST SHOW RECORD FIRST
01172          MOVE ER-2056            TO  EMI-ERROR                    EL652
01173          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL652
01174          MOVE -1                 TO  MAINTYPL                     EL652
01175          GO TO 8200-SEND-DATAONLY.                                EL652
01176                                                                   EL652
01177      PERFORM 7000-EDIT  THRU  7000-EXIT.                          EL652
01178                                                                   EL652
01179      IF EMI-NO-ERRORS                                             EL652
01180          NEXT SENTENCE                                            EL652
01181      ELSE                                                         EL652
01182          GO TO 8200-SEND-DATAONLY.                                EL652
01183                                                                   EL652
01184      PERFORM 7200-READ-ERCOMP-UPDATE  THRU  7200-EXIT.               CL**6
01185                                                                   EL652
01186      MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.                 CL**6
01187      MOVE CO-AR-SUMMARY-CODE     TO  WS-SAVE-SUMM.                   CL**6
01188                                                                   EL652
01189      PERFORM 6000-CHECK-FOR-UPDATE  THRU  6099-EXIT.              EL652
01190                                                                   EL652
01191      IF (CO-LAST-MAINT-USER   = PI-UPDATE-BY)
01192         OR (CO-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS)
01193         CONTINUE
01194      ELSE
01195         EXEC CICS UNLOCK
01196              DATASET  (COMP-FILE-ID)
01197         END-EXEC
01198         MOVE ER-0068             TO EMI-ERROR
01199         PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
01200         GO TO 8200-SEND-DATAONLY
           END-IF
01201                                                                   EL652
01202      MOVE PI-PROCESSOR-ID        TO  CO-LAST-MAINT-USER.          EL652
01203      MOVE EIBTIME                TO  CO-LAST-MAINT-HHMMSS.        EL652
01204      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL652
01205      MOVE '5'                    TO  DC-OPTION-CODE.              EL652
01206      MOVE LINK-ELDATCV           TO  PGM-NAME.                       CL*36
01207                                                                   EL652
01208      EXEC CICS LINK                                               EL652
01209          PROGRAM   (PGM-NAME)                                     EL652
01210          COMMAREA  (DATE-CONVERSION-DATA)                         EL652
01211          LENGTH    (DC-COMM-LENGTH)                               EL652
01212      END-EXEC.                                                    EL652
01213                                                                   EL652
01214      MOVE DC-BIN-DATE-1          TO  CO-LAST-MAINT-DT             EL652
01215                                      BIN-CURRENT-SAVE.            EL652
01216      MOVE SPACE                  TO  DC-OPTION-CODE.              EL652
01217      MOVE PI-CR-MONTH-END-DT     TO  CO-ROLADEX-PRINT-DT.            CL**3
01218      MOVE PI-CR-MONTH-END-DT     TO  DC-BIN-DATE-1                EL652
01219                                                                   EL652
01220      EXEC CICS LINK                                               EL652
01221          PROGRAM   (PGM-NAME)                                     EL652
01222          COMMAREA  (DATE-CONVERSION-DATA)                         EL652
01223          LENGTH    (DC-COMM-LENGTH)                               EL652
01224      END-EXEC.                                                    EL652
01225                                                                   EL652
01226      MOVE 'N'                    TO  CO-INTERNAL-CONTROL-1.       EL652
01227      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL652
01228      MOVE COMP-FILE-ID           TO  FILE-ID.                     EL652
01229                                                                   EL652
01230      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL652
01231                                                                   EL652
01232      MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.              EL652
01233                                                                   EL652
01234      PERFORM 4300-ADD-COMPENSATION-NAME  THRU  4300-EXIT.            CL*28
01235                                                                      CL*28
01236      EXEC CICS REWRITE                                            EL652
01237          DATASET  (COMP-FILE-ID)                                  EL652
01238          FROM     (COMPENSATION-MASTER)                           EL652
01239      END-EXEC.                                                    EL652
01240                                                                   EL652
01241      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL652
01242                                                                   EL652
01243      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL652
01244                                                                      CL**7
01245      IF NOT PI-AR-PROCESSING                                         CL**7
01246          GO TO 4400-CONT.                                            CL**7
01247                                                                   EL652
01248      IF SUMMNOL NOT > ZERO                                           CL**6
01249          GO TO 4400-CONT.                                            CL**6
01250                                                                      CL**6
01251      IF SUMMNOI  = WS-SAVE-SUMM                                      CL**6
01252          GO TO 4400-CONT                                             CL*34
01253      ELSE                                                            CL*34
01254          PERFORM 7600-DELETE-SUMM  THRU  7699-EXIT.                  CL**6
01255                                                                      CL*34
01256      MOVE SUMMNOI                 TO WS-SUMM-FOR-RQST.               CL*34
01257      PERFORM 6500-UPDATE-RQST  THRU  6599-EXIT.                      CL*34
01258                                                                      CL**6
01259      IF SUMMNOI = SPACES                                             CL**6
01260          GO TO 4400-CONT.                                            CL**6
01261                                                                      CL**6
01262      MOVE SUMMNOI                 TO PI-AR-SUMMARY-CODE.             CL**6
01263      PERFORM 7500-UPDATE-SUMM  THRU  7599-EXIT.                      CL**6
01264                                                                      CL**6
01265  4400-CONT.                                                          CL**6
01266      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT.             EL652
01267                                                                   EL652
01268      MOVE ER-0000                TO  EMI-ERROR.                   EL652
01269      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL652
01270                                                                   EL652
01271      MOVE LOW-VALUES             TO  EL652AO.                     EL652
01272      MOVE PI-ERC-CARRIER         TO  CARRIERO.                    EL652
01273      MOVE PI-ERC-TYPE            TO  TYPEO.                       EL652
01274      MOVE AL-UANON               TO  CARRIERA                     EL652
01275                                      TYPEA.                       EL652
01276                                                                   EL652
01277      IF PI-ERC-GROUP NOT = SPACES                                    CL**6
01278          MOVE PI-ERC-GROUP       TO  GROUPO                          CL**6
01279          MOVE AL-UANON           TO  GROUPA.                      EL652
01280                                                                   EL652
01281      IF PI-ERC-RESP NOT = SPACES                                  EL652
01282          MOVE PI-ERC-RESP        TO  FINRESPO                     EL652
01283          MOVE AL-UANON           TO  FINRESPA.                    EL652
01284                                                                   EL652
01285      IF PI-ERC-ACCT NOT = SPACES                                  EL652
01286          MOVE PI-ERC-ACCT        TO  ACCTNOO                      EL652
01287          MOVE AL-UANON           TO  ACCTNOA.                     EL652
01288                                                                   EL652
01289      GO TO 5000-BUILD-INITIAL-SCREEN.                                CL**4
01290                                                                   EL652
01291  4400-EXIT.                                                       EL652
01292      EXIT.                                                        EL652
01293  EJECT                                                            EL652
01294  4600-DELETE.                                                     EL652
01295      IF PI-ERCOMP-KEY = PI-SAVE-ERCOMP-KEY                        EL652
01296          NEXT SENTENCE                                            EL652
01297      ELSE                                                         EL652
01298          MOVE ER-2056            TO  EMI-ERROR                    EL652
01299          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL652
01300          MOVE -1                 TO  MAINTYPL                     EL652
01301          GO TO 8200-SEND-DATAONLY.                                EL652
01302                                                                   EL652
01303      PERFORM 7200-READ-ERCOMP-UPDATE  THRU  7200-EXIT.            EL652
01304                                                                      CL**6
01305      MOVE CO-AR-SUMMARY-CODE     TO  WS-SAVE-SUMM.                   CL*35
01306                                                                   EL652
01307      IF (CO-YTD-COM NOT = ZERO) OR                                   CL*27
01308         (CO-END-BAL NOT = ZERO)                                      CL*27
01309          EXEC CICS UNLOCK                                         EL652
01310              DATASET  (COMP-FILE-ID)                              EL652
01311          END-EXEC                                                 EL652
01312          MOVE ER-2092            TO  EMI-ERROR                    EL652
01313          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL652
01314          MOVE -1                 TO  MAINTYPL                     EL652
01315          GO TO 8200-SEND-DATAONLY.                                EL652
01316                                                                   EL652
01317      MOVE 'D'                    TO  JP-RECORD-TYPE.              EL652
01318      MOVE COMPENSATION-MASTER    TO  JP-RECORD-AREA.              EL652
01319      MOVE COMP-FILE-ID           TO  FILE-ID.                     EL652
01320                                                                   EL652
01321      IF CO-LAST-MAINT-USER   = PI-UPDATE-BY OR                    EL652
01322         CO-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL652
01323          continue                                                 EL652
01324      ELSE                                                         EL652
01325          EXEC CICS UNLOCK                                         EL652
01326              DATASET  (COMP-FILE-ID)                              EL652
01327          END-EXEC                                                 EL652
01328          MOVE ER-0068            TO  EMI-ERROR                    EL652
01329          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL652
01330          GO TO 8200-SEND-DATAONLY                                 EL652
01331      end-if                                                       EL652

pemtst     if pi-el652-del-sw = 'Y'
              continue
pemtst     else
pemtst        move 'Y'                 to pi-el652-del-sw
pemtst        move er-1299             to emi-error
pemtst        perform 9900-error-format thru 9900-exit
pemtst        move spaces              to maintypo
pemtst        MOVE -1                  TO MAINTypL
pemtst        GO TO 8200-send-dataonly
01337      end-if                                                       EL652

01332      EXEC CICS DELETE
01333          DATASET  (COMP-FILE-ID)
01334      END-EXEC
01335
01336      PERFORM 8400-LOG-JOURNAL-RECORD

01338      MOVE EIBDATE                TO  DC-JULIAN-YYDDD
01339      MOVE '5'                    TO  DC-OPTION-CODE
01340      MOVE LINK-ELDATCV           TO  PGM-NAME
01341
01342      EXEC CICS LINK
01343          PROGRAM   (PGM-NAME)                                     EL652
01344          COMMAREA  (DATE-CONVERSION-DATA)                         EL652
01345          LENGTH    (DC-COMM-LENGTH)                               EL652
01346      END-EXEC                                                     EL652
01347                                                                   EL652
01348      MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE             EL652
01349                                                                   EL652
01350      PERFORM 8000-UPDATE-MAINT-DATE  THRU  8000-EXIT              EL652
01351                                                                   EL652
01352      MOVE ER-0000                TO  EMI-ERROR                    EL652
01353      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   EL652
01354                                                                   EL652
01355      IF PI-AR-PROCESSING                                             CL**7
01356          IF WS-SAVE-SUMM NOT = SPACES                                CL**7
01357              PERFORM 7600-DELETE-SUMM  THRU  7699-EXIT               CL*34
01358              MOVE SPACES         TO  WS-SUMM-FOR-RQST                CL*34
01359              PERFORM  6500-UPDATE-RQST  THRU  6599-EXIT              CL*34
               end-if
           end-if
01360                                                                      CL**6
01361      MOVE LOW-VALUES             TO  EL652AO                      EL652
01362      MOVE PI-ERC-CARRIER         TO  CARRIERO                     EL652
01363      MOVE PI-ERC-TYPE            TO  TYPEO                        EL652
01364      MOVE AL-UANON               TO  CARRIERA                     EL652
01365                                      TYPEA                        EL652
01366                                                                   EL652
01367      IF PI-ERC-GROUP NOT = SPACES                                    CL**6
01368          MOVE PI-ERC-GROUP       TO  GROUPO                          CL**6
01369          MOVE AL-UANON           TO  GROUPA                       EL652
           end-if
01370                                                                   EL652
01371      IF PI-ERC-RESP NOT = SPACES                                  EL652
01372          MOVE PI-ERC-RESP        TO  FINRESPO                     EL652
01373          MOVE AL-UANON           TO  FINRESPA                     EL652
           end-if
01374                                                                   EL652
01375      IF PI-ERC-ACCT NOT = SPACES                                  EL652
01376          MOVE PI-ERC-ACCT        TO  ACCTNOO                      EL652
01377          MOVE AL-UANON           TO  ACCTNOA                      EL652
           end-if
01378                                                                   EL652
01379      MOVE LOW-VALUES             TO  PI-SAVE-ERCOMP-KEY           EL652
01380                                                                   EL652
01381      GO TO 8100-SEND-INITIAL-MAP                                  EL652
01382      .                                                            EL652
01383  4600-EXIT.                                                       EL652
01384      EXIT.                                                        EL652
01385  EJECT                                                            EL652
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
111103     EXEC CICS  HANDLE CONDITION
111103         NOTFND  (4750-NO-STATE)
111103     END-EXEC.
111103
111103     EXEC CICS  READ
111103         DATASET  (CNTL-FILE-ID)
111103         SET      (ADDRESS OF CONTROL-FILE)
111103         RIDFLD   (ELCNTL-KEY2)
111103     END-EXEC.
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
01386  5000-BUILD-INITIAL-SCREEN.                                       EL652
01387      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.              CL*50
01388                                                                   EL652
01389      PERFORM 7050-READ-ERCOMP  THRU  7050-EXIT.                   EL652
01390                                                                   EL652
01391      MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.             CL**4
01392      MOVE CO-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL652
01393                                                                      CL*35
01394      IF CO-LAST-MAINT-HHMMSS NUMERIC                                 CL*25
01395         MOVE CO-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS             CL*25
01396         ELSE                                                         CL*25
01397         MOVE ZEROS                  TO  PI-UPDATE-HHMMSS.            CL*25
01398                                                                   EL652
01399  5050-SET-UP-SCREEN.                                              EL652
CIDMOD     MOVE CO-CONTROL-NAME        TO  PCONTO.
01400      MOVE CO-MAIL-NAME           TO  MAILNAMO.                    EL652
01401      MOVE CO-ACCT-NAME           TO  ACCTNAMO.                    EL652
102202     MOVE CO-ACCT-NAME           TO  PI-SAVE-ACCT-NAME. 
01402      MOVE CO-ADDR-1              TO  ADDR1O.                      EL652
01403      MOVE CO-ADDR-2              TO  ADDR2O                       EL652
CIDMOD                                     PI-SAVE-ADDR2
01404      MOVE CO-ADDR-CITY           TO  CITYO
CIDMOD                                     PI-SAVE-CITY
01404      MOVE CO-ADDR-STATE          TO  STATEO
CIDMOD                                     PI-SAVE-STATE
01405      MOVE SPACES                 TO  WS-ZIP-CODE.                    CL*28
01406                                                                      CL*35
01407      IF CO-CANADIAN-POST-CODE                                        CL*28
01408          MOVE CO-CAN-POSTAL-1    TO  WS-ZIP-CAN-2-POST1              CL*28
01409          MOVE CO-CAN-POSTAL-2    TO  WS-ZIP-CAN-2-POST2              CL*28
01410      ELSE                                                            CL*28
01411          MOVE CO-ZIP-PRIME       TO  WS-ZIP-AM-2-CODE                CL*28
01412          IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS                    CL*28
01413              MOVE '-'            TO  WS-ZIP-AM-2-DASH                CL*28
01414              MOVE CO-ZIP-PLUS4   TO  WS-ZIP-AM-2-PLUS4.              CL*28
01414
01415      MOVE WS-ZIP-CODE            TO  ZIPCODEO.                       CL*28
CIDMOD
CIDMOD     IF CO-BILL-SW = ' ' OR 'B' OR 'R' OR 'T' OR 'S'
060506        OR 'O' OR 'E' OR 'C'
CIDMOD         MOVE AL-UANON           TO  BILLPRTA                          000
CIDMOD         MOVE CO-BILL-SW         TO  BILLPRTO                          000
CIDMOD     END-IF.
CIDMOD
LGC186     MOVE CO-CSO-1099            TO  PNT1099O.
LGC186
01417      MOVE CO-CSR-CODE            TO  CSRO.                           CL*31
01418                                                                      CL*41
020816     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
011410        AND (CO-TYPE = 'A')
011410        IF CO-COMP-TYPE = '1'
011410           MOVE 'Y'              TO SPPDDO
011410        ELSE
011410           MOVE 'N'              TO SPPDDO
011410        END-IF
011410     END-IF
01428      MOVE CO-SOC-SEC             TO  SSNO.                           CL*43
01429                                                                      CL*41
01430      MOVE CO-BALANCE-CONTROL     TO  CARBALO.                     EL652
01431                                                                   EL652
01432      IF PI-AR-PROCESSING                                             CL**6
01433          IF CO-AR-SUMMARY-CODE > SPACES                              CL**6
01434              MOVE AL-UANON       TO  SUMMNOA                         CL**6
01435              MOVE CO-AR-SUMMARY-CODE                                 CL**6
01436                                  TO  SUMMNOO                         CL**6
01437          ELSE                                                        CL**6
01438              MOVE LOW-VALUES     TO  SUMMNOO.                        CL**6
01439                                                                      CL**6
01440      IF PI-AR-PROCESSING                                             CL**6
01441         IF NOT CO-ACCOUNT-TYPE                                       CL**9
01442            MOVE AL-SADOF         TO SCDESCA                          CL**9
01443            MOVE AL-SANOF         TO SUMMNOA                          CL**9
01444         ELSE                                                         CL**9
01445            MOVE AL-UANOF         TO SUMMNOA.                         CL**9
01446                                                                      CL**9
01447      IF PI-AR-PROCESSING                                             CL**9
071712*        MOVE CO-AR-BAL-LEVEL    TO  ARBALO                          CL**6
01449          MOVE CO-AR-PULL-CHECK   TO  CKPULLO.                        CL*12
01450                                                                      CL*27
01451      IF PI-AR-PROCESSING                                             CL*27
01452          IF CO-ACCOUNT-TYPE                                          CL*28
01453              MOVE CO-AR-REPORTING                                    CL*27
01454                                  TO  NETGRSO                         CL*27
01455          ELSE                                                        CL*27
01456              MOVE SPACES         TO  NETGRSO.                        CL*27
01457                                                                      CL**6
01461      MOVE SPACES                 TO  FLITYPO.                     EL652
01462                                                                      CL*41
01469      IF CO-COMPANY-TYPE  
01470          MOVE SPACES             TO  RPTCD2O  
01471          MOVE AL-SADOF           TO  RPTCDDA.
01472                                                                   EL652
01482      MOVE SPACES                 TO  LETRCDO.
01483      MOVE AL-SADOF               TO  LETDESCA.
01494      MOVE SPACES                 TO  BALCDO. 
01495      MOVE AL-SADOF               TO  BALPRTA.                        CL*52
111103     MOVE CO-CLP-STATE           TO  CLPSTO.
111103     MOVE CO-MAX-BANK-FEE        TO  MAXFEEO
072406     MOVE CO-SPP-REFUND-EDIT     TO  REFEO
092205     IF CO-MAX-BANK-FEE-LEASE NOT NUMERIC
092205        MOVE +0                  TO CO-MAX-BANK-FEE-LEASE
092205     END-IF
092205     MOVE CO-MAX-BANK-FEE-LEASE  TO  MAXLFO
           .
01497  5060-BUILD-TOTALS.                                                  CL*53
01498      IF SHOW-SAVE-TOTALS                                             CL*53
01499          NEXT SENTENCE                                               CL*53
01500      ELSE                                                            CL*53
01501          GO TO 5070-BUILD-CURRENT-TOTALS.                            CL*53
01502                                                                      CL*53
01503      IF CO-LAST-STMT-DT = SPACES                                     CL*53
01504          MOVE SPACES             TO  LSTMDTO                      EL652
01505          GO TO 5060-LST-STMT-OK.                                     CL*53
01506                                                                   EL652
01507      MOVE CO-LAST-STMT-MONTH     TO  WS-YMD-MM.                      CL*53
01508      MOVE CO-LAST-STMT-DAY       TO  WS-YMD-DD.                      CL*53
01509      MOVE CO-LAST-STMT-YEAR      TO  WS-YMD-YY.                      CL*53
01510                                                                   EL652
01511      IF WS-YMD-DATE-NUM NUMERIC                                   EL652
01512          NEXT SENTENCE                                            EL652
01513      ELSE                                                         EL652
01514          MOVE SPACES             TO  LSTMDTO                      EL652
01515          GO TO 5060-LST-STMT-OK.                                     CL*53
01516                                                                   EL652
01517      MOVE WS-YMD-DATE            TO  DC-GREG-DATE-1-YMD.          EL652
01518      MOVE '3'                    TO  DC-OPTION-CODE.              EL652
01519                                                                   EL652
01520      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.             EL652
01521                                                                   EL652
01522      IF NO-CONVERSION-ERROR                                       EL652
01523          NEXT SENTENCE                                            EL652
01524      ELSE                                                         EL652
01525          MOVE SPACES             TO  LSTMDTO                      EL652
01526          GO TO 5060-LST-STMT-OK.                                     CL*53
01527                                                                   EL652
01528      MOVE CO-LAST-STMT-MONTH     TO  WS-DMDY8-MM.                    CL*53
01529      MOVE CO-LAST-STMT-DAY       TO  WS-DMDY8-DD.                    CL*53
01530      MOVE CO-LAST-STMT-YEAR      TO  WS-DMDY8-YY.                    CL*53
01531      MOVE '/'                    TO  WS-DMDY8-SL1                    CL*53
01532                                      WS-DMDY8-SL2.                   CL*53
01533      MOVE WS-DATE-MDY-8          TO  LSTMDTO.                        CL*53
01534                                                                   EL652
01535  5060-LST-STMT-OK.                                                   CL*53
01536      MOVE CO-BAL-FWD             TO  BALFWDO.                     EL652
01537      MOVE CO-CUR-COM             TO  CURCOMO.                     EL652
01538      MOVE CO-CUR-PMT             TO  CURPMTO.                     EL652
01539      MOVE CO-CUR-CHG             TO  CURCHGO.                     EL652
01540      MOVE CO-END-BAL             TO  ENDBALO.                     EL652
01541      MOVE CO-CUR                 TO  CURRENTO.                    EL652
01542      MOVE CO-OV30                TO  OVER30O.                     EL652
01543      MOVE CO-OV60                TO  OVER60O.                     EL652
01544      MOVE CO-OV90                TO  OVER90O.                     EL652

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
01550                                                                   EL652
01551      MOVE AL-SADOF               TO  PFK9A.                       EL652
01552      MOVE 'PREV. END-OF-MONTH TOTALS DISPLAYED'                   EL652
01553                                  TO  EMI-MESSAGE-AREA (1).           CL*40
01554                                                                   EL652
01555      GO TO 5090-CONTD.                                            EL652
01556                                                                   EL652
01557  5070-BUILD-CURRENT-TOTALS.                                       EL652
01558      IF CO-CURRENT-LAST-STMT-DT = SPACES                             CL*53
01559          MOVE SPACES             TO  LSTMDTO                         CL*53
01560          GO TO 5070-LST-STMT-OK.                                     CL*53
01561                                                                      CL*53
01562      MOVE CO-CURRENT-LAST-STMT-MONTH                                 CL*53
01563                                  TO WS-YMD-MM.                       CL*53
01564      MOVE CO-CURRENT-LAST-STMT-DAY                                   CL*53
01565                                  TO  WS-YMD-DD.                      CL*53
01566      MOVE CO-CURRENT-LAST-STMT-YEAR                                  CL*53
01567                                  TO  WS-YMD-YY.                      CL*53
01568                                                                      CL*53
01569      IF WS-YMD-DATE-NUM NUMERIC                                      CL*53
01570          NEXT SENTENCE                                               CL*53
01571      ELSE                                                            CL*53
01572          MOVE SPACES             TO  LSTMDTO                         CL*53
01573          GO TO 5070-LST-STMT-OK.                                     CL*53
01574                                                                      CL*53
01575      MOVE WS-YMD-DATE            TO  DC-GREG-DATE-1-YMD.             CL*53
01576      MOVE '3'                    TO  DC-OPTION-CODE.                 CL*53
01577                                                                      CL*53
01578      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.                CL*53
01579                                                                      CL*53
01580      IF NO-CONVERSION-ERROR                                          CL*53
01581          NEXT SENTENCE                                               CL*53
01582      ELSE                                                            CL*53
01583          MOVE SPACES             TO  LSTMDTO                         CL*53
01584          GO TO 5070-LST-STMT-OK.                                     CL*53
01585                                                                      CL*53
01586      MOVE CO-CURRENT-LAST-STMT-MONTH                                 CL*53
01587                                  TO  WS-DMDY8-MM.                    CL*53
01588      MOVE CO-CURRENT-LAST-STMT-DAY                                   CL*53
01589                                  TO  WS-DMDY8-DD.                    CL*53
01590      MOVE CO-CURRENT-LAST-STMT-YEAR                                  CL*53
01591                                  TO  WS-DMDY8-YY.                    CL*53
01592      MOVE '/'                    TO  WS-DMDY8-SL1                    CL*53
01593                                      WS-DMDY8-SL2.                   CL*53
01594      MOVE WS-DATE-MDY-8          TO  LSTMDTO.                        CL*53
01595                                                                      CL*53
01596  5070-LST-STMT-OK.                                                   CL*53
01597      MOVE CO-CURRENT-BAL-FWD     TO  BALFWDO.                     EL652
01598      MOVE CO-CURRENT-CUR-COM     TO  CURCOMO.                     EL652
01599      MOVE CO-CURRENT-CUR-PMT     TO  CURPMTO.                     EL652
01600      MOVE CO-CURRENT-CUR-CHG     TO  CURCHGO.                     EL652
01601      MOVE CO-CURRENT-END-BAL     TO  ENDBALO                      EL652
01602                                      PI-ERC-END-BAL.              EL652
01603      MOVE CO-CURRENT-CUR         TO  CURRENTO.                    EL652
01604      MOVE CO-CURRENT-OV30        TO  OVER30O.                     EL652
01605      MOVE CO-CURRENT-OV60        TO  OVER60O.                     EL652
01606      MOVE CO-CURRENT-OV90        TO  OVER90O.                     EL652

080612     if pi-company-id = 'AHL'
080612        if co-current-ov120 not numeric
080612           move zeros            to co-current-ov120
080612        end-if
080612        move co-current-ov120    to over120o
080612     end-if

01608      IF CO-ACCOUNT-TYPE
01609         MOVE CO-CURRENT-YTD-COM  TO YTDCOMO
01610      ELSE                                                         EL652
01611         MOVE CO-CURRENT-YTD-OV   TO YTDCOMO
           END-IF

           .
01613  5090-CONTD.                                                      EL652
01614                                                                   EL652
01615      MOVE CO-LAST-MAINT-USER     TO LSTUSRO.                      EL652
01616                                                                   EL652
01617      MOVE ' '              TO DC-OPTION-CODE.                     EL652
01618      MOVE CO-LAST-MAINT-DT TO DC-BIN-DATE-1.                      EL652
01619                                                                   EL652
01620      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.             EL652
01621                                                                   EL652
01622      MOVE DC-GREG-DATE-1-EDIT    TO LSTDTEO.                      EL652
01623                                                                      CL*25
01624      IF CO-LAST-MAINT-HHMMSS NUMERIC                                 CL*25
01625          MOVE CO-LAST-MAINT-HHMMSS                                   CL*25
01626                                  TO TIME-IN                          CL*25
01627          MOVE TIME-OUT           TO LSTTIMEO                         CL*25
01628      ELSE                                                            CL*25
01629          MOVE ZEROS              TO TIME-IN                          CL*25
01630          MOVE TIME-OUT           TO LSTTIMEO.                        CL*25
01631                                                                   EL652
01632      IF PI-COMPANY-ID =  'FLI'  OR  'FLU'  OR  'UCL'                 CL*51
01633          MOVE AL-SANOF           TO  FLITYPEA                     EL652
01634          MOVE AL-UANON           TO  FLITYPA                      EL652
01635      ELSE                                                         EL652
01636          MOVE AL-SANON           TO  FLITYPA.                     EL652
01637                                                                      CL*41
01638      IF PI-COMPANY-ID = 'NCL'                                        CL*53
01639          IF CO-ACCOUNT-TYPE                                          CL*41
01640              MOVE AL-SANOF       TO  RPTCD2A                         CL*41
01641          ELSE                                                        CL*41
01642              IF CO-COMPANY-TYPE                                      CL*43
01643                 MOVE AL-SANOF    TO  RPTCD2A                         CL*43
01644                 MOVE AL-SADOF    TO  RPTCDDA.                        CL*43
01645                                                                      CL*42
01646      IF PI-COMPANY-ID = 'NCL'                                        CL*53
01647          IF CO-GEN-AGENT-TYPE                                        CL*43
01648             MOVE AL-UANON       TO  RPTCD2A.                         CL*43
01649                                                                      CL*43
01650      IF PI-COMPANY-ID = 'NCL'                                        CL*53
01651          MOVE AL-UANON           TO  LETRCDA                         CL*42
01652      ELSE                                                            CL*42
01653          MOVE AL-SANOF           TO  LETRCDA                         CL*42
01654          MOVE AL-SADOF           TO  LETDESCA.                       CL*42
01655                                                                      CL**6
01656      IF PI-COMPANY-ID  NOT =  'DDB' AND 'ANT' AND 'ASL' AND          CL*58
01657                               'AN1' AND 'TFS'                        CL*58
01658          MOVE AL-SANOF           TO  BALCDA                          CL*52
01659          MOVE AL-SADOF           TO  BALPRTA.                        CL*52
01660                                                                      CL*52
01661      IF PI-AR-PROCESSING                                             CL**6
01662          IF CO-ACCOUNT-TYPE                                          CL**6
01663              MOVE AL-SANOF       TO  SCDESCA                         CL**6
01664              MOVE AL-SANOF       TO  NGDESCA                         CL**6
01665              MOVE AL-UANON       TO  NETGRSA                         CL*52
01666              IF PI-COMPANY-ID  =  'DDB' OR 'ANT' OR 'ASL' OR         CL*55
01667                                   'AN1' OR 'TFS'                     CL*55
01668                  MOVE AL-SANOF   TO  BALPRTA                         CL*52
01669                  MOVE AL-UANON   TO  BALCDA.                         CL*52
01670                                                                      CL**6
01671      IF PI-AR-PROCESSING                                             CL**6
071712*            MOVE AL-SANOF       TO  BALDESCA                        CL**6
071712             move al-sanof       to  CKDESCA                         CL*12
071712*            MOVE AL-UANON       TO  ARBALA                          CL**6
071712             move al-uanon       to  CKPULLA.                        CL*12
01676                                                                      CL**6
01677                                                                   EL652
01678      MOVE AL-UANON               TO  MAILNAMA                     EL652
01679                                      ACCTNAMA                     EL652
01680                                      ADDR1A                       EL652
01681                                      ADDR2A                       EL652
01682                                      CITYA
                                           STATEA
01683                                      SSNA                         EL652
01684                                      ZIPCODEA                     EL652
CIDMOD                                     BILLPRTA                          000
LGC186                                     PNT1099A                          000
01685                                      CARBALA.
01686                                                                   EL652
01687      IF NOT SHOW-SAVE-TOTALS                                      EL652
01688          IF PI-PROCESSOR-ID = 'LGXX'                              EL652
01689              GO TO 5100-CONTD.                                    EL652
01690                                                                   EL652
01691      MOVE AL-SANOF               TO  BALFWDA                      EL652
01692                                      CURCOMA                      EL652
01693                                      CURPMTA                      EL652
01694                                      CURCHGA                      EL652
01695                                      ENDBALA.                        CL*23
01696                                                                   EL652
01697  5100-CONTD.                                                      EL652
01698      IF CO-TELEPHONE NUMERIC                                      EL652
01699          MOVE CO-TELEPHONE       TO  WS-PHONE-IN                  EL652
01700                                      PI-SAVE-PHONE                EL652
01701          MOVE WSPI-AREA          TO  WSPO-AREA                    EL652
01702          MOVE WSPI-PFX           TO  WSPO-PFX                     EL652
01703          MOVE WSPI-SFX           TO  WSPO-SFX                     EL652
01704          MOVE WS-PHONE-OUT       TO  PHONEO                       EL652
01705          MOVE AL-UNNON           TO  PHONEA                       EL652
01706      ELSE                                                         EL652
01707          MOVE SPACES             TO  PHONEO.                         CL*48
01708                                                                      CL*48
01709      IF CO-FAXNO NUMERIC                                             CL*48
01710          MOVE CO-FAXNO           TO  WS-PHONE-IN                     CL*48
01711                                      PI-SAVE-FAXNO                   CL*48
01712          MOVE WSPI-AREA          TO  WSPO-AREA                       CL*48
01713          MOVE WSPI-PFX           TO  WSPO-PFX                        CL*48
01714          MOVE WSPI-SFX           TO  WSPO-SFX                        CL*48
01715          MOVE WS-PHONE-OUT       TO  FAXNOO                          CL*48
01716          MOVE AL-UNNON           TO  FAXNOA                          CL*48
01717      ELSE                                                            CL*48
01718          MOVE SPACES             TO  FAXNOO.                         CL*48
01719                                                                   EL652
01720      MOVE 'S'                    TO  MAINTYPO                     EL652
01721                                      PI-CHECK-MAINT-TYPE.         EL652
01722      MOVE AL-UANON               TO  MAINTYPA.                       CL**4
01723      MOVE -1                     TO  MAINTYPL.                    EL652
01724                                                                   EL652
01725      GO TO 8100-SEND-INITIAL-MAP.                                 EL652
01726  EJECT                                                            EL652
01727  6000-CHECK-FOR-UPDATE.                                           EL652
01728      IF CHANGE-FUNCTION                                           EL652
01729          GO TO 6010-CONT.                                         EL652
01730                                                                   EL652
01731      IF CARRIERL > ZERO                                           EL652
01732          MOVE CARRIERI           TO  CO-CARRIER.                  EL652
01733                                                                   EL652
01734      IF GROUPL > ZERO                                             EL652
01735          MOVE GROUPI             TO  CO-GROUPING                  EL652
01736      ELSE                                                         EL652
01737          MOVE LOW-VALUES         TO  CO-GROUPING.                 EL652
01738                                                                   EL652
01739      IF FINRESPL > ZERO                                           EL652
01740          MOVE FINRESPI           TO  CO-RESP-NO                   EL652
01741      ELSE                                                         EL652
01742          MOVE LOW-VALUES         TO  CO-RESP-NO.                  EL652
01743                                                                   EL652
01744      IF ACCTNOL > ZERO                                            EL652
01745          MOVE ACCTNOI            TO  CO-ACCOUNT                   EL652
01746      ELSE                                                         EL652
01747          MOVE LOW-VALUES         TO  CO-ACCOUNT.                  EL652
01748                                                                   EL652
01749      IF TYPEL > ZERO                                              EL652
01750          MOVE TYPEI              TO  CO-TYPE.                     EL652
01751                                                                   EL652
01752  6010-CONT.                                                       EL652
01753      IF PI-PROCESSOR-ID = 'LGXX'                                  EL652
01754          IF LSTMDTL > ZERO                                        EL652
01755              MOVE WS-YMD-YY      TO  CO-CURRENT-LAST-STMT-YEAR    EL652
01756                                      WS-DMDY8-YY                  EL652
01757              MOVE WS-YMD-MM      TO  CO-CURRENT-LAST-STMT-MONTH   EL652
01758                                      WS-DMDY8-MM                  EL652
01759              MOVE WS-YMD-DD      TO  CO-CURRENT-LAST-STMT-DAY     EL652
01760                                      WS-DMDY8-DD                  EL652
01761              MOVE '/'            TO  WS-DMDY8-SL1  WS-DMDY8-SL2   EL652
01762              MOVE WS-DATE-MDY-8  TO  LSTMDTO.                     EL652
01763                                                                   EL652
01764      IF PI-AR-PROCESSING                                             CL**6
01765          IF TYPEI = 'A'                                              CL**6
01766              IF SUMMNOL > ZERO                                       CL**6
01767                  MOVE SUMMNOI   TO  CO-AR-SUMMARY-CODE               CL**6
01768                                     WS-SAVE-SUMMARY.
01778                                                                      CL**6
01779      IF PI-AR-PROCESSING                                             CL**6
01780          IF TYPEI = 'A'                                              CL**6
01781              IF NETGRSL > ZERO                                       CL*10
01782                  MOVE NETGRSI   TO  CO-AR-REPORTING                  CL*10
01783              ELSE                                                    CL*10
01784                  IF ADD-FUNCTION                                     CL*18
01788                      MOVE 'G'   TO  CO-AR-REPORTING.                 CL*11
01790                                                                      CL**6
01791      IF PI-AR-PROCESSING                                             CL**6
071712*        IF ARBALL > ZERO                                            CL**6
071712*            MOVE ARBALI        TO  CO-AR-BAL-LEVEL                  CL*10
071712*        ELSE                                                        CL*10
01795              IF ADD-FUNCTION                                         CL*18
01796                  MOVE '1'       TO  CO-AR-BAL-LEVEL.                 CL*18
01797                                                                      CL**6
01798      IF PI-AR-PROCESSING                                             CL**6
01799          IF CKPULLL > ZERO                                           CL*12
01800              MOVE CKPULLI       TO  CO-AR-PULL-CHECK                 CL*12
01801          ELSE                                                        CL*12
01802              IF ADD-FUNCTION                                         CL*18
01803                  MOVE 'Y'       TO  CO-AR-PULL-CHECK.                CL*18
01804                                                                      CL**6
01818      IF MAILNAML > ZERO                                           EL652
01819          MOVE MAILNAMI           TO  CO-MAIL-NAME                    CL**6
01820                                      WS-SAVE-NAME.                   CL**6
01821                                                                   EL652
01822      IF ACCTNAML > ZERO                                           EL652
01823          MOVE ACCTNAMI           TO  CO-ACCT-NAME.                EL652
01824                                                                   EL652
01825      IF ADDR1L > ZERO                                             EL652
01826          MOVE ADDR1I             TO  CO-ADDR-1.                   EL652
01827                                                                   EL652
01828      IF ADDR2L > ZERO                                             EL652
01829          MOVE ADDR2I             TO  CO-ADDR-2.                   EL652
01830                                                                   EL652
01831      IF CITYL > ZERO
01832          MOVE CITYI              TO  CO-ADDR-CITY.
01831      IF STATEL > ZERO
01832          MOVE STATEI             TO  CO-ADDR-STATE.
01833                                                                   EL652
01834      IF ZIPCODEL NOT > ZERO                                          CL*28
01835          GO TO 6015-CONT.                                            CL*28
01836                                                                   EL652
01837      MOVE ZIPCODEI               TO  WS-ZIP-CODE.                    CL*28
01838                                                                   EL652
01839      IF WS-CANADIAN-ZIP                                              CL*28
01840          IF WS-ZIP-4 = SPACE  OR  '-'                                CL*28
01841              MOVE WS-ZIP-CAN-2-POST1     TO  CO-CAN-POSTAL-1         CL*28
01842              MOVE WS-ZIP-CAN-2-POST2     TO  CO-CAN-POSTAL-2         CL*28
01843          ELSE                                                        CL*28
01844              MOVE WS-ZIP-CAN-1-POST1     TO  CO-CAN-POSTAL-1         CL*28
01845              MOVE WS-ZIP-CAN-1-POST2     TO  CO-CAN-POSTAL-2         CL*28
01846      ELSE                                                            CL*28
01847          IF WS-ZIP-6 = SPACE  OR  '-'                                CL*28
01848              MOVE WS-ZIP-AM-2-CODE       TO  CO-ZIP-PRIME            CL*28
01849              MOVE WS-ZIP-AM-2-PLUS4      TO  CO-ZIP-PLUS4            CL*28
01850          ELSE                                                        CL*28
01851              MOVE WS-ZIP-AM-1-CODE       TO  CO-ZIP-PRIME            CL*28
01852              MOVE WS-ZIP-AM-1-PLUS4      TO  CO-ZIP-PLUS4.           CL*28
01853                                                                      CL*28
01854  6015-CONT.                                                          CL*28
CIDMOD     IF BILLPRTL > ZERO                                                000
CIDMOD         IF BILLPRTI = ' ' OR 'B' OR 'R' OR 'T' OR 'S'
060506            OR 'O' OR 'E' OR 'C'
CIDMOD             MOVE  BILLPRTI          TO  CO-BILL-SW               00015923
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
LGC186     IF PNT1099L > ZERO                                                000
LGC186         IF PNT1099I = 'N' OR 'Y'
LGC186             MOVE  PNT1099I      TO  CO-CSO-1099                  00015923
LGC186         ELSE
LGC186             MOVE 'N'            TO  CO-CSO-1099
LGC186                                     PNT1099O
LGC186         END-IF
LGC186     END-IF.
LGC186                                                                       000
01856      IF CSRL > ZERO                                                  CL*31
01857          MOVE CSRI               TO  CO-CSR-CODE.                    CL*31
01858                                                                      CL*42
01876      IF SSNL > ZERO                                               EL652
01887          MOVE SSNI               TO  CO-SOC-SEC.                     CL*41
01888                                                                   EL652
01889      IF PHONEL > ZERO                                             EL652
01890          MOVE PI-SAVE-PHONE      TO  CO-TELEPHONE                 EL652
01891          MOVE ZEROS              TO  PI-SAVE-PHONE.               EL652
01892                                                                      CL*48
01893      IF FAXNOL > ZERO                                                CL*48
01894          MOVE PI-SAVE-FAXNO      TO  CO-FAXNO                        CL*48
01895          MOVE ZEROS              TO  PI-SAVE-FAXNO.                  CL*48
01896                                                                   EL652
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

01897      IF CARBALL > ZERO                                            EL652
01898          MOVE CARBALI            TO  CO-BALANCE-CONTROL              CL*22
01899          GO TO 6020-CONT.                                            CL*22
01900                                                                   EL652
01901      IF NOT PI-AR-PROCESSING                                         CL*22
01902          GO TO 6020-CONT.                                            CL*22
01903                                                                      CL*22
033105     IF ADD-FUNCTION
033105        AND (TYPEI = 'G' OR 'B')
01905          MOVE 'Y'                TO  CO-BALANCE-CONTROL              CL*22
01906          GO TO 6020-CONT.                                            CL*22
01907                                                                      CL*22
01908      IF ADD-FUNCTION                                                 CL*22
01909          IF TYPEI = 'A'                                              CL*22
01910              IF FINRESPI = ACCTNOI                                   CL*22
01911                  MOVE 'Y'        TO  CO-BALANCE-CONTROL              CL*22
01912              ELSE                                                    CL*22
01913                  MOVE 'N'        TO  CO-BALANCE-CONTROL.             CL*22
01914                                                                      CL*22
01915      IF ADD-FUNCTION AND TYPEI = 'C'                                 CL*22
01916          MOVE 'N'                TO  CO-BALANCE-CONTROL.             CL*22
01917                                                                      CL*22
01918  6020-CONT.                                                          CL*22
01919      IF BALFWDL > ZERO                                            EL652
01920          MOVE WS-SAVE-BALFWD     TO  CO-CURRENT-BAL-FWD.          EL652
01921                                                                   EL652
01922      IF CURCOML > ZERO                                            EL652
01923          MOVE WS-SAVE-CURCOM     TO  CO-CURRENT-CUR-COM.          EL652
01924                                                                   EL652
01925      IF CURCHGL > ZERO                                            EL652
01926          MOVE WS-SAVE-CURCHG     TO  CO-CURRENT-CUR-CHG.          EL652
01927                                                                   EL652
01928      IF CURPMTL > ZERO                                            EL652
01929          MOVE WS-SAVE-CURPMT     TO  CO-CURRENT-CUR-PMT.          EL652
01930                                                                   EL652
01931      IF ENDBALL > ZERO                                            EL652
01932          MOVE WS-SAVE-ENDBAL     TO  CO-CURRENT-END-BAL.          EL652
01933                                                                   EL652
043007*    IF YTDCOML > ZERO
043007*       IF CO-ACCOUNT-TYPE
043007*          MOVE WS-SAVE-YTDCOM   TO CO-CURRENT-YTD-COM
043007*       ELSE
043007*          MOVE WS-SAVE-YTDCOM   TO CO-CURRENT-YTD-OV
043007*       END-IF
043007*    END-IF

           .
01940  6099-EXIT.                                                       EL652
01941      EXIT.                                                        EL652
01942  EJECT                                                            EL652
       6100-EDIT-ERAGTC.

           EXEC CICS READ
                SET     (ADDRESS OF AGENT-COMMISSIONS)
                DATASET ('ERAGTC')
                RIDFLD  (PI-ERCOMP-KEY)
                RESP    (WS-RESPONSE)
           END-EXEC
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

01943  6500-UPDATE-RQST.                                                   CL*34
01944                                                                      CL*34
01945      MOVE LOW-VALUES             TO  ERRQST-KEY-3.                   CL*34
01946      MOVE PI-ERC-COMPANY-CD      TO  RQST-COMP-ID.                   CL*50
01947      MOVE PI-ERC-CARRIER         TO  RQST-CARRIER.                   CL*34
01948      MOVE PI-ERC-GROUP           TO  RQST-GROUP.                     CL*34
01949      MOVE PI-ERC-RESP            TO  RQST-FIN-RESP.                  CL*34
01950      MOVE PI-ERC-ACCT            TO  RQST-ACCT-AGENT.                CL*34
01951                                                                      CL*34
01952      EXEC CICS HANDLE CONDITION                                      CL*34
01953          ENDFILE  (6550-END-BROWSE)                                  CL*34
01954          NOTFND   (6550-END-BROWSE)                                  CL*34
01955      END-EXEC.                                                       CL*34
01956                                                                      CL*34
01957      EXEC CICS STARTBR                                               CL*34
01958          DATASET  (RQST-FILE-ID-3)                                   CL*34
01959          RIDFLD   (ERRQST-KEY-3)                                     CL*34
01960      END-EXEC.                                                       CL*34
01961                                                                      CL*34
01962  6525-READ-LOOP.                                                     CL*34
01963      EXEC CICS HANDLE CONDITION                                      CL*34
01964          ENDFILE  (6550-END-BROWSE)                                  CL*34
01965          NOTFND   (6550-END-BROWSE)                                  CL*34
01966      END-EXEC.                                                       CL*34
01967                                                                      CL*34
01968      EXEC CICS READNEXT                                              CL*34
01969          DATASET  (RQST-FILE-ID-3)                                   CL*34
01970          SET      (ADDRESS OF AR-REQUEST-RECORD)                     CL*55
01971          RIDFLD   (ERRQST-KEY-3)                                     CL*34
01972      END-EXEC.                                                       CL*34
01973                                                                      CL*34
01974      IF RQ-CONTROL-BY-FIN-RESP = WS-SAVE-RQST                        CL*34
01975          GO TO 6525-READ-LOOP.                                       CL*34
01976                                                                      CL*34
01977      IF RQ-COMPANY-CD-A2 =  PI-ERC-COMPANY-CD AND                    CL*50
01978         RQ-CARRIER-A2    =  PI-ERC-CARRIER    AND                    CL*34
01979         RQ-GROUPING-A2   =  PI-ERC-GROUP      AND                    CL*34
01980         RQ-FIN-RESP-A2   =  PI-ERC-RESP       AND                    CL*34
01981         RQ-ACCT-AGENT-A2 =  PI-ERC-ACCT                              CL*34
01982          NEXT SENTENCE                                               CL*34
01983      ELSE                                                            CL*34
01984          GO TO 6550-END-BROWSE.                                      CL*34
01985                                                                      CL*34
01986      IF WS-SUMM-FOR-RQST = RQ-SUMMARY-CODE                           CL*34
01987          GO TO 6525-READ-LOOP.                                       CL*34
01988                                                                      CL*34
01989      MOVE RQ-CONTROL-BY-FIN-RESP TO  WS-SAVE-RQST.                   CL*34
01990      MOVE RQ-CONTROL-PRIMARY     TO  ERRQST-KEY.                     CL*34
01991                                                                      CL*34
01992      EXEC CICS ENDBR                                                 CL*34
01993          DATASET  (RQST-FILE-ID-3)                                   CL*34
01994      END-EXEC.                                                       CL*34
01995                                                                      CL*34
01996      EXEC CICS HANDLE CONDITION                                      CL*34
01997          ENDFILE  (6599-EXIT)                                        CL*34
01998          NOTFND   (6599-EXIT)                                        CL*34
01999      END-EXEC.                                                       CL*34
02000                                                                      CL*34
02001      EXEC CICS READ                                                  CL*34
02002          DATASET  (RQST-FILE-ID)                                     CL*34
02003          SET      (ADDRESS OF AR-REQUEST-RECORD)                     CL*55
02004          RIDFLD   (ERRQST-KEY)                                       CL*34
02005          UPDATE                                                      CL*34
02006      END-EXEC.                                                       CL*34
02007                                                                      CL*34
02008      MOVE WS-SUMM-FOR-RQST       TO  RQ-SUMMARY-CODE.                CL*34
02009                                                                      CL*34
02010      EXEC CICS REWRITE                                               CL*34
02011          DATASET  (RQST-FILE-ID)                                     CL*34
02012          FROM     (AR-REQUEST-RECORD)                                CL*34
02013      END-EXEC.                                                       CL*34
02014                                                                      CL*34
02015      GO TO 6500-UPDATE-RQST.                                         CL*34
02016                                                                      CL*34
02017  6550-END-BROWSE.                                                    CL*34
02018                                                                      CL*34
02019      EXEC CICS ENDBR                                                 CL*34
02020          DATASET  (RQST-FILE-ID-3)                                   CL*34
02021      END-EXEC.                                                       CL*34
02022                                                                      CL*34
02023  6599-EXIT.                                                          CL*34
02024      EXIT.                                                           CL*34
02025  EJECT                                                               CL*34
02026                                                                      CL*34
02027  7000-EDIT.                                                       EL652
02028      MOVE LSTMDTI                TO  WS-DATE-MDY-8.                  CL*35
02029                                                                   EL652
02030      IF LSTMDTL > ZERO                                            EL652
02031          NEXT SENTENCE                                            EL652
02032      ELSE                                                         EL652
02033          GO TO 7100-EDIT-CONTD.                                   EL652
02034                                                                   EL652
02035      IF LSTMDTI = SPACES                                          EL652
02036          GO TO 7100-EDIT-CONTD.                                   EL652
02037                                                                   EL652
02038      IF WS-DMDY8-SL1 = '/'  AND                                   EL652
02039         WS-DMDY8-SL2 = '/'                                        EL652
02040          MOVE WS-DMDY8-MM        TO  WS-YMD-MM                    EL652
02041          MOVE WS-DMDY8-DD        TO  WS-YMD-DD                    EL652
02042          MOVE WS-DMDY8-YY        TO  WS-YMD-YY                    EL652
02043      ELSE                                                         EL652
02044          MOVE WS-DMDY6-MM        TO  WS-YMD-MM                    EL652
02045          MOVE WS-DMDY6-DD        TO  WS-YMD-DD                    EL652
02046          MOVE WS-DMDY6-YY        TO  WS-YMD-YY.                   EL652
02047                                                                   EL652
02048      MOVE WS-YMD-DATE            TO  DC-GREG-DATE-1-YMD.          EL652
02049      MOVE '3'                    TO  DC-OPTION-CODE.              EL652
02050                                                                   EL652
02051      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.             EL652
02052                                                                   EL652
02053      IF NO-CONVERSION-ERROR                                       EL652
02054          NEXT SENTENCE                                            EL652
02055      ELSE                                                         EL652
02056          GO TO 7090-DATE-ERROR.                                   EL652
02057                                                                   EL652
02058      IF DC-BIN-DATE-1 > PI-CR-MONTH-END-DT                        EL652
02059          NEXT SENTENCE                                            EL652
02060      ELSE                                                         EL652
02061          GO TO 7100-EDIT-CONTD.                                   EL652
02062                                                                   EL652
02063  7090-DATE-ERROR.                                                 EL652
02064      MOVE -1                     TO  LSTMDTL.                     EL652
02065      MOVE AL-UABON               TO  LSTMDTA.                     EL652
02066      MOVE ER-0314                TO  EMI-ERROR.                   EL652
02067                                                                   EL652
02068      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL652
02069                                                                   EL652
02070  7100-EDIT-CONTD.                                                 EL652
02104                                                                      CL*31
02105      IF CSRL > ZEROS                                                 CL*31
02106          MOVE AL-UANON           TO  CSRA                            CL*31
111103     END-IF
02130                                                                   EL652
02131      IF MAILNAML > ZERO                                           EL652
02132          MOVE AL-UANON           TO  MAILNAMA.                    EL652
02133                                                                   EL652
02134      IF SSNL > ZERO                                               EL652
02135          MOVE AL-UANON           TO  SSNA.                        EL652
02136                                                                   EL652
02137      IF ACCTNAML > ZERO                                           EL652
02138          MOVE AL-UANON           TO  ACCTNAMA                     EL652
02139      ELSE                                                         EL652
02140          IF ADD-FUNCTION                                          EL652
02141              MOVE -1             TO  ACCTNAML                     EL652
02142              MOVE AL-UABON       TO  ACCTNAMA                     EL652
02143              MOVE ER-2045        TO  EMI-ERROR                    EL652
02144              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL652
02145                                                                   EL652
02146      IF PHONEL > ZERO                                             EL652
02147          MOVE PHONEI               TO  DEEDIT-FIELD               EL652
02148          PERFORM 7100-DEEDIT  THRU  7100-EXIT                     EL652
02149          IF DEEDIT-FIELD-V0 = ZEROS                               EL652
02150              MOVE SPACES               TO  PHONEO                 EL652
02151                                            PI-SAVE-PHONE          EL652
02152          ELSE                                                     EL652
02153              IF (DEEDIT-FIELD-V0 > 9999999999                     EL652
02154                OR < 2000000000)                                   EL652
02155                  MOVE -1               TO  PHONEL                 EL652
02156                  MOVE AL-UNBON         TO  PHONEA                 EL652
02157                  MOVE ER-2046          TO  EMI-ERROR              EL652
02158                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT       EL652
02159              ELSE                                                 EL652
02160                  MOVE DEEDIT-FIELD-V0  TO  WS-PHONE-IN            EL652
02161                                            PI-SAVE-PHONE-RED      EL652
02162                  MOVE WSPI-AREA        TO  WSPO-AREA              EL652
02163                  MOVE WSPI-PFX         TO  WSPO-PFX               EL652
02164                  MOVE WSPI-SFX         TO  WSPO-SFX               EL652
02165                  MOVE WS-PHONE-OUT     TO  PHONEO                 EL652
02166                  MOVE AL-UNNON         TO  PHONEA.                EL652
02167                                                                   EL652
02168      IF FAXNOL > ZERO                                                CL*48
02169          MOVE FAXNOI               TO  DEEDIT-FIELD                  CL*48
02170          PERFORM 7100-DEEDIT  THRU  7100-EXIT                        CL*48
02171          IF DEEDIT-FIELD-V0 = ZEROS                                  CL*48
02172              MOVE SPACES               TO  FAXNOO                    CL*48
02173                                            PI-SAVE-FAXNO             CL*48
02174          ELSE                                                        CL*48
02175              IF (DEEDIT-FIELD-V0 > 9999999999                        CL*48
02176                OR < 2000000000)                                      CL*48
02177                  MOVE -1               TO  FAXNOL                    CL*48
02178                  MOVE AL-UNBON         TO  FAXNOA                    CL*48
02179                  MOVE ER-3055          TO  EMI-ERROR                 CL*48
02180                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL*48
02181              ELSE                                                    CL*48
02182                  MOVE DEEDIT-FIELD-V0  TO  WS-PHONE-IN               CL*48
02183                                            PI-SAVE-FAXNO-RED         CL*48
02184                  MOVE WSPI-AREA        TO  WSPO-AREA                 CL*48
02185                  MOVE WSPI-PFX         TO  WSPO-PFX                  CL*48
02186                  MOVE WSPI-SFX         TO  WSPO-SFX                  CL*48
02187                  MOVE WS-PHONE-OUT     TO  FAXNOO                    CL*48
02188                  MOVE AL-UNNON         TO  FAXNOA.                   CL*48
02189                                                                      CL*48
02190      IF ADDR1L > ZERO                                             EL652
02191          MOVE AL-UANON           TO  ADDR1A                       EL652
02192      ELSE                                                         EL652
02193          IF ADD-FUNCTION                                          EL652
02194              MOVE -1             TO  ADDR1L                       EL652
02195              MOVE AL-UABON       TO  ADDR1A                       EL652
02196              MOVE ER-2047        TO  EMI-ERROR                    EL652
02197              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL652
02198                                                                   EL652
02199      IF ADDR2L > ZERO                                             EL652
02200          MOVE AL-UANON           TO  ADDR2A.                      EL652
02201                                                                   EL652
02202      IF CARBALL > ZERO OR CHANGE-FUNCTION                            CL*22
02203          MOVE CARBALI            TO  PI-CHECK-CARRY-BAL           EL652
02204          IF VALID-CARRY-BAL                                       EL652
02205              PERFORM 7010-EDIT-CARBAL  THRU  7010-EXIT            EL652
02206          ELSE                                                     EL652
02207              MOVE -1             TO  CARBALL                      EL652
02208              MOVE AL-UABON       TO  CARBALA                      EL652
02209              MOVE ER-2048        TO  EMI-ERROR                    EL652
02210              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL652
02211      ELSE                                                         EL652
02212          IF ADD-FUNCTION AND NOT PI-AR-PROCESSING                    CL*22
02213              MOVE -1             TO  CARBALL                      EL652
02214              MOVE AL-UABON       TO  CARBALA                      EL652
02215              MOVE ER-2048        TO  EMI-ERROR                    EL652
02216              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL652
02217                                                                      CL*14
02218      IF PI-AR-PROCESSING                                             CL**6
02219          IF TYPEI = 'A'                                              CL**6
02220             IF NETGRSL > ZERO OR CHANGE-FUNCTION                     CL*22
02221                  IF NETGRSI = 'N' OR 'G'                             CL**6
02222                     MOVE AL-UANON    TO  NETGRSA                     CL**6
02223                  ELSE                                                CL**6
02224                      MOVE -1         TO  NETGRSL                     CL**6
02225                      MOVE AL-UABON   TO  NETGRSA                     CL**6
02226                      MOVE ER-3151    TO  EMI-ERROR                   CL**6
02227                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.     CL*18
02228                                                                      CL**6
02229      IF CITYL > ZERO
02230          MOVE AL-UANON           TO  CITYA
02231      ELSE                                                         EL652
02232          IF ADD-FUNCTION                                          EL652
02233              MOVE -1             TO  CITYL
02234              MOVE AL-UABON       TO  CITYA
02235              MOVE ER-2049        TO  EMI-ERROR                    EL652
02236              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL652
02237                                                                      CL**6
02229      IF STATEL   > ZERO                                           EL652
02230         MOVE AL-UANON            TO STATEA
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO CNTL-COMP-ID
              MOVE '3'                 TO CNTL-REC-TYPE
              MOVE STATEI              TO CNTL-ACCESS
              MOVE +0                  TO CNTL-SEQ-NO
              EXEC CICS READ
                 DATASET   (CNTL-FILE-ID)
                 SET       (ADDRESS OF CONTROL-FILE)
                 RIDFLD    (ELCNTL-KEY)
                 RESP      (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE ER-2209          TO EMI-ERROR
                 MOVE -1               TO STATEL
                 MOVE AL-UABON         TO STATEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
02231      ELSE                                                         EL652
02232          IF ADD-FUNCTION                                          EL652
02233              MOVE -1             TO  STATEL                       EL652
02234              MOVE AL-UABON       TO  STATEA                       EL652
02235              MOVE ER-2049        TO  EMI-ERROR                    EL652
02236              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.          EL652
02237                                                                      CL**6
071712*     IF PI-AR-PROCESSING                                             CL**6
071712*         IF ARBALL > ZERO OR CHANGE-FUNCTION                         CL*22
071712*             IF ARBALI = '1' OR '2' OR '3' OR '4'                    CL**6
071712*                 MOVE AL-UANON   TO  ARBALA                          CL*10
071712*             ELSE                                                    CL**6
071712*                 MOVE -1         TO  ARBALL                          CL**6
071712*                 MOVE AL-UABON   TO  ARBALA                          CL**6
071712*                 MOVE ER-3150    TO  EMI-ERROR                       CL**6
071712*                 PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.         CL*18
02247                                                                      CL*42
02284      IF PI-AR-PROCESSING                                             CL*12
02285          IF CKPULLL > ZERO OR CHANGE-FUNCTION                        CL*22
02286              IF CKPULLI = 'Y' OR 'N'                                 CL*12
02287                  MOVE AL-UANON   TO  CKPULLA                         CL*12
02288              ELSE                                                    CL*12
02289                  MOVE -1         TO  CKPULLL                         CL*12
02290                  MOVE AL-UABON   TO  CKPULLA                         CL*12
02291                  MOVE ER-3170    TO  EMI-ERROR                       CL*12
02292                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.         CL*18
02293                                                                   EL652
02294      IF ZIPCODEL > ZERO                                           EL652
02295          MOVE AL-UANON           TO  ZIPCODEA.                    EL652
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
111103              EXEC CICS BIF 
111103                   DEEDIT
111103                   FIELD   (MAXFEEI)  
111103                   LENGTH  (6)      
111103              END-EXEC      
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
092205              EXEC CICS BIF 
092205                   DEEDIT
092205                   FIELD   (MAXLFI)  
092205                   LENGTH  (6)      
092205              END-EXEC      
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
02296                                                                   EL652
02297      IF BALFWDL > ZERO                                            EL652
02298          EXEC CICS BIF                                            EL652
02299              DEEDIT                                               EL652
02300              FIELD   (BALFWDI)                                    EL652
02301              LENGTH  (13)                                         EL652
02302          END-EXEC                                                 EL652
02303          MOVE AL-UNNON           TO  BALFWDA                      EL652
02304          MOVE BALFWDI            TO  WS-SAVE-BALFWD.              EL652
02305                                                                   EL652
02306      IF CURCOML >        ZERO                                     EL652
02307          EXEC CICS BIF                                            EL652
02308              DEEDIT                                               EL652
02309              FIELD   (CURCOMI)                                    EL652
02310              LENGTH  (13)                                         EL652
02311          END-EXEC                                                 EL652
02312          MOVE AL-UNNON           TO  CURCOMA                      EL652
02313          MOVE CURCOMI            TO  WS-SAVE-CURCOM.              EL652
02314                                                                   EL652
02315      IF CURCHGL > ZERO                                            EL652
02316          EXEC CICS BIF                                            EL652
02317              DEEDIT                                               EL652
02318              FIELD   (CURCHGI)                                    EL652
02319              LENGTH  (13)                                         EL652
02320          END-EXEC                                                 EL652
02321          MOVE AL-UNNON           TO  CURCHGA                      EL652
02322          MOVE CURCHGI            TO  WS-SAVE-CURCHG.              EL652
02323                                                                   EL652
02324      IF CURPMTL > ZERO                                            EL652
02325          EXEC CICS BIF                                            EL652
02326              DEEDIT                                               EL652
02327              FIELD   (CURPMTI)                                    EL652
02328              LENGTH  (13)                                         EL652
02329          END-EXEC                                                 EL652
02330          MOVE AL-UNNON           TO  CURPMTA                      EL652
02331          MOVE CURPMTI            TO  WS-SAVE-CURPMT.              EL652
02332                                                                   EL652
02333      IF ENDBALL > ZERO                                            EL652
02334          EXEC CICS BIF                                            EL652
02335              DEEDIT                                               EL652
02336              FIELD   (ENDBALI)                                    EL652
02337              LENGTH  (13)                                         EL652
02338          END-EXEC                                                 EL652
02339          MOVE AL-UNNON           TO  ENDBALA                      EL652
02340          MOVE ENDBALI            TO  WS-SAVE-ENDBAL.              EL652
02341                                                                   EL652
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
02351  7000-EXIT.                                                       EL652
02352      EXIT.                                                        EL652
02353  EJECT                                                            EL652
02354  7010-EDIT-CARBAL.                                                EL652
033105     IF TYPEI = 'G' OR 'B'
02356          IF CARBALI NOT = 'Y'                                     EL652
02357              MOVE -1             TO  CARBALL                      EL652
02358              MOVE AL-UABON       TO  CARBALA                      EL652
02359              MOVE ER-2093        TO  EMI-ERROR                    EL652
02360              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL652
02361              GO TO 7010-EXIT.                                     EL652
02362                                                                   EL652
02363      IF TYPEI = 'A'                                                  CL*17
02367          IF FINRESPI = ACCTNOI                         
02368              IF CARBALI NOT = 'Y'                     
02369                  MOVE -1         TO  CARBALL         
02370                  MOVE AL-UABON   TO  CARBALA        
02371                  MOVE ER-2094    TO  EMI-ERROR     
02372                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT   
02373                  GO TO 7010-EXIT.                            
02374                                                                      CL*15
02375      IF PI-PROCESSOR-ID = 'E864'                                     CL*17
02376          NEXT SENTENCE                                               CL*17
02377      ELSE                                                            CL*17
02378         IF TYPEI = 'A'                                               CL*17
02379            IF PI-AR-PROCESSING                                       CL*17
02380                IF FINRESPI NOT = ACCTNOI                             CL*17
02381                     IF CARBALI = 'Y'                                 CL*17
02382                        MOVE -1             TO  CARBALL               CL*17
02383                        MOVE AL-UABON       TO  CARBALA               CL*17
02384                        MOVE ER-3174        TO  EMI-ERROR             CL*17
02385                        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.   CL*17
02386                                                                   EL652
02387      IF TYPEI = 'C'                                               EL652
02388          IF CARBALI NOT = 'N'                                     EL652
02389              MOVE -1             TO  CARBALL                      EL652
02390              MOVE AL-UABON       TO  CARBALA                      EL652
02391              MOVE ER-2096        TO  EMI-ERROR                    EL652
02392              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL652
02393              GO TO 7010-EXIT.                                     EL652
02394                                                                   EL652
02395      IF PI-PROCESSOR-ID = 'E864'                                     CL*17
02396          NEXT SENTENCE                                               CL*17
02397      ELSE                                                            CL*17
02398         IF CHANGE-FUNCTION                                           CL*17
02402             IF PI-ERC-END-BAL NOT = ZERO              
02403                 IF CARBALI = 'N'                     
02404                     MOVE -1         TO  CARBALL     
02405                     MOVE AL-UABON   TO  CARBALA    
02406                     MOVE ER-2095    TO  EMI-ERROR 
02407                     PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT 
02408                     GO TO 7010-EXIT.                          
02409                                                                   EL652
02410      MOVE AL-UANON               TO  CARBALA.                     EL652
02411                                                                   EL652
02412  7010-EXIT.                                                       EL652
02413      EXIT.                                                        EL652
02414  EJECT                                                            EL652
02415  7050-READ-ERCOMP.                                                EL652
02416      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.              CL*50
02417                                                                   EL652
02418      EXEC CICS READ                                               EL652
02419          DATASET  (COMP-FILE-ID)                                  EL652
02420          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*55
02421          RIDFLD   (PI-ERCOMP-KEY)                                 EL652
02422      END-EXEC.                                                    EL652
02423                                                                   EL652
02424      MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.          EL652
02425                                                                   EL652
02426  7050-EXIT.                                                       EL652
02427      EXIT.                                                        EL652
02428  EJECT                                                            EL652
02429  7100-DEEDIT.                                                     EL652
02430      EXEC CICS BIF                                                EL652
02431          DEEDIT                                                   EL652
02432          FIELD   (DEEDIT-FIELD)                                   EL652
02433          LENGTH  (15)                                             EL652
02434      END-EXEC.                                                    EL652
02435                                                                   EL652
02436  7100-EXIT.                                                       EL652
02437      EXIT.                                                        EL652
02438  EJECT                                                            EL652
02439  7150-ERCOMP-GETMAIN.                                             EL652
02440      EXEC CICS GETMAIN                                            EL652
02441          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*55
02442          LENGTH   (ERCOMP-LENGTH)                                 EL652
02443          INITIMG  (GETMAIN-SPACE)                                 EL652
02444      END-EXEC.                                                    EL652
02445                                                                   EL652
02446  7150-EXIT.                                                       EL652
02447      EXIT.                                                        EL652
02448  EJECT                                                            EL652
02449  7200-READ-ERCOMP-UPDATE.                                         EL652
02450      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.              CL*50
02451                                                                   EL652
02452      EXEC CICS READ                                               EL652
02453          DATASET  (COMP-FILE-ID)                                  EL652
02454          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*55
02455          RIDFLD   (PI-ERCOMP-KEY)                                 EL652
02456          UPDATE                                                   EL652
02457      END-EXEC.                                                    EL652
02458                                                                   EL652
02459  7200-EXIT.                                                       EL652
02460      EXIT.                                                        EL652
02461  EJECT                                                            EL652
02462  7250-PAGE-FORWARD.                                               EL652
02463      MOVE SPACES                 TO  PI-ERCOMP-EOF-SW.            EL652
02464                                                                   EL652
02465      IF MAINTYPL > ZERO                                           EL652
02466          MOVE GROUPI             TO  PI-ERC-GROUP                    CL**6
02467          MOVE CARRIERI           TO  PI-ERC-CARRIER               EL652
02468          MOVE FINRESPI           TO  PI-ERC-RESP                  EL652
02469          MOVE ACCTNOI            TO  PI-ERC-ACCT                  EL652
02470          MOVE TYPEI              TO  PI-ERC-TYPE                  EL652
02471      ELSE                                                         EL652
02472          MOVE LOW-VALUES         TO  PI-ERCOMP-KEY                EL652
02473          MOVE 'Y'                TO  PI-FIRST-TIME-SW.            EL652
02474                                                                   EL652
02475      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.              CL*50
02476      MOVE PI-ERCOMP-KEY          TO  WS-SAVE-KEY.                 EL652
02477                                                                   EL652
02478      EXEC CICS HANDLE CONDITION                                   EL652
02479          ENDFILE  (7250-ENDFILE)                                  EL652
02480          NOTFND   (7250-ENDFILE)                                  EL652
02481          ERROR    (9990-ABEND)                                    EL652
02482      END-EXEC.                                                    EL652
02483                                                                   EL652
02484      PERFORM 7850-START-BROWSE  THRU  7850-EXIT.                  EL652
02485                                                                   EL652
02486  7250-READ-NEXT.                                                  EL652
02487      EXEC CICS HANDLE CONDITION                                   EL652
02488          ENDFILE  (7250-ENDFILE)                                  EL652
02489          NOTFND   (7275-NOTFOUND)                                 EL652
02490      END-EXEC.                                                    EL652
02491                                                                   EL652
02492      PERFORM 7900-READNEXT  THRU  7900-EXIT.                      EL652
02493                                                                      CL**2
02494      IF PI-CARRIER-SECURITY > SPACES                                 CL**2
02495          IF PI-ERC-CARRIER = PI-CARRIER-SECURITY                     CL**2
02496              NEXT SENTENCE                                           CL**2
02497          ELSE                                                        CL**2
02498              GO TO 7250-READ-NEXT.                                   CL**2
02499                                                                   EL652
02500      IF ERCOMP-EOF                                                EL652
02501          IF FIRST-TIME                                            EL652
02502              MOVE LOW-VALUES     TO  EL652AO                      EL652
02503              MOVE ER-0584        TO  EMI-ERROR                    EL652
02504              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL652
02505              GO TO 8100-SEND-INITIAL-MAP                          EL652
02506          ELSE                                                     EL652
02507              MOVE LOW-VALUES     TO  EL652AO                      EL652
02508              MOVE ER-2067        TO  EMI-ERROR                    EL652
02509              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL652
02510              PERFORM 7950-END-BROWSE  THRU  7950-EXIT             EL652
02511              GO TO 7250-PAGE-FORWARD.                             EL652
02512                                                                   EL652
02513      IF PI-ERCOMP-KEY = WS-SAVE-KEY                               EL652
02514          GO TO 7250-READ-NEXT.                                    EL652
02515                                                                   EL652
02516      MOVE CO-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL652
02517      MOVE CO-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL652
02518      MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.          EL652
02519      MOVE LOW-VALUES             TO  EL652AO.                     EL652
02520      MOVE CO-CARRIER             TO  CARRIERO.                    EL652
02521      MOVE CO-GROUPING            TO  GROUPO.                      EL652
02522      MOVE CO-RESP-NO             TO  FINRESPO.                    EL652
02523      MOVE CO-ACCOUNT             TO  ACCTNOO.                     EL652
02524      MOVE CO-TYPE                TO  TYPEO.                       EL652
02525      MOVE AL-UANON               TO  CARRIERA                     EL652
02526                                      GROUPA                       EL652
02527                                      TYPEA                        EL652
02528                                      FINRESPA                     EL652
02529                                      ACCTNOA                      EL652
02530                                      MAINTYPA.                    EL652
02531      MOVE 'S'                    TO  MAINTYPO                     EL652
02532                                      PI-CHECK-MAINT-TYPE.         EL652
02533                                                                   EL652
02534      GO TO 5050-SET-UP-SCREEN.                                    EL652
02535                                                                   EL652
02536  7250-ENDFILE.                                                    EL652
02537      IF FIRST-TIME                                                EL652
02538          MOVE LOW-VALUES         TO  EL652AO                      EL652
02539          MOVE ER-0584            TO  EMI-ERROR                    EL652
02540          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL652
02541          GO TO 8100-SEND-INITIAL-MAP.                             EL652
02542                                                                   EL652
02543      IF BROWSE-STARTED                                            EL652
02544          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL652
02545                                                                   EL652
02546      MOVE ER-2067                TO  EMI-ERROR.                   EL652
02547      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL652
02548                                                                   EL652
02549      MOVE LOW-VALUES             TO  EL652AO.                     EL652
02550                                                                   EL652
02551      GO TO 7250-PAGE-FORWARD.                                     EL652
02552                                                                   EL652
02553  7275-NOTFOUND.                                                   EL652
02554      IF BROWSE-STARTED                                            EL652
02555          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                EL652
02556                                                                   EL652
02557      GO TO 8880-NOT-FOUND.                                        EL652
02558                                                                   EL652
02559  7299-EXIT.                                                       EL652
02560      EXIT.                                                        EL652
02561  EJECT                                                            EL652
02562  7300-PAGE-BACKWARD.                                                 CL*20
02563      MOVE SPACES                 TO  PI-ERCOMP-EOF-SW.               CL*20
02564                                                                      CL*20
02565      IF MAINTYPL > ZERO                                              CL*20
02566          MOVE GROUPI             TO  PI-ERC-GROUP                    CL*20
02567          MOVE CARRIERI           TO  PI-ERC-CARRIER                  CL*20
02568          MOVE FINRESPI           TO  PI-ERC-RESP                     CL*20
02569          MOVE ACCTNOI            TO  PI-ERC-ACCT                     CL*20
02570          MOVE TYPEI              TO  PI-ERC-TYPE                     CL*20
02571      ELSE                                                            CL*20
02572          MOVE LOW-VALUES         TO  PI-ERCOMP-KEY.                  CL*20
02573                                                                      CL*20
02574      MOVE PI-COMPANY-CD          TO  PI-ERC-COMPANY-CD.              CL*50
02575                                                                      CL*20
02576      EXEC CICS HANDLE CONDITION                                      CL*20
02577          ENDFILE  (7375-ENDFILE)                                     CL*20
02578          NOTFND   (7375-ENDFILE)                                     CL*20
02579          ERROR    (9990-ABEND)                                       CL*20
02580      END-EXEC.                                                       CL*20
02581                                                                      CL*20
02582      PERFORM 7850-START-BROWSE  THRU  7850-EXIT.                     CL*20
02583                                                                      CL*20
02584      PERFORM 8050-READPREV  THRU  8050-EXIT.                         CL*20
02585                                                                      CL*20
02586  7350-READ-PREV.                                                     CL*20
02587      EXEC CICS HANDLE CONDITION                                      CL*20
02588          ENDFILE  (7375-ENDFILE)                                     CL*20
02589          NOTFND   (7275-NOTFOUND)                                    CL*20
02590      END-EXEC.                                                       CL*20
02591                                                                      CL*20
02592      PERFORM 8050-READPREV  THRU  8050-EXIT.                         CL*20
02593                                                                      CL*20
02594      IF PI-CARRIER-SECURITY > SPACES                                 CL*20
02595          IF PI-ERC-CARRIER = PI-CARRIER-SECURITY                     CL*20
02596              NEXT SENTENCE                                           CL*20
02597          ELSE                                                        CL*20
02598              GO TO 7350-READ-PREV.                                   CL*20
02599                                                                      CL*20
02600      IF PI-COMPANY-CD NOT = CO-COMPANY-CD                            CL*20
02601          GO TO 7375-ENDFILE.                                         CL*20
02602                                                                      CL*20
02603      MOVE CO-LAST-MAINT-USER     TO  PI-UPDATE-BY.                   CL*20
02604      MOVE CO-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.               CL*20
02605      MOVE PI-ERCOMP-KEY          TO  PI-SAVE-ERCOMP-KEY.             CL*20
02606      MOVE LOW-VALUES             TO  EL652AO.                        CL*20
02607      MOVE CO-CARRIER             TO  CARRIERO.                       CL*20
02608      MOVE CO-GROUPING            TO  GROUPO.                         CL*20
02609      MOVE CO-RESP-NO             TO  FINRESPO.                       CL*20
02610      MOVE CO-ACCOUNT             TO  ACCTNOO.                        CL*20
02611      MOVE CO-TYPE                TO  TYPEO.                          CL*20
02612      MOVE AL-UANON               TO  CARRIERA                        CL*20
02613                                      GROUPA                          CL*20
02614                                      TYPEA                           CL*20
02615                                      FINRESPA                        CL*20
02616                                      ACCTNOA                         CL*20
02617                                      MAINTYPA.                       CL*20
02618      MOVE 'S'                    TO  MAINTYPO                        CL*20
02619                                      PI-CHECK-MAINT-TYPE.            CL*20
02620                                                                      CL*20
02621      GO TO 5050-SET-UP-SCREEN.                                       CL*20
02622                                                                      CL*20
02623  7375-ENDFILE.                                                       CL*20
02624      IF BROWSE-STARTED                                               CL*20
02625          PERFORM 7950-END-BROWSE  THRU  7950-EXIT.                   CL*20
02626                                                                      CL*20
02627      MOVE ER-2238                TO  EMI-ERROR.                      CL*20
02628      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL*36
02629                                                                      CL*20
02630      MOVE -1                     TO  MAINTYPL.                       CL*20
02631                                                                      CL*20
02632      GO TO 8200-SEND-DATAONLY.                                       CL*20
02633                                                                      CL*20
02634  EJECT                                                               CL*20
02635  7400-READ-CONTROL-FILE.                                          EL652
02636      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL652
02637      MOVE WS-ACCESS              TO  CNTL-ACCESS.                 EL652
02638                                                                   EL652
02639      EXEC CICS HANDLE CONDITION                                   EL652
02640          NOTFND  (7490-NOT-FOUND)                                 EL652
02641          ERROR   (9990-ABEND)                                     EL652
02642      END-EXEC.                                                    EL652
02643                                                                   EL652
02644      EXEC CICS READ                                               EL652
02645          DATASET  (CNTL-FILE-ID)                                  EL652
02646          SET      (ADDRESS OF CONTROL-FILE)                          CL*55
02647          RIDFLD   (ELCNTL-KEY)                                    EL652
02648      END-EXEC.                                                    EL652
02649                                                                   EL652
02650      IF CNTL-REC-TYPE = '6'                                       EL652
02651          MOVE AL-UANON           TO  CARRIERA                     EL652
02652          MOVE CARRIERI           TO  PI-ERC-CARRIER               EL652
02653          GO TO 7499-EXIT.                                         EL652
02654                                                                   EL652
02655      IF CF-COMPENSATION-MSTR-MAINT-DT NOT = LOW-VALUES            EL652
02656          GO TO 7499-EXIT                                          EL652
02657      ELSE                                                         EL652
02658          MOVE -1                 TO  MAINTYPL                     EL652
02659          MOVE ER-2572            TO  EMI-ERROR                    EL652
02660          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL652
02661          GO TO 7499-EXIT.                                         EL652
02662                                                                   EL652
02663  7490-NOT-FOUND.                                                  EL652
02664      IF CNTL-REC-TYPE = '6'                                       EL652
02665          MOVE -1                 TO  CARRIERL                     EL652
02666          MOVE AL-UABON           TO  CARRIERA                     EL652
02667          MOVE ER-0193            TO  EMI-ERROR                    EL652
02668      ELSE                                                         EL652
02669          IF CNTL-REC-TYPE = '2'                                      CL*31
02670              MOVE -1                TO  CSRL                         CL*31
02671              MOVE AL-UABON          TO  CSRA                         CL*31
02672              MOVE ER-1883           TO  EMI-ERROR                    CL*31
02673          ELSE                                                        CL*31
02674             MOVE ER-0002            TO  EMI-ERROR                    CL*31
02675             MOVE -1                 TO  PFKEYL.                      CL*31
02676                                                                   EL652
02677      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL652
02678                                                                   EL652
02679  7499-EXIT.                                                       EL652
02680      EXIT.                                                        EL652
02681  EJECT                                                            EL652
02682                                                                      CL**6
02683  7500-UPDATE-SUMM.                                                   CL**6
02684                                                                      CL**6
02685      PERFORM 7700-ERSUMM-GETMAIN  THRU  7700-EXIT.                   CL**6
02686                                                                      CL**6
02687      MOVE 'SX'                   TO SX-RECORD-ID.                    CL**6
02688      MOVE PI-COMPANY-CD          TO SX-COMPANY-CD.                   CL**6
02689      MOVE PI-AR-SUMMARY-CODE     TO SX-SUMMARY.                      CL**6
02690      MOVE PI-ERC-CARRIER         TO SX-CARRIER.                      CL**6
02691      MOVE PI-ERC-GROUP           TO SX-GROUP.                        CL**6
02692      MOVE PI-ERC-RESP            TO SX-FIN-RESP.                     CL**6
02693      MOVE PI-ERC-ACCT            TO SX-ACCT-AGENT.                   CL**6
02694      MOVE WS-SAVE-NAME           TO SX-SUMM-OR-AGT-NAME.             CL**6
02695      MOVE PI-PROCESSOR-ID        TO SX-LAST-MAINT-BY.                CL**6
02696      MOVE EIBTIME                TO SX-LAST-MAINT-HHMMSS.            CL**6
02697      MOVE BIN-CURRENT-SAVE       TO SX-LAST-MAINT-DT.                CL**6
02698      MOVE PI-COMPANY-CD          TO SX-COMPANY-A1.                   CL**6
02699      MOVE PI-ERC-ACCT            TO SX-ACCT-AGENT-A1.                CL**6
02700      MOVE PI-AR-SUMMARY-CODE     TO SX-SUMMARY-A1.                   CL**6
02701      MOVE PI-ERC-CARRIER         TO SX-CARR-A1.                      CL**6
02702      MOVE PI-ERC-GROUP           TO SX-GROUP-A1.                     CL**6
02703      MOVE PI-ERC-RESP            TO SX-FIN-RESP-A1.                  CL**6
02704                                                                      CL**6
02705      EXEC CICS HANDLE CONDITION                                      CL**6
02706          DUPREC  (7550-DUP)                                          CL**6
02707      END-EXEC.                                                       CL**6
02708                                                                      CL**6
02709      EXEC CICS WRITE                                                 CL**6
02710          DATASET  (SUMM-FILE-ID)                                     CL**6
02711          FROM     (SUMM-CROSS-REFERENCE)                             CL**6
02712          RIDFLD   (SX-CONTROL-PRIMARY)                               CL**6
02713      END-EXEC.                                                       CL**6
02714                                                                      CL**6
02715      MOVE LOW-VALUES             TO ERSUMM-KEY.                      CL**6
02716      MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.                    CL**6
02717      MOVE PI-AR-SUMMARY-CODE     TO SUMM-SUMMARY.                    CL**6
02718                                                                      CL**6
02719      EXEC CICS HANDLE CONDITION                                      CL**6
02720          NOTFND  (7540-WRITE)                                        CL**6
02721      END-EXEC.                                                       CL**6
02722                                                                      CL**6
02723      EXEC CICS READ                                                  CL**6
02724          DATASET  (SUMM-FILE-ID)                                     CL**6
02725          SET      (ADDRESS OF SUMM-CROSS-REFERENCE)                  CL*55
02726          RIDFLD   (ERSUMM-KEY)                                       CL**6
02727      END-EXEC.                                                       CL**6
02728      GO TO 7599-EXIT.                                                CL**6
02729                                                                      CL**6
02730  7540-WRITE.                                                         CL**6
02731                                                                      CL**6
02732      MOVE ERSUMM-KEY             TO SX-CONTROL-PRIMARY.              CL**6
02733      MOVE LOW-VALUES             TO SX-CONTROL-A1.                   CL**6
02734      MOVE PI-COMPANY-CD          TO SX-COMPANY-A1.                   CL**6
02735      MOVE PI-AR-SUMMARY-CODE     TO SX-SUMMARY-A1.                   CL**6
02736      MOVE SPACES                 TO SX-SUMM-OR-AGT-NAME.             CL**6
02737      MOVE 'LGXX'                 TO SX-LAST-MAINT-BY.                CL**6
02738                                                                      CL**6
02739      EXEC CICS WRITE                                                 CL**6
02740          DATASET  (SUMM-FILE-ID)                                     CL**6
02741          FROM     (SUMM-CROSS-REFERENCE)                             CL**6
02742          RIDFLD   (SX-CONTROL-PRIMARY)                               CL**6
02743      END-EXEC.                                                       CL**6
02744                                                                      CL**6
02745      MOVE PI-ERC-CARRIER         TO PI-CR-CARRIER.                   CL**6
02746      MOVE PI-ERC-GROUP           TO PI-CR-GROUPING.                  CL**6
02747      MOVE PI-ERC-TYPE            TO PI-CR-TYPE.                      CL**6
02748      MOVE PI-ERC-RESP            TO PI-CR-FIN-RESP.                  CL**6
02749      MOVE PI-ERC-ACCT            TO PI-CR-ACCOUNT.                   CL**6
02750                                                                      CL**6
02751      MOVE XCTL-856               TO PGM-NAME.                        CL**6
02752      GO TO 9300-XCTL.                                                CL**6
02753                                                                      CL**6
02754  7550-DUP.                                                           CL**6
02755                                                                      CL**6
02756      MOVE ER-3152                TO EMI-ERROR.                       CL**6
02757      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**6
02758                                                                      CL**6
02759      MOVE -1                     TO MAINTYPL.                        CL**6
02760                                                                      CL**6
02761      GO TO 8200-SEND-DATAONLY.                                       CL**6
02762                                                                      CL**6
02763  7599-EXIT.                                                          CL**6
02764      EXIT.                                                           CL**6
02765  EJECT                                                               CL**6
02766                                                                      CL**6
02767  7600-DELETE-SUMM.                                                   CL**6
02768                                                                      CL**6
02769      MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.                    CL**6
02770      MOVE WS-SAVE-SUMM           TO SUMM-SUMMARY.                    CL**6
02771      MOVE PI-ERC-CARRIER         TO SUMM-CARRIER.                    CL**6
02772      MOVE PI-ERC-GROUP           TO SUMM-GROUP.                      CL**6
02773      MOVE PI-ERC-RESP            TO SUMM-FIN-RESP.                   CL**6
02774      MOVE PI-ERC-ACCT            TO SUMM-ACCT-AGENT.                 CL**6
02775                                                                      CL**6
02776  7620-READ-ERSUMM.                                                   CL**6
02777      EXEC CICS HANDLE CONDITION                                      CL**6
02778          NOTFND  (7699-EXIT)                                         CL**6
02779      END-EXEC.                                                       CL**6
02780                                                                      CL**6
02781      EXEC CICS READ                                                  CL**6
02782          DATASET  (SUMM-FILE-ID)                                     CL**6
02783          SET      (ADDRESS OF SUMM-CROSS-REFERENCE)                  CL*55
02784          RIDFLD   (ERSUMM-KEY)                                       CL**6
02785          UPDATE                                                      CL**6
02786      END-EXEC.                                                       CL**6
02787                                                                      CL**6
02788      EXEC CICS DELETE                                                CL**6
02789          DATASET  (SUMM-FILE-ID)                                     CL*34
02790      END-EXEC.                                                       CL*34
02791                                                                      CL*34
02792      MOVE LOW-VALUES             TO ERSUMM-KEY.                      CL*34
02793      MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.                    CL*34
02794      MOVE WS-SAVE-SUMM           TO SUMM-SUMMARY.                    CL*34
02795                                                                      CL*34
02796      EXEC CICS STARTBR                                               CL*34
02797          DATASET  (SUMM-FILE-ID)                                     CL*34
02798          RIDFLD   (ERSUMM-KEY)                                       CL*34
02799      END-EXEC.                                                       CL*34
02800                                                                      CL*34
02801      EXEC CICS READNEXT                                              CL*34
02802          DATASET  (SUMM-FILE-ID)                                     CL*34
02803          SET      (ADDRESS OF SUMM-CROSS-REFERENCE)                  CL*55
02804          RIDFLD   (ERSUMM-KEY)                                       CL*34
02805      END-EXEC.                                                       CL*34
02806                                                                      CL*34
02807      IF SX-CARRIER = LOW-VALUES                                      CL*34
02808          NEXT SENTENCE                                               CL*34
02809      ELSE                                                            CL*34
02810          GO TO 7640-END-BROWSE.                                      CL*34
02811                                                                      CL*34
02812      EXEC CICS HANDLE CONDITION                                      CL*34
02813          ENDFILE  (7630-DELETE)                                      CL*34
02814      END-EXEC.                                                       CL*34
02815                                                                      CL*34
02816      EXEC CICS READNEXT                                              CL*34
02817          DATASET  (SUMM-FILE-ID)                                     CL*34
02818          SET      (ADDRESS OF SUMM-CROSS-REFERENCE)                  CL*55
02819          RIDFLD   (ERSUMM-KEY)                                       CL*34
02820      END-EXEC.                                                       CL*34
02821                                                                      CL*34
02822      IF SX-SUMMARY = WS-SAVE-SUMM                                    CL*34
02823          GO TO 7640-END-BROWSE.                                      CL*34
02824                                                                      CL*34
02825  7630-DELETE.                                                        CL*34
02826      EXEC CICS ENDBR                                                 CL*34
02827          DATASET  (SUMM-FILE-ID)                                     CL*34
02828      END-EXEC.                                                       CL*34
02829                                                                      CL*34
02830      MOVE LOW-VALUES             TO ERSUMM-KEY.                      CL*34
02831      MOVE PI-COMPANY-CD          TO SUMM-COMP-ID.                    CL*34
02832      MOVE WS-SAVE-SUMM           TO SUMM-SUMMARY.                    CL*34
02833                                                                      CL*34
02834      EXEC CICS READ                                                  CL*34
02835          DATASET  (SUMM-FILE-ID)                                     CL*34
02836          SET      (ADDRESS OF SUMM-CROSS-REFERENCE)                  CL*55
02837          RIDFLD   (ERSUMM-KEY)                                       CL*34
02838          UPDATE                                                      CL*34
02839      END-EXEC.                                                       CL*34
02840                                                                      CL*34
02841      EXEC CICS DELETE                                                CL*34
02842          DATASET  (SUMM-FILE-ID)                                     CL*34
02843      END-EXEC.                                                       CL*34
02844                                                                      CL*34
02845      GO TO 7699-EXIT.                                                CL*34
02846                                                                      CL*34
02847  7640-END-BROWSE.                                                    CL*34
02848      EXEC CICS ENDBR                                                 CL*34
02849          DATASET  (SUMM-FILE-ID)                                     CL**6
02850      END-EXEC.                                                       CL**6
02851                                                                      CL**6
02852  7699-EXIT.                                                          CL**6
02853      EXIT.                                                           CL**6
02854  EJECT                                                               CL**6
02855                                                                      CL**6
02856  7700-ERSUMM-GETMAIN.                                                CL**6
02857      EXEC CICS GETMAIN                                               CL**6
02858          SET      (ADDRESS OF SUMM-CROSS-REFERENCE)                  CL*55
02859          LENGTH   (ERSUMM-LENGTH)                                    CL**6
02860          INITIMG  (GETMAIN-SPACE)                                    CL**6
02861      END-EXEC.                                                       CL**6
02862                                                                      CL**6
02863  7700-EXIT.                                                          CL**6
02864      EXIT.                                                           CL**6
02865  EJECT                                                               CL**6
02866                                                                      CL**6
02867  7850-START-BROWSE.                                               EL652
02868      EXEC CICS STARTBR                                            EL652
02869          DATASET  (COMP-FILE-ID)                                  EL652
02870          RIDFLD   (PI-ERCOMP-KEY)                                 EL652
02871      END-EXEC.                                                    EL652
02872                                                                   EL652
02873      MOVE 'Y'                    TO  WS-BROWSE-SW.                EL652
02874                                                                   EL652
02875  7850-EXIT.                                                       EL652
02876      EXIT.                                                        EL652
02877  EJECT                                                            EL652
02878  7900-READNEXT.                                                   EL652
02879      EXEC CICS READNEXT                                           EL652
02880          DATASET  (COMP-FILE-ID)                                  EL652
02881          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*55
02882          RIDFLD   (PI-ERCOMP-KEY)                                 EL652
02883      END-EXEC.                                                    EL652
02884                                                                   EL652
02885      IF PI-COMPANY-CD NOT = CO-COMPANY-CD                         EL652
02886          MOVE 'Y'                TO  PI-ERCOMP-EOF-SW             EL652
02887      ELSE                                                         EL652
02888          MOVE SPACE              TO  PI-FIRST-TIME-SW.            EL652
02889                                                                   EL652
02890  7900-EXIT.                                                       EL652
02891      EXIT.                                                        EL652
02892  EJECT                                                            EL652
02893  7950-END-BROWSE.                                                 EL652
02894      EXEC CICS ENDBR                                              EL652
02895          DATASET  (COMP-FILE-ID)                                  EL652
02896      END-EXEC.                                                    EL652
02897                                                                   EL652
02898      MOVE 'N'                    TO  WS-BROWSE-SW.                EL652
02899                                                                   EL652
02900  7950-EXIT.                                                       EL652
02901      EXIT.                                                        EL652
02902  EJECT                                                            EL652
02903  8000-UPDATE-MAINT-DATE.                                          EL652
02904      MOVE SPACES                 TO  ELCNTL-KEY.                  EL652
02905      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL652
02906      MOVE '1'                    TO  CNTL-REC-TYPE.               EL652
02907      MOVE +0                     TO  CNTL-SEQ-NO.                 EL652
02908                                                                   EL652
02909      EXEC CICS HANDLE CONDITION                                   EL652
02910          NOTFND  (8000-EXIT)                                      EL652
02911      END-EXEC.                                                    EL652
02912                                                                   EL652
02913      EXEC CICS READ                                               EL652
02914          UPDATE                                                   EL652
02915          DATASET  (CNTL-FILE-ID)                                  EL652
02916          SET      (ADDRESS OF CONTROL-FILE)                          CL*55
02917          RIDFLD   (ELCNTL-KEY)                                    EL652
02918      END-EXEC.                                                    EL652
02919                                                                   EL652
02920      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL652
02921      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL652
02922      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL652
02923                                                                   EL652
02924      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL652
02925                                                                   EL652
02926      MOVE BIN-CURRENT-SAVE  TO  CF-COMPENSATION-MSTR-MAINT-DT.    EL652
02927      MOVE CONTROL-FILE      TO  JP-RECORD-AREA.                   EL652
02928      MOVE 'C'               TO  JP-RECORD-TYPE.                   EL652
02929      MOVE CNTL-FILE-ID      TO  FILE-ID.                          EL652
02930                                                                   EL652
02931      EXEC CICS REWRITE                                            EL652
02932          DATASET  (CNTL-FILE-ID)                                  EL652
02933          FROM     (CONTROL-FILE)                                  EL652
02934      END-EXEC.                                                    EL652
02935                                                                   EL652
02936      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL652
02937                                                                   EL652
02938  8000-EXIT.                                                       EL652
02939       EXIT.                                                       EL652
02940  EJECT                                                               CL*20
02941  8050-READPREV.                                                      CL*20
02942      EXEC CICS READPREV                                              CL*20
02943          DATASET  (COMP-FILE-ID)                                     CL*20
02944          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*55
02945          RIDFLD   (PI-ERCOMP-KEY)                                    CL*20
02946      END-EXEC.                                                       CL*20
02947                                                                      CL*20
02948  8050-EXIT.                                                          CL*20
02949      EXIT.                                                           CL*20
02950  EJECT                                                            EL652
02951  8100-SEND-INITIAL-MAP.                                           EL652
02952                                                                      CL*42
02953      IF  NOT CREDIT-SESSION                                          CL*46
02954          MOVE AL-SADOF           TO PFK5A                            CL*46
02955                                     PFK6A.                           CL*46
02956                                                                      CL*46
02957      IF PI-COMPANY-ID NOT = 'NCL'                                    CL*53
02958          MOVE AL-SANOF           TO  LETRCDA                         CL*42
02959          MOVE AL-SADOF           TO  LETDESCA.                       CL*42
02960                                                                      CL*52
02961      IF PI-COMPANY-ID  NOT = 'DDB' AND 'ANT' AND 'ASL' AND           CL*58
02962                              'AN1' AND 'TFS'                         CL*58
02963          MOVE AL-SANOF           TO  BALCDA                          CL*52
02964          MOVE AL-SADOF           TO  BALPRTA.                        CL*52
02965                                                                      CL*43
02966      IF PI-COMPANY-ID   = 'NCL'                                      CL*53
033105        IF TYPEI =  'G' OR 'B'                                       CL*43
02968            MOVE AL-UNNOF         TO  RPTCD2A                         CL*43
02969            MOVE AL-SANOF         TO  RPTCDDA.                        CL*43
02970                                                                      CL*35
020816     IF PI-COMPANY-ID = 'DCC' or 'VPP'
011410        IF TYPEI = 'A'
011410           MOVE AL-SANOF         TO SPPDDHA
011410           MOVE AL-UANON         TO SPPDDA
011410        END-IF
011410     END-IF
02975      MOVE AL-SANON               TO  FLITYPA.                     EL652
02976                                                                      CL*51
02980      IF EIBTRNID NOT = TRANS-ID                                      CL**6
02981         IF PI-AR-PROCESSING                                          CL**9
02982              MOVE AL-SANOF       TO  SCDESCA                         CL**6
02983                                      NGDESCA                         CL**6
071712*                                    BALDESCA                        CL**6
02985                                      CKDESCA                         CL*12
02986              MOVE AL-UANON       TO  SUMMNOA                         CL*35
02987                                      NETGRSA                         CL**6
071712*                                    ARBALA                          CL**6
02989                                      CKPULLA.                        CL*12
02990                                                                   EL652
02991      IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'                        EL652
02992          MOVE AL-UANOF           TO  LSTMDTA                         CL*35
02993          MOVE AL-SANOF           TO  PFK9A                           CL*36
02994      ELSE                                                         EL652
02995          IF PI-AR-PROCESSING                                         CL*54
02996              MOVE AL-SANOF       TO  PFK9A                           CL*54
02997          ELSE                                                        CL*54
02998              MOVE AL-SANOF       TO  LSTMDTA                         CL*54
02999              MOVE AL-SADOF       TO  PFK9A.                          CL*54
03000                                                                      CL*40
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

03006      MOVE SAVE-DATE              TO  RUNDATEO.                    EL652
03007      MOVE EIBTIME                TO  TIME-IN.                     EL652
03008      MOVE TIME-OUT               TO  RUNTIMEO.                    EL652
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
03009      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL652
03010      MOVE -1                     TO  MAINTYPL.                    EL652
03011                                                                   EL652
03012      EXEC CICS SEND                                               EL652
03013          MAP     (MAP-NAME)                                       EL652
03014          MAPSET  (MAPSET-NAME)                                    EL652
03015          FROM    (EL652AO)                                        EL652
03016          ERASE                                                    EL652
03017          CURSOR                                                   EL652
03018      END-EXEC.                                                    EL652
03019                                                                   EL652
03020      GO TO 9100-RETURN-TRAN.                                      EL652
03021                                                                   EL652
03022  8200-SEND-DATAONLY.                                              EL652
03023                                                                      CL*46
03024      IF  NOT CREDIT-SESSION                                          CL*46
03025          MOVE AL-SADOF           TO PFK5A                            CL*46
03026                                     PFK6A.                           CL*46
03027                                                                      CL*46
03028      IF PI-PROCESSOR-ID = 'LGXX' OR 'PEMA'                        EL652
03029          MOVE AL-UANOF           TO  LSTMDTA                         CL*35
03030          MOVE AL-SANOF           TO  PFK9A                           CL*36
03031      ELSE                                                         EL652
03032          IF PI-AR-PROCESSING                                         CL*54
03033              MOVE AL-SANOF       TO  PFK9A                           CL*54
03034          ELSE                                                        CL*54
03035              MOVE AL-SANOF       TO  LSTMDTA                         CL*54
03036              MOVE AL-SADOF       TO  PFK9A.                          CL*54

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
03037                                                                   EL652
03038      MOVE SAVE-DATE              TO  RUNDATEO.                    EL652
03039      MOVE EIBTIME                TO  TIME-IN.                     EL652
03040      MOVE TIME-OUT               TO  RUNTIMEO.                    EL652
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
03041      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL652
03042                                                                   EL652
03043      EXEC CICS SEND                                               EL652
03044          MAP     (MAP-NAME)                                       EL652
03045          MAPSET  (MAPSET-NAME)                                    EL652
03046          FROM    (EL652AO)                                        EL652
03047          DATAONLY                                                 EL652
03048          CURSOR                                                   EL652
03049      END-EXEC.                                                    EL652
03050                                                                   EL652
03051      GO TO 9100-RETURN-TRAN.                                      EL652
03052                                                                   EL652
03053  8300-SEND-TEXT.                                                  EL652
03054      EXEC CICS SEND TEXT                                          EL652
03055          FROM    (LOGOFF-TEXT)                                    EL652
03056          LENGTH  (LOGOFF-LENGTH)                                  EL652
03057          ERASE                                                    EL652
03058          FREEKB                                                   EL652
03059      END-EXEC.                                                    EL652
03060                                                                   EL652
03061      EXEC CICS RETURN                                             EL652
03062      END-EXEC.                                                    EL652
03063                                                                   EL652
03064  8400-LOG-JOURNAL-RECORD.                                         EL652
03065      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL652
03066      MOVE FILE-ID                TO  JP-FILE-ID.                  EL652
03067      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL652
03068                                                                   EL652
pemuni*    EXEC CICS JOURNAL                                            EL652
pemuni*        JFILEID  (PI-JOURNAL-FILE-ID)                            EL652
pemuni*        JTYPEID  ('EL')                                          EL652
pemuni*        FROM     (JOURNAL-RECORD)                                EL652
pemuni*        LENGTH   (473)                                           EL652
pemuni*    END-EXEC.                                                    EL652
03075                                                                   EL652
03076  8800-UNAUTHORIZED-ACCESS.                                        EL652
03077      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL652
03078                                                                   EL652
03079      GO TO 8300-SEND-TEXT.                                        EL652
03080                                                                   EL652
03081  8810-PF23.                                                       EL652
03082      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL652
03083      MOVE XCTL-005               TO  PGM-NAME.                    EL652
03084                                                                   EL652
03085      GO TO 9300-XCTL.                                             EL652
03086                                                                   EL652
03087  8880-NOT-FOUND.                                                  EL652
03088      MOVE ER-0142                TO  EMI-ERROR.                   EL652
03089      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL652
03090                                                                   EL652
03091      MOVE SPACE                  TO  PI-ERC-TYPE.                    CL*47
03092      MOVE -1                     TO  MAINTYPL.                    EL652
03093                                                                   EL652
03094      IF EIBTRNID NOT = TRANS-ID                                   EL652
03095          GO TO 8100-SEND-INITIAL-MAP.                             EL652
03096                                                                   EL652
03097      GO TO 8200-SEND-DATAONLY.                                    EL652
03098                                                                   EL652
03099  9100-RETURN-TRAN.                                                EL652
03100      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL652
03101      MOVE '652A'                 TO  PI-CURRENT-SCREEN-NO.        EL652
03102                                                                   EL652
03103      EXEC CICS RETURN                                             EL652
03104          TRANSID   (TRANS-ID)                                     EL652
03105          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL652
03106          LENGTH    (PI-COMM-LENGTH)                               EL652
03107      END-EXEC.                                                    EL652
03108                                                                   EL652
03109  9200-RETURN-MAIN-MENU.                                           EL652
03110                                                                      CL*19
03111      IF  MORTGAGE-SESSION                                            CL*19
03112          MOVE XCTL-EM626         TO PGM-NAME                         CL*19
03113      ELSE                                                            CL*19
03114          IF  CREDIT-SESSION                                          CL*19
03115              MOVE XCTL-626       TO PGM-NAME                         CL*19
03116          ELSE                                                        CL*19
03117              MOVE XCTL-126       TO PGM-NAME.                        CL*19
03118                                                                   EL652
03119      GO TO 9300-XCTL.                                             EL652
03120                                                                   EL652
03121  9300-XCTL.                                                       EL652
03122      EXEC CICS XCTL                                               EL652
03123          PROGRAM   (PGM-NAME)                                     EL652
03124          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL652
03125          LENGTH    (PI-COMM-LENGTH)                               EL652
03126      END-EXEC.                                                    EL652
03127                                                                   EL652
03128  9400-CLEAR.                                                      EL652
03129      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL652
03130                                                                   EL652
03131      GO TO 9300-XCTL.                                             EL652
03132                                                                   EL652
03133  9500-PF12.                                                       EL652
03134      MOVE XCTL-010               TO  PGM-NAME.                    EL652
03135                                                                   EL652
03136      GO TO 9300-XCTL.                                             EL652
03137                                                                   EL652
03138  9600-PGMID-ERROR.                                                EL652
03139      EXEC CICS HANDLE CONDITION                                   EL652
03140          PGMIDERR  (8300-SEND-TEXT)                               EL652
03141      END-EXEC.                                                    EL652
03142                                                                   EL652
03143      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL652
03144      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL652
03145      MOVE XCTL-005               TO  PGM-NAME.                    EL652
03146      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL652
03147      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL652
03148                                                                   EL652
03149      GO TO 9300-XCTL.                                             EL652
03150                                                                   EL652
03151  9700-LINK-DATE-CONVERT.                                          EL652
03152      EXEC CICS LINK                                               EL652
03153          PROGRAM   ('ELDATCV')                                    EL652
03154          COMMAREA  (DATE-CONVERSION-DATA)                         EL652
03155          LENGTH    (DC-COMM-LENGTH)                               EL652
03156      END-EXEC.                                                    EL652
03157                                                                   EL652
03158  9700-EXIT.                                                       EL652
03159      EXIT.                                                        EL652
03160                                                                   EL652
03161  9900-ERROR-FORMAT.                                               EL652
03162      IF NOT EMI-ERRORS-COMPLETE                                   EL652
03163          MOVE LINK-001           TO  PGM-NAME                     EL652
03164          EXEC CICS LINK                                           EL652
03165              PROGRAM   (PGM-NAME)                                 EL652
03166              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL652
03167              LENGTH    (EMI-COMM-LENGTH)                          EL652
03168          END-EXEC.                                                EL652
03169                                                                   EL652
03170  9900-EXIT.                                                       EL652
03171      EXIT.                                                           CL*19
03172                                                                      CL*19
03173  9910-INITIALIZE-SECURITY.                                           CL*19
03174 ******************************************************************   CL*19
03175 *                                                                *   CL*19
03176 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *   CL*19
03177 *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *   CL*19
03178 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *   CL*19
03179 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *   CL*19
03180 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *   CL*19
03181 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *   CL*19
03182 *                                                                *   CL*19
03183 ******************************************************************   CL*19
03184                                                                      CL*19
03185      IF  PI-PROCESSOR-ID NOT = 'LGXX'                                CL*19
03186                                                                      CL*19
03187          IF  MORTGAGE-SESSION                                        CL*19
03188              MOVE '125E'             TO SC-QUID-SYSTEM               CL*19
03189              MOVE EIBTRMID           TO SC-QUID-TERMINAL             CL*19
03190                                                                      CL*19
03191              EXEC CICS READQ TS                                      CL*19
03192                  QUEUE  (SC-QUID-KEY)                                CL*19
03193                  INTO   (SECURITY-CONTROL-E)                         CL*19
03194                  LENGTH (SC-COMM-LENGTH-E)                           CL*19
03195                  ITEM   (SC-ITEM)                                    CL*19
03196              END-EXEC                                                CL*19
03197                                                                      CL*19
03198              MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)                   CL*19
03199                                      TO PI-DISPLAY-CAP               CL*19
03200              MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)                    CL*19
03201                                      TO PI-MODIFY-CAP                CL*19
03202                                                                      CL*19
03203              IF  NOT DISPLAY-CAP                                     CL*19
03204                  MOVE 'READ'         TO SM-READ                      CL*19
03205                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT      CL*19
03206                  MOVE ER-9097        TO EMI-ERROR                    CL*19
03207                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*19
03208                  GO TO 8100-SEND-INITIAL-MAP                         CL*19
03209              ELSE                                                    CL*19
03210                  GO TO 9910-EXIT                                     CL*19
03211          ELSE                                                        CL*19
03212              EXEC CICS  READQ TS                                     CL*19
03213                  QUEUE   (PI-SECURITY-TEMP-STORE-ID)                 CL*19
03214                  INTO    (SECURITY-CONTROL)                          CL*19
03215                  LENGTH  (SC-COMM-LENGTH)                            CL*19
03216                  ITEM    (SC-ITEM-CL-CR)                             CL*19
03217                  END-EXEC                                            CL*19
03218                                                                      CL*19
03219              MOVE SC-CREDIT-DISPLAY (05)                             CL*19
03220                                  TO PI-DISPLAY-CAP                   CL*19
03221              MOVE SC-CREDIT-UPDATE  (05)                             CL*19
03222                                  TO PI-MODIFY-CAP                    CL*19
03223                                                                      CL*19
03224              IF  NOT DISPLAY-CAP                                     CL*19
03225                  MOVE 'READ'     TO SM-READ                          CL*19
03226                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT      CL*19
03227                  MOVE ER-0070    TO  EMI-ERROR                       CL*19
03228                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*19
03229                  GO TO 8100-SEND-INITIAL-MAP.                        CL*19
03230                                                                      CL*19
03231  9910-EXIT.                                                          CL*19
03232      EXIT.                                                        EL652
03233                                                                   EL652
03234  9990-ABEND.                                                      EL652
03235      MOVE LINK-004               TO  PGM-NAME.                    EL652
03236      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL652
03237                                                                   EL652
03238      EXEC CICS LINK                                               EL652
03239          PROGRAM   (PGM-NAME)                                     EL652
03240          COMMAREA  (EMI-LINE1)                                    EL652
03241          LENGTH    (72)                                           EL652
03242      END-EXEC.                                                    EL652
03243                                                                   EL652
03244      GO TO 8200-SEND-DATAONLY.                                    EL652
03245                                                                   EL652
03246  9995-SECURITY-VIOLATION.                                         EL652
03247             COPY ELCSCTP.                                         EL652
03248  9995-EXIT.                                                       EL652
03249       EXIT.                                                       EL652
