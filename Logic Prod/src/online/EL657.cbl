00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL657 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/14/96 12:01:06.
00007 *                            VMOD=2.010.
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS.    TRANSACTION - EXH1 - REINSURANCE TABLES TEST CASE.
040208******************************************************************
040208*                   C H A N G E   L O G
040208*
040208* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
040208*-----------------------------------------------------------------
040208*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
040208* EFFECTIVE    NUMBER
040208*-----------------------------------------------------------------
040208* 040208  IR2008032700001  PEMA  FIX LFPRM,AHPRM,LFBEN,AHBEN FLDS
040208******************************************************************
00024
00025  ENVIRONMENT DIVISION.
00026      EJECT
00027  DATA DIVISION.
00028  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00029  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.
00030  77  FILLER  PIC X(32)  VALUE '********************************'.
00031  77  FILLER  PIC X(32)  VALUE '*    EL657 WORKING STORAGE     *'.
00032  77  FILLER   PIC X(32) VALUE '******** VMOD=2.010 ************'.
00033
00034  77  INCR1                           PIC S999    VALUE +1  COMP-3.
00035  77  INCR2                           PIC S999    VALUE +1  COMP-3.
00036
00037  01  WS-WORK-AREA.
00038      05  WS-LF-PREMIUM-AMT           PIC S9(7)V99 VALUE ZEROS.
00039      05  WS-LF-BENEFIT-AMT           PIC S9(9)V99 VALUE ZEROS.
00040
00041  01  WS-DATE-AREA.
00042      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00043      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00044
00045 *                                    COPY ELCREIN.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCREIN.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.006                          *
00006 *                                                                *
00007 *   DESCRIPTION:  CALCULATE REINSURANCE TEST CASE                *
00008 *                                                                *
00009 *  PASSED TO ELREIN                                              *
00010 *  -----------------                                             *
00011 *  TABLE CODE                                                    *
00012 *  EFFECTIVE DATE                                                *
00013 *  ISSUE AGE                                                     *
00014 *  ORIGINAL TERM                                                 *
00015 *  LIFE TYPE                                                     *
00016 *  LIFE PREMIUM                                                  *
00017 *  LIFE BENEFIT                                                  *
00018 *  LIFE REFUND                                                   *
00019 *  A/H  TYPE                                                     *
00020 *  A/H  PREMIUM                                                  *
00021 *  A/H  BENEFIT                                                  *
00022 *  A/H  REFUND                                                   *
00023 *  I/G  CODE                                                     *
00024 *  CLAIM AMOUNT                                                  *
00025 *                                                                *
00026 *  RETURNED FROM ELREIN                                          *
00027 *  --------------------                                          *
00028 *  CEDED TO                                                      *
00029 *  LIFE PREMIUM                                                  *
00030 *  LIFE BENEFIT                                                  *
00031 *  LIFE REFUND                                                   *
00032 *  LIFE CLAIM                                                    *
00033 *  A/H  PREMIUM                                                  *
00034 *  A/H  BENEFIT                                                  *
00035 *  A/H  REFUND                                                   *
00036 *  A/H  CLAIM                                                    *
00037 *                                                                *
00038 ******************************************************************
00039
00040  01  REINSURANCE-PASS-AREA.
00041      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +2868
00042                                      COMP.
00043
00044      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00045        88  NO-CP-ERROR                             VALUE ZERO.
00046        88  CP-ERROR-OCCURED                        VALUE '1'  '2'
00047                                                          '3'  '4'
00048                                                          '5'.
00049        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00050        88  CP-ERROR-IN-DATES                       VALUE '2'.
00051        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00052        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00053        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00054
00055 ***********************  INPUT AREAS ****************************
00056
00057      12  CP-CALCULATION-AREA.
00058          16  CP-TBCOD              PIC X(3).
00059          16  CP-COMPANY-CD         PIC X.
00060          16  CP-COMPANY-ID         PIC X(3).
00061          16  CP-EFF-DT             PIC XX.
00062          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00063                                      COMP-3.
00064          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00065                                      COMP-3.
00066          16  CP-LF-TYPE            PIC XX.
00067          16  CP-LF-PREM            PIC S9(7)V99      VALUE ZERO
00068                                      COMP-3.
00069          16  CP-LF-BEN             PIC S9(9)V99      VALUE ZERO
00070                                      COMP-3.
00071          16  CP-LF-REF             PIC S9(7)V99      VALUE ZERO
00072                                      COMP-3.
00073          16  CP-AH-TYPE            PIC XX.
00074          16  CP-AH-PREM            PIC S9(7)V99      VALUE ZERO
00075                                      COMP-3.
00076          16  CP-AH-BEN             PIC S9(7)V99      VALUE ZERO
00077                                      COMP-3.
00078          16  CP-AH-REF             PIC S9(7)V99      VALUE ZERO
00079                                      COMP-3.
00080          16  CP-CLAIM-AMT          PIC S9(9)V99      VALUE ZERO
00081                                      COMP-3.
00082          16  CP-IG-CODE            PIC X.
00083
00084          16  CP-LFPRM-ALT          PIC S9(07)V99     VALUE +0
00085                                      COMP-3.
00086          16  CP-LFAMT-ALT          PIC S9(09)V99     VALUE +0
00087                                      COMP-3.
00088          16  FILLER                PIC X(59).
00089
00090 ****************** OUTPUT AREA **********************************
00091      12  REIN-WORK-AREAS.
00092          16  RW-LFAMT        PIC S9(9)V99   VALUE +0    COMP-3.
00093          16  RW-AHWRK        PIC S9(7)V99   VALUE +0    COMP-3.
00094          16  RW-LFPRM        PIC S9(7)V99   VALUE +0    COMP-3.
00095          16  RW-LFPRMC       PIC S9(7)V99   VALUE +0    COMP-3.
00096          16  RW-AHPRM        PIC S9(7)V99   VALUE +0    COMP-3.
00097          16  RW-AHPRMC       PIC S9(7)V99   VALUE +0    COMP-3.
00098          16  RW-LFCLMWK      PIC S9(9)V99   VALUE +0    COMP-3.
00099          16  RW-AHCLMWK      PIC S9(9)V99   VALUE +0    COMP-3.
00100          16  RW-LFCLM        PIC S9(9)V99   VALUE +0    COMP-3.
00101          16  RW-AHCLM        PIC S9(9)V99   VALUE +0    COMP-3.
00102          16  RW-AH-LIMIT     PIC S9(7)V99   VALUE +0    COMP-3.
00103          16  RW-LFIBNR       PIC S9(7)V99   VALUE +0    COMP-3.
00104          16  RW-LFPAYCUR     PIC S9(7)V99   VALUE +0    COMP-3.
00105          16  RW-LFFUTRSV     PIC S9(7)V99   VALUE +0    COMP-3.
00106          16  RW-AHIBNR       PIC S9(7)V99   VALUE +0    COMP-3.
00107          16  RW-AHPAYCUR     PIC S9(7)V99   VALUE +0    COMP-3.
00108          16  RW-AHFUTRSV     PIC S9(7)V99   VALUE +0    COMP-3.
00109          16  RW-LFIBNRWK     PIC S9(7)V99   VALUE +0    COMP-3.
00110          16  RW-LFPAYCURWK   PIC S9(7)V99   VALUE +0    COMP-3.
00111          16  RW-LFFUTRSVWK   PIC S9(7)V99   VALUE +0    COMP-3.
00112          16  RW-AHIBNRWK     PIC S9(7)V99   VALUE +0    COMP-3.
00113          16  RW-AHPAYCURWK   PIC S9(7)V99   VALUE +0    COMP-3.
00114          16  RW-AHFUTRSVWK   PIC S9(7)V99   VALUE +0    COMP-3.
PEMMOD         16  RW-AHLIM-LO     PIC S9(9)V99   VALUE +0    COMP-3.
pemmod         16  RW-AHLIM-HI     PIC S9(9)V99   VALUE +0    COMP-3.
00117          16  RW-ACCUM-LF     PIC S9(5)V99   VALUE +0    COMP-3.
00118          16  RW-ACCUM-AH     PIC S9(5)V99   VALUE +0    COMP-3.
00119          16  RW-ACCUM-CLM    PIC S9(5)V99   VALUE +0    COMP-3.
00120          16  RW-ACCUM-IBNR   PIC S9(5)V99   VALUE +0    COMP-3.
00121          16  RW-ACCUM-PAYCUR PIC S9(5)V99   VALUE +0    COMP-3.
00122          16  RW-ACCUM-FUTRSV PIC S9(5)V99   VALUE +0    COMP-3.
00123
00124      12  REIN-HOLD-AREAS.
00125          16  REIN-LEVELS                 OCCURS 30 TIMES.
00126              20  REIN-COMP.
00127                  24  REIN-CO-PRIME       PIC XXX.
00128                  24  REIN-CO-SUB         PIC XXX.
00129              20  REIN-LF-AH-FLGS.
00130                  24  REIN-LF-FLG         PIC X.
00131                  24  REIN-AH-FLG         PIC X.
00132
00133              20  REIN-WORK-FLDS.
00134                  24  REIN-LFAMT          PIC S9(9)V99   COMP-3.
00135                  24  REIN-LFPRM          PIC S9(7)V99   COMP-3.
00136                  24  REIN-AHAMT          PIC S9(7)V99   COMP-3.
00137                  24  REIN-AHPRM          PIC S9(7)V99   COMP-3.
00138                  24  REIN-LFRFND         PIC S9(7)V99   COMP-3.
00139                  24  REIN-AHRFND         PIC S9(7)V99   COMP-3.
00140                  24  REIN-LFCLM          PIC S9(9)V99   COMP-3.
00141                  24  REIN-AHCLM          PIC S9(9)V99   COMP-3.
00142                  24  REIN-LFCLML         PIC S9(9)V99   COMP-3.
00143                  24  REIN-AHCLML         PIC S9(9)V99   COMP-3.
00144                  24  REIN-DIS-IBNR       PIC S9(7)V99   COMP-3.
00145                  24  REIN-DIS-PAYCUR     PIC S9(7)V99   COMP-3.
00146                  24  REIN-DIS-FUTRSV     PIC S9(7)V99   COMP-3.
00147                  24  REIN-DIS-IBNRL      PIC S9(7)V99   COMP-3.
00148                  24  REIN-DIS-PAYCURL    PIC S9(7)V99   COMP-3.
00149                  24  REIN-DIS-FUTRSVL    PIC S9(7)V99   COMP-3.
00150                  24  REIN-DTH-IBNR       PIC S9(7)V99   COMP-3.
00151                  24  REIN-DTH-PAYCUR     PIC S9(7)V99   COMP-3.
00152                  24  REIN-DTH-FUTRSV     PIC S9(7)V99   COMP-3.
00153                  24  REIN-DTH-IBNRL      PIC S9(7)V99   COMP-3.
00154                  24  REIN-DTH-PAYCURL    PIC S9(7)V99   COMP-3.
00155                  24  REIN-DTH-FUTRSVL    PIC S9(7)V99   COMP-3.
00156                  24  REIN-AH-LIMIT       PIC S9(7)V99   COMP-3.
00157                  24  REIN-REM-SW         PIC X.
00158                  24  REIN-REM-AH-100     PIC X.
00159      12  REIN-LEVELS-END                 PIC X(6).
00160
00161      12  RWF-FIELDS.
00162          16  RWF-LFAMT                   PIC S9(9)V99   COMP-3.
00163          16  RWF-LFPRM                   PIC S9(7)V99   COMP-3.
00164          16  RWF-AHAMT                   PIC S9(7)V99   COMP-3.
00165          16  RWF-AHPRM                   PIC S9(7)V99   COMP-3.
00166          16  RWF-LFRFND                  PIC S9(7)V99   COMP-3.
00167          16  RWF-AHRFND                  PIC S9(7)V99   COMP-3.
00168          16  RWF-LFCLM                   PIC S9(9)V99   COMP-3.
00169          16  RWF-AHCLM                   PIC S9(9)V99   COMP-3.
00170          16  RWF-LFCLML                  PIC S9(9)V99   COMP-3.
00171          16  RWF-AHCLML                  PIC S9(9)V99   COMP-3.
00172          16  RWF-DIS-IBNR                PIC S9(7)V99   COMP-3.
00173          16  RWF-DIS-PAYCUR              PIC S9(7)V99   COMP-3.
00174          16  RWF-DIS-FUTRSV              PIC S9(7)V99   COMP-3.
00175          16  RWF-DIS-IBNRL               PIC S9(7)V99   COMP-3.
00176          16  RWF-DIS-PAYCURL             PIC S9(7)V99   COMP-3.
00177          16  RWF-DIS-FUTRSVL             PIC S9(7)V99   COMP-3.
00178          16  RWF-DTH-IBNR                PIC S9(7)V99   COMP-3.
00179          16  RWF-DTH-PAYCUR              PIC S9(7)V99   COMP-3.
00180          16  RWF-DTH-FUTRSV              PIC S9(7)V99   COMP-3.
00181          16  RWF-DTH-IBNRL               PIC S9(7)V99   COMP-3.
00182          16  RWF-DTH-PAYCURL             PIC S9(7)V99   COMP-3.
00183          16  RWF-DTH-FUTRSVL             PIC S9(7)V99   COMP-3.
00184          16  RWF-AH-LIMIT                PIC S9(7)V99   COMP-3.
00185          16  RWF-REM-SW                  PIC X.
00186          16  RWF-REM-AH-100              PIC X.
00187
00188      12  WT-COMP-INFO.
00189          16  WT-REI-COMP.
00190              20  WT-REIN-PRIME           PIC XXX.
00191              20  WT-REIN-SUB             PIC XXX.
00192          16  WT-LF-QC                    PIC X.
00193          16  WT-AH-QC                    PIC X.
00194          16  WT-LO-DATE                  PIC 9(11)  COMP-3.
00195          16  WT-HI-DATE                  PIC 9(11)  COMP-3.
00196          16  WT-LFAGE-LO                 PIC 99.
00197          16  WT-LFAGE-HI                 PIC 99.
00198          16  WT-AHAGE-LO                 PIC 99.
00199          16  WT-AHAGE-HI                 PIC 99.
00200          16  WT-LFTRM-LO                 PIC S9(3)      COMP-3.
00201          16  WT-LFTRM-HI                 PIC S9(3)      COMP-3.
00202          16  WT-AHTRM-LO                 PIC S9(3)      COMP-3.
00203          16  WT-AHTRM-HI                 PIC S9(3)      COMP-3.
00204          16  WT-LF-PCT                   PIC S9V9999    COMP-3.
00205          16  WT-AH-PCT                   PIC S9V9999    COMP-3.
00206          16  WT-LF-LIM-LO                PIC S9(9)V99   COMP-3.
00207          16  WT-LF-LIM-HI                PIC S9(9)V99   COMP-3.
00208          16  WT-LF-LO                    PIC S9(9)V99   COMP-3.
00209          16  WT-LF-HI                    PIC S9(9)V99   COMP-3.
00210          16  WT-AHBEN-LIM-LO             PIC S9(7)V99   COMP-3.
00211          16  WT-AHBEN-LIM-HI             PIC S9(7)V99   COMP-3.
00212          16  WT-AHBEN-LO                 PIC S9(7)V99   COMP-3.
00213          16  WT-AHBEN-HI                 PIC S9(7)V99   COMP-3.
00214          16  WT-AHMOA-LIM-LO             PIC S9(7)V99   COMP-3.
00215          16  WT-AHMOA-LIM-HI             PIC S9(7)V99   COMP-3.
00216          16  WT-AHMOA-LO                 PIC S9(7)V99   COMP-3.
00217          16  WT-AHMOA-HI                 PIC S9(7)V99   COMP-3.
00218          16  WT-LF-BEN-CODE              PIC X.
00219          16  WT-AH-BEN-CODE              PIC X.
00220          16  WT-INTERACTIVE              PIC X.
00221          16  WT-REMAINING                PIC X.
00222
00223      12  REIN-MISC-WORK-FIELDS.
00224          16  SUB1                    PIC S9(3)      COMP.
00225          16  SUB2                    PIC S9(3)      COMP.
00226          16  CO-SUB                  PIC S9(3)      COMP.
00227          16  REIN-FACTOR             PIC S99V9(7) COMP-3 VALUE +0.
00228          16  REIN-CALCED-LIFE        PIC S9(7)V99 COMP-3 VALUE +0.
00229          16  REIN-CALCED-AH          PIC S9(7)V99 COMP-3 VALUE +0.
00230
00231          16  REIN-SW-DTE             PIC  9(11)  COMP-3.
00232          16  REIN-OPEN-SW            PIC  X          VALUE SPACE.
00233          16  REIN-SEARCH.
00234              20  REIN-SRCH-CODE      PIC  X          VALUE SPACE.
00235              20  REIN-SRCH           PIC  XXX        VALUE SPACE.
00236          16  SAVE-REIN-SRCH          PIC  X(4)       VALUE SPACE.
00237          16  REIN-BUS-FLAG           PIC  X          VALUE SPACE.
00238          16  RATE-WORK               PIC S9(8)V9(5) COMP-3.
00239          16  RATE-WORK-L             PIC S9(8)V9(5) COMP-3.
00240          16  RATE-WORK-A             PIC S9(8)V9(5) COMP-3.
00241          16  REIN-WRK                PIC S9(7)V99   COMP-3.
00242          16  REIN-WRK-2              PIC S9(7)V99   COMP-3.
00243          16  RS-LIFE-BEN       PIC S9(9)V99   COMP-3    VALUE +0.
00244          16  RS-LIFE-PREM      PIC S9(7)V99   COMP-3    VALUE +0.
00245          16  RS-AH-BEN         PIC S9(7)V99   COMP-3    VALUE +0.
00246          16  RS-AH-PREM        PIC S9(7)V99   COMP-3    VALUE +0.
00247          16  RS-LP-CALC        PIC S9(7)V99   COMP-3    VALUE +0.
00248          16  RS-AP-CALC        PIC S9(7)V99   COMP-3    VALUE +0.
00249          16  RS-R-LB           PIC S9(9)V99   COMP-3    VALUE +0.
00250          16  RS-R-LP           PIC S9(7)V99   COMP-3    VALUE +0.
00251          16  RS-R-AB           PIC S9(9)V99   COMP-3    VALUE +0.
00252          16  RS-R-AP           PIC S9(7)V99   COMP-3    VALUE +0.
00253          16  RS-R-LPC          PIC S9(7)V99   COMP-3    VALUE +0.
00254          16  RS-R-APC          PIC S9(7)V99   COMP-3    VALUE +0.
00255
00256      12  REIN-CO-HOLD-TABLE.
00257          16  REIN-CO-HOLD-ENTRIES  OCCURS 200 TIMES.
00258              20  RCT-REIN-CO.
00259                  24  RCT-REIN-PRIME   PIC XXX.
00260                  24  RCT-REIN-SUB     PIC XXX.
00261              20  RCT-CLM-CUTOFF-DT    PIC 9(11)  COMP-3.
00262              20  RCT-LF-CLM-PCT       PIC S9V9(4)    COMP-3.
00263              20  RCT-LF-CLM-MAX       PIC S9(7)V99   COMP-3.
00264              20  RCT-AH-CLM-PCT       PIC S9V9(4)    COMP-3.
00265              20  RCT-AH-CLM-MAX       PIC S9(7)V99   COMP-3.
00266
00267      12  REIN-CO-TABLE-ENT-CNT     PIC S9(3) COMP-3    VALUE +200.
00268 ******************************************************************
00046
00047      EJECT
00048  01  STANDARD-AREAS.
00049      12  MAP-NAME                    PIC X(8)    VALUE 'EL657A'.
00050      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL657S'.
00051      12  TRANS-ID                    PIC X(4)    VALUE 'EXH1'.
00052      12  PGM-NAME                    PIC X(8)    VALUE SPACES.
00053      12  THIS-PGM                    PIC X(8)    VALUE 'EL657'.
00054      12  LINK-ELREIN                 PIC X(8)    VALUE 'ELREIN'.
00055      12  CERT-FILE-ID                PIC X(8)    VALUE 'ELCERT'.
00056      12  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.
00057      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00058      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00059      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00060      12  LINK-005                    PIC X(8)    VALUE 'EL005'.
00061      12  LINK-010                    PIC X(8)    VALUE 'EL010'.
00062      12  LINK-126                    PIC X(8)    VALUE 'EL126'.
00063      12  TIME-IN                     PIC S9(7).
00064      12  TIME-OUT-R   REDEFINES TIME-IN.
00065          16  FILLER                  PIC X.
00066          16  TIME-OUT                PIC 99V99.
00067          16  FILLER                  PIC XX.
00068      12  REIN-LENGTH                 PIC S9(4)   VALUE +4000 COMP.
00069      12  ITEM-VALUE                  PIC S9(4)   VALUE +1    COMP.
00070      12  QID.
00071          16  QID-TERM                PIC X(4).
00072          16  FILLER                  PIC X(4)    VALUE '657A'.
00073
00074  01  BLD-LINE.
00075      12  FILLER                      PIC X       VALUE SPACE.
00076      12  BL-CEDED                    PIC XXX.
00077      12  FILLER                      PIC X       VALUE SPACE.
00078      12  BL-CEDED-SUB                PIC XXX.
00079      12  FILLER                      PIC X(6)    VALUE SPACE.
00080      12  BL-LF-PREMIUM               PIC ZZZZ,ZZZ.ZZ.
00081      12  FILLER                      PIC XXX     VALUE SPACE.
00082      12  BL-LF-BENEFIT               PIC ZZZ,ZZZ,ZZZ.ZZ.
00083      12  FILLER                      PIC X(7)    VALUE SPACE.
00084      12  BL-AH-PREMIUM               PIC ZZZZ,ZZZ.ZZ.
00085      12  FILLER                      PIC XXX     VALUE SPACE.
00086      12  BL-AH-BENEFIT               PIC ZZZZ,ZZZ.ZZ.
00087      12  FILLER                      PIC X(5).
00088
00089  01  SAVE-REIN-WORK.
00090      12  REIN-SUB-1                  PIC S9(4)   VALUE +0   COMP.
00091      12  REIN-SUB-2                  PIC S9(4)   VALUE +0   COMP.
00092      12  REIN-MAX                    PIC S9(4)   VALUE +30  COMP.
00093      12  REIN-LOW-TIME               PIC 9(6)    VALUE  999999.
00094      12  REIN-LOW-SUB                PIC S9(4)   VALUE +1   COMP.
00095
00096      EJECT
00097  01  ERROR-MESSAGES.
00098      12  ER-0008                     PIC X(4)    VALUE '0008'.
00099      12  ER-0029                     PIC X(4)    VALUE '0029'.
00100      12  ER-0130                     PIC X(4)    VALUE '0130'.
00101      12  ER-0131                     PIC X(4)    VALUE '0131'.
00102      12  ER-0195                     PIC X(4)    VALUE '0195'.
00103      12  ER-0196                     PIC X(4)    VALUE '0196'.
00104      12  ER-0197                     PIC X(4)    VALUE '0197'.
00105      12  ER-0203                     PIC X(4)    VALUE '0203'.
00106      12  ER-0244                     PIC X(4)    VALUE '0244'.
00107      12  ER-2203                     PIC X(4)    VALUE '2203'.
00108      12  ER-2205                     PIC X(4)    VALUE '2205'.
00109      12  ER-2223                     PIC X(4)    VALUE '2223'.
00110      12  ER-2301                     PIC X(4)    VALUE '2301'.
00111      12  ER-2341                     PIC X(4)    VALUE '2341'.
00112      12  ER-2425                     PIC X(4)    VALUE '2425'.
00113      12  ER-2429                     PIC X(4)    VALUE '2429'.
00114      12  ER-2473                     PIC X(4)    VALUE '2473'.
00115      12  ER-2474                     PIC X(4)    VALUE '2474'.
00116      12  ER-2482                     PIC X(4)    VALUE '2482'.
00117      12  ER-2484                     PIC X(4)    VALUE '2484'.
00118      12  ER-2485                     PIC X(4)    VALUE '2485'.
00119      12  ER-2489                     PIC X(4)    VALUE '2489'.
00120      12  ER-2504                     PIC X(4)    VALUE '2504'.
00121      12  ER-2505                     PIC X(4)    VALUE '2505'.
00122      12  ER-2615                     PIC X(4)    VALUE '2615'.
00123
00124      EJECT
00125  01  MISC-WORK-AREAS.
00126      12  WS-CONTROL-PRIMARY.
00127        16  WS-COMPANY-CD             PIC X.
00128        16  WS-CARRIER                PIC X.
00129        16  WS-GROUPING               PIC X(6).
00130        16  WS-STATE                  PIC XX.
00131        16  WS-ACCOUNT                PIC X(10).
00132        16  WS-CERT-EFF-DT            PIC XX.
00133        16  WS-CERT-NO                PIC X(11).
00134
00135      12  WS-COMPANY-CODE             PIC S9(4)   VALUE +0   COMP.
00136      12  WS-COMPANY-CODE-R      REDEFINES WS-COMPANY-CODE.
00137          16  FILLER                  PIC X.
00138          16  WS-COMP-CD              PIC X.
00139
00140      12  TEXT-AREA                   PIC X(66).
00141      12  TEXT-LENGTH                 PIC S9(4)   VALUE +66  COMP.
00142
00143  01  TEXT-MESSAGES.
00144      12  TRAN-COMPLETE-MSG.
00145          16  FILLER                  PIC X(45)
00146            VALUE '     CLEAR ENTERED - SESSION ENDED'.
00147      EJECT
00148 *                                    COPY ELCDATE.
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
00149      EJECT
00150 *                                    COPY ELCATTR.
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
00151      EJECT
00152 *                                    COPY ELCEMIB.
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
00070      12  FILLER                      PIC X(137)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00153      EJECT
00154 *                                    COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00155
00156  01  FILLER REDEFINES DFHAID.
00157      05  FILLER                      PIC X(8).
00158      05  PF-VALUES                   PIC X  OCCURS 24.
00159
00160      EJECT
00161 *                                    COPY ELCLOGOF.
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
00162      EJECT
00163 *                                    COPY EL657S.
       01  EL657AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  TBCODL PIC S9(0004) COMP.
           05  TBCODF PIC  X(0001).
           05  FILLER REDEFINES TBCODF.
               10  TBCODA PIC  X(0001).
           05  TBCODI PIC  X(0003).
      *    -------------------------------
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0006).
      *    -------------------------------
           05  LFHDG1L PIC S9(0004) COMP.
           05  LFHDG1F PIC  X(0001).
           05  FILLER REDEFINES LFHDG1F.
               10  LFHDG1A PIC  X(0001).
           05  LFHDG1I PIC  X(0002).
      *    -------------------------------
           05  LFTYPL PIC S9(0004) COMP.
           05  LFTYPF PIC  X(0001).
           05  FILLER REDEFINES LFTYPF.
               10  LFTYPA PIC  X(0001).
           05  LFTYPI PIC  X(0002).
      *    -------------------------------
           05  LFHDG2L PIC S9(0004) COMP.
           05  LFHDG2F PIC  X(0001).
           05  FILLER REDEFINES LFHDG2F.
               10  LFHDG2A PIC  X(0001).
           05  LFHDG2I PIC  X(0002).
      *    -------------------------------
           05  LFPRML PIC S9(0004) COMP.
           05  LFPRMF PIC  X(0001).
           05  FILLER REDEFINES LFPRMF.
               10  LFPRMA PIC  X(0001).
           05  LFPRMI PIC  9(9)V99.
      *    -------------------------------
           05  LFHDG3L PIC S9(0004) COMP.
           05  LFHDG3F PIC  X(0001).
           05  FILLER REDEFINES LFHDG3F.
               10  LFHDG3A PIC  X(0001).
           05  LFHDG3I PIC  X(0002).
      *    -------------------------------
           05  LFBENL PIC S9(0004) COMP.
           05  LFBENF PIC  X(0001).
           05  FILLER REDEFINES LFBENF.
               10  LFBENA PIC  X(0001).
           05  LFBENI PIC  9(12)V99.
      *    -------------------------------
           05  ORIGTRML PIC S9(0004) COMP.
           05  ORIGTRMF PIC  X(0001).
           05  FILLER REDEFINES ORIGTRMF.
               10  ORIGTRMA PIC  X(0001).
           05  ORIGTRMI PIC  9(3).
      *    -------------------------------
           05  IGCODEL PIC S9(0004) COMP.
           05  IGCODEF PIC  X(0001).
           05  FILLER REDEFINES IGCODEF.
               10  IGCODEA PIC  X(0001).
           05  IGCODEI PIC  X(0001).
      *    -------------------------------
           05  ISSAGEL PIC S9(0004) COMP.
           05  ISSAGEF PIC  X(0001).
           05  FILLER REDEFINES ISSAGEF.
               10  ISSAGEA PIC  X(0001).
           05  ISSAGEI PIC  X(0002).
      *    -------------------------------
           05  AHHDG1L PIC S9(0004) COMP.
           05  AHHDG1F PIC  X(0001).
           05  FILLER REDEFINES AHHDG1F.
               10  AHHDG1A PIC  X(0001).
           05  AHHDG1I PIC  X(0002).
      *    -------------------------------
           05  AHTYPL PIC S9(0004) COMP.
           05  AHTYPF PIC  X(0001).
           05  FILLER REDEFINES AHTYPF.
               10  AHTYPA PIC  X(0001).
           05  AHTYPI PIC  X(0002).
      *    -------------------------------
           05  AHHDG2L PIC S9(0004) COMP.
           05  AHHDG2F PIC  X(0001).
           05  FILLER REDEFINES AHHDG2F.
               10  AHHDG2A PIC  X(0001).
           05  AHHDG2I PIC  X(0002).
      *    -------------------------------
           05  AHPRML PIC S9(0004) COMP.
           05  AHPRMF PIC  X(0001).
           05  FILLER REDEFINES AHPRMF.
               10  AHPRMA PIC  X(0001).
           05  AHPRMI PIC  9(9)V99.
      *    -------------------------------
           05  AHHDG3L PIC S9(0004) COMP.
           05  AHHDG3F PIC  X(0001).
           05  FILLER REDEFINES AHHDG3F.
               10  AHHDG3A PIC  X(0001).
           05  AHHDG3I PIC  X(0002).
      *    -------------------------------
           05  AHBENL PIC S9(0004) COMP.
           05  AHBENF PIC  X(0001).
           05  FILLER REDEFINES AHBENF.
               10  AHBENA PIC  X(0001).
           05  AHBENI PIC  9(9)V99.
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  CERTL PIC S9(0004) COMP.
           05  CERTF PIC  X(0001).
           05  FILLER REDEFINES CERTF.
               10  CERTA PIC  X(0001).
           05  CERTI PIC  X(0011).
      *    -------------------------------
           05  CEFDTL PIC S9(0004) COMP.
           05  CEFDTF PIC  X(0001).
           05  FILLER REDEFINES CEFDTF.
               10  CEFDTA PIC  X(0001).
           05  CEFDTI PIC  X(0006).
      *    -------------------------------
           05  LFHDG4L PIC S9(0004) COMP.
           05  LFHDG4F PIC  X(0001).
           05  FILLER REDEFINES LFHDG4F.
               10  LFHDG4A PIC  X(0001).
           05  LFHDG4I PIC  X(0002).
      *    -------------------------------
           05  LFHDG5L PIC S9(0004) COMP.
           05  LFHDG5F PIC  X(0001).
           05  FILLER REDEFINES LFHDG5F.
               10  LFHDG5A PIC  X(0001).
           05  LFHDG5I PIC  X(0002).
      *    -------------------------------
           05  AHHDG4L PIC S9(0004) COMP.
           05  AHHDG4F PIC  X(0001).
           05  FILLER REDEFINES AHHDG4F.
               10  AHHDG4A PIC  X(0001).
           05  AHHDG4I PIC  X(0002).
      *    -------------------------------
           05  AHHDG5L PIC S9(0004) COMP.
           05  AHHDG5F PIC  X(0001).
           05  FILLER REDEFINES AHHDG5F.
               10  AHHDG5A PIC  X(0001).
           05  AHHDG5I PIC  X(0002).
           05  REPLINED OCCURS 10  TIMES.
      *    -------------------------------
               10  REPLINEL PIC S9(0004) COMP.
               10  REPLINEF PIC  X(0001).
               10  FILLER REDEFINES REPLINEF.
                   15  REPLINEA PIC  X(0001).
               10  REPLINEI PIC  X(0079).
      *    -------------------------------
           05  MSGL PIC S9(0004) COMP.
           05  MSGF PIC  X(0001).
           05  FILLER REDEFINES MSGF.
               10  MSGA PIC  X(0001).
           05  MSGI PIC  X(0079).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  99.
       01  EL657AO REDEFINES EL657AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TBCODO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHDG1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTYPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHDG2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPRMO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHDG3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFBENO PIC  ZZZ,ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ORIGTRMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IGCODEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ISSAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHDG1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTYPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHDG2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPRMO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHDG3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHBENO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFDTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHDG4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHDG5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHDG4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHHDG5O PIC  X(0002).
      *    -------------------------------
           05  REPLINED OCCURS 10  TIMES.
               10  FILLER        PIC  X(0003).
               10  REPLINEO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  99.
      *    -------------------------------
00164      EJECT
00165 *                                    COPY ELCINTF.
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
00131      12  FILLER                          PIC X(4).
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
00166      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00167          16  FILLER                  PIC X(4).
00168          16  PI-TBCOD                PIC XXX.
00169          16  FILLER                  PIC XXX.
00170          16  PI-SAVE-INCR1           PIC S999    COMP-3.
00171          16  PI-FIRST-SW             PIC X.
00172          16  FILLER                  PIC X(97).
00173          16  PI-SAVE-SW              PIC X.
00174          16  PI-SAVE-TBCOD           PIC XXX.
00175          16  PI-SAVE-EFFDT           PIC X(6).
00176          16  PI-SAVE-AGE             PIC 99.
00177          16  PI-SAVE-TERM            PIC 999.
00178          16  PI-SAVE-LFTYP           PIC XX.
00179          16  PI-SAVE-LFPRM           PIC 9(7)V99.
00180          16  PI-SAVE-LFBEN           PIC 9(9)V99.
00181          16  PI-SAVE-AHTYP           PIC XX.
00182          16  PI-SAVE-AHPRM           PIC 9(7)V99.
00183          16  PI-SAVE-AHBEN           PIC 9(7)V99.
00184          16  PI-SAVE-IGCOD           PIC X.
00185          16  PI-SAVE-CARR            PIC X.
00186          16  PI-SAVE-GRP             PIC X(6).
00187          16  PI-SAVE-STATE           PIC XX.
00188          16  PI-SAVE-ACCT            PIC X(10).
00189          16  PI-SAVE-CERT            PIC X(11).
00190          16  PI-SAVE-CEFDT           PIC X(6).
00191          16  FILLER                  PIC X(436).
00192
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
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
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
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
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00194  01  DFHCOMMAREA                     PIC X(1024).
00195 *01 PARMLIST    COMP.
00196 *    02  FILLER                      PIC S9(8).
00197 *    02  ELCERT-POINTER              PIC S9(8).
00198
00199 *                                    COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
00108          16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00125          16  CM-AH-POLICY-FEE              PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
072308         16  CM-NH-INTERFACE-SW            PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
072308     12  CM-NH-INT-ON-REFS                 PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
00286 ******************************************************************
00200      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                CERTIFICATE-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL657' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00202
00203      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00204      MOVE '5'                    TO  DC-OPTION-CODE.
00205      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00206      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00207      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00208
00209      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00210
00211      MOVE EIBTRMID               TO  QID-TERM.
00212
00213      IF EIBCALEN = 0
00214          GO TO 8800-UNAUTHORIZED-ACCESS.
00215
00216      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00217          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00218              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00219              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00220              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00221              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00222              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00223              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00224              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00225              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM.
00226
00227      IF EIBTRNID  = TRANS-ID
00228         GO TO SAME-TRAN.
00229
00230      MOVE LOW-VALUES             TO  EL657AI.
00231
00232      IF PI-TBCOD NOT = PI-SAVE-TBCOD
00233          MOVE PI-TBCOD           TO  TBCODO
00234          MOVE AL-UANON           TO  TBCODA
00235          GO TO 8100-SEND-INITIAL-MAP.
00236
00237      IF PI-SAVE-SW = 'X'
00238          MOVE PI-SAVE-TBCOD      TO  TBCODO
00239          MOVE PI-SAVE-EFFDT      TO  EFFDTO
00240          MOVE PI-SAVE-AGE        TO  ISSAGEO
00241          MOVE PI-SAVE-TERM       TO  ORIGTRMO
00242          MOVE PI-SAVE-LFTYP      TO  LFTYPO
00243          MOVE PI-SAVE-LFPRM      TO  LFPRMO
00244          MOVE PI-SAVE-LFBEN      TO  LFBENO
00245          MOVE PI-SAVE-AHTYP      TO  AHTYPO
00246          MOVE PI-SAVE-AHPRM      TO  AHPRMO
00247          MOVE PI-SAVE-AHBEN      TO  AHBENO
00248          MOVE PI-SAVE-IGCOD      TO  IGCODEO
00249          MOVE PI-SAVE-CARR       TO  CARRO
00250          MOVE PI-SAVE-GRP        TO  GROUPO
00251          MOVE PI-SAVE-STATE      TO  STATEO
00252          MOVE PI-SAVE-ACCT       TO  ACCTO
00253          MOVE PI-SAVE-CERT       TO  CERTO
00254          MOVE PI-SAVE-CEFDT      TO  CEFDTO
00255          MOVE AL-UNNON           TO  ISSAGEA ORIGTRMA
00256                                      LFPRMA  AHPRMA   EFFDTA
00257                                      LFBENA  AHBENA
00258          MOVE AL-UANON           TO  TBCODA  LFTYPA
00259                                      AHTYPA  IGCODEA.
00260
00261      GO TO 8100-SEND-INITIAL-MAP.
00262
00263  SAME-TRAN.
00264      IF EIBAID = DFHCLEAR
00265          GO TO 9400-CLEAR.
00266
00267      
      * EXEC CICS HANDLE CONDITION
00268 *        MAPFAIL (8100-SEND-INITIAL-MAP)
00269 *    END-EXEC.
      *    MOVE '"$?                   ! " #00001916' TO DFHEIV0
           MOVE X'22243F202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00270
00271      
      * EXEC CICS RECEIVE
00272 *        MAP    (MAP-NAME)
00273 *        MAPSET (MAPSET-NAME)
00274 *        INTO   (EL657AI)
00275 *    END-EXEC.
           MOVE LENGTH OF
            EL657AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001920' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL657AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00276
00277      EJECT
00278
00279      IF PFKEYL = 0
00280          GO TO 0200-CHECK-PFKEYS.
00281
00282      IF EIBAID NOT = DFHENTER
00283          MOVE ER-0008            TO  EMI-ERROR
00284          MOVE -1                 TO  PFKEYL
00285          GO TO 0320-INPUT-ERROR.
00286
00287      IF PFKEYI GREATER 0 AND LESS 25
00288              MOVE PF-VALUES (PFKEYI)    TO  EIBAID
00289      ELSE
00290          MOVE ER-0029                    TO  EMI-ERROR
00291          GO TO 0320-INPUT-ERROR.
00292
00293  0200-CHECK-PFKEYS.
00294      IF EIBAID = DFHPF12
00295          MOVE LINK-010           TO  PGM-NAME
00296          GO TO 9300-XCTL.
00297
00298      IF EIBAID = DFHPF23
00299          GO TO 9000-RETURN-CICS.
00300
00301      IF EIBAID = DFHPF24
00302          MOVE LINK-126           TO  PGM-NAME
00303          GO TO 9300-XCTL.
00304
00305      IF (EIBAID = DFHPF1 OR DFHPF2)  AND
00306         (PI-FIRST-SW NOT = 'X')
00307          GO TO 0320-INPUT-ERROR
00308        ELSE
00309          PERFORM 7200-RECOVER-TEMP-STORAGE THRU 7200-EXIT.
00310
00311      IF EIBAID = DFHPF1
00312          GO TO 6000-PAGE-FORWARD.
00313      IF EIBAID = DFHPF2
00314          GO TO 7000-PAGE-BACKWARD.
00315
00316      IF EIBAID = DFHENTER
00317          GO TO 1000-EDIT-INPUT.
00318
00319  0320-INPUT-ERROR.
00320      MOVE ER-0029                TO  EMI-ERROR.
00321      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00322      MOVE -1                     TO  TBCODL.
00323      GO TO 8200-SEND-DATAONLY.
00324      EJECT
00325
00326  1000-EDIT-INPUT.
00327      IF CARRL NOT = ZEROS
00328          GO TO 2000-EDIT-CERT.
00329
00330 *******************************TABLE CODE
00331      IF TBCODL NOT = ZEROS
00332          MOVE TBCODI             TO  CP-TBCOD PI-SAVE-TBCOD
00333                                               PI-TBCOD
00334          MOVE AL-UANON           TO  TBCODA
00335      ELSE
00336          MOVE ER-2341            TO  EMI-ERROR
00337          MOVE -1                 TO  TBCODL
00338          MOVE AL-UABON           TO  TBCODA
00339          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00340
00341 *****************************EFFECTIVE DATE
00342      IF EFFDTL NOT = ZEROS
00343          IF EFFDTI NUMERIC
00344              MOVE EFFDTI         TO  DC-GREG-DATE-1-MDY
00345                                      PI-SAVE-EFFDT
00346              MOVE 4              TO  DC-OPTION-CODE
00347              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00348              IF NO-CONVERSION-ERROR
00349                  MOVE DC-BIN-DATE-1  TO  CP-EFF-DT
00350                  MOVE AL-UNNON   TO  EFFDTA
00351              ELSE
00352                  MOVE ER-2474    TO  EMI-ERROR
00353                  MOVE -1         TO  EFFDTL
00354                  MOVE AL-UNBON   TO  EFFDTA
00355                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00356          ELSE
00357              MOVE ER-2482        TO  EMI-ERROR
00358              MOVE -1             TO  EFFDTL
00359              MOVE AL-UNBON       TO  EFFDTA
00360              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00361      ELSE
00362          MOVE ER-2473            TO  EMI-ERROR
00363          MOVE -1                 TO  EFFDTL
00364          MOVE AL-UNBON           TO  EFFDTA
00365          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00366
00367 *****************************ISSUE AGE
00368      IF ISSAGEL NOT = ZEROS
00369          IF ISSAGEI NUMERIC
00370              MOVE ISSAGEI        TO  CP-ISSUE-AGE PI-SAVE-AGE
00371                                      ISSAGEO
00372              MOVE AL-UNNON       TO  ISSAGEA
00373          ELSE
00374              MOVE ER-2505        TO  EMI-ERROR
00375              MOVE -1             TO  ISSAGEL
00376              MOVE AL-UNBON       TO  ISSAGEA
00377              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00378        ELSE
00379           MOVE ZEROS             TO  CP-ISSUE-AGE PI-SAVE-AGE.
00380
00381 *******************************ORIGINAL TERM
00382      
      * EXEC CICS BIF DEEDIT
00383 *        FIELD  (ORIGTRMI)
00384 *        LENGTH (3)
00385 *    END-EXEC.
           MOVE 3
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002031' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ORIGTRMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00386
00387      IF ORIGTRML NOT = ZEROS
00388          IF ORIGTRMI NUMERIC
00389              MOVE ORIGTRMI       TO  CP-ORIGINAL-TERM
00390                                      PI-SAVE-TERM
00391                                      ORIGTRMO
00392              MOVE AL-UANON       TO  ORIGTRMA
00393          ELSE
00394              MOVE ER-2504        TO  EMI-ERROR
00395              MOVE -1             TO  ORIGTRML
00396              MOVE AL-UABON       TO  ORIGTRMA
00397              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00398       ELSE
00399           MOVE ZEROS             TO  CP-ORIGINAL-TERM
00400                                      ORIGTRMO
00401                                      PI-SAVE-TERM.
00402
00403 *******************************LF TYPE
00404      IF LFTYPL NOT = ZEROS
00405          MOVE LFTYPI             TO  CP-LF-TYPE  PI-SAVE-LFTYP
00406          MOVE AL-UANON           TO  LFTYPA
00407       ELSE
00408         MOVE ZEROS               TO  CP-LF-TYPE PI-SAVE-LFTYP.
00409
00410 ******************************LF PREMIUM
00411      
      * EXEC CICS BIF DEEDIT
00412 *        FIELD  (LFPRMI)
00413 *        LENGTH (11)
00414 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002060' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LFPRMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00415
00416        IF LFPRML NOT = ZEROS
00417          IF LFPRMI NUMERIC
00418              MOVE LFPRMI         TO  CP-LF-PREM  PI-SAVE-LFPRM
040208             MOVE PI-SAVE-LFPRM  TO  LFPRMO
00420              MOVE AL-UNNON       TO  LFPRMA
00421          ELSE
00422              MOVE ER-2484        TO  EMI-ERROR
00423              MOVE -1             TO  LFPRML
00424              MOVE AL-UNBON       TO  LFPRMA
00425              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00426        ELSE
00427           MOVE ZEROS             TO  CP-LF-PREM PI-SAVE-LFPRM
00428                                      LFPRMO.
00429
00430 ******************************LF BENEFIT
00431      
      * EXEC CICS BIF DEEDIT
00432 *        FIELD  (LFBENI)
00433 *        LENGTH (14)
00434 *    END-EXEC.
           MOVE 14
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002080' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LFBENI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00435
00436      IF LFBENL NOT = ZEROS
00437          IF LFBENI NUMERIC
00438              MOVE LFBENI         TO  CP-LF-BEN PI-SAVE-LFBEN
040208             MOVE PI-SAVE-LFBEN  TO  LFBENO
00440              MOVE AL-UNNON       TO  LFBENA
00441          ELSE
00442              MOVE ER-2485        TO  EMI-ERROR
00443              MOVE -1             TO  LFBENL
00444              MOVE AL-UNBON       TO  LFBENA
00445              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00446        ELSE
00447           MOVE ZEROS             TO  CP-LF-BEN PI-SAVE-LFBEN
00448                                      LFBENO.
00449
00450 *******************************AH TYPE
00451      IF AHTYPL NOT = ZEROS
00452          MOVE AHTYPI             TO  CP-AH-TYPE  PI-SAVE-AHTYP
00453          MOVE AL-UANON           TO  AHTYPA
00454       ELSE
00455        MOVE ZERO                 TO  CP-AH-TYPE PI-SAVE-AHTYP.
00456
00457 ******************************AH PREMIUM
00458      
      * EXEC CICS BIF DEEDIT
00459 *        FIELD  (AHPRMI)
00460 *        LENGTH (11)
00461 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002107' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AHPRMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00462
00463      IF AHPRML NOT = ZEROS
00464          IF AHPRMI NUMERIC
00465              MOVE AHPRMI         TO  CP-AH-PREM  PI-SAVE-AHPRM
040208             MOVE PI-SAVE-AHPRM  TO  AHPRMO
00467              MOVE AL-UNNON       TO  AHPRMA
00468          ELSE
00469              MOVE ER-2484        TO  EMI-ERROR
00470              MOVE -1             TO  AHPRML
00471              MOVE AL-UNBON       TO  AHPRMA
00472              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00473        ELSE
00474            MOVE ZEROS            TO  CP-AH-PREM PI-SAVE-AHPRM
00475                                      AHPRMO.
00476
00477 ******************************AH BENEFIT
00478      
      * EXEC CICS BIF DEEDIT
00479 *        FIELD  (AHBENI)
00480 *        LENGTH (11)
00481 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002127' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AHBENI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00482
00483      IF AHBENL NOT = ZEROS
00484          IF AHBENI NUMERIC
00485              MOVE AHBENI         TO  CP-AH-BEN  PI-SAVE-AHBEN
040208             MOVE PI-SAVE-AHBEN  TO  AHBENO
00487              MOVE AL-UNNON       TO  AHBENA
00488          ELSE
00489              MOVE ER-2485        TO  EMI-ERROR
00490              MOVE -1             TO  AHBENL
00491              MOVE AL-UNBON       TO  AHBENA
00492              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00493        ELSE
00494            MOVE ZEROS            TO  CP-AH-BEN PI-SAVE-AHBEN
00495                                      AHBENO.
00496
00497 *******************************I/G
00498      IF IGCODEI = 'I' OR 'G'
00499          MOVE IGCODEI            TO  CP-IG-CODE  PI-SAVE-IGCOD
00500          MOVE AL-UANON           TO  IGCODEA
00501      ELSE
00502          MOVE SPACE              TO  CP-IG-CODE  PI-SAVE-IGCOD
00503                                      IGCODEO
00504          MOVE AL-UANON           TO  IGCODEA.
00505
00506      GO TO 3000-PERFORM-CALC-ROUTINE.
00507
00508  2000-EDIT-CERT.
00509 *******************************CARRIER
00510      IF CARRI NOT = SPACES
00511          MOVE CARRI              TO  WS-CARRIER PI-SAVE-CARR
00512          MOVE AL-UANON           TO  CARRA
00513      ELSE
00514          MOVE ZEROS              TO  CARRL
00515          MOVE SPACES             TO  GROUPI STATEI CERTI
00516                                      ACCTI CEFDTI
00517          GO TO 1000-EDIT-INPUT.
00518
00519 *******************************GROUPING
00520      IF GROUPI NOT = SPACES
00521          MOVE GROUPI             TO  WS-GROUPING  PI-SAVE-GRP
00522          MOVE AL-UANON           TO  GROUPA
00523      ELSE
00524          MOVE ER-0195            TO  EMI-ERROR
00525          MOVE -1                 TO  GROUPL
00526          MOVE AL-UABON           TO  GROUPA
00527          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00528
00529 *******************************STATE
00530      IF STATEI NOT = SPACES
00531          MOVE STATEI             TO  WS-STATE  PI-SAVE-STATE
00532          MOVE AL-UANON           TO  STATEA
00533      ELSE
00534          MOVE ER-0196            TO  EMI-ERROR
00535          MOVE -1                 TO  STATEL
00536          MOVE AL-UABON           TO  STATEA
00537          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00538
00539 *******************************ACCOUNT
00540      IF ACCTI NOT = SPACES
00541          MOVE ACCTI              TO  WS-ACCOUNT PI-SAVE-ACCT
00542          MOVE AL-UANON           TO  ACCTA
00543      ELSE
00544          MOVE ER-0197            TO  EMI-ERROR
00545          MOVE -1                 TO  ACCTL
00546          MOVE AL-UABON           TO  ACCTA
00547          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00548
00549 *******************************CERT
00550      IF CERTI NOT = SPACES
00551          MOVE CERTI              TO  WS-CERT-NO  PI-SAVE-CERT
00552          MOVE AL-UANON           TO  CERTA
00553      ELSE
00554          MOVE ER-0203            TO  EMI-ERROR
00555          MOVE -1                 TO  CERTL
00556          MOVE AL-UABON           TO  CERTA
00557          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00558
00559 *****************************EFFECTIVE DATE
00560      IF CEFDTL NOT = ZEROS
00561          IF CEFDTI NUMERIC
00562              MOVE CEFDTI         TO  DC-GREG-DATE-1-MDY
00563                                      PI-SAVE-CEFDT
00564              MOVE 4              TO  DC-OPTION-CODE
00565              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00566              IF NO-CONVERSION-ERROR
00567                  MOVE DC-BIN-DATE-1  TO  WS-CERT-EFF-DT
00568                  MOVE AL-UNNON   TO  CEFDTA
00569              ELSE
00570                  MOVE ER-2474    TO  EMI-ERROR
00571                  MOVE -1         TO  CEFDTL
00572                  MOVE AL-UNBON   TO  CEFDTA
00573                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00574          ELSE
00575              MOVE ER-2482        TO  EMI-ERROR
00576              MOVE -1             TO  CEFDTL
00577              MOVE AL-UNBON       TO  CEFDTA
00578              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00579      ELSE
00580          MOVE ER-2473            TO  EMI-ERROR
00581          MOVE -1                 TO  CEFDTL
00582          MOVE AL-UNBON           TO  CEFDTA
00583          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00584
00585      MOVE PI-COMPANY-CD          TO  WS-COMPANY-CD.
00586
00587      
      * EXEC CICS HANDLE CONDITION
00588 *        NOTFND (2500-CERT-NOT-FOUND)
00589 *    END-EXEC.
      *    MOVE '"$I                   ! # #00002236' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032323336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00590
00591      
      * EXEC CICS READ
00592 *         SET      (ADDRESS OF CERTIFICATE-MASTER)
00593 *         DATASET  ('ELCERT')
00594 *         RIDFLD   (WS-CONTROL-PRIMARY)
00595 *    END-EXEC.
           MOVE 'ELCERT' TO DFHEIV1
      *    MOVE '&"S        E          (   #00002240' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00596
00597      IF TBCODL = ZERO  OR
00598         TBCODI = SPACES
00599         MOVE CM-REIN-TABLE       TO  CP-TBCOD PI-SAVE-TBCOD
00600                                      PI-TBCOD TBCODO
00601        ELSE
00602          MOVE TBCODI             TO  CP-TBCOD PI-SAVE-TBCOD
00603                                      PI-TBCOD
00604          MOVE AL-UANON           TO  TBCODA.
00605
00606      MOVE CM-CERT-EFF-DT         TO  CP-EFF-DT.
00607      MOVE CP-EFF-DT              TO  DC-BIN-DATE-1.
00608      MOVE SPACES                 TO  DC-OPTION-CODE.
00609      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00610      MOVE DC-GREG-DATE-1-MDY     TO  EFFDTO PI-SAVE-CEFDT
00611                                      CEFDTO PI-SAVE-EFFDT.
00612
00613      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE ISSAGEO
00614                                      PI-SAVE-AGE.
00615      IF CM-LF-BENEFIT-CD NOT = ZEROS  AND  SPACES
00616          MOVE CM-LF-ORIG-TERM    TO  CP-ORIGINAL-TERM ORIGTRMO
00617                                      PI-SAVE-TERM
00618      ELSE
00619          MOVE CM-AH-ORIG-TERM    TO  CP-ORIGINAL-TERM ORIGTRMO
00620                                      PI-SAVE-TERM.
00621      MOVE CM-LF-BENEFIT-CD       TO  CP-LF-TYPE    LFTYPO
00622                                      PI-SAVE-LFTYP.
00623      MOVE CM-AH-BENEFIT-CD       TO  CP-AH-TYPE    AHTYPO
00624                                      PI-SAVE-AHTYP.
00625
00626      ADD CM-LF-ALT-PREMIUM-AMT
00627          CM-LF-PREMIUM-AMT       GIVING WS-LF-PREMIUM-AMT.
00628
00629      MOVE WS-LF-PREMIUM-AMT      TO  CP-LF-PREM    LFPRMO
00630                                      PI-SAVE-LFPRM.
00631      MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-LFPRM-ALT.
00632
00633      MOVE CM-AH-PREMIUM-AMT      TO  CP-AH-PREM    AHPRMO
00634                                      PI-SAVE-AHPRM.
00635      ADD CM-LF-ALT-BENEFIT-AMT
00636          CM-LF-BENEFIT-AMT       GIVING WS-LF-BENEFIT-AMT.
00637
00638      MOVE WS-LF-BENEFIT-AMT      TO  CP-LF-BEN     LFBENO
00639                                      PI-SAVE-LFBEN.
00640      MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-LFAMT-ALT.
00641      MOVE CM-AH-BENEFIT-AMT      TO  CP-AH-BEN     AHBENO
00642                                      PI-SAVE-AHBEN.
00643      MOVE CM-IND-GRP-TYPE        TO  CP-IG-CODE    IGCODEO
00644                                      PI-SAVE-IGCOD.
00645
00646      MOVE AL-UNNON               TO  ISSAGEA  ORIGTRMA  LFPRMA
00647                                      AHPRMA   EFFDTA    LFBENA
00648                                      AHBENA.
00649      MOVE AL-UANON               TO  TBCODA   LFTYPA    AHTYPA
00650                                      IGCODEA.
00651
00652      GO TO 3000-PERFORM-CALC-ROUTINE.
00653
00654  2500-CERT-NOT-FOUND.
00655      MOVE ER-0244                TO  EMI-ERROR
00656      MOVE -1                     TO  CARRL.
00657      MOVE AL-UABON               TO  CARRA.
00658      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00659      GO TO 8200-SEND-DATAONLY.
00660
00661  3000-PERFORM-CALC-ROUTINE.
00662      IF EMI-ERROR NOT = ZEROS
00663          GO TO 8200-SEND-DATAONLY.
00664
00665      MOVE 'X'                    TO  PI-SAVE-SW PI-FIRST-SW.
00666      MOVE PI-COMPANY-CD          TO  CP-COMPANY-CD.
00667      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
00668      MOVE ZERO                   TO  CP-RETURN-CODE.
00669      MOVE LINK-ELREIN            TO  PGM-NAME.
00670
00671      PERFORM 9700-LINK THRU 9700-EXIT.
00672      PERFORM 7100-CREATE-TEMP-STORAGE THRU 7100-EXIT.
00673
00674  4000-DISPLAY-RESULTS.
00675      IF CP-RETURN-CODE NOT = 0
00676          GO TO 4000-ERROR.
00677
00678      MOVE 1                      TO  INCR1 PI-SAVE-INCR1.
00679
00680      PERFORM 5000-BLD-LINE THRU 5000-XIT.
00681
00682      IF CARRL NOT = ZEROS
00683          MOVE -1                 TO  CARRL
00684      ELSE
00685          MOVE -1                 TO  TBCODL.
00686
00687      GO TO 8200-SEND-DATAONLY.
00688
00689  4000-ERROR.
00690      IF CP-RETURN-CODE = 1
00691          MOVE ER-2425            TO  EMI-ERROR
00692          MOVE -1                 TO  LFTYPL
00693          MOVE AL-UABON           TO  LFTYPA
00694          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00695      IF CP-RETURN-CODE = 2
00696          MOVE ER-2429            TO  EMI-ERROR
00697          MOVE -1                 TO  AHTYPL
00698          MOVE AL-UABON           TO  AHTYPA
00699          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00700      IF CP-RETURN-CODE = 3
00701          MOVE ER-2615            TO  EMI-ERROR
00702          MOVE -1                 TO  TBCODL
00703          MOVE AL-UABON           TO  TBCODA
00704          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00705
00706      GO TO 8200-SEND-DATAONLY.
00707
00708  5000-BLD-LINE.
00709      MOVE 1                      TO  INCR2.
00710      MOVE SPACES       TO  REPLINEO (1) REPLINEO (2) REPLINEO (3)
00711                            REPLINEO (4) REPLINEO (5) REPLINEO (6)
00712                            REPLINEO (7) REPLINEO (8) REPLINEO (9)
00713                            REPLINEO (10).
00714
00715  5000-BLD-LINE-LOOP.
00716      IF REIN-COMP (INCR1) = SPACES
00717          GO TO 5000-XIT.
00718
00719      MOVE SPACES                     TO  BLD-LINE.
00720      MOVE REIN-CO-PRIME (INCR1)      TO  BL-CEDED.
00721      MOVE REIN-CO-SUB   (INCR1)      TO  BL-CEDED-SUB.
00722      MOVE REIN-LFPRM    (INCR1)      TO  BL-LF-PREMIUM.
00723      MOVE REIN-AHPRM    (INCR1)      TO  BL-AH-PREMIUM.
00724      MOVE REIN-LFAMT    (INCR1)      TO  BL-LF-BENEFIT.
00725      MOVE REIN-AHAMT    (INCR1)      TO  BL-AH-BENEFIT.
00726
00727      MOVE BLD-LINE               TO  REPLINEO (INCR2).
00728      ADD  1                      TO  INCR1 INCR2  PI-SAVE-INCR1.
00729      IF INCR2 = 11
00730           GO TO 5000-XIT.
00731
00732      GO TO 5000-BLD-LINE-LOOP.
00733
00734  5000-XIT.
00735      EXIT.
00736
00737  6000-PAGE-FORWARD.
00738      MOVE PI-SAVE-INCR1          TO  INCR1.
00739      IF REIN-COMP (INCR1) = SPACES
00740          MOVE -1                 TO  TBCODL
00741          MOVE ER-0130            TO  EMI-ERROR
00742          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00743          GO TO 8200-SEND-DATAONLY.
00744
00745 *    MOVE 11                     TO  INCR1 PI-SAVE-INCR1.
00746      PERFORM 5000-BLD-LINE THRU 5000-XIT.
00747      MOVE -1                     TO  TBCODL.
00748      GO TO 8200-SEND-DATAONLY.
00749
00750  7000-PAGE-BACKWARD.
00751      MOVE PI-SAVE-INCR1          TO  INCR1.
00752      IF INCR1 LESS 12
00753          MOVE -1                 TO  TBCODL
00754          MOVE ER-0131            TO  EMI-ERROR
00755          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00756          GO TO 8200-SEND-DATAONLY.
00757
00758      MOVE 1                      TO  INCR1 PI-SAVE-INCR1.
00759      PERFORM 5000-BLD-LINE THRU 5000-XIT.
00760      MOVE -1                     TO  TBCODL.
00761      GO TO 8200-SEND-DATAONLY.
00762
00763    EJECT
00764  7100-CREATE-TEMP-STORAGE.
00765      PERFORM 7300-DELETE-TEMP-STORAGE THRU 7300-EXIT.
00766      
      * EXEC CICS WRITEQ TS
00767 *        QUEUE   (QID)
00768 *        FROM    (REIN-HOLD-AREAS)
00769 *        LENGTH  (REIN-LENGTH)
00770 *        ITEM    (ITEM-VALUE)
00771 *    END-EXEC.
      *    MOVE '*" I                  ''   #00002415' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 REIN-HOLD-AREAS, 
                 REIN-LENGTH, 
                 ITEM-VALUE, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00772
00773  7100-EXIT.
00774       EXIT.
00775
00776  7200-RECOVER-TEMP-STORAGE.
00777      
      * EXEC CICS HANDLE CONDITION
00778 *        QIDERR  (7200-EXIT)
00779 *    END-EXEC.
      *    MOVE '"$N                   ! $ #00002426' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032343236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00780
00781      
      * EXEC CICS READQ TS
00782 *        QUEUE    (QID)
00783 *        INTO     (REIN-HOLD-AREAS)
00784 *        LENGTH   (REIN-LENGTH)
00785 *        ITEM     (ITEM-VALUE)
00786 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00002430' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 REIN-HOLD-AREAS, 
                 REIN-LENGTH, 
                 ITEM-VALUE, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00787
00788  7200-EXIT.
00789       EXIT.
00790
00791  7300-DELETE-TEMP-STORAGE.
00792      
      * EXEC CICS HANDLE CONDITION
00793 *        QIDERR  (7300-EXIT)
00794 *    END-EXEC.
      *    MOVE '"$N                   ! % #00002441' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032343431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00795
00796      
      * EXEC CICS DELETEQ TS
00797 *        QUEUE  (QID)
00798 *    END-EXEC.
      *    MOVE '*&                    #   #00002445' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00799
00800  7300-EXIT.
00801       EXIT.
00802
00803  8100-SEND-INITIAL-MAP.
00804      MOVE SAVE-DATE              TO  DATEO.
00805      MOVE EIBTIME                TO  TIME-IN.
00806      MOVE TIME-OUT               TO  TIMEO.
00807      MOVE -1                     TO  TBCODL.
00808      MOVE EMI-MESSAGE-AREA (1)   TO  MSGO.
00809
00810      MOVE PI-LIFE-OVERRIDE-L2    TO  LFHDG1O
00811                                      LFHDG2O
00812                                      LFHDG3O
00813                                      LFHDG4O
00814                                      LFHDG5O.
00815      MOVE PI-AH-OVERRIDE-L2      TO  AHHDG1O
00816                                      AHHDG2O
00817                                      AHHDG3O
00818                                      AHHDG4O
00819                                      AHHDG5O.
00820      MOVE AL-SABON               TO  LFHDG1A  LFHDG2A  LFHDG3A
00821                                      LFHDG4A  LFHDG5A
00822                                      AHHDG1A  AHHDG2A  AHHDG3A
00823                                      AHHDG4A  AHHDG5A.
00824
00825      
      * EXEC CICS SEND
00826 *        MAP    (MAP-NAME)
00827 *        MAPSET (MAPSET-NAME)
00828 *        FROM   (EL657AI)
00829 *        ERASE
00830 *        CURSOR
00831 *    END-EXEC.
           MOVE LENGTH OF
            EL657AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002474' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL657AI, 
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
           
00832
00833      GO TO 9100-RETURN-TRAN.
00834
00835  8200-SEND-DATAONLY.
00836      MOVE SAVE-DATE              TO  DATEO.
00837      MOVE EIBTIME                TO  TIME-IN.
00838      MOVE TIME-OUT               TO  TIMEO.
00839      MOVE EMI-MESSAGE-AREA (1)   TO  MSGO.
00840
00841      MOVE PI-LIFE-OVERRIDE-L2    TO  LFHDG1O
00842                                      LFHDG2O
00843                                      LFHDG3O
00844                                      LFHDG4O
00845                                      LFHDG5O.
00846      MOVE PI-AH-OVERRIDE-L2      TO  AHHDG1O
00847                                      AHHDG2O
00848                                      AHHDG3O
00849                                      AHHDG4O
00850                                      AHHDG5O.
00851      MOVE AL-SABON               TO  LFHDG1A  LFHDG2A  LFHDG3A
00852                                      LFHDG4A  LFHDG5A
00853                                      AHHDG1A  AHHDG2A  AHHDG3A
00854                                      AHHDG4A  AHHDG5A.
00855
00856      
      * EXEC CICS SEND
00857 *        MAP    (MAP-NAME)
00858 *        MAPSET (MAPSET-NAME)
00859 *        FROM   (EL657AI)
00860 *        DATAONLY
00861 *        CURSOR
00862 *    END-EXEC.
           MOVE LENGTH OF
            EL657AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002505' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL657AI, 
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
           
00863
00864      GO TO 9100-RETURN-TRAN.
00865
00866      EJECT
00867  8300-SEND-TEXT SECTION.
00868      
      * EXEC CICS SEND TEXT
00869 *        FROM   (LOGOFF-TEXT)
00870 *        LENGTH (LOGOFF-LENGTH)
00871 *        ERASE
00872 *        FREEKB
00873 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002517' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353137' TO DFHEIV0(25:11)
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
           
00874
00875      
      * EXEC CICS RETURN
00876 *    END-EXEC.
      *    MOVE '.(                    &   #00002524' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00877
00878  8300-EXIT.
00879      EXIT.
00880
00881      EJECT
00882  8500-DATE-CONVERT.
00883      MOVE LINK-ELDATCV           TO  PGM-NAME.
00884      
      * EXEC CICS LINK
00885 *        PROGRAM    (PGM-NAME)
00886 *        COMMAREA   (DATE-CONVERSION-DATA)
00887 *        LENGTH     (DC-COMM-LENGTH)
00888 *    END-EXEC.
      *    MOVE '."C                   ''   #00002533' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00889
00890  8500-EXIT.
00891      EXIT.
00892
00893  8800-UNAUTHORIZED-ACCESS.
00894      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
00895      GO TO 8300-SEND-TEXT.
00896
00897  8950-CLEAR-RETURN.
00898      MOVE TRAN-COMPLETE-MSG      TO  TEXT-AREA.
00899
00900  8990-SEND-TEXT.
00901      
      * EXEC CICS SEND TEXT
00902 *        FROM    (TEXT-AREA)
00903 *        LENGTH  (TEXT-LENGTH)
00904 *        ERASE
00905 *        FREEKB
00906 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002550' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEXT-AREA, 
                 TEXT-LENGTH, 
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
           
00907       EJECT
00908
00909  9000-RETURN-CICS.
00910      MOVE LINK-005               TO  PGM-NAME.
00911      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00912      GO TO 9300-XCTL.
00913
00914  9100-RETURN-TRAN.
00915      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
00916      MOVE '657A'                 TO  PI-CURRENT-SCREEN-NO.
00917
00918      
      * EXEC CICS RETURN
00919 *        TRANSID   (TRANS-ID)
00920 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00921 *        LENGTH    (PI-COMM-LENGTH)
00922 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002567' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00923
00924      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL657' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00925
00926  9300-XCTL.
00927      
      * EXEC CICS XCTL
00928 *        PROGRAM    (PGM-NAME)
00929 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00930 *        LENGTH     (PI-COMM-LENGTH)
00931 *    END-EXEC.
      *    MOVE '.$C                   $   #00002576' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00932
00933  9400-CLEAR.
00934      MOVE SPACE                  TO  PI-FIRST-SW.
00935      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
00936      GO TO 9300-XCTL.
00937
00938  9700-LINK.
00939      
      * EXEC CICS LINK
00940 *        PROGRAM   (PGM-NAME)
00941 *        COMMAREA  (REINSURANCE-PASS-AREA)
00942 *        LENGTH    (CP-COMM-LENGTH)
00943 *    END-EXEC.
      *    MOVE '."C                   ''   #00002588' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 REINSURANCE-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00944
00945  9700-EXIT.
00946       EXIT.
00947       EJECT
00948
00949  9900-ERROR-FORMAT.
00950      IF NOT EMI-ERRORS-COMPLETE
00951          MOVE LINK-001           TO  PGM-NAME
00952          
      * EXEC CICS LINK
00953 *            PROGRAM   (PGM-NAME)
00954 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00955 *            LENGTH    (EMI-COMM-LENGTH)
00956 *         END-EXEC.
      *    MOVE '."C                   ''   #00002601' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00957
00958  9900-EXIT.
00959      EXIT.
00960
00961  9990-ABEND.
00962      MOVE LINK-004               TO  PGM-NAME.
00963      MOVE DFHEIBLK               TO  EMI-LINE1.
00964      
      * EXEC CICS LINK
00965 *        PROGRAM   (PGM-NAME)
00966 *        COMMAREA  (EMI-LINE1)
00967 *        LENGTH    (72)
00968 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002613' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00969
00970      GO TO 8200-SEND-DATAONLY.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL657' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8100-SEND-INITIAL-MAP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2500-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7200-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7300-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL657' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
