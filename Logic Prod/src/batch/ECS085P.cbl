00001  IDENTIFICATION DIVISION.                                         09/18/97
00002                                                                   ECS085
00003  PROGRAM-ID.                ECS085.                                  LV001
00004 *              PROGRAM CONVERTED BY                               ECS085
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS085
00006 *              CONVERSION DATE 02/08/96 18:45:53.                 ECS085
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS085
00008 *                           VMOD=2.008.                           ECS085
00009                                                                   ECS085
00010 *AUTHOR.        LOGIC, INC.                                       ECS085
00011 *               DALLAS, TEXAS.                                    ECS085
00012                                                                   ECS085
00013 *DATE-COMPILED.                                                   ECS085
00014                                                                   ECS085
00015 *SECURITY.   *****************************************************ECS085
00016 *            *                                                   *ECS085
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS085
00018 *            *                                                   *ECS085
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS085
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS085
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS085
00022 *            *                                                   *ECS085
00023 *            *****************************************************ECS085
00024                                                                   ECS085
00025 *REMARKS.                                                         ECS085
00026 *        PRINT UNEARNED PREMIUM AND COMMISSION ANALYSIS -         ECS085
00027 *        DETAIL.  PROGRAM SWITCHES NOT APPLICABLE.                ECS085
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 300 TO 900
092602******************************************************************
00028                                                                   ECS085
00029  ENVIRONMENT DIVISION.                                            ECS085
00030  INPUT-OUTPUT SECTION.                                            ECS085
00031  FILE-CONTROL.                                                    ECS085
00032                                                                   ECS085
00033      SELECT PRNTR        ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS085
00034      SELECT ACCT-MSTR    ASSIGN TO SYS010-3380-ERACCTT            ECS085
00035                          ACCESS IS SEQUENTIAL                     ECS085
00036                          ORGANIZATION IS INDEXED                  ECS085
00037                          FILE STATUS IS AM-FILE-STATUS            ECS085
00038                          RECORD KEY IS AM-CONTROL-PRIMARY.        ECS085
00039                                                                   ECS085
00040      SELECT GAAP-EXTR    ASSIGN TO SYS011-UT-2400-S-SYS011.       ECS085
00041      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.       ECS085
00042      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS085
00043  EJECT                                                            ECS085
00044  DATA DIVISION.                                                   ECS085
00045  FILE SECTION.                                                    ECS085
00046                                                                   ECS085
00047  FD  PRNTR                                                        ECS085
00048                              COPY ELCPRTFD.                       ECS085
00049  EJECT                                                            ECS085
00050  FD  ACCT-MSTR.                                                   ECS085
00051                                                                   ECS085
00052                              COPY ERCACCT.                        ECS085
00053                                                                   ECS085
00054  EJECT                                                            ECS085
00055  FD  GAAP-EXTR                                                    ECS085
00056                              COPY ECSGAPFD.                       ECS085
00057                                                                   ECS085
00058                              COPY ECSGAP01.                       ECS085
00059  EJECT                                                            ECS085
00060  FD  DISK-DATE                                                    ECS085
00061                              COPY ELCDTEFD.                       ECS085
00062  EJECT                                                            ECS085
00063  FD  FICH                                                         ECS085
00064                              COPY ECSFICH.                        ECS085
00065  EJECT                                                            ECS085
00066  WORKING-STORAGE SECTION.                                         ECS085
00067  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS085
00068  77  FILLER  PIC X(32) VALUE '********************************'.  ECS085
00069  77  FILLER  PIC X(32) VALUE '     ECS085 WORKING STORAGE     '.  ECS085
00070  77  FILLER  PIC X(32) VALUE '*****VMOD=2.008*****************'.  ECS085
00071                                                                   ECS085
00072  77  PGM-SUB                 PIC S999    COMP    VALUE +085.      ECS085
00073  77  AH-SW                   PIC S9      COMP-3.                  ECS085
00074  77  LIFE-SW                 PIC S9      COMP-3.                  ECS085
00075  77  FST-SW                  PIC S9      COMP-3.                  ECS085
00076  77  HD-SW                   PIC S9      COMP-3  VALUE +0.        ECS085
00077  77  ZERO-SW                 PIC S9      COMP-3  VALUE +1.        ECS085
00078  77  X1                      PIC S999    COMP-3.                  ECS085
00079  77  X2                      PIC S999    COMP-3.                  ECS085
00080  77  LNCTR                   PIC S999    COMP-3.                  ECS085
092602 77  MAX-BEN                 PIC S999    COMP-3  VALUE +900.      ECS085
LGC191 77  SELECT-CT               PIC S9999   COMP-3  VALUE +4000.     ECS085
00082  77  PGCTR                   PIC S9(5)   COMP-3.                  ECS085
00083  77  X                       PIC X               VALUE SPACE.     ECS085
00084  EJECT                                                            ECS085
00085  01  HD1.                                                         ECS085
00086      12  FILLER              PIC X(43)           VALUE SPACES.    ECS085
00087      12  FILLER              PIC X(38)           VALUE            ECS085
00088              'UNEARNED PREMIUM AND COMMISSION DETAIL'.            ECS085
00089      12  FILLER              PIC X(38)           VALUE SPACES.    ECS085
00090      12  FILLER              PIC X(6)            VALUE 'ECS085'.  ECS085
00091                                                                   ECS085
00092  01  HD2.                                                         ECS085
00093      12  SUB-HD2             PIC X(47)           VALUE SPACES.    ECS085
00094      12  HD-CO               PIC X(30).                           ECS085
00095      12  FILLER              PIC X(42)           VALUE SPACES.    ECS085
00096      12  HD-RD               PIC X(8).                            ECS085
00097                                                                   ECS085
00098  01  HD3.                                                         ECS085
00099      12  SUB-HD3             PIC X(53)           VALUE SPACES.    ECS085
00100      12  HD-DT               PIC X(18).                           ECS085
00101      12  FILLER              PIC X(48)           VALUE SPACES.    ECS085
00102      12  FILLER              PIC X(5)            VALUE 'PAGE '.   ECS085
00103      12  HD-PAGE             PIC ZZ,ZZ9.                          ECS085
00104                                                                   ECS085
00105  01  HD4.                                                         ECS085
00106      12 FILLER               PIC X(44)           VALUE            ECS085
00107             '                             * * * * * * * *'.       ECS085
00108      12 FILLER               PIC X(44)           VALUE            ECS085
00109             ' * *    P R E M I U M    * * * * * * * * * *'.       ECS085
00110      12 FILLER               PIC X(44)           VALUE            ECS085
00111             '                                            '.       ECS085
00112                                                                   ECS085
00113  01  HD5.                                                         ECS085
00114      12 HD-DESC              PIC X(15).                           ECS085
00115      12 FILLER               PIC X(29)           VALUE            ECS085
00116                            'IN FORCE                  UNE'.       ECS085
00117      12 FILLER               PIC X(44)           VALUE            ECS085
00118             'ARNED     UNEARNED     DOMICILE      STATE  '.       ECS085
00119      12 FILLER               PIC X(44)           VALUE            ECS085
00120             '    MORTALITY    ALTERNATE        REMAINING '.       ECS085
00121                                                                   ECS085
00122  01  HD6.                                                         ECS085
00123      12 FILLER               PIC X(44)           VALUE            ECS085
00124             '                COUNT        WRITTEN      RU'.       ECS085
00125      12 FILLER               PIC X(44)           VALUE            ECS085
00126             'LE 78     PRO-RATA    STATUTORY    STATUTORY'.       ECS085
00127      12 FILLER               PIC X(44)           VALUE            ECS085
00128             '     RESERVE      RESERVE           AMOUNT  '.       ECS085
00129                                                                   ECS085
00130  01  HD7.                                                         ECS085
00131      12 FILLER               PIC X(44)           VALUE            ECS085
00132             '                                * *   C O M '.       ECS085
00133      12 FILLER               PIC X(44)           VALUE            ECS085
00134             'M I S S I O N   * *        * * * *    T A X '.       ECS085
00135      12 FILLER               PIC X(44)           VALUE            ECS085
00136             'E S    * * * *                              '.       ECS085
00137                                                                   ECS085
00138  01  HD8.                                                         ECS085
00139      12 FILLER               PIC X(44)           VALUE            ECS085
00140             '                                PAID      RU'.       ECS085
00141      12 FILLER               PIC X(44)           VALUE            ECS085
00142             'LE 78      PRO-RATA        PAID      RULE 78'.       ECS085
00143      12 FILLER               PIC X(44)           VALUE            ECS085
00144             '      PRO-RATA                              '.       ECS085
00145  EJECT                                                            ECS085
00146  01  SUB-HEADINGS.                                                ECS085
00147      12  HEAD2.                                                   ECS085
00148          16  FILLER          PIC X(41)           VALUE            ECS085
00149                  'CARR  GROUP  STATE                  ACCT '.     ECS085
00150      12  ACCT-HDA REDEFINES HEAD2.                                ECS085
00151          16  ACCT-HD2        PIC X(41).                           ECS085
00152      12  ST-HDA REDEFINES HEAD2.                                  ECS085
00153          16  ST-HD2          PIC X(28).                           ECS085
00154          16  FILLER          PIC X(13).                           ECS085
00155      12  CO-HDA REDEFINES HEAD2.                                  ECS085
00156          16  CO-HD2          PIC X(12).                           ECS085
00157          16  FILLER          PIC X(29).                           ECS085
00158      12  CARR-HDA REDEFINES HEAD2.                                ECS085
00159          16  CARR-HD2        PIC X(4).                            ECS085
00160          16  FILLER          PIC X(37).                           ECS085
00161      12  HEAD3.                                                   ECS085
00162          16  FILLER          PIC XX              VALUE SPACES.    ECS085
00163          16  HD-CARR         PIC X.                               ECS085
00164          16  FILLER          PIC XXX             VALUE SPACES.    ECS085
00165          16  HD-COMP         PIC X(6).                            ECS085
00166          16  FILLER          PIC X               VALUE SPACES.    ECS085
00167          16  HD-ST           PIC XX.                              ECS085
00168          16  FILLER          PIC X               VALUE SPACES.    ECS085
00169          16  HD-ST-NM        PIC X(15).                           ECS085
00170          16  FILLER          PIC XX              VALUE SPACES.    ECS085
00171          16  HD-ACCT         PIC X(10).                           ECS085
00172      12  ACCT-HDB REDEFINES HEAD3.                                ECS085
00173          16  ACCT-HD3        PIC X(43).                           ECS085
00174      12  ST-HDB REDEFINES HEAD3.                                  ECS085
00175          16  ST-HD3          PIC X(31).                           ECS085
00176          16  FILLER          PIC X(12).                           ECS085
00177      12  CO-HDB REDEFINES HEAD3.                                  ECS085
00178          16  CO-HD3          PIC X(13).                           ECS085
00179          16  FILLER          PIC X(30).                           ECS085
00180      12  CARR-HDB REDEFINES HEAD3.                                ECS085
00181          16  CARR-HD3        PIC X(4).                            ECS085
00182          16  FILLER          PIC X(39).                           ECS085
00183      12  HD3A.                                                    ECS085
00184          16  FILLER          PIC X(39)           VALUE SPACES.    ECS085
00185      12  ACCT-SUMM-HD.                                            ECS085
00186          16  FILLER          PIC X(8)            VALUE 'ACCOUNT'. ECS085
00187          16  ACCOUNT-NAME    PIC X(30).                           ECS085
00188  EJECT                                                            ECS085
00189  01  DETAIL-HEADINGS.                                             ECS085
00190      12  DTL-4.                                                   ECS085
00191          16  FILLER          PIC X(44)           VALUE            ECS085
00192                  '    CERT.      EFF.        BENEFIT   * TRM *'.  ECS085
00193          16  FILLER          PIC X(44)           VALUE            ECS085
00194                  '       ORIG           REM                   '.  ECS085
00195      12  DTL-5.                                                   ECS085
00196          16  FILLER          PIC X(44)           VALUE            ECS085
00197                  '   NUMBER      DATE         TYPE     ORG REM'.  ECS085
00198          16  FILLER          PIC X(44)           VALUE            ECS085
00199                  '       AMT            AMT        PREMIUM    '.  ECS085
00200          16  FILLER          PIC X(44)           VALUE            ECS085
00201                  'PRO-RATA     RULE-78       STATE            '.  ECS085
00202  EJECT                                                            ECS085
00203  01  MISC-WS.                                                     ECS085
00204      12  AM-FILE-STATUS      PIC XX.                              ECS085
00205      12  WS-RETURN-CODE      PIC S9(4)   COMP.                    ECS085
00206      12  WS-ABEND-FILE-STATUS PIC XX     VALUE '00'.              ECS085
00207      12  WS-ABEND-MESSAGE    PIC X(80).                           ECS085
00208      12  WS-ABEND-MESSAGE-R REDEFINES WS-ABEND-MESSAGE.           ECS085
00209          16  FILLER          PIC X(9).                            ECS085
00210          16  WS-ABEND-BEN-TYPE PIC X(6).                          ECS085
00211          16  FILLER          PIC X(65).                           ECS085
00212      12  WS-ZERO             PIC S9      VALUE +0  COMP-3.        ECS085
00213      12  ACCT-SEQ.                                                ECS085
00214          16  FILLER          PIC X(19)           VALUE LOW-VALUE. ECS085
00215      12  SAVE-LPOINTERS.                                          ECS085
00216          16  SAVE-LPOINTER   PIC XXX             VALUE LOW-VALUE. ECS085
00217          16  SAVE-LX2        PIC S999    COMP-3.                  ECS085
00218          16  SAVE-LDESC      PIC X(10)           VALUE SPACES.    ECS085
00219      12  SAVE-APOINTERS.                                          ECS085
00220          16  SAVE-APOINTER   PIC XXX             VALUE LOW-VALUE. ECS085
00221          16  SAVE-AX2        PIC S999.                            ECS085
00222          16  SAVE-ADESC      PIC X(10)           VALUE SPACES.    ECS085
00223      12  CUR-SEQ.                                                 ECS085
00224              20  CUR-CARR    PIC X.                               ECS085
00225              20  CUR-CO      PIC X(6).                            ECS085
00226              20  CUR-ST      PIC XX.                              ECS085
00227              20  CUR-ACCT    PIC X(10).                           ECS085
00228      12  WX-SEQ.                                                  ECS085
00229              20  WX-CARR     PIC X.                               ECS085
00230              20  WX-CO       PIC X(6).                            ECS085
00231              20  WX-ST       PIC XX.                              ECS085
00232              20  WX-ACCT     PIC X(10).                           ECS085
00233      12  X-POINTERS.                                              ECS085
092602         16  X-MATCH     OCCURS 900 TIMES.                        ECS085
00235              20  X-POINTER.                                       ECS085
00236                  24  X-BEN   PIC XX.                              ECS085
00237                  24  X-TYP   PIC 9.                               ECS085
00238              20  X-DESC      PIC X(10).                           ECS085
00239      12  WX-POINTERS.                                             ECS085
00240          16  WX-POINTER.                                          ECS085
00241              20  WX-BEN      PIC XX.                              ECS085
00242              20  WX-TYP      PIC 9.                               ECS085
00243      12  PX-POINTER.                                              ECS085
00244          16  PX-BEN          PIC XXX.                             ECS085
00245          16  PX-DESC.                                             ECS085
00246              20  FILLER      PIC X(6).                            ECS085
00247              20  PX-BEN-DESC PIC X(4).                            ECS085
00248  EJECT                                                            ECS085
00249  01  TOTALS-AREA.                                                 ECS085
00250      12  X-TOTALS.                                                ECS085
00251          16  X-TOTS.                                              ECS085
00252              20  X-LEVEL OCCURS 900 TIMES.                        ECS085
00253                  24  X-AMTS  PIC X(90).                           ECS085
00254      12  X-DETL.                                                  ECS085
00255          16  X-COUNT         PIC S9(7)     COMP-3.                ECS085
00256          16  X-WRITTEN       PIC S9(9)V99  COMP-3.                ECS085
00257          16  X-P78           PIC S9(9)V99  COMP-3.                ECS085
00258          16  X-PRATA         PIC S9(9)V99  COMP-3.                ECS085
00259          16  X-DOMICILE      PIC S9(9)V99  COMP-3.                ECS085
00260          16  X-STATE         PIC S9(9)V99  COMP-3.                ECS085
00261          16  X-RESERV        PIC S9(9)V99  COMP-3.                ECS085
00262          16  X-ALTRSV        PIC S9(9)V99  COMP-3.                ECS085
00263          16  X-REMAIN        PIC S9(13)V99 COMP-3.                ECS085
00264          16  X-PAID          PIC S9(9)V99  COMP-3.                ECS085
00265          16  X-C78           PIC S9(9)V99  COMP-3.                ECS085
00266          16  X-CRATA         PIC S9(9)V99  COMP-3.                ECS085
00267          16  X-TAX           PIC S9(9)V99  COMP-3.                ECS085
00268          16  X-T78           PIC S9(9)V99  COMP-3.                ECS085
00269          16  X-TRATA         PIC S9(9)V99  COMP-3.                ECS085
00270      12  R-DETL.                                                  ECS085
00271          16  R-COUNT         PIC S9(7)     COMP-3.                ECS085
00272          16  R-WRITTEN       PIC S9(9)V99  COMP-3.                ECS085
00273          16  R-P78           PIC S9(9)V99  COMP-3.                ECS085
00274          16  R-PRATA         PIC S9(9)V99  COMP-3.                ECS085
00275          16  R-DOMICILE      PIC S9(9)V99  COMP-3.                ECS085
00276          16  R-STATE         PIC S9(9)V99  COMP-3.                ECS085
00277          16  R-RESERV        PIC S9(9)V99  COMP-3.                ECS085
00278          16  R-ALTRSV        PIC S9(9)V99  COMP-3.                ECS085
00279          16  R-REMAIN        PIC S9(13)V99 COMP-3.                ECS085
00280          16  R-PAID          PIC S9(9)V99  COMP-3.                ECS085
00281          16  R-C78           PIC S9(9)V99  COMP-3.                ECS085
00282          16  R-CRATA         PIC S9(9)V99  COMP-3.                ECS085
00283          16  R-TAX           PIC S9(9)V99  COMP-3.                ECS085
00284          16  R-T78           PIC S9(9)V99  COMP-3.                ECS085
00285          16  R-TRATA         PIC S9(9)V99  COMP-3.                ECS085
00286                                                                   ECS085
00287  01  ST-TOTALS.                                                   ECS085
00288      12  ST-TOTS.                                                 ECS085
092602         16  ST-LEVEL     OCCURS 900 TIMES.                       ECS085
00290              20  ST-AMTS     PIC X(90).                           ECS085
00291  01  CO-TOTALS.                                                   ECS085
092602     12  FILLER              PIC X(81000).                        ECS085
00293  01  CARR-TOTALS.                                                 ECS085
092602     12  FILLER              PIC X(81000).                        ECS085
00295  01  FINAL-TOTALS.                                                ECS085
092602     12  FILLER              PIC X(81000).                        ECS085
00297  EJECT                                                            ECS085
00298  01  CALC-CTRS.                                                   ECS085
00299      12  SUB-TOTALS.                                              ECS085
00300          16  S-COUNT         PIC S9(7)     COMP-3 VALUE +0.       ECS085
00301          16  S-WRITTEN       PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00302          16  S-P78           PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00303          16  S-PRATA         PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00304          16  S-DOMICILE      PIC S9(9)V99  COMP-3.                ECS085
00305          16  S-STATE         PIC S9(9)V99  COMP-3.                ECS085
00306          16  S-RESERV        PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00307          16  S-ALTRSV        PIC S9(9)V99  COMP-3.                ECS085
00308          16  S-REMAIN        PIC S9(13)V99 COMP-3 VALUE +0.       ECS085
00309          16  S-PAID          PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00310          16  S-C78           PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00311          16  S-CRATA         PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00312          16  S-TAX           PIC S9(9)V99  COMP-3.                ECS085
00313          16  S-T78           PIC S9(9)V99  COMP-3.                ECS085
00314          16  S-TRATA         PIC S9(9)V99  COMP-3.                ECS085
00315      12  TOTALS.                                                  ECS085
00316          16  T-COUNT         PIC S9(7)     COMP-3 VALUE +0.       ECS085
00317          16  T-WRITTEN       PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00318          16  T-P78           PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00319          16  T-PRATA         PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00320          16  T-DOMICILE      PIC S9(9)V99  COMP-3.                ECS085
00321          16  T-STATE         PIC S9(9)V99  COMP-3.                ECS085
00322          16  T-RESERV        PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00323          16  T-ALTRSV        PIC S9(9)V99  COMP-3.                ECS085
00324          16  T-REMAIN        PIC S9(13)V99 COMP-3 VALUE +0.       ECS085
00325          16  T-PAID          PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00326          16  T-C78           PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00327          16  T-CRATA         PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00328          16  T-TAX           PIC S9(9)V99  COMP-3.                ECS085
00329          16  T-T78           PIC S9(9)V99  COMP-3.                ECS085
00330          16  T-TRATA         PIC S9(9)V99  COMP-3.                ECS085
00331      12  X-TOTAL             PIC S9(9)V99  COMP-3 VALUE +0.       ECS085
00332                                                                   ECS085
00333  01  SPECIAL-TOTALS.                                              ECS085
00334      12  CERT-WORK.                                               ECS085
00335          16  X-CALC-REMAIN       PIC S9(15)V99 COMP-3 VALUE +0.   ECS085
00336          16  X-AH-EXP            PIC S9(9)V99  COMP-3 VALUE +0.   ECS085
00337      12  ACCT-WORK.                                               ECS085
00338          16  ACCT-AGE-REMAIN     PIC S9(15)V99 COMP-3 VALUE +0.   ECS085
00339          16  ACCT-AH-EXP         PIC S9(9)V99  COMP-3 VALUE +0.   ECS085
00340      12  STATE-WORK.                                              ECS085
00341          16  ST-AGE-REMAIN       PIC S9(15)V99 COMP-3 VALUE +0.   ECS085
00342          16  ST-AH-EXP           PIC S9(9)V99  COMP-3 VALUE +0.   ECS085
00343      12  COMPANY-WORK.                                            ECS085
00344          16  CO-AGE-REMAIN       PIC S9(15)V99 COMP-3 VALUE +0.   ECS085
00345          16  CO-AH-EXP           PIC S9(9)V99  COMP-3 VALUE +0.   ECS085
00346      12  CARRIER-WORK.                                            ECS085
00347          16  CARR-AGE-REMAIN     PIC S9(15)V99 COMP-3 VALUE +0.   ECS085
00348          16  CARR-AH-EXP         PIC S9(9)V99  COMP-3 VALUE +0.   ECS085
00349      12  FINAL-WORK.                                              ECS085
00350          16  FINAL-AGE-REMAIN    PIC S9(15)V99 COMP-3 VALUE +0.   ECS085
00351          16  FINAL-AH-EXP        PIC S9(9)V99  COMP-3 VALUE +0.   ECS085
00352      12  OTHR-WORK.                                               ECS085
00353          16  X-WT-REMAIN         PIC S9(5)V99  COMP-3 VALUE +0.   ECS085
00354      12  X-DUPS.                                                  ECS085
00355          16  X-DUP               PIC S9(7)     COMP-3.            ECS085
00356      12  ST-DUPS.                                                 ECS085
00357          16  ST-DUP              PIC S9(7)     COMP-3.            ECS085
00358      12  CO-DUPS.                                                 ECS085
00359          16  CO-DUP              PIC S9(7)     COMP-3.            ECS085
00360      12  CARR-DUPS.                                               ECS085
00361          16  CARR-DUP            PIC S9(7)     COMP-3.            ECS085
00362      12  FINL-DUPS.                                               ECS085
00363          16  FINL-DUP            PIC S9(7)     COMP-3.            ECS085
00364      12  PRT-SPECIAL.                                             ECS085
00365          16  FILLER              PIC X(6)        VALUE            ECS085
00366                  'TOTAL '.                                        ECS085
00367          16  PRT-SPC-BEN-TYPE    PIC XX          VALUE SPACES.    ECS085
00368          16  FILLER              PIC X(19)       VALUE            ECS085
00369                  '  MONTHLY EXPOSURE '.                           ECS085
00370          16  P-AH-EXP            PIC Z,ZZZ,ZZZ,ZZZ.ZZ-.           ECS085
00371          16  FILLER              PIC XXX         VALUE SPACES.    ECS085
00372          16  FILLER              PIC X(35)       VALUE            ECS085
00373                  '  AGE WEIGHTED BY REMAINING BENEFIT'.           ECS085
00374          16  P-WT-REMAIN         PIC ZZZZZ.ZZ-.                   ECS085
00375  EJECT                                                            ECS085
00376  01  P-REC.                                                       ECS085
00377      12  P-CCSW                  PIC X.                           ECS085
00378      12  P-LN.                                                    ECS085
00379          16  P-BEN               PIC XXX.                         ECS085
00380          16  P-DESC              PIC X(10).                       ECS085
00381          16  P-COUNT             PIC ZZ,ZZZ,ZZZ-.                 ECS085
00382          16  P-DETAIL-1.                                          ECS085
00383              20  P-WRITTEN       PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00384              20  P-P78           PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00385              20  P-PRATA         PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00386              20  P-PDOMICILE     PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00387              20  P-PSTATE        PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00388              20  P-RESERV        PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00389              20  P-ALTRSV        PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00390              20  P-REMAIN        PIC ZZZZ,ZZZ,ZZZ,ZZZ-.           ECS085
00391          16  P-DETAIL-2 REDEFINES P-DETAIL-1.                     ECS085
00392              20  P-PAID          PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00393              20  P-C78           PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00394              20  P-CRATA         PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00395              20  P-TAX           PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00396              20  P-T78           PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00397              20  P-TRATA         PIC ZZZZ,ZZZ,ZZZ-.               ECS085
00398              20  FILLER          PIC X(30).                       ECS085
00399          16  FILLER              PIC X(3).                        ECS085
00400      12  P-DETAIL REDEFINES P-LN.                                 ECS085
00401          16  PD-CERT         PIC X(11).                           ECS085
00402          16  FILLER          PIC XX.                              ECS085
00403          16  PD-MO           PIC XX.                              ECS085
00404          16  PD-DASH-1       PIC X.                               ECS085
00405          16  PD-DA           PIC XX.                              ECS085
00406          16  PD-DASH-2       PIC X.                               ECS085
00407          16  PD-YR           PIC XX.                              ECS085
00408          16  FILLER          PIC X(3).                            ECS085
00409          16  PD-BEN-DESC     PIC X(10).                           ECS085
00410          16  FILLER          PIC X(3).                            ECS085
00411          16  PD-ORIG         PIC 999.                             ECS085
00412          16  FILLER          PIC X.                               ECS085
00413          16  PD-REM          PIC 999.                             ECS085
00414          16  FILLER          PIC X.                               ECS085
00415          16  PD-BEN          PIC ZZZ,ZZZ,ZZZ.99.                  ECS085
00416          16  PD-REMAMT       PIC ZZZ,ZZZ,ZZZ.99.                  ECS085
00417          16  PD-PREM         PIC Z,ZZZ,ZZZ.99.                    ECS085
00418          16  PD-RATA         PIC Z,ZZZ,ZZZ.99.                    ECS085
00419          16  PD-78           PIC Z,ZZZ,ZZZ.99.                    ECS085
00420          16  PD-STATE        PIC Z,ZZZ,ZZZ.99.                    ECS085
00421          16  FILLER          PIC X(14).                           ECS085
00422                                                                   ECS085
00423                              COPY ELCDTECX.                       ECS085
00424                                                                   ECS085
00425                              COPY ELCDTEVR.                       ECS085
00426                                                                   ECS085
00427                              COPY ELCGAPVR.                       ECS085
00428  EJECT                                                            ECS085
00429  PROCEDURE DIVISION.                                              ECS085
00430                                                                   ECS085
00431  0000-SET-START.                                                  ECS085
00432                              COPY ELCDTERX.                       ECS085
00433                                                                   ECS085
00434  0100-INTL-RTN.                                                   ECS085
00435      MOVE WS-CURRENT-DATE TO HD-RD.                               ECS085
00436      MOVE COMPANY-NAME    TO HD-CO.                               ECS085
00437      MOVE ALPH-DATE       TO HD-DT.                               ECS085
00438                                                                   ECS085
00439      MOVE AH-OVERRIDE-L2   TO PRT-SPC-BEN-TYPE.                   ECS085
00440                                                                   ECS085
00441      OPEN INPUT  ACCT-MSTR  GAAP-EXTR                             ECS085
00442           OUTPUT PRNTR.                                           ECS085
00443                                                                   ECS085
00444      IF AM-FILE-STATUS  = '00' OR '97'                            ECS085
00445          NEXT SENTENCE                                            ECS085
00446        ELSE                                                       ECS085
00447         MOVE ' ERROR ON ERACCTT, OPEN ' TO WS-ABEND-MESSAGE       ECS085
00448         MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS               ECS085
00449         GO TO ABEND-PGM.                                          ECS085
00450                                                                   ECS085
00451      MOVE SPACES    TO P-REC.                                     ECS085
00452      MOVE LOW-VALUE TO CUR-SEQ  WX-SEQ.                           ECS085
00453      MOVE +0        TO FST-SW  PGCTR.                             ECS085
00454      MOVE +121      TO LNCTR.                                     ECS085
00455                                                                   ECS085
00456      PERFORM 1100-LOAD-TABLES THRU 1199-LOAD-TAB-XIT.             ECS085
00457                                                                   ECS085
00458  0110-READ-RTN.                                                   ECS085
00459      READ GAAP-EXTR AT END                                        ECS085
00460          GO TO 1400-END-JOB.                                      ECS085
00461                                                                   ECS085
00462      IF GR-REIN NOT = 'P'                                         ECS085
00463          GO TO 0110-READ-RTN.                                     ECS085
00464                                                                   ECS085
           IF GR-STATE = 'MN'
              AND GR-AHTYP = '02'
              CONTINUE
           ELSE
              GO TO 0110-READ-RTN
           END-IF


      *    IF GR-STATE = 'TX'
      *       AND GR-ACCOUNT = '0000851900'
      *       CONTINUE
      *    ELSE
      *       GO TO 0110-READ-RTN
      *    END-IF

      *    IF GR-ACCOUNT NOT = '0000438400' AND '0000682700'
      *       AND '0000682701' AND '0000682702'
      *       GO TO 0110-READ-RTN
      *    END-IF
LGC191     IF DTE-PGM-OPT = '2'                                         ECS085
LGC191         IF SELECT-CT  >  3999                                    ECS085
LGC191             MOVE 1 TO SELECT-CT                                  ECS085
LGC191             GO TO 0110-CONTINUE                                  ECS085
LGC191         ELSE                                                     ECS085
LGC191             ADD 1 TO SELECT-CT                                   ECS085
LGC191             GO TO 0110-READ-RTN                                  ECS085
LGC191         END-IF                                                   ECS085
LGC191     END-IF.                                                      ECS085
LGC191                                                                  ECS085
LGC191 0110-CONTINUE.                                                   ECS085
LGC191                                                                  ECS085
00465      COPY ELCGAPM1.                                               ECS085
00466                                                                   ECS085
00467      MOVE GR-CONTROL TO WX-SEQ.                                   ECS085
00468                                                                   ECS085
00469      IF WX-SEQ NOT = CUR-SEQ                                      ECS085
00470          PERFORM 0300-BREAK-RTN THRU 0399-BREAK-XIT.              ECS085
00471                                                                   ECS085
00472      MOVE +1 TO X2.                                               ECS085
00473                                                                   ECS085
00474  0120-FIND-LIFE.                                                  ECS085
00475      IF GR-LFTYP = ZEROS                                          ECS085
00476          GO TO 0150-FIND-AH.                                      ECS085
00477                                                                   ECS085
00478      IF GR-SUMMARY-REC                                            ECS085
00479          MOVE GR-CNT-LF   TO X-COUNT                              ECS085
00480      ELSE                                                         ECS085
00481          MOVE +1          TO X-COUNT.                             ECS085
00482      MOVE GR-LFPRM    TO X-WRITTEN.                               ECS085
00483      MOVE GRR-LFPRM   TO X-P78.                                   ECS085
00484      MOVE GRP-LFPRM   TO X-PRATA.                                 ECS085
00485      MOVE GRD-LFPRM   TO X-DOMICILE.                              ECS085
00486      MOVE GRS-LFPRM   TO X-STATE.                                 ECS085
00487      MOVE GR-RESV     TO X-RESERV.                                ECS085
00488      MOVE GR-ALT-RESV TO X-ALTRSV.                                ECS085
00489      MOVE GR-REM-AMT  TO X-REMAIN.                                ECS085
00490      MOVE GR-LFCOM    TO X-PAID.                                  ECS085
00491      MOVE GRR-LFCOM   TO X-C78.                                   ECS085
00492      MOVE GRP-LFCOM   TO X-CRATA.                                 ECS085
00493      MOVE GR-LFTAX    TO X-TAX.                                   ECS085
00494      MOVE GRR-LFTAX   TO X-T78.                                   ECS085
00495      MOVE GRP-LFTAX   TO X-TRATA.                                 ECS085
00496      MOVE GR-LFTYP    TO WX-BEN.                                  ECS085
00497      MOVE +1          TO WX-TYP.                                  ECS085
00498                                                                   ECS085
00499      IF WX-POINTER = SAVE-LPOINTER                                ECS085
00500          MOVE SAVE-LX2 TO X2.                                     ECS085
00501                                                                   ECS085
00502      COMPUTE X-CALC-REMAIN ROUNDED = GR-AGE * GR-REM-AMT.         ECS085
00503                                                                   ECS085
00504      ADD X-CALC-REMAIN TO ACCT-AGE-REMAIN.                        ECS085
00505                                                                   ECS085
00506  0130-LOOP-LF.                                                    ECS085
00507      IF X2 GREATER THAN MAX-BEN                                   ECS085
00508         MOVE 0401 TO WS-RETURN-CODE                               ECS085
00509         MOVE ' INVALID XXXXXX BENEFIT TYPE ' TO WS-ABEND-MESSAGE  ECS085
00510         MOVE LIFE-OVERRIDE-L6  TO WS-ABEND-BEN-TYPE               ECS085
00511         GO TO ABEND-PGM.                                          ECS085
00512                                                                   ECS085
00513      IF WX-POINTER NOT = X-POINTER (X2)                           ECS085
00514          ADD +1 TO X2                                             ECS085
00515          GO TO 0130-LOOP-LF.                                      ECS085
00516                                                                   ECS085
00517      MOVE WX-POINTER  TO SAVE-LPOINTER.                           ECS085
00518      MOVE X2          TO SAVE-LX2.                                ECS085
00519      MOVE X-DESC (X2) TO SAVE-LDESC.                              ECS085
00520                                                                   ECS085
00521      MOVE X-AMTS (X2) TO R-DETL.                                  ECS085
00522      ADD X-COUNT      TO R-COUNT.                                 ECS085
00523      ADD X-WRITTEN    TO R-WRITTEN.                               ECS085
00524      ADD X-P78        TO R-P78.                                   ECS085
00525      ADD X-PRATA      TO R-PRATA.                                 ECS085
00526      ADD X-DOMICILE   TO R-DOMICILE.                              ECS085
00527      ADD X-STATE      TO R-STATE.                                 ECS085
00528      ADD X-RESERV     TO R-RESERV.                                ECS085
00529      ADD X-ALTRSV     TO R-ALTRSV.                                ECS085
00530      ADD X-REMAIN     TO R-REMAIN.                                ECS085
00531      ADD X-PAID       TO R-PAID.                                  ECS085
00532      ADD X-C78        TO R-C78.                                   ECS085
00533      ADD X-CRATA      TO R-CRATA.                                 ECS085
00534      ADD X-TAX        TO R-TAX.                                   ECS085
00535      ADD X-T78        TO R-T78.                                   ECS085
00536      ADD X-TRATA      TO R-TRATA.                                 ECS085
00537      MOVE R-DETL      TO X-AMTS (X2).                             ECS085
00538                                                                   ECS085
00539  0140-END-LIFE.                                                   ECS085
00540      IF GR-AHTYP NOT = ZEROS                                      ECS085
00541          ADD +1 TO X-DUP.                                         ECS085
00542                                                                   ECS085
00543  0150-FIND-AH.                                                    ECS085
00544      IF GR-AHTYP = ZEROS                                          ECS085
00545          GO TO 0170-BRANCH-BACK.                                  ECS085
00546                                                                   ECS085
00547      IF GR-SUMMARY-REC                                            ECS085
00548          MOVE GR-CNT-AH   TO X-COUNT                              ECS085
00549      ELSE                                                         ECS085
00550          MOVE +1          TO X-COUNT.                             ECS085
00551      MOVE GR-AHPRM  TO X-WRITTEN.                                 ECS085
00552      MOVE GRR-AHPRM TO X-P78.                                     ECS085
00553      MOVE GRP-AHPRM TO X-PRATA.                                   ECS085
00554      MOVE GRD-AHPRM TO X-DOMICILE.                                ECS085
00555      MOVE GRS-AHPRM TO X-STATE.                                   ECS085
00556      MOVE GR-AHCOM  TO X-PAID.                                    ECS085
00557      MOVE GRR-AHCOM TO X-C78.                                     ECS085
00558      MOVE GRP-AHCOM TO X-CRATA.                                   ECS085
00559      MOVE GR-AHTAX  TO X-TAX.                                     ECS085
00560      MOVE GRR-AHTAX TO X-T78.                                     ECS085
00561      MOVE GRP-AHTAX TO X-TRATA.                                   ECS085
00562                                                                   ECS085
00563      COMPUTE X-REMAIN ROUNDED = GR-AHBEN * GR-AH-REMTERM.         ECS085
00564                                                                   ECS085
00565      MOVE +0       TO X-RESERV.                                   ECS085
00566      MOVE GR-AHTYP TO WX-BEN.                                     ECS085
00567      MOVE +2       TO WX-TYP.                                     ECS085
00568                                                                   ECS085
00569      IF WX-POINTER = SAVE-APOINTER                                ECS085
00570          MOVE SAVE-AX2 TO X2.                                     ECS085
00571                                                                   ECS085
00572      ADD GR-AHBEN TO ACCT-AH-EXP.                                 ECS085
00573                                                                   ECS085
00574  0160-LOOP-HEALTH.                                                ECS085
00575      IF X2 GREATER THAN MAX-BEN                                   ECS085
00576         MOVE 0402 TO WS-RETURN-CODE                               ECS085
00577         MOVE ' INVALID XXXXXX BENEFIT TYPE ' TO WS-ABEND-MESSAGE  ECS085
00578         MOVE AH-OVERRIDE-L6  TO WS-ABEND-BEN-TYPE                 ECS085
00579         GO TO ABEND-PGM.                                          ECS085
00580                                                                   ECS085
00581      IF WX-POINTER NOT = X-POINTER (X2)                           ECS085
00582          ADD +1 TO X2                                             ECS085
00583          GO TO 0160-LOOP-HEALTH.                                  ECS085
00584                                                                   ECS085
00585      MOVE WX-POINTER  TO SAVE-APOINTER.                           ECS085
00586      MOVE X2          TO SAVE-AX2.                                ECS085
00587      MOVE X-DESC (X2) TO SAVE-ADESC.                              ECS085
00588                                                                   ECS085
00589      MOVE X-AMTS (X2) TO R-DETL.                                  ECS085
00590      ADD X-COUNT      TO R-COUNT.                                 ECS085
00591      ADD X-WRITTEN    TO R-WRITTEN.                               ECS085
00592      ADD X-P78        TO R-P78.                                   ECS085
00593      ADD X-PRATA      TO R-PRATA.                                 ECS085
00594      ADD X-DOMICILE   TO R-DOMICILE.                              ECS085
00595      ADD X-STATE      TO R-STATE.                                 ECS085
00596      ADD X-RESERV     TO R-RESERV.                                ECS085
00597      ADD X-ALTRSV     TO R-ALTRSV.                                ECS085
00598      ADD X-REMAIN     TO R-REMAIN.                                ECS085
00599      ADD X-PAID       TO R-PAID.                                  ECS085
00600      ADD X-C78        TO R-C78.                                   ECS085
00601      ADD X-CRATA      TO R-CRATA.                                 ECS085
00602      ADD X-TAX        TO R-TAX.                                   ECS085
00603      ADD X-T78        TO R-T78.                                   ECS085
00604      ADD X-TRATA      TO R-TRATA.                                 ECS085
00605      MOVE R-DETL      TO X-AMTS (X2).                             ECS085
00606                                                                   ECS085
00607  0170-BRANCH-BACK.                                                ECS085
00608                                                                   ECS085
00609      PERFORM 0200-PRT-DTL-RTN THRU 0299-PRT-DTL-XIT.              ECS085
00610                                                                   ECS085
00611      GO TO 0110-READ-RTN.                                         ECS085
00612  EJECT                                                            ECS085
00613  0200-PRT-DTL-RTN.                                                ECS085
CIDMOD*    IF LNCTR GREATER THAN +58                                    ECS085
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS085
00615          PERFORM 1200-HD-RTN THRU 1299-HD-XIT                     ECS085
00616          MOVE '0' TO P-CCSW                                       ECS085
00617          MOVE DTL-4 TO P-LN                                       ECS085
00618          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
00619          MOVE DTL-5 TO P-LN                                       ECS085
00620          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
00621          MOVE '0' TO P-CCSW.                                      ECS085
00622                                                                   ECS085
00623      MOVE GR-CERT-NO TO PD-CERT.                                  ECS085
00624      MOVE GR-MO     TO PD-MO.                                     ECS085
00625      MOVE '-'       TO PD-DASH-1.                                 ECS085
00626      MOVE GR-DA     TO PD-DA.                                     ECS085
00627      MOVE '-'       TO PD-DASH-2.                                 ECS085
00628      MOVE GR-YR     TO PD-YR.                                     ECS085
00629                                                                   ECS085
00630      IF GR-LFTYP = ZEROS                                          ECS085
00631          GO TO 0210-SKIP-LIFE-DTL.                                ECS085
00632                                                                   ECS085
00633      MOVE SAVE-LDESC TO PD-BEN-DESC.                              ECS085
00634      MOVE GR-LF-UP-REMTERM TO PD-REM.                             ECS085
00635      MOVE GR-LF-TERM TO PD-ORIG.                                  ECS085
00636      MOVE GR-LFBEN   TO PD-BEN.                                   ECS085
00637      MOVE GR-REM-AMT TO PD-REMAMT.                                ECS085
00638      MOVE GR-LFPRM   TO PD-PREM.                                  ECS085
00639      MOVE GRP-LFPRM  TO PD-RATA.                                  ECS085
00640      MOVE GRR-LFPRM  TO PD-78.                                    ECS085
00641      MOVE GRS-LFPRM  TO PD-STATE.                                 ECS085
00642                                                                   ECS085
00643      PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT.                      ECS085
00644                                                                   ECS085
00645  0210-SKIP-LIFE-DTL.                                              ECS085
00646      IF GR-AHTYP = ZEROS                                          ECS085
00647          GO TO 0299-PRT-DTL-XIT.                                  ECS085
00648                                                                   ECS085
00649      IF GR-LFTYP NOT = ZEROS                                      ECS085
00650         MOVE SPACES TO P-DETAIL.                                  ECS085
00651                                                                   ECS085
00652      MOVE SAVE-ADESC TO PD-BEN-DESC.                              ECS085
00653      MOVE GR-AH-UP-REMTERM TO PD-REM.                             ECS085
00654      MOVE GR-AH-TERM TO PD-ORIG.                                  ECS085
00655      COMPUTE PD-REMAMT = (GR-AHBEN * GR-AH-REMTERM).              ECS085
00656      MOVE GR-AHBEN  TO PD-BEN.                                    ECS085
00657      MOVE GR-AHPRM  TO PD-PREM.                                   ECS085
00658      MOVE GRP-AHPRM TO PD-RATA.                                   ECS085
00659      MOVE GRR-AHPRM TO PD-78.                                     ECS085
00660      MOVE GRS-AHPRM TO PD-STATE.                                  ECS085
00661                                                                   ECS085
00662      PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT.                      ECS085
00663                                                                   ECS085
00664  0299-PRT-DTL-XIT.                                                ECS085
00665      EXIT.                                                        ECS085
00666  EJECT                                                            ECS085
00667  0300-BREAK-RTN.                                                  ECS085
00668      IF FST-SW = +0                                               ECS085
00669          MOVE +1 TO FST-SW                                        ECS085
00670          GO TO 0330-INTL-FINAL.                                   ECS085
00671                                                                   ECS085
00672  0305-ACCT-BREAK.                                                 ECS085
00673      PERFORM 1000-ROLL-RTN THRU 1099-ROLL-XIT.                    ECS085
00674                                                                   ECS085
00675      ADD ACCT-AGE-REMAIN TO ST-AGE-REMAIN.                        ECS085
00676      ADD ACCT-AH-EXP     TO ST-AH-EXP.                            ECS085
00677      MOVE ACCT-WORK      TO CERT-WORK.                            ECS085
00678      ADD X-DUP           TO ST-DUP.                               ECS085
00679                                                                   ECS085
00680      PERFORM 0400-BLD-RTN THRU 0499-BLD-XIT.                      ECS085
00681                                                                   ECS085
00682      MOVE WX-ACCT TO CUR-ACCT.                                    ECS085
00683      MOVE SPACES  TO HD3A.                                        ECS085
00684                                                                   ECS085
00685      IF CUR-SEQ = WX-SEQ                                          ECS085
00686          GO TO 0355-INTL-ACCT.                                    ECS085
00687                                                                   ECS085
00688  0310-ST-BREAK.                                                   ECS085
00689      MOVE ST-HD2    TO SUB-HD2.                                   ECS085
00690      MOVE ST-HD3    TO SUB-HD3.                                   ECS085
00691      MOVE ST-TOTALS TO X-TOTALS.                                  ECS085
00692      MOVE CO-TOTALS TO ST-TOTALS.                                 ECS085
00693                                                                   ECS085
00694      PERFORM 1000-ROLL-RTN THRU 1099-ROLL-XIT.                    ECS085
00695                                                                   ECS085
00696      MOVE ST-TOTALS    TO CO-TOTALS.                              ECS085
00697      ADD ST-AGE-REMAIN TO CO-AGE-REMAIN.                          ECS085
00698      ADD ST-AH-EXP     TO CO-AH-EXP.                              ECS085
00699      MOVE STATE-WORK   TO CERT-WORK.                              ECS085
00700      ADD ST-DUP        TO CO-DUP.                                 ECS085
00701      MOVE ST-DUP       TO X-DUP.                                  ECS085
00702                                                                   ECS085
00703      PERFORM 0400-BLD-RTN THRU 0499-BLD-XIT.                      ECS085
00704                                                                   ECS085
00705      MOVE WX-ST TO CUR-ST.                                        ECS085
00706                                                                   ECS085
00707      IF CUR-SEQ = WX-SEQ                                          ECS085
00708          GO TO 0345-INTL-ST.                                      ECS085
00709                                                                   ECS085
00710  0315-CO-BREAK.                                                   ECS085
00711      MOVE CO-HD2      TO SUB-HD2.                                 ECS085
00712      MOVE CO-HD3      TO SUB-HD3.                                 ECS085
00713      MOVE CO-TOTALS   TO X-TOTALS.                                ECS085
00714      MOVE CARR-TOTALS TO ST-TOTALS.                               ECS085
00715                                                                   ECS085
00716      PERFORM 1000-ROLL-RTN THRU 1099-ROLL-XIT.                    ECS085
00717                                                                   ECS085
00718      MOVE ST-TOTALS    TO CARR-TOTALS.                            ECS085
00719      ADD CO-AGE-REMAIN TO CARR-AGE-REMAIN.                        ECS085
00720      ADD CO-AH-EXP     TO CARR-AH-EXP.                            ECS085
00721      MOVE COMPANY-WORK TO CERT-WORK.                              ECS085
00722      ADD CO-DUP        TO CARR-DUP.                               ECS085
00723      MOVE CO-DUPS      TO X-DUPS.                                 ECS085
00724                                                                   ECS085
00725      PERFORM 0400-BLD-RTN THRU 0499-BLD-XIT.                      ECS085
00726                                                                   ECS085
00727      MOVE WX-CO TO CUR-CO.                                        ECS085
00728                                                                   ECS085
00729      IF WX-SEQ = CUR-SEQ                                          ECS085
00730          GO TO 0340-INTL-CO.                                      ECS085
00731                                                                   ECS085
00732  0320-CARR-BREAK.                                                 ECS085
00733      MOVE CARR-HD2     TO SUB-HD2.                                ECS085
00734      MOVE CARR-HD3     TO SUB-HD3.                                ECS085
00735      MOVE CARR-TOTALS  TO X-TOTALS.                               ECS085
00736      MOVE FINAL-TOTALS TO ST-TOTALS.                              ECS085
00737                                                                   ECS085
00738      PERFORM 1000-ROLL-RTN THRU 1099-ROLL-XIT.                    ECS085
00739                                                                   ECS085
00740      MOVE ST-TOTALS      TO FINAL-TOTALS.                         ECS085
00741      ADD CARR-AGE-REMAIN TO FINAL-AGE-REMAIN.                     ECS085
00742      ADD CARR-AH-EXP     TO FINAL-AH-EXP.                         ECS085
00743      MOVE CARRIER-WORK   TO CERT-WORK.                            ECS085
00744      ADD CARR-DUP        TO FINL-DUP.                             ECS085
00745      MOVE CARR-DUPS      TO X-DUPS.                               ECS085
00746                                                                   ECS085
00747      PERFORM 0400-BLD-RTN THRU 0499-BLD-XIT.                      ECS085
00748                                                                   ECS085
00749      IF FST-SW NOT = +2                                           ECS085
00750          GO TO 0335-INTL-CARR.                                    ECS085
00751                                                                   ECS085
00752  0325-FINAL-BREAK.                                                ECS085
00753      MOVE 'FINAL TOTALS' TO HD3A.                                 ECS085
00754      MOVE SPACES         TO SUB-HD2                               ECS085
00755                             SUB-HD3.                              ECS085
00756      MOVE FINAL-TOTALS   TO X-TOTALS.                             ECS085
00757      MOVE FINAL-WORK     TO CERT-WORK.                            ECS085
00758      MOVE FINL-DUPS      TO X-DUPS.                               ECS085
00759                                                                   ECS085
00760      PERFORM 0400-BLD-RTN THRU 0499-BLD-XIT.                      ECS085
00761                                                                   ECS085
00762      GO TO 0399-BREAK-XIT.                                        ECS085
00763                                                                   ECS085
00764  0330-INTL-FINAL.                                                 ECS085
00765      PERFORM 0900-ZERO-RTN THRU 0999-ZERO-XIT.                    ECS085
00766                                                                   ECS085
00767      MOVE X-TOTALS TO FINAL-TOTALS.                               ECS085
00768      MOVE +0       TO FINAL-AGE-REMAIN  FINAL-AH-EXP              ECS085
00769                       FINL-DUP.                                   ECS085
00770                                                                   ECS085
00771  0335-INTL-CARR.                                                  ECS085
00772      MOVE WX-CARR TO CUR-CARR  HD-CARR.                           ECS085
00773                                                                   ECS085
00774      IF ZERO-SW NOT = +0                                          ECS085
00775          PERFORM 0900-ZERO-RTN THRU 0999-ZERO-XIT.                ECS085
00776                                                                   ECS085
00777      MOVE X-TOTALS TO CARR-TOTALS.                                ECS085
00778      MOVE +0       TO CARR-AGE-REMAIN  CARR-AH-EXP                ECS085
00779                       CARR-DUP.                                   ECS085
00780                                                                   ECS085
00781  0340-INTL-CO.                                                    ECS085
00782      MOVE WX-CO TO CUR-CO  HD-COMP.                               ECS085
00783                                                                   ECS085
00784      IF ZERO-SW NOT = +0                                          ECS085
00785          PERFORM 0900-ZERO-RTN THRU 0999-ZERO-XIT.                ECS085
00786                                                                   ECS085
00787      MOVE X-TOTALS TO CO-TOTALS.                                  ECS085
00788      MOVE +0       TO CO-AGE-REMAIN  CO-AH-EXP                    ECS085
00789                       CO-DUP.                                     ECS085
00790                                                                   ECS085
00791  0345-INTL-ST.                                                    ECS085
00792      MOVE WX-ST TO CUR-ST  HD-ST  STATE-L.                        ECS085
00793                                                                   ECS085
00794      IF ZERO-SW NOT = +0                                          ECS085
00795          PERFORM 0900-ZERO-RTN THRU 0999-ZERO-XIT.                ECS085
00796                                                                   ECS085
00797      MOVE X-TOTALS        TO ST-TOTALS.                           ECS085
00798      MOVE +0              TO ST-AGE-REMAIN  ST-AH-EXP             ECS085
00799                              ST-DUP.                              ECS085
00800      MOVE CLAS-STARTS     TO CLAS-INDEXS.                         ECS085
00801      MOVE 'INVALID STATE' TO HD-ST-NM.                            ECS085
00802                                                                   ECS085
00803  0350-FIND-ST-DESC.                                               ECS085
00804      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                        ECS085
00805          GO TO 0355-INTL-ACCT.                                    ECS085
00806                                                                   ECS085
00807      IF STATE-SUB (CLAS-INDEXS) NOT = STATE-L                     ECS085
00808          ADD +1 TO CLAS-INDEXS                                    ECS085
00809          GO TO 0350-FIND-ST-DESC.                                 ECS085
00810                                                                   ECS085
00811      MOVE STATE-PIC (CLAS-INDEXS) TO HD-ST-NM.                    ECS085
00812                                                                   ECS085
00813  0355-INTL-ACCT.                                                  ECS085
00814      MOVE WX-ACCT TO CUR-ACCT  HD-ACCT.                           ECS085
00815                                                                   ECS085
00816      IF ZERO-SW NOT = +0                                          ECS085
00817          PERFORM 0900-ZERO-RTN THRU 0999-ZERO-XIT.                ECS085
00818                                                                   ECS085
00819      MOVE +0 TO ACCT-AGE-REMAIN  ACCT-AH-EXP.                     ECS085
00820      MOVE +0 TO X-DUP.                                            ECS085
00821      MOVE +1 TO ZERO-SW.                                          ECS085
00822                                                                   ECS085
00823  0360-FIND-ACCT-NM.                                               ECS085
00824      IF ACCT-SEQ GREATER THAN CUR-SEQ                             ECS085
00825          MOVE 'INVALID ACCOUNT' TO ACCOUNT-NAME                   ECS085
00826          MOVE ACCT-SUMM-HD TO HD3A                                ECS085
00827          GO TO 0370-SET-ACCT-HD.                                  ECS085
00828                                                                   ECS085
00829      IF ACCT-SEQ = CUR-SEQ                                        ECS085
00830          MOVE AM-NAME TO ACCOUNT-NAME                             ECS085
00831          MOVE ACCT-SUMM-HD TO HD3A                                ECS085
00832          GO TO 0370-SET-ACCT-HD.                                  ECS085
00833                                                                   ECS085
00834  0365-READ-ACCT.                                                  ECS085
00835      READ ACCT-MSTR.                                              ECS085
00836      IF AM-FILE-STATUS = '10'                                     ECS085
00837         MOVE HIGH-VALUES TO ACCT-SEQ                              ECS085
00838         GO TO 0360-FIND-ACCT-NM.                                  ECS085
00839                                                                   ECS085
00840      IF AM-FILE-STATUS NOT = '00'                                 ECS085
00841         MOVE ' ERROR ON ERACCTT, READ ' TO WS-ABEND-MESSAGE       ECS085
00842         MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS               ECS085
00843         GO TO ABEND-PGM.                                          ECS085
00844                                                                   ECS085
00845      MOVE AM-CONTROL-A TO ACCT-SEQ.                               ECS085
00846                                                                   ECS085
00847      GO TO 0360-FIND-ACCT-NM.                                     ECS085
00848                                                                   ECS085
00849  0370-SET-ACCT-HD.                                                ECS085
00850      MOVE ACCT-HD2 TO SUB-HD2.                                    ECS085
00851      MOVE ACCT-HD3 TO SUB-HD3.                                    ECS085
00852      MOVE +121     TO LNCTR.                                      ECS085
00853                                                                   ECS085
00854  0399-BREAK-XIT.                                                  ECS085
00855      EXIT.                                                        ECS085
00856  EJECT                                                            ECS085
00857  0400-BLD-RTN.                                                    ECS085
LGC191     IF DTE-PGM-OPT = '2'                                         ECS085
LGC191         GO TO 0499-BLD-XIT                                       ECS085
LGC191     END-IF.                                                      ECS085
00858      PERFORM 1200-HD-RTN THRU 1299-HD-XIT.                        ECS085
00859                                                                   ECS085
00860      MOVE +0      TO X-WT-REMAIN.                                 ECS085
00861      MOVE 'GROSS' TO HD-DESC.                                     ECS085
00862      MOVE '-' TO P-CCSW.                                          ECS085
00863                                                                   ECS085
00864      PERFORM 0500-PRT-EXTRACT THRU 0599-PRT-EXTRACT-XIT.          ECS085
00865                                                                   ECS085
00866  0410-BLD-SPECIAL.                                                ECS085
00867      MOVE X-AH-EXP    TO P-AH-EXP.                                ECS085
00868      MOVE X-WT-REMAIN TO P-WT-REMAIN.                             ECS085
00869      MOVE PRT-SPECIAL TO P-LN.                                    ECS085
00870      MOVE '-'         TO P-CCSW.                                  ECS085
00871                                                                   ECS085
00872      PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT.                      ECS085
00873                                                                   ECS085
00874  0499-BLD-XIT.                                                    ECS085
00875      EXIT.                                                        ECS085
00876  EJECT                                                            ECS085
00877  0500-PRT-EXTRACT.                                                ECS085
00878      MOVE +0 TO LIFE-SW                                           ECS085
00879                 AH-SW                                             ECS085
00880                 HD-SW                                             ECS085
00881                 S-COUNT                                           ECS085
00882                 S-WRITTEN                                         ECS085
00883                 S-P78                                             ECS085
00884                 S-PRATA                                           ECS085
00885                 S-DOMICILE                                        ECS085
00886                 S-STATE                                           ECS085
00887                 S-RESERV                                          ECS085
00888                 S-ALTRSV                                          ECS085
00889                 S-REMAIN.                                         ECS085
00890      MOVE +1 TO X2.                                               ECS085
00891                                                                   ECS085
00892  0510-LOOP-LIFE.                                                  ECS085
00893      IF X2 GREATER THAN MAX-BEN                                   ECS085
00894          GO TO 0520-OUT-LIFE.                                     ECS085
00895                                                                   ECS085
00896      IF X-TYP (X2) NOT = 1                                        ECS085
00897          GO TO 0520-OUT-LIFE.                                     ECS085
00898                                                                   ECS085
00899      MOVE X-AMTS (X2) TO X-DETL.                                  ECS085
00900                                                                   ECS085
00901      IF X-COUNT = +0                                              ECS085
00902          ADD +1 TO X2                                             ECS085
00903          GO TO 0510-LOOP-LIFE.                                    ECS085
00904                                                                   ECS085
CIDMOD*    IF LNCTR GREATER THAN +58                                    ECS085
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS085
00906         MOVE +0       TO HD-SW                                    ECS085
00907         PERFORM 1200-HD-RTN THRU 1299-HD-XIT.                     ECS085
00908                                                                   ECS085
00909      IF HD-SW = +0                                                ECS085
00910          MOVE +1      TO LIFE-SW  HD-SW                           ECS085
00911          MOVE HD4     TO P-LN                                     ECS085
00912          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
00913          MOVE HD5     TO P-LN                                     ECS085
00914          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
00915          MOVE HD6     TO P-LN                                     ECS085
00916          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
00917          MOVE '0' TO P-CCSW.                                      ECS085
00918                                                                   ECS085
00919      MOVE X-BEN (X2)  TO PX-BEN.                                  ECS085
00920      MOVE X-DESC (X2) TO PX-DESC.                                 ECS085
00921      ADD X-COUNT      TO S-COUNT.                                 ECS085
00922      ADD X-WRITTEN    TO S-WRITTEN.                               ECS085
00923      ADD X-P78        TO S-P78.                                   ECS085
00924      ADD X-PRATA      TO S-PRATA.                                 ECS085
00925      ADD X-DOMICILE   TO S-DOMICILE.                              ECS085
00926      ADD X-STATE      TO S-STATE.                                 ECS085
00927      ADD X-RESERV     TO S-RESERV.                                ECS085
00928      ADD X-ALTRSV     TO S-ALTRSV.                                ECS085
00929      ADD X-REMAIN     TO S-REMAIN.                                ECS085
00930                                                                   ECS085
00931      PERFORM 0600-PRT-LINE THRU 0699-PRT-LINE-XIT.                ECS085
00932                                                                   ECS085
00933      ADD +1 TO X2.                                                ECS085
00934                                                                   ECS085
00935      GO TO 0510-LOOP-LIFE.                                        ECS085
00936                                                                   ECS085
00937  0520-OUT-LIFE.                                                   ECS085
00938      MOVE SUB-TOTALS TO TOTALS.                                   ECS085
00939                                                                   ECS085
00940      IF LIFE-SW = +0                                              ECS085
00941          GO TO 0540-LOOP-AH.                                      ECS085
00942                                                                   ECS085
00943      MOVE 'TOTAL     ' TO PX-DESC.                                ECS085
00944      MOVE LIFE-OVERRIDE-L2 TO PX-BEN-DESC.                        ECS085
00945      MOVE SPACES       TO PX-BEN.                                 ECS085
00946      MOVE SUB-TOTALS   TO X-DETL.                                 ECS085
00947                                                                   ECS085
00948      PERFORM 0600-PRT-LINE THRU 0699-PRT-LINE-XIT.                ECS085
00949                                                                   ECS085
00950      MOVE '0' TO P-CCSW.                                          ECS085
00951                                                                   ECS085
00952      IF S-REMAIN NOT = ZERO                                       ECS085
00953          DIVIDE S-REMAIN INTO X-CALC-REMAIN                       ECS085
00954              GIVING X-WT-REMAIN ROUNDED                           ECS085
00955      ELSE                                                         ECS085
00956          MOVE ZERO TO X-WT-REMAIN.                                ECS085
00957                                                                   ECS085
00958  0530-ZERO-SUBTOTAL.                                              ECS085
00959      MOVE +0 TO S-COUNT                                           ECS085
00960                 S-WRITTEN                                         ECS085
00961                 S-P78                                             ECS085
00962                 S-PRATA                                           ECS085
00963                 S-DOMICILE                                        ECS085
00964                 S-STATE                                           ECS085
00965                 S-RESERV                                          ECS085
00966                 S-ALTRSV                                          ECS085
00967                 S-REMAIN.                                         ECS085
00968                                                                   ECS085
00969  0540-LOOP-AH.                                                    ECS085
00970      IF X2 GREATER THAN MAX-BEN                                   ECS085
00971          GO TO 0550-OUT-AH.                                       ECS085
00972                                                                   ECS085
00973      IF X-TYP (X2) NOT = 2                                        ECS085
00974          GO TO 0540-LOOP-AH.                                      ECS085
00975                                                                   ECS085
00976      MOVE X-AMTS (X2) TO X-DETL.                                  ECS085
00977                                                                   ECS085
00978      IF X-COUNT = +0                                              ECS085
00979          ADD +1 TO X2                                             ECS085
00980          GO TO 0540-LOOP-AH.                                      ECS085
00981                                                                   ECS085
CIDMOD*    IF LNCTR GREATER THAN +58                                    ECS085
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS085
00983         MOVE +0       TO HD-SW                                    ECS085
00984         PERFORM 1200-HD-RTN THRU 1299-HD-XIT.                     ECS085
00985                                                                   ECS085
00986      IF HD-SW = +0                                                ECS085
00987          MOVE +1  TO HD-SW                                        ECS085
00988          MOVE HD4 TO P-LN                                         ECS085
00989          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
00990          MOVE HD5 TO P-LN                                         ECS085
00991          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
00992          MOVE HD6 TO P-LN                                         ECS085
00993          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
00994          MOVE '0' TO P-CCSW.                                      ECS085
00995                                                                   ECS085
00996      MOVE +1          TO AH-SW.                                   ECS085
00997      MOVE X-BEN (X2)  TO PX-BEN.                                  ECS085
00998      MOVE X-DESC (X2) TO PX-DESC.                                 ECS085
00999      ADD X-COUNT      TO S-COUNT.                                 ECS085
01000      ADD X-WRITTEN    TO S-WRITTEN.                               ECS085
01001      ADD X-P78        TO S-P78.                                   ECS085
01002      ADD X-PRATA      TO S-PRATA.                                 ECS085
01003      ADD X-DOMICILE   TO S-DOMICILE.                              ECS085
01004      ADD X-STATE      TO S-STATE.                                 ECS085
01005      ADD X-RESERV     TO S-RESERV.                                ECS085
01006      ADD X-ALTRSV     TO S-ALTRSV.                                ECS085
01007      ADD X-REMAIN     TO S-REMAIN.                                ECS085
01008                                                                   ECS085
01009      PERFORM 0600-PRT-LINE THRU 0699-PRT-LINE-XIT.                ECS085
01010                                                                   ECS085
01011      ADD +1 TO X2.                                                ECS085
01012                                                                   ECS085
01013      GO TO 0540-LOOP-AH.                                          ECS085
01014                                                                   ECS085
01015  0550-OUT-AH.                                                     ECS085
01016      IF AH-SW = +0                                                ECS085
01017          GO TO 0590-PRE-XIT.                                      ECS085
01018                                                                   ECS085
01019      MOVE 'TOTAL    ' TO PX-DESC.                                 ECS085
01020      MOVE AH-OVERRIDE-L2 TO PX-BEN-DESC.                          ECS085
01021      MOVE SPACES      TO PX-BEN.                                  ECS085
01022      MOVE SUB-TOTALS  TO X-DETL.                                  ECS085
01023                                                                   ECS085
01024      PERFORM 0600-PRT-LINE THRU 0699-PRT-LINE-XIT.                ECS085
01025                                                                   ECS085
01026      MOVE '0'       TO P-CCSW.                                    ECS085
01027      ADD S-COUNT    TO T-COUNT.                                   ECS085
01028      ADD S-WRITTEN  TO T-WRITTEN.                                 ECS085
01029      ADD S-P78      TO T-P78.                                     ECS085
01030      ADD S-PRATA    TO T-PRATA.                                   ECS085
01031      ADD S-DOMICILE TO T-DOMICILE.                                ECS085
01032      ADD S-STATE    TO T-STATE.                                   ECS085
01033      ADD S-RESERV   TO T-RESERV.                                  ECS085
01034      ADD S-ALTRSV   TO T-ALTRSV.                                  ECS085
01035      ADD S-REMAIN   TO T-REMAIN.                                  ECS085
01036                                                                   ECS085
01037  0560-PRT-TOTALS.                                                 ECS085
01038      IF LIFE-SW = +0                                              ECS085
01039          GO TO 0590-PRE-XIT.                                      ECS085
01040                                                                   ECS085
01041      MOVE TOTALS      TO X-DETL.                                  ECS085
01042      SUBTRACT X-DUP   FROM X-COUNT.                               ECS085
01043      MOVE '  TOTAL'   TO PX-DESC.                                 ECS085
01044      MOVE SPACES      TO PX-BEN.                                  ECS085
01045                                                                   ECS085
01046      PERFORM 0600-PRT-LINE THRU 0699-PRT-LINE-XIT.                ECS085
01047                                                                   ECS085
01048  0590-PRE-XIT.                                                    ECS085
01049      PERFORM 0700-PRT-EXTRACT THRU 0799-PRT-EXTRACT-XIT.          ECS085
01050                                                                   ECS085
01051  0599-PRT-EXTRACT-XIT.                                            ECS085
01052      EXIT.                                                        ECS085
01053  EJECT                                                            ECS085
01054  0600-PRT-LINE.                                                   ECS085
01055      MOVE PX-BEN     TO P-BEN.                                    ECS085
01056      MOVE PX-DESC    TO P-DESC.                                   ECS085
01057      MOVE X-COUNT    TO P-COUNT.                                  ECS085
01058      MOVE X-WRITTEN  TO P-WRITTEN.                                ECS085
01059      MOVE X-P78      TO P-P78.                                    ECS085
01060      MOVE X-PRATA    TO P-PRATA.                                  ECS085
01061      MOVE X-DOMICILE TO P-PDOMICILE.                              ECS085
01062      MOVE X-STATE    TO P-PSTATE.                                 ECS085
01063      MOVE X-RESERV   TO P-RESERV.                                 ECS085
01064      MOVE X-ALTRSV   TO P-ALTRSV.                                 ECS085
01065      MOVE X-REMAIN   TO P-REMAIN.                                 ECS085
01066                                                                   ECS085
01067      PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT.                      ECS085
01068                                                                   ECS085
01069  0699-PRT-LINE-XIT.                                               ECS085
01070      EXIT.                                                        ECS085
01071                                                                   ECS085
01072  0700-PRT-EXTRACT.                                                ECS085
01073      MOVE +0 TO LIFE-SW                                           ECS085
01074                 AH-SW                                             ECS085
01075                 HD-SW                                             ECS085
01076                 S-PAID                                            ECS085
01077                 S-C78                                             ECS085
01078                 S-CRATA                                           ECS085
01079                 S-TAX                                             ECS085
01080                 S-T78                                             ECS085
01081                 S-TRATA.                                          ECS085
01082      MOVE +1 TO X2.                                               ECS085
01083                                                                   ECS085
01084  0710-LOOP-LIFE.                                                  ECS085
01085      IF X2 GREATER THAN MAX-BEN                                   ECS085
01086          GO TO 0720-OUT-LIFE.                                     ECS085
01087                                                                   ECS085
01088      IF X-TYP (X2) NOT = 1                                        ECS085
01089          GO TO 0720-OUT-LIFE.                                     ECS085
01090                                                                   ECS085
01091      MOVE X-AMTS (X2) TO X-DETL.                                  ECS085
01092                                                                   ECS085
01093      IF X-COUNT = +0                                              ECS085
01094          ADD +1 TO X2                                             ECS085
01095          GO TO 0710-LOOP-LIFE.                                    ECS085
01096                                                                   ECS085
CIDMOD*    IF LNCTR GREATER THAN +58                                    ECS085
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS085
01098         MOVE +0       TO HD-SW                                    ECS085
01099         PERFORM 1200-HD-RTN THRU 1299-HD-XIT.                     ECS085
01100                                                                   ECS085
01101      IF HD-SW = +0                                                ECS085
01102          MOVE +1      TO LIFE-SW  HD-SW                           ECS085
01103          MOVE HD7     TO P-LN                                     ECS085
01104          MOVE '-' TO P-CCSW                                       ECS085
01105          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
01106          MOVE HD8     TO P-LN                                     ECS085
01107          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
01108          MOVE '0' TO P-CCSW.                                      ECS085
01109                                                                   ECS085
01110      MOVE X-BEN (X2)  TO PX-BEN.                                  ECS085
01111      MOVE X-DESC (X2) TO PX-DESC.                                 ECS085
01112      ADD X-PAID       TO S-PAID.                                  ECS085
01113      ADD X-C78        TO S-C78.                                   ECS085
01114      ADD X-CRATA      TO S-CRATA.                                 ECS085
01115      ADD X-TAX        TO S-TAX.                                   ECS085
01116      ADD X-T78        TO S-T78.                                   ECS085
01117      ADD X-TRATA      TO S-TRATA.                                 ECS085
01118                                                                   ECS085
01119      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS085
01120                                                                   ECS085
01121      ADD +1 TO X2.                                                ECS085
01122                                                                   ECS085
01123      GO TO 0710-LOOP-LIFE.                                        ECS085
01124                                                                   ECS085
01125  0720-OUT-LIFE.                                                   ECS085
01126      MOVE SUB-TOTALS TO TOTALS.                                   ECS085
01127                                                                   ECS085
01128      IF LIFE-SW = +0                                              ECS085
01129          GO TO 0740-LOOP-AH.                                      ECS085
01130                                                                   ECS085
01131      MOVE 'TOTAL     ' TO PX-DESC.                                ECS085
01132      MOVE LIFE-OVERRIDE-L2 TO PX-BEN-DESC.                        ECS085
01133      MOVE SPACES       TO PX-BEN.                                 ECS085
01134      MOVE SUB-TOTALS   TO X-DETL.                                 ECS085
01135                                                                   ECS085
01136      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS085
01137                                                                   ECS085
01138      MOVE '0' TO P-CCSW.                                          ECS085
01139                                                                   ECS085
01140  0730-ZERO-SUBTOTAL.                                              ECS085
01141      MOVE +0 TO S-PAID                                            ECS085
01142                 S-C78                                             ECS085
01143                 S-CRATA                                           ECS085
01144                 S-TAX                                             ECS085
01145                 S-T78                                             ECS085
01146                 S-TRATA.                                          ECS085
01147                                                                   ECS085
01148  0740-LOOP-AH.                                                    ECS085
01149      IF X2 GREATER THAN MAX-BEN                                   ECS085
01150          GO TO 0750-OUT-AH.                                       ECS085
01151                                                                   ECS085
01152      IF X-TYP (X2) NOT = 2                                        ECS085
01153          GO TO 0740-LOOP-AH.                                      ECS085
01154                                                                   ECS085
01155      MOVE X-AMTS (X2) TO X-DETL.                                  ECS085
01156                                                                   ECS085
01157      IF X-COUNT = +0                                              ECS085
01158          ADD +1 TO X2                                             ECS085
01159          GO TO 0740-LOOP-AH.                                      ECS085
01160                                                                   ECS085
CIDMOD*    IF LNCTR GREATER THAN +58                                    ECS085
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS085
01162         MOVE +0       TO HD-SW                                    ECS085
01163         PERFORM 1200-HD-RTN THRU 1299-HD-XIT.                     ECS085
01164                                                                   ECS085
01165      IF HD-SW = +0                                                ECS085
01166          MOVE +1  TO HD-SW                                        ECS085
01167          MOVE HD7 TO P-LN                                         ECS085
01168          MOVE '-' TO P-CCSW                                       ECS085
01169          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
01170          MOVE HD8 TO P-LN                                         ECS085
01171          PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT                   ECS085
01172          MOVE '0' TO P-CCSW.                                      ECS085
01173                                                                   ECS085
01174      MOVE +1          TO AH-SW.                                   ECS085
01175      MOVE X-BEN (X2)  TO PX-BEN.                                  ECS085
01176      MOVE X-DESC (X2) TO PX-DESC.                                 ECS085
01177      ADD X-PAID       TO S-PAID.                                  ECS085
01178      ADD X-C78        TO S-C78.                                   ECS085
01179      ADD X-CRATA      TO S-CRATA.                                 ECS085
01180      ADD X-TAX        TO S-TAX.                                   ECS085
01181      ADD X-T78        TO S-T78.                                   ECS085
01182      ADD X-TRATA      TO S-TRATA.                                 ECS085
01183                                                                   ECS085
01184      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS085
01185                                                                   ECS085
01186      ADD +1 TO X2.                                                ECS085
01187                                                                   ECS085
01188      GO TO 0740-LOOP-AH.                                          ECS085
01189                                                                   ECS085
01190  0750-OUT-AH.                                                     ECS085
01191      IF AH-SW = +0                                                ECS085
01192          GO TO 0799-PRT-EXTRACT-XIT.                              ECS085
01193                                                                   ECS085
01194      MOVE 'TOTAL    ' TO PX-DESC.                                 ECS085
01195      MOVE AH-OVERRIDE-L2 TO PX-BEN-DESC.                          ECS085
01196      MOVE SPACES      TO PX-BEN.                                  ECS085
01197      MOVE SUB-TOTALS  TO X-DETL.                                  ECS085
01198                                                                   ECS085
01199      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS085
01200                                                                   ECS085
01201      MOVE '0'       TO P-CCSW.                                    ECS085
01202      ADD S-PAID     TO T-PAID.                                    ECS085
01203      ADD S-C78      TO T-C78.                                     ECS085
01204      ADD S-CRATA    TO T-CRATA.                                   ECS085
01205      ADD S-TAX      TO T-TAX.                                     ECS085
01206      ADD S-T78      TO T-T78.                                     ECS085
01207      ADD S-TRATA    TO T-TRATA.                                   ECS085
01208                                                                   ECS085
01209  0760-PRT-TOTALS.                                                 ECS085
01210      IF LIFE-SW = +0                                              ECS085
01211          GO TO 0799-PRT-EXTRACT-XIT.                              ECS085
01212                                                                   ECS085
01213      MOVE TOTALS      TO X-DETL.                                  ECS085
01214      SUBTRACT X-DUP   FROM X-COUNT.                               ECS085
01215      MOVE '  TOTAL'   TO PX-DESC.                                 ECS085
01216      MOVE SPACES      TO PX-BEN.                                  ECS085
01217                                                                   ECS085
01218      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS085
01219                                                                   ECS085
01220  0799-PRT-EXTRACT-XIT.                                            ECS085
01221      EXIT.                                                        ECS085
01222  EJECT                                                            ECS085
01223  0800-PRT-LINE.                                                   ECS085
01224      MOVE PX-BEN     TO P-BEN.                                    ECS085
01225      MOVE PX-DESC    TO P-DESC.                                   ECS085
01226      MOVE X-PAID     TO P-PAID.                                   ECS085
01227      MOVE X-C78      TO P-C78.                                    ECS085
01228      MOVE X-CRATA    TO P-CRATA.                                  ECS085
01229      MOVE X-TAX      TO P-TAX.                                    ECS085
01230      MOVE X-T78      TO P-T78.                                    ECS085
01231      MOVE X-TRATA    TO P-TRATA.                                  ECS085
01232                                                                   ECS085
01233      PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT.                      ECS085
01234                                                                   ECS085
01235  0899-PRT-LINE-XIT.                                               ECS085
01236      EXIT.                                                        ECS085
01237                                                                   ECS085
01238  0900-ZERO-RTN.                                                   ECS085
01239      MOVE +0 TO X-COUNT                                           ECS085
01240                 X-WRITTEN                                         ECS085
01241                 X-P78                                             ECS085
01242                 X-PRATA                                           ECS085
01243                 X-DOMICILE                                        ECS085
01244                 X-STATE                                           ECS085
01245                 X-RESERV                                          ECS085
01246                 X-ALTRSV                                          ECS085
01247                 X-REMAIN                                          ECS085
01248                 X-PAID                                            ECS085
01249                 X-C78                                             ECS085
01250                 X-CRATA                                           ECS085
01251                 X-TAX                                             ECS085
01252                 X-T78                                             ECS085
01253                 X-TRATA                                           ECS085
01254                 ZERO-SW                                           ECS085
01255                 X2.                                               ECS085
01256                                                                   ECS085
01257  0910-ZERO-X2.                                                    ECS085
01258      ADD +1 TO X2.                                                ECS085
01259                                                                   ECS085
01260      IF X2 GREATER THAN MAX-BEN                                   ECS085
01261          GO TO 0999-ZERO-XIT.                                     ECS085
01262                                                                   ECS085
01263      MOVE X-DETL TO X-AMTS (X2).                                  ECS085
01264                                                                   ECS085
01265      GO TO 0910-ZERO-X2.                                          ECS085
01266                                                                   ECS085
01267  0999-ZERO-XIT.                                                   ECS085
01268      EXIT.                                                        ECS085
01269                                                                   ECS085
01270  1000-ROLL-RTN.                                                   ECS085
01271      MOVE +0 TO X2.                                               ECS085
01272                                                                   ECS085
01273  1010-ROLL-X2.                                                    ECS085
01274      ADD +1 TO X2.                                                ECS085
01275                                                                   ECS085
01276      IF X2 GREATER THAN MAX-BEN                                   ECS085
01277          GO TO 1099-ROLL-XIT.                                     ECS085
01278                                                                   ECS085
01279      MOVE X-AMTS (X2)  TO X-DETL.                                 ECS085
01280      MOVE ST-AMTS (X2) TO R-DETL.                                 ECS085
01281      ADD X-COUNT       TO R-COUNT.                                ECS085
01282      ADD X-WRITTEN     TO R-WRITTEN.                              ECS085
01283      ADD X-P78         TO R-P78.                                  ECS085
01284      ADD X-PRATA       TO R-PRATA.                                ECS085
01285      ADD X-DOMICILE    TO R-DOMICILE.                             ECS085
01286      ADD X-STATE       TO R-STATE.                                ECS085
01287      ADD X-RESERV      TO R-RESERV.                               ECS085
01288      ADD X-ALTRSV      TO R-ALTRSV.                               ECS085
01289      ADD X-REMAIN      TO R-REMAIN.                               ECS085
01290      ADD X-PAID        TO R-PAID.                                 ECS085
01291      ADD X-C78         TO R-C78.                                  ECS085
01292      ADD X-CRATA       TO R-CRATA.                                ECS085
01293      ADD X-TAX         TO R-TAX.                                  ECS085
01294      ADD X-T78         TO R-T78.                                  ECS085
01295      ADD X-TRATA       TO R-TRATA.                                ECS085
01296      MOVE R-DETL       TO ST-AMTS (X2).                           ECS085
01297                                                                   ECS085
01298      GO TO 1010-ROLL-X2.                                          ECS085
01299                                                                   ECS085
01300  1099-ROLL-XIT.                                                   ECS085
01301      EXIT.                                                        ECS085
01302  EJECT                                                            ECS085
01303  1100-LOAD-TABLES.                                                ECS085
01304      MOVE +0          TO X2.                                      ECS085
01305      MOVE CLAS-STARTL TO CLAS-INDEXL.                             ECS085
01306      MOVE CLAS-STARTA TO CLAS-INDEXA.                             ECS085
01307      MOVE HIGH-VALUE  TO X-POINTERS.                              ECS085
01308                                                                   ECS085
01309      IF CLAS-MAXL = ZEROS                                         ECS085
01310          GO TO 1120-FORMAT-AH-RTN.                                ECS085
01311                                                                   ECS085
01312  1110-FORMAT-LIFE.                                                ECS085
01313      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS085
01314          GO TO 1120-FORMAT-AH-RTN.                                ECS085
01315                                                                   ECS085
01316      ADD +1                         TO X2.                        ECS085
01317      MOVE CLAS-I-BEN (CLAS-INDEXL)  TO X-BEN (X2).                ECS085
01318      MOVE 1                         TO X-TYP (X2).                ECS085
01319      MOVE CLAS-I-AB10 (CLAS-INDEXL) TO X-DESC (X2).               ECS085
01320      ADD +1                         TO CLAS-INDEXL.               ECS085
01321                                                                   ECS085
01322      GO TO 1110-FORMAT-LIFE.                                      ECS085
01323                                                                   ECS085
01324  1120-FORMAT-AH-RTN.                                              ECS085
01325      IF CLAS-MAXA = ZEROS                                         ECS085
01326          GO TO 1140-FORMAT-SET.                                   ECS085
01327                                                                   ECS085
01328  1130-FORMAT-AH.                                                  ECS085
01329      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS085
01330          GO TO 1140-FORMAT-SET.                                   ECS085
01331                                                                   ECS085
01332      ADD +1                         TO X2.                        ECS085
01333      MOVE CLAS-I-BEN (CLAS-INDEXA)  TO X-BEN (X2).                ECS085
01334      MOVE 2                         TO X-TYP (X2).                ECS085
01335      MOVE CLAS-I-AB10 (CLAS-INDEXA) TO X-DESC (X2).               ECS085
01336      ADD +1                         TO CLAS-INDEXA.               ECS085
01337                                                                   ECS085
01338      GO TO 1130-FORMAT-AH.                                        ECS085
01339                                                                   ECS085
01340  1140-FORMAT-SET.                                                 ECS085
01341      MOVE X2 TO MAX-BEN.                                          ECS085
01342                                                                   ECS085
01343  1199-LOAD-TAB-XIT.                                               ECS085
01344      EXIT.                                                        ECS085
01345  EJECT                                                            ECS085
01346  1200-HD-RTN.                                                     ECS085
01347      ADD +1       TO PGCTR.                                       ECS085
01348      MOVE PGCTR   TO HD-PAGE.                                     ECS085
01349      MOVE HD1     TO P-LN.                                        ECS085
01350      MOVE '1' TO P-CCSW.                                          ECS085
01351      PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT.                      ECS085
01352                                                                   ECS085
01353      MOVE HD2 TO P-LN.                                            ECS085
01354      PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT.                      ECS085
01355                                                                   ECS085
01356      MOVE HD3 TO P-LN.                                            ECS085
01357      PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT.                      ECS085
01358                                                                   ECS085
01359      IF HD3A = SPACES                                             ECS085
01360          ADD +1 TO LNCTR                                          ECS085
01361          GO TO 1299-HD-XIT.                                       ECS085
01362                                                                   ECS085
01363      MOVE HD3A    TO P-LN.                                        ECS085
01364      MOVE '0' TO P-CCSW.                                          ECS085
01365      PERFORM 1300-PRT-RTN THRU 1399-PRT-XIT.                      ECS085
01366                                                                   ECS085
01367  1299-HD-XIT.                                                     ECS085
01368      EXIT.                                                        ECS085
01369                                                                   ECS085
01370  1300-PRT-RTN.                                                    ECS085
01371      MOVE P-CCSW  TO X P-CTL.                                     ECS085
01372      MOVE P-LN    TO P-DATA.                                      ECS085
01373      MOVE ' '     TO P-REC.                                       ECS085
01374                                                                   ECS085
01375      IF X = ' '                                                   ECS085
01376          ADD +1 TO LNCTR                                          ECS085
01377      ELSE                                                         ECS085
01378          IF X = '0'                                               ECS085
01379              ADD +2 TO LNCTR                                      ECS085
01380          ELSE                                                     ECS085
01381              IF X = '-'                                           ECS085
01382                  ADD +3 TO LNCTR                                  ECS085
01383              ELSE                                                 ECS085
01384                  MOVE +1 TO LNCTR.                                ECS085
01385                                                                   ECS085
01386  1310-PRT-COPY-RTN.                                               ECS085
01387                              COPY ELCPRT2.                        ECS085
01388                                                                   ECS085
01389  1399-PRT-XIT.                                                    ECS085
01390      EXIT.                                                        ECS085
01391  EJECT                                                            ECS085
01392  1400-END-JOB.                                                    ECS085
01393      MOVE +2         TO FST-SW.                                   ECS085
01394      MOVE HIGH-VALUE TO WX-SEQ.                                   ECS085
01395      PERFORM 0300-BREAK-RTN THRU 0399-BREAK-XIT.                  ECS085
01396                                                                   ECS085
01397  1410-CLOSE-RTN.                                                  ECS085
01398                              COPY ELCPRTC.                        ECS085
01399                                                                   ECS085
01400      CLOSE PRNTR  ACCT-MSTR  GAAP-EXTR.                           ECS085
01401                                                                   ECS085
01402      IF AM-FILE-STATUS NOT = '00'                                 ECS085
01403         MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS               ECS085
01404         MOVE 'ERROR ON ERACCTT, CLOSE ' TO WS-ABEND-MESSAGE       ECS085
01405         GO TO ABEND-PGM.                                          ECS085
01406                                                                   ECS085
01407      GOBACK.                                                      ECS085
01408                                                                   ECS085
01409  ABEND-PGM.                                                       ECS085
01410                        COPY ELCABEND.                             ECS085
01411      EJECT                                                        ECS085
