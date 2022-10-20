70001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS084
00003  PROGRAM-ID.               ECS084.                                   LV006
00004 *              PROGRAM CONVERTED BY                               ECS084
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS084
00006 *              CONVERSION DATE 02/08/96 18:44:11.                 ECS084
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS084
00008 *                          VMOD=2.007.                            ECS084
00009 *AUTHOR.        LOGIC, INC.                                       ECS084
00010 *               DALLAS, TEXAS.                                    ECS084
00011                                                                   ECS084
00012 *DATE-COMPILED.                                                   ECS084
00013                                                                   ECS084
00014 *SECURITY.   *****************************************************ECS084
00015 *            *                                                   *ECS084
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS084
00017 *            *                                                   *ECS084
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS084
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS084
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS084
00021 *            *                                                   *ECS084
00022 *            *****************************************************ECS084
00023                                                                   ECS084
00024 *REMARKS.                                                         ECS084
00025 *        PRINT UNEARNED PREMIUM AND COMMISSION ANALYSIS.          ECS084
00026 *        PROGRAM SWITCHES NOT APPLICABLE - CONTROL IS IN          ECS084
00027 *        ECS083.                                                  ECS084
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
052303* 052303    2001061800003  SMVA  DO NOT PRINT TAX DATA FOR DCC
111203* 111203    2002012100004  SMVA  INCREASE MAX NUM MORTALITY TABLES
010508* 010508    2008010200008  PEMA  GO BACK 15 YEARS FOR ISS DATE
121610* 121610    2010120900001  PEMA  CORRECT ISS YR SORT
020113* 020113  IR2013020100001  PEMA  INCREASE # OF MORT TABLES
040114* 040114  CR2011122200002  AJRA  MODIFY DOMICILE PREM, COMM, AND TAX
011719* 011719  IR2019011700001  PEMA  Fix bug when > 99 mort tables
092602******************************************************************
00028                                                                   ECS084
00029  ENVIRONMENT DIVISION.                                            ECS084
00030  INPUT-OUTPUT SECTION.                                            ECS084
00031  FILE-CONTROL.                                                    ECS084
00032                                                                   ECS084
00033      SELECT SORTFL          ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.   ECS084
00034      SELECT REPTFL          ASSIGN TO SYS004-UT-FBA1-S-SYS004.    ECS084
pemuni*    SELECT PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.
pemuni     SELECT PRNTR           ASSIGN TO SYS008.
00036      SELECT DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.    ECS084
00037      SELECT FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.    ECS084
00038  EJECT                                                            ECS084
00039  DATA DIVISION.                                                   ECS084
00040  FILE SECTION.                                                    ECS084
00041                                                                   ECS084
00042  SD  SORTFL.                                                      ECS084
00043                                                                   ECS084
00044  01  SRT-REC.                                                     ECS084
011719     12  S-PARM              PIC X(27).
040114     12  FILLER              PIC X(130).                          ECS084
00047  EJECT                                                            ECS084
00048  FD  REPTFL                                                       ECS084
00049      BLOCK CONTAINS 0 RECORDS
00050      RECORDING MODE F.                                            ECS084
00051                                                                   ECS084
00052  01  RPT-REC.                                                     ECS084
011719     12  R-PARM              PIC X(27).
040114     12  FILLER              PIC X(130).                          ECS084
00055  EJECT                                                            ECS084
00056  FD  PRNTR                                                        ECS084
00057                              COPY ELCPRTFD.                       ECS084
00058  EJECT                                                            ECS084
00059  FD  DISK-DATE                                                    ECS084
00060                              COPY ELCDTEFD.                       ECS084
00061  EJECT                                                            ECS084
00062  FD  FICH                                                         ECS084
00063                              COPY ECSFICH.                        ECS084
00064  EJECT                                                            ECS084
00065  WORKING-STORAGE SECTION.                                         ECS084
00066  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS084
00067  77  FILLER  PIC X(32) VALUE '********************************'.  ECS084
00068  77  FILLER  PIC X(32) VALUE '     ECS084 WORKING STORAGE     '.  ECS084
00069  77  FILLER  PIC X(32) VALUE '*****VMOD=2.007*****************'.  ECS084
00070                                                                   ECS084
00071  77  PGM-SUB                 PIC S999    COMP    VALUE +084.      ECS084
CIDMOD 77  CC                      PIC 9               VALUE ZEROS.     ECS084
00072  77  AH-SW                   PIC S9      COMP-3.                  ECS084
00073  77  LIFE-SW                 PIC S9      COMP-3.                  ECS084
00074  77  ZERO-SW                 PIC S9      COMP-3  VALUE +1.        ECS084
00075  77  HD-SW                   PIC S9      COMP-3  VALUE +0.        ECS084
00076  77  TAB-SW                  PIC S9      COMP-3  VALUE +0.        ECS084
00077  77  LNCTR                   PIC S999    COMP-3.                  ECS084
092602 77  MAX-BEN                 PIC S999    COMP-3  VALUE +900.      ECS084
00079  77  M1                      PIC S9(5)   COMP-3.                  ECS084
00080  77  M2                      PIC S9(5)   COMP-3.                  ECS084
00081  77  X                       PIC X               VALUE SPACE.     ECS084
00082  77  X1                      PIC S9(5)   COMP-3.                  ECS084
00083  77  X2                      PIC S9(5)   COMP-3.                  ECS084
00084  77  X3                      PIC S999    COMP-3.                  ECS084
00085  77  PGCTR                   PIC S9(5)   COMP-3.                  ECS084
00086  EJECT                                                            ECS084
00087  01  WORK-REC.                                                    ECS084
00088      12  W-SEQ.                                                   ECS084
00089          16  W-SEQ1.                                              ECS084
00090              20  W-REIN      PIC X(6).                            ECS084
00091              20  W-CARR      PIC X.                               ECS084
00092              20  W-CO        PIC X(6).                            ECS084
00093              20  W-ST        PIC XX.                              ECS084
00094          16  W-SEQ2.                                              ECS084
121610             20  W-YR        PIC 9999.
00096              20  W-CODE      PIC 9.                               ECS084
00097          16  W-SEQ3.                                              ECS084
00098              20  W-LAH       PIC X.                               ECS084
00099              20  W-BEN       PIC XX.                              ECS084
011719             20  W-MORT      PIC 999.
00101      12  FILLER              PIC X.                               ECS084
00102      12  W-AMTS.                                                  ECS084
00103          16  W-DUP           PIC S9(7)     COMP-3.                ECS084
00104          16  W-COUNT         PIC S9(7)     COMP-3.                ECS084
00105          16  W-WRITTEN       PIC S9(9)V99  COMP-3.                ECS084
00106          16  W-P78           PIC S9(9)V99  COMP-3.                ECS084
00107          16  W-PRATA         PIC S9(9)V99  COMP-3.                ECS084
00108          16  W-DOMICILE      PIC S9(9)V99  COMP-3.                ECS084
00109          16  W-STATE         PIC S9(9)V99  COMP-3.                ECS084
00110          16  W-RESERV        PIC S9(9)V99  COMP-3.                ECS084
00111          16  W-ALTRSV        PIC S9(9)V99  COMP-3.                ECS084
00112          16  W-REMAIN        PIC S9(13)V99 COMP-3.                ECS084
00113          16  W-PAID          PIC S9(9)V99  COMP-3.                ECS084
00114          16  W-C78           PIC S9(9)V99  COMP-3.                ECS084
00115          16  W-CRATA         PIC S9(9)V99  COMP-3.                ECS084
040114         16  W-CDOMI         PIC S9(9)V99  COMP-3.
00116          16  W-TAX           PIC S9(9)V99  COMP-3.                ECS084
00117          16  W-T78           PIC S9(9)V99  COMP-3.                ECS084
00118          16  W-TRATA         PIC S9(9)V99  COMP-3.                ECS084
040114         16  W-TDOMI         PIC S9(9)V99  COMP-3.
00119      12  W-M-AMTS.                                                ECS084
00120          16  W-IUNDR         PIC S9(9)V99  COMP-3.                ECS084
00121          16  W-IOVER         PIC S9(9)V99  COMP-3.                ECS084
00122          16  W-GUNDR         PIC S9(9)V99  COMP-3.                ECS084
00123          16  W-GOVER         PIC S9(9)V99  COMP-3.                ECS084
00124  EJECT                                                            ECS084
CIDMOD 01  WS-REPORT-TITLE           PIC X(15)   VALUE SPACES.
00125  01  MISC-WS.                                                     ECS084
00126      12  WS-RETURN-CODE        PIC S9(4)   COMP.                  ECS084
00127      12  WS-ABEND-MESSAGE      PIC X(80).                         ECS084
00128      12  WS-ABEND-FILE-STATUS  PIC X(2).                          ECS084
00129      12  WS-ZERO               PIC S9      COMP-3  VALUE +0.      ECS084
00130      12  WS-ZERO-FIELDS.                                          ECS084
00131          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00132          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00133          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00134          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00135      12  WS-ZERO-FIELDS-2.                                        ECS084
00136          16  FILLER          PIC S9(7)     COMP-3  VALUE +0.      ECS084
00137          16  FILLER          PIC S9(7)     COMP-3  VALUE +0.      ECS084
00138          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00139          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00140          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00141          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00142          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00143          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00144          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00145          16  FILLER          PIC S9(13)V99 COMP-3  VALUE +0.      ECS084
00146          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00147          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00148          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00149          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00150          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
00151          16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.      ECS084
040114         16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.
040114         16  FILLER          PIC S9(9)V99  COMP-3  VALUE +0.
00152      12  CUR-SEQ.                                                 ECS084
00153          16  CUR-SEQ1.                                            ECS084
00154              20  CUR-REIN    PIC X(6).                            ECS084
00155              20  CUR-CARR    PIC X.                               ECS084
00156              20  CUR-CO      PIC X(6).                            ECS084
00157              20  CUR-ST      PIC XX.                              ECS084
00158          16  CUR-SEQ2.                                            ECS084
121610             20  CUR-YR      PIC 9999.                            ECS084
00160              20  CUR-CODE    PIC 9.                               ECS084
00161              20  CUR-LAH     PIC 9.                               ECS084
00162              20  CUR-BEN     PIC 99.                              ECS084
011719             20  CUR-MORT    PIC 999.
00164      12  CUR-SEQX REDEFINES CUR-SEQ.                              ECS084
121610         16  CUR-11          PIC X(19).                           ECS084
011719         16  FILLER          PIC X(7).
00167      12  PRE-SEQ.                                                 ECS084
00168          16  PRE-SEQ1.                                            ECS084
00169              20  PRE-REIN    PIC X(6)            VALUE LOW-VALUE. ECS084
00170              20  PRE-CARR    PIC X               VALUE LOW-VALUE. ECS084
00171              20  PRE-CO      PIC X(6)            VALUE LOW-VALUE. ECS084
00172              20  PRE-ST      PIC XX              VALUE LOW-VALUE. ECS084
00173          16  PRE-SEQ2.                                            ECS084
121610             20  PRE-YR      PIC 9999            VALUE 9999.      ECS084
00175              20  PRE-CODE    PIC 9               VALUE 9.         ECS084
00176              20  PRE-LAH     PIC 9               VALUE 9.         ECS084
00177              20  PRE-BEN     PIC XX       VALUE HIGH-VALUES.      ECS084
011719             20  PRE-MORT    PIC 999             VALUE 999.
00179      12  PRE-SEQX REDEFINES PRE-SEQ.                              ECS084
121610         16  PRE-11          PIC X(19).                           ECS084
011719         16  FILLER          PIC X(7).
121610     12  SAV-11              PIC X(19)           VALUE LOW-VALUE. ECS084
00183      12  HEADER-MSG.                                              ECS084
00184          16  HEADER-MSG-1    PIC X(6)    VALUE ' YEAR '.             CL**4
121610         16  HEADER-MSG-2    PIC XX      VALUE '  '.                 CL**2
00186                                                                   ECS084
00187  SKIP3                                                            ECS084
00188  01  PASS-TWO-WORK.                                               ECS084
00189      12  X-POINTERS.                                              ECS084
092602         16  X-MATCH     OCCURS 900 TIMES.                        ECS084
00191              20  X-POINTER.                                       ECS084
00192                  24  X-BEN   PIC XX.                              ECS084
00193                  24  X-TYP   PIC 9.                               ECS084
00194              20  X-DESC      PIC X(10).                           ECS084
00195      12  WX-POINTER.                                              ECS084
00196          16  WX-BEN          PIC XX.                              ECS084
00197          16  WX-TYP          PIC 9.                               ECS084
00198      12  PX-POINTER.                                              ECS084
00199          16  PX-BEN          PIC XXX.                             ECS084
00200          16  PX-DESC         PIC X(10).                           ECS084
00201  EJECT                                                            ECS084
00202  01  COMMON-TOTALS.                                               ECS084
00203      12  X-DETL.                                                  ECS084
00204          16  X-DUP           PIC S9(7)     COMP-3.                ECS084
00205          16  X-COUNT         PIC S9(7)     COMP-3.                ECS084
00206          16  X-WRITTEN       PIC S9(9)V99  COMP-3.                ECS084
00207          16  X-P78           PIC S9(9)V99  COMP-3.                ECS084
00208          16  X-PRATA         PIC S9(9)V99  COMP-3.                ECS084
00209          16  X-DOMICILE      PIC S9(9)V99  COMP-3.                ECS084
00210          16  X-STATE         PIC S9(9)V99  COMP-3.                ECS084
00211          16  X-RESERV        PIC S9(9)V99  COMP-3.                ECS084
00212          16  X-ALTRSV        PIC S9(9)V99  COMP-3.                ECS084
00213          16  X-REMAIN        PIC S9(13)V99 COMP-3.                ECS084
00214          16  X-PAID          PIC S9(9)V99  COMP-3.                ECS084
00215          16  X-C78           PIC S9(9)V99  COMP-3.                ECS084
00216          16  X-CRATA         PIC S9(9)V99  COMP-3.                ECS084
040114         16  X-CDOMI         PIC S9(9)V99  COMP-3.
00217          16  X-TAX           PIC S9(9)V99  COMP-3.                ECS084
00218          16  X-T78           PIC S9(9)V99  COMP-3.                ECS084
00219          16  X-TRATA         PIC S9(9)V99  COMP-3.                ECS084
040114         16  X-TDOMI         PIC S9(9)V99  COMP-3.
00220      12  X-M-DETL.                                                ECS084
00221          16  X-IUNDR         PIC S9(9)V99  COMP-3.                ECS084
00222          16  X-IOVER         PIC S9(9)V99  COMP-3.                ECS084
00223          16  X-GUNDR         PIC S9(9)V99  COMP-3.                ECS084
00224          16  X-GOVER         PIC S9(9)V99  COMP-3.                ECS084
00225  SKIP3                                                            ECS084
00226  01  PASS-TWO-TOTALS-1.                                           ECS084
00227      12  PASS-TWO-TOTS.                                           ECS084
00228          16  PT-LEVELS         OCCURS 2 TIMES.                    ECS084
092602             20  PT-TYPES      OCCURS 900 TIMES.                  ECS084
040114                 24  PT-AMTS       PIC X(106).                    ECS084
00231                                                                   ECS084
00232  01  FILLER PIC  X(23) VALUE 'TABLE PT-M-AMTS-G START'.           ECS084
00233  01  PASS-TWO-TOTALS-2-GROSS.                                     ECS084
020113     12  PT-M-CODES-G          OCCURS 150 TIMES.
092602         16  PT-M-TYPES-G      OCCURS 900 TIMES.                  ECS084
00236              20  PT-M-AMTS-G       PIC X(24).                     ECS084
092602*01  PASS-TWO-TOTALS-2-GROSS-2.                                   ECS084
092602*    12  FILLER                OCCURS 18 TIMES.                   ECS084
092602*        16  FILLER            OCCURS 900 TIMES.                  ECS084
092602*            20  FILLER            PIC X(24).                     ECS084
092602*01  PASS-TWO-TOTALS-2-GROSS-3.                                   ECS084
CIDMOD*    12  FILLER                OCCURS 04 TIMES.                   ECS084
092602*    12  FILLER                OCCURS 24 TIMES.                   ECS084
092602*        16  FILLER            OCCURS 300 TIMES.                  ECS084
092602*            20  FILLER            PIC X(24).                     ECS084
00245                                                                   ECS084
00246  01  FILLER PIC  X(23) VALUE 'TABLE PT-M-AMTS-R START'.           ECS084
00247  01  PASS-TWO-TOTALS-2-REIN.                                      ECS084
020113     12  PT-M-CODES-R          OCCURS 150 TIMES.
092602         16  PT-M-TYPES-R      OCCURS 900 TIMES.                  ECS084
00250              20  PT-M-AMTS-R       PIC X(24).                     ECS084
092602*01  PASS-TWO-TOTALS-2-REIN-2.                                    ECS084
092602*    12  FILLER                OCCURS 18 TIMES.                   ECS084
092602*        16  FILLER            OCCURS 300 TIMES.                  ECS084
092602*            20  FILLER            PIC X(24).                     ECS084
092602*01  PASS-TWO-TOTALS-2-REIN-3.                                    ECS084
CIDMOD*    12  FILLER                OCCURS 04 TIMES.                   ECS084
092602*    12  FILLER                OCCURS 24 TIMES.                   ECS084
092602*        16  FILLER            OCCURS 300 TIMES.                  ECS084
092602*            20  FILLER            PIC X(24).                     ECS084
00259                                                                   ECS084
00260  01  FILLER PIC  X(19) VALUE 'TABLE XT-AMTS START'.               ECS084
00261  01  PASS-TWO-TOTALS-3.                                           ECS084
00262      12  XT-TOTALS.                                               ECS084
092602         16  XT-AMTS           OCCURS 900 TIMES.                  ECS084
040114             20  FILLER            PIC X(106).                    ECS084
00265                                                                   ECS084
00266  01  FILLER PIC  X(19) VALUE 'TABLE XM-AMTS START'.               ECS084
00267  01  PASS-TWO-TOTALS-4.                                           ECS084
020113     12  XM-LEVEL              OCCURS 150 TIMES.
092602         16  XM-AMTS           OCCURS 900 TIMES.                  ECS084
00270              20  FILLER            PIC X(24).                     ECS084
092602*01  PASS-TWO-TOTALS-4-1.                                         ECS084
092602*    12  FILLER                OCCURS 18 TIMES.                   ECS084
092602*        16  FILLER            OCCURS 300 TIMES.                  ECS084
092602*            20  FILLER            PIC X(24).                     ECS084
00275 *01  PASS-TWO-TOTALS-4-2.                                         ECS084
CIDMOD*    12  FILLER                OCCURS 04 TIMES.                   ECS084
092602*    12  FILLER                OCCURS 24 TIMES.                   ECS084
092602*        16  FILLER            OCCURS 300 TIMES.                  ECS084
092602*            20  FILLER            PIC X(24).                     ECS084
00279  01  FILLER PIC  X(18) VALUE 'TABLE XM-AMTS ENDS'.                ECS084
00280  EJECT                                                            ECS084
00281  01  PASS-TWO-TOTALS-5.                                           ECS084
00282      12  SUB-TOTALS.                                              ECS084
00283          16  S-DUP           PIC S9(7)     COMP-3.                ECS084
00284          16  S-COUNT         PIC S9(7)     COMP-3.                ECS084
00285          16  S-WRITTEN       PIC S9(9)V99  COMP-3.                ECS084
00286          16  S-P78           PIC S9(9)V99  COMP-3.                ECS084
00287          16  S-PRATA         PIC S9(9)V99  COMP-3.                ECS084
00288          16  S-DOMICILE      PIC S9(9)V99  COMP-3.                ECS084
00289          16  S-STATE         PIC S9(9)V99  COMP-3.                ECS084
00290          16  S-RESERV        PIC S9(9)V99  COMP-3.                ECS084
00291          16  S-ALTRSV        PIC S9(9)V99  COMP-3.                ECS084
00292          16  S-REMAIN        PIC S9(13)V99 COMP-3.                ECS084
00293          16  S-PAID          PIC S9(9)V99  COMP-3.                ECS084
00294          16  S-C78           PIC S9(9)V99  COMP-3.                ECS084
00295          16  S-CRATA         PIC S9(9)V99  COMP-3.                ECS084
040114         16  S-CDOMI         PIC S9(9)V99  COMP-3.
00296          16  S-TAX           PIC S9(9)V99  COMP-3.                ECS084
00297          16  S-T78           PIC S9(9)V99  COMP-3.                ECS084
00298          16  S-TRATA         PIC S9(9)V99  COMP-3.                ECS084
040114         16  S-TDOMI         PIC S9(9)V99  COMP-3.
00299      12  SUB-M-TOTALS.                                            ECS084
00300          16  S-IUNDR         PIC S9(9)V99  COMP-3.                ECS084
00301          16  S-IOVER         PIC S9(9)V99  COMP-3.                ECS084
00302          16  S-GUNDR         PIC S9(9)V99  COMP-3.                ECS084
00303          16  S-GOVER         PIC S9(9)V99  COMP-3.                ECS084
00304      12  TOTALS.                                                  ECS084
00305          16  T-DUP           PIC S9(7)     COMP-3.                ECS084
00306          16  T-COUNT         PIC S9(7)     COMP-3.                ECS084
00307          16  T-WRITTEN       PIC S9(9)V99  COMP-3.                ECS084
00308          16  T-P78           PIC S9(9)V99  COMP-3.                ECS084
00309          16  T-PRATA         PIC S9(9)V99  COMP-3.                ECS084
00310          16  T-DOMICILE      PIC S9(9)V99  COMP-3.                ECS084
00311          16  T-STATE         PIC S9(9)V99  COMP-3.                ECS084
00312          16  T-RESERV        PIC S9(9)V99  COMP-3.                ECS084
00313          16  T-ALTRSV        PIC S9(9)V99  COMP-3.                ECS084
00314          16  T-REMAIN        PIC S9(13)V99 COMP-3.                ECS084
00315          16  T-PAID          PIC S9(9)V99  COMP-3.                ECS084
00316          16  T-C78           PIC S9(9)V99  COMP-3.                ECS084
00317          16  T-CRATA         PIC S9(9)V99  COMP-3.                ECS084
040114         16  T-CDOMI         PIC S9(9)V99  COMP-3.
00318          16  T-TAX           PIC S9(9)V99  COMP-3.                ECS084
00319          16  T-T78           PIC S9(9)V99  COMP-3.                ECS084
00320          16  T-TRATA         PIC S9(9)V99  COMP-3.                ECS084
040114         16  T-TDOMI         PIC S9(9)V99  COMP-3.
00321      12  T-M-TOTALS.                                              ECS084
00322          16  T-IUNDR         PIC S9(9)V99  COMP-3.                ECS084
00323          16  T-IOVER         PIC S9(9)V99  COMP-3.                ECS084
00324          16  T-GUNDR         PIC S9(9)V99  COMP-3.                ECS084
00325          16  T-GOVER         PIC S9(9)V99  COMP-3.                ECS084
00326      12  WORK-TOTALS.                                             ECS084
00327          16  X-TOTAL         PIC S9(9)V99    COMP-3.              ECS084
00328          16  S-TOTAL         PIC S9(9)V99    COMP-3.              ECS084
00329          16  T-TOTAL         PIC S9(9)V99    COMP-3.              ECS084
00330  EJECT                                                            ECS084
00331  01  HD1.                                                         ECS084
00332      12  FILLER              PIC X(42)           VALUE SPACES.    ECS084
00333      12  FILLER              PIC X(40)           VALUE            ECS084
00334              'UNEARNED PREMIUM AND COMMISSION ANALYSIS'.          ECS084
CIDMOD     12  FILLER              PIC X(5)            VALUE SPACES.
CIDMOD     12  WS-HD1-OVERRIDE     PIC X(32)           VALUE SPACES.
CIDMOD*    12  FILLER              PIC X(37)           VALUE SPACES.    ECS084
00336      12  FILLER              PIC X(8)            VALUE 'ECS084'.  ECS084
00337                                                                   ECS084
00338  01  HD2.                                                         ECS084
00339      12  SUB-HD2             PIC X(30)           VALUE SPACES.    ECS084
00340      12  SUB-HD2-YEAR        PIC X(8)            VALUE SPACES.       CL**5
00341      12  SUB-HD2-YR          PIC X(9)            VALUE SPACES.    ECS084
00342      12  HD-CO               PIC X(30).                           ECS084
00343      12  FILLER              PIC X(42)           VALUE SPACES.       CL**5
00344      12  HD-RD               PIC X(8).                            ECS084
00345                                                                   ECS084
00346  01  HD3.                                                         ECS084
00347      12  SUB-HD3             PIC X(53)           VALUE SPACES.    ECS084
00348      12  HD-DT               PIC X(18).                           ECS084
00349      12  FILLER              PIC X(48)           VALUE SPACES.    ECS084
00350      12  FILLER              PIC X(5)            VALUE 'PAGE '.   ECS084
00351      12  HD-PAGE             PIC ZZ,ZZ9.                          ECS084
00352                                                                   ECS084
00353  01  HD4.                                                         ECS084
00354      12 FILLER               PIC X(44)           VALUE            ECS084
00355             '                             * * * * * * * *'.       ECS084
00356      12 FILLER               PIC X(44)           VALUE            ECS084
00357             ' * *    P R E M I U M    * * * * * * * * * *'.       ECS084
00358      12 FILLER               PIC X(44)           VALUE            ECS084
00359             '                                            '.       ECS084
00360                                                                   ECS084
00361  01  HD5.                                                         ECS084
00362      12 HD-DESC              PIC X(15).                           ECS084
00363      12 FILLER               PIC X(29)           VALUE            ECS084
00364                            'IN FORCE                  UNE'.       ECS084
00365      12 FILLER               PIC X(44)           VALUE            ECS084
00366             'ARNED     UNEARNED     DOMICILE      STATE  '.       ECS084
00367      12 FILLER               PIC X(44)           VALUE            ECS084
00368             '    MORTALITY    ALTERNATE        REMAINING '.       ECS084
040114
040114 01  HD5-AHL.
040114     12 HD-DESC-AHL          PIC X(15).
040114     12 FILLER               PIC X(29)           VALUE
040114                           'IN FORCE                  UNE'.
040114     12 FILLER               PIC X(44)           VALUE
040114            'ARNED     UNEARNED     UNEARNED      STATE  '.
040114     12 FILLER               PIC X(44)           VALUE
040114            '    MORTALITY    ALTERNATE        REMAINING '.
00369                                                                   ECS084
00370  01  HD6.                                                         ECS084
00371      12 FILLER               PIC X(44)           VALUE            ECS084
00372             '                COUNT        WRITTEN      RU'.       ECS084
00373      12 FILLER               PIC X(44)           VALUE            ECS084
00374             'LE 78     PRO-RATA    STATUTORY    STATUTORY'.       ECS084
00375      12 FILLER               PIC X(44)           VALUE            ECS084
00376             '     RESERVE      RESERVE           AMOUNT  '.       ECS084
040114
040114 01  HD6-AHL.
040114     12 FILLER               PIC X(44)           VALUE
040114            '                COUNT        WRITTEN      RU'.
040114     12 FILLER               PIC X(44)           VALUE
040114            'LE 78     PRO-RATA       GAAP      STATUTORY'.
040114     12 FILLER               PIC X(44)           VALUE
040114            '     RESERVE      RESERVE           AMOUNT  '.
00377  EJECT                                                            ECS084
00378  01  HD7.                                                         ECS084
00379      12 FILLER               PIC X(44)           VALUE            ECS084
040114            '                        * * * * * * *   C O '.       ECS084
00381      12 FILLER               PIC X(44)           VALUE            ECS084
040114            'M M I S S I O N   * * * * * * *      * * * *'.       ECS084
00383      12 FILLER               PIC X(44)           VALUE            ECS084
040114            ' * * *    T A X E S     * * * * * * * *     '.       ECS084
00385                                                                   ECS084
00386  01  HD8.                                                         ECS084
00387      12 FILLER               PIC X(44)           VALUE            ECS084
040114            '                                PAID      RU'.       ECS084
00389      12 FILLER               PIC X(44)           VALUE            ECS084
040114            'LE 78     PRO-RATA     DOMICILE         PAID'.       ECS084
00391      12 FILLER               PIC X(44)           VALUE            ECS084
040114            '      RULE 78     PRO-RATA     DOMICILE     '.       ECS084
040114                                                                  ECS084
040114 01  HD8-AHL.
040114     12 FILLER               PIC X(44)           VALUE
040114            '                                PAID      RU'.
040114     12 FILLER               PIC X(44)           VALUE
040114            'LE 78     PRO-RATA       GAAP           PAID'.
040114     12 FILLER               PIC X(44)           VALUE
040114            '      RULE 78     PRO-RATA       GAAP       '.
00393  SKIP3                                                            ECS084
00394   01  MORTALITY-HDS.                                              ECS084
00395      12  HD-4M.                                                   ECS084
00396          16  FILLER          PIC X(44)           VALUE            ECS084
00397                  '  MORTALITY                      BENEFIT    '.  ECS084
00398          16  FILLER          PIC X(44)           VALUE            ECS084
00399                  '        ------ INDIVIDUAL -------          -'.  ECS084
00400          16  FILLER          PIC X(44)           VALUE            ECS084
00401                  '-------- GROUP ---------             TOTAL  '.  ECS084
00402      12  HD-5M.                                                   ECS084
00403          16  FILLER          PIC X(44)           VALUE            ECS084
00404                  '    TABLE                         TYPE      '.  ECS084
00405          16  FILLER          PIC X(44)           VALUE            ECS084
00406                  '        UNDER 121        OVER 120          U'.  ECS084
00407          16  FILLER          PIC X(44)           VALUE            ECS084
00408                  'NDER 121        OVER 120            RESERVE '.  ECS084
00409  EJECT                                                            ECS084
00410  01  SUB-HEADINGS.                                                ECS084
00411      12  HEAD2.                                                   ECS084
00412          16  FILLER          PIC X(36)           VALUE            ECS084
00413                  'CARR  GROUP   STATE                 '.          ECS084
00414      12  ST-HDA REDEFINES HEAD2.                                  ECS084
00415          16  ST-HD2          PIC X(28).                           ECS084
00416          16  FILLER          PIC X(8).                            ECS084
00417      12  CO-HDA REDEFINES HEAD2.                                  ECS084
00418          16  CO-HD2          PIC X(12).                           ECS084
00419          16  FILLER          PIC X(24).                           ECS084
00420      12  CARR-HDA REDEFINES HEAD2.                                ECS084
00421          16  CARR-HD2        PIC X(4).                            ECS084
00422          16  FILLER          PIC X(32).                           ECS084
00423      12  HEAD3.                                                   ECS084
00424          16  FILLER          PIC XX              VALUE SPACES.    ECS084
00425          16  HD-CARR         PIC X.                               ECS084
00426          16  FILLER          PIC XXX             VALUE SPACES.    ECS084
00427          16  HD-COMP         PIC X(6).                            ECS084
00428          16  FILLER          PIC X               VALUE SPACES.    ECS084
00429          16  HD-ST           PIC XX.                              ECS084
00430          16  FILLER          PIC X               VALUE SPACES.    ECS084
00431          16  HD-ST-NM        PIC X(20).                           ECS084
00432          16  FILLER          PIC XXX             VALUE SPACES.    ECS084
00433      12  ST-HDB REDEFINES HEAD3.                                  ECS084
00434          16  ST-HD3          PIC X(28).                           ECS084
00435          16  FILLER          PIC X(11).                           ECS084
00436      12  CO-HDB REDEFINES HEAD3.                                  ECS084
00437          16  CO-HD3          PIC X(12).                           ECS084
00438          16  FILLER          PIC X(27).                           ECS084
00439      12  CARR-HDB REDEFINES HEAD3.                                ECS084
00440          16  CARR-HD3        PIC X(4).                            ECS084
00441          16  FILLER          PIC X(35).                           ECS084
00442      12  FINL-HD2.                                                ECS084
00443          16  FD-DESC         PIC X(9)            VALUE 'CARRIER'. ECS084
00444          16  FD-CARR         PIC XX              VALUE SPACES.    ECS084
00445          16  FILLER          PIC X(6)            VALUE 'STATE '.  ECS084
00446          16  FD-ST           PIC XXX             VALUE SPACES.    ECS084
00447          16  FD-ST-NM        PIC X(15)           VALUE SPACES.    ECS084
00448      12  REIN-HD.                                                 ECS084
00449          16  FILLER          PIC X(20)           VALUE            ECS084
00450                  'REINSURANCE COMPANY '.                          ECS084
00451          16  HD-REIN         PIC X(6)            VALUE SPACES.    ECS084
00452  EJECT                                                            ECS084
00453  01  P-REC.                                                       ECS084
00454      12  P-CCSW                  PIC X.                           ECS084
00455      12  P-LN.                                                    ECS084
00456          16  P-BEN               PIC X(3).                        ECS084
00457          16  P-DESC              PIC X(10).                       ECS084
00458          16  P-COUNT             PIC ZZ,ZZZ,ZZZ-.                 ECS084
00459          16  P-DETAIL-1.                                          ECS084
00460              20  P-WRITTEN       PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00461              20  P-P78           PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00462              20  P-PRATA         PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00463              20  P-DOMICILE      PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00464              20  P-STATE         PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00465              20  P-RESERV        PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00466              20  P-ALTRSV        PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00467              20  P-REMAIN        PIC ZZZZ,ZZZ,ZZZ,ZZZ-.           ECS084
00468          16  P-DETAIL-2 REDEFINES P-DETAIL-1.                     ECS084
00469              20  P-PAID          PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00470              20  P-C78           PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00471              20  P-CRATA         PIC ZZZZ,ZZZ,ZZZ-.               ECS084
040114             20  P-CDOMI         PIC ZZZZ,ZZZ,ZZZ-.
00472              20  P-TAX           PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00473              20  P-T78           PIC ZZZZ,ZZZ,ZZZ-.               ECS084
00474              20  P-TRATA         PIC ZZZZ,ZZZ,ZZZ-.               ECS084
040114             20  P-TDOMI         PIC ZZZZ,ZZZ,ZZZ-.
040114             20  FILLER          PIC X(4).                        ECS084
00476      12  P-LN-M REDEFINES P-LN.                                   ECS084
00477          16  PM-TABLE        PIC X(5).                            ECS084
00478          16  PM-TDESC        PIC X(27).                           ECS084
00479          16  PM-BEN          PIC XXX.                             ECS084
00480          16  PM-BDESC        PIC X(11).                           ECS084
00481          16  PM-IUNDR        PIC ZZZ,ZZZ,ZZZ,ZZZ-.                ECS084
00482          16  PM-IOVER        PIC ZZZ,ZZZ,ZZZ,ZZZ-.                ECS084
00483          16  PM-GUNDR        PIC ZZ,ZZZ,ZZZ,ZZZ,ZZZ-.             ECS084
00484          16  PM-GOVER        PIC ZZZ,ZZZ,ZZZ,ZZZ-.                ECS084
00485          16  PM-TOTAL        PIC ZZ,ZZZ,ZZZ,ZZZ,ZZZ-.             ECS084
00486                                                                   ECS084
00487  COPY ELCDATE.                                                       CL**6
00488                              COPY ELCDTECX.                       ECS084
00489  EJECT                                                            ECS084
00490                              COPY ELCDTEVR.                       ECS084
00491  EJECT                                                            ECS084
00492                                                                   ECS084
00493  PROCEDURE DIVISION.                                              ECS084
00494                                                                   ECS084
00495  0000-SET-START.                                                  ECS084
CIDMOD
CIDMOD     ACCEPT      WS-REPORT-TITLE.
CIDMOD
pemmod     if ws-report-title (1:1) = 'T'
pemmod        move ' TAX  '            to ws-hd1-override (1:6)
pemmod     else
pemmod        move ' STAT '            to ws-hd1-override (1:6)
pemmod     end-if

CIDMOD     IF WS-REPORT-TITLE (2:10) = 'NON CREDIT'
CIDMOD        MOVE 'NON CREDIT'        TO WS-HD1-OVERRIDE (7:12)
CIDMOD     ELSE
CIDMOD        IF WS-REPORT-TITLE (2:6) = 'CREDIT'
CIDMOD           MOVE 'CREDIT'  TO WS-HD1-OVERRIDE (7:12)
CIDMOD        ELSE
CIDMOD           MOVE 'ALL BUSINESS'   TO WS-HD1-OVERRIDE (7:12)
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
00496                              COPY ELCDTERX.                       ECS084
00497                                                                   ECS084
00498      MOVE WS-CURRENT-DATE TO HD-RD.                               ECS084
00499      MOVE COMPANY-NAME    TO HD-CO.                               ECS084
00500      MOVE ALPH-DATE       TO HD-DT.                               ECS084
00501      MOVE +1              TO ZERO-SW.                             ECS084
00502      PERFORM 8000-INITIALIZE-TABLES-MAX.                          ECS084
00503                                                                   ECS084
00504      GO TO 0100-SORT-RTN.                                         ECS084
00505                                                                   ECS084
00506  0100-SORT-RTN SECTION.                                           ECS084
00507      SORT SORTFL ON ASCENDING KEY S-PARM                          ECS084
00508          INPUT PROCEDURE 0200-INPUT-RTN   THRU 0299-INPUT-XIT     ECS084
00509          OUTPUT PROCEDURE 0300-OUTPUT-RTN THRU 1799-OUTPUT-XIT.   ECS084
00510                                                                   ECS084
00511      IF SORT-RETURN NOT = ZEROS                                   ECS084
00512         MOVE 0101              TO WS-RETURN-CODE                  ECS084
00513         MOVE ' ERROR IN SORT ' TO WS-ABEND-MESSAGE                ECS084
00514         GO TO ABEND-PGM.                                          ECS084
00515                                                                   ECS084
00516      GO TO 9010-EOJ-RTN.                                          ECS084
00517  EJECT                                                            ECS084
00518  0200-INPUT-RTN SECTION.                                          ECS084
00519      OPEN INPUT REPTFL.                                           ECS084
040114
040114     IF DTE-CLIENT NOT = 'DCC'
040114        MOVE HD5-AHL             TO HD5
040114        MOVE HD6-AHL             TO HD6
040114        MOVE HD8-AHL             TO HD8
040114     END-IF.
00520                                                                   ECS084
00521  0210-READ-RTN.                                                   ECS084
00522      READ REPTFL AT END                                           ECS084
00523          GO TO 0220-END-INPUT.                                    ECS084
00524                                                                   ECS084
00525      MOVE RPT-REC TO SRT-REC.                                     ECS084
00526                                                                   ECS084
00527      RELEASE SRT-REC.                                             ECS084
00528                                                                   ECS084
00529      GO TO 0210-READ-RTN.                                         ECS084
00530                                                                   ECS084
00531  0220-END-INPUT.
pemuni
pemuni     OPEN OUTPUT PRNTR
pemuni*    MOVE +0      TO HD-SW                                        ECS084
pemuni*    PERFORM 1500-HD-RTN THRU 1599-HD-XIT
pemuni*    MOVE +1         TO HD-SW                                     ECS084
pemuni                                                                  ECS084
00532      CLOSE REPTFL.                                                ECS084
00533                                                                   ECS084
00534  0299-INPUT-XIT.                                                  ECS084
00535      EXIT.                                                        ECS084
00536  EJECT                                                            ECS084
00537  0300-OUTPUT-RTN SECTION.                                         ECS084
pemuni*    OPEN OUTPUT PRNTR.                                           ECS084
00539                                                                   ECS084
00540      MOVE '1'       TO P-REC.                                     ECS084
00541      MOVE +66       TO LNCTR.                                     ECS084
00542      MOVE +0        TO PGCTR.                                     ECS084
00543      MOVE LOW-VALUE TO PRE-SEQ.                                   ECS084
00544                                                                   ECS084
00545      PERFORM 1300-LOAD-TABLES THRU 1399-LOAD-TAB-XIT.             ECS084
00546                                                                   ECS084
00547      PERFORM 1400-LOAD-MORT THRU 1499-LOAD-MORT-XIT.              ECS084
00548                                                                   ECS084
00549  0310-RETURN-LOOP.                                                ECS084
00550      RETURN SORTFL AT END                                         ECS084
00551          GO TO 1700-END-OUTPUT.                                   ECS084
00552                                                                   ECS084
00553      MOVE SRT-REC TO WORK-REC.                                    ECS084
00554      MOVE W-SEQ   TO CUR-SEQ.                                     ECS084
00555                                                                   ECS084
00556      IF CUR-SEQ = PRE-SEQ                                         ECS084
00557          GO TO 0320-SAME-SEQ.                                     ECS084
00558                                                                   ECS084
00559      PERFORM 0400-ACCUM-RTN THRU 0499-ACCUM-XIT.                  ECS084
00560                                                                   ECS084
00561  0320-SAME-SEQ.                                                   ECS084
00562      ADD W-DUP      TO X-DUP.                                     ECS084
00563      ADD W-COUNT    TO X-COUNT.                                   ECS084
00564      ADD W-WRITTEN  TO X-WRITTEN.                                 ECS084
00565      ADD W-P78      TO X-P78.                                     ECS084
00566      ADD W-PRATA    TO X-PRATA.                                   ECS084
00567      ADD W-DOMICILE TO X-DOMICILE.                                ECS084
00568      ADD W-STATE    TO X-STATE.                                   ECS084
00569      ADD W-RESERV   TO X-RESERV.                                  ECS084
00570      ADD W-ALTRSV   TO X-ALTRSV.                                  ECS084
00571      ADD W-REMAIN   TO X-REMAIN.                                  ECS084
00572      ADD W-PAID     TO X-PAID.                                    ECS084
00573      ADD W-C78      TO X-C78.                                     ECS084
00574      ADD W-CRATA    TO X-CRATA.                                   ECS084
052303     IF DTE-CLIENT NOT = 'DCC'
040114         ADD W-CDOMI    TO X-CDOMI
040114         ADD W-TDOMI    TO X-TDOMI
00575          ADD W-TAX      TO X-TAX
00576          ADD W-T78      TO X-T78
00577          ADD W-TRATA    TO X-TRATA.
00578      ADD W-IUNDR    TO X-IUNDR.                                   ECS084
00579      ADD W-IOVER    TO X-IOVER.                                   ECS084
00580      ADD W-GUNDR    TO X-GUNDR.                                   ECS084
00581      ADD W-GOVER    TO X-GOVER.                                   ECS084
00582                                                                   ECS084
00583      GO TO 0310-RETURN-LOOP.                                      ECS084
00584  EJECT                                                            ECS084
00585  0400-ACCUM-RTN.                                                  ECS084
00586      IF PRE-SEQ = LOW-VALUE                                       ECS084
00587          PERFORM 0500-BREAK-RTN THRU 0599-BREAK-XIT               ECS084
00588          GO TO 0499-ACCUM-XIT.                                    ECS084
00589                                                                   ECS084
00590      MOVE PRE-MORT TO X2.                                         ECS084
00591      MOVE +1       TO X3.                                         ECS084
00592      MOVE PRE-BEN  TO WX-BEN.                                     ECS084
00593      MOVE PRE-LAH  TO WX-TYP.                                     ECS084
00594                                                                   ECS084
00595  0410-LOOP-AX3.                                                   ECS084
00596      IF WX-POINTER = X-POINTER (X3)                               ECS084
00597          NEXT SENTENCE                                            ECS084
00598      ELSE                                                         ECS084
00599          ADD +1 TO X3                                             ECS084
00600          GO TO 0410-LOOP-AX3.                                     ECS084
00601                                                                   ECS084
00602      IF PRE-MORT = ZEROS                                          ECS084
00603          GO TO 0420-INTL-MORT.                                    ECS084
00604                                                                   ECS084
00605      MOVE XM-AMTS (X2 X3) TO SUB-M-TOTALS.                        ECS084
00606                                                                   ECS084
00607      ADD X-IUNDR TO S-IUNDR.                                      ECS084
00608      ADD X-IOVER TO S-IOVER.                                      ECS084
00609      ADD X-GUNDR TO S-GUNDR.                                      ECS084
00610      ADD X-GOVER TO S-GOVER.                                      ECS084
00611                                                                   ECS084
00612      MOVE SUB-M-TOTALS TO XM-AMTS (X2 X3).                        ECS084
00613                                                                   ECS084
00614  0420-INTL-MORT.                                                  ECS084
00615      MOVE CUR-MORT TO PRE-MORT.                                   ECS084
00616      MOVE +0       TO X-IUNDR                                     ECS084
00617                       X-IOVER                                     ECS084
00618                       X-GUNDR                                     ECS084
00619                       X-GOVER.                                    ECS084
00620                                                                   ECS084
00621      IF CUR-SEQ = PRE-SEQ                                         ECS084
00622          GO TO 0499-ACCUM-XIT.                                    ECS084
00623  EJECT                                                            ECS084
00624  0430-ACCUM-REST.                                                 ECS084
00625      MOVE XT-AMTS (X3) TO SUB-TOTALS.                             ECS084
00626                                                                   ECS084
00627      ADD X-DUP      TO S-DUP.                                     ECS084
00628      ADD X-COUNT    TO S-COUNT.                                   ECS084
00629      ADD X-WRITTEN  TO S-WRITTEN.                                 ECS084
00630      ADD X-P78      TO S-P78.                                     ECS084
00631      ADD X-PRATA    TO S-PRATA.                                   ECS084
00632      ADD X-DOMICILE TO S-DOMICILE.                                ECS084
00633      ADD X-STATE    TO S-STATE.                                   ECS084
00634      ADD X-RESERV   TO S-RESERV.                                  ECS084
00635      ADD X-ALTRSV   TO S-ALTRSV.                                  ECS084
00636      ADD X-REMAIN   TO S-REMAIN.                                  ECS084
00637      ADD X-PAID     TO S-PAID.                                    ECS084
00638      ADD X-C78      TO S-C78.                                     ECS084
00639      ADD X-CRATA    TO S-CRATA.                                   ECS084
052303     IF DTE-CLIENT NOT = 'DCC'
040114         ADD X-CDOMI    TO S-CDOMI
040114         ADD X-TDOMI    TO S-TDOMI
00640          ADD X-TAX      TO S-TAX
00641          ADD X-T78      TO S-T78
00642          ADD X-TRATA    TO S-TRATA. 
00643                                                                   ECS084
00644      MOVE SUB-TOTALS TO XT-AMTS (X3).                             ECS084
00645                                                                   ECS084
00646  0440-INTL-REST.                                                  ECS084
00647      MOVE CUR-LAH TO PRE-LAH.                                     ECS084
00648      MOVE CUR-BEN TO PRE-BEN.                                     ECS084
00649      MOVE +0      TO X-DUP                                        ECS084
00650                      X-COUNT                                      ECS084
00651                      X-WRITTEN                                    ECS084
00652                      X-P78                                        ECS084
00653                      X-PRATA                                      ECS084
00654                      X-DOMICILE                                   ECS084
00655                      X-STATE                                      ECS084
00656                      X-RESERV                                     ECS084
00657                      X-ALTRSV                                     ECS084
00658                      X-REMAIN                                     ECS084
00659                      X-PAID                                       ECS084
00660                      X-C78                                        ECS084
00661                      X-CRATA                                      ECS084
040114                     X-CDOMI
040114                     X-TDOMI
00662                      X-TAX                                        ECS084
00663                      X-T78                                        ECS084
00664                      X-TRATA.                                     ECS084
00665                                                                   ECS084
00666      IF CUR-SEQ NOT = PRE-SEQ                                     ECS084
00667          PERFORM 0500-BREAK-RTN THRU 0599-BREAK-XIT.              ECS084
00668                                                                   ECS084
00669  0499-ACCUM-XIT.                                                  ECS084
00670      EXIT.                                                        ECS084
00671  EJECT                                                            ECS084
00672  0500-BREAK-RTN.                                                  ECS084
00673      IF PRE-SEQ = LOW-VALUE                                       ECS084
00674          GO TO 0550-INTL-ALL.                                     ECS084
00675                                                                   ECS084
00676      IF PRE-ST NOT = HIGH-VALUE AND                               ECS084
00677         PRE-CO = HIGH-VALUE                                       ECS084
00678          GO TO 0520-OVERALL-HD.                                   ECS084
00679                                                                   ECS084
00680  0510-REGULAR-HD.                                                 ECS084
00681      IF PRE-ST NOT = HIGH-VALUE                                   ECS084
00682          MOVE ST-HD2 TO SUB-HD2                                   ECS084
00683          MOVE ST-HD3 TO SUB-HD3                                   ECS084
00684          GO TO 0530-BYPASS-HD.                                    ECS084
00685                                                                   ECS084
00686      IF PRE-CO NOT = HIGH-VALUE                                   ECS084
00687          MOVE CO-HD2 TO SUB-HD2                                   ECS084
00688          MOVE CO-HD3 TO SUB-HD3                                   ECS084
00689          GO TO 0530-BYPASS-HD.                                    ECS084
00690                                                                   ECS084
00691      IF PRE-CARR NOT = HIGH-VALUE                                 ECS084
00692          MOVE CARR-HD2 TO SUB-HD2                                 ECS084
00693          MOVE CARR-HD3 TO SUB-HD3                                 ECS084
00694          GO TO 0530-BYPASS-HD.                                    ECS084
00695                                                                   ECS084
00696      MOVE 'FINAL TOTALS' TO SUB-HD2.                              ECS084
00697      MOVE SPACES         TO SUB-HD3.                              ECS084
00698                                                                   ECS084
00699      GO TO 0530-BYPASS-HD.                                        ECS084
00700                                                                   ECS084
00701  0520-OVERALL-HD.                                                 ECS084
00702      IF PRE-CARR = HIGH-VALUE                                     ECS084
00703          MOVE SPACES TO FD-DESC  FD-CARR.                         ECS084
00704                                                                   ECS084
00705      MOVE FINL-HD2 TO SUB-HD2.                                    ECS084
00706      MOVE SPACES   TO SUB-HD3.                                    ECS084
00707  EJECT                                                            ECS084
00708  0530-BYPASS-HD.                                                  ECS084
00709      MOVE PRE-YR     TO SUB-HD2-YR                                ECS084
121610*                       DC-ALPHA-YEAR.                            ECS084
121610*    MOVE '7'        TO DC-OPTION-CODE.                           ECS084
121610*    PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.                 ECS084
121610*                                                                    CL**4
121610*    IF DATE-CONVERSION-ERROR                                     ECS084
121610*       IF ONLY-CENTURY                                           ECS084
121610*          MOVE DC-ALPHA-CEN-N TO HEADER-MSG-2                    ECS084
121610*       ELSE                                                         CL**2
121610*          MOVE 'ERROR CREATING CENTURY' TO WS-ABEND-MESSAGE         CL**2
121610*          MOVE DC-ERROR-CODE TO WS-ABEND-FILE-STATUS                CL**2
121610*          GO TO ABEND-PGM                                           CL**4
121610*       END-IF                                                    ECS084
121610*    ELSE                                                         ECS084
121610*       MOVE 'ERROR FINDING CENTURY' TO WS-ABEND-MESSAGE          ECS084
121610*       MOVE DC-ERROR-CODE TO WS-ABEND-FILE-STATUS                ECS084
121610*       GO TO ABEND-PGM.                                             CL**4
121610                                                                  ECS084
00727      MOVE HEADER-MSG TO SUB-HD2-YEAR.                             ECS084
00728      MOVE PRE-CODE   TO X1.                                       ECS084
00729                                                                   ECS084
00730      PERFORM 1100-ROLL-RTN THRU 1199-ROLL-XIT.                    ECS084
00731                                                                   ECS084
00732      IF SAV-11 NOT = PRE-11                                       ECS084
00733          MOVE PRE-11 TO SAV-11                                    ECS084
00734          PERFORM 1500-HD-RTN THRU 1599-HD-XIT.                    ECS084
00735                                                                   ECS084
00736      PERFORM 0600-BLD-RTN THRU 0699-BLD-XIT.                      ECS084
00737                                                                   ECS084
00738      MOVE CUR-YR   TO PRE-YR.                                     ECS084
00739      MOVE CUR-CODE TO PRE-CODE.                                   ECS084
00740                                                                   ECS084
00741      IF CUR-SEQ = PRE-SEQ                                         ECS084
00742          GO TO 0560-INTL-FINAL.                                   ECS084
00743                                                                   ECS084
00744      MOVE SPACES TO SUB-HD2-YR  SUB-HD2-YEAR.                     ECS084
00745                                                                   ECS084
00746      PERFORM 1500-HD-RTN THRU 1599-HD-XIT.                        ECS084
00747                                                                   ECS084
00748      MOVE +1 TO X1.                                               ECS084
00749                                                                   ECS084
00750  0540-PRT-ALL-YEARS.                                              ECS084
00751      MOVE PT-LEVELS (X1)   TO XT-TOTALS.                          ECS084
00752                                                                   ECS084
00753      IF X1 = +1                                                   ECS084
00754          PERFORM 0580-MOVE-GROSS THRU 0580-EXIT                   ECS084
00755                  VARYING                                          ECS084
00756              M1 FROM 1 BY 1                                       ECS084
00757                  UNTIL                                            ECS084
PEMTMP*            M1 GREATER THAN CLAS-MAXM                            ECS084
PEMTMP*                OR                                               ECS084
020113             M1 GREATER THAN +150                                 ECS084
00761                                                                   ECS084
00762      ELSE                                                         ECS084
00763          PERFORM 0585-MOVE-REINS THRU 0585-EXIT                   ECS084
00764                  VARYING                                          ECS084
00765              M1 FROM 1 BY 1                                       ECS084
00766                  UNTIL                                            ECS084
PEMTMP*            M1 GREATER THAN CLAS-MAXM                            ECS084
PEMTMP*                OR                                               ECS084
020113             M1 GREATER THAN +150.                                ECS084
00770                                                                   ECS084
00771      PERFORM 0600-BLD-RTN THRU 0699-BLD-XIT.                      ECS084
00772                                                                   ECS084
00773      IF X1 LESS THAN +2                                           ECS084
00774          ADD +1 TO X1                                             ECS084
00775          GO TO 0540-PRT-ALL-YEARS.                                ECS084
00776                                                                   ECS084
00777  0550-INTL-ALL.                                                   ECS084
00778      IF CUR-SEQ = HIGH-VALUE                                      ECS084
00779          GO TO 0599-BREAK-XIT.                                    ECS084
00780                                                                   ECS084
00781      MOVE +1 TO ZERO-SW.                                          ECS084
00782  EJECT                                                            ECS084
00783  0560-INTL-FINAL.                                                 ECS084
00784      PERFORM 8000-INITIALIZE-TABLES-MAX.                          ECS084
00785                                                                   ECS084
00786      MOVE CUR-SEQ   TO PRE-SEQ.                                   ECS084
00787      MOVE PRE-REIN  TO HD-REIN.                                   ECS084
00788      MOVE PRE-CARR  TO HD-CARR  FD-CARR.                          ECS084
00789      MOVE PRE-CO    TO HD-COMP.                                   ECS084
00790      MOVE PRE-ST    TO HD-ST  STATE-L  FD-ST.                     ECS084
00791      MOVE 'CARRIER' TO FD-DESC.                                   ECS084
00792                                                                   ECS084
00793      IF PRE-ST = HIGH-VALUE                                       ECS084
00794          GO TO 0599-BREAK-XIT.                                    ECS084
00795                                                                   ECS084
00796      MOVE CLAS-STARTS     TO CLAS-INDEXS.                         ECS084
00797      MOVE 'INVALID STATE' TO HD-ST-NM  FD-ST-NM.                  ECS084
00798                                                                   ECS084
00799  0570-FIND-ST-DESC.                                               ECS084
00800      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                        ECS084
00801          GO TO 0599-BREAK-XIT.                                    ECS084
00802                                                                   ECS084
00803      IF STATE-SUB (CLAS-INDEXS) NOT = STATE-L                     ECS084
00804          ADD +1 TO CLAS-INDEXS                                    ECS084
00805          GO TO 0570-FIND-ST-DESC.                                 ECS084
00806                                                                   ECS084
00807      MOVE STATE-PIC (CLAS-INDEXS) TO HD-ST-NM  FD-ST-NM.          ECS084
00808                                                                   ECS084
00809      GO TO 0599-BREAK-XIT.                                        ECS084
00810                                                                   ECS084
00811  0580-MOVE-GROSS.                                                 ECS084
00812                                                                   ECS084
00813      MOVE PT-M-CODES-G (M1)      TO XM-LEVEL (M1).                ECS084
00814                                                                   ECS084
00815  0580-EXIT.                                                       ECS084
00816      EXIT.                                                        ECS084
00817                                                                   ECS084
00818  0585-MOVE-REINS.                                                 ECS084
00819                                                                   ECS084
00820      MOVE PT-M-CODES-R (M1)      TO XM-LEVEL (M1).                ECS084
00821                                                                   ECS084
00822  0585-EXIT.                                                       ECS084
00823      EXIT.                                                        ECS084
00824                                                                   ECS084
00825  0599-BREAK-XIT.                                                  ECS084
00826      EXIT.                                                        ECS084
00827  EJECT                                                            ECS084
00828  0600-BLD-RTN.                                                    ECS084
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS084
00830          PERFORM 1500-HD-RTN THRU 1599-HD-XIT.                    ECS084
00831                                                                   ECS084
00832      IF X1 = +1                                                   ECS084
00833          MOVE 'GROSS'       TO HD-DESC                            ECS084
00834      ELSE                                                         ECS084
00835          MOVE 'REINSURANCE' TO HD-DESC.                           ECS084
00836                                                                   ECS084
00837      MOVE +0      TO LIFE-SW                                      ECS084
00838                      AH-SW                                        ECS084
00839                      HD-SW                                        ECS084
00840                      S-DUP                                        ECS084
00841                      S-COUNT                                      ECS084
00842                      S-WRITTEN                                    ECS084
00843                      S-P78                                        ECS084
00844                      S-PRATA                                      ECS084
00845                      S-DOMICILE                                   ECS084
00846                      S-STATE                                      ECS084
00847                      S-RESERV                                     ECS084
00848                      S-ALTRSV                                     ECS084
00849                      S-REMAIN.                                    ECS084
00850      MOVE +1      TO X3.                                          ECS084
00851  EJECT                                                            ECS084
00852  0610-LOOP-LIFE.                                                  ECS084
00853      IF X3 GREATER THAN MAX-BEN                                   ECS084
00854          GO TO 0620-OUT-LIFE.                                     ECS084
00855                                                                   ECS084
00856      IF X-TYP (X3) NOT = 1                                        ECS084
00857          GO TO 0620-OUT-LIFE.                                     ECS084
00858                                                                   ECS084
00859      MOVE XT-AMTS (X3) TO X-DETL.                                 ECS084
00860                                                                   ECS084
00861      IF X-COUNT = +0                                              ECS084
00862          ADD +1 TO X3                                             ECS084
00863          GO TO 0610-LOOP-LIFE.                                    ECS084
00864                                                                   ECS084
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS084
00866          MOVE +0      TO HD-SW                                    ECS084
00867          PERFORM 1500-HD-RTN THRU 1599-HD-XIT.                    ECS084
00868                                                                   ECS084
00869      IF HD-SW = +0                                                ECS084
00870          MOVE +1      TO LIFE-SW  HD-SW                           ECS084
00871          MOVE '-'     TO P-CCSW                                   ECS084
00872          MOVE HD4     TO P-LN                                     ECS084
00873          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
00874          MOVE HD5     TO P-LN                                     ECS084
00875          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
00876          MOVE HD6     TO P-LN                                     ECS084
00877          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
00878          MOVE '0' TO P-CCSW.                                      ECS084
00879                                                                   ECS084
00880      MOVE X-BEN (X3)  TO PX-BEN.                                  ECS084
00881      MOVE X-DESC (X3) TO PX-DESC.                                 ECS084
00882                                                                   ECS084
00883      ADD X-DUP      TO S-DUP.                                     ECS084
00884      ADD X-COUNT    TO S-COUNT.                                   ECS084
00885      ADD X-WRITTEN  TO S-WRITTEN.                                 ECS084
00886      ADD X-P78      TO S-P78.                                     ECS084
00887      ADD X-PRATA    TO S-PRATA.                                   ECS084
00888      ADD X-DOMICILE TO S-DOMICILE.                                ECS084
00889      ADD X-STATE    TO S-STATE.                                   ECS084
00890      ADD X-RESERV   TO S-RESERV.                                  ECS084
00891      ADD X-ALTRSV   TO S-ALTRSV.                                  ECS084
00892      ADD X-REMAIN   TO S-REMAIN.                                  ECS084
00893                                                                   ECS084
00894      PERFORM 0900-PRT-LINE THRU 0999-PRT-LINE-XIT.                ECS084
00895                                                                   ECS084
00896      ADD +1 TO X3.                                                ECS084
00897                                                                   ECS084
00898      GO TO 0610-LOOP-LIFE.                                        ECS084
00899  EJECT                                                            ECS084
00900  0620-OUT-LIFE.                                                   ECS084
00901      MOVE SUB-TOTALS TO TOTALS.                                   ECS084
00902                                                                   ECS084
00903      IF LIFE-SW = 0                                               ECS084
00904          GO TO 0640-LOOP-AH.                                      ECS084
00905                                                                   ECS084
00906      MOVE 'TOTAL LIFE' TO PX-DESC.                                ECS084
00907      MOVE SPACES       TO PX-BEN.                                 ECS084
00908      MOVE SUB-TOTALS   TO X-DETL.                                 ECS084
00909                                                                   ECS084
00910      PERFORM 0900-PRT-LINE THRU 0999-PRT-LINE-XIT.                ECS084
00911                                                                   ECS084
00912      MOVE '0' TO P-CCSW.                                          ECS084
00913                                                                   ECS084
00914  0630-ZERO-SUBTOTAL.                                              ECS084
00915      MOVE +0 TO S-DUP                                             ECS084
00916                 S-COUNT                                           ECS084
00917                 S-WRITTEN                                         ECS084
00918                 S-P78                                             ECS084
00919                 S-PRATA                                           ECS084
00920                 S-DOMICILE                                        ECS084
00921                 S-STATE                                           ECS084
00922                 S-RESERV                                          ECS084
00923                 S-ALTRSV                                          ECS084
00924                 S-REMAIN.                                         ECS084
00925  EJECT                                                            ECS084
00926  0640-LOOP-AH.                                                    ECS084
00927      IF X3 GREATER THAN MAX-BEN                                   ECS084
00928          GO TO 0650-OUT-AH.                                       ECS084
00929                                                                   ECS084
00930      IF X-TYP (X3) NOT = 2                                        ECS084
00931          GO TO 0640-LOOP-AH.                                      ECS084
00932                                                                   ECS084
00933      MOVE XT-AMTS (X3) TO X-DETL.                                 ECS084
00934                                                                   ECS084
00935      IF X-COUNT = +0                                              ECS084
00936          ADD +1 TO X3                                             ECS084
00937          GO TO 0640-LOOP-AH.                                      ECS084
00938                                                                   ECS084
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS084
00940          MOVE +0      TO HD-SW                                    ECS084
00941          PERFORM 1500-HD-RTN THRU 1599-HD-XIT.                    ECS084
00942                                                                   ECS084
00943      IF HD-SW = +0                                                ECS084
00944          MOVE +1      TO HD-SW                                    ECS084
00945          MOVE '-'     TO P-CCSW                                   ECS084
00946          MOVE HD4     TO P-LN                                     ECS084
00947          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
00948          MOVE HD5     TO P-LN                                     ECS084
00949          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
00950          MOVE HD6     TO P-LN                                     ECS084
00951          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
00952          MOVE '0' TO P-CCSW.                                      ECS084
00953                                                                   ECS084
00954      MOVE +1 TO AH-SW.                                            ECS084
00955                                                                   ECS084
00956      ADD X-DUP TO S-DUP.                                          ECS084
00957                                                                   ECS084
00958      MOVE X-BEN (X3)  TO PX-BEN.                                  ECS084
00959      MOVE X-DESC (X3) TO PX-DESC.                                 ECS084
00960                                                                   ECS084
00961      ADD X-COUNT    TO S-COUNT.                                   ECS084
00962      ADD X-WRITTEN  TO S-WRITTEN.                                 ECS084
00963      ADD X-P78      TO S-P78.                                     ECS084
00964      ADD X-PRATA    TO S-PRATA.                                   ECS084
00965      ADD X-DOMICILE TO S-DOMICILE.                                ECS084
00966      ADD X-STATE    TO S-STATE.                                   ECS084
00967      ADD X-RESERV   TO S-RESERV.                                  ECS084
00968      ADD X-ALTRSV   TO S-ALTRSV.                                  ECS084
00969      ADD X-REMAIN   TO S-REMAIN.                                  ECS084
00970                                                                   ECS084
00971      PERFORM 0900-PRT-LINE THRU 0999-PRT-LINE-XIT.                ECS084
00972                                                                   ECS084
00973      ADD +1 TO X3.                                                ECS084
00974                                                                   ECS084
00975      GO TO 0640-LOOP-AH.                                          ECS084
00976  EJECT                                                            ECS084
00977  0650-OUT-AH.                                                     ECS084
00978      IF AH-SW = +0                                                ECS084
00979          GO TO 0690-PRE-XIT.                                      ECS084
00980                                                                   ECS084
00981      MOVE 'TOTAL A-H' TO PX-DESC.                                 ECS084
00982      MOVE SPACES      TO PX-BEN.                                  ECS084
00983      MOVE SUB-TOTALS  TO X-DETL.                                  ECS084
00984                                                                   ECS084
00985      PERFORM 0900-PRT-LINE THRU 0999-PRT-LINE-XIT.                ECS084
00986                                                                   ECS084
00987      MOVE '0' TO P-CCSW.                                          ECS084
00988                                                                   ECS084
00989      ADD S-DUP      TO T-DUP.                                     ECS084
00990      ADD S-COUNT    TO T-COUNT.                                   ECS084
00991      ADD S-WRITTEN  TO T-WRITTEN.                                 ECS084
00992      ADD S-P78      TO T-P78.                                     ECS084
00993      ADD S-PRATA    TO T-PRATA.                                   ECS084
00994      ADD S-DOMICILE TO T-DOMICILE.                                ECS084
00995      ADD S-STATE    TO T-STATE.                                   ECS084
00996      ADD S-RESERV   TO T-RESERV.                                  ECS084
00997      ADD S-ALTRSV   TO T-ALTRSV.                                  ECS084
00998      ADD S-REMAIN   TO T-REMAIN.                                  ECS084
00999                                                                   ECS084
01000  0660-PRT-TOTALS.                                                 ECS084
01001      IF LIFE-SW = +0                                              ECS084
01002          GO TO 0690-PRE-XIT.                                      ECS084
01003                                                                   ECS084
01004      MOVE TOTALS TO X-DETL.                                       ECS084
01005                                                                   ECS084
01006      SUBTRACT X-DUP FROM X-COUNT.                                 ECS084
01007                                                                   ECS084
01008      MOVE '  TOTAL  ' TO PX-DESC.                                 ECS084
01009      MOVE SPACES      TO PX-BEN.                                  ECS084
01010                                                                   ECS084
01011      PERFORM 0900-PRT-LINE THRU 0999-PRT-LINE-XIT.                ECS084
01012                                                                   ECS084
01013  0690-PRE-XIT.                                                    ECS084
01014      PERFORM 0700-BLD-RTN THRU 0799-BLD-XIT.                      ECS084
01015                                                                   ECS084
01016  0699-BLD-XIT.                                                    ECS084
01017      EXIT.                                                        ECS084
01018  EJECT                                                            ECS084
01019  0700-BLD-RTN.                                                    ECS084
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS084
01021          PERFORM 1500-HD-RTN THRU 1599-HD-XIT.                    ECS084
01022                                                                   ECS084
01023      IF X1 = +1                                                   ECS084
01024          MOVE 'GROSS'       TO HD-DESC                            ECS084
01025      ELSE                                                         ECS084
01026          MOVE 'REINSURANCE' TO HD-DESC.                           ECS084
01027                                                                   ECS084
01028      MOVE +0      TO LIFE-SW                                      ECS084
01029                      AH-SW                                        ECS084
01030                      HD-SW                                        ECS084
01031                      S-PAID                                       ECS084
01032                      S-C78                                        ECS084
01033                      S-CRATA                                      ECS084
040114                     S-CDOMI
040114                     S-TDOMI
01034                      S-TAX                                        ECS084
01035                      S-T78                                        ECS084
01036                      S-TRATA.                                     ECS084
01037      MOVE +1      TO X3.                                          ECS084
01038  EJECT                                                            ECS084
01039  0710-LOOP-LIFE.                                                  ECS084
01040      IF X3 GREATER THAN MAX-BEN                                   ECS084
01041          GO TO 0720-OUT-LIFE.                                     ECS084
01042                                                                   ECS084
01043      IF X-TYP (X3) NOT = 1                                        ECS084
01044          GO TO 0720-OUT-LIFE.                                     ECS084
01045                                                                   ECS084
01046      MOVE XT-AMTS (X3) TO X-DETL.                                 ECS084
01047                                                                   ECS084
01048      IF X-COUNT = +0                                              ECS084
01049          ADD +1 TO X3                                             ECS084
01050          GO TO 0710-LOOP-LIFE.                                    ECS084
01051                                                                   ECS084
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS084
01053          MOVE +0      TO HD-SW                                    ECS084
01054          PERFORM 1500-HD-RTN THRU 1599-HD-XIT.                    ECS084
01055                                                                   ECS084
01056      IF HD-SW = +0                                                ECS084
01057          MOVE +1      TO LIFE-SW  HD-SW                           ECS084
01058          MOVE '-'     TO P-CCSW                                   ECS084
01059          MOVE HD7     TO P-LN                                     ECS084
01060          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
01061          MOVE HD8     TO P-LN                                     ECS084
01062          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
01063          MOVE '0' TO P-CCSW.                                      ECS084
01064                                                                   ECS084
01065      MOVE X-BEN (X3)  TO PX-BEN.                                  ECS084
01066      MOVE X-DESC (X3) TO PX-DESC.                                 ECS084
01067                                                                   ECS084
01068      ADD X-PAID     TO S-PAID.                                    ECS084
01069      ADD X-C78      TO S-C78.                                     ECS084
01070      ADD X-CRATA    TO S-CRATA.                                   ECS084
052303     IF DTE-CLIENT NOT = 'DCC'
040114         ADD X-CDOMI    TO S-CDOMI
040114         ADD X-TDOMI    TO S-TDOMI
01071          ADD X-TAX      TO S-TAX
01072          ADD X-T78      TO S-T78
01073          ADD X-TRATA    TO S-TRATA. 
01074                                                                   ECS084
01075      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS084
01076                                                                   ECS084
01077      ADD +1 TO X3.                                                ECS084
01078                                                                   ECS084
01079      GO TO 0710-LOOP-LIFE.                                        ECS084
01080  EJECT                                                            ECS084
01081  0720-OUT-LIFE.                                                   ECS084
01082      MOVE SUB-TOTALS TO TOTALS.                                   ECS084
01083                                                                   ECS084
01084      IF LIFE-SW = 0                                               ECS084
01085          GO TO 0740-LOOP-AH.                                      ECS084
01086                                                                   ECS084
01087      MOVE 'TOTAL LIFE' TO PX-DESC.                                ECS084
01088      MOVE SPACES       TO PX-BEN.                                 ECS084
01089      MOVE SUB-TOTALS   TO X-DETL.                                 ECS084
01090                                                                   ECS084
01091      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS084
01092                                                                   ECS084
01093      MOVE '0' TO P-CCSW.                                          ECS084
01094                                                                   ECS084
01095  0730-ZERO-SUBTOTAL.                                              ECS084
01096      MOVE +0 TO S-PAID                                            ECS084
01097                 S-C78                                             ECS084
01098                 S-CRATA                                           ECS084
040114                S-CDOMI
040114                S-TDOMI
01099                 S-TAX                                             ECS084
01100                 S-T78                                             ECS084
01101                 S-TRATA.                                          ECS084
01102  EJECT                                                            ECS084
01103  0740-LOOP-AH.                                                    ECS084
01104      IF X3 GREATER THAN MAX-BEN                                   ECS084
01105          GO TO 0750-OUT-AH.                                       ECS084
01106                                                                   ECS084
01107      IF X-TYP (X3) NOT = 2                                        ECS084
01108          GO TO 0740-LOOP-AH.                                      ECS084
01109                                                                   ECS084
01110      MOVE XT-AMTS (X3) TO X-DETL.                                 ECS084
01111                                                                   ECS084
01112      IF X-COUNT = +0                                              ECS084
01113          ADD +1 TO X3                                             ECS084
01114          GO TO 0740-LOOP-AH.                                      ECS084
01115                                                                   ECS084
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS084
01117          MOVE +0      TO HD-SW                                    ECS084
01118          PERFORM 1500-HD-RTN THRU 1599-HD-XIT.                    ECS084
01119                                                                   ECS084
01120      IF HD-SW = +0                                                ECS084
01121          MOVE +1      TO HD-SW                                    ECS084
01122          MOVE '-'     TO P-CCSW                                   ECS084
01123          MOVE HD7     TO P-LN                                     ECS084
01124          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
01125          MOVE HD8     TO P-LN                                     ECS084
01126          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
01127          MOVE '0' TO P-CCSW.                                      ECS084
01128                                                                   ECS084
01129      MOVE +1          TO AH-SW.                                   ECS084
01130      MOVE X-BEN (X3)  TO PX-BEN.                                  ECS084
01131      MOVE X-DESC (X3) TO PX-DESC.                                 ECS084
01132                                                                   ECS084
01133      ADD X-PAID     TO S-PAID.                                    ECS084
01134      ADD X-C78      TO S-C78.                                     ECS084
01135      ADD X-CRATA    TO S-CRATA.                                   ECS084
052303     IF DTE-CLIENT NOT = 'DCC'
040114         ADD X-CDOMI    TO S-CDOMI
040114         ADD X-TDOMI    TO S-TDOMI
01136          ADD X-TAX      TO S-TAX
01137          ADD X-T78      TO S-T78
01138          ADD X-TRATA    TO S-TRATA. 
01139                                                                   ECS084
01140      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS084
01141                                                                   ECS084
01142      ADD +1 TO X3.                                                ECS084
01143                                                                   ECS084
01144      GO TO 0740-LOOP-AH.                                          ECS084
01145  EJECT                                                            ECS084
01146  0750-OUT-AH.                                                     ECS084
01147      IF AH-SW = +0                                                ECS084
01148          GO TO 0790-PRE-XIT.                                      ECS084
01149                                                                   ECS084
01150      MOVE 'TOTAL A-H' TO PX-DESC.                                 ECS084
01151      MOVE SPACES      TO PX-BEN.                                  ECS084
01152      MOVE SUB-TOTALS  TO X-DETL.                                  ECS084
01153                                                                   ECS084
01154      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS084
01155                                                                   ECS084
01156      MOVE '0' TO P-CCSW.                                          ECS084
01157                                                                   ECS084
01158      ADD S-PAID     TO T-PAID.                                    ECS084
01159      ADD S-C78      TO T-C78.                                     ECS084
01160      ADD S-CRATA    TO T-CRATA.                                   ECS084
052303     IF DTE-CLIENT NOT = 'DCC'
040114         ADD S-CDOMI    TO T-CDOMI
040114         ADD S-TDOMI    TO T-TDOMI
01161          ADD S-TAX      TO T-TAX
01162          ADD S-T78      TO T-T78
01163          ADD S-TRATA    TO T-TRATA.
01164                                                                   ECS084
01165  0760-PRT-TOTALS.                                                 ECS084
01166      IF LIFE-SW = +0                                              ECS084
01167          GO TO 0790-PRE-XIT.                                      ECS084
01168                                                                   ECS084
01169      MOVE TOTALS      TO X-DETL.                                  ECS084
01170                                                                   ECS084
01171      SUBTRACT X-DUP FROM X-COUNT.                                 ECS084
01172                                                                   ECS084
01173      MOVE '  TOTAL  ' TO PX-DESC.                                 ECS084
01174      MOVE SPACES      TO PX-BEN.                                  ECS084
01175                                                                   ECS084
01176      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS084
01177                                                                   ECS084
01178  0790-PRE-XIT.                                                    ECS084
01179      PERFORM 0800-PRT-MORT THRU 0899-XIT.                         ECS084
01180                                                                   ECS084
01181  0799-BLD-XIT.                                                    ECS084
01182      EXIT.                                                        ECS084
01183  EJECT                                                            ECS084
01184  0800-PRT-MORT.                                                   ECS084
01185      MOVE +0 TO X2                                                ECS084
01186                 HD-SW                                             ECS084
01187                 T-IUNDR                                           ECS084
01188                 T-IOVER                                           ECS084
01189                 T-GUNDR                                           ECS084
01190                 T-GOVER                                           ECS084
01191                 T-TOTAL.                                          ECS084
01192                                                                   ECS084
01193                                                                   ECS084
01194  0810-PRT-M2.                                                     ECS084
01195      ADD +1 TO X2.                                                ECS084
01196                                                                   ECS084
01197 *    IF X2 GREATER THAN CLAS-MAXM                                 ECS084
020113     IF X2 > +150
01198          GO TO 0840-PRT-MORT-TOTAL.                               ECS084
01199                                                                   ECS084
01200      MOVE +0 TO X3                                                ECS084
01201                 TAB-SW                                            ECS084
01202                 S-IUNDR                                           ECS084
01203                 S-IOVER                                           ECS084
01204                 S-GUNDR                                           ECS084
01205                 S-GOVER                                           ECS084
01206                 S-TOTAL.                                          ECS084
01207                                                                   ECS084
01208  0820-PRT-M3.                                                     ECS084
01209      ADD +1 TO X3.                                                ECS084
01210                                                                   ECS084
01211      IF X3 GREATER THAN MAX-BEN                                   ECS084
01212          GO TO 0830-PRT-MORT-SUB.                                 ECS084
01213                                                                   ECS084
01214      MOVE XM-AMTS (X2 X3) TO X-M-DETL.                            ECS084
01215                                                                   ECS084
01216      COMPUTE X-TOTAL = X-IUNDR + X-IOVER + X-GUNDR + X-GOVER.     ECS084
01217                                                                   ECS084
01218      IF X-TOTAL = +0                                              ECS084
01219          GO TO 0820-PRT-M3.                                       ECS084
01220                                                                   ECS084
CIDMOD     IF LNCTR GREATER THAN +50                                    ECS084
01222          MOVE +0 TO HD-SW  TAB-SW                                 ECS084
01223          PERFORM 1500-HD-RTN THRU 1599-HD-XIT.                    ECS084
01224                                                                   ECS084
01225      IF HD-SW = +0                                                ECS084
01226          MOVE +1      TO HD-SW                                    ECS084
01227          MOVE HD-4M   TO P-LN                                     ECS084
01228          MOVE '-' TO P-CCSW                                       ECS084
01229          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
01230          MOVE HD-5M   TO P-LN                                     ECS084
01231          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT                   ECS084
01232          MOVE '0' TO P-CCSW.                                      ECS084
01233                                                                   ECS084
01234      IF TAB-SW = +0                                               ECS084
01235          MOVE +1                  TO TAB-SW                       ECS084
01236          MOVE '0'                 TO P-CCSW                       ECS084
01237          MOVE CLAS-MORT-CODE (X2) TO PM-TABLE                     ECS084
01238          MOVE CLAS-MORT-DESC (X2) TO PM-TDESC.                    ECS084
01239                                                                   ECS084
01240      MOVE X-BEN (X3)  TO PM-BEN.                                  ECS084
01241      MOVE X-DESC (X3) TO PM-BDESC.                                ECS084
01242      MOVE X-IUNDR     TO PM-IUNDR.                                ECS084
01243      MOVE X-IOVER     TO PM-IOVER.                                ECS084
01244      MOVE X-GUNDR     TO PM-GUNDR.                                ECS084
01245      MOVE X-GOVER     TO PM-GOVER.                                ECS084
01246      MOVE X-TOTAL     TO PM-TOTAL.                                ECS084
01247                                                                   ECS084
01248      PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT.                      ECS084
01249                                                                   ECS084
01250      ADD X-IUNDR TO S-IUNDR.                                      ECS084
01251      ADD X-IOVER TO S-IOVER.                                      ECS084
01252      ADD X-GUNDR TO S-GUNDR.                                      ECS084
01253      ADD X-GOVER TO S-GOVER.                                      ECS084
01254      ADD X-TOTAL TO S-TOTAL.                                      ECS084
01255                                                                   ECS084
01256      GO TO 0820-PRT-M3.                                           ECS084
01257                                                                   ECS084
01258  0830-PRT-MORT-SUB.                                               ECS084
01259      IF S-TOTAL = +0                                              ECS084
01260          GO TO 0810-PRT-M2.                                       ECS084
01261                                                                   ECS084
01262      MOVE 'TOTAL' TO PM-BDESC.                                    ECS084
01263      MOVE S-IUNDR TO PM-IUNDR.                                    ECS084
01264      MOVE S-IOVER TO PM-IOVER.                                    ECS084
01265      MOVE S-GUNDR TO PM-GUNDR.                                    ECS084
01266      MOVE S-GOVER TO PM-GOVER.                                    ECS084
01267      MOVE S-TOTAL TO PM-TOTAL.                                    ECS084
01268                                                                   ECS084
01269      PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT.                      ECS084
01270                                                                   ECS084
01271      ADD S-IUNDR TO T-IUNDR.                                      ECS084
01272      ADD S-IOVER TO T-IOVER.                                      ECS084
01273      ADD S-GUNDR TO T-GUNDR.                                      ECS084
01274      ADD S-GOVER TO T-GOVER.                                      ECS084
01275      ADD S-TOTAL TO T-TOTAL.                                      ECS084
01276                                                                   ECS084
01277      GO TO 0810-PRT-M2.                                           ECS084
01278                                                                   ECS084
01279  0840-PRT-MORT-TOTAL.                                             ECS084
01280      IF T-TOTAL = +0                                              ECS084
01281          GO TO 0899-XIT.                                          ECS084
01282                                                                   ECS084
01283      MOVE '0' TO P-CCSW.                                          ECS084
01284      MOVE 'TOTAL' TO PM-TDESC.                                    ECS084
01285      MOVE T-IUNDR TO PM-IUNDR.                                    ECS084
01286      MOVE T-IOVER TO PM-IOVER.                                    ECS084
01287      MOVE T-GUNDR TO PM-GUNDR.                                    ECS084
01288      MOVE T-GOVER TO PM-GOVER.                                    ECS084
01289      MOVE T-TOTAL TO PM-TOTAL.                                    ECS084
01290                                                                   ECS084
01291      PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT.                      ECS084
01292                                                                   ECS084
01293  0899-XIT.                                                        ECS084
01294      EXIT.                                                        ECS084
01295  EJECT                                                            ECS084
01296  0900-PRT-LINE.                                                   ECS084
01297                                                                   ECS084
01298      MOVE PX-BEN     TO P-BEN.                                    ECS084
01299      MOVE PX-DESC    TO P-DESC.                                   ECS084
01300      MOVE X-COUNT    TO P-COUNT.                                  ECS084
01301      MOVE X-WRITTEN  TO P-WRITTEN.                                ECS084
01302      MOVE X-P78      TO P-P78.                                    ECS084
01303      MOVE X-PRATA    TO P-PRATA.                                  ECS084
01304      MOVE X-DOMICILE TO P-DOMICILE.                               ECS084
01305      MOVE X-STATE    TO P-STATE.                                  ECS084
01306      MOVE X-RESERV   TO P-RESERV.                                 ECS084
01307      MOVE X-ALTRSV   TO P-ALTRSV.                                 ECS084
01308      MOVE X-REMAIN   TO P-REMAIN.                                 ECS084
01309                                                                   ECS084
01310      PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT.                      ECS084
01311                                                                   ECS084
01312  0999-PRT-LINE-XIT.                                               ECS084
01313      EXIT.                                                        ECS084
01314  EJECT                                                            ECS084
01315  1000-PRT-LINE.                                                   ECS084
01316                                                                   ECS084
01317      MOVE PX-BEN     TO P-BEN.                                    ECS084
01318      MOVE PX-DESC    TO P-DESC.                                   ECS084
01319                                                                   ECS084
01320      MOVE X-PAID     TO P-PAID.                                   ECS084
01321      MOVE X-C78      TO P-C78.                                    ECS084
01322      MOVE X-CRATA    TO P-CRATA.                                  ECS084
052303     IF DTE-CLIENT NOT = 'DCC'
040114         MOVE X-CDOMI    TO P-CDOMI
040114         MOVE X-TDOMI    TO P-TDOMI
01323          MOVE X-TAX      TO P-TAX
01324          MOVE X-T78      TO P-T78
01325          MOVE X-TRATA    TO P-TRATA. 
01326                                                                   ECS084
01327      PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT.                      ECS084
01328                                                                   ECS084
01329  1099-PRT-LINE-XIT.                                               ECS084
01330      EXIT.                                                        ECS084
01331  EJECT                                                            ECS084
01332  1100-ROLL-RTN.                                                   ECS084
01333      MOVE +0 TO X3.                                               ECS084
01334                                                                   ECS084
01335  1110-ROLL-X3.                                                    ECS084
01336      ADD +1 TO X3.                                                ECS084
01337                                                                   ECS084
01338      IF X3 GREATER THAN MAX-BEN                                   ECS084
01339          GO TO 1120-ROLL-MORT.                                    ECS084
01340                                                                   ECS084
01341      MOVE XT-AMTS (X3) TO X-DETL.                                 ECS084
01342                                                                   ECS084
01343      IF X-COUNT = +0                                              ECS084
01344          GO TO 1110-ROLL-X3.                                      ECS084
01345                                                                   ECS084
01346      MOVE PT-AMTS (X1 X3) TO SUB-TOTALS.                          ECS084
01347                                                                   ECS084
01348      ADD X-DUP      TO S-DUP.                                     ECS084
01349      ADD X-COUNT    TO S-COUNT.                                   ECS084
01350      ADD X-WRITTEN  TO S-WRITTEN.                                 ECS084
01351      ADD X-P78      TO S-P78.                                     ECS084
01352      ADD X-PRATA    TO S-PRATA.                                   ECS084
01353      ADD X-DOMICILE TO S-DOMICILE.                                ECS084
01354      ADD X-STATE    TO S-STATE.                                   ECS084
01355      ADD X-RESERV   TO S-RESERV.                                  ECS084
01356      ADD X-ALTRSV   TO S-ALTRSV.                                  ECS084
01357      ADD X-REMAIN   TO S-REMAIN.                                  ECS084
01358      ADD X-PAID     TO S-PAID.                                    ECS084
01359      ADD X-C78      TO S-C78.                                     ECS084
01360      ADD X-CRATA    TO S-CRATA.                                   ECS084
052303     IF DTE-CLIENT NOT = 'DCC'
040114         ADD X-CDOMI    TO S-CDOMI
040114         ADD X-TDOMI    TO S-TDOMI
01361          ADD X-TAX      TO S-TAX
01362          ADD X-T78      TO S-T78
01363          ADD X-TRATA    TO S-TRATA.  
01364                                                                   ECS084
01365      MOVE SUB-TOTALS TO PT-AMTS (X1 X3).                          ECS084
01366                                                                   ECS084
01367      GO TO 1110-ROLL-X3.                                          ECS084
01368                                                                   ECS084
01369  1120-ROLL-MORT.                                                  ECS084
01370      MOVE CLAS-STARTM TO X2.                                      ECS084
01371                                                                   ECS084
01372  1130-ROLL-MX2.                                                   ECS084
01373 *    IF X2 GREATER THAN CLAS-MAXM                                 ECS084
020113     IF X2 > 150
01374          GO TO 1199-ROLL-XIT.                                     ECS084
01375                                                                   ECS084
01376      MOVE +0 TO X3.                                               ECS084
01377  EJECT                                                            ECS084
01378  1140-ROLL-MX3.                                                   ECS084
01379      ADD +1 TO X3.                                                ECS084
01380                                                                   ECS084
01381      IF X3 GREATER THAN MAX-BEN                                   ECS084
01382          ADD +1 TO X2                                             ECS084
01383          GO TO 1130-ROLL-MX2.                                     ECS084
01384                                                                   ECS084
01385      MOVE XM-AMTS (X2 X3) TO X-M-DETL.                            ECS084
01386                                                                   ECS084
01387      COMPUTE X-TOTAL = X-IUNDR + X-IOVER + X-GUNDR + X-GOVER.     ECS084
01388                                                                   ECS084
01389      IF X-TOTAL = +0                                              ECS084
01390          GO TO 1140-ROLL-MX3.                                     ECS084
01391                                                                   ECS084
01392      IF X1 = +1                                                   ECS084
01393          MOVE PT-M-AMTS-G (X2 X3)  TO SUB-M-TOTALS                ECS084
01394      ELSE                                                         ECS084
01395          MOVE PT-M-AMTS-R (X2 X3)  TO SUB-M-TOTALS.               ECS084
01396                                                                   ECS084
01397      ADD X-IUNDR TO S-IUNDR.                                      ECS084
01398      ADD X-IOVER TO S-IOVER.                                      ECS084
01399      ADD X-GUNDR TO S-GUNDR.                                      ECS084
01400      ADD X-GOVER TO S-GOVER.                                      ECS084
01401                                                                   ECS084
01402      IF X1 = +1                                                   ECS084
01403          MOVE SUB-M-TOTALS         TO PT-M-AMTS-G (X2 X3)         ECS084
01404      ELSE                                                         ECS084
01405          MOVE SUB-M-TOTALS         TO PT-M-AMTS-R (X2 X3).        ECS084
01406                                                                   ECS084
01407      GO TO 1140-ROLL-MX3.                                         ECS084
01408                                                                   ECS084
01409  1199-ROLL-XIT.                                                   ECS084
01410      EXIT.                                                        ECS084
01411  EJECT                                                            ECS084
01412  1300-LOAD-TABLES.                                                ECS084
01413      MOVE +0          TO X2.                                      ECS084
01414      MOVE CLAS-STARTL TO CLAS-INDEXL.                             ECS084
01415      MOVE CLAS-STARTA TO CLAS-INDEXA.                             ECS084
01416      MOVE HIGH-VALUE  TO X-POINTERS.                              ECS084
01417                                                                   ECS084
01418      IF CLAS-MAXL = ZEROS                                         ECS084
01419          GO TO 1320-FORMAT-AH-RTN.                                ECS084
01420                                                                   ECS084
01421  1310-FORMAT-LIFE.                                                ECS084
01422      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS084
01423          GO TO 1320-FORMAT-AH-RTN.                                ECS084
01424                                                                   ECS084
01425      ADD +1 TO X2.                                                ECS084
01426                                                                   ECS084
01427      MOVE CLAS-I-BEN (CLAS-INDEXL)  TO X-BEN (X2).                ECS084
01428      MOVE 1                         TO X-TYP (X2).                ECS084
01429      MOVE CLAS-I-AB10 (CLAS-INDEXL) TO X-DESC (X2).               ECS084
01430                                                                   ECS084
01431      ADD +1 TO CLAS-INDEXL.                                       ECS084
01432                                                                   ECS084
01433      GO TO 1310-FORMAT-LIFE.                                      ECS084
01434                                                                   ECS084
01435  1320-FORMAT-AH-RTN.                                              ECS084
01436      IF CLAS-MAXA = ZEROS                                         ECS084
01437          GO TO 1340-FORMAT-SET.                                   ECS084
01438                                                                   ECS084
01439  1330-FORMAT-AH.                                                  ECS084
01440      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS084
01441          GO TO 1340-FORMAT-SET.                                   ECS084
01442                                                                   ECS084
01443      ADD +1 TO X2.                                                ECS084
01444                                                                   ECS084
01445      MOVE CLAS-I-BEN (CLAS-INDEXA)  TO X-BEN (X2).                ECS084
01446      MOVE 2                         TO X-TYP (X2).                ECS084
01447      MOVE CLAS-I-AB10 (CLAS-INDEXA) TO X-DESC (X2).               ECS084
01448                                                                   ECS084
01449      ADD +1 TO CLAS-INDEXA.                                       ECS084
01450                                                                   ECS084
01451      GO TO 1330-FORMAT-AH.                                        ECS084
01452                                                                   ECS084
01453  1340-FORMAT-SET.                                                 ECS084
01454      MOVE X2 TO MAX-BEN.                                          ECS084
01455                                                                   ECS084
01456  1399-LOAD-TAB-XIT.                                               ECS084
01457      EXIT.                                                        ECS084
01458  EJECT                                                            ECS084
01459  1400-LOAD-MORT.                                                  ECS084
01460      IF CLAS-MAXM = ZEROS                                         ECS084
01461          MOVE +1 TO CLAS-STARTM.                                  ECS084
01462                                                                   ECS084
01463      ADD +1 TO CLAS-MAXM.                                         ECS084
01464                                                                   ECS084
020113     IF CLAS-MAXM GREATER THAN +150
020113         MOVE +150  TO CLAS-MAXM.                                 ECS084
01467                                                                   ECS084
01468      MOVE CLAS-MAXM TO X1.                                        ECS084
01469      MOVE SPACES    TO CLAS-MORT-CODE (X1).                       ECS084
01470      MOVE 'OTHERS'  TO CLAS-MORT-DESC (X1).                       ECS084
01471                                                                   ECS084
01472  1499-LOAD-MORT-XIT.                                              ECS084
01473      EXIT.                                                        ECS084
01474  EJECT                                                            ECS084
01475  1500-HD-RTN.                                                     ECS084
01476      ADD +1       TO PGCTR.                                       ECS084
01477      MOVE PGCTR   TO HD-PAGE.                                     ECS084
01478      MOVE HD1     TO P-LN.                                        ECS084
01479      MOVE '1' TO P-CCSW.                                          ECS084
01480      PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT.                      ECS084
01481                                                                   ECS084
01482      MOVE HD2 TO P-LN.                                            ECS084
01483      PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT.                      ECS084
01484                                                                   ECS084
01485      MOVE HD3 TO P-LN.                                            ECS084
01486      PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT.                      ECS084
01487                                                                   ECS084
01488      IF HD-REIN NOT = LOW-VALUES                                  ECS084
01489          MOVE '0' TO P-CCSW                                       ECS084
01490          MOVE REIN-HD TO P-LN                                     ECS084
01491          PERFORM 1600-PRT-RTN THRU 1699-PRT-XIT.                  ECS084
01492                                                                   ECS084
01493  1599-HD-XIT.                                                     ECS084
01494      EXIT.                                                        ECS084
01495  EJECT                                                            ECS084
01496  1600-PRT-RTN.                                                    ECS084
01497      MOVE P-CCSW  TO X P-CTL.                                     ECS084
01498      MOVE P-LN    TO P-DATA.                                      ECS084
01499      MOVE ' '     TO P-REC.                                       ECS084
01500                                                                   ECS084
01501      IF X = ' '                                                   ECS084
01502          ADD +1 TO LNCTR                                          ECS084
CIDMOD         MOVE 1 TO CC                                             ECS084
01503      ELSE                                                         ECS084
01504          IF X = '0'                                               ECS084
01505              ADD +2 TO LNCTR                                      ECS084
CIDMOD             MOVE 2 TO CC                                         ECS084
01506          ELSE                                                     ECS084
01507              IF X = '-'                                           ECS084
01508                  ADD +3 TO LNCTR                                  ECS084
CIDMOD                 MOVE 3 TO CC                                     ECS084
01509              ELSE                                                 ECS084
CIDMOD                 MOVE  1 TO CC                                    ECS084
01510                  MOVE +1 TO LNCTR.                                ECS084
01511                                                                   ECS084
01512  1610-PRT-COPY-RTN.                                               ECS084
CIDMOD*                            COPY ELCPRT2.                        ECS084
CIDMOD                                                                  ECS084
CIDMOD******************************************************************04/15/98
CIDMOD*                                                                *ELCPRT2
CIDMOD*                                                                *ELCPRT2
CIDMOD*                            ELCPRT2                             *   LV005
CIDMOD*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE               CL**5
CIDMOD*                            VMOD=2.002                          *   CL**5
CIDMOD*                                                                *   CL**4
CIDMOD******************************************************************   CL**4
CIDMOD                                                                     CL**4
CIDMOD     IF DTE-FICH NOT = SPACE AND                                     CL**4
CIDMOD        FICH-OPEN    = SPACE                                         CL**4
CIDMOD         MOVE 'X'                TO  FICH-OPEN                       CL**4
CIDMOD         OPEN OUTPUT FICH.                                           CL**4
CIDMOD                                                                     CL**4
CIDMOD     IF DTE-FICH NOT = SPACE                                         CL**4
CIDMOD         MOVE X                  TO  P-CTL                           CL**4
CIDMOD         WRITE FICH-REC FROM PRT.                                    CL**4
CIDMOD                                                                     CL**4
CIDMOD     IF DTE-FICH = SPACE OR '2'                                      CL**4
CIDMOD       MOVE X                    TO  CC                              CL**4
CIDMOD       IF CC    = ' '                                                CL**4
CIDMOD         WRITE PRT AFTER ADVANCING 1 LINE                            CL**4
CIDMOD       ELSE                                                          CL**4
CIDMOD         IF CC    = '0'                                              CL**4
CIDMOD           WRITE PRT AFTER ADVANCING 2 LINES                         CL**4
CIDMOD         ELSE                                                        CL**4
CIDMOD           IF CC    = '-'                                            CL**4
CIDMOD             WRITE PRT AFTER ADVANCING 3 LINES                       CL**4
CIDMOD           ELSE                                                      CL**4
CIDMOD             WRITE PRT AFTER ADVANCING PAGE.                         CL**4
CIDMOD                                                                     CL**4
CIDMOD******************************************************************   CL**4
01514                                                                   ECS084
01515  1699-PRT-XIT.                                                    ECS084
01516      EXIT.                                                        ECS084
01517  EJECT                                                            ECS084
01518  1700-END-OUTPUT.                                                 ECS084
01519      MOVE HIGH-VALUE TO CUR-SEQ.                                  ECS084
01520                                                                   ECS084
01521      PERFORM 0400-ACCUM-RTN THRU 0499-ACCUM-XIT.                  ECS084
01522                                                                   ECS084
01523  1799-OUTPUT-XIT.                                                 ECS084
01524      EXIT.                                                        ECS084
01525                                                                   ECS084
01526  8000-INITIALIZE-TABLES-MAX SECTION.                              ECS084
01527                                                                   ECS084
01528      MOVE WS-ZERO-FIELDS       TO X-M-DETL.                       ECS084
01529      MOVE WS-ZERO-FIELDS-2     TO X-DETL.                         ECS084
01530                                                                   ECS084
111203     PERFORM 8010-TABLES THRU 8010-EXIT 
01532              VARYING                                              ECS084
01533          M1 FROM 1 BY 1                                           ECS084
01534              UNTIL                                                ECS084
092602         M1 GREATER THAN 900.                                     ECS084
01536                                                                   ECS084
111203     PERFORM 8020-TABLES THRU 8020-EXIT      
01538              VARYING                                              ECS084
01539          M1 FROM 1 BY 1                                           ECS084
01540              UNTIL                                                ECS084
020113         M1 GREATER THAN +150.                                    ECS084
01542                                                                   ECS084
01543      MOVE +0                       TO ZERO-SW.                    ECS084
01544      GO TO 8999-EXIT.                                             ECS084
01545                                                                   ECS084
111203 8010-TABLES. 
01547                                                                   ECS084
01548      MOVE WS-ZERO-FIELDS-2         TO XT-AMTS (M1).               ECS084
01549                                                                   ECS084
01550      IF  ZERO-SW EQUAL +1                                         ECS084
01551          MOVE WS-ZERO-FIELDS-2     TO PT-AMTS (1 M1)              ECS084
01552                                       PT-AMTS (2 M1).             ECS084
01553                                                                   ECS084
01554  8010-EXIT.                                                       ECS084
01555      EXIT.                                                        ECS084
01556                                                                   ECS084
111203 8020-TABLES.
01558                                                                   ECS084
111203     PERFORM 8030-TABLES THRU 8030-EXIT   
01560              VARYING                                              ECS084
01561          M2 FROM 1 BY 1                                           ECS084
01562              UNTIL                                                ECS084
092602         M2 GREATER THAN +900.                                    ECS084
01564                                                                   ECS084
01565  8020-EXIT.                                                       ECS084
01566      EXIT.                                                        ECS084
01567                                                                   ECS084
111203 8030-TABLES. 
01569                                                                   ECS084
01570      MOVE WS-ZERO-FIELDS           TO XM-AMTS (M1 M2).            ECS084
01571                                                                   ECS084
01572      IF  ZERO-SW EQUAL +1                                         ECS084
01573          MOVE WS-ZERO-FIELDS       TO PT-M-AMTS-G (M1 M2)         ECS084
01574                                       PT-M-AMTS-R (M1 M2).        ECS084
01575                                                                   ECS084
01576  8030-EXIT.                                                       ECS084
01577      EXIT.                                                        ECS084
01578                                                                   ECS084
01579  8999-EXIT.                                                       ECS084
01580      EXIT.                                                        ECS084
01581      EJECT                                                        ECS084
01582  9000-END-OF-JOB SECTION.                                         ECS084
01583                                                                   ECS084
01584  9010-EOJ-RTN.                                                    ECS084
01585                              COPY ELCPRTC.                        ECS084
01586                                                                   ECS084
01587      CLOSE PRNTR.                                                 ECS084
01588      GOBACK.                                                      ECS084
01589                                                                   ECS084
01590  COPY ELCDCS.                                                     ECS084
01591                                                                   ECS084
01592  ABEND-PGM.                                                       ECS084
01593                      COPY ELCABEND.                               ECS084
01594                                                                   ECS084
