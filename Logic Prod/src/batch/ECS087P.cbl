00001  IDENTIFICATION DIVISION.                                         09/17/97
00002                                                                   ECS087
00003  PROGRAM-ID.                 ECS087.                                 LV001
00004 *              PROGRAM CONVERTED BY                               ECS087
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS087
00006 *              CONVERSION DATE 02/09/96 07:53:38.                 ECS087
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS087
00008 *                            VMOD=2.007.                          ECS087
00009                                                                   ECS087
00010 *AUTHOR.     LOGIC, INC.                                          ECS087
00011 *            DALLAS, TEXAS.                                       ECS087
00012                                                                   ECS087
00013 *DATE-COMPILED.                                                   ECS087
00014                                                                   ECS087
00015 *SECURITY.   *****************************************************ECS087
00016 *            *                                                   *ECS087
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS087
00018 *            *                                                   *ECS087
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS087
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS087
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS087
00022 *            *                                                   *ECS087
00023 *            *****************************************************ECS087
00024                                                                   ECS087
00025 *REMARKS.                                                         ECS087
00026 *        PRINT UNEARNED PREMIUM AND COMMISSION ANALYSIS - DETAIL. ECS087
00027 *        REINSURANCE ONLY.                                        ECS087
00028 *        PROGRAM SWITCHES NOT APPLICABLE.                         ECS087
00029                                                                   ECS087
00030  ENVIRONMENT DIVISION.                                            ECS087
00031  INPUT-OUTPUT SECTION.                                            ECS087
00032  FILE-CONTROL.                                                    ECS087
00033                                                                   ECS087
00034      SELECT SORTFL    ASSIGN TO SYS001-UT-3380-S-SORTWK1.         ECS087
00035      SELECT PRNTR     ASSIGN TO SYS008-UR-1403-S-SYS008.          ECS087
00036      SELECT GAAP-EXTR ASSIGN TO SYS011-UT-2400-S-SYS011.          ECS087
00037      SELECT DISK-DATE ASSIGN TO SYS019-UT-3380-S-SYS019.          ECS087
00038      SELECT FICH      ASSIGN TO SYS020-UT-2400-S-SYS020.          ECS087
00039  EJECT                                                            ECS087
00040  DATA DIVISION.                                                   ECS087
00041  FILE SECTION.                                                    ECS087
00042                                                                   ECS087
00043  SD  SORTFL.                                                      ECS087
00044                                                                   ECS087
00045  01  SORT-RECORD.                                                 ECS087
00046      12  S-CONTROL           PIC X(40).                           ECS087
00047      12  S-REIN-CO           PIC X(6).                            ECS087
00048      12  FILLER              PIC X(319).                          ECS087
00049  EJECT                                                            ECS087
00050  FD  PRNTR                                                        ECS087
00051                              COPY ELCPRTFD.                       ECS087
00052  EJECT                                                            ECS087
00053  FD  GAAP-EXTR                                                    ECS087
00054                              COPY ECSGAPFD.                       ECS087
00055  EJECT                                                            ECS087
00056  FD  DISK-DATE                                                    ECS087
00057                              COPY ELCDTEFD.                       ECS087
00058  EJECT                                                            ECS087
00059  FD  FICH                                                         ECS087
00060                              COPY ECSFICH.                        ECS087
00061  EJECT                                                            ECS087
00062  WORKING-STORAGE SECTION.                                         ECS087
00063  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS087
00064  77  FILLER  PIC X(32) VALUE '********************************'.  ECS087
00065  77  FILLER  PIC X(32) VALUE '     ECS087 WORKING STORAGE     '.  ECS087
00066  77  FILLER  PIC X(32) VALUE '*****VMOD=2.007*****************'.  ECS087
00067                                                                   ECS087
00068  77  PGM-SUB                 PIC S999    COMP    VALUE +087.      ECS087
00069  77  AH-SW                   PIC S9      COMP-3.                  ECS087
00070  77  LIFE-SW                 PIC S9      COMP-3.                  ECS087
00071  77  FST-SW                  PIC S9      COMP-3.                  ECS087
00072  77  HD-SW                   PIC S9      COMP-3  VALUE +0.        ECS087
00073  77  ZERO-SW                 PIC S9      COMP-3  VALUE +1.        ECS087
00074  77  X1                      PIC S999    COMP-3.                  ECS087
00075  77  X2                      PIC S999    COMP-3.                  ECS087
00076  77  LNCTR                   PIC S999    COMP-3.                  ECS087
00077  77  MAX-BEN                 PIC S999    COMP-3  VALUE +300.      ECS087
00078  77  PGCTR                   PIC S9(5)   COMP-3.                  ECS087
00079  77  RELEASED-RECS           PIC S9(5)   COMP-3  VALUE ZERO.      ECS087
00080  77  X                       PIC X               VALUE SPACE.     ECS087
00081  EJECT                                                            ECS087
00082      COPY ECSGAP01.                                               ECS087
00083      COPY ELCGAPVR.                                               ECS087
00084  EJECT                                                            ECS087
00085  01  HD1.                                                         ECS087
00086      12  FILLER              PIC X(43)           VALUE SPACES.    ECS087
00087      12  FILLER              PIC X(38)           VALUE            ECS087
00088              'UNEARNED PREMIUM AND COMMISSION DETAIL'.            ECS087
00089      12  FILLER              PIC X(38)           VALUE SPACES.    ECS087
00090      12  FILLER              PIC X(8)            VALUE 'ECS087'.  ECS087
00091                                                                   ECS087
00092  01  HD2.                                                         ECS087
00093      12  SUB-HD2             PIC X(47)           VALUE SPACES.    ECS087
00094      12  HD-CO               PIC X(30).                           ECS087
00095      12  FILLER              PIC X(42)           VALUE SPACES.    ECS087
00096      12  HD-RD               PIC X(8).                            ECS087
00097                                                                   ECS087
00098  01  HD3.                                                         ECS087
00099      12  SUB-HD3             PIC X(53)           VALUE SPACES.    ECS087
00100      12  HD-DT               PIC X(18).                           ECS087
00101      12  FILLER              PIC X(48)           VALUE SPACES.    ECS087
00102      12  FILLER              PIC X(5)            VALUE 'PAGE '.   ECS087
00103      12  HD-PAGE             PIC ZZ,ZZ9.                          ECS087
00104                                                                   ECS087
00105  01  HD4.                                                         ECS087
00106      12 FILLER               PIC X(44)           VALUE            ECS087
00107             '                             * * * * * * * *'.       ECS087
00108      12 FILLER               PIC X(44)           VALUE            ECS087
00109             ' * *    P R E M I U M    * * * * * * * * * *'.       ECS087
00110      12 FILLER               PIC X(44)           VALUE            ECS087
00111             '                                            '.       ECS087
00112                                                                   ECS087
00113  01  HD5.                                                         ECS087
00114      12 HD-DESC              PIC X(15).                           ECS087
00115      12 FILLER               PIC X(29)           VALUE            ECS087
00116                            'IN FORCE                  UNE'.       ECS087
00117      12 FILLER               PIC X(44)           VALUE            ECS087
00118             'ARNED     UNEARNED     DOMICILE      STATE  '.       ECS087
00119      12 FILLER               PIC X(44)           VALUE            ECS087
00120             '    MORTALITY    ALTERNATE        REMAINING '.       ECS087
00121                                                                   ECS087
00122  01  HD6.                                                         ECS087
00123      12 FILLER               PIC X(44)           VALUE            ECS087
00124             '                COUNT        WRITTEN      RU'.       ECS087
00125      12 FILLER               PIC X(44)           VALUE            ECS087
00126             'LE 78     PRO-RATA    STATUTORY    STATUTORY'.       ECS087
00127      12 FILLER               PIC X(44)           VALUE            ECS087
00128             '     RESERVE      RESERVE           AMOUNT  '.       ECS087
00129                                                                   ECS087
00130  01  HD7.                                                         ECS087
00131      12 FILLER               PIC X(44)           VALUE            ECS087
00132             '                                * *   C O M '.       ECS087
00133      12 FILLER               PIC X(44)           VALUE            ECS087
00134             'M I S S I O N   * *        * * * *    T A X '.       ECS087
00135      12 FILLER               PIC X(44)           VALUE            ECS087
00136             'E S    * * * *                              '.       ECS087
00137                                                                   ECS087
00138  01  HD8.                                                         ECS087
00139      12 FILLER               PIC X(44)           VALUE            ECS087
00140             '                                PAID      RU'.       ECS087
00141      12 FILLER               PIC X(44)           VALUE            ECS087
00142             'LE 78      PRO-RATA        PAID      RULE 78'.       ECS087
00143      12 FILLER               PIC X(44)           VALUE            ECS087
00144             '      PRO-RATA                              '.       ECS087
00145  EJECT                                                            ECS087
00146  01  SUB-HEADINGS.                                                ECS087
00147      12  HEAD2.                                                   ECS087
00148          16  FILLER          PIC X(38)           VALUE            ECS087
00149                  'CARR   COMP   STATE               ACCT'.        ECS087
00150      12  ACCT-HDA REDEFINES HEAD2.                                ECS087
00151          16  ACCT-HD2        PIC X(38).                           ECS087
00152      12  ST-HDA REDEFINES HEAD2.                                  ECS087
00153          16  ST-HD2          PIC X(30).                           ECS087
00154          16  FILLER          PIC X(8).                            ECS087
00155      12  CO-HDA REDEFINES HEAD2.                                  ECS087
00156          16  CO-HD2          PIC X(12).                           ECS087
00157          16  FILLER          PIC X(26).                           ECS087
00158      12  CARR-HDA REDEFINES HEAD2.                                ECS087
00159          16  CARR-HD2        PIC X(4).                            ECS087
00160          16  FILLER          PIC X(34).                           ECS087
00161      12  HEAD3.                                                   ECS087
00162          16  FILLER          PIC XX              VALUE SPACES.    ECS087
00163          16  HD-CARR         PIC X.                               ECS087
00164          16  FILLER          PIC XXX             VALUE SPACES.    ECS087
00165          16  HD-COMP         PIC X(6).                            ECS087
00166          16  FILLER          PIC X               VALUE SPACES.    ECS087
00167          16  HD-ST           PIC XX.                              ECS087
00168          16  FILLER          PIC X               VALUE SPACES.    ECS087
00169          16  HD-ST-NM        PIC X(15).                           ECS087
00170          16  FILLER          PIC XX              VALUE SPACES.    ECS087
00171          16  HD-ACCT         PIC X(10).                           ECS087
00172      12  ACCT-HDB REDEFINES HEAD3.                                ECS087
00173          16  ACCT-HD3        PIC X(43).                           ECS087
00174      12  ST-HDB REDEFINES HEAD3.                                  ECS087
00175          16  ST-HD3          PIC X(31).                           ECS087
00176          16  FILLER          PIC X(12).                           ECS087
00177      12  CO-HDB REDEFINES HEAD3.                                  ECS087
00178          16  CO-HD3          PIC X(12).                           ECS087
00179          16  FILLER          PIC X(31).                           ECS087
00180      12  CARR-HDB REDEFINES HEAD3.                                ECS087
00181          16  CARR-HD3        PIC X(4).                            ECS087
00182          16  FILLER          PIC X(39).                           ECS087
00183      12  HD3A.                                                    ECS087
00184          16  REIN-HEADING.                                        ECS087
00185              20  FILLER      PIC X(20)           VALUE            ECS087
00186                      'REINSURANCE COMPANY '.                      ECS087
00187              20  REIN-COMP   PIC X(6).                            ECS087
00188  EJECT                                                            ECS087
00189  01  DETAIL-HEADINGS.                                             ECS087
00190      12  DTL-4.                                                   ECS087
00191          16  FILLER          PIC X(44)           VALUE            ECS087
00192                  '    CERT.      EFF.        BENEFIT   * TRM *'.  ECS087
00193          16  FILLER          PIC X(44)           VALUE            ECS087
00194                  '       ORIG           REM                   '.  ECS087
00195      12  DTL-5.                                                   ECS087
00196          16  FILLER          PIC X(44)           VALUE            ECS087
00197                  '   NUMBER      DATE         TYPE     ORG REM'.  ECS087
00198          16  FILLER          PIC X(44)           VALUE            ECS087
00199                  '       AMT            AMT        PREMIUM    '.  ECS087
00200          16  FILLER          PIC X(44)           VALUE            ECS087
00201                  'PRO-RATA     RULE-78       STATE            '.  ECS087
00202  EJECT                                                            ECS087
00203  01  MISC-WS.                                                     ECS087
00204      12  WS-RETURN-CODE      PIC S9(4)   COMP.                    ECS087
00205      12  WS-ABEND-FILE-STATUS PIC XX     VALUE '00'.              ECS087
00206      12  WS-ZERO             PIC S9 VALUE +0      COMP-3.         ECS087
00207      12  WS-ABEND-MESSAGE    PIC X(80).                           ECS087
00208      12  WS-ABEND-MESSAGE-R REDEFINES WS-ABEND-MESSAGE.           ECS087
00209          16  FILLER          PIC X(9).                            ECS087
00210          16  WS-ABEND-BEN-TYPE PIC X(6).                          ECS087
00211          16  FILLER          PIC X(65).                           ECS087
00212      12  SAVE-LPOINTERS.                                          ECS087
00213          16  SAVE-LPOINTER   PIC XXX             VALUE LOW-VALUE. ECS087
00214          16  SAVE-LX2        PIC S999 COMP-3.                     ECS087
00215          16  SAVE-LDESC      PIC X(10)           VALUE SPACES.    ECS087
00216      12  SAVE-APOINTERS.                                          ECS087
00217          16  SAVE-APOINTER   PIC XXX             VALUE LOW-VALUE. ECS087
00218          16  SAVE-AX2        PIC S999.                            ECS087
00219          16  SAVE-ADESC      PIC X(10)           VALUE SPACES.    ECS087
00220      12  CUR-SEQ.                                                 ECS087
00221          16  CUR-SEQ1.                                            ECS087
00222              20  CUR-REIN    PIC X(6).                            ECS087
00223          16  CUR-SEQ2.                                            ECS087
00224              20  CUR-CARR    PIC X.                               ECS087
00225              20  CUR-CO      PIC X(6).                            ECS087
00226              20  CUR-ST      PIC XX.                              ECS087
00227              20  CUR-ACCT    PIC X(10).                           ECS087
00228      12  WX-SEQ.                                                  ECS087
00229          16  WX-SEQ1.                                             ECS087
00230              20  WX-REIN     PIC X(6).                            ECS087
00231          16  WX-SEQ2.                                             ECS087
00232              20  WX-CARR     PIC X.                               ECS087
00233              20  WX-CO       PIC X(6).                            ECS087
00234              20  WX-ST       PIC XX.                              ECS087
00235              20  WX-ACCT     PIC X(10).                           ECS087
00236      12  X-POINTERS.                                              ECS087
00237          16  X-MATCH     OCCURS 300 TIMES.                        ECS087
00238              20  X-POINTER.                                       ECS087
00239                  24  X-BEN   PIC XX.                              ECS087
00240                  24  X-TYP   PIC 9.                               ECS087
00241              20  X-DESC      PIC X(10).                           ECS087
00242      12  WX-POINTERS.                                             ECS087
00243          16  WX-POINTER.                                          ECS087
00244              20  WX-BEN      PIC XX.                              ECS087
00245              20  WX-TYP      PIC 9.                               ECS087
00246      12  PX-POINTER.                                              ECS087
00247          16  PX-BEN          PIC XXX.                             ECS087
00248          16  PX-DESC.                                             ECS087
00249              20  FILLER      PIC X(6).                            ECS087
00250              20  PX-BEN-DESC PIC X(4).                            ECS087
00251                                                                   ECS087
00252  EJECT                                                            ECS087
00253  01  TOTALS-AREA.                                                 ECS087
00254      12  X-TOTALS.                                                ECS087
00255          16  X-TOTS.                                              ECS087
00256              20  X-LEVEL OCCURS 300 TIMES.                        ECS087
00257                  24  X-AMTS  PIC X(90).                           ECS087
00258      12  X-DETL.                                                  ECS087
00259          16  X-COUNT         PIC S9(7)     COMP-3.                ECS087
00260          16  X-WRITTEN       PIC S9(9)V99  COMP-3.                ECS087
00261          16  X-P78           PIC S9(9)V99  COMP-3.                ECS087
00262          16  X-PRATA         PIC S9(9)V99  COMP-3.                ECS087
00263          16  X-DOMICILE      PIC S9(9)V99  COMP-3.                ECS087
00264          16  X-STATE         PIC S9(9)V99  COMP-3.                ECS087
00265          16  X-RESERV        PIC S9(9)V99  COMP-3.                ECS087
00266          16  X-ALTRSV        PIC S9(9)V99  COMP-3.                ECS087
00267          16  X-REMAIN        PIC S9(13)V99 COMP-3.                ECS087
00268          16  X-PAID          PIC S9(9)V99  COMP-3.                ECS087
00269          16  X-C78           PIC S9(9)V99  COMP-3.                ECS087
00270          16  X-CRATA         PIC S9(9)V99  COMP-3.                ECS087
00271          16  X-TAX           PIC S9(9)V99  COMP-3.                ECS087
00272          16  X-T78           PIC S9(9)V99  COMP-3.                ECS087
00273          16  X-TRATA         PIC S9(9)V99  COMP-3.                ECS087
00274      12  R-DETL.                                                  ECS087
00275          16  R-COUNT         PIC S9(7)     COMP-3.                ECS087
00276          16  R-WRITTEN       PIC S9(9)V99  COMP-3.                ECS087
00277          16  R-P78           PIC S9(9)V99  COMP-3.                ECS087
00278          16  R-PRATA         PIC S9(9)V99  COMP-3.                ECS087
00279          16  R-DOMICILE      PIC S9(9)V99  COMP-3.                ECS087
00280          16  R-STATE         PIC S9(9)V99  COMP-3.                ECS087
00281          16  R-RESERV        PIC S9(9)V99  COMP-3.                ECS087
00282          16  R-ALTRSV        PIC S9(9)V99  COMP-3.                ECS087
00283          16  R-REMAIN        PIC S9(13)V99 COMP-3.                ECS087
00284          16  R-PAID          PIC S9(9)V99  COMP-3.                ECS087
00285          16  R-C78           PIC S9(9)V99  COMP-3.                ECS087
00286          16  R-CRATA         PIC S9(9)V99  COMP-3.                ECS087
00287          16  R-TAX           PIC S9(9)V99  COMP-3.                ECS087
00288          16  R-T78           PIC S9(9)V99  COMP-3.                ECS087
00289          16  R-TRATA         PIC S9(9)V99  COMP-3.                ECS087
00290                                                                   ECS087
00291  01  ST-TOTALS.                                                   ECS087
00292      12  ST-TOTS.                                                 ECS087
00293          16  ST-LEVEL      OCCURS 300 TIMES.                      ECS087
00294              20  ST-AMTS     PIC X(90).                           ECS087
00295  01  CO-TOTALS.                                                   ECS087
00296      12  FILLER              PIC X(27000).                        ECS087
00297  01  CARR-TOTALS.                                                 ECS087
00298      12  FILLER              PIC X(27000).                        ECS087
00299  01  FINAL-TOTALS.                                                ECS087
00300      12  FILLER              PIC X(27000).                        ECS087
00301  01  OVERALL-TOTALS.                                              ECS087
00302      12  FILLER              PIC X(27000).                        ECS087
00303  EJECT                                                            ECS087
00304  01  CALC-CTRS.                                                   ECS087
00305      12  SUB-TOTALS.                                              ECS087
00306          16  S-COUNT         PIC S9(7)     COMP-3.                ECS087
00307          16  S-WRITTEN       PIC S9(9)V99  COMP-3.                ECS087
00308          16  S-P78           PIC S9(9)V99  COMP-3.                ECS087
00309          16  S-PRATA         PIC S9(9)V99  COMP-3.                ECS087
00310          16  S-DOMICILE      PIC S9(9)V99  COMP-3.                ECS087
00311          16  S-STATE         PIC S9(9)V99  COMP-3.                ECS087
00312          16  S-RESERV        PIC S9(9)V99  COMP-3.                ECS087
00313          16  S-ALTRSV        PIC S9(9)V99  COMP-3.                ECS087
00314          16  S-REMAIN        PIC S9(13)V99 COMP-3.                ECS087
00315          16  S-PAID          PIC S9(9)V99  COMP-3.                ECS087
00316          16  S-C78           PIC S9(9)V99  COMP-3.                ECS087
00317          16  S-CRATA         PIC S9(9)V99  COMP-3.                ECS087
00318          16  S-TAX           PIC S9(9)V99  COMP-3.                ECS087
00319          16  S-T78           PIC S9(9)V99  COMP-3.                ECS087
00320          16  S-TRATA         PIC S9(9)V99  COMP-3.                ECS087
00321      12  TOTALS.                                                  ECS087
00322          16  T-COUNT         PIC S9(7)     COMP-3.                ECS087
00323          16  T-WRITTEN       PIC S9(9)V99  COMP-3.                ECS087
00324          16  T-P78           PIC S9(9)V99  COMP-3.                ECS087
00325          16  T-PRATA         PIC S9(9)V99  COMP-3.                ECS087
00326          16  T-DOMICILE      PIC S9(9)V99  COMP-3.                ECS087
00327          16  T-STATE         PIC S9(9)V99  COMP-3.                ECS087
00328          16  T-RESERV        PIC S9(9)V99  COMP-3.                ECS087
00329          16  T-ALTRSV        PIC S9(9)V99  COMP-3.                ECS087
00330          16  T-REMAIN        PIC S9(13)V99 COMP-3.                ECS087
00331          16  T-PAID          PIC S9(9)V99  COMP-3.                ECS087
00332          16  T-C78           PIC S9(9)V99  COMP-3.                ECS087
00333          16  T-CRATA         PIC S9(9)V99  COMP-3.                ECS087
00334          16  T-TAX           PIC S9(9)V99  COMP-3.                ECS087
00335          16  T-T78           PIC S9(9)V99  COMP-3.                ECS087
00336          16  T-TRATA         PIC S9(9)V99  COMP-3.                ECS087
00337      12  X-TOTAL             PIC S9(9)V99  COMP-3.                ECS087
00338                                                                   ECS087
00339  01  SPECIAL-TOTALS.                                              ECS087
00340      12  CERT-WORK.                                               ECS087
00341          16  X-CALC-REMAIN       PIC S9(15)V99   COMP-3.          ECS087
00342          16  X-AH-EXP            PIC S9(9)V99    COMP-3.          ECS087
00343      12  ACCT-WORK.                                               ECS087
00344          16  ACCT-AGE-REMAIN     PIC S9(15)V99   COMP-3.          ECS087
00345          16  ACCT-AH-EXP         PIC S9(9)V99    COMP-3.          ECS087
00346      12  STATE-WORK.                                              ECS087
00347          16  ST-AGE-REMAIN       PIC S9(15)V99   COMP-3.          ECS087
00348          16  ST-AH-EXP           PIC S9(9)V99    COMP-3.          ECS087
00349      12  COMPANY-WORK.                                            ECS087
00350          16  CO-AGE-REMAIN       PIC S9(15)V99   COMP-3.          ECS087
00351          16  CO-AH-EXP           PIC S9(9)V99    COMP-3.          ECS087
00352      12  CARRIER-WORK.                                            ECS087
00353          16  CARR-AGE-REMAIN     PIC S9(15)V99   COMP-3.          ECS087
00354          16  CARR-AH-EXP         PIC S9(9)V99    COMP-3.          ECS087
00355      12  FINAL-WORK.                                              ECS087
00356          16  FINAL-AGE-REMAIN    PIC S9(15)V99   COMP-3.          ECS087
00357          16  FINAL-AH-EXP        PIC S9(9)V99    COMP-3.          ECS087
00358      12  OVERALL-WORK.                                            ECS087
00359          16  OVER-AGE-REMAIN     PIC S9(15)V99   COMP-3.          ECS087
00360          16  OVER-AH-EXP         PIC S9(9)V99    COMP-3.          ECS087
00361      12  OTHR-WORK.                                               ECS087
00362          16  X-WT-REMAIN         PIC S9(5)V99    COMP-3.          ECS087
00363      12  X-DUPS.                                                  ECS087
00364          16  X-DUP               PIC S9(7)       COMP-3.          ECS087
00365      12  ST-DUPS.                                                 ECS087
00366          16  ST-DUP              PIC S9(7)       COMP-3.          ECS087
00367      12  CO-DUPS.                                                 ECS087
00368          16  CO-DUP              PIC S9(7)       COMP-3.          ECS087
00369      12  CARR-DUPS.                                               ECS087
00370          16  CARR-DUP            PIC S9(7)       COMP-3.          ECS087
00371      12  FINL-DUPS.                                               ECS087
00372          16  FINL-DUP            PIC S9(7)       COMP-3.          ECS087
00373      12  OVER-DUPS.                                               ECS087
00374          16  OVER-DUP            PIC S9(7)       COMP-3.          ECS087
00375      12  PRT-SPECIAL.                                             ECS087
00376          16  FILLER              PIC X(6)        VALUE            ECS087
00377                  'TOTAL '.                                        ECS087
00378          16  PRT-SPC-BEN-TYPE    PIC XX          VALUE SPACES.    ECS087
00379          16  FILLER              PIC X(19)       VALUE            ECS087
00380                  '  MONTHLY EXPOSURE '.                           ECS087
00381          16  P-AH-EXP            PIC Z,ZZZ,ZZZ,ZZZ.ZZ-.           ECS087
00382          16  FILLER              PIC XXX         VALUE SPACES.    ECS087
00383          16  FILLER              PIC X(35)       VALUE            ECS087
00384                  '  AGE WEIGHTED BY REMAINING BENEFIT'.           ECS087
00385          16  P-WT-REMAIN         PIC ZZZZZ.ZZ-.                   ECS087
00386  EJECT                                                            ECS087
00387  01  P-REC.                                                       ECS087
00388      12  P-CCSW                  PIC X.                           ECS087
00389      12  P-LN.                                                    ECS087
00390          16  P-BEN               PIC XXX.                         ECS087
00391          16  P-DESC              PIC X(10).                       ECS087
00392          16  P-COUNT             PIC ZZ,ZZZ,ZZZ-.                 ECS087
00393          16  P-DETAIL-1.                                          ECS087
00394              20  P-WRITTEN       PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00395              20  P-P78           PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00396              20  P-PRATA         PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00397              20  P-PDOMICILE     PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00398              20  P-PSTATE        PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00399              20  P-RESERV        PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00400              20  P-ALTRSV        PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00401              20  P-REMAIN        PIC ZZZZ,ZZZ,ZZZ,ZZZ-.           ECS087
00402          16  P-DETAIL-2 REDEFINES P-DETAIL-1.                     ECS087
00403              20  P-PAID          PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00404              20  P-C78           PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00405              20  P-CRATA         PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00406              20  P-TAX           PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00407              20  P-T78           PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00408              20  P-TRATA         PIC ZZZZ,ZZZ,ZZZ-.               ECS087
00409              20  FILLER          PIC X(30).                       ECS087
00410 *        16  FILLER              PIC X(3).                        ECS087
00411      12  P-DETAIL REDEFINES P-LN.                                 ECS087
00412          16  PD-CERT         PIC X(11).                           ECS087
00413          16  FILLER          PIC XX.                              ECS087
00414          16  PD-MO           PIC XX.                              ECS087
00415          16  PD-DASH-1       PIC X.                               ECS087
00416          16  PD-DA           PIC XX.                              ECS087
00417          16  PD-DASH-2       PIC X.                               ECS087
00418          16  PD-YR           PIC XX.                              ECS087
00419          16  FILLER          PIC X(3).                            ECS087
00420          16  PD-BEN-DESC     PIC X(10).                           ECS087
00421          16  FILLER          PIC X(3).                            ECS087
00422          16  PD-ORIG         PIC 999.                             ECS087
00423          16  FILLER          PIC X.                               ECS087
00424          16  PD-REM          PIC 999.                             ECS087
00425          16  FILLER          PIC X.                               ECS087
00426          16  PD-BEN          PIC ZZZ,ZZZ,ZZZ.99.                  ECS087
00427          16  PD-REMAMT       PIC ZZZ,ZZZ,ZZZ.99.                  ECS087
00428          16  PD-PREM         PIC Z,ZZZ,ZZZ.99.                    ECS087
00429          16  PD-RATA         PIC Z,ZZZ,ZZZ.99.                    ECS087
00430          16  PD-78           PIC Z,ZZZ,ZZZ.99.                    ECS087
00431          16  PD-STATE        PIC Z,ZZZ,ZZZ.99.                    ECS087
00432          16  FILLER          PIC X(11).                           ECS087
00433                                                                   ECS087
00434      COPY ELCDTECX.                                               ECS087
00435                                                                   ECS087
00436      COPY ELCDTEVR.                                               ECS087
00437  EJECT                                                            ECS087
00438  PROCEDURE DIVISION.                                              ECS087
00439                                                                   ECS087
00440  0000-COPY-STANDARD.                                              ECS087
00441                              COPY ELCDTERX.                       ECS087
00442                                                                   ECS087
00443  0100-SORT-ROUTINE SECTION.                                       ECS087
00444                                                                   ECS087
00445      MOVE AH-OVERRIDE-L2 TO PRT-SPC-BEN-TYPE.                     ECS087
00446                                                                   ECS087
00447  0110-SORT-RTN.                                                   ECS087
00448      SORT SORTFL ON ASCENDING KEY S-REIN-CO S-CONTROL             ECS087
00449          INPUT PROCEDURE 0200-INPUT-RTN   THRU 0299-INPUT-XIT     ECS087
00450          OUTPUT PROCEDURE 0300-OUTPUT-RTN THRU 1699-OUTPUT-XIT.   ECS087
00451                                                                   ECS087
00452      IF  RELEASED-RECS = ZERO                                     ECS087
00453          GO TO 1710-END-JOB.                                      ECS087
00454                                                                   ECS087
00455      IF SORT-RETURN NOT = ZEROS                                   ECS087
00456          MOVE 0101 TO WS-RETURN-CODE                              ECS087
00457          MOVE ' SORT ROUTINE ABENDED ' TO WS-ABEND-MESSAGE        ECS087
00458          GO TO ABEND-PGM.                                         ECS087
00459                                                                   ECS087
00460      GO TO 1710-END-JOB.                                          ECS087
00461  EJECT                                                            ECS087
00462  0200-INPUT-RTN SECTION.                                          ECS087
00463                                                                   ECS087
00464  0210-INTL-INPUT.                                                 ECS087
00465      OPEN INPUT GAAP-EXTR.                                        ECS087
00466                                                                   ECS087
00467  0220-READ-INPUT.                                                 ECS087
00468      READ GAAP-EXTR INTO GAAP-RECORD AT END                       ECS087
00469          GO TO 0230-END-INPUT.                                    ECS087
00470                                                                   ECS087
00471      IF GR-REIN NOT = 'R'                                         ECS087
00472          GO TO 0220-READ-INPUT.                                   ECS087
00473                                                                   ECS087
PEMTST     IF (GR-ACCOUNT = '0000629200')
               AND (GR-CERT-NO = '4500180058 ' OR '4500180074 ')
              CONTINUE
           ELSE
              GO TO 0220-READ-INPUT
           END-IF


00474      MOVE GAAP-RECORD  TO SORT-RECORD.                            ECS087
00475                                                                   ECS087
00476      RELEASE SORT-RECORD.                                         ECS087
00477                                                                   ECS087
00478      ADD 1 TO RELEASED-RECS.                                      ECS087
00479                                                                   ECS087
00480      GO TO 0220-READ-INPUT.                                       ECS087
00481                                                                   ECS087
00482  0230-END-INPUT.                                                  ECS087
00483                                                                   ECS087
00484      CLOSE GAAP-EXTR.                                             ECS087
00485                                                                   ECS087
00486  0299-INPUT-XIT.                                                  ECS087
00487      EXIT.                                                        ECS087
00488  EJECT                                                            ECS087
00489  0300-OUTPUT-RTN SECTION.                                         ECS087
00490                                                                   ECS087
00491  0310-INTL-RTN.                                                   ECS087
00492      MOVE WS-CURRENT-DATE TO HD-RD.                               ECS087
00493      MOVE COMPANY-NAME    TO HD-CO.                               ECS087
00494      MOVE ALPH-DATE       TO HD-DT.                               ECS087
00495                                                                   ECS087
00496      OPEN OUTPUT PRNTR.                                           ECS087
00497                                                                   ECS087
00498      MOVE SPACES    TO P-REC.                                     ECS087
00499      MOVE LOW-VALUE TO CUR-SEQ  WX-SEQ.                           ECS087
00500      MOVE +0        TO FST-SW   PGCTR.                            ECS087
00501      MOVE +76       TO LNCTR.                                     ECS087
00502                                                                   ECS087
00503      IF  RELEASED-RECS = ZERO                                     ECS087
00504          PERFORM 1400-HD-RTN THRU 1499-HD-XIT                     ECS087
00505          MOVE 'ECS087 PROCESSES REINSURANCE GAAP RECORDS INTO THISECS087
00506 -             'REPORT.  HOWEVER,'                                 ECS087
00507                                  TO P-LN                          ECS087
00508          MOVE '-'                TO P-CCSW                        ECS087
00509          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00510          MOVE 'NO REINSURANCE GAAP RECORDS WERE FOUND IN THE CURREECS087
00511 -            'NT GAAP FILE.'     TO P-LN                          ECS087
00512          MOVE ' '                TO P-CCSW                        ECS087
00513          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00514          MOVE 'IF YOUR COMPANY DOES NO REINSURANCE BUSINESS OR DOEECS087
00515 -            'S NOT'             TO P-LN                          ECS087
00516          MOVE ' '                TO P-CCSW                        ECS087
00517          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00518          MOVE 'WANT THIS REPORT, PLEASE SET PROCESSING OPTIONS FORECS087
00519 -            'ECS087 TO X.'     TO P-LN                           ECS087
00520          MOVE ' '                TO P-CCSW                        ECS087
00521          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00522          MOVE 'THIS WILL SAVE SIGNIFICANT COMPUTER COST AND WILL SECS087
00523 -            'TOP THIS REPORT.'  TO P-LN                          ECS087
00524          MOVE ' '                TO P-CCSW                        ECS087
00525          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00526          MOVE 'IF REINSURANCE DATA WAS EXPECTED CHECK ECS050 AND EECS087
00527 -        'CS080.'            TO P-LN                              ECS087
00528          MOVE '0'                TO P-CCSW                        ECS087
00529          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00530          GO TO 1699-OUTPUT-XIT.                                   ECS087
00531                                                                   ECS087
00532      PERFORM 1300-LOAD-TABLES THRU 1399-LOAD-TAB-XIT.             ECS087
00533                                                                   ECS087
00534  0320-READ-RTN.                                                   ECS087
00535      RETURN SORTFL AT END                                         ECS087
00536          GO TO 1600-END-OUTPUT.                                   ECS087
00537                                                                   ECS087
00538      MOVE SORT-RECORD  TO GAAP-RECORD.                            ECS087
00539      COPY ELCGAPM1.                                               ECS087
00540      MOVE GR-REIN-COMP TO WX-SEQ1.                                ECS087
00541      MOVE GR-CONTROL   TO WX-SEQ2.                                ECS087
00542                                                                   ECS087
00543      IF WX-SEQ NOT = CUR-SEQ                                      ECS087
00544          PERFORM 0500-BREAK-RTN THRU 0599-BREAK-XIT.              ECS087
00545                                                                   ECS087
00546      MOVE +1 TO X2.                                               ECS087
00547                                                                   ECS087
00548  0330-FIND-LIFE.                                                  ECS087
00549      IF GR-LFTYP = ZEROS                                          ECS087
00550          GO TO 0360-FIND-AH.                                      ECS087
00551                                                                   ECS087
00552      MOVE +1          TO X-COUNT.                                 ECS087
00553      MOVE GR-LFPRM    TO X-WRITTEN.                               ECS087
00554      MOVE GRR-LFPRM   TO X-P78.                                   ECS087
00555      MOVE GRP-LFPRM   TO X-PRATA.                                 ECS087
00556      MOVE GRD-LFPRM   TO X-DOMICILE.                              ECS087
00557      MOVE GRS-LFPRM   TO X-STATE.                                 ECS087
00558      MOVE GR-RESV     TO X-RESERV.                                ECS087
00559      MOVE GR-ALT-RESV TO X-ALTRSV.                                ECS087
00560      MOVE GR-REM-AMT  TO X-REMAIN.                                ECS087
00561      MOVE GR-LFCOM    TO X-PAID.                                  ECS087
00562      MOVE GRR-LFCOM   TO X-C78.                                   ECS087
00563      MOVE GRP-LFCOM   TO X-CRATA.                                 ECS087
00564      MOVE GR-LFTAX    TO X-TAX.                                   ECS087
00565      MOVE GRR-LFTAX   TO X-T78.                                   ECS087
00566      MOVE GRP-LFTAX   TO X-TRATA.                                 ECS087
00567      MOVE GR-LFTYP    TO WX-BEN.                                  ECS087
00568      MOVE +1          TO WX-TYP.                                  ECS087
00569                                                                   ECS087
00570      IF WX-POINTER = SAVE-LPOINTER                                ECS087
00571          MOVE SAVE-LX2 TO X2.                                     ECS087
00572                                                                   ECS087
00573      COMPUTE X-CALC-REMAIN ROUNDED = GR-AGE * GR-REM-AMT.         ECS087
00574                                                                   ECS087
00575      ADD X-CALC-REMAIN TO ACCT-AGE-REMAIN.                        ECS087
00576                                                                   ECS087
00577  0340-LOOP-LF.                                                    ECS087
00578      IF X2 GREATER THAN MAX-BEN                                   ECS087
00579         MOVE ' INVALID XXXXXX BENEFIT TYPE ' TO WS-ABEND-MESSAGE  ECS087
00580         MOVE LIFE-OVERRIDE-L6 TO WS-ABEND-BEN-TYPE                ECS087
00581         MOVE 0401 TO WS-RETURN-CODE                               ECS087
00582         GO TO ABEND-PGM.                                          ECS087
00583                                                                   ECS087
00584      IF WX-POINTER NOT = X-POINTER (X2)                           ECS087
00585          ADD +1 TO X2                                             ECS087
00586          GO TO 0340-LOOP-LF.                                      ECS087
00587                                                                   ECS087
00588      MOVE WX-POINTER  TO SAVE-LPOINTER.                           ECS087
00589      MOVE X2          TO SAVE-LX2.                                ECS087
00590      MOVE X-DESC (X2) TO SAVE-LDESC.                              ECS087
00591                                                                   ECS087
00592      MOVE X-AMTS (X2) TO R-DETL.                                  ECS087
00593      ADD X-COUNT      TO R-COUNT.                                 ECS087
00594      ADD X-WRITTEN    TO R-WRITTEN.                               ECS087
00595      ADD X-P78        TO R-P78.                                   ECS087
00596      ADD X-PRATA      TO R-PRATA.                                 ECS087
00597      ADD X-DOMICILE   TO R-DOMICILE.                              ECS087
00598      ADD X-STATE      TO R-STATE.                                 ECS087
00599      ADD X-RESERV     TO R-RESERV.                                ECS087
00600      ADD X-ALTRSV     TO R-ALTRSV.                                ECS087
00601      ADD X-REMAIN     TO R-REMAIN.                                ECS087
00602      ADD X-PAID       TO R-PAID.                                  ECS087
00603      ADD X-C78        TO R-C78.                                   ECS087
00604      ADD X-CRATA      TO R-CRATA.                                 ECS087
00605      ADD X-TAX        TO R-TAX.                                   ECS087
00606      ADD X-T78        TO R-T78.                                   ECS087
00607      ADD X-TRATA      TO R-TRATA.                                 ECS087
00608      MOVE R-DETL      TO X-AMTS (X2).                             ECS087
00609                                                                   ECS087
00610  0350-END-LIFE.                                                   ECS087
00611      IF GR-AHTYP NOT = ZEROS                                      ECS087
00612          ADD +1 TO X-DUP.                                         ECS087
00613                                                                   ECS087
00614  0360-FIND-AH.                                                    ECS087
00615      IF GR-AHTYP = ZEROS                                          ECS087
00616          GO TO 0380-BRANCH-BACK.                                  ECS087
00617                                                                   ECS087
00618      MOVE +1        TO X-COUNT.                                   ECS087
00619      MOVE GR-AHPRM  TO X-WRITTEN.                                 ECS087
00620      MOVE GRR-AHPRM TO X-P78.                                     ECS087
00621      MOVE GRP-AHPRM TO X-PRATA.                                   ECS087
00622      MOVE GRD-AHPRM TO X-DOMICILE.                                ECS087
00623      MOVE GRS-AHPRM TO X-STATE.                                   ECS087
00624      MOVE GR-AHCOM  TO X-PAID.                                    ECS087
00625      MOVE GRR-AHCOM TO X-C78.                                     ECS087
00626      MOVE GRP-AHCOM TO X-CRATA.                                   ECS087
00627      MOVE GR-AHTAX  TO X-TAX.                                     ECS087
00628      MOVE GRR-AHTAX TO X-T78.                                     ECS087
00629      MOVE GRP-AHTAX TO X-TRATA.                                   ECS087
00630                                                                   ECS087
00631      COMPUTE X-REMAIN ROUNDED = GR-AHBEN * GR-AH-REMTERM.         ECS087
00632                                                                   ECS087
00633      MOVE +0       TO X-RESERV.                                   ECS087
00634      MOVE GR-AHTYP TO WX-BEN.                                     ECS087
00635      MOVE +2       TO WX-TYP.                                     ECS087
00636                                                                   ECS087
00637      IF WX-POINTER = SAVE-APOINTER                                ECS087
00638          MOVE SAVE-AX2 TO X2.                                     ECS087
00639                                                                   ECS087
00640      ADD GR-AHBEN TO ACCT-AH-EXP.                                 ECS087
00641                                                                   ECS087
00642  0370-LOOP-HEALTH.                                                ECS087
00643      IF X2 GREATER THAN MAX-BEN                                   ECS087
00644         MOVE ' INVALID XXXXXX BENEFIT TYPE ' TO WS-ABEND-MESSAGE  ECS087
00645         MOVE AH-OVERRIDE-L6 TO WS-ABEND-BEN-TYPE                  ECS087
00646         MOVE 0402 TO WS-RETURN-CODE                               ECS087
00647         GO TO ABEND-PGM.                                          ECS087
00648                                                                   ECS087
00649      IF WX-POINTER NOT = X-POINTER (X2)                           ECS087
00650          ADD +1 TO X2                                             ECS087
00651          GO TO 0370-LOOP-HEALTH.                                  ECS087
00652                                                                   ECS087
00653      MOVE WX-POINTER  TO SAVE-APOINTER.                           ECS087
00654      MOVE X2          TO SAVE-AX2.                                ECS087
00655      MOVE X-DESC (X2) TO SAVE-ADESC.                              ECS087
00656                                                                   ECS087
00657      MOVE X-AMTS (X2) TO R-DETL.                                  ECS087
00658      ADD X-COUNT      TO R-COUNT.                                 ECS087
00659      ADD X-WRITTEN    TO R-WRITTEN.                               ECS087
00660      ADD X-P78        TO R-P78.                                   ECS087
00661      ADD X-PRATA      TO R-PRATA.                                 ECS087
00662      ADD X-DOMICILE   TO R-DOMICILE.                              ECS087
00663      ADD X-STATE      TO R-STATE.                                 ECS087
00664      ADD X-RESERV     TO R-RESERV.                                ECS087
00665      ADD X-ALTRSV     TO R-ALTRSV.                                ECS087
00666      ADD X-REMAIN     TO R-REMAIN.                                ECS087
00667      ADD X-PAID       TO R-PAID.                                  ECS087
00668      ADD X-C78        TO R-C78.                                   ECS087
00669      ADD X-CRATA      TO R-CRATA.                                 ECS087
00670      ADD X-TAX        TO R-TAX.                                   ECS087
00671      ADD X-T78        TO R-T78.                                   ECS087
00672      ADD X-TRATA      TO R-TRATA.                                 ECS087
00673      MOVE R-DETL      TO X-AMTS (X2).                             ECS087
00674                                                                   ECS087
00675  0380-BRANCH-BACK.                                                ECS087
00676                                                                   ECS087
00677      PERFORM 0400-PRT-DTL-RTN THRU 0499-PRT-DTL-XIT.              ECS087
00678                                                                   ECS087
00679      GO TO 0320-READ-RTN.                                         ECS087
00680  EJECT                                                            ECS087
00681  0400-PRT-DTL-RTN.                                                ECS087

CIDMOD     IF LNCTR GREATER THAN +50
00682 *    IF LNCTR GREATER THAN +58                                    ECS087
00683          PERFORM 1400-HD-RTN THRU 1499-HD-XIT                     ECS087
00684          MOVE '0' TO P-CCSW                                       ECS087
00685          MOVE DTL-4   TO P-LN                                     ECS087
00686          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00687          MOVE DTL-5 TO P-LN                                       ECS087
00688          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00689          MOVE '0' TO P-CCSW.                                      ECS087
00690                                                                   ECS087
00691      MOVE GR-CERT   TO PD-CERT.                                   ECS087
00692      MOVE GR-MO     TO PD-MO.                                     ECS087
00693      MOVE '-'       TO PD-DASH-1.                                 ECS087
00694      MOVE GR-DA     TO PD-DA.                                     ECS087
00695      MOVE '-'       TO PD-DASH-2.                                 ECS087
00696      MOVE GR-YR     TO PD-YR.                                     ECS087
00697                                                                   ECS087
00698      IF GR-LFTYP = ZEROS                                          ECS087
00699          GO TO 0410-SKIP-LIFE-DTL.                                ECS087
00700                                                                   ECS087
00701      MOVE SAVE-LDESC TO PD-BEN-DESC.                              ECS087
00702      MOVE GR-LF-TERM TO PD-ORIG.                                  ECS087
00703      MOVE GR-LF-UP-REMTERM TO PD-REM.                             ECS087
00704      MOVE GR-LFBEN   TO PD-BEN.                                   ECS087
00705      MOVE GR-REM-AMT TO PD-REMAMT.                                ECS087
00706      MOVE GR-LFPRM   TO PD-PREM.                                  ECS087
00707      MOVE GRP-LFPRM  TO PD-RATA.                                  ECS087
00708      MOVE GRR-LFPRM  TO PD-78.                                    ECS087
00709      MOVE GRS-LFPRM  TO PD-STATE.                                 ECS087
00710                                                                   ECS087
00711      PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT.                      ECS087
00712                                                                   ECS087
00713  0410-SKIP-LIFE-DTL.                                              ECS087
00714      IF GR-AHTYP = ZEROS                                          ECS087
00715          GO TO 0499-PRT-DTL-XIT.                                  ECS087
00716                                                                   ECS087
00717      IF GR-LFTYP NOT = ZEROS                                      ECS087
00718         MOVE SPACES TO P-DETAIL.                                  ECS087
00719                                                                   ECS087
00720      MOVE SAVE-ADESC       TO PD-BEN-DESC.                        ECS087
00721      MOVE GR-AH-UP-REMTERM TO PD-REM.                             ECS087
00722      MOVE GR-AH-TERM       TO PD-ORIG.                            ECS087
00723      COMPUTE PD-REMAMT = GR-AH-REMTERM * GR-AHBEN.                ECS087
00724      MOVE GR-AHBEN         TO PD-BEN.                             ECS087
00725      MOVE GR-AHPRM         TO PD-PREM.                            ECS087
00726      MOVE GRP-AHPRM        TO PD-RATA.                            ECS087
00727      MOVE GRR-AHPRM        TO PD-78.                              ECS087
00728      MOVE GRS-AHPRM        TO PD-STATE.                           ECS087
00729                                                                   ECS087
00730  0420-SKIP-AH-DTL.                                                ECS087
00731      PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT.                      ECS087
00732                                                                   ECS087
00733  0499-PRT-DTL-XIT.                                                ECS087
00734      EXIT.                                                        ECS087
00735  EJECT                                                            ECS087
00736  0500-BREAK-RTN.                                                  ECS087
00737      IF FST-SW = +0                                               ECS087
00738          MOVE +1 TO FST-SW                                        ECS087
00739          GO TO 0535-INTL-OVERALL.                                 ECS087
00740                                                                   ECS087
00741  0505-ACCT-BREAK.                                                 ECS087
00742      PERFORM 1200-ROLL-RTN THRU 1299-ROLL-XIT.                    ECS087
00743                                                                   ECS087
00744      ADD ACCT-AGE-REMAIN TO ST-AGE-REMAIN.                        ECS087
00745      ADD ACCT-AH-EXP     TO ST-AH-EXP.                            ECS087
00746      MOVE ACCT-WORK      TO CERT-WORK.                            ECS087
00747      ADD X-DUP           TO ST-DUP.                               ECS087
00748                                                                   ECS087
00749      PERFORM 0600-BLD-RTN THRU 0699-BLD-XIT.                      ECS087
00750                                                                   ECS087
00751      MOVE WX-ACCT TO CUR-ACCT.                                    ECS087
00752                                                                   ECS087
00753      IF CUR-SEQ = WX-SEQ                                          ECS087
00754          GO TO 0565-INTL-ACCT.                                    ECS087
00755                                                                   ECS087
00756  0510-ST-BREAK.                                                   ECS087
00757      MOVE ST-HD2    TO SUB-HD2.                                   ECS087
00758      MOVE ST-HD3    TO SUB-HD3.                                   ECS087
00759      MOVE ST-TOTALS TO X-TOTALS.                                  ECS087
00760      MOVE CO-TOTALS TO ST-TOTALS.                                 ECS087
00761                                                                   ECS087
00762      PERFORM 1200-ROLL-RTN THRU 1299-ROLL-XIT.                    ECS087
00763                                                                   ECS087
00764      MOVE ST-TOTALS    TO CO-TOTALS.                              ECS087
00765      ADD ST-AGE-REMAIN TO CO-AGE-REMAIN.                          ECS087
00766      ADD ST-AH-EXP     TO CO-AH-EXP.                              ECS087
00767      MOVE STATE-WORK   TO CERT-WORK.                              ECS087
00768      ADD ST-DUP        TO CO-DUP.                                 ECS087
00769      MOVE ST-DUP       TO X-DUP.                                  ECS087
00770                                                                   ECS087
00771      PERFORM 0600-BLD-RTN THRU 0699-BLD-XIT.                      ECS087
00772                                                                   ECS087
00773      MOVE WX-ST TO CUR-ST.                                        ECS087
00774                                                                   ECS087
00775      IF CUR-SEQ = WX-SEQ                                          ECS087
00776          GO TO 0555-INTL-ST.                                      ECS087
00777                                                                   ECS087
00778  0515-CO-BREAK.                                                   ECS087
00779      MOVE CO-HD2      TO SUB-HD2.                                 ECS087
00780      MOVE CO-HD3      TO SUB-HD3.                                 ECS087
00781      MOVE CO-TOTALS   TO X-TOTALS.                                ECS087
00782      MOVE CARR-TOTALS TO ST-TOTALS.                               ECS087
00783                                                                   ECS087
00784      PERFORM 1200-ROLL-RTN THRU 1299-ROLL-XIT.                    ECS087
00785                                                                   ECS087
00786      MOVE ST-TOTALS    TO CARR-TOTALS.                            ECS087
00787      ADD CO-AGE-REMAIN TO CARR-AGE-REMAIN.                        ECS087
00788      ADD CO-AH-EXP     TO CARR-AH-EXP.                            ECS087
00789      MOVE COMPANY-WORK TO CERT-WORK.                              ECS087
00790      ADD CO-DUP        TO CARR-DUP.                               ECS087
00791      MOVE CO-DUPS      TO X-DUPS.                                 ECS087
00792                                                                   ECS087
00793      PERFORM 0600-BLD-RTN THRU 0699-BLD-XIT.                      ECS087
00794                                                                   ECS087
00795      MOVE WX-CO TO CUR-CO.                                        ECS087
00796                                                                   ECS087
00797      IF WX-SEQ = CUR-SEQ                                          ECS087
00798          GO TO 0550-INTL-CO.                                      ECS087
00799                                                                   ECS087
00800  0520-CARR-BREAK.                                                 ECS087
00801      MOVE CARR-HD2     TO SUB-HD2.                                ECS087
00802      MOVE CARR-HD3     TO SUB-HD3.                                ECS087
00803      MOVE CARR-TOTALS  TO X-TOTALS.                               ECS087
00804      MOVE FINAL-TOTALS TO ST-TOTALS.                              ECS087
00805                                                                   ECS087
00806      PERFORM 1200-ROLL-RTN THRU 1299-ROLL-XIT.                    ECS087
00807                                                                   ECS087
00808      MOVE ST-TOTALS      TO FINAL-TOTALS.                         ECS087
00809      ADD CARR-AGE-REMAIN TO FINAL-AGE-REMAIN.                     ECS087
00810      ADD CARR-AH-EXP     TO FINAL-AH-EXP.                         ECS087
00811      MOVE CARRIER-WORK   TO CERT-WORK.                            ECS087
00812      ADD CARR-DUP        TO FINL-DUP.                             ECS087
00813      MOVE CARR-DUPS      TO X-DUPS.                               ECS087
00814                                                                   ECS087
00815      PERFORM 0600-BLD-RTN THRU 0699-BLD-XIT.                      ECS087
00816                                                                   ECS087
00817      MOVE WX-CARR TO CUR-CARR.                                    ECS087
00818                                                                   ECS087
00819      IF CUR-SEQ = WX-SEQ                                          ECS087
00820          GO TO 0545-INTL-CARR.                                    ECS087
00821                                                                   ECS087
00822  0525-FINAL-BREAK.                                                ECS087
00823      MOVE SPACES         TO SUB-HD2                               ECS087
00824                             SUB-HD3.                              ECS087
00825      MOVE FINAL-TOTALS   TO X-TOTALS.                             ECS087
00826      MOVE OVERALL-TOTALS TO ST-TOTALS.                            ECS087
00827                                                                   ECS087
00828      PERFORM 1200-ROLL-RTN THRU 1299-ROLL-XIT.                    ECS087
00829                                                                   ECS087
00830      MOVE ST-TOTALS       TO OVERALL-TOTALS.                      ECS087
00831      ADD FINAL-AH-EXP     TO OVER-AH-EXP.                         ECS087
00832      ADD FINAL-AGE-REMAIN TO OVER-AGE-REMAIN.                     ECS087
00833      MOVE FINAL-WORK      TO CERT-WORK.                           ECS087
00834      ADD FINL-DUP         TO OVER-DUP.                            ECS087
00835      MOVE FINL-DUPS       TO X-DUPS.                              ECS087
00836                                                                   ECS087
00837      PERFORM 0600-BLD-RTN THRU 0699-BLD-XIT.                      ECS087
00838                                                                   ECS087
00839      IF FST-SW NOT = +2                                           ECS087
00840          GO TO 0540-INTL-FINAL.                                   ECS087
00841                                                                   ECS087
00842  0530-OVERALL-BREAK.                                              ECS087
00843      MOVE 'FINAL TOTALS' TO REIN-HEADING.                         ECS087
00844      MOVE SPACES         TO SUB-HD2                               ECS087
00845                             SUB-HD3.                              ECS087
00846      MOVE OVERALL-TOTALS TO X-TOTALS.                             ECS087
00847      MOVE OVERALL-WORK   TO CERT-WORK.                            ECS087
00848      MOVE OVER-DUPS      TO X-DUPS.                               ECS087
00849                                                                   ECS087
00850      PERFORM 0600-BLD-RTN THRU 0699-BLD-XIT.                      ECS087
00851                                                                   ECS087
00852      GO TO 0599-BREAK-XIT.                                        ECS087
00853                                                                   ECS087
00854  0535-INTL-OVERALL.                                               ECS087
00855      PERFORM 1100-ZERO-RTN THRU 1199-ZERO-XIT.                    ECS087
00856                                                                   ECS087
00857      MOVE X-TOTALS TO OVERALL-TOTALS.                             ECS087
00858      MOVE +0       TO OVER-AGE-REMAIN  OVER-AH-EXP                ECS087
00859                       OVER-DUP.                                   ECS087
00860                                                                   ECS087
00861  0540-INTL-FINAL.                                                 ECS087
00862      IF ZERO-SW NOT = +0                                          ECS087
00863          PERFORM 1100-ZERO-RTN THRU 1199-ZERO-XIT.                ECS087
00864                                                                   ECS087
00865      MOVE WX-REIN  TO CUR-REIN REIN-COMP.                         ECS087
00866      MOVE X-TOTALS TO FINAL-TOTALS.                               ECS087
00867      MOVE +0       TO FINAL-AGE-REMAIN  FINAL-AH-EXP              ECS087
00868                       FINL-DUP.                                   ECS087
00869                                                                   ECS087
00870  0545-INTL-CARR.                                                  ECS087
00871      MOVE WX-CARR TO CUR-CARR  HD-CARR.                           ECS087
00872                                                                   ECS087
00873      IF ZERO-SW NOT = +0                                          ECS087
00874          PERFORM 1100-ZERO-RTN THRU 1199-ZERO-XIT.                ECS087
00875                                                                   ECS087
00876      MOVE X-TOTALS TO CARR-TOTALS.                                ECS087
00877      MOVE +0       TO CARR-AGE-REMAIN  CARR-AH-EXP                ECS087
00878                       CARR-DUP.                                   ECS087
00879                                                                   ECS087
00880  0550-INTL-CO.                                                    ECS087
00881      MOVE WX-CO TO CUR-CO HD-COMP.                                ECS087
00882                                                                   ECS087
00883      IF ZERO-SW NOT = +0                                          ECS087
00884          PERFORM 1100-ZERO-RTN THRU 1199-ZERO-XIT.                ECS087
00885                                                                   ECS087
00886      MOVE X-TOTALS TO CO-TOTALS.                                  ECS087
00887      MOVE +0       TO CO-AGE-REMAIN  CO-AH-EXP                    ECS087
00888                       CO-DUP.                                     ECS087
00889                                                                   ECS087
00890  0555-INTL-ST.                                                    ECS087
00891      MOVE WX-ST TO CUR-ST  HD-ST  STATE-L.                        ECS087
00892                                                                   ECS087
00893      IF ZERO-SW NOT = +0                                          ECS087
00894          PERFORM 1100-ZERO-RTN THRU 1199-ZERO-XIT.                ECS087
00895                                                                   ECS087
00896      MOVE X-TOTALS        TO ST-TOTALS.                           ECS087
00897      MOVE +0              TO ST-AGE-REMAIN  ST-AH-EXP             ECS087
00898                              ST-DUP.                              ECS087
00899      MOVE CLAS-STARTS     TO CLAS-INDEXS.                         ECS087
00900      MOVE 'INVALID STATE' TO HD-ST-NM.                            ECS087
00901                                                                   ECS087
00902  0560-FIND-ST-DESC.                                               ECS087
00903      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                        ECS087
00904          GO TO 0565-INTL-ACCT.                                    ECS087
00905                                                                   ECS087
00906      IF STATE-SUB (CLAS-INDEXS) NOT = STATE-L                     ECS087
00907          ADD 1 TO CLAS-INDEXS                                     ECS087
00908          GO TO 0560-FIND-ST-DESC.                                 ECS087
00909                                                                   ECS087
00910      MOVE STATE-PIC (CLAS-INDEXS) TO HD-ST-NM.                    ECS087
00911                                                                   ECS087
00912  0565-INTL-ACCT.                                                  ECS087
00913      MOVE WX-ACCT TO CUR-ACCT  HD-ACCT.                           ECS087
00914                                                                   ECS087
00915      IF ZERO-SW NOT = +0                                          ECS087
00916          PERFORM 1100-ZERO-RTN THRU 1199-ZERO-XIT.                ECS087
00917                                                                   ECS087
00918      MOVE +0       TO ACCT-AGE-REMAIN  ACCT-AH-EXP.               ECS087
00919      MOVE +0       TO X-DUP.                                      ECS087
00920      MOVE +1       TO ZERO-SW.                                    ECS087
00921      MOVE ACCT-HD2 TO SUB-HD2.                                    ECS087
00922      MOVE ACCT-HD3 TO SUB-HD3.                                    ECS087
00923      MOVE +76      TO LNCTR.                                      ECS087
00924                                                                   ECS087
00925  0599-BREAK-XIT.                                                  ECS087
00926      EXIT.                                                        ECS087
00927  EJECT                                                            ECS087
00928  0600-BLD-RTN.                                                    ECS087
00929      PERFORM 1400-HD-RTN THRU 1499-HD-XIT.                        ECS087
00930                                                                   ECS087
00931      MOVE +0             TO X-WT-REMAIN.                          ECS087
00932      MOVE 'REINSURANCE ' TO HD-DESC.                              ECS087
00933      MOVE '-'            TO P-CCSW.                               ECS087
00934                                                                   ECS087
00935      PERFORM 0700-PRT-EXTRACT THRU 0799-PRT-EXTRACT-XIT.          ECS087
00936                                                                   ECS087
00937  0610-BLD-SPECIAL.                                                ECS087
00938      MOVE X-AH-EXP    TO P-AH-EXP.                                ECS087
00939      MOVE X-WT-REMAIN TO P-WT-REMAIN.                             ECS087
00940      MOVE PRT-SPECIAL TO P-LN.                                    ECS087
00941      MOVE '-'         TO P-CCSW.                                  ECS087
00942                                                                   ECS087
00943      PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT.                      ECS087
00944                                                                   ECS087
00945  0699-BLD-XIT.                                                    ECS087
00946      EXIT.                                                        ECS087
00947  EJECT                                                            ECS087
00948  0700-PRT-EXTRACT.                                                ECS087
00949      MOVE +0 TO LIFE-SW                                           ECS087
00950                 AH-SW                                             ECS087
00951                 HD-SW                                             ECS087
00952                 S-COUNT                                           ECS087
00953                 S-WRITTEN                                         ECS087
00954                 S-P78                                             ECS087
00955                 S-PRATA                                           ECS087
00956                 S-DOMICILE                                        ECS087
00957                 S-STATE                                           ECS087
00958                 S-RESERV                                          ECS087
00959                 S-ALTRSV                                          ECS087
00960                 S-REMAIN.                                         ECS087
00961      MOVE +1 TO X2.                                               ECS087
00962                                                                   ECS087
00963  0710-LOOP-LIFE.                                                  ECS087
00964      IF X2 GREATER THAN MAX-BEN                                   ECS087
00965          GO TO 0720-OUT-LIFE.                                     ECS087
00966                                                                   ECS087
00967      IF X-TYP (X2) NOT = 1                                        ECS087
00968          GO TO 0720-OUT-LIFE.                                     ECS087
00969                                                                   ECS087
00970      MOVE X-AMTS (X2) TO X-DETL.                                  ECS087
00971                                                                   ECS087
00972      IF X-COUNT = +0                                              ECS087
00973          ADD +1 TO X2                                             ECS087
00974          GO TO 0710-LOOP-LIFE.                                    ECS087
00975                                                                   ECS087
CIDMOD     IF LNCTR GREATER THAN +50
00976 *    IF LNCTR GREATER THAN +58                                    ECS087
00977          MOVE +0      TO HD-SW                                    ECS087
00978          PERFORM 1400-HD-RTN THRU 1499-HD-XIT.                    ECS087
00979                                                                   ECS087
00980      IF HD-SW = +0                                                ECS087
00981          MOVE +1      TO LIFE-SW  HD-SW                           ECS087
00982          MOVE HD4     TO P-LN                                     ECS087
00983          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00984          MOVE HD5     TO P-LN                                     ECS087
00985          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00986          MOVE HD6     TO P-LN                                     ECS087
00987          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
00988          MOVE '0' TO P-CCSW.                                      ECS087
00989                                                                   ECS087
00990      MOVE X-BEN (X2)  TO PX-BEN.                                  ECS087
00991      MOVE X-DESC (X2) TO PX-DESC.                                 ECS087
00992      ADD X-COUNT      TO S-COUNT.                                 ECS087
00993      ADD X-WRITTEN    TO S-WRITTEN.                               ECS087
00994      ADD X-P78        TO S-P78.                                   ECS087
00995      ADD X-PRATA      TO S-PRATA.                                 ECS087
00996      ADD X-DOMICILE   TO S-DOMICILE.                              ECS087
00997      ADD X-STATE      TO S-STATE.                                 ECS087
00998      ADD X-RESERV     TO S-RESERV.                                ECS087
00999      ADD X-ALTRSV     TO S-ALTRSV.                                ECS087
01000      ADD X-REMAIN     TO S-REMAIN.                                ECS087
01001                                                                   ECS087
01002      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS087
01003                                                                   ECS087
01004      ADD +1 TO X2.                                                ECS087
01005                                                                   ECS087
01006      GO TO 0710-LOOP-LIFE.                                        ECS087
01007                                                                   ECS087
01008  0720-OUT-LIFE.                                                   ECS087
01009      MOVE SUB-TOTALS TO TOTALS.                                   ECS087
01010                                                                   ECS087
01011      IF LIFE-SW = 0                                               ECS087
01012          GO TO 0740-LOOP-AH.                                      ECS087
01013                                                                   ECS087
01014      MOVE 'TOTAL'      TO PX-DESC.                                ECS087
01015      MOVE LIFE-OVERRIDE-L2 TO PX-BEN-DESC.                        ECS087
01016      MOVE SPACES       TO PX-BEN.                                 ECS087
01017      MOVE SUB-TOTALS   TO X-DETL.                                 ECS087
01018                                                                   ECS087
01019      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS087
01020                                                                   ECS087
01021      MOVE '0' TO P-CCSW.                                          ECS087
01022                                                                   ECS087
01023      IF S-REMAIN = +0                                             ECS087
01024          MOVE +0 TO X-WT-REMAIN                                   ECS087
01025      ELSE                                                         ECS087
01026          COMPUTE X-WT-REMAIN ROUNDED = X-CALC-REMAIN / S-REMAIN.  ECS087
01027                                                                   ECS087
01028  0730-ZERO-SUBTOTAL.                                              ECS087
01029      MOVE +0 TO S-COUNT                                           ECS087
01030                 S-WRITTEN                                         ECS087
01031                 S-P78                                             ECS087
01032                 S-PRATA                                           ECS087
01033                 S-DOMICILE                                        ECS087
01034                 S-STATE                                           ECS087
01035                 S-RESERV                                          ECS087
01036                 S-ALTRSV                                          ECS087
01037                 S-REMAIN.                                         ECS087
01038                                                                   ECS087
01039  0740-LOOP-AH.                                                    ECS087
01040      IF X2 GREATER THAN MAX-BEN                                   ECS087
01041          GO TO 0750-OUT-AH.                                       ECS087
01042                                                                   ECS087
01043      IF X-TYP (X2) NOT = 2                                        ECS087
01044          GO TO 0740-LOOP-AH.                                      ECS087
01045                                                                   ECS087
01046      MOVE X-AMTS (X2) TO X-DETL.                                  ECS087
01047                                                                   ECS087
01048      IF X-COUNT = +0                                              ECS087
01049          ADD +1 TO X2                                             ECS087
01050          GO TO 0740-LOOP-AH.                                      ECS087
01051                                                                   ECS087
CIDMOD     IF LNCTR GREATER THAN +50
01052 *    IF LNCTR GREATER THAN +58                                    ECS087
01053          MOVE +0      TO HD-SW                                    ECS087
01054          PERFORM 1400-HD-RTN THRU 1499-HD-XIT.                    ECS087
01055                                                                   ECS087
01056      IF HD-SW = +0                                                ECS087
01057          MOVE +1      TO HD-SW                                    ECS087
01058          MOVE HD4     TO P-LN                                     ECS087
01059          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
01060          MOVE HD5     TO P-LN                                     ECS087
01061          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
01062          MOVE HD6     TO P-LN                                     ECS087
01063          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
01064          MOVE '0' TO P-CCSW.                                      ECS087
01065                                                                   ECS087
01066      MOVE +1          TO AH-SW.                                   ECS087
01067      MOVE X-BEN (X2)  TO PX-BEN.                                  ECS087
01068      MOVE X-DESC (X2) TO PX-DESC.                                 ECS087
01069      ADD X-COUNT      TO S-COUNT.                                 ECS087
01070      ADD X-WRITTEN    TO S-WRITTEN.                               ECS087
01071      ADD X-P78        TO S-P78.                                   ECS087
01072      ADD X-PRATA      TO S-PRATA.                                 ECS087
01073      ADD X-DOMICILE   TO S-DOMICILE.                              ECS087
01074      ADD X-STATE      TO S-STATE.                                 ECS087
01075      ADD X-RESERV     TO S-RESERV.                                ECS087
01076      ADD X-ALTRSV     TO S-ALTRSV.                                ECS087
01077      ADD X-REMAIN     TO S-REMAIN.                                ECS087
01078                                                                   ECS087
01079      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS087
01080                                                                   ECS087
01081      ADD +1 TO X2.                                                ECS087
01082                                                                   ECS087
01083      GO TO 0740-LOOP-AH.                                          ECS087
01084                                                                   ECS087
01085  0750-OUT-AH.                                                     ECS087
01086      IF AH-SW = +0                                                ECS087
01087          GO TO 0790-PRE-XIT.                                      ECS087
01088                                                                   ECS087
01089      MOVE 'TOTAL    ' TO PX-DESC.                                 ECS087
01090      MOVE AH-OVERRIDE-L2 TO PX-BEN-DESC.                          ECS087
01091      MOVE SPACES      TO PX-BEN.                                  ECS087
01092      MOVE SUB-TOTALS  TO X-DETL.                                  ECS087
01093                                                                   ECS087
01094      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS087
01095                                                                   ECS087
01096      MOVE '0'       TO P-CCSW.                                    ECS087
01097      ADD S-COUNT    TO T-COUNT.                                   ECS087
01098      ADD S-WRITTEN  TO T-WRITTEN.                                 ECS087
01099      ADD S-P78      TO T-P78.                                     ECS087
01100      ADD S-PRATA    TO T-PRATA.                                   ECS087
01101      ADD S-DOMICILE TO T-DOMICILE.                                ECS087
01102      ADD S-STATE    TO T-STATE.                                   ECS087
01103      ADD S-RESERV   TO T-RESERV.                                  ECS087
01104      ADD S-ALTRSV   TO T-ALTRSV.                                  ECS087
01105      ADD S-REMAIN   TO T-REMAIN.                                  ECS087
01106                                                                   ECS087
01107  0760-PRT-TOTALS.                                                 ECS087
01108      IF LIFE-SW = 0                                               ECS087
01109          GO TO 0790-PRE-XIT.                                      ECS087
01110                                                                   ECS087
01111      MOVE TOTALS      TO X-DETL.                                  ECS087
01112      SUBTRACT X-DUP   FROM X-COUNT.                               ECS087
01113      MOVE '  TOTAL  ' TO PX-DESC.                                 ECS087
01114      MOVE SPACES      TO PX-BEN.                                  ECS087
01115                                                                   ECS087
01116      PERFORM 0800-PRT-LINE THRU 0899-PRT-LINE-XIT.                ECS087
01117                                                                   ECS087
01118  0790-PRE-XIT.                                                    ECS087
01119      PERFORM 0900-PRT-EXTRACT THRU 0999-PRT-EXTRACT-XIT.          ECS087
01120                                                                   ECS087
01121  0799-PRT-EXTRACT-XIT.                                            ECS087
01122      EXIT.                                                        ECS087
01123  EJECT                                                            ECS087
01124  0800-PRT-LINE.                                                   ECS087
01125      MOVE PX-BEN     TO P-BEN.                                    ECS087
01126      MOVE PX-DESC    TO P-DESC.                                   ECS087
01127      MOVE X-COUNT    TO P-COUNT.                                  ECS087
01128      MOVE X-WRITTEN  TO P-WRITTEN.                                ECS087
01129      MOVE X-P78      TO P-P78.                                    ECS087
01130      MOVE X-PRATA    TO P-PRATA.                                  ECS087
01131      MOVE X-DOMICILE TO P-PDOMICILE.                              ECS087
01132      MOVE X-STATE    TO P-PSTATE.                                 ECS087
01133      MOVE X-RESERV   TO P-RESERV.                                 ECS087
01134      MOVE X-ALTRSV   TO P-ALTRSV.                                 ECS087
01135      MOVE X-REMAIN   TO P-REMAIN.                                 ECS087
01136                                                                   ECS087
01137      PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT.                      ECS087
01138                                                                   ECS087
01139  0899-PRT-LINE-XIT.                                               ECS087
01140      EXIT.                                                        ECS087
01141  EJECT                                                            ECS087
01142  0900-PRT-EXTRACT.                                                ECS087
01143      MOVE +0 TO LIFE-SW                                           ECS087
01144                 AH-SW                                             ECS087
01145                 HD-SW                                             ECS087
01146                 S-PAID                                            ECS087
01147                 S-C78                                             ECS087
01148                 S-CRATA                                           ECS087
01149                 S-TAX                                             ECS087
01150                 S-T78                                             ECS087
01151                 S-TRATA.                                          ECS087
01152      MOVE +1 TO X2.                                               ECS087
01153                                                                   ECS087
01154  0910-LOOP-LIFE.                                                  ECS087
01155      IF X2 GREATER THAN MAX-BEN                                   ECS087
01156          GO TO 0920-OUT-LIFE.                                     ECS087
01157                                                                   ECS087
01158      IF X-TYP (X2) NOT = 1                                        ECS087
01159          GO TO 0920-OUT-LIFE.                                     ECS087
01160                                                                   ECS087
01161      MOVE X-AMTS (X2) TO X-DETL.                                  ECS087
01162                                                                   ECS087
01163      IF X-COUNT = +0                                              ECS087
01164          ADD +1 TO X2                                             ECS087
01165          GO TO 0910-LOOP-LIFE.                                    ECS087
01166                                                                   ECS087
CIDMOD     IF LNCTR GREATER THAN +50
01167 *    IF LNCTR GREATER THAN +58                                    ECS087
01168          MOVE +0      TO HD-SW                                    ECS087
01169          PERFORM 1400-HD-RTN THRU 1499-HD-XIT.                    ECS087
01170                                                                   ECS087
01171      IF HD-SW = +0                                                ECS087
01172          MOVE +1      TO LIFE-SW  HD-SW                           ECS087
01173          MOVE '-' TO P-CCSW                                       ECS087
01174          MOVE HD7     TO P-LN                                     ECS087
01175          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
01176          MOVE HD8     TO P-LN                                     ECS087
01177          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
01178          MOVE '0' TO P-CCSW.                                      ECS087
01179                                                                   ECS087
01180      MOVE X-BEN (X2)  TO PX-BEN.                                  ECS087
01181      MOVE X-DESC (X2) TO PX-DESC.                                 ECS087
01182      ADD X-PAID       TO S-PAID.                                  ECS087
01183      ADD X-C78        TO S-C78.                                   ECS087
01184      ADD X-CRATA      TO S-CRATA.                                 ECS087
01185      ADD X-TAX        TO S-TAX.                                   ECS087
01186      ADD X-T78        TO S-T78.                                   ECS087
01187      ADD X-TRATA      TO S-TRATA.                                 ECS087
01188                                                                   ECS087
01189      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS087
01190                                                                   ECS087
01191      ADD +1 TO X2.                                                ECS087
01192                                                                   ECS087
01193      GO TO 0910-LOOP-LIFE.                                        ECS087
01194                                                                   ECS087
01195  0920-OUT-LIFE.                                                   ECS087
01196      MOVE SUB-TOTALS TO TOTALS.                                   ECS087
01197                                                                   ECS087
01198      IF LIFE-SW = 0                                               ECS087
01199          GO TO 0940-LOOP-AH.                                      ECS087
01200                                                                   ECS087
01201      MOVE 'TOTAL     ' TO PX-DESC.                                ECS087
01202      MOVE LIFE-OVERRIDE-L2 TO PX-BEN-DESC.                        ECS087
01203      MOVE SPACES       TO PX-BEN.                                 ECS087
01204      MOVE SUB-TOTALS   TO X-DETL.                                 ECS087
01205                                                                   ECS087
01206      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS087
01207                                                                   ECS087
01208      MOVE '0' TO P-CCSW.                                          ECS087
01209                                                                   ECS087
01210  0930-ZERO-SUBTOTAL.                                              ECS087
01211      MOVE +0 TO S-PAID                                            ECS087
01212                 S-C78                                             ECS087
01213                 S-CRATA                                           ECS087
01214                 S-TAX                                             ECS087
01215                 S-T78                                             ECS087
01216                 S-TRATA.                                          ECS087
01217                                                                   ECS087
01218  0940-LOOP-AH.                                                    ECS087
01219      IF X2 GREATER THAN MAX-BEN                                   ECS087
01220          GO TO 0950-OUT-AH.                                       ECS087
01221                                                                   ECS087
01222      IF X-TYP (X2) NOT = 2                                        ECS087
01223          GO TO 0940-LOOP-AH.                                      ECS087
01224                                                                   ECS087
01225      MOVE X-AMTS (X2) TO X-DETL.                                  ECS087
01226                                                                   ECS087
01227      IF X-COUNT = +0                                              ECS087
01228          ADD +1 TO X2                                             ECS087
01229          GO TO 0940-LOOP-AH.                                      ECS087
01230                                                                   ECS087
CIDMOD     IF LNCTR GREATER THAN +50
01231 *    IF LNCTR GREATER THAN +58                                    ECS087
01232          MOVE +0      TO HD-SW                                    ECS087
01233          PERFORM 1400-HD-RTN THRU 1499-HD-XIT.                    ECS087
01234                                                                   ECS087
01235      IF HD-SW = +0                                                ECS087
01236          MOVE +1  TO HD-SW                                        ECS087
01237          MOVE '-' TO P-CCSW                                       ECS087
01238          MOVE HD7     TO P-LN                                     ECS087
01239          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
01240          MOVE HD8     TO P-LN                                     ECS087
01241          PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT                   ECS087
01242          MOVE '0' TO P-CCSW.                                      ECS087
01243                                                                   ECS087
01244      MOVE +1          TO AH-SW.                                   ECS087
01245      MOVE X-BEN (X2)  TO PX-BEN.                                  ECS087
01246      MOVE X-DESC (X2) TO PX-DESC.                                 ECS087
01247      ADD X-PAID       TO S-PAID.                                  ECS087
01248      ADD X-C78        TO S-C78.                                   ECS087
01249      ADD X-CRATA      TO S-CRATA.                                 ECS087
01250      ADD X-TAX        TO S-TAX.                                   ECS087
01251      ADD X-T78        TO S-T78.                                   ECS087
01252      ADD X-TRATA      TO S-TRATA.                                 ECS087
01253                                                                   ECS087
01254      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS087
01255                                                                   ECS087
01256      ADD +1 TO X2.                                                ECS087
01257                                                                   ECS087
01258      GO TO 0940-LOOP-AH.                                          ECS087
01259                                                                   ECS087
01260  0950-OUT-AH.                                                     ECS087
01261      IF AH-SW = +0                                                ECS087
01262          GO TO 0999-PRT-EXTRACT-XIT.                              ECS087
01263                                                                   ECS087
01264      MOVE 'TOTAL    ' TO PX-DESC.                                 ECS087
01265      MOVE AH-OVERRIDE-L2 TO PX-BEN-DESC.                          ECS087
01266      MOVE SPACES      TO PX-BEN.                                  ECS087
01267      MOVE SUB-TOTALS  TO X-DETL.                                  ECS087
01268                                                                   ECS087
01269      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS087
01270                                                                   ECS087
01271      MOVE '0'       TO P-CCSW.                                    ECS087
01272      ADD S-PAID     TO T-PAID.                                    ECS087
01273      ADD S-C78      TO T-C78.                                     ECS087
01274      ADD S-CRATA    TO T-CRATA.                                   ECS087
01275      ADD S-TAX      TO T-TAX.                                     ECS087
01276      ADD S-T78      TO T-T78.                                     ECS087
01277      ADD S-TRATA    TO T-TRATA.                                   ECS087
01278                                                                   ECS087
01279  0960-PRT-TOTALS.                                                 ECS087
01280      IF LIFE-SW = 0                                               ECS087
01281          GO TO 0999-PRT-EXTRACT-XIT.                              ECS087
01282                                                                   ECS087
01283      MOVE TOTALS      TO X-DETL.                                  ECS087
01284      SUBTRACT X-DUP   FROM X-COUNT.                               ECS087
01285      MOVE '  TOTAL  ' TO PX-DESC.                                 ECS087
01286      MOVE SPACES      TO PX-BEN.                                  ECS087
01287                                                                   ECS087
01288      PERFORM 1000-PRT-LINE THRU 1099-PRT-LINE-XIT.                ECS087
01289                                                                   ECS087
01290  0999-PRT-EXTRACT-XIT.                                            ECS087
01291      EXIT.                                                        ECS087
01292  EJECT                                                            ECS087
01293  1000-PRT-LINE.                                                   ECS087
01294      MOVE PX-BEN     TO P-BEN.                                    ECS087
01295      MOVE PX-DESC    TO P-DESC.                                   ECS087
01296      MOVE X-PAID     TO P-PAID.                                   ECS087
01297      MOVE X-C78      TO P-C78.                                    ECS087
01298      MOVE X-CRATA    TO P-CRATA.                                  ECS087
01299      MOVE X-TAX      TO P-TAX.                                    ECS087
01300      MOVE X-T78      TO P-T78.                                    ECS087
01301      MOVE X-TRATA    TO P-TRATA.                                  ECS087
01302                                                                   ECS087
01303      PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT.                      ECS087
01304                                                                   ECS087
01305  1099-PRT-LINE-XIT.                                               ECS087
01306      EXIT.                                                        ECS087
01307  EJECT                                                            ECS087
01308  1100-ZERO-RTN.                                                   ECS087
01309      MOVE +0 TO X-COUNT                                           ECS087
01310                 X-WRITTEN                                         ECS087
01311                 X-P78                                             ECS087
01312                 X-PRATA                                           ECS087
01313                 X-DOMICILE                                        ECS087
01314                 X-STATE                                           ECS087
01315                 X-RESERV                                          ECS087
01316                 X-ALTRSV                                          ECS087
01317                 X-REMAIN                                          ECS087
01318                 X-PAID                                            ECS087
01319                 X-C78                                             ECS087
01320                 X-CRATA                                           ECS087
01321                 X-TAX                                             ECS087
01322                 X-T78                                             ECS087
01323                 X-TRATA                                           ECS087
01324                 ZERO-SW.                                          ECS087
01325      MOVE +0 TO X2.                                               ECS087
01326                                                                   ECS087
01327  1110-ZERO-X2.                                                    ECS087
01328      ADD +1 TO X2.                                                ECS087
01329                                                                   ECS087
01330      IF X2 GREATER THAN MAX-BEN                                   ECS087
01331          GO TO 1199-ZERO-XIT.                                     ECS087
01332                                                                   ECS087
01333      MOVE X-DETL TO X-AMTS (X2).                                  ECS087
01334                                                                   ECS087
01335      GO TO 1110-ZERO-X2.                                          ECS087
01336                                                                   ECS087
01337  1199-ZERO-XIT.                                                   ECS087
01338      EXIT.                                                        ECS087
01339                                                                   ECS087
01340  1200-ROLL-RTN.                                                   ECS087
01341      MOVE +0 TO X2.                                               ECS087
01342                                                                   ECS087
01343  1210-ROLL-X2.                                                    ECS087
01344      ADD +1 TO X2.                                                ECS087
01345                                                                   ECS087
01346      IF X2 GREATER THAN MAX-BEN                                   ECS087
01347          GO TO 1299-ROLL-XIT.                                     ECS087
01348                                                                   ECS087
01349      MOVE X-AMTS (X2)  TO X-DETL.                                 ECS087
01350      MOVE ST-AMTS (X2) TO R-DETL.                                 ECS087
01351      ADD X-COUNT       TO R-COUNT.                                ECS087
01352      ADD X-WRITTEN     TO R-WRITTEN.                              ECS087
01353      ADD X-P78         TO R-P78.                                  ECS087
01354      ADD X-PRATA       TO R-PRATA.                                ECS087
01355      ADD X-DOMICILE    TO R-DOMICILE.                             ECS087
01356      ADD X-STATE       TO R-STATE.                                ECS087
01357      ADD X-RESERV      TO R-RESERV.                               ECS087
01358      ADD X-ALTRSV      TO R-ALTRSV.                               ECS087
01359      ADD X-REMAIN      TO R-REMAIN.                               ECS087
01360      ADD X-PAID        TO R-PAID.                                 ECS087
01361      ADD X-C78         TO R-C78.                                  ECS087
01362      ADD X-CRATA       TO R-CRATA.                                ECS087
01363      ADD X-TAX         TO R-TAX.                                  ECS087
01364      ADD X-T78         TO R-T78.                                  ECS087
01365      ADD X-TRATA       TO R-TRATA.                                ECS087
01366      MOVE R-DETL       TO ST-AMTS (X2).                           ECS087
01367                                                                   ECS087
01368      GO TO 1210-ROLL-X2.                                          ECS087
01369                                                                   ECS087
01370  1299-ROLL-XIT.                                                   ECS087
01371      EXIT.                                                        ECS087
01372  EJECT                                                            ECS087
01373  1300-LOAD-TABLES.                                                ECS087
01374      MOVE +0          TO X2.                                      ECS087
01375      MOVE CLAS-STARTL TO CLAS-INDEXL.                             ECS087
01376      MOVE CLAS-STARTA TO CLAS-INDEXA.                             ECS087
01377      MOVE HIGH-VALUE  TO X-POINTERS.                              ECS087
01378                                                                   ECS087
01379      IF CLAS-MAXL = ZEROS                                         ECS087
01380          GO TO 1320-FORMAT-AH-RTN.                                ECS087
01381                                                                   ECS087
01382  1310-FORMAT-LIFE.                                                ECS087
01383      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS087
01384          GO TO 1320-FORMAT-AH-RTN.                                ECS087
01385                                                                   ECS087
01386      ADD +1                         TO X2.                        ECS087
01387      MOVE CLAS-I-BEN (CLAS-INDEXL)  TO X-BEN (X2).                ECS087
01388      MOVE 1                         TO X-TYP (X2).                ECS087
01389      MOVE CLAS-I-AB10 (CLAS-INDEXL) TO X-DESC (X2).               ECS087
01390      ADD +1                         TO CLAS-INDEXL.               ECS087
01391                                                                   ECS087
01392      GO TO 1310-FORMAT-LIFE.                                      ECS087
01393                                                                   ECS087
01394  1320-FORMAT-AH-RTN.                                              ECS087
01395      IF CLAS-MAXA = ZEROS                                         ECS087
01396          GO TO 1340-FORMAT-SET.                                   ECS087
01397                                                                   ECS087
01398  1330-FORMAT-AH.                                                  ECS087
01399      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS087
01400          GO TO 1340-FORMAT-SET.                                   ECS087
01401                                                                   ECS087
01402      ADD +1                         TO X2.                        ECS087
01403      MOVE CLAS-I-BEN (CLAS-INDEXA)  TO X-BEN (X2).                ECS087
01404      MOVE 2                         TO X-TYP (X2).                ECS087
01405      MOVE CLAS-I-AB10 (CLAS-INDEXA) TO X-DESC (X2).               ECS087
01406      ADD +1                         TO CLAS-INDEXA.               ECS087
01407                                                                   ECS087
01408      GO TO 1330-FORMAT-AH.                                        ECS087
01409                                                                   ECS087
01410  1340-FORMAT-SET.                                                 ECS087
01411      MOVE X2 TO MAX-BEN.                                          ECS087
01412                                                                   ECS087
01413  1399-LOAD-TAB-XIT.                                               ECS087
01414      EXIT.                                                        ECS087
01415  EJECT                                                            ECS087
01416  1400-HD-RTN.                                                     ECS087
01417      ADD +1       TO PGCTR.                                       ECS087
01418      MOVE PGCTR   TO HD-PAGE.                                     ECS087
01419      MOVE HD1     TO P-LN.                                        ECS087
01420      MOVE '1' TO P-CCSW.                                          ECS087
01421      PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT.                      ECS087
01422                                                                   ECS087
01423      MOVE HD2 TO P-LN.                                            ECS087
01424      PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT.                      ECS087
01425                                                                   ECS087
01426      MOVE HD3 TO P-LN.                                            ECS087
01427      PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT.                      ECS087
01428                                                                   ECS087
01429      MOVE HD3A    TO P-LN.                                        ECS087
01430      MOVE '0' TO P-CCSW.                                          ECS087
01431      PERFORM 1500-PRT-RTN THRU 1599-PRT-XIT.                      ECS087
01432                                                                   ECS087
01433  1499-HD-XIT.                                                     ECS087
01434      EXIT.                                                        ECS087
01435                                                                   ECS087
01436  1500-PRT-RTN.                                                    ECS087
01437      MOVE P-CCSW  TO X P-CTL.                                     ECS087
01438      MOVE P-LN    TO P-DATA.                                      ECS087
01439      MOVE ' '     TO P-REC.                                       ECS087
01440                                                                   ECS087
01441      IF X = ' '                                                   ECS087
01442          ADD +1 TO LNCTR                                          ECS087
01443      ELSE                                                         ECS087
01444          IF X = '0'                                               ECS087
01445              ADD +2 TO LNCTR                                      ECS087
01446          ELSE                                                     ECS087
01447              IF X = '-'                                           ECS087
01448                  ADD +3 TO LNCTR                                  ECS087
01449              ELSE                                                 ECS087
01450                  MOVE +1 TO LNCTR.                                ECS087
01451                                                                   ECS087
01452  1510-PRT-COPY-RTN.                                               ECS087
01453                              COPY ELCPRT2.                        ECS087
01454                                                                   ECS087
01455  1599-PRT-XIT.                                                    ECS087
01456      EXIT.                                                        ECS087
01457                                                                   ECS087
01458  1600-END-OUTPUT.                                                 ECS087
01459      MOVE +2         TO FST-SW.                                   ECS087
01460      MOVE HIGH-VALUE TO WX-SEQ.                                   ECS087
01461                                                                   ECS087
01462      PERFORM 0500-BREAK-RTN THRU 0599-BREAK-XIT.                  ECS087
01463                                                                   ECS087
01464  1699-OUTPUT-XIT.                                                 ECS087
01465      EXIT.                                                        ECS087
01466  EJECT                                                            ECS087
01467  1700-END-OF-JOB SECTION.                                         ECS087
01468                                                                   ECS087
01469  1710-END-JOB.                                                    ECS087
01470                              COPY ELCPRTC.                        ECS087
01471                                                                   ECS087
01472      CLOSE PRNTR.                                                 ECS087
01473      GOBACK.                                                      ECS087
01474                                                                   ECS087
01475  ABEND-PGM.                                                       ECS087
01476                         COPY ELCABEND.                            ECS087
01477                                                                   ECS087
