00001  IDENTIFICATION DIVISION.                                         12/02/98
00002                                                                   ECS156
00003  PROGRAM-ID.                 ECS156.                                 LV020
00004 *              PROGRAM CONVERTED BY                               ECS156
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS156
00006 *              CONVERSION DATE 10/17/97 08:55:12.                 ECS156
00007 *                            VMOD=2.004.                          ECS156
00008 *                                                                 ECS156
00009 *AUTHOR.        LOGIC, INC.                                       ECS156
00010 *               DALLAS, TEXAS.                                    ECS156
00011                                                                   ECS156
00012 *DATE-COMPILED.                                                   ECS156
00013                                                                   ECS156
00014 *SECURITY.   *****************************************************ECS156
00015 *            *                                                   *ECS156
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS156
00017 *            *                                                   *ECS156
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS156
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS156
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS156
00021 *            *                                                   *ECS156
00022 *            *****************************************************ECS156
00023                                                                   ECS156
00024 *REMARKS.                                                         ECS156
00025 *      THIS PROGRAM COMPARES PRIOR YEAR END CERTIFICATE MASTER    ECS156
00026 *    TO CURRENT YEAR END CERTIFICATE MASTER.  FOR ANY CERTIFICATE ECS156
00027 *    CANCELLED IN THE PREVIOUS YEAR WHERE MONEY WAS REFUNDED,     ECS156
00028 *    AMOUNTS ARE COMPARED AND ANY VARIANCES ARE REPORTED AND      ECS156
00029 *    TOTALED.                                                     ECS156
00030 *                                                                 ECS156
00031 *   REPORT TOTAL  OPTIONS :                                       ECS156
00032 *                  1 =  CARRIER, STATE, RATE/CLASS                ECS156
00033 *                  2 =  CARRIER, STATE, RATE DEVIATION            ECS156
00034 *                  3 =  CARRIER, STATE, ACCOUNT, RATE DEVIATION   ECS156
00035 *                  4 =  CARRIER, STATE, DEVIATION BUSINESS TYPE   ECS156
00036 *                  5 =  REPORTS WITH ALL OF THE ABOVE TOTALS      ECS156
00037 *                  6 =  REPORTS WITH NONE OF THE ABOVE TOTALING   ECS156
00038 *                     NOTE : OPTION '6' MUST BE USED WITH 'FORMAT'ECS156
00039 *                     OPTION '2' WHICH WILL GIVE A DETAIL LISTING ECS156
00040 *                     ONLY WITH LIFE AND A&H TOTALS AT THE END.   ECS156
00041 *                                                                 ECS156
00042 *   REPORT FORMAT OPTIONS :                                       ECS156
00043 *                  2 =  PRINT DETAIL                              ECS156
00044 *                                                                 ECS156
00045 *                                                                 ECS156
00046 *                                                                 ECS156
00047                                                                   ECS156
00048  ENVIRONMENT DIVISION.                                            ECS156
00049  CONFIGURATION SECTION.                                           ECS156
00050  SPECIAL-NAMES.                                                   ECS156
00051      C02 IS LCP-CH2                                               ECS156
00052      C03 IS LCP-CH3                                               ECS156
00053      C04 IS LCP-CH4                                               ECS156
00054      C05 IS LCP-CH5                                               ECS156
00055      C06 IS LCP-CH6                                               ECS156
00056      C07 IS LCP-CH7                                               ECS156
00057      C08 IS LCP-CH8                                               ECS156
00058      C09 IS LCP-CH9                                               ECS156
00059      C10 IS LCP-CH10                                              ECS156
00060      C11 IS LCP-CH11                                              ECS156
00061      C12 IS LCP-CH12                                              ECS156
00062      S01 IS LCP-P01                                               ECS156
00063      S02 IS LCP-P02.                                              ECS156
00064  INPUT-OUTPUT SECTION.                                            ECS156
00065  FILE-CONTROL.                                                    ECS156
00066                                                                   ECS156
00067      SELECT DISK-DATE        ASSIGN TO SYS019-UT-S-SYS019.        ECS156
00068      SELECT CERT-PREV        ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS156
00069      SELECT CERT-CURR        ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS156
00070      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS156
00071      SELECT WORK-FILE        ASSIGN TO SYS001-UT-2400-S-SYS012.   ECS156
00072      SELECT SORT-FILE        ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  ECS156
00073                                                                   ECS156
00074      EJECT                                                        ECS156
00075  DATA DIVISION.                                                   ECS156
00076  FILE SECTION.                                                    ECS156
00077                                                                   ECS156
00078  FD  DISK-DATE                                                    ECS156
00079                              COPY ELCDTEFD.                       ECS156
00080                                                                   ECS156
00081  EJECT                                                            ECS156
00082  FD  CERT-PREV                                                    ECS156
00083      BLOCK CONTAINS 0 RECORDS
00084      RECORDING MODE F.                                            ECS156
00085                                                                   ECS156
00086                                  COPY ECSCRT01.                   ECS156
00087                                                                   ECS156
00088  FD  CERT-CURR                                                    ECS156
00089      BLOCK CONTAINS 0 RECORDS
00090      RECORDING MODE F.                                            ECS156
00091                                                                   ECS156
00092      COPY ECSCRT01 REPLACING                                      ECS156
00093      CR-FULL-CONTROL   BY   CO-FULL-CONTROL                       ECS156
00094      CR-CERT-NO        BY   CO-CERT-NO                            ECS156
00095      CR-LFTYP          BY   CO-LFTYP                              ECS156
00096      CR-LF-CANC-DT     BY   CO-LF-CANC-DT                         ECS156
00097      CR-LF-CANCEL-EXIT-DATE BY                                    ECS156
00098                             CO-LF-CANCEL-EXIT-DATE                ECS156
00099      CR-LF-DEV-CODE    BY   CO-LF-DEV-CODE                        ECS156
00100      CR-LFRFND         BY   CO-LFRFND                             ECS156
00101      CR-AHTYP          BY   CO-AHTYP                              ECS156
00102      CR-AH-CANC-DT     BY   CO-AH-CANC-DT                         ECS156
00103      CR-AH-CANCEL-EXIT-DATE BY                                    ECS156
00104                             CO-AH-CANCEL-EXIT-DATE                ECS156
00105      CR-AH-DEV-CODE    BY   CO-AH-DEV-CODE                        ECS156
00106      CR-GRPTYP         BY   CO-GRPTYP                             ECS156
00107      CR-RATING-CLASS   BY   CO-RATING-CLASS                       ECS156
00108      CR-AHRFND         BY   CO-AHRFND.                            ECS156
00109                                                                   ECS156
00110  01  CERT-CURR-REC             PIC  X(1056).                      ECS156
00111      EJECT                                                        ECS156
00112                                                                   ECS156
00113  FD  PRNTR                                                        ECS156
00114      RECORDING MODE F.                                            ECS156
00115  01  PRT.                                                         ECS156
00116      03  P-CTL      PIC X.                                        ECS156
00117      03  P-LINE     PIC X(132).                                   ECS156
00118  EJECT                                                            ECS156
00119  FD  WORK-FILE                                                    ECS156
00120      BLOCK CONTAINS 0 RECORDS
00121      RECORDING MODE F.                                            ECS156
00122                                                                   ECS156
00123  01  WORK-RECORD.                                                 ECS156
00124      12  WK-CONTROL.                                              ECS156
00125          16  WK-CARRIER    PIC X.                                 ECS156
00126          16  WK-GROUPING   PIC X(6).                              ECS156
00127          16  WK-STATE      PIC XX.                                ECS156
00128          16  WK-ACCOUNT    PIC X(10).                             ECS156
00129          16  WK-COVERAGE   PIC XX.                                ECS156
00130      12  WK-RATE-CLASS     PIC XX.                                ECS156
00131      12  WK-RATE-DEVIATION PIC XXX.                               ECS156
00132      12  WK-BUSINESS-TYPE  PIC XX.                                ECS156
00133      12  WK-VARIANCE       PIC S9(7)V99.                          ECS156
00134      12  FILLER            PIC X(39).                             ECS156
00135      12  WK-CERT-NO        PIC X(11).                             ECS156
00136                                                                   ECS156
00137  EJECT                                                            ECS156
00138  SD  SORT-FILE.                                                   ECS156
00139                                                                   ECS156
00140  01  SORT-RECORD.                                                 ECS156
00141      12  SW-CONTROL.                                              ECS156
00142          16  SW-CARRIER    PIC X.                                 ECS156
00143          16  SW-GROUPING   PIC X(6).                              ECS156
00144          16  SW-STATE      PIC XX.                                ECS156
00145          16  SW-ACCOUNT    PIC X(10).                             ECS156
00146          16  SW-COVERAGE   PIC XX.                                ECS156
00147      12  SW-RATE-CLASS     PIC XX.                                ECS156
00148      12  SW-RATE-DEVIATION PIC XXX.                               ECS156
00149      12  SW-BUSINESS-TYPE  PIC XX.                                ECS156
00150      12  SW-VARIANCE       PIC S9(7)V99.                          ECS156
00151      12  FILLER            PIC X(39).                             ECS156
00152      12  SW-CERT-NO        PIC X(11).                             ECS156
00153                                                                   ECS156
00154  WORKING-STORAGE SECTION.                                         ECS156
00155  77  FILLER  PIC  X(32) VALUE '********************************'. ECS156
00156  77  FILLER  PIC  X(32) VALUE '*           ECS156             *'. ECS156
00157  77  FILLER  PIC  X(32) VALUE '***********VMOD=2.004***********'. ECS156
00158                                                                   ECS156
00159  77  LCP-ASA                     PIC X.                           ECS156
00160  77  SUB1                        PIC  S9(04) COMP VALUE +0.       ECS156
00161  77  SUB2                        PIC  S9(04) COMP VALUE +0.       ECS156
00162  77  RECORDS-WRITTEN             PIC  9(07) VALUE ZERO.           ECS156
00163  77  RECORDS-RELEASED            PIC  9(07) VALUE ZERO.           ECS156
00164  77  RECORDS-RECEIVED            PIC  9(07) VALUE ZERO.           ECS156
00165  77  CERT-PREV-RECS              PIC  9(07) VALUE ZERO.           ECS156
00166  77  CERT-CURR-RECS              PIC  9(07) VALUE ZERO.           ECS156
00167  77  CERT-DUP-RECS               PIC  9(07) VALUE ZERO.           ECS156
00168  77  PRINT-DATA                  PIC  X      VALUE SPACE.         ECS156
00169  77  WRITE-FILE                  PIC  X      VALUE SPACE.         ECS156
00170  77  X                           PIC  X      VALUE SPACE.         ECS156
00171  77  LINER                       PIC  S9(3)  COMP-3  VALUE +80.   ECS156
00172  77  LINER-2                     PIC  S9(3)  COMP-3  VALUE +80.   ECS156
00173  77  PAGER                       PIC  S9(3)  COMP-3  VALUE +1.    ECS156
00174                                                                   ECS156
00175  01  LCP-ABND-CODE               PIC S999 COMP VALUE +519.        ECS156
00176  01  LCP-CURRENT-DATE-68.                                         ECS156
00177      05  LCP-MONTH               PIC XX.                          ECS156
00178      05  FILLER                  PIC X VALUE '/'.                 ECS156
00179      05  LCP-DAY1                PIC XX.                          ECS156
00180      05  FILLER                  PIC X VALUE '/'.                 ECS156
00181      05  LCP-YEAR                PIC XX.                          ECS156
00182  01  LCP-DATE-NEW-74.                                             ECS156
00183      05  LCP-YEAR                PIC XX.                          ECS156
00184      05  LCP-MONTH               PIC XX.                          ECS156
00185      05  LCP-DAY1                PIC XX.                          ECS156
00186                                                                   ECS156
00187  01  REPORT-TOTALING-FIELDS.                                      ECS156
00188      12  LIFE-DATA.                                               ECS156
00189          16  LIFE-VARIANCE       PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00190          16  LIFE-DEV-VARIANCE   PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00191          16  LIFE-BUSTP-VARIANCE PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00192          16  LIFE-CLASS-VARIANCE PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00193          16  LIFE-ACCT-VARIANCE  PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00194          16  LIFE-STATE-VARIANCE PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00195          16  LIFE-CARR-VARIANCE  PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00196          16  LIFE-TOTAL-VARIANCE PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00197                                                                   ECS156
00198          16  AH-VARIANCE         PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00199          16  AH-DEV-VARIANCE     PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00200          16  AH-BUSTP-VARIANCE   PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00201          16  AH-CLASS-VARIANCE   PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00202          16  AH-ACCT-VARIANCE    PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00203          16  AH-STATE-VARIANCE   PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00204          16  AH-CARR-VARIANCE    PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00205          16  AH-TOTAL-VARIANCE   PIC S9(11)V99 COMP-3 VALUE +0.   ECS156
00206                                                                   ECS156
00207  01  WS-SAVE-FIELDS.                                              ECS156
00208      12  WS-SAVE-CARRIER             PIC X.                       ECS156
00209      12  WS-SAVE-STATE               PIC XX.                      ECS156
00210      12  WS-SAVE-ACCOUNT             PIC X(10).                   ECS156
00211      12  WS-SAVE-DEV                 PIC XXX.                     ECS156
00212      12  WS-SAVE-BUSTP               PIC XX.                      ECS156
00213      12  WS-SAVE-CLASS               PIC XX.                      ECS156
00214      12  WS-SAVE-COVERAGE            PIC XX.                      ECS156
00215                                                                   ECS156
00216  01  WS-COMPARE-FIELDS.                                           ECS156
00217      12  WS-PREV-CONTROL.                                         ECS156
00218          16  WS-PREV-CARRIER         PIC X.                       ECS156
00219          16  WS-PREV-GROUPING        PIC X(6).                    ECS156
00220          16  WS-PREV-STATE           PIC XX.                      ECS156
00221          16  WS-PREV-ACCOUNT         PIC X(10).                   ECS156
00222          16  WS-PREV-CR-DT           PIC 9(11)   COMP-3.             CL**8
00223          16  WS-PREV-CERT-NO         PIC X(11).                   ECS156
00224                                                                   ECS156
00225      12  WS-CURR-CONTROL.                                         ECS156
00226          16  WS-CURR-CARRIER         PIC X.                       ECS156
00227          16  WS-CURR-GROUPING        PIC X(6).                    ECS156
00228          16  WS-CURR-STATE           PIC XX.                      ECS156
00229          16  WS-CURR-ACCOUNT         PIC X(10).                   ECS156
00230          16  WS-CURR-CR-DT           PIC 9(11)    COMP-3.            CL**9
00231          16  WS-CURR-CERT-NO         PIC X(11).                   ECS156
00232                                                                   ECS156
00233      12  WS-PREV-CNC-DT              PIC 9(11).                      CL*13
00234      12  WS-PREV-CNC-DT-RDEF  REDEFINES  WS-PREV-CNC-DT.             CL*10
00235          16  FILLER                  PIC 999.                        CL*16
00236          16  WS-PREV-CNC-CCYY        PIC 9(4).                       CL*15
00237          16  WS-PREV-CNC-CCYR  REDEFINES  WS-PREV-CNC-CCYY.       ECS156
00238              20  WS-PREV-CNC-CC      PIC 99.                         CL*16
00239              20  WS-PREV-CNC-YR      PIC 99.                         CL*16
00240          16  WS-PREV-CNC-MO          PIC 99.                         CL*16
00241          16  WS-PREV-CNC-DA          PIC 99.                         CL*16
00242                                                                   ECS156
00243      12  WS-CURR-CNC-DT              PIC 9(11).                      CL*13
00244      12  WS-CURR-CNC-DT-RDEF  REDEFINES  WS-CURR-CNC-DT.             CL*10
00245          16  FILLER                  PIC 999.                        CL*16
00246          16  WS-CURR-CNC-CCYY        PIC 9(4).                       CL*15
00247          16  WS-CURR-CNC-CCYR  REDEFINES  WS-CURR-CNC-CCYY.          CL**4
00248              20  WS-CURR-CNC-CC      PIC 99.                         CL*16
00249              20  WS-CURR-CNC-YR      PIC 99.                         CL*16
00250          16  WS-CURR-CNC-MO          PIC 99.                         CL*16
00251          16  WS-CURR-CNC-DA          PIC 99.                         CL*16
00252                                                                   ECS156
00253      12  WS-PREV-EFF-DT              PIC 9(11).                      CL*18
00254      12  WS-PREV-EFF-DT-RDEF  REDEFINES  WS-PREV-EFF-DT.             CL*18
00255          16  FILLER                  PIC 999.                        CL*18
00256          16  WS-PREV-EFF-CCYY        PIC 9(4).                       CL*18
00257          16  WS-PREV-EFF-CCYR  REDEFINES  WS-PREV-EFF-CCYY.          CL*18
00258              20  WS-PREV-EFF-CC      PIC 99.                         CL*18
00259              20  WS-PREV-EFF-YR      PIC 99.                         CL*18
00260          16  WS-PREV-EFF-MO          PIC 99.                         CL*18
00261          16  WS-PREV-EFF-DA          PIC 99.                         CL*18
00262                                                                      CL*18
00263  01  WS-MISC.                                                     ECS156
00264      12  PGM-SUB                 PIC S9(04) VALUE +156 COMP.      ECS156
00265      12  WS-TOT-PREV-YR          PIC S9(11)V99 COMP-3  VALUE +0.  ECS156
00266      12  WS-TOT-CURR-YR          PIC S9(11)V99 COMP-3  VALUE +0.  ECS156
00267      12  WS-TOT-VAR              PIC S9(11)V99 COMP-3  VALUE +0.  ECS156
00268                                                                   ECS156
00269      12  WS-PREV-YEAR            PIC 9(04).                       ECS156
00270                                                                   ECS156
00271      12  WS-CORR-DATE.                                            ECS156
00272          16  WS-CORR-MO          PIC 99.                          ECS156
00273          16  FILLER              PIC X VALUE '/'.                 ECS156
00274          16  WS-CORR-DA          PIC 99.                          ECS156
00275          16  FILLER              PIC X VALUE '/'.                 ECS156
00276          16  WS-CORR-YR          PIC 99.                          ECS156
00277                                                                   ECS156
00278      12  WS-BIN-EFF-DT           PIC XX.                          ECS156
00279      12  WS-WORK-DATE.                                            ECS156
00280          16  WS-WORK-CCYY        PIC 9(04).                       ECS156
00281          16  WS-WORK-CCYR  REDEFINES  WS-WORK-CCYY.               ECS156
00282              20  WS-WORK-CC      PIC 99.                          ECS156
00283              20  WS-WORK-YR      PIC 99.                          ECS156
00284          16  WS-WORK-MO          PIC 99.                          ECS156
00285          16  WS-WORK-DA          PIC 99.                          ECS156
00286      12  WRITE-SWITCH            PIC X(01)  VALUE SPACES.         ECS156
00287          88  WRITING-RECORD      VALUE ' '.                       ECS156
00288      12  DSP-LINE1.                                               ECS156
00289          16  DSP1-BEFORE         PIC X(36).                       ECS156
00290          16  FILLER              PIC X(01).                       ECS156
00291          16  DSP1-COMMENT1       PIC X(20).                       ECS156
00292          16  DSP1-B4-DATE        PIC X(06).                       ECS156
00293          16  DSP1-COMMENT2       PIC X(20).                       ECS156
00294          16  DSP1-AFTER          PIC X(06).                       ECS156
00295      12  DSP-LINE2.                                               ECS156
00296          16  DSP-BEFORE          PIC X(36).                       ECS156
00297          16  FILLER              PIC X(05).                       ECS156
00298          16  DSP-AFTER           PIC X(36).                       ECS156
00299      12  WS-LAST-SORT-KEY.                                        ECS156
00300          16  FILLER              PIC X(35) VALUE LOW-VALUES.      ECS156
00301          16  WS-LAST-SUFF        PIC X(01) VALUE LOW-VALUES.      ECS156
00302      12  WS-RETURN-CODE          PIC S9(04) COMP   VALUE +0.      ECS156
00303      12  WS-ABEND-FILE-STATUS    PIC X(02)         VALUE SPACES.  ECS156
00304      12  WS-ABEND-MESSAGE        PIC X(80)         VALUE SPACES.  ECS156
00305      12  WS-ZERO                 PIC S9(01) COMP-3 VALUE +0.      ECS156
00306                                                                   ECS156
00307  01  SWITCHES.                                                    ECS156
00308      12  EOF-SW                 PIC  X.                           ECS156
00309         88  NOT-OF-PREV-FILE      VALUE ' '.                      ECS156
00310         88  END-OF-PREV-FILE      VALUE 'Y'.                      ECS156
00311      12  EOF-SW-2               PIC  X.                           ECS156
00312         88  NOT-OF-CURR-FILE      VALUE ' '.                      ECS156
00313         88  END-OF-CURR-FILE      VALUE 'Y'.                      ECS156
00314                                                                   ECS156
00315      EJECT                                                        ECS156
00316  01  HEAD-A.                                                      ECS156
00317      03  FILLER      PIC X(51)   VALUE SPACES.                    ECS156
00318      03  HA-NAME     PIC X(30)   VALUE SPACES.                    ECS156
00319      03  FILLER      PIC X(47)   VALUE SPACES.                    ECS156
00320                                                                   ECS156
00321  01  HEAD-B.                                                      ECS156
00322      03  FILLER      PIC X(51)   VALUE SPACES.                    ECS156
00323      03  FILLER      PIC X(32)   VALUE                            ECS156
00324           'CANCELLED CERTIFICATE COMPARISON'.                     ECS156
00325      03  FILLER      PIC X(47)   VALUE SPACES.                    ECS156
00326                                                                   ECS156
00327  01  HEAD-C.                                                      ECS156
00328      12  FILLER      PIC X(56)   VALUE SPACES.                    ECS156
00329      12  FILLER      PIC X(11)   VALUE 'YEAR ENDING'.             ECS156
00330      12  FILLER      PIC X(3)    VALUE SPACES.                    ECS156
00331      03  HC-DATE.                                                 ECS156
00332        05  HC-MO     PIC XX.                                      ECS156
00333        05  FILLER    PIC X       VALUE '/'.                       ECS156
00334        05  HC-DA     PIC XX.                                      ECS156
00335        05  FILLER    PIC X       VALUE '/'.                       ECS156
00336        05  HC-YR     PIC XX.                                      ECS156
00337      03  FILLER      PIC X(39)   VALUE SPACES.                    ECS156
00338      03  FILLER      PIC X(5)    VALUE 'PAGE'.                    ECS156
00339      03  HC-PAGE     PIC ZZ9.                                     ECS156
00340                                                                   ECS156
00341  01  HEAD-D.                                                      ECS156
00342      03  FILLER      PIC X(8)    VALUE ' CARRIER'.                ECS156
00343      03  FILLER      PIC X(4)    VALUE SPACES.                    ECS156
00344      03  FILLER      PIC X(8)    VALUE 'GROUPING'.                ECS156
00345      03  FILLER      PIC X(4)    VALUE ' ST '.                    ECS156
00346      03  FILLER      PIC X(5)    VALUE SPACES.                    ECS156
00347      03  FILLER      PIC X(8)    VALUE 'ACCOUNT '.                ECS156
00348      03  FILLER      PIC X(11)   VALUE 'CERTIFICATE'.             ECS156
00349      03  FILLER      PIC XX      VALUE SPACES.                    ECS156
00350      03  FILLER      PIC X(8)    VALUE 'EFF DATE'.                ECS156
00351      03  FILLER      PIC X       VALUE SPACE.                     ECS156
00352      03  FILLER      PIC X(4)    VALUE 'TYP '.                    ECS156
00353      03  FILLER      PIC X       VALUE SPACES.                    ECS156
00354      03  FILLER      PIC X(25)                                    ECS156
00355                           VALUE '<----- PREVIOUS ----->   '.      ECS156
00356      03  FILLER      PIC XX      VALUE SPACES.                    ECS156
00357      03  FILLER      PIC X(25)                                    ECS156
00358                           VALUE '<----- CURRENT ----->    '.      ECS156
00359      03  FILLER      PIC X(8)    VALUE SPACES.                    ECS156
00360                                                                   ECS156
00361  01  HEAD-E.                                                      ECS156
00362      03  FILLER      PIC X(64)   VALUE SPACES.                    ECS156
00363      03  FILLER      PIC X(11)   VALUE 'CANCEL DATE'.             ECS156
00364      03  FILLER      PIC X(5)    VALUE SPACES.                    ECS156
00365      03  FILLER      PIC X(8)    VALUE 'AMOUNT  '.                ECS156
00366      03  FILLER      PIC X(3)    VALUE SPACES.                    ECS156
00367      03  FILLER      PIC X(11)   VALUE 'CANCEL DATE'.             ECS156
00368      03  FILLER      PIC X(4)    VALUE SPACES.                    ECS156
00369      03  FILLER      PIC X(8)    VALUE 'AMOUNT  '.                ECS156
00370      03  FILLER      PIC X(2)    VALUE SPACES.                    ECS156
00371      03  FILLER      PIC X(8)    VALUE 'VARIANCE'.                ECS156
00372                                                                   ECS156
00373  01  HEAD-A1.                                                     ECS156
00374      12  FILLER      PIC X(5)    VALUE SPACES.                    ECS156
00375      12  HD-RUN-DT   PIC X(8)    VALUE SPACES.                    ECS156
00376      12  FILLER      PIC X(23)   VALUE SPACES.                    ECS156
00377      12  FILLER      PIC X(24)   VALUE 'CREDIT LIFE AND CREDIT H'.ECS156
00378      12  FILLER      PIC X(24)   VALUE 'EALTH AND ACCIDENT - REF'.ECS156
00379      12  FILLER      PIC X(19)   VALUE 'UND VARIANCE REPORT'.     ECS156
00380      12  FILLER      PIC X(16)   VALUE SPACES.                    ECS156
00381      12  FILLER      PIC X(7)    VALUE 'ECS-156'.                 ECS156
00382      12  HEAD-A-TYPE PIC X(01)   VALUE 'A'.                       ECS156
00383                                                                   ECS156
00384  01  HEAD-AA.                                                     ECS156
00385      12  FILLER      PIC X(47)   VALUE SPACES.                    ECS156
00386      12  HD-RPT-TYP  PIC X(30)   VALUE SPACES.                    ECS156
00387      12  FILLER      PIC X(55)   VALUE SPACES.                    ECS156
00388                                                                   ECS156
00389  01  HEAD-B1.                                                     ECS156
00390      12  FILLER      PIC X(18)   VALUE SPACES.                    ECS156
00391      12  HDB-REIN    PIC X(13)   VALUE SPACES.                    ECS156
00392      12  FILLER      PIC X(16)   VALUE SPACES.                    ECS156
00393      12  COM-NAM     PIC X(30)   VALUE SPACES.                    ECS156
00394      12  FILLER      PIC X(47)   VALUE SPACES.                    ECS156
00395      12  FILLER      PIC X(8)    VALUE SPACES.                    ECS156
00396                                                                   ECS156
00397  01  HEAD-C1.                                                     ECS156
00398      12  FILLER      PIC X(46)   VALUE SPACES.                    ECS156
00399      12  FILLER      PIC X(11)   VALUE 'YEAR ENDING'.             ECS156
00400      12  FILLER      PIC X(3)    VALUE SPACES.                    ECS156
00401      12  HC-DATE     PIC X(18).                                   ECS156
00402      12  FILLER      PIC X(34)   VALUE SPACES.                    ECS156
00403      12  FILLER      PIC X(5)    VALUE 'PAGE '.                   ECS156
00404      12  HD-PAGE     PIC ZZ,ZZ9.                                  ECS156
00405                                                                   ECS156
00406  01  HEAD-CC.                                                     ECS156
00407      12  FILLER      PIC X(18)   VALUE ' ***** TOTALS FOR'.       ECS156
00408      12  HD-TYPE-TOT PIC X(33)   VALUE SPACES.                    ECS156
00409                                                                   ECS156
00410  01  HEAD-HH.                                                     ECS156
00411      12  FILLER                  PIC X(38) VALUE SPACES.          ECS156
00412      12  HH-YEARS  OCCURS 3.                                      ECS156
00413          16  FILLER              PIC X(03).                       ECS156
00414          16  HEAD-HH-MO          PIC 9(02).                       ECS156
00415          16  HEAD-HH-SLASH-1     PIC X(01).                       ECS156
00416          16  HEAD-HH-DA          PIC 9(02).                       ECS156
00417          16  HEAD-HH-SLASH-2     PIC X(01).                       ECS156
00418          16  HEAD-HH-YR          PIC 9(02).                       ECS156
00419          16  HEAD-HH-THRU        PIC X(05).                       ECS156
00420                                                                   ECS156
00421  01  HEAD-H1.                                                     ECS156
00422      12  FILLER                  PIC X(08) VALUE SPACES.          ECS156
00423      12  HH-BENEFIT              PIC X(19).                       ECS156
00424      12  FILLER                  PIC X(11) VALUE SPACES.          ECS156
00425      12  HH-NAMES OCCURS 4.                                       ECS156
00426          16  FILLER              PIC X(03).                       ECS156
00427          16  HEAD-H-MO           PIC 9(02).                       ECS156
00428          16  HEAD-H-SLASH-1      PIC X(01).                       ECS156
00429          16  HEAD-H-DA           PIC 9(02).                       ECS156
00430          16  HEAD-H-SLASH-2      PIC X(01).                       ECS156
00431          16  HEAD-H-YR           PIC 9(02).                       ECS156
00432          16  FILLER              PIC X(05).                       ECS156
00433                                                                   ECS156
00434  01  HEAD-D-REIN.                                                 ECS156
00435      12  FILLER                  PIC X(24)   VALUE                ECS156
00436          ' REINSURANCE COMPANY -  '.                              ECS156
00437      12  HEAD-D-REIN-CO          PIC X(6)    VALUE SPACES.        ECS156
00438                                                                   ECS156
00439  01  HEAD-E-CARR.                                                 ECS156
00440      12  FILLER                  PIC X(11)   VALUE                ECS156
00441          ' CARRIER - '.                                           ECS156
00442      12  HEADE-CARR              PIC X       VALUE SPACES.        ECS156
00443                                                                   ECS156
00444  01  HEAD-F-COMP.                                                 ECS156
00445      12  FILLER                  PIC X(11)   VALUE                ECS156
00446          ' GROUPING- '.                                           ECS156
00447      12  HEADF-COMP              PIC X(6)    VALUE SPACES.        ECS156
00448                                                                   ECS156
00449  01  HEAD-FF-VAR.                                                 ECS156
00450      12  HEADFF-VAR-DESC         PIC X(25)   VALUE SPACES.        ECS156
00451      12  HEADFF-VAR              PIC X(25)   VALUE SPACES.        ECS156
00452      12  FILLER                  PIC X(5)    VALUE SPACES.        ECS156
00453      12  HEADFF-VARA             PIC X(30)   VALUE SPACES.        ECS156
00454                                                                   ECS156
00455  01  HEAD-G-STATE.                                                ECS156
00456      12  FILLER                  PIC X(09)   VALUE                ECS156
00457          ' STATE - '.                                             ECS156
00458      12  HEADG-STATE             PIC X(20)   VALUE SPACES.        ECS156
00459                                                                   ECS156
00460  01  HEAD-GG-VAR.                                                 ECS156
00461      12  HEADGG-VAR-DESC         PIC X(25)   VALUE SPACES.        ECS156
00462      12  HEADGG-VAR.                                              ECS156
00463          16  HEADGG-VAR-ACCT     PIC X(10)   VALUE SPACES.        ECS156
00464          16  FILLER              PIC X(02)   VALUE SPACES.        ECS156
00465          16  HEADGG-VAR-SLASH    PIC X(01)   VALUE SPACES.        ECS156
00466          16  FILLER              PIC X(02)   VALUE SPACES.        ECS156
00467          16  HEADGG-VAR-DEV      PIC X(03)   VALUE SPACES.        ECS156
00468          16  FILLER              PIC X(07)   VALUE SPACES.        ECS156
00469      12  FILLER                  PIC X(5)    VALUE SPACES.        ECS156
00470      12  HEADGG-VARA             PIC X(50)   VALUE SPACES.        ECS156
00471                                                                   ECS156
00472  01  HEAD-HH-VAR.                                                 ECS156
00473      12  HEADHH-VAR-DESC.                                         ECS156
00474          16  FILLER              PIC X(01)   VALUE SPACES.        ECS156
00475          16  HEADHH-VAR-TERM     PIC X(07)   VALUE SPACES.        ECS156
00476          16  HEADHH-TERM-DESC    PIC X(15)   VALUE SPACES.        ECS156
00477          16  FILLER              PIC X(02)   VALUE SPACES.        ECS156
00478      12  HEADHH-VAR              PIC X(50)   VALUE SPACES.        ECS156
00479      12  FILLER                  PIC X(5)    VALUE SPACES.        ECS156
00480      12  HEADHH-VARA             PIC X(30)   VALUE SPACES.        ECS156
00481                                                                   ECS156
00482  01  WS-DETAIL-PRINT-LINE.                                        ECS156
00483      12  WS-PRINT-CARRIAGE       PIC X(5)     VALUE '0'.          ECS156
00484      12  WS-PRINT-CARRIER        PIC X        VALUE SPACE.        ECS156
00485      12  FILLER                  PIC X(7)     VALUE SPACES.       ECS156
00486      12  WS-PRINT-GROUPING       PIC X(6)     VALUE SPACES.       ECS156
00487      12  FILLER                  PIC XX       VALUE SPACES.       ECS156
00488      12  WS-PRINT-STATE          PIC XX       VALUE SPACES.       ECS156
00489      12  FILLER                  PIC X(4)     VALUE SPACES.       ECS156
00490      12  WS-PRINT-ACCOUNT        PIC X(10).                       ECS156
00491      12  FILLER                  PIC X        VALUE SPACES.       ECS156
00492      12  WS-PRINT-CERT-NO        PIC X(11)    VALUE SPACES.       ECS156
00493      12  FILLER                  PIC X        VALUE SPACES.       ECS156
00494      12  WS-PRINT-EFF-DATE       PIC X(8)     VALUE SPACES.       ECS156
00495      12  FILLER                  PIC X        VALUE SPACES.       ECS156
00496      12  WS-PRINT-TYPE           PIC X(3)     VALUE SPACES.       ECS156
00497      12  FILLER                  PIC X(4)     VALUE SPACES.       ECS156
00498      12  WS-PRINT-CANC-DT-1      PIC X(8)     VALUE SPACES.       ECS156
00499      12  FILLER                  PIC X(1)     VALUE SPACES.       ECS156
00500      12  WS-PRINT-CANC-AMT-1     PIC ZZZZ,ZZ9.99.                 ECS156
00501      12  FILLER                  PIC X(6)     VALUE SPACES.       ECS156
00502      12  WS-PRINT-CANC-DT-2      PIC X(8)     VALUE SPACES.       ECS156
00503      12  FILLER                  PIC X(1)     VALUE SPACES.       ECS156
00504      12  WS-PRINT-CANC-AMT-2     PIC ZZZZ,ZZ9.99.                 ECS156
00505      12  FILLER                  PIC X(1)     VALUE SPACES.       ECS156
00506      12  WS-PRINT-VARIANCE       PIC ZZZZ,ZZ9.99-.                ECS156
00507                                                                   ECS156
00508  01  WS-DETAIL-PRINT-ERROR.                                       ECS156
00509      12  FILLER                  PIC X(59)    VALUE '0'.          ECS156
00510      12  WS-PRINT-MESSAGE        PIC X(60)    VALUE SPACES.       ECS156
00511                                                                   ECS156
00512  01  WS-MESSAGE-1.                                                ECS156
00513      12  FILLER                  PIC X(8)     VALUE '0'.          ECS156
00514      12  FILLER                  PIC X(50)  VALUE                 ECS156
00515         'PREVIOUS REFUND AMOUNT WAS ZERO.  THE NET PREMIUM '.     ECS156
00516      12  FILLER                  PIC X(50)  VALUE                 ECS156
00517         'WILL NOT BE AFFECTED IN THIS CIRCUMSTANCE.        '.     ECS156
00518      12  FILLER                  PIC X(11)  VALUE                 ECS156
00519         '           '.                                            ECS156
00520                                                                   ECS156
00521  01  WS-MESSAGE-2.                                                ECS156
00522      12  FILLER                  PIC X(6)     VALUE '0'.          ECS156
00523      12  FILLER                  PIC X(50)    VALUE               ECS156
00524         'CANCEL EXIT DATES ARE THE SAME BUT REFUND AMOUNT W'.     ECS156
00525      12  FILLER                  PIC X(50)    VALUE               ECS156
00526         'AS ADJUSTED. THE PREMIUM WILL BE AFFECTED BY THE N'.     ECS156
00527      12  FILLER                  PIC X(13)    VALUE               ECS156
00528         'ET OF THE TWO'.                                          ECS156
00529                                                                   ECS156
00530  01  WS-MESSAGE-3.                                                ECS156
00531      12  FILLER                  PIC X(8)     VALUE '0'.          ECS156
00532      12  FILLER                  PIC X(50)    VALUE               ECS156
00533         'CANCEL EXIT DATES REFLECT DIFFERENT YEARS.  THE NE'.     ECS156
00534      12  FILLER                  PIC X(50)    VALUE               ECS156
00535         'T PREMIUM WILL BE AFFECTED BY THE PREVIOUS REFUND '.     ECS156
00536      12  FILLER                  PIC X(11)    VALUE               ECS156
00537         'AMOUNT ONLY'.                                            ECS156
00538                                                                   ECS156
00539  01  WS-TOTAL-LINE.                                               ECS156
00540      12  FILLER                  PIC X(25)    VALUE '0'.          ECS156
00541      12  FILLER                  PIC X(10)    VALUE               ECS156
00542                         'TOTAL LIFE'.                             ECS156
00543      12  FILLER                  PIC X(5)     VALUE SPACES.       ECS156
00544      12  WS-TOTAL-LIFE-VARIANCE  PIC ZZZZ,ZZ9.99-.                ECS156
00545      12  FILLER                  PIC X(5)     VALUE SPACES.       ECS156
00546      12  FILLER                  PIC X(10)    VALUE               ECS156
00547                         'TOTAL AH  '.                             ECS156
00548      12  FILLER                  PIC X(5)     VALUE SPACES.       ECS156
00549      12  WS-TOTAL-AH-VARIANCE    PIC ZZZZ,ZZ9.99-.                ECS156
00550      12  FILLER                  PIC X(5)     VALUE SPACES.       ECS156
00551      12  FILLER                  PIC X(14)    VALUE               ECS156
00552                         'TOTAL VARIANCE'.                         ECS156
00553      12  WS-TOTAL-VARIANCE       PIC ZZZZ,ZZ9.99-.                ECS156
00554                                                                   ECS156
00555  01  HEAD-CARRIER.                                                ECS156
00556      12  FILLER                  PIC X(5)     VALUE '0'.          ECS156
00557      12  FILLER                  PIC X(12)                        ECS156
00558                             VALUE 'CARRIER :'.                    ECS156
00559      12  HEAD-CARRIER-D          PIC X.                           ECS156
00560                                                                   ECS156
00561  01  HEAD-STATE.                                                  ECS156
00562      12  FILLER                  PIC X(5)     VALUE '0'.          ECS156
00563      12  FILLER                  PIC X(12)                        ECS156
00564                             VALUE 'STATE   :'.                    ECS156
00565      12  HEAD-STATE-D            PIC XX.                          ECS156
00566                                                                   ECS156
00567  01  HEAD-ACCOUNT.                                                ECS156
00568      12  FILLER                  PIC X(5)     VALUE '0'.          ECS156
00569      12  FILLER                  PIC X(12)                        ECS156
00570                             VALUE 'ACCOUNT :'.                    ECS156
00571      12  HEAD-ACCOUNT-D          PIC X(10).                       ECS156
00572                                                                   ECS156
00573  01  HEAD-BUSINESS-TYPE.                                          ECS156
00574      12  FILLER                  PIC X(5)     VALUE '0'.          ECS156
00575      12  FILLER                  PIC X(12)                        ECS156
00576                             VALUE 'BUS TYPE:'.                    ECS156
00577      12  HEAD-BUSTP              PIC XX.                          ECS156
00578                                                                   ECS156
00579  01  HEAD-RATE-CLASS.                                             ECS156
00580      12  FILLER                  PIC X(5)     VALUE '0'.          ECS156
00581      12  FILLER                  PIC X(12)                        ECS156
00582                             VALUE 'BUS TYPE:'.                    ECS156
00583      12  HEAD-CLASS-D            PIC XX.                          ECS156
00584                                                                   ECS156
00585  01  HEAD-DEVIATION.                                              ECS156
00586      12  FILLER                  PIC X(5)     VALUE '0'.          ECS156
00587      12  FILLER                  PIC X(12)                        ECS156
00588                             VALUE 'DEVIATION :'.                  ECS156
00589      12  HEAD-DEVIATION-D        PIC XXX.                         ECS156
00590                                                                   ECS156
00591  01  WS-SAVE-PRINT-LINE          PIC X(132).                      ECS156
00592  01  WS-DESC-REPORT-A            PIC X(132)                       ECS156
00593                VALUE '                     REPORT A'.             ECS156
00594  01  WS-DESC-REPORT-B            PIC X(132)                       ECS156
00595                VALUE '                     REPORT B'.             ECS156
00596  01  WS-DESC-REPORT-C            PIC X(132)                       ECS156
00597                VALUE '                     REPORT C'.             ECS156
00598  01  WS-DESC-REPORT-D            PIC X(132)                       ECS156
00599                VALUE '                     REPORT D'.             ECS156
00600                                                                   ECS156
00601  01  HEAD-VARIANCE-D.                                             ECS156
00602      03  FILLER      PIC X(29)   VALUE SPACES.                    ECS156
00603      03  FILLER      PIC X(11)   VALUE 'DESCRIPTION'.             ECS156
00604      03  FILLER      PIC X(27)   VALUE SPACES.                    ECS156
00605      03  FILLER      PIC X(20)                                    ECS156
00606                           VALUE '  LIFE VARIANCE'.                ECS156
00607      03  FILLER      PIC XX      VALUE SPACES.                    ECS156
00608      03  FILLER      PIC X(25)                                    ECS156
00609                           VALUE 'AH VARIANCE'.                    ECS156
00610      03  FILLER      PIC X(8)    VALUE SPACES.                    ECS156
00611                                                                   ECS156
00612  01  WS-VARIANCE-PRINT-LINE.                                      ECS156
00613      12  FILLER                  PIC X(25)    VALUE '0'.          ECS156
00614      12  WS-PRINT-DESC           PIC X(25)    VALUE SPACES.       ECS156
00615      12  FILLER                  PIC X(5)     VALUE SPACES.       ECS156
00616      12  WS-PRINT-CURRENT-OPT    PIC X(10)    VALUE SPACES.       ECS156
00617      12  FILLER                  PIC X(4)     VALUE SPACES.       ECS156
00618      12  WS-PRINT-LIFE-VARIANCE  PIC ZZZZ,ZZ9.99-.                ECS156
00619      12  FILLER                  PIC X(5)     VALUE SPACES.       ECS156
00620      12  WS-PRINT-AH-VARIANCE    PIC ZZZZ,ZZ9.99-.                ECS156
00621                                                                   ECS156
00622  01  ERROR-MESSAGES.                                              ECS156
00623      03  ERR-E       PIC X(60)                                    ECS156
00624        VALUE 'CERT EXISTED LAST YEAR AND HAS BEEN CONVERTED.    '.ECS156
00625                                                                   ECS156
00626      03  ERR-C       PIC X(60)                                    ECS156
00627        VALUE 'CERT WAS CONVERTED WITHIN THE LAST YEAR.          '.ECS156
00628                                                                   ECS156
00629      EJECT                                                        ECS156
00630                                  COPY ELCDATE.                    ECS156
00631      EJECT                                                        ECS156
00632                                  COPY ELCDTECX.                   ECS156
00633      EJECT                                                        ECS156
00634                                  COPY ELCDTEVR.                   ECS156
00635      EJECT                                                        ECS156
00636                                                                   ECS156
00637  PROCEDURE DIVISION.                                              ECS156
00638                                                                   ECS156
00639  0000-LOAD-DATE.                                                  ECS156
00640                                  COPY ELCDTERX SUPPRESS.          ECS156
00641  0001-INITIALIZE  SECTION.                                        ECS156
00642                                                                   ECS156
00643      OPEN INPUT  CERT-PREV                                        ECS156
00644                  CERT-CURR                                        ECS156
00645           OUTPUT PRNTR                                            ECS156
00646                  WORK-FILE.                                       ECS156
00647      ACCEPT  LCP-DATE-NEW-74 FROM DATE                            ECS156
00648      MOVE CORRESPONDING LCP-DATE-NEW-74 TO LCP-CURRENT-DATE-68    ECS156
00649                                                                   ECS156
00650      MOVE WS-CURRENT-DATE        TO HD-RUN-DT.                       CL**2
00651                                                                   ECS156
00652      MOVE RUN-MO                 TO HC-MO                         ECS156
00653                                     WS-WORK-MO.                   ECS156
00654                                                                   ECS156
00655      MOVE RUN-DA                 TO HC-DA                         ECS156
00656                                     WS-WORK-DA.                   ECS156
00657                                                                   ECS156
00658      MOVE RUN-YR                 TO HC-YR.                           CL**2
00659      MOVE RUN-CCYY               TO WS-WORK-CCYY.                    CL**4
00660                                                                   ECS156
00661      COMPUTE WS-PREV-YEAR = WS-WORK-CCYY - 1.                        CL**2
00662                                                                   ECS156
00663      MOVE SPACE                   TO EOF-SW                       ECS156
00664                                      EOF-SW-2.                    ECS156
00665                                                                   ECS156
00666      MOVE COMPANY-NAME            TO HA-NAME.                     ECS156
00667      MOVE SPACES                  TO HEAD-HH.                     ECS156
00668                                                                   ECS156
00669      MOVE RUN-YR                  TO HEAD-HH-YR (3).              ECS156
00670      MOVE RUN-MO                  TO HEAD-HH-MO (3).              ECS156
00671      MOVE RUN-DA                  TO HEAD-HH-DA (3).              ECS156
00672      MOVE '/'                     TO HEAD-HH-SLASH-1 (3)          ECS156
00673                                      HEAD-HH-SLASH-2 (3).         ECS156
00674      MOVE ' THRU'                 TO HEAD-HH-THRU (3).            ECS156
00675      MOVE SPACES                  TO HEAD-H1.                     ECS156
00676      MOVE EP-MO                   TO HEAD-H-MO (3).               ECS156
00677      MOVE EP-DA                   TO HEAD-H-DA (3).               ECS156
00678      MOVE EP-YR                   TO HEAD-H-YR (3).               ECS156
00679      MOVE '/'                     TO HEAD-H-SLASH-1 (3)           ECS156
00680                                      HEAD-H-SLASH-2 (3).          ECS156
00681                                                                   ECS156
00682      MOVE RUN-MO                  TO HEAD-HH-MO (1)               ECS156
00683                                      HEAD-HH-MO (2)               ECS156
00684                                      HEAD-HH-MO (3).              ECS156
00685                                                                   ECS156
00686      MOVE RUN-DA                  TO HEAD-HH-DA (1)               ECS156
00687                                      HEAD-HH-DA (2)               ECS156
00688                                      HEAD-HH-DA (3).              ECS156
00689                                                                   ECS156
00690      MOVE RUN-YR                  TO HEAD-HH-YR (3).              ECS156
00691                                                                   ECS156
00692      COMPUTE HEAD-HH-YR (2) EQUAL (RUN-YR - 1).                   ECS156
00693                                                                      CL**2
00694      IF HEAD-HH-YR (2) < 0                                           CL**2
00695          ADD 100 TO HEAD-HH-YR (2).                                  CL**2
00696                                                                      CL**2
00697      COMPUTE HEAD-HH-YR (1) EQUAL (RUN-YR - 2).                   ECS156
00698                                                                   ECS156
00699      IF HEAD-HH-YR (1) < 0                                           CL**2
00700          ADD 100 TO HEAD-HH-YR (1).                                  CL**2
00701                                                                      CL**2
00702      MOVE '/'                     TO HEAD-HH-SLASH-1 (1)          ECS156
00703                                      HEAD-HH-SLASH-1 (2)          ECS156
00704                                      HEAD-HH-SLASH-1 (3)          ECS156
00705                                      HEAD-HH-SLASH-2 (1)          ECS156
00706                                      HEAD-HH-SLASH-2 (2)          ECS156
00707                                      HEAD-HH-SLASH-2 (3).         ECS156
00708                                                                   ECS156
00709      MOVE ' THRU'                 TO HEAD-HH-THRU (1)             ECS156
00710                                      HEAD-HH-THRU (2)             ECS156
00711                                      HEAD-HH-THRU (3).            ECS156
00712                                                                   ECS156
00713      MOVE SPACES                  TO HEAD-H1.                     ECS156
00714                                                                   ECS156
00715      MOVE ' T O T A L '           TO HH-NAMES (4).                ECS156
00716                                                                   ECS156
00717      MOVE EP-MO                   TO HEAD-H-MO (1)                ECS156
00718                                      HEAD-H-MO (2)                ECS156
00719                                      HEAD-H-MO (3).               ECS156
00720                                                                   ECS156
00721      MOVE EP-DA                   TO HEAD-H-DA (1)                ECS156
00722                                      HEAD-H-DA (2)                ECS156
00723                                      HEAD-H-DA (3).               ECS156
00724                                                                   ECS156
00725      MOVE EP-YR                   TO HEAD-H-YR (3).               ECS156
00726                                                                   ECS156
00727      COMPUTE HEAD-H-YR (2)  EQUAL (EP-YR - 1).                    ECS156
00728      COMPUTE HEAD-H-YR (1)  EQUAL (EP-YR - 2).                    ECS156
00729                                                                   ECS156
00730      MOVE '/'                     TO HEAD-H-SLASH-1 (1)           ECS156
00731                                      HEAD-H-SLASH-1 (2)           ECS156
00732                                      HEAD-H-SLASH-1 (3)           ECS156
00733                                      HEAD-H-SLASH-2 (1)           ECS156
00734                                      HEAD-H-SLASH-2 (2)           ECS156
00735                                      HEAD-H-SLASH-2 (3).          ECS156
00736                                                                   ECS156
00737      MOVE ALL '9'                 TO WS-SAVE-FIELDS.              ECS156
00738                                                                   ECS156
00739 **NOTE: DEFAULT OPTION IS '1'                                     ECS156
00740                                                                   ECS156
00741      IF (DTE-TOT-OPT EQUAL '6')                                   ECS156
00742            AND (DTE-FMT-OPT NOT EQUAL '2')                        ECS156
00743         MOVE '2'                   TO DTE-FMT-OPT.                ECS156
00744                                                                   ECS156
00745      PERFORM  0100-COMPARE-CONTROL THRU 0199-CONTROL-EXIT.        ECS156
00746                                                                   ECS156
00747      IF DTE-FMT-OPT EQUAL '2'                                     ECS156
00748          MOVE LIFE-TOTAL-VARIANCE TO WS-TOTAL-LIFE-VARIANCE       ECS156
00749          MOVE AH-TOTAL-VARIANCE   TO WS-TOTAL-AH-VARIANCE         ECS156
00750          COMPUTE WS-TOTAL-VARIANCE =                              ECS156
00751                LIFE-TOTAL-VARIANCE + AH-TOTAL-VARIANCE            ECS156
00752          MOVE 'E'                 TO PRINT-DATA                   ECS156
00753          PERFORM  0500-PRINT-LINE THRU 0500-EXIT.                 ECS156
00754                                                                   ECS156
00755      DISPLAY ' '.                                                 ECS156
00756      DISPLAY 'END OF EXTRACT '.                                   ECS156
00757      DISPLAY ' RECORDS WRITTEN  ' RECORDS-WRITTEN.                ECS156
00758                                                                   ECS156
00759      CLOSE WORK-FILE.                                             ECS156
00760                                                                   ECS156
00761      IF DTE-TOT-OPT EQUAL '6'                                     ECS156
00762         DISPLAY 'NO TOTALS REQUESTED '                            ECS156
00763         GO TO 0001-END-OF-JOB.                                    ECS156
00764                                                                   ECS156
00765      IF (DTE-TOT-OPT EQUAL '1' OR  '2' OR  '3' OR  '4'            ECS156
00766                                    OR  '5' OR '6')                ECS156
00767         NEXT SENTENCE                                             ECS156
00768      ELSE                                                         ECS156
00769         MOVE '1'                  TO DTE-TOT-OPT.                 ECS156
00770                                                                   ECS156
00771      IF DTE-TOT-OPT EQUAL '1'                                     ECS156
00772         MOVE 'A'                  TO HEAD-A-TYPE                  ECS156
00773         SORT SORT-FILE ON ASCENDING  SW-CARRIER                   ECS156
00774                                      SW-STATE                     ECS156
00775                                      SW-RATE-CLASS                ECS156
00776                                      SW-COVERAGE                  ECS156
00777            INPUT PROCEDURE 0700-SORT-RECS THRU                    ECS156
00778                            0700-SORT-EXIT                         ECS156
00779            OUTPUT PROCEDURE 1000-PRINT-REPORT THRU                ECS156
00780                             1099-END-OUTPUT.                      ECS156
00781                                                                   ECS156
00782      IF DTE-TOT-OPT EQUAL '2'                                     ECS156
00783         MOVE 'B'                  TO HEAD-A-TYPE                  ECS156
00784         SORT SORT-FILE ON ASCENDING  SW-CARRIER                   ECS156
00785                                      SW-STATE                     ECS156
00786                                      SW-RATE-DEVIATION            ECS156
00787                                      SW-COVERAGE                  ECS156
00788           INPUT PROCEDURE 0700-SORT-RECS THRU                     ECS156
00789                            0700-SORT-EXIT                         ECS156
00790           OUTPUT PROCEDURE 1000-PRINT-REPORT THRU                 ECS156
00791                             1099-END-OUTPUT.                      ECS156
00792                                                                   ECS156
00793      IF DTE-TOT-OPT EQUAL '3'                                     ECS156
00794         MOVE 'C'                  TO HEAD-A-TYPE                  ECS156
00795         SORT SORT-FILE ON ASCENDING  SW-CARRIER                   ECS156
00796                                      SW-STATE                     ECS156
00797                                      SW-ACCOUNT                   ECS156
00798                                      SW-RATE-DEVIATION            ECS156
00799                                      SW-COVERAGE                  ECS156
00800           INPUT PROCEDURE 0700-SORT-RECS THRU                     ECS156
00801                            0700-SORT-EXIT                         ECS156
00802           OUTPUT PROCEDURE 1000-PRINT-REPORT THRU                 ECS156
00803                             1099-END-OUTPUT.                      ECS156
00804                                                                   ECS156
00805      IF DTE-TOT-OPT EQUAL '4'                                     ECS156
00806         MOVE 'D'                  TO HEAD-A-TYPE                  ECS156
00807         SORT SORT-FILE ON ASCENDING  SW-CARRIER                   ECS156
00808                                      SW-STATE                     ECS156
00809                                      SW-RATE-DEVIATION            ECS156
00810                                      SW-BUSINESS-TYPE             ECS156
00811                                      SW-COVERAGE                  ECS156
00812           INPUT PROCEDURE 0700-SORT-RECS THRU                     ECS156
00813                           0700-SORT-EXIT                          ECS156
00814           OUTPUT PROCEDURE 1000-PRINT-REPORT THRU                 ECS156
00815                            1099-END-OUTPUT.                       ECS156
00816                                                                   ECS156
00817      IF DTE-TOT-OPT EQUAL '5'                                     ECS156
00818         MOVE ZEROS                TO LIFE-TOTAL-VARIANCE          ECS156
00819                                      AH-TOTAL-VARIANCE            ECS156
00820         MOVE ALL '9'              TO WS-SAVE-FIELDS               ECS156
00821         MOVE '1'                  TO DTE-TOT-OPT                  ECS156
00822         MOVE 'A'                  TO HEAD-A-TYPE                  ECS156
00823         MOVE +80                  TO LINER                        ECS156
00824         SORT SORT-FILE ON ASCENDING  SW-CARRIER                   ECS156
00825                                      SW-STATE                     ECS156
00826                                      SW-RATE-CLASS                ECS156
00827                                      SW-COVERAGE                  ECS156
00828            INPUT PROCEDURE 0700-SORT-RECS THRU                    ECS156
00829                            0700-SORT-EXIT                         ECS156
00830            OUTPUT PROCEDURE 1000-PRINT-REPORT THRU                ECS156
00831                             1099-END-OUTPUT                       ECS156
00832         MOVE ZEROS                TO LIFE-TOTAL-VARIANCE          ECS156
00833                                      AH-TOTAL-VARIANCE            ECS156
00834         MOVE ALL '9'              TO WS-SAVE-FIELDS               ECS156
00835         MOVE '2'                  TO DTE-TOT-OPT                  ECS156
00836         MOVE 'B'                  TO HEAD-A-TYPE                  ECS156
00837         MOVE +80                  TO LINER                        ECS156
00838         MOVE SPACES               TO WS-SAVE-PRINT-LINE              CL**2
00839         SORT SORT-FILE ON ASCENDING  SW-CARRIER                   ECS156
00840                                      SW-STATE                     ECS156
00841                                      SW-RATE-DEVIATION            ECS156
00842                                      SW-COVERAGE                  ECS156
00843           INPUT PROCEDURE 0700-SORT-RECS THRU                     ECS156
00844                            0700-SORT-EXIT                         ECS156
00845           OUTPUT PROCEDURE 1000-PRINT-REPORT THRU                 ECS156
00846                             1099-END-OUTPUT                       ECS156
00847         MOVE ZEROS                TO LIFE-TOTAL-VARIANCE          ECS156
00848                                      AH-TOTAL-VARIANCE            ECS156
00849         MOVE ALL '9'              TO WS-SAVE-FIELDS               ECS156
00850         MOVE '3'                  TO DTE-TOT-OPT                  ECS156
00851         MOVE 'C'                  TO HEAD-A-TYPE                  ECS156
00852         MOVE +80                  TO LINER                        ECS156
00853         MOVE SPACES               TO WS-SAVE-PRINT-LINE              CL**2
00854         SORT SORT-FILE ON ASCENDING  SW-CARRIER                   ECS156
00855                                      SW-STATE                     ECS156
00856                                      SW-ACCOUNT                   ECS156
00857                                      SW-RATE-DEVIATION            ECS156
00858                                      SW-COVERAGE                  ECS156
00859           INPUT PROCEDURE 0700-SORT-RECS THRU                     ECS156
00860                            0700-SORT-EXIT                         ECS156
00861           OUTPUT PROCEDURE 1000-PRINT-REPORT THRU                 ECS156
00862                             1099-END-OUTPUT                       ECS156
00863         MOVE ZEROS                TO LIFE-TOTAL-VARIANCE          ECS156
00864                                      AH-TOTAL-VARIANCE            ECS156
00865         MOVE ALL '9'              TO WS-SAVE-FIELDS               ECS156
00866         MOVE 'D'                  TO HEAD-A-TYPE                  ECS156
00867         MOVE '4'                  TO DTE-TOT-OPT                  ECS156
00868         MOVE +80                  TO LINER                        ECS156
00869         SORT SORT-FILE ON ASCENDING  SW-CARRIER                   ECS156
00870                                      SW-STATE                     ECS156
00871                                      SW-RATE-DEVIATION            ECS156
00872                                      SW-BUSINESS-TYPE             ECS156
00873                                      SW-COVERAGE                  ECS156
00874           INPUT PROCEDURE 0700-SORT-RECS THRU                     ECS156
00875                           0700-SORT-EXIT                          ECS156
00876           OUTPUT PROCEDURE 1000-PRINT-REPORT THRU                 ECS156
00877                            1099-END-OUTPUT.                       ECS156
00878                                                                   ECS156
00879      IF SORT-RETURN NOT EQUAL ZEROS                               ECS156
00880          MOVE +101       TO WS-RETURN-CODE                        ECS156
00881          MOVE 'ERROR ON SORT ' TO WS-ABEND-MESSAGE                ECS156
00882          MOVE SPACES           TO WS-ABEND-FILE-STATUS            ECS156
00883          GO TO ABEND-PGM.                                         ECS156
00884                                                                   ECS156
00885  0001-END-OF-JOB.                                                 ECS156
00886      DISPLAY '     '.                                             ECS156
00887      DISPLAY '     '.                                             ECS156
00888      DISPLAY '     '.                                             ECS156
00889      DISPLAY RECORDS-RELEASED ' = RECORDS RELEASED     '.         ECS156
00890      DISPLAY RECORDS-RECEIVED ' = RECORDS RECEIVED     '.         ECS156
00891      DISPLAY CERT-PREV-RECS   ' = PREV INPUT RECORDS   '.         ECS156
00892      DISPLAY CERT-CURR-RECS   ' = CURR INPUT RECORDS   '.         ECS156
00893                                                                   ECS156
00894      CLOSE   CERT-PREV                                            ECS156
00895              CERT-CURR                                            ECS156
00896              PRNTR.                                               ECS156
00897      MOVE ZEROS  TO RETURN-CODE.
00897      GOBACK.                                                      ECS156
00898                                                                   ECS156
00899      EJECT                                                        ECS156
00900                                                                   ECS156
00901  0100-COMPARE-CONTROL SECTION.                                    ECS156
00902                                                                   ECS156
00903      PERFORM 0300-READ-PREV-CERT   THRU 0300-READ-PREV-EXIT.      ECS156
00904      PERFORM 0310-READ-CURR-CERT   THRU 0310-READ-CURR-EXIT.      ECS156
00905                                                                   ECS156
00906  0110-COMPARE-READLOOP.                                           ECS156
00907                                                                   ECS156
00908      IF END-OF-PREV-FILE AND NOT-OF-CURR-FILE                     ECS156
00909         DISPLAY 'END OF PREV '                                    ECS156
00910           'NOT END OF CURR FILE ' CO-FULL-CONTROL                 ECS156
00911         GO TO 0199-CONTROL-EXIT.                                  ECS156
00912                                                                   ECS156
00913      IF END-OF-CURR-FILE AND NOT-OF-PREV-FILE                     ECS156
00914         DISPLAY 'END OF CURR '                                    ECS156
00915           'NOT END OF PREV FILE ' CR-FULL-CONTROL                 ECS156
00916         GO TO 0199-CONTROL-EXIT.                                  ECS156
00917                                                                   ECS156
00918      IF END-OF-CURR-FILE AND END-OF-PREV-FILE                     ECS156
00919         DISPLAY 'END OF LOGIC '                                   ECS156
00920         GO TO 0199-CONTROL-EXIT.                                  ECS156
00921                                                                   ECS156
00922      IF CR-FULL-CONTROL EQUAL CO-FULL-CONTROL                     ECS156
00923          PERFORM 0200-CHECK-AMOUNTS  THRU 0200-CHECK-AMOUNTS-EXIT ECS156
00924          PERFORM 0300-READ-PREV-CERT THRU 0300-READ-PREV-EXIT     ECS156
00925          PERFORM 0310-READ-CURR-CERT THRU 0310-READ-CURR-EXIT     ECS156
00926          GO TO 0110-COMPARE-READLOOP.                             ECS156
00927                                                                   ECS156
00928      IF CR-FULL-CONTROL LESS THAN CO-FULL-CONTROL                 ECS156
00929 *        PERFORM 0210-CHECK-AMOUNTS  THRU 0210-CHECK-AMOUNTS-EXIT ECS156
00930 *        PERFORM 0230-CHECK-ERRORS   THRU 0230-CHECK-ERRORS-EXIT  ECS156
00931          PERFORM 0300-READ-PREV-CERT THRU 0300-READ-PREV-EXIT     ECS156
00932          GO TO 0110-COMPARE-READLOOP.                             ECS156
00933                                                                   ECS156
00934      IF CR-FULL-CONTROL GREATER THAN  CO-FULL-CONTROL             ECS156
00935 *        PERFORM 0220-CHECK-AMOUNTS  THRU 0220-CHECK-AMOUNTS-EXIT ECS156
00936 *        PERFORM 0230-CHECK-ERRORS   THRU 0230-CHECK-ERRORS-EXIT  ECS156
00937          PERFORM 0310-READ-CURR-CERT THRU 0310-READ-CURR-EXIT     ECS156
00938            GO TO 0110-COMPARE-READLOOP.                           ECS156
00939                                                                   ECS156
00940      DISPLAY 'FALL THRU ' CO-FULL-CONTROL ' ' CO-FULL-CONTROL.    ECS156
00941      GO TO 0110-COMPARE-READLOOP.                                 ECS156
00942                                                                   ECS156
00943  0199-CONTROL-EXIT.                                               ECS156
00944      EXIT.                                                        ECS156
00945  EJECT                                                            ECS156
00946 ************************************************************      ECS156
00947 * THE CERTIFICATE IS FOUND ON THE CURRENT AND PREVIOUS     *      ECS156
00948 * YEAR END CERTIFICATE FILE. A CHECK IS MADE TO SEE IF     *      ECS156
00949 * THE CERTIFICATE WAS CANCELLED IN THE PREVIOUS YEAR AND   *      ECS156
00950 * IF THE REFUND AMOUNTS ARE STILL THE SAME AMOUNT.         *      ECS156
00951 ************************************************************      ECS156
00952  0200-CHECK-AMOUNTS.                                              ECS156
00953      MOVE SPACE                TO PRINT-DATA                      ECS156
00954                                   WRITE-FILE                      ECS156
00955                                   WORK-RECORD                     ECS156
00956                                   WS-DETAIL-PRINT-ERROR.          ECS156
00957                                                                   ECS156
00958      MOVE ZEROS                TO WK-VARIANCE.                    ECS156
00959                                                                   ECS156
00960      IF CR-LF-CANCEL-EXIT-DATE NOT NUMERIC                        ECS156
00961          MOVE ZEROS            TO CR-LF-CANCEL-EXIT-DATE.         ECS156
00962                                                                   ECS156
00963      IF CO-LF-CANCEL-EXIT-DATE NOT NUMERIC                        ECS156
00964          MOVE ZEROS            TO CO-LF-CANCEL-EXIT-DATE.         ECS156
00965                                                                   ECS156
00966      IF CR-AH-CANCEL-EXIT-DATE NOT NUMERIC                        ECS156
00967          MOVE ZEROS            TO CR-AH-CANCEL-EXIT-DATE.         ECS156
00968                                                                   ECS156
00969      IF CO-AH-CANCEL-EXIT-DATE NOT NUMERIC                        ECS156
00970          MOVE ZEROS            TO CO-AH-CANCEL-EXIT-DATE.         ECS156
00971                                                                   ECS156
00972      IF CR-AHRFND NOT NUMERIC                                     ECS156
00973          MOVE ZEROS            TO CR-AHRFND.                      ECS156
00974                                                                   ECS156
00975      IF CR-LFRFND NOT NUMERIC                                     ECS156
00976          MOVE ZEROS            TO CR-LFRFND.                      ECS156
00977                                                                   ECS156
00978      IF CO-AHRFND NOT NUMERIC                                     ECS156
00979          MOVE ZEROS            TO CO-AHRFND.                      ECS156
00980                                                                   ECS156
00981      IF CO-LFRFND NOT NUMERIC                                     ECS156
00982          MOVE ZEROS            TO CO-LFRFND.                      ECS156
00983                                                                   ECS156
00984      IF (CR-LF-CANCEL-EXIT-DATE EQUAL ZEROS                       ECS156
00985           AND CR-AH-CANCEL-EXIT-DATE EQUAL ZEROS)                 ECS156
00986          GO TO 0200-CHECK-AMOUNTS-EXIT.                           ECS156
00987                                                                   ECS156
00988      IF ((CR-LFTYP EQUAL '00' OR '  ') OR                         ECS156
00989          CR-LF-CANCEL-EXIT-DATE EQUAL ZEROS)                      ECS156
00990          GO TO 0200-CHECK-AH-AMOUNTS.                             ECS156
00991                                                                   ECS156
00992      IF CR-LFRFND EQUAL CO-LFRFND                                 ECS156
00993          GO TO 0200-CHECK-AH-AMOUNTS.                             ECS156
00994                                                                   ECS156
00995      MOVE CR-LF-CANCEL-EXIT-DATE   TO WS-PREV-CNC-DT.                CL*18
00996      MOVE CO-LF-CANCEL-EXIT-DATE   TO WS-CURR-CNC-DT.             ECS156
00997                                                                   ECS156
00998      IF WS-PREV-CNC-DT = WS-CURR-CNC-DT                              CL**3
00999        OR ((WS-PREV-CNC-CCYY LESS THAN WS-WORK-CCYY) AND             CL**3
01000           (WS-CURR-CNC-CCYY LESS THAN WS-WORK-CCYY))                 CL**3
01001          COMPUTE WK-VARIANCE = CR-LFRFND - CO-LFRFND              ECS156
01002          MOVE 'Y'                TO WRITE-FILE                       CL**3
01003          IF WS-CURR-CNC-YR = ZEROS                                   CL*12
01004              MOVE WS-MESSAGE-3   TO WS-DETAIL-PRINT-ERROR.           CL**3
01005                                                                   ECS156
CIDMOD     IF (WS-PREV-CNC-DT = ZEROS) AND
CIDMOD        (WS-CURR-CNC-DT NOT = ZEROS)
CIDMOD        MOVE ' LIFE COV PREVIOUSLY CANCELED ' TO
CIDMOD                    WS-DETAIL-PRINT-ERROR
CIDMOD        MOVE CR-LFRFND        TO WK-VARIANCE
CIDMOD        MOVE 'Y'              TO WRITE-FILE
CIDMOD     END-IF
CIDMOD
01006      IF WS-CURR-CNC-YR = WS-WORK-YR                                  CL**3
01007          IF CR-LFRFND = ZEROS                                        CL**3
01008              MOVE ZEROS          TO WK-VARIANCE                      CL**3
01009              MOVE WS-MESSAGE-1   TO WS-DETAIL-PRINT-ERROR            CL**3
01010          ELSE                                                     ECS156
01011              MOVE 'Y'            TO WRITE-FILE                       CL**3
01012              MOVE CR-LFRFND      TO WK-VARIANCE                      CL**3
01013              MOVE WS-MESSAGE-3   TO WS-DETAIL-PRINT-ERROR.           CL**3
01014                                                                   ECS156
01015      MOVE WS-PREV-CARRIER        TO WK-CARRIER.                   ECS156
01016      MOVE WS-PREV-GROUPING       TO WK-GROUPING.                  ECS156
01017      MOVE WS-PREV-STATE          TO WK-STATE.                     ECS156
01018      MOVE WS-PREV-ACCOUNT        TO WK-ACCOUNT.                   ECS156
01019      MOVE 'LF'                   TO WK-COVERAGE.                  ECS156
01020      MOVE CR-RATING-CLASS        TO WK-RATE-CLASS.                ECS156
01021      MOVE CR-LF-DEV-CODE         TO WK-RATE-DEVIATION.            ECS156
01022      MOVE CR-GRPTYP              TO WK-BUSINESS-TYPE.             ECS156
01023      MOVE CR-CERT-NO             TO WK-CERT-NO.                   ECS156
01024                                                                   ECS156
01025      IF DTE-FMT-OPT EQUAL '2'                                     ECS156
01026          MOVE WK-VARIANCE         TO WS-PRINT-VARIANCE            ECS156
01027          ADD  WK-VARIANCE         TO LIFE-TOTAL-VARIANCE          ECS156
01028 *        MOVE CR-LF-CANCEL-EXIT-DATE                                 CL*18
01029 *                                 TO WS-PREV-CNC-DT                  CL*18
01030 *        MOVE CO-LF-CANCEL-EXIT-DATE                                 CL*18
01031 *                                  TO WS-CURR-CNC-DT                 CL*18
01032          MOVE 'LF '                TO WS-PRINT-TYPE               ECS156
01033          MOVE CR-LFRFND            TO WS-PRINT-CANC-AMT-1         ECS156
01034                                                                   ECS156
01035          MOVE CO-LFRFND            TO WS-PRINT-CANC-AMT-2         ECS156
01036          PERFORM  0500-PRINT-LINE  THRU 0500-EXIT                 ECS156
01037          MOVE 'Y'                  TO PRINT-DATA.                 ECS156
01038                                                                   ECS156
01039      IF WRITE-FILE EQUAL 'Y'                                      ECS156
01040          PERFORM 0600-WRITE-WORK-FILE THRU 0600-EXIT.             ECS156
01041                                                                   ECS156
01042  0200-CHECK-AH-AMOUNTS.                                           ECS156
01043                                                                   ECS156
01044      IF PRINT-DATA EQUAL 'Y'                                      ECS156
01045          MOVE SPACES           TO WS-DETAIL-PRINT-LINE.           ECS156
01046                                                                   ECS156
01047      MOVE SPACES               TO WORK-RECORD                     ECS156
01048                                   WRITE-FILE.                     ECS156
01049                                                                   ECS156
01050      IF ((CR-AHTYP EQUAL '00' OR '  ') OR                         ECS156
01051          CR-AH-CANCEL-EXIT-DATE EQUAL ZEROS)                      ECS156
01052          GO TO 0200-CHECK-AMOUNTS-A.                              ECS156
01053                                                                   ECS156
01054      IF CR-AHRFND EQUAL CO-AHRFND                                 ECS156
01055          GO TO 0200-CHECK-AMOUNTS-A.                              ECS156
01056                                                                   ECS156
01057      MOVE ZEROS                TO WS-PRINT-CANC-AMT-1             ECS156
01058                                   WS-PRINT-CANC-AMT-2             ECS156
01059                                   WS-PRINT-VARIANCE               ECS156
01060                                   WK-VARIANCE.                    ECS156
01061                                                                   ECS156
01062      MOVE CR-AH-CANCEL-EXIT-DATE   TO WS-PREV-CNC-DT.             ECS156
01063      MOVE CO-AH-CANCEL-EXIT-DATE   TO WS-CURR-CNC-DT.             ECS156
01064                                                                   ECS156
01065      IF WS-PREV-CNC-DT = WS-CURR-CNC-DT                              CL**3
01066        OR ((WS-PREV-CNC-CCYY LESS THAN WS-WORK-CCYY) AND             CL**3
01067           (WS-CURR-CNC-CCYY LESS THAN WS-WORK-CCYY))                 CL**3
01068          COMPUTE WK-VARIANCE = CR-AHRFND - CO-AHRFND              ECS156
01069          MOVE 'Y'                  TO WRITE-FILE                  ECS156
01070          IF WS-CURR-CNC-YR = ZEROS                                   CL**3
01071              MOVE WS-MESSAGE-3     TO WS-DETAIL-PRINT-ERROR.         CL**3
01072                                                                   ECS156
01073      IF WS-CURR-CNC-YR EQUAL WS-WORK-YR                           ECS156
01074          IF CR-AHRFND EQUAL ZEROS                                 ECS156
01075              MOVE ZEROS                TO WK-VARIANCE             ECS156
01076              MOVE WS-MESSAGE-1         TO WS-DETAIL-PRINT-ERROR   ECS156
01077          ELSE                                                     ECS156
01078              MOVE 'Y'                  TO WRITE-FILE              ECS156
01079              MOVE CR-AHRFND            TO WK-VARIANCE             ECS156
01080              MOVE WS-MESSAGE-3         TO WS-DETAIL-PRINT-ERROR.  ECS156
01081                                                                   ECS156
01082      MOVE WS-PREV-CARRIER        TO WK-CARRIER.                   ECS156
01083      MOVE WS-PREV-GROUPING       TO WK-GROUPING.                  ECS156
01084      MOVE WS-PREV-STATE          TO WK-STATE.                     ECS156
01085      MOVE WS-PREV-ACCOUNT        TO WK-ACCOUNT.                   ECS156
01086      MOVE 'AH'                   TO WK-COVERAGE.                  ECS156
01087      MOVE CR-RATING-CLASS        TO WK-RATE-CLASS.                ECS156
01088      MOVE CR-AH-DEV-CODE         TO WK-RATE-DEVIATION.            ECS156
01089      MOVE CR-GRPTYP              TO WK-BUSINESS-TYPE.             ECS156
01090      MOVE CR-CERT-NO             TO WK-CERT-NO.                   ECS156
01091                                                                   ECS156
01092      IF DTE-FMT-OPT EQUAL '2'                                     ECS156
01093          MOVE WK-VARIANCE          TO WS-PRINT-VARIANCE           ECS156
01094          ADD  WK-VARIANCE          TO AH-TOTAL-VARIANCE           ECS156
01095 *        MOVE CR-AH-CANCEL-EXIT-DATE                                 CL*18
01096 *                                  TO WS-PREV-CNC-DT                 CL*18
01097 *        MOVE CO-AH-CANCEL-EXIT-DATE                                 CL*18
01098 *                                  TO WS-CURR-CNC-DT                 CL*18
01099          MOVE 'AH '                TO WS-PRINT-TYPE               ECS156
01100          MOVE CR-AHRFND            TO WS-PRINT-CANC-AMT-1         ECS156
01101                                                                   ECS156
01102          MOVE CO-AHRFND            TO WS-PRINT-CANC-AMT-2         ECS156
01103          PERFORM  0500-PRINT-LINE  THRU 0500-EXIT                 ECS156
01104          MOVE 'Y'                  TO PRINT-DATA.                 ECS156
01105                                                                   ECS156
01106      IF WRITE-FILE EQUAL 'Y'                                      ECS156
01107          PERFORM 0600-WRITE-WORK-FILE THRU 0600-EXIT.             ECS156
01108                                                                   ECS156
01109  0200-CHECK-AMOUNTS-A.                                            ECS156
01110                                                                   ECS156
01111      IF WS-DETAIL-PRINT-ERROR NOT EQUAL SPACES                    ECS156
01112          MOVE 'Z'                  TO PRINT-DATA                  ECS156
01113          PERFORM  0500-PRINT-LINE  THRU 0500-EXIT.                ECS156
01114                                                                   ECS156
01115  0200-CHECK-AMOUNTS-EXIT.                                         ECS156
01116      EXIT.                                                           CL**3
01117  EJECT                                                               CL**3
01118 ************************************************************      ECS156
01119 * THE CERTIFICATE  EXISTED ON LAST YEAR'S CERTIFICATE FILE *      ECS156
01120 * BUT IS NOT FOUND ON THE CURRENT CERTIFICATE MASTER.  AN  *      ECS156
01121 * ASSUMPTION IS MADE THAT A CONVERSION OF SOME KIND        *      ECS156
01122 * TRANSPIRED DURING THE PAST YEAR AND THE CERTIFICATE      *      ECS156
01123 * WILL BE FOUND ON THE CURRENT FILE UNDER A NEW CONTROL.   *      ECS156
01124 * A CHECK WILL BE MADE TO SEE IF THE CERFTIFICATE WAS      *      ECS156
01125 * CANCELLED IN THE PREVIOUS YEAR AND ANNOTATES THAT THERE  *      ECS156
01126 * COULD BE A SWING FROM ONE CARRIER OR ACCOUNT TO ANOTHER. *      ECS156
01127 ************************************************************      ECS156
01128  0210-CHECK-AMOUNTS.                                              ECS156
01129      MOVE SPACE                TO PRINT-DATA.                     ECS156
01130      MOVE SPACES               TO WS-DETAIL-PRINT-ERROR.          ECS156
01131                                                                   ECS156
01132      IF (CR-LF-CANC-DT EQUAL ZEROS                                ECS156
01133           AND CR-AH-CANC-DT EQUAL ZEROS)                          ECS156
01134          GO TO 0210-CHECK-AMOUNTS-EXIT.                           ECS156
01135                                                                   ECS156
01136      IF ((CR-LFTYP EQUAL '00' OR '  ') OR                         ECS156
01137          CR-LF-CANC-DT EQUAL ZEROS)                               ECS156
01138          GO TO 0210-CHECK-AH-AMOUNTS.                             ECS156
01139                                                                   ECS156
01140      IF CR-LFRFND EQUAL ZEROS                                     ECS156
01141          GO TO 0210-CHECK-AH-AMOUNTS.                             ECS156
01142                                                                   ECS156
01143      MOVE CR-LF-CANC-DT        TO WS-PREV-CNC-DT.                 ECS156
01144      MOVE ZEROS                TO WS-CURR-CNC-DT.                 ECS156
01145                                                                   ECS156
01146      IF WS-PREV-CNC-CCYY GREATER THAN WS-PREV-YEAR                   CL**5
01147          GO TO 0210-CHECK-AH-AMOUNTS.                             ECS156
01148                                                                   ECS156
01149      MOVE 'LF '                TO WS-PRINT-TYPE.                  ECS156
01150      MOVE CR-LFRFND            TO WS-PRINT-CANC-AMT-1.            ECS156
01151                                                                   ECS156
01152      MOVE ERR-E                TO WS-PRINT-MESSAGE.               ECS156
01153      PERFORM  0500-PRINT-LINE  THRU 0500-EXIT.                    ECS156
01154      MOVE 'Y'                  TO PRINT-DATA.                     ECS156
01155                                                                   ECS156
01156  0210-CHECK-AH-AMOUNTS.                                           ECS156
01157                                                                   ECS156
01158      IF PRINT-DATA EQUAL 'Y'                                      ECS156
01159          MOVE SPACES           TO WS-DETAIL-PRINT-LINE.           ECS156
01160                                                                   ECS156
01161      IF ((CR-AHTYP EQUAL '00' OR '  ') OR                         ECS156
01162          CR-AH-CANC-DT EQUAL ZEROS)                               ECS156
01163          GO TO 0210-CHECK-AMOUNTS-EXIT.                           ECS156
01164                                                                   ECS156
01165      IF CR-AHRFND EQUAL CO-AHRFND                                 ECS156
01166          GO TO 0210-CHECK-AMOUNTS-EXIT.                           ECS156
01167                                                                   ECS156
01168      MOVE CR-AH-CANC-DT        TO WS-PREV-CNC-DT.                 ECS156
01169      MOVE ZEROS                TO WS-CURR-CNC-DT.                 ECS156
01170                                                                   ECS156
01171      IF WS-PREV-CNC-CCYY GREATER THAN WS-PREV-YEAR                   CL**5
01172          GO TO 0210-CHECK-AMOUNTS-EXIT.                           ECS156
01173                                                                   ECS156
01174      MOVE 'AH '                TO WS-PRINT-TYPE.                  ECS156
01175      MOVE CR-AHRFND            TO WS-PRINT-CANC-AMT-1.            ECS156
01176                                                                   ECS156
01177      MOVE ERR-C                TO WS-PRINT-MESSAGE.               ECS156
01178      PERFORM  0500-PRINT-LINE  THRU 0500-EXIT.                    ECS156
01179      MOVE 'Y'                  TO PRINT-DATA.                     ECS156
01180                                                                   ECS156
01181  0210-CHECK-AMOUNTS-EXIT.                                         ECS156
01182      EXIT.                                                           CL**3
01183  EJECT                                                               CL**3
01184 ************************************************************      ECS156
01185 * THE CERTIFICATE DID NOT EXIST ON LAST YEAR'S CERTIFICATE *      ECS156
01186 * MASTER BUT IS FOUND ON THE CURRENT CERTIFICATE MASTER.   *      ECS156
01187 * AN ASSUMPTION IS MADE THAT A CONVERSION OF SOME KIND     *      ECS156
01188 * TRANSPIRED DURING THE PAST YEAR AND THE CERTIFICATE      *      ECS156
01189 * WAS MODIFIED  EXISTING ON THE CURRENT FILE UNDER A NEW   *      ECS156
01190 * CONTROL.                                                 *      ECS156
01191 * A CHECK WILL BE MADE TO SEE IF THE CERFTIFICATE WAS      *      ECS156
01192 * CANCELLED IN THE PREVIOUS YEAR AND ANNOTATES THAT THERE  *      ECS156
01193 * COULD BE A SWING FROM ONE CARRIER OR ACCOUNT TO ANOTHER. *      ECS156
01194 ************************************************************      ECS156
01195  0220-CHECK-AMOUNTS.                                              ECS156
01196      MOVE SPACE                TO PRINT-DATA.                     ECS156
01197      MOVE SPACES               TO WS-DETAIL-PRINT-ERROR.          ECS156
01198                                                                   ECS156
01199      IF (CO-LF-CANC-DT EQUAL ZEROS                                ECS156
01200           AND CO-AH-CANC-DT EQUAL ZEROS)                          ECS156
01201          GO TO 0220-CHECK-AMOUNTS-EXIT.                           ECS156
01202                                                                   ECS156
01203      IF ((CO-LFTYP EQUAL '00' OR '  ') OR                         ECS156
01204          CO-LF-CANC-DT EQUAL ZEROS)                               ECS156
01205          GO TO 0220-CHECK-AH-AMOUNTS.                             ECS156
01206                                                                   ECS156
01207      IF CO-LFRFND EQUAL ZEROS                                     ECS156
01208          GO TO 0220-CHECK-AH-AMOUNTS.                             ECS156
01209                                                                   ECS156
01210      MOVE CO-LF-CANC-DT          TO WS-CURR-CNC-DT.                  CL**3
01211                                                                      CL**3
01212      MOVE ZEROS                  TO WS-PREV-CNC-DT.                  CL**3
01213                                                                   ECS156
01214      IF WS-CURR-CNC-CCYY GREATER THAN WS-PREV-YEAR                   CL**5
01215          GO TO 0220-CHECK-AH-AMOUNTS.                             ECS156
01216                                                                   ECS156
01217      MOVE 'LF '                  TO WS-PRINT-TYPE.                   CL**3
01218      MOVE CO-LFRFND              TO WS-PRINT-CANC-AMT-2.             CL**3
01219                                                                   ECS156
01220      MOVE ERR-C                  TO WS-PRINT-MESSAGE.                CL**3
01221      PERFORM  0500-PRINT-LINE  THRU 0500-EXIT.                    ECS156
01222      MOVE 'Y'                    TO PRINT-DATA.                      CL**3
01223                                                                   ECS156
01224  0220-CHECK-AH-AMOUNTS.                                           ECS156
01225                                                                   ECS156
01226      IF PRINT-DATA EQUAL 'Y'                                      ECS156
01227          MOVE SPACES             TO WS-DETAIL-PRINT-LINE.            CL**3
01228                                                                   ECS156
01229      IF ((CO-AHTYP EQUAL '00' OR '  ') OR                         ECS156
01230          CO-AH-CANC-DT EQUAL ZEROS)                               ECS156
01231          GO TO 0220-CHECK-AMOUNTS-EXIT.                           ECS156
01232                                                                   ECS156
01233      IF CO-AHRFND EQUAL ZEROS                                     ECS156
01234          GO TO 0220-CHECK-AMOUNTS-EXIT.                           ECS156
01235                                                                   ECS156
01236      MOVE CO-AH-CANC-DT          TO WS-CURR-CNC-DT.                  CL**3
01237                                                                      CL**3
01238      MOVE ZEROS                  TO WS-PREV-CNC-DT.                  CL**3
01239                                                                   ECS156
01240      IF WS-CURR-CNC-CCYY GREATER THAN WS-PREV-YEAR                   CL**3
01241          GO TO 0220-CHECK-AMOUNTS-EXIT.                           ECS156
01242                                                                   ECS156
01243      MOVE 'AH '                TO WS-PRINT-TYPE.                  ECS156
01244      MOVE CO-AHRFND            TO WS-PRINT-CANC-AMT-2.            ECS156
01245                                                                   ECS156
01246 *    COMPUTE WS-PRINT-VARIANCE = CO-AHRFND.                       ECS156
01247                                                                   ECS156
01248      MOVE ERR-C                TO WS-PRINT-MESSAGE.               ECS156
01249      PERFORM  0500-PRINT-LINE  THRU 0500-EXIT.                    ECS156
01250      MOVE 'Y'                  TO PRINT-DATA.                     ECS156
01251                                                                   ECS156
01252  0220-CHECK-AMOUNTS-EXIT.                                         ECS156
01253      EXIT.                                                           CL**3
01254  EJECT                                                            ECS156
01255  0230-CHECK-ERRORS.                                               ECS156
01256      IF PRINT-DATA  EQUAL 'Y'                                     ECS156
01257          MOVE 'Z'              TO PRINT-DATA                         CL**3
01258          PERFORM  0500-PRINT-LINE  THRU 0500-EXIT.                ECS156
01259  0230-CHECK-ERRORS-EXIT.                                          ECS156
01260  EJECT                                                            ECS156
01261  0300-READ-PREV-CERT.                                             ECS156
01262                                                                   ECS156
01263      READ CERT-PREV                                               ECS156
01264            AT END                                                 ECS156
01265               MOVE 'Y'           TO EOF-SW                        ECS156
01266               GO TO 0300-READ-PREV-EXIT.                          ECS156
01267                                                                   ECS156
01268      MOVE CR-FULL-CONTROL        TO WS-PREV-CONTROL.              ECS156
01269                                                                   ECS156
01270      ADD 1                       TO CERT-PREV-RECS.               ECS156
01271                                                                   ECS156
01272  0300-READ-PREV-EXIT.                                             ECS156
01273      EXIT.                                                           CL**4
01274  EJECT                                                               CL**4
01275  0310-READ-CURR-CERT.                                             ECS156
01276                                                                   ECS156
01277      READ CERT-CURR                                               ECS156
01278            AT END                                                 ECS156
01279               MOVE 'Y'           TO EOF-SW-2                      ECS156
01280               GO TO 0310-READ-CURR-EXIT.                          ECS156
01281                                                                   ECS156
01282      ADD 1                       TO CERT-CURR-RECS.               ECS156
01283      MOVE CO-FULL-CONTROL        TO WS-CURR-CONTROL.              ECS156
01284                                                                   ECS156
01285  0310-READ-CURR-EXIT.                                             ECS156
01286      EXIT.                                                           CL**3
01287      EJECT                                                           CL**3
01288  0500-PRINT-LINE.                                                 ECS156
01289                                                                   ECS156
01290      IF LINER GREATER THAN 63                                     ECS156
01291          MOVE PAGER            TO HC-PAGE                         ECS156
01292          ADD +1                TO PAGER                           ECS156
01293          MOVE HEAD-A           TO PRT                             ECS156
01294          MOVE '1'              TO X                               ECS156
01295          MOVE X TO LCP-ASA                                        ECS156
01296          PERFORM LCP-WRITE-POS-PRT                                ECS156
01297              THRU LCP-WRITE-END-PRT                               ECS156
01298          MOVE HEAD-A1          TO PRT                             ECS156
01299          MOVE ' '              TO X                               ECS156
01300          MOVE X                TO LCP-ASA                            CL**3
01301          PERFORM LCP-WRITE-POS-PRT                                ECS156
01302              THRU LCP-WRITE-END-PRT                               ECS156
01303          MOVE HEAD-C           TO PRT                             ECS156
01304          MOVE ' '              TO X                               ECS156
01305          MOVE X                TO LCP-ASA                            CL**3
01306          PERFORM LCP-WRITE-POS-PRT                                ECS156
01307              THRU LCP-WRITE-END-PRT                               ECS156
01308          MOVE HEAD-D           TO PRT                             ECS156
01309          MOVE '0'              TO X                               ECS156
01310          MOVE X TO LCP-ASA                                        ECS156
01311          PERFORM LCP-WRITE-POS-PRT                                ECS156
01312              THRU LCP-WRITE-END-PRT                               ECS156
01313          MOVE HEAD-E           TO PRT                             ECS156
01314          MOVE ' '              TO X                               ECS156
01315          MOVE X                TO LCP-ASA                            CL**3
01316          PERFORM LCP-WRITE-POS-PRT                                ECS156
01317              THRU LCP-WRITE-END-PRT                               ECS156
01318          MOVE +5               TO LINER.                          ECS156
01319                                                                   ECS156
01320      IF PRINT-DATA EQUAL SPACE                                    ECS156
01321         MOVE WS-PREV-CARRIER   TO WS-PRINT-CARRIER                ECS156
01322         MOVE WS-PREV-GROUPING  TO WS-PRINT-GROUPING               ECS156
01323         MOVE WS-PREV-STATE     TO WS-PRINT-STATE                  ECS156
01324         MOVE WS-PREV-ACCOUNT   TO WS-PRINT-ACCOUNT                ECS156
01325         MOVE WS-PREV-CERT-NO   TO WS-PRINT-CERT-NO                ECS156
01326         MOVE WS-PREV-CR-DT     TO WS-PREV-EFF-DT                     CL*19
01327         MOVE WS-PREV-EFF-YR    TO WS-CORR-YR                         CL*19
01328         MOVE WS-PREV-EFF-MO    TO WS-CORR-MO                         CL*18
01329         MOVE WS-PREV-EFF-DA    TO WS-CORR-DA                         CL*18
01330         MOVE WS-CORR-DATE      TO WS-PRINT-EFF-DATE.              ECS156
01331                                                                   ECS156
01332      MOVE WS-PREV-CNC-YR       TO WS-CORR-YR.                     ECS156
01333      MOVE WS-PREV-CNC-MO       TO WS-CORR-MO.                     ECS156
01334      MOVE WS-PREV-CNC-DA       TO WS-CORR-DA.                     ECS156
01335      IF WS-CORR-DATE NOT EQUAL '00/00/00'                         ECS156
01336          MOVE WS-CORR-DATE     TO WS-PRINT-CANC-DT-1.             ECS156
01337                                                                   ECS156
01338      MOVE WS-CURR-CNC-YR       TO WS-CORR-YR.                     ECS156
01339      MOVE WS-CURR-CNC-MO       TO WS-CORR-MO.                     ECS156
01340      MOVE WS-CURR-CNC-DA       TO WS-CORR-DA.                     ECS156
01341      IF WS-CORR-DATE NOT EQUAL '00/00/00'                         ECS156
01342          MOVE WS-CORR-DATE     TO WS-PRINT-CANC-DT-2.             ECS156
01343                                                                   ECS156
01344      IF PRINT-DATA EQUAL 'E'                                      ECS156
01345         MOVE WS-TOTAL-LINE     TO PRT                             ECS156
01346      ELSE                                                         ECS156
01347         IF PRINT-DATA EQUAL 'Z'                                   ECS156
01348             MOVE WS-DETAIL-PRINT-ERROR TO PRT                     ECS156
01349         ELSE                                                      ECS156
01350             MOVE WS-DETAIL-PRINT-LINE  TO PRT.                    ECS156
01351                                                                   ECS156
01352      IF PRINT-DATA EQUAL 'Y'                                      ECS156
01353          MOVE ' '              TO X                               ECS156
01354          ADD 1                 TO LINER                           ECS156
01355      ELSE                                                         ECS156
01356          MOVE '0'              TO X                               ECS156
01357          ADD 2                 TO LINER.                          ECS156
01358                                                                   ECS156
01359      MOVE X TO LCP-ASA                                            ECS156
01360      PERFORM LCP-WRITE-POS-PRT                                    ECS156
01361          THRU LCP-WRITE-END-PRT.                                  ECS156
01362                                                                   ECS156
01363      MOVE X                    TO P-CTL.                          ECS156
01364                                                                   ECS156
01365      MOVE SPACES               TO WS-DETAIL-PRINT-LINE.           ECS156
01366      MOVE ZEROS                TO WS-PRINT-CANC-AMT-1.            ECS156
01367      MOVE ZEROS                TO WS-PRINT-CANC-AMT-2.            ECS156
01368      MOVE ZEROS                TO WS-PRINT-VARIANCE.              ECS156
01369      MOVE '0'                  TO WS-PRINT-CARRIAGE.              ECS156
01370                                                                   ECS156
01371  0500-EXIT.                                                       ECS156
01372      EXIT.                                                           CL**3
01373                                                                      CL**3
01374  0600-WRITE-WORK-FILE.                                            ECS156
01375                                                                   ECS156
01376      WRITE WORK-RECORD.                                           ECS156
01377      ADD +1                    TO RECORDS-WRITTEN.                ECS156
01378                                                                   ECS156
01379  0600-EXIT.                                                       ECS156
01380      EXIT.                                                           CL**3
01381  EJECT                                                            ECS156
01382  0700-SORT-RECS SECTION.                                          ECS156
01383                                                                   ECS156
01384      OPEN  INPUT WORK-FILE.                                       ECS156
01385                                                                   ECS156
01386  0700-SORT-LOOP.                                                  ECS156
01387      READ WORK-FILE                                               ECS156
01388          AT END                                                   ECS156
01389              GO TO 0700-SORT-DISPLAY.                             ECS156
01390                                                                   ECS156
01391      MOVE WORK-RECORD         TO SORT-RECORD.                     ECS156
01392      RELEASE SORT-RECORD.                                         ECS156
01393      ADD +1                   TO RECORDS-RELEASED.                ECS156
01394      GO TO 0700-SORT-LOOP.                                        ECS156
01395                                                                   ECS156
01396  0700-SORT-DISPLAY.                                               ECS156
01397                                                                   ECS156
01398      CLOSE  WORK-FILE.                                            ECS156
01399      DISPLAY ' '.                                                 ECS156
01400      DISPLAY 'END OF SORT '.                                      ECS156
01401      DISPLAY ' RECORDS RELEASED ' RECORDS-RELEASED.               ECS156
01402      DISPLAY ' OPTION REQUESTED ' DTE-TOT-OPT.                    ECS156
01403                                                                   ECS156
01404  0700-SORT-EXIT.                                                  ECS156
01405      EXIT.                                                        ECS156
01406  EJECT                                                            ECS156
01407  1000-PRINT-REPORT.                                               ECS156
01408      RETURN SORT-FILE                                             ECS156
01409          AT END                                                   ECS156
01410             MOVE HIGH-VALUES   TO SORT-RECORD                     ECS156
01411             GO TO 1098-TOTAL-LINE.                                ECS156
01412                                                                   ECS156
01413      ADD +1                    TO RECORDS-RECEIVED.               ECS156
01414                                                                   ECS156
01415      IF WS-SAVE-FIELDS EQUAL ALL '9'                              ECS156
01416          MOVE SW-CARRIER        TO WS-SAVE-CARRIER                ECS156
01417                                    HEAD-CARRIER-D                 ECS156
01418          MOVE SW-STATE          TO WS-SAVE-STATE                  ECS156
01419                                    HEAD-STATE-D                   ECS156
01420          MOVE SW-ACCOUNT        TO WS-SAVE-ACCOUNT                ECS156
01421                                    HEAD-ACCOUNT-D                 ECS156
01422          MOVE SW-COVERAGE       TO WS-SAVE-COVERAGE               ECS156
01423          MOVE SW-RATE-CLASS     TO WS-SAVE-CLASS                  ECS156
01424                                    HEAD-CLASS-D                   ECS156
01425          MOVE SW-RATE-DEVIATION TO WS-SAVE-DEV                    ECS156
01426                                    HEAD-DEVIATION-D               ECS156
01427          MOVE SW-BUSINESS-TYPE  TO WS-SAVE-BUSTP                  ECS156
01428                                    HEAD-BUSTP.                    ECS156
01429                                                                   ECS156
01430      IF SW-CARRIER NOT EQUAL  WS-SAVE-CARRIER                     ECS156
01431         PERFORM 1200-CARRIER-BREAK THRU 1200-CARRIER-EXIT.        ECS156
01432                                                                   ECS156
01433      IF (SW-STATE   NOT EQUAL  WS-SAVE-STATE)                     ECS156
01434         PERFORM 1300-STATE-BREAK THRU 1300-STATE-EXIT.            ECS156
01435                                                                   ECS156
01436      IF (SW-ACCOUNT NOT EQUAL  WS-SAVE-ACCOUNT)                   ECS156
01437          AND (DTE-TOT-OPT EQUAL '3')                              ECS156
01438         PERFORM 1400-ACCOUNT-BREAK THRU 1400-ACCOUNT-EXIT.        ECS156
01439                                                                   ECS156
01440      IF (SW-RATE-CLASS NOT EQUAL  WS-SAVE-CLASS)                  ECS156
01441          AND (DTE-TOT-OPT EQUAL '1')                              ECS156
01442         PERFORM 1600-CLASS-BREAK THRU 1600-CLASS-EXIT.            ECS156
01443                                                                   ECS156
01444      IF (SW-RATE-DEVIATION  NOT EQUAL  WS-SAVE-DEV)               ECS156
01445          AND (DTE-TOT-OPT EQUAL '2' OR '3' OR '4')                ECS156
01446         PERFORM 1700-DEV-BREAK THRU 1700-DEV-EXIT.                ECS156
01447                                                                   ECS156
01448      IF (SW-BUSINESS-TYPE NOT EQUAL  WS-SAVE-BUSTP)               ECS156
01449          AND (DTE-TOT-OPT EQUAL '4')                              ECS156
01450         PERFORM 1800-BUSTP-BREAK THRU 1800-BUSTP-EXIT.            ECS156
01451                                                                   ECS156
01452      IF SW-COVERAGE EQUAL 'LF'                                    ECS156
01453          ADD SW-VARIANCE         TO LIFE-VARIANCE                 ECS156
01454                                     LIFE-DEV-VARIANCE             ECS156
01455                                     LIFE-BUSTP-VARIANCE           ECS156
01456                                     LIFE-CLASS-VARIANCE           ECS156
01457                                     LIFE-ACCT-VARIANCE            ECS156
01458                                     LIFE-STATE-VARIANCE           ECS156
01459                                     LIFE-CARR-VARIANCE            ECS156
01460                                     LIFE-TOTAL-VARIANCE           ECS156
01461      ELSE                                                         ECS156
01462          ADD SW-VARIANCE         TO AH-VARIANCE                   ECS156
01463                                     AH-DEV-VARIANCE               ECS156
01464                                     AH-BUSTP-VARIANCE             ECS156
01465                                     AH-CLASS-VARIANCE             ECS156
01466                                     AH-ACCT-VARIANCE              ECS156
01467                                     AH-STATE-VARIANCE             ECS156
01468                                     AH-CARR-VARIANCE              ECS156
01469                                     AH-TOTAL-VARIANCE.            ECS156
01470                                                                   ECS156
01471      GO TO 1000-PRINT-REPORT.                                     ECS156
01472                                                                   ECS156
01473  1098-TOTAL-LINE.                                                 ECS156
01474                                                                   ECS156
01475      PERFORM 1200-CARRIER-BREAK THRU 1200-CARRIER-EXIT.           ECS156
01476                                                                   ECS156
01477      MOVE 'GRAND TOTAL ** '      TO WS-PRINT-DESC.                ECS156
01478      MOVE SPACES                 TO WS-PRINT-CURRENT-OPT.         ECS156
01479      MOVE LIFE-TOTAL-VARIANCE    TO WS-PRINT-LIFE-VARIANCE.       ECS156
01480      MOVE AH-TOTAL-VARIANCE      TO WS-PRINT-AH-VARIANCE.         ECS156
01481      MOVE WS-VARIANCE-PRINT-LINE TO WS-SAVE-PRINT-LINE.           ECS156
01482      MOVE '0'                    TO X.                            ECS156
01483      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01484      ADD +2                    TO LINER.                          ECS156
01485      MOVE SPACES                 TO WS-SAVE-PRINT-LINE.           ECS156
01486      MOVE '-'                    TO X.                            ECS156
01487      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01488      ADD +3                    TO LINER.                          ECS156
01489                                                                   ECS156
01490  1099-END-OUTPUT.                                                 ECS156
01491      EXIT.                                                           CL**3
01492  EJECT                                                               CL**3
01493  1200-CARRIER-BREAK.                                              ECS156
01494                                                                   ECS156
01495      PERFORM 1300-STATE-BREAK      THRU 1300-STATE-EXIT.          ECS156
01496                                                                   ECS156
01497      MOVE '***CARRIER TOTALS '   TO WS-PRINT-DESC.                ECS156
01498      MOVE WS-SAVE-CARRIER        TO WS-PRINT-CURRENT-OPT.         ECS156
01499      MOVE LIFE-CARR-VARIANCE     TO WS-PRINT-LIFE-VARIANCE.       ECS156
01500      MOVE AH-CARR-VARIANCE       TO WS-PRINT-AH-VARIANCE.         ECS156
01501      MOVE WS-VARIANCE-PRINT-LINE TO WS-SAVE-PRINT-LINE.           ECS156
01502      MOVE '0'                    TO X.                            ECS156
01503      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01504      ADD +2                    TO LINER.                          ECS156
01505      MOVE SPACES                 TO WS-SAVE-PRINT-LINE.           ECS156
01506      MOVE '-'                    TO X.                            ECS156
01507      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01508      ADD +3                    TO LINER.                          ECS156
01509                                                                   ECS156
01510      MOVE ZEROS                  TO LIFE-VARIANCE                 ECS156
01511                                     LIFE-DEV-VARIANCE             ECS156
01512                                     LIFE-BUSTP-VARIANCE           ECS156
01513                                     LIFE-CLASS-VARIANCE           ECS156
01514                                     LIFE-ACCT-VARIANCE            ECS156
01515                                     LIFE-STATE-VARIANCE           ECS156
01516                                     LIFE-CARR-VARIANCE            ECS156
01517                                     AH-VARIANCE                   ECS156
01518                                     AH-DEV-VARIANCE               ECS156
01519                                     AH-BUSTP-VARIANCE             ECS156
01520                                     AH-CLASS-VARIANCE             ECS156
01521                                     AH-ACCT-VARIANCE              ECS156
01522                                     AH-STATE-VARIANCE             ECS156
01523                                     AH-CARR-VARIANCE.             ECS156
01524                                                                   ECS156
01525      MOVE SW-CARRIER            TO WS-SAVE-CARRIER.               ECS156
01526                                                                   ECS156
01527  1200-CARRIER-EXIT.                                               ECS156
01528      EXIT.                                                           CL**3
01529                                                                      CL**3
01530  1300-STATE-BREAK.                                                ECS156
01531                                                                   ECS156
01532      IF DTE-TOT-OPT EQUAL '1'                                     ECS156
01533         PERFORM 1600-CLASS-BREAK   THRU 1600-CLASS-EXIT.          ECS156
01534                                                                   ECS156
01535      IF DTE-TOT-OPT EQUAL '2'                                     ECS156
01536         PERFORM 1700-DEV-BREAK     THRU 1700-DEV-EXIT.            ECS156
01537                                                                   ECS156
01538      IF DTE-TOT-OPT EQUAL '3'                                     ECS156
01539         PERFORM 1400-ACCOUNT-BREAK THRU 1400-ACCOUNT-EXIT.        ECS156
01540                                                                   ECS156
01541      IF DTE-TOT-OPT EQUAL '4'                                     ECS156
01542         PERFORM 1800-BUSTP-BREAK   THRU 1800-BUSTP-EXIT           ECS156
01543         PERFORM 1700-DEV-BREAK     THRU 1700-DEV-EXIT.            ECS156
01544                                                                   ECS156
01545      MOVE '**STATE TOTALS '      TO WS-PRINT-DESC.                ECS156
01546      MOVE WS-SAVE-STATE          TO WS-PRINT-CURRENT-OPT.         ECS156
01547      MOVE LIFE-STATE-VARIANCE    TO WS-PRINT-LIFE-VARIANCE.       ECS156
01548      MOVE AH-STATE-VARIANCE      TO WS-PRINT-AH-VARIANCE.         ECS156
01549      MOVE WS-VARIANCE-PRINT-LINE TO WS-SAVE-PRINT-LINE.           ECS156
01550      MOVE '0'                    TO X.                            ECS156
01551      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01552      ADD +2                    TO LINER.                          ECS156
01553      MOVE SPACES                 TO WS-SAVE-PRINT-LINE.           ECS156
01554      MOVE '-'                    TO X.                            ECS156
01555      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01556      ADD +3                    TO LINER.                          ECS156
01557                                                                   ECS156
01558      MOVE ZEROS                  TO LIFE-VARIANCE                 ECS156
01559                                     LIFE-DEV-VARIANCE             ECS156
01560                                     LIFE-BUSTP-VARIANCE           ECS156
01561                                     LIFE-CLASS-VARIANCE           ECS156
01562                                     LIFE-ACCT-VARIANCE            ECS156
01563                                     LIFE-STATE-VARIANCE           ECS156
01564                                     AH-VARIANCE                   ECS156
01565                                     AH-DEV-VARIANCE               ECS156
01566                                     AH-BUSTP-VARIANCE             ECS156
01567                                     AH-CLASS-VARIANCE             ECS156
01568                                     AH-ACCT-VARIANCE              ECS156
01569                                     AH-STATE-VARIANCE.            ECS156
01570                                                                   ECS156
01571      MOVE SW-STATE              TO WS-SAVE-STATE.                 ECS156
01572                                                                   ECS156
01573  1300-STATE-EXIT.                                                 ECS156
01574      EXIT.                                                           CL**3
01575                                                                      CL**3
01576  1400-ACCOUNT-BREAK.                                              ECS156
01577                                                                   ECS156
01578      IF DTE-TOT-OPT EQUAL '3'                                     ECS156
01579         PERFORM 1700-DEV-BREAK   THRU 1700-DEV-EXIT.              ECS156
01580                                                                   ECS156
01581      MOVE 'ACCOUNT TOTALS '      TO WS-PRINT-DESC.                ECS156
01582      MOVE WS-SAVE-ACCOUNT        TO WS-PRINT-CURRENT-OPT.         ECS156
01583      MOVE LIFE-ACCT-VARIANCE     TO WS-PRINT-LIFE-VARIANCE.       ECS156
01584      MOVE AH-ACCT-VARIANCE       TO WS-PRINT-AH-VARIANCE.         ECS156
01585      MOVE WS-VARIANCE-PRINT-LINE TO WS-SAVE-PRINT-LINE.           ECS156
01586      MOVE '0'                    TO X.                            ECS156
01587      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01588      ADD +2                    TO LINER.                          ECS156
01589      MOVE SPACES                 TO WS-SAVE-PRINT-LINE.           ECS156
01590      MOVE '-'                    TO X.                            ECS156
01591      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01592      ADD +3                    TO LINER.                          ECS156
01593                                                                   ECS156
01594                                                                   ECS156
01595      MOVE ZEROS                  TO LIFE-VARIANCE                 ECS156
01596                                     LIFE-DEV-VARIANCE             ECS156
01597                                     LIFE-BUSTP-VARIANCE           ECS156
01598                                     LIFE-ACCT-VARIANCE            ECS156
01599                                     AH-VARIANCE                   ECS156
01600                                     AH-DEV-VARIANCE               ECS156
01601                                     AH-BUSTP-VARIANCE             ECS156
01602                                     AH-ACCT-VARIANCE.             ECS156
01603                                                                   ECS156
01604      MOVE SW-ACCOUNT            TO WS-SAVE-ACCOUNT.               ECS156
01605                                                                   ECS156
01606  1400-ACCOUNT-EXIT.                                               ECS156
01607      EXIT.                                                           CL**3
01608                                                                      CL**3
01609  1500-COVERAG-BREAK.                                              ECS156
01610                                                                   ECS156
01611      MOVE 'COVERAGE TOTALS '     TO WS-PRINT-DESC.                ECS156
01612      MOVE LIFE-VARIANCE          TO WS-PRINT-LIFE-VARIANCE.       ECS156
01613      MOVE AH-VARIANCE            TO WS-PRINT-AH-VARIANCE.         ECS156
01614      MOVE WS-VARIANCE-PRINT-LINE TO WS-SAVE-PRINT-LINE.           ECS156
01615      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01616                                                                   ECS156
01617      IF SW-COVERAGE EQUAL 'LF'                                    ECS156
01618          MOVE ZEROS              TO LIFE-VARIANCE                 ECS156
01619      ELSE                                                         ECS156
01620          MOVE ZEROS              TO AH-VARIANCE.                  ECS156
01621                                                                   ECS156
01622  1500-COVERAG-EXIT.                                               ECS156
01623      EXIT.                                                           CL**4
01624                                                                      CL**3
01625  1600-CLASS-BREAK.                                                ECS156
01626                                                                   ECS156
01627      MOVE 'RATE/CLASS TOTALS '   TO WS-PRINT-DESC.                ECS156
01628      MOVE WS-SAVE-CLASS          TO WS-PRINT-CURRENT-OPT.         ECS156
01629      MOVE LIFE-CLASS-VARIANCE    TO WS-PRINT-LIFE-VARIANCE.       ECS156
01630      MOVE AH-CLASS-VARIANCE      TO WS-PRINT-AH-VARIANCE.         ECS156
01631      MOVE WS-VARIANCE-PRINT-LINE TO WS-SAVE-PRINT-LINE.           ECS156
01632      MOVE '0'                    TO X.                            ECS156
01633      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01634      ADD +2                    TO LINER.                          ECS156
01635                                                                   ECS156
01636      MOVE ZEROS                  TO LIFE-CLASS-VARIANCE           ECS156
01637                                     AH-CLASS-VARIANCE.            ECS156
01638                                                                   ECS156
01639      MOVE SW-RATE-CLASS         TO WS-SAVE-CLASS.                 ECS156
01640                                                                   ECS156
01641  1600-CLASS-EXIT.                                                 ECS156
01642      EXIT.                                                           CL**3
01643                                                                      CL**3
01644  1700-DEV-BREAK.                                                  ECS156
01645                                                                   ECS156
01646      MOVE 'DEVIATION  TOTALS '   TO WS-PRINT-DESC.                ECS156
01647      MOVE WS-SAVE-DEV            TO WS-PRINT-CURRENT-OPT.         ECS156
01648      MOVE LIFE-DEV-VARIANCE      TO WS-PRINT-LIFE-VARIANCE.       ECS156
01649      MOVE AH-DEV-VARIANCE        TO WS-PRINT-AH-VARIANCE.         ECS156
01650      MOVE WS-VARIANCE-PRINT-LINE TO WS-SAVE-PRINT-LINE.           ECS156
01651      MOVE '0'                    TO X.                            ECS156
01652      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01653      ADD +2                    TO LINER.                          ECS156
01654                                                                   ECS156
01655      MOVE ZEROS                  TO LIFE-DEV-VARIANCE             ECS156
01656                                     AH-DEV-VARIANCE.              ECS156
01657                                                                   ECS156
01658      MOVE SW-RATE-DEVIATION     TO WS-SAVE-DEV.                   ECS156
01659                                                                   ECS156
01660                                                                   ECS156
01661  1700-DEV-EXIT.                                                   ECS156
01662      EXIT.                                                           CL**3
01663                                                                      CL**3
01664  1800-BUSTP-BREAK.                                                ECS156
01665                                                                   ECS156
01666      MOVE 'BUS TYPE   TOTALS '   TO WS-PRINT-DESC.                ECS156
01667      MOVE LIFE-BUSTP-VARIANCE    TO WS-PRINT-LIFE-VARIANCE.       ECS156
01668      MOVE AH-BUSTP-VARIANCE      TO WS-PRINT-AH-VARIANCE.         ECS156
01669      MOVE SPACES                 TO WS-PRINT-CURRENT-OPT.         ECS156
01670      MOVE WS-SAVE-BUSTP          TO WS-PRINT-CURRENT-OPT.         ECS156
01671      MOVE WS-VARIANCE-PRINT-LINE TO WS-SAVE-PRINT-LINE.           ECS156
01672      MOVE '0'                    TO X.                            ECS156
01673      PERFORM 1900-PRINT-DETAIL   THRU 1900-PRINT-EXIT.            ECS156
01674      ADD +2                    TO LINER.                          ECS156
01675                                                                   ECS156
01676      MOVE ZEROS                  TO LIFE-BUSTP-VARIANCE           ECS156
01677                                     AH-BUSTP-VARIANCE.            ECS156
01678                                                                   ECS156
01679      MOVE SW-BUSINESS-TYPE      TO WS-SAVE-BUSTP.                 ECS156
01680                                                                   ECS156
01681  1800-BUSTP-EXIT.                                                 ECS156
01682      EXIT.                                                           CL**3
01683                                                                      CL**3
01684  1900-PRINT-DETAIL.                                               ECS156
01685                                                                   ECS156
01686      IF LINER GREATER THAN 63                                     ECS156
01687          PERFORM 1999-PRINT-HEADINGS THRU 1999-HEADINGS-EXIT.     ECS156
01688                                                                   ECS156
01689      MOVE WS-SAVE-PRINT-LINE   TO PRT.                            ECS156
01690      MOVE X TO LCP-ASA                                            ECS156
01691      PERFORM LCP-WRITE-POS-PRT                                    ECS156
01692          THRU LCP-WRITE-END-PRT.                                  ECS156
01693                                                                   ECS156
01694  1900-PRINT-EXIT.                                                 ECS156
01695      EXIT.                                                           CL**3
01696                                                                      CL**3
01697  1999-PRINT-HEADINGS.                                             ECS156
01698                                                                   ECS156
01699      MOVE SPACES               TO PRT.                            ECS156
01700                                                                   ECS156
01701      ADD +1                    TO PAGER                           ECS156
01702      MOVE PAGER                TO HC-PAGE.                        ECS156
01703                                                                   ECS156
01704      MOVE HEAD-A               TO PRT.                            ECS156
01705      MOVE '1'                  TO X.                              ECS156
01706      MOVE X TO LCP-ASA                                            ECS156
01707      PERFORM LCP-WRITE-POS-PRT                                    ECS156
01708          THRU LCP-WRITE-END-PRT.                                  ECS156
01709                                                                   ECS156
01710      MOVE HEAD-A1              TO PRT.                            ECS156
01711      MOVE ' '                  TO X.                              ECS156
01712      MOVE X TO LCP-ASA                                            ECS156
01713      PERFORM LCP-WRITE-POS-PRT                                    ECS156
01714          THRU LCP-WRITE-END-PRT.                                  ECS156
01715                                                                   ECS156
01716      MOVE HEAD-C               TO PRT.                            ECS156
01717      MOVE ' '                  TO X.                              ECS156
01718      MOVE X TO LCP-ASA                                            ECS156
01719      PERFORM LCP-WRITE-POS-PRT                                    ECS156
01720          THRU LCP-WRITE-END-PRT.                                  ECS156
01721                                                                   ECS156
01722      MOVE HEAD-VARIANCE-D      TO PRT.                            ECS156
01723      MOVE '0'                  TO X.                              ECS156
01724      MOVE X TO LCP-ASA                                            ECS156
01725      PERFORM LCP-WRITE-POS-PRT                                    ECS156
01726          THRU LCP-WRITE-END-PRT.                                  ECS156
01727                                                                   ECS156
01728      MOVE +5                   TO LINER.                          ECS156
01729                                                                   ECS156
01730      MOVE SPACES               TO PRT.                            ECS156
01731                                                                   ECS156
01732  1999-HEADINGS-EXIT.                                              ECS156
01733      EXIT.                                                           CL**3
01734  EJECT                                                               CL**3
01735  8500-DATE-CONVERSION.                                            ECS156
01736                                                                   ECS156
01737      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   ECS156
01738                                                                   ECS156
01739  8500-EXIT.                                                       ECS156
01740      EXIT.                                                        ECS156
01741  ABEND-PGM  SECTION.                                              ECS156
01742      DISPLAY WS-ABEND-MESSAGE.                                       CL**6
01743      DISPLAY 'PROGRAM WILL NOW ABEND ********'.                      CL**6
01744      DIVIDE WS-ZERO BY WS-ZERO GIVING WS-ZERO.                    ECS156
01745 /                                                                 ECS156
01746  LCP-WRITE-POS-PRT SECTION.                                       ECS156
01747      IF LCP-ASA = '+'                                             ECS156
01748          WRITE PRT AFTER 0 LINE                                   ECS156
01749      ELSE                                                         ECS156
01750      IF LCP-ASA = ' '                                             ECS156
01751          WRITE PRT AFTER ADVANCING 1 LINE                         ECS156
01752      ELSE                                                         ECS156
01753      IF LCP-ASA = '0'                                             ECS156
01754          WRITE PRT AFTER ADVANCING 2 LINE                         ECS156
01755      ELSE                                                         ECS156
01756      IF LCP-ASA = '-'                                             ECS156
01757          WRITE PRT AFTER ADVANCING 3 LINE                         ECS156
01758      ELSE                                                         ECS156
01759      IF LCP-ASA = '1'                                             ECS156
01760          WRITE PRT AFTER ADVANCING PAGE                           ECS156
01761      ELSE                                                         ECS156
01762      IF LCP-ASA = '2'                                             ECS156
01763          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS156
01764      ELSE                                                         ECS156
01765      IF LCP-ASA = '3'                                             ECS156
01766          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS156
01767      ELSE                                                         ECS156
01768      IF LCP-ASA = '4'                                             ECS156
01769          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS156
01770      ELSE                                                         ECS156
01771      IF LCP-ASA = '5'                                             ECS156
01772          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS156
01773      ELSE                                                         ECS156
01774      IF LCP-ASA = '6'                                             ECS156
01775          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS156
01776      ELSE                                                         ECS156
01777      IF LCP-ASA = '7'                                             ECS156
01778          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS156
01779      ELSE                                                         ECS156
01780      IF LCP-ASA = '8'                                             ECS156
01781          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS156
01782      ELSE                                                         ECS156
01783      IF LCP-ASA = '9'                                             ECS156
01784          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS156
01785      ELSE                                                         ECS156
01786      IF LCP-ASA = 'A'                                             ECS156
01787          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS156
01788      ELSE                                                         ECS156
01789      IF LCP-ASA = 'B'                                             ECS156
01790          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS156
01791      ELSE                                                         ECS156
01792      IF LCP-ASA = 'C'                                             ECS156
01793          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS156
01794      ELSE                                                         ECS156
01795      IF LCP-ASA = 'V'                                             ECS156
01796          WRITE PRT AFTER ADVANCING LCP-P01                        ECS156
01797      ELSE                                                         ECS156
01798      IF LCP-ASA = 'W'                                             ECS156
01799          WRITE PRT AFTER ADVANCING LCP-P02                        ECS156
01800      ELSE                                                         ECS156
01801      DISPLAY 'ASA CODE ERROR'.                                    ECS156
01802  LCP-WRITE-END-PRT.                                               ECS156
01803      EXIT.                                                        ECS156
