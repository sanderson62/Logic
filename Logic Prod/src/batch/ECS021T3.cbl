00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   ECS021T3
00003  PROGRAM-ID.                ECS021T3.                                LV011
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 11/28/95 11:02:38.                    CL**7
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             CL**8
00008 *                           VMOD=2.008.                              CL*11
00009                                                                   ECS021T3
00010                                                                   ECS021T3
00011 *AUTHOR.        LOGIC, INC.                                          CL**7
00012 *               DALLAS, TEXAS.                                       CL**7
00013                                                                   ECS021T3
00014                                                                   ECS021T3
00015 *DATE-COMPILED.                                                      CL**7
00016                                                                   ECS021T3
00017 *SECURITY.   *****************************************************   CL**7
00018 *            *                                                   *   CL**7
00019 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00020 *            *                                                   *   CL**7
00021 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00022 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00023 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00024 *            *                                                   *   CL**7
00025 *            *****************************************************   CL**7
00026                                                                   ECS021T3
00027 *REMARKS.                                                            CL**3
00028 *        THIS SUBMODULE WORKS IN CONJUNCTION WITH ECS021             CL**3
00029 *        TO ACCUMULATE FIGURES AND CREATE THE VARIOUS BREAK          CL**3
00030 *        LEVELS IN THE PROFIT ANALYSIS REPORT.                       CL**3
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 075 TO 900
092602******************************************************************
00031                                                                   ECS021T3
00032  ENVIRONMENT DIVISION.                                            ECS021T3
00033  INPUT-OUTPUT SECTION.                                            ECS021T3
00034  FILE-CONTROL.                                                    ECS021T3
00035                                                                   ECS021T3
00036  EJECT                                                            ECS021T3
00037  DATA DIVISION.                                                   ECS021T3
00038  FILE SECTION.                                                    ECS021T3
00039                                                                   ECS021T3
00040                                                                   ECS021T3
00041  EJECT                                                            ECS021T3
00042  WORKING-STORAGE SECTION.                                         ECS021T3
00043  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**7
00044                                                                   ECS021T3
00045  77  FILLER  PIC X(32) VALUE '********************************'.  ECS021T3
00046  77  FILLER  PIC X(32) VALUE '     ECS021T3 WORKING STORAGE   '.  ECS021T3
00047  77  FILLER  PIC X(32) VALUE '********* VMOD=2.008 ***********'.     CL*10
00048                                                                   ECS021T3
00049  77  NET-COUNT               PIC S9(7)     COMP-3.                   CL**5
00050  77  DTE-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T3
00051  77  TOT-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T3
00052  77  BEN-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T3
00053  77  BRK-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T3
00054  77  REQ-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T3
00055  77  ONE                     PIC  99              VALUE 1.        ECS021T3
00056  77  THREE                   PIC  99              VALUE 3.        ECS021T3
00057  77  SIXTEEN                 PIC S9(4)     COMP   VALUE +16.      ECS021T3
00058  77  THREE-HUNDRED           PIC S9(4)     COMP   VALUE +300.     ECS021T3
092602 77  NINE-HUNDRED            PIC S9(4)     COMP   VALUE +900.     ECS021T3
00059                                                                   ECS021T3
00060  01  WS-ABEND-STORAGE.                                            ECS021T3
00061      12  WS-RETURN-CODE          PIC S9(4)  VALUE +0 COMP.        ECS021T3
00062      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         ECS021T3
00063      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          ECS021T3
00064      12  WS-ZERO                 PIC S9     VALUE +0 COMP-3.      ECS021T3
00065      12  WS-ABEND-CODE           PIC S9(4).                       ECS021T3
00066      12  WORK-ABEND-CODE  REDEFINES  WS-ABEND-CODE.               ECS021T3
00067          16  WAC-1               PIC X.                           ECS021T3
00068          16  WAC-2               PIC X.                           ECS021T3
00069          16  WAC-3-4             PIC 99.                          ECS021T3
00070                                                                   ECS021T3
00071  01  WS-PROGRAM-ACTIONS.                                          ECS021T3
00072      12  W-BUILD-ZERO-TABLE      PIC X(02) VALUE 'BZ'.            ECS021T3
00073      12  W-ZERO-TABLE            PIC X(02) VALUE 'ZT'.            ECS021T3
00074      12  W-APPLY-SORT-RCD        PIC X(02) VALUE 'AS'.            ECS021T3
00075      12  W-ADD-LINK-TABLES       PIC X(02) VALUE 'AT'.            ECS021T3
00076      12  W-MOVE-TO-LINK          PIC X(02) VALUE 'MT'.            ECS021T3
00077                                                                   ECS021T3
00078      EJECT                                                        ECS021T3
00079  01  TABLE-IDENTIFIER.                                            ECS021T3
00080      12  FILLER                       PIC X(28)                   ECS021T3
00081                        VALUE '**** START OF TABLE TWO ****'.      ECS021T3
00082                                                                   ECS021T3
00083  01  BREAK-TABLE.                                                 ECS021T3
092602     12  TABLE-ACCUMULATORS     OCCURS  900 TIMES.                   CL**6
00085          16  TBL-BENEFIT-TYPE             PIC X.                  ECS021T3
00086          16  TBL-BENEFIT-CODE             PIC XX.                 ECS021T3
00087          16  TBL-PERIOD     OCCURS   15  TIMES  COMP-3.           ECS021T3
00088              20  TBL-ISS-CNT          PIC S9(7).                  ECS021T3
00089              20  TBL-CNC-CNT          PIC S9(7).                  ECS021T3
00090              20  TBL-ISS-PREM         PIC S9(11)V99.              ECS021T3
00091              20  TBL-CNC-PREM         PIC S9(11)V99.              ECS021T3
00092              20  TBL-NET-COMPEN       PIC S9(11)V99.              ECS021T3
00093              20  TBL-CLM-CNT          PIC S9(7).                  ECS021T3
00094              20  TBL-CLM-AMT          PIC S9(11)V99.              ECS021T3
00095              20  TBL-LOSS-RESV        PIC S9(11)V99.              ECS021T3
00096              20  TBL-EARND-PREM       PIC S9(9)V99.               ECS021T3
00097              20  TBL-PRM-INFRC        PIC S9(9)V99.               ECS021T3
00098              20  TBL-INFRC-CNT        PIC S9(9).                  ECS021T3
00099              20  TBL-AVG-AGE          PIC S9(9).                     CL**6
00100              20  TBL-AVG-ORG-TRM      PIC S9(9).                     CL**6
00101              20  TBL-WGHT-AGE         PIC S9(9).                     CL**6
00102              20  TBL-WGHT-ORG-TRM     PIC S9(9).                     CL**6
00103              20  TBL-EXP-PCT          PIC S9(3)V9(4).             ECS021T3
00104              20  TBL-ADDED-TO-CNT     PIC S9(5).                  ECS021T3
00105                                                                   ECS021T3
00106      EJECT                                                        ECS021T3
092602*01  BREAK-EXTENSION-1.                                           ECS021T3
00108 *    12  TABLE-EXTENSION-1      OCCURS   75 TIMES.                   CL**6
00109 *        16  FILLER                       PIC X.                  ECS021T3
00110 *        16  FILLER                       PIC XX.                 ECS021T3
00111 *        16  TBL-EXTENSION-1    OCCURS   15  TIMES  COMP-3.       ECS021T3
00112 *            20  FILLER               PIC S9(7).                  ECS021T3
00113 *            20  FILLER               PIC S9(7).                  ECS021T3
00114 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00115 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00116 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00117 *            20  FILLER               PIC S9(7).                  ECS021T3
00118 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00119 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00120 *            20  FILLER               PIC S9(9)V99.               ECS021T3
00121 *            20  FILLER               PIC S9(9)V99.               ECS021T3
00122 *            20  FILLER               PIC S9(9).                  ECS021T3
00123 *            20  FILLER               PIC S9(9).                     CL**6
00124 *            20  FILLER               PIC S9(9).                     CL**6
00125 *            20  FILLER               PIC S9(9).                     CL**6
00126 *            20  FILLER               PIC S9(9).                     CL**6
00127 *            20  FILLER               PIC S9(3)V9(4).             ECS021T3
00128 *            20  FILLER               PIC S9(5).                  ECS021T3
00129 *                                                                 ECS021T3
00130 *    EJECT                                                        ECS021T3
00131 *01  BREAK-EXTENSION-2.                                           ECS021T3
00132 *    12  TABLE-EXTENSION-2      OCCURS   75 TIMES.                   CL**6
00133 *        16  FILLER                       PIC X.                  ECS021T3
00134 *        16  FILLER                       PIC XX.                 ECS021T3
00135 *        16  TBL-EXTENSION-2    OCCURS   15  TIMES  COMP-3.       ECS021T3
00136 *            20  FILLER               PIC S9(7).                  ECS021T3
00137 *            20  FILLER               PIC S9(7).                  ECS021T3
00138 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00139 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00140 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00141 *            20  FILLER               PIC S9(7).                  ECS021T3
00142 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00143 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00144 *            20  FILLER               PIC S9(9)V99.               ECS021T3
00145 *            20  FILLER               PIC S9(9)V99.               ECS021T3
00146 *            20  FILLER               PIC S9(9).                  ECS021T3
00147 *            20  FILLER               PIC S9(9).                     CL**6
00148 *            20  FILLER               PIC S9(9).                     CL**6
00149 *            20  FILLER               PIC S9(9).                     CL**6
00150 *            20  FILLER               PIC S9(9).                     CL**6
00151 *            20  FILLER               PIC S9(3)V9(4).                CL**6
00152 *            20  FILLER               PIC S9(5).                     CL**6
00153 *                                                                    CL**6
00154 *    EJECT                                                           CL**6
00155 *01  BREAK-EXTENSION-3.                                              CL**6
00156 *    12  TABLE-EXTENSION-3      OCCURS   75 TIMES.                   CL**6
00157 *        16  FILLER                       PIC X.                     CL**6
00158 *        16  FILLER                       PIC XX.                    CL**6
00159 *        16  TBL-EXTENSION-3    OCCURS   15  TIMES  COMP-3.          CL**6
00160 *            20  FILLER               PIC S9(7).                     CL**2
00161 *            20  FILLER               PIC S9(7).                     CL**2
00162 *            20  FILLER               PIC S9(11)V99.                 CL**6
00163 *            20  FILLER               PIC S9(11)V99.                 CL**6
00164 *            20  FILLER               PIC S9(11)V99.                 CL**6
00165 *            20  FILLER               PIC S9(7).                     CL**2
00166 *            20  FILLER               PIC S9(11)V99.                 CL**6
00167 *            20  FILLER               PIC S9(11)V99.                 CL**6
00168 *            20  FILLER               PIC S9(9)V99.                  CL**6
00169 *            20  FILLER               PIC S9(9)V99.                  CL**6
00170 *            20  FILLER               PIC S9(9).                     CL**6
00171 *            20  FILLER               PIC S9(9).                     CL**6
00172 *            20  FILLER               PIC S9(9).                     CL**6
00173 *            20  FILLER               PIC S9(9).                     CL**6
00174 *            20  FILLER               PIC S9(9).                     CL**6
00175 *            20  FILLER               PIC S9(3)V9(4).             ECS021T3
092602*            20  FILLER               PIC S9(5).                  ECS021T3
00177                                                                   ECS021T3
00178      EJECT                                                        ECS021T3
00179  01  TOTAL-IDENTIFIER.                                            ECS021T3
00180      12  FILLER                       PIC X(28)                   ECS021T3
00181                        VALUE '**** START OF TOTAL TWO ****'.      ECS021T3
00182                                                                   ECS021T3
00183  01  BREAK-TOTAL-TABLE.                                           ECS021T3
00184      12  TABLE-TOTALS           OCCURS  3 TIMES.                  ECS021T3
00185          16  TOT-PERIOD     OCCURS   15  TIMES  COMP-3.           ECS021T3
00186              20  TOT-ISS-CNT          PIC S9(7).                  ECS021T3
00187              20  TOT-CNC-CNT          PIC S9(7).                  ECS021T3
00188              20  TOT-SINGLE-ELEM      PIC S9(7).                  ECS021T3
00189              20  TOT-JOINT-RETRO      PIC S9(7).                  ECS021T3
00190              20  TOT-LIFE-LEVEL       PIC S9(7).                  ECS021T3
00191              20  TOT-ISS-PREM         PIC S9(11)V99.              ECS021T3
00192              20  TOT-CNC-PREM         PIC S9(11)V99.              ECS021T3
00193              20  TOT-NET-COMPEN       PIC S9(11)V99.              ECS021T3
00194              20  TOT-LF-CLM-CNT       PIC S9(7).                  ECS021T3
00195              20  TOT-LF-CLM-AMT       PIC S9(11)V99.              ECS021T3
00196              20  TOT-AH-CLM-CNT       PIC S9(7).                  ECS021T3
00197              20  TOT-AH-CLM-AMT       PIC S9(11)V99.              ECS021T3
00198              20  TOT-LOSS-RESV        PIC S9(11)V99.              ECS021T3
00199              20  TOT-EARND-PREM       PIC S9(9)V99.               ECS021T3
00200              20  TOT-PRM-INFRC        PIC S9(9)V99.               ECS021T3
00201              20  TOT-INFRC-CNT        PIC S9(9).                  ECS021T3
00202              20  TOT-AVG-AGE          PIC S9(9).                     CL**6
00203              20  TOT-AVG-ORG-TRM      PIC S9(9).                     CL**6
00204              20  TOT-WGHT-AGE         PIC S9(9).                     CL**6
00205              20  TOT-WGHT-ORG-TRM     PIC S9(9).                     CL**6
00206              20  TOT-EXP-PCT          PIC S999V9(4).              ECS021T3
00207              20  TOT-ADDED-TO-CNT     PIC S9(5).                  ECS021T3
00208                                                                   ECS021T3
00209      EJECT                                                        ECS021T3
00210                                                                   ECS021T3
00211  01  ZERO-IDENTIFIER.                                             ECS021T3
00212      12  FILLER                       PIC X(28)                   ECS021T3
00213                        VALUE '**** START OF ZERO TABLE****'.      ECS021T3
00214                                                                   ECS021T3
00215  01  ZERO-TABLE.                                                  ECS021T3
00216      12  ZERO-ACCUMULATORS.                                       ECS021T3
00217          16  ZERO-BENEFIT-TYPE            PIC X    VALUE SPACES.  ECS021T3
00218          16  ZERO-BENEFIT-CODE            PIC XX   VALUE ZEROS.   ECS021T3
00219          16  ZERO-PERIOD       OCCURS     15 TIMES COMP-3.        ECS021T3
00220              20  ZERO-ISS-CNT         PIC S9(7).                  ECS021T3
00221              20  ZERO-CNC-CNT         PIC S9(7).                  ECS021T3
00222              20  ZERO-ISS-PREM        PIC S9(11)V99.              ECS021T3
00223              20  ZERO-CNC-PREM        PIC S9(11)V99.              ECS021T3
00224              20  ZERO-NET-COMPEN      PIC S9(11)V99.              ECS021T3
00225              20  ZERO-CLM-CNT         PIC S9(7).                  ECS021T3
00226              20  ZERO-CLM-AMT         PIC S9(11)V99.              ECS021T3
00227              20  ZERO-LOSS-RESV       PIC S9(11)V99.              ECS021T3
00228              20  ZERO-EARND-PREM      PIC S9(9)V99.               ECS021T3
00229              20  ZERO-PRM-INFRC       PIC S9(9)V99.               ECS021T3
00230              20  ZERO-INFRC-CNT       PIC S9(9).                  ECS021T3
00231              20  ZERO-AVG-AGE         PIC S9(9).                     CL**6
00232              20  ZERO-AVG-ORG-TRM     PIC S9(9).                     CL**6
00233              20  ZERO-WGHT-AGE        PIC S9(9).                     CL**6
00234              20  ZERO-WGHT-ORG-TRM    PIC S9(9).                     CL**6
00235              20  ZERO-EXP-PCT         PIC S9(3)V9(4).             ECS021T3
00236              20  ZERO-ADDED-TO-CNT    PIC S9(5).                  ECS021T3
00237                                                                   ECS021T3
00238  01  ZERO-ENTRY-IDENTIFIER.                                       ECS021T3
00239      12  FILLER                   PIC X(28)                       ECS021T3
00240                            VALUE '****START OF ZERO ENTRY ****'.  ECS021T3
00241                                                                   ECS021T3
00242  01  ZERO-TABLE-ENTRIES.                                          ECS021T3
00243      12  ZERO-ACCUMS              COMP-3.                         ECS021T3
00244          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T3
00245          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T3
00246          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T3
00247          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T3
00248          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T3
00249          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T3
00250          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T3
00251          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T3
00252          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021T3
00253          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021T3
00254          16  FILLER               PIC S9(9)       VALUE +0.       ECS021T3
00255          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00256          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00257          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00258          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00259          16  FILLER               PIC S9(3)V9(4)  VALUE +0.       ECS021T3
00260          16  FILLER               PIC S9(5)       VALUE +0.       ECS021T3
00261                                                                   ECS021T3
00262      EJECT                                                        ECS021T3
00263                                                                   ECS021T3
00264  01  ZERO-TOTAL-IDENTIFIER.                                       ECS021T3
00265      12  FILLER                   PIC X(28)                       ECS021T3
00266                            VALUE '****START OF ZERO TOTAL ****'.  ECS021T3
00267                                                                   ECS021T3
00268  01  ZERO-TOTAL-TABLE.                                            ECS021T3
00269      12  ZERO-TOTAL-ACCUMS  OCCURS  15 TIMES  COMP-3.             ECS021T3
00270          16  FILLER                   PIC S9(7).                  ECS021T3
00271          16  FILLER                   PIC S9(7).                  ECS021T3
00272          16  FILLER                   PIC S9(7).                  ECS021T3
00273          16  FILLER                   PIC S9(7).                  ECS021T3
00274          16  FILLER                   PIC S9(7).                  ECS021T3
00275          16  FILLER                   PIC S9(11)V99.              ECS021T3
00276          16  FILLER                   PIC S9(11)V99.              ECS021T3
00277          16  FILLER                   PIC S9(11)V99.              ECS021T3
00278          16  FILLER                   PIC S9(7).                  ECS021T3
00279          16  FILLER                   PIC S9(11)V99.              ECS021T3
00280          16  FILLER                   PIC S9(7).                  ECS021T3
00281          16  FILLER                   PIC S9(11)V99.              ECS021T3
00282          16  FILLER                   PIC S9(11)V99.              ECS021T3
00283          16  FILLER                   PIC S9(9)V99.               ECS021T3
00284          16  FILLER                   PIC S9(9)V99.               ECS021T3
00285          16  FILLER                   PIC S9(9).                  ECS021T3
00286          16  FILLER                   PIC S9(9).                     CL**6
00287          16  FILLER                   PIC S9(9).                     CL**6
00288          16  FILLER                   PIC S9(9).                     CL**6
00289          16  FILLER                   PIC S9(9).                     CL**6
00290          16  FILLER                   PIC S999V9(4).              ECS021T3
00291          16  FILLER                   PIC S9(5).                  ECS021T3
00292                                                                   ECS021T3
00293  01  ZERO-TOTAL-ENTRY         COMP-3.                             ECS021T3
00294      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T3
00295      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T3
00296      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T3
00297      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T3
00298      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T3
00299      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T3
00300      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T3
00301      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T3
00302      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T3
00303      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T3
00304      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T3
00305      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T3
00306      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T3
00307      12  FILLER                       PIC S9(9)V99   VALUE +0.    ECS021T3
00308      12  FILLER                       PIC S9(9)V99   VALUE +0.    ECS021T3
00309      12  FILLER                       PIC S9(9)      VALUE +0.    ECS021T3
00310      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00311      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00312      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00313      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00314      12  FILLER                       PIC S999V9(4)  VALUE +0.    ECS021T3
00315      12  FILLER                       PIC S9(5)      VALUE +0.    ECS021T3
00316                                                                   ECS021T3
00317                                                                   ECS021T3
00318      EJECT                                                        ECS021T3
00319  LINKAGE SECTION.                                                 ECS021T3
00320                                                                   ECS021T3
00321 *************************************************                 ECS021T3
00322 *  ACTION REQUESTS:                             *                 ECS021T3
00323 *                                               *                 ECS021T3
00324 *  BZ - BUILD ZERO TABLE (1ST TIME PROCESSING)  *                 ECS021T3
00325 *  ZT - ZERO INTERNAL TABLES                    *                 ECS021T3
00326 *  AS - APPLY SORT RECORD TO INTERNAL TABLES    *                 ECS021T3
00327 *  AT - ADD LINKAGE TABLES TO INTERNAL TABLES   *                 ECS021T3
00328 *  MT - MOVE TABLES TO LINK TABLES              *                 ECS021T3
00329 *************************************************                 ECS021T3
00330                                                                   ECS021T3
00331  01  REQUEST-TABLE.                                               ECS021T3
00332      12  PROCESSING-REQUEST    OCCURS 10   PIC X(2).              ECS021T3
00333      12  NUMBER-OF-REQUESTS                PIC S9(04) COMP.       ECS021T3
00334                                                                   ECS021T3
00335  01  SW-RECORD.                                                   ECS021T3
00336      12  SW-REPORT-CONTROL-KEY.                                   ECS021T3
00337          16  SW-BREAK-FIELD-1             PIC X(10).              ECS021T3
00338          16  SW-BREAK-FIELD-2             PIC X(10).              ECS021T3
00339          16  SW-BREAK-FIELD-3             PIC X(10).              ECS021T3
00340          16  SW-BREAK-FIELD-4             PIC X(10).              ECS021T3
00341          16  SW-BREAK-FIELD-5             PIC X(10).              ECS021T3
00342          16  SW-BREAK-FIELD-6             PIC X(10).              ECS021T3
00343          16  SW-ACCT-NAME                 PIC X(30).              ECS021T3
121707         16  SW-ACCT-STATUS               PIC X.
00344          16  SW-GA-NAME                   PIC X(30).                 CL**3
00345          16  SW-STATE-NAME                PIC X(25).                 CL**4
00346          16  SW-PROD-DATE                 PIC X(6).               ECS021T3
00347                                                                   ECS021T3
00348      12  SW-RECORD-DATA            OCCURS 23.                        CL**6
00349          16  SW-BENEFIT-TYPE              PIC X.                  ECS021T3
00350          16  SW-BENEFIT-CODE              PIC 99.                 ECS021T3
00351          16  SW-BEN-TBL-POS               PIC S999 COMP.          ECS021T3
00352                                                                   ECS021T3
00353          16  SW-PERIOD    COMP-3   OCCURS 15.                     ECS021T3
00354              24  SW-ISS-CNT               PIC S9(7).              ECS021T3
00355              24  SW-CNC-CNT               PIC S9(7).              ECS021T3
00356              24  SW-ISS-PREM              PIC S9(11)V99.          ECS021T3
00357              24  SW-CNC-PREM              PIC S9(11)V99.          ECS021T3
00358              24  SW-NET-COMPEN            PIC S9(11)V99.          ECS021T3
00359              24  SW-CLM-CNT               PIC S9(7).              ECS021T3
00360              24  SW-CLM-AMT               PIC S9(11)V99.          ECS021T3
00361              24  SW-LOSS-RESV             PIC S9(11)V99.          ECS021T3
00362              24  SW-PRM-EARND             PIC S9(9)V99.           ECS021T3
00363              24  SW-PRM-INFRC             PIC S9(9)V99.           ECS021T3
00364              24  SW-INFRC-CNT             PIC S9(9).              ECS021T3
00365              24  SW-AVG-AGE               PIC S9(9).                 CL**6
00366              24  SW-AVG-ORG-TRM           PIC S9(9).                 CL**6
00367              24  SW-WGHT-AGE              PIC S9(9).                 CL**6
00368              24  SW-WGHT-ORG-TRM          PIC S9(9).                 CL**6
00369              24  SW-EXP-PCT               PIC S999V9(4).          ECS021T3
00370              24  SW-ADDED-TO-CNT          PIC S9(5).              ECS021T3
00371                                                                   ECS021T3
00372      EJECT                                                        ECS021T3
00373  01  LINK-TABLE.                                                  ECS021T3
092602     12  LINK-ACCUMULATORS   OCCURS  900 TIMES.                      CL**6
00375          16  LINK-BENEFIT-TYPE        PIC X.                      ECS021T3
00376          16  LINK-BENEFIT-CODE        PIC 99.                     ECS021T3
00377          16  LINK-PERIOD    OCCURS   15  TIMES  COMP-3.           ECS021T3
00378              20  LINK-ISS-CNT         PIC S9(7).                  ECS021T3
00379              20  LINK-CNC-CNT         PIC S9(7).                  ECS021T3
00380              20  LINK-ISS-PREM        PIC S9(11)V99.              ECS021T3
00381              20  LINK-CNC-PREM        PIC S9(11)V99.              ECS021T3
00382              20  LINK-NET-COMPEN      PIC S9(11)V99.              ECS021T3
00383              20  LINK-CLM-CNT         PIC S9(7).                  ECS021T3
00384              20  LINK-CLM-AMT         PIC S9(11)V99.              ECS021T3
00385              20  LINK-LOSS-RESV       PIC S9(11)V99.              ECS021T3
00386              20  LINK-EARND-PREM      PIC S9(9)V99.               ECS021T3
00387              20  LINK-PRM-INFRC       PIC S9(9)V99.               ECS021T3
00388              20  LINK-INFRC-CNT       PIC S9(9).                  ECS021T3
00389              20  LINK-AVG-AGE         PIC S9(9).                     CL**6
00390              20  LINK-AVG-ORG-TRM     PIC S9(9).                     CL**6
00391              20  LINK-WGHT-AGE        PIC S9(9).                     CL**6
00392              20  LINK-WGHT-ORG-TRM    PIC S9(9).                     CL**6
00393              20  LINK-EXP-PCT         PIC S999V9(4).              ECS021T3
00394              20  LINK-ADDED-TO-CNT    PIC S9(5).                  ECS021T3
00395                                                                   ECS021T3
092602*01  LINK-EXTENSION-1.                                            ECS021T3
00397 *    12  LINK-TBL-EXTENSION-1    OCCURS   75 TIMES.                  CL**6
00398 *        16  FILLER                       PIC X.                  ECS021T3
00399 *        16  FILLER                       PIC 99.                 ECS021T3
00400 *        16  LINK-EXT-1          OCCURS   15  TIMES  COMP-3.      ECS021T3
00401 *            20  FILLER               PIC S9(7).                  ECS021T3
00402 *            20  FILLER               PIC S9(7).                  ECS021T3
00403 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00404 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00405 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00406 *            20  FILLER               PIC S9(7).                  ECS021T3
00407 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00408 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00409 *            20  FILLER               PIC S9(9)V99.               ECS021T3
00410 *            20  FILLER               PIC S9(9)V99.               ECS021T3
00411 *            20  FILLER               PIC S9(9).                  ECS021T3
00412 *            20  FILLER               PIC S9(9).                     CL**6
00413 *            20  FILLER               PIC S9(9).                     CL**6
00414 *            20  FILLER               PIC S9(9).                     CL**6
00415 *            20  FILLER               PIC S9(9).                     CL**6
00416 *            20  FILLER               PIC S9(3)V9(4).             ECS021T3
00417 *            20  FILLER               PIC S9(5).                  ECS021T3
00418 *                                                                 ECS021T3
00419 *01  LINK-EXTENSION-2.                                            ECS021T3
00420 *    12  LINK-TBL-EXTENSION-2    OCCURS   75 TIMES.                  CL**6
00421 *        16  FILLER                       PIC X.                  ECS021T3
00422 *        16  FILLER                       PIC 99.                 ECS021T3
00423 *        16  LINK-EXT-2          OCCURS   15  TIMES  COMP-3.      ECS021T3
00424 *            20  FILLER               PIC S9(7).                  ECS021T3
00425 *            20  FILLER               PIC S9(7).                  ECS021T3
00426 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00427 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00428 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00429 *            20  FILLER               PIC S9(7).                  ECS021T3
00430 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00431 *            20  FILLER               PIC S9(11)V99.              ECS021T3
00432 *            20  FILLER               PIC S9(9)V99.               ECS021T3
00433 *            20  FILLER               PIC S9(9)V99.               ECS021T3
00434 *            20  FILLER               PIC S9(9).                  ECS021T3
00435 *            20  FILLER               PIC S9(9).                     CL**6
00436 *            20  FILLER               PIC S9(9).                     CL**6
00437 *            20  FILLER               PIC S9(9).                     CL**6
00438 *            20  FILLER               PIC S9(9).                     CL**6
00439 *            20  FILLER               PIC S9(3)V9(4).                CL**6
00440 *            20  FILLER               PIC S9(5).                     CL**6
00441 *                                                                    CL**6
00442 *01  LINK-EXTENSION-3.                                               CL**6
00443 *    12  LINK-TBL-EXTENSION-3    OCCURS   75 TIMES.                  CL**6
00444 *        16  FILLER                       PIC X.                     CL**6
00445 *        16  FILLER                       PIC 99.                    CL**6
00446 *        16  LINK-EXT-3          OCCURS   15  TIMES  COMP-3.         CL**6
00447 *            20  FILLER               PIC S9(7).                     CL**2
00448 *            20  FILLER               PIC S9(7).                     CL**2
00449 *            20  FILLER               PIC S9(11)V99.                 CL**6
00450 *            20  FILLER               PIC S9(11)V99.                 CL**6
00451 *            20  FILLER               PIC S9(11)V99.                 CL**6
00452 *            20  FILLER               PIC S9(7).                     CL**2
00453 *            20  FILLER               PIC S9(11)V99.                 CL**6
00454 *            20  FILLER               PIC S9(11)V99.                 CL**6
00455 *            20  FILLER               PIC S9(9)V99.                  CL**6
00456 *            20  FILLER               PIC S9(9)V99.                  CL**6
00457 *            20  FILLER               PIC S9(9).                     CL**6
00458 *            20  FILLER               PIC S9(9).                     CL**6
00459 *            20  FILLER               PIC S9(9).                     CL**6
00460 *            20  FILLER               PIC S9(9).                     CL**6
00461 *            20  FILLER               PIC S9(9).                     CL**6
00462 *            20  FILLER               PIC S9(3)V9(4).             ECS021T3
092602*            20  FILLER               PIC S9(5).                  ECS021T3
00464                                                                   ECS021T3
00465                                                                   ECS021T3
00466      EJECT                                                        ECS021T3
00467  01  LINK-TOTAL-TABLE.                                            ECS021T3
00468      12  LINK-TOTALS     OCCURS  3 TIMES.                         ECS021T3
00469          16  LINK-T-PERIOD  COMP-3 OCCURS 15 TIMES.               ECS021T3
00470              20  LINK-T-ISS-CNT       PIC S9(7).                  ECS021T3
00471              20  LINK-T-CNC-CNT       PIC S9(7).                  ECS021T3
00472              20  LINK-T-SINGLE-ELEM   PIC S9(7).                  ECS021T3
00473              20  LINK-T-JOINT-RETRO   PIC S9(7).                  ECS021T3
00474              20  LINK-T-LIFE-LEVEL    PIC S9(7).                  ECS021T3
00475              20  LINK-T-ISS-PREM      PIC S9(11)V99.              ECS021T3
00476              20  LINK-T-CNC-PREM      PIC S9(11)V99.              ECS021T3
00477              20  LINK-T-NET-COMPEN    PIC S9(11)V99.              ECS021T3
00478              20  LINK-T-LF-CLM-CNT    PIC S9(7).                  ECS021T3
00479              20  LINK-T-LF-CLM-AMT    PIC S9(11)V99.              ECS021T3
00480              20  LINK-T-AH-CLM-CNT    PIC S9(7).                  ECS021T3
00481              20  LINK-T-AH-CLM-AMT    PIC S9(11)V99.              ECS021T3
00482              20  LINK-T-LOSS-RESV     PIC S9(11)V99.              ECS021T3
00483              20  LINK-T-EARND-PREM    PIC S9(9)V99.               ECS021T3
00484              20  LINK-T-PRM-INFRC     PIC S9(9)V99.               ECS021T3
00485              20  LINK-T-INFRC-CNT     PIC S9(9).                  ECS021T3
00486              20  LINK-T-AVG-AGE       PIC S9(9).                     CL**6
00487              20  LINK-T-AVG-ORG-TRM   PIC S9(9).                     CL**6
00488              20  LINK-T-WGHT-AGE      PIC S9(9).                     CL**6
00489              20  LINK-T-WGHT-ORG-TRM  PIC S9(9).                     CL**6
00490              20  LINK-T-EXP-PCT       PIC S999V9(4).              ECS021T3
00491              20  LINK-T-ADDED-TO-CNT  PIC S9(5).                  ECS021T3
00492                                                                   ECS021T3
00493      EJECT                                                        ECS021T3
00494  01  CLASIC-SYSTEM-CODES.                                         ECS021T3
00495      12  DTE-COLC-ID                 PIC  X(4).                   ECS021T3
00496      12  DTE-CLASIC-COMPANY-CD       PIC  X.                      ECS021T3
00497      12  DTE-CLASIC-COMPANY-NUMBER   PIC  999.                    ECS021T3
00498      12  DTE-CLASIC-CLAIM-ACCESS     PIC  X.                      ECS021T3
00499      12  CLASIC-REIN-MAINT           PIC  XX.                     ECS021T3
00500      12  CLASIC-COMP-MAINT           PIC  XX.                     ECS021T3
00501      12  CLASIC-ACCT-MAINT           PIC  XX.                     ECS021T3
00502      12  CLASIC-CTBL-MAINT           PIC  XX.                     ECS021T3
00503      12  CLASIC-RATE-MAINT           PIC  XX.                     ECS021T3
00504      12  CLASIC-CREDIT-EOM-DT        PIC  XX.                     ECS021T3
00505      12  CLASIC-CLAIMS-EOM-DT        PIC  XX.                     ECS021T3
00506                                                                   ECS021T3
00507      12  LIFE-OVERRIDE-L1            PIC  X.                      ECS021T3
00508      12  LIFE-OVERRIDE-L2            PIC  XX.                     ECS021T3
00509      12  LIFE-OVERRIDE-L6            PIC  X(6).                   ECS021T3
00510      12  LIFE-OVERRIDE-L12           PIC  X(12).                  ECS021T3
00511                                                                   ECS021T3
00512      12  AH-OVERRIDE-L1              PIC  X.                      ECS021T3
00513      12  AH-OVERRIDE-L2              PIC  XX.                     ECS021T3
00514      12  AH-OVERRIDE-L6              PIC  X(6).                   ECS021T3
00515      12  AH-OVERRIDE-L12             PIC  X(12).                  ECS021T3
00516                                                                   ECS021T3
00517      12  CLAS-REPORT-CD1-CAPTION     PIC  X(10).                  ECS021T3
00518      12  CLAS-REPORT-CD2-CAPTION     PIC  X(10).                  ECS021T3
00519                                                                   ECS021T3
00520      12  CLASIC-MORTG-EOM-DT         PIC  XX.                     ECS021T3
00521      12  CLASIC-AR-EOM-DT            PIC  XX.                     ECS021T3
00522                                                                   ECS021T3
00523      12  FILLER                      PIC  X(11).                  ECS021T3
00524                                                                   ECS021T3
00525                                                                   ECS021T3
00526  01  CLAS-INS-TYPES.                                              ECS021T3
092602     12 CLAS-ALL-TYPES               OCCURS 900 TIMES.            ECS021T3
00528          16  CLAS-I-BEN              PIC  XX.                     ECS021T3
00529          16  CLAS-I-AB3.                                          ECS021T3
00530              20  FILLER              PIC  X.                      ECS021T3
00531              20  CLAS-I-AB2.                                      ECS021T3
00532                  24  FILLER          PIC  X.                      ECS021T3
00533                  24  CLAS-I-AB1      PIC  X.                      ECS021T3
00534          16  CLAS-I-AB10.                                         ECS021T3
00535              20  FILLER              PIC  X(9).                   ECS021T3
00536              20  CLAS-I-REIN-YN      PIC  X.                      ECS021T3
00537          16  CLAS-I-COMMENT          PIC  X(10).                  ECS021T3
00538          16  CLAS-I-JOINT            PIC  X.                      ECS021T3
00539          16  CLAS-I-RL-AH            PIC  X.                      ECS021T3
00540          16  CLAS-I-CALC-TYPE.                                    ECS021T3
00541              20  CLAS-I-BAL          PIC  X.                      ECS021T3
00542          16  CLAS-I-EP               PIC  X.                      ECS021T3
00543          16  FILLER                  PIC  X(9).                   ECS021T3
00544                                                                   ECS021T3
00545  01  CLAS-INDEX-TBL.                                              ECS021T3
00546      12  CLAX-ID                     PIC X(4).                    ECS021T3
00547      12  CLAS-STARTC                 PIC S9(4) COMP.              ECS021T3
00548      12  CLAS-MAXC                   PIC S9(4) COMP.              ECS021T3
00549      12  CLAS-STARTL                 PIC S9(4) COMP.              ECS021T3
00550      12  CLAS-MAXL                   PIC S9(4) COMP.              ECS021T3
00551      12  CLAS-STARTA                 PIC S9(4) COMP.              ECS021T3
00552      12  CLAS-MAXA                   PIC S9(4) COMP.              ECS021T3
00553      12  CLAS-STARTM                 PIC S9(4) COMP.              ECS021T3
00554      12  CLAS-MAXM                   PIC S9(4) COMP.              ECS021T3
00555      12  CLAS-STARTB                 PIC S9(4) COMP.              ECS021T3
00556      12  CLAS-MAXB                   PIC S9(4) COMP.              ECS021T3
00557      12  CLAS-STARTS                 PIC S9(4) COMP.              ECS021T3
00558      12  CLAS-MAXS                   PIC S9(4) COMP.              ECS021T3
00559      12  CLAS-STARTE                 PIC S9(4) COMP.              ECS021T3
00560      12  CLAS-MAXE                   PIC S9(4) COMP.              ECS021T3
00561      12  CLAS-STARTCN                PIC S9(4) COMP.              ECS021T3
00562      12  CLAS-MAXCN                  PIC S9(4) COMP.              ECS021T3
00563                                                                   ECS021T3
00564      EJECT                                                        ECS021T3
00565  PROCEDURE DIVISION USING REQUEST-TABLE                           ECS021T3
00566                           SW-RECORD                               ECS021T3
00567                           LINK-TABLE                              ECS021T3
092602*                         LINK-EXTENSION-1                        ECS021T3
092602*                         LINK-EXTENSION-2                        ECS021T3
092602*                         LINK-EXTENSION-3                           CL**6
00571                           LINK-TOTAL-TABLE                        ECS021T3
00572                           CLASIC-SYSTEM-CODES                     ECS021T3
00573                           CLAS-INS-TYPES                          ECS021T3
00574                           CLAS-INDEX-TBL.                         ECS021T3
00575                                                                   ECS021T3
00576  1000-PROCESS-REQUESTS.                                           ECS021T3
00577                                                                   ECS021T3
00578      PERFORM 2000-READ-REQUEST-TABLE THRU 2000-EXIT               ECS021T3
00579          VARYING REQ-IDX  FROM  ONE BY ONE                        ECS021T3
00580              UNTIL REQ-IDX GREATER THAN NUMBER-OF-REQUESTS.       ECS021T3
00581                                                                   ECS021T3
00582      GOBACK.                                                      ECS021T3
00583                                                                   ECS021T3
00584  1000-EXIT.                                                       ECS021T3
00585      EXIT.                                                        ECS021T3
00586                                                                   ECS021T3
00587  2000-READ-REQUEST-TABLE.                                         ECS021T3
00588                                                                   ECS021T3
00589      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-BUILD-ZERO-TABLE     ECS021T3
00590          PERFORM 9000-INITIALIZE-ZERO-TABLE THRU 9000-EXIT        ECS021T3
00591          PERFORM 8000-ZERO-ENTIRE-TABLE THRU 8000-EXIT            ECS021T3
00592          GO TO 2000-EXIT.                                         ECS021T3
00593                                                                   ECS021T3
00594      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-ZERO-TABLE           ECS021T3
00595          PERFORM 8000-ZERO-ENTIRE-TABLE THRU 8000-EXIT            ECS021T3
00596          GO TO 2000-EXIT.                                         ECS021T3
00597                                                                   ECS021T3
00598      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-APPLY-SORT-RCD       ECS021T3
00599          PERFORM 5000-APPLY-SORT-RECORD THRU 5000-EXIT            ECS021T3
00600          GO TO 2000-EXIT.                                         ECS021T3
00601                                                                   ECS021T3
00602      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-ADD-LINK-TABLES      ECS021T3
00603          PERFORM 3000-ADD-TABLE-RTN THRU 3000-EXIT                ECS021T3
00604          PERFORM 4000-ADD-TOTAL-RTN THRU 4000-EXIT                ECS021T3
00605          GO TO 2000-EXIT.                                         ECS021T3
00606                                                                   ECS021T3
00607      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-MOVE-TO-LINK         ECS021T3
00608          PERFORM 7000-MOVE-TABLES THRU 7000-EXIT.                 ECS021T3
00609                                                                   ECS021T3
00610  2000-EXIT.                                                       ECS021T3
00611      EXIT.                                                        ECS021T3
00612                                                                   ECS021T3
00613      EJECT                                                        ECS021T3
00614 *******************************************                       ECS021T3
00615 *  (2000) ADDS TABLE (2) TO LINK TABLE    *                       ECS021T3
00616 *******************************************                       ECS021T3
00617                                                                   ECS021T3
00618  3000-ADD-TABLE-RTN.                                              ECS021T3
00619      MOVE +1                     TO BEN-IDX.                      ECS021T3
00620                                                                   ECS021T3
00621  3000-ZERO-ACCESS-LOOP.                                           ECS021T3
00622      IF BEN-IDX GREATER CLAS-MAXA                                 ECS021T3
00623          GO TO 3000-EXIT.                                         ECS021T3
00624                                                                   ECS021T3
00625      IF LINK-BENEFIT-CODE (BEN-IDX) = ZERO                        ECS021T3
00626          ADD +1                  TO BEN-IDX                       ECS021T3
00627          GO TO 3000-ZERO-ACCESS-LOOP.                             ECS021T3
00628                                                                   ECS021T3
00629      MOVE LINK-BENEFIT-TYPE (BEN-IDX)                             ECS021T3
00630                                  TO TBL-BENEFIT-TYPE (BEN-IDX).   ECS021T3
00631      MOVE LINK-BENEFIT-CODE (BEN-IDX)                             ECS021T3
00632                                  TO TBL-BENEFIT-CODE (BEN-IDX).   ECS021T3
00633                                                                   ECS021T3
00634      MOVE +1                     TO DTE-IDX.                      ECS021T3
00635                                                                   ECS021T3
00636  3000-ACTIVE-BENEFIT-LOOP.                                        ECS021T3
00637                                                                   ECS021T3
00638      ADD LINK-ISS-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T3
00639                               TBL-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T3
00640      ADD LINK-CNC-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T3
00641                               TBL-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T3
00642      ADD LINK-ISS-PREM    (BEN-IDX DTE-IDX) TO                    ECS021T3
00643                               TBL-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T3
00644      ADD LINK-CNC-PREM    (BEN-IDX DTE-IDX) TO                    ECS021T3
00645                               TBL-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T3
00646      ADD LINK-NET-COMPEN  (BEN-IDX DTE-IDX) TO                    ECS021T3
00647                               TBL-NET-COMPEN   (BEN-IDX DTE-IDX). ECS021T3
00648      ADD LINK-CLM-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T3
00649                               TBL-CLM-CNT      (BEN-IDX DTE-IDX). ECS021T3
00650      ADD LINK-CLM-AMT     (BEN-IDX DTE-IDX) TO                    ECS021T3
00651                               TBL-CLM-AMT      (BEN-IDX DTE-IDX). ECS021T3
00652      ADD LINK-LOSS-RESV   (BEN-IDX DTE-IDX) TO                    ECS021T3
00653                               TBL-LOSS-RESV    (BEN-IDX DTE-IDX). ECS021T3
00654      ADD LINK-EARND-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T3
00655                               TBL-EARND-PREM   (BEN-IDX DTE-IDX). ECS021T3
00656      ADD LINK-PRM-INFRC   (BEN-IDX DTE-IDX) TO                    ECS021T3
00657                               TBL-PRM-INFRC    (BEN-IDX DTE-IDX). ECS021T3
00658      ADD LINK-INFRC-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T3
00659                               TBL-INFRC-CNT    (BEN-IDX DTE-IDX). ECS021T3
00660      ADD LINK-AVG-AGE      (BEN-IDX DTE-IDX) TO                   ECS021T3
00661                               TBL-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T3
00662      ADD LINK-AVG-ORG-TRM (BEN-IDX DTE-IDX) TO                    ECS021T3
00663                               TBL-AVG-ORG-TRM (BEN-IDX DTE-IDX).  ECS021T3
00664      ADD LINK-WGHT-AGE    (BEN-IDX DTE-IDX) TO                    ECS021T3
00665                               TBL-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T3
00666      ADD LINK-WGHT-ORG-TRM(BEN-IDX DTE-IDX) TO                    ECS021T3
00667                               TBL-WGHT-ORG-TRM(BEN-IDX DTE-IDX).  ECS021T3
00668      ADD LINK-EXP-PCT     (BEN-IDX DTE-IDX) TO                    ECS021T3
00669                               TBL-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T3
00670      ADD LINK-ADDED-TO-CNT(BEN-IDX DTE-IDX) TO                    ECS021T3
00671                               TBL-ADDED-TO-CNT(BEN-IDX DTE-IDX).  ECS021T3
00672                                                                   ECS021T3
00673      ADD +1 TO DTE-IDX.                                           ECS021T3
00674                                                                   ECS021T3
00675      IF DTE-IDX LESS +16                                          ECS021T3
00676          GO TO 3000-ACTIVE-BENEFIT-LOOP                           ECS021T3
00677      ELSE                                                         ECS021T3
00678          ADD +1 TO BEN-IDX                                        ECS021T3
00679          GO TO 3000-ZERO-ACCESS-LOOP.                             ECS021T3
00680                                                                   ECS021T3
00681  3000-EXIT.                                                       ECS021T3
00682       EXIT.                                                       ECS021T3
00683                                                                   ECS021T3
00684      EJECT                                                        ECS021T3
00685 *******************************************************           ECS021T3
00686 *  (3050) DUMPS TOTAL TABLE (2) INTO TOTAL TABLE (1)  *           ECS021T3
00687 *******************************************************           ECS021T3
00688  4000-ADD-TOTAL-RTN.                                              ECS021T3
00689      MOVE +1                     TO BEN-IDX                       ECS021T3
00690                                     DTE-IDX.                      ECS021T3
00691                                                                   ECS021T3
00692  4000-TOTAL-BENEFITS-LOOP.                                        ECS021T3
00693      ADD LINK-T-ISS-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T3
00694                               TOT-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T3
00695      ADD LINK-T-CNC-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T3
00696                               TOT-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T3
00697      ADD LINK-T-SINGLE-ELEM (BEN-IDX DTE-IDX) TO                  ECS021T3
00698                               TOT-SINGLE-ELEM (BEN-IDX DTE-IDX).     CL**2
00699      ADD LINK-T-JOINT-RETRO (BEN-IDX DTE-IDX) TO                  ECS021T3
00700                               TOT-JOINT-RETRO (BEN-IDX DTE-IDX).     CL**2
00701      ADD LINK-T-LIFE-LEVEL (BEN-IDX DTE-IDX) TO                   ECS021T3
00702                               TOT-LIFE-LEVEL (BEN-IDX DTE-IDX).   ECS021T3
00703      ADD LINK-T-ISS-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T3
00704                               TOT-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T3
00705      ADD LINK-T-CNC-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T3
00706                               TOT-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T3
00707      ADD LINK-T-NET-COMPEN (BEN-IDX DTE-IDX) TO                   ECS021T3
00708                               TOT-NET-COMPEN (BEN-IDX DTE-IDX).   ECS021T3
00709      ADD LINK-T-LF-CLM-CNT (BEN-IDX DTE-IDX) TO                   ECS021T3
00710                               TOT-LF-CLM-CNT (BEN-IDX DTE-IDX).   ECS021T3
00711      ADD LINK-T-LF-CLM-AMT (BEN-IDX DTE-IDX) TO                   ECS021T3
00712                               TOT-LF-CLM-AMT (BEN-IDX DTE-IDX).   ECS021T3
00713      ADD LINK-T-AH-CLM-CNT (BEN-IDX DTE-IDX) TO                   ECS021T3
00714                               TOT-AH-CLM-CNT (BEN-IDX DTE-IDX).   ECS021T3
00715      ADD LINK-T-AH-CLM-AMT (BEN-IDX DTE-IDX) TO                   ECS021T3
00716                               TOT-AH-CLM-AMT (BEN-IDX DTE-IDX).   ECS021T3
00717      ADD LINK-T-LOSS-RESV (BEN-IDX DTE-IDX) TO                    ECS021T3
00718                               TOT-LOSS-RESV (BEN-IDX DTE-IDX).    ECS021T3
00719      ADD LINK-T-EARND-PREM (BEN-IDX DTE-IDX) TO                   ECS021T3
00720                               TOT-EARND-PREM (BEN-IDX DTE-IDX).   ECS021T3
00721      ADD LINK-T-PRM-INFRC (BEN-IDX DTE-IDX) TO                    ECS021T3
00722                               TOT-PRM-INFRC (BEN-IDX DTE-IDX).    ECS021T3
00723      ADD LINK-T-INFRC-CNT (BEN-IDX DTE-IDX) TO                    ECS021T3
00724                               TOT-INFRC-CNT (BEN-IDX DTE-IDX).    ECS021T3
00725      ADD LINK-T-AVG-AGE    (BEN-IDX DTE-IDX) TO                   ECS021T3
00726                               TOT-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T3
00727      ADD LINK-T-AVG-ORG-TRM (BEN-IDX DTE-IDX) TO                  ECS021T3
00728                               TOT-AVG-ORG-TRM (BEN-IDX DTE-IDX).     CL**2
00729      ADD LINK-T-WGHT-AGE  (BEN-IDX DTE-IDX) TO                    ECS021T3
00730                               TOT-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T3
00731      ADD LINK-T-WGHT-ORG-TRM(BEN-IDX DTE-IDX) TO                  ECS021T3
00732                               TOT-WGHT-ORG-TRM(BEN-IDX DTE-IDX).     CL**2
00733      ADD LINK-T-EXP-PCT   (BEN-IDX DTE-IDX) TO                    ECS021T3
00734                               TOT-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T3
00735      ADD LINK-T-ADDED-TO-CNT(BEN-IDX DTE-IDX) TO                  ECS021T3
00736                               TOT-ADDED-TO-CNT(BEN-IDX DTE-IDX).     CL**2
00737                                                                   ECS021T3
00738      ADD +1 TO DTE-IDX.                                           ECS021T3
00739                                                                   ECS021T3
00740      IF DTE-IDX LESS +16                                          ECS021T3
00741          GO TO 4000-TOTAL-BENEFITS-LOOP.                          ECS021T3
00742                                                                   ECS021T3
00743      IF BEN-IDX LESS +3                                           ECS021T3
00744          ADD +1                  TO BEN-IDX                       ECS021T3
00745          MOVE +1                 TO DTE-IDX                       ECS021T3
00746          GO TO 4000-TOTAL-BENEFITS-LOOP.                          ECS021T3
00747                                                                   ECS021T3
00748  4000-EXIT.                                                       ECS021T3
00749       EXIT.                                                       ECS021T3
00750                                                                   ECS021T3
00751      EJECT                                                        ECS021T3
00752 **************************************************************    ECS021T3
00753 *  THE FOLLOWING INDEXES ARE USED IN ROUTINES 5000 AND 6000  *    ECS021T3
00754 *                                                            *    ECS021T3
00755 *  1. BRK-IDX - USED TO INDEX BENEFIT LEVEL OF SORT RECORD   *    ECS021T3
00756 *                 VARIED FROM 1 TO 23                        *       CL**6
00757 *  2. BEN-IDX - USED TO INDEX BENEFIT LEVEL OF ACCUMULATED   *    ECS021T3
00758 *                 BREAK TABLES VARIED FROM 1 TO 75           *       CL**6
00759 *  3. TOT-IDX - USED TO INDEX BENEFIT TYPE OF TOTALS         *    ECS021T3
00760 *                 TABLES VARIED FROM 1 TO 2                  *    ECS021T3
00761 *  4. DTE-IDX - USED TO INDEX DATE RANGES OF ALL TABLES      *    ECS021T3
00762 *                 AND RECORDS VARIED FROM 1 TO 15            *    ECS021T3
00763 **************************************************************    ECS021T3
00764                                                                   ECS021T3
00765  5000-APPLY-SORT-RECORD.                                          ECS021T3
00766      MOVE +1                     TO BRK-IDX.                      ECS021T3
00767                                                                   ECS021T3
00768  5000-BENEFIT-LOOP.                                               ECS021T3
00769      IF SW-BENEFIT-CODE   (BRK-IDX) = ZEROS                       ECS021T3
00770          GO TO 5000-EXIT.                                         ECS021T3
00771                                                                   ECS021T3
00772      MOVE SW-BEN-TBL-POS  (BRK-IDX)                               ECS021T3
00773                                  TO BEN-IDX.                      ECS021T3
00774      MOVE SW-BENEFIT-CODE (BRK-IDX)                               ECS021T3
00775                                  TO TBL-BENEFIT-CODE (BEN-IDX).   ECS021T3
00776      MOVE SW-BENEFIT-TYPE (BRK-IDX)                               ECS021T3
00777                                  TO TBL-BENEFIT-TYPE (BEN-IDX).   ECS021T3
00778      MOVE +1                     TO DTE-IDX.                      ECS021T3
00779                                                                   ECS021T3
00780  5000-DATE-RANGE-LOOP.                                            ECS021T3
00781      ADD SW-ISS-CNT     (BRK-IDX DTE-IDX)                         ECS021T3
00782                            TO TBL-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T3
00783      ADD SW-CNC-CNT     (BRK-IDX DTE-IDX)                         ECS021T3
00784                            TO TBL-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T3
00785      ADD SW-ISS-PREM    (BRK-IDX DTE-IDX)                         ECS021T3
00786                            TO TBL-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T3
00787      ADD SW-CNC-PREM    (BRK-IDX DTE-IDX)                         ECS021T3
00788                            TO TBL-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T3
00789      ADD SW-NET-COMPEN  (BRK-IDX DTE-IDX)                         ECS021T3
00790                            TO TBL-NET-COMPEN   (BEN-IDX DTE-IDX). ECS021T3
00791      ADD SW-CLM-CNT     (BRK-IDX DTE-IDX)                         ECS021T3
00792                            TO TBL-CLM-CNT      (BEN-IDX DTE-IDX). ECS021T3
00793      ADD SW-CLM-AMT     (BRK-IDX DTE-IDX)                         ECS021T3
00794                            TO TBL-CLM-AMT      (BEN-IDX DTE-IDX). ECS021T3
00795      ADD SW-LOSS-RESV   (BRK-IDX DTE-IDX)                         ECS021T3
00796                            TO TBL-LOSS-RESV    (BEN-IDX DTE-IDX). ECS021T3
00797      ADD SW-PRM-EARND   (BRK-IDX DTE-IDX)                         ECS021T3
00798                            TO TBL-EARND-PREM   (BEN-IDX DTE-IDX). ECS021T3
00799      ADD SW-PRM-INFRC   (BRK-IDX DTE-IDX)                         ECS021T3
00800                            TO TBL-PRM-INFRC    (BEN-IDX DTE-IDX). ECS021T3
00801      ADD SW-INFRC-CNT   (BRK-IDX DTE-IDX)                         ECS021T3
00802                            TO TBL-INFRC-CNT    (BEN-IDX DTE-IDX). ECS021T3
00803      ADD SW-AVG-AGE     (BRK-IDX DTE-IDX)                         ECS021T3
00804                            TO TBL-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T3
00805      ADD SW-AVG-ORG-TRM (BRK-IDX DTE-IDX)                         ECS021T3
00806                            TO TBL-AVG-ORG-TRM (BEN-IDX DTE-IDX).  ECS021T3
00807      ADD SW-WGHT-AGE    (BRK-IDX DTE-IDX)                         ECS021T3
00808                            TO TBL-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T3
00809      ADD SW-WGHT-ORG-TRM(BRK-IDX DTE-IDX)                         ECS021T3
00810                            TO TBL-WGHT-ORG-TRM(BEN-IDX DTE-IDX).  ECS021T3
00811      ADD SW-EXP-PCT     (BRK-IDX DTE-IDX)                         ECS021T3
00812                            TO TBL-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T3
00813      ADD SW-ADDED-TO-CNT(BRK-IDX DTE-IDX)                         ECS021T3
00814                            TO TBL-ADDED-TO-CNT(BEN-IDX DTE-IDX).  ECS021T3
00815                                                                   ECS021T3
00816      ADD +1                TO DTE-IDX.                            ECS021T3
00817                                                                   ECS021T3
00818      IF DTE-IDX LESS +16                                          ECS021T3
00819          GO TO 5000-DATE-RANGE-LOOP.                              ECS021T3
00820                                                                   ECS021T3
00821      PERFORM 6000-BENEFIT-TYPE-TOTAL THRU 6000-EXIT.              ECS021T3
00822                                                                   ECS021T3
00823      ADD +1                TO BRK-IDX.                            ECS021T3
00824                                                                   ECS021T3
00825      IF BRK-IDX LESS +24                                             CL**6
00826          GO TO 5000-BENEFIT-LOOP.                                 ECS021T3
00827                                                                   ECS021T3
00828  5000-EXIT.                                                       ECS021T3
00829       EXIT.                                                       ECS021T3
00830                                                                   ECS021T3
00831      EJECT                                                        ECS021T3
00832 **************************************************************    ECS021T3
00833 *  THE FOLLOWING INDEXES ARE USED IN ROUTINES 5000 AND 6000  *    ECS021T3
00834 *                                                            *    ECS021T3
00835 *  1. BRK-IDX - USED TO INDEX BENEFIT LEVEL OF SORT RECORD   *    ECS021T3
00836 *                 VARIED FROM 1 TO 23                        *       CL**6
00837 *  2. BEN-IDX - USED TO INDEX BENEFIT LEVEL OF ACCUMULATED   *    ECS021T3
00838 *                 BREAK TABLES VARIED FROM 1 TO 75           *       CL**6
00839 *  3. TOT-IDX - USED TO INDEX BENEFIT TYPE OF TOTALS         *    ECS021T3
00840 *                 TABLES VARIED FROM 1 TO 2                  *    ECS021T3
00841 *  4. DTE-IDX - USED TO INDEX DATE RANGES OF ALL TABLES      *    ECS021T3
00842 *                 AND RECORDS VARIED FROM 1 TO 15            *    ECS021T3
00843 **************************************************************    ECS021T3
00844                                                                   ECS021T3
00845  6000-BENEFIT-TYPE-TOTAL.                                         ECS021T3
00846                                                                   ECS021T3
00847      MOVE +1                     TO DTE-IDX.                      ECS021T3
00848                                                                   ECS021T3
00849  6000-DATE-RANGE-LOOP.                                            ECS021T3
00850                                                                   ECS021T3
00851      COMPUTE NET-COUNT = SW-ISS-CNT (BRK-IDX DTE-IDX) -              CL**5
00852          SW-CNC-CNT (BRK-IDX DTE-IDX).                               CL**5
00853                                                                      CL**5
00854      IF SW-BENEFIT-TYPE (BRK-IDX) = LIFE-OVERRIDE-L1              ECS021T3
00855          MOVE +1                 TO TOT-IDX                       ECS021T3
00856          IF CLAS-I-RL-AH (BEN-IDX) = 'R'                          ECS021T3
00857              IF CLAS-I-JOINT (BEN-IDX) = 'J'                      ECS021T3
00858                  ADD NET-COUNT TO                                    CL**5
00859                      TOT-JOINT-RETRO (TOT-IDX DTE-IDX)            ECS021T3
00860              ELSE                                                 ECS021T3
00861                  ADD NET-COUNT TO                                    CL**5
00862                      TOT-SINGLE-ELEM (TOT-IDX DTE-IDX)            ECS021T3
00863          ELSE                                                     ECS021T3
00864              ADD NET-COUNT TO                                        CL**5
00865                  TOT-LIFE-LEVEL (TOT-IDX DTE-IDX)                 ECS021T3
00866      ELSE                                                         ECS021T3
00867          MOVE +2                 TO TOT-IDX                       ECS021T3
00868          IF CLAS-I-AB1 (BEN-IDX) = 'E'                            ECS021T3
00869              ADD NET-COUNT TO                                        CL**5
00870                  TOT-SINGLE-ELEM (TOT-IDX DTE-IDX)                ECS021T3
00871          ELSE                                                     ECS021T3
00872              IF CLAS-I-AB1 (BEN-IDX) = 'R'                        ECS021T3
00873                  ADD NET-COUNT TO                                    CL**5
00874                      TOT-JOINT-RETRO (TOT-IDX DTE-IDX).           ECS021T3
00875                                                                   ECS021T3
00876      ADD SW-ISS-CNT    (BRK-IDX DTE-IDX)                          ECS021T3
00877                            TO TOT-ISS-CNT     (TOT-IDX DTE-IDX)   ECS021T3
00878                               TOT-ISS-CNT     (3 DTE-IDX).        ECS021T3
00879      ADD SW-CNC-CNT    (BRK-IDX DTE-IDX)                          ECS021T3
00880                            TO TOT-CNC-CNT     (TOT-IDX DTE-IDX)   ECS021T3
00881                               TOT-CNC-CNT     (3 DTE-IDX).        ECS021T3
00882      ADD SW-ISS-PREM   (BRK-IDX DTE-IDX)                          ECS021T3
00883                            TO TOT-ISS-PREM    (TOT-IDX DTE-IDX)   ECS021T3
00884                               TOT-ISS-PREM    (3 DTE-IDX).        ECS021T3
00885      ADD SW-CNC-PREM   (BRK-IDX DTE-IDX)                          ECS021T3
00886                            TO TOT-CNC-PREM    (TOT-IDX DTE-IDX)   ECS021T3
00887                               TOT-CNC-PREM    (3 DTE-IDX).        ECS021T3
00888      ADD SW-NET-COMPEN (BRK-IDX DTE-IDX)                          ECS021T3
00889                            TO TOT-NET-COMPEN (TOT-IDX DTE-IDX)    ECS021T3
00890                               TOT-NET-COMPEN (3 DTE-IDX).         ECS021T3
00891                                                                   ECS021T3
00892      IF SW-BENEFIT-TYPE (BRK-IDX) = LIFE-OVERRIDE-L1              ECS021T3
00893          ADD SW-CLM-CNT (BRK-IDX DTE-IDX)                         ECS021T3
00894                            TO TOT-LF-CLM-CNT   (TOT-IDX DTE-IDX)  ECS021T3
00895                               TOT-LF-CLM-CNT   (3 DTE-IDX)        ECS021T3
00896          ADD SW-CLM-AMT (BRK-IDX DTE-IDX)                         ECS021T3
00897                            TO TOT-LF-CLM-AMT   (TOT-IDX DTE-IDX)  ECS021T3
00898                               TOT-LF-CLM-AMT   (3 DTE-IDX).       ECS021T3
00899                                                                   ECS021T3
00900      IF SW-BENEFIT-TYPE (BRK-IDX) = AH-OVERRIDE-L1                ECS021T3
00901          ADD SW-CLM-CNT (BRK-IDX DTE-IDX)                         ECS021T3
00902                            TO TOT-AH-CLM-CNT   (TOT-IDX DTE-IDX)  ECS021T3
00903                               TOT-AH-CLM-CNT   (3 DTE-IDX)        ECS021T3
00904          ADD SW-CLM-AMT (BRK-IDX DTE-IDX)                         ECS021T3
00905                            TO TOT-AH-CLM-AMT   (TOT-IDX DTE-IDX)  ECS021T3
00906                               TOT-AH-CLM-AMT   (3 DTE-IDX).       ECS021T3
00907                                                                   ECS021T3
00908      ADD SW-LOSS-RESV   (BRK-IDX DTE-IDX)                         ECS021T3
00909                            TO TOT-LOSS-RESV    (TOT-IDX DTE-IDX)  ECS021T3
00910                               TOT-LOSS-RESV    (3 DTE-IDX).       ECS021T3
00911      ADD SW-PRM-EARND   (BRK-IDX DTE-IDX)                         ECS021T3
00912                            TO TOT-EARND-PREM   (TOT-IDX DTE-IDX)  ECS021T3
00913                               TOT-EARND-PREM   (3 DTE-IDX).       ECS021T3
00914      ADD SW-PRM-INFRC   (BRK-IDX DTE-IDX)                         ECS021T3
00915                            TO TOT-PRM-INFRC    (TOT-IDX DTE-IDX)  ECS021T3
00916                               TOT-PRM-INFRC    (3 DTE-IDX).       ECS021T3
00917      ADD SW-INFRC-CNT   (BRK-IDX DTE-IDX)                         ECS021T3
00918                            TO TOT-INFRC-CNT    (TOT-IDX DTE-IDX)  ECS021T3
00919                               TOT-INFRC-CNT    (3 DTE-IDX).       ECS021T3
00920      ADD SW-AVG-AGE     (BRK-IDX DTE-IDX)                         ECS021T3
00921                            TO TOT-AVG-AGE      (TOT-IDX DTE-IDX)  ECS021T3
00922                               TOT-AVG-AGE      (3 DTE-IDX).       ECS021T3
00923      ADD SW-AVG-ORG-TRM (BRK-IDX DTE-IDX)                         ECS021T3
00924                            TO TOT-AVG-ORG-TRM (TOT-IDX DTE-IDX)   ECS021T3
00925                               TOT-AVG-ORG-TRM (3 DTE-IDX).        ECS021T3
00926      ADD SW-WGHT-AGE    (BRK-IDX DTE-IDX)                         ECS021T3
00927                            TO TOT-WGHT-AGE     (TOT-IDX DTE-IDX)  ECS021T3
00928                               TOT-WGHT-AGE     (3 DTE-IDX).       ECS021T3
00929      ADD SW-WGHT-ORG-TRM(BRK-IDX DTE-IDX)                         ECS021T3
00930                            TO TOT-WGHT-ORG-TRM(TOT-IDX DTE-IDX)   ECS021T3
00931                               TOT-WGHT-ORG-TRM(3 DTE-IDX).        ECS021T3
00932      ADD SW-EXP-PCT     (BRK-IDX DTE-IDX)                         ECS021T3
00933                            TO TOT-EXP-PCT      (TOT-IDX DTE-IDX)  ECS021T3
00934                               TOT-EXP-PCT      (3 DTE-IDX).       ECS021T3
00935      ADD SW-ADDED-TO-CNT (BRK-IDX DTE-IDX)                        ECS021T3
00936                            TO TOT-ADDED-TO-CNT(TOT-IDX DTE-IDX)   ECS021T3
00937                               TOT-ADDED-TO-CNT(3 DTE-IDX).        ECS021T3
00938                                                                   ECS021T3
00939      ADD +1 TO DTE-IDX.                                           ECS021T3
00940                                                                   ECS021T3
00941      IF DTE-IDX LESS +16                                          ECS021T3
00942          GO TO 6000-DATE-RANGE-LOOP.                              ECS021T3
00943                                                                   ECS021T3
00944  6000-EXIT.                                                       ECS021T3
00945       EXIT.                                                       ECS021T3
00946                                                                   ECS021T3
00947      EJECT                                                        ECS021T3
00948  7000-MOVE-TABLES.                                                ECS021T3
00949                                                                   ECS021T3
00950      MOVE BREAK-TABLE            TO LINK-TABLE.                   ECS021T3
092602*    MOVE BREAK-EXTENSION-1      TO LINK-EXTENSION-1.             ECS021T3
092602*    MOVE BREAK-EXTENSION-2      TO LINK-EXTENSION-2.             ECS021T3
092602*    MOVE BREAK-EXTENSION-3      TO LINK-EXTENSION-3.                CL**6
00954      MOVE BREAK-TOTAL-TABLE      TO LINK-TOTAL-TABLE.             ECS021T3
00955                                                                   ECS021T3
00956  7000-EXIT.                                                       ECS021T3
00957       EXIT.                                                       ECS021T3
00958                                                                   ECS021T3
00959      EJECT                                                        ECS021T3
00960 ***********************************                               ECS021T3
00961 *     (8000) ZERO TABLE TWO       *                               ECS021T3
00962 ***********************************                               ECS021T3
00963                                                                   ECS021T3
00964  8000-ZERO-ENTIRE-TABLE.                                          ECS021T3
00965                                                                   ECS021T3
00966       MOVE +1                    TO BEN-IDX.                      ECS021T3
00967                                                                   ECS021T3
00968  8000-ZERO-BENEFIT-LOOP.                                          ECS021T3
00969                                                                   ECS021T3
00970       MOVE ZERO-TABLE            TO                               ECS021T3
00971                                  TABLE-ACCUMULATORS (BEN-IDX).    ECS021T3
00972                                                                   ECS021T3
092602*     IF BEN-IDX LESS THAN THREE-HUNDRED                          ECS021T3
092602      IF BEN-IDX LESS THAN  NINE-HUNDRED                          ECS021T3
00974           ADD +1                 TO BEN-IDX                       ECS021T3
00975               GO TO 8000-ZERO-BENEFIT-LOOP.                       ECS021T3
00976                                                                   ECS021T3
00977  8000-ZERO-TOTAL-TABLE.                                           ECS021T3
00978                                                                   ECS021T3
00979       MOVE +1                    TO TOT-IDX.                      ECS021T3
00980                                                                   ECS021T3
00981  8000-ZERO-TOTAL-LOOP.                                            ECS021T3
00982                                                                   ECS021T3
00983       MOVE ZERO-TOTAL-TABLE      TO TABLE-TOTALS (TOT-IDX).       ECS021T3
00984                                                                   ECS021T3
00985       IF TOT-IDX LESS THAN THREE                                  ECS021T3
00986           ADD +1                 TO TOT-IDX                       ECS021T3
00987               GO TO 8000-ZERO-TOTAL-LOOP.                         ECS021T3
00988                                                                   ECS021T3
00989  8000-EXIT.                                                       ECS021T3
00990      EXIT.                                                        ECS021T3
00991                                                                   ECS021T3
00992      EJECT                                                        ECS021T3
00993 *******************************************                       ECS021T3
00994 *     (9000) INITIALIZE ZERO-ACCUMS       *                       ECS021T3
00995 *******************************************                       ECS021T3
00996                                                                   ECS021T3
00997  9000-INITIALIZE-ZERO-TABLE.                                      ECS021T3
00998                                                                   ECS021T3
00999       MOVE +1                    TO DTE-IDX.                      ECS021T3
01000                                                                   ECS021T3
01001  9000-ZERO-DATE-LOOP.                                             ECS021T3
01002                                                                   ECS021T3
01003       MOVE ZERO-TABLE-ENTRIES    TO ZERO-PERIOD (DTE-IDX).        ECS021T3
01004                                                                   ECS021T3
01005       IF DTE-IDX LESS THAN SIXTEEN                                ECS021T3
01006           ADD +1                 TO DTE-IDX                       ECS021T3
01007               GO TO 9000-ZERO-DATE-LOOP.                          ECS021T3
01008                                                                   ECS021T3
01009       MOVE +1                    TO DTE-IDX.                      ECS021T3
01010                                                                   ECS021T3
01011  9000-ZERO-TOTAL-DATE-LOOP.                                       ECS021T3
01012                                                                   ECS021T3
01013       MOVE ZERO-TOTAL-ENTRY      TO ZERO-TOTAL-ACCUMS (DTE-IDX).  ECS021T3
01014                                                                   ECS021T3
01015       IF DTE-IDX LESS THAN SIXTEEN                                ECS021T3
01016           ADD +1                 TO DTE-IDX                       ECS021T3
01017               GO TO 9000-ZERO-TOTAL-DATE-LOOP.                    ECS021T3
01018                                                                   ECS021T3
01019  9000-EXIT.                                                       ECS021T3
01020      EXIT.                                                        ECS021T3
01021                                                                   ECS021T3
01022      EJECT                                                        ECS021T3
01023  ABEND-PGM SECTION.                                               ECS021T3
01024            COPY ELCABEND SUPPRESS.                                ECS021T3
