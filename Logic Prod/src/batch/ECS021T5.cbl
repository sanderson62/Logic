00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   ECS021T5
00003  PROGRAM-ID.                ECS021T5.                                LV009
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 11/28/95 11:03:45.                    CL**7
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             CL**8
00008 *                           VMOD=2.008.                              CL**9
00009                                                                   ECS021T5
00010                                                                   ECS021T5
00011 *AUTHOR.        LOGIC, INC.                                          CL**7
00012 *               DALLAS, TEXAS.                                       CL**7
00013                                                                   ECS021T5
00014                                                                   ECS021T5
00015 *DATE-COMPILED.                                                      CL**7
00016                                                                   ECS021T5
00017 *SECURITY.   *****************************************************   CL**7
00018 *            *                                                   *   CL**7
00019 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00020 *            *                                                   *   CL**7
00021 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00022 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00023 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00024 *            *                                                   *   CL**7
00025 *            *****************************************************   CL**7
00026                                                                   ECS021T5
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
00031                                                                   ECS021T5
00032  ENVIRONMENT DIVISION.                                            ECS021T5
00033  INPUT-OUTPUT SECTION.                                            ECS021T5
00034  FILE-CONTROL.                                                    ECS021T5
00035                                                                   ECS021T5
00036  EJECT                                                            ECS021T5
00037  DATA DIVISION.                                                   ECS021T5
00038  FILE SECTION.                                                    ECS021T5
00039                                                                   ECS021T5
00040                                                                   ECS021T5
00041  EJECT                                                            ECS021T5
00042  WORKING-STORAGE SECTION.                                         ECS021T5
00043  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**7
00044                                                                   ECS021T5
00045  77  FILLER  PIC X(32) VALUE '********************************'.  ECS021T5
00046  77  FILLER  PIC X(32) VALUE '     ECS021T5 WORKING STORAGE   '.  ECS021T5
00047  77  FILLER  PIC X(32) VALUE '********* VMOD=2.008 ***********'.     CL**8
00048                                                                   ECS021T5
00049  77  NET-COUNT               PIC S9(7)     COMP-3.                   CL**5
00050  77  DTE-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T5
00051  77  TOT-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T5
00052  77  BEN-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T5
00053  77  BRK-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T5
00054  77  REQ-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T5
00055  77  ONE                     PIC  99              VALUE 1.        ECS021T5
00056  77  THREE                   PIC  99              VALUE 3.        ECS021T5
00057  77  SIXTEEN                 PIC S9(4)     COMP   VALUE +16.      ECS021T5
00058  77  THREE-HUNDRED           PIC S9(4)     COMP   VALUE +300.     ECS021T5
092602 77  NINE-HUNDRED            PIC S9(4)     COMP   VALUE +900.     ECS021T5
00059                                                                   ECS021T5
00060  01  WS-ABEND-STORAGE.                                            ECS021T5
00061      12  WS-RETURN-CODE          PIC S9(4)  VALUE +0 COMP.        ECS021T5
00062      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         ECS021T5
00063      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          ECS021T5
00064      12  WS-ZERO                 PIC S9     VALUE +0 COMP-3.      ECS021T5
00065      12  WS-ABEND-CODE           PIC S9(4).                       ECS021T5
00066      12  WORK-ABEND-CODE  REDEFINES  WS-ABEND-CODE.               ECS021T5
00067          16  WAC-1               PIC X.                           ECS021T5
00068          16  WAC-2               PIC X.                           ECS021T5
00069          16  WAC-3-4             PIC 99.                          ECS021T5
00070                                                                   ECS021T5
00071  01  WS-PROGRAM-ACTIONS.                                          ECS021T5
00072      12  W-BUILD-ZERO-TABLE      PIC X(02) VALUE 'BZ'.            ECS021T5
00073      12  W-ZERO-TABLE            PIC X(02) VALUE 'ZT'.            ECS021T5
00074      12  W-APPLY-SORT-RCD        PIC X(02) VALUE 'AS'.            ECS021T5
00075      12  W-ADD-LINK-TABLES       PIC X(02) VALUE 'AT'.            ECS021T5
00076      12  W-MOVE-TO-LINK          PIC X(02) VALUE 'MT'.            ECS021T5
00077                                                                   ECS021T5
00078      EJECT                                                        ECS021T5
00079  01  TABLE-IDENTIFIER.                                            ECS021T5
00080      12  FILLER                       PIC X(28)                   ECS021T5
00081                        VALUE '**** START OF TABLE TWO ****'.      ECS021T5
00082                                                                   ECS021T5
00083  01  BREAK-TABLE.                                                 ECS021T5
092602     12  TABLE-ACCUMULATORS     OCCURS  900 TIMES.                   CL**6
00085          16  TBL-BENEFIT-TYPE             PIC X.                  ECS021T5
00086          16  TBL-BENEFIT-CODE             PIC XX.                 ECS021T5
00087          16  TBL-PERIOD     OCCURS   15  TIMES  COMP-3.           ECS021T5
00088              20  TBL-ISS-CNT          PIC S9(7).                  ECS021T5
00089              20  TBL-CNC-CNT          PIC S9(7).                  ECS021T5
00090              20  TBL-ISS-PREM         PIC S9(11)V99.              ECS021T5
00091              20  TBL-CNC-PREM         PIC S9(11)V99.              ECS021T5
00092              20  TBL-NET-COMPEN       PIC S9(11)V99.              ECS021T5
00093              20  TBL-CLM-CNT          PIC S9(7).                  ECS021T5
00094              20  TBL-CLM-AMT          PIC S9(11)V99.              ECS021T5
00095              20  TBL-LOSS-RESV        PIC S9(11)V99.              ECS021T5
00096              20  TBL-EARND-PREM       PIC S9(9)V99.               ECS021T5
00097              20  TBL-PRM-INFRC        PIC S9(9)V99.               ECS021T5
00098              20  TBL-INFRC-CNT        PIC S9(9).                  ECS021T5
00099              20  TBL-AVG-AGE          PIC S9(9).                     CL**6
00100              20  TBL-AVG-ORG-TRM      PIC S9(9).                     CL**6
00101              20  TBL-WGHT-AGE         PIC S9(9).                     CL**6
00102              20  TBL-WGHT-ORG-TRM     PIC S9(9).                     CL**6
00103              20  TBL-EXP-PCT          PIC S9(3)V9(4).             ECS021T5
00104              20  TBL-ADDED-TO-CNT     PIC S9(5).                  ECS021T5
00105                                                                   ECS021T5
00106      EJECT                                                        ECS021T5
092602*01  BREAK-EXTENSION-1.                                           ECS021T5
00108 *    12  TABLE-EXTENSION-1      OCCURS   75 TIMES.                   CL**6
00109 *        16  FILLER                       PIC X.                  ECS021T5
00110 *        16  FILLER                       PIC XX.                 ECS021T5
00111 *        16  TBL-EXTENSION-1    OCCURS   15  TIMES  COMP-3.       ECS021T5
00112 *            20  FILLER               PIC S9(7).                  ECS021T5
00113 *            20  FILLER               PIC S9(7).                  ECS021T5
00114 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00115 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00116 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00117 *            20  FILLER               PIC S9(7).                  ECS021T5
00118 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00119 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00120 *            20  FILLER               PIC S9(9)V99.               ECS021T5
00121 *            20  FILLER               PIC S9(9)V99.               ECS021T5
00122 *            20  FILLER               PIC S9(9).                  ECS021T5
00123 *            20  FILLER               PIC S9(9).                     CL**6
00124 *            20  FILLER               PIC S9(9).                     CL**6
00125 *            20  FILLER               PIC S9(9).                     CL**6
00126 *            20  FILLER               PIC S9(9).                     CL**6
00127 *            20  FILLER               PIC S9(3)V9(4).             ECS021T5
00128 *            20  FILLER               PIC S9(5).                  ECS021T5
00129 *                                                                 ECS021T5
00130 *    EJECT                                                        ECS021T5
00131 *01  BREAK-EXTENSION-2.                                           ECS021T5
00132 *    12  TABLE-EXTENSION-2      OCCURS   75 TIMES.                   CL**6
00133 *        16  FILLER                       PIC X.                  ECS021T5
00134 *        16  FILLER                       PIC XX.                 ECS021T5
00135 *        16  TBL-EXTENSION-2    OCCURS   15  TIMES  COMP-3.       ECS021T5
00136 *            20  FILLER               PIC S9(7).                  ECS021T5
00137 *            20  FILLER               PIC S9(7).                  ECS021T5
00138 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00139 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00140 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00141 *            20  FILLER               PIC S9(7).                  ECS021T5
00142 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00143 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00144 *            20  FILLER               PIC S9(9)V99.               ECS021T5
00145 *            20  FILLER               PIC S9(9)V99.               ECS021T5
00146 *            20  FILLER               PIC S9(9).                  ECS021T5
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
00175 *            20  FILLER               PIC S9(3)V9(4).             ECS021T5
092602*            20  FILLER               PIC S9(5).                  ECS021T5
00177                                                                   ECS021T5
00178      EJECT                                                        ECS021T5
00179  01  TOTAL-IDENTIFIER.                                            ECS021T5
00180      12  FILLER                       PIC X(28)                   ECS021T5
00181                        VALUE '**** START OF TOTAL TWO ****'.      ECS021T5
00182                                                                   ECS021T5
00183  01  BREAK-TOTAL-TABLE.                                           ECS021T5
00184      12  TABLE-TOTALS           OCCURS  3 TIMES.                  ECS021T5
00185          16  TOT-PERIOD     OCCURS   15  TIMES  COMP-3.           ECS021T5
00186              20  TOT-ISS-CNT          PIC S9(7).                  ECS021T5
00187              20  TOT-CNC-CNT          PIC S9(7).                  ECS021T5
00188              20  TOT-SINGLE-ELEM      PIC S9(7).                  ECS021T5
00189              20  TOT-JOINT-RETRO      PIC S9(7).                  ECS021T5
00190              20  TOT-LIFE-LEVEL       PIC S9(7).                  ECS021T5
00191              20  TOT-ISS-PREM         PIC S9(11)V99.              ECS021T5
00192              20  TOT-CNC-PREM         PIC S9(11)V99.              ECS021T5
00193              20  TOT-NET-COMPEN       PIC S9(11)V99.              ECS021T5
00194              20  TOT-LF-CLM-CNT       PIC S9(7).                  ECS021T5
00195              20  TOT-LF-CLM-AMT       PIC S9(11)V99.              ECS021T5
00196              20  TOT-AH-CLM-CNT       PIC S9(7).                  ECS021T5
00197              20  TOT-AH-CLM-AMT       PIC S9(11)V99.              ECS021T5
00198              20  TOT-LOSS-RESV        PIC S9(11)V99.              ECS021T5
00199              20  TOT-EARND-PREM       PIC S9(9)V99.               ECS021T5
00200              20  TOT-PRM-INFRC        PIC S9(9)V99.               ECS021T5
00201              20  TOT-INFRC-CNT        PIC S9(9).                  ECS021T5
00202              20  TOT-AVG-AGE          PIC S9(9).                     CL**6
00203              20  TOT-AVG-ORG-TRM      PIC S9(9).                     CL**6
00204              20  TOT-WGHT-AGE         PIC S9(9).                     CL**6
00205              20  TOT-WGHT-ORG-TRM     PIC S9(9).                     CL**6
00206              20  TOT-EXP-PCT          PIC S999V9(4).              ECS021T5
00207              20  TOT-ADDED-TO-CNT     PIC S9(5).                  ECS021T5
00208                                                                   ECS021T5
00209      EJECT                                                        ECS021T5
00210                                                                   ECS021T5
00211  01  ZERO-IDENTIFIER.                                             ECS021T5
00212      12  FILLER                       PIC X(28)                   ECS021T5
00213                        VALUE '**** START OF ZERO TABLE****'.      ECS021T5
00214                                                                   ECS021T5
00215  01  ZERO-TABLE.                                                  ECS021T5
00216      12  ZERO-ACCUMULATORS.                                       ECS021T5
00217          16  ZERO-BENEFIT-TYPE            PIC X    VALUE SPACES.  ECS021T5
00218          16  ZERO-BENEFIT-CODE            PIC XX   VALUE ZEROS.   ECS021T5
00219          16  ZERO-PERIOD       OCCURS     15 TIMES COMP-3.        ECS021T5
00220              20  ZERO-ISS-CNT         PIC S9(7).                  ECS021T5
00221              20  ZERO-CNC-CNT         PIC S9(7).                  ECS021T5
00222              20  ZERO-ISS-PREM        PIC S9(11)V99.              ECS021T5
00223              20  ZERO-CNC-PREM        PIC S9(11)V99.              ECS021T5
00224              20  ZERO-NET-COMPEN      PIC S9(11)V99.              ECS021T5
00225              20  ZERO-CLM-CNT         PIC S9(7).                  ECS021T5
00226              20  ZERO-CLM-AMT         PIC S9(11)V99.              ECS021T5
00227              20  ZERO-LOSS-RESV       PIC S9(11)V99.              ECS021T5
00228              20  ZERO-EARND-PREM      PIC S9(9)V99.               ECS021T5
00229              20  ZERO-PRM-INFRC       PIC S9(9)V99.               ECS021T5
00230              20  ZERO-INFRC-CNT       PIC S9(9).                  ECS021T5
00231              20  ZERO-AVG-AGE         PIC S9(9).                     CL**6
00232              20  ZERO-AVG-ORG-TRM     PIC S9(9).                     CL**6
00233              20  ZERO-WGHT-AGE        PIC S9(9).                     CL**6
00234              20  ZERO-WGHT-ORG-TRM    PIC S9(9).                     CL**6
00235              20  ZERO-EXP-PCT         PIC S9(3)V9(4).             ECS021T5
00236              20  ZERO-ADDED-TO-CNT    PIC S9(5).                  ECS021T5
00237                                                                   ECS021T5
00238  01  ZERO-ENTRY-IDENTIFIER.                                       ECS021T5
00239      12  FILLER                   PIC X(28)                       ECS021T5
00240                            VALUE '****START OF ZERO ENTRY ****'.  ECS021T5
00241                                                                   ECS021T5
00242  01  ZERO-TABLE-ENTRIES.                                          ECS021T5
00243      12  ZERO-ACCUMS              COMP-3.                         ECS021T5
00244          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T5
00245          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T5
00246          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T5
00247          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T5
00248          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T5
00249          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T5
00250          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T5
00251          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T5
00252          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021T5
00253          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021T5
00254          16  FILLER               PIC S9(9)       VALUE +0.       ECS021T5
00255          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00256          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00257          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00258          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00259          16  FILLER               PIC S9(3)V9(4)  VALUE +0.       ECS021T5
00260          16  FILLER               PIC S9(5)       VALUE +0.       ECS021T5
00261                                                                   ECS021T5
00262      EJECT                                                        ECS021T5
00263                                                                   ECS021T5
00264  01  ZERO-TOTAL-IDENTIFIER.                                       ECS021T5
00265      12  FILLER                   PIC X(28)                       ECS021T5
00266                            VALUE '****START OF ZERO TOTAL ****'.  ECS021T5
00267                                                                   ECS021T5
00268  01  ZERO-TOTAL-TABLE.                                            ECS021T5
00269      12  ZERO-TOTAL-ACCUMS  OCCURS  15 TIMES  COMP-3.             ECS021T5
00270          16  FILLER                   PIC S9(7).                  ECS021T5
00271          16  FILLER                   PIC S9(7).                  ECS021T5
00272          16  FILLER                   PIC S9(7).                  ECS021T5
00273          16  FILLER                   PIC S9(7).                  ECS021T5
00274          16  FILLER                   PIC S9(7).                  ECS021T5
00275          16  FILLER                   PIC S9(11)V99.              ECS021T5
00276          16  FILLER                   PIC S9(11)V99.              ECS021T5
00277          16  FILLER                   PIC S9(11)V99.              ECS021T5
00278          16  FILLER                   PIC S9(7).                  ECS021T5
00279          16  FILLER                   PIC S9(11)V99.              ECS021T5
00280          16  FILLER                   PIC S9(7).                  ECS021T5
00281          16  FILLER                   PIC S9(11)V99.              ECS021T5
00282          16  FILLER                   PIC S9(11)V99.              ECS021T5
00283          16  FILLER                   PIC S9(9)V99.               ECS021T5
00284          16  FILLER                   PIC S9(9)V99.               ECS021T5
00285          16  FILLER                   PIC S9(9).                  ECS021T5
00286          16  FILLER                   PIC S9(9).                     CL**6
00287          16  FILLER                   PIC S9(9).                     CL**6
00288          16  FILLER                   PIC S9(9).                     CL**6
00289          16  FILLER                   PIC S9(9).                     CL**6
00290          16  FILLER                   PIC S999V9(4).              ECS021T5
00291          16  FILLER                   PIC S9(5).                  ECS021T5
00292                                                                   ECS021T5
00293  01  ZERO-TOTAL-ENTRY         COMP-3.                             ECS021T5
00294      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T5
00295      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T5
00296      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T5
00297      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T5
00298      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T5
00299      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T5
00300      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T5
00301      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T5
00302      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T5
00303      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T5
00304      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T5
00305      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T5
00306      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T5
00307      12  FILLER                       PIC S9(9)V99   VALUE +0.    ECS021T5
00308      12  FILLER                       PIC S9(9)V99   VALUE +0.    ECS021T5
00309      12  FILLER                       PIC S9(9)      VALUE +0.    ECS021T5
00310      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00311      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00312      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00313      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00314      12  FILLER                       PIC S999V9(4)  VALUE +0.    ECS021T5
00315      12  FILLER                       PIC S9(5)      VALUE +0.    ECS021T5
00316                                                                   ECS021T5
00317                                                                   ECS021T5
00318      EJECT                                                        ECS021T5
00319  LINKAGE SECTION.                                                 ECS021T5
00320                                                                   ECS021T5
00321 *************************************************                 ECS021T5
00322 *  ACTION REQUESTS:                             *                 ECS021T5
00323 *                                               *                 ECS021T5
00324 *  BZ - BUILD ZERO TABLE (1ST TIME PROCESSING)  *                 ECS021T5
00325 *  ZT - ZERO INTERNAL TABLES                    *                 ECS021T5
00326 *  AS - APPLY SORT RECORD TO INTERNAL TABLES    *                 ECS021T5
00327 *  AT - ADD LINKAGE TABLES TO INTERNAL TABLES   *                 ECS021T5
00328 *  MT - MOVE TABLES TO LINK TABLES              *                 ECS021T5
00329 *************************************************                 ECS021T5
00330                                                                   ECS021T5
00331  01  REQUEST-TABLE.                                               ECS021T5
00332      12  PROCESSING-REQUEST    OCCURS 10   PIC X(2).              ECS021T5
00333      12  NUMBER-OF-REQUESTS                PIC S9(04) COMP.       ECS021T5
00334                                                                   ECS021T5
00335  01  SW-RECORD.                                                   ECS021T5
00336      12  SW-REPORT-CONTROL-KEY.                                   ECS021T5
00337          16  SW-BREAK-FIELD-1             PIC X(10).              ECS021T5
00338          16  SW-BREAK-FIELD-2             PIC X(10).              ECS021T5
00339          16  SW-BREAK-FIELD-3             PIC X(10).              ECS021T5
00340          16  SW-BREAK-FIELD-4             PIC X(10).              ECS021T5
00341          16  SW-BREAK-FIELD-5             PIC X(10).              ECS021T5
00342          16  SW-BREAK-FIELD-6             PIC X(10).              ECS021T5
00343          16  SW-ACCT-NAME                 PIC X(30).              ECS021T5
121707         16  SW-ACCT-STATUS               PIC X.
00344          16  SW-GA-NAME                   PIC X(30).                 CL**3
00345          16  SW-STATE-NAME                PIC X(25).                 CL**4
00346          16  SW-PROD-DATE                 PIC X(6).               ECS021T5
00347                                                                   ECS021T5
00348      12  SW-RECORD-DATA            OCCURS 23.                        CL**6
00349          16  SW-BENEFIT-TYPE              PIC X.                  ECS021T5
00350          16  SW-BENEFIT-CODE              PIC 99.                 ECS021T5
00351          16  SW-BEN-TBL-POS               PIC S999 COMP.          ECS021T5
00352                                                                   ECS021T5
00353          16  SW-PERIOD    COMP-3   OCCURS 15.                     ECS021T5
00354              24  SW-ISS-CNT               PIC S9(7).              ECS021T5
00355              24  SW-CNC-CNT               PIC S9(7).              ECS021T5
00356              24  SW-ISS-PREM              PIC S9(11)V99.          ECS021T5
00357              24  SW-CNC-PREM              PIC S9(11)V99.          ECS021T5
00358              24  SW-NET-COMPEN            PIC S9(11)V99.          ECS021T5
00359              24  SW-CLM-CNT               PIC S9(7).              ECS021T5
00360              24  SW-CLM-AMT               PIC S9(11)V99.          ECS021T5
00361              24  SW-LOSS-RESV             PIC S9(11)V99.          ECS021T5
00362              24  SW-PRM-EARND             PIC S9(9)V99.           ECS021T5
00363              24  SW-PRM-INFRC             PIC S9(9)V99.           ECS021T5
00364              24  SW-INFRC-CNT             PIC S9(9).              ECS021T5
00365              24  SW-AVG-AGE               PIC S9(9).                 CL**6
00366              24  SW-AVG-ORG-TRM           PIC S9(9).                 CL**6
00367              24  SW-WGHT-AGE              PIC S9(9).                 CL**6
00368              24  SW-WGHT-ORG-TRM          PIC S9(9).                 CL**6
00369              24  SW-EXP-PCT               PIC S999V9(4).          ECS021T5
00370              24  SW-ADDED-TO-CNT          PIC S9(5).              ECS021T5
00371                                                                   ECS021T5
00372      EJECT                                                        ECS021T5
00373  01  LINK-TABLE.                                                  ECS021T5
092602     12  LINK-ACCUMULATORS   OCCURS  900 TIMES.                      CL**6
00375          16  LINK-BENEFIT-TYPE        PIC X.                      ECS021T5
00376          16  LINK-BENEFIT-CODE        PIC 99.                     ECS021T5
00377          16  LINK-PERIOD    OCCURS   15  TIMES  COMP-3.           ECS021T5
00378              20  LINK-ISS-CNT         PIC S9(7).                  ECS021T5
00379              20  LINK-CNC-CNT         PIC S9(7).                  ECS021T5
00380              20  LINK-ISS-PREM        PIC S9(11)V99.              ECS021T5
00381              20  LINK-CNC-PREM        PIC S9(11)V99.              ECS021T5
00382              20  LINK-NET-COMPEN      PIC S9(11)V99.              ECS021T5
00383              20  LINK-CLM-CNT         PIC S9(7).                  ECS021T5
00384              20  LINK-CLM-AMT         PIC S9(11)V99.              ECS021T5
00385              20  LINK-LOSS-RESV       PIC S9(11)V99.              ECS021T5
00386              20  LINK-EARND-PREM      PIC S9(9)V99.               ECS021T5
00387              20  LINK-PRM-INFRC       PIC S9(9)V99.               ECS021T5
00388              20  LINK-INFRC-CNT       PIC S9(9).                  ECS021T5
00389              20  LINK-AVG-AGE         PIC S9(9).                     CL**6
00390              20  LINK-AVG-ORG-TRM     PIC S9(9).                     CL**6
00391              20  LINK-WGHT-AGE        PIC S9(9).                     CL**6
00392              20  LINK-WGHT-ORG-TRM    PIC S9(9).                     CL**6
00393              20  LINK-EXP-PCT         PIC S999V9(4).              ECS021T5
00394              20  LINK-ADDED-TO-CNT    PIC S9(5).                  ECS021T5
00395                                                                   ECS021T5
092602*01  LINK-EXTENSION-1.                                            ECS021T5
00397 *    12  LINK-TBL-EXTENSION-1    OCCURS   75 TIMES.                  CL**6
00398 *        16  FILLER                       PIC X.                  ECS021T5
00399 *        16  FILLER                       PIC 99.                 ECS021T5
00400 *        16  LINK-EXT-1          OCCURS   15  TIMES  COMP-3.      ECS021T5
00401 *            20  FILLER               PIC S9(7).                  ECS021T5
00402 *            20  FILLER               PIC S9(7).                  ECS021T5
00403 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00404 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00405 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00406 *            20  FILLER               PIC S9(7).                  ECS021T5
00407 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00408 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00409 *            20  FILLER               PIC S9(9)V99.               ECS021T5
00410 *            20  FILLER               PIC S9(9)V99.               ECS021T5
00411 *            20  FILLER               PIC S9(9).                  ECS021T5
00412 *            20  FILLER               PIC S9(9).                     CL**6
00413 *            20  FILLER               PIC S9(9).                     CL**6
00414 *            20  FILLER               PIC S9(9).                     CL**6
00415 *            20  FILLER               PIC S9(9).                     CL**6
00416 *            20  FILLER               PIC S9(3)V9(4).             ECS021T5
00417 *            20  FILLER               PIC S9(5).                  ECS021T5
00418 *                                                                 ECS021T5
00419 *01  LINK-EXTENSION-2.                                            ECS021T5
00420 *    12  LINK-TBL-EXTENSION-2    OCCURS   75 TIMES.                  CL**6
00421 *        16  FILLER                       PIC X.                  ECS021T5
00422 *        16  FILLER                       PIC 99.                 ECS021T5
00423 *        16  LINK-EXT-2          OCCURS   15  TIMES  COMP-3.      ECS021T5
00424 *            20  FILLER               PIC S9(7).                  ECS021T5
00425 *            20  FILLER               PIC S9(7).                  ECS021T5
00426 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00427 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00428 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00429 *            20  FILLER               PIC S9(7).                  ECS021T5
00430 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00431 *            20  FILLER               PIC S9(11)V99.              ECS021T5
00432 *            20  FILLER               PIC S9(9)V99.               ECS021T5
00433 *            20  FILLER               PIC S9(9)V99.               ECS021T5
00434 *            20  FILLER               PIC S9(9).                  ECS021T5
00435 *            20  FILLER               PIC S9(9).                     CL**6
00436 *            20  FILLER               PIC S9(9).                     CL**6
00437 *            20  FILLER               PIC S9(9).                     CL**6
00438 *            20  FILLER               PIC S9(9).                     CL**6
00439 *            20  FILLER               PIC S9(3)V9(4).                CL**6
00440 *            20  FILLER               PIC S9(5).                     CL**6
00441 *                                                                    CL**6
00442      EJECT                                                           CL**6
00443 *01  LINK-EXTENSION-3.                                               CL**6
00444 *    12  LINK-TBL-EXTENSION-3    OCCURS   75 TIMES.                  CL**6
00445 *        16  FILLER                       PIC X.                     CL**6
00446 *        16  FILLER                       PIC 99.                    CL**6
00447 *        16  LINK-EXT-3          OCCURS   15  TIMES  COMP-3.         CL**6
00448 *            20  FILLER               PIC S9(7).                     CL**2
00449 *            20  FILLER               PIC S9(7).                     CL**2
00450 *            20  FILLER               PIC S9(11)V99.                 CL**6
00451 *            20  FILLER               PIC S9(11)V99.                 CL**6
00452 *            20  FILLER               PIC S9(11)V99.                 CL**6
00453 *            20  FILLER               PIC S9(7).                     CL**2
00454 *            20  FILLER               PIC S9(11)V99.                 CL**6
00455 *            20  FILLER               PIC S9(11)V99.                 CL**6
00456 *            20  FILLER               PIC S9(9)V99.                  CL**6
00457 *            20  FILLER               PIC S9(9)V99.                  CL**6
00458 *            20  FILLER               PIC S9(9).                     CL**6
00459 *            20  FILLER               PIC S9(9).                     CL**6
00460 *            20  FILLER               PIC S9(9).                     CL**6
00461 *            20  FILLER               PIC S9(9).                     CL**6
00462 *            20  FILLER               PIC S9(9).                     CL**6
00463 *            20  FILLER               PIC S9(3)V9(4).             ECS021T5
00464 *            20  FILLER               PIC S9(5).                  ECS021T5
00465                                                                   ECS021T5
00466                                                                   ECS021T5
00467      EJECT                                                        ECS021T5
00468  01  LINK-TOTAL-TABLE.                                            ECS021T5
00469      12  LINK-TOTALS     OCCURS  3 TIMES.                         ECS021T5
00470          16  LINK-T-PERIOD  COMP-3 OCCURS 15 TIMES.               ECS021T5
00471              20  LINK-T-ISS-CNT       PIC S9(7).                  ECS021T5
00472              20  LINK-T-CNC-CNT       PIC S9(7).                  ECS021T5
00473              20  LINK-T-SINGLE-ELEM   PIC S9(7).                  ECS021T5
00474              20  LINK-T-JOINT-RETRO   PIC S9(7).                  ECS021T5
00475              20  LINK-T-LIFE-LEVEL    PIC S9(7).                  ECS021T5
00476              20  LINK-T-ISS-PREM      PIC S9(11)V99.              ECS021T5
00477              20  LINK-T-CNC-PREM      PIC S9(11)V99.              ECS021T5
00478              20  LINK-T-NET-COMPEN    PIC S9(11)V99.              ECS021T5
00479              20  LINK-T-LF-CLM-CNT    PIC S9(7).                  ECS021T5
00480              20  LINK-T-LF-CLM-AMT    PIC S9(11)V99.              ECS021T5
00481              20  LINK-T-AH-CLM-CNT    PIC S9(7).                  ECS021T5
00482              20  LINK-T-AH-CLM-AMT    PIC S9(11)V99.              ECS021T5
00483              20  LINK-T-LOSS-RESV     PIC S9(11)V99.              ECS021T5
00484              20  LINK-T-EARND-PREM    PIC S9(9)V99.               ECS021T5
00485              20  LINK-T-PRM-INFRC     PIC S9(9)V99.               ECS021T5
00486              20  LINK-T-INFRC-CNT     PIC S9(9).                  ECS021T5
00487              20  LINK-T-AVG-AGE       PIC S9(9).                     CL**6
00488              20  LINK-T-AVG-ORG-TRM   PIC S9(9).                     CL**6
00489              20  LINK-T-WGHT-AGE      PIC S9(9).                     CL**6
00490              20  LINK-T-WGHT-ORG-TRM  PIC S9(9).                     CL**6
00491              20  LINK-T-EXP-PCT       PIC S999V9(4).              ECS021T5
00492              20  LINK-T-ADDED-TO-CNT  PIC S9(5).                  ECS021T5
00493                                                                   ECS021T5
00494      EJECT                                                        ECS021T5
00495  01  CLASIC-SYSTEM-CODES.                                         ECS021T5
00496      12  DTE-COLC-ID                 PIC  X(4).                   ECS021T5
00497      12  DTE-CLASIC-COMPANY-CD       PIC  X.                      ECS021T5
00498      12  DTE-CLASIC-COMPANY-NUMBER   PIC  999.                    ECS021T5
00499      12  DTE-CLASIC-CLAIM-ACCESS     PIC  X.                      ECS021T5
00500      12  CLASIC-REIN-MAINT           PIC  XX.                     ECS021T5
00501      12  CLASIC-COMP-MAINT           PIC  XX.                     ECS021T5
00502      12  CLASIC-ACCT-MAINT           PIC  XX.                     ECS021T5
00503      12  CLASIC-CTBL-MAINT           PIC  XX.                     ECS021T5
00504      12  CLASIC-RATE-MAINT           PIC  XX.                     ECS021T5
00505      12  CLASIC-CREDIT-EOM-DT        PIC  XX.                     ECS021T5
00506      12  CLASIC-CLAIMS-EOM-DT        PIC  XX.                     ECS021T5
00507                                                                   ECS021T5
00508      12  LIFE-OVERRIDE-L1            PIC  X.                      ECS021T5
00509      12  LIFE-OVERRIDE-L2            PIC  XX.                     ECS021T5
00510      12  LIFE-OVERRIDE-L6            PIC  X(6).                   ECS021T5
00511      12  LIFE-OVERRIDE-L12           PIC  X(12).                  ECS021T5
00512                                                                   ECS021T5
00513      12  AH-OVERRIDE-L1              PIC  X.                      ECS021T5
00514      12  AH-OVERRIDE-L2              PIC  XX.                     ECS021T5
00515      12  AH-OVERRIDE-L6              PIC  X(6).                   ECS021T5
00516      12  AH-OVERRIDE-L12             PIC  X(12).                  ECS021T5
00517                                                                   ECS021T5
00518      12  CLAS-REPORT-CD1-CAPTION     PIC  X(10).                  ECS021T5
00519      12  CLAS-REPORT-CD2-CAPTION     PIC  X(10).                  ECS021T5
00520                                                                   ECS021T5
00521      12  CLASIC-MORTG-EOM-DT         PIC  XX.                     ECS021T5
00522      12  CLASIC-AR-EOM-DT            PIC  XX.                     ECS021T5
00523                                                                   ECS021T5
00524      12  FILLER                      PIC  X(11).                  ECS021T5
00525                                                                   ECS021T5
00526                                                                   ECS021T5
00527  01  CLAS-INS-TYPES.                                              ECS021T5
092602     12 CLAS-ALL-TYPES               OCCURS 900 TIMES.            ECS021T5
00529          16  CLAS-I-BEN              PIC  XX.                     ECS021T5
00530          16  CLAS-I-AB3.                                          ECS021T5
00531              20  FILLER              PIC  X.                      ECS021T5
00532              20  CLAS-I-AB2.                                      ECS021T5
00533                  24  FILLER          PIC  X.                      ECS021T5
00534                  24  CLAS-I-AB1      PIC  X.                      ECS021T5
00535          16  CLAS-I-AB10.                                         ECS021T5
00536              20  FILLER              PIC  X(9).                   ECS021T5
00537              20  CLAS-I-REIN-YN      PIC  X.                      ECS021T5
00538          16  CLAS-I-COMMENT          PIC  X(10).                  ECS021T5
00539          16  CLAS-I-JOINT            PIC  X.                      ECS021T5
00540          16  CLAS-I-RL-AH            PIC  X.                      ECS021T5
00541          16  CLAS-I-CALC-TYPE.                                    ECS021T5
00542              20  CLAS-I-BAL          PIC  X.                      ECS021T5
00543          16  CLAS-I-EP               PIC  X.                      ECS021T5
00544          16  FILLER                  PIC  X(9).                   ECS021T5
00545                                                                   ECS021T5
00546  01  CLAS-INDEX-TBL.                                              ECS021T5
00547      12  CLAX-ID                     PIC X(4).                    ECS021T5
00548      12  CLAS-STARTC                 PIC S9(4) COMP.              ECS021T5
00549      12  CLAS-MAXC                   PIC S9(4) COMP.              ECS021T5
00550      12  CLAS-STARTL                 PIC S9(4) COMP.              ECS021T5
00551      12  CLAS-MAXL                   PIC S9(4) COMP.              ECS021T5
00552      12  CLAS-STARTA                 PIC S9(4) COMP.              ECS021T5
00553      12  CLAS-MAXA                   PIC S9(4) COMP.              ECS021T5
00554      12  CLAS-STARTM                 PIC S9(4) COMP.              ECS021T5
00555      12  CLAS-MAXM                   PIC S9(4) COMP.              ECS021T5
00556      12  CLAS-STARTB                 PIC S9(4) COMP.              ECS021T5
00557      12  CLAS-MAXB                   PIC S9(4) COMP.              ECS021T5
00558      12  CLAS-STARTS                 PIC S9(4) COMP.              ECS021T5
00559      12  CLAS-MAXS                   PIC S9(4) COMP.              ECS021T5
00560      12  CLAS-STARTE                 PIC S9(4) COMP.              ECS021T5
00561      12  CLAS-MAXE                   PIC S9(4) COMP.              ECS021T5
00562      12  CLAS-STARTCN                PIC S9(4) COMP.              ECS021T5
00563      12  CLAS-MAXCN                  PIC S9(4) COMP.              ECS021T5
00564                                                                   ECS021T5
00565      EJECT                                                        ECS021T5
00566  PROCEDURE DIVISION USING REQUEST-TABLE                           ECS021T5
00567                           SW-RECORD                               ECS021T5
00568                           LINK-TABLE                              ECS021T5
092602*                         LINK-EXTENSION-1                        ECS021T5
092602*                         LINK-EXTENSION-2                        ECS021T5
092602*                         LINK-EXTENSION-3                           CL**6
00572                           LINK-TOTAL-TABLE                        ECS021T5
00573                           CLASIC-SYSTEM-CODES                     ECS021T5
00574                           CLAS-INS-TYPES                          ECS021T5
00575                           CLAS-INDEX-TBL.                         ECS021T5
00576                                                                   ECS021T5
00577  1000-PROCESS-REQUESTS.                                           ECS021T5
00578                                                                   ECS021T5
00579      PERFORM 2000-READ-REQUEST-TABLE THRU 2000-EXIT               ECS021T5
00580          VARYING REQ-IDX  FROM  ONE BY ONE                        ECS021T5
00581              UNTIL REQ-IDX GREATER THAN NUMBER-OF-REQUESTS.       ECS021T5
00582                                                                   ECS021T5
00583      GOBACK.                                                      ECS021T5
00584                                                                   ECS021T5
00585  1000-EXIT.                                                       ECS021T5
00586      EXIT.                                                        ECS021T5
00587                                                                   ECS021T5
00588  2000-READ-REQUEST-TABLE.                                         ECS021T5
00589                                                                   ECS021T5
00590      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-BUILD-ZERO-TABLE     ECS021T5
00591          PERFORM 9000-INITIALIZE-ZERO-TABLE THRU 9000-EXIT        ECS021T5
00592          PERFORM 8000-ZERO-ENTIRE-TABLE THRU 8000-EXIT            ECS021T5
00593          GO TO 2000-EXIT.                                         ECS021T5
00594                                                                   ECS021T5
00595      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-ZERO-TABLE           ECS021T5
00596          PERFORM 8000-ZERO-ENTIRE-TABLE THRU 8000-EXIT            ECS021T5
00597          GO TO 2000-EXIT.                                         ECS021T5
00598                                                                   ECS021T5
00599      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-APPLY-SORT-RCD       ECS021T5
00600          PERFORM 5000-APPLY-SORT-RECORD THRU 5000-EXIT            ECS021T5
00601          GO TO 2000-EXIT.                                         ECS021T5
00602                                                                   ECS021T5
00603      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-ADD-LINK-TABLES      ECS021T5
00604          PERFORM 3000-ADD-TABLE-RTN THRU 3000-EXIT                ECS021T5
00605          PERFORM 4000-ADD-TOTAL-RTN THRU 4000-EXIT                ECS021T5
00606          GO TO 2000-EXIT.                                         ECS021T5
00607                                                                   ECS021T5
00608      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-MOVE-TO-LINK         ECS021T5
00609          PERFORM 7000-MOVE-TABLES THRU 7000-EXIT.                 ECS021T5
00610                                                                   ECS021T5
00611  2000-EXIT.                                                       ECS021T5
00612      EXIT.                                                        ECS021T5
00613                                                                   ECS021T5
00614      EJECT                                                        ECS021T5
00615 *******************************************                       ECS021T5
00616 *  (2000) ADDS TABLE (2) TO LINK TABLE    *                       ECS021T5
00617 *******************************************                       ECS021T5
00618                                                                   ECS021T5
00619  3000-ADD-TABLE-RTN.                                              ECS021T5
00620      MOVE +1                     TO BEN-IDX.                      ECS021T5
00621                                                                   ECS021T5
00622  3000-ZERO-ACCESS-LOOP.                                           ECS021T5
00623      IF BEN-IDX GREATER CLAS-MAXA                                 ECS021T5
00624          GO TO 3000-EXIT.                                         ECS021T5
00625                                                                   ECS021T5
00626      IF LINK-BENEFIT-CODE (BEN-IDX) = ZERO                        ECS021T5
00627          ADD +1                  TO BEN-IDX                       ECS021T5
00628          GO TO 3000-ZERO-ACCESS-LOOP.                             ECS021T5
00629                                                                   ECS021T5
00630      MOVE LINK-BENEFIT-TYPE (BEN-IDX)                             ECS021T5
00631                                  TO TBL-BENEFIT-TYPE (BEN-IDX).   ECS021T5
00632      MOVE LINK-BENEFIT-CODE (BEN-IDX)                             ECS021T5
00633                                  TO TBL-BENEFIT-CODE (BEN-IDX).   ECS021T5
00634                                                                   ECS021T5
00635      MOVE +1                     TO DTE-IDX.                      ECS021T5
00636                                                                   ECS021T5
00637  3000-ACTIVE-BENEFIT-LOOP.                                        ECS021T5
00638                                                                   ECS021T5
00639      ADD LINK-ISS-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T5
00640                               TBL-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T5
00641      ADD LINK-CNC-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T5
00642                               TBL-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T5
00643      ADD LINK-ISS-PREM    (BEN-IDX DTE-IDX) TO                    ECS021T5
00644                               TBL-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T5
00645      ADD LINK-CNC-PREM    (BEN-IDX DTE-IDX) TO                    ECS021T5
00646                               TBL-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T5
00647      ADD LINK-NET-COMPEN  (BEN-IDX DTE-IDX) TO                    ECS021T5
00648                               TBL-NET-COMPEN   (BEN-IDX DTE-IDX). ECS021T5
00649      ADD LINK-CLM-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T5
00650                               TBL-CLM-CNT      (BEN-IDX DTE-IDX). ECS021T5
00651      ADD LINK-CLM-AMT     (BEN-IDX DTE-IDX) TO                    ECS021T5
00652                               TBL-CLM-AMT      (BEN-IDX DTE-IDX). ECS021T5
00653      ADD LINK-LOSS-RESV   (BEN-IDX DTE-IDX) TO                    ECS021T5
00654                               TBL-LOSS-RESV    (BEN-IDX DTE-IDX). ECS021T5
00655      ADD LINK-EARND-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T5
00656                               TBL-EARND-PREM   (BEN-IDX DTE-IDX). ECS021T5
00657      ADD LINK-PRM-INFRC   (BEN-IDX DTE-IDX) TO                    ECS021T5
00658                               TBL-PRM-INFRC    (BEN-IDX DTE-IDX). ECS021T5
00659      ADD LINK-INFRC-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T5
00660                               TBL-INFRC-CNT    (BEN-IDX DTE-IDX). ECS021T5
00661      ADD LINK-AVG-AGE      (BEN-IDX DTE-IDX) TO                   ECS021T5
00662                               TBL-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T5
00663      ADD LINK-AVG-ORG-TRM (BEN-IDX DTE-IDX) TO                    ECS021T5
00664                               TBL-AVG-ORG-TRM (BEN-IDX DTE-IDX).  ECS021T5
00665      ADD LINK-WGHT-AGE    (BEN-IDX DTE-IDX) TO                    ECS021T5
00666                               TBL-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T5
00667      ADD LINK-WGHT-ORG-TRM(BEN-IDX DTE-IDX) TO                    ECS021T5
00668                               TBL-WGHT-ORG-TRM(BEN-IDX DTE-IDX).  ECS021T5
00669      ADD LINK-EXP-PCT     (BEN-IDX DTE-IDX) TO                    ECS021T5
00670                               TBL-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T5
00671      ADD LINK-ADDED-TO-CNT(BEN-IDX DTE-IDX) TO                    ECS021T5
00672                               TBL-ADDED-TO-CNT(BEN-IDX DTE-IDX).  ECS021T5
00673                                                                   ECS021T5
00674      ADD +1 TO DTE-IDX.                                           ECS021T5
00675                                                                   ECS021T5
00676      IF DTE-IDX LESS +16                                          ECS021T5
00677          GO TO 3000-ACTIVE-BENEFIT-LOOP                           ECS021T5
00678      ELSE                                                         ECS021T5
00679          ADD +1 TO BEN-IDX                                        ECS021T5
00680          GO TO 3000-ZERO-ACCESS-LOOP.                             ECS021T5
00681                                                                   ECS021T5
00682  3000-EXIT.                                                       ECS021T5
00683       EXIT.                                                       ECS021T5
00684                                                                   ECS021T5
00685      EJECT                                                        ECS021T5
00686 *******************************************************           ECS021T5
00687 *  (3050) DUMPS TOTAL TABLE (2) INTO TOTAL TABLE (1)  *           ECS021T5
00688 *******************************************************           ECS021T5
00689  4000-ADD-TOTAL-RTN.                                              ECS021T5
00690      MOVE +1                     TO BEN-IDX                       ECS021T5
00691                                     DTE-IDX.                      ECS021T5
00692                                                                   ECS021T5
00693  4000-TOTAL-BENEFITS-LOOP.                                        ECS021T5
00694      ADD LINK-T-ISS-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T5
00695                               TOT-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T5
00696      ADD LINK-T-CNC-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T5
00697                               TOT-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T5
00698      ADD LINK-T-SINGLE-ELEM (BEN-IDX DTE-IDX) TO                  ECS021T5
00699                               TOT-SINGLE-ELEM (BEN-IDX DTE-IDX).     CL**2
00700      ADD LINK-T-JOINT-RETRO (BEN-IDX DTE-IDX) TO                  ECS021T5
00701                               TOT-JOINT-RETRO (BEN-IDX DTE-IDX).     CL**2
00702      ADD LINK-T-LIFE-LEVEL (BEN-IDX DTE-IDX) TO                   ECS021T5
00703                               TOT-LIFE-LEVEL (BEN-IDX DTE-IDX).   ECS021T5
00704      ADD LINK-T-ISS-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T5
00705                               TOT-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T5
00706      ADD LINK-T-CNC-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T5
00707                               TOT-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T5
00708      ADD LINK-T-NET-COMPEN (BEN-IDX DTE-IDX) TO                   ECS021T5
00709                               TOT-NET-COMPEN (BEN-IDX DTE-IDX).   ECS021T5
00710      ADD LINK-T-LF-CLM-CNT (BEN-IDX DTE-IDX) TO                   ECS021T5
00711                               TOT-LF-CLM-CNT (BEN-IDX DTE-IDX).   ECS021T5
00712      ADD LINK-T-LF-CLM-AMT (BEN-IDX DTE-IDX) TO                   ECS021T5
00713                               TOT-LF-CLM-AMT (BEN-IDX DTE-IDX).   ECS021T5
00714      ADD LINK-T-AH-CLM-CNT (BEN-IDX DTE-IDX) TO                   ECS021T5
00715                               TOT-AH-CLM-CNT (BEN-IDX DTE-IDX).   ECS021T5
00716      ADD LINK-T-AH-CLM-AMT (BEN-IDX DTE-IDX) TO                   ECS021T5
00717                               TOT-AH-CLM-AMT (BEN-IDX DTE-IDX).   ECS021T5
00718      ADD LINK-T-LOSS-RESV (BEN-IDX DTE-IDX) TO                    ECS021T5
00719                               TOT-LOSS-RESV (BEN-IDX DTE-IDX).    ECS021T5
00720      ADD LINK-T-EARND-PREM (BEN-IDX DTE-IDX) TO                   ECS021T5
00721                               TOT-EARND-PREM (BEN-IDX DTE-IDX).   ECS021T5
00722      ADD LINK-T-PRM-INFRC (BEN-IDX DTE-IDX) TO                    ECS021T5
00723                               TOT-PRM-INFRC (BEN-IDX DTE-IDX).    ECS021T5
00724      ADD LINK-T-INFRC-CNT (BEN-IDX DTE-IDX) TO                    ECS021T5
00725                               TOT-INFRC-CNT (BEN-IDX DTE-IDX).    ECS021T5
00726      ADD LINK-T-AVG-AGE    (BEN-IDX DTE-IDX) TO                   ECS021T5
00727                               TOT-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T5
00728      ADD LINK-T-AVG-ORG-TRM (BEN-IDX DTE-IDX) TO                  ECS021T5
00729                               TOT-AVG-ORG-TRM (BEN-IDX DTE-IDX).     CL**2
00730      ADD LINK-T-WGHT-AGE  (BEN-IDX DTE-IDX) TO                    ECS021T5
00731                               TOT-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T5
00732      ADD LINK-T-WGHT-ORG-TRM(BEN-IDX DTE-IDX) TO                  ECS021T5
00733                               TOT-WGHT-ORG-TRM(BEN-IDX DTE-IDX).     CL**2
00734      ADD LINK-T-EXP-PCT   (BEN-IDX DTE-IDX) TO                    ECS021T5
00735                               TOT-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T5
00736      ADD LINK-T-ADDED-TO-CNT(BEN-IDX DTE-IDX) TO                  ECS021T5
00737                               TOT-ADDED-TO-CNT(BEN-IDX DTE-IDX).     CL**2
00738                                                                   ECS021T5
00739      ADD +1 TO DTE-IDX.                                           ECS021T5
00740                                                                   ECS021T5
00741      IF DTE-IDX LESS +16                                          ECS021T5
00742          GO TO 4000-TOTAL-BENEFITS-LOOP.                          ECS021T5
00743                                                                   ECS021T5
00744      IF BEN-IDX LESS +3                                           ECS021T5
00745          ADD +1                  TO BEN-IDX                       ECS021T5
00746          MOVE +1                 TO DTE-IDX                       ECS021T5
00747          GO TO 4000-TOTAL-BENEFITS-LOOP.                          ECS021T5
00748                                                                   ECS021T5
00749  4000-EXIT.                                                       ECS021T5
00750       EXIT.                                                       ECS021T5
00751                                                                   ECS021T5
00752      EJECT                                                        ECS021T5
00753 **************************************************************    ECS021T5
00754 *  THE FOLLOWING INDEXES ARE USED IN ROUTINES 5000 AND 6000  *    ECS021T5
00755 *                                                            *    ECS021T5
00756 *  1. BRK-IDX - USED TO INDEX BENEFIT LEVEL OF SORT RECORD   *    ECS021T5
00757 *                 VARIED FROM 1 TO 23                        *       CL**6
00758 *  2. BEN-IDX - USED TO INDEX BENEFIT LEVEL OF ACCUMULATED   *    ECS021T5
00759 *                 BREAK TABLES VARIED FROM 1 TO 75           *       CL**6
00760 *  3. TOT-IDX - USED TO INDEX BENEFIT TYPE OF TOTALS         *    ECS021T5
00761 *                 TABLES VARIED FROM 1 TO 2                  *    ECS021T5
00762 *  4. DTE-IDX - USED TO INDEX DATE RANGES OF ALL TABLES      *    ECS021T5
00763 *                 AND RECORDS VARIED FROM 1 TO 15            *    ECS021T5
00764 **************************************************************    ECS021T5
00765                                                                   ECS021T5
00766  5000-APPLY-SORT-RECORD.                                          ECS021T5
00767      MOVE +1                     TO BRK-IDX.                      ECS021T5
00768                                                                   ECS021T5
00769  5000-BENEFIT-LOOP.                                               ECS021T5
00770      IF SW-BENEFIT-CODE   (BRK-IDX) = ZEROS                       ECS021T5
00771          GO TO 5000-EXIT.                                         ECS021T5
00772                                                                   ECS021T5
00773      MOVE SW-BEN-TBL-POS  (BRK-IDX)                               ECS021T5
00774                                  TO BEN-IDX.                      ECS021T5
00775      MOVE SW-BENEFIT-CODE (BRK-IDX)                               ECS021T5
00776                                  TO TBL-BENEFIT-CODE (BEN-IDX).   ECS021T5
00777      MOVE SW-BENEFIT-TYPE (BRK-IDX)                               ECS021T5
00778                                  TO TBL-BENEFIT-TYPE (BEN-IDX).   ECS021T5
00779      MOVE +1                     TO DTE-IDX.                      ECS021T5
00780                                                                   ECS021T5
00781  5000-DATE-RANGE-LOOP.                                            ECS021T5
00782      ADD SW-ISS-CNT     (BRK-IDX DTE-IDX)                         ECS021T5
00783                            TO TBL-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T5
00784      ADD SW-CNC-CNT     (BRK-IDX DTE-IDX)                         ECS021T5
00785                            TO TBL-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T5
00786      ADD SW-ISS-PREM    (BRK-IDX DTE-IDX)                         ECS021T5
00787                            TO TBL-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T5
00788      ADD SW-CNC-PREM    (BRK-IDX DTE-IDX)                         ECS021T5
00789                            TO TBL-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T5
00790      ADD SW-NET-COMPEN  (BRK-IDX DTE-IDX)                         ECS021T5
00791                            TO TBL-NET-COMPEN   (BEN-IDX DTE-IDX). ECS021T5
00792      ADD SW-CLM-CNT     (BRK-IDX DTE-IDX)                         ECS021T5
00793                            TO TBL-CLM-CNT      (BEN-IDX DTE-IDX). ECS021T5
00794      ADD SW-CLM-AMT     (BRK-IDX DTE-IDX)                         ECS021T5
00795                            TO TBL-CLM-AMT      (BEN-IDX DTE-IDX). ECS021T5
00796      ADD SW-LOSS-RESV   (BRK-IDX DTE-IDX)                         ECS021T5
00797                            TO TBL-LOSS-RESV    (BEN-IDX DTE-IDX). ECS021T5
00798      ADD SW-PRM-EARND   (BRK-IDX DTE-IDX)                         ECS021T5
00799                            TO TBL-EARND-PREM   (BEN-IDX DTE-IDX). ECS021T5
00800      ADD SW-PRM-INFRC   (BRK-IDX DTE-IDX)                         ECS021T5
00801                            TO TBL-PRM-INFRC    (BEN-IDX DTE-IDX). ECS021T5
00802      ADD SW-INFRC-CNT   (BRK-IDX DTE-IDX)                         ECS021T5
00803                            TO TBL-INFRC-CNT    (BEN-IDX DTE-IDX). ECS021T5
00804      ADD SW-AVG-AGE     (BRK-IDX DTE-IDX)                         ECS021T5
00805                            TO TBL-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T5
00806      ADD SW-AVG-ORG-TRM (BRK-IDX DTE-IDX)                         ECS021T5
00807                            TO TBL-AVG-ORG-TRM (BEN-IDX DTE-IDX).  ECS021T5
00808      ADD SW-WGHT-AGE    (BRK-IDX DTE-IDX)                         ECS021T5
00809                            TO TBL-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T5
00810      ADD SW-WGHT-ORG-TRM(BRK-IDX DTE-IDX)                         ECS021T5
00811                            TO TBL-WGHT-ORG-TRM(BEN-IDX DTE-IDX).  ECS021T5
00812      ADD SW-EXP-PCT     (BRK-IDX DTE-IDX)                         ECS021T5
00813                            TO TBL-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T5
00814      ADD SW-ADDED-TO-CNT(BRK-IDX DTE-IDX)                         ECS021T5
00815                            TO TBL-ADDED-TO-CNT(BEN-IDX DTE-IDX).  ECS021T5
00816                                                                   ECS021T5
00817      ADD +1                TO DTE-IDX.                            ECS021T5
00818                                                                   ECS021T5
00819      IF DTE-IDX LESS +16                                          ECS021T5
00820          GO TO 5000-DATE-RANGE-LOOP.                              ECS021T5
00821                                                                   ECS021T5
00822      PERFORM 6000-BENEFIT-TYPE-TOTAL THRU 6000-EXIT.              ECS021T5
00823                                                                   ECS021T5
00824      ADD +1                TO BRK-IDX.                            ECS021T5
00825                                                                   ECS021T5
00826      IF BRK-IDX LESS +24                                             CL**6
00827          GO TO 5000-BENEFIT-LOOP.                                 ECS021T5
00828                                                                   ECS021T5
00829  5000-EXIT.                                                       ECS021T5
00830       EXIT.                                                       ECS021T5
00831                                                                   ECS021T5
00832      EJECT                                                        ECS021T5
00833 **************************************************************    ECS021T5
00834 *  THE FOLLOWING INDEXES ARE USED IN ROUTINES 5000 AND 6000  *    ECS021T5
00835 *                                                            *    ECS021T5
00836 *  1. BRK-IDX - USED TO INDEX BENEFIT LEVEL OF SORT RECORD   *    ECS021T5
00837 *                 VARIED FROM 1 TO 23                        *       CL**6
00838 *  2. BEN-IDX - USED TO INDEX BENEFIT LEVEL OF ACCUMULATED   *    ECS021T5
00839 *                 BREAK TABLES VARIED FROM 1 TO 75           *       CL**6
00840 *  3. TOT-IDX - USED TO INDEX BENEFIT TYPE OF TOTALS         *    ECS021T5
00841 *                 TABLES VARIED FROM 1 TO 2                  *    ECS021T5
00842 *  4. DTE-IDX - USED TO INDEX DATE RANGES OF ALL TABLES      *    ECS021T5
00843 *                 AND RECORDS VARIED FROM 1 TO 15            *    ECS021T5
00844 **************************************************************    ECS021T5
00845                                                                   ECS021T5
00846  6000-BENEFIT-TYPE-TOTAL.                                         ECS021T5
00847                                                                   ECS021T5
00848      MOVE +1                     TO DTE-IDX.                      ECS021T5
00849                                                                   ECS021T5
00850  6000-DATE-RANGE-LOOP.                                            ECS021T5
00851                                                                   ECS021T5
00852      COMPUTE NET-COUNT = SW-ISS-CNT (BRK-IDX DTE-IDX) -              CL**5
00853          SW-CNC-CNT (BRK-IDX DTE-IDX).                               CL**5
00854                                                                      CL**5
00855      IF SW-BENEFIT-TYPE (BRK-IDX) = LIFE-OVERRIDE-L1              ECS021T5
00856          MOVE +1                 TO TOT-IDX                       ECS021T5
00857          IF CLAS-I-RL-AH (BEN-IDX) = 'R'                          ECS021T5
00858              IF CLAS-I-JOINT (BEN-IDX) = 'J'                      ECS021T5
00859                  ADD NET-COUNT TO                                    CL**5
00860                      TOT-JOINT-RETRO (TOT-IDX DTE-IDX)            ECS021T5
00861              ELSE                                                 ECS021T5
00862                  ADD NET-COUNT TO                                    CL**5
00863                      TOT-SINGLE-ELEM (TOT-IDX DTE-IDX)            ECS021T5
00864          ELSE                                                     ECS021T5
00865              ADD NET-COUNT TO                                        CL**5
00866                  TOT-LIFE-LEVEL (TOT-IDX DTE-IDX)                 ECS021T5
00867      ELSE                                                         ECS021T5
00868          MOVE +2                 TO TOT-IDX                       ECS021T5
00869          IF CLAS-I-AB1 (BEN-IDX) = 'E'                            ECS021T5
00870              ADD NET-COUNT TO                                        CL**5
00871                  TOT-SINGLE-ELEM (TOT-IDX DTE-IDX)                ECS021T5
00872          ELSE                                                     ECS021T5
00873              IF CLAS-I-AB1 (BEN-IDX) = 'R'                        ECS021T5
00874                  ADD NET-COUNT TO                                    CL**5
00875                      TOT-JOINT-RETRO (TOT-IDX DTE-IDX).           ECS021T5
00876                                                                   ECS021T5
00877      ADD SW-ISS-CNT    (BRK-IDX DTE-IDX)                          ECS021T5
00878                            TO TOT-ISS-CNT     (TOT-IDX DTE-IDX)   ECS021T5
00879                               TOT-ISS-CNT     (3 DTE-IDX).        ECS021T5
00880      ADD SW-CNC-CNT    (BRK-IDX DTE-IDX)                          ECS021T5
00881                            TO TOT-CNC-CNT     (TOT-IDX DTE-IDX)   ECS021T5
00882                               TOT-CNC-CNT     (3 DTE-IDX).        ECS021T5
00883      ADD SW-ISS-PREM   (BRK-IDX DTE-IDX)                          ECS021T5
00884                            TO TOT-ISS-PREM    (TOT-IDX DTE-IDX)   ECS021T5
00885                               TOT-ISS-PREM    (3 DTE-IDX).        ECS021T5
00886      ADD SW-CNC-PREM   (BRK-IDX DTE-IDX)                          ECS021T5
00887                            TO TOT-CNC-PREM    (TOT-IDX DTE-IDX)   ECS021T5
00888                               TOT-CNC-PREM    (3 DTE-IDX).        ECS021T5
00889      ADD SW-NET-COMPEN (BRK-IDX DTE-IDX)                          ECS021T5
00890                            TO TOT-NET-COMPEN (TOT-IDX DTE-IDX)    ECS021T5
00891                               TOT-NET-COMPEN (3 DTE-IDX).         ECS021T5
00892                                                                   ECS021T5
00893      IF SW-BENEFIT-TYPE (BRK-IDX) = LIFE-OVERRIDE-L1              ECS021T5
00894          ADD SW-CLM-CNT (BRK-IDX DTE-IDX)                         ECS021T5
00895                            TO TOT-LF-CLM-CNT   (TOT-IDX DTE-IDX)  ECS021T5
00896                               TOT-LF-CLM-CNT   (3 DTE-IDX)        ECS021T5
00897          ADD SW-CLM-AMT (BRK-IDX DTE-IDX)                         ECS021T5
00898                            TO TOT-LF-CLM-AMT   (TOT-IDX DTE-IDX)  ECS021T5
00899                               TOT-LF-CLM-AMT   (3 DTE-IDX).       ECS021T5
00900                                                                   ECS021T5
00901      IF SW-BENEFIT-TYPE (BRK-IDX) = AH-OVERRIDE-L1                ECS021T5
00902          ADD SW-CLM-CNT (BRK-IDX DTE-IDX)                         ECS021T5
00903                            TO TOT-AH-CLM-CNT   (TOT-IDX DTE-IDX)  ECS021T5
00904                               TOT-AH-CLM-CNT   (3 DTE-IDX)        ECS021T5
00905          ADD SW-CLM-AMT (BRK-IDX DTE-IDX)                         ECS021T5
00906                            TO TOT-AH-CLM-AMT   (TOT-IDX DTE-IDX)  ECS021T5
00907                               TOT-AH-CLM-AMT   (3 DTE-IDX).       ECS021T5
00908                                                                   ECS021T5
00909      ADD SW-LOSS-RESV   (BRK-IDX DTE-IDX)                         ECS021T5
00910                            TO TOT-LOSS-RESV    (TOT-IDX DTE-IDX)  ECS021T5
00911                               TOT-LOSS-RESV    (3 DTE-IDX).       ECS021T5
00912      ADD SW-PRM-EARND   (BRK-IDX DTE-IDX)                         ECS021T5
00913                            TO TOT-EARND-PREM   (TOT-IDX DTE-IDX)  ECS021T5
00914                               TOT-EARND-PREM   (3 DTE-IDX).       ECS021T5
00915      ADD SW-PRM-INFRC   (BRK-IDX DTE-IDX)                         ECS021T5
00916                            TO TOT-PRM-INFRC    (TOT-IDX DTE-IDX)  ECS021T5
00917                               TOT-PRM-INFRC    (3 DTE-IDX).       ECS021T5
00918      ADD SW-INFRC-CNT   (BRK-IDX DTE-IDX)                         ECS021T5
00919                            TO TOT-INFRC-CNT    (TOT-IDX DTE-IDX)  ECS021T5
00920                               TOT-INFRC-CNT    (3 DTE-IDX).       ECS021T5
00921      ADD SW-AVG-AGE     (BRK-IDX DTE-IDX)                         ECS021T5
00922                            TO TOT-AVG-AGE      (TOT-IDX DTE-IDX)  ECS021T5
00923                               TOT-AVG-AGE      (3 DTE-IDX).       ECS021T5
00924      ADD SW-AVG-ORG-TRM (BRK-IDX DTE-IDX)                         ECS021T5
00925                            TO TOT-AVG-ORG-TRM (TOT-IDX DTE-IDX)   ECS021T5
00926                               TOT-AVG-ORG-TRM (3 DTE-IDX).        ECS021T5
00927      ADD SW-WGHT-AGE    (BRK-IDX DTE-IDX)                         ECS021T5
00928                            TO TOT-WGHT-AGE     (TOT-IDX DTE-IDX)  ECS021T5
00929                               TOT-WGHT-AGE     (3 DTE-IDX).       ECS021T5
00930      ADD SW-WGHT-ORG-TRM(BRK-IDX DTE-IDX)                         ECS021T5
00931                            TO TOT-WGHT-ORG-TRM(TOT-IDX DTE-IDX)   ECS021T5
00932                               TOT-WGHT-ORG-TRM(3 DTE-IDX).        ECS021T5
00933      ADD SW-EXP-PCT     (BRK-IDX DTE-IDX)                         ECS021T5
00934                            TO TOT-EXP-PCT      (TOT-IDX DTE-IDX)  ECS021T5
00935                               TOT-EXP-PCT      (3 DTE-IDX).       ECS021T5
00936      ADD SW-ADDED-TO-CNT (BRK-IDX DTE-IDX)                        ECS021T5
00937                            TO TOT-ADDED-TO-CNT(TOT-IDX DTE-IDX)   ECS021T5
00938                               TOT-ADDED-TO-CNT(3 DTE-IDX).        ECS021T5
00939                                                                   ECS021T5
00940      ADD +1 TO DTE-IDX.                                           ECS021T5
00941                                                                   ECS021T5
00942      IF DTE-IDX LESS +16                                          ECS021T5
00943          GO TO 6000-DATE-RANGE-LOOP.                              ECS021T5
00944                                                                   ECS021T5
00945  6000-EXIT.                                                       ECS021T5
00946       EXIT.                                                       ECS021T5
00947                                                                   ECS021T5
00948      EJECT                                                        ECS021T5
00949  7000-MOVE-TABLES.                                                ECS021T5
00950                                                                   ECS021T5
00951      MOVE BREAK-TABLE            TO LINK-TABLE.                   ECS021T5
092602*    MOVE BREAK-EXTENSION-1      TO LINK-EXTENSION-1.             ECS021T5
092602*    MOVE BREAK-EXTENSION-2      TO LINK-EXTENSION-2.             ECS021T5
092602*    MOVE BREAK-EXTENSION-3      TO LINK-EXTENSION-3.                CL**6
00955      MOVE BREAK-TOTAL-TABLE      TO LINK-TOTAL-TABLE.             ECS021T5
00956                                                                   ECS021T5
00957  7000-EXIT.                                                       ECS021T5
00958       EXIT.                                                       ECS021T5
00959                                                                   ECS021T5
00960      EJECT                                                        ECS021T5
00961 ***********************************                               ECS021T5
00962 *     (8000) ZERO TABLE TWO       *                               ECS021T5
00963 ***********************************                               ECS021T5
00964                                                                   ECS021T5
00965  8000-ZERO-ENTIRE-TABLE.                                          ECS021T5
00966                                                                   ECS021T5
00967       MOVE +1                    TO BEN-IDX.                      ECS021T5
00968                                                                   ECS021T5
00969  8000-ZERO-BENEFIT-LOOP.                                          ECS021T5
00970                                                                   ECS021T5
00971       MOVE ZERO-TABLE            TO                               ECS021T5
00972                                  TABLE-ACCUMULATORS (BEN-IDX).    ECS021T5
00973                                                                   ECS021T5
092602*     IF BEN-IDX LESS THAN THREE-HUNDRED                          ECS021T5
092602      IF BEN-IDX LESS THAN  NINE-HUNDRED                          ECS021T5
00975           ADD +1                 TO BEN-IDX                       ECS021T5
00976               GO TO 8000-ZERO-BENEFIT-LOOP.                       ECS021T5
00977                                                                   ECS021T5
00978  8000-ZERO-TOTAL-TABLE.                                           ECS021T5
00979                                                                   ECS021T5
00980       MOVE +1                    TO TOT-IDX.                      ECS021T5
00981                                                                   ECS021T5
00982  8000-ZERO-TOTAL-LOOP.                                            ECS021T5
00983                                                                   ECS021T5
00984       MOVE ZERO-TOTAL-TABLE      TO TABLE-TOTALS (TOT-IDX).       ECS021T5
00985                                                                   ECS021T5
00986       IF TOT-IDX LESS THAN THREE                                  ECS021T5
00987           ADD +1                 TO TOT-IDX                       ECS021T5
00988               GO TO 8000-ZERO-TOTAL-LOOP.                         ECS021T5
00989                                                                   ECS021T5
00990  8000-EXIT.                                                       ECS021T5
00991      EXIT.                                                        ECS021T5
00992                                                                   ECS021T5
00993      EJECT                                                        ECS021T5
00994 *******************************************                       ECS021T5
00995 *     (9000) INITIALIZE ZERO-ACCUMS       *                       ECS021T5
00996 *******************************************                       ECS021T5
00997                                                                   ECS021T5
00998  9000-INITIALIZE-ZERO-TABLE.                                      ECS021T5
00999                                                                   ECS021T5
01000       MOVE +1                    TO DTE-IDX.                      ECS021T5
01001                                                                   ECS021T5
01002  9000-ZERO-DATE-LOOP.                                             ECS021T5
01003                                                                   ECS021T5
01004       MOVE ZERO-TABLE-ENTRIES    TO ZERO-PERIOD (DTE-IDX).        ECS021T5
01005                                                                   ECS021T5
01006       IF DTE-IDX LESS THAN SIXTEEN                                ECS021T5
01007           ADD +1                 TO DTE-IDX                       ECS021T5
01008               GO TO 9000-ZERO-DATE-LOOP.                          ECS021T5
01009                                                                   ECS021T5
01010       MOVE +1                    TO DTE-IDX.                      ECS021T5
01011                                                                   ECS021T5
01012  9000-ZERO-TOTAL-DATE-LOOP.                                       ECS021T5
01013                                                                   ECS021T5
01014       MOVE ZERO-TOTAL-ENTRY      TO ZERO-TOTAL-ACCUMS (DTE-IDX).  ECS021T5
01015                                                                   ECS021T5
01016       IF DTE-IDX LESS THAN SIXTEEN                                ECS021T5
01017           ADD +1                 TO DTE-IDX                       ECS021T5
01018               GO TO 9000-ZERO-TOTAL-DATE-LOOP.                    ECS021T5
01019                                                                   ECS021T5
01020  9000-EXIT.                                                       ECS021T5
01021      EXIT.                                                        ECS021T5
01022                                                                   ECS021T5
01023      EJECT                                                        ECS021T5
01024  ABEND-PGM SECTION.                                               ECS021T5
01025            COPY ELCABEND SUPPRESS.                                ECS021T5
