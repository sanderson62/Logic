00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   ECS021T2
00003  PROGRAM-ID.                ECS021T2.                                LV009
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 11/28/95 11:02:02.                    CL**7
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE.            CL**8
00008 *                           VMOD=2.008.                              CL**9
00009                                                                   ECS021T2
00010                                                                   ECS021T2
00011 *AUTHOR.        LOGIC, INC.                                          CL**7
00012 *               DALLAS, TEXAS.                                       CL**7
00013                                                                   ECS021T2
00014                                                                   ECS021T2
00015 *DATE-COMPILED.                                                      CL**7
00016                                                                   ECS021T2
00017 *SECURITY.   *****************************************************   CL**7
00018 *            *                                                   *   CL**7
00019 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00020 *            *                                                   *   CL**7
00021 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00022 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00023 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00024 *            *                                                   *   CL**7
00025 *            *****************************************************   CL**7
00026                                                                   ECS021T2
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
00031                                                                   ECS021T2
00032  ENVIRONMENT DIVISION.                                            ECS021T2
00033  INPUT-OUTPUT SECTION.                                            ECS021T2
00034  FILE-CONTROL.                                                    ECS021T2
00035                                                                   ECS021T2
00036  EJECT                                                            ECS021T2
00037  DATA DIVISION.                                                   ECS021T2
00038  FILE SECTION.                                                    ECS021T2
00039                                                                   ECS021T2
00040                                                                   ECS021T2
00041  EJECT                                                            ECS021T2
00042  WORKING-STORAGE SECTION.                                         ECS021T2
00043  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL**7
00044                                                                   ECS021T2
00045  77  FILLER  PIC X(32) VALUE '********************************'.  ECS021T2
00046  77  FILLER  PIC X(32) VALUE '     ECS021T2 WORKING STORAGE   '.  ECS021T2
00047  77  FILLER  PIC X(32) VALUE '********* VMOD=2.008 ***********'.     CL**8
00048                                                                   ECS021T2
00049  77  NET-COUNT               PIC S9(7)     COMP-3.                   CL**5
00050  77  DTE-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T2
00051  77  TOT-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T2
00052  77  BEN-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T2
00053  77  BRK-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T2
00054  77  REQ-IDX                 PIC S9(4)     COMP   VALUE +0.       ECS021T2
00055  77  ONE                     PIC  99              VALUE 1.        ECS021T2
00056  77  THREE                   PIC  99              VALUE 3.        ECS021T2
00057  77  SIXTEEN                 PIC S9(4)     COMP   VALUE +16.      ECS021T2
00058  77  THREE-HUNDRED           PIC S9(4)     COMP   VALUE +300.     ECS021T2
092602 77  NINE-HUNDRED            PIC S9(4)     COMP   VALUE +900.     ECS021T2
00059                                                                   ECS021T2
00060  01  WS-ABEND-STORAGE.                                            ECS021T2
00061      12  WS-RETURN-CODE          PIC S9(4)  VALUE +0 COMP.        ECS021T2
00062      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         ECS021T2
00063      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          ECS021T2
00064      12  WS-ZERO                 PIC S9     VALUE +0 COMP-3.      ECS021T2
00065      12  WS-ABEND-CODE           PIC S9(4).                       ECS021T2
00066      12  WORK-ABEND-CODE  REDEFINES  WS-ABEND-CODE.               ECS021T2
00067          16  WAC-1               PIC X.                           ECS021T2
00068          16  WAC-2               PIC X.                           ECS021T2
00069          16  WAC-3-4             PIC 99.                          ECS021T2
00070                                                                   ECS021T2
00071  01  WS-PROGRAM-ACTIONS.                                          ECS021T2
00072      12  W-BUILD-ZERO-TABLE      PIC X(02) VALUE 'BZ'.            ECS021T2
00073      12  W-ZERO-TABLE            PIC X(02) VALUE 'ZT'.            ECS021T2
00074      12  W-APPLY-SORT-RCD        PIC X(02) VALUE 'AS'.            ECS021T2
00075      12  W-ADD-LINK-TABLES       PIC X(02) VALUE 'AT'.            ECS021T2
00076      12  W-MOVE-TO-LINK          PIC X(02) VALUE 'MT'.            ECS021T2
00077                                                                   ECS021T2
00078      EJECT                                                        ECS021T2
00079  01  TABLE-IDENTIFIER.                                            ECS021T2
00080      12  FILLER                       PIC X(28)                   ECS021T2
00081                        VALUE '**** START OF TABLE TWO ****'.      ECS021T2
00082                                                                   ECS021T2
00083  01  BREAK-TABLE.                                                 ECS021T2
092602     12  TABLE-ACCUMULATORS     OCCURS  900 TIMES.                   CL**6
00085          16  TBL-BENEFIT-TYPE             PIC X.                  ECS021T2
00086          16  TBL-BENEFIT-CODE             PIC XX.                 ECS021T2
00087          16  TBL-PERIOD     OCCURS   15  TIMES  COMP-3.           ECS021T2
00088              20  TBL-ISS-CNT          PIC S9(7).                  ECS021T2
00089              20  TBL-CNC-CNT          PIC S9(7).                  ECS021T2
00090              20  TBL-ISS-PREM         PIC S9(11)V99.              ECS021T2
00091              20  TBL-CNC-PREM         PIC S9(11)V99.              ECS021T2
00092              20  TBL-NET-COMPEN       PIC S9(11)V99.              ECS021T2
00093              20  TBL-CLM-CNT          PIC S9(7).                  ECS021T2
00094              20  TBL-CLM-AMT          PIC S9(11)V99.              ECS021T2
00095              20  TBL-LOSS-RESV        PIC S9(11)V99.              ECS021T2
00096              20  TBL-EARND-PREM       PIC S9(9)V99.               ECS021T2
00097              20  TBL-PRM-INFRC        PIC S9(9)V99.               ECS021T2
00098              20  TBL-INFRC-CNT        PIC S9(9).                  ECS021T2
00099              20  TBL-AVG-AGE          PIC S9(9).                     CL**6
00100              20  TBL-AVG-ORG-TRM      PIC S9(9).                     CL**6
00101              20  TBL-WGHT-AGE         PIC S9(9).                     CL**6
00102              20  TBL-WGHT-ORG-TRM     PIC S9(9).                     CL**6
00103              20  TBL-EXP-PCT          PIC S9(3)V9(4).             ECS021T2
00104              20  TBL-ADDED-TO-CNT     PIC S9(5).                  ECS021T2
00105                                                                   ECS021T2
00106      EJECT                                                        ECS021T2
092602*01  BREAK-EXTENSION-1.                                           ECS021T2
00108 *    12  TABLE-EXTENSION-1      OCCURS   75 TIMES.                   CL**6
00109 *        16  FILLER                       PIC X.                  ECS021T2
00110 *        16  FILLER                       PIC XX.                 ECS021T2
00111 *        16  TBL-EXTENSION-1    OCCURS   15  TIMES  COMP-3.       ECS021T2
00112 *            20  FILLER               PIC S9(7).                  ECS021T2
00113 *            20  FILLER               PIC S9(7).                  ECS021T2
00114 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00115 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00116 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00117 *            20  FILLER               PIC S9(7).                  ECS021T2
00118 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00119 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00120 *            20  FILLER               PIC S9(9)V99.               ECS021T2
00121 *            20  FILLER               PIC S9(9)V99.               ECS021T2
00122 *            20  FILLER               PIC S9(9).                  ECS021T2
00123 *            20  FILLER               PIC S9(9).                     CL**6
00124 *            20  FILLER               PIC S9(9).                     CL**6
00125 *            20  FILLER               PIC S9(9).                     CL**6
00126 *            20  FILLER               PIC S9(9).                     CL**6
00127 *            20  FILLER               PIC S9(3)V9(4).             ECS021T2
00128 *            20  FILLER               PIC S9(5).                  ECS021T2
00129 *                                                                 ECS021T2
00130 *    EJECT                                                        ECS021T2
00131 *01  BREAK-EXTENSION-2.                                           ECS021T2
00132 *    12  TABLE-EXTENSION-2      OCCURS   75 TIMES.                   CL**6
00133 *        16  FILLER                       PIC X.                  ECS021T2
00134 *        16  FILLER                       PIC XX.                 ECS021T2
00135 *        16  TBL-EXTENSION-2    OCCURS   15  TIMES  COMP-3.       ECS021T2
00136 *            20  FILLER               PIC S9(7).                  ECS021T2
00137 *            20  FILLER               PIC S9(7).                  ECS021T2
00138 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00139 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00140 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00141 *            20  FILLER               PIC S9(7).                  ECS021T2
00142 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00143 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00144 *            20  FILLER               PIC S9(9)V99.               ECS021T2
00145 *            20  FILLER               PIC S9(9)V99.               ECS021T2
00146 *            20  FILLER               PIC S9(9).                  ECS021T2
00147 *            20  FILLER               PIC S9(9).                     CL**6
00148 *            20  FILLER               PIC S9(9).                     CL**6
00149 *            20  FILLER               PIC S9(9).                     CL**6
00150 *            20  FILLER               PIC S9(9).                     CL**6
00151 *            20  FILLER               PIC S9(3)V9(4).                CL**6
00152 *            20  FILLER               PIC S9(5).                     CL**6
00153 *                                                                    CL**6
00154 *01  BREAK-EXTENSION-3.                                              CL**6
00155 *    12  TABLE-EXTENSION-3      OCCURS   75 TIMES.                   CL**6
00156 *        16  FILLER                       PIC X.                     CL**6
00157 *        16  FILLER                       PIC XX.                    CL**6
00158 *        16  TBL-EXTENSION-3    OCCURS   15  TIMES  COMP-3.          CL**6
00159 *            20  FILLER               PIC S9(7).                     CL**2
00160 *            20  FILLER               PIC S9(7).                     CL**2
00161 *            20  FILLER               PIC S9(11)V99.                 CL**6
00162 *            20  FILLER               PIC S9(11)V99.                 CL**6
00163 *            20  FILLER               PIC S9(11)V99.                 CL**6
00164 *            20  FILLER               PIC S9(7).                     CL**2
00165 *            20  FILLER               PIC S9(11)V99.                 CL**6
00166 *            20  FILLER               PIC S9(11)V99.                 CL**6
00167 *            20  FILLER               PIC S9(9)V99.                  CL**6
00168 *            20  FILLER               PIC S9(9)V99.                  CL**6
00169 *            20  FILLER               PIC S9(9).                     CL**6
00170 *            20  FILLER               PIC S9(9).                     CL**6
00171 *            20  FILLER               PIC S9(9).                     CL**6
00172 *            20  FILLER               PIC S9(9).                     CL**6
00173 *            20  FILLER               PIC S9(9).                     CL**6
00174 *            20  FILLER               PIC S9(3)V9(4).             ECS021T2
092602*            20  FILLER               PIC S9(5).                  ECS021T2
00176                                                                   ECS021T2
00177      EJECT                                                        ECS021T2
00178  01  TOTAL-IDENTIFIER.                                            ECS021T2
00179      12  FILLER                       PIC X(28)                   ECS021T2
00180                        VALUE '**** START OF TOTAL TWO ****'.      ECS021T2
00181                                                                   ECS021T2
00182  01  BREAK-TOTAL-TABLE.                                           ECS021T2
00183      12  TABLE-TOTALS           OCCURS  3 TIMES.                  ECS021T2
00184          16  TOT-PERIOD     OCCURS   15  TIMES  COMP-3.           ECS021T2
00185              20  TOT-ISS-CNT          PIC S9(7).                  ECS021T2
00186              20  TOT-CNC-CNT          PIC S9(7).                  ECS021T2
00187              20  TOT-SINGLE-ELEM      PIC S9(7).                  ECS021T2
00188              20  TOT-JOINT-RETRO      PIC S9(7).                  ECS021T2
00189              20  TOT-LIFE-LEVEL       PIC S9(7).                  ECS021T2
00190              20  TOT-ISS-PREM         PIC S9(11)V99.              ECS021T2
00191              20  TOT-CNC-PREM         PIC S9(11)V99.              ECS021T2
00192              20  TOT-NET-COMPEN       PIC S9(11)V99.              ECS021T2
00193              20  TOT-LF-CLM-CNT       PIC S9(7).                  ECS021T2
00194              20  TOT-LF-CLM-AMT       PIC S9(11)V99.              ECS021T2
00195              20  TOT-AH-CLM-CNT       PIC S9(7).                  ECS021T2
00196              20  TOT-AH-CLM-AMT       PIC S9(11)V99.              ECS021T2
00197              20  TOT-LOSS-RESV        PIC S9(11)V99.              ECS021T2
00198              20  TOT-EARND-PREM       PIC S9(9)V99.               ECS021T2
00199              20  TOT-PRM-INFRC        PIC S9(9)V99.               ECS021T2
00200              20  TOT-INFRC-CNT        PIC S9(9).                  ECS021T2
00201              20  TOT-AVG-AGE          PIC S9(9).                     CL**6
00202              20  TOT-AVG-ORG-TRM      PIC S9(9).                     CL**6
00203              20  TOT-WGHT-AGE         PIC S9(9).                     CL**6
00204              20  TOT-WGHT-ORG-TRM     PIC S9(9).                     CL**6
00205              20  TOT-EXP-PCT          PIC S999V9(4).              ECS021T2
00206              20  TOT-ADDED-TO-CNT     PIC S9(5).                  ECS021T2
00207                                                                   ECS021T2
00208      EJECT                                                        ECS021T2
00209                                                                   ECS021T2
00210  01  ZERO-IDENTIFIER.                                             ECS021T2
00211      12  FILLER                       PIC X(28)                   ECS021T2
00212                        VALUE '**** START OF ZERO TABLE****'.      ECS021T2
00213                                                                   ECS021T2
00214  01  ZERO-TABLE.                                                  ECS021T2
00215      12  ZERO-ACCUMULATORS.                                       ECS021T2
00216          16  ZERO-BENEFIT-TYPE            PIC X    VALUE SPACES.  ECS021T2
00217          16  ZERO-BENEFIT-CODE            PIC XX   VALUE ZEROS.   ECS021T2
00218          16  ZERO-PERIOD       OCCURS     15 TIMES COMP-3.        ECS021T2
00219              20  ZERO-ISS-CNT         PIC S9(7).                  ECS021T2
00220              20  ZERO-CNC-CNT         PIC S9(7).                  ECS021T2
00221              20  ZERO-ISS-PREM        PIC S9(11)V99.              ECS021T2
00222              20  ZERO-CNC-PREM        PIC S9(11)V99.              ECS021T2
00223              20  ZERO-NET-COMPEN      PIC S9(11)V99.              ECS021T2
00224              20  ZERO-CLM-CNT         PIC S9(7).                  ECS021T2
00225              20  ZERO-CLM-AMT         PIC S9(11)V99.              ECS021T2
00226              20  ZERO-LOSS-RESV       PIC S9(11)V99.              ECS021T2
00227              20  ZERO-EARND-PREM      PIC S9(9)V99.               ECS021T2
00228              20  ZERO-PRM-INFRC       PIC S9(9)V99.               ECS021T2
00229              20  ZERO-INFRC-CNT       PIC S9(9).                  ECS021T2
00230              20  ZERO-AVG-AGE         PIC S9(9).                     CL**6
00231              20  ZERO-AVG-ORG-TRM     PIC S9(9).                     CL**6
00232              20  ZERO-WGHT-AGE        PIC S9(9).                     CL**6
00233              20  ZERO-WGHT-ORG-TRM    PIC S9(9).                     CL**6
00234              20  ZERO-EXP-PCT         PIC S9(3)V9(4).             ECS021T2
00235              20  ZERO-ADDED-TO-CNT    PIC S9(5).                  ECS021T2
00236                                                                   ECS021T2
00237  01  ZERO-ENTRY-IDENTIFIER.                                       ECS021T2
00238      12  FILLER                   PIC X(28)                       ECS021T2
00239                            VALUE '****START OF ZERO ENTRY ****'.  ECS021T2
00240                                                                   ECS021T2
00241  01  ZERO-TABLE-ENTRIES.                                          ECS021T2
00242      12  ZERO-ACCUMS              COMP-3.                         ECS021T2
00243          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T2
00244          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T2
00245          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T2
00246          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T2
00247          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T2
00248          16  FILLER               PIC S9(7)       VALUE +0.       ECS021T2
00249          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T2
00250          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021T2
00251          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021T2
00252          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021T2
00253          16  FILLER               PIC S9(9)       VALUE +0.       ECS021T2
00254          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00255          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00256          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00257          16  FILLER               PIC S9(9)       VALUE +0.          CL**6
00258          16  FILLER               PIC S9(3)V9(4)  VALUE +0.       ECS021T2
00259          16  FILLER               PIC S9(5)       VALUE +0.       ECS021T2
00260                                                                   ECS021T2
00261      EJECT                                                        ECS021T2
00262                                                                   ECS021T2
00263  01  ZERO-TOTAL-IDENTIFIER.                                       ECS021T2
00264      12  FILLER                   PIC X(28)                       ECS021T2
00265                            VALUE '****START OF ZERO TOTAL ****'.  ECS021T2
00266                                                                   ECS021T2
00267  01  ZERO-TOTAL-TABLE.                                            ECS021T2
00268      12  ZERO-TOTAL-ACCUMS  OCCURS  15 TIMES  COMP-3.             ECS021T2
00269          16  FILLER                   PIC S9(7).                  ECS021T2
00270          16  FILLER                   PIC S9(7).                  ECS021T2
00271          16  FILLER                   PIC S9(7).                  ECS021T2
00272          16  FILLER                   PIC S9(7).                  ECS021T2
00273          16  FILLER                   PIC S9(7).                  ECS021T2
00274          16  FILLER                   PIC S9(11)V99.              ECS021T2
00275          16  FILLER                   PIC S9(11)V99.              ECS021T2
00276          16  FILLER                   PIC S9(11)V99.              ECS021T2
00277          16  FILLER                   PIC S9(7).                  ECS021T2
00278          16  FILLER                   PIC S9(11)V99.              ECS021T2
00279          16  FILLER                   PIC S9(7).                  ECS021T2
00280          16  FILLER                   PIC S9(11)V99.              ECS021T2
00281          16  FILLER                   PIC S9(11)V99.              ECS021T2
00282          16  FILLER                   PIC S9(9)V99.               ECS021T2
00283          16  FILLER                   PIC S9(9)V99.               ECS021T2
00284          16  FILLER                   PIC S9(9).                  ECS021T2
00285          16  FILLER                   PIC S9(9).                     CL**6
00286          16  FILLER                   PIC S9(9).                     CL**6
00287          16  FILLER                   PIC S9(9).                     CL**6
00288          16  FILLER                   PIC S9(9).                     CL**6
00289          16  FILLER                   PIC S999V9(4).              ECS021T2
00290          16  FILLER                   PIC S9(5).                  ECS021T2
00291                                                                   ECS021T2
00292  01  ZERO-TOTAL-ENTRY         COMP-3.                             ECS021T2
00293      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T2
00294      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T2
00295      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T2
00296      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T2
00297      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T2
00298      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T2
00299      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T2
00300      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T2
00301      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T2
00302      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T2
00303      12  FILLER                       PIC S9(7)      VALUE +0.    ECS021T2
00304      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T2
00305      12  FILLER                       PIC S9(11)V99  VALUE +0.    ECS021T2
00306      12  FILLER                       PIC S9(9)V99   VALUE +0.    ECS021T2
00307      12  FILLER                       PIC S9(9)V99   VALUE +0.    ECS021T2
00308      12  FILLER                       PIC S9(9)      VALUE +0.    ECS021T2
00309      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00310      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00311      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00312      12  FILLER                       PIC S9(9)      VALUE +0.       CL**6
00313      12  FILLER                       PIC S999V9(4)  VALUE +0.    ECS021T2
00314      12  FILLER                       PIC S9(5)      VALUE +0.    ECS021T2
00315                                                                   ECS021T2
00316                                                                   ECS021T2
00317      EJECT                                                        ECS021T2
00318  LINKAGE SECTION.                                                 ECS021T2
00319                                                                   ECS021T2
00320 *************************************************                 ECS021T2
00321 *  ACTION REQUESTS:                             *                 ECS021T2
00322 *                                               *                 ECS021T2
00323 *  BZ - BUILD ZERO TABLE (1ST TIME PROCESSING)  *                 ECS021T2
00324 *  ZT - ZERO INTERNAL TABLES                    *                 ECS021T2
00325 *  AS - APPLY SORT RECORD TO INTERNAL TABLES    *                 ECS021T2
00326 *  AT - ADD LINKAGE TABLES TO INTERNAL TABLES   *                 ECS021T2
00327 *  MT - MOVE TABLES TO LINK TABLES              *                 ECS021T2
00328 *************************************************                 ECS021T2
00329                                                                   ECS021T2
00330  01  REQUEST-TABLE.                                               ECS021T2
00331      12  PROCESSING-REQUEST    OCCURS 10   PIC X(2).              ECS021T2
00332      12  NUMBER-OF-REQUESTS                PIC S9(04) COMP.       ECS021T2
00333                                                                   ECS021T2
00334  01  SW-RECORD.                                                   ECS021T2
00335      12  SW-REPORT-CONTROL-KEY.                                   ECS021T2
00336          16  SW-BREAK-FIELD-1             PIC X(10).              ECS021T2
00337          16  SW-BREAK-FIELD-2             PIC X(10).              ECS021T2
00338          16  SW-BREAK-FIELD-3             PIC X(10).              ECS021T2
00339          16  SW-BREAK-FIELD-4             PIC X(10).              ECS021T2
00340          16  SW-BREAK-FIELD-5             PIC X(10).              ECS021T2
00341          16  SW-BREAK-FIELD-6             PIC X(10).              ECS021T2
00342          16  SW-ACCT-NAME                 PIC X(30).              ECS021T2
121707         16  SW-ACCT-STATUS               PIC X.
00343          16  SW-GA-NAME                   PIC X(30).                 CL**3
00344          16  SW-STATE-NAME                PIC X(25).                 CL**4
00345          16  SW-PROD-DATE                 PIC X(6).               ECS021T2
00346                                                                   ECS021T2
00347      12  SW-RECORD-DATA            OCCURS 23.                        CL**6
00348          16  SW-BENEFIT-TYPE              PIC X.                  ECS021T2
00349          16  SW-BENEFIT-CODE              PIC 99.                 ECS021T2
00350          16  SW-BEN-TBL-POS               PIC S999 COMP.          ECS021T2
00351                                                                   ECS021T2
00352          16  SW-PERIOD    COMP-3   OCCURS 15.                     ECS021T2
00353              24  SW-ISS-CNT               PIC S9(7).              ECS021T2
00354              24  SW-CNC-CNT               PIC S9(7).              ECS021T2
00355              24  SW-ISS-PREM              PIC S9(11)V99.          ECS021T2
00356              24  SW-CNC-PREM              PIC S9(11)V99.          ECS021T2
00357              24  SW-NET-COMPEN            PIC S9(11)V99.          ECS021T2
00358              24  SW-CLM-CNT               PIC S9(7).              ECS021T2
00359              24  SW-CLM-AMT               PIC S9(11)V99.          ECS021T2
00360              24  SW-LOSS-RESV             PIC S9(11)V99.          ECS021T2
00361              24  SW-PRM-EARND             PIC S9(9)V99.           ECS021T2
00362              24  SW-PRM-INFRC             PIC S9(9)V99.           ECS021T2
00363              24  SW-INFRC-CNT             PIC S9(9).              ECS021T2
00364              24  SW-AVG-AGE               PIC S9(9).                 CL**6
00365              24  SW-AVG-ORG-TRM           PIC S9(9).                 CL**6
00366              24  SW-WGHT-AGE              PIC S9(9).                 CL**6
00367              24  SW-WGHT-ORG-TRM          PIC S9(9).                 CL**6
00368              24  SW-EXP-PCT               PIC S999V9(4).          ECS021T2
00369              24  SW-ADDED-TO-CNT          PIC S9(5).              ECS021T2
00370                                                                   ECS021T2
00371      EJECT                                                        ECS021T2
00372  01  LINK-TABLE.                                                  ECS021T2
092602     12  LINK-ACCUMULATORS   OCCURS  900 TIMES.                      CL**6
00374          16  LINK-BENEFIT-TYPE        PIC X.                      ECS021T2
00375          16  LINK-BENEFIT-CODE        PIC 99.                     ECS021T2
00376          16  LINK-PERIOD    OCCURS   15  TIMES  COMP-3.           ECS021T2
00377              20  LINK-ISS-CNT         PIC S9(7).                  ECS021T2
00378              20  LINK-CNC-CNT         PIC S9(7).                  ECS021T2
00379              20  LINK-ISS-PREM        PIC S9(11)V99.              ECS021T2
00380              20  LINK-CNC-PREM        PIC S9(11)V99.              ECS021T2
00381              20  LINK-NET-COMPEN      PIC S9(11)V99.              ECS021T2
00382              20  LINK-CLM-CNT         PIC S9(7).                  ECS021T2
00383              20  LINK-CLM-AMT         PIC S9(11)V99.              ECS021T2
00384              20  LINK-LOSS-RESV       PIC S9(11)V99.              ECS021T2
00385              20  LINK-EARND-PREM      PIC S9(9)V99.               ECS021T2
00386              20  LINK-PRM-INFRC       PIC S9(9)V99.               ECS021T2
00387              20  LINK-INFRC-CNT       PIC S9(9).                  ECS021T2
00388              20  LINK-AVG-AGE         PIC S9(9).                     CL**6
00389              20  LINK-AVG-ORG-TRM     PIC S9(9).                     CL**6
00390              20  LINK-WGHT-AGE        PIC S9(9).                     CL**6
00391              20  LINK-WGHT-ORG-TRM    PIC S9(9).                     CL**6
00392              20  LINK-EXP-PCT         PIC S999V9(4).              ECS021T2
00393              20  LINK-ADDED-TO-CNT    PIC S9(5).                  ECS021T2
00394                                                                   ECS021T2
092602*01  LINK-EXTENSION-1.                                            ECS021T2
00396 *    12  LINK-TBL-EXTENSION-1    OCCURS   75 TIMES.                  CL**6
00397 *        16  FILLER                       PIC X.                  ECS021T2
00398 *        16  FILLER                       PIC 99.                 ECS021T2
00399 *        16  LINK-EXT-1          OCCURS   15  TIMES  COMP-3.      ECS021T2
00400 *            20  FILLER               PIC S9(7).                  ECS021T2
00401 *            20  FILLER               PIC S9(7).                  ECS021T2
00402 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00403 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00404 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00405 *            20  FILLER               PIC S9(7).                  ECS021T2
00406 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00407 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00408 *            20  FILLER               PIC S9(9)V99.               ECS021T2
00409 *            20  FILLER               PIC S9(9)V99.               ECS021T2
00410 *            20  FILLER               PIC S9(9).                  ECS021T2
00411 *            20  FILLER               PIC S9(9).                     CL**6
00412 *            20  FILLER               PIC S9(9).                     CL**6
00413 *            20  FILLER               PIC S9(9).                     CL**6
00414 *            20  FILLER               PIC S9(9).                     CL**6
00415 *            20  FILLER               PIC S9(3)V9(4).             ECS021T2
00416 *            20  FILLER               PIC S9(5).                  ECS021T2
00417 *                                                                 ECS021T2
00418 *01  LINK-EXTENSION-2.                                            ECS021T2
00419 *    12  LINK-TBL-EXTENSION-2    OCCURS   75 TIMES.                  CL**6
00420 *        16  FILLER                       PIC X.                  ECS021T2
00421 *        16  FILLER                       PIC 99.                 ECS021T2
00422 *        16  LINK-EXT-2          OCCURS   15  TIMES  COMP-3.      ECS021T2
00423 *            20  FILLER               PIC S9(7).                  ECS021T2
00424 *            20  FILLER               PIC S9(7).                  ECS021T2
00425 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00426 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00427 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00428 *            20  FILLER               PIC S9(7).                  ECS021T2
00429 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00430 *            20  FILLER               PIC S9(11)V99.              ECS021T2
00431 *            20  FILLER               PIC S9(9)V99.               ECS021T2
00432 *            20  FILLER               PIC S9(9)V99.               ECS021T2
00433 *            20  FILLER               PIC S9(9).                  ECS021T2
00434 *            20  FILLER               PIC S9(9).                     CL**6
00435 *            20  FILLER               PIC S9(9).                     CL**6
00436 *            20  FILLER               PIC S9(9).                     CL**6
00437 *            20  FILLER               PIC S9(9).                     CL**6
00438 *            20  FILLER               PIC S9(3)V9(4).                CL**6
00439 *            20  FILLER               PIC S9(5).                     CL**6
00440 *                                                                    CL**6
00441 *01  LINK-EXTENSION-3.                                               CL**6
00442 *    12  LINK-TBL-EXTENSION-3    OCCURS   75 TIMES.                  CL**6
00443 *        16  FILLER                       PIC X.                     CL**6
00444 *        16  FILLER                       PIC 99.                    CL**6
00445 *        16  LINK-EXT-3          OCCURS   15  TIMES  COMP-3.         CL**6
00446 *            20  FILLER               PIC S9(7).                     CL**2
00447 *            20  FILLER               PIC S9(7).                     CL**2
00448 *            20  FILLER               PIC S9(11)V99.                 CL**6
00449 *            20  FILLER               PIC S9(11)V99.                 CL**6
00450 *            20  FILLER               PIC S9(11)V99.                 CL**6
00451 *            20  FILLER               PIC S9(7).                     CL**2
00452 *            20  FILLER               PIC S9(11)V99.                 CL**6
00453 *            20  FILLER               PIC S9(11)V99.                 CL**6
00454 *            20  FILLER               PIC S9(9)V99.                  CL**6
00455 *            20  FILLER               PIC S9(9)V99.                  CL**6
00456 *            20  FILLER               PIC S9(9).                     CL**6
00457 *            20  FILLER               PIC S9(9).                     CL**6
00458 *            20  FILLER               PIC S9(9).                     CL**6
00459 *            20  FILLER               PIC S9(9).                     CL**6
00460 *            20  FILLER               PIC S9(9).                     CL**6
00461 *            20  FILLER               PIC S9(3)V9(4).             ECS021T2
092602*            20  FILLER               PIC S9(5).                  ECS021T2
00463                                                                   ECS021T2
00464                                                                   ECS021T2
00465      EJECT                                                        ECS021T2
00466  01  LINK-TOTAL-TABLE.                                            ECS021T2
00467      12  LINK-TOTALS     OCCURS  3 TIMES.                         ECS021T2
00468          16  LINK-T-PERIOD  COMP-3 OCCURS 15 TIMES.               ECS021T2
00469              20  LINK-T-ISS-CNT       PIC S9(7).                  ECS021T2
00470              20  LINK-T-CNC-CNT       PIC S9(7).                  ECS021T2
00471              20  LINK-T-SINGLE-ELEM   PIC S9(7).                  ECS021T2
00472              20  LINK-T-JOINT-RETRO   PIC S9(7).                  ECS021T2
00473              20  LINK-T-LIFE-LEVEL    PIC S9(7).                  ECS021T2
00474              20  LINK-T-ISS-PREM      PIC S9(11)V99.              ECS021T2
00475              20  LINK-T-CNC-PREM      PIC S9(11)V99.              ECS021T2
00476              20  LINK-T-NET-COMPEN    PIC S9(11)V99.              ECS021T2
00477              20  LINK-T-LF-CLM-CNT    PIC S9(7).                  ECS021T2
00478              20  LINK-T-LF-CLM-AMT    PIC S9(11)V99.              ECS021T2
00479              20  LINK-T-AH-CLM-CNT    PIC S9(7).                  ECS021T2
00480              20  LINK-T-AH-CLM-AMT    PIC S9(11)V99.              ECS021T2
00481              20  LINK-T-LOSS-RESV     PIC S9(11)V99.              ECS021T2
00482              20  LINK-T-EARND-PREM    PIC S9(9)V99.               ECS021T2
00483              20  LINK-T-PRM-INFRC     PIC S9(9)V99.               ECS021T2
00484              20  LINK-T-INFRC-CNT     PIC S9(9).                  ECS021T2
00485              20  LINK-T-AVG-AGE       PIC S9(9).                     CL**6
00486              20  LINK-T-AVG-ORG-TRM   PIC S9(9).                     CL**6
00487              20  LINK-T-WGHT-AGE      PIC S9(9).                     CL**6
00488              20  LINK-T-WGHT-ORG-TRM  PIC S9(9).                     CL**6
00489              20  LINK-T-EXP-PCT       PIC S999V9(4).              ECS021T2
00490              20  LINK-T-ADDED-TO-CNT  PIC S9(5).                  ECS021T2
00491                                                                   ECS021T2
00492      EJECT                                                        ECS021T2
00493  01  CLASIC-SYSTEM-CODES.                                         ECS021T2
00494      12  DTE-COLC-ID                 PIC  X(4).                   ECS021T2
00495      12  DTE-CLASIC-COMPANY-CD       PIC  X.                      ECS021T2
00496      12  DTE-CLASIC-COMPANY-NUMBER   PIC  999.                    ECS021T2
00497      12  DTE-CLASIC-CLAIM-ACCESS     PIC  X.                      ECS021T2
00498      12  CLASIC-REIN-MAINT           PIC  XX.                     ECS021T2
00499      12  CLASIC-COMP-MAINT           PIC  XX.                     ECS021T2
00500      12  CLASIC-ACCT-MAINT           PIC  XX.                     ECS021T2
00501      12  CLASIC-CTBL-MAINT           PIC  XX.                     ECS021T2
00502      12  CLASIC-RATE-MAINT           PIC  XX.                     ECS021T2
00503      12  CLASIC-CREDIT-EOM-DT        PIC  XX.                     ECS021T2
00504      12  CLASIC-CLAIMS-EOM-DT        PIC  XX.                     ECS021T2
00505                                                                   ECS021T2
00506      12  LIFE-OVERRIDE-L1            PIC  X.                      ECS021T2
00507      12  LIFE-OVERRIDE-L2            PIC  XX.                     ECS021T2
00508      12  LIFE-OVERRIDE-L6            PIC  X(6).                   ECS021T2
00509      12  LIFE-OVERRIDE-L12           PIC  X(12).                  ECS021T2
00510                                                                   ECS021T2
00511      12  AH-OVERRIDE-L1              PIC  X.                      ECS021T2
00512      12  AH-OVERRIDE-L2              PIC  XX.                     ECS021T2
00513      12  AH-OVERRIDE-L6              PIC  X(6).                   ECS021T2
00514      12  AH-OVERRIDE-L12             PIC  X(12).                  ECS021T2
00515                                                                   ECS021T2
00516      12  CLAS-REPORT-CD1-CAPTION     PIC  X(10).                  ECS021T2
00517      12  CLAS-REPORT-CD2-CAPTION     PIC  X(10).                  ECS021T2
00518                                                                   ECS021T2
00519      12  CLASIC-MORTG-EOM-DT         PIC  XX.                     ECS021T2
00520      12  CLASIC-AR-EOM-DT            PIC  XX.                     ECS021T2
00521                                                                   ECS021T2
00522      12  FILLER                      PIC  X(11).                  ECS021T2
00523                                                                   ECS021T2
00524                                                                   ECS021T2
00525  01  CLAS-INS-TYPES.                                              ECS021T2
092602     12 CLAS-ALL-TYPES               OCCURS 900 TIMES.            ECS021T2
00527          16  CLAS-I-BEN              PIC  XX.                     ECS021T2
00528          16  CLAS-I-AB3.                                          ECS021T2
00529              20  FILLER              PIC  X.                      ECS021T2
00530              20  CLAS-I-AB2.                                      ECS021T2
00531                  24  FILLER          PIC  X.                      ECS021T2
00532                  24  CLAS-I-AB1      PIC  X.                      ECS021T2
00533          16  CLAS-I-AB10.                                         ECS021T2
00534              20  FILLER              PIC  X(9).                   ECS021T2
00535              20  CLAS-I-REIN-YN      PIC  X.                      ECS021T2
00536          16  CLAS-I-COMMENT          PIC  X(10).                  ECS021T2
00537          16  CLAS-I-JOINT            PIC  X.                      ECS021T2
00538          16  CLAS-I-RL-AH            PIC  X.                      ECS021T2
00539          16  CLAS-I-CALC-TYPE.                                    ECS021T2
00540              20  CLAS-I-BAL          PIC  X.                      ECS021T2
00541          16  CLAS-I-EP               PIC  X.                      ECS021T2
00542          16  FILLER                  PIC  X(9).                   ECS021T2
00543                                                                   ECS021T2
00544  01  CLAS-INDEX-TBL.                                              ECS021T2
00545      12  CLAX-ID                     PIC X(4).                    ECS021T2
00546      12  CLAS-STARTC                 PIC S9(4) COMP.              ECS021T2
00547      12  CLAS-MAXC                   PIC S9(4) COMP.              ECS021T2
00548      12  CLAS-STARTL                 PIC S9(4) COMP.              ECS021T2
00549      12  CLAS-MAXL                   PIC S9(4) COMP.              ECS021T2
00550      12  CLAS-STARTA                 PIC S9(4) COMP.              ECS021T2
00551      12  CLAS-MAXA                   PIC S9(4) COMP.              ECS021T2
00552      12  CLAS-STARTM                 PIC S9(4) COMP.              ECS021T2
00553      12  CLAS-MAXM                   PIC S9(4) COMP.              ECS021T2
00554      12  CLAS-STARTB                 PIC S9(4) COMP.              ECS021T2
00555      12  CLAS-MAXB                   PIC S9(4) COMP.              ECS021T2
00556      12  CLAS-STARTS                 PIC S9(4) COMP.              ECS021T2
00557      12  CLAS-MAXS                   PIC S9(4) COMP.              ECS021T2
00558      12  CLAS-STARTE                 PIC S9(4) COMP.              ECS021T2
00559      12  CLAS-MAXE                   PIC S9(4) COMP.              ECS021T2
00560      12  CLAS-STARTCN                PIC S9(4) COMP.              ECS021T2
00561      12  CLAS-MAXCN                  PIC S9(4) COMP.              ECS021T2
00562                                                                   ECS021T2
00563      EJECT                                                        ECS021T2
00564  PROCEDURE DIVISION USING REQUEST-TABLE                           ECS021T2
00565                           SW-RECORD                               ECS021T2
00566                           LINK-TABLE                              ECS021T2
092602*                         LINK-EXTENSION-1                        ECS021T2
092602*                         LINK-EXTENSION-2                        ECS021T2
092602*                         LINK-EXTENSION-3                           CL**6
00570                           LINK-TOTAL-TABLE                        ECS021T2
00571                           CLASIC-SYSTEM-CODES                     ECS021T2
00572                           CLAS-INS-TYPES                          ECS021T2
00573                           CLAS-INDEX-TBL.                         ECS021T2
00574                                                                   ECS021T2
00575  1000-PROCESS-REQUESTS.                                           ECS021T2
00576                                                                   ECS021T2
00577      PERFORM 2000-READ-REQUEST-TABLE THRU 2000-EXIT               ECS021T2
00578          VARYING REQ-IDX  FROM  ONE BY ONE                        ECS021T2
00579              UNTIL REQ-IDX GREATER THAN NUMBER-OF-REQUESTS.       ECS021T2
00580                                                                   ECS021T2
00581      GOBACK.                                                      ECS021T2
00582                                                                   ECS021T2
00583  1000-EXIT.                                                       ECS021T2
00584      EXIT.                                                        ECS021T2
00585                                                                   ECS021T2
00586  2000-READ-REQUEST-TABLE.                                         ECS021T2
00587                                                                   ECS021T2
00588      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-BUILD-ZERO-TABLE     ECS021T2
00589          PERFORM 9000-INITIALIZE-ZERO-TABLE THRU 9000-EXIT        ECS021T2
00590          PERFORM 8000-ZERO-ENTIRE-TABLE THRU 8000-EXIT            ECS021T2
00591          GO TO 2000-EXIT.                                         ECS021T2
00592                                                                   ECS021T2
00593      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-ZERO-TABLE           ECS021T2
00594          PERFORM 8000-ZERO-ENTIRE-TABLE THRU 8000-EXIT            ECS021T2
00595          GO TO 2000-EXIT.                                         ECS021T2
00596                                                                   ECS021T2
00597      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-APPLY-SORT-RCD       ECS021T2
00598          PERFORM 5000-APPLY-SORT-RECORD THRU 5000-EXIT            ECS021T2
00599          GO TO 2000-EXIT.                                         ECS021T2
00600                                                                   ECS021T2
00601      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-ADD-LINK-TABLES      ECS021T2
00602          PERFORM 3000-ADD-TABLE-RTN THRU 3000-EXIT                ECS021T2
00603          PERFORM 4000-ADD-TOTAL-RTN THRU 4000-EXIT                ECS021T2
00604          GO TO 2000-EXIT.                                         ECS021T2
00605                                                                   ECS021T2
00606      IF PROCESSING-REQUEST (REQ-IDX) EQUAL W-MOVE-TO-LINK         ECS021T2
00607          PERFORM 7000-MOVE-TABLES THRU 7000-EXIT.                 ECS021T2
00608                                                                   ECS021T2
00609  2000-EXIT.                                                       ECS021T2
00610      EXIT.                                                        ECS021T2
00611                                                                   ECS021T2
00612      EJECT                                                        ECS021T2
00613 *******************************************                       ECS021T2
00614 *  (2000) ADDS TABLE (2) TO LINK TABLE    *                       ECS021T2
00615 *******************************************                       ECS021T2
00616                                                                   ECS021T2
00617  3000-ADD-TABLE-RTN.                                              ECS021T2
00618      MOVE +1                     TO BEN-IDX.                      ECS021T2
00619                                                                   ECS021T2
00620  3000-ZERO-ACCESS-LOOP.                                           ECS021T2
00621      IF BEN-IDX GREATER CLAS-MAXA                                 ECS021T2
00622          GO TO 3000-EXIT.                                         ECS021T2
00623                                                                   ECS021T2
00624      IF LINK-BENEFIT-CODE (BEN-IDX) = ZERO                        ECS021T2
00625          ADD +1                  TO BEN-IDX                       ECS021T2
00626          GO TO 3000-ZERO-ACCESS-LOOP.                             ECS021T2
00627                                                                   ECS021T2
00628      MOVE LINK-BENEFIT-TYPE (BEN-IDX)                             ECS021T2
00629                                  TO TBL-BENEFIT-TYPE (BEN-IDX).   ECS021T2
00630      MOVE LINK-BENEFIT-CODE (BEN-IDX)                             ECS021T2
00631                                  TO TBL-BENEFIT-CODE (BEN-IDX).   ECS021T2
00632                                                                   ECS021T2
00633      MOVE +1                     TO DTE-IDX.                      ECS021T2
00634                                                                   ECS021T2
00635  3000-ACTIVE-BENEFIT-LOOP.                                        ECS021T2
00636                                                                   ECS021T2
00637      ADD LINK-ISS-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T2
00638                               TBL-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T2
00639      ADD LINK-CNC-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T2
00640                               TBL-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T2
00641      ADD LINK-ISS-PREM    (BEN-IDX DTE-IDX) TO                    ECS021T2
00642                               TBL-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T2
00643      ADD LINK-CNC-PREM    (BEN-IDX DTE-IDX) TO                    ECS021T2
00644                               TBL-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T2
00645      ADD LINK-NET-COMPEN  (BEN-IDX DTE-IDX) TO                    ECS021T2
00646                               TBL-NET-COMPEN   (BEN-IDX DTE-IDX). ECS021T2
00647      ADD LINK-CLM-CNT     (BEN-IDX DTE-IDX) TO                    ECS021T2
00648                               TBL-CLM-CNT      (BEN-IDX DTE-IDX). ECS021T2
00649      ADD LINK-CLM-AMT     (BEN-IDX DTE-IDX) TO                    ECS021T2
00650                               TBL-CLM-AMT      (BEN-IDX DTE-IDX). ECS021T2
00651      ADD LINK-LOSS-RESV   (BEN-IDX DTE-IDX) TO                    ECS021T2
00652                               TBL-LOSS-RESV    (BEN-IDX DTE-IDX). ECS021T2
00653      ADD LINK-EARND-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T2
00654                               TBL-EARND-PREM   (BEN-IDX DTE-IDX). ECS021T2
00655      ADD LINK-PRM-INFRC   (BEN-IDX DTE-IDX) TO                    ECS021T2
00656                               TBL-PRM-INFRC    (BEN-IDX DTE-IDX). ECS021T2
00657      ADD LINK-INFRC-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T2
00658                               TBL-INFRC-CNT    (BEN-IDX DTE-IDX). ECS021T2
00659      ADD LINK-AVG-AGE      (BEN-IDX DTE-IDX) TO                   ECS021T2
00660                               TBL-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T2
00661      ADD LINK-AVG-ORG-TRM (BEN-IDX DTE-IDX) TO                    ECS021T2
00662                               TBL-AVG-ORG-TRM (BEN-IDX DTE-IDX).  ECS021T2
00663      ADD LINK-WGHT-AGE    (BEN-IDX DTE-IDX) TO                    ECS021T2
00664                               TBL-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T2
00665      ADD LINK-WGHT-ORG-TRM(BEN-IDX DTE-IDX) TO                    ECS021T2
00666                               TBL-WGHT-ORG-TRM(BEN-IDX DTE-IDX).  ECS021T2
00667      ADD LINK-EXP-PCT     (BEN-IDX DTE-IDX) TO                    ECS021T2
00668                               TBL-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T2
00669      ADD LINK-ADDED-TO-CNT(BEN-IDX DTE-IDX) TO                    ECS021T2
00670                               TBL-ADDED-TO-CNT(BEN-IDX DTE-IDX).  ECS021T2
00671                                                                   ECS021T2
00672      ADD +1 TO DTE-IDX.                                           ECS021T2
00673                                                                   ECS021T2
00674      IF DTE-IDX LESS +16                                          ECS021T2
00675          GO TO 3000-ACTIVE-BENEFIT-LOOP                           ECS021T2
00676      ELSE                                                         ECS021T2
00677          ADD +1 TO BEN-IDX                                        ECS021T2
00678          GO TO 3000-ZERO-ACCESS-LOOP.                             ECS021T2
00679                                                                   ECS021T2
00680  3000-EXIT.                                                       ECS021T2
00681       EXIT.                                                       ECS021T2
00682                                                                   ECS021T2
00683      EJECT                                                        ECS021T2
00684 *******************************************************           ECS021T2
00685 *  (3050) DUMPS TOTAL TABLE (2) INTO TOTAL TABLE (1)  *           ECS021T2
00686 *******************************************************           ECS021T2
00687  4000-ADD-TOTAL-RTN.                                              ECS021T2
00688      MOVE +1                     TO BEN-IDX                       ECS021T2
00689                                     DTE-IDX.                      ECS021T2
00690                                                                   ECS021T2
00691  4000-TOTAL-BENEFITS-LOOP.                                        ECS021T2
00692      ADD LINK-T-ISS-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T2
00693                               TOT-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T2
00694      ADD LINK-T-CNC-CNT   (BEN-IDX DTE-IDX) TO                    ECS021T2
00695                               TOT-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T2
00696      ADD LINK-T-SINGLE-ELEM (BEN-IDX DTE-IDX) TO                  ECS021T2
00697                               TOT-SINGLE-ELEM (BEN-IDX DTE-IDX).     CL**2
00698      ADD LINK-T-JOINT-RETRO (BEN-IDX DTE-IDX) TO                  ECS021T2
00699                               TOT-JOINT-RETRO (BEN-IDX DTE-IDX).     CL**2
00700      ADD LINK-T-LIFE-LEVEL (BEN-IDX DTE-IDX) TO                   ECS021T2
00701                               TOT-LIFE-LEVEL (BEN-IDX DTE-IDX).   ECS021T2
00702      ADD LINK-T-ISS-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T2
00703                               TOT-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T2
00704      ADD LINK-T-CNC-PREM  (BEN-IDX DTE-IDX) TO                    ECS021T2
00705                               TOT-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T2
00706      ADD LINK-T-NET-COMPEN (BEN-IDX DTE-IDX) TO                   ECS021T2
00707                               TOT-NET-COMPEN (BEN-IDX DTE-IDX).   ECS021T2
00708      ADD LINK-T-LF-CLM-CNT (BEN-IDX DTE-IDX) TO                   ECS021T2
00709                               TOT-LF-CLM-CNT (BEN-IDX DTE-IDX).   ECS021T2
00710      ADD LINK-T-LF-CLM-AMT (BEN-IDX DTE-IDX) TO                   ECS021T2
00711                               TOT-LF-CLM-AMT (BEN-IDX DTE-IDX).   ECS021T2
00712      ADD LINK-T-AH-CLM-CNT (BEN-IDX DTE-IDX) TO                   ECS021T2
00713                               TOT-AH-CLM-CNT (BEN-IDX DTE-IDX).   ECS021T2
00714      ADD LINK-T-AH-CLM-AMT (BEN-IDX DTE-IDX) TO                   ECS021T2
00715                               TOT-AH-CLM-AMT (BEN-IDX DTE-IDX).   ECS021T2
00716      ADD LINK-T-LOSS-RESV (BEN-IDX DTE-IDX) TO                    ECS021T2
00717                               TOT-LOSS-RESV (BEN-IDX DTE-IDX).    ECS021T2
00718      ADD LINK-T-EARND-PREM (BEN-IDX DTE-IDX) TO                   ECS021T2
00719                               TOT-EARND-PREM (BEN-IDX DTE-IDX).   ECS021T2
00720      ADD LINK-T-PRM-INFRC (BEN-IDX DTE-IDX) TO                    ECS021T2
00721                               TOT-PRM-INFRC (BEN-IDX DTE-IDX).    ECS021T2
00722      ADD LINK-T-INFRC-CNT (BEN-IDX DTE-IDX) TO                    ECS021T2
00723                               TOT-INFRC-CNT (BEN-IDX DTE-IDX).    ECS021T2
00724      ADD LINK-T-AVG-AGE    (BEN-IDX DTE-IDX) TO                   ECS021T2
00725                               TOT-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T2
00726      ADD LINK-T-AVG-ORG-TRM (BEN-IDX DTE-IDX) TO                  ECS021T2
00727                               TOT-AVG-ORG-TRM (BEN-IDX DTE-IDX).     CL**2
00728      ADD LINK-T-WGHT-AGE  (BEN-IDX DTE-IDX) TO                    ECS021T2
00729                               TOT-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T2
00730      ADD LINK-T-WGHT-ORG-TRM(BEN-IDX DTE-IDX) TO                  ECS021T2
00731                               TOT-WGHT-ORG-TRM(BEN-IDX DTE-IDX).     CL**2
00732      ADD LINK-T-EXP-PCT   (BEN-IDX DTE-IDX) TO                    ECS021T2
00733                               TOT-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T2
00734      ADD LINK-T-ADDED-TO-CNT(BEN-IDX DTE-IDX) TO                  ECS021T2
00735                               TOT-ADDED-TO-CNT(BEN-IDX DTE-IDX).     CL**2
00736                                                                   ECS021T2
00737      ADD +1 TO DTE-IDX.                                           ECS021T2
00738                                                                   ECS021T2
00739      IF DTE-IDX LESS +16                                          ECS021T2
00740          GO TO 4000-TOTAL-BENEFITS-LOOP.                          ECS021T2
00741                                                                   ECS021T2
00742      IF BEN-IDX LESS +3                                           ECS021T2
00743          ADD +1                  TO BEN-IDX                       ECS021T2
00744          MOVE +1                 TO DTE-IDX                       ECS021T2
00745          GO TO 4000-TOTAL-BENEFITS-LOOP.                          ECS021T2
00746                                                                   ECS021T2
00747  4000-EXIT.                                                       ECS021T2
00748       EXIT.                                                       ECS021T2
00749                                                                   ECS021T2
00750      EJECT                                                        ECS021T2
00751 **************************************************************    ECS021T2
00752 *  THE FOLLOWING INDEXES ARE USED IN ROUTINES 5000 AND 6000  *    ECS021T2
00753 *                                                            *    ECS021T2
00754 *  1. BRK-IDX - USED TO INDEX BENEFIT LEVEL OF SORT RECORD   *    ECS021T2
00755 *                 VARIED FROM 1 TO 23                        *       CL**6
00756 *  2. BEN-IDX - USED TO INDEX BENEFIT LEVEL OF ACCUMULATED   *    ECS021T2
00757 *                 BREAK TABLES VARIED FROM 1 TO 75           *       CL**6
00758 *  3. TOT-IDX - USED TO INDEX BENEFIT TYPE OF TOTALS         *    ECS021T2
00759 *                 TABLES VARIED FROM 1 TO 2                  *    ECS021T2
00760 *  4. DTE-IDX - USED TO INDEX DATE RANGES OF ALL TABLES      *    ECS021T2
00761 *                 AND RECORDS VARIED FROM 1 TO 15            *    ECS021T2
00762 **************************************************************    ECS021T2
00763                                                                   ECS021T2
00764  5000-APPLY-SORT-RECORD.                                          ECS021T2
00765      MOVE +1                     TO BRK-IDX.                      ECS021T2
00766                                                                   ECS021T2
00767  5000-BENEFIT-LOOP.                                               ECS021T2
00768      IF SW-BENEFIT-CODE   (BRK-IDX) = ZEROS                       ECS021T2
00769          GO TO 5000-EXIT.                                         ECS021T2
00770                                                                   ECS021T2
00771      MOVE SW-BEN-TBL-POS  (BRK-IDX)                               ECS021T2
00772                                  TO BEN-IDX.                      ECS021T2
00773      MOVE SW-BENEFIT-CODE (BRK-IDX)                               ECS021T2
00774                                  TO TBL-BENEFIT-CODE (BEN-IDX).   ECS021T2
00775      MOVE SW-BENEFIT-TYPE (BRK-IDX)                               ECS021T2
00776                                  TO TBL-BENEFIT-TYPE (BEN-IDX).   ECS021T2
00777      MOVE +1                     TO DTE-IDX.                      ECS021T2
00778                                                                   ECS021T2
00779  5000-DATE-RANGE-LOOP.                                            ECS021T2
00780      ADD SW-ISS-CNT     (BRK-IDX DTE-IDX)                         ECS021T2
00781                            TO TBL-ISS-CNT      (BEN-IDX DTE-IDX). ECS021T2
00782      ADD SW-CNC-CNT     (BRK-IDX DTE-IDX)                         ECS021T2
00783                            TO TBL-CNC-CNT      (BEN-IDX DTE-IDX). ECS021T2
00784      ADD SW-ISS-PREM    (BRK-IDX DTE-IDX)                         ECS021T2
00785                            TO TBL-ISS-PREM     (BEN-IDX DTE-IDX). ECS021T2
00786      ADD SW-CNC-PREM    (BRK-IDX DTE-IDX)                         ECS021T2
00787                            TO TBL-CNC-PREM     (BEN-IDX DTE-IDX). ECS021T2
00788      ADD SW-NET-COMPEN  (BRK-IDX DTE-IDX)                         ECS021T2
00789                            TO TBL-NET-COMPEN   (BEN-IDX DTE-IDX). ECS021T2
00790      ADD SW-CLM-CNT     (BRK-IDX DTE-IDX)                         ECS021T2
00791                            TO TBL-CLM-CNT      (BEN-IDX DTE-IDX). ECS021T2
00792      ADD SW-CLM-AMT     (BRK-IDX DTE-IDX)                         ECS021T2
00793                            TO TBL-CLM-AMT      (BEN-IDX DTE-IDX). ECS021T2
00794      ADD SW-LOSS-RESV   (BRK-IDX DTE-IDX)                         ECS021T2
00795                            TO TBL-LOSS-RESV    (BEN-IDX DTE-IDX). ECS021T2
00796      ADD SW-PRM-EARND   (BRK-IDX DTE-IDX)                         ECS021T2
00797                            TO TBL-EARND-PREM   (BEN-IDX DTE-IDX). ECS021T2
00798      ADD SW-PRM-INFRC   (BRK-IDX DTE-IDX)                         ECS021T2
00799                            TO TBL-PRM-INFRC    (BEN-IDX DTE-IDX). ECS021T2
00800      ADD SW-INFRC-CNT   (BRK-IDX DTE-IDX)                         ECS021T2
00801                            TO TBL-INFRC-CNT    (BEN-IDX DTE-IDX). ECS021T2
00802      ADD SW-AVG-AGE     (BRK-IDX DTE-IDX)                         ECS021T2
00803                            TO TBL-AVG-AGE      (BEN-IDX DTE-IDX). ECS021T2
00804      ADD SW-AVG-ORG-TRM (BRK-IDX DTE-IDX)                         ECS021T2
00805                            TO TBL-AVG-ORG-TRM (BEN-IDX DTE-IDX).  ECS021T2
00806      ADD SW-WGHT-AGE    (BRK-IDX DTE-IDX)                         ECS021T2
00807                            TO TBL-WGHT-AGE     (BEN-IDX DTE-IDX). ECS021T2
00808      ADD SW-WGHT-ORG-TRM(BRK-IDX DTE-IDX)                         ECS021T2
00809                            TO TBL-WGHT-ORG-TRM(BEN-IDX DTE-IDX).  ECS021T2
00810      ADD SW-EXP-PCT     (BRK-IDX DTE-IDX)                         ECS021T2
00811                            TO TBL-EXP-PCT      (BEN-IDX DTE-IDX). ECS021T2
00812      ADD SW-ADDED-TO-CNT(BRK-IDX DTE-IDX)                         ECS021T2
00813                            TO TBL-ADDED-TO-CNT(BEN-IDX DTE-IDX).  ECS021T2
00814                                                                   ECS021T2
00815      ADD +1                TO DTE-IDX.                            ECS021T2
00816                                                                   ECS021T2
00817      IF DTE-IDX LESS +16                                          ECS021T2
00818          GO TO 5000-DATE-RANGE-LOOP.                              ECS021T2
00819                                                                   ECS021T2
00820      PERFORM 6000-BENEFIT-TYPE-TOTAL THRU 6000-EXIT.              ECS021T2
00821                                                                   ECS021T2
00822      ADD +1                TO BRK-IDX.                            ECS021T2
00823                                                                   ECS021T2
00824      IF BRK-IDX LESS +24                                             CL**6
00825          GO TO 5000-BENEFIT-LOOP.                                 ECS021T2
00826                                                                   ECS021T2
00827  5000-EXIT.                                                       ECS021T2
00828       EXIT.                                                       ECS021T2
00829                                                                   ECS021T2
00830      EJECT                                                        ECS021T2
00831 **************************************************************    ECS021T2
00832 *  THE FOLLOWING INDEXES ARE USED IN ROUTINES 5000 AND 6000  *    ECS021T2
00833 *                                                            *    ECS021T2
00834 *  1. BRK-IDX - USED TO INDEX BENEFIT LEVEL OF SORT RECORD   *    ECS021T2
00835 *                 VARIED FROM 1 TO 23                        *       CL**6
00836 *  2. BEN-IDX - USED TO INDEX BENEFIT LEVEL OF ACCUMULATED   *    ECS021T2
00837 *                 BREAK TABLES VARIED FROM 1 TO 75           *       CL**6
00838 *  3. TOT-IDX - USED TO INDEX BENEFIT TYPE OF TOTALS         *    ECS021T2
00839 *                 TABLES VARIED FROM 1 TO 2                  *    ECS021T2
00840 *  4. DTE-IDX - USED TO INDEX DATE RANGES OF ALL TABLES      *    ECS021T2
00841 *                 AND RECORDS VARIED FROM 1 TO 15            *    ECS021T2
00842 **************************************************************    ECS021T2
00843                                                                   ECS021T2
00844  6000-BENEFIT-TYPE-TOTAL.                                         ECS021T2
00845                                                                   ECS021T2
00846      MOVE +1                     TO DTE-IDX.                      ECS021T2
00847                                                                   ECS021T2
00848  6000-DATE-RANGE-LOOP.                                            ECS021T2
00849      COMPUTE NET-COUNT = SW-ISS-CNT (BRK-IDX DTE-IDX)                CL**5
00850           - SW-CNC-CNT (BRK-IDX DTE-IDX).                            CL**5
00851                                                                   ECS021T2
00852      IF SW-BENEFIT-TYPE (BRK-IDX) = LIFE-OVERRIDE-L1              ECS021T2
00853          MOVE +1                 TO TOT-IDX                       ECS021T2
00854          IF CLAS-I-RL-AH (BEN-IDX) = 'R'                          ECS021T2
00855              IF CLAS-I-JOINT (BEN-IDX) = 'J'                      ECS021T2
00856                  ADD NET-COUNT TO                                    CL**5
00857                      TOT-JOINT-RETRO (TOT-IDX DTE-IDX)            ECS021T2
00858              ELSE                                                 ECS021T2
00859                  ADD NET-COUNT TO                                    CL**5
00860                      TOT-SINGLE-ELEM (TOT-IDX DTE-IDX)            ECS021T2
00861          ELSE                                                     ECS021T2
00862              ADD NET-COUNT TO                                        CL**5
00863                  TOT-LIFE-LEVEL (TOT-IDX DTE-IDX)                 ECS021T2
00864      ELSE                                                         ECS021T2
00865          MOVE +2                 TO TOT-IDX                       ECS021T2
00866          IF CLAS-I-AB1 (BEN-IDX) = 'E'                            ECS021T2
00867              ADD NET-COUNT TO                                        CL**5
00868                  TOT-SINGLE-ELEM (TOT-IDX DTE-IDX)                ECS021T2
00869          ELSE                                                     ECS021T2
00870              IF CLAS-I-AB1 (BEN-IDX) = 'R'                        ECS021T2
00871                  ADD NET-COUNT TO                                    CL**5
00872                      TOT-JOINT-RETRO (TOT-IDX DTE-IDX).           ECS021T2
00873                                                                   ECS021T2
00874      ADD SW-ISS-CNT    (BRK-IDX DTE-IDX)                          ECS021T2
00875                            TO TOT-ISS-CNT     (TOT-IDX DTE-IDX)   ECS021T2
00876                               TOT-ISS-CNT     (3 DTE-IDX).        ECS021T2
00877      ADD SW-CNC-CNT    (BRK-IDX DTE-IDX)                          ECS021T2
00878                            TO TOT-CNC-CNT     (TOT-IDX DTE-IDX)   ECS021T2
00879                               TOT-CNC-CNT     (3 DTE-IDX).        ECS021T2
00880      ADD SW-ISS-PREM   (BRK-IDX DTE-IDX)                          ECS021T2
00881                            TO TOT-ISS-PREM    (TOT-IDX DTE-IDX)   ECS021T2
00882                               TOT-ISS-PREM    (3 DTE-IDX).        ECS021T2
00883      ADD SW-CNC-PREM   (BRK-IDX DTE-IDX)                          ECS021T2
00884                            TO TOT-CNC-PREM    (TOT-IDX DTE-IDX)   ECS021T2
00885                               TOT-CNC-PREM    (3 DTE-IDX).        ECS021T2
00886      ADD SW-NET-COMPEN (BRK-IDX DTE-IDX)                          ECS021T2
00887                            TO TOT-NET-COMPEN (TOT-IDX DTE-IDX)    ECS021T2
00888                               TOT-NET-COMPEN (3 DTE-IDX).         ECS021T2
00889                                                                   ECS021T2
00890      IF SW-BENEFIT-TYPE (BRK-IDX) = LIFE-OVERRIDE-L1              ECS021T2
00891          ADD SW-CLM-CNT (BRK-IDX DTE-IDX)                         ECS021T2
00892                            TO TOT-LF-CLM-CNT   (TOT-IDX DTE-IDX)  ECS021T2
00893                               TOT-LF-CLM-CNT   (3 DTE-IDX)        ECS021T2
00894          ADD SW-CLM-AMT (BRK-IDX DTE-IDX)                         ECS021T2
00895                            TO TOT-LF-CLM-AMT   (TOT-IDX DTE-IDX)  ECS021T2
00896                               TOT-LF-CLM-AMT   (3 DTE-IDX).       ECS021T2
00897                                                                   ECS021T2
00898      IF SW-BENEFIT-TYPE (BRK-IDX) = AH-OVERRIDE-L1                ECS021T2
00899          ADD SW-CLM-CNT (BRK-IDX DTE-IDX)                         ECS021T2
00900                            TO TOT-AH-CLM-CNT   (TOT-IDX DTE-IDX)  ECS021T2
00901                               TOT-AH-CLM-CNT   (3 DTE-IDX)        ECS021T2
00902          ADD SW-CLM-AMT (BRK-IDX DTE-IDX)                         ECS021T2
00903                            TO TOT-AH-CLM-AMT   (TOT-IDX DTE-IDX)  ECS021T2
00904                               TOT-AH-CLM-AMT   (3 DTE-IDX).       ECS021T2
00905                                                                   ECS021T2
00906      ADD SW-LOSS-RESV   (BRK-IDX DTE-IDX)                         ECS021T2
00907                            TO TOT-LOSS-RESV    (TOT-IDX DTE-IDX)  ECS021T2
00908                               TOT-LOSS-RESV    (3 DTE-IDX).       ECS021T2
00909      ADD SW-PRM-EARND   (BRK-IDX DTE-IDX)                         ECS021T2
00910                            TO TOT-EARND-PREM   (TOT-IDX DTE-IDX)  ECS021T2
00911                               TOT-EARND-PREM   (3 DTE-IDX).       ECS021T2
00912      ADD SW-PRM-INFRC   (BRK-IDX DTE-IDX)                         ECS021T2
00913                            TO TOT-PRM-INFRC    (TOT-IDX DTE-IDX)  ECS021T2
00914                               TOT-PRM-INFRC    (3 DTE-IDX).       ECS021T2
00915      ADD SW-INFRC-CNT   (BRK-IDX DTE-IDX)                         ECS021T2
00916                            TO TOT-INFRC-CNT    (TOT-IDX DTE-IDX)  ECS021T2
00917                               TOT-INFRC-CNT    (3 DTE-IDX).       ECS021T2
00918      ADD SW-AVG-AGE     (BRK-IDX DTE-IDX)                         ECS021T2
00919                            TO TOT-AVG-AGE      (TOT-IDX DTE-IDX)  ECS021T2
00920                               TOT-AVG-AGE      (3 DTE-IDX).       ECS021T2
00921      ADD SW-AVG-ORG-TRM (BRK-IDX DTE-IDX)                         ECS021T2
00922                            TO TOT-AVG-ORG-TRM (TOT-IDX DTE-IDX)   ECS021T2
00923                               TOT-AVG-ORG-TRM (3 DTE-IDX).        ECS021T2
00924      ADD SW-WGHT-AGE    (BRK-IDX DTE-IDX)                         ECS021T2
00925                            TO TOT-WGHT-AGE     (TOT-IDX DTE-IDX)  ECS021T2
00926                               TOT-WGHT-AGE     (3 DTE-IDX).       ECS021T2
00927      ADD SW-WGHT-ORG-TRM(BRK-IDX DTE-IDX)                         ECS021T2
00928                            TO TOT-WGHT-ORG-TRM(TOT-IDX DTE-IDX)   ECS021T2
00929                               TOT-WGHT-ORG-TRM(3 DTE-IDX).        ECS021T2
00930      ADD SW-EXP-PCT     (BRK-IDX DTE-IDX)                         ECS021T2
00931                            TO TOT-EXP-PCT      (TOT-IDX DTE-IDX)  ECS021T2
00932                               TOT-EXP-PCT      (3 DTE-IDX).       ECS021T2
00933      ADD SW-ADDED-TO-CNT (BRK-IDX DTE-IDX)                        ECS021T2
00934                            TO TOT-ADDED-TO-CNT(TOT-IDX DTE-IDX)   ECS021T2
00935                               TOT-ADDED-TO-CNT(3 DTE-IDX).        ECS021T2
00936                                                                   ECS021T2
00937      ADD +1 TO DTE-IDX.                                           ECS021T2
00938                                                                   ECS021T2
00939      IF DTE-IDX LESS +16                                          ECS021T2
00940          GO TO 6000-DATE-RANGE-LOOP.                              ECS021T2
00941                                                                   ECS021T2
00942  6000-EXIT.                                                       ECS021T2
00943       EXIT.                                                       ECS021T2
00944                                                                   ECS021T2
00945      EJECT                                                        ECS021T2
00946  7000-MOVE-TABLES.                                                ECS021T2
00947                                                                   ECS021T2
00948      MOVE BREAK-TABLE            TO LINK-TABLE.                   ECS021T2
092602*    MOVE BREAK-EXTENSION-1      TO LINK-EXTENSION-1.             ECS021T2
092602*    MOVE BREAK-EXTENSION-2      TO LINK-EXTENSION-2.             ECS021T2
092602*    MOVE BREAK-EXTENSION-3      TO LINK-EXTENSION-3.                CL**6
00952      MOVE BREAK-TOTAL-TABLE      TO LINK-TOTAL-TABLE.             ECS021T2
00953                                                                   ECS021T2
00954  7000-EXIT.                                                       ECS021T2
00955       EXIT.                                                       ECS021T2
00956                                                                   ECS021T2
00957      EJECT                                                        ECS021T2
00958 ***********************************                               ECS021T2
00959 *     (8000) ZERO TABLE TWO       *                               ECS021T2
00960 ***********************************                               ECS021T2
00961                                                                   ECS021T2
00962  8000-ZERO-ENTIRE-TABLE.                                          ECS021T2
00963                                                                   ECS021T2
00964       MOVE +1                    TO BEN-IDX.                      ECS021T2
00965                                                                   ECS021T2
00966  8000-ZERO-BENEFIT-LOOP.                                          ECS021T2
00967                                                                   ECS021T2
00968       MOVE ZERO-TABLE            TO                               ECS021T2
00969                                  TABLE-ACCUMULATORS (BEN-IDX).    ECS021T2
00970                                                                   ECS021T2
092602*     IF BEN-IDX LESS THAN THREE-HUNDRED                          ECS021T2
092602      IF BEN-IDX LESS THAN  NINE-HUNDRED                          ECS021T2
00972           ADD +1                 TO BEN-IDX                       ECS021T2
00973               GO TO 8000-ZERO-BENEFIT-LOOP.                       ECS021T2
00974                                                                   ECS021T2
00975  8000-ZERO-TOTAL-TABLE.                                           ECS021T2
00976                                                                   ECS021T2
00977       MOVE +1                    TO TOT-IDX.                      ECS021T2
00978                                                                   ECS021T2
00979  8000-ZERO-TOTAL-LOOP.                                            ECS021T2
00980                                                                   ECS021T2
00981       MOVE ZERO-TOTAL-TABLE      TO TABLE-TOTALS (TOT-IDX).       ECS021T2
00982                                                                   ECS021T2
00983       IF TOT-IDX LESS THAN THREE                                  ECS021T2
00984           ADD +1                 TO TOT-IDX                       ECS021T2
00985               GO TO 8000-ZERO-TOTAL-LOOP.                         ECS021T2
00986                                                                   ECS021T2
00987  8000-EXIT.                                                       ECS021T2
00988      EXIT.                                                        ECS021T2
00989                                                                   ECS021T2
00990      EJECT                                                        ECS021T2
00991 *******************************************                       ECS021T2
00992 *     (9000) INITIALIZE ZERO-ACCUMS       *                       ECS021T2
00993 *******************************************                       ECS021T2
00994                                                                   ECS021T2
00995  9000-INITIALIZE-ZERO-TABLE.                                      ECS021T2
00996                                                                   ECS021T2
00997       MOVE +1                    TO DTE-IDX.                      ECS021T2
00998                                                                   ECS021T2
00999  9000-ZERO-DATE-LOOP.                                             ECS021T2
01000                                                                   ECS021T2
01001       MOVE ZERO-TABLE-ENTRIES    TO ZERO-PERIOD (DTE-IDX).        ECS021T2
01002                                                                   ECS021T2
01003       IF DTE-IDX LESS THAN SIXTEEN                                ECS021T2
01004           ADD +1                 TO DTE-IDX                       ECS021T2
01005               GO TO 9000-ZERO-DATE-LOOP.                          ECS021T2
01006                                                                   ECS021T2
01007       MOVE +1                    TO DTE-IDX.                      ECS021T2
01008                                                                   ECS021T2
01009  9000-ZERO-TOTAL-DATE-LOOP.                                       ECS021T2
01010                                                                   ECS021T2
01011       MOVE ZERO-TOTAL-ENTRY      TO ZERO-TOTAL-ACCUMS (DTE-IDX).  ECS021T2
01012                                                                   ECS021T2
01013       IF DTE-IDX LESS THAN SIXTEEN                                ECS021T2
01014           ADD +1                 TO DTE-IDX                       ECS021T2
01015               GO TO 9000-ZERO-TOTAL-DATE-LOOP.                    ECS021T2
01016                                                                   ECS021T2
01017  9000-EXIT.                                                       ECS021T2
01018      EXIT.                                                        ECS021T2
01019                                                                   ECS021T2
01020      EJECT                                                        ECS021T2
01021  ABEND-PGM SECTION.                                               ECS021T2
01022            COPY ELCABEND SUPPRESS.                                ECS021T2
