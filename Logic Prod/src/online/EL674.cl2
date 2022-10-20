00001  IDENTIFICATION DIVISION.                                         11/10/98
00002                                                                   EL674
00003  PROGRAM-ID.                 EL674 .                                 LV003
00004 *              PROGRAM CONVERTED BY                                  CL**1
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**1
00006 *              CONVERSION DATE 11/07/94 10:47:56.                    CL**1
00007 *                            VMOD=2.004                              CL**1
00008 *                                                                    CL**1
00009 *AUTHOR.     LOGIC INC.                                              CL**1
00010 *            DALLAS, TEXAS.                                          CL**1
00011                                                                      CL**1
00012 *DATE-COMPILED.                                                      CL**1
00013 *SECURITY.   *****************************************************   CL**1
00014 *            *                                                   *   CL**1
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**1
00016 *            *                                                   *   CL**1
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**1
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**1
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**1
00020 *            *                                                   *   CL**1
00021 *            *****************************************************   CL**1
00022                                                                      CL**1
00023 *REMARKS.   TRANSACTION - EXE8- REINSURANCE TABLE PRINT              CL**1
00024 *           STARTED FROM EL671.                                      CL**1
00025                                                                      CL**1
00026      EJECT                                                           CL**1
00027  ENVIRONMENT DIVISION.                                               CL**1
00028  DATA DIVISION.                                                      CL**1
00029  WORKING-STORAGE SECTION.                                            CL**1
00030  01  LCP-TIME-OF-DAY-XX.                                             CL**1
00031      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL**1
00032      05  FILLER                    PIC 99.                           CL**1
00033  01  LCP-CICS-TIME                 PIC 9(15).                        CL**1
00034  77  FILLER  PIC X(32)  VALUE '********************************'.    CL**1
00035  77  FILLER  PIC X(32)  VALUE '*    EL674 WORKING STORAGE     *'.    CL**1
00036  77  FILLER  PIC X(32)  VALUE '************ V/M 2.004 *********'.    CL**1
00037                                                                      CL**1
00038  77  CLEN    PIC S9(4)  COMP    VALUE +1024.                         CL**1
00039  77  LEVL    PIC 99             VALUE 0.                             CL**1
00040                                                                      CL**1
00041  01  WS-DATE-AREA.                                                   CL**1
00042      03  SAVE-DATE               PIC X(8)    VALUE SPACES.           CL**1
00043      03  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.           CL**1
00044      03  SAVE-DATE-ALPHA         PIC X(18)   VALUE SPACES.           CL**1
00045                                                                      CL**1
00046  01  WORK-AREAS.                                                     CL**1
00047      03  WS-REPORT-ID            PIC X(6)    VALUE SPACES.           CL**1
00048      03  PGM-NAME                PIC X(8)    VALUE SPACES.           CL**1
00049      03  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.        CL**1
00050      03  EMI-LINE1               PIC X(72).                          CL**1
00051      03  WS-NEXT-TRAN            PIC X(4).                           CL**1
00052      03  WS-TERMINAL-ID.                                             CL**1
00053          05  WS-TERM-PREFIX      PIC XX.                             CL**1
00054          05  FILLER              PIC XX.                             CL**1
00055      03  PRT-CNT                 PIC S9(3)   VALUE +0   COMP-3.      CL**1
00056      03  WS-LINE-NUMBER          PIC S9(7)   VALUE ZERO COMP-3.      CL**1
00057      03  WS-PAGE                 PIC S9(5)   VALUE ZERO COMP-3.      CL**1
00058      03  WS-REPORT-SW            PIC S9      VALUE ZERO COMP-3.      CL**1
00059      03  WS-PRINT-SW             PIC S9      VALUE ZERO COMP-3.      CL**1
00060      03  REPT-FILE-ID            PIC X(8)    VALUE 'ELREPT'.         CL**1
00061      03  REIN-FILE-ID            PIC X(8)    VALUE 'ERREIN'.         CL**1
00062      03  CNTL-ID                 PIC X(8)    VALUE 'ELCNTL'.         CL**1
00063      03  SA                      PIC S999    COMP.                   CL**1
00064      03  B-REC-FLAG              PIC X       VALUE SPACES.           CL**1
00065      03  WS-DATE                 PIC 9(11).                          CL**3
00066      03  WS-DATE-RDEF  REDEFINES WS-DATE.                            CL**2
00067          05  FILLER              PIC 999.                            CL**1
00068          05  WS-CC               PIC 99.                             CL**1
00069          05  WS-YR               PIC 99.                             CL**1
00070          05  WS-MO               PIC 99.                             CL**1
00071          05  WS-DA               PIC 99.                             CL**1
00072      03  ABEND-AREA              PIC X(72).                          CL**1
00073                                                                      CL**1
00074  01  ACCESS-KEYS.                                                    CL**1
00075      03  ELCNTL-KEY.                                                 CL**1
00076          05  CNTL-COMP-ID         PIC X(3).                          CL**1
00077          05  CNTL-REC-TYPE        PIC X.                             CL**1
00078          05  CNTL-ACCESS          PIC X(4).                          CL**1
00079          05  CNTL-SEQ-NO          PIC S9(4)    COMP.                 CL**1
00080                                                                      CL**1
00081                        COPY ELCREPT.                                 CL**1
00082                                                                      CL**1
00083      EJECT                                                           CL**1
00084                            COPY ELCDATE.                             CL**1
00085                                                                      CL**1
00086      EJECT                                                           CL**1
00087                                                                      CL**1
00088  01  PRT-LINES.                                                      CL**1
00089      03  HDR-A1.                                                     CL**1
00090          05  FILLER          PIC X(21)   VALUE SPACES.               CL**1
00091          05  FILLER          PIC X(33)   VALUE                       CL**1
00092                  'REINSURANCE TABLES - FILE LISTING'.                CL**1
00093          05  FILLER          PIC X(13)   VALUE SPACES.               CL**1
00094          05  FILLER          PIC X(8)    VALUE 'EL - 674'.           CL**1
00095                                                                      CL**1
00096      03  HDR-A2.                                                     CL**1
00097          05  FILLER          PIC X(29)   VALUE SPACES.               CL**1
00098          05  H2-COMP         PIC X(30)   VALUE 'LOGIC, INC.'.        CL**1
00099          05  FILLER          PIC X(8)    VALUE SPACES.               CL**1
00100          05  H2-DATE         PIC X(8).                               CL**1
00101                                                                      CL**1
00102      03  HDR-A3.                                                     CL**1
00103          05  FILLER          PIC X(27)   VALUE SPACES.               CL**1
00104          05  H3-DATE.                                                CL**1
00105            07  H3-DATE1      PIC X(8)    VALUE SPACES.               CL**1
00106            07  H3-THRU       PIC X(6)    VALUE SPACES.               CL**1
00107            07  H3-DATE2      PIC X(8)    VALUE SPACES.               CL**1
00108            07  FILLER        PIC X(15)   VALUE SPACES.               CL**1
00109          05  FILLER          PIC X(5)    VALUE 'PAGE '.              CL**1
00110          05  H3-PAGE         PIC ZZ,ZZZ-.                            CL**1
00111                                                                      CL**1
00112      03  HDR-A4.                                                     CL**1
00113          05  FILLER          PIC X(27)   VALUE                       CL**1
00114                  '************** TABL CODE = '.                      CL**1
00115          05  H4-CODE         PIC X(3).                               CL**1
00116          05  FILLER          PIC X(16)   VALUE                       CL**1
00117                  '    REIN COMP = '.                                 CL**1
00118          05  H4-COMP         PIC X(6).                               CL**1
00119          05  FILLER          PIC X(12)   VALUE                       CL**1
00120                  '    LEVEL = '.                                     CL**1
00121          05  H4-LEVL         PIC XX.                                 CL**1
00122                                                                      CL**1
00123      03  HDR-A5.                                                     CL**1
00124          05  FILLER          PIC X(16)   VALUE SPACES.               CL**1
00125          05  FILLER          PIC X(42)   VALUE                       CL**1
00126                  '**------------------REINSURANCE LIMITS---'.        CL**1
00127          05  FILLER          PIC X(19)   VALUE                       CL**1
00128                  '-----------------**'.                              CL**1
00129                                                                      CL**1
00130      03  HDR-A6.                                                     CL**1
00131          05  FILLER          PIC X(11)   VALUE SPACES.               CL**1
00132          05  FILLER          PIC X(42)   VALUE                       CL**1
00133                  'EFFECTIVE EXPIRY  LOW HIGH  LOW HIGH'.             CL**1
00134          05  FILLER          PIC X(19)   VALUE                       CL**1
00135                  '  LOW          HIGH'.                              CL**1
00136                                                                      CL**1
00137      03  HDR-A7.                                                     CL**1
00138          05  FILLER          PIC X(14)   VALUE SPACES.               CL**1
00139          05  FILLER          PIC X(51)   VALUE                       CL**1
00140             'DATE    DATE   AGE  AGE TERM TERM     BENEFIT'.         CL**1
00141          05  FILLER          PIC X(7)    VALUE 'BENEFIT'.            CL**1
00142                                                                      CL**1
00143      03  HDR-A8.                                                     CL**1
00144          05  FILLER          PIC X(33)   VALUE SPACES.               CL**1
00145          05  FILLER          PIC X(42)   VALUE                       CL**1
00146                  '**---------REINSURANCE AMOUNTS---------**'.        CL**1
00147                                                                      CL**1
00148      03  HDR-A9.                                                     CL**1
00149          05  FILLER         PIC X(5)    VALUE SPACES.                CL**1
00150          05  FILLER         PIC X(16)   VALUE 'Q B I R       ST'.    CL**1
00151          05  FILLER         PIC X(33)   VALUE SPACES.                CL**1
00152          05  FILLER         PIC X(16)   VALUE 'LOW         HIGH'.    CL**1
00153                                                                      CL**1
00154      03  HDR-A10.                                                    CL**1
00155          05  FILLER         PIC X(5)    VALUE SPACES.                CL**1
00156          05  FILLER         PIC X(16)   VALUE 'C C N M       CD'.    CL**1
00157          05  FILLER         PIC X(18)   VALUE SPACES.                CL**1
00158          05  FILLER         PIC X(33)   VALUE                        CL**1
00159                  'PERCENT      BENEFIT      BENEFIT'.                CL**1
00160                                                                      CL**1
00161      03  HDR-B1.                                                     CL**1
00162          05  FILLER         PIC X(45)   VALUE                        CL**1
00163                  '* - REINSURANCE COMPANY - *  CESSION METHOD '.     CL**1
00164          05  HL6-1          PIC X(8).                                CL**1
00165          05  HA6-1          PIC X(6).                                CL**1
00166          05  HA6-2          PIC X(6).                                CL**1
00167          05  HA6-3          PIC X(7).                                CL**1
00168          05  FILLER         PIC X(5)    VALUE 'PRINT'.               CL**1
00169                                                                      CL**1
00170      03  HDR-B2.                                                     CL**1
00171          05  FILLER          PIC X(31)   VALUE                       CL**1
00172                  ' CODE  NAME'.                                      CL**1
00173          05  HL6-2           PIC X(7).                               CL**1
00174          05  HA6-4           PIC X(6).                               CL**1
00175          05  FILLER          PIC X(35)   VALUE                       CL**1
00176                  '  FEE     FEE   PR    R78  TAX O/W'.               CL**1
00177                                                                      CL**1
00178      03  HDR-B3.                                                     CL**1
00179          05  FILLER          PIC X(32)   VALUE SPACES.               CL**1
00180          05  FILLER          PIC X(26)   VALUE                       CL**1
00181                  'CLAIM  FEE   COMM  TAX'.                           CL**1
00182                                                                      CL**1
00183      03  HDR-B4.                                                     CL**1
00184          05  FILLER          PIC X(26)   VALUE SPACES.               CL**1
00185          05  FILLER          PIC X(12)   VALUE 'MORT  P/I'.          CL**1
00186          05  HL1-1           PIC X.                                  CL**1
00187          05  FILLER          PIC X       VALUE '-'.                  CL**1
00188          05  HA1-1           PIC X.                                  CL**1
00189          05  FILLER          PIC XXX     VALUE SPACES.               CL**1
00190          05  HL1-2           PIC X.                                  CL**1
00191          05  FILLER          PIC X       VALUE '-'.                  CL**1
00192          05  HA1-2           PIC X.                                  CL**1
00193          05  FILLER          PIC XXX     VALUE SPACES.               CL**1
00194          05  HL1-3           PIC X.                                  CL**1
00195          05  FILLER          PIC X       VALUE '-'.                  CL**1
00196          05  HA1-3           PIC X.                                  CL**1
00197          05  FILLER          PIC XX      VALUE SPACES.               CL**1
00198          05  FILLER          PIC X(16)   VALUE 'CEDING COMP NAME'.   CL**1
00199                                                                      CL**1
00200      03  DTL-A1.                                                     CL**1
00201          05  D1-BEN-DESC.                                            CL**1
00202            07  D1-BEN-LFAH   PIC X(3).                               CL**1
00203            07  D1-BEN-REST   PIC X(8).                               CL**1
00204          05  D1-EFF-MO       PIC XX.                                 CL**1
00205          05  D1-SLSH-1       PIC X.                                  CL**1
00206          05  D1-EFF-DA       PIC XX.                                 CL**1
00207          05  D1-SLSH-2       PIC X.                                  CL**1
00208          05  D1-EFF-YR       PIC XX.                                 CL**1
00209          05  FILLER          PIC X.                                  CL**1
00210          05  D1-EXP-MO       PIC XX.                                 CL**1
00211          05  D1-SLSH-3       PIC X.                                  CL**1
00212          05  D1-EXP-DA       PIC XX.                                 CL**1
00213          05  D1-SLSH-4       PIC X.                                  CL**1
00214          05  D1-EXP-YR       PIC XX.                                 CL**1
00215          05  FILLER          PIC XX.                                 CL**1
00216          05  D1-LO-AGE       PIC 99.                                 CL**1
00217          05  FILLER          PIC XX.                                 CL**1
00218          05  D1-HI-AGE       PIC 99.                                 CL**1
00219          05  FILLER          PIC XX.                                 CL**1
00220          05  D1-LO-TRM       PIC 999.                                CL**1
00221          05  FILLER          PIC XX.                                 CL**1
00222          05  D1-HI-TRM       PIC 999.                                CL**1
00223          05  FILLER          PIC X.                                  CL**1
00224          05  D1-LO-LIM       PIC Z,ZZZ,ZZ9.99-.                      CL**1
00225          05  FILLER          PIC XX.                                 CL**1
00226          05  D1-HI-LIM       PIC Z,ZZZ,ZZ9.99-.                      CL**1
00227                                                                      CL**1
00228      03  DTL-A2.                                                     CL**1
00229          05  FILLER          PIC X(5).                               CL**1
00230          05  D1-QC           PIC X.                                  CL**1
00231          05  FILLER          PIC X.                                  CL**1
00232          05  D1-BEN-CODE     PIC X.                                  CL**1
00233          05  FILLER          PIC X.                                  CL**1
00234          05  D1-IN           PIC X.                                  CL**1
00235          05  FILLER          PIC X.                                  CL**1
00236          05  D1-RM           PIC X.                                  CL**1
00237          05  FILLER          PIC X(7).                               CL**1
00238          05  D1-ST           PIC XX.                                 CL**1
00239          05  FILLER          PIC X(19).                              CL**1
00240          05  D1-PCT          PIC ZZ9.99-.                            CL**1
00241          05  D1-LO           PIC Z,ZZZ,ZZ9.99-.                      CL**1
00242          05  FILLER          PIC XX.                                 CL**1
00243          05  D1-HI           PIC Z,ZZZ,ZZ9.99-.                      CL**1
00244                                                                      CL**1
00245      03  DTL-B1.                                                     CL**1
00246          05  FILLER          PIC X.                                  CL**1
00247          05  D2-COMP         PIC X(6).                               CL**1
00248          05  FILLER          PIC X.                                  CL**1
00249          05  D2-NAME         PIC X(20).                              CL**1
00250          05  FILLER          PIC X(5).                               CL**1
00251          05  D2-LF-PE        PIC X.                                  CL**1
00252          05  FILLER          PIC X(5).                               CL**1
00253          05  D2-AH-PE        PIC X.                                  CL**1
00254          05  FILLER          PIC X(3).                               CL**1
00255          05  D2-LF-FEE       PIC ZZ9.99-.                            CL**1
00256          05  D2-LF-MSG REDEFINES D2-LF-FEE                           CL**1
00257                              PIC X(7).                               CL**1
00258          05  FILLER          PIC X.                                  CL**1
00259          05  D2-AH-FEE       PIC ZZ9.99-.                            CL**1
00260          05  D2-AH-MSG REDEFINES D2-AH-FEE                           CL**1
00261                              PIC X(7).                               CL**1
00262          05  D2-PR-PCT       PIC ZZ9.99-.                            CL**1
00263          05  D2-78-PCT       PIC ZZ9.99-.                            CL**1
00264          05  FILLER          PIC XX.                                 CL**1
00265          05  D2-ST           PIC X.                                  CL**1
00266          05  FILLER          PIC X(3).                               CL**1
00267          05  D2-OW           PIC X.                                  CL**1
00268                                                                      CL**1
00269      03  DTL-B2.                                                     CL**1
00270          05  FILLER          PIC X(26).                              CL**1
00271          05  D2-MORT         PIC X(4).                               CL**1
00272          05  FILLER          PIC X(3).                               CL**1
00273          05  D2-CLAIM        PIC X.                                  CL**1
00274          05  FILLER          PIC X(4).                               CL**1
00275          05  D2-Z-L-FE       PIC X.                                  CL**1
00276          05  FILLER          PIC X.                                  CL**1
00277          05  D2-Z-A-FE       PIC X.                                  CL**1
00278          05  FILLER          PIC XXX.                                CL**1
00279          05  D2-L-COMM       PIC X.                                  CL**1
00280          05  FILLER          PIC X.                                  CL**1
00281          05  D2-A-COMM       PIC X.                                  CL**1
00282          05  FILLER          PIC XX.                                 CL**1
00283          05  D2-L-TAX        PIC X.                                  CL**1
00284          05  FILLER          PIC X.                                  CL**1
00285          05  D2-A-TAX        PIC X.                                  CL**1
00286          05  FILLER          PIC XX.                                 CL**1
00287          05  D2-CEDE-NAME    PIC X(20).                              CL**1
00288                                                                      CL**1
00289      EJECT                                                           CL**1
00290              COPY ELCAID.                                            CL**1
00291                                                                      CL**1
00292  01  PF-AID REDEFINES DFHAID.                                        CL**1
00293      05  FILLER                      PIC X(8).                       CL**1
00294      05  PF-VALUES  OCCURS 24        PIC X.                          CL**1
00295      EJECT                                                           CL**1
00296                               COPY ELCINTF.                          CL**1
00297                                                                      CL**1
00298      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.                CL**1
00299          16  PI-LO-DATE          PIC XX.                             CL**1
00300          16  PI-HI-DATE          PIC XX.                             CL**1
00301                                                                      CL**1
00302          16  PI-ERREIN-KEY.                                          CL**1
00303              20  PI-CRR-COMPANY-CD  PIC X.                           CL**1
00304              20  PI-CRR-CODE        PIC X.                           CL**1
00305              20  PI-CRR-TABLE       PIC X(3).                        CL**1
00306              20  PI-CRR-TABLE-SUB   PIC X(3).                        CL**1
00307          16  FILLER                 PIC X(628).                      CL**1
00308                                                                      CL**1
00309      EJECT                                                           CL**1
00310  LINKAGE SECTION.                                                    CL**1
00311  01  DFHCOMMAREA                     PIC X(1024).                    CL**1
00312 *01 PARM-LIST .                                                      CL**1
00313 *    02  FILLER              PIC S9(8)   COMP.                       CL**1
00314 *    02  ERREIN-POINTER      PIC S9(8)   COMP.                       CL**1
00315 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL**1
00316      EJECT                                                           CL**1
00317                          COPY ERCREIN.                               CL**1
00318                                                                      CL**1
00319                                                                      CL**1
00320                    COPY ELCCNTL.                                     CL**1
00321                                                                      CL**1
00322      EJECT                                                           CL**1
00323                                                                      CL**1
00324  PROCEDURE DIVISION.                                                 CL**1
00325      CONTINUE.                                                       CL**1
00326                                                                      CL**1
00327      MOVE EIBDATE               TO DC-JULIAN-YYDDD.                  CL**1
00328      MOVE '5'                   TO DC-OPTION-CODE.                   CL**1
00329      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                       CL**1
00330      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                       CL**1
00331      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                   CL**1
00332      MOVE DC-GREG-DATE-1-ALPHA  TO  SAVE-DATE-ALPHA.                 CL**1
00333                                                                      CL**1
00334      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.         CL**1
00335                                                                      CL**1
00336      MOVE 'EL674'               TO  WS-REPORT-ID.                    CL**1
00337                                                                      CL**1
00338  1000-START.                                                         CL**1
00339      EXEC CICS  HANDLE CONDITION                                     CL**1
00340             ERROR    (8800-ABEND)                                    CL**1
00341             PGMIDERR (8900-PGMIDERR)                                 CL**1
00342      END-EXEC.                                                       CL**1
00343                                                                      CL**1
00344  2000-RECEIVE.                                                       CL**1
00345      EXEC CICS RETRIEVE                                              CL**1
00346          INTO   (PROGRAM-INTERFACE-BLOCK)                            CL**1
00347          LENGTH (CLEN)                                               CL**1
00348      END-EXEC.                                                       CL**1
00349                                                                      CL**1
00350      MOVE PI-LIFE-OVERRIDE-L1   TO HL1-1 HL1-2 HL1-3.                CL**1
00351      MOVE PI-AH-OVERRIDE-L1     TO HA1-1 HA1-2 HA1-3.                CL**1
00352      MOVE PI-LIFE-OVERRIDE-L6   TO HL6-1 HL6-2.                      CL**1
00353      MOVE PI-AH-OVERRIDE-L6     TO HA6-1 HA6-2 HA6-3 HA6-4.          CL**1
00354                                                                      CL**1
00355  2000-CHECK-IN-PROGRESS.                                             CL**1
00356      EXEC CICS  HANDLE CONDITION                                     CL**1
00357             NOTFND   (2000-WRITE-INITIAL-TRAILER)                    CL**1
00358      END-EXEC.                                                       CL**1
00359                                                                      CL**1
00360      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.                  CL**1
00361      MOVE 'RF'                   TO  RF-RECORD-ID.                   CL**1
00362      MOVE '2'                    TO  RF-RECORD-TYPE.                 CL**1
00363      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                   CL**1
00364      MOVE ZEROS                  TO  RF-LINE-NUMBER.                 CL**1
00365                                                                      CL**1
00366      EXEC CICS READ                                                  CL**1
00367          DATASET (REPT-FILE-ID)                                      CL**1
00368          INTO    (REPORT-SAVE-FILE)                                  CL**1
00369          RIDFLD  (RF-CONTROL-PRIMARY)                                CL**1
00370      END-EXEC.                                                       CL**1
00371                                                                      CL**1
00372 ********IF RECORD FOUND, THEN ANOTHER REPORT HAS ALREADY             CL**1
00373 ********BEEN STARTED.  IF PREVIOUS REPORT ABENDED AND DIDN'T         CL**1
00374 ********COMPLETE, THEN OPERATOR MUST PURGE REPORT AND CREATE         CL**1
00375 ********A NEW ONE.                                                   CL**1
00376      GO TO 9999-RETURN-CICS.                                         CL**1
00377                                                                      CL**1
00378  2000-WRITE-INITIAL-TRAILER.                                         CL**1
00379      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.                  CL**1
00380      MOVE 'RF'                   TO  RF-RECORD-ID.                   CL**1
00381      MOVE '2'                    TO  RF-RECORD-TYPE.                 CL**1
00382      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                   CL**1
00383      MOVE ZEROS                  TO  RF-LINE-NUMBER.                 CL**1
00384                                                                      CL**1
00385      MOVE SPACES                 TO  RF-TRAILER-RECORD.              CL**1
00386      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**1
00387      END-EXEC                                                        CL**1
00388      EXEC CICS FORMATTIME                                            CL**1
00389                ABSTIME(LCP-CICS-TIME)                                CL**1
00390                TIME(LCP-TIME-OF-DAY-XX)                              CL**1
00391      END-EXEC                                                        CL**1
00392      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**1
00393      MOVE 'STARTED'              TO  RF-CURRENT-DATE.                CL**1
00394                                                                      CL**1
00395      EXEC CICS WRITE                                                 CL**1
00396          DATASET (REPT-FILE-ID)                                      CL**1
00397          FROM    (REPORT-SAVE-FILE)                                  CL**1
00398          RIDFLD  (RF-CONTROL-PRIMARY)                                CL**1
00399      END-EXEC.                                                       CL**1
00400                                                                      CL**1
00401  2100-DELETE-REC.                                                    CL**1
00402      MOVE 1 TO RF-LINE-NUMBER.                                       CL**1
00403      EXEC CICS  HANDLE CONDITION                                     CL**1
00404             NOTFND   (2300-DELETE-REC)                               CL**1
00405      END-EXEC.                                                       CL**1
00406                                                                      CL**1
00407  2200-DELETE-1.                                                      CL**1
00408      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                            CL**1
00409      MOVE 'RF'          TO RF-RECORD-ID.                             CL**1
00410      MOVE '1'           TO RF-RECORD-TYPE.                           CL**1
00411      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                             CL**1
00412                                                                      CL**1
00413      EXEC CICS DELETE                                                CL**1
00414          DATASET   (REPT-FILE-ID)                                    CL**1
00415          RIDFLD    (RF-CONTROL-PRIMARY)                              CL**1
00416          KEYLENGTH (11)                                              CL**1
00417      END-EXEC.                                                       CL**1
00418                                                                      CL**1
00419      ADD 1 TO RF-LINE-NUMBER.                                        CL**1
00420      GO TO 2200-DELETE-1.                                            CL**1
00421                                                                      CL**1
00422  2300-DELETE-REC.                                                    CL**1
00423      EXEC CICS  HANDLE CONDITION                                     CL**1
00424             NOTFND   (3000-START-BROWSE)                             CL**1
00425      END-EXEC.                                                       CL**1
00426                                                                      CL**1
00427  2400-DELETE-2.                                                      CL**1
00428      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                            CL**1
00429      MOVE 'RF'          TO RF-RECORD-ID.                             CL**1
00430      MOVE '2'           TO RF-RECORD-TYPE.                           CL**1
00431      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                             CL**1
00432                                                                      CL**1
00433      EXEC CICS DELETE                                                CL**1
00434          DATASET   (REPT-FILE-ID)                                    CL**1
00435          RIDFLD    (RF-CONTROL-PRIMARY)                              CL**1
00436          KEYLENGTH (11)                                              CL**1
00437      END-EXEC.                                                       CL**1
00438                                                                      CL**1
00439      ADD 1 TO RF-LINE-NUMBER.                                        CL**1
00440      GO TO 2400-DELETE-2.                                            CL**1
00441                                                                      CL**1
00442  3000-START-BROWSE.                                                  CL**1
00443      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL**1
00444      MOVE SPACES                 TO CNTL-ACCESS.                     CL**1
00445      MOVE '1'                    TO CNTL-REC-TYPE.                   CL**1
00446      MOVE +0                     TO CNTL-SEQ-NO.                     CL**1
00447                                                                      CL**1
00448      PERFORM 9000-READ-CONTROL THRU 9000-EXIT.                       CL**1
00449                                                                      CL**1
00450      MOVE CF-CL-MAIL-TO-NAME TO H2-COMP.                             CL**1
00451      MOVE SAVE-DATE          TO H2-DATE.                             CL**1
00452      MOVE ' THRU '           TO H3-THRU.                             CL**1
00453                                                                      CL**1
00454      MOVE SPACE              TO DC-OPTION-CODE.                      CL**1
00455      MOVE PI-LO-DATE         TO DC-BIN-DATE-1.                       CL**1
00456      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                       CL**1
00457      MOVE DC-GREG-DATE-1-EDIT TO H3-DATE1.                           CL**1
00458                                                                      CL**1
00459      IF PI-LO-DATE = LOW-VALUES                                      CL**1
00460         MOVE 'INCEPT.'        TO H3-DATE1.                           CL**1
00461                                                                      CL**1
00462      MOVE SPACE               TO DC-OPTION-CODE.                     CL**1
00463      MOVE PI-HI-DATE          TO DC-BIN-DATE-1.                      CL**1
00464      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                       CL**1
00465      MOVE DC-GREG-DATE-1-EDIT TO H3-DATE2.                           CL**1
00466                                                                      CL**1
00467      IF PI-HI-DATE = HIGH-VALUES                                     CL**1
00468         MOVE 'CURRENT'        TO H3-DATE2.                           CL**1
00469                                                                      CL**1
00470      EXEC CICS  HANDLE CONDITION                                     CL**1
00471             NOTFND   (9999-RETURN-CICS)                              CL**1
00472      END-EXEC.                                                       CL**1
00473                                                                      CL**1
00474      MOVE LOW-VALUES    TO PI-ERREIN-KEY.                            CL**1
00475      MOVE PI-COMPANY-CD TO PI-CRR-COMPANY-CD.                        CL**1
00476                                                                      CL**1
00477      EXEC CICS STARTBR                                               CL**1
00478           DATASET  (REIN-FILE-ID)                                    CL**1
00479           RIDFLD   (PI-ERREIN-KEY)                                   CL**1
00480      END-EXEC.                                                       CL**1
00481                                                                      CL**1
00482      EXEC CICS  HANDLE CONDITION                                     CL**1
00483             ENDFILE  (4500-ENDBROWSE)                                CL**1
00484      END-EXEC.                                                       CL**1
00485                                                                      CL**1
00486  4000-READNEXT.                                                      CL**1
00487      EXEC CICS READNEXT                                              CL**1
00488           DATASET  (REIN-FILE-ID)                                    CL**1
00489           SET      (ADDRESS OF REINSURANCE-RECORD)                   CL**1
00490           RIDFLD   (PI-ERREIN-KEY)                                   CL**1
00491      END-EXEC.                                                       CL**1
00492                                                                      CL**1
00493      CONTINUE.                                                       CL**1
00494                                                                      CL**1
00495      IF PI-COMPANY-CD = RE-COMPANY-CD                                CL**1
00496         GO TO 5000-PRINT-IT.                                         CL**1
00497                                                                      CL**1
00498  4500-ENDBROWSE.                                                     CL**1
00499      EXEC CICS ENDBR                                                 CL**1
00500           DATASET  (REIN-FILE-ID)                                    CL**1
00501      END-EXEC.                                                       CL**1
00502                                                                      CL**1
00503  4600-DELETE-INITIAL-2.                                              CL**1
00504      MOVE PI-COMPANY-CD TO RF-COMPANY-CD.                            CL**1
00505      MOVE 'RF'          TO RF-RECORD-ID.                             CL**1
00506      MOVE '2'           TO RF-RECORD-TYPE.                           CL**1
00507      MOVE WS-REPORT-ID  TO RF-REPORT-ID.                             CL**1
00508      MOVE ZEROS         TO RF-LINE-NUMBER.                           CL**1
00509                                                                      CL**1
00510      EXEC CICS DELETE                                                CL**1
00511          DATASET   (REPT-FILE-ID)                                    CL**1
00512          RIDFLD    (RF-CONTROL-PRIMARY)                              CL**1
00513          KEYLENGTH (11)                                              CL**1
00514      END-EXEC.                                                       CL**1
00515                                                                      CL**1
00516      PERFORM 8999-WRITE-TRAILER.                                     CL**1
00517      GO TO 9999-RETURN-CICS.                                         CL**1
00518                                                                      CL**1
00519  5000-PRINT-IT.                                                      CL**1
00520      IF RE-LAST-MAINT-DT LESS    PI-LO-DATE OR                       CL**1
00521                          GREATER PI-HI-DATE                          CL**1
00522         GO TO 4000-READNEXT.                                         CL**1
00523                                                                      CL**1
00524      MOVE 0 TO LEVL.                                                 CL**1
00525                                                                      CL**1
00526      IF RE-CODE = 'A'                                                CL**1
00527          PERFORM 5200-TABLE-PRT-RTN THRU 5200-EXIT                   CL**1
00528      ELSE                                                            CL**1
00529      IF RE-CODE = 'B'                                                CL**1
00530          PERFORM 6000-COMP-PRT-RTN THRU 6000-EXIT.                   CL**1
00531                                                                      CL**1
00532      GO TO 4000-READNEXT.                                            CL**1
00533                                                                      CL**1
00534  5200-TABLE-PRT-RTN.                                                 CL**1
00535      MOVE 6 TO PRT-CNT.                                              CL**1
00536                                                                      CL**1
00537      PERFORM 5500-PRINT-COMP-INFO THRU 5500-EXIT                     CL**1
00538          VARYING SA FROM +1 BY +1 UNTIL SA = +31.                    CL**1
00539                                                                      CL**1
00540  5200-EXIT.                                                          CL**1
00541      EXIT.                                                           CL**1
00542                                                                      CL**1
00543  5500-PRINT-COMP-INFO.                                               CL**1
00544      IF RE-REI-COMP (SA) = SPACES                                    CL**1
00545          GO TO 5500-EXIT.                                            CL**1
00546                                                                      CL**1
00547      IF PRT-CNT = 6                                                  CL**1
00548          PERFORM 6500-HDR-RTN THRU 6500-EXIT.                        CL**1
00549                                                                      CL**1
00550      MOVE '0'                         TO RF-CTL-CHAR-133.            CL**1
00551      ADD 1                            TO LEVL.                       CL**1
00552      MOVE LEVL                        TO H4-LEVL.                    CL**1
00553      MOVE RE-TABLE                    TO H4-CODE.                    CL**1
00554      MOVE RE-REI-COMP-NO (SA)         TO H4-COMP.                    CL**1
00555      MOVE HDR-A4                      TO RF-DATA-133.                CL**1
00556      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00557      MOVE ' '                         TO RF-CTL-CHAR-133.            CL**1
00558      MOVE HDR-A5                      TO RF-DATA-133.                CL**1
00559      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00560                                                                      CL**1
00561      IF PRT-CNT = 1                                                  CL**1
00562          MOVE HDR-A6 TO RF-DATA-133                                  CL**1
00563          PERFORM 7000-PRT-LINE THRU 7000-EXIT                        CL**1
00564          MOVE HDR-A7 TO RF-DATA-133                                  CL**1
00565          PERFORM 7000-PRT-LINE THRU 7000-EXIT.                       CL**1
00566                                                                      CL**1
00567      MOVE SPACES TO DTL-A1.                                          CL**1
00568                                                                      CL**1
00569      IF SA = RE-100-COMP                                             CL**1
00570          MOVE 'EXCESS' TO D1-BEN-DESC                                CL**1
00571          MOVE DTL-A1   TO RF-DATA-133                                CL**1
00572          PERFORM 7000-PRT-LINE THRU 7000-EXIT                        CL**1
00573          GO TO 5500-EXIT.                                            CL**1
00574                                                                      CL**1
00575      MOVE PI-LIFE-OVERRIDE-L6 TO D1-BEN-DESC.                        CL**1
00576      MOVE RE-LO-DATE (SA)     TO WS-DATE.                            CL**1
00577      MOVE WS-MO               TO D1-EFF-MO.                          CL**1
00578      MOVE WS-DA               TO D1-EFF-DA.                          CL**1
00579      MOVE WS-YR               TO D1-EFF-YR.                          CL**1
00580      MOVE RE-HI-DATE (SA)     TO WS-DATE.                            CL**1
00581      MOVE WS-MO               TO D1-EXP-MO.                          CL**1
00582      MOVE WS-DA               TO D1-EXP-DA.                          CL**1
00583      MOVE WS-YR               TO D1-EXP-YR.                          CL**1
00584                                                                      CL**1
00585      MOVE '/'                 TO D1-SLSH-1 D1-SLSH-2                 CL**1
00586                                  D1-SLSH-3 D1-SLSH-4.                CL**1
00587                                                                      CL**1
00588      MOVE RE-LFAGE-LO (SA)    TO D1-LO-AGE.                          CL**1
00589      MOVE RE-LFAGE-HI (SA)    TO D1-HI-AGE.                          CL**1
00590      MOVE RE-LFTRM-LO (SA)    TO D1-LO-TRM.                          CL**1
00591      MOVE RE-LFTRM-HI (SA)    TO D1-HI-TRM.                          CL**1
00592      MOVE RE-LF-LIM-LO (SA)   TO D1-LO-LIM.                          CL**1
00593      MOVE RE-LF-LIM-HI (SA)   TO D1-HI-LIM.                          CL**1
00594      MOVE DTL-A1              TO RF-DATA-133.                        CL**1
00595      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00596                                                                      CL**1
00597      MOVE SPACES               TO DTL-A1.                            CL**1
00598      MOVE PI-AH-OVERRIDE-L2    TO D1-BEN-LFAH.                       CL**1
00599      MOVE 'TOT BEN'            TO D1-BEN-REST.                       CL**1
00600      MOVE RE-AHAGE-LO (SA)     TO D1-LO-AGE.                         CL**1
00601      MOVE RE-AHAGE-HI (SA)     TO D1-HI-AGE.                         CL**1
00602      MOVE RE-AHTRM-LO (SA)     TO D1-LO-TRM.                         CL**1
00603      MOVE RE-AHTRM-HI (SA)     TO D1-HI-TRM.                         CL**1
00604      MOVE RE-AHBEN-LIM-LO (SA) TO D1-LO-LIM.                         CL**1
00605      MOVE RE-AHBEN-LIM-HI (SA) TO D1-HI-LIM.                         CL**1
00606      MOVE DTL-A1               TO RF-DATA-133.                       CL**1
00607      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00608                                                                      CL**1
00609      MOVE SPACES               TO DTL-A1.                            CL**1
00610      MOVE PI-AH-OVERRIDE-L2    TO D1-BEN-LFAH.                       CL**1
00611      MOVE 'MON BEN'            TO D1-BEN-REST.                       CL**1
00612      MOVE RE-AHMOA-LIM-LO (SA) TO D1-LO-LIM.                         CL**1
00613      MOVE RE-AHMOA-LIM-HI (SA) TO D1-HI-LIM.                         CL**1
00614      MOVE DTL-A1               TO RF-DATA-133.                       CL**1
00615      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00616                                                                      CL**1
00617      MOVE HDR-A8               TO RF-DATA-133.                       CL**1
00618      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00619      MOVE SPACES               TO DTL-A2.                            CL**1
00620                                                                      CL**1
00621      IF PRT-CNT = 1                                                  CL**1
00622          MOVE HDR-A9           TO RF-DATA-133                        CL**1
00623          PERFORM 7000-PRT-LINE THRU 7000-EXIT                        CL**1
00624          MOVE HDR-A10          TO RF-DATA-133                        CL**1
00625          PERFORM 7000-PRT-LINE THRU 7000-EXIT.                       CL**1
00626                                                                      CL**1
00627      MOVE RE-LF-BEN-CODE (SA)    TO D1-BEN-CODE.                     CL**1
00628      MOVE RE-LF-QC (SA)          TO D1-QC.                           CL**1
00629      MOVE RE-INTERACTIVE (SA)    TO D1-IN.                           CL**1
00630      MOVE RE-REMAINING (SA)      TO D1-RM.                           CL**1
00631      MOVE RE-NSP-ST-CD-LF        TO D1-ST.                           CL**1
00632      MULTIPLY RE-LF-PCT (SA) BY +100 GIVING D1-PCT.                  CL**1
00633      MOVE RE-LF-LO (SA)          TO D1-LO.                           CL**1
00634      MOVE RE-LF-HI (SA)          TO D1-HI.                           CL**1
00635      MOVE DTL-A2                 TO RF-DATA-133.                     CL**1
00636      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00637                                                                      CL**1
00638      MOVE SPACES                 TO DTL-A2.                          CL**1
00639      MOVE RE-AH-BEN-CODE (SA)    TO D1-BEN-CODE.                     CL**1
00640      MOVE RE-AH-QC (SA)          TO D1-QC.                           CL**1
00641      MOVE RE-NSP-ST-CD-AH        TO D1-ST.                           CL**1
00642      MULTIPLY RE-AH-PCT (SA) BY +100 GIVING D1-PCT.                  CL**1
00643      MOVE RE-AHBEN-LO (SA)       TO D1-LO.                           CL**1
00644      MOVE RE-AHBEN-HI (SA)       TO D1-HI.                           CL**1
00645      MOVE DTL-A2                 TO RF-DATA-133.                     CL**1
00646      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00647                                                                      CL**1
00648      MOVE SPACES                 TO DTL-A2.                          CL**1
00649      MOVE RE-AHMOA-LO (SA)       TO D1-LO.                           CL**1
00650      MOVE RE-AHMOA-HI (SA)       TO D1-HI.                           CL**1
00651      MOVE DTL-A2                 TO RF-DATA-133.                     CL**1
00652      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00653      ADD 1 TO PRT-CNT.                                               CL**1
00654                                                                      CL**1
00655  5500-EXIT.                                                          CL**1
00656      EXIT.                                                           CL**1
00657                                                                      CL**1
00658  6000-COMP-PRT-RTN.                                                  CL**1
00659      IF B-REC-FLAG = SPACES                                          CL**1
00660          MOVE 'X' TO B-REC-FLAG                                      CL**1
00661          MOVE 7   TO PRT-CNT.                                        CL**1
00662                                                                      CL**1
00663      IF PRT-CNT = 7                                                  CL**1
00664          PERFORM 6500-HDR-RTN THRU 6500-EXIT.                        CL**1
00665                                                                      CL**1
00666      MOVE '0'     TO RF-CTL-CHAR-133.                                CL**1
00667      MOVE HDR-B1  TO RF-DATA-133.                                    CL**1
00668      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00669      MOVE ' '     TO RF-CTL-CHAR-133.                                CL**1
00670      MOVE HDR-B2  TO RF-DATA-133.                                    CL**1
00671      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00672                                                                      CL**1
00673      MOVE SPACES     TO DTL-B1.                                      CL**1
00674      MOVE RE-COMPANY TO D2-COMP.                                     CL**1
00675      MOVE RE-NAME    TO D2-NAME.                                     CL**1
00676      MOVE RE-LF-PE   TO D2-LF-PE.                                    CL**1
00677      MOVE RE-AH-PE   TO D2-AH-PE.                                    CL**1
00678      MOVE RE-PRT-ST  TO D2-ST.                                       CL**1
00679      MOVE RE-PRT-OW  TO D2-OW.                                       CL**1
00680                                                                      CL**1
00681      MULTIPLY RE-LF-FEE    BY +100 GIVING D2-LF-FEE.                 CL**1
00682      MULTIPLY RE-AH-FEE    BY +100 GIVING D2-AH-FEE.                 CL**1
00683      MULTIPLY RE-AH-PR-PCT BY +100 GIVING D2-PR-PCT.                 CL**1
00684      MULTIPLY RE-AH-78-PCT BY +100 GIVING D2-78-PCT.                 CL**1
00685                                                                      CL**1
00686      IF RE-LF-FEE = ZERO                                             CL**1
00687          MOVE 'ACCOUNT' TO D2-LF-MSG.                                CL**1
00688                                                                      CL**1
00689      IF RE-AH-FEE = ZERO                                             CL**1
00690          MOVE 'ACCOUNT' TO D2-AH-MSG.                                CL**1
00691                                                                      CL**1
00692      IF RE-ZERO-LF-FEE = 'Y'                                         CL**1
00693          MOVE '   ZERO' TO D2-LF-MSG.                                CL**1
00694                                                                      CL**1
00695      IF RE-ZERO-AH-FEE = 'Y'                                         CL**1
00696          MOVE '   ZERO' TO D2-AH-MSG.                                CL**1
00697                                                                      CL**1
00698      MOVE DTL-B1     TO RF-DATA-133.                                 CL**1
00699      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00700                                                                      CL**1
00701      MOVE '0'        TO RF-CTL-CHAR-133.                             CL**1
00702      MOVE HDR-B3     TO RF-DATA-133.                                 CL**1
00703      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00704      MOVE ' '        TO RF-CTL-CHAR-133.                             CL**1
00705      MOVE HDR-B4     TO RF-DATA-133.                                 CL**1
00706      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00707                                                                      CL**1
00708      MOVE SPACES         TO DTL-B2.                                  CL**1
00709      MOVE RE-MORT-CODE   TO D2-MORT.                                 CL**1
00710      MOVE RE-CLAIM-CODE  TO D2-CLAIM.                                CL**1
00711      MOVE RE-ZERO-LF-FEE TO D2-Z-L-FE.                               CL**1
00712      MOVE RE-ZERO-AH-FEE TO D2-Z-A-FE.                               CL**1
00713      MOVE RE-LF-COMM     TO D2-L-COMM.                               CL**1
00714      MOVE RE-AH-COMM     TO D2-A-COMM.                               CL**1
00715      MOVE RE-LF-TAX      TO D2-L-TAX.                                CL**1
00716      MOVE RE-AH-TAX      TO D2-A-TAX.                                CL**1
00717      MOVE RE-CEDE-NAME   TO D2-CEDE-NAME.                            CL**1
00718      MOVE ' '            TO RF-CTL-CHAR-133.                         CL**1
00719      MOVE DTL-B2         TO RF-DATA-133.                             CL**1
00720      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00721      ADD +1              TO PRT-CNT.                                 CL**1
00722                                                                      CL**1
00723  6000-EXIT.                                                          CL**1
00724      EXIT.                                                           CL**1
00725                                                                      CL**1
00726  6500-HDR-RTN.                                                       CL**1
00727      ADD +1              TO WS-PAGE.                                 CL**1
00728      MOVE WS-PAGE        TO H3-PAGE.                                 CL**1
00729      MOVE '1'            TO RF-CTL-CHAR-133.                         CL**1
00730      MOVE HDR-A1         TO RF-DATA-133.                             CL**1
00731      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00732      MOVE ' '            TO RF-CTL-CHAR-133.                         CL**1
00733      MOVE HDR-A2         TO RF-DATA-133.                             CL**1
00734      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00735      MOVE ' '            TO RF-CTL-CHAR-133.                         CL**1
00736      MOVE HDR-A3         TO RF-DATA-133.                             CL**1
00737      PERFORM 7000-PRT-LINE THRU 7000-EXIT.                           CL**1
00738      MOVE +1             TO PRT-CNT.                                 CL**1
00739                                                                      CL**1
00740  6500-EXIT.                                                          CL**1
00741      EXIT.                                                           CL**1
00742                                                                      CL**1
00743  7000-PRT-LINE.                                                      CL**1
00744      MOVE PI-COMPANY-CD  TO RF-COMPANY-CD.                           CL**1
00745      MOVE 'RF'           TO RF-RECORD-ID.                            CL**1
00746      MOVE '1'            TO RF-RECORD-TYPE.                          CL**1
00747      MOVE WS-REPORT-ID   TO RF-REPORT-ID.                            CL**1
00748      ADD 1               TO WS-LINE-NUMBER.                          CL**1
00749      MOVE WS-LINE-NUMBER TO RF-LINE-NUMBER.                          CL**1
00750                                                                      CL**1
00751      EXEC CICS WRITE                                                 CL**1
00752          DATASET (REPT-FILE-ID)                                      CL**1
00753          FROM    (REPORT-SAVE-FILE)                                  CL**1
00754          RIDFLD  (RF-CONTROL-PRIMARY)                                CL**1
00755      END-EXEC.                                                       CL**1
00756                                                                      CL**1
00757  7000-EXIT.                                                          CL**1
00758       EXIT.                                                          CL**1
00759                                                                      CL**1
00760  8500-DATE-CONVERT.                                                  CL**1
00761      MOVE LINK-ELDATCV           TO PGM-NAME.                        CL**1
00762                                                                      CL**1
00763      EXEC CICS LINK                                                  CL**1
00764          PROGRAM    (PGM-NAME)                                       CL**1
00765          COMMAREA   (DATE-CONVERSION-DATA)                           CL**1
00766          LENGTH     (DC-COMM-LENGTH)                                 CL**1
00767      END-EXEC.                                                       CL**1
00768                                                                      CL**1
00769  8500-EXIT.                                                          CL**1
00770      EXIT.                                                           CL**1
00771                                                                      CL**1
00772  8800-ABEND.                                                         CL**1
00773      MOVE DFHEIBLK TO EMI-LINE1.                                     CL**1
00774                                                                      CL**1
00775      EXEC CICS LINK                                                  CL**1
00776          PROGRAM   ('EL004')                                         CL**1
00777          COMMAREA  (EMI-LINE1)                                       CL**1
00778          LENGTH    (72)                                              CL**1
00779      END-EXEC.                                                       CL**1
00780                                                                      CL**1
00781      GO TO 9999-RETURN-CICS.                                         CL**1
00782                                                                      CL**1
00783  8900-PGMIDERR.                                                      CL**1
00784      GO TO 9999-RETURN-CICS.                                         CL**1
00785       GOBACK.                                                        CL**1
00786                                                                      CL**1
00787  8999-WRITE-TRAILER.                                                 CL**1
00788      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.                  CL**1
00789      MOVE 'RF'                   TO  RF-RECORD-ID.                   CL**1
00790      MOVE '2'                    TO  RF-RECORD-TYPE.                 CL**1
00791      MOVE WS-REPORT-ID           TO  RF-REPORT-ID.                   CL**1
00792      ADD +1                      TO  WS-LINE-NUMBER.                 CL**1
00793      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.                 CL**1
00794                                                                      CL**1
00795      MOVE SPACES                 TO  RF-TRAILER-RECORD.              CL**1
00796      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL**1
00797      END-EXEC                                                        CL**1
00798      EXEC CICS FORMATTIME                                            CL**1
00799                ABSTIME(LCP-CICS-TIME)                                CL**1
00800                TIME(LCP-TIME-OF-DAY-XX)                              CL**1
00801      END-EXEC                                                        CL**1
00802      MOVE  LCP-TIME-OF-DAY-68 TO RF-PRINT-HH-MM-SS.                  CL**1
00803      MOVE SAVE-DATE              TO  RF-CURRENT-DATE.                CL**1
00804                                                                      CL**1
00805      EXEC CICS WRITE                                                 CL**1
00806          DATASET (REPT-FILE-ID)                                      CL**1
00807          FROM    (REPORT-SAVE-FILE)                                  CL**1
00808          RIDFLD  (RF-CONTROL-PRIMARY)                                CL**1
00809      END-EXEC.                                                       CL**1
00810                                                                      CL**1
00811  9000-READ-CONTROL.                                                  CL**1
00812      EXEC CICS READ                                                  CL**1
00813          DATASET     (CNTL-ID)                                       CL**1
00814          SET         (ADDRESS OF CONTROL-FILE)                       CL**1
00815          RIDFLD      (ELCNTL-KEY)                                    CL**1
00816      END-EXEC.                                                       CL**1
00817                                                                      CL**1
00818      CONTINUE.                                                       CL**1
00819                                                                      CL**1
00820  9000-EXIT.                                                          CL**1
00821       EXIT.                                                          CL**1
00822                                                                      CL**1
00823  9999-RETURN-CICS.                                                   CL**1
00824      EXEC CICS  RETURN                                               CL**1
00825      END-EXEC.                                                       CL**1
00826                                                                      CL**1
00827  9999-EXIT.                                                          CL**1
00828       EXIT.                                                          CL**1
00829                                                                      CL**1
