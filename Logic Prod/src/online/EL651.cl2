00001  ID DIVISION.                                                     06/10/98
00002                                                                   EL651
00003  PROGRAM-ID.                 EL651.                                  LV041
00004 *              PROGRAM CONVERTED BY                                  CL*16
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*16
00006 *              CONVERSION DATE 05/16/94 10:57:36.                    CL*16
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE               CL*38
00008 *                            VMOD=2.019                              CL*27
00009                                                                   EL651
00010 *AUTHOR.     LOGIC,INC.                                              CL*16
00011 *            DALLAS, TEXAS.                                          CL*16
00012                                                                   EL651
00013 *DATE-COMPILED.                                                      CL*16
00014                                                                      CL*13
00015 *SECURITY.   *****************************************************   CL*16
00016 *            *                                                   *   CL*16
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*16
00018 *            *                                                   *   CL*16
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*16
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*16
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*16
00022 *            *                                                   *   CL*16
00023 *            *****************************************************   CL*16
00024                                                                   EL651
00025 *REMARKS.    TRANSACTION - EXD1 - REINSURANCE MASTER MAINT.          CL*16
00026                                                                   EL651
00027      EJECT                                                        EL651
00028  ENVIRONMENT DIVISION.                                            EL651
00029  DATA DIVISION.                                                   EL651
00030  WORKING-STORAGE SECTION.                                         EL651
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL651
00032  77  FILLER  PIC X(32)  VALUE '*    EL651 WORKING STORAGE     *'. EL651
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.019 *********'.    CL*27
00034                                                                   EL651
00035                            COPY ELCSCTM.                             CL*12
00036                            COPY ELCSCRTY.                            CL*12
00037      EJECT                                                        EL651
00038  01  WS-DATE-AREA.                                                EL651
00039      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL651
00040      05  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.    EL651
00041                                                                   EL651
00042  01  STANDARD-AREAS.                                              EL651
00043      05  GETMAIN-SPACE               PIC X       VALUE SPACE.     EL651
00044      05  MAP-NAME                    PIC X(8)    VALUE 'EL651A'.  EL651
00045      05  MAPSET-NAME                 PIC X(8)    VALUE 'EL651S'.  EL651
00046      05  TRANS-ID                    PIC X(4)    VALUE 'EXD1'.    EL651
00047      05  EL6592-TRANS-ID             PIC X(4)    VALUE 'EX66'.       CL**9
00048      05  THIS-PGM                    PIC X(8)    VALUE 'EL651'.   EL651
00049      05  PGM-NAME                    PIC X(8).                    EL651
00050      05  SUB1                        PIC S99     VALUE +0.        EL651
00051      05  SC-ITEM                     PIC S9(4) COMP VALUE +1.     EL651
00052      05  WS-SAVE-PI-SUB              PIC S99     VALUE +0.        EL651
00053                                                                   EL651
00054      05  NINES                       PIC S9(9)V99 VALUE           EL651
00055                                                  +999999999.99.   EL651
00056      05  TIME-IN                     PIC S9(7).                   EL651
00057      05  TIME-OUT-R  REDEFINES TIME-IN.                           EL651
00058          10  FILLER                  PIC X.                       EL651
00059          10  TIME-OUT                PIC 99V99.                   EL651
00060          10  FILLER                  PIC X(2).                    EL651
00061      05  TIME-MT                     PIC S9(7).                   EL651
00062      05  TIME-MT-R  REDEFINES TIME-MT.                            EL651
00063          10  FILLER                  PIC X.                       EL651
00064          10  TIME-LMT                PIC 99V99.                   EL651
00065          10  FILLER                  PIC X(2).                    EL651
00066      05  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL651
00067      05  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL651
00068      05  XCTL-626                    PIC X(8)    VALUE 'EL126'.   EL651
00069      05  XCTL-6511                   PIC X(8)    VALUE 'EL6511'.  EL651
00070      05  XCTL-657                    PIC X(8)    VALUE 'EL657'.   EL651
00071      05  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL651
00072      05  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL651
00073      05  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL651
00074      05  FILE-ID                     PIC X(8)    VALUE SPACES.    EL651
00075      05  REIN-FILE-ID                PIC X(8)    VALUE 'ERREIN'.  EL651
00076      05  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.  EL651
00077      05  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.    EL651
00078      05  WS-SAVE-REC                 PIC X(131)  VALUE SPACES.    EL651
00079                                                                   EL651
00080  01  MISC-WORK-AREAS.                                             EL651
00081      05  WS-LOW-DATE                 PIC X(8)    VALUE '00/00/00'.EL651
00082      05  WS-HIGH-DATE                PIC X(8)    VALUE '99/99/99'.EL651
00083      05  WS-HOLD-PIC98               PIC 9(8)    VALUE ZEROS.        CL*41
00084      05  WS-HOLD-PIC98-2             PIC 9(8)    VALUE ZEROS.        CL*41
00085      05  WS-PHONE-IN                 PIC 9(10).                   EL651
00086      05  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.                    EL651
00087          10  WSPI-AREA               PIC X(3).                    EL651
00088          10  WSPI-PFX                PIC X(3).                    EL651
00089          10  WSPI-SFX                PIC X(4).                    EL651
00090      05  WS-PHONE-OUT.                                            EL651
00091          10  WSPO-AREA               PIC X(3).                    EL651
00092          10  FILLER                  PIC X       VALUE '-'.       EL651
00093          10  WSPO-PFX                PIC X(3).                    EL651
00094          10  FILLER                  PIC X       VALUE '-'.       EL651
00095          10  WSPO-SFX                PIC X(4).                    EL651
00096      05  WS-SAVE-KEY                 PIC X(8).                    EL651
00097      05  WS-GETMAIN-SW               PIC X       VALUE ' '.       EL651
00098                                                                   EL651
00099      05  DEEDIT-FIELD                PIC X(15).                   EL651
00100      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL651
00101      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(13)V99.   EL651
00102                                                                   EL651
00103      05  ERREIN-LENGTH               PIC S9(4)   VALUE +4023 COMP.EL651
00104      05  ELCNTL-LENGTH               PIC S9(4)   VALUE +773  COMP.   CL*13
00105      05  FILE-LENGTH                 PIC S9(4)   VALUE +0    COMP.EL651
00106      05  DATE-TEST-AREA              PIC 9(8).                       CL*29
00107      05  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.              EL651
00108          10  DATE-TEST-CC            PIC 99.                         CL*29
00109          10  DATE-TEST-YY            PIC 99.                         CL*29
00110          10  DATE-TEST-MM            PIC 99.                         CL*29
00111          10  DATE-TEST-DD            PIC 99.                      EL651
00112      05  DIVIDE-RESULT               PIC 99.                      EL651
00113      05  DIVIDE-REMAINDER            PIC 9.                       EL651
00114                                                                   EL651
00115      05  WS-PREV-AGE                 PIC 99       VALUE ZERO.     EL651
00116      05  WS-PREV-TRM                 PIC 999      VALUE ZERO.     EL651
00117      05  WS-PREV-AMT                 PIC S9(9)V99 VALUE ZERO.     EL651
00118                                                                   EL651
00119      05  WS-CHANGE-SW                PIC XX      VALUE SPACES.    EL651
00120          88  CHANGES-NOT-MADE                    VALUE SPACES.    EL651
00121                                                                   EL651
00122      05  WS-PAGE-LEVEL-SW            PIC X       VALUE SPACE.     EL651
00123          88  PAGE-LEVEL-FORWARD                  VALUE 'F'.       EL651
00124          88  PAGE-LEVEL-BACKWARD                 VALUE 'B'.       EL651
00125                                                                   EL651
00126      05  WS-CHECK-LEVEL              PIC S9(4)  VALUE +0  COMP.   EL651
00127          88  VALID-LEVEL                        VALUE +1 THRU +30.EL651
00128                                                                   EL651
00129      05  WS-CHECK-PERCENT            PIC X       VALUE SPACE.     EL651
00130          88  EXCESS-LEVEL                        VALUE 'X'.       EL651
00131          88  NO-LEVEL                            VALUE ' '.       EL651
00132          88  VALID-PERCENT                       VALUE 'X' ' '.   EL651
00133                                                                   EL651
00134      05  WS-CHECK-QTED               PIC X       VALUE SPACE.     EL651
00135          88  VALID-QTED-CALC                     VALUE '1' '2'    EL651
00136                                                        '3' '4'    EL651
00137                                                        ' '.       EL651
00138                                                                   EL651
00139      05  WS-CHECK-BEN-CODE           PIC X      VALUE SPACE.      EL651
00140          88  VALID-BEN-CODE                     VALUE 'A' THRU 'N'   CL*10
00141                                                   'P' 'R' 'S' ' '    CL*11
00142                                                       '2' '3' '4' EL651
00143                                                       '7' '8'.    EL651
00144                                                                   EL651
00145      05  WS-CHECK-INTR               PIC X       VALUE SPACE.     EL651
00146          88  VALID-INTR-CODE                     VALUE 'X' 'Y' 'Z'EL651
00147                                                        'E' 'W'       CL*18
00148                                                        'N' ' '.   EL651
00149                                                                   EL651
00150      05  WS-CHECK-REM                PIC X       VALUE SPACE.     EL651
00151          88  VALID-REM-SW                        VALUE 'X' 'Y' 'N'EL651
00152                                                        'R' 'L' 'Z'EL651
00153                                                        ' '.       EL651
00154                                                                   EL651
00155      05  WS-STATE-FOUND-SW           PIC X       VALUE SPACE.     EL651
00156          88  STATE-FOUND                         VALUE 'Y'.       EL651
00157                                                                   EL651
00158      05  WS-CARRIER-FOUND-SW         PIC X       VALUE SPACE.     EL651
00159          88  CARRIER-FOUND                       VALUE 'Y'.       EL651
00160                                                                   EL651
00161      05  WS-ACCESS.                                               EL651
00162          10  WS-STATE                PIC XX      VALUE SPACES.    EL651
00163          10  FILLER                  PIC XX      VALUE SPACES.    EL651
00164      05  CARRIER-ACCESS      REDEFINES                            EL651
00165          WS-ACCESS.                                               EL651
00166          10  FILLER                  PIC X(3).                    EL651
00167          10  WS-CARRIER              PIC X.                       EL651
00168                                                                   EL651
00169      05  ELCNTL-KEY.                                              EL651
00170          10  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.    EL651
00171          10  CNTL-REC-TYPE           PIC X       VALUE SPACES.    EL651
00172          10  CNTL-ACCESS             PIC X(4)    VALUE SPACES.    EL651
00173          10  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.  EL651
00174                                                                   EL651
00175      05  MISC-SAVE-AREAS.                                         EL651
00176          10  WS-LO-HI-DATES.                                      EL651
00177              15  WS-LO-DATE          PIC 9(8).                       CL*20
00178              15  WS-HI-DATE          PIC 9(8).                       CL*20
00179          10  WS-LIFE-AGE-TRMS.                                    EL651
00180              15  WS-LFAGE-LO         PIC 99.                      EL651
00181              15  WS-LFAGE-HI         PIC 99.                      EL651
00182              15  WS-LFTRM-LO         PIC S999.                    EL651
00183              15  WS-LFTRM-HI         PIC S999.                    EL651
00184          10  WS-AH-AGE-TRMS.                                      EL651
00185              15  WS-AHAGE-LO         PIC 99.                      EL651
00186              15  WS-AHAGE-HI         PIC 99.                      EL651
00187              15  WS-AHTRM-LO         PIC S999.                    EL651
00188              15  WS-AHTRM-HI         PIC S999.                    EL651
00189          10  WS-LIFE-BEN-AMTS.                                    EL651
00190              15  WS-LF-LIM-LO        PIC S9(9)V99.                EL651
00191              15  WS-LF-LIM-HI        PIC S9(9)V99.                EL651
00192              15  WS-LF-LO            PIC S9(9)V99.                EL651
00193              15  WS-LF-HI            PIC S9(9)V99.                EL651
00194          10  WS-AH-BEN-AMTS.                                      EL651
00195              15  WS-AHBEN-LIM-LO     PIC S9(7)V99.                EL651
00196              15  WS-AHBEN-LIM-HI     PIC S9(7)V99.                EL651
00197              15  WS-AHMOA-LIM-LO     PIC S9(7)V99.                EL651
00198              15  WS-AHMOA-LIM-HI     PIC S9(7)V99.                EL651
00199              15  WS-AHBEN-LO         PIC S9(7)V99.                EL651
00200              15  WS-AHBEN-HI         PIC S9(7)V99.                EL651
00201              15  WS-AHMOA-LO         PIC S9(7)V99.                EL651
00202              15  WS-AHMOA-HI         PIC S9(7)V99.                EL651
00203      EJECT                                                        EL651
00204      05  ERROR-MESSAGES.                                          EL651
00205          10  ER-0000                 PIC X(4)    VALUE '0000'.    EL651
00206          10  ER-0004                 PIC X(4)    VALUE '0004'.    EL651
00207          10  ER-0008                 PIC X(4)    VALUE '0008'.    EL651
00208          10  ER-0023                 PIC X(4)    VALUE '0023'.    EL651
00209          10  ER-0029                 PIC X(4)    VALUE '0029'.    EL651
00210          10  ER-0050                 PIC X(4)    VALUE '0050'.    EL651
00211          10  ER-0068                 PIC X(4)    VALUE '0068'.    EL651
00212          10  ER-0070                 PIC X(4)    VALUE '0070'.    EL651
00213          10  ER-0142                 PIC X(4)    VALUE '0142'.    EL651
00214          10  ER-0585                 PIC X(4)    VALUE '0585'.    EL651
00215          10  ER-0589                 PIC X(4)    VALUE '0589'.    EL651
00216          10  ER-0592                 PIC X(4)    VALUE '0592'.    EL651
00217          10  ER-0593                 PIC X(4)    VALUE '0593'.    EL651
00218          10  ER-2055                 PIC X(4)    VALUE '2055'.    EL651
00219          10  ER-2056                 PIC X(4)    VALUE '2056'.    EL651
00220          10  ER-2067                 PIC X(4)    VALUE '2067'.    EL651
00221          10  ER-2112                 PIC X(4)    VALUE '2112'.    EL651
00222          10  ER-2139                 PIC X(4)    VALUE '2139'.    EL651
00223          10  ER-2140                 PIC X(4)    VALUE '2140'.    EL651
00224          10  ER-2141                 PIC X(4)    VALUE '2141'.    EL651
00225          10  ER-2208                 PIC X(4)    VALUE '2208'.    EL651
00226          10  ER-2310                 PIC X(4)    VALUE '2310'.    EL651
00227          10  ER-2311                 PIC X(4)    VALUE '2311'.    EL651
00228          10  ER-2312                 PIC X(4)    VALUE '2312'.    EL651
00229          10  ER-2313                 PIC X(4)    VALUE '2313'.    EL651
00230          10  ER-2316                 PIC X(4)    VALUE '2316'.    EL651
00231          10  ER-2317                 PIC X(4)    VALUE '2317'.    EL651
00232          10  ER-2318                 PIC X(4)    VALUE '2318'.    EL651
00233          10  ER-2319                 PIC X(4)    VALUE '2319'.    EL651
00234          10  ER-2320                 PIC X(4)    VALUE '2320'.    EL651
00235          10  ER-2321                 PIC X(4)    VALUE '2321'.    EL651
00236          10  ER-2322                 PIC X(4)    VALUE '2322'.    EL651
00237          10  ER-2323                 PIC X(4)    VALUE '2323'.    EL651
00238          10  ER-2324                 PIC X(4)    VALUE '2324'.    EL651
00239          10  ER-2325                 PIC X(4)    VALUE '2325'.    EL651
00240          10  ER-2326                 PIC X(4)    VALUE '2326'.    EL651
00241          10  ER-2327                 PIC X(4)    VALUE '2327'.    EL651
00242          10  ER-2328                 PIC X(4)    VALUE '2328'.    EL651
00243          10  ER-2329                 PIC X(4)    VALUE '2329'.    EL651
00244          10  ER-2330                 PIC X(4)    VALUE '2330'.    EL651
00245          10  ER-2331                 PIC X(4)    VALUE '2331'.    EL651
00246          10  ER-2333                 PIC X(4)    VALUE '2333'.    EL651
00247          10  ER-2334                 PIC X(4)    VALUE '2334'.    EL651
00248          10  ER-2335                 PIC X(4)    VALUE '2335'.    EL651
00249          10  ER-2338                 PIC X(4)    VALUE '2338'.    EL651
00250          10  ER-2339                 PIC X(4)    VALUE '2339'.    EL651
00251          10  ER-2340                 PIC X(4)    VALUE '2340'.    EL651
00252          10  ER-2341                 PIC X(4)    VALUE '2341'.    EL651
00253          10  ER-2342                 PIC X(4)    VALUE '2342'.    EL651
00254          10  ER-2343                 PIC X(4)    VALUE '2343'.    EL651
00255          10  ER-2344                 PIC X(4)    VALUE '2344'.    EL651
00256          10  ER-2345                 PIC X(4)    VALUE '2345'.    EL651
00257          10  ER-2346                 PIC X(4)    VALUE '2346'.    EL651
00258          10  ER-2347                 PIC X(4)    VALUE '2347'.    EL651
00259          10  ER-2348                 PIC X(4)    VALUE '2348'.    EL651
00260          10  ER-2356                 PIC X(4)    VALUE '2356'.    EL651
00261          10  ER-2370                 PIC X(4)    VALUE '2370'.    EL651
00262          10  ER-4008                 PIC X(4)    VALUE '4008'.       CL*14
00263          10  ER-7805                 PIC X(4)    VALUE '7805'.       CL*12
00264                                                                   EL651
00265      EJECT                                                        EL651
00266                                      COPY ELCDATE.                   CL*12
00267      EJECT                                                        EL651
00268                                      COPY ELCLOGOF.                  CL*12
00269      EJECT                                                        EL651
00270                                      COPY ELCATTR.                   CL*12
00271      EJECT                                                        EL651
00272                                      COPY ELCEMIB.                   CL*12
00273      EJECT                                                        EL651
00274                                      COPY ELCINTF.                   CL*12
00275      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.             EL651
00276          16  PI-CHECK-MAINT-TYPE     PIC X.                       EL651
00277              88  VALID-MAINT-TYPE                VALUE 'S' 'A' 'C'EL651
00278                                                        'D' 'K'.   EL651
00279              88  ADD-FUNCTION                    VALUE 'A'.       EL651
00280              88  SHOW-FUNCTION                   VALUE 'S'.       EL651
00281              88  DELETE-FUNCTION                 VALUE 'D'.       EL651
00282              88  CHANGE-FUNCTION                 VALUE 'C'.       EL651
00283              88  COPY-FUNCTION                   VALUE 'K'.       EL651
00284                                                                   EL651
00285          16  PI-PREV-MAINTYP         PIC X.                       EL651
00286                                                                   EL651
00287          16  PI-ERREIN-KEY.                                       EL651
00288              20  PI-ERR-COMPANY-CD   PIC X.                       EL651
00289              20  PI-ERR-CODE         PIC X.                       EL651
00290              20  PI-ERR-TABLE        PIC X(3).                    EL651
00291              20  PI-ERR-TABLE-SUB    PIC X(3).                    EL651
00292                                                                   EL651
00293          16  PI-START-LEVEL          PIC 9(2).                       CL**9
00294                                                                      CL**9
00295          16  PI-SAVE-ERREIN-KEY      PIC X(8).                    EL651
00296                                                                   EL651
00297          16  PI-FIRST-TIME-SW        PIC X.                       EL651
00298              88  FIRST-TIME                      VALUE 'Y'.       EL651
00299          16  PI-ENTRY-SW             PIC X.                       EL651
00300              88  NOT-FIRST-ENTRY                 VALUE 'Y'.       EL651
00301          16  PI-BROWSE-SW            PIC X.                       EL651
00302              88  BROWSE-STARTED                  VALUE 'Y'.       EL651
00303          16  PI-ERREIN-EOF-SW        PIC X.                       EL651
00304              88  ERREIN-EOF                      VALUE 'Y'.       EL651
00305          16  PI-EXCESS-SW            PIC X.                       EL651
00306              88  EXCESS-LEVEL-EXISTS             VALUE 'X'.       EL651
00307          16  PI-COMPANY-ADD-SW       PIC X.                       EL651
00308              88  COMPANY-RECORD-ADDED            VALUE 'Y'.       EL651
00309                                                                   EL651
00310          16  PI-SUB                  PIC S99.                     EL651
00311          16  PI-LAST-LEVEL           PIC S99.                     EL651
00312                                                                   EL651
00313          16  PI-SAVE-TABLE           PIC X(3).                    EL651
00314          16  PI-SAVE-COMPANY         PIC X(3).                    EL651
00315          16  PI-SAVE-COMP-SUB        PIC X(3).                    EL651
00316          16  PI-MAPNAME              PIC X(8).                    EL651
00317          16  FILLER                  PIC X(593).                     CL*17
00318                                                                   EL651
00319      EJECT                                                        EL651
00320                                      COPY ELCJPFX.                   CL*12
00321                                      PIC X(4000).                 EL651
00322                                                                   EL651
00323      EJECT                                                        EL651
00324                                      COPY ELCAID.                    CL*12
00325  01  FILLER    REDEFINES DFHAID.                                  EL651
00326      05  FILLER                      PIC X(8).                    EL651
00327      05  PF-VALUES                   PIC X       OCCURS 2.        EL651
00328                                                                   EL651
00329      EJECT                                                        EL651
00330                                      COPY EL651S.                    CL*12
00331                                                                   EL651
00332      EJECT                                                        EL651
00333                                                                   EL651
00334  LINKAGE SECTION.                                                 EL651
00335                                                                   EL651
00336  01  DFHCOMMAREA                     PIC X(1024).                 EL651
00337                                                                   EL651
00338 *01 PARMLIST .                                                       CL*16
00339 *    02  FILLER                      PIC S9(8)   COMP.               CL*16
00340 *    02  ERREIN-POINTER              PIC S9(8)   COMP.               CL*16
00341 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.               CL*16
00342      EJECT                                                        EL651
00343                                      COPY ERCREIN.                   CL*12
00344      EJECT                                                        EL651
00345                                      COPY ELCCNTL.                   CL*12
00346      EJECT                                                        EL651
00347                                                                   EL651
00348  PROCEDURE DIVISION.                                              EL651
00349      CONTINUE.                                                       CL*16
00350                                                                   EL651
00351      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL651
00352      MOVE '5'                    TO  DC-OPTION-CODE.              EL651
00353      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL651
00354      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL651
00355      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL651
00356                                                                   EL651
00357      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL651
00358                                                                   EL651
00359  1000-START.                                                      EL651
00360      IF EIBCALEN = 0                                              EL651
00361          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL651
00362                                                                   EL651
00363      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL651
00364          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL651
00365              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL651
00366              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL651
00367              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL651
00368              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL651
00369              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL651
00370              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL651
00371              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL651
00372              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     EL651
00373          ELSE                                                     EL651
00374              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL651
00375              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL651
00376              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL651
00377              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL651
00378              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL651
00379              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL651
00380              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL651
00381              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.    EL651
00382                                                                   EL651
00383      EXEC CICS HANDLE CONDITION                                   EL651
00384          NOTOPEN  (9990-ABEND)                                    EL651
00385          NOTFND   (8880-NOT-FOUND)                                EL651
00386          PGMIDERR (9600-PGMID-ERROR)                              EL651
00387          ERROR    (9990-ABEND)                                    EL651
00388      END-EXEC.                                                    EL651
00389                                                                      CL**9
00390      IF EIBTRNID = EL6592-TRANS-ID                                   CL**9
00391          MOVE 'S'                TO  MAINTYPI                        CL**9
00392                                      PI-CHECK-MAINT-TYPE             CL**9
00393          MOVE PI-ERR-TABLE       TO  TABLEI                          CL**9
00394          MOVE PI-START-LEVEL     TO  LEVELI                          CL**9
00395          MOVE +1                 TO  MAINTYPL                        CL**9
00396          MOVE +3                 TO  TABLEL                          CL**9
00397          MOVE +2                 TO  LEVELL                          CL**9
00398          GO TO 4000-EDIT-MAINT.                                      CL**9
00399                                                                   EL651
00400      IF EIBTRNID NOT = TRANS-ID                                   EL651
00401          IF NOT-FIRST-ENTRY                                       EL651
00402              MOVE +1             TO  PI-SUB                       EL651
00403              MOVE ZEROS          TO  PI-LAST-LEVEL                EL651
00404                                      MISC-SAVE-AREAS              EL651
00405              GO TO 5000-BUILD-INITIAL-SCREEN                      EL651
00406          ELSE                                                     EL651
00407              MOVE LOW-VALUES     TO  EL651AI                      EL651
00408                                      PI-PROGRAM-WORK-AREA         EL651
00409              MOVE 'Y'            TO  PI-ENTRY-SW                  EL651
00410                                      PI-FIRST-TIME-SW             EL651
00411              MOVE ZEROS          TO  PI-LAST-LEVEL                EL651
00412              MOVE +1             TO  PI-SUB                       EL651
00413              GO TO 8100-SEND-INITIAL-MAP.                         EL651
00414                                                                   EL651
00415                                                                   EL651
00416      IF EIBAID = DFHCLEAR                                         EL651
00417          GO TO 9400-CLEAR.                                        EL651
00418                                                                   EL651
00419      IF PI-PROCESSOR-ID = 'LGXX'                                  EL651
00420          GO TO 2000-RECEIVE.                                      EL651
00421                                                                   EL651
00422      EXEC CICS READQ TS                                           EL651
00423          QUEUE  (PI-SECURITY-TEMP-STORE-ID)                       EL651
00424          INTO   (SECURITY-CONTROL)                                EL651
00425          LENGTH (SC-COMM-LENGTH)                                  EL651
00426          ITEM   (SC-ITEM)                                         EL651
00427      END-EXEC.                                                    EL651
00428                                                                   EL651
00429      MOVE SC-CREDIT-DISPLAY (07)  TO PI-DISPLAY-CAP.              EL651
00430      MOVE SC-CREDIT-UPDATE  (07)  TO PI-MODIFY-CAP.               EL651
00431                                                                   EL651
00432      IF NOT DISPLAY-CAP                                           EL651
00433          MOVE 'READ'          TO SM-READ                          EL651
00434          PERFORM 9995-SECURITY-VIOLATION                          EL651
00435          MOVE ER-0070        TO  EMI-ERROR                        EL651
00436          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
00437          GO TO 8100-SEND-INITIAL-MAP.                             EL651
00438                                                                   EL651
00439      EJECT                                                        EL651
00440  2000-RECEIVE.                                                    EL651
00441      MOVE LOW-VALUES             TO  EL651AI.                     EL651
00442                                                                   EL651
00443      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL651
00444          MOVE ER-0008            TO  EMI-ERROR                    EL651
00445          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
00446          MOVE -1                 TO  MAINTYPL                     EL651
00447          GO TO 8200-SEND-DATAONLY.                                EL651
00448                                                                   EL651
00449      EXEC CICS RECEIVE                                            EL651
00450          MAP    (MAP-NAME)                                        EL651
00451          MAPSET (MAPSET-NAME)                                     EL651
00452          INTO   (EL651AI)                                         EL651
00453      END-EXEC.                                                    EL651
00454                                                                   EL651
00455      IF PFENTERL = 0                                              EL651
00456          GO TO 3000-CHECK-PFENTERS.                               EL651
00457                                                                   EL651
00458      IF EIBAID NOT = DFHENTER                                     EL651
00459          MOVE ER-0004            TO  EMI-ERROR                    EL651
00460          GO TO 3100-INPUT-ERROR.                                  EL651
00461                                                                   EL651
00462      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)      CL**5
00463          MOVE PF-VALUES (PFENTERI)   TO EIBAID                    EL651
00464      ELSE                                                         EL651
00465          MOVE ER-0029                TO EMI-ERROR                 EL651
00466          GO TO 3100-INPUT-ERROR.                                  EL651
00467                                                                   EL651
00468      EJECT                                                        EL651
00469  3000-CHECK-PFENTERS.                                             EL651
00470      IF EIBAID = DFHPF23                                          EL651
00471          GO TO 8810-PF23.                                         EL651
00472                                                                   EL651
00473      IF EIBAID = DFHPF24                                          EL651
00474          GO TO 9200-RETURN-MAIN-MENU.                             EL651
00475                                                                   EL651
00476      IF EIBAID = DFHPF12                                          EL651
00477          GO TO 9500-PF12.                                         EL651
00478                                                                   EL651
00479      IF MAINTYPL GREATER ZERO                                     EL651
00480          IF MAINTYPI NOT = SPACE                                  EL651
00481              IF EIBAID = DFHENTER OR DFHPF10                      EL651
00482                 NEXT SENTENCE                                     EL651
00483                ELSE                                               EL651
00484                 MOVE ER-0050    TO  EMI-ERROR                     EL651
00485                 GO TO 3100-INPUT-ERROR.                           EL651
00486                                                                   EL651
00487      IF EIBAID = DFHPF1                                           EL651
00488          GO TO 7100-PAGE-TABLE-FORWARD.                           EL651
00489                                                                   EL651
00490      IF EIBAID = DFHPF2                                           EL651
00491          GO TO 7200-PAGE-TABLE-BACKWARD.                          EL651
00492                                                                   EL651
00493      IF EIBAID = DFHPF3                                           EL651
00494          MOVE 'F'                TO  WS-PAGE-LEVEL-SW             EL651
00495          GO TO 7300-PAGE-LEVEL.                                   EL651
00496                                                                   EL651
00497      IF EIBAID = DFHPF4                                           EL651
00498          MOVE 'B'                TO  WS-PAGE-LEVEL-SW             EL651
00499          GO TO 7300-PAGE-LEVEL.                                   EL651
00500                                                                   EL651
00501      IF EIBAID = DFHPF7                                           EL651
00502          MOVE PI-ERR-TABLE       TO  PI-SAVE-TABLE                EL651
00503          MOVE PI-SAVE-COMPANY    TO  PI-ERR-TABLE                 EL651
00504          MOVE PI-SAVE-COMP-SUB   TO  PI-ERR-TABLE-SUB             EL651
00505          MOVE 'EL6511B'          TO  PI-MAPNAME                   EL651
00506          MOVE XCTL-6511          TO  PGM-NAME                     EL651
00507          GO TO 9300-XCTL.                                         EL651
00508                                                                   EL651
00509      IF EIBAID = DFHPF8                                           EL651
00510          MOVE XCTL-657           TO  PGM-NAME                     EL651
00511          GO TO 9300-XCTL.                                         EL651
00512                                                                   EL651
00513      IF EIBAID = DFHPF9                                           EL651
00514          MOVE PI-ERR-TABLE       TO  PI-SAVE-TABLE                EL651
00515          MOVE PI-SAVE-COMPANY    TO  PI-ERR-TABLE                 EL651
00516          MOVE PI-SAVE-COMP-SUB   TO  PI-ERR-TABLE-SUB             EL651
00517          MOVE XCTL-6511          TO  PGM-NAME                     EL651
00518          MOVE 'EL6511C'          TO  PI-MAPNAME                   EL651
00519          GO TO 9300-XCTL.                                         EL651
00520                                                                   EL651
00521      IF EIBAID = DFHENTER OR EIBAID = DFHPF10                     EL651
00522          GO TO 4000-EDIT-MAINT.                                   EL651
00523                                                                   EL651
00524      MOVE ER-0029                TO  EMI-ERROR.                   EL651
00525                                                                   EL651
00526  3100-INPUT-ERROR.                                                EL651
00527      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
00528      MOVE AL-UNBON               TO  PFENTERA.                    EL651
00529      MOVE -1                     TO  PFENTERL.                    EL651
00530      GO TO 8200-SEND-DATAONLY.                                    EL651
00531                                                                   EL651
00532      EJECT                                                        EL651
00533  4000-EDIT-MAINT.                                                 EL651
00534      IF MAINTYPL GREATER ZERO                                     EL651
00535          MOVE MAINTYPI           TO  PI-CHECK-MAINT-TYPE          EL651
00536          IF VALID-MAINT-TYPE                                      EL651
00537              MOVE AL-UANON       TO  MAINTYPA                     EL651
00538          ELSE                                                     EL651
00539              MOVE -1             TO  MAINTYPL                     EL651
00540              MOVE AL-UABON       TO  MAINTYPA                     EL651
00541              MOVE ER-0023        TO  EMI-ERROR                    EL651
00542              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
00543      ELSE                                                         EL651
00544          MOVE -1                 TO  MAINTYPL                     EL651
00545          MOVE AL-UABON           TO  MAINTYPA                     EL651
00546          MOVE ER-0023            TO  EMI-ERROR                    EL651
00547          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL651
00548                                                                   EL651
00549      IF NOT MODIFY-CAP                                            EL651
00550          IF NOT SHOW-FUNCTION                                     EL651
00551              MOVE 'UPDATE'       TO SM-READ                       EL651
00552              PERFORM 9995-SECURITY-VIOLATION                      EL651
00553              MOVE ER-0070        TO  EMI-ERROR                    EL651
00554              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL651
00555                                                                   EL651
00556      IF TABLEL GREATER ZERO                                       EL651
00557          MOVE ZERO TO TALLY                                          CL*16
00558          INSPECT TABLEI TALLYING TALLY FOR ALL SPACES                CL*16
00559          IF TALLY GREATER ZERO                                    EL651
00560              MOVE -1             TO  TABLEL                       EL651
00561              MOVE AL-UABON       TO  TABLEA                       EL651
00562              MOVE ER-2341        TO  EMI-ERROR                    EL651
00563              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
00564          ELSE                                                     EL651
00565              MOVE AL-UANON       TO  TABLEA                       EL651
00566              MOVE TABLEI         TO  PI-ERR-TABLE                 EL651
00567              MOVE LOW-VALUES     TO  PI-ERR-TABLE-SUB             EL651
00568      ELSE                                                         EL651
00569          MOVE -1                 TO  TABLEL                       EL651
00570          MOVE AL-UABON           TO  TABLEA                       EL651
00571          MOVE ER-2140            TO  EMI-ERROR                    EL651
00572          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL651
00573                                                                   EL651
00574      IF LEVELL GREATER ZERO                                       EL651
00575          IF LEVELI NOT NUMERIC                                       CL**6
00576              MOVE -1                     TO  LEVELL                  CL**6
00577              MOVE AL-UNBON               TO  LEVELA                  CL**6
00578              MOVE ER-2310                TO  EMI-ERROR               CL**6
00579              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**6
00580          ELSE                                                     EL651
00581              MOVE LEVELI                 TO  WS-CHECK-LEVEL          CL**6
00582              IF VALID-LEVEL                                          CL**6
00583                  IF CHANGE-FUNCTION                                  CL**6
00584                      IF LEVELI = PI-SUB                              CL**6
00585                          MOVE AL-UNNON   TO  LEVELA                  CL**6
00586                      ELSE                                            CL**6
00587                          MOVE -1         TO  MAINTYPL                CL**6
00588                          MOVE ER-2056    TO  EMI-ERROR               CL**6
00589                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    CL**6
00590                          GO TO 8200-SEND-DATAONLY                    CL**6
00591                  ELSE                                                CL**6
00592                      MOVE WS-CHECK-LEVEL TO  PI-SUB                  CL**6
00593                      MOVE AL-UNNON       TO  LEVELA                  CL**6
00594              ELSE                                                    CL**6
00595                  MOVE -1                 TO  LEVELL                  CL**6
00596                  MOVE AL-UNBON           TO  LEVELA                  CL**6
00597                  MOVE ER-2310            TO  EMI-ERROR               CL**6
00598                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL**6
00599                                                                   EL651
00600      IF TOTABLEL GREATER ZERO                                     EL651
00601          MOVE ZERO TO TALLY                                          CL*16
00602          INSPECT TOTABLEI TALLYING TALLY FOR ALL SPACES              CL*16
00603          IF TALLY GREATER ZERO                                    EL651
00604              MOVE -1             TO  TOTABLEL                     EL651
00605              MOVE AL-UABON       TO  TOTABLEA                     EL651
00606              MOVE ER-2342        TO  EMI-ERROR                    EL651
00607              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
00608          ELSE                                                     EL651
00609              MOVE AL-UANON       TO  TOTABLEA                     EL651
00610      ELSE                                                         EL651
00611          IF COPY-FUNCTION                                         EL651
00612              MOVE -1             TO  TOTABLEL                     EL651
00613              MOVE AL-UABON       TO  TOTABLEA                     EL651
00614              MOVE ER-2343        TO  EMI-ERROR                    EL651
00615              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*12
00616                                                                      CL*12
00617      IF ADD-FUNCTION OR COPY-FUNCTION OR CHANGE-FUNCTION             CL*12
00618          IF (COMPANYI EQUAL ZEROS) AND                               CL*12
00619                      (COMPSUBI EQUAL ZEROS)                          CL*12
00620              MOVE -1             TO  COMPANYL                        CL*12
00621              MOVE AL-UABON       TO  COMPANYA                        CL*12
00622                                      COMPSUBA                        CL*12
00623              MOVE ER-7805        TO  EMI-ERROR                       CL*12
00624              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL651
00625                                                                   EL651
00626      IF EMI-NO-ERRORS                                             EL651
00627          NEXT SENTENCE                                            EL651
00628      ELSE                                                         EL651
00629          GO TO 8200-SEND-DATAONLY.                                EL651
00630                                                                   EL651
00631      IF ADD-FUNCTION                                              EL651
00632          GO TO 4200-ADD.                                          EL651
00633                                                                   EL651
00634      IF CHANGE-FUNCTION                                           EL651
00635          GO TO 4400-CHANGE.                                       EL651
00636                                                                   EL651
00637      IF COPY-FUNCTION                                             EL651
00638          GO TO 4500-COPY.                                         EL651
00639                                                                   EL651
00640      IF DELETE-FUNCTION OR EIBAID = DFHPF10                       EL651
00641          GO TO 4600-DELETE-LEVEL.                                 EL651
00642                                                                   EL651
00643      IF SHOW-FUNCTION                                             EL651
00644          MOVE 'Y'                TO  WS-CHANGE-SW                 EL651
00645          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL651
00646                                                                   EL651
00647      MOVE -1                     TO  MAINTYPL.                    EL651
00648      MOVE ER-2056                TO  EMI-ERROR.                   EL651
00649      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
00650      GO TO 8200-SEND-DATAONLY.                                    EL651
00651                                                                   EL651
00652  4000-EXIT.                                                       EL651
00653      EXIT.                                                        EL651
00654      EJECT                                                        EL651
00655                                                                   EL651
00656  4200-ADD.                                                        EL651
00657      PERFORM 7000-EDIT THRU 7000-EXIT.                            EL651
00658                                                                   EL651
00659      IF EMI-NO-ERRORS                                             EL651
00660          NEXT SENTENCE                                            EL651
00661      ELSE                                                         EL651
00662          GO TO 8200-SEND-DATAONLY.                                EL651
00663                                                                   EL651
00664      EXEC CICS HANDLE CONDITION                                   EL651
00665          NOTOPEN  (9990-ABEND)                                    EL651
00666          NOTFND   (4250-ADD-REC)                                  EL651
00667          END-EXEC.                                                EL651
00668                                                                   EL651
00669      MOVE 'A'                    TO  PI-ERR-CODE.                 EL651
00670                                                                   EL651
00671      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.                     EL651
00672                                                                   EL651
00673 ******************************************************************EL651
00674 *  CODE ADDED TO INSURE IF USER HAS CARRIER SECURITY THAT THEY   *EL651
00675 *  ARE ONLY ALLOWED TO APPLY MAINTENANCE TO THOSE REIN. RECORDS  *EL651
00676 *  WITH A MATCHING CARRIER IN THE RE-TABLE-CARRIER-SECURITY FIELD*EL651
00677 ******************************************************************EL651
00678                                                                   EL651
00679      IF PI-CARRIER-SECURITY GREATER SPACE                         EL651
00680          IF PI-CARRIER-SECURITY = RE-TABLE-CARRIER-SECURITY       EL651
00681              NEXT SENTENCE                                        EL651
00682          ELSE                                                     EL651
00683              MOVE -1             TO  MAINTYPL                     EL651
00684              MOVE ER-2370        TO  EMI-ERROR                    EL651
00685              MOVE AL-UANON       TO  MAINTYPA                     EL651
00686              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
00687              GO TO 8200-SEND-DATAONLY.                            EL651
00688                                                                   EL651
00689      GO TO 4300-ADD-LEVEL.                                        EL651
00690                                                                   EL651
00691  4250-ADD-REC.                                                    EL651
00692      IF WS-GETMAIN-SW = '1'                                       EL651
00693          MOVE ' '                TO  WS-GETMAIN-SW                EL651
00694          MOVE SPACES             TO  REINSURANCE-RECORD           EL651
00695      ELSE                                                         EL651
00696          PERFORM 7700-ERREIN-GETMAIN THRU 7700-EXIT.              EL651
00697                                                                   EL651
00698      MOVE ZEROS                  TO  RE-100-COMP                  EL651
00699                                      RE-LAST-MAINT-HHMMSS.        EL651
00700      MOVE LOW-VALUES             TO  RE-KEY.                      EL651
00701      MOVE PI-ERR-CODE            TO  RE-CODE.                     EL651
00702      MOVE PI-ERR-TABLE           TO  RE-TABLE.                    EL651
00703                                                                   EL651
00704      MOVE +1                     TO  PI-SUB.                      EL651
00705                                                                   EL651
00706  4255-ZERO-RECORD.                                                EL651
00707      MOVE ZEROS                  TO  RE-LFAGE-LO (PI-SUB)         EL651
00708                                      RE-AHAGE-LO (PI-SUB)         EL651
00709                                      RE-LFTRM-LO (PI-SUB)         EL651
00710                                      RE-AHTRM-LO (PI-SUB)         EL651
00711                                      RE-LF-PCT (PI-SUB)           EL651
00712                                      RE-AH-PCT (PI-SUB)           EL651
00713                                      RE-LF-LIM-LO (PI-SUB)        EL651
00714                                      RE-LF-LO (PI-SUB)            EL651
00715                                      RE-AHBEN-LIM-LO (PI-SUB)     EL651
00716                                      RE-AHBEN-LO (PI-SUB)         EL651
00717                                      RE-AHMOA-LIM-LO (PI-SUB)     EL651
00718                                      RE-AHMOA-LO (PI-SUB)         EL651
00719                                      RE-LO-DATE (PI-SUB).         EL651
00720                                                                   EL651
00721       MOVE ALL '9'               TO  RE-LFAGE-HI (PI-SUB)         EL651
00722                                      RE-AHAGE-HI (PI-SUB)         EL651
00723                                      RE-LFTRM-HI (PI-SUB)         EL651
00724                                      RE-AHTRM-HI (PI-SUB)         EL651
00725                                      RE-HI-DATE (PI-SUB).         EL651
00726                                                                   EL651
00727      MOVE NINES                  TO  RE-LF-LIM-HI (PI-SUB)        EL651
00728                                      RE-LF-HI (PI-SUB)            EL651
00729                                      RE-AHBEN-LIM-HI (PI-SUB)     EL651
00730                                      RE-AHBEN-HI (PI-SUB)         EL651
00731                                      RE-AHMOA-LIM-HI (PI-SUB)     EL651
00732                                      RE-AHMOA-HI (PI-SUB).        EL651
00733      ADD +1                      TO  PI-SUB.                      EL651
00734                                                                   EL651
00735      IF PI-SUB GREATER +30                                        EL651
00736          MOVE +1                 TO  PI-SUB                       EL651
00737      ELSE                                                         EL651
00738          GO TO 4255-ZERO-RECORD.                                  EL651
00739                                                                   EL651
00740      PERFORM 6000-CHECK-FOR-UPDATE THRU 6099-EXIT.                EL651
00741                                                                   EL651
00742      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.          EL651
00743      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.        EL651
00744      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL651
00745                                                                   EL651
00746      MOVE '5'                    TO  DC-OPTION-CODE.              EL651
00747      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL651
00748                                                                   EL651
00749      EXEC CICS LINK                                               EL651
00750          PROGRAM   (PGM-NAME)                                     EL651
00751          COMMAREA  (DATE-CONVERSION-DATA)                         EL651
00752          LENGTH    (DC-COMM-LENGTH)                               EL651
00753      END-EXEC.                                                    EL651
00754                                                                   EL651
00755      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT             EL651
00756                                      BIN-CURRENT-SAVE.            EL651
00757      MOVE PI-COMPANY-CD          TO  RE-COMPANY-CD.               EL651
00758      MOVE 'RE'                   TO  RE-RECORD-ID.                EL651
00759 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL651
00760 *    MOVE 'A'                    TO  JP-RECORD-TYPE               EL651
00761 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH                  EL651
00762 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL651
00763                                                                   EL651
00764      EXEC CICS WRITE                                              EL651
00765          DATASET (REIN-FILE-ID)                                   EL651
00766          FROM    (REINSURANCE-RECORD)                             EL651
00767          RIDFLD  (RE-CONTROL-PRIMARY)                             EL651
00768      END-EXEC.                                                    EL651
00769                                                                   EL651
00770 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
00771      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL651
00772                                                                   EL651
00773      IF COMPANY-RECORD-ADDED                                      EL651
00774          MOVE SPACE              TO  PI-COMPANY-ADD-SW            EL651
00775          MOVE ER-2345            TO  EMI-ERROR                    EL651
00776      ELSE                                                         EL651
00777          MOVE ER-0000            TO  EMI-ERROR.                   EL651
00778                                                                   EL651
00779      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
00780                                                                   EL651
00781      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL651
00782                                                                   EL651
00783  4200-EXIT.                                                       EL651
00784      EXIT.                                                        EL651
00785      EJECT                                                        EL651
00786                                                                   EL651
00787  4300-ADD-LEVEL.                                                  EL651
00788      IF PI-LAST-LEVEL = +30                                       EL651
00789          MOVE -1                 TO  MAINTYPL                     EL651
00790          MOVE ER-2344            TO  EMI-ERROR                    EL651
00791          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
00792          GO TO 8200-SEND-DATAONLY.                                EL651
00793                                                                   EL651
00794      PERFORM 7750-READ-ERREIN-UPDATE THRU 7750-EXIT.              EL651
00795                                                                   EL651
00796 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL651
00797                                                                   EL651
00798      IF EXCESS-LEVEL-EXISTS                                       EL651
00799          MOVE RE-COMP-INFO (PI-LAST-LEVEL) TO WS-SAVE-REC         EL651
00800          MOVE PI-SUB             TO  WS-SAVE-PI-SUB               EL651
00801          MOVE PI-LAST-LEVEL      TO  PI-SUB                       EL651
00802          PERFORM 6000-CHECK-FOR-UPDATE THRU 6099-EXIT             EL651
00803          MOVE WS-SAVE-PI-SUB     TO  PI-SUB                       EL651
00804          ADD +1                  TO  PI-LAST-LEVEL                EL651
00805                                      RE-100-COMP                  EL651
00806          MOVE WS-SAVE-REC        TO  RE-COMP-INFO (PI-LAST-LEVEL) EL651
00807      ELSE                                                         EL651
00808          ADD +1                  TO  PI-LAST-LEVEL                EL651
00809          MOVE PI-SUB             TO  WS-SAVE-PI-SUB               EL651
00810          MOVE PI-LAST-LEVEL      TO  PI-SUB                       EL651
00811          PERFORM 6000-CHECK-FOR-UPDATE THRU 6099-EXIT             EL651
00812          MOVE WS-SAVE-PI-SUB     TO  PI-SUB.                      EL651
00813                                                                   EL651
00814      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR                      EL651
00815         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL651
00816          NEXT SENTENCE                                            EL651
00817      ELSE                                                         EL651
00818          EXEC CICS UNLOCK                                         EL651
00819               DATASET  (REIN-FILE-ID)                             EL651
00820          END-EXEC                                                 EL651
00821          MOVE ER-0068            TO  EMI-ERROR                    EL651
00822          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
00823          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL651
00824                                                                   EL651
00825      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.          EL651
00826      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.        EL651
00827      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL651
00828      MOVE '5'                    TO  DC-OPTION-CODE.              EL651
00829      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL651
00830                                                                   EL651
00831      EXEC CICS LINK                                               EL651
00832          PROGRAM   (PGM-NAME)                                     EL651
00833          COMMAREA  (DATE-CONVERSION-DATA)                         EL651
00834          LENGTH    (DC-COMM-LENGTH)                               EL651
00835      END-EXEC.                                                    EL651
00836                                                                   EL651
00837      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT             EL651
00838                                      BIN-CURRENT-SAVE.            EL651
00839 *    MOVE 'B'                    TO  JP-RECORD-TYPE               EL651
00840 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL651
00841 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH                  EL651
00842 *    PERFORM 8400-LOG-JOURNAL-RECORD                              EL651
00843 *    MOVE REINSURANCE-RECORD     TO JP-RECORD-AREA.               EL651
00844                                                                   EL651
00845      EXEC CICS REWRITE                                            EL651
00846          DATASET  (REIN-FILE-ID)                                  EL651
00847          FROM     (REINSURANCE-RECORD)                            EL651
00848      END-EXEC.                                                    EL651
00849                                                                   EL651
00850 *    MOVE 'C'                    TO  JP-RECORD-TYPE               EL651
00851 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH                  EL651
00852 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
00853      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL651
00854      MOVE ER-0000                TO  EMI-ERROR.                   EL651
00855      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
00856                                                                   EL651
00857      MOVE 'Y'                    TO  WS-CHANGE-SW.                EL651
00858                                                                   EL651
00859      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL651
00860                                                                   EL651
00861  4300-EXIT.                                                       EL651
00862      EXIT.                                                        EL651
00863      EJECT                                                        EL651
00864  4400-CHANGE.                                                     EL651
00865      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY                        EL651
00866          NEXT SENTENCE                                            EL651
00867      ELSE                                                         EL651
00868          MOVE ER-2056            TO  EMI-ERROR                    EL651
00869          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
00870          MOVE -1                 TO  MAINTYPL                     EL651
00871          GO TO 8200-SEND-DATAONLY.                                EL651
00872                                                                   EL651
00873      PERFORM 7000-EDIT               THRU 7000-EXIT.              EL651
00874                                                                   EL651
00875      IF EMI-NO-ERRORS                                             EL651
00876          NEXT SENTENCE                                            EL651
00877      ELSE                                                         EL651
00878          GO TO 8200-SEND-DATAONLY.                                EL651
00879                                                                   EL651
00880      PERFORM 7750-READ-ERREIN-UPDATE THRU 7750-EXIT.              EL651
00881                                                                   EL651
00882 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL651
00883                                                                   EL651
00884      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6099-EXIT.              EL651
00885                                                                   EL651
00886      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR                      EL651
00887         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL651
00888          NEXT SENTENCE                                            EL651
00889      ELSE                                                         EL651
00890          EXEC CICS UNLOCK                                         EL651
00891               DATASET  (REIN-FILE-ID)                             EL651
00892          END-EXEC                                                 EL651
00893          MOVE ER-0068            TO  EMI-ERROR                    EL651
00894          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
00895          GO TO 8200-SEND-DATAONLY.                                EL651
00896                                                                   EL651
00897      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.          EL651
00898      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.        EL651
00899      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL651
00900      MOVE '5'                    TO  DC-OPTION-CODE.              EL651
00901      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL651
00902                                                                   EL651
00903      EXEC CICS LINK                                               EL651
00904          PROGRAM   (PGM-NAME)                                     EL651
00905          COMMAREA  (DATE-CONVERSION-DATA)                         EL651
00906          LENGTH    (DC-COMM-LENGTH)                               EL651
00907      END-EXEC.                                                    EL651
00908                                                                   EL651
00909      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT             EL651
00910                                      BIN-CURRENT-SAVE.            EL651
00911 *    MOVE 'B'                    TO  JP-RECORD-TYPE.              EL651
00912 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL651
00913 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH.                 EL651
00914 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
00915 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL651
00916                                                                   EL651
00917      EXEC CICS REWRITE                                            EL651
00918          DATASET  (REIN-FILE-ID)                                  EL651
00919          FROM     (REINSURANCE-RECORD)                            EL651
00920      END-EXEC.                                                    EL651
00921                                                                   EL651
00922 *    MOVE 'C'                    TO  JP-RECORD-TYPE               EL651
00923 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH                  EL651
00924 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
00925      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL651
00926      MOVE ER-0000                TO  EMI-ERROR.                   EL651
00927      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
00928                                                                   EL651
00929      MOVE 'Y'                    TO  WS-CHANGE-SW.                EL651
00930                                                                   EL651
00931      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL651
00932                                                                   EL651
00933  4400-EXIT.                                                       EL651
00934      EXIT.                                                        EL651
00935      EJECT                                                        EL651
00936                                                                   EL651
00937  4500-COPY.                                                       EL651
00938      MOVE LOW-VALUES             TO  PI-ERREIN-KEY.               EL651
00939      MOVE 'A'                    TO  PI-ERR-CODE.                 EL651
00940      MOVE TOTABLEI               TO  PI-ERR-TABLE.                EL651
00941                                                                   EL651
00942      EXEC CICS HANDLE CONDITION                                   EL651
00943          NOTOPEN  (9990-ABEND)                                    EL651
00944          NOTFND   (4550-COPY-REC)                                 EL651
00945      END-EXEC.                                                    EL651
00946                                                                   EL651
00947      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.                     EL651
00948                                                                   EL651
00949      MOVE -1                     TO  TOTABLEL.                    EL651
00950      MOVE AL-UABON               TO  TOTABLEA.                    EL651
00951      MOVE ER-2139                TO  EMI-ERROR.                   EL651
00952      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
00953                                                                   EL651
00954      GO TO 8200-SEND-DATAONLY.                                    EL651
00955                                                                   EL651
00956  4550-COPY-REC.                                                   EL651
00957      EXEC CICS HANDLE CONDITION                                   EL651
00958          NOTFND (8880-NOT-FOUND)                                  EL651
00959      END-EXEC.                                                    EL651
00960                                                                   EL651
00961      MOVE TABLEI                 TO  PI-ERR-TABLE.                EL651
00962                                                                   EL651
00963      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.                     EL651
00964                                                                   EL651
00965      MOVE TOTABLEI               TO  PI-ERR-TABLE                 EL651
00966                                      RE-TABLE.                    EL651
00967                                                                   EL651
00968      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.          EL651
00969      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.        EL651
00970      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL651
00971                                                                   EL651
00972      MOVE '5'                    TO  DC-OPTION-CODE.              EL651
00973      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL651
00974                                                                   EL651
00975      EXEC CICS LINK                                               EL651
00976          PROGRAM   (PGM-NAME)                                     EL651
00977          COMMAREA  (DATE-CONVERSION-DATA)                         EL651
00978          LENGTH    (DC-COMM-LENGTH)                               EL651
00979      END-EXEC.                                                    EL651
00980                                                                   EL651
00981      MOVE DC-BIN-DATE-1           TO  RE-LAST-MAINT-DT            EL651
00982                                       BIN-CURRENT-SAVE.           EL651
00983      MOVE PI-COMPANY-CD           TO  RE-COMPANY-CD.              EL651
00984 *    MOVE 'RE'                    TO  RE-RECORD-ID.               EL651
00985 *    MOVE REIN-FILE-ID            TO  FILE-ID.                    EL651
00986 *    MOVE 'A'                     TO  JP-RECORD-TYPE.             EL651
00987 *    MOVE ERREIN-LENGTH           TO  FILE-LENGTH.                EL651
00988 *    MOVE REINSURANCE-RECORD      TO  JP-RECORD-AREA.             EL651
00989                                                                   EL651
00990      EXEC CICS WRITE                                              EL651
00991          DATASET (REIN-FILE-ID)                                   EL651
00992          FROM    (REINSURANCE-RECORD)                             EL651
00993          RIDFLD  (RE-CONTROL-PRIMARY)                             EL651
00994      END-EXEC.                                                    EL651
00995                                                                   EL651
00996 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
00997      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL651
00998      MOVE ER-0000                TO  EMI-ERROR.                   EL651
00999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
01000                                                                   EL651
01001      MOVE LOW-VALUES             TO  EL651AO.                     EL651
01002                                                                   EL651
01003      MOVE PI-ERR-TABLE           TO  TABLEO.                      EL651
01004      MOVE AL-UANON               TO  TABLEA.                      EL651
01005                                                                   EL651
01006      GO TO 8100-SEND-INITIAL-MAP.                                 EL651
01007                                                                   EL651
01008  4500-EXIT.                                                       EL651
01009      EXIT.                                                        EL651
01010      EJECT                                                        EL651
01011                                                                   EL651
01012  4600-DELETE-LEVEL.                                               EL651
01013      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY                        EL651
01014          NEXT SENTENCE                                            EL651
01015      ELSE                                                         EL651
01016          MOVE ER-2056            TO  EMI-ERROR                    EL651
01017          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
01018          MOVE -1                 TO  MAINTYPL                     EL651
01019          GO TO 8200-SEND-DATAONLY.                                EL651
01020                                                                   EL651
01021      IF (PI-SUB  =  +1 AND PI-LAST-LEVEL = +1) OR                 EL651
01022          EIBAID = DFHPF10                                         EL651
01023              GO TO 4800-DELETE.                                   EL651
01024                                                                   EL651
01025      PERFORM 7750-READ-ERREIN-UPDATE THRU 7750-EXIT.              EL651
01026                                                                   EL651
01027      MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL651
01028                                                                   EL651
01029  4625-ZERO-RECORD.                                                EL651
01030      IF RE-REI-COMP (PI-SUB) NOT = SPACES                            CL**7
01031          IF PI-SUB NOT = +30                                         CL**7
01032              MOVE PI-SUB         TO  SUB1                         EL651
01033              ADD +1              TO  SUB1                         EL651
01034              MOVE RE-COMP-INFO (SUB1)                             EL651
01035                                  TO  RE-COMP-INFO (PI-SUB)        EL651
01036              ADD +1              TO  PI-SUB                       EL651
01037              GO TO 4625-ZERO-RECORD.                              EL651
01038                                                                   EL651
01039      IF RE-REI-COMP (PI-SUB) = SPACES                             EL651
01040          GO TO 4650-CONT.                                         EL651
01041                                                                   EL651
01042      MOVE SPACES                 TO  RE-COMP-INFO (30).           EL651
01043      MOVE ZEROS                  TO  RE-LFAGE-LO (30)             EL651
01044                                      RE-AHAGE-LO (30)             EL651
01045                                      RE-LFTRM-LO (30)             EL651
01046                                      RE-AHTRM-LO (30)             EL651
01047                                      RE-LF-PCT (30)               EL651
01048                                      RE-AH-PCT (30)               EL651
01049                                      RE-LF-LIM-LO (30)            EL651
01050                                      RE-LF-LO (30)                EL651
01051                                      RE-AHBEN-LIM-LO (30)         EL651
01052                                      RE-AHBEN-LO (30)             EL651
01053                                      RE-AHMOA-LIM-LO (30)         EL651
01054                                      RE-AHMOA-LO (30)             EL651
01055                                      RE-LO-DATE (30).             EL651
01056                                                                   EL651
01057       MOVE ALL '9'               TO  RE-LFAGE-HI (30)             EL651
01058                                      RE-AHAGE-HI (30)             EL651
01059                                      RE-LFTRM-HI (30)             EL651
01060                                      RE-AHTRM-HI (30)             EL651
01061                                      RE-HI-DATE (30).             EL651
01062                                                                   EL651
01063      MOVE NINES                  TO  RE-LF-LIM-HI (30)            EL651
01064                                      RE-LF-HI (30)                EL651
01065                                      RE-AHBEN-LIM-HI (30)         EL651
01066                                      RE-AHBEN-HI (30)             EL651
01067                                      RE-AHMOA-LIM-HI (30)         EL651
01068                                      RE-AHMOA-HI (30).            EL651
01069                                                                   EL651
01070  4650-CONT.                                                       EL651
01071      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.          EL651
01072      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.        EL651
01073      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL651
01074      MOVE '5'                    TO  DC-OPTION-CODE.              EL651
01075      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL651
01076                                                                   EL651
01077      EXEC CICS LINK                                               EL651
01078          PROGRAM   (PGM-NAME)                                     EL651
01079          COMMAREA  (DATE-CONVERSION-DATA)                         EL651
01080          LENGTH    (DC-COMM-LENGTH)                               EL651
01081      END-EXEC.                                                    EL651
01082                                                                   EL651
01083      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT             EL651
01084                                      BIN-CURRENT-SAVE.            EL651
01085 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL651
01086 *    MOVE 'B'                    TO  JP-RECORD-TYPE.              EL651
01087 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH.                 EL651
01088 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
01089 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL651
01090                                                                   EL651
01091      EXEC CICS REWRITE                                            EL651
01092          DATASET  (REIN-FILE-ID)                                  EL651
01093          FROM     (REINSURANCE-RECORD)                            EL651
01094      END-EXEC.                                                    EL651
01095                                                                   EL651
01096 *    MOVE 'C'                    TO  JP-RECORD-TYPE               EL651
01097 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH                  EL651
01098 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
01099      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL651
01100      MOVE ER-0000                TO  EMI-ERROR.                   EL651
01101      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
01102                                                                   EL651
01103      MOVE +1                     TO  PI-SUB.                      EL651
01104                                                                   EL651
01105      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL651
01106                                                                   EL651
01107  4600-EXIT.                                                       EL651
01108      EXIT.                                                        EL651
01109      EJECT                                                        EL651
01110  4800-DELETE.                                                     EL651
01111      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY                        EL651
01112          NEXT SENTENCE                                            EL651
01113      ELSE                                                         EL651
01114          MOVE ER-2056            TO  EMI-ERROR                    EL651
01115          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
01116          MOVE -1                 TO  MAINTYPL                     EL651
01117          GO TO 8200-SEND-DATAONLY.                                EL651
01118                                                                   EL651
01119      PERFORM 7750-READ-ERREIN-UPDATE THRU 7750-EXIT.              EL651
01120                                                                   EL651
01121 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL651
01122                                                                   EL651
01123      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR                      EL651
01124         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL651
01125          NEXT SENTENCE                                            EL651
01126      ELSE                                                         EL651
01127          EXEC CICS UNLOCK                                         EL651
01128               DATASET  (REIN-FILE-ID)                             EL651
01129          END-EXEC                                                 EL651
01130          MOVE ER-0068            TO  EMI-ERROR                    EL651
01131          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
01132          GO TO 8200-SEND-DATAONLY.                                EL651
01133                                                                   EL651
01134      EXEC CICS DELETE                                             EL651
01135          DATASET  (REIN-FILE-ID)                                  EL651
01136      END-EXEC.                                                    EL651
01137                                                                   EL651
01138 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL651
01139 *    MOVE ERREIN-LENGTH          TO  FILE-LENGTH.                 EL651
01140 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
01141                                                                   EL651
01142      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL651
01143      MOVE '5'                    TO  DC-OPTION-CODE.              EL651
01144      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL651
01145                                                                   EL651
01146      EXEC CICS LINK                                               EL651
01147          PROGRAM   (PGM-NAME)                                     EL651
01148          COMMAREA  (DATE-CONVERSION-DATA)                         EL651
01149          LENGTH    (DC-COMM-LENGTH)                               EL651
01150      END-EXEC.                                                    EL651
01151                                                                   EL651
01152      MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE.            EL651
01153      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL651
01154                                                                   EL651
01155      MOVE ER-0000                TO  EMI-ERROR.                   EL651
01156      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
01157                                                                   EL651
01158      MOVE LOW-VALUES             TO  EL651AO.                     EL651
01159                                                                   EL651
01160      MOVE PI-ERR-TABLE           TO  TABLEO.                      EL651
01161      MOVE AL-UANON               TO  TABLEA.                      EL651
01162                                                                   EL651
01163      GO TO 8100-SEND-INITIAL-MAP.                                 EL651
01164                                                                   EL651
01165  4800-EXIT.                                                       EL651
01166      EXIT.                                                        EL651
01167      EJECT                                                        EL651
01168                                                                   EL651
01169  5000-BUILD-INITIAL-SCREEN.                                       EL651
01170      MOVE LOW-VALUES             TO  EL651AO.                     EL651
01171      MOVE ZEROS                  TO  MISC-SAVE-AREAS.             EL651
01172      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.           EL651
01173      MOVE 'A'                    TO  PI-ERR-CODE.                 EL651
01174                                                                   EL651
01175      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.                     EL651
01176                                                                   EL651
01177 ******************************************************************EL651
01178 *    ADD CODE TO SHOW RECORDS WHICH HAVE A MATCHING CARRIER IF   *EL651
01179 *    USER HAS CARRIER SECURITY.                                  *EL651
01180 ******************************************************************EL651
01181                                                                   EL651
01182      IF PI-CARRIER-SECURITY GREATER SPACE                         EL651
01183          IF PI-CARRIER-SECURITY = RE-TABLE-CARRIER-SECURITY       EL651
01184              NEXT SENTENCE                                        EL651
01185          ELSE                                                     EL651
01186              GO TO 8880-NOT-FOUND.                                EL651
01187                                                                   EL651
01188      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL651
01189      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL651
01190      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.          EL651
01191      MOVE ZEROS                  TO  PI-LAST-LEVEL.               EL651
01192                                                                   EL651
01193      IF CHANGES-NOT-MADE                                          EL651
01194          MOVE +1                 TO  PI-SUB.                      EL651
01195                                                                   EL651
01196      IF RE-100-COMP = ZERO                                        EL651
01197          MOVE SPACE              TO  PI-EXCESS-SW                 EL651
01198      ELSE                                                         EL651
01199          MOVE 'X'                TO  PI-EXCESS-SW.                EL651
01200                                                                   EL651
01201  5050-SET-UP-SCREEN.                                              EL651
01202      IF PI-SUB NOT NUMERIC                                        EL651
01203          MOVE +1                 TO  PI-SUB.                      EL651
01204                                                                   EL651
01205      MOVE PI-SUB                 TO  WS-CHECK-LEVEL               EL651
01206                                                                   EL651
01207      IF VALID-LEVEL                                               EL651
01208         IF RE-REI-COMP (PI-SUB) = SPACES                          EL651
01209            MOVE +1               TO  PI-SUB                       EL651
01210            MOVE ER-2338          TO  EMI-ERROR                    EL651
01211            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL651
01212         ELSE                                                      EL651
01213            NEXT SENTENCE                                          EL651
01214      ELSE                                                         EL651
01215         MOVE +1                  TO  PI-SUB                       EL651
01216         MOVE ER-2338             TO  EMI-ERROR                    EL651
01217         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL651
01218                                                                   EL651
01219      IF RE-AH-PCT (PI-SUB) NOT NUMERIC                            EL651
01220         GO TO 5070-SEARCH-LAST-LEVEL.                             EL651
01221                                                                   EL651
01222      IF RE-AH-PCT (PI-SUB) = ZEROS                                EL651
01223          NEXT SENTENCE                                            EL651
01224      ELSE                                                         EL651
01225          MOVE RE-AH-PCT (PI-SUB) TO  AHBENO                       EL651
01226          MOVE AL-UNNON           TO  AHBENA.                      EL651
01227                                                                   EL651
01228      IF RE-LF-PCT (PI-SUB) = ZEROS                                EL651
01229          NEXT SENTENCE                                            EL651
01230      ELSE                                                         EL651
01231          MOVE RE-LF-PCT (PI-SUB)     TO  LFBENO                   EL651
01232          MOVE AL-UNNON               TO  LFBENA.                  EL651
01233                                                                   EL651
01234      IF RE-LO-DATE (PI-SUB) = ZEROS                               EL651
01235          MOVE WS-LOW-DATE            TO  FDATEO                   EL651
01236          MOVE AL-UNNON               TO  FDATEA                   EL651
01237      ELSE                                                         EL651
01238          MOVE RE-LO-DATE (PI-SUB)    TO  DC-GREG-DATE-1-YMD       EL651
01239                                          WS-LO-DATE               EL651
01240          MOVE '3'                    TO  DC-OPTION-CODE           EL651
01241          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL651
01242          MOVE DC-GREG-DATE-1-EDIT    TO  FDATEO                   EL651
01243          MOVE AL-UNNON               TO  FDATEA.                  EL651
01244                                                                   EL651
01245 ***  Y2K PROJ 7744                                                   CL*37
01246      MOVE RE-HI-DATE(PI-SUB)         TO WS-HOLD-PIC98.               CL*41
01247                                                                      CL*24
01248      IF WS-HOLD-PIC98 = 99999999                                     CL*41
01249          MOVE WS-HIGH-DATE           TO  TDATEO                   EL651
01250          MOVE AL-UNNON               TO  TDATEA                   EL651
01251      ELSE                                                         EL651
01252          MOVE RE-HI-DATE (PI-SUB)    TO  DC-GREG-DATE-1-YMD       EL651
01253                                          WS-HI-DATE               EL651
01254          MOVE '3'                    TO  DC-OPTION-CODE           EL651
01255          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            EL651
01256          MOVE DC-GREG-DATE-1-EDIT    TO  TDATEO                   EL651
01257          MOVE AL-UNNON               TO  TDATEA                      CL*28
01258          MOVE DC-ALPHA-CENTURY       TO  WS-HI-DATE(1:2).            CL*40
01259                                                                   EL651
01260      MOVE RE-LF-LIM-LO (PI-SUB)         TO  LFLOBENO.             EL651
01261      MOVE RE-LF-LIM-HI (PI-SUB)         TO  LFHIBENO.             EL651
01262      MOVE RE-AHBEN-LIM-LO (PI-SUB)      TO  AHLOBENO.             EL651
01263      MOVE RE-AHBEN-LIM-HI (PI-SUB)      TO  AHHIBENO.             EL651
01264      MOVE RE-AHMOA-LIM-LO (PI-SUB)      TO  AMLOBENO.             EL651
01265      MOVE RE-AHMOA-LIM-HI (PI-SUB)      TO  AMHIBENO.             EL651
01266      MOVE RE-LF-LO (PI-SUB)             TO  LFLOAMTO.             EL651
01267      MOVE RE-LF-HI (PI-SUB)             TO  LFHIAMTO.             EL651
01268      MOVE RE-AHBEN-LO (PI-SUB)          TO  AHLOAMTO.             EL651
01269      MOVE RE-AHBEN-HI (PI-SUB)          TO  AHHIAMTO.             EL651
01270      MOVE RE-AHMOA-LO (PI-SUB)          TO  AMLOAMTO.             EL651
01271      MOVE RE-AHMOA-HI (PI-SUB)          TO  AMHIAMTO.             EL651
01272      MOVE RE-REI-COMP (PI-SUB)          TO  COMPANYO              EL651
01273                                             PI-SAVE-COMPANY.      EL651
01274      MOVE RE-REI-COMP-SUB (PI-SUB)      TO  COMPSUBO              EL651
01275                                             PI-SAVE-COMP-SUB.     EL651
01276      MOVE RE-LFAGE-LO (PI-SUB)          TO  LFLOAGEO.             EL651
01277      MOVE RE-LFAGE-HI (PI-SUB)          TO  LFHIAGEO.             EL651
01278      MOVE RE-LFTRM-LO (PI-SUB)          TO  LFLOTRMO.             EL651
01279      MOVE RE-LFTRM-HI (PI-SUB)          TO  LFHITRMO.             EL651
01280      MOVE RE-LF-BEN-CODE (PI-SUB)       TO  LIFEBENO.             EL651
01281      MOVE RE-AHAGE-LO (PI-SUB)          TO  AHLOAGEO.             EL651
01282      MOVE RE-AHAGE-HI (PI-SUB)          TO  AHHIAGEO.             EL651
01283      MOVE RE-AHTRM-LO (PI-SUB)          TO  AHLOTRMO.             EL651
01284      MOVE RE-AHTRM-HI (PI-SUB)          TO  AHHITRMO.             EL651
01285      MOVE RE-AH-BEN-CODE (PI-SUB)       TO  AHTBENO.              EL651
01286      MOVE RE-LF-QC (PI-SUB)             TO  LFQTEDO.              EL651
01287      MOVE RE-AH-QC (PI-SUB)             TO  AHQTEDO.              EL651
01288      MOVE RE-REMAINING (PI-SUB)         TO  LFREMO.               EL651
01289                                                                      CL*14
01290      MOVE RE-INTERACTIVE (PI-SUB)       TO  LFINTRO.              EL651
01291      MOVE RE-NSP-ST-CD-LF               TO  LFSTATEO.             EL651
01292      MOVE RE-NSP-ST-CD-AH               TO  AHSTATEO.             EL651
01293      MOVE RE-TABLE-CARRIER-SECURITY     TO  CARRIERO.             EL651
01294                                                                   EL651
01295      IF PI-NO-CARRIER-SECURITY                                    EL651
01296          MOVE AL-UANOF                  TO  CARRIERA              EL651
01297      ELSE                                                         EL651
01298          MOVE AL-SANOF                  TO  CARRIERA.             EL651
01299                                                                   EL651
01300      MOVE AL-UANON               TO  AHTBENA     TABLEA           EL651
01301                                      LFREMA      COMPANYA         EL651
01302                                      COMPSUBA    AHQTEDA          EL651
01303                                      LIFEBENA    LFQTEDA          EL651
01304                                      LFINTRA.                          000
01305                                                                   EL651
01306      MOVE AL-UNNON               TO  LFHIBENA    AHLOAMTA         EL651
01307                                      LFHIAMTA    AMLOAMTA         EL651
01308                                      AHHIAMTA    AMLOBENA         EL651
01309                                      AHHIBENA    LFLOAGEA         EL651
01310                                      AMHIBENA    LFHIAGEA         EL651
01311                                      AMHIAMTA    AHLOAGEA         EL651
01312                                      AHLOTRMA    AHHIAGEA         EL651
01313                                      AHHITRMA    LFLOTRMA         EL651
01314                                      LFLOBENA    LFHITRMA         EL651
01315                                      AHLOBENA    LFLOAMTA.        EL651
01316                                                                   EL651
01317      MOVE PI-SUB                 TO   PI-LAST-LEVEL               EL651
01318                                       SUB1.                       EL651
01319                                                                   EL651
01320      MOVE RE-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL651
01321      MOVE SPACE                  TO  DC-OPTION-CODE.              EL651
01322      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL651
01323      MOVE DC-GREG-DATE-1-EDIT    TO  LMDATEO.                     EL651
01324                                                                   EL651
01325      MOVE RE-LAST-MAINT-USER     TO  LMBYO.                       EL651
01326      MOVE RE-LAST-MAINT-HHMMSS   TO  TIME-MT.                     EL651
01327      MOVE TIME-LMT               TO  LMTIMEO.                     EL651
01328                                                                   EL651
01329  5070-SEARCH-LAST-LEVEL.                                          EL651
01330      ADD +1                      TO  SUB1.                        EL651
01331                                                                   EL651
01332      IF SUB1 GREATER +30                                          EL651
01333          NEXT SENTENCE                                            EL651
01334      ELSE                                                         EL651
01335          IF RE-REI-COMP (SUB1) = SPACES                           EL651
01336              NEXT SENTENCE                                        EL651
01337          ELSE                                                     EL651
01338              MOVE SUB1           TO  PI-LAST-LEVEL                EL651
01339              GO TO 5070-SEARCH-LAST-LEVEL.                        EL651
01340                                                                   EL651
01341      IF EXCESS-LEVEL-EXISTS AND                                   EL651
01342         PI-SUB = PI-LAST-LEVEL                                    EL651
01343            MOVE 'X'              TO  PERCNTO                      EL651
01344            MOVE AL-UANON         TO  PERCNTA.                     EL651
01345                                                                   EL651
01346      MOVE PI-SUB                 TO  LEVELO.                      EL651
01347      MOVE PI-LAST-LEVEL          TO  OFLEVELO.                    EL651
01348      MOVE PI-ERR-TABLE           TO  TABLEO.                      EL651
01349      MOVE AL-SANON               TO  OFLEVELA.                    EL651
01350      MOVE AL-UNNOF               TO  LEVELA.                      EL651
01351                                                                   EL651
01352      MOVE SPACE                  TO  PI-FIRST-TIME-SW.            EL651
01353      GO TO 8100-SEND-INITIAL-MAP.                                 EL651
01354                                                                   EL651
01355      EJECT                                                        EL651
01356  6000-CHECK-FOR-UPDATE.                                           EL651
01357       IF CHANGE-FUNCTION                                          EL651
01358           GO TO 6010-CONT.                                        EL651
01359                                                                   EL651
01360       IF TABLEL GREATER ZERO                                      EL651
01361           MOVE TABLEI            TO  RE-TABLE.                    EL651
01362                                                                   EL651
01363  6010-CONT.                                                       EL651
01364       IF COMPANYL GREATER ZERO                                    EL651
01365           MOVE COMPANYI          TO  RE-REI-COMP (PI-SUB).        EL651
01366                                                                   EL651
01367       IF COMPSUBL GREATER ZERO                                    EL651
01368           MOVE COMPSUBI          TO  RE-REI-COMP-SUB (PI-SUB).    EL651
01369                                                                   EL651
01370       IF FDATEL GREATER ZERO                                      EL651
01371           MOVE WS-LO-DATE        TO  RE-LO-DATE (PI-SUB).         EL651
01372                                                                   EL651
01373       IF TDATEL GREATER ZERO                                      EL651
01374           MOVE WS-HI-DATE        TO  RE-HI-DATE (PI-SUB).         EL651
01375                                                                   EL651
01376       IF PERCNTL GREATER ZERO                                     EL651
01377           IF PERCNTI = SPACE                                      EL651
01378               MOVE ZEROS         TO  RE-100-COMP                  EL651
01379           ELSE                                                    EL651
01380               MOVE PI-SUB        TO  RE-100-COMP.                 EL651
01381                                                                   EL651
01382       IF LFLOAGEL GREATER ZERO                                    EL651
01383           MOVE WS-LFAGE-LO       TO  RE-LFAGE-LO (PI-SUB).        EL651
01384                                                                   EL651
01385       IF LFHIAGEL GREATER ZERO                                    EL651
01386           MOVE WS-LFAGE-HI       TO  RE-LFAGE-HI (PI-SUB).        EL651
01387                                                                   EL651
01388       IF AHLOAGEL GREATER ZERO                                    EL651
01389           MOVE WS-AHAGE-LO       TO  RE-AHAGE-LO (PI-SUB).        EL651
01390                                                                   EL651
01391       IF AHHIAGEL GREATER ZERO                                    EL651
01392           MOVE WS-AHAGE-HI       TO  RE-AHAGE-HI (PI-SUB).        EL651
01393                                                                   EL651
01394       IF LFLOTRML GREATER ZERO                                    EL651
01395           MOVE WS-LFTRM-LO       TO  RE-LFTRM-LO (PI-SUB).        EL651
01396                                                                   EL651
01397       IF LFHITRML GREATER ZERO                                    EL651
01398           MOVE WS-LFTRM-HI       TO  RE-LFTRM-HI (PI-SUB).        EL651
01399                                                                   EL651
01400       IF AHLOTRML GREATER ZERO                                    EL651
01401           MOVE WS-AHTRM-LO       TO  RE-AHTRM-LO (PI-SUB).        EL651
01402                                                                   EL651
01403       IF AHHITRML GREATER ZERO                                    EL651
01404           MOVE WS-AHTRM-HI       TO  RE-AHTRM-HI (PI-SUB).        EL651
01405                                                                   EL651
01406       IF LFLOBENL GREATER ZERO                                    EL651
01407           MOVE WS-LF-LIM-LO      TO  RE-LF-LIM-LO (PI-SUB).       EL651
01408                                                                   EL651
01409       IF LFHIBENL GREATER ZERO                                    EL651
01410           MOVE WS-LF-LIM-HI      TO  RE-LF-LIM-HI (PI-SUB).       EL651
01411                                                                   EL651
01412       IF AHLOBENL GREATER ZERO                                    EL651
01413           MOVE WS-AHBEN-LIM-LO   TO  RE-AHBEN-LIM-LO (PI-SUB).    EL651
01414                                                                   EL651
01415       IF AHHIBENL GREATER ZERO                                    EL651
01416           MOVE WS-AHBEN-LIM-HI   TO  RE-AHBEN-LIM-HI (PI-SUB).    EL651
01417                                                                   EL651
01418       IF AMLOBENL GREATER ZERO                                    EL651
01419           MOVE WS-AHMOA-LIM-LO   TO  RE-AHMOA-LIM-LO (PI-SUB).    EL651
01420                                                                   EL651
01421       IF AMHIBENL GREATER ZERO                                    EL651
01422           MOVE WS-AHMOA-LIM-HI   TO  RE-AHMOA-LIM-HI (PI-SUB).    EL651
01423                                                                   EL651
01424       IF LFLOAMTL GREATER ZERO                                    EL651
01425           MOVE WS-LF-LO          TO  RE-LF-LO (PI-SUB).           EL651
01426                                                                   EL651
01427       IF LFHIAMTL GREATER ZERO                                    EL651
01428           MOVE WS-LF-HI          TO  RE-LF-HI (PI-SUB).           EL651
01429                                                                   EL651
01430       IF AHLOAMTL GREATER ZERO                                    EL651
01431           MOVE WS-AHBEN-LO       TO  RE-AHBEN-LO (PI-SUB).        EL651
01432                                                                   EL651
01433       IF AHHIAMTL GREATER ZERO                                    EL651
01434           MOVE WS-AHBEN-HI       TO  RE-AHBEN-HI (PI-SUB).        EL651
01435                                                                   EL651
01436       IF AMLOAMTL GREATER ZERO                                    EL651
01437           MOVE WS-AHMOA-LO       TO  RE-AHMOA-LO (PI-SUB).        EL651
01438                                                                   EL651
01439       IF AMHIAMTL GREATER ZERO                                    EL651
01440           MOVE WS-AHMOA-HI       TO  RE-AHMOA-HI (PI-SUB).        EL651
01441                                                                   EL651
01442       IF LIFEBENL GREATER ZERO                                    EL651
01443           MOVE LIFEBENI          TO  RE-LF-BEN-CODE (PI-SUB).     EL651
01444                                                                   EL651
01445       IF LFSTATEL GREATER ZERO                                    EL651
01446           MOVE LFSTATEI          TO  RE-NSP-ST-CD-LF              EL651
01447           MOVE ER-0585           TO  EMI-ERROR                    EL651
01448           PERFORM 9900-ERROR-FORMAT                               EL651
01449              THRU 9900-EXIT.                                      EL651
01450                                                                   EL651
01451       IF AHSTATEL GREATER ZERO                                    EL651
01452           MOVE AHSTATEI          TO  RE-NSP-ST-CD-AH              EL651
01453           MOVE ER-0585           TO  EMI-ERROR                    EL651
01454           PERFORM 9900-ERROR-FORMAT                               EL651
01455              THRU 9900-EXIT.                                      EL651
01456                                                                   EL651
01457       IF AHTBENL GREATER ZERO                                     EL651
01458           MOVE AHTBENI           TO  RE-AH-BEN-CODE (PI-SUB).     EL651
01459                                                                   EL651
01460       IF LFBENL GREATER ZERO                                      EL651
01461           MOVE LFBENI            TO  RE-LF-PCT (PI-SUB)           EL651
01462       ELSE                                                        EL651
01463           MOVE ZEROS             TO  RE-LF-PCT (PI-SUB).          EL651
01464                                                                   EL651
01465       IF AHBENL GREATER ZERO                                      EL651
01466           MOVE AHBENI            TO  RE-AH-PCT (PI-SUB)           EL651
01467       ELSE                                                        EL651
01468           MOVE ZEROS             TO  RE-AH-PCT (PI-SUB).          EL651
01469                                                                   EL651
01470       IF LFBENL GREATER ZERO AND                                  EL651
01471          AHBENL GREATER ZERO AND                                  EL651
01472          AHBENI = ZEROS      AND                                  EL651
01473          LFBENI = ZEROS                                           EL651
01474           MOVE ER-0593           TO  EMI-ERROR                    EL651
01475           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               EL651
01476                                                                   EL651
01477       IF LFINTRL GREATER ZERO                                     EL651
01478           MOVE LFINTRI           TO  RE-INTERACTIVE (PI-SUB).     EL651
01479                                                                   EL651
01480       IF LFQTEDL GREATER ZERO                                     EL651
01481           MOVE LFQTEDI           TO  RE-LF-QC (PI-SUB).           EL651
01482                                                                   EL651
01483       IF AHQTEDL GREATER ZERO                                     EL651
01484           MOVE AHQTEDI           TO  RE-AH-QC (PI-SUB).           EL651
01485                                                                   EL651
01486       IF LFREML GREATER ZERO                                      EL651
01487           MOVE LFREMI            TO  RE-REMAINING (PI-SUB).       EL651
01488                                                                   EL651
01489       IF CARRIERL GREATER ZERO                                    EL651
01490           MOVE CARRIERI          TO  RE-TABLE-CARRIER-SECURITY.   EL651
01491                                                                   EL651
01492       MOVE 'A'                   TO  RE-CODE.                     EL651
01493                                                                   EL651
01494  6099-EXIT.                                                       EL651
01495      EXIT.                                                        EL651
01496      EJECT                                                        EL651
01497                                                                   EL651
01498  7000-EDIT.                                                       EL651
01499      IF COMPSUBL = ZERO                                           EL651
01500          MOVE AL-UANON           TO  COMPSUBA                     EL651
01501          MOVE SPACES             TO  COMPSUBI.                    EL651
01502                                                                   EL651
01503      IF COMPANYL GREATER ZERO                                     EL651
01504          MOVE ZERO TO TALLY                                          CL*16
01505          INSPECT COMPANYI TALLYING TALLY FOR ALL SPACES              CL*16
01506          IF TALLY GREATER ZERO                                    EL651
01507              MOVE -1             TO  COMPANYL                     EL651
01508              MOVE AL-UABON       TO  COMPANYA                     EL651
01509              MOVE ER-2340        TO  EMI-ERROR                    EL651
01510              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01511          ELSE                                                     EL651
01512              PERFORM 7400-SEARCH-FOR-COMPANY THRU 7499-EXIT       EL651
01513              MOVE AL-UANON       TO  COMPANYA                     EL651
01514      ELSE                                                         EL651
01515          IF ADD-FUNCTION                                          EL651
01516              MOVE -1             TO  COMPANYL                     EL651
01517              MOVE AL-UABON       TO  COMPANYA                     EL651
01518              MOVE ER-2311        TO  EMI-ERROR                    EL651
01519              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL651
01520                                                                   EL651
01521      IF PERCNTL GREATER ZERO                                      EL651
01522          MOVE PERCNTI            TO  WS-CHECK-PERCENT             EL651
01523          IF EXCESS-LEVEL                                          EL651
01524              IF EXCESS-LEVEL-EXISTS                               EL651
01525                  IF PI-LAST-LEVEL = PI-SUB                        EL651
01526                      MOVE AL-UANON   TO  PERCNTA                  EL651
01527                  ELSE                                             EL651
01528                      MOVE -1         TO  PERCNTL                  EL651
01529                      MOVE AL-UABON   TO  PERCNTA                  EL651
01530                      MOVE ER-2347    TO  EMI-ERROR                EL651
01531                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL651
01532              ELSE                                                 EL651
01533                  IF PI-LAST-LEVEL = PI-SUB                        EL651
01534                      MOVE AL-UANON   TO  PERCNTA                  EL651
01535                  ELSE                                             EL651
01536                      MOVE -1         TO  PERCNTL                  EL651
01537                      MOVE AL-UABON   TO  PERCNTA                  EL651
01538                      MOVE ER-2348    TO  EMI-ERROR                EL651
01539                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL651
01540          ELSE                                                     EL651
01541              IF NO-LEVEL                                          EL651
01542                  MOVE AL-UANON       TO  PERCNTA                  EL651
01543              ELSE                                                 EL651
01544                  MOVE -1             TO  PERCNTL                  EL651
01545                  MOVE AL-UABON       TO  PERCNTA                  EL651
01546                  MOVE ER-2316        TO  EMI-ERROR                EL651
01547                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01548      ELSE                                                         EL651
01549          MOVE AL-UANON           TO  PERCNTA                      EL651
01550          MOVE SPACE              TO  PERCNTO.                     EL651
01551                                                                   EL651
01552      IF PI-CARRIER-SECURITY GREATER SPACE                         EL651
01553          MOVE PI-CARRIER-SECURITY    TO  CARRIERI                 EL651
01554          MOVE AL-SANON               TO  CARRIERA.                EL651
01555                                                                   EL651
01556      IF CARRIERL GREATER ZERO                                     EL651
01557          MOVE CARRIERI           TO  WS-CARRIER                   EL651
01558          PERFORM 7550-CHECK-CARRIER THRU 7550-EXIT                EL651
01559          IF CARRIER-FOUND                                         EL651
01560              MOVE AL-UANON       TO  CARRIERA                     EL651
01561          ELSE                                                     EL651
01562              IF CARRIERI = SPACES                                 EL651
01563                  MOVE AL-UANON   TO  CARRIERA                     EL651
01564              ELSE                                                 EL651
01565                  MOVE -1         TO  CARRIERL                     EL651
01566                  MOVE AL-UABON   TO  CARRIERA                     EL651
01567                  MOVE ER-2208    TO  EMI-ERROR                    EL651
01568                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL651
01569                                                                   EL651
01570      MOVE ZEROS                  TO  DATE-TEST-AREA.              EL651
01571                                                                   EL651
01572 ***  Y2K PROJ 7744                                                   CL*37
01573      IF FDATEL GREATER ZERO                                       EL651
01574          MOVE FDATEI             TO  DEEDIT-FIELD                 EL651
01575          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
01576          IF DEEDIT-FIELD-V0 GREATER ZERO                          EL651
01577              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY       EL651
01578              MOVE '4'                TO  DC-OPTION-CODE           EL651
01579              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL651
01580              IF NO-CONVERSION-ERROR                               EL651
01581                  MOVE AL-UANON       TO  FDATEA                   EL651
01582                  MOVE DC-GREG-DATE-1-YMD   TO  DATE-TEST-AREA        CL*29
01583                                                WS-LO-DATE            CL*29
01584                  MOVE DC-ALPHA-CENTURY     TO  DATE-TEST-AREA(1:2)   CL*39
01585                                                WS-LO-DATE(1:2)       CL*29
01586                  MOVE DC-GREG-DATE-1-EDIT  TO FDATEO              EL651
01587              ELSE                                                 EL651
01588                  MOVE -1         TO  FDATEL                       EL651
01589                  MOVE AL-UABON   TO  FDATEA                       EL651
01590                  MOVE ER-2312    TO  EMI-ERROR                    EL651
01591                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01592              END-IF                                                  CL*33
01593          ELSE                                                     EL651
01594              MOVE WS-LOW-DATE    TO  FDATEO                       EL651
01595              MOVE ZEROS          TO  WS-LO-DATE                   EL651
01596              MOVE AL-UANON       TO  FDATEA                       EL651
01597          END-IF                                                      CL*33
01598      ELSE                                                         EL651
01599          IF ADD-FUNCTION                                          EL651
01600              MOVE WS-LOW-DATE    TO  FDATEO                       EL651
01601              MOVE ZEROS          TO  WS-LO-DATE                   EL651
01602              MOVE AL-UANON       TO  FDATEA                          CL*34
01603          END-IF                                                      CL*33
01604      END-IF.                                                         CL*33
01605                                                                   EL651
01606      IF TDATEL GREATER ZERO                                       EL651
01607          MOVE TDATEI             TO  DEEDIT-FIELD                 EL651
01608          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
01609          IF DEEDIT-FIELD-V0 = 999999                              EL651
01610              MOVE WS-HIGH-DATE   TO  TDATEO                       EL651
01611              MOVE 99999999       TO  WS-HI-DATE                      CL*20
01612              MOVE AL-UANON       TO  TDATEA                       EL651
01613          ELSE                                                     EL651
01614              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY       EL651
01615              MOVE '4'                TO  DC-OPTION-CODE           EL651
01616              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        EL651
01617              IF NO-CONVERSION-ERROR                               EL651
01618                  MOVE DC-GREG-DATE-1-YMD TO WS-HOLD-PIC98-2          CL*35
01619                  MOVE DC-ALPHA-CENTURY   TO WS-HOLD-PIC98-2(1:2)     CL*39
01620                  IF WS-HOLD-PIC98-2 > DATE-TEST-AREA                 CL*35
01621                      MOVE AL-UANON            TO  TDATEA          EL651
01622                      MOVE DC-GREG-DATE-1-EDIT TO  TDATEO          EL651
01623                      MOVE DC-GREG-DATE-1-YMD  TO  WS-HI-DATE         CL*30
01624                      MOVE DC-ALPHA-CENTURY    TO  WS-HI-DATE(1:2)    CL*39
01625                  ELSE                                             EL651
01626                      MOVE -1         TO  TDATEL                   EL651
01627                      MOVE AL-UABON   TO  TDATEA                   EL651
01628                      MOVE ER-2339    TO  EMI-ERROR                EL651
01629                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL651
01630                  END-IF                                              CL*32
01631              ELSE                                                 EL651
01632                  MOVE -1         TO  TDATEL                       EL651
01633                  MOVE AL-UABON   TO  TDATEA                       EL651
01634                  MOVE ER-2313    TO  EMI-ERROR                    EL651
01635                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01636              END-IF                                                  CL*32
01637          END-IF                                                      CL*32
01638      ELSE                                                         EL651
01639          IF ADD-FUNCTION                                          EL651
01640              MOVE WS-HIGH-DATE   TO  TDATEO                       EL651
01641              MOVE 99999999       TO  WS-HI-DATE                      CL*20
01642              MOVE AL-UANON       TO  TDATEA                          CL*32
01643          END-IF                                                      CL*32
01644      END-IF.                                                         CL*32
01645 ***  Y2K PROJ 7744                                                   CL*37
01646                                                                   EL651
01647      MOVE ZEROS                  TO  WS-PREV-AGE                  EL651
01648                                      WS-PREV-TRM                  EL651
01649                                      WS-PREV-AMT.                 EL651
01650                                                                   EL651
01651      IF LFLOAGEL GREATER ZERO                                     EL651
01652          IF LFLOAGEI NUMERIC                                      EL651
01653              MOVE AL-UANON       TO  LFLOAGEA                     EL651
01654              MOVE LFLOAGEI       TO  WS-PREV-AGE                  EL651
01655                                      WS-LFAGE-LO                  EL651
01656          ELSE                                                     EL651
01657              MOVE -1             TO  LFLOAGEL                     EL651
01658              MOVE AL-UNBON       TO  LFLOAGEA                     EL651
01659              MOVE ER-2317        TO  EMI-ERROR                    EL651
01660              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01661      ELSE                                                         EL651
01662          IF ADD-FUNCTION                                          EL651
01663              MOVE ZEROS          TO  LFLOAGEO                     EL651
01664                                      WS-LFAGE-LO                  EL651
01665              MOVE AL-UNNON       TO  LFLOAGEA.                    EL651
01666                                                                   EL651
01667      IF LFHIAGEL GREATER ZERO                                     EL651
01668          IF LFHIAGEI NUMERIC                                      EL651
01669              IF LFHIAGEI LESS WS-PREV-AGE                         EL651
01670                  MOVE -1         TO  LFHIAGEL                     EL651
01671                  MOVE AL-UNBON   TO  LFHIAGEA                     EL651
01672                  MOVE ER-2318    TO  EMI-ERROR                    EL651
01673                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01674              ELSE                                                 EL651
01675                  MOVE AL-UNNON   TO  LFHIAGEA                     EL651
01676                  MOVE LFHIAGEI   TO  WS-LFAGE-HI                  EL651
01677          ELSE                                                     EL651
01678              MOVE -1             TO  LFHIAGEL                     EL651
01679              MOVE AL-UNBON       TO  LFHIAGEA                     EL651
01680              MOVE ER-2319        TO  EMI-ERROR                    EL651
01681              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01682      ELSE                                                         EL651
01683          IF ADD-FUNCTION                                          EL651
01684              MOVE 99             TO  LFHIAGEO                     EL651
01685                                      WS-LFAGE-HI                  EL651
01686              MOVE AL-UNNON       TO  LFHIAGEA.                    EL651
01687                                                                   EL651
01688      IF LFLOTRML GREATER ZERO                                     EL651
01689          IF LFLOTRMI NUMERIC                                      EL651
01690              MOVE AL-UNNON       TO  LFLOTRMA                     EL651
01691              MOVE LFLOTRMI       TO  WS-PREV-TRM                  EL651
01692                                      WS-LFTRM-LO                  EL651
01693          ELSE                                                     EL651
01694              MOVE -1             TO  LFLOTRML                     EL651
01695              MOVE AL-UNBON       TO  LFLOTRMA                     EL651
01696              MOVE ER-2320        TO  EMI-ERROR                    EL651
01697              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01698      ELSE                                                         EL651
01699          IF ADD-FUNCTION                                          EL651
01700              MOVE ZEROS          TO  LFLOTRMO                     EL651
01701                                      WS-LFTRM-LO                  EL651
01702              MOVE AL-UNNON       TO  LFLOTRMA.                    EL651
01703                                                                   EL651
01704      IF LFHITRML GREATER ZERO                                     EL651
01705          IF LFHITRMI NUMERIC                                      EL651
01706              IF LFHITRMI LESS WS-PREV-TRM                         EL651
01707                  MOVE -1         TO  LFHITRML                     EL651
01708                  MOVE AL-UABON   TO  LFHITRMA                     EL651
01709                  MOVE ER-2321    TO  EMI-ERROR                    EL651
01710                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01711              ELSE                                                 EL651
01712                  MOVE AL-UNNON   TO  LFHITRMA                     EL651
01713                  MOVE LFHITRMI   TO  WS-LFTRM-HI                  EL651
01714          ELSE                                                     EL651
01715              MOVE -1             TO  LFHITRML                     EL651
01716              MOVE AL-UNBON       TO  LFHITRMA                     EL651
01717              MOVE ER-2322        TO  EMI-ERROR                    EL651
01718              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01719      ELSE                                                         EL651
01720          IF ADD-FUNCTION                                          EL651
01721              MOVE 999            TO  LFHITRMO                     EL651
01722                                      WS-LFTRM-HI                  EL651
01723              MOVE AL-UNNON       TO  LFHITRMA.                    EL651
01724                                                                   EL651
01725      IF LFLOBENL GREATER ZERO                                     EL651
01726          MOVE LFLOBENI           TO  DEEDIT-FIELD                 EL651
01727          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
01728          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
01729              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT              EL651
01730                                          LFLOBENO                 EL651
01731                                          WS-LF-LIM-LO             EL651
01732              MOVE AL-UNNON           TO  LFLOBENA                 EL651
01733          ELSE                                                     EL651
01734              MOVE -1             TO  LFLOBENL                     EL651
01735              MOVE AL-UNBON       TO  LFLOBENA                     EL651
01736              MOVE ER-2323        TO  EMI-ERROR                    EL651
01737              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01738      ELSE                                                         EL651
01739          IF ADD-FUNCTION                                          EL651
01740              MOVE ZEROS          TO  LFLOBENO                     EL651
01741                                      WS-LF-LIM-LO                 EL651
01742              MOVE AL-UNNON       TO  LFLOBENA.                    EL651
01743                                                                   EL651
01744      IF LFHIBENL GREATER ZERO                                     EL651
01745          MOVE LFHIBENI           TO  DEEDIT-FIELD                 EL651
01746          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
01747          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
01748              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT                  EL651
01749                  MOVE -1         TO  LFHIBENL                     EL651
01750                  MOVE AL-UNBON   TO  LFHIBENA                     EL651
01751                  MOVE ER-2324    TO  EMI-ERROR                    EL651
01752                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01753              ELSE                                                 EL651
01754                  MOVE AL-UNNON   TO  LFHIBENA                     EL651
01755                  MOVE DEEDIT-FIELD-V1    TO  LFHIBENO             EL651
01756                                              WS-LF-LIM-HI         EL651
01757          ELSE                                                     EL651
01758              MOVE -1             TO  LFHIBENL                     EL651
01759              MOVE AL-UNBON       TO  LFHIBENA                     EL651
01760              MOVE ER-2325        TO  EMI-ERROR                    EL651
01761              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01762      ELSE                                                         EL651
01763          IF ADD-FUNCTION                                          EL651
01764              MOVE NINES          TO  LFHIBENO                     EL651
01765                                      WS-LF-LIM-HI                 EL651
01766              MOVE AL-UNNON       TO  LFHIBENA.                    EL651
01767                                                                   EL651
01768      MOVE ZEROS                  TO  WS-PREV-AGE                  EL651
01769                                      WS-PREV-TRM                  EL651
01770                                      WS-PREV-AMT.                 EL651
01771                                                                   EL651
01772      IF AHLOAGEL GREATER ZERO                                     EL651
01773          IF AHLOAGEI NUMERIC                                      EL651
01774              MOVE AL-UNNON       TO  AHLOAGEA                     EL651
01775              MOVE AHLOAGEI       TO  WS-PREV-AGE                  EL651
01776                                      WS-AHAGE-LO                  EL651
01777          ELSE                                                     EL651
01778              MOVE -1             TO  AHLOAGEL                     EL651
01779              MOVE AL-UNBON       TO  AHLOAGEA                     EL651
01780              MOVE ER-2317        TO  EMI-ERROR                    EL651
01781              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01782      ELSE                                                         EL651
01783          IF ADD-FUNCTION                                          EL651
01784              MOVE ZEROS          TO  AHLOAGEO                     EL651
01785                                      WS-AHAGE-LO                  EL651
01786              MOVE AL-UNNON       TO  AHLOAGEA.                    EL651
01787                                                                   EL651
01788      IF AHHIAGEL GREATER ZERO                                     EL651
01789          IF AHHIAGEI NUMERIC                                      EL651
01790              IF AHHIAGEI LESS WS-PREV-AGE                         EL651
01791                  MOVE -1         TO  AHHIAGEL                     EL651
01792                  MOVE AL-UNBON   TO  AHHIAGEA                     EL651
01793                  MOVE ER-2318    TO  EMI-ERROR                    EL651
01794                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01795              ELSE                                                 EL651
01796                  MOVE AL-UNNON   TO  AHHIAGEA                     EL651
01797                  MOVE AHHIAGEI   TO  WS-AHAGE-HI                  EL651
01798          ELSE                                                     EL651
01799              MOVE -1             TO  AHHIAGEL                     EL651
01800              MOVE AL-UNBON       TO  AHHIAGEA                     EL651
01801              MOVE ER-2319        TO  EMI-ERROR                    EL651
01802              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01803      ELSE                                                         EL651
01804          IF ADD-FUNCTION                                          EL651
01805              MOVE 99             TO  AHHIAGEO                     EL651
01806                                      WS-AHAGE-HI                  EL651
01807              MOVE AL-UNNON       TO  AHHIAGEA.                    EL651
01808                                                                   EL651
01809      IF AHLOTRML GREATER ZERO                                     EL651
01810          IF AHLOTRMI NUMERIC                                      EL651
01811              MOVE AL-UNNON       TO  AHLOTRMA                     EL651
01812              MOVE AHLOTRMI       TO  WS-PREV-TRM                  EL651
01813                                      WS-AHTRM-LO                  EL651
01814          ELSE                                                     EL651
01815              MOVE -1             TO  AHLOTRML                     EL651
01816              MOVE AL-UNBON       TO  AHLOTRMA                     EL651
01817              MOVE ER-2320        TO  EMI-ERROR                    EL651
01818              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01819      ELSE                                                         EL651
01820          IF ADD-FUNCTION                                          EL651
01821              MOVE ZEROS          TO  AHLOTRMO                     EL651
01822                                      WS-AHTRM-LO                  EL651
01823              MOVE AL-UNNON       TO  AHLOTRMA.                    EL651
01824                                                                   EL651
01825      IF AHHITRML GREATER ZERO                                     EL651
01826          IF AHHITRMI NUMERIC                                      EL651
01827              IF AHHITRMI LESS WS-PREV-TRM                         EL651
01828                  MOVE -1         TO  AHHITRML                     EL651
01829                  MOVE AL-UNBON   TO  AHHITRMA                     EL651
01830                  MOVE ER-2321    TO  EMI-ERROR                    EL651
01831                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01832              ELSE                                                 EL651
01833                  MOVE AL-UNNON   TO  AHHITRMA                     EL651
01834                  MOVE AHHITRMI   TO  WS-AHTRM-HI                  EL651
01835          ELSE                                                     EL651
01836              MOVE -1             TO  AHHITRML                     EL651
01837              MOVE AL-UNBON       TO  AHHITRMA                     EL651
01838              MOVE ER-2322        TO  EMI-ERROR                    EL651
01839              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01840      ELSE                                                         EL651
01841          IF ADD-FUNCTION                                          EL651
01842              MOVE 999            TO  AHHITRMO                     EL651
01843                                      WS-AHTRM-HI                  EL651
01844              MOVE AL-UNNON       TO  AHHITRMA.                    EL651
01845                                                                   EL651
01846      IF LFBENL GREATER ZERO AND                                   EL651
01847         LFBENI NUMERIC AND                                        EL651
01848         LFBENI GREATER +1                                         EL651
01849         MOVE -1                  TO  LFBENL                       EL651
01850         MOVE AL-UNBON            TO  LFBENA                       EL651
01851         MOVE ER-2356             TO  EMI-ERROR                    EL651
01852         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL651
01853                                                                   EL651
01854      IF AHBENL GREATER ZERO AND                                   EL651
01855         AHBENI NUMERIC AND                                        EL651
01856         AHBENI GREATER +1                                         EL651
01857         MOVE -1                  TO  AHBENL                       EL651
01858         MOVE AL-UNBON            TO  AHBENA                       EL651
01859         MOVE ER-2356             TO  EMI-ERROR                    EL651
01860         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL651
01861                                                                   EL651
01862      IF AHLOBENL GREATER ZERO                                     EL651
01863          MOVE AHLOBENI           TO  DEEDIT-FIELD                 EL651
01864          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
01865          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
01866              MOVE AL-UNNON       TO  AHLOBENA                     EL651
01867              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT              EL651
01868                                          AHLOBENO                 EL651
01869                                          WS-AHBEN-LIM-LO          EL651
01870          ELSE                                                     EL651
01871              MOVE -1             TO  AHLOBENL                     EL651
01872              MOVE AL-UNBON       TO  AHLOBENA                     EL651
01873              MOVE ER-2323        TO  EMI-ERROR                    EL651
01874              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01875      ELSE                                                         EL651
01876          IF ADD-FUNCTION                                          EL651
01877              MOVE ZEROS          TO  AHLOBENO                     EL651
01878                                      WS-AHBEN-LIM-LO              EL651
01879              MOVE AL-UNNON       TO  AHLOBENA.                    EL651
01880                                                                   EL651
01881      IF AHHIBENL GREATER ZERO                                     EL651
01882          MOVE AHHIBENI           TO  DEEDIT-FIELD                 EL651
01883          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
01884          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
01885              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT                  EL651
01886                  MOVE -1         TO  AHHIBENL                     EL651
01887                  MOVE AL-UNBON   TO  AHHIBENA                     EL651
01888                  MOVE ER-2324    TO  EMI-ERROR                    EL651
01889                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01890              ELSE                                                 EL651
01891                  MOVE AL-UNNON   TO  AHHIBENA                     EL651
01892                  MOVE DEEDIT-FIELD-V1    TO  AHHIBENO             EL651
01893                                              WS-AHBEN-LIM-HI      EL651
01894          ELSE                                                     EL651
01895              MOVE -1             TO  AHHIBENL                     EL651
01896              MOVE AL-UNBON       TO  AHHIBENA                     EL651
01897              MOVE ER-2325        TO  EMI-ERROR                    EL651
01898              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01899      ELSE                                                         EL651
01900          IF ADD-FUNCTION                                          EL651
01901              MOVE NINES          TO  AHHIBENO                     EL651
01902                                      WS-AHBEN-LIM-HI              EL651
01903              MOVE AL-UNNON       TO  AHHIBENA.                    EL651
01904                                                                   EL651
01905      MOVE ZEROS                  TO  WS-PREV-AGE                  EL651
01906                                      WS-PREV-TRM                  EL651
01907                                      WS-PREV-AMT.                 EL651
01908                                                                   EL651
01909      IF AMLOBENL GREATER ZERO                                     EL651
01910          MOVE AMLOBENI           TO  DEEDIT-FIELD                 EL651
01911          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
01912          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
01913              MOVE AL-UNNON       TO  AMLOBENA                     EL651
01914              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT              EL651
01915                                          AMLOBENO                 EL651
01916                                          WS-AHMOA-LIM-LO          EL651
01917          ELSE                                                     EL651
01918              MOVE -1             TO  AMLOBENL                     EL651
01919              MOVE AL-UNBON       TO  AMLOBENA                     EL651
01920              MOVE ER-2323        TO  EMI-ERROR                    EL651
01921              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01922      ELSE                                                         EL651
01923          IF ADD-FUNCTION                                          EL651
01924              MOVE ZEROS          TO  AMLOBENO                     EL651
01925                                      WS-AHMOA-LIM-LO              EL651
01926              MOVE AL-UNNON       TO  AMLOBENA.                    EL651
01927                                                                   EL651
01928      IF AMHIBENL GREATER ZERO                                     EL651
01929          MOVE AMHIBENI           TO  DEEDIT-FIELD                 EL651
01930          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
01931          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
01932              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT                  EL651
01933                  MOVE -1         TO  AMHIBENL                     EL651
01934                  MOVE AL-UNBON   TO  AMHIBENA                     EL651
01935                  MOVE ER-2324    TO  EMI-ERROR                    EL651
01936                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
01937              ELSE                                                 EL651
01938                  MOVE AL-UNNON   TO  AMHIBENA                     EL651
01939                  MOVE DEEDIT-FIELD-V1 TO AMHIBENO                 EL651
01940                                          WS-AHMOA-LIM-HI          EL651
01941          ELSE                                                     EL651
01942              MOVE -1             TO  AMHIBENL                     EL651
01943              MOVE AL-UNBON       TO  AMHIBENA                     EL651
01944              MOVE ER-2325        TO  EMI-ERROR                    EL651
01945              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01946      ELSE                                                         EL651
01947          IF ADD-FUNCTION                                          EL651
01948              MOVE NINES          TO  AMHIBENO                     EL651
01949                                      WS-AHMOA-LIM-HI              EL651
01950              MOVE AL-UNNON       TO  AMHIBENA.                    EL651
01951                                                                   EL651
01952      IF LFQTEDL GREATER ZERO                                      EL651
01953          MOVE LFQTEDI            TO  WS-CHECK-QTED                EL651
01954          IF VALID-QTED-CALC                                       EL651
01955              MOVE AL-UANON       TO  LFQTEDA                      EL651
01956          ELSE                                                     EL651
01957              MOVE -1             TO  LFQTEDL                      EL651
01958              MOVE AL-UABON       TO  LFQTEDA                      EL651
01959              MOVE ER-2333        TO  EMI-ERROR                    EL651
01960              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01961      ELSE                                                         EL651
01962          IF ADD-FUNCTION                                          EL651
01963              MOVE SPACE          TO  LFQTEDO                      EL651
01964              MOVE AL-UANON       TO  LFQTEDA.                     EL651
01965                                                                   EL651
01966      IF LIFEBENL GREATER ZERO                                     EL651
01967          MOVE LIFEBENI           TO  WS-CHECK-BEN-CODE            EL651
01968          IF VALID-BEN-CODE                                        EL651
01969              MOVE AL-UANON       TO  LIFEBENA                     EL651
01970          ELSE                                                     EL651
01971              MOVE -1             TO  LIFEBENL                     EL651
01972              MOVE AL-UABON       TO  LIFEBENA                     EL651
01973              MOVE ER-2326        TO  EMI-ERROR                    EL651
01974              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01975      ELSE                                                         EL651
01976          IF ADD-FUNCTION                                          EL651
01977              MOVE SPACE          TO  LIFEBENO                     EL651
01978              MOVE AL-UABON       TO  LIFEBENA.                    EL651
01979                                                                   EL651
01980      IF LFINTRL GREATER ZERO                                      EL651
01981          MOVE LFINTRI            TO  WS-CHECK-INTR                EL651
01982          IF VALID-INTR-CODE                                       EL651
01983              MOVE AL-UANON       TO  LFINTRA                      EL651
01984          ELSE                                                     EL651
01985              MOVE -1             TO  LFINTRL                      EL651
01986              MOVE AL-UABON       TO  LFINTRA                      EL651
01987              MOVE ER-2327        TO  EMI-ERROR                    EL651
01988              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
01989      ELSE                                                         EL651
01990          IF ADD-FUNCTION                                          EL651
01991              MOVE SPACE          TO  LFINTRO                      EL651
01992              MOVE AL-UABON       TO  LFINTRA.                     EL651
01993                                                                   EL651
01994      IF LFREML GREATER ZERO                                       EL651
01995          MOVE LFREMI             TO  WS-CHECK-REM                 EL651
01996          IF VALID-REM-SW                                          EL651
01997              MOVE AL-UANON       TO  LFREMA                       EL651
01998          ELSE                                                     EL651
01999          IF PI-COMPANY-ID EQUAL 'NSL' AND                            CL*16
02000             WS-CHECK-REM EQUAL 'I'                                   CL*16
02001              MOVE AL-UANON       TO  LFREMA                          CL*16
02002          ELSE                                                        CL*16
02003              MOVE -1             TO  LFREML                       EL651
02004              MOVE AL-UABON       TO  LFREMA                       EL651
02005              MOVE ER-2334        TO  EMI-ERROR                    EL651
02006              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02007      ELSE                                                         EL651
02008          IF ADD-FUNCTION                                          EL651
02009              MOVE SPACE          TO  LFREMO                       EL651
02010              MOVE AL-UANON       TO  LFREMA.                      EL651
02011                                                                   EL651
02012      IF LFSTATEL GREATER ZERO                                     EL651
02013          MOVE SPACES             TO  WS-ACCESS                    EL651
02014          MOVE LFSTATEI           TO  WS-STATE                     EL651
02015          PERFORM 7050-CHECK-STATE THRU 7050-EXIT                  EL651
02016          IF STATE-FOUND                                           EL651
02017              MOVE AL-UANON       TO  LFSTATEA                     EL651
02018          ELSE                                                     EL651
02019              IF LFSTATEI = SPACES                                 EL651
02020                  MOVE AL-UANON   TO  LFSTATEA                     EL651
02021              ELSE                                                 EL651
02022                  MOVE -1         TO  LFSTATEL                     EL651
02023                  MOVE AL-UABON   TO  LFSTATEA                     EL651
02024                  MOVE ER-2335    TO  EMI-ERROR                    EL651
02025                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL651
02026                                                                   EL651
02027      IF LFBENL GREATER ZERO                                       EL651
02028          IF LFBENI NUMERIC                                        EL651
02029              MOVE AL-UNNON       TO  LFBENA                       EL651
02030          ELSE                                                     EL651
02031              MOVE -1             TO  LFBENL                       EL651
02032              MOVE AL-UNBON       TO  LFBENA                       EL651
02033              MOVE ER-2331        TO  EMI-ERROR                    EL651
02034              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02035      ELSE                                                         EL651
02036          IF ADD-FUNCTION                                          EL651
02037              MOVE ZEROS          TO  LFBENO                       EL651
02038              MOVE AL-UNNON       TO  LFBENA.                      EL651
02039                                                                   EL651
02040      MOVE ZEROS                  TO  WS-PREV-AMT.                 EL651
02041                                                                   EL651
02042      IF LFLOAMTL GREATER ZERO                                     EL651
02043          MOVE LFLOAMTI           TO  DEEDIT-FIELD                 EL651
02044          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
02045          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
02046              MOVE AL-UNNON       TO  LFLOAMTA                     EL651
02047              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT              EL651
02048                                          LFLOAMTO                 EL651
02049                                          WS-LF-LO                 EL651
02050          ELSE                                                     EL651
02051              MOVE -1             TO  LFLOAMTL                     EL651
02052              MOVE AL-UNBON       TO  LFLOAMTA                     EL651
02053              MOVE ER-2328        TO  EMI-ERROR                    EL651
02054              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02055      ELSE                                                         EL651
02056          IF ADD-FUNCTION                                          EL651
02057              MOVE ZEROS          TO  LFLOAMTO                     EL651
02058                                      WS-LF-LO                     EL651
02059              MOVE AL-UNNON       TO  LFLOAMTA.                    EL651
02060                                                                   EL651
02061      IF LFHIAMTL GREATER ZERO                                     EL651
02062          MOVE LFHIAMTI           TO  DEEDIT-FIELD                 EL651
02063          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
02064          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
02065              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT                  EL651
02066                  MOVE -1         TO  LFHIAMTL                     EL651
02067                  MOVE AL-UNBON   TO  LFHIAMTA                     EL651
02068                  MOVE ER-2329    TO  EMI-ERROR                    EL651
02069                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
02070              ELSE                                                 EL651
02071                  MOVE AL-UNNON   TO  LFHIAMTA                     EL651
02072                  MOVE DEEDIT-FIELD-V1 TO LFHIAMTO                 EL651
02073                                          WS-LF-HI                 EL651
02074          ELSE                                                     EL651
02075              MOVE -1             TO  LFHIAMTL                     EL651
02076              MOVE AL-UNBON       TO  LFHIAMTA                     EL651
02077              MOVE ER-2330        TO  EMI-ERROR                    EL651
02078              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02079      ELSE                                                         EL651
02080          IF ADD-FUNCTION                                          EL651
02081              MOVE NINES          TO  LFHIAMTO                     EL651
02082                                      WS-LF-HI                     EL651
02083              MOVE AL-UNNON       TO  LFHIAMTA.                    EL651
02084                                                                   EL651
02085      IF AHQTEDL GREATER ZERO                                      EL651
02086          MOVE AHQTEDI            TO  WS-CHECK-QTED                EL651
02087          IF VALID-QTED-CALC                                       EL651
02088              MOVE AL-UANON       TO  AHQTEDA                      EL651
02089          ELSE                                                     EL651
02090              MOVE -1             TO  AHQTEDL                      EL651
02091              MOVE AL-UABON       TO  AHQTEDA                      EL651
02092              MOVE ER-2333        TO  EMI-ERROR                    EL651
02093              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02094      ELSE                                                         EL651
02095          IF ADD-FUNCTION                                          EL651
02096              MOVE SPACE          TO  AHQTEDO                      EL651
02097              MOVE AL-UANON       TO  AHQTEDA.                     EL651
02098                                                                   EL651
02099      IF AHTBENL GREATER ZERO                                      EL651
02100          MOVE AHTBENI            TO  WS-CHECK-BEN-CODE            EL651
02101          IF VALID-BEN-CODE                                        EL651
02102              MOVE AL-UANON       TO  AHTBENA                      EL651
02103          ELSE                                                     EL651
02104              MOVE -1             TO  AHTBENL                      EL651
02105              MOVE AL-UABON       TO  AHTBENA                      EL651
02106              MOVE ER-2326        TO  EMI-ERROR                    EL651
02107              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02108      ELSE                                                         EL651
02109          IF ADD-FUNCTION                                          EL651
02110              MOVE SPACE          TO  AHTBENO                      EL651
02111              MOVE AL-UANON       TO  AHTBENA.                     EL651
02112                                                                   EL651
02113      IF AHSTATEL GREATER ZERO                                     EL651
02114          MOVE SPACES             TO  WS-ACCESS                    EL651
02115          MOVE AHSTATEI           TO  WS-STATE                     EL651
02116          PERFORM 7050-CHECK-STATE THRU 7050-EXIT                  EL651
02117          IF STATE-FOUND                                           EL651
02118              MOVE AL-UANON       TO  AHSTATEA                     EL651
02119          ELSE                                                     EL651
02120              IF AHSTATEI = SPACES                                 EL651
02121                  MOVE AL-UANON   TO  AHSTATEA                     EL651
02122              ELSE                                                 EL651
02123                  MOVE -1         TO  AHSTATEL                     EL651
02124                  MOVE AL-UABON   TO  AHSTATEA                     EL651
02125                  MOVE ER-2335    TO  EMI-ERROR                    EL651
02126                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL651
02127                                                                   EL651
02128      IF AHBENL GREATER ZERO                                       EL651
02129          IF AHBENI NUMERIC                                        EL651
02130              MOVE AL-UNNON       TO  AHBENA                       EL651
02131          ELSE                                                     EL651
02132              MOVE -1             TO  AHBENL                       EL651
02133              MOVE AL-UNBON       TO  AHBENA                       EL651
02134              MOVE ER-2331        TO  EMI-ERROR                    EL651
02135              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02136      ELSE                                                         EL651
02137          IF ADD-FUNCTION                                          EL651
02138              MOVE ZEROS          TO  AHBENO                       EL651
02139              MOVE AL-UNNON       TO  AHBENA.                      EL651
02140                                                                   EL651
02141      MOVE ZEROS                  TO  WS-PREV-AMT.                 EL651
02142                                                                   EL651
02143      IF AHLOAMTL GREATER ZERO                                     EL651
02144          MOVE AHLOAMTI           TO  DEEDIT-FIELD                 EL651
02145          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
02146          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
02147              MOVE AL-UNNON       TO  AHLOAMTA                     EL651
02148              MOVE DEEDIT-FIELD-V1 TO WS-PREV-AMT                  EL651
02149                                      AHLOAMTO                     EL651
02150                                      WS-AHBEN-LO                  EL651
02151          ELSE                                                     EL651
02152              MOVE -1             TO  AHLOAMTL                     EL651
02153              MOVE AL-UNBON       TO  AHLOAMTA                     EL651
02154              MOVE ER-2328        TO  EMI-ERROR                    EL651
02155              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02156      ELSE                                                         EL651
02157          IF ADD-FUNCTION                                          EL651
02158              MOVE ZEROS          TO  AHLOAMTO                     EL651
02159                                      WS-AHBEN-LO                  EL651
02160              MOVE AL-UNNON       TO  AHLOAMTA.                    EL651
02161                                                                   EL651
02162      IF AHHIAMTL GREATER ZERO                                     EL651
02163          MOVE AHHIAMTI           TO  DEEDIT-FIELD                 EL651
02164          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
02165          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
02166              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT                  EL651
02167                  MOVE -1         TO  AHHIAMTL                     EL651
02168                  MOVE AL-UNBON   TO  AHHIAMTA                     EL651
02169                  MOVE ER-2329    TO  EMI-ERROR                    EL651
02170                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
02171              ELSE                                                 EL651
02172                  MOVE AL-UNNON   TO  AHHIAMTA                     EL651
02173                  MOVE DEEDIT-FIELD-V1    TO  AHHIAMTO             EL651
02174                                              WS-AHBEN-HI          EL651
02175          ELSE                                                     EL651
02176              MOVE -1             TO  AHHIAMTL                     EL651
02177              MOVE AL-UNBON       TO  AHHIAMTA                     EL651
02178              MOVE ER-2330        TO  EMI-ERROR                    EL651
02179              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02180      ELSE                                                         EL651
02181          IF ADD-FUNCTION                                          EL651
02182              MOVE NINES          TO  AHHIAMTO                     EL651
02183                                      WS-AHBEN-HI                  EL651
02184              MOVE AL-UNNON       TO  AHHIAMTA.                    EL651
02185                                                                   EL651
02186                                                                   EL651
02187      MOVE ZEROS                  TO  WS-PREV-AMT.                 EL651
02188                                                                   EL651
02189      IF AMLOAMTL GREATER ZERO                                     EL651
02190          MOVE AMLOAMTI           TO  DEEDIT-FIELD                 EL651
02191          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
02192          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
02193              MOVE AL-UNNON       TO  AMLOAMTA                     EL651
02194              MOVE DEEDIT-FIELD-V1    TO  WS-PREV-AMT              EL651
02195                                          AMLOAMTO                 EL651
02196                                          WS-AHMOA-LO              EL651
02197          ELSE                                                     EL651
02198              MOVE -1             TO  AMLOAMTL                     EL651
02199              MOVE AL-UNBON       TO  AMLOAMTA                     EL651
02200              MOVE ER-2328        TO  EMI-ERROR                    EL651
02201              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02202      ELSE                                                         EL651
02203          IF ADD-FUNCTION                                          EL651
02204              MOVE ZEROS          TO  AMLOAMTO                     EL651
02205                                      WS-AHMOA-LO                  EL651
02206              MOVE AL-UNNON       TO  AMLOAMTA.                    EL651
02207                                                                   EL651
02208      IF AMHIAMTL GREATER ZERO                                     EL651
02209          MOVE AMHIAMTI           TO  DEEDIT-FIELD                 EL651
02210          PERFORM 7500-DEEDIT THRU 7500-EXIT                       EL651
02211          IF DEEDIT-FIELD-V0 NUMERIC                               EL651
02212              IF DEEDIT-FIELD-V1 LESS WS-PREV-AMT                  EL651
02213                  MOVE -1         TO  AMHIAMTL                     EL651
02214                  MOVE AL-UNBON   TO  AMHIAMTA                     EL651
02215                  MOVE ER-2329    TO  EMI-ERROR                    EL651
02216                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
02217              ELSE                                                 EL651
02218                  MOVE AL-UNNON   TO  AMHIAMTA                     EL651
02219                  MOVE DEEDIT-FIELD-V1    TO  AMHIAMTO             EL651
02220                                              WS-AHMOA-HI          EL651
02221          ELSE                                                     EL651
02222              MOVE -1             TO  AMHIAMTL                     EL651
02223              MOVE AL-UNBON       TO  AMHIAMTA                     EL651
02224              MOVE ER-2330        TO  EMI-ERROR                    EL651
02225              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02226      ELSE                                                         EL651
02227          IF ADD-FUNCTION                                          EL651
02228              MOVE NINES          TO  AMHIAMTO                     EL651
02229                                      WS-AHMOA-HI                  EL651
02230              MOVE AL-UNNON       TO  AMHIAMTA.                    EL651
02231                                                                   EL651
02232      IF WS-LIFE-AGE-TRMS = ZEROS                                  EL651
02233          IF WS-LIFE-BEN-AMTS NOT = ZEROS                          EL651
02234              MOVE -1             TO  LFLOAGEL                     EL651
02235              MOVE ER-2112        TO  EMI-ERROR                    EL651
02236              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02237              MOVE AL-UNBON       TO  LFLOAGEA  LFHIAGEA           EL651
02238                                      LFLOTRMA  LFHITRMA           EL651
02239                                      LFLOBENA  LFHIBENA           EL651
02240                                      LFLOAMTA  LFHIAMTA.          EL651
02241                                                                   EL651
02242      IF WS-AH-AGE-TRMS = ZEROS                                    EL651
02243          IF WS-AH-BEN-AMTS NOT = ZEROS                            EL651
02244              MOVE -1             TO  AHLOAGEL                     EL651
02245              MOVE ER-2112        TO  EMI-ERROR                    EL651
02246              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02247              MOVE AL-UNBON       TO  AHLOAGEA  AHHIAGEA           EL651
02248                                      AHLOTRMA  AHHITRMA           EL651
02249                                      AHLOBENA  AHHIBENA           EL651
02250                                      AHLOAMTA  AHHIAMTA           EL651
02251                                      AMLOBENA  AMHIBENA           EL651
02252                                      AMLOAMTA  AMHIAMTA.          EL651
02253                                                                   EL651
02254  7000-EXIT.                                                       EL651
02255      EXIT.                                                        EL651
02256      EJECT                                                        EL651
02257  7050-CHECK-STATE.                                                EL651
02258      MOVE SPACES                 TO  WS-STATE-FOUND-SW            EL651
02259                                      ELCNTL-KEY.                  EL651
02260      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL651
02261      MOVE '3'                    TO  CNTL-REC-TYPE.               EL651
02262      MOVE WS-ACCESS              TO  CNTL-ACCESS.                 EL651
02263      MOVE +0                     TO  CNTL-SEQ-NO.                 EL651
02264                                                                   EL651
02265      EXEC CICS HANDLE CONDITION                                   EL651
02266          NOTFND   (7050-EXIT)                                     EL651
02267      END-EXEC.                                                    EL651
02268                                                                   EL651
02269      EXEC CICS READ                                               EL651
02270          DATASET   (CNTL-FILE-ID)                                 EL651
02271          SET       (ADDRESS OF CONTROL-FILE)                         CL*16
02272          RIDFLD    (ELCNTL-KEY)                                   EL651
02273      END-EXEC.                                                    EL651
02274                                                                   EL651
02275      MOVE 'Y'                    TO  WS-STATE-FOUND-SW.           EL651
02276                                                                   EL651
02277  7050-EXIT.                                                       EL651
02278      EXIT.                                                        EL651
02279      EJECT                                                        EL651
02280  7100-PAGE-TABLE-FORWARD.                                         EL651
02281      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.            EL651
02282                                                                   EL651
02283      IF TABLEL GREATER ZERO                                       EL651
02284          MOVE ZERO TO TALLY                                          CL*16
02285          INSPECT TABLEI TALLYING TALLY FOR ALL SPACES                CL*16
02286          IF TALLY GREATER ZERO                                    EL651
02287              MOVE -1             TO  TABLEL                       EL651
02288              MOVE AL-UABON       TO  TABLEA                       EL651
02289              MOVE ER-2341        TO  EMI-ERROR                    EL651
02290              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02291              GO TO 8200-SEND-DATAONLY                             EL651
02292          ELSE                                                     EL651
02293              MOVE AL-UANON       TO  TABLEA                       EL651
02294              MOVE TABLEI         TO  PI-ERR-TABLE                 EL651
02295      ELSE                                                         EL651
02296          MOVE LOW-VALUES         TO  PI-ERREIN-KEY                EL651
02297          MOVE  'Y'               TO  PI-FIRST-TIME-SW.            EL651
02298                                                                   EL651
02299      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.           EL651
02300      MOVE 'A'                    TO  PI-ERR-CODE.                 EL651
02301      MOVE PI-ERREIN-KEY          TO  WS-SAVE-KEY.                 EL651
02302                                                                   EL651
02303      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL651
02304                                                                   EL651
02305      EXEC CICS HANDLE CONDITION                                   EL651
02306          ENDFILE  (7150-ENDFILE)                                  EL651
02307          NOTFND   (7175-NOTFOUND)                                 EL651
02308          END-EXEC.                                                EL651
02309                                                                   EL651
02310  7110-READ-NEXT.                                                  EL651
02311      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL651
02312                                                                   EL651
02313      IF ERREIN-EOF                                                EL651
02314          GO TO 7150-ENDFILE.                                      EL651
02315                                                                   EL651
02316      IF PI-ERREIN-KEY = WS-SAVE-KEY                               EL651
02317          GO TO 7110-READ-NEXT.                                    EL651
02318                                                                   EL651
02319      IF PI-CARRIER-SECURITY GREATER SPACE                         EL651
02320          IF PI-CARRIER-SECURITY = RE-TABLE-CARRIER-SECURITY       EL651
02321              NEXT SENTENCE                                        EL651
02322          ELSE                                                     EL651
02323              GO TO 7110-READ-NEXT.                                EL651
02324                                                                   EL651
02325      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL651
02326      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL651
02327                                                                   EL651
02328      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.          EL651
02329                                                                   EL651
02330      MOVE LOW-VALUES             TO  EL651AO.                     EL651
02331                                                                   EL651
02332      MOVE +1                     TO  PI-SUB.                      EL651
02333                                                                   EL651
02334      GO TO 5050-SET-UP-SCREEN.                                    EL651
02335                                                                   EL651
02336  7150-ENDFILE.                                                    EL651
02337      IF BROWSE-STARTED                                            EL651
02338          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL651
02339                                                                   EL651
02340      IF FIRST-TIME                                                EL651
02341          MOVE LOW-VALUES         TO  EL651AO                      EL651
02342          MOVE ER-2346            TO  EMI-ERROR                    EL651
02343          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
02344          MOVE -1                 TO  MAINTYPL                     EL651
02345          GO TO 8100-SEND-INITIAL-MAP                              EL651
02346      ELSE                                                         EL651
02347          MOVE ER-2067            TO  EMI-ERROR                    EL651
02348          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
02349          MOVE LOW-VALUES         TO  EL651AO                      EL651
02350          GO TO 7100-PAGE-TABLE-FORWARD.                           EL651
02351                                                                   EL651
02352  7175-NOTFOUND.                                                   EL651
02353      IF BROWSE-STARTED                                            EL651
02354          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL651
02355                                                                   EL651
02356      GO TO 8880-NOT-FOUND.                                        EL651
02357                                                                   EL651
02358  7199-EXIT.                                                       EL651
02359      EXIT.                                                        EL651
02360      EJECT                                                        EL651
02361  7200-PAGE-TABLE-BACKWARD.                                        EL651
02362      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.            EL651
02363                                                                   EL651
02364      IF TABLEL GREATER ZERO                                       EL651
02365          MOVE ZERO TO TALLY                                          CL*16
02366          INSPECT TABLEI TALLYING TALLY FOR ALL SPACES                CL*16
02367          IF TALLY GREATER ZERO                                    EL651
02368              MOVE -1             TO  TABLEL                       EL651
02369              MOVE AL-UABON       TO  TABLEA                       EL651
02370              MOVE ER-2341        TO  EMI-ERROR                    EL651
02371              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02372              GO TO 8200-SEND-DATAONLY                             EL651
02373          ELSE                                                     EL651
02374              MOVE AL-UANON       TO  TABLEA                       EL651
02375              MOVE TABLEI         TO  PI-ERR-TABLE                 EL651
02376              MOVE LOW-VALUES     TO  PI-ERR-TABLE-SUB             EL651
02377      ELSE                                                         EL651
02378          GO TO 7275-NOTFOUND.                                     EL651
02379                                                                   EL651
02380      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.           EL651
02381      MOVE 'A'                    TO  PI-ERR-CODE.                 EL651
02382      MOVE PI-ERREIN-KEY          TO  WS-SAVE-KEY.                 EL651
02383                                                                   EL651
02384      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL651
02385                                                                   EL651
02386      EXEC CICS HANDLE CONDITION                                   EL651
02387          ENDFILE  (7250-ENDFILE)                                  EL651
02388          NOTFND   (7275-NOTFOUND)                                 EL651
02389      END-EXEC.                                                    EL651
02390                                                                   EL651
02391      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL651
02392                                                                   EL651
02393      IF ERREIN-EOF                                                EL651
02394          GO TO 7250-ENDFILE.                                      EL651
02395                                                                   EL651
02396  7210-READ-PREV.                                                  EL651
02397      PERFORM 7900-READPREV THRU 7900-EXIT.                        EL651
02398                                                                   EL651
02399      IF ERREIN-EOF                                                EL651
02400          GO TO 7250-ENDFILE.                                      EL651
02401                                                                   EL651
02402      IF PI-ERREIN-KEY = WS-SAVE-KEY                               EL651
02403          GO TO 7210-READ-PREV.                                    EL651
02404                                                                   EL651
02405      IF PI-CARRIER-SECURITY GREATER SPACE                         EL651
02406         IF PI-CARRIER-SECURITY NOT = RE-TABLE-CARRIER-SECURITY    EL651
02407              GO TO 7210-READ-PREV.                                EL651
02408                                                                   EL651
02409      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL651
02410      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL651
02411                                                                   EL651
02412      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.          EL651
02413                                                                   EL651
02414      MOVE LOW-VALUES             TO  EL651AO.                     EL651
02415                                                                   EL651
02416      MOVE +1                     TO  PI-SUB.                      EL651
02417                                                                   EL651
02418      GO TO 5050-SET-UP-SCREEN.                                    EL651
02419                                                                   EL651
02420  7250-ENDFILE.                                                    EL651
02421      IF BROWSE-STARTED                                            EL651
02422          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL651
02423                                                                   EL651
02424      MOVE ER-2067                TO  EMI-ERROR                    EL651
02425      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     EL651
02426      MOVE LOW-VALUES             TO  EL651AO                      EL651
02427      GO TO 7100-PAGE-TABLE-FORWARD.                               EL651
02428                                                                   EL651
02429  7275-NOTFOUND.                                                   EL651
02430      IF BROWSE-STARTED                                            EL651
02431          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL651
02432                                                                   EL651
02433      GO TO 8880-NOT-FOUND.                                        EL651
02434                                                                   EL651
02435  7299-EXIT.                                                       EL651
02436      EXIT.                                                        EL651
02437      EJECT                                                        EL651
02438  7300-PAGE-LEVEL.                                                 EL651
02439      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.            EL651
02440                                                                   EL651
02441      IF TABLEL GREATER ZERO                                       EL651
02442          MOVE ZERO TO TALLY                                          CL*16
02443          INSPECT TABLEI TALLYING TALLY FOR ALL SPACES                CL*16
02444          IF TALLY GREATER ZERO                                    EL651
02445              MOVE -1             TO  TABLEL                       EL651
02446              MOVE AL-UABON       TO  TABLEA                       EL651
02447              MOVE ER-2341        TO  EMI-ERROR                    EL651
02448              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02449              GO TO 8200-SEND-DATAONLY                             EL651
02450          ELSE                                                     EL651
02451              MOVE AL-UANON       TO  TABLEA                       EL651
02452              MOVE TABLEI         TO  PI-ERR-TABLE                 EL651
02453              MOVE LOW-VALUES     TO  PI-ERR-TABLE-SUB             EL651
02454      ELSE                                                         EL651
02455          MOVE -1                 TO  TABLEL                       EL651
02456          MOVE AL-UABON           TO  TABLEA                       EL651
02457          MOVE ER-2140            TO  EMI-ERROR                    EL651
02458          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL651
02459          GO TO 8200-SEND-DATAONLY.                                EL651
02460                                                                   EL651
02461      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.           EL651
02462      MOVE 'A'                    TO  PI-ERR-CODE.                 EL651
02463                                                                   EL651
02464      PERFORM 7800-START-BROWSE THRU 7800-EXIT.                    EL651
02465                                                                   EL651
02466      EXEC CICS HANDLE CONDITION                                   EL651
02467          ENDFILE  (7350-ENDFILE)                                  EL651
02468          NOTFND   (7375-NOTFOUND)                                 EL651
02469      END-EXEC.                                                    EL651
02470                                                                   EL651
02471      PERFORM 7850-READNEXT THRU 7850-EXIT.                        EL651
02472                                                                   EL651
02473      IF ERREIN-EOF                                                EL651
02474          IF BROWSE-STARTED                                        EL651
02475              PERFORM 7950-END-BROWSE THRU 7950-EXIT               EL651
02476              GO TO 7100-PAGE-TABLE-FORWARD.                       EL651
02477                                                                   EL651
02478      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL651
02479      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL651
02480                                                                   EL651
02481      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.          EL651
02482                                                                   EL651
02483      IF LEVELL GREATER ZERO                                       EL651
02484          MOVE LEVELI             TO  WS-CHECK-LEVEL               EL651
02485          IF VALID-LEVEL                                           EL651
02486              MOVE LEVELI         TO  PI-SUB                       EL651
02487              IF RE-REI-COMP (PI-SUB) = SPACES                     EL651
02488                  MOVE +1         TO  PI-SUB                       EL651
02489                  MOVE ER-2338    TO  EMI-ERROR                    EL651
02490                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL651
02491                  GO TO 5000-BUILD-INITIAL-SCREEN                  EL651
02492              ELSE                                                 EL651
02493                  MOVE LOW-VALUES TO  EL651AO                      EL651
02494                  GO TO 5050-SET-UP-SCREEN                         EL651
02495          ELSE                                                     EL651
02496              MOVE +1             TO  PI-SUB                       EL651
02497              MOVE ER-2338        TO  EMI-ERROR                    EL651
02498              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02499              GO TO 5000-BUILD-INITIAL-SCREEN.                     EL651
02500                                                                   EL651
02501      MOVE LOW-VALUES             TO  EL651AO.                     EL651
02502                                                                   EL651
02503      IF PAGE-LEVEL-FORWARD                                        EL651
02504          ADD +1                  TO  PI-SUB                       EL651
02505      ELSE                                                         EL651
02506          SUBTRACT +1 FROM PI-SUB.                                 EL651
02507                                                                   EL651
02508      IF PI-SUB GREATER +30 OR                                     EL651
02509         RE-REI-COMP (PI-SUB) = SPACES                             EL651
02510             MOVE +1              TO  PI-SUB                       EL651
02511             MOVE ER-2338         TO  EMI-ERROR                    EL651
02512             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              EL651
02513             GO TO 5000-BUILD-INITIAL-SCREEN                       EL651
02514      ELSE                                                         EL651
02515          IF PI-SUB LESS +1                                        EL651
02516              MOVE PI-LAST-LEVEL  TO  PI-SUB                       EL651
02517              MOVE ER-0592        TO  EMI-ERROR                    EL651
02518              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL651
02519                                                                   EL651
02520      GO TO 5050-SET-UP-SCREEN.                                    EL651
02521                                                                   EL651
02522  7350-ENDFILE.                                                    EL651
02523      IF BROWSE-STARTED                                            EL651
02524          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL651
02525                                                                   EL651
02526      MOVE ER-2067                TO  EMI-ERROR.                   EL651
02527      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
02528      MOVE LOW-VALUES             TO  EL651AO.                     EL651
02529      GO TO 7100-PAGE-TABLE-FORWARD.                               EL651
02530                                                                   EL651
02531  7375-NOTFOUND.                                                   EL651
02532      IF BROWSE-STARTED                                            EL651
02533          PERFORM 7950-END-BROWSE THRU 7950-EXIT.                  EL651
02534                                                                   EL651
02535      GO TO 8880-NOT-FOUND.                                        EL651
02536                                                                   EL651
02537  7399-EXIT.                                                       EL651
02538      EXIT.                                                        EL651
02539      EJECT                                                        EL651
02540                                                                   EL651
02541  7400-SEARCH-FOR-COMPANY.                                         EL651
02542      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.          EL651
02543      MOVE COMPANYI               TO  PI-ERR-TABLE.                EL651
02544      MOVE COMPSUBI               TO  PI-ERR-TABLE-SUB.            EL651
02545      MOVE 'B'                    TO  PI-ERR-CODE.                 EL651
02546                                                                   EL651
02547      EXEC CICS HANDLE CONDITION                                   EL651
02548          NOTOPEN  (9990-ABEND)                                    EL651
02549          NOTFND   (7425-NOT-FOUND)                                EL651
02550      END-EXEC.                                                    EL651
02551                                                                   EL651
02552      PERFORM 7650-READ-ERREIN THRU 7650-EXIT.                     EL651
02553                                                                   EL651
02554      MOVE PI-SAVE-ERREIN-KEY     TO  PI-ERREIN-KEY.               EL651
02555                                                                   EL651
02556      GO TO 7499-EXIT.                                             EL651
02557                                                                   EL651
02558  7425-NOT-FOUND.                                                  EL651
02559      PERFORM 7700-ERREIN-GETMAIN THRU 7700-EXIT.                  EL651
02560      MOVE '1'                    TO  WS-GETMAIN-SW.               EL651
02561                                                                   EL651
02562      MOVE ZEROS                  TO  RE-LF-FEE                    EL651
02563                                      RE-AH-FEE                    EL651
02564                                      RE-AH-PR-PCT                 EL651
02565                                      RE-AH-78-PCT                 EL651
02566                                      RE-LF-PR-PCT                 EL651
02567                                      RE-LF-78-PCT                 EL651
02568 *                                    RE-PR-PCT                    EL651
02569 *                                    RE-78-PCT                    EL651
02570                                      RE-LF-IBNR-PCT               EL651
02571                                      RE-AH-IBNR-PCT               EL651
02572                                      RE-CLM-INCURRED-LIM          EL651
02573                                      RE-LF-CLM-PCT                EL651
02574                                      RE-AH-CLM-PCT                EL651
02575                                      RE-LF-CLM-MAX                EL651
02576                                      RE-AH-CLM-MAX                EL651
02577                                      RE-LF-CEDING-FEE-BRACKETS    EL651
02578                                      RE-AH-CEDING-FEE-BRACKETS    EL651
CIDMOD                                     RE-EARNING-START-DT
CIDMOD                                     RE-EARNING-STOP-DT
CIDMOD                                     RE-CUSTODIAL-BAL
02579                                      RE-LAST-MAINT-HHMMSS.        EL651
02580                                                                      CL**4
02581      MOVE SPACES                 TO  RE-LF-FEE-METHOD                CL**4
02582                                      RE-AH-FEE-METHOD                CL**4
02583                                      RE-LF-FEE-BASIS                 CL**4
02584                                      RE-AH-FEE-BASIS.                CL**4
02585                                                                   EL651
02586      MOVE COMPANYI               TO  RE-COMP-PRIME                EL651
02587                                      PI-ERR-TABLE.                EL651
02588      MOVE COMPSUBI               TO  RE-COMP-SUB                  EL651
02589                                      PI-ERR-TABLE-SUB.            EL651
02590      MOVE 'B'                    TO  RE-CODE                      EL651
02591                                      PI-ERR-CODE.                 EL651
02592      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD            EL651
02593                                      RE-COMPANY-CD.               EL651
02594                                                                   EL651
02595      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.          EL651
02596      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.        EL651
02597      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL651
02598                                                                   EL651
02599      MOVE '5'                    TO  DC-OPTION-CODE.              EL651
02600      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL651
02601                                                                   EL651
02602      EXEC CICS LINK                                               EL651
02603          PROGRAM   (PGM-NAME)                                     EL651
02604          COMMAREA  (DATE-CONVERSION-DATA)                         EL651
02605          LENGTH    (DC-COMM-LENGTH)                               EL651
02606      END-EXEC.                                                    EL651
02607                                                                   EL651
02608      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT.            EL651
02609      MOVE PI-COMPANY-CD          TO  RE-COMPANY-CD.               EL651
02610      MOVE 'RE'                   TO  RE-RECORD-ID.                EL651
02611 *    MOVE REIN-FILE-ID           TO  FILE-ID.                     EL651
02612 *    MOVE 'A'                    TO  JP-RECORD-AREA               EL651
02613                                                                   EL651
02614      EXEC CICS WRITE                                              EL651
02615          DATASET (REIN-FILE-ID)                                   EL651
02616          FROM    (REINSURANCE-RECORD)                             EL651
02617          RIDFLD  (RE-CONTROL-PRIMARY)                             EL651
02618      END-EXEC.                                                    EL651
02619                                                                   EL651
02620 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.              EL651
02621                                                                   EL651
02622 *    PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
02623                                                                   EL651
02624      MOVE PI-SAVE-ERREIN-KEY     TO  PI-ERREIN-KEY.               EL651
02625      MOVE 'Y'                    TO  PI-COMPANY-ADD-SW.           EL651
02626                                                                   EL651
02627  7499-EXIT.                                                       EL651
02628      EXIT.                                                        EL651
02629      EJECT                                                        EL651
02630  7500-DEEDIT.                                                     EL651
02631      EXEC CICS BIF                                                EL651
02632           DEEDIT                                                  EL651
02633           FIELD  (DEEDIT-FIELD)                                   EL651
02634           LENGTH (15)                                             EL651
02635      END-EXEC.                                                    EL651
02636                                                                   EL651
02637  7500-EXIT.                                                       EL651
02638      EXIT.                                                        EL651
02639      EJECT                                                        EL651
02640  7550-CHECK-CARRIER.                                              EL651
02641      MOVE SPACES                 TO  WS-CARRIER-FOUND-SW          EL651
02642                                      ELCNTL-KEY.                  EL651
02643      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL651
02644      MOVE '6'                    TO  CNTL-REC-TYPE.               EL651
02645      MOVE CARRIER-ACCESS         TO  CNTL-ACCESS.                 EL651
02646      MOVE +0                     TO  CNTL-SEQ-NO.                 EL651
02647                                                                   EL651
02648      EXEC CICS HANDLE CONDITION                                   EL651
02649          NOTFND   (7550-EXIT)                                     EL651
02650      END-EXEC.                                                    EL651
02651                                                                   EL651
02652      EXEC CICS READ                                               EL651
02653          DATASET   (CNTL-FILE-ID)                                 EL651
02654          SET       (ADDRESS OF CONTROL-FILE)                         CL*16
02655          RIDFLD    (ELCNTL-KEY)                                   EL651
02656      END-EXEC.                                                    EL651
02657                                                                   EL651
02658      MOVE 'Y'                    TO  WS-CARRIER-FOUND-SW.         EL651
02659                                                                   EL651
02660  7550-EXIT.                                                       EL651
02661      EXIT.                                                        EL651
02662      EJECT                                                        EL651
02663  7650-READ-ERREIN.                                                EL651
02664      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.           EL651
02665                                                                   EL651
02666      EXEC CICS READ                                               EL651
02667           DATASET  (REIN-FILE-ID)                                 EL651
02668           SET      (ADDRESS OF REINSURANCE-RECORD)                   CL*16
02669           RIDFLD   (PI-ERREIN-KEY)                                EL651
02670      END-EXEC.                                                    EL651
02671                                                                   EL651
02672      CONTINUE.                                                       CL*16
02673                                                                   EL651
02674  7650-EXIT.                                                       EL651
02675      EXIT.                                                        EL651
02676      EJECT                                                        EL651
02677  7700-ERREIN-GETMAIN.                                             EL651
02678      EXEC CICS GETMAIN                                            EL651
02679           SET     (ADDRESS OF REINSURANCE-RECORD)                    CL*16
02680           LENGTH  (ERREIN-LENGTH)                                 EL651
02681           INITIMG (GETMAIN-SPACE)                                 EL651
02682      END-EXEC.                                                    EL651
02683                                                                   EL651
02684      CONTINUE.                                                       CL*16
02685                                                                   EL651
02686  7700-EXIT.                                                       EL651
02687      EXIT.                                                        EL651
02688      EJECT                                                        EL651
02689  7750-READ-ERREIN-UPDATE.                                         EL651
02690      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.           EL651
02691      MOVE 'A'                    TO  PI-ERR-CODE.                 EL651
02692                                                                   EL651
02693      EXEC CICS READ                                               EL651
02694           DATASET  (REIN-FILE-ID)                                 EL651
02695           SET      (ADDRESS OF REINSURANCE-RECORD)                   CL*16
02696           RIDFLD   (PI-ERREIN-KEY)                                EL651
02697           UPDATE                                                  EL651
02698      END-EXEC.                                                    EL651
02699                                                                   EL651
02700      CONTINUE.                                                       CL*16
02701                                                                   EL651
02702                                                                   EL651
02703  7750-EXIT.                                                       EL651
02704      EXIT.                                                        EL651
02705      EJECT                                                        EL651
02706  7800-START-BROWSE.                                               EL651
02707      EXEC CICS STARTBR                                            EL651
02708           DATASET  (REIN-FILE-ID)                                 EL651
02709           RIDFLD   (PI-ERREIN-KEY)                                EL651
02710      END-EXEC.                                                    EL651
02711                                                                   EL651
02712      MOVE 'Y'                    TO  PI-BROWSE-SW.                EL651
02713                                                                   EL651
02714  7800-EXIT.                                                       EL651
02715      EXIT.                                                        EL651
02716      EJECT                                                        EL651
02717  7850-READNEXT.                                                   EL651
02718      EXEC CICS READNEXT                                           EL651
02719           DATASET  (REIN-FILE-ID)                                 EL651
02720           SET      (ADDRESS OF REINSURANCE-RECORD)                   CL*16
02721           RIDFLD   (PI-ERREIN-KEY)                                EL651
02722      END-EXEC.                                                    EL651
02723                                                                   EL651
02724      CONTINUE.                                                       CL*16
02725                                                                   EL651
02726      IF PI-COMPANY-CD NOT = RE-COMPANY-CD OR                      EL651
02727         PI-ERR-CODE NOT = 'A'                                     EL651
02728          MOVE LOW-VALUES         TO  EL651AO                      EL651
02729          MOVE 'Y'                TO  PI-ERREIN-EOF-SW             EL651
02730          IF FIRST-TIME                                            EL651
02731              MOVE ER-2346        TO  EMI-ERROR                    EL651
02732          ELSE                                                     EL651
02733              MOVE ER-2067        TO  EMI-ERROR                    EL651
02734      ELSE                                                         EL651
02735          GO TO 7850-EXIT.                                         EL651
02736                                                                   EL651
02737      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
02738                                                                   EL651
02739  7850-EXIT.                                                       EL651
02740      EXIT.                                                        EL651
02741      EJECT                                                        EL651
02742  7900-READPREV.                                                   EL651
02743      EXEC CICS READPREV                                           EL651
02744           DATASET  (REIN-FILE-ID)                                 EL651
02745           SET      (ADDRESS OF REINSURANCE-RECORD)                   CL*16
02746           RIDFLD   (PI-ERREIN-KEY)                                EL651
02747      END-EXEC.                                                    EL651
02748                                                                   EL651
02749      CONTINUE.                                                       CL*16
02750                                                                   EL651
02751      IF PI-COMPANY-CD NOT = RE-COMPANY-CD OR                      EL651
02752         PI-ERR-CODE NOT = 'A'                                     EL651
02753          MOVE LOW-VALUES         TO  EL651AO                      EL651
02754          MOVE 'Y'                TO  PI-ERREIN-EOF-SW             EL651
02755          IF FIRST-TIME                                            EL651
02756              MOVE ER-2346        TO  EMI-ERROR                    EL651
02757              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL651
02758          ELSE                                                     EL651
02759              MOVE ER-2067        TO  EMI-ERROR                    EL651
02760              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL651
02761                                                                   EL651
02762  7900-EXIT.                                                       EL651
02763      EXIT.                                                        EL651
02764      EJECT                                                        EL651
02765  7950-END-BROWSE.                                                 EL651
02766      EXEC CICS ENDBR                                              EL651
02767           DATASET  (REIN-FILE-ID)                                 EL651
02768      END-EXEC.                                                    EL651
02769                                                                   EL651
02770      MOVE SPACE                  TO  PI-BROWSE-SW.                EL651
02771                                                                   EL651
02772  7950-EXIT.                                                       EL651
02773      EXIT.                                                        EL651
02774      EJECT                                                        EL651
02775  8000-UPDATE-MAINT-DATE.                                          EL651
02776      MOVE SPACES                 TO  ELCNTL-KEY.                  EL651
02777                                                                   EL651
02778      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL651
02779      MOVE '1'                    TO  CNTL-REC-TYPE.               EL651
02780      MOVE +0                     TO  CNTL-SEQ-NO.                 EL651
02781                                                                   EL651
02782      EXEC CICS HANDLE CONDITION                                   EL651
02783          NOTFND   (8000-EXIT)                                     EL651
02784      END-EXEC.                                                    EL651
02785                                                                   EL651
02786      EXEC CICS READ                                               EL651
02787          UPDATE                                                   EL651
02788          DATASET   (CNTL-FILE-ID)                                 EL651
02789          SET       (ADDRESS OF CONTROL-FILE)                         CL*16
02790          RIDFLD    (ELCNTL-KEY)                                   EL651
02791      END-EXEC.                                                    EL651
02792                                                                   EL651
02793      CONTINUE.                                                       CL*16
02794                                                                   EL651
02795      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL651
02796      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL651
02797      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL651
02798      MOVE ELCNTL-LENGTH          TO  FILE-LENGTH.                 EL651
02799      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
02800                                                                   EL651
02801      MOVE BIN-CURRENT-SAVE       TO  CF-REINSURANCE-TAB-MAINT-DT. EL651
02802                                                                   EL651
02803      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL651
02804      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL651
02805      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL651
02806      MOVE ELCNTL-LENGTH          TO  FILE-LENGTH.                 EL651
02807                                                                   EL651
02808      EXEC CICS REWRITE                                            EL651
02809          DATASET   (CNTL-FILE-ID)                                 EL651
02810          FROM      (CONTROL-FILE)                                 EL651
02811      END-EXEC.                                                    EL651
02812                                                                   EL651
02813      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL651
02814                                                                   EL651
02815  8000-EXIT.                                                       EL651
02816       EXIT.                                                       EL651
02817      EJECT                                                        EL651
02818                                                                   EL651
02819  8100-SEND-INITIAL-MAP.                                           EL651
02820      MOVE SAVE-DATE              TO  RUNDATEO.                    EL651
02821      MOVE EIBTIME                TO  TIME-IN.                     EL651
02822      MOVE TIME-OUT               TO  RUNTIMEO.                    EL651
02823      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL651
02824      MOVE -1                     TO  MAINTYPL.                    EL651
02825      MOVE PI-LIFE-OVERRIDE-L2    TO  LIFEHDO.                     EL651
02826      MOVE PI-AH-OVERRIDE-L2      TO  AHHD1O  AHHD2O.              EL651
02827      MOVE AL-SANON               TO  LIFEHDA  AHHD1A  AHHD2A.     EL651
02828                                                                   EL651
02829      EXEC CICS SEND                                               EL651
02830          MAP     (MAP-NAME)                                       EL651
02831          MAPSET  (MAPSET-NAME)                                    EL651
02832          FROM    (EL651AO)                                        EL651
02833          ERASE                                                    EL651
02834          CURSOR                                                   EL651
02835      END-EXEC.                                                    EL651
02836                                                                   EL651
02837      GO TO 9100-RETURN-TRAN.                                      EL651
02838                                                                   EL651
02839  8200-SEND-DATAONLY.                                              EL651
02840      MOVE SAVE-DATE              TO  RUNDATEO.                    EL651
02841      MOVE EIBTIME                TO  TIME-IN.                     EL651
02842      MOVE TIME-OUT               TO  RUNTIMEO.                    EL651
02843      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL651
02844      MOVE PI-LIFE-OVERRIDE-L2    TO  LIFEHDO.                     EL651
02845      MOVE PI-AH-OVERRIDE-L2      TO  AHHD1O  AHHD2O.              EL651
02846      MOVE AL-SANON               TO  LIFEHDA  AHHD1A  AHHD2A.     EL651
02847                                                                   EL651
02848      EXEC CICS SEND                                               EL651
02849          MAP     (MAP-NAME)                                       EL651
02850          MAPSET  (MAPSET-NAME)                                    EL651
02851          FROM    (EL651AO)                                        EL651
02852          DATAONLY                                                 EL651
02853          CURSOR                                                   EL651
02854      END-EXEC.                                                    EL651
02855                                                                   EL651
02856      GO TO 9100-RETURN-TRAN.                                      EL651
02857                                                                   EL651
02858  8300-SEND-TEXT.                                                  EL651
02859      EXEC CICS SEND TEXT                                          EL651
02860          FROM    (LOGOFF-TEXT)                                    EL651
02861          LENGTH  (LOGOFF-LENGTH)                                  EL651
02862          ERASE                                                    EL651
02863          FREEKB                                                   EL651
02864      END-EXEC.                                                    EL651
02865                                                                   EL651
02866      EXEC CICS RETURN                                             EL651
02867      END-EXEC.                                                    EL651
02868                                                                   EL651
02869  8400-LOG-JOURNAL-RECORD.                                         EL651
02870      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL651
02871      MOVE FILE-ID                TO  JP-FILE-ID.                  EL651
02872      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL651
02873 *    IF PI-JOURNAL-FILE-ID NOT = ZERO                             EL651
02874 *        EXEC CICS JOURNAL                                        EL651
02875 *            JFILEID  (PI-JOURNAL-FILE-ID)                        EL651
02876 *            JTYPEID  ('EL')                                      EL651
02877 *            FROM     (JOURNAL-RECORD)                            EL651
02878 *            LENGTH   (FILE-LENGTH)                               EL651
02879 *        END-EXEC.                                                EL651
02880                                                                   EL651
02881  8800-UNAUTHORIZED-ACCESS.                                        EL651
02882      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL651
02883      GO TO 8300-SEND-TEXT.                                        EL651
02884                                                                   EL651
02885  8810-PF23.                                                       EL651
02886      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL651
02887      MOVE XCTL-005               TO  PGM-NAME.                    EL651
02888      GO TO 9300-XCTL.                                             EL651
02889                                                                   EL651
02890  8870-NOTOPEN.                                                    EL651
02891      MOVE ER-2055                TO  EMI-ERROR.                   EL651
02892      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
02893      MOVE -1                     TO  PFENTERL.                    EL651
02894      IF EIBTRNID NOT = TRANS-ID                                   EL651
02895          GO TO 8100-SEND-INITIAL-MAP.                             EL651
02896                                                                   EL651
02897      GO TO 8200-SEND-DATAONLY.                                    EL651
02898                                                                   EL651
02899  8880-NOT-FOUND.                                                  EL651
02900      MOVE ER-0142                TO  EMI-ERROR.                   EL651
02901      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL651
02902      MOVE -1                     TO  MAINTYPL.                    EL651
02903      IF EIBTRNID NOT = TRANS-ID                                   EL651
02904          GO TO 8100-SEND-INITIAL-MAP.                             EL651
02905                                                                   EL651
02906      GO TO 8200-SEND-DATAONLY.                                    EL651
02907                                                                   EL651
02908  9000-RETURN-CICS.                                                EL651
02909      EXEC CICS RETURN                                             EL651
02910      END-EXEC.                                                    EL651
02911                                                                   EL651
02912  9100-RETURN-TRAN.                                                EL651
02913      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL651
02914      MOVE '651S'                 TO  PI-CURRENT-SCREEN-NO.        EL651
02915      EXEC CICS RETURN                                             EL651
02916          TRANSID   (TRANS-ID)                                     EL651
02917          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL651
02918          LENGTH    (PI-COMM-LENGTH)                               EL651
02919      END-EXEC.                                                    EL651
02920                                                                   EL651
02921  9200-RETURN-MAIN-MENU.                                           EL651
02922      MOVE XCTL-626               TO  PGM-NAME.                    EL651
02923      GO TO 9300-XCTL.                                             EL651
02924                                                                   EL651
02925  9300-XCTL.                                                       EL651
02926      EXEC CICS XCTL                                               EL651
02927          PROGRAM   (PGM-NAME)                                     EL651
02928          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL651
02929          LENGTH    (PI-COMM-LENGTH)                               EL651
02930      END-EXEC.                                                    EL651
02931                                                                   EL651
02932  9400-CLEAR.                                                      EL651
02933      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL651
02934      GO TO 9300-XCTL.                                             EL651
02935                                                                   EL651
02936  9500-PF12.                                                       EL651
02937      MOVE XCTL-010               TO  PGM-NAME.                    EL651
02938      GO TO 9300-XCTL.                                             EL651
02939                                                                   EL651
02940  9600-PGMID-ERROR.                                                EL651
02941      EXEC CICS HANDLE CONDITION                                   EL651
02942          PGMIDERR  (8300-SEND-TEXT)                               EL651
02943      END-EXEC.                                                    EL651
02944                                                                   EL651
02945      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL651
02946      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL651
02947      MOVE XCTL-005               TO  PGM-NAME.                    EL651
02948      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL651
02949      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL651
02950      GO TO 9300-XCTL.                                             EL651
02951                                                                   EL651
02952  9700-LINK-DATE-CONVERT.                                          EL651
02953      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL651
02954                                                                   EL651
02955      EXEC CICS LINK                                               EL651
02956          PROGRAM   (PGM-NAME)                                     EL651
02957          COMMAREA  (DATE-CONVERSION-DATA)                         EL651
02958          LENGTH    (DC-COMM-LENGTH)                               EL651
02959      END-EXEC.                                                    EL651
02960                                                                   EL651
02961  9700-EXIT.                                                       EL651
02962      EXIT.                                                        EL651
02963                                                                   EL651
02964  9900-ERROR-FORMAT.                                               EL651
02965      IF NOT EMI-ERRORS-COMPLETE                                   EL651
02966          MOVE LINK-001           TO  PGM-NAME                     EL651
02967          EXEC CICS LINK                                           EL651
02968              PROGRAM   (PGM-NAME)                                 EL651
02969              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL651
02970              LENGTH    (EMI-COMM-LENGTH)                          EL651
02971          END-EXEC.                                                EL651
02972                                                                   EL651
02973  9900-EXIT.                                                       EL651
02974      EXIT.                                                        EL651
02975                                                                   EL651
02976  9990-ABEND.                                                      EL651
02977      MOVE LINK-004               TO  PGM-NAME.                    EL651
02978      MOVE DFHEIBLK               TO  EMI-LINE1                    EL651
02979      EXEC CICS LINK                                               EL651
02980          PROGRAM   (PGM-NAME)                                     EL651
02981          COMMAREA  (EMI-LINE1)                                    EL651
02982          LENGTH    (72)                                           EL651
02983      END-EXEC.                                                    EL651
02984                                                                   EL651
02985      GO TO 8200-SEND-DATAONLY.                                    EL651
02986                                                                   EL651
02987      GOBACK.                                                      EL651
02988                                                                   EL651
02989  9995-SECURITY-VIOLATION.                                         EL651
02990             COPY ELCSCTP.                                         EL651
02991                                                                   EL651
02992  9995-EXIT.                                                       EL651
02993       EXIT.                                                       EL651
