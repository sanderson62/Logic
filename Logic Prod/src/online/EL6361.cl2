00001  IDENTIFICATION DIVISION.                                         02/26/96
00002                                                                   EL6361
00003  PROGRAM-ID.                 EL6361.                                 LV007
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/12/96 09:53:06.                    CL**7
00007 *                            VMOD=2.007.                             CL**7
00008 *                                                                 EL6361
00008 *                                                                 EL6361
00009 *AUTHOR.     LOGIC,INC.                                              CL**7
00010 *            DALLAS, TEXAS.                                          CL**7
00011                                                                   EL6361
00012 *DATE-COMPILED.                                                      CL**7
00013 *SECURITY.   *****************************************************   CL**7
00014 *            *                                                   *   CL**7
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00016 *            *                                                   *   CL**7
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00020 *            *                                                   *   CL**7
00021 *            *****************************************************   CL**7
00022                                                                   EL6361
00023 *REMARKS. TRANSACTION - EXJE - COMMISSION CHECK STUB TEXT            CL**7
00024 *                              MAINTENANCE.                          CL**7
00025                                                                   EL6361
00026  ENVIRONMENT DIVISION.                                            EL6361
00027                                                                   EL6361
00028      EJECT                                                        EL6361
00029  DATA DIVISION.                                                   EL6361
00030  WORKING-STORAGE SECTION.                                         EL6361
00031                                                                   EL6361
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL6361
00033  77  FILLER  PIC X(32)  VALUE '*    EL6361 WORKING STORAGE    *'. EL6361
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.007 *********'.    CL**7
00035                                                                   EL6361
00036     EJECT                                                         EL6361
00037                                                                   EL6361
00038                              COPY ELCSCTM.                           CL**2
00039                              COPY ELCSCRTY.                          CL**2
00040                                                                   EL6361
00041     EJECT                                                         EL6361
00042                                                                   EL6361
00043 ******************************************************************EL6361
00044 *                                                                *EL6361
00045 *              S T A N D A R D   A R E A S                       *EL6361
00046 *                                                                *EL6361
00047 ******************************************************************EL6361
00048                                                                   EL6361
00049  01  STANDARD-AREAS.                                              EL6361
00050      12  SC-ITEM                     PIC S9(4)   VALUE +1   COMP. EL6361
00051      12  GETMAIN-SPACE               PIC X       VALUE SPACE.     EL6361
00052      12  EL636B                      PIC X(8)    VALUE 'EL636B'.  EL6361
00053      12  MAPSET-EL6361S              PIC X(8)    VALUE 'EL6361S'. EL6361
00054      12  TRANS-EXJE                  PIC X(4)    VALUE 'EXJE'.    EL6361
00055      12  THIS-PGM                    PIC X(8)    VALUE 'EL6361'.  EL6361
00056      12  PGM-NAME                    PIC X(8).                    EL6361
00057      12  TIME-IN                     PIC S9(7).                   EL6361
00058      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6361
00059          16  FILLER                  PIC X.                       EL6361
00060          16  TIME-OUT                PIC 99V99.                   EL6361
00061          16  FILLER                  PIC X(2).                    EL6361
00062      12  LINK-EL001                  PIC X(8)    VALUE 'EL001'.   EL6361
00063      12  LINK-EL004                  PIC X(8)    VALUE 'EL004'.   EL6361
00064      12  XCTL-EL005                  PIC X(8)    VALUE 'EL005'.   EL6361
00065      12  XCTL-EL010                  PIC X(8)    VALUE 'EL010'.   EL6361
00066      12  XCTL-EL626                  PIC X(8)    VALUE 'EL626'.   EL6361
00067      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL6361
00068      12  FILE-ID-ERCMCK              PIC X(8)    VALUE 'ERCMCK'.  EL6361
00069      12  FILE-ID-ERCKWK              PIC X(8)    VALUE 'ERCKWK '. EL6361
00070      12  FILE-ID-ERCOMP              PIC X(8)    VALUE 'ERCOMP '. EL6361
00071      12  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL '.    CL**2
00072      12  WS-CURRENT-DT               PIC X(8)    VALUE SPACES.    EL6361
00073      12  WS-CURRENT-BIN-DT           PIC XX      VALUE SPACES.    EL6361
00074      12  QID.                                                     EL6361
00075          16  QID-TERM                PIC X(4)    VALUE SPACES.    EL6361
00076          16  FILLER                  PIC X(4)    VALUE '125D'.    EL6361
00077      12  WS-TIME                     PIC 9(6)    VALUE ZEROS.     EL6361
00078      12  WS-HR-MINS-SECS REDEFINES WS-TIME.                       EL6361
00079          16  WS-HR-MINS              PIC 99V99.                   EL6361
00080          16  FILLER                  PIC XX.                      EL6361
00081                                                                   EL6361
00082      EJECT                                                        EL6361
00083                                                                   EL6361
00084  01  WORK-AREA.                                                   EL6361
00085      12  DEEDIT-FIELD                PIC X(12).                   EL6361
00086      12  WS-SEQUENCE-NO              PIC S9(4)    VALUE +0 COMP.  EL6361
00087      12  WS-DETAMT OCCURS 5 TIMES    PIC S9(7)V99.                EL6361
00088      12  WS-DETAIL-AMOUNT            PIC S9(7)V99 VALUE +0 COMP-3.EL6361
00089      12  WS-UPDATE-SW                PIC X        VALUE SPACE.    EL6361
00090          88  WS-ENTRIES-UPDATED                   VALUE 'Y'.      EL6361
00091      12  WS-SUB1                     PIC S9(4)    VALUE +0 COMP.  EL6361
00092      12  WS-SUB2                     PIC S9(4)    VALUE +0 COMP.  EL6361
00093      12  WS-BROWSE-SW                PIC X        VALUE SPACE.    EL6361
00094          88  WS-BROWSE-STARTED                    VALUE 'Y'.      EL6361
00095      12  WS-FIRST-DETAIL-SW          PIC X        VALUE SPACE.    EL6361
00096          88  FIRST-DETAIL-READ                    VALUE 'Y'.      EL6361
00097      12  WS-HEADER-INFO.                                          EL6361
00098          16  WS-PAYEE-NAME           PIC X(30)    VALUE SPACES.   EL6361
00099          16  WS-PAYEE-ADDRESS-1      PIC X(30)    VALUE SPACES.   EL6361
00100          16  WS-PAYEE-ADDRESS-2      PIC X(30)    VALUE SPACES.   EL6361
00101          16  WS-PAYEE-CITY-ST        PIC X(30)    VALUE SPACES.   EL6361
00102          16  WS-PAYEE-ZIP-CODE.                                   EL6361
00103              20  WS-PAYEE-ZIP        PIC X(5)     VALUE SPACES.   EL6361
00104              20  WS-PAYEE-ZIP-EXT    PIC X(4)     VALUE SPACES.   EL6361
00105          16  WS-AMOUNT-PAID          PIC S9(7)V99 VALUE +0 COMP-3.EL6361
00106          16  WS-TOTAL-ENTRIES        PIC S9(3)    VALUE +0 COMP-3.EL6361
00107      12  WS-SEQ-NINES                PIC S9(4)    VALUE +9999     EL6361
00108                                                   COMP.           EL6361
00109      12  WS-PAY-SEQ-NUM              PIC S9(4)    VALUE +0 COMP.  EL6361
00110      12  WS-NOFIRST                  PIC S9(4)    VALUE +0 COMP.  EL6361
00111      12  WS-DELETE-SEQ-NO            PIC S9(4)    VALUE +0 COMP.  EL6361
00112      12  WS-DELETE-AMOUNT            PIC S9(7)V99 VALUE +0 COMP-3.EL6361
00113      12  WS-DELETE-COUNT             PIC S9(3)    VALUE +0 COMP-3.EL6361
00114      12  WS-SAVE-ACTION-CODE         PIC X        VALUE SPACES.   EL6361
00115          88 SHOW-FUNCTION            VALUE 'S'.                   EL6361
00116      12  WS-SAVE-STMT-DT             PIC XX       VALUE SPACES.   EL6361
00117                                                                   EL6361
00118      EJECT                                                        EL6361
00119                                                                   EL6361
00120  01  ERROR-MESSAGES.                                              EL6361
00121      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL6361
00122      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL6361
00123      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL6361
00124      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL6361
00125      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL6361
00126      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL6361
00127      12  ER-0194                 PIC X(4)  VALUE '0194'.          EL6361
00128      12  ER-0195                 PIC X(4)  VALUE '0195'.          EL6361
00129      12  ER-1883                 PIC X(4)  VALUE '1883'.             CL**2
00130      12  ER-2056                 PIC X(4)  VALUE '2056'.          EL6361
00131      12  ER-2132                 PIC X(4)  VALUE '2132'.          EL6361
00132      12  ER-2223                 PIC X(4)  VALUE '2223'.          EL6361
00133      12  ER-2251                 PIC X(4)  VALUE '2251'.          EL6361
00134      12  ER-2252                 PIC X(4)  VALUE '2252'.          EL6361
00135      12  ER-2800                 PIC X(4)  VALUE '2800'.          EL6361
00136      12  ER-3139                 PIC X(4)  VALUE '3139'.          EL6361
00137      12  ER-3147                 PIC X(4)  VALUE '3147'.          EL6361
00138      12  ER-3148                 PIC X(4)  VALUE '3148'.          EL6361
00139      12  ER-3149                 PIC X(4)  VALUE '3149'.          EL6361
00140      12  ER-3160                 PIC X(4)  VALUE '3160'.          EL6361
00141      12  ER-3161                 PIC X(4)  VALUE '3161'.          EL6361
00142      12  ER-3185                 PIC X(4)  VALUE '3185'.          EL6361
00143      12  ER-3186                 PIC X(4)  VALUE '3186'.          EL6361
00144      12  ER-3192                 PIC X(4)  VALUE '3192'.             CL**6
00145      12  ER-7043                 PIC X(4)  VALUE '7043'.          EL6361
00146                                                                   EL6361
00147      EJECT                                                        EL6361
00148                                                                   EL6361
00149  01  ACCESS-KEYS.                                                 EL6361
00150                                                                   EL6361
00151      12  ELCNTL-KEY.                                                 CL**2
00152          16  CNTL-COMP-ID          PIC X(3)       VALUE SPACES.      CL**2
00153          16  CNTL-REC-TYPE         PIC X          VALUE SPACES.      CL**2
00154          16  CNTL-ACCESS           PIC X(4)       VALUE SPACES.      CL**2
00155          16  CNTL-SEQ-NO           PIC S9(4)      VALUE +0  COMP.    CL**2
00156      12  ERCMCK-KEY.                                              EL6361
00157          16  ERCMCK-COMPANY-CD       PIC X          VALUE SPACES. EL6361
00158          16  ERCMCK-CSR              PIC X(4)       VALUE SPACES.    CL**2
00159          16  ERCMCK-CARRIER          PIC X          VALUE SPACES. EL6361
00160          16  ERCMCK-GROUPING         PIC X(6)       VALUE SPACES. EL6361
00161          16  ERCMCK-PAYEE            PIC X(10)      VALUE SPACES. EL6361
00162          16  ERCMCK-PAYEE-SEQ        PIC S9(4) COMP VALUE +0.     EL6361
00163          16  ERCMCK-SEQUENCE-NO      PIC S9(4) COMP VALUE +0.     EL6361
00164                                                                   EL6361
00165      12  SAVE-ERCMCK-CONTROL.                                     EL6361
00166          16  SVCMCK-COMPANY-CD       PIC X          VALUE SPACES. EL6361
00167          16  SVCMCK-CSR              PIC X(4)       VALUE SPACES.    CL**2
00168          16  SVCMCK-CARRIER          PIC X          VALUE SPACES. EL6361
00169          16  SVCMCK-GROUPING         PIC X(6)       VALUE SPACES. EL6361
00170          16  SVCMCK-PAYEE            PIC X(10)      VALUE SPACES. EL6361
00171          16  SVCMCK-PAYEE-SEQ        PIC S9(4) COMP VALUE +0.     EL6361
00172          16  SVCMCK-SEQUENCE-NO      PIC S9(4) COMP VALUE +0.     EL6361
00173                                                                   EL6361
00174      12  ERCKWK-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.   EL6361
00175                                                                   EL6361
00176      12  ERCKWK-KEY.                                              EL6361
00177          16  ERCKWK-COMPANY-CD       PIC X           VALUE SPACES.EL6361
00178          16  ERCKWK-CSR              PIC X(4)        VALUE SPACES.   CL**2
00179          16  ERCKWK-CARRIER          PIC X           VALUE SPACES.EL6361
00180          16  ERCKWK-GROUPING         PIC X(6)        VALUE SPACES.EL6361
00181          16  ERCKWK-PAYEE            PIC X(10)       VALUE SPACES.EL6361
00182          16  ERCKWK-PAYEE-SEQ        PIC S9(4)  COMP VALUE +0.    EL6361
00183          16  ERCKWK-SEQUENCE-NO      PIC S9(4)  COMP VALUE +0.    EL6361
00184                                                                   EL6361
00185      12  ERCKWK-COMPARE-KEY          PIC X(26)       VALUE SPACE.    CL**2
00186                                                                   EL6361
00187      12  SAVE-ERCKWK-CONTROL.                                     EL6361
00188          16  SVCKWK-COMPANY-CD       PIC X           VALUE SPACES.EL6361
00189          16  SVCKWK-CSR              PIC X(4)        VALUE SPACES.   CL**2
00190          16  SVCKWK-CARRIER          PIC X           VALUE SPACES.EL6361
00191          16  SVCKWK-GROUPING         PIC X(6)        VALUE SPACES.EL6361
00192          16  SVCKWK-PAYEE            PIC X(10)       VALUE SPACES.EL6361
00193          16  SVCKWK-PAYEE-SEQ        PIC S9(4) COMP  VALUE +0.    EL6361
00194          16  SVCKWK-SEQUENCE-NO      PIC S9(4) COMP  VALUE +0.    EL6361
00195                                                                   EL6361
00196      12  SVCKWK-COMPARE-KEY          PIC X(26)       VALUE SPACE.    CL**2
00197                                                                   EL6361
00198      12  ERCMCK-RECORD-LENGTH        PIC S9(4) COMP VALUE +2000.  EL6361
00199                                                                   EL6361
00200      12  ERCOMP-KEY.                                              EL6361
00201          16  ERCOMP-COMP-CD      PIC X     VALUE SPACE.           EL6361
00202          16  ERCOMP-CARRIER      PIC X     VALUE SPACES.          EL6361
00203          16  ERCOMP-GROUPING     PIC X(6)  VALUE SPACES.          EL6361
00204          16  ERCOMP-FIN-RESP     PIC X(10) VALUE SPACES.          EL6361
00205          16  ERCOMP-ACCT         PIC X(10) VALUE SPACES.          EL6361
00206          16  ERCOMP-RECORD-TYPE  PIC X     VALUE SPACES.          EL6361
00207                                                                   EL6361
00208      EJECT                                                        EL6361
00209                                                                   EL6361
00210                              COPY ELCDATE.                           CL**2
00211                                                                   EL6361
00212      EJECT                                                        EL6361
00213                              COPY ELCLOGOF.                          CL**2
00214                                                                   EL6361
00215      EJECT                                                        EL6361
00216                              COPY ELCATTR.                           CL**2
00217                                                                   EL6361
00218      EJECT                                                        EL6361
00219                              COPY ELCEMIB.                           CL**2
00220                                                                   EL6361
00221      EJECT                                                        EL6361
00222                              COPY ELCINTF.                           CL**2
00223      12  FILLER  REDEFINES PI-PROGRAM-WORK-AREA.                  EL6361
00224          16  PI-MAINT-FUNCTION       PIC X.                       EL6361
00225              88  PI-ADD-FUNCTION         VALUE 'A'.               EL6361
00226              88  PI-CHG-FUNCTION         VALUE 'C'.               EL6361
00227              88  PI-DEL-FUNCTION         VALUE 'D'.               EL6361
00228              88  PI-SHOW-FUNCTION        VALUE 'S'.               EL6361
00229          16  PI-ERCKWK-KEY.                                       EL6361
00230              20  PI-ERCKWK-COMP-CD   PIC X.                       EL6361
00231              20  PI-ERCKWK-CSR       PIC X(4).                       CL**2
00232              20  PI-ERCKWK-CARRIER   PIC X.                       EL6361
00233              20  PI-ERCKWK-GROUPING  PIC X(6).                    EL6361
00234              20  PI-ERCKWK-PAYEE     PIC X(10).                   EL6361
00235              20  PI-ERCKWK-PAYEE-SEQ PIC S9(4)   COMP.            EL6361
00236              20  PI-ERCKWK-SEQ-NO    PIC S9(4)   COMP.            EL6361
00237          16  PI-SAVE-ERCKWK-KEY.                                  EL6361
00238              20  PI-SVCKWK-COMP-CD   PIC X.                       EL6361
00239              20  PI-SVCKWK-CSR       PIC X(4).                       CL**2
00240              20  PI-SVCKWK-CARRIER   PIC X.                       EL6361
00241              20  PI-SVCKWK-GROUPING  PIC X(6).                    EL6361
00242              20  PI-SVCKWK-PAYEE     PIC X(10).                   EL6361
00243              20  PI-SVCKWK-PAYEE-SEQ PIC S9(4)   COMP.            EL6361
00244              20  PI-SVCKWK-SEQ-NO    PIC S9(4)   COMP.            EL6361
00245          16  PI-STUB-KEY OCCURS 5    PIC X(26).                      CL**2
00246          16  PI-NO-ENTRIES-DISPLAYED PIC S9.                      EL6361
00247          16  PI-PREV-FUNCTION        PIC X.                       EL6361
00248          16  PI-LAST-STUB-KEY.                                    EL6361
00249              20  FILLER              PIC X(24).                      CL**2
00250              20  PI-LAST-STUB-SEQ-NO PIC S9(4)   COMP.            EL6361
00251          16  FILLER                  PIC X(429).                     CL**7
00252                                                                   EL6361
00253      EJECT                                                        EL6361
00254                              COPY ELCJPFX.                           CL**2
00255                              PIC X(1583).                         EL6361
00256                                                                   EL6361
00257      EJECT                                                        EL6361
00258                              COPY ELCAID.                            CL**2
00259  01  FILLER    REDEFINES DFHAID.                                  EL6361
00260      12  FILLER              PIC X(8).                            EL6361
00261      12  PF-VALUES           PIC X       OCCURS 2.                EL6361
00262                                                                   EL6361
00263      EJECT                                                        EL6361
00264                              COPY EL6361S.                           CL**2
00265                                                                   EL6361
00266  01  DISPLAY-MAP REDEFINES EL636BI.                               EL6361
00267      12  FILLER                  PIC X(323).                         CL**2
00268      12  CK-STUB-TEXT OCCURS 3 TIMES INDEXED BY SCRN-NDX.         EL6361
00269          16  STUB-LEN            PIC S9(4)   COMP.                EL6361
00270          16  STUBA               PIC X.                           EL6361
00271          16  STUB                PIC X(70).                       EL6361
00272      12  FILLER                  PIC X(169).                         CL**7
00273      EJECT                                                        EL6361
00274                                                                   EL6361
00275                                                                   EL6361
00276      EJECT                                                        EL6361
00277  LINKAGE SECTION.                                                 EL6361
00278  01  DFHCOMMAREA             PIC X(1024).                         EL6361
00279                                                                   EL6361
00280      EJECT                                                        EL6361
00281 *01 PARMLIST .                                                       CL**7
00282 *    02  FILLER              PIC S9(8)   COMP.                       CL**7
00283 *    02  ERCKWK-POINTER      PIC S9(8)   COMP.                       CL**7
00284 *    02  ERCMCK-POINTER      PIC S9(8)   COMP.                       CL**7
00285 *    02  ERCOMP-POINTER      PIC S9(8)   COMP.                       CL**7
00286 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL**7
00287                                                                   EL6361
00288      EJECT                                                        EL6361
00289                                                                   EL6361
00290                                  COPY ERCCKWK.                       CL**2
00291      EJECT                                                        EL6361
00292                                                                   EL6361
00293                                  COPY ERCCMCK.                       CL**2
00294      EJECT                                                        EL6361
00295                                                                   EL6361
00296                                  COPY ERCCOMP.                       CL**2
00297      EJECT                                                        EL6361
00298                                                                   EL6361
00299                                  COPY ELCCNTL.                       CL**2
00300      EJECT                                                           CL**2
00301                                                                   EL6361
00302  PROCEDURE DIVISION.                                              EL6361
00303                                                                   EL6361
00304      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL6361
00305      MOVE 2                      TO EMI-NUMBER-OF-LINES.          EL6361
00306                                                                   EL6361
00307      IF EIBCALEN = 0                                              EL6361
00308          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6361
00309                                                                   EL6361
00310      MOVE EIBTRMID               TO QID-TERM.                     EL6361
00311      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL6361
00312      MOVE '5'                    TO DC-OPTION-CODE.               EL6361
00313      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL6361
00314      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL6361
00315      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                EL6361
00316      MOVE ZEROS                  TO WS-NOFIRST.                   EL6361
00317                                                                   EL6361
00318      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6361
00319          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6361
00320              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6361
00321              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6361
00322              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6361
00323              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6361
00324              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6361
00325              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6361
00326              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6361
00327              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL6361
00328          ELSE                                                     EL6361
00329              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6361
00330              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6361
00331              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6361
00332              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6361
00333              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6361
00334              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6361
00335              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6361
00336              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL6361
00337                                                                   EL6361
00338      MOVE LOW-VALUES             TO EL636BI.                      EL6361
00339                                                                   EL6361
00340      IF EIBTRNID NOT = TRANS-EXJE                                 EL6361
00341         IF PI-ERCKWK-CARRIER EQUAL LOW-VALUES OR SPACES           EL6361
00342             MOVE SPACES              TO PI-PROGRAM-WORK-AREA      EL6361
00343             MOVE PI-COMPANY-CD       TO PI-ERCKWK-COMP-CD         EL6361
00344             MOVE +0                  TO PI-ERCKWK-SEQ-NO          EL6361
00345             GO TO 8100-SEND-INITIAL-MAP                           EL6361
00346         ELSE                                                      EL6361
00347             MOVE LOW-VALUES          TO EL636BO                   EL6361
00348             MOVE 'S'                 TO MAINTO                    EL6361
00349             GO TO 5000-DISPLAY-TEXT.                              EL6361
00350                                                                   EL6361
00351      EXEC CICS HANDLE CONDITION                                   EL6361
00352          PGMIDERR  (9600-PGMID-ERROR)                             EL6361
00353          ERROR     (9990-ABEND)                                   EL6361
00354      END-EXEC.                                                    EL6361
00355                                                                   EL6361
00356      IF EIBAID = DFHCLEAR                                         EL6361
00357          GO TO 9400-CLEAR.                                        EL6361
00358                                                                   EL6361
00359      IF PI-PROCESSOR-ID = 'LGXX'                                  EL6361
00360          GO TO 0200-RECEIVE.                                      EL6361
00361                                                                   EL6361
00362      EXEC CICS READQ TS                                           EL6361
00363          QUEUE  (QID)                                             EL6361
00364          INTO   (SECURITY-CONTROL)                                EL6361
00365          LENGTH (SC-COMM-LENGTH)                                  EL6361
00366          ITEM   (SC-ITEM)                                         EL6361
00367      END-EXEC.                                                    EL6361
00368                                                                   EL6361
00369      MOVE SC-CREDIT-DISPLAY (6)   TO PI-DISPLAY-CAP.              EL6361
00370      MOVE SC-CREDIT-UPDATE  (6)   TO PI-MODIFY-CAP.               EL6361
00371                                                                   EL6361
00372      IF NOT DISPLAY-CAP                                           EL6361
00373          MOVE 'READ'          TO SM-READ                          EL6361
00374          PERFORM 9995-SECURITY-VIOLATION                          EL6361
00375          MOVE ER-0070         TO  EMI-ERROR                       EL6361
00376          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6361
00377          GO TO 8100-SEND-INITIAL-MAP.                             EL6361
00378                                                                   EL6361
00379      EJECT                                                        EL6361
00380                                                                   EL6361
00381  0200-RECEIVE.                                                    EL6361
00382                                                                   EL6361
00383      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6361
00384          MOVE ER-0008            TO EMI-ERROR                     EL6361
00385          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6361
00386          MOVE -1                 TO MAINTL                        EL6361
00387          GO TO 8200-SEND-DATAONLY.                                EL6361
00388                                                                   EL6361
00389      EXEC CICS RECEIVE                                            EL6361
00390          MAP      (EL636B)                                        EL6361
00391          MAPSET   (MAPSET-EL6361S)                                EL6361
00392          INTO     (EL636BI)                                       EL6361
00393      END-EXEC.                                                    EL6361
00394                                                                   EL6361
00395      IF PFENTERL GREATER THAN ZERO                                EL6361
00396         IF EIBAID NOT = DFHENTER                                  EL6361
00397            MOVE ER-0004          TO EMI-ERROR                     EL6361
00398            MOVE AL-UNBOF         TO PFENTERA                      EL6361
00399            MOVE -1               TO PFENTERL                      EL6361
00400            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6361
00401            GO TO 8200-SEND-DATAONLY                               EL6361
00402         ELSE                                                      EL6361
00403            IF PFENTERI NUMERIC AND                                EL6361
00404               PFENTERI GREATER 0 AND LESS 25                      EL6361
00405               MOVE PF-VALUES (PFENTERI) TO EIBAID                 EL6361
00406            ELSE                                                   EL6361
00407               MOVE ER-0029       TO EMI-ERROR                     EL6361
00408               MOVE AL-UNBOF      TO PFENTERA                      EL6361
00409               MOVE -1            TO PFENTERL                      EL6361
00410               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            EL6361
00411               GO TO 8200-SEND-DATAONLY.                           EL6361
00412                                                                   EL6361
00413      EJECT                                                        EL6361
00414                                                                   EL6361
00415  0300-CHECK-PFKEYS.                                               EL6361
00416                                                                   EL6361
00417      IF EIBAID = DFHPF23                                          EL6361
00418          GO TO 8810-PF23.                                         EL6361
00419                                                                   EL6361
00420      IF EIBAID = DFHPF24                                          EL6361
00421          GO TO 9200-RETURN-MAIN-MENU.                             EL6361
00422                                                                   EL6361
00423      IF EIBAID = DFHPF12                                          EL6361
00424          GO TO 9500-PF12.                                         EL6361
00425                                                                   EL6361
00426      IF EIBAID = DFHPF1                                              CL**4
00427          GO TO 6000-DISPLAY-NEXT-PAYEE.                           EL6361
00428                                                                   EL6361
00429      IF EIBAID = DFHPF2                                              CL**4
00430          GO TO 6100-DISPLAY-PREV-PAYEE.                           EL6361
00431                                                                   EL6361
00432      IF EIBAID = DFHENTER                                         EL6361
00433          GO TO 1000-EDIT-MAP.                                     EL6361
00434                                                                   EL6361
00435      MOVE ER-0008 TO EMI-ERROR.                                   EL6361
00436      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
00437      MOVE -1                     TO PFENTERL.                     EL6361
00438                                                                   EL6361
00439      GO TO 8200-SEND-DATAONLY.                                    EL6361
00440                                                                   EL6361
00441      EJECT                                                        EL6361
00442                                                                   EL6361
00443  1000-EDIT-MAP.                                                   EL6361
00444                                                                   EL6361
00445      IF MAINTI = 'C' OR 'D' OR 'S' OR 'A'                         EL6361
00446         MOVE MAINTI              TO PI-MAINT-FUNCTION             EL6361
00447         MOVE AL-UANON            TO MAINTA                        EL6361
00448      ELSE                                                         EL6361
00449         MOVE ER-0023             TO EMI-ERROR                     EL6361
00450         MOVE -1                  TO MAINTL                        EL6361
00451         MOVE AL-UABON            TO MAINTA                        EL6361
00452         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL6361
00453         GO TO 8200-SEND-DATAONLY.                                 EL6361
00454                                                                   EL6361
00455      IF PAYSEQL GREATER THAN 0                                    EL6361
00456           MOVE PAYSEQI           TO WS-PAY-SEQ-NUM                EL6361
00457      ELSE                                                         EL6361
00458           MOVE +1                TO WS-PAY-SEQ-NUM.               EL6361
00459                                                                   EL6361
00460      IF MAINTI = 'C'                                              EL6361
00461         IF PI-COMPANY-CD   = PI-SVCKWK-COMP-CD  AND               EL6361
00462            CSRI            = PI-SVCKWK-CSR      AND                  CL**2
00463            CARI            = PI-SVCKWK-CARRIER  AND               EL6361
00464            GROUPI          = PI-SVCKWK-GROUPING AND               EL6361
00465            PAYEEI          = PI-SVCKWK-PAYEE    AND               EL6361
00466            WS-PAY-SEQ-NUM  = PI-SVCKWK-PAYEE-SEQ                  EL6361
00467            NEXT SENTENCE                                          EL6361
00468         ELSE                                                      EL6361
00469            MOVE ER-2056             TO EMI-ERROR                  EL6361
00470            MOVE -1                  TO MAINTL                     EL6361
00471            MOVE AL-UABON            TO MAINTA                     EL6361
00472            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6361
00473            GO TO 8200-SEND-DATAONLY.                              EL6361
00474                                                                   EL6361
00475      IF NOT MODIFY-CAP                                            EL6361
00476         IF MAINTI = 'S'                                           EL6361
00477            MOVE MAINTI              TO PI-MAINT-FUNCTION          EL6361
00478         ELSE                                                      EL6361
00479            MOVE 'UPDATE'            TO SM-READ                    EL6361
00480            PERFORM 9995-SECURITY-VIOLATION                        EL6361
00481            MOVE ER-0070             TO EMI-ERROR                  EL6361
00482            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL6361
00483            GO TO 8100-SEND-INITIAL-MAP.                           EL6361
00484                                                                   EL6361
00485      IF MAINTI = 'S'                                                 CL**4
00486         GO TO 1050-EDIT-COMPLETE.                                 EL6361
00487                                                                      CL**4
00488      IF MAINTI = 'D'                                                 CL**4
00489         GO TO 1025-READ-CHECK-WORK.                                  CL**4
00490                                                                      CL**2
00491      IF MAINTI = 'A' OR 'C'                                          CL**2
00492          IF CSRL GREATER THAN ZERO                                   CL**2
00493              MOVE AL-UANON            TO CSRA                        CL**2
00494              IF PI-COMPANY-ID  =  'NCL'                              CL**4
00495                  IF CSRI  =  SPACES OR ZEROS                         CL**2
00496                      MOVE  -1         TO   CSRL                      CL**2
00497                      MOVE  AL-UABON   TO CSRA                        CL**2
00498                      MOVE  ER-1883    TO  EMI-ERROR                  CL**2
00499                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL**2
00500                   ELSE                                               CL**2
00501                      MOVE '2'         TO CNTL-REC-TYPE               CL**2
00502                      PERFORM 1600-READ-CNTL-FILE THRU 1690-EXIT      CL**2
00503              ELSE                                                    CL**2
00504                  NEXT SENTENCE                                       CL**2
00505          ELSE                                                        CL**2
00506              IF MAINTI  =  'A'                                       CL**2
00507                  IF PI-COMPANY-ID  =  'NCL'                          CL**4
00508                      MOVE  -1         TO   CSRL                      CL**2
00509                      MOVE  AL-UABON   TO CSRA                        CL**2
00510                      MOVE  ER-1883    TO  EMI-ERROR                  CL**2
00511                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.       CL**2
00512                                                                      CL**2
00513                                                                   EL6361
00514      IF CARL GREATER THAN ZERO                                    EL6361
00515         MOVE AL-UANON            TO CARA                          EL6361
00516      ELSE                                                         EL6361
00517         MOVE -1                  TO CARL                          EL6361
00518         MOVE ER-0194             TO EMI-ERROR                     EL6361
00519         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL6361
00520                                                                   EL6361
00521      IF GROUPL GREATER THAN ZEROS                                 EL6361
00522         MOVE AL-UANON            TO GROUPA                        EL6361
00523      ELSE                                                         EL6361
00524         MOVE -1                  TO GROUPL                        EL6361
00525         MOVE ER-0195             TO EMI-ERROR                     EL6361
00526         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL6361
00527                                                                   EL6361
00528      IF PAYEEL GREATER THAN ZEROS                                 EL6361
00529         MOVE AL-UANON            TO PAYEEA                        EL6361
00530      ELSE                                                         EL6361
00531         MOVE -1                  TO PAYEEL                        EL6361
00532         MOVE ER-3148             TO EMI-ERROR                     EL6361
00533         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL6361
00534                                                                   EL6361
00535      IF EMI-FATAL-CTR NOT = ZEROS                                 EL6361
00536          GO TO 8200-SEND-DATAONLY.                                EL6361
00537                                                                   EL6361
00538  1025-READ-CHECK-WORK.                                               CL**4
00539                                                                      CL**4
00540      MOVE PI-COMPANY-CD          TO PI-ERCKWK-COMP-CD.            EL6361
00541      MOVE CSRI                   TO PI-ERCKWK-CSR.                   CL**2
00542      MOVE CARI                   TO PI-ERCKWK-CARRIER.            EL6361
00543      MOVE GROUPI                 TO PI-ERCKWK-GROUPING.           EL6361
00544      MOVE PAYEEI                 TO PI-ERCKWK-PAYEE.              EL6361
00545      MOVE WS-PAY-SEQ-NUM         TO PI-ERCKWK-PAYEE-SEQ.          EL6361
00546      MOVE ZEROS                  TO PI-ERCKWK-SEQ-NO.             EL6361
00547                                                                   EL6361
00548      MOVE PI-ERCKWK-KEY          TO ERCKWK-KEY.                   EL6361
00549                                                                   EL6361
00550      EXEC CICS HANDLE CONDITION                                   EL6361
00551          NOTFND    (1090-CHECKS-NOTFND)                           EL6361
00552          ENDFILE   (1090-CHECKS-NOTFND)                           EL6361
00553      END-EXEC.                                                    EL6361
00554                                                                   EL6361
00555      EXEC CICS READ                                               EL6361
00556          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL**7
00557          DATASET (FILE-ID-ERCKWK)                                 EL6361
00558          RIDFLD  (ERCKWK-KEY)                                     EL6361
00559      END-EXEC.                                                    EL6361
00560                                                                   EL6361
00561      IF NOT CW-HEADER                                             EL6361
00562         GO TO 1090-CHECKS-NOTFND.                                 EL6361
00563                                                                   EL6361
00564      IF CW-RELEASE-DT GREATER THAN LOW-VALUES                        CL**4
00565          GO TO 1085-RELEASE-ERROR.                                   CL**4
00566                                                                      CL**4
00567      MOVE CW-AR-STATEMENT-DT TO WS-SAVE-STMT-DT.                  EL6361
00568                                                                   EL6361
00569  1050-EDIT-COMPLETE.                                              EL6361
00570                                                                   EL6361
00571      IF MAINTI = 'A'                                              EL6361
00572         GO TO 2000-ADD-TEXT.                                      EL6361
00573                                                                   EL6361
00574      IF MAINTI = 'C'                                              EL6361
00575         GO TO 3000-CHANGE-TEXT.                                   EL6361
00576                                                                   EL6361
00577      IF MAINTI = 'D'                                              EL6361
00578         GO TO 4000-DELETE-TEXT.                                   EL6361
00579                                                                   EL6361
00580      IF MAINTI = 'S'                                              EL6361
00581         MOVE +9000               TO PI-ERCKWK-SEQ-NO              EL6361
00582         GO TO 5000-DISPLAY-TEXT.                                  EL6361
00583                                                                   EL6361
00584  1085-RELEASE-ERROR.                                                 CL**4
00585                                                                      CL**4
00586      MOVE SPACE                  TO PI-MAINT-FUNCTION.               CL**4
00587      MOVE ER-3139                TO EMI-ERROR.                       CL**4
00588      MOVE -1                     TO MAINTL.                          CL**4
00589      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00590      GO TO 8200-SEND-DATAONLY.                                       CL**4
00591                                                                      CL**4
00592  1090-CHECKS-NOTFND.                                              EL6361
00593      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL6361
00594      MOVE ER-3160                TO EMI-ERROR.                    EL6361
00595      MOVE -1                     TO CARL.                         EL6361
00596      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
00597      GO TO 8200-SEND-DATAONLY.                                    EL6361
00598                                                                   EL6361
00599      EJECT                                                        EL6361
00600  1600-READ-CNTL-FILE.                                                CL**2
00601      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.                    CL**2
00602      MOVE '2'                    TO CNTL-REC-TYPE.                   CL**2
00603      MOVE CSRI                   TO CNTL-ACCESS.                     CL**2
00604                                                                      CL**4
00605      EXEC CICS HANDLE CONDITION                                      CL**2
00606          NOTFND  (1680-NOT-FOUND)                                    CL**2
00607          ERROR   (9990-ABEND)                                        CL**2
00608      END-EXEC.                                                       CL**2
00609                                                                      CL**4
00610      EXEC CICS READ                                                  CL**2
00611          DATASET  (CNTL-FILE-ID)                                     CL**2
00612          SET      (ADDRESS OF CONTROL-FILE)                          CL**7
00613          RIDFLD   (ELCNTL-KEY)                                       CL**2
00614      END-EXEC.                                                       CL**2
00615                                                                      CL**4
00616      GO TO 1690-EXIT.                                                CL**2
00617                                                                      CL**4
00618  1680-NOT-FOUND.                                                     CL**2
00619      MOVE ER-1883                TO EMI-ERROR.                       CL**2
00620      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**2
00621      MOVE -1                     TO CSRL.                            CL**2
00622      MOVE AL-UABON               TO CSRA.                            CL**2
00623  1690-EXIT.                                                          CL**2
00624       EXIT.                                                          CL**2
00625   EJECT                                                              CL**2
00626  2000-ADD-TEXT.                                                   EL6361
00627                                                                      CL**4
00628      PERFORM 4100-EDIT-SCRN THRU 4199-EXIT.                          CL**4
00629                                                                   EL6361
00630      IF STUB (1) EQUAL SPACES AND                                    CL**6
00631         STUB (2) EQUAL SPACES AND                                    CL**6
00632         STUB (3) EQUAL SPACES                                        CL**6
00633          MOVE ER-3192            TO EMI-ERROR                        CL**6
00634          MOVE -1                 TO MAINTL                           CL**6
00635          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**6
00636          GO TO 8200-SEND-DATAONLY.                                   CL**6
00637                                                                      CL**6
00638      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.            EL6361
00639      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.                      CL**2
00640      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.               EL6361
00641      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.              EL6361
00642      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.                 EL6361
00643      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.             EL6361
00644      MOVE +8999                  TO ERCKWK-SEQUENCE-NO.           EL6361
00645                                                                   EL6361
00646      MOVE SPACES                 TO CHECK-WORK-RECORDS.           EL6361
00647      MOVE 'CW'                   TO CW-RECORD-ID.                 EL6361
00648      MOVE '2'                    TO CW-RECORD-TYPE.               EL6361
00649      MOVE WS-SAVE-STMT-DT        TO CW-AR-STATEMENT-DT.           EL6361
00650      MOVE WS-CURRENT-BIN-DT      TO CW-RECORDED-DT.               EL6361
00651      MOVE PI-PROCESSOR-ID        TO CW-RECORDED-BY.               EL6361
00652      MOVE EIBTIME                TO CW-LAST-MAINT-HHMMSS.         EL6361
00653                                                                   EL6361
00654      MOVE LOW-VALUES             TO CW-RELEASE-DT                    CL**5
00655                                     CW-CREDIT-SELECT-DT              CL**5
00656                                     CW-CREDIT-ACCEPT-DT.             CL**3
00657                                                                   EL6361
00658      SET SCRN-NDX TO +1.                                          EL6361
00659  2020-NEXT-TEXT-LINE.                                             EL6361
00660                                                                   EL6361
00661      ADD +1                      TO ERCKWK-SEQUENCE-NO.           EL6361
00662      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.           EL6361
00663                                                                   EL6361
00664  2030-WRITE-HEADER.                                               EL6361
00665                                                                   EL6361
00666      INSPECT STUB (SCRN-NDX) REPLACING ALL '_' BY SPACES          EL6361
00667                                            LOW-VALUES BY SPACES.  EL6361
00668      MOVE STUB (SCRN-NDX)        TO CW-STUB-TEXT.                 EL6361
00669      MOVE SPACES                 TO CW-FILLER.                       CL**3
00670                                                                   EL6361
00671      EXEC CICS HANDLE CONDITION                                   EL6361
00672          DUPREC    (2060-DUPLICATE-TEXT)                          EL6361
00673      END-EXEC.                                                    EL6361
00674                                                                   EL6361
00675      EXEC CICS WRITE                                              EL6361
00676           FROM    (CHECK-WORK-RECORDS)                            EL6361
00677           DATASET (FILE-ID-ERCKWK)                                EL6361
00678           RIDFLD  (ERCKWK-KEY)                                    EL6361
00679      END-EXEC.                                                    EL6361
00680                                                                   EL6361
00681      SET SCRN-NDX UP BY +1.                                       EL6361
00682      IF SCRN-NDX NOT GREATER THAN +3                              EL6361
00683          GO TO 2020-NEXT-TEXT-LINE.                               EL6361
00684                                                                   EL6361
00685      IF EMI-WARNING-CTR EQUAL ZEROS                               EL6361
00686          MOVE ER-0000            TO EMI-ERROR                     EL6361
00687          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6361
00688                                                                   EL6361
00689      GO TO 5000-DISPLAY-TEXT.                                     EL6361
00690                                                                   EL6361
00691  2060-DUPLICATE-TEXT.                                             EL6361
00692                                                                   EL6361
00693      EXEC CICS SYNCPOINT ROLLBACK                                 EL6361
00694      END-EXEC.                                                    EL6361
00695                                                                   EL6361
00696      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL6361
00697      MOVE ER-3186                TO EMI-ERROR.                    EL6361
00698      MOVE -1                     TO CARL.                         EL6361
00699      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
00700      GO TO 8200-SEND-DATAONLY.                                    EL6361
00701                                                                   EL6361
00702      EJECT                                                        EL6361
00703                                                                   EL6361
00704  3000-CHANGE-TEXT.                                                EL6361
00705                                                                   EL6361
00706      PERFORM 4100-EDIT-SCRN THRU 4199-EXIT.                          CL**4
00707                                                                      CL**4
00708      SET SCRN-NDX TO +1.                                          EL6361
00709      MOVE +9000                  TO ERCKWK-SEQUENCE-NO.           EL6361
00710                                                                   EL6361
00711  3020-NEXT-TEXT-LINE.                                             EL6361
00712      MOVE PI-SVCKWK-COMP-CD      TO ERCKWK-COMPANY-CD.            EL6361
00713      MOVE PI-SVCKWK-CSR          TO ERCKWK-CSR.                      CL**2
00714      MOVE PI-SVCKWK-CARRIER      TO ERCKWK-CARRIER.               EL6361
00715      MOVE PI-SVCKWK-GROUPING     TO ERCKWK-GROUPING.              EL6361
00716      MOVE PI-SVCKWK-PAYEE        TO ERCKWK-PAYEE.                 EL6361
00717      MOVE PI-SVCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.             EL6361
00718                                                                   EL6361
00719      MOVE SPACES                 TO CHECK-WORK-RECORDS.           EL6361
00720      MOVE 'CW'                   TO CW-RECORD-ID.                 EL6361
00721      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.           EL6361
00722                                                                      CL**4
00723      EXEC CICS HANDLE CONDITION                                   EL6361
00724          NOTFND   (3080-NOTFND)                                   EL6361
00725      END-EXEC.                                                    EL6361
00726                                                                   EL6361
00727      EXEC CICS READ                                               EL6361
00728          SET (ADDRESS OF CHECK-WORK-RECORDS)                         CL**7
00729          DATASET (FILE-ID-ERCKWK)                                 EL6361
00730          RIDFLD  (ERCKWK-KEY)                                     EL6361
00731          UPDATE                                                   EL6361
00732      END-EXEC.                                                    EL6361
00733                                                                   EL6361
00734      IF CW-RELEASE-DT GREATER THAN LOW-VALUES                        CL**5
00735          GO TO 3085-RELEASE-ERROR.                                   CL**5
00736                                                                   EL6361
00737  3030-UPDATE-HEADER.                                              EL6361
00738                                                                   EL6361
00739      INSPECT STUB (SCRN-NDX) REPLACING ALL '_' BY SPACES          EL6361
00740                                            LOW-VALUES BY SPACES.  EL6361
00741      MOVE STUB (SCRN-NDX)        TO CW-STUB-TEXT.                 EL6361
00742      MOVE WS-CURRENT-BIN-DT      TO CW-RECORDED-DT.               EL6361
00743      MOVE PI-PROCESSOR-ID        TO CW-RECORDED-BY.               EL6361
00744      MOVE EIBTIME                TO CW-LAST-MAINT-HHMMSS.         EL6361
00745                                                                   EL6361
00746      EXEC CICS REWRITE                                            EL6361
00747           FROM (CHECK-WORK-RECORDS)                               EL6361
00748           DATASET (FILE-ID-ERCKWK)                                EL6361
00749      END-EXEC.                                                    EL6361
00750                                                                   EL6361
00751      ADD +1                      TO ERCKWK-SEQUENCE-NO.           EL6361
00752                                                                   EL6361
00753      SET SCRN-NDX UP BY +1.                                       EL6361
00754      IF SCRN-NDX NOT GREATER THAN +3                              EL6361
00755          GO TO 3020-NEXT-TEXT-LINE.                               EL6361
00756                                                                   EL6361
00757      IF EMI-WARNING-CTR EQUAL ZEROS                               EL6361
00758          MOVE ER-0000            TO EMI-ERROR                     EL6361
00759          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6361
00760                                                                   EL6361
00761      MOVE ERCKWK-KEY             TO PI-ERCKWK-KEY.                EL6361
00762      GO TO 5000-DISPLAY-TEXT.                                     EL6361
00763                                                                   EL6361
00764  3080-NOTFND.                                                     EL6361
00765                                                                   EL6361
00766      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL6361
00767      MOVE ER-3185                TO EMI-ERROR.                    EL6361
00768      MOVE -1                     TO CARL.                         EL6361
00769      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
00770      GO TO 8200-SEND-DATAONLY.                                    EL6361
00771                                                                   EL6361
00772  3085-RELEASE-ERROR.                                              EL6361
00773                                                                   EL6361
00774      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL6361
00775      MOVE ER-3139                TO EMI-ERROR.                    EL6361
00776      MOVE -1                     TO MAINTL.                       EL6361
00777      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
00778      GO TO 8200-SEND-DATAONLY.                                    EL6361
00779                                                                   EL6361
00780      EJECT                                                        EL6361
00781  4000-DELETE-TEXT.                                                EL6361
00782                                                                   EL6361
00783      MOVE 'D'                    TO PI-MAINT-FUNCTION.            EL6361
00784                                                                   EL6361
00785      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.            EL6361
00786      MOVE CSRI                   TO ERCKWK-CSR.                      CL**2
00787      MOVE CARI                   TO ERCKWK-CARRIER.               EL6361
00788      MOVE GROUPI                 TO ERCKWK-GROUPING.              EL6361
00789      MOVE PAYEEI                 TO ERCKWK-PAYEE.                 EL6361
00790      MOVE PAYSEQI                TO ERCKWK-PAYEE-SEQ.             EL6361
00791      MOVE +9000                  TO ERCKWK-SEQUENCE-NO.           EL6361
00792                                                                   EL6361
00793      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.          EL6361
00794                                                                   EL6361
00795      EXEC CICS HANDLE CONDITION                                   EL6361
00796          NOTFND    (4080-TEXT-NOTFND)                             EL6361
00797          ENDFILE   (4070-TEXT-DELETED)                            EL6361
00798      END-EXEC.                                                    EL6361
00799                                                                   EL6361
00800  4010-READ-CHECK-WORK-FILE.                                       EL6361
00801                                                                   EL6361
00802      EXEC CICS STARTBR                                            EL6361
00803          DATASET (FILE-ID-ERCKWK)                                 EL6361
00804          RIDFLD  (ERCKWK-KEY)                                     EL6361
00805      END-EXEC.                                                    EL6361
00806                                                                   EL6361
00807      EXEC CICS READNEXT                                           EL6361
00808          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL**7
00809          DATASET (FILE-ID-ERCKWK)                                 EL6361
00810          RIDFLD  (ERCKWK-KEY)                                     EL6361
00811      END-EXEC.                                                    EL6361
00812                                                                   EL6361
00813      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL**2
00814          GO TO      4070-TEXT-DELETED.                               CL**2
00815                                                                      CL**2
00816      IF CW-COMPANY-CD = SVCKWK-COMPANY-CD AND                     EL6361
00817         CW-CSR        = SVCKWK-CSR        AND                        CL**2
00818         CW-CARRIER    = SVCKWK-CARRIER    AND                     EL6361
00819         CW-GROUPING   = SVCKWK-GROUPING   AND                     EL6361
00820         CW-PAYEE      = SVCKWK-PAYEE      AND                     EL6361
00821         CW-PAYEE-SEQ  = SVCKWK-PAYEE-SEQ                          EL6361
00822         NEXT SENTENCE                                             EL6361
00823      ELSE                                                         EL6361
00824         GO TO 4070-TEXT-DELETED.                                  EL6361
00825                                                                   EL6361
00826      IF CW-TEXT                                                   EL6361
00827         NEXT SENTENCE                                             EL6361
00828      ELSE                                                         EL6361
00829         GO TO 4070-TEXT-DELETED.                                  EL6361
00830                                                                   EL6361
00831      EXEC CICS ENDBR                                              EL6361
00832           DATASET (FILE-ID-ERCKWK)                                EL6361
00833      END-EXEC.                                                    EL6361
00834                                                                   EL6361
00835      EXEC CICS READ                                               EL6361
00836          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL**7
00837          DATASET (FILE-ID-ERCKWK)                                 EL6361
00838          RIDFLD  (ERCKWK-KEY)                                     EL6361
00839          UPDATE                                                   EL6361
00840      END-EXEC.                                                    EL6361
00841                                                                   EL6361
00842      EXEC CICS DELETE                                             EL6361
00843           DATASET (FILE-ID-ERCKWK)                                EL6361
00844      END-EXEC.                                                    EL6361
00845                                                                   EL6361
00846      GO TO 4010-READ-CHECK-WORK-FILE.                             EL6361
00847                                                                   EL6361
00848  4070-TEXT-DELETED.                                               EL6361
00849                                                                   EL6361
00850      MOVE LOW-VALUES             TO  EL636BI.                     EL6361
00851      MOVE SVCKWK-CSR             TO  CSRO.                           CL**2
00852      MOVE SVCKWK-CARRIER         TO  CARO.                        EL6361
00853      MOVE SVCKWK-GROUPING        TO  GROUPO.                      EL6361
00854      MOVE SVCKWK-PAYEE           TO  PAYEEO.                      EL6361
00855      MOVE SVCKWK-PAYEE-SEQ       TO  PAYSEQO.                     EL6361
00856                                                                   EL6361
00857      MOVE AL-UANON               TO  CARA                         EL6361
00858                                      GROUPA                       EL6361
00859                                      PAYEEA                       EL6361
00860                                      PAYSEQA.                     EL6361
00861                                                                   EL6361
00862      MOVE ER-0000                TO  EMI-ERROR.                   EL6361
00863      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
00864      MOVE -1                     TO  MAINTL.                      EL6361
00865      GO TO 8100-SEND-INITIAL-MAP.                                 EL6361
00866                                                                   EL6361
00867  4080-TEXT-NOTFND.                                                EL6361
00868                                                                   EL6361
00869      MOVE ER-3185                TO EMI-ERROR.                    EL6361
00870      MOVE -1                     TO CARL.                         EL6361
00871      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
00872      GO TO 8200-SEND-DATAONLY.                                    EL6361
00873                                                                   EL6361
00874  4100-EDIT-SCRN.                                                     CL**4
00875                                                                      CL**4
00876      SET SCRN-NDX TO +1.                                             CL**4
00877                                                                      CL**4
00878  4110-NEXT-LINE.                                                     CL**4
00879      INSPECT STUB (SCRN-NDX) REPLACING ALL '_' BY SPACES             CL**4
00880                                            LOW-VALUES BY SPACES.     CL**4
00881      SET SCRN-NDX UP BY +1.                                          CL**4
00882      IF SCRN-NDX NOT GREATER THAN +3                                 CL**4
00883          GO TO 4110-NEXT-LINE.                                       CL**4
00884                                                                      CL**4
00885  4110-SPACE-STUB-TEST.                                               CL**6
00886      IF STUB (1) EQUAL SPACES                                        CL**4
00887          MOVE STUB (2)           TO STUB (1)                         CL**6
00888          MOVE STUB (3)           TO STUB (2)                         CL**6
00889          MOVE SPACES             TO STUB (3)                         CL**6
00890          IF STUB (1) EQUAL SPACES                                    CL**6
00891              MOVE STUB (2)       TO STUB (1)                         CL**6
00892              MOVE STUB (3)       TO STUB (2)                         CL**6
00893              MOVE SPACES         TO STUB (3)                         CL**6
00894      ELSE                                                            CL**4
00895          IF STUB (2) EQUAL SPACES                                    CL**4
00896              MOVE STUB (3)       TO STUB (2)                         CL**6
00897              MOVE SPACES         TO STUB (3).                        CL**6
00898                                                                      CL**4
00899  4199-EXIT.  EXIT.                                                   CL**4
00900                                                                      CL**4
00901      EJECT                                                        EL6361
00902  5000-DISPLAY-TEXT.                                               EL6361
00903                                                                   EL6361
00904      SET SCRN-NDX TO +1.                                          EL6361
00905      MOVE PI-ERCKWK-COMP-CD      TO ERCKWK-COMPANY-CD.            EL6361
00906      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.                      CL**2
00907      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.               EL6361
00908      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.              EL6361
00909      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.                 EL6361
00910      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.             EL6361
00911      MOVE +0                     TO ERCKWK-SEQUENCE-NO.           EL6361
00912                                                                   EL6361
00913      EXEC CICS HANDLE CONDITION                                   EL6361
00914          NOTFND    (5080-HEADER-NOTFND)                           EL6361
00915          ENDFILE   (5080-HEADER-NOTFND)                           EL6361
00916      END-EXEC.                                                    EL6361
00917                                                                   EL6361
00918      EXEC CICS READ                                               EL6361
00919          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL**7
00920          DATASET (FILE-ID-ERCKWK)                                 EL6361
00921          RIDFLD  (ERCKWK-KEY)                                     EL6361
00922      END-EXEC.                                                    EL6361
00923                                                                   EL6361
00924      MOVE ERCKWK-KEY             TO  PI-SAVE-ERCKWK-KEY.          EL6361
00925                                                                   EL6361
00926      IF NOT CW-HEADER                                             EL6361
00927         GO TO 5080-HEADER-NOTFND.                                 EL6361
00928                                                                   EL6361
00929      MOVE MAINTI TO WS-SAVE-ACTION-CODE.                          EL6361
00930      MOVE LOW-VALUES             TO  EL636BI.                     EL6361
00931      MOVE ALL '_'                TO  STUB1O                       EL6361
00932                                      STUB2O                       EL6361
00933                                      STUB3O.                      EL6361
00934      MOVE 'C'                    TO  MAINTI.                      EL6361
00935      MOVE AL-UANON               TO  MAINTA.                      EL6361
00936      MOVE CW-CSR                 TO  CSRO.                           CL**2
00937      MOVE AL-UANON               TO  CSRA.                           CL**2
00938      MOVE CW-CARRIER             TO  CARO.                        EL6361
00939      MOVE AL-UANON               TO  CARA.                        EL6361
00940      MOVE CW-GROUPING            TO  GROUPO.                      EL6361
00941      MOVE AL-UANON               TO  GROUPA.                      EL6361
00942      MOVE CW-PAYEE               TO  PAYEEO.                      EL6361
00943      MOVE AL-UANON               TO  PAYEEA.                      EL6361
00944      MOVE CW-PAYEE-SEQ           TO  PAYSEQO.                     EL6361
00945      MOVE AL-UANON               TO  PAYSEQA.                     EL6361
00946      MOVE CW-PAYEE-NAME          TO  PAYTOO.                      EL6361
00947      MOVE CW-ADDRESS-1           TO  ADDRS1O.                     EL6361
00948      MOVE CW-ADDRESS-2           TO  ADDRS2O.                     EL6361
00949      MOVE CW-PAYEE-CITY-ST       TO  CITYSTO.                     EL6361
00950      MOVE CW-PAYEE-ZIP           TO  ZIPO.                        EL6361
00951      MOVE CW-PAYEE-ZIP-EXT       TO  ZIPEXTO.                     EL6361
00952                                                                   EL6361
00953      MOVE CW-RELEASE-DT          TO  DC-BIN-DATE-1.                  CL**4
00954      MOVE SPACE                  TO  DC-OPTION-CODE.                 CL**4
00955      PERFORM 8500-DATE-CONVERT.                                      CL**4
00956                                                                      CL**4
00957      IF NO-CONVERSION-ERROR                                          CL**4
00958         MOVE DC-GREG-DATE-1-EDIT TO  RELDTO.                         CL**4
00959                                                                   EL6361
00960      MOVE CW-TOTAL-COMMISSION    TO  CHKAMTO.                     EL6361
00961                                                                   EL6361
00962      MOVE PI-ERCKWK-COMP-CD      TO ERCKWK-COMPANY-CD.            EL6361
00963      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.                      CL**2
00964      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.               EL6361
00965      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.              EL6361
00966      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.                 EL6361
00967      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.             EL6361
00968      MOVE +9000                  TO ERCKWK-SEQUENCE-NO.           EL6361
00969                                                                   EL6361
00970      EXEC CICS HANDLE CONDITION                                   EL6361
00971          NOTFND    (5090-TEXT-NOTFND)                             EL6361
00972          ENDFILE   (5090-TEXT-NOTFND)                             EL6361
00973      END-EXEC.                                                    EL6361
00974                                                                   EL6361
00975  5050-READ-NEXT-TEXT.                                             EL6361
00976      EXEC CICS READ                                               EL6361
00977          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL**7
00978          DATASET (FILE-ID-ERCKWK)                                 EL6361
00979          RIDFLD  (ERCKWK-KEY)                                     EL6361
00980      END-EXEC.                                                    EL6361
00981                                                                   EL6361
00982      IF NOT CW-TEXT                                               EL6361
00983         GO TO 5090-TEXT-NOTFND.                                   EL6361
00984                                                                   EL6361
00985      IF CW-STUB-TEXT EQUAL SPACES                                 EL6361
00986          MOVE ALL '_'      TO  STUB (SCRN-NDX)                    EL6361
00987      ELSE                                                         EL6361
00988          MOVE CW-STUB-TEXT TO  STUB (SCRN-NDX).                   EL6361
00989                                                                   EL6361
00990      IF SCRN-NDX NOT EQUAL +3                                     EL6361
00991          GO TO 5055-CONTINUE.                                     EL6361
00992                                                                   EL6361
00993      IF CW-RECORDED-BY = 'OFFL'                                   EL6361
00994         MOVE 'OFF-LINE'          TO  MAINTBYO                     EL6361
00995      ELSE                                                         EL6361
00996         MOVE CW-RECORDED-BY      TO  MAINTBYO.                    EL6361
00997                                                                   EL6361
00998      MOVE CW-LAST-MAINT-HHMMSS   TO  WS-TIME.                     EL6361
00999      MOVE WS-HR-MINS             TO  MAINTATO.                    EL6361
01000      MOVE CW-RECORDED-DT         TO  DC-BIN-DATE-1.               EL6361
01001      MOVE SPACE                  TO  DC-OPTION-CODE.              EL6361
01002      PERFORM 8500-DATE-CONVERT.                                   EL6361
01003                                                                   EL6361
01004      IF NO-CONVERSION-ERROR                                       EL6361
01005         MOVE DC-GREG-DATE-1-EDIT TO  MAINTONO.                    EL6361
01006                                                                   EL6361
01007      MOVE CW-RELEASE-DT          TO  DC-BIN-DATE-1.               EL6361
01008      MOVE SPACE                  TO  DC-OPTION-CODE.              EL6361
01009      PERFORM 8500-DATE-CONVERT.                                   EL6361
01010                                                                   EL6361
01011      IF NO-CONVERSION-ERROR                                       EL6361
01012         MOVE DC-GREG-DATE-1-EDIT TO  RELDTO.                      EL6361
01013                                                                   EL6361
01014  5055-CONTINUE.                                                   EL6361
01015      ADD +1 TO ERCKWK-SEQUENCE-NO.                                EL6361
01016      SET SCRN-NDX UP BY +1.                                       EL6361
01017      IF SCRN-NDX NOT GREATER THAN +3                              EL6361
01018          GO TO 5050-READ-NEXT-TEXT.                               EL6361
01019                                                                   EL6361
01020      MOVE 'C'                    TO  PI-MAINT-FUNCTION.           EL6361
01021      MOVE -1                     TO  MAINTL.                      EL6361
01022      MOVE AL-UANON               TO  STUBA(1)                     EL6361
01023                                      STUBA(2)                     EL6361
01024                                      STUBA(3).                    EL6361
01025      GO TO 8100-SEND-INITIAL-MAP.                                 EL6361
01026                                                                   EL6361
01027  5060-END-OF-FILE.                                                EL6361
01028                                                                   EL6361
01029      MOVE ER-2251                TO EMI-ERROR.                    EL6361
01030      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
01031      MOVE 'S'                    TO  PI-MAINT-FUNCTION.           EL6361
01032      MOVE -1                     TO  MAINTL.                      EL6361
01033      GO TO 8100-SEND-INITIAL-MAP.                                 EL6361
01034                                                                   EL6361
01035  5080-HEADER-NOTFND.                                              EL6361
01036      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL6361
01037      MOVE ER-3161                TO EMI-ERROR.                    EL6361
01038      MOVE -1                     TO CARL.                         EL6361
01039      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
01040      GO TO 8100-SEND-INITIAL-MAP.                                 EL6361
01041                                                                   EL6361
01042  5090-TEXT-NOTFND.                                                EL6361
01043      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL6361
01044      MOVE ER-3185                TO EMI-ERROR.                    EL6361
01045      MOVE -1                     TO CARL.                         EL6361
01046      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
01047      GO TO 8100-SEND-INITIAL-MAP.                                 EL6361
01048                                                                   EL6361
01049      EJECT                                                        EL6361
01050                                                                   EL6361
01051  6000-DISPLAY-NEXT-PAYEE.                                         EL6361
01052                                                                   EL6361
01053      MOVE PI-SAVE-ERCKWK-KEY     TO PI-ERCKWK-KEY.                EL6361
01054      MOVE PI-COMPANY-CD          TO PI-ERCKWK-COMP-CD.            EL6361
01055      MOVE +9999                  TO PI-ERCKWK-SEQ-NO.             EL6361
01056                                                                   EL6361
01057      EXEC CICS HANDLE CONDITION                                   EL6361
01058          ENDFILE   (6030-END-OF-FILE)                             EL6361
01059      END-EXEC.                                                    EL6361
01060                                                                   EL6361
01061  6010-READ-NEXT-PAYEE.                                            EL6361
01062                                                                   EL6361
01063      EXEC CICS READ                                               EL6361
01064          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL**7
01065          DATASET (FILE-ID-ERCKWK)                                 EL6361
01066          RIDFLD  (PI-ERCKWK-KEY)                                  EL6361
01067          GTEQ                                                     EL6361
01068      END-EXEC.                                                    EL6361
01069                                                                   EL6361
01070      IF CW-COMPANY-CD NOT = PI-COMPANY-CD                         EL6361
01071         GO TO 6030-END-OF-FILE.                                   EL6361
01072                                                                   EL6361
01073      MOVE  CW-CSR                TO  PI-ERCKWK-CSR.                  CL**2
01074      MOVE  CW-CARRIER            TO  PI-ERCKWK-CARRIER.           EL6361
01075      MOVE  CW-GROUPING           TO  PI-ERCKWK-GROUPING.          EL6361
01076      MOVE  CW-PAYEE              TO  PI-ERCKWK-PAYEE.             EL6361
01077      MOVE  CW-PAYEE-SEQ          TO  PI-ERCKWK-PAYEE-SEQ.         EL6361
01078      MOVE  CW-SEQUENCE-NO        TO  PI-ERCKWK-SEQ-NO.            EL6361
01079                                                                   EL6361
01080      GO TO 5000-DISPLAY-TEXT.                                     EL6361
01081                                                                   EL6361
01082  6030-END-OF-FILE.                                                EL6361
01083                                                                   EL6361
01084      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL6361
01085      MOVE ER-2251                TO EMI-ERROR.                    EL6361
01086      MOVE -1                     TO CARL.                         EL6361
01087      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
01088      GO TO 8200-SEND-DATAONLY.                                    EL6361
01089                                                                   EL6361
01090      EJECT                                                        EL6361
01091  6100-DISPLAY-PREV-PAYEE.                                         EL6361
01092                                                                   EL6361
01093      IF PI-SAVE-ERCKWK-KEY EQUAL SPACES                           EL6361
01094          GO TO 6160-END-OF-FILE.                                  EL6361
01095                                                                   EL6361
01096      MOVE PI-SAVE-ERCKWK-KEY     TO PI-ERCKWK-KEY.                EL6361
01097                                                                   EL6361
01098      MOVE PI-COMPANY-CD          TO PI-ERCKWK-COMP-CD.            EL6361
01099      MOVE +0000                  TO PI-ERCKWK-SEQ-NO.             EL6361
01100                                                                   EL6361
01101      EXEC CICS HANDLE CONDITION                                   EL6361
01102          ENDFILE   (6160-END-OF-FILE)                             EL6361
01103      END-EXEC.                                                    EL6361
01104                                                                   EL6361
01105      EXEC CICS STARTBR                                            EL6361
01106          DATASET (FILE-ID-ERCKWK)                                 EL6361
01107          RIDFLD  (PI-ERCKWK-KEY)                                  EL6361
01108      END-EXEC.                                                    EL6361
01109                                                                   EL6361
01110      MOVE PI-ERCKWK-KEY          TO SVCKWK-COMPARE-KEY.           EL6361
01111                                                                   EL6361
01112      EXEC CICS READNEXT                                           EL6361
01113          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL**7
01114          DATASET (FILE-ID-ERCKWK)                                 EL6361
01115          RIDFLD  (PI-ERCKWK-KEY)                                  EL6361
01116      END-EXEC.                                                    EL6361
01117                                                                   EL6361
01118      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL**2
01119          GO TO      6160-END-OF-FILE.                                CL**2
01120                                                                      CL**2
01121      EXEC CICS RESETBR                                            EL6361
01122          DATASET (FILE-ID-ERCKWK)                                 EL6361
01123          RIDFLD  (PI-ERCKWK-KEY)                                  EL6361
01124      END-EXEC.                                                    EL6361
01125                                                                   EL6361
01126  6110-READ-PREV-ENTRY.                                            EL6361
01127                                                                   EL6361
01128      EXEC CICS READPREV                                           EL6361
01129          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL**7
01130          DATASET (FILE-ID-ERCKWK)                                 EL6361
01131          RIDFLD  (PI-ERCKWK-KEY)                                  EL6361
01132      END-EXEC.                                                    EL6361
01133                                                                      CL**2
01134      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL**2
01135          GO TO      6160-END-OF-FILE.                                CL**2
01136                                                                   EL6361
01137      MOVE PI-ERCKWK-KEY          TO ERCKWK-COMPARE-KEY.           EL6361
01138                                                                   EL6361
01139      IF ERCKWK-COMPARE-KEY LESS THAN SVCKWK-COMPARE-KEY           EL6361
01140         NEXT SENTENCE                                             EL6361
01141      ELSE                                                         EL6361
01142         GO TO 6110-READ-PREV-ENTRY.                               EL6361
01143                                                                   EL6361
01144      MOVE +1                     TO PI-ERCKWK-SEQ-NO.             EL6361
01145      MOVE PI-ERCKWK-KEY          TO PI-SAVE-ERCKWK-KEY.           EL6361
01146                                                                   EL6361
01147      EXEC CICS ENDBR                                              EL6361
01148           DATASET (FILE-ID-ERCKWK)                                EL6361
01149      END-EXEC.                                                    EL6361
01150                                                                   EL6361
01151      IF PI-ERCKWK-COMP-CD NOT = PI-COMPANY-CD                     EL6361
01152         GO TO 6160-END-OF-FILE.                                   EL6361
01153                                                                   EL6361
01154      GO TO 5000-DISPLAY-TEXT.                                        CL**4
01155                                                                   EL6361
01156  6160-END-OF-FILE.                                                EL6361
01157                                                                   EL6361
01158      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL6361
01159      MOVE ER-2252                TO EMI-ERROR.                    EL6361
01160      MOVE -1                     TO CARL.                         EL6361
01161      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6361
01162      GO TO 8200-SEND-DATAONLY.                                    EL6361
01163                                                                   EL6361
01164      EJECT                                                        EL6361
01165                                                                   EL6361
01166  8100-SEND-INITIAL-MAP.                                           EL6361
01167                                                                   EL6361
01168      MOVE WS-CURRENT-DT          TO DATEO.                        EL6361
01169      MOVE EIBTIME                TO TIME-IN.                      EL6361
01170      MOVE TIME-OUT               TO TIMEO.                        EL6361
01171      MOVE -1                     TO MAINTL.                       EL6361
01172                                                                   EL6361
01173      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL6361
01174      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL6361
01175                                                                   EL6361
01176      EXEC CICS SEND                                               EL6361
01177          MAP      (EL636B)                                        EL6361
01178          MAPSET   (MAPSET-EL6361S)                                EL6361
01179          FROM     (EL636BI)                                       EL6361
01180          ERASE                                                    EL6361
01181          CURSOR                                                   EL6361
01182      END-EXEC.                                                    EL6361
01183                                                                   EL6361
01184      GO TO 9100-RETURN-TRAN.                                      EL6361
01185                                                                   EL6361
01186      EJECT                                                        EL6361
01187  8200-SEND-DATAONLY.                                              EL6361
01188                                                                   EL6361
01189      MOVE WS-CURRENT-DT          TO DATEO.                        EL6361
01190      MOVE EIBTIME                TO TIME-IN.                      EL6361
01191      MOVE TIME-OUT               TO TIMEO.                        EL6361
01192                                                                   EL6361
01193      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL6361
01194      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL6361
01195                                                                   EL6361
01196      EXEC CICS SEND                                               EL6361
01197           MAP      (EL636B)                                       EL6361
01198           MAPSET   (MAPSET-EL6361S)                               EL6361
01199           FROM     (EL636BI)                                      EL6361
01200           DATAONLY                                                EL6361
01201           CURSOR                                                  EL6361
01202      END-EXEC.                                                    EL6361
01203                                                                   EL6361
01204      GO TO 9100-RETURN-TRAN.                                      EL6361
01205                                                                   EL6361
01206      EJECT                                                        EL6361
01207                                                                   EL6361
01208  8300-SEND-TEXT.                                                  EL6361
01209      EXEC CICS SEND TEXT                                          EL6361
01210          FROM     (LOGOFF-TEXT)                                   EL6361
01211          LENGTH   (LOGOFF-LENGTH)                                 EL6361
01212          ERASE                                                    EL6361
01213          FREEKB                                                   EL6361
01214      END-EXEC.                                                    EL6361
01215                                                                   EL6361
01216      EXEC CICS RETURN                                             EL6361
01217      END-EXEC.                                                    EL6361
01218                                                                   EL6361
01219                                                                   EL6361
01220  8400-LOG-JOURNAL-RECORD.                                         EL6361
01221      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL6361
01222      MOVE THIS-PGM                TO JP-PROGRAM-ID.               EL6361
01223                                                                   EL6361
01224 *    EXEC CICS JOURNAL                                            EL6361
01225 *        JFILEID     (PI-JOURNAL-FILE-ID)                         EL6361
01226 *        JTYPEID     ('EL')                                       EL6361
01227 *        FROM        (JOURNAL-RECORD)                             EL6361
01228 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   EL6361
01229 *        END-EXEC.                                                EL6361
01230                                                                   EL6361
01231  8500-DATE-CONVERT.                                               EL6361
01232      EXEC CICS LINK                                               EL6361
01233          PROGRAM  (LINK-ELDATCV)                                  EL6361
01234          COMMAREA (DATE-CONVERSION-DATA)                          EL6361
01235          LENGTH   (DC-COMM-LENGTH)                                EL6361
01236      END-EXEC.                                                    EL6361
01237                                                                   EL6361
01238  8500-EXIT.                                                       EL6361
01239      EXIT.                                                        EL6361
01240                                                                   EL6361
01241      EJECT                                                        EL6361
01242                                                                   EL6361
01243  8600-DEEDIT.                                                     EL6361
01244                                                                   EL6361
01245      EXEC CICS BIF DEEDIT                                         EL6361
01246           FIELD   (DEEDIT-FIELD)                                  EL6361
01247           LENGTH  (12)                                            EL6361
01248      END-EXEC.                                                    EL6361
01249                                                                   EL6361
01250  8800-UNAUTHORIZED-ACCESS.                                        EL6361
01251      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL6361
01252      GO TO 8300-SEND-TEXT.                                        EL6361
01253                                                                   EL6361
01254  8810-PF23.                                                       EL6361
01255      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL6361
01256      MOVE XCTL-EL005             TO PGM-NAME.                     EL6361
01257      GO TO 9300-XCTL.                                             EL6361
01258                                                                   EL6361
01259  9200-RETURN-MAIN-MENU.                                           EL6361
01260      MOVE XCTL-EL626             TO PGM-NAME.                     EL6361
01261      GO TO 9300-XCTL.                                             EL6361
01262                                                                   EL6361
01263  9000-RETURN-CICS.                                                EL6361
01264      EXEC CICS RETURN                                             EL6361
01265      END-EXEC.                                                    EL6361
01266                                                                   EL6361
01267  9100-RETURN-TRAN.                                                EL6361
01268                                                                   EL6361
01269      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL6361
01270      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.         EL6361
01271                                                                   EL6361
01272      EXEC CICS RETURN                                             EL6361
01273          TRANSID    (TRANS-EXJE)                                  EL6361
01274          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6361
01275          LENGTH     (PI-COMM-LENGTH)                              EL6361
01276      END-EXEC.                                                    EL6361
01277                                                                   EL6361
01278  9300-XCTL.                                                       EL6361
01279      EXEC CICS XCTL                                               EL6361
01280          PROGRAM    (PGM-NAME)                                    EL6361
01281          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6361
01282          LENGTH     (PI-COMM-LENGTH)                              EL6361
01283      END-EXEC.                                                    EL6361
01284                                                                   EL6361
01285  9400-CLEAR.                                                      EL6361
01286      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL6361
01287      GO TO 9300-XCTL.                                             EL6361
01288                                                                   EL6361
01289  9500-PF12.                                                       EL6361
01290      MOVE XCTL-EL010             TO PGM-NAME.                     EL6361
01291      GO TO 9300-XCTL.                                             EL6361
01292                                                                   EL6361
01293  9600-PGMID-ERROR.                                                EL6361
01294                                                                   EL6361
01295      EXEC CICS HANDLE CONDITION                                   EL6361
01296          PGMIDERR    (8300-SEND-TEXT)                             EL6361
01297      END-EXEC.                                                    EL6361
01298                                                                   EL6361
01299      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL6361
01300      MOVE ' '                    TO PI-ENTRY-CD-1.                EL6361
01301      MOVE XCTL-EL005             TO PGM-NAME.                     EL6361
01302      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL6361
01303      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL6361
01304      GO TO 9300-XCTL.                                             EL6361
01305                                                                   EL6361
01306  9900-ERROR-FORMAT.                                               EL6361
01307                                                                   EL6361
01308      IF NOT EMI-ERRORS-COMPLETE                                   EL6361
01309          MOVE LINK-EL001         TO PGM-NAME                      EL6361
01310          EXEC CICS LINK                                           EL6361
01311              PROGRAM    (PGM-NAME)                                EL6361
01312              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6361
01313              LENGTH     (EMI-COMM-LENGTH)                         EL6361
01314          END-EXEC.                                                EL6361
01315                                                                   EL6361
01316  9900-EXIT.                                                       EL6361
01317      EXIT.                                                        EL6361
01318                                                                   EL6361
01319  9990-ABEND.                                                      EL6361
01320      MOVE LINK-EL004             TO PGM-NAME.                     EL6361
01321      MOVE DFHEIBLK               TO EMI-LINE1.                    EL6361
01322      EXEC CICS LINK                                               EL6361
01323          PROGRAM   (PGM-NAME)                                     EL6361
01324          COMMAREA  (EMI-LINE1)                                    EL6361
01325          LENGTH    (72)                                           EL6361
01326      END-EXEC.                                                    EL6361
01327                                                                   EL6361
01328      MOVE -1                     TO PFENTERL.                     EL6361
01329                                                                   EL6361
01330      GO TO 8200-SEND-DATAONLY.                                    EL6361
01331                                                                   EL6361
01332      GOBACK.                                                      EL6361
01333                                                                   EL6361
01334      EJECT                                                        EL6361
01335                                                                   EL6361
01336  9995-SECURITY-VIOLATION.                                         EL6361
01337                              COPY ELCSCTP.                        EL6361
01338                                                                   EL6361
01339  9995-EXIT.                                                       EL6361
01340      EXIT.                                                        EL6361
01341                                                                   EL6361
