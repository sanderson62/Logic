00001  IDENTIFICATION DIVISION.                                         10/18/96
00002                                                                   EL637
00003  PROGRAM-ID.                 EL637 .                                 LV012
00004 *              PROGRAM CONVERTED BY                                  CL*11
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*11
00006 *              CONVERSION DATE 02/12/96 09:53:54.                    CL*11
00007 *                            VMOD=2.012                              CL*12
00008 *                                                                 EL637
00008 *                                                                 EL637
00009 *AUTHOR.     LOGIC,INC.                                              CL*11
00010 *            DALLAS, TEXAS.                                          CL*11
00011                                                                   EL637
00012 *DATE-COMPILED.                                                      CL*11
00013 *SECURITY.   *****************************************************   CL*11
00014 *            *                                                   *   CL*11
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*11
00016 *            *                                                   *   CL*11
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*11
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*11
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*11
00020 *            *                                                   *   CL*11
00021 *            *****************************************************   CL*11
00022                                                                   EL637
00023 *REMARKS. TRANSACTION - EXJB - CHECK VOID PROGRAM.                   CL*11
00024                                                                   EL637
00025  ENVIRONMENT DIVISION.                                            EL637
00026                                                                   EL637
00027      EJECT                                                        EL637
00028  DATA DIVISION.                                                   EL637
00029  WORKING-STORAGE SECTION.                                         EL637
00030                                                                   EL637
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL637
00032  77  FILLER  PIC X(32)  VALUE '*    EL637 WORKING STORAGE     *'. EL637
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.012 *********'.    CL*12
00034                                                                   EL637
00035     EJECT                                                         EL637
00036                                                                   EL637
00037                              COPY ELCSCTM.                           CL**4
00038                              COPY ELCSCRTY.                          CL**4
00039                                                                   EL637
00040     EJECT                                                         EL637
00041                                                                   EL637
00042 ******************************************************************EL637
00043 *                                                                *EL637
00044 *              S T A N D A R D   A R E A S                       *EL637
00045 *                                                                *EL637
00046 ******************************************************************EL637
00047                                                                   EL637
00048  01  STANDARD-AREAS.                                              EL637
00049      12  SC-ITEM                     PIC S9(4)   VALUE +1   COMP. EL637
00050      12  GETMAIN-SPACE               PIC X       VALUE SPACE.     EL637
00051      12  EL637A                      PIC X(8)    VALUE 'EL637A'.  EL637
00052      12  MAPSET-EL637S               PIC X(8)    VALUE 'EL637S'.  EL637
00053      12  TRANS-EXJB                  PIC X(4)    VALUE 'EXJB'.    EL637
00054      12  THIS-PGM                    PIC X(8)    VALUE 'EL637 '.  EL637
00055      12  PGM-NAME                    PIC X(8).                    EL637
00056      12  TIME-IN                     PIC S9(7).                   EL637
00057      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL637
00058          16  FILLER                  PIC X.                       EL637
00059          16  TIME-OUT                PIC 99V99.                   EL637
00060          16  FILLER                  PIC X(2).                    EL637
00061      12  LINK-EL001                  PIC X(8)    VALUE 'EL001'.   EL637
00062      12  LINK-EL004                  PIC X(8)    VALUE 'EL004'.   EL637
00063      12  XCTL-EL005                  PIC X(8)    VALUE 'EL005'.   EL637
00064      12  XCTL-EL010                  PIC X(8)    VALUE 'EL010'.   EL637
00065      12  XCTL-EL626                  PIC X(8)    VALUE 'EL626'.   EL637
00066      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL637
00067      12  FILE-ID-ERCMKQ              PIC X(8)    VALUE 'ERCMKQ '. EL637
00068      12  FILE-ID-ERCMKQ2             PIC X(8)    VALUE 'ERCMKQ2'. EL637
00069      12  FILE-ID-ERCMCK              PIC X(8)    VALUE 'ERCMCK '. EL637
00070      12  FILE-ID-ERCKWK              PIC X(8)    VALUE 'ERCKWK '. EL637
00071      12  FILE-ID-ERPYAJ              PIC X(8)    VALUE 'ERPYAJ '. EL637
00072      12  WS-CURRENT-DT               PIC X(8)    VALUE SPACES.    EL637
00073      12  WS-CURRENT-BIN-DT           PIC XX      VALUE SPACES.    EL637
00074      12  QID.                                                     EL637
00075          16  QID-TERM                PIC X(4)    VALUE SPACES.    EL637
00076          16  FILLER                  PIC X(4)    VALUE '125D'.    EL637
00077      12  WS-TIME                     PIC 9(6)    VALUE ZEROS.     EL637
00078      12  WS-HR-MINS-SECS REDEFINES WS-TIME.                       EL637
00079          16  WS-HR-MINS              PIC 99V99.                   EL637
00080          16  FILLER                  PIC XX.                      EL637
00081                                                                   EL637
00082      EJECT                                                        EL637
00083                                                                   EL637
00084  01  WORK-AREA.                                                   EL637
00085      12  DEEDIT-FIELD               PIC X(12).                       CL**4
00086      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(10)V99.  EL637
00087      12  WS-WORK-SEQ-NO             PIC S9(9)   VALUE +0 COMP.       CL**4
00088      12  WS-WORK-SEQ-HOLD           PIC S9(3)   VALUE +0.            CL**8
00089      12  WS-SUB1                    PIC S9(4)   VALUE +0 COMP.       CL**4
00090      12  WS-ERCKWK-PROCESS-SW       PIC X       VALUE SPACE.         CL**4
00091          88  WS-ERCKWK-PROCESSED                VALUE 'Y'.           CL**4
00092      12  WS-BROWSE-SW               PIC X       VALUE SPACE.         CL**4
00093          88  WS-BROWSE-STARTED                  VALUE 'Y'.           CL**4
00094      12  WS-FIRST-TIME-SW           PIC X       VALUE 'Y'.           CL**4
00095          88  WS-FIRST-TIME                      VALUE 'Y'.           CL**4
00096      12  WS-STRT-CK-NO              PIC 9(6)  VALUE ZEROS.           CL**9
00097                                                                      CL**4
00098  01  WS-MAX-TEXT-SEQ                PIC S9(4)   VALUE +9002 COMP.    CL**4
00099                                                                   EL637
00100      EJECT                                                        EL637
00101                                                                   EL637
00102 ******************************************************************EL637
00103 *                                                                *EL637
00104 *                E R R O R   M E S S A G E S                     *EL637
00105 *                                                                *EL637
00106 ******************************************************************EL637
00107                                                                   EL637
00108  01  ERROR-MESSAGES.                                              EL637
00109      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL637
00110      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL637
00111      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL637
00112      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL637
00113      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL637
00114      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL637
00115      12  ER-0194                 PIC X(4)  VALUE '0194'.          EL637
00116      12  ER-0195                 PIC X(4)  VALUE '0195'.          EL637
00117      12  ER-2056                 PIC X(4)  VALUE '2056'.          EL637
00118      12  ER-2132                 PIC X(4)  VALUE '2132'.          EL637
00119      12  ER-2223                 PIC X(4)  VALUE '2223'.          EL637
00120      12  ER-2251                 PIC X(4)  VALUE '2251'.          EL637
00121      12  ER-2252                 PIC X(4)  VALUE '2252'.          EL637
00122      12  ER-2800                 PIC X(4)  VALUE '2800'.          EL637
00123      12  ER-3129                 PIC X(4)  VALUE '3129'.             CL**8
00124      12  ER-3147                 PIC X(4)  VALUE '3147'.          EL637
00125      12  ER-3148                 PIC X(4)  VALUE '3148'.          EL637
00126      12  ER-3158                 PIC X(4)  VALUE '3158'.          EL637
00127      12  ER-3159                 PIC X(4)  VALUE '3159'.          EL637
00128      12  ER-3160                 PIC X(4)  VALUE '3160'.          EL637
00129      12  ER-3161                 PIC X(4)  VALUE '3161'.          EL637
00130      12  ER-3162                 PIC X(4)  VALUE '3162'.          EL637
00131      12  ER-3163                 PIC X(4)  VALUE '3163'.          EL637
00132      12  ER-3164                 PIC X(4)  VALUE '3164'.          EL637
00133      12  ER-3165                 PIC X(4)  VALUE '3165'.          EL637
00134      12  ER-3171                 PIC X(4)  VALUE '3171'.          EL637
00135      12  ER-3176                 PIC X(4)  VALUE '3176'.             CL**2
00136      12  ER-3185                 PIC X(4)  VALUE '3185'.             CL**4
00137                                                                   EL637
00138      EJECT                                                        EL637
00139                                                                   EL637
00140 ******************************************************************EL637
00141 *                                                                *EL637
00142 *              A C C E S S   K E Y S                             *EL637
00143 *                                                                *EL637
00144 ******************************************************************EL637
00145                                                                   EL637
00146  01  ACCESS-KEYS.                                                 EL637
00147                                                                   EL637
00148      12  ERCMKQ-KEY.                                              EL637
00149          16  ERCMKQ-COMPANY-CD       PIC X           VALUE SPACES.EL637
00150          16  ERCMKQ-CONTROL-NUMBER   PIC S9(8) COMP VALUE +0.     EL637
00151          16  ERCMKQ-SEQUENCE-NUMBER  PIC S9(4) COMP VALUE +0.     EL637
00152                                                                   EL637
00153      12  ERCMKQ2-KEY.                                             EL637
00154          16  ERCMKQ2-COMPANY-CD      PIC X          VALUE SPACES. EL637
00155          16  ERCMKQ2-CSR             PIC X(4)       VALUE SPACES.    CL**5
00156          16  ERCMKQ2-CARRIER         PIC X          VALUE SPACES. EL637
00157          16  ERCMKQ2-GROUPING        PIC X(6)       VALUE SPACES. EL637
00158          16  ERCMKQ2-PAYEE           PIC X(10)      VALUE SPACES. EL637
00159          16  ERCMKQ2-PAYEE-SEQ       PIC S9(4) COMP VALUE +0.        CL**2
00160          16  ERCMKQ2-CONTROL-NUMBER  PIC S9(8) COMP VALUE +0.     EL637
00161          16  ERCMKQ2-SEQUENCE-NUMBER PIC S9(4) COMP VALUE +0.     EL637
00162                                                                   EL637
00163      12  ERCMKQ2-PREV-KEY            PIC X(30)      VALUE SPACES.    CL**5
00164                                                                   EL637
00165      12  ERCMKQ2-COMPARE-KEY         PIC X(28)      VALUE SPACES.    CL**6
00166                                                                   EL637
00167      12  SVCMKQ2-COMPARE-KEY         PIC X(28)      VALUE SPACES.    CL**6
00168                                                                   EL637
00169      12  ERCMKQ-RECORD-LENGTH        PIC S9(4) COMP VALUE +1800.     CL**4
00170                                                                   EL637
00171      12  ERCMCK-KEY.                                              EL637
00172          16  ERCMCK-COMPANY-CD       PIC X          VALUE SPACES. EL637
00173          16  ERCMCK-CSR              PIC X(4)       VALUE SPACES.    CL**5
00174          16  ERCMCK-CARRIER          PIC X          VALUE SPACES. EL637
00175          16  ERCMCK-GROUPING         PIC X(6)       VALUE SPACES. EL637
00176          16  ERCMCK-PAYEE            PIC X(10)      VALUE SPACES. EL637
00177          16  ERCMCK-PAYEE-SEQ        PIC S9(4) COMP VALUE +0.        CL**2
00178          16  ERCMCK-SEQUENCE-NO      PIC S9(4) COMP VALUE +0.     EL637
00179                                                                   EL637
00180      12  ERCMCK-RECORD-LENGTH        PIC S9(4) COMP VALUE +2000.     CL**4
00181                                                                   EL637
00182      12  ERCKWK-KEY.                                              EL637
00183          16  ERCKWK-COMPANY-CD       PIC X           VALUE SPACES.EL637
00184          16  ERCKWK-CSR              PIC X(4)        VALUE SPACES.   CL**5
00185          16  ERCKWK-CARRIER          PIC X           VALUE SPACES.EL637
00186          16  ERCKWK-GROUPING         PIC X(6)        VALUE SPACES.EL637
00187          16  ERCKWK-PAYEE            PIC X(10)       VALUE SPACES.EL637
00188          16  ERCKWK-PAYEE-SEQ        PIC S9(4)  COMP VALUE +0.       CL**2
00189          16  ERCKWK-SEQUENCE-NO      PIC S9(4)  COMP VALUE +0.    EL637
00190                                                                   EL637
00191                                                                   EL637
00192      12  ERPYAJ-KEY.                                              EL637
00193          16  ERPYAJ-COMPANY-CD       PIC X           VALUE SPACE. EL637
00194          16  ERPYAJ-CARRIER          PIC X           VALUE SPACE. EL637
00195          16  ERPYAJ-GROUPING         PIC X(6)        VALUE SPACE. EL637
00196          16  ERPYAJ-FIN-RESP         PIC X(10)       VALUE SPACE. EL637
00197          16  ERPYAJ-ACCOUNT          PIC X(10)       VALUE SPACE. EL637
00198          16  ERPYAJ-FILE-SEQ-NO      PIC S9(8) COMP  VALUE +0.    EL637
00199          16  ERPYAJ-RECORD-TYPE      PIC X.                       EL637
00200                                                                   EL637
00201      12  ERPYAJ-RECORD-LENGTH        PIC S9(4) COMP  VALUE +200.  EL637
00202                                                                   EL637
00203      EJECT                                                        EL637
00204                                                                   EL637
00205                              COPY ELCDATE.                           CL**4
00206                                                                   EL637
00207      EJECT                                                        EL637
00208                              COPY ELCLOGOF.                          CL**4
00209                                                                   EL637
00210      EJECT                                                        EL637
00211                              COPY ELCATTR.                           CL**4
00212                                                                   EL637
00213      EJECT                                                        EL637
00214                              COPY ELCEMIB.                           CL**4
00215                                                                   EL637
00216      EJECT                                                        EL637
00217                              COPY ELCINTF.                           CL**4
00218                                                                   EL637
00219      12  FILLER  REDEFINES PI-PROGRAM-WORK-AREA.                  EL637
00220          16  PI-MAINT-FUNCTION               PIC X.               EL637
00221              88  PI-VOID-FUNCTION                VALUE 'V'.       EL637
00222              88  PI-SHOW-FUNCTION                VALUE 'S'.       EL637
00223                                                                   EL637
00224          16  PI-END-OF-FILE-SW               PIC X.               EL637
00225              88  PI-END-OF-FILE                  VALUE 'Y'.       EL637
00226          16  PI-ERCMKQ2-KEY.                                      EL637
00227              20  PI-ERCMKQ2-COMPANY-CD       PIC X.               EL637
00228              20  PI-ERCMKQ2-CSR              PIC X(4).               CL**5
00229              20  PI-ERCMKQ2-CARRIER          PIC X.               EL637
00230              20  PI-ERCMKQ2-GROUPING         PIC X(6).            EL637
00231              20  PI-ERCMKQ2-PAYEE            PIC X(10).           EL637
00232              20  PI-ERCMKQ2-PAYEE-SEQ        PIC S9(4) COMP.         CL**2
00233              20  PI-ERCMKQ2-CONTROL-NUMBER   PIC S9(8) COMP.      EL637
00234              20  PI-ERCMKQ2-SEQUENCE-NUMBER  PIC S9(4) COMP.      EL637
00235                                                                   EL637
00236          16  PI-SAVE-ERCMKQ2-KEY.                                 EL637
00237              20  PI-SVCMKQ2-COMPANY-CD       PIC X.               EL637
00238              20  PI-SVCMKQ2-CSR              PIC X(4).               CL**5
00239              20  PI-SVCMKQ2-CARRIER          PIC X.               EL637
00240              20  PI-SVCMKQ2-GROUPING         PIC X(6).            EL637
00241              20  PI-SVCMKQ2-PAYEE            PIC X(10).           EL637
00242              20  PI-SVCMKQ2-PAYEE-SEQ        PIC S9(4) COMP.         CL**2
00243              20  PI-SVCMKQ2-CONTROL-NUMBER   PIC S9(8) COMP.      EL637
00244              20  PI-SVCMKQ2-SEQUENCE-NUMBER  PIC S9(4) COMP.      EL637
00245          16  FILLER                          PIC X(578).             CL*11
00246                                                                   EL637
00247                                                                   EL637
00248      EJECT                                                        EL637
00249                              COPY ELCJPFX.                           CL**4
00250                              PIC X(1583).                         EL637
00251                                                                   EL637
00252      EJECT                                                        EL637
00253                              COPY ELCAID.                            CL**4
00254  01  FILLER    REDEFINES DFHAID.                                  EL637
00255      12  FILLER              PIC X(8).                            EL637
00256      12  PF-VALUES           PIC X       OCCURS 2.                EL637
00257                                                                   EL637
00258      EJECT                                                        EL637
00259                              COPY EL637S.                            CL**4
00260                                                                   EL637
00261                                                                   EL637
00262                                                                   EL637
00263      EJECT                                                        EL637
00264  LINKAGE SECTION.                                                 EL637
00265  01  DFHCOMMAREA             PIC X(1024).                         EL637
00266                                                                   EL637
00267      EJECT                                                        EL637
00268 *01 PARMLIST .                                                       CL*11
00269 *    02  FILLER              PIC S9(8)   COMP.                       CL*11
00270 *    02  ERCKWK-POINTER      PIC S9(8)   COMP.                       CL*11
00271 *    02  ERCMCK-POINTER      PIC S9(8)   COMP.                       CL*11
00272 *    02  ERCMKQ-POINTER      PIC S9(8)   COMP.                       CL*11
00273 *    02  ERPYAJ-POINTER      PIC S9(8)   COMP.                       CL*11
00274                                                                   EL637
00275      EJECT                                                        EL637
00276                                                                   EL637
00277                                  COPY ERCCKWK.                       CL**4
00278      EJECT                                                        EL637
00279                                                                   EL637
00280                                  COPY ERCCMCK.                       CL**4
00281      EJECT                                                        EL637
00282                                                                   EL637
00283                                  COPY ERCCMKQ.                    EL637
00284      EJECT                                                        EL637
00285                                  COPY ERCPYAJ.                    EL637
00286      EJECT                                                        EL637
00287                                                                   EL637
00288  PROCEDURE DIVISION.                                              EL637
00289                                                                   EL637
00290      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL637
00291      MOVE 1                      TO EMI-NUMBER-OF-LINES.          EL637
00292                                                                   EL637
00293      IF EIBCALEN = 0                                              EL637
00294          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL637
00295                                                                   EL637
00296      MOVE EIBTRMID               TO QID-TERM.                     EL637
00297      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL637
00298      MOVE '5'                    TO DC-OPTION-CODE.               EL637
00299      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL637
00300      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL637
00301      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                EL637
00302                                                                   EL637
00303      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL637
00304          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL637
00305              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL637
00306              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL637
00307              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL637
00308              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL637
00309              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL637
00310              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL637
00311              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL637
00312              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL637
00313          ELSE                                                     EL637
00314              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL637
00315              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL637
00316              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL637
00317              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL637
00318              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL637
00319              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL637
00320              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL637
00321              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL637
00322                                                                   EL637
00323      MOVE LOW-VALUES             TO EL637AI.                      EL637
00324                                                                   EL637
00325      IF EIBTRNID NOT = TRANS-EXJB                                 EL637
00326         MOVE SPACES              TO PI-PROGRAM-WORK-AREA          EL637
00327         MOVE PI-COMPANY-CD       TO PI-ERCMKQ2-COMPANY-CD         EL637
00328         MOVE +9999               TO PI-ERCMKQ2-SEQUENCE-NUMBER    EL637
00329         GO TO 8100-SEND-INITIAL-MAP.                              EL637
00330                                                                   EL637
00331      EXEC CICS HANDLE CONDITION                                   EL637
00332          PGMIDERR  (9600-PGMID-ERROR)                             EL637
00333          ERROR     (9990-ABEND)                                   EL637
00334      END-EXEC.                                                    EL637
00335                                                                   EL637
00336      IF EIBAID = DFHCLEAR                                         EL637
00337          GO TO 9400-CLEAR.                                        EL637
00338                                                                   EL637
00339      IF PI-PROCESSOR-ID = 'LGXX'                                  EL637
00340          GO TO 0200-RECEIVE.                                      EL637
00341                                                                   EL637
00342      EXEC CICS READQ TS                                           EL637
00343          QUEUE  (QID)                                             EL637
00344          INTO   (SECURITY-CONTROL)                                EL637
00345          LENGTH (SC-COMM-LENGTH)                                  EL637
00346          ITEM   (SC-ITEM)                                         EL637
00347      END-EXEC.                                                    EL637
00348                                                                   EL637
00349      MOVE SC-CREDIT-DISPLAY (7)   TO PI-DISPLAY-CAP.              EL637
00350      MOVE SC-CREDIT-UPDATE  (7)   TO PI-MODIFY-CAP.               EL637
00351                                                                   EL637
00352      IF NOT DISPLAY-CAP                                           EL637
00353          MOVE 'READ'          TO SM-READ                          EL637
00354          PERFORM 9995-SECURITY-VIOLATION                          EL637
00355          MOVE ER-0070         TO  EMI-ERROR                       EL637
00356          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL637
00357          GO TO 8100-SEND-INITIAL-MAP.                             EL637
00358                                                                   EL637
00359      EJECT                                                        EL637
00360                                                                   EL637
00361 ******************************************************************EL637
00362 *                                                                *EL637
00363 *              R E C E I V E   M A P                             *EL637
00364 *                                                                *EL637
00365 ******************************************************************EL637
00366                                                                   EL637
00367  0200-RECEIVE.                                                    EL637
00368                                                                   EL637
00369      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL637
00370          MOVE ER-0008            TO EMI-ERROR                     EL637
00371          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL637
00372          MOVE -1                 TO MAINTL                        EL637
00373          GO TO 8200-SEND-DATAONLY.                                EL637
00374                                                                   EL637
00375      EXEC CICS RECEIVE                                            EL637
00376          MAP      (EL637A)                                        EL637
00377          MAPSET   (MAPSET-EL637S)                                 EL637
00378          INTO     (EL637AI)                                       EL637
00379      END-EXEC.                                                    EL637
00380                                                                   EL637
00381      IF PFENTERL GREATER THAN ZERO                                EL637
00382         IF EIBAID NOT = DFHENTER                                  EL637
00383            MOVE ER-0004          TO EMI-ERROR                     EL637
00384            MOVE AL-UNBOF         TO PFENTERA                      EL637
00385            MOVE -1               TO PFENTERL                      EL637
00386            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL637
00387            GO TO 8200-SEND-DATAONLY                               EL637
00388         ELSE                                                      EL637
00389            IF PFENTERI NUMERIC AND                                EL637
00390               PFENTERI GREATER 0 AND LESS 23                      EL637
00391               MOVE PF-VALUES (PFENTERI) TO EIBAID                 EL637
00392            ELSE                                                   EL637
00393               MOVE ER-0029       TO EMI-ERROR                     EL637
00394               MOVE AL-UNBOF      TO PFENTERA                      EL637
00395               MOVE -1            TO PFENTERL                      EL637
00396               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            EL637
00397               GO TO 8200-SEND-DATAONLY.                           EL637
00398                                                                   EL637
00399      EJECT                                                        EL637
00400                                                                   EL637
00401 ******************************************************************EL637
00402 *                                                                *EL637
00403 *              C H E C K   P F K E Y S                           *EL637
00404 *                                                                *EL637
00405 ******************************************************************EL637
00406                                                                   EL637
00407  0300-CHECK-PFKEYS.                                               EL637
00408                                                                   EL637
00409      IF EIBAID = DFHPF23                                          EL637
00410          GO TO 8810-PF23.                                         EL637
00411                                                                   EL637
00412      IF EIBAID = DFHPF24                                          EL637
00413          GO TO 9200-RETURN-MAIN-MENU.                             EL637
00414                                                                   EL637
00415      IF EIBAID = DFHPF12                                          EL637
00416          GO TO 9500-PF12.                                         EL637
00417                                                                   EL637
00418      IF EIBAID = DFHPF1                                           EL637
00419         IF PI-END-OF-FILE                                         EL637
00420            MOVE -1               TO MAINTL                        EL637
00421            MOVE ER-2251          TO EMI-ERROR                     EL637
00422            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL637
00423            GO TO 8200-SEND-DATAONLY                               EL637
00424         ELSE                                                      EL637
00425            MOVE SPACE            TO PI-MAINT-FUNCTION             EL637
00426            GO TO 5000-DISPLAY-PAYEE.                              EL637
00427                                                                   EL637
00428      IF EIBAID = DFHPF2                                           EL637
00429          MOVE SPACE              TO PI-MAINT-FUNCTION             EL637
00430          GO TO 6100-DISPLAY-PREV-PAYEE.                           EL637
00431                                                                   EL637
00432                                                                   EL637
00433      IF EIBAID = DFHENTER                                         EL637
00434          GO TO 1000-EDIT-MAP.                                     EL637
00435                                                                   EL637
00436      MOVE ER-0008 TO EMI-ERROR.                                   EL637
00437      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL637
00438      MOVE -1                     TO PFENTERL.                     EL637
00439                                                                   EL637
00440      GO TO 8200-SEND-DATAONLY.                                    EL637
00441                                                                   EL637
00442      EJECT                                                        EL637
00443                                                                   EL637
00444 ******************************************************************EL637
00445 *                                                                *EL637
00446 *                  E D I T    M A P                              *EL637
00447 *                                                                *EL637
00448 ******************************************************************EL637
00449                                                                   EL637
00450  1000-EDIT-MAP.                                                   EL637
00451                                                                   EL637
00452      IF MAINTI = 'S' OR 'R'                                       EL637
00453         MOVE MAINTI              TO PI-MAINT-FUNCTION             EL637
00454         MOVE AL-UANON            TO MAINTA                        EL637
00455      ELSE                                                         EL637
00456         MOVE ER-0023             TO EMI-ERROR                     EL637
00457         MOVE -1                  TO MAINTL                        EL637
00458         MOVE AL-UABON            TO MAINTA                        EL637
00459         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL637
00460         GO TO 8200-SEND-DATAONLY.                                 EL637
00461                                                                   EL637
00462      IF NOT MODIFY-CAP                                            EL637
00463         IF MAINTI = 'S'                                           EL637
00464            MOVE MAINTI              TO PI-MAINT-FUNCTION          EL637
00465         ELSE                                                      EL637
00466            MOVE 'UPDATE'            TO SM-READ                    EL637
00467            PERFORM 9995-SECURITY-VIOLATION                        EL637
00468            MOVE ER-0070             TO EMI-ERROR                  EL637
00469            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL637
00470            GO TO 8100-SEND-INITIAL-MAP.                           EL637
00471                                                                   EL637
00472      IF MAINTI = 'R'                                              EL637
00473         IF PI-COMPANY-CD = PI-SVCMKQ2-COMPANY-CD AND              EL637
00474            CSRI          = PI-SVCMKQ2-CSR        AND                 CL**5
00475            CARI          = PI-SVCMKQ2-CARRIER    AND              EL637
00476            GROUPI        = PI-SVCMKQ2-GROUPING   AND              EL637
00477            PAYEEI        = PI-SVCMKQ2-PAYEE      AND              EL637
00478            PAYSEQI       = PI-SVCMKQ2-PAYEE-SEQ  AND                 CL**2
00479            CNTRLNOI      = PI-SVCMKQ2-CONTROL-NUMBER              EL637
00480            GO TO 7500-REVERSE-COMM-CHECK                          EL637
00481         ELSE                                                      EL637
00482            MOVE ER-2056             TO EMI-ERROR                  EL637
00483            MOVE -1                  TO MAINTL                     EL637
00484            MOVE AL-UABON            TO MAINTA                     EL637
00485            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL637
00486            GO TO 8200-SEND-DATAONLY.                              EL637
00487                                                                      CL**5
00488      IF CSRL GREATER THAN ZERO                                       CL**5
00489         MOVE AL-UANON            TO CSRA                             CL**8
00490      ELSE                                                            CL**8
00491         MOVE LOW-VALUES          TO CSRI.                            CL**8
00492                                                                   EL637
00493      IF CARL GREATER THAN ZERO                                    EL637
00494         MOVE AL-UANON            TO CARA                          EL637
00495      ELSE                                                         EL637
00496         MOVE -1                  TO CARL                          EL637
00497         MOVE ER-0194             TO EMI-ERROR                     EL637
00498         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL637
00499                                                                   EL637
00500      IF GROUPL GREATER THAN ZEROS                                 EL637
00501         MOVE AL-UANON            TO GROUPA                        EL637
00502      ELSE                                                         EL637
00503         MOVE -1                  TO GROUPL                        EL637
00504         MOVE ER-0195             TO EMI-ERROR                     EL637
00505         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL637
00506                                                                   EL637
00507      IF PAYEEL GREATER THAN ZEROS                                 EL637
00508         MOVE AL-UANON            TO PAYEEA                        EL637
00509      ELSE                                                         EL637
00510         MOVE -1                  TO PAYEEL                        EL637
00511         MOVE ER-3148             TO EMI-ERROR                     EL637
00512         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL637
00513                                                                   EL637
00514      IF PAYSEQL GREATER THAN ZEROS                                   CL**2
00515         MOVE AL-UANON            TO PAYSEQA                          CL**2
00516      ELSE                                                            CL**2
00517         MOVE -1                  TO PAYSEQL                          CL**2
00518         MOVE ER-3176             TO EMI-ERROR                        CL**2
00519         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    CL**2
00520                                                                      CL**2
00521      IF CNTRLNOL GREATER THAN ZEROS                               EL637
00522         MOVE AL-UANON            TO CNTRLNOA                      EL637
00523      ELSE                                                         EL637
00524         MOVE -1                  TO CNTRLNOL                      EL637
00525         MOVE ER-3129             TO EMI-ERROR                        CL**8
00526         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL637
00527                                                                   EL637
00528                                                                   EL637
00529  1050-EDIT-COMPLETE.                                              EL637
00530                                                                   EL637
00531      IF EMI-ERROR NOT = ZEROS                                     EL637
00532         GO TO 8200-SEND-DATAONLY.                                 EL637
00533                                                                   EL637
00534      MOVE PI-COMPANY-CD          TO PI-ERCMKQ2-COMPANY-CD.        EL637
00535      MOVE CSRI                   TO PI-ERCMKQ2-CSR.                  CL**5
00536      MOVE CARI                   TO PI-ERCMKQ2-CARRIER.           EL637
00537      MOVE GROUPI                 TO PI-ERCMKQ2-GROUPING.          EL637
00538      MOVE PAYEEI                 TO PI-ERCMKQ2-PAYEE.             EL637
00539      MOVE PAYSEQI                TO PI-ERCMKQ2-PAYEE-SEQ.            CL**2
00540      MOVE CNTRLNOI               TO PI-ERCMKQ2-CONTROL-NUMBER.    EL637
00541                                                                   EL637
00542      IF MAINTI = 'S'                                              EL637
00543         MOVE +0                 TO PI-ERCMKQ2-SEQUENCE-NUMBER     EL637
00544         GO TO 5000-DISPLAY-PAYEE.                                 EL637
00545                                                                   EL637
00546                                                                   EL637
00547      EJECT                                                        EL637
00548                                                                   EL637
00549 ******************************************************************EL637
00550 *                                                                *EL637
00551 *               D I S P L A Y   P A Y E E                        *EL637
00552 *                                                                *EL637
00553 ******************************************************************EL637
00554                                                                   EL637
00555  5000-DISPLAY-PAYEE.                                              EL637
00556                                                                   EL637
00557      MOVE SPACE                  TO  PI-END-OF-FILE-SW.           EL637
00558                                                                   EL637
00559      IF EIBAID = DFHPF1 OR DFHPF2                                    CL**8
00560          IF CSRI EQUAL SPACES OR LOW-VALUES OR ALL '_'               CL**8
00561              MOVE LOW-VALUES TO PI-ERCMKQ2-CSR.                      CL**8
00562                                                                      CL**8
00563      MOVE LOW-VALUES             TO  EL637AI.                     EL637
00564      MOVE 'S'                    TO  MAINTI.                      EL637
00565      MOVE AL-UANON               TO  MAINTA.                      EL637
00566                                                                   EL637
00567      MOVE PI-ERCMKQ2-KEY         TO  SVCMKQ2-COMPARE-KEY.         EL637
00568                                                                   EL637
00569      EXEC CICS HANDLE CONDITION                                   EL637
00570          NOTFND    (5090-CHECKS-NOTFND)                           EL637
00571      END-EXEC.                                                    EL637
00572                                                                   EL637
00573      MOVE SPACE                  TO WS-BROWSE-SW.                 EL637
00574                                                                   EL637
00575      EXEC CICS STARTBR                                            EL637
00576          DATASET (FILE-ID-ERCMKQ2)                                EL637
00577          RIDFLD  (PI-ERCMKQ2-KEY)                                 EL637
00578      END-EXEC.                                                    EL637
00579                                                                   EL637
00580      EXEC CICS HANDLE CONDITION                                   EL637
00581          NOTFND    (5070-CHECK-PROCESSED)                         EL637
00582          ENDFILE   (5060-END-OF-FILE)                             EL637
00583      END-EXEC.                                                    EL637
00584                                                                   EL637
00585      MOVE 'Y'                    TO WS-BROWSE-SW.                 EL637
00586                                                                   EL637
00587  5010-READ-CHECK-QUE-FILE.                                        EL637
00588                                                                   EL637
00589      EXEC CICS READNEXT                                           EL637
00590          SET     (ADDRESS OF COMMISSION-CHECK-QUE)                   CL*11
00591          DATASET (FILE-ID-ERCMKQ2)                                EL637
00592          RIDFLD  (PI-ERCMKQ2-KEY)                                 EL637
00593      END-EXEC.                                                    EL637
00594                                                                      CL**5
00595      IF PI-COMPANY-CD  NOT =  MQ-COMPANY-CD                          CL**5
00596          GO TO      5060-END-OF-FILE.                                CL**5
00597                                                                   EL637
00598      IF CHECK-ON-QUE OR ACH-PAYMENT                                  CL*12
00599          NEXT SENTENCE                                               CL*12
00600      ELSE                                                            CL*12
00601          GO TO 5010-READ-CHECK-QUE-FILE.                             CL*12
00602                                                                   EL637
00603      MOVE PI-ERCMKQ2-KEY TO ERCMKQ2-COMPARE-KEY.                  EL637
00604                                                                   EL637
00605      IF WS-FIRST-TIME                                             EL637
00606         IF PI-SHOW-FUNCTION                                       EL637
00607            IF ERCMKQ2-COMPARE-KEY NOT = SVCMKQ2-COMPARE-KEY       EL637
00608               MOVE PI-SAVE-ERCMKQ2-KEY TO PI-ERCMKQ2-KEY          EL637
00609               GO TO 5090-CHECKS-NOTFND                            EL637
00610            ELSE                                                   EL637
00611               MOVE PI-ERCMKQ2-KEY    TO PI-SAVE-ERCMKQ2-KEY       EL637
00612               MOVE 'N'               TO WS-FIRST-TIME-SW          EL637
00613               GO TO 5025-PROCESS-PAYEE                            EL637
00614         ELSE                                                      EL637
00615            MOVE PI-ERCMKQ2-KEY       TO PI-SAVE-ERCMKQ2-KEY       EL637
00616            MOVE 'N'                  TO WS-FIRST-TIME-SW          EL637
00617            GO TO 5025-PROCESS-PAYEE.                              EL637
00618                                                                   EL637
00619      IF ERCMKQ2-COMPARE-KEY  NOT =  SVCMKQ2-COMPARE-KEY           EL637
00620          GO TO 5070-CHECK-PROCESSED.                                 CL*10
00621                                                                   EL637
00622      IF MQ-CHECK-AMOUNT NOT = ZERO                                   CL**9
00623          MOVE MQ-CHECK-AMOUNT    TO CHKAMTO.                         CL**9
00624                                                                      CL**9
00625      IF MQ-CHECK-NUMBER GREATER THAN WS-STRT-CK-NO                   CL**9
00626          MOVE '-'                TO DASHO                            CL**9
00627          MOVE MQ-CHECK-NUMBER    TO ENDCHKO.                         CL**9
00628                                                                      CL**9
00629      GO TO 5010-READ-CHECK-QUE-FILE.                              EL637
00630                                                                   EL637
00631  5025-PROCESS-PAYEE.                                              EL637
00632                                                                   EL637
00633      MOVE AL-UANON               TO  CSRA.                           CL**5
00634      MOVE MQ-CSR-A1              TO  CSRO.                           CL**5
00635      MOVE AL-UANON               TO  CARA.                           CL**5
00636      MOVE MQ-CARRIER-A1          TO  CARO.                        EL637
00637      MOVE AL-UANON               TO  CARA.                        EL637
00638      MOVE MQ-GROUPING-A1         TO  GROUPO.                      EL637
00639      MOVE AL-UANON               TO  GROUPA.                      EL637
00640      MOVE MQ-PAYEE-A1            TO  PAYEEO.                      EL637
00641      MOVE AL-UANON               TO  PAYEEA.                      EL637
00642      MOVE MQ-PAYEE-SEQ-A1        TO  PAYSEQO.                        CL**2
00643      MOVE AL-UANON               TO  PAYSEQA.                        CL**2
00644      MOVE MQ-CONTROL-NUMBER-A1   TO  CNTRLNOO.                    EL637
00645      MOVE AL-UANON               TO  CNTRLNOA.                    EL637
00646      MOVE MQ-PAYEE-NAME          TO  PAYTOO.                      EL637
00647      MOVE MQ-PAYEE-ADDRESS-1     TO  ADDRS1O.                     EL637
00648      MOVE MQ-PAYEE-ADDRESS-2     TO  ADDRS2O.                     EL637
00649      MOVE MQ-PAYEE-CITY-ST       TO  CITYSTO.                     EL637
00650                                                                      CL**4
00651      IF  MQ-PAYEE-CANADIAN-POST-CODE                                 CL**4
00652          MOVE MQ-PAY-CAN-POSTAL-CD-1                                 CL**4
00653                                  TO  ZIPO                            CL**4
00654          MOVE MQ-PAY-CAN-POSTAL-CD-2                                 CL**4
00655                                  TO  ZIPEXTO                         CL**4
00656                                                                      CL**4
00657      ELSE                                                            CL**4
00658          MOVE MQ-PAYEE-ZIP       TO  ZIPO                            CL**4
00659          MOVE MQ-PAYEE-ZIP-EXT   TO  ZIPEXTO.                        CL**4
00660                                                                      CL**4
00661      MOVE MQ-LAST-MAINT-BY       TO  MAINTBYO.                    EL637
00662      MOVE MQ-LAST-MAINT-HHMMSS   TO  WS-TIME.                     EL637
00663      MOVE WS-HR-MINS             TO  MAINTATO.                    EL637
00664      MOVE MQ-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               EL637
00665      MOVE SPACE                  TO  DC-OPTION-CODE.              EL637
00666      PERFORM 8500-DATE-CONVERT.                                   EL637
00667                                                                   EL637
00668      IF NO-CONVERSION-ERROR                                       EL637
00669         MOVE DC-GREG-DATE-1-EDIT TO  MAINTONO.                    EL637
00670                                                                   EL637
00671      IF MQ-CHECK-WRITTEN-DT GREATER THAN LOW-VALUES               EL637
00672         MOVE MQ-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1                EL637
00673         MOVE SPACE               TO  DC-OPTION-CODE               EL637
00674         PERFORM 8500-DATE-CONVERT                                 EL637
00675         IF NO-CONVERSION-ERROR                                    EL637
00676            MOVE DC-GREG-DATE-1-EDIT TO  CHKWRTNO.                 EL637
00677                                                                      CL**3
00678      IF MQ-CHECK-RELEASE-DT GREATER THAN LOW-VALUES                  CL**3
00679         MOVE MQ-CHECK-RELEASE-DT TO  DC-BIN-DATE-1                   CL**3
00680         MOVE SPACE               TO  DC-OPTION-CODE                  CL**3
00681         PERFORM 8500-DATE-CONVERT                                    CL**3
00682         IF NO-CONVERSION-ERROR                                       CL**3
00683            MOVE DC-GREG-DATE-1-EDIT TO  CHKRELO.                     CL**3
00684                                                                   EL637
00685      IF MQ-VOID-DT   GREATER THAN LOW-VALUES                      EL637
00686         MOVE MQ-VOID-DT TO  DC-BIN-DATE-1                         EL637
00687         MOVE SPACE               TO  DC-OPTION-CODE               EL637
00688         PERFORM 8500-DATE-CONVERT                                 EL637
00689         IF NO-CONVERSION-ERROR                                    EL637
00690            MOVE DC-GREG-DATE-1-EDIT TO  VOIDDTO.                  EL637
00691                                                                      CL*10
00692      IF MQ-CHECK-AMOUNT NOT = ZERO                                   CL*10
00693          MOVE MQ-CHECK-AMOUNT    TO CHKAMTO.                         CL*10
00694                                                                      CL*10
00695      IF MQ-CHECK-NUMBER GREATER THAN WS-STRT-CK-NO                   CL*10
00696          MOVE '-'                TO DASHO                            CL*10
00697          MOVE MQ-CHECK-NUMBER    TO ENDCHKO.                         CL*10
00698                                                                   EL637
00699      MOVE ' '                    TO DASHO.                           CL**9
00700                                                                      CL**7
00701      MOVE MQ-CHECK-NUMBER        TO STRTCHKO.                        CL**9
00702                                                                   EL637
00703      GO TO 5010-READ-CHECK-QUE-FILE.                              EL637
00704                                                                   EL637
00705                                                                   EL637
00706  5060-END-OF-FILE.                                                EL637
00707                                                                   EL637
00708      MOVE 'Y'                    TO PI-END-OF-FILE-SW.            EL637
00709      MOVE ER-2251                TO EMI-ERROR.                    EL637
00710      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL637
00711                                                                   EL637
00712  5070-CHECK-PROCESSED.                                            EL637
00713                                                                   EL637
00714      IF WS-BROWSE-STARTED                                         EL637
00715         EXEC CICS ENDBR                                           EL637
00716              DATASET (FILE-ID-ERCMKQ2)                            EL637
00717         END-EXEC.                                                 EL637
00718                                                                   EL637
00719      MOVE 'S'                    TO  PI-MAINT-FUNCTION.           EL637
00720      MOVE -1                     TO  MAINTL.                      EL637
00721      GO TO 8100-SEND-INITIAL-MAP.                                 EL637
00722                                                                   EL637
00723  5080-END-OF-ENTRIES.                                             EL637
00724                                                                   EL637
00725      MOVE -1                     TO MAINTL.                       EL637
00726      GO TO 8200-SEND-DATAONLY.                                    EL637
00727                                                                   EL637
00728  5090-CHECKS-NOTFND.                                              EL637
00729                                                                   EL637
00730      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL637
00731      MOVE ER-3160                TO EMI-ERROR.                    EL637
00732      MOVE -1                     TO CARL.                         EL637
00733      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL637
00734      GO TO 8200-SEND-DATAONLY.                                    EL637
00735                                                                   EL637
00736      EJECT                                                        EL637
00737                                                                   EL637
00738 ******************************************************************EL637
00739 *                                                                *EL637
00740 *            D I S P L A Y   P R E V   P A Y E E                 *EL637
00741 *                                                                *EL637
00742 ******************************************************************EL637
00743                                                                   EL637
00744  6100-DISPLAY-PREV-PAYEE.                                         EL637
00745                                                                   EL637
00746                                                                      CL**5
00747      IF PI-SAVE-ERCMKQ2-KEY = SPACES                              EL637
00748         GO TO 6160-END-OF-FILE.                                   EL637
00749                                                                   EL637
00750      MOVE PI-SAVE-ERCMKQ2-KEY    TO PI-ERCMKQ2-KEY.               EL637
00751                                                                   EL637
00752      EXEC CICS HANDLE CONDITION                                   EL637
00753          ENDFILE   (6160-END-OF-FILE)                             EL637
00754      END-EXEC.                                                    EL637
00755                                                                   EL637
00756      EXEC CICS STARTBR                                            EL637
00757          DATASET (FILE-ID-ERCMKQ2)                                EL637
00758          RIDFLD  (PI-ERCMKQ2-KEY)                                 EL637
00759      END-EXEC.                                                    EL637
00760                                                                   EL637
00761  6110-READ-PREV-ENTRY.                                            EL637
00762                                                                   EL637
00763      EXEC CICS READPREV                                           EL637
00764          SET     (ADDRESS OF COMMISSION-CHECK-QUE)                   CL*11
00765          DATASET (FILE-ID-ERCMKQ2)                                EL637
00766          RIDFLD  (PI-ERCMKQ2-KEY)                                 EL637
00767      END-EXEC.                                                    EL637
00768                                                                      CL**5
00769      IF PI-COMPANY-CD  NOT =  MQ-COMPANY-CD                          CL**5
00770          GO TO      6160-END-OF-FILE.                                CL**5
00771                                                                   EL637
00772      IF PI-ERCMKQ2-KEY = PI-SAVE-ERCMKQ2-KEY                      EL637
00773         GO TO 6110-READ-PREV-ENTRY.                               EL637
00774                                                                   EL637
00775      IF CHECK-ON-QUE OR ACH-PAYMENT                                  CL*12
00776          NEXT SENTENCE                                               CL*12
00777      ELSE                                                            CL*12
00778          GO TO 6110-READ-PREV-ENTRY.                                 CL*12
00779                                                                   EL637
00780      IF MQ-TEXT                                                      CL**4
00781         GO TO 6110-READ-PREV-ENTRY.                                  CL**4
00782                                                                      CL**4
00783      EXEC CICS ENDBR                                              EL637
00784           DATASET (FILE-ID-ERCMKQ2)                               EL637
00785      END-EXEC.                                                    EL637
00786                                                                   EL637
00787      IF PI-ERCMKQ2-COMPANY-CD NOT = PI-COMPANY-CD                 EL637
00788         GO TO 6160-END-OF-FILE                                    EL637
00789      ELSE                                                         EL637
00790         MOVE +0                  TO PI-ERCMKQ2-SEQUENCE-NUMBER    EL637
00791         GO TO 5000-DISPLAY-PAYEE.                                 EL637
00792                                                                   EL637
00793  6160-END-OF-FILE.                                                EL637
00794                                                                   EL637
00795      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL637
00796      MOVE ER-2252                TO EMI-ERROR.                    EL637
00797      MOVE -1                     TO CARL.                         EL637
00798      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL637
00799      MOVE  PI-SAVE-ERCMKQ2-KEY   TO  PI-ERCMKQ2-KEY.                 CL**5
00800      GO TO 8200-SEND-DATAONLY.                                    EL637
00801                                                                   EL637
00802      EJECT                                                        EL637
00803                                                                   EL637
00804 ******************************************************************EL637
00805 *                                                                *EL637
00806 *       R E V E R S E   C O M M I S I O N   C H E C K            *EL637
00807 *                CHECK QUE FILE (ERCMKQ)                         *   CL**8
00808 *                                                                *EL637
00809 ******************************************************************EL637
00810                                                                   EL637
00811  7500-REVERSE-COMM-CHECK.                                         EL637
00812                                                                   EL637
00813      EXEC CICS HANDLE CONDITION                                   EL637
00814          NOTFND    (7580-PAYEE-NOTFND)                            EL637
00815      END-EXEC.                                                    EL637
00816                                                                   EL637
00817      MOVE SPACE                  TO WS-BROWSE-SW.                 EL637
00818      MOVE PI-SAVE-ERCMKQ2-KEY    TO PI-ERCMKQ2-KEY                EL637
00819                                     SVCMKQ2-COMPARE-KEY.          EL637
00820                                                                   EL637
00821  7505-PROCESS-CHECK-QUE-FILE.                                     EL637
00822                                                                   EL637
00823      EXEC CICS STARTBR                                            EL637
00824          DATASET (FILE-ID-ERCMKQ2)                                EL637
00825          RIDFLD  (PI-ERCMKQ2-KEY)                                 EL637
00826      END-EXEC.                                                    EL637
00827                                                                   EL637
00828      EXEC CICS HANDLE CONDITION                                   EL637
00829          NOTFND    (7570-COMM-CHECK-REVERSED)                     EL637
00830          ENDFILE   (7570-COMM-CHECK-REVERSED)                     EL637
00831      END-EXEC.                                                    EL637
00832                                                                   EL637
00833      MOVE 'Y'                    TO WS-BROWSE-SW.                 EL637
00834                                                                   EL637
00835  7510-READ-CHECK-QUE-FILE.                                        EL637
00836                                                                   EL637
00837      EXEC CICS READNEXT                                           EL637
00838          SET     (ADDRESS OF COMMISSION-CHECK-QUE)                   CL*11
00839          DATASET (FILE-ID-ERCMKQ2)                                EL637
00840          RIDFLD  (PI-ERCMKQ2-KEY)                                 EL637
00841      END-EXEC.                                                    EL637
00842                                                                      CL**5
00843      IF PI-COMPANY-CD  NOT =  MQ-COMPANY-CD                          CL**5
00844          GO TO      7570-COMM-CHECK-REVERSED.                        CL**5
00845                                                                   EL637
00846      IF CHECK-ON-QUE OR ACH-PAYMENT                                  CL*12
00847          NEXT SENTENCE                                               CL*12
00848      ELSE                                                            CL*12
00849          GO TO 7510-READ-CHECK-QUE-FILE.                             CL*12
00850                                                                   EL637
00851      IF PI-ERCMKQ2-KEY = ERCMKQ2-PREV-KEY                         EL637
00852         GO TO 7510-READ-CHECK-QUE-FILE.                           EL637
00853                                                                   EL637
00854      MOVE PI-ERCMKQ2-KEY TO ERCMKQ2-COMPARE-KEY.                  EL637
00855                                                                   EL637
00856      IF WS-FIRST-TIME                                             EL637
00857         IF ERCMKQ2-COMPARE-KEY NOT = SVCMKQ2-COMPARE-KEY          EL637
00858            GO TO 7580-PAYEE-NOTFND                                EL637
00859         ELSE                                                      EL637
00860            MOVE 'N'        TO WS-FIRST-TIME-SW                    EL637
00861            GO TO 7525-PROCESS-PAYEE.                              EL637
00862                                                                   EL637
00863      IF ERCMKQ2-COMPARE-KEY  NOT =  SVCMKQ2-COMPARE-KEY           EL637
00864         GO TO 7570-COMM-CHECK-REVERSED.                           EL637
00865                                                                   EL637
00866  7525-PROCESS-PAYEE.                                              EL637
00867                                                                   EL637
00868      IF MQ-VOID-DT GREATER THAN LOW-VALUES                        EL637
00869         GO TO 7585-REVERSAL-ERROR.                                EL637
00870                                                                   EL637
00871      MOVE PI-ERCMKQ2-KEY         TO ERCMKQ2-PREV-KEY.             EL637
00872                                                                   EL637
00873      IF MQ-TEXT                                                      CL**4
00874          NEXT SENTENCE                                               CL**4
00875      ELSE                                                            CL**4
00876         IF MQ-CHECK-WRITTEN-DT EQUAL LOW-VALUES OR SPACES            CL**4
00877             NEXT SENTENCE                                            CL**4
00878         ELSE                                                         CL**4
00879             PERFORM 7600-REVERSE-PMTS-ADJS  THRU 7690-EXIT.          CL**4
00880                                                                   EL637
00881      PERFORM 7700-REVERSE-COMM-CHECK THRU 7790-EXIT.              EL637
00882                                                                   EL637
00883      MOVE MQ-COMPANY-CD          TO ERCMKQ-COMPANY-CD.            EL637
00884      MOVE MQ-CONTROL-NUMBER      TO ERCMKQ-CONTROL-NUMBER.        EL637
00885      MOVE MQ-SEQUENCE-NUMBER     TO ERCMKQ-SEQUENCE-NUMBER.       EL637
00886                                                                   EL637
00887      EXEC CICS ENDBR                                              EL637
00888           DATASET (FILE-ID-ERCMKQ2)                               EL637
00889      END-EXEC.                                                    EL637
00890                                                                   EL637
00891      EXEC CICS READ                                               EL637
00892          SET     (ADDRESS OF COMMISSION-CHECK-QUE)                   CL*11
00893          DATASET (FILE-ID-ERCMKQ)                                 EL637
00894          RIDFLD  (ERCMKQ-KEY)                                     EL637
00895          UPDATE                                                   EL637
00896      END-EXEC.                                                    EL637
00897                                                                   EL637
00898      MOVE PI-PROCESSOR-ID        TO MQ-LAST-MAINT-BY.             EL637
00899      MOVE EIBTIME                TO MQ-LAST-MAINT-HHMMSS.         EL637
00900      MOVE WS-CURRENT-BIN-DT      TO MQ-LAST-MAINT-DT              EL637
00901                                     MQ-VOID-DT.                   EL637
00902                                                                   EL637
00903      MOVE LOW-VALUES             TO MQ-CREDIT-SELECT-DATE         EL637
00904                                     MQ-CREDIT-ACCEPT-DATE.        EL637
00905                                                                   EL637
00906      EXEC CICS REWRITE                                            EL637
00907           FROM    (COMMISSION-CHECK-QUE)                          EL637
00908           DATASET (FILE-ID-ERCMKQ)                                EL637
00909      END-EXEC.                                                    EL637
00910                                                                   EL637
00911      GO TO 7505-PROCESS-CHECK-QUE-FILE.                           EL637
00912                                                                   EL637
00913  7570-COMM-CHECK-REVERSED.                                        EL637
00914                                                                   EL637
00915      IF WS-BROWSE-STARTED                                         EL637
00916         EXEC CICS ENDBR                                           EL637
00917              DATASET (FILE-ID-ERCMKQ2)                            EL637
00918         END-EXEC.                                                 EL637
00919                                                                   EL637
00920      MOVE SPACE                     TO WS-BROWSE-SW.              EL637
00921      MOVE 'Y'                       TO WS-FIRST-TIME-SW.          EL637
00922                                                                   EL637
00923      MOVE PI-SAVE-ERCMKQ2-KEY       TO PI-ERCMKQ2-KEY.            EL637
00924                                                                   EL637
00925      MOVE ER-0000                TO EMI-ERROR.                    EL637
00926      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL637
00927      GO TO 5000-DISPLAY-PAYEE.                                    EL637
00928                                                                   EL637
00929  7580-PAYEE-NOTFND.                                               EL637
00930                                                                   EL637
00931      EXEC CICS ENDBR                                              EL637
00932           DATASET (FILE-ID-ERCMKQ2)                               EL637
00933      END-EXEC.                                                    EL637
00934                                                                   EL637
00935      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL637
00936      MOVE ER-3160                TO EMI-ERROR.                    EL637
00937      MOVE -1                     TO CARL.                         EL637
00938      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL637
00939      GO TO 8200-SEND-DATAONLY.                                    EL637
00940                                                                   EL637
00941  7585-REVERSAL-ERROR.                                             EL637
00942                                                                   EL637
00943      EXEC CICS ENDBR                                              EL637
00944           DATASET (FILE-ID-ERCMKQ2)                               EL637
00945      END-EXEC.                                                    EL637
00946                                                                   EL637
00947      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL637
00948      MOVE ER-3171                TO EMI-ERROR.                    EL637
00949      MOVE -1                     TO MAINTL.                       EL637
00950      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL637
00951      GO TO 8200-SEND-DATAONLY.                                    EL637
00952                                                                   EL637
00953  7590-EXIT.                                                       EL637
00954      EXIT.                                                        EL637
00955      EJECT                                                        EL637
00956                                                                   EL637
00957 ******************************************************************EL637
00958 *                                                                *EL637
00959 *   R E V E R S E   P A Y M E N T S  &  A D J U S T M E N T S    *EL637
00960 *                                                                *EL637
00961 ******************************************************************EL637
00962                                                                   EL637
00963  7600-REVERSE-PMTS-ADJS.                                          EL637
00964                                                                   EL637
00965      EXEC CICS GETMAIN                                            EL637
00966           SET     (ADDRESS OF PENDING-PAY-ADJ)                       CL*11
00967           LENGTH  (ERPYAJ-RECORD-LENGTH)                          EL637
00968           INITIMG (GETMAIN-SPACE)                                 EL637
00969      END-EXEC.                                                    EL637
00970                                                                   EL637
00971      COMPUTE WS-WORK-SEQ-NO = EIBTIME * 10.                       EL637
00972                                                                   EL637
00973      MOVE SPACES                 TO PENDING-PAY-ADJ.              EL637
00974      MOVE 'PY'                   TO PY-RECORD-ID.                 EL637
00975      MOVE MQ-COMPANY-CD-A1       TO PY-COMPANY-CD.                EL637
00976      MOVE MQ-CARRIER-A1          TO PY-CARRIER.                   EL637
00977      MOVE MQ-GROUPING-A1         TO PY-GROUPING.                  EL637
00978      MOVE MQ-PAYEE-A1            TO PY-FIN-RESP.                  EL637
00979      MOVE 'B'                    TO PY-PYMT-TYPE.                 EL637
00980      MOVE WS-CURRENT-BIN-DT      TO PY-LAST-MAINT-DT.             EL637
00981      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.             EL637
00982      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.         EL637
00983      MOVE PI-AR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT.          EL637
00984      MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT.          EL637
00985      MOVE LOW-VALUES             TO PY-BILLED-DATE.               EL637
00986      MOVE LOW-VALUES             TO PY-REPORTED-DT.               EL637
00987      MOVE WS-CURRENT-BIN-DT      TO PY-INPUT-DT.                  EL637
00988      MOVE MQ-CHECK-NUMBER        TO PY-CHECK-NUMBER.              EL637
00989      MOVE ' '                    TO PY-VOID-SW.                   EL637
00990      MOVE LOW-VALUES             TO PY-BILLED-DATE.               EL637
00991      MOVE LOW-VALUE              TO PY-CHECK-WRITTEN-DT.          EL637
00992      MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL.         EL637
00993      MOVE ZEROS                  TO PY-CHECK-QUE-SEQUENCE.        EL637
00994      MOVE LOW-VALUES             TO PY-AR-DATE.                   EL637
00995      MOVE 'V'                    TO PY-CHECK-ORIGIN-SW.           EL637
00996                                                                   EL637
00997      MOVE +0                     TO WS-SUB1.                      EL637
00998                                                                   EL637
00999  7610-PROCESS-PMT-ADJS.                                           EL637
01000                                                                   EL637
01001      ADD +1                      TO WS-SUB1.                      EL637
01002                                                                   EL637
01003      IF WS-SUB1 GREATER THAN +15                                  EL637
01004         GO TO 7690-EXIT.                                          EL637
01005                                                                   EL637
01006      IF MQ-LAST-MAINT-APPLIED (WS-SUB1) EQUAL 'H'                    CL**4
01007          GO TO 7610-PROCESS-PMT-ADJS                                 CL**4
01008      ELSE                                                            CL**4
01009          IF MQ-PYAJ-SEQ    (WS-SUB1) = ZEROS                         CL**4
01010              GO TO 7690-EXIT.                                        CL**4
01011                                                                   EL637
01012 ******************************************************************   CL**5
01013 *    MODIFIED 07/14 TO MATCH FIX APPLIED TO PRODUCTION MODULE.       CL**5
01014 *    THIS WAS APPLIED TO ELIMINATE DUPREC CONDITION ON WRITE OF      CL**5
01015 *    PYAJ RECORDS WRITTEN IN REVERSAL                                CL**5
01016 ******************************************************************   CL**5
01017                                                                      CL**5
01018      ADD +1                          TO WS-WORK-SEQ-HOLD.            CL**4
01019      COMPUTE PY-FILE-SEQ-NO = WS-WORK-SEQ-NO + WS-WORK-SEQ-HOLD.     CL*12
01020                                                                   EL637
01021      MOVE MQ-PYAJ-REC-TYPE    (WS-SUB1) TO PY-RECORD-TYPE.        EL637
01022      MOVE MQ-ACCT-AGENT       (WS-SUB1) TO PY-ACCOUNT.            EL637
01023      MOVE MQ-INVOICE          (WS-SUB1) TO PY-BIL-INV.            EL637
01024      MOVE MQ-REFERENCE        (WS-SUB1) TO PY-REF-NO.             EL637
01025      MOVE MQ-LEDGER-NO        (WS-SUB1) TO PY-GL-CR.              EL637
01026      MOVE MQ-PYAJ-PMT-APPLIED (WS-SUB1) TO PY-PMT-APPLIED.        EL637
01027                                                                   EL637
01028      COMPUTE PY-ENTRY-AMT = MQ-PYAJ-AMT (WS-SUB1) * -1.           EL637
01029                                                                   EL637
01030      MOVE 'CHECK REVERSAL'           TO PY-ENTRY-COMMENT.         EL637
01031                                                                   EL637
01032      EXEC CICS WRITE                                              EL637
01033           DATASET   (FILE-ID-ERPYAJ)                              EL637
01034           FROM      (PENDING-PAY-ADJ)                             EL637
01035           RIDFLD    (PY-CONTROL-PRIMARY)                          EL637
01036      END-EXEC.                                                    EL637
01037                                                                   EL637
01038      GO TO 7610-PROCESS-PMT-ADJS.                                 EL637
01039                                                                   EL637
01040  7690-EXIT.                                                       EL637
01041      EXIT.                                                        EL637
01042                                                                   EL637
01043      EJECT                                                        EL637
01044                                                                   EL637
01045                                                                   EL637
01046 ******************************************************************EL637
01047 *                                                                *EL637
01048 *       R E V E R S E   C O M M I S S I O N   C H E C K          *EL637
01049 *              COMMISSION CHECK FILE (ERCMCK)                    *   CL**8
01050 *                                                                *EL637
01051 ******************************************************************EL637
01052                                                                   EL637
01053  7700-REVERSE-COMM-CHECK.                                         EL637
01054                                                                   EL637
01055      EXEC CICS HANDLE CONDITION                                   EL637
01056          NOTFND    (7790-EXIT)                                    EL637
01057      END-EXEC.                                                    EL637
01058                                                                   EL637
01059      MOVE MQ-COMPANY-CD          TO ERCMCK-COMPANY-CD.            EL637
01060      MOVE MQ-CHEK-CSR            TO ERCMCK-CSR.                      CL**5
01061      MOVE MQ-CHEK-CARRIER        TO ERCMCK-CARRIER.               EL637
01062      MOVE MQ-CHEK-GROUPING       TO ERCMCK-GROUPING.              EL637
01063      MOVE MQ-CHEK-PAYEE          TO ERCMCK-PAYEE.                 EL637
01064      MOVE MQ-CHEK-PAYEE-SEQ      TO ERCMCK-PAYEE-SEQ.                CL**2
01065      MOVE MQ-CHEK-SEQ-NO         TO ERCMCK-SEQUENCE-NO.           EL637
01066                                                                   EL637
01067      EXEC CICS READ                                               EL637
01068          SET     (ADDRESS OF COMM-CHECK-RECORDS)                     CL*11
01069          DATASET (FILE-ID-ERCMCK)                                 EL637
01070          RIDFLD  (ERCMCK-KEY)                                     EL637
01071          UPDATE                                                   EL637
01072      END-EXEC.                                                    EL637
01073                                                                   EL637
01074      IF MQ-TEXT                                                      CL**4
01075          PERFORM 7900-REVERSE-CHECK-TEXT-RECS THRU 7990-EXIT         CL**4
01076      ELSE                                                            CL**4
01077          PERFORM 7800-REVERSE-CHECK-WORK-RECS THRU 7890-EXIT.        CL**4
01078                                                                   EL637
01079      EXEC CICS DELETE                                             EL637
01080           DATASET (FILE-ID-ERCMCK)                                EL637
01081      END-EXEC.                                                    EL637
01082                                                                   EL637
01083  7790-EXIT.                                                       EL637
01084      EXIT.                                                        EL637
01085                                                                   EL637
01086      EJECT                                                        EL637
01087                                                                   EL637
01088 ******************************************************************EL637
01089 *                                                                *EL637
01090 *       R E V E R S E   C H E C K   W O R K   R E C S .          *EL637
01091 *               CHECK WORK FILE (ERCKWK)                         *   CL**8
01092 *                                                                *EL637
01093 ******************************************************************EL637
01094                                                                   EL637
01095  7800-REVERSE-CHECK-WORK-RECS.                                    EL637
01096                                                                   EL637
01097      EXEC CICS HANDLE CONDITION                                   EL637
01098          NOTFND    (7810-PROCESS-CHECK-WORK-RECS)                 EL637
01099      END-EXEC.                                                    EL637
01100                                                                   EL637
01101      MOVE +0                     TO WS-SUB1.                      EL637
01102                                                                   EL637
01103  7810-PROCESS-CHECK-WORK-RECS.                                    EL637
01104                                                                   EL637
01105      ADD +1                      TO WS-SUB1.                      EL637
01106                                                                   EL637
01107      IF WS-SUB1 GREATER THAN +15                                  EL637
01108         GO TO 7870-UPDATE-HEADER-REC.                             EL637
01109                                                                   EL637
01110      IF CK-CKWK-PAYEE      (WS-SUB1) = SPACES                     EL637
01111         GO TO 7870-UPDATE-HEADER-REC.                             EL637
01112                                                                   EL637
01113      MOVE CK-COMPANY-CD              TO ERCKWK-COMPANY-CD.        EL637
01114      MOVE CK-CKWK-CSR      (WS-SUB1) TO ERCKWK-CSR.                  CL**5
01115      MOVE CK-CKWK-CARRIER  (WS-SUB1) TO ERCKWK-CARRIER.           EL637
01116      MOVE CK-CKWK-GROUPING (WS-SUB1) TO ERCKWK-GROUPING.          EL637
01117      MOVE CK-CKWK-PAYEE    (WS-SUB1) TO ERCKWK-PAYEE.             EL637
01118      MOVE CK-CKWK-PAYEE-SEQ (WS-SUB1)                                CL**2
01119                                      TO ERCKWK-PAYEE-SEQ.            CL**2
01120      MOVE CK-CKWK-SEQ-NO   (WS-SUB1) TO ERCKWK-SEQUENCE-NO.       EL637
01121                                                                   EL637
01122      EXEC CICS READ                                               EL637
01123          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*11
01124          DATASET (FILE-ID-ERCKWK)                                 EL637
01125          RIDFLD  (ERCKWK-KEY)                                     EL637
01126          UPDATE                                                   EL637
01127      END-EXEC.                                                    EL637
01128                                                                   EL637
01129      MOVE 'Y'                        TO WS-ERCKWK-PROCESS-SW.     EL637
01130                                                                   EL637
01131      MOVE WS-CURRENT-BIN-DT          TO CW-RECORDED-DT.           EL637
01132      MOVE PI-PROCESSOR-ID            TO CW-RECORDED-BY.           EL637
01133      MOVE EIBTIME                    TO CW-LAST-MAINT-HHMMSS.     EL637
01134                                                                   EL637
01135      MOVE LOW-VALUES                 TO CW-RELEASE-DT             EL637
01136                                         CW-CREDIT-SELECT-DT       EL637
01137                                         CW-CREDIT-ACCEPT-DT.      EL637
01138                                                                   EL637
01139      EXEC CICS REWRITE                                            EL637
01140           FROM    (CHECK-WORK-RECORDS)                            EL637
01141           DATASET (FILE-ID-ERCKWK)                                EL637
01142      END-EXEC.                                                    EL637
01143                                                                   EL637
01144      GO TO 7810-PROCESS-CHECK-WORK-RECS.                          EL637
01145                                                                   EL637
01146  7870-UPDATE-HEADER-REC.                                          EL637
01147                                                                   EL637
01148      MOVE +0                        TO ERCKWK-SEQUENCE-NO.        EL637
01149                                                                   EL637
01150      EXEC CICS HANDLE CONDITION                                   EL637
01151          NOTFND    (7880-HEADER-NOTFND)                           EL637
01152      END-EXEC.                                                    EL637
01153                                                                   EL637
01154      EXEC CICS READ                                               EL637
01155          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*11
01156          DATASET (FILE-ID-ERCKWK)                                 EL637
01157          RIDFLD  (ERCKWK-KEY)                                     EL637
01158          UPDATE                                                   EL637
01159      END-EXEC.                                                    EL637
01160                                                                   EL637
01161      MOVE WS-CURRENT-BIN-DT          TO CW-RECORDED-DT.           EL637
01162      MOVE PI-PROCESSOR-ID            TO CW-RECORDED-BY.           EL637
01163      MOVE EIBTIME                    TO CW-LAST-MAINT-HHMMSS.     EL637
01164                                                                   EL637
01165      MOVE LOW-VALUES                 TO CW-RELEASE-DT             EL637
01166                                         CW-CREDIT-SELECT-DT       EL637
01167                                         CW-CREDIT-ACCEPT-DT.      EL637
01168                                                                   EL637
01169      EXEC CICS REWRITE                                            EL637
01170           FROM    (CHECK-WORK-RECORDS)                            EL637
01171           DATASET (FILE-ID-ERCKWK)                                EL637
01172      END-EXEC.                                                    EL637
01173                                                                   EL637
01174      GO TO 7890-EXIT.                                             EL637
01175                                                                   EL637
01176  7880-HEADER-NOTFND.                                              EL637
01177                                                                   EL637
01178      IF WS-ERCKWK-PROCESSED                                       EL637
01179         NEXT SENTENCE                                             EL637
01180      ELSE                                                         EL637
01181         GO TO 7890-EXIT.                                          EL637
01182                                                                   EL637
01183      EXEC CICS SYNCPOINT ROLLBACK                                 EL637
01184      END-EXEC.                                                    EL637
01185                                                                   EL637
01186      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL637
01187      MOVE ER-3161                TO EMI-ERROR.                    EL637
01188      MOVE -1                     TO CARL.                         EL637
01189      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL637
01190      GO TO 8200-SEND-DATAONLY.                                    EL637
01191                                                                   EL637
01192  7890-EXIT.                                                       EL637
01193      EXIT.                                                           CL**4
01194                                                                      CL**4
01195      EJECT                                                           CL**4
01196                                                                      CL**4
01197 ******************************************************************   CL**4
01198 *                                                                *   CL**4
01199 *       R E V E R S E   C H E C K   T E X T   R E C S .          *   CL**4
01200 *              COMMISSION CHECK FILE (ERCMCK)                    *   CL**8
01201 *                                                                *   CL**4
01202 ******************************************************************   CL**4
01203                                                                      CL**4
01204  7900-REVERSE-CHECK-TEXT-RECS.                                       CL**4
01205                                                                      CL**4
01206      EXEC CICS HANDLE CONDITION                                      CL**4
01207          NOTFND    (7990-EXIT)                                       CL**8
01208      END-EXEC.                                                       CL**4
01209                                                                      CL**4
01210      MOVE MQ-COMPANY-CD              TO ERCKWK-COMPANY-CD.           CL**4
01211      MOVE MQ-CHEK-CSR                TO ERCKWK-CSR.                  CL**5
01212      MOVE MQ-CHEK-CARRIER            TO ERCKWK-CARRIER.              CL**4
01213      MOVE MQ-CHEK-GROUPING           TO ERCKWK-GROUPING.             CL**4
01214      MOVE MQ-CHEK-PAYEE              TO ERCKWK-PAYEE.                CL**4
01215      MOVE MQ-CHEK-PAYEE-SEQ          TO ERCKWK-PAYEE-SEQ.            CL**4
01216      MOVE +8999                      TO ERCKWK-SEQUENCE-NO.          CL**4
01217                                                                      CL**4
01218  7910-PROCESS-CHECK-TEXT-RECS.                                       CL**4
01219                                                                      CL**4
01220      IF ERCKWK-SEQUENCE-NO LESS THAN WS-MAX-TEXT-SEQ                 CL**4
01221          ADD +1                      TO ERCKWK-SEQUENCE-NO           CL**4
01222      ELSE                                                            CL**4
01223          GO TO 7990-EXIT.                                            CL**4
01224                                                                      CL**4
01225      EXEC CICS READ                                                  CL**4
01226          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*11
01227          DATASET (FILE-ID-ERCKWK)                                    CL**4
01228          RIDFLD  (ERCKWK-KEY)                                        CL**4
01229          UPDATE                                                      CL**4
01230      END-EXEC.                                                       CL**4
01231                                                                      CL**4
01232      MOVE 'Y'                        TO WS-ERCKWK-PROCESS-SW.        CL**4
01233                                                                      CL**4
01234      MOVE WS-CURRENT-BIN-DT          TO CW-RECORDED-DT.              CL**4
01235      MOVE PI-PROCESSOR-ID            TO CW-RECORDED-BY.              CL**4
01236      MOVE EIBTIME                    TO CW-LAST-MAINT-HHMMSS.        CL**4
01237                                                                      CL**4
01238      MOVE LOW-VALUES                 TO CW-RELEASE-DT                CL**4
01239                                         CW-CREDIT-SELECT-DT          CL**4
01240                                         CW-CREDIT-ACCEPT-DT.         CL**4
01241                                                                      CL**4
01242      EXEC CICS REWRITE                                               CL**4
01243           FROM    (CHECK-WORK-RECORDS)                               CL**4
01244           DATASET (FILE-ID-ERCKWK)                                   CL**4
01245      END-EXEC.                                                       CL**4
01246                                                                      CL**4
01247      GO TO 7910-PROCESS-CHECK-TEXT-RECS.                             CL**4
01248                                                                      CL**4
01249  7990-EXIT.                                                          CL**4
01250      EXIT.                                                        EL637
01251                                                                   EL637
01252      EJECT                                                        EL637
01253                                                                   EL637
01254 ******************************************************************EL637
01255 *                                                                *EL637
01256 *             S E N D    I N I T I A L   M A P                   *EL637
01257 *                                                                *EL637
01258 ******************************************************************EL637
01259                                                                   EL637
01260  8100-SEND-INITIAL-MAP.                                           EL637
01261                                                                   EL637
01262      MOVE WS-CURRENT-DT          TO DATEO.                        EL637
01263      MOVE EIBTIME                TO TIME-IN.                      EL637
01264      MOVE TIME-OUT               TO TIMEO.                        EL637
01265      MOVE -1                     TO MAINTL.                       EL637
01266                                                                   EL637
01267      MOVE EMI-MESSAGE-AREA (1)   TO ERMESGO.                      EL637
01268                                                                   EL637
01269      EXEC CICS SEND                                               EL637
01270          MAP      (EL637A)                                        EL637
01271          MAPSET   (MAPSET-EL637S)                                 EL637
01272          FROM     (EL637AI)                                       EL637
01273          ERASE                                                    EL637
01274          CURSOR                                                   EL637
01275      END-EXEC.                                                    EL637
01276                                                                   EL637
01277      GO TO 9100-RETURN-TRAN.                                      EL637
01278                                                                   EL637
01279      EJECT                                                        EL637
01280                                                                   EL637
01281 ******************************************************************EL637
01282 *                                                                *EL637
01283 *              S E N D    D A T A O N L Y                        *EL637
01284 *                                                                *EL637
01285 ******************************************************************EL637
01286                                                                   EL637
01287  8200-SEND-DATAONLY.                                              EL637
01288                                                                   EL637
01289      MOVE WS-CURRENT-DT          TO DATEO.                        EL637
01290      MOVE EIBTIME                TO TIME-IN.                      EL637
01291      MOVE TIME-OUT               TO TIMEO.                        EL637
01292                                                                   EL637
01293      MOVE EMI-MESSAGE-AREA (1)   TO ERMESGO.                      EL637
01294                                                                   EL637
01295      EXEC CICS SEND                                               EL637
01296           MAP      (EL637A)                                       EL637
01297           MAPSET   (MAPSET-EL637S)                                EL637
01298           FROM     (EL637AI)                                      EL637
01299           DATAONLY                                                EL637
01300           CURSOR                                                  EL637
01301      END-EXEC.                                                    EL637
01302                                                                   EL637
01303      GO TO 9100-RETURN-TRAN.                                      EL637
01304                                                                   EL637
01305      EJECT                                                        EL637
01306                                                                   EL637
01307  8300-SEND-TEXT.                                                  EL637
01308      EXEC CICS SEND TEXT                                          EL637
01309          FROM     (LOGOFF-TEXT)                                   EL637
01310          LENGTH   (LOGOFF-LENGTH)                                 EL637
01311          ERASE                                                    EL637
01312          FREEKB                                                   EL637
01313      END-EXEC.                                                    EL637
01314                                                                   EL637
01315      EXEC CICS RETURN                                             EL637
01316      END-EXEC.                                                    EL637
01317                                                                   EL637
01318                                                                   EL637
01319  8400-LOG-JOURNAL-RECORD.                                         EL637
01320      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL637
01321      MOVE THIS-PGM                TO JP-PROGRAM-ID.               EL637
01322                                                                   EL637
01323 *    EXEC CICS JOURNAL                                            EL637
01324 *        JFILEID     (PI-JOURNAL-FILE-ID)                         EL637
01325 *        JTYPEID     ('EL')                                       EL637
01326 *        FROM        (JOURNAL-RECORD)                             EL637
01327 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   EL637
01328 *        END-EXEC.                                                EL637
01329                                                                   EL637
01330  8500-DATE-CONVERT.                                               EL637
01331      EXEC CICS LINK                                               EL637
01332          PROGRAM  (LINK-ELDATCV)                                  EL637
01333          COMMAREA (DATE-CONVERSION-DATA)                          EL637
01334          LENGTH   (DC-COMM-LENGTH)                                EL637
01335      END-EXEC.                                                    EL637
01336                                                                   EL637
01337  8500-EXIT.                                                       EL637
01338      EXIT.                                                        EL637
01339                                                                   EL637
01340      EJECT                                                        EL637
01341                                                                   EL637
01342  8600-DEEDIT.                                                     EL637
01343                                                                   EL637
01344      EXEC CICS BIF DEEDIT                                         EL637
01345           FIELD   (DEEDIT-FIELD)                                  EL637
01346           LENGTH  (12)                                            EL637
01347      END-EXEC.                                                    EL637
01348                                                                   EL637
01349  8800-UNAUTHORIZED-ACCESS.                                        EL637
01350      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL637
01351      GO TO 8300-SEND-TEXT.                                        EL637
01352                                                                   EL637
01353  8810-PF23.                                                       EL637
01354      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL637
01355      MOVE XCTL-EL005             TO PGM-NAME.                     EL637
01356      GO TO 9300-XCTL.                                             EL637
01357                                                                   EL637
01358  9200-RETURN-MAIN-MENU.                                           EL637
01359      MOVE XCTL-EL626             TO PGM-NAME.                     EL637
01360      GO TO 9300-XCTL.                                             EL637
01361                                                                   EL637
01362  9000-RETURN-CICS.                                                EL637
01363      EXEC CICS RETURN                                             EL637
01364      END-EXEC.                                                    EL637
01365                                                                   EL637
01366  9100-RETURN-TRAN.                                                EL637
01367                                                                   EL637
01368      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL637
01369      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.         EL637
01370                                                                   EL637
01371      EXEC CICS RETURN                                             EL637
01372          TRANSID    (TRANS-EXJB)                                  EL637
01373          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL637
01374          LENGTH     (PI-COMM-LENGTH)                              EL637
01375      END-EXEC.                                                    EL637
01376                                                                   EL637
01377  9300-XCTL.                                                       EL637
01378      EXEC CICS XCTL                                               EL637
01379          PROGRAM    (PGM-NAME)                                    EL637
01380          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL637
01381          LENGTH     (PI-COMM-LENGTH)                              EL637
01382      END-EXEC.                                                    EL637
01383                                                                   EL637
01384  9400-CLEAR.                                                      EL637
01385      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL637
01386      GO TO 9300-XCTL.                                             EL637
01387                                                                   EL637
01388  9500-PF12.                                                       EL637
01389      MOVE XCTL-EL010             TO PGM-NAME.                     EL637
01390      GO TO 9300-XCTL.                                             EL637
01391                                                                   EL637
01392  9600-PGMID-ERROR.                                                EL637
01393                                                                   EL637
01394      EXEC CICS HANDLE CONDITION                                   EL637
01395          PGMIDERR    (8300-SEND-TEXT)                             EL637
01396      END-EXEC.                                                    EL637
01397                                                                   EL637
01398      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL637
01399      MOVE ' '                    TO PI-ENTRY-CD-1.                EL637
01400      MOVE XCTL-EL005             TO PGM-NAME.                     EL637
01401      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL637
01402      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL637
01403      GO TO 9300-XCTL.                                             EL637
01404                                                                   EL637
01405  9900-ERROR-FORMAT.                                               EL637
01406                                                                   EL637
01407      IF NOT EMI-ERRORS-COMPLETE                                   EL637
01408          MOVE LINK-EL001         TO PGM-NAME                      EL637
01409          EXEC CICS LINK                                           EL637
01410              PROGRAM    (PGM-NAME)                                EL637
01411              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL637
01412              LENGTH     (EMI-COMM-LENGTH)                         EL637
01413          END-EXEC.                                                EL637
01414                                                                   EL637
01415  9900-EXIT.                                                       EL637
01416      EXIT.                                                        EL637
01417                                                                   EL637
01418  9990-ABEND.                                                      EL637
01419      MOVE LINK-EL004             TO PGM-NAME.                     EL637
01420      MOVE DFHEIBLK               TO EMI-LINE1.                    EL637
01421      EXEC CICS LINK                                               EL637
01422          PROGRAM   (PGM-NAME)                                     EL637
01423          COMMAREA  (EMI-LINE1)                                    EL637
01424          LENGTH    (72)                                           EL637
01425      END-EXEC.                                                    EL637
01426                                                                   EL637
01427      MOVE -1                     TO PFENTERL.                     EL637
01428                                                                   EL637
01429      GO TO 8200-SEND-DATAONLY.                                    EL637
01430                                                                   EL637
01431      GOBACK.                                                      EL637
01432                                                                   EL637
01433      EJECT                                                        EL637
01434                                                                   EL637
01435  9995-SECURITY-VIOLATION.                                         EL637
01436                              COPY ELCSCTP.                        EL637
01437                                                                   EL637
01438  9995-EXIT.                                                       EL637
01439      EXIT.                                                        EL637
01440                                                                   EL637
