00001  IDENTIFICATION DIVISION.                                         02/27/96
00002                                                                   EL6351
00003  PROGRAM-ID.                 EL6351.                                 LV031
00004 *              PROGRAM CONVERTED BY                                  CL*30
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*30
00006 *              CONVERSION DATE 02/12/96 09:52:13.                    CL*30
00007 *                            VMOD=2.031                              CL*31
00008 *                                                                 EL6351
00008 *                                                                 EL6351
00009 *AUTHOR.        LOGIC,INC.                                           CL*30
00010 *               DALLAS, TEXAS.                                       CL*30
00011                                                                   EL6351
00012 *DATE-COMPILED.                                                      CL*30
00013                                                                   EL6351
00014 *SECURITY.   *****************************************************   CL*30
00015 *            *                                                   *   CL*30
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*30
00017 *            *                                                   *   CL*30
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*30
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*30
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*30
00021 *            *                                                   *   CL*30
00022 *            *****************************************************   CL*30
00023                                                                   EL6351
00024 *REMARKS.                                                            CL**4
00025 *        TRANSACTION - EXJ5 - COMPENSATION PAYMENTS/ADJUSTMENTS.     CL**4
00026                                                                   EL6351
00027  ENVIRONMENT DIVISION.                                            EL6351
00028  DATA DIVISION.                                                   EL6351
00029  EJECT                                                            EL6351
00030  WORKING-STORAGE SECTION.                                         EL6351
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6351
00032  77  FILLER  PIC X(32)  VALUE '*    EL6351 WORKING STORAGE    *'. EL6351
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.031 *********'.    CL*31
00034                                                                   EL6351
00035      COPY ELCSCTM.                                                   CL**9
00036      COPY ELCSCRTY.                                                  CL**9
00037      COPY MPCSCRT.                                                   CL**9
00038                                                                   EL6351
00039     EJECT                                                         EL6351
00040                                                                   EL6351
00041  01  STANDARD-AREAS.                                              EL6351
00042      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.         EL6351
00043      12  MAP-NAME            PIC  X(8)       VALUE 'EL635B '.     EL6351
00044      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL6351S'.     EL6351
00045      12  SCREEN-NUMBER       PIC  X(4)       VALUE '633B'.        EL6351
00046      12  TRANS-ID            PIC  X(4)       VALUE 'EXJ5'.        EL6351
00047      12  THIS-PGM            PIC  X(8)       VALUE 'EL6351'.      EL6351
00048      12  PGM-NAME            PIC  X(8).                           EL6351
00049      12  TIME-IN             PIC S9(7).                           EL6351
00050      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL6351
00051          16  FILLER          PIC  X.                              EL6351
00052          16  TIME-OUT        PIC  9(2)V9(2).                      EL6351
00053          16  FILLER          PIC  X(2).                           EL6351
00054      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL6351
00055      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL6351
00056      12  XCTL-EL126          PIC  X(8)       VALUE 'EL126'.          CL**9
00057      12  XCTL-EL626          PIC  X(8)       VALUE 'EL626'.          CL**9
00058      12  XCTL-EM626          PIC  X(8)       VALUE 'EM626'.          CL**8
00059      12  XCTL-GL800          PIC  X(8)       VALUE 'GL800'.          CL**9
00060      12  XCTL-652            PIC  X(8)       VALUE 'EL652'.       EL6351
00061      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL6351
00062      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL6351
00063      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.     EL6351
00064      12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.      EL6351
00065      12  MPPYAJ-FILE-ID      PIC  X(8)       VALUE 'MPPYAJ'.         CL**8
00066      12  MPPRCN-FILE-ID      PIC  X(8)       VALUE 'MPPRCN'.         CL**8
00067      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.      EL6351
00068      12  RECV-FILE-ID        PIC  X(8)       VALUE 'ERRECV'.         CL**5
00069      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.        EL6351
00070      12  WS-CURRENT-MDY      PIC  X(6)       VALUE SPACES.           CL*19
00071      12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.        EL6351
00072      12  WS-COMMENT-FULL.                                            CL*19
00073          16  WS-COMMENT-24-POS    PIC  X(24) VALUE SPACES.           CL*19
00074          16  FILLER               PIC  X(06) VALUE SPACES.           CL*19
00075      12  WORK-SEQ-NO         PIC S9(9)                  COMP-3.   EL6351
00076      12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.         EL6351
00077          88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'      CL*19
00078                                                     'S' 'T' 'U'      CL*19
00079                                                     'X' 'Y' 'Z'      CL*19
00080                                                     'F'.             CL*19
00081          88  MON-VALID-REC-TYPE              VALUE  'R' 'D' 'C'      CL*19
00082                                                     'Z'.             CL*19
00083          88  ANL-VALID-REC-TYPE              VALUE  'R' 'D' 'C'      CL*19
00084                                                     'S' 'T' 'U'      CL*19
00085                                                     'X' 'Y' 'Z'.     CL*19
00086          88  NCL-VALID-REC-TYPE              VALUE  'R' 'D' 'C'      CL*19
00087                                                     'X' 'Y' 'Z'.     CL*28
00088      12  ERROR-3183          PIC  X          VALUE SPACE.            CL*16
00089          88  ER-3183-OCCURRED                VALUE 'Y'.              CL*16
00090                                                                      CL*13
00091      12  DEEDIT-FIELD            PIC X(11).                       EL6351
00092      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(11).          CL*24
00093      12  DATE-TEST-AREA          PIC 9(6).                           CL*24
00094      12  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.                 CL*24
00095          16  DATE-TEST-MM        PIC 99.                             CL*24
00096          16  DATE-TEST-DD        PIC 99.                             CL*24
00097          16  DATE-TEST-YY        PIC 99.                             CL*24
00098      12  DIVIDE-RESULT           PIC 99.                             CL*24
00099      12  DIVIDE-REMAINDER        PIC 9.                              CL*24
00100      12  WS-EOM-DTS     OCCURS 5 TIMES                               CL*24
00101                                 INDEXED BY MINDEX.                   CL*24
00102          16  WS-EOM-DT           PIC XX.                             CL*24
00103      12  WS-INP-DTS     OCCURS 5 TIMES                               CL*24
00104                                 INDEXED BY DINDEX.                   CL*24
00105          16  WS-INP-DT           PIC XX.                             CL*24
00106      12  WS-EDITED-AMTS OCCURS 6 TIMES                            EL6351
00107                                 INDEXED BY WS-INDX.               EL6351
00108          16  WS-EDITED-AMT       PIC S9(9)V99.                    EL6351
00109                                                                      CL**3
00110      12  WS-ENTRY-AMT        PIC S9(7)V99      VALUE ZEROS.          CL*12
00111      12  WS-PREV-PF5         PIC X.                                  CL*17
00112      12  WS-ACCEPT.                                                  CL*17
00113          16  WS-ACCEPT-1   OCCURS 5 TIMES INDEXED BY W-INDX          CL*17
00114                              PIC  X.                                 CL*17
00115                                                                   EL6351
00116  01  ACCESS-KEYS.                                                 EL6351
00117      12  ERPYAJ-KEY.                                              EL6351
00118          16  PYAJ-COMP-CD        PIC  X      VALUE SPACE.         EL6351
00119          16  PYAJ-CARRIER        PIC  X      VALUE SPACES.        EL6351
00120          16  PYAJ-GROUPING       PIC  X(6)   VALUE SPACES.        EL6351
00121          16  PYAJ-FIN-RESP       PIC  X(10)  VALUE SPACES.        EL6351
00122          16  PYAJ-ACCOUNT        PIC  X(10)  VALUE SPACES.        EL6351
00123          16  PYAJ-FILE-SEQ-NO    PIC S9(8)   VALUE +0   COMP.     EL6351
00124          16  PYAJ-RECORD-TYPE    PIC  X      VALUE SPACES.        EL6351
00125                                                                   EL6351
00126      12  ERPYAJ-RECORD-LENGTH    PIC S9(4)   VALUE +200 COMP.     EL6351
00127      12  ERPYAJ-JOURNAL-LENGTH   PIC S9(4)   VALUE +223 COMP.     EL6351
00128                                                                   EL6351
00129      12  ERCOMP-KEY.                                              EL6351
00130          16  COMP-COMP-CD        PIC  X      VALUE SPACE.         EL6351
00131          16  COMP-CARRIER        PIC  X      VALUE SPACES.        EL6351
00132          16  COMP-GROUPING       PIC  X(6)   VALUE SPACES.        EL6351
00133          16  COMP-FIN-RESP       PIC  X(10)  VALUE SPACES.        EL6351
00134          16  COMP-ACCOUNT        PIC  X(10)  VALUE SPACES.        EL6351
00135          16  COMP-RECORD-TYPE    PIC  X      VALUE SPACES.        EL6351
00136                                                                      CL*17
00137      12  ERCOMP-RECORD-LENGTH    PIC S9(4)   VALUE +700 COMP.        CL*23
00138                                                                      CL*17
00139      12  ERRECV-KEY.                                                 CL**5
00140          16  RECV-COMP-CD        PIC X       VALUE SPACES.           CL**5
00141          16  RECV-TYPE           PIC X       VALUE SPACES.           CL**5
00142          16  RECV-CARRIER        PIC X       VALUE SPACES.           CL**5
00143          16  RECV-GROUPING       PIC X(6)    VALUE SPACES.           CL**5
00144          16  RECV-BAL-LVL        PIC X       VALUE SPACES.           CL**5
00145          16  RECV-ENTRY-TYPE     PIC X       VALUE SPACES.           CL*26
00146          16  RECV-FIN-RESP       PIC X(10)   VALUE SPACES.           CL**5
00147          16  RECV-ACCOUNT        PIC X(10)   VALUE SPACES.           CL**5
00148          16  RECV-INVOICE        PIC X(6)    VALUE SPACES.           CL**5
00149          16  RECV-REFERENCE      PIC X(12)   VALUE SPACES.           CL**5
00150          16  RECV-RESPONSIBLE    PIC X       VALUE SPACES.           CL*26
00151          16  RECV-RECORD-TYPE    PIC X       VALUE SPACES.           CL**5
00152          16  RECV-RECORD-SEQ     PIC S9(4)   VALUE +0 COMP.          CL**5
00153                                                                      CL**8
00154      12  MPPRCN-KEY.                                                 CL**8
00155          16  MPPRCN-COMPANY-CD       PIC X       VALUE SPACES.       CL**8
00156          16  MPPRCN-INVOICE          PIC X(6)    VALUE SPACES.       CL**8
00157          16  MPPRCN-RECORD-SEQU      PIC S9(7)   VALUE +0  COMP-3.   CL**8
00158                                                                      CL**8
00159  EJECT                                                            EL6351
00160  01  ERROR-NUMBERS.                                               EL6351
00161      12  ER-0000             PIC  X(4)       VALUE '0000'.        EL6351
00162      12  ER-0008             PIC  X(4)       VALUE '0008'.        EL6351
00163      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL6351
00164      12  ER-0070             PIC  X(4)       VALUE '0070'.        EL6351
00165      12  ER-0194             PIC  X(4)       VALUE '0194'.        EL6351
00166      12  ER-0195             PIC  X(4)       VALUE '0195'.        EL6351
00167      12  ER-0197             PIC  X(4)       VALUE '0197'.        EL6351
00168      12  ER-0587             PIC  X(4)       VALUE '0587'.           CL*24
00169      12  ER-0714             PIC  X(4)       VALUE '0714'.           CL*24
00170      12  ER-0761             PIC  X(4)       VALUE '0761'.           CL*24
00171      12  ER-2232             PIC  X(4)       VALUE '2232'.        EL6351
00172      12  ER-2233             PIC  X(4)       VALUE '2233'.        EL6351
00173      12  ER-2234             PIC  X(4)       VALUE '2234'.        EL6351
00174      12  ER-2235             PIC  X(4)       VALUE '2235'.        EL6351
00175      12  ER-2236             PIC  X(4)       VALUE '2236'.        EL6351
00176      12  ER-2245             PIC  X(4)       VALUE '2245'.        EL6351
00177      12  ER-2562             PIC  X(4)       VALUE '2562'.        EL6351
00178      12  ER-2587             PIC  X(4)       VALUE '2587'.        EL6351
00179      12  ER-2588             PIC  X(4)       VALUE '2588'.        EL6351
00180      12  ER-2595             PIC  X(4)       VALUE '2595'.        EL6351
00181      12  ER-2596             PIC  X(4)       VALUE '2596'.        EL6351
00182      12  ER-7806             PIC  X(4)       VALUE '7806'.           CL*19
00183      12  ER-9296             PIC  X(4)       VALUE '9296'.           CL*10
00184      12  ER-3146             PIC  X(4)       VALUE '3146'.           CL**2
00185      12  ER-3172             PIC  X(4)       VALUE '3172'.           CL**5
00186      12  ER-3175             PIC  X(4)       VALUE '3175'.           CL**8
00187      12  ER-3177             PIC  X(4)       VALUE '3177'.           CL**8
00188      12  ER-3178             PIC  X(4)       VALUE '3178'.           CL*11
00189      12  ER-3179             PIC  X(4)       VALUE '3179'.           CL*11
00190      12  ER-3180             PIC  X(4)       VALUE '3180'.           CL*11
00191      12  ER-3183             PIC  X(4)       VALUE '3183'.           CL*16
00192      12  ER-3184             PIC  X(4)       VALUE '3184'.           CL*17
00193      12  ER-9280             PIC  X(4)       VALUE '9280'.           CL**8
00194      12  ER-9094             PIC  X(4)       VALUE '9094'.           CL**9
00195      12  ER-9095             PIC  X(4)       VALUE '9095'.           CL**9
00196      12  ER-9179             PIC  X(4)       VALUE '9179'.           CL*13
00197                                                                      CL**8
00198  EJECT                                                            EL6351
00199      COPY ELCDATE.                                                   CL*18
00200  EJECT                                                            EL6351
00201      COPY ELCLOGOF.                                                  CL*18
00202  EJECT                                                            EL6351
00203      COPY ELCATTR.                                                   CL*18
00204  EJECT                                                            EL6351
00205      COPY ELCEMIB.                                                   CL*18
00206  EJECT                                                            EL6351
00207      COPY ELCINTF.                                                   CL*18
00208      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL6351
00209          16  PI-PYAJ-FILE-SW         PIC  X.                      EL6351
00210              88  END-OF-ACCT                 VALUE 'A'.           EL6351
00211              88  END-OF-FILE                 VALUE 'X'.           EL6351
00212              88  NO-RECORDS                  VALUE 'Y'.           EL6351
00213              88  NOT-OPEN                    VALUE 'Z'.           EL6351
00214          16  PI-PREV-FUNCTION        PIC  X.                      EL6351
00215          16  PI-SAV-FUNCTION         PIC  X.                      EL6351
00216          16  PI-SEQ-NOS.                                          EL6351
00217              20  FILLER  OCCURS  5 TIMES                          EL6351
00218                              INDEXED BY NDX.                      EL6351
00219                  24  PI-REC-TYPE     PIC  X.                      EL6351
00220                  24  PI-FILE-SEQ-NO  PIC S9(8).                   EL6351
00221          16  PI-SAV-ENDING-PYAJ-KEY.                              EL6351
00222              20  PI-SAV-COMP-CD      PIC  X.                      EL6351
00223              20  PI-SAV-CARRIER      PIC  X.                      EL6351
00224              20  PI-SAV-GROUPING     PIC  X(3).                   EL6351
00225              20  PI-SAV-FIN-RESP     PIC  X(6).                   EL6351
00226              20  PI-SAV-ACCOUNT      PIC  X(6).                   EL6351
00227              20  PI-SAV-FILE-SEQ-NO  PIC S9(8)          COMP.     EL6351
00228              20  PI-SAV-RECORD-TYPE  PIC  X.                      EL6351
00229          16  PI-ACCEPT-DESC          PIC  X(6).                      CL*17
00230          16  PI-ACCEPT               PIC  X(5).                      CL*17
00231          16  PI-SAVE-COMP-BAL-CODE   PIC  X.                         CL*17
00232          16  PI-SEQ-FIRST-TIME-SW    PIC  X.                         CL*24
00233              88  PI-SEQ-FST-TIME       VALUE 'Y'.                    CL*24
00234          16  FILLER                  PIC  X(557).                    CL*30
00235  EJECT                                                            EL6351
00236      COPY ELCJPFX.                                                   CL*18
00237                              PIC  X(223).                         EL6351
00238  EJECT                                                            EL6351
00239      COPY ELCAID.                                                    CL*18
00240                                                                   EL6351
00241  01  FILLER    REDEFINES DFHAID.                                  EL6351
00242      12  FILLER              PIC  X(8).                           EL6351
00243      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.      EL6351
00244  EJECT                                                            EL6351
00245      COPY EL6351S.                                                   CL*18
00246                                                                   EL6351
00247  01  MAP-EL635B   REDEFINES  EL635BI.                                CL**5
00248      12  FILLER                  PIC  X(31).                      EL6351
00249      12  DATA-AREA       OCCURS  5 TIMES                          EL6351
00250                              INDEXED BY INDX.                     EL6351
00251          16  CARR-LEN            PIC S9(4)              COMP.     EL6351
00252          16  CARR-ATTRB          PIC  X.                          EL6351
00253          16  CARRIER             PIC  X.                          EL6351
00254          16  GRP-LEN             PIC S9(4)              COMP.     EL6351
00255          16  GRP-ATTRB           PIC  X.                          EL6351
00256          16  GROUPING            PIC  X(6).                       EL6351
00257          16  FIN-LEN             PIC S9(4)              COMP.     EL6351
00258          16  FIN-ATTRB           PIC  X.                          EL6351
00259          16  FIN-RESP            PIC  X(10).                      EL6351
00260          16  AHDG-LEN            PIC S9(4)              COMP.        CL**8
00261          16  AHDG-ATTRB          PIC  X.                             CL**8
00262          16  AHDG                PIC  X(5).                          CL**8
00263          16  ACCT-LEN            PIC S9(4)              COMP.     EL6351
00264          16  ACCT-ATTRB          PIC  X.                          EL6351
00265          16  ACCT                PIC  X(10).                      EL6351
00266          16  RTYPE-LEN           PIC S9(4)              COMP.     EL6351
00267          16  RTYPE-ATTRB         PIC  X.                          EL6351
00268          16  RTYPE               PIC  X.                          EL6351
00269          16  AMT-LEN             PIC S9(4)              COMP.     EL6351
00270          16  AMT-ATTRB           PIC  X.                          EL6351
00271          16  AMT                 PIC 9(11).                       EL6351
00272          16  AMTO  REDEFINES                                      EL6351
00273              AMT                 PIC S9(9)V99.                       CL*31
00274          16  COMM-LEN            PIC S9(4)              COMP.     EL6351
00275          16  COMM-ATTRB          PIC  X.                          EL6351
00276          16  COMM                PIC  X(30).                      EL6351
00277          16  NCL-COMM  REDEFINES  COMM.                              CL*19
00278              20  NCL-COMM-DTE.                                       CL*19
00279                  24  NCL-MO      PIC  X(2).                          CL*19
00280                  24  NCL-DA      PIC  X(2).                          CL*19
00281                  24  NCL-YR      PIC  X(2).                          CL*19
00282              20  NCL-COMM-REST   PIC  X(24).                         CL*19
00283          16  REF-LEN             PIC S9(4)              COMP.     EL6351
00284          16  REF-ATTRB           PIC  X.                          EL6351
00285          16  REF                 PIC  X(12).                      EL6351
00286          16  INVOICE-LEN         PIC S9(4)              COMP.     EL6351
00287          16  INVOICE-ATTRB       PIC  X.                          EL6351
00288          16  INVOICE             PIC  X(6).                       EL6351
00289          16  APPLIED-LEN         PIC S9(4)              COMP.        CL**2
00290          16  APPLIED-ATTRB       PIC  X.                             CL**2
00291          16  APPLIED             PIC  X.                             CL**2
00292          16  CREDIT-LEN          PIC S9(4)              COMP.     EL6351
00293          16  CREDIT-ATTRB        PIC  X.                          EL6351
00294          16  CREDIT              PIC  X(14).                      EL6351
00295          16  DEBIT-LEN           PIC S9(4)              COMP.     EL6351
00296          16  DEBIT-ATTRB         PIC  X.                          EL6351
00297          16  DEBIT               PIC  X(14).                      EL6351
00298          16  EOM-DT-LEN          PIC S9(4)              COMP.        CL*24
00299          16  EOM-DT-ATTRB        PIC  X.                             CL*24
00300          16  EOM-DT              PIC X(6).                           CL*24
00301          16  INDATE-LEN          PIC S9(4)              COMP.        CL*24
00302          16  INDATE-ATTRB        PIC  X.                             CL*24
00303          16  INDATE              PIC X(6).                           CL*24
00304      12  FILLER                  PIC X(169).                         CL*30
00305  EJECT                                                            EL6351
00306  LINKAGE SECTION.                                                 EL6351
00307  01  DFHCOMMAREA             PIC  X(1024).                        EL6351
00308  EJECT                                                            EL6351
00309 *01 PARMLIST         COMP.                                           CL*30
00310 *    12  FILLER              PIC S9(8).                              CL*30
00311 *    12  ERPYAJ-POINTER      PIC S9(8).                              CL*30
00312 *    12  ERCOMP-POINTER      PIC S9(8).                              CL*30
00313 *    12  ERRECV-POINTER      PIC S9(8).                              CL*30
00314 *    12  MPPRCN-POINTER      PIC S9(8).                              CL*30
00315                                                                      CL**8
00316  EJECT                                                            EL6351
00317      COPY ERCPYAJ.                                                   CL*18
00318  EJECT                                                            EL6351
00319      COPY ERCCOMP.                                                   CL*18
00320  EJECT                                                               CL**5
00321      COPY ERCRECV.                                                   CL*18
00322  EJECT                                                               CL**8
00323      COPY MPCPRCN.                                                   CL*18
00324  EJECT                                                            EL6351
00325  PROCEDURE DIVISION.                                              EL6351
00326                                                                   EL6351
00327      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6351
00328      MOVE 2                      TO  EMI-NUMBER-OF-LINES.         EL6351
00329                                                                   EL6351
00330      IF EIBCALEN = ZERO                                           EL6351
00331          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6351
00332                                                                   EL6351
00333      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6351
00334      MOVE '5'                    TO  DC-OPTION-CODE.              EL6351
00335                                                                   EL6351
00336      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  EL6351
00337                                                                   EL6351
00338      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.           EL6351
00339      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.               EL6351
00340      MOVE DC-GREG-DATE-1-MDY     TO  WS-CURRENT-MDY.                 CL*19
00341                                                                      CL**8
00342      IF MORTGAGE-SESSION                                             CL**8
00343         MOVE XCTL-EM626           TO XCTL-EL626                      CL**9
00344         MOVE MPPYAJ-FILE-ID       TO PYAJ-FILE-ID.                   CL**8
00345                                                                   EL6351
00346      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6351
00347          MOVE  'Y'                TO  PI-SEQ-FIRST-TIME-SW           CL*24
00348          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6351
00349              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL6351
00350              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL6351
00351              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL6351
00352              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL6351
00353              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL6351
00354              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL6351
00355              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL6351
00356              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL6351
00357          ELSE                                                     EL6351
00358              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL6351
00359              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL6351
00360              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL6351
00361              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL6351
00362              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL6351
00363              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL6351
00364              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL6351
00365              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.   EL6351
00366                                                                      CL*24
00367      IF PI-SEQ-FIRST-TIME-SW    =   'Y'                              CL*24
00368          MOVE ZEROS               TO  PI-FILE-SEQ-NO (1)             CL*24
00369          MOVE ZEROS               TO  PI-FILE-SEQ-NO (2)             CL*24
00370          MOVE ZEROS               TO  PI-FILE-SEQ-NO (3)             CL*24
00371          MOVE ZEROS               TO  PI-FILE-SEQ-NO (4)             CL*24
00372          MOVE ZEROS               TO  PI-FILE-SEQ-NO (5)             CL*24
00373          MOVE 'N'                 TO  PI-SEQ-FIRST-TIME-SW.          CL*24
00374                                                                   EL6351
00375      MOVE LOW-VALUES             TO  EL635BI.                     EL6351
00376                                                                   EL6351
00377      COMPUTE WORK-SEQ-NO  =  EIBTIME  *  10.                      EL6351
00378                                                                   EL6351
00379      IF EIBTRNID NOT = TRANS-ID                                   EL6351
00380          MOVE SPACE              TO  PI-PYAJ-FILE-SW              EL6351
00381          GO TO 8100-SEND-INITIAL-MAP.                             EL6351
00382                                                                   EL6351
00383      EXEC CICS HANDLE CONDITION                                   EL6351
00384          PGMIDERR  (9600-PGMID-ERROR)                             EL6351
00385          ERROR     (9990-ABEND)                                   EL6351
00386          END-EXEC.                                                EL6351
00387                                                                   EL6351
00388      IF EIBAID = DFHCLEAR                                         EL6351
00389          GO TO 9400-CLEAR.                                        EL6351
00390  EJECT                                                            EL6351
00391  0200-RECEIVE.                                                    EL6351
00392      IF EIBAID = DFHPA1              OR  DFHPA2  OR  DFHPA3       EL6351
00393          MOVE ER-0008            TO  EMI-ERROR                    EL6351
00394          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               EL6351
00395          MOVE -1                 TO  PFENTERL                     EL6351
00396          GO TO 8200-SEND-DATAONLY.                                EL6351
00397                                                                   EL6351
00398      EXEC CICS RECEIVE                                            EL6351
00399          MAP     (MAP-NAME)                                       EL6351
00400          MAPSET  (MAPSET-NAME)                                    EL6351
00401          INTO    (EL635BI)                                        EL6351
00402          END-EXEC.                                                EL6351
00403                                                                   EL6351
00404      IF PFENTERL = ZERO                                           EL6351
00405          GO TO 0300-CHECK-PFKEYS.                                 EL6351
00406                                                                   EL6351
00407      IF (PFENTERI  IS NUMERIC)                                    EL6351
00408        AND (PFENTERI  IS GREATER THAN  ZERO                       EL6351
00409        AND  IS LESS THAN  25)                                     EL6351
00410          MOVE PF-VALUES (PFENTERI)  TO  EIBAID                    EL6351
00411      ELSE                                                         EL6351
00412          MOVE ER-0029               TO  EMI-ERROR                 EL6351
00413          GO TO 0320-INPUT-ERROR.                                  EL6351
00414                                                                   EL6351
00415  0300-CHECK-PFKEYS.                                               EL6351
00416      IF EIBAID = DFHPF23                                          EL6351
00417          GO TO 8810-PF23.                                         EL6351
00418                                                                   EL6351
00419      IF EIBAID = DFHPF24                                          EL6351
00420          GO TO 9200-RETURN-MAIN-MENU.                             EL6351
00421                                                                   EL6351
00422      IF EIBAID = DFHPF12                                          EL6351
00423          GO TO 9500-PF12.                                         EL6351
00424                                                                   EL6351
00425      IF EIBAID = DFHENTER                                            CL*17
00426          MOVE SPACES             TO  WS-ACCEPT                       CL*17
00427          MOVE SPACES             TO  WS-PREV-PF5                     CL*17
00428          GO TO 1000-EDIT-DATA.                                       CL*17
00429                                                                      CL*17
00430      IF EIBAID = DFHPF5                                              CL*17
00431          MOVE PI-ACCEPT          TO  WS-ACCEPT                       CL*17
00432          GO TO 1000-EDIT-DATA.                                    EL6351
00433                                                                   EL6351
00434  0320-INPUT-ERROR.                                                EL6351
00435      MOVE ER-0029                TO  EMI-ERROR.                   EL6351
00436                                                                   EL6351
00437      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL6351
00438                                                                   EL6351
00439      MOVE AL-UNBON               TO  PFENTERA.                    EL6351
00440                                                                   EL6351
00441      IF PFENTERL = ZERO                                           EL6351
00442          MOVE -1                 TO  PFENTERL                     EL6351
00443      ELSE                                                         EL6351
00444          MOVE -1                 TO  PFENTERL.                    EL6351
00445                                                                   EL6351
00446      GO TO 8200-SEND-DATAONLY.                                    EL6351
00447  EJECT                                                            EL6351
00448  1000-EDIT-DATA.                                                  EL6351
00449                                                                   EL6351
00450      IF NOT MODIFY-CAP                                            EL6351
00451          MOVE 'UPDATE'       TO SM-READ                           EL6351
00452          PERFORM 9995-SECURITY-VIOLATION                          EL6351
00453          MOVE ER-0070        TO EMI-ERROR                         EL6351
00454          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6351
00455          GO TO 8100-SEND-INITIAL-MAP.                             EL6351
00456                                                                   EL6351
00457      MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD               EL6351
00458                                      COMP-COMP-CD.                EL6351
00459      SET INDX                    TO  1.                           EL6351
00460      MOVE 'ACCEPT'               TO  PI-ACCEPT-DESC.                 CL*17
00461      MOVE ZEROS                  TO  WS-EOM-DT (1)  WS-INP-DT (1)    CL*25
00462                                      WS-EOM-DT (2)  WS-INP-DT (2)    CL*25
00463                                      WS-EOM-DT (3)  WS-INP-DT (3)    CL*25
00464                                      WS-EOM-DT (4)  WS-INP-DT (4)    CL*25
00465                                      WS-EOM-DT (5)  WS-INP-DT (5).   CL*25
00466                                                                   EL6351
00467  1010-EDIT-LOOP.                                                  EL6351
00468                                                                      CL*17
00469      IF CARR-LEN        (INDX) = ZEROS                               CL*29
00470         AND GRP-LEN     (INDX) = ZEROS                               CL*29
00471         AND FIN-LEN     (INDX) = ZEROS                               CL*29
00472         AND ACCT-LEN    (INDX) = ZEROS                               CL*29
00473         AND COMM-LEN    (INDX) = ZEROS                               CL*29
00474         AND RTYPE-LEN   (INDX) = ZEROS                               CL*29
00475         AND AMT-LEN     (INDX) = ZEROS                               CL*29
00476         AND APPLIED-LEN (INDX) = ZEROS                               CL*29
00477         AND INVOICE-LEN (INDX) = ZEROS                               CL*29
00478         AND EOM-DT-LEN  (INDX) = ZEROS                               CL*29
00479         AND INDATE-LEN  (INDX) = ZEROS                               CL*29
00480          NEXT SENTENCE                                               CL*29
00481      ELSE                                                            CL*17
00482          GO TO 1010-EDIT-PROCESS.                                    CL*29
00483                                                                      CL*17
00484      IF EIBAID = DFHPF5                                              CL*17
00485          GO TO 1010-EDIT-PROCESS.                                    CL*17
00486                                                                      CL*17
00487      IF WS-PREV-PF5 = 'Y' AND                                        CL*17
00488          CARRIER    (INDX) NOT = LOW-VALUES AND                      CL*24
00489          GROUPING   (INDX) NOT = LOW-VALUES AND                      CL*24
00490          FIN-RESP   (INDX) NOT = LOW-VALUES AND                      CL*24
00491          ACCT       (INDX) NOT = LOW-VALUES AND                      CL*24
00492          RTYPE      (INDX) NOT = LOW-VALUES AND                      CL*24
00493          AMT        (INDX) NOT = LOW-VALUES AND                      CL*24
00494          COMM       (INDX) NOT = LOW-VALUES AND                      CL*24
00495          APPLIED    (INDX) NOT = LOW-VALUES AND                      CL*24
00496          INVOICE    (INDX) NOT = LOW-VALUES AND                      CL*24
00497          REF        (INDX) NOT = LOW-VALUES                          CL*24
00498           MOVE 'N'                TO  WS-PREV-PF5                    CL*24
00499           GO TO 1010-EDIT-PROCESS.                                   CL*24
00500                                                                      CL*17
00501      GO TO 1060-INCREMENT-INDX.                                      CL*17
00502                                                                      CL*17
00503  1010-EDIT-PROCESS.                                                  CL*17
00504                                                                      CL*13
00505 **************************************************************       CL*13
00506 *                                                            *       CL*13
00507 *    THE COMPENSATION CONTROL IS NOT REQUIRED FOR SPECIAL    *       CL*13
00508 *    GROUPING PROCESS IF THE TYPE IS 'G' AND AN INVOICE      *       CL*13
00509 *    IS ENTERED.                                             *       CL*13
00510 *                                                            *       CL*13
00511 **************************************************************       CL*13
00512                                                                      CL*13
00513      IF MORTGAGE-SESSION                                             CL*13
00514         IF RTYPE            (INDX) = 'G'                             CL*13
00515            IF CARR-LEN      (INDX) GREATER THAN +0                   CL*13
00516               OR  GRP-LEN   (INDX) GREATER THAN +0                   CL*13
00517               OR  FIN-LEN   (INDX) GREATER THAN +0                   CL*13
00518               OR  ACCT-LEN  (INDX) GREATER THAN +0                   CL*13
00519                  MOVE ER-9179        TO  EMI-ERROR                   CL*13
00520                  MOVE -1             TO  CARR-LEN   (INDX)           CL*13
00521                  MOVE AL-UABON       TO  CARR-ATTRB (INDX)           CL*13
00522                                          FIN-ATTRB  (INDX)           CL*13
00523                                          GRP-ATTRB  (INDX)           CL*13
00524                                          ACCT-ATTRB (INDX)           CL*13
00525                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL*13
00526                  GO TO 1040-CONTINUE-EDIT                            CL*17
00527            ELSE                                                      CL*13
00528               GO TO 1040-CONTINUE-EDIT                               CL*17
00529         ELSE                                                         CL*13
00530            NEXT SENTENCE.                                            CL*13
00531                                                                   EL6351
00532      IF CARR-LEN (INDX) NOT = ZEROS                               EL6351
00533          MOVE AL-UANON           TO  CARR-ATTRB (INDX)            EL6351
00534          MOVE CARRIER (INDX)     TO  COMP-CARRIER                 EL6351
00535                                      PI-SAV-CARRIER               EL6351
00536          IF CARRIER (INDX) NOT = ZEROS                            EL6351
00537            AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)           EL6351
00538              MOVE ER-2587        TO  EMI-ERROR                    EL6351
00539              MOVE -1             TO  CARR-LEN (INDX)              EL6351
00540              MOVE AL-UABON       TO  CARR-ATTRB (INDX)            EL6351
00541              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL6351
00542          ELSE                                                     EL6351
00543              NEXT SENTENCE                                        EL6351
00544      ELSE                                                         EL6351
00545          MOVE ER-0194            TO  EMI-ERROR                    EL6351
00546          MOVE -1                 TO  CARR-LEN (INDX)              EL6351
00547          MOVE AL-UABON           TO  CARR-ATTRB (INDX)            EL6351
00548          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6351
00549                                                                   EL6351
00550      IF GRP-LEN (INDX) NOT = ZEROS                                EL6351
00551          MOVE AL-UANON           TO  GRP-ATTRB (INDX)             EL6351
00552          MOVE GROUPING (INDX)    TO  COMP-GROUPING                EL6351
00553                                      PI-SAV-GROUPING              EL6351
00554          IF GROUPING (INDX) NOT = ZEROS                           EL6351
00555            AND (PI-ZERO-GROUPING  OR  PI-ZERO-CAR-GROUP)          EL6351
00556              MOVE ER-2588        TO  EMI-ERROR                    EL6351
00557              MOVE -1             TO  GRP-LEN (INDX)               EL6351
00558              MOVE AL-UABON       TO  GRP-ATTRB (INDX)             EL6351
00559              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL6351
00560          ELSE                                                     EL6351
00561              NEXT SENTENCE                                        EL6351
00562      ELSE                                                         EL6351
00563          MOVE ER-0195            TO  EMI-ERROR                    EL6351
00564          MOVE -1                 TO  GRP-LEN (INDX)               EL6351
00565          MOVE AL-UABON           TO  GRP-ATTRB (INDX)             EL6351
00566          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6351
00567                                                                   EL6351
00568      IF FIN-LEN (INDX) NOT = ZEROS                                EL6351
00569          MOVE AL-UANON           TO  FIN-ATTRB (INDX)             EL6351
00570          MOVE FIN-RESP (INDX)    TO  COMP-FIN-RESP                EL6351
00571                                      PI-SAV-FIN-RESP              EL6351
00572                                      PI-CR-FIN-RESP               EL6351
00573      ELSE                                                         EL6351
00574          MOVE ER-2562            TO  EMI-ERROR                    EL6351
00575          MOVE -1                 TO  FIN-LEN (INDX)               EL6351
00576          MOVE AL-UABON           TO  FIN-ATTRB (INDX)             EL6351
00577          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6351
00578                                                                   EL6351
00579      IF ACCT-LEN (INDX) NOT = ZEROS                               EL6351
00580          MOVE AL-UANON           TO  ACCT-ATTRB (INDX)            EL6351
00581          MOVE ACCT (INDX)        TO  COMP-ACCOUNT                 EL6351
00582                                      PI-SAV-ACCOUNT               EL6351
00583      ELSE                                                         EL6351
00584          MOVE +1                 TO  ACCT-LEN (INDX)                 CL*15
00585          MOVE LOW-VALUES         TO  COMP-ACCOUNT                 EL6351
00586                                      ACCT (INDX)                     CL*15
00587                                      PI-SAV-ACCOUNT.              EL6351
00588                                                                   EL6351
00589      MOVE 'A'                    TO  COMP-RECORD-TYPE.               CL*17
00590                                                                   EL6351
00591      IF ACCT (INDX) = LOW-VALUES                                     CL*17
00592          IF APPLIED (INDX) NOT = 'G' AND 'O'                         CL*17
00593              MOVE ER-3183        TO  EMI-ERROR                       CL*17
00594              MOVE -1             TO  APPLIED-LEN (INDX)              CL*17
00595              MOVE AL-UABON       TO  APPLIED-ATTRB(INDX)             CL*17
00596              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL*17
00597              GO TO 1040-CONTINUE-EDIT                                CL*17
00598          ELSE                                                        CL*17
00599              GO TO 1030-VERIFY-GA-ONLY.                              CL*17
00600                                                                   EL6351
00601      IF ACCT (INDX) = FIN-RESP (INDX)                                CL*17
00602          IF APPLIED (INDX) = 'A'                                     CL*17
00603              GO TO 1015-VERIFY-ACCT-RESP                             CL*17
00604          ELSE                                                        CL*17
00605          IF APPLIED-LEN (INDX) = ZERO                                CL*17
00606              MOVE 'A'            TO  APPLIED (INDX)                  CL*17
00607              MOVE +1             TO  APPLIED-ATTRB (INDX)            CL*17
00608              GO TO 1015-VERIFY-ACCT-RESP                             CL*17
00609          ELSE                                                        CL*17
00610              IF PI-COMPANY-ID = 'NCL'                                CL*28
00611                  IF APPLIED (INDX) = 'G'                             CL*28
00612                      GO TO 1015-VERIFY-ACCT-RESP                     CL*28
00613                  ELSE                                                CL*28
00614                      MOVE ER-3184                                    CL*28
00615                                  TO  EMI-ERROR                       CL*28
00616                      MOVE -1     TO  APPLIED-LEN (INDX)              CL*28
00617                      MOVE AL-UABON                                   CL*28
00618                                  TO  APPLIED-ATTRB(INDX)             CL*28
00619                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT      CL*28
00620                      GO TO 1040-CONTINUE-EDIT                        CL*28
00621              ELSE                                                    CL*28
00622                  MOVE ER-3184    TO  EMI-ERROR                       CL*28
00623                  MOVE -1         TO  APPLIED-LEN (INDX)              CL*28
00624                  MOVE AL-UABON   TO  APPLIED-ATTRB(INDX)             CL*28
00625                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL*28
00626                  GO TO 1040-CONTINUE-EDIT.                           CL*28
00627                                                                      CL**2
00628      IF APPLIED (INDX) = 'G'                                         CL*17
00629          GO TO 1020-VERIFY-RESP-GA.                                  CL*17
00630                                                                      CL*17
00631      IF APPLIED (INDX) = 'O'                                         CL*17
00632          GO TO 1030-VERIFY-GA-ONLY.                                  CL*17
00633                                                                      CL*17
00634      EXEC CICS GETMAIN                                               CL*17
00635          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*30
00636          LENGTH   (ERCOMP-RECORD-LENGTH)                             CL*17
00637          INITIMG  (GETMAIN-SPACE)                                    CL*17
00638          END-EXEC.                                                EL6351
00639                                                                   EL6351
00640      EXEC CICS HANDLE CONDITION                                      CL*17
00641          NOTFND   (1015-NO-COMP-MSTR)                                CL*17
00642          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL*17
00643          END-EXEC.                                                   CL*17
00644                                                                      CL*11
00645      EXEC CICS READ                                                  CL*17
00646          DATASET  (COMP-FILE-ID)                                     CL*17
00647          INTO     (COMPENSATION-MASTER)                              CL*17
00648          RIDFLD   (ERCOMP-KEY)                                       CL*17
00649          END-EXEC.                                                   CL*17
00650                                                                      CL*17
00651      IF RTYPE (INDX) = 'R' OR 'D' OR 'S' OR 'T' OR 'Z'               CL*19
00652            MOVE 'G'              TO  APPLIED (INDX)                  CL*17
00653         ELSE                                                         CL*17
00654            MOVE 'A'              TO  APPLIED (INDX).                 CL*17
00655                                                                      CL*17
00656      MOVE +1                     TO  APPLIED-LEN (INDX).             CL*17
00657      GO TO 1020-VERIFY-RESP-GA.                                      CL*17
00658                                                                      CL*17
00659  1012-NO-COMP-MSTR.                                                  CL*17
00660      MOVE 'O'                    TO  APPLIED (INDX).                 CL*17
00661      MOVE +1                     TO  APPLIED-LEN (INDX).             CL*17
00662      GO TO 1030-VERIFY-GA-ONLY.                                      CL*17
00663                                                                      CL*17
00664  1015-VERIFY-ACCT-RESP.                                              CL*17
00665                                                                      CL*17
00666      EXEC CICS GETMAIN                                               CL*17
00667          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*30
00668          LENGTH   (ERCOMP-RECORD-LENGTH)                             CL*17
00669          INITIMG  (GETMAIN-SPACE)                                    CL*17
00670          END-EXEC.                                                   CL*17
00671                                                                      CL*17
00672      EXEC CICS HANDLE CONDITION                                      CL*17
00673          NOTFND   (1015-NO-COMP-MSTR)                                CL*17
00674          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL*17
00675          END-EXEC.                                                   CL*17
00676                                                                      CL*17
00677      EXEC CICS READ                                                  CL*17
00678          DATASET  (COMP-FILE-ID)                                     CL*17
00679          INTO     (COMPENSATION-MASTER)                              CL*17
00680          RIDFLD   (ERCOMP-KEY)                                       CL*17
00681          END-EXEC.                                                   CL*17
00682                                                                      CL*17
00683      MOVE CO-AR-BAL-LEVEL        TO  PI-SAVE-COMP-BAL-CODE.          CL*17
00684                                                                      CL*17
00685      GO TO 1040-CONTINUE-EDIT.                                       CL*17
00686                                                                      CL*17
00687  1015-NO-COMP-MSTR.                                                  CL*17
00688                                                                      CL*17
00689      MOVE ER-3178                TO  EMI-ERROR                       CL*17
00690      MOVE -1                     TO  CARR-LEN (INDX)                 CL*17
00691      MOVE AL-UABON               TO  CARR-ATTRB (INDX)               CL*17
00692                                      GRP-ATTRB (INDX)                CL*17
00693                                      FIN-ATTRB (INDX)                CL*17
00694                                      ACCT-ATTRB (INDX).              CL*17
00695      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL*17
00696      GO TO 1040-CONTINUE-EDIT.                                       CL*17
00697                                                                      CL*17
00698  1020-VERIFY-RESP-GA.                                                CL*17
00699                                                                      CL*17
00700      EXEC CICS GETMAIN                                               CL*17
00701          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*30
00702          LENGTH   (ERCOMP-RECORD-LENGTH)                             CL*17
00703          INITIMG  (GETMAIN-SPACE)                                    CL*17
00704          END-EXEC.                                                   CL*17
00705                                                                      CL*17
00706      EXEC CICS HANDLE CONDITION                                      CL*17
00707          NOTFND   (1015-NO-COMP-MSTR)                                CL*17
00708          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL*17
00709          END-EXEC.                                                   CL*17
00710                                                                      CL*17
00711      EXEC CICS READ                                                  CL*17
00712          DATASET  (COMP-FILE-ID)                                     CL*17
00713          INTO     (COMPENSATION-MASTER)                              CL*17
00714          RIDFLD   (ERCOMP-KEY)                                       CL*17
00715          END-EXEC.                                                   CL*17
00716                                                                      CL*17
00717      MOVE CO-AR-BAL-LEVEL        TO  PI-SAVE-COMP-BAL-CODE.          CL*17
00718                                                                      CL*17
00719      MOVE LOW-VALUES             TO  COMP-ACCOUNT.                   CL*17
00720      MOVE 'G'                    TO  COMP-RECORD-TYPE.               CL*17
00721                                                                      CL*17
00722      EXEC CICS GETMAIN                                               CL*17
00723          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*30
00724          LENGTH   (ERCOMP-RECORD-LENGTH)                             CL*17
00725          INITIMG  (GETMAIN-SPACE)                                    CL*17
00726          END-EXEC.                                                   CL*17
00727                                                                      CL*17
00728      EXEC CICS HANDLE CONDITION                                      CL*17
00729          NOTFND   (1015-NO-COMP-MSTR)                                CL*17
00730          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL*17
00731          END-EXEC.                                                   CL*17
00732                                                                      CL*17
00733      EXEC CICS READ                                                  CL*17
00734          DATASET  (COMP-FILE-ID)                                     CL*17
00735          INTO     (COMPENSATION-MASTER)                              CL*17
00736          RIDFLD   (ERCOMP-KEY)                                       CL*17
00737          END-EXEC.                                                   CL*17
00738                                                                      CL*17
00739      GO TO 1040-CONTINUE-EDIT.                                       CL*17
00740                                                                   EL6351
00741  1020-NO-COMP-MSTR.                                               EL6351
00742                                                                      CL**2
00743      IF COMP-RECORD-TYPE = 'A'                                       CL*11
00744          MOVE ER-3178            TO  EMI-ERROR                       CL*11
00745      ELSE                                                            CL*11
00746          MOVE ER-3179            TO  EMI-ERROR.                      CL*11
00747                                                                      CL*11
00748      MOVE -1                     TO  CARR-LEN (INDX)              EL6351
00749      MOVE AL-UABON               TO  CARR-ATTRB (INDX)            EL6351
00750                                      GRP-ATTRB (INDX)             EL6351
00751                                      FIN-ATTRB (INDX)             EL6351
00752                                      ACCT-ATTRB (INDX).              CL*11
00753      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL6351
00754      GO TO 1040-CONTINUE-EDIT.                                       CL*17
00755                                                                   EL6351
00756  1030-VERIFY-GA-ONLY.                                                CL*17
00757                                                                      CL*17
00758      MOVE 'G'                    TO  COMP-RECORD-TYPE.               CL*17
00759      MOVE LOW-VALUES             TO  COMP-ACCOUNT.                   CL*17
00760                                                                      CL*17
00761      EXEC CICS GETMAIN                                               CL*17
00762          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL*30
00763          LENGTH   (ERCOMP-RECORD-LENGTH)                             CL*17
00764          INITIMG  (GETMAIN-SPACE)                                    CL*17
00765          END-EXEC.                                                   CL*17
00766                                                                      CL*17
00767      EXEC CICS HANDLE CONDITION                                      CL*17
00768          NOTFND   (1015-NO-COMP-MSTR)                                CL*17
00769          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL*17
00770          END-EXEC.                                                   CL*17
00771                                                                      CL*17
00772      EXEC CICS READ                                                  CL*17
00773          DATASET  (COMP-FILE-ID)                                     CL*17
00774          INTO     (COMPENSATION-MASTER)                              CL*17
00775          RIDFLD   (ERCOMP-KEY)                                       CL*17
00776          END-EXEC.                                                   CL*17
00777                                                                      CL*17
00778      MOVE CO-AR-BAL-LEVEL        TO  PI-SAVE-COMP-BAL-CODE.          CL*17
00779                                                                      CL*17
00780      GO TO 1040-CONTINUE-EDIT.                                       CL*17
00781                                                                      CL*17
00782  1030-NO-COMP-MSTR.                                                  CL*17
00783                                                                      CL*17
00784      MOVE ER-3179                TO  EMI-ERROR.                      CL*17
00785      MOVE -1                     TO  CARR-LEN (INDX)                 CL*17
00786      MOVE AL-UABON               TO  CARR-ATTRB (INDX)               CL*17
00787                                      GRP-ATTRB (INDX)                CL*17
00788                                      FIN-ATTRB (INDX)                CL*17
00789                                      ACCT-ATTRB (INDX).              CL*17
00790      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL*17
00791      GO TO 1040-CONTINUE-EDIT.                                       CL*17
00792                                                                      CL*17
00793  1040-CONTINUE-EDIT.                                                 CL*17
00794                                                                      CL*11
00795      IF COMM-LEN (INDX) NOT = ZEROS                               EL6351
00796          MOVE AL-UANON           TO  COMM-ATTRB (INDX)               CL*19
00797          IF PI-COMPANY-ID NOT = 'NCL'                                CL*19
00798              NEXT SENTENCE                                           CL*19
00799          ELSE                                                        CL*19
00800              IF NCL-COMM-DTE (INDX) IS NUMERIC                       CL*19
00801                  MOVE NCL-COMM-DTE (INDX)                            CL*19
00802                                  TO  DC-GREG-DATE-1-MDY              CL*19
00803                  MOVE '4'        TO  DC-OPTION-CODE                  CL*19
00804                  PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT          CL*19
00805                  IF NO-CONVERSION-ERROR                              CL*19
00806                      NEXT SENTENCE                                   CL*19
00807                  ELSE                                                CL*19
00808                      MOVE ER-2595                                    CL*19
00809                                  TO  EMI-ERROR                       CL*19
00810                      MOVE -1     TO  COMM-LEN (INDX)                 CL*19
00811                      MOVE AL-UABON                                   CL*19
00812                                  TO  COMM-ATTRB (INDX)               CL*19
00813                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL*19
00814              ELSE                                                    CL*19
00815                  IF NCL-COMM-DTE (INDX) = SPACES OR LOW-VALUES       CL*19
00816                      MOVE WS-CURRENT-MDY                             CL*19
00817                                  TO  NCL-COMM-DTE (INDX)             CL*19
00818                  ELSE                                                CL*19
00819                      MOVE COMM (INDX)                                CL*19
00820                                  TO  WS-COMMENT-FULL                 CL*19
00821                      MOVE WS-COMMENT-24-POS                          CL*19
00822                                  TO  NCL-COMM-REST (INDX)            CL*19
00823                      MOVE WS-CURRENT-MDY                             CL*19
00824                                  TO  NCL-COMM-DTE (INDX)             CL*19
00825      ELSE                                                            CL*19
00826          IF PI-COMPANY-ID = 'NCL'                                    CL*19
00827            AND PI-FILE-SEQ-NO (NDX) = ZEROS                          CL*19
00828              MOVE +6             TO  COMM-LEN (INDX)                 CL*19
00829              MOVE WS-CURRENT-MDY TO  NCL-COMM-DTE (INDX).            CL*19
00830                                                                      CL*19
00831                                                                   EL6351
00832      IF RTYPE-LEN (INDX) NOT = ZEROS                              EL6351
00833          MOVE RTYPE (INDX)       TO  CHECK-REC-TYPE               EL6351
00834          IF PI-COMPANY-ID = 'MON'                                    CL*19
00835              IF NOT MON-VALID-REC-TYPE                               CL*19
00836                  MOVE -1         TO  RTYPE-LEN (INDX)                CL*19
00837                  MOVE ER-7806    TO  EMI-ERROR                       CL*19
00838                  MOVE AL-UABON   TO  RTYPE-ATTRB (INDX)              CL*19
00839                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL*19
00840              ELSE                                                    CL*19
00841                  MOVE AL-UANON   TO  RTYPE-ATTRB (INDX)              CL*19
00842          ELSE                                                     EL6351
00843              IF PI-COMPANY-ID = 'NCL'                                CL*19
00844                  IF NOT NCL-VALID-REC-TYPE                           CL*19
00845                      MOVE -1     TO  RTYPE-LEN (INDX)                CL*19
00846                      MOVE ER-7806                                    CL*19
00847                                  TO  EMI-ERROR                       CL*19
00848                      MOVE AL-UABON                                   CL*19
00849                                  TO  RTYPE-ATTRB (INDX)              CL*19
00850                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT      CL*19
00851                  ELSE                                                CL*19
00852                      MOVE AL-UANON                                   CL*19
00853                                  TO  RTYPE-ATTRB (INDX)              CL*19
00854              ELSE                                                    CL*19
00855                  IF PI-COMPANY-ID = 'ANL'                            CL*19
00856                      IF NOT ANL-VALID-REC-TYPE                       CL*19
00857                          MOVE -1 TO  RTYPE-LEN (INDX)                CL*19
00858                          MOVE ER-7806                                CL*19
00859                                  TO  EMI-ERROR                       CL*19
00860                          MOVE AL-UABON                               CL*19
00861                                  TO  RTYPE-ATTRB (INDX)              CL*19
00862                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    CL*19
00863                      ELSE                                            CL*19
00864                          MOVE AL-UANON                               CL*19
00865                                  TO  RTYPE-ATTRB (INDX)              CL*19
00866                  ELSE                                                CL*19
00867                      IF NOT VALID-REC-TYPE                           CL*19
00868                          MOVE -1 TO  RTYPE-LEN (INDX)                CL*19
00869                          MOVE ER-2234                                CL*19
00870                                  TO  EMI-ERROR                       CL*19
00871                          MOVE AL-UABON                               CL*19
00872                                  TO  RTYPE-ATTRB (INDX)              CL*19
00873                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT    CL*19
00874                      ELSE                                            CL*19
00875                          MOVE AL-UANON                               CL*19
00876                                  TO  RTYPE-ATTRB (INDX)              CL*19
00877      ELSE                                                         EL6351
00878          MOVE -1                 TO  RTYPE-LEN (INDX)             EL6351
00879          MOVE ER-2235            TO  EMI-ERROR                    EL6351
00880          MOVE AL-UABON           TO  RTYPE-ATTRB (INDX)           EL6351
00881          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6351
00882                                                                   EL6351
00883      IF AMT-LEN (INDX) NOT = ZEROS                                EL6351
00884          MOVE AL-UNNON           TO  AMT-ATTRB (INDX)             EL6351
00885          EXEC CICS BIF DEEDIT                                        CL*30
00886              FIELD   (AMTO (INDX))                                   CL*31
00887              LENGTH  (11)                                            CL*30
00888          END-EXEC                                                    CL*30
00889          IF AMTO (INDX) = ZEROS                                      CL*31
00890              MOVE ER-2245        TO  EMI-ERROR                    EL6351
00891              MOVE -1             TO  AMT-LEN(INDX)                EL6351
00892              MOVE AL-UNBON       TO  AMT-ATTRB (INDX)             EL6351
00893              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT           EL6351
00894          ELSE                                                     EL6351
00895              SET WS-INDX          TO INDX                         EL6351
00896              MOVE AMTO (INDX)    TO WS-EDITED-AMT (WS-INDX)          CL*31
00897      ELSE                                                         EL6351
00898          MOVE -1                 TO  AMT-LEN (INDX)               EL6351
00899          MOVE ER-2236            TO  EMI-ERROR                    EL6351
00900          MOVE AL-UNBON           TO  AMT-ATTRB (INDX)             EL6351
00901          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.              EL6351
00902                                                                      CL**2
00903      IF APPLIED-LEN (INDX) NOT = ZEROS                               CL**2
00904         IF APPLIED  (INDX) = 'A' OR 'G' OR 'O'                       CL**6
00905            NEXT SENTENCE                                             CL*16
00906         ELSE                                                         CL**2
00907            MOVE -1               TO APPLIED-LEN   (INDX)             CL**2
00908            MOVE ER-3146          TO EMI-ERROR                        CL**2
00909            MOVE AL-UABON         TO APPLIED-ATTRB (INDX)             CL**2
00910            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**2
00911                                                                      CL**8
00912 ****************************************************************     CL*24
00913 **   THIS CODE EDITS THE MONTH END DATE. DATE MUT BE VALID, AN       CL*25
00914 **   EOM DAY, AND NOT MORE THAN TWO MONTHS PASSED THE CREDIT         CL*25
00915 **   EOM DATE                                                        CL*25
00916 ****************************************************************     CL*24
00917                                                                      CL*24
00918      IF EOM-DT-LEN (INDX)  =  ZEROS                                  CL*24
00919          GO TO 1040-CHECK-INPUT-DATE.                                CL*29
00920                                                                      CL*24
00921      MOVE AL-UNNON               TO  EOM-DT-ATTRB (INDX).            CL*24
00922                                                                      CL*24
00923      MOVE EOM-DT (INDX)    TO  DEEDIT-FIELD.                         CL*24
00924      PERFORM 8600-DEEDIT.                                            CL*24
00925                                                                      CL*24
00926      IF DEEDIT-FIELD-V0  NOT NUMERIC                                 CL*24
00927         GO TO 1040-DAY-ERROR.                                        CL*24
00928                                                                      CL*24
00929      MOVE DEEDIT-FIELD-V0      TO  DC-GREG-DATE-1-MDY.               CL*24
00930      MOVE '4'                  TO  DC-OPTION-CODE.                   CL*24
00931      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                     CL*24
00932                                                                      CL*24
00933      IF DATE-CONVERSION-ERROR                                        CL*24
00934         GO TO 1040-DAY-ERROR                                         CL*24
00935      ELSE                                                            CL*24
00936         SET MINDEX             TO  INDX                              CL*24
00937         MOVE DC-BIN-DATE-1     TO  WS-EOM-DT (MINDEX).               CL*24
00938                                                                      CL*24
00939      MOVE DEEDIT-FIELD-V0      TO  DATE-TEST-AREA.                   CL*24
00940                                                                      CL*24
00941      IF DATE-TEST-MM    =  4 OR 6 OR 9 OR 11                         CL*24
00942         IF DATE-TEST-DD  =  30                                       CL*24
00943            GO TO 1040-CHECK-FUTURE-MO                                CL*24
00944         ELSE                                                         CL*24
00945            GO TO 1040-DAY-ERROR.                                     CL*24
00946                                                                      CL*24
00947      IF DATE-TEST-MM   = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12           CL*24
00948         IF DATE-TEST-DD  =  31                                       CL*24
00949            GO TO 1040-CHECK-FUTURE-MO                                CL*24
00950         ELSE                                                         CL*24
00951            GO TO 1040-DAY-ERROR.                                     CL*24
00952                                                                      CL*24
00953      DIVIDE DATE-TEST-YY  BY  4  GIVING  DIVIDE-RESULT               CL*24
00954         REMAINDER  DIVIDE-REMAINDER.                                 CL*24
00955                                                                      CL*24
00956      IF (DATE-TEST-YY  =  ZERO)  OR                                  CL*24
00957         (DIVIDE-REMAINDER  NOT =  ZERO)                              CL*24
00958         IF DATE-TEST-DD  NOT =  28                                   CL*24
00959             GO TO 1040-DAY-ERROR                                     CL*24
00960         ELSE                                                         CL*24
00961             GO TO 1040-CHECK-FUTURE-MO                               CL*24
00962      ELSE                                                            CL*24
00963         IF DATE-TEST-DD  =  29                                       CL*24
00964             GO TO 1040-CHECK-FUTURE-MO.                              CL*24
00965                                                                      CL*24
00966  1040-CHECK-FUTURE-MO.                                               CL*24
00967                                                                      CL*24
00968      IF WS-EOM-DT (MINDEX) NOT GREATER THAN PI-CR-MONTH-END-DT       CL*27
00969          GO TO 1040-CHECK-INPUT-DATE.                                CL*27
00970                                                                      CL*27
00971      MOVE PI-CR-MONTH-END-DT  TO  DC-BIN-DATE-1.                     CL*24
00972      MOVE WS-EOM-DT (MINDEX)  TO  DC-BIN-DATE-2.                     CL*24
00973      MOVE '1'                 TO  DC-OPTION-CODE.                    CL*24
00974      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                     CL*24
00975                                                                      CL*24
00976      IF DATE-CONVERSION-ERROR                                        CL*24
00977          GO TO 1040-DAY-ERROR.                                       CL*24
00978                                                                      CL*24
00979      IF DC-ELAPSED-MONTHS  GREATER THAN +2                           CL*24
00980          MOVE -1        TO  EOM-DT-LEN (INDX)                        CL*24
00981          MOVE AL-UNBON  TO  EOM-DT-ATTRB (INDX)                      CL*24
00982          MOVE ER-0761   TO  EMI-ERROR                                CL*24
00983          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                 CL*27
00984                                                                      CL*27
00985      GO TO 1040-CHECK-INPUT-DATE.                                    CL*27
00986                                                                      CL*24
00987  1040-DAY-ERROR.                                                     CL*24
00988                                                                      CL*24
00989      MOVE -1                    TO  EOM-DT-LEN (INDX).               CL*24
00990      MOVE AL-UNBON              TO  EOM-DT-ATTRB (INDX).             CL*24
00991      MOVE ER-0587               TO  EMI-ERROR.                       CL*24
00992      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL*24
00993                                                                      CL*24
00994  1040-CHECK-INPUT-DATE.                                              CL*27
00995                                                                      CL*25
00996 ****************************************************************     CL*25
00997 **   THIS CODE EDITS THE INPUT DATE                                  CL*29
00998 ****************************************************************     CL*25
00999                                                                      CL*24
01000      IF INDATE-LEN (INDX)  = ZEROS                                   CL*24
01001          GO TO 1040-CONTINUE.                                        CL*24
01002                                                                      CL*24
01003      MOVE AL-UNNON              TO  INDATE-ATTRB (INDX).             CL*24
01004      MOVE INDATE (INDX)         TO  DEEDIT-FIELD.                    CL*24
01005      PERFORM 8600-DEEDIT.                                            CL*24
01006                                                                      CL*24
01007      IF DEEDIT-FIELD-V0  NOT NUMERIC                                 CL*24
01008         MOVE -1                  TO  INDATE-LEN (INDX)               CL*24
01009         MOVE AL-UNBON            TO  INDATE-ATTRB (INDX)             CL*24
01010         MOVE ER-0714             TO  EMI-ERROR                       CL*24
01011         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  CL*24
01012                                                                      CL*24
01013      MOVE DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY.              CL*24
01014      MOVE '4'                   TO  DC-OPTION-CODE.                  CL*24
01015      PERFORM 8500-DATE-CONVERT  THRU 8500-EXIT.                      CL*24
01016                                                                      CL*24
01017      IF DATE-CONVERSION-ERROR                                        CL*24
01018         MOVE -1                  TO  INDATE-LEN (INDX)               CL*24
01019         MOVE AL-UNBON            TO  INDATE-ATTRB (INDX)             CL*24
01020         MOVE ER-0714             TO  EMI-ERROR                       CL*24
01021         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   CL*24
01022      ELSE                                                            CL*24
01023         SET DINDEX               TO  INDX                            CL*24
01024         MOVE DC-BIN-DATE-1       TO  WS-INP-DT (DINDEX).             CL*24
01025                                                                      CL*24
01026  1040-CONTINUE.                                                      CL*24
01027                                                                      CL*24
01028      IF MORTGAGE-SESSION                                             CL**8
01029         IF INVOICE-LEN (INDX) = ZEROS                                CL**8
01030            MOVE -1               TO INVOICE-LEN (INDX)               CL**8
01031            MOVE ER-3175          TO EMI-ERROR                        CL**8
01032            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  CL**8
01033            GO TO 1060-INCREMENT-INDX                                 CL*17
01034         ELSE                                                         CL**8
01035            PERFORM 6500-VERIFY-RECON-HEADER THRU 6590-EXIT.          CL**8
01036                                                                   EL6351
01037      IF INVOICE (INDX) = LOW-VALUES                                  CL*17
01038          GO TO 1060-INCREMENT-INDX.                                  CL*17
01039                                                                      CL**5
01040      IF WS-ACCEPT-1 (W-INDX) = 'A'                                   CL*17
01041          GO TO 1060-INCREMENT-INDX.                                  CL*17
01042                                                                      CL*17
01043      IF EIBAID = DFHPF5                                              CL*11
01044          MOVE 'Y'                TO  WS-PREV-PF5                     CL*19
01045          MOVE DFHENTER           TO  EIBAID                          CL*19
01046          MOVE 'A'                TO  WS-ACCEPT-1 (W-INDX)            CL*19
01047          GO TO 1060-INCREMENT-INDX.                                  CL*19
01048                                                                      CL*11
01049      MOVE PI-COMPANY-CD          TO  RECV-COMP-CD.                   CL**5
01050      MOVE '1'                    TO  RECV-TYPE.                      CL**5
01051      MOVE CARRIER (INDX)         TO  RECV-CARRIER.                   CL**5
01052      MOVE GROUPING (INDX)        TO  RECV-GROUPING.                  CL**5
01053      MOVE PI-SAVE-COMP-BAL-CODE  TO  RECV-BAL-LVL.                   CL*17
01054      MOVE FIN-RESP (INDX)        TO  RECV-FIN-RESP.                  CL**5
01055      MOVE ACCT (INDX)            TO  RECV-ACCOUNT.                   CL**5
01056      MOVE INVOICE (INDX)         TO  RECV-INVOICE.                   CL**5
01057                                                                      CL*26
01058      IF APPLIED (INDX) = 'A'                                         CL*26
01059          MOVE '1'                TO  RECV-ENTRY-TYPE                 CL*26
01060          MOVE 'A'                TO  RECV-RESPONSIBLE                CL*26
01061      ELSE                                                            CL*26
01062          MOVE LOW-VALUES         TO  RECV-RESPONSIBLE                CL*26
01063          IF APPLIED (INDX) = 'G'                                     CL*26
01064              MOVE '2'            TO  RECV-ENTRY-TYPE                 CL*26
01065          ELSE                                                        CL*26
01066              MOVE '3'            TO  RECV-ENTRY-TYPE.                CL*26
01067                                                                      CL**5
01068      IF REF-LEN (INDX) NOT = ZEROS                                   CL**5
01069          MOVE REF (INDX)         TO  RECV-REFERENCE                  CL**5
01070      ELSE                                                            CL**5
01071          MOVE LOW-VALUES         TO  RECV-REFERENCE.                 CL*21
01072                                                                      CL**5
01073      MOVE ZERO                   TO  RECV-RECORD-TYPE.               CL**5
01074      MOVE +0                     TO  RECV-RECORD-SEQ.                CL**5
01075                                                                      CL**5
01076      INSPECT ERRECV-KEY CONVERTING SPACES TO LOW-VALUES.             CL*30
01077                                                                      CL**5
01078      EXEC CICS HANDLE CONDITION                                      CL**5
01079          NOTFND   (1050-RECV-NOTFND)                                 CL*17
01080          NOTOPEN  (7200-RECV-FILE-NOTOPEN)                           CL**7
01081          END-EXEC.                                                   CL**5
01082                                                                      CL*20
01083      IF RECV-REFERENCE  =  LOW-VALUES                                CL*20
01084          IF MORTGAGE-SESSION                                         CL*21
01085              MOVE SPACES              TO RECV-REFERENCE.             CL*21
01086                                                                      CL**5
01087      EXEC CICS READ                                                  CL**5
01088          DATASET  (RECV-FILE-ID)                                     CL**5
01089          SET      (ADDRESS OF ACCOUNTS-RECEIVABLE)                   CL*30
01090          RIDFLD   (ERRECV-KEY)                                       CL**5
01091          EQUAL                                                       CL**5
01092          END-EXEC.                                                   CL**5
01093                                                                      CL**5
01094      GO TO 1060-INCREMENT-INDX.                                      CL*17
01095                                                                      CL**5
01096  1050-RECV-NOTFND.                                                   CL*17
01097      MOVE ER-3180                TO EMI-ERROR.                       CL*11
01098      MOVE -1                     TO INVOICE-LEN (INDX).              CL*11
01099      MOVE AL-UABON               TO INVOICE-ATTRB (INDX).            CL*11
01100                                                                      CL*11
01101      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL*11
01102                                                                      CL**5
01103  1060-INCREMENT-INDX.                                                CL*17
01104                                                                   EL6351
01105      IF INDX  IS LESS THAN +5                                        CL*14
01106          SET INDX  UP  BY  1                                         CL*14
01107          SET W-INDX TO INDX                                          CL*17
01108          GO TO 1010-EDIT-LOOP.                                    EL6351
01109                                                                   EL6351
01110      MOVE WS-ACCEPT             TO  PI-ACCEPT.                       CL*17
01111                                                                      CL*13
01112      IF EMI-ERROR = ZEROS                                         EL6351
01113          GO TO 2000-UPDATE-THE-FILE                               EL6351
01114      ELSE                                                         EL6351
01115          GO TO 8200-SEND-DATAONLY.                                EL6351
01116  EJECT                                                            EL6351
01117  2000-UPDATE-THE-FILE.                                            EL6351
01118      SET INDX                    TO  1.                           EL6351
01119                                                                   EL6351
01120  2100-UPDATE-LOOP.                                                EL6351
01121                                                                      CL*13
01122      IF INDX  IS GREATER THAN  +5                                 EL6351
01123          GO TO 2200-UPDATE-COMPLETE.                              EL6351
01124                                                                   EL6351
01125      IF CARR-LEN        (INDX) = ZEROS                               CL*29
01126         AND GRP-LEN     (INDX) = ZEROS                               CL*29
01127         AND FIN-LEN     (INDX) = ZEROS                               CL*29
01128         AND ACCT-LEN    (INDX) = ZEROS                               CL*29
01129         AND COMM-LEN    (INDX) = ZEROS                               CL*29
01130         AND RTYPE-LEN   (INDX) = ZEROS                               CL*29
01131         AND AMT-LEN     (INDX) = ZEROS                               CL*29
01132         AND EOM-DT-LEN  (INDX) = ZEROS                               CL*29
01133         AND INDATE-LEN  (INDX) = ZEROS                               CL*29
01134         SET INDX  UP  BY  1                                          CL*29
01135         GO TO 2100-UPDATE-LOOP.                                      CL*29
01136                                                                   EL6351
01137      EXEC CICS GETMAIN                                            EL6351
01138          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL*30
01139          LENGTH   (ERPYAJ-RECORD-LENGTH)                          EL6351
01140          INITIMG  (GETMAIN-SPACE)                                 EL6351
01141          END-EXEC.                                                EL6351
01142                                                                   EL6351
01143      MOVE SPACES                 TO  PENDING-PAY-ADJ.                CL*22
01144                                                                      CL*22
01145      MOVE 'PY'                   TO  PY-RECORD-ID.                EL6351
01146      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.               EL6351
01147                                                                      CL*17
01148      IF CARR-LEN   (INDX)        GREATER THAN ZEROS                  CL*13
01149         MOVE CARRIER     (INDX)  TO  PY-CARRIER.                     CL*13
01150                                                                      CL*13
01151      IF GRP-LEN       (INDX)     GREATER THAN ZEROS                  CL*13
01152         MOVE GROUPING (INDX)     TO  PY-GROUPING.                    CL*13
01153                                                                      CL*13
01154      IF FIN-LEN       (INDX)     GREATER THAN ZEROS                  CL*13
01155         MOVE FIN-RESP (INDX)     TO  PY-FIN-RESP.                    CL*13
01156                                                                      CL*13
01157      IF ACCT-LEN      (INDX)     GREATER THAN ZEROS                  CL*13
01158         MOVE ACCT     (INDX)     TO  PY-ACCOUNT.                     CL*13
01159                                                                      CL*13
01160      MOVE RTYPE       (INDX)     TO  PY-RECORD-TYPE.              EL6351
01161                                                                   EL6351
01162      IF REF-LEN       (INDX) NOT = ZEROS                          EL6351
01163         MOVE REF      (INDX)     TO  PY-REF-NO.                   EL6351
01164                                                                   EL6351
01165      IF INVOICE-LEN   (INDX) NOT = ZEROS                          EL6351
01166         MOVE INVOICE  (INDX)     TO  PY-BIL-INV.                  EL6351
01167                                                                   EL6351
01168      IF CREDIT-LEN    (INDX) NOT = ZEROS                          EL6351
01169         MOVE CREDIT   (INDX)     TO  PY-GL-CR.                    EL6351
01170                                                                   EL6351
01171      IF DEBIT-LEN     (INDX) NOT = ZEROS                          EL6351
01172         MOVE DEBIT    (INDX)     TO  PY-GL-DB.                    EL6351
01173                                                                   EL6351
01174      SET MINDEX  TO  INDX.                                           CL*25
01175                                                                      CL*25
01176      IF WS-EOM-DT (MINDEX)  NOT = ZEROS                              CL*25
01177          MOVE WS-EOM-DT (MINDEX) TO  PY-CREDIT-SELECT-DT             CL*25
01178      ELSE                                                            CL*25
01179          MOVE PI-CR-MONTH-END-DT TO  PY-CREDIT-SELECT-DT.            CL*25
01180                                                                      CL*25
01181      SET DINDEX  TO  INDX.                                           CL*25
01182                                                                      CL*25
01183      IF WS-INP-DT (DINDEX)  NOT = ZEROS                              CL*29
01184          MOVE WS-INP-DT (DINDEX) TO  PY-INPUT-DT                     CL*29
01185      ELSE                                                            CL*25
01186          MOVE WS-CURRENT-BIN-DT  TO  PY-INPUT-DT.                    CL*25
01187                                                                      CL*24
01188      MOVE APPLIED (INDX)         TO  PY-PMT-APPLIED.                 CL*17
01189      MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.              EL6351
01190                                                                   EL6351
01191      ADD +1                      TO  WORK-SEQ-NO.                 EL6351
01192                                                                   EL6351
01193      IF COMM-LEN (INDX) NOT = ZEROS                               EL6351
01194          MOVE COMM (INDX)        TO  PY-ENTRY-COMMENT.            EL6351
01195                                                                   EL6351
01196      SET WS-INDX                 TO  INDX.                        EL6351
01197      MOVE WS-EDITED-AMT(WS-INDX) TO  PY-ENTRY-AMT                    CL**8
01198                                      WS-ENTRY-AMT.                   CL**8
01199      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.            EL6351
01200      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.        EL6351
01201      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT.               CL*25
01202 *                                    PY-INPUT-DT.                    CL*25
01203      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL         EL6351
01204                                      PY-CHECK-QUE-SEQUENCE.       EL6351
01205      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT          EL6351
01206                                      PY-BILLED-DATE               EL6351
01207                                      PY-AR-DATE                   EL6351
01208                                      PY-REPORTED-DT               EL6351
01209                                      PY-CHECK-WRITTEN-DT             CL*22
01210                                      PY-GL-DATE.                     CL*22
01211                                                                      CL**2
01212  2115-WRITE-REC.                                                     CL**2
01213                                                                   EL6351
01214      IF MORTGAGE-SESSION                                             CL*10
01215          MOVE PI-MP-MONTH-END-DT TO  PY-CREDIT-SELECT-DT.            CL*25
01216                                                                      CL*10
01217      MOVE 'A'                    TO  JP-RECORD-TYPE.              EL6351
01218      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.              EL6351
01219                                                                      CL*10
01220      IF MORTGAGE-SESSION                                             CL*10
01221         PERFORM 6600-UPDATE-RECON-HEADER THRU 6690-EXIT.             CL*10
01222                                                                   EL6351
01223      EXEC CICS HANDLE CONDITION                                      CL*13
01224          DUPREC   (2125-DUP-RECORD)                                  CL*13
01225          NOTOPEN  (7000-PYAJ-FILE-NOTOPEN)                           CL*17
01226          END-EXEC.                                                   CL*13
01227                                                                      CL*13
01228  2120-WRITE-PYAJ-REC.                                                CL*13
01229                                                                      CL*13
01230      EXEC CICS WRITE                                              EL6351
01231          DATASET  (PYAJ-FILE-ID)                                  EL6351
01232          FROM     (PENDING-PAY-ADJ)                               EL6351
01233          RIDFLD   (PY-CONTROL-PRIMARY)                            EL6351
01234          END-EXEC.                                                EL6351
01235                                                                   EL6351
01236      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6351
01237                                                                   EL6351
01238      MOVE LOW-VALUES             TO  DATA-AREA (INDX).            EL6351
01239                                                                   EL6351
01240      GO TO 2100-UPDATE-LOOP.                                      EL6351
01241                                                                   EL6351
01242  2125-DUP-RECORD.                                                    CL*13
01243                                                                      CL*13
01244      COMPUTE PY-FILE-SEQ-NO = PY-FILE-SEQ-NO + 1.                    CL*13
01245                                                                      CL*13
01246      GO TO 2120-WRITE-PYAJ-REC.                                      CL*13
01247  2200-UPDATE-COMPLETE.                                            EL6351
01248                                                                      CL**5
01249      MOVE LOW-VALUES             TO  EL635BI.                        CL*12
01250      MOVE ER-0000                TO  EMI-ERROR                       CL*12
01251      MOVE -1                     TO  CAR1L.                       EL6351
01252                                                                   EL6351
01253      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   EL6351
01254                                                                   EL6351
01255      GO TO 8100-SEND-INITIAL-MAP.                                 EL6351
01256                                                                      CL**8
01257      EJECT                                                           CL**8
01258                                                                      CL**8
01259  6500-VERIFY-RECON-HEADER.                                           CL**8
01260                                                                      CL*10
01261      IF RTYPE-LEN (INDX)  GREATER THAN ZEROS                         CL*10
01262         IF RTYPE  (INDX)  NOT = 'R'                                  CL*10
01263            GO TO 6590-EXIT.                                          CL*10
01264                                                                      CL**8
01265      MOVE PI-COMPANY-CD           TO MPPRCN-COMPANY-CD.              CL**8
01266      MOVE INVOICE (INDX)          TO MPPRCN-INVOICE.                 CL**8
01267      MOVE +999999999              TO MPPRCN-RECORD-SEQU.             CL**8
01268                                                                      CL**8
01269      EXEC CICS HANDLE CONDITION                                      CL**8
01270          NOTFND   (6580-HEADER-NOTFND)                               CL**8
01271          END-EXEC.                                                   CL**8
01272                                                                      CL**8
01273      EXEC CICS READ                                                  CL**8
01274          DATASET   (MPPRCN-FILE-ID)                                  CL**8
01275          SET       (ADDRESS OF PAYMENT-RECONCILIATION)               CL*30
01276          RIDFLD    (MPPRCN-KEY)                                      CL**8
01277          END-EXEC.                                                   CL**8
01278                                                                      CL**8
01279      IF  NOT PI-NO-CARRIER-SECURITY                                  CL**9
01280              AND                                                     CL**9
01281          PI-CARRIER-SECURITY NOT EQUAL PR-CARRIER-A2                 CL**9
01282          MOVE -1                 TO CARR-LEN      (INDX)             CL**9
01283          MOVE AL-UANON           TO CARR-ATTRB    (INDX)             CL**9
01284          MOVE ER-9095            TO EMI-ERROR                        CL**9
01285          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**9
01286          GO TO 8200-SEND-DATAONLY.                                   CL**9
01287                                                                      CL**9
01288      MOVE PR-SECURITY-ACCESS-CODE                                    CL**9
01289                                  TO SC-SECURITY-ACCESS-CODE.         CL**9
01290      PERFORM 9920-PRODUCER-EVALUATION THRU 9920-EXIT.                CL**9
01291                                                                      CL**9
01292      IF  SC-PRODUCER-NOT-AUTHORIZED                                  CL**9
01293          MOVE -1                 TO ACCT-LEN      (INDX)             CL**9
01294          MOVE AL-UANON           TO ACCT-ATTRB    (INDX)             CL**9
01295          MOVE ER-9094            TO EMI-ERROR                        CL**9
01296          GO TO 8200-SEND-DATAONLY.                                   CL**9
01297                                                                      CL**9
01298      IF PR-HDR-POSTED                                                CL*10
01299         GO TO 6560-POST-ERROR.                                       CL**9
01300                                                                      CL**9
01301      GO TO 6590-EXIT.                                                CL**9
01302                                                                      CL**9
01303  6560-POST-ERROR.                                                    CL**9
01304                                                                      CL**9
01305      MOVE -1                     TO INVOICE-LEN (INDX)               CL**9
01306      MOVE ER-9296                TO EMI-ERROR.                       CL*10
01307      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**9
01308                                                                      CL**9
01309      GO TO 6590-EXIT.                                                CL**8
01310                                                                      CL**8
01311  6580-HEADER-NOTFND.                                                 CL**8
01312                                                                      CL**8
01313      MOVE -1                     TO INVOICE-LEN (INDX)               CL**8
01314      MOVE ER-9280                TO EMI-ERROR.                       CL**8
01315      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**8
01316                                                                      CL**8
01317  6590-EXIT.                                                          CL**8
01318      EXIT.                                                           CL**8
01319                                                                      CL**8
01320      EJECT                                                           CL**8
01321                                                                      CL**8
01322                                                                      CL**8
01323  6600-UPDATE-RECON-HEADER.                                           CL**8
01324                                                                      CL*10
01325      IF (PY-REMIT-RECEIVED OR PY-REMIT-IND-GROUPING)                 CL*13
01326         NEXT SENTENCE                                                CL*13
01327      ELSE                                                            CL*13
01328         GO TO 6690-EXIT.                                             CL*10
01329                                                                      CL**8
01330      MOVE PI-COMPANY-CD           TO MPPRCN-COMPANY-CD.              CL**8
01331      MOVE INVOICE (INDX)          TO MPPRCN-INVOICE.                 CL**8
01332      MOVE +999999999              TO MPPRCN-RECORD-SEQU.             CL**8
01333                                                                      CL**8
01334      EXEC CICS HANDLE CONDITION                                      CL**8
01335          NOTFND   (6680-HEADER-NOTFND)                               CL**8
01336          END-EXEC.                                                   CL**8
01337                                                                      CL**8
01338      EXEC CICS READ                                                  CL**8
01339          DATASET   (MPPRCN-FILE-ID)                                  CL**8
01340          SET       (ADDRESS OF PAYMENT-RECONCILIATION)               CL*30
01341          RIDFLD    (MPPRCN-KEY)                                      CL**8
01342          UPDATE                                                      CL**8
01343          END-EXEC.                                                   CL**8
01344                                                                      CL**8
01345      IF PR-HDR-POSTED                                                CL*10
01346         GO TO 6660-POST-ERROR.                                       CL**9
01347                                                                      CL**9
01348      ADD  WS-ENTRY-AMT           TO PR-RECEIVED-PREMIUM.             CL**8
01349      MOVE WS-CURRENT-BIN-DT      TO PR-RECEIVED-DT.                  CL**8
01350                                                                      CL*10
01351      MOVE PI-PROCESSOR-ID        TO PR-LAST-CHANGE-PROCESSOR.        CL*10
01352      MOVE EIBTIME                TO PR-LAST-CHANGE-TIME.             CL*10
01353      MOVE WS-CURRENT-BIN-DT      TO PR-LAST-CHANGE-DT.               CL*10
01354                                                                      CL**8
01355      EXEC CICS REWRITE                                               CL**8
01356          DATASET   (MPPRCN-FILE-ID)                                  CL**8
01357          FROM      (PAYMENT-RECONCILIATION)                          CL**8
01358          END-EXEC.                                                   CL**8
01359                                                                      CL**8
01360      GO TO 6690-EXIT.                                                CL**8
01361                                                                      CL**9
01362  6660-POST-ERROR.                                                    CL**9
01363                                                                      CL**9
01364      EXEC CICS SYNCPOINT ROLLBACK                                    CL**9
01365      END-EXEC.                                                       CL**9
01366                                                                      CL**9
01367      MOVE -1                     TO INVOICE-LEN (INDX)               CL**9
01368      MOVE ER-9296                TO EMI-ERROR.                       CL*10
01369      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**9
01370      GO TO 8200-SEND-DATAONLY.                                       CL**9
01371                                                                      CL**8
01372  6680-HEADER-NOTFND.                                                 CL**8
01373                                                                      CL**8
01374      EXEC CICS SYNCPOINT ROLLBACK                                    CL**8
01375      END-EXEC.                                                       CL**8
01376                                                                      CL**8
01377      MOVE -1                     TO INVOICE-LEN (INDX).              CL**8
01378      MOVE ER-9280                TO EMI-ERROR.                       CL**8
01379      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**8
01380      GO TO 8200-SEND-DATAONLY.                                       CL**8
01381                                                                      CL**8
01382  6690-EXIT.                                                          CL**8
01383      EXIT.                                                           CL**8
01384                                                                      CL**8
01385      EJECT                                                           CL**8
01386                                                                      CL**8
01387  EJECT                                                            EL6351
01388  7000-PYAJ-FILE-NOTOPEN.                                          EL6351
01389      MOVE -1                     TO  PFENTERL.                    EL6351
01390      MOVE ER-2232                TO  EMI-ERROR.                   EL6351
01391                                                                   EL6351
01392      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL6351
01393                                                                   EL6351
01394      GO TO 8200-SEND-DATAONLY.                                    EL6351
01395                                                                   EL6351
01396  7100-COMP-FILE-NOTOPEN.                                          EL6351
01397      MOVE -1                     TO  PFENTERL.                    EL6351
01398      MOVE ER-2233                TO  EMI-ERROR.                   EL6351
01399                                                                      CL**7
01400      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**7
01401                                                                      CL**7
01402      GO TO 8200-SEND-DATAONLY.                                       CL**7
01403  7200-RECV-FILE-NOTOPEN.                                             CL**7
01404      MOVE -1                     TO  PFENTERL.                       CL**7
01405      MOVE ER-3177                TO  EMI-ERROR.                      CL**8
01406                                                                   EL6351
01407      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  EL6351
01408                                                                   EL6351
01409      GO TO 8200-SEND-DATAONLY.                                    EL6351
01410  EJECT                                                            EL6351
01411  8100-SEND-INITIAL-MAP.                                           EL6351
01412                                                                      CL*24
01413      MOVE WS-CURRENT-DT          TO  DATEO.                       EL6351
01414      MOVE EIBTIME                TO  TIME-IN.                     EL6351
01415      MOVE TIME-OUT               TO  TIMEO.                       EL6351
01416      MOVE -1                     TO  CAR1L.                       EL6351
01417      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6351
01418      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    EL6351
01419                                                                      CL**8
01420      IF MORTGAGE-SESSION                                             CL**8
01421         MOVE  'PROD:'            TO ACTHDG1O                         CL**8
01422         MOVE AL-SANOF            TO ACTHDG1A                         CL**8
01423         MOVE  'PROD:'            TO ACTHDG2O                         CL**8
01424         MOVE AL-SANOF            TO ACTHDG2A                         CL**8
01425         MOVE  'PROD:'            TO ACTHDG3O                         CL**8
01426         MOVE AL-SANOF            TO ACTHDG3A                         CL**8
01427         MOVE  'PROD:'            TO ACTHDG4O                         CL**8
01428         MOVE AL-SANOF            TO ACTHDG4A                         CL**8
01429         MOVE  'PROD:'            TO ACTHDG5O                         CL**8
01430         MOVE AL-SANOF            TO ACTHDG5A.                        CL**8
01431                                                                   EL6351
01432      EXEC CICS SEND                                               EL6351
01433          MAP     (MAP-NAME)                                       EL6351
01434          MAPSET  (MAPSET-NAME)                                    EL6351
01435          FROM    (EL635BO)                                        EL6351
01436          ERASE                                                    EL6351
01437          CURSOR                                                   EL6351
01438          END-EXEC.                                                EL6351
01439                                                                   EL6351
01440      GO TO 9100-RETURN-TRAN.                                      EL6351
01441  EJECT                                                            EL6351
01442  8200-SEND-DATAONLY.                                              EL6351
01443                                                                      CL*24
01444      MOVE WS-CURRENT-DT          TO  DATEO.                       EL6351
01445      MOVE EIBTIME                TO  TIME-IN.                     EL6351
01446      MOVE TIME-OUT               TO  TIMEO.                       EL6351
01447      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6351
01448      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                    EL6351
01449                                                                   EL6351
01450      EXEC CICS SEND                                               EL6351
01451          MAP     (MAP-NAME)                                       EL6351
01452          MAPSET  (MAPSET-NAME)                                    EL6351
01453          FROM    (EL635BO)                                        EL6351
01454          DATAONLY                                                 EL6351
01455          CURSOR                                                   EL6351
01456          END-EXEC.                                                EL6351
01457                                                                   EL6351
01458      GO TO 9100-RETURN-TRAN.                                      EL6351
01459                                                                   EL6351
01460  8300-SEND-TEXT.                                                  EL6351
01461      EXEC CICS SEND TEXT                                          EL6351
01462          FROM    (LOGOFF-TEXT)                                    EL6351
01463          LENGTH  (LOGOFF-LENGTH)                                  EL6351
01464          ERASE                                                    EL6351
01465          FREEKB                                                   EL6351
01466          END-EXEC.                                                EL6351
01467                                                                   EL6351
01468      EXEC CICS RETURN                                             EL6351
01469          END-EXEC.                                                EL6351
01470  EJECT                                                            EL6351
01471  8400-LOG-JOURNAL-RECORD.                                         EL6351
01472 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL6351
01473 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL6351
01474 *    MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                  EL6351
01475 *    MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.       EL6351
01476                                                                   EL6351
01477 *    EXEC CICS JOURNAL                                            EL6351
01478 *        JFILEID  (PI-JOURNAL-FILE-ID)                            EL6351
01479 *        JTYPEID  ('EL')                                          EL6351
01480 *        FROM     (JOURNAL-RECORD)                                EL6351
01481 *        LENGTH   (223)                                           EL6351
01482 *        END-EXEC.                                                EL6351
01483                                                                   EL6351
01484  8500-DATE-CONVERT.                                               EL6351
01485      EXEC CICS LINK                                               EL6351
01486          PROGRAM   (LINK-CLDATCV)                                 EL6351
01487          COMMAREA  (DATE-CONVERSION-DATA)                         EL6351
01488          LENGTH    (DC-COMM-LENGTH)                               EL6351
01489          END-EXEC.                                                EL6351
01490                                                                   EL6351
01491  8500-EXIT.                                                       EL6351
01492      EXIT.                                                        EL6351
01493                                                                   EL6351
01494  8600-DEEDIT.                                                     EL6351
01495      EXEC CICS BIF DEEDIT                                         EL6351
01496          FIELD   (DEEDIT-FIELD)                                   EL6351
01497          LENGTH  (11)                                             EL6351
01498          END-EXEC.                                                EL6351
01499                                                                   EL6351
01500  8600-EXIT.                                                       EL6351
01501      EXIT.                                                        EL6351
01502  EJECT                                                            EL6351
01503  8800-UNAUTHORIZED-ACCESS.                                        EL6351
01504      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL6351
01505                                                                   EL6351
01506      GO TO 8300-SEND-TEXT.                                        EL6351
01507                                                                   EL6351
01508  8810-PF23.                                                       EL6351
01509      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6351
01510      MOVE XCTL-005               TO  PGM-NAME.                    EL6351
01511                                                                   EL6351
01512      GO TO 9300-XCTL.                                             EL6351
01513                                                                   EL6351
01514  9000-RETURN-CICS.                                                EL6351
01515      EXEC CICS RETURN                                             EL6351
01516          END-EXEC.                                                EL6351
01517                                                                   EL6351
01518  9100-RETURN-TRAN.                                                EL6351
01519      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6351
01520      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL6351
01521                                                                   EL6351
01522      EXEC CICS RETURN                                             EL6351
01523          TRANSID   (TRANS-ID)                                     EL6351
01524          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6351
01525          LENGTH    (PI-COMM-LENGTH)                               EL6351
01526          END-EXEC.                                                EL6351
01527                                                                   EL6351
01528  9200-RETURN-MAIN-MENU.                                           EL6351
01529                                                                      CL**9
01530      IF  CREDIT-SESSION                                              CL**9
01531          MOVE XCTL-EL626         TO PGM-NAME                         CL**9
01532                                                                      CL**9
01533      ELSE                                                            CL**9
01534          IF  CLAIM-SESSION                                           CL**9
01535              MOVE XCTL-EL126     TO PGM-NAME                         CL**9
01536                                                                      CL**9
01537          ELSE                                                        CL**9
01538              IF  MORTGAGE-SESSION                                    CL**9
01539                  MOVE XCTL-EM626 TO PGM-NAME                         CL**9
01540                                                                      CL**9
01541              ELSE                                                    CL**9
01542                  IF  GENERAL-LEDGER-SESSION                          CL**9
01543                      MOVE XCTL-GL800                                 CL**9
01544                                  TO PGM-NAME.                        CL**9
01545                                                                   EL6351
01546      GO TO 9300-XCTL.                                             EL6351
01547                                                                   EL6351
01548  9300-XCTL.                                                       EL6351
01549      EXEC CICS XCTL                                               EL6351
01550          PROGRAM   (PGM-NAME)                                     EL6351
01551          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL6351
01552          LENGTH    (PI-COMM-LENGTH)                               EL6351
01553          END-EXEC.                                                EL6351
01554                                                                   EL6351
01555  9400-CLEAR.                                                      EL6351
01556      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL6351
01557                                                                   EL6351
01558      GO TO 9300-XCTL.                                             EL6351
01559                                                                   EL6351
01560  9500-PF12.                                                       EL6351
01561      MOVE XCTL-010               TO  PGM-NAME.                    EL6351
01562                                                                   EL6351
01563      GO TO 9300-XCTL.                                             EL6351
01564                                                                   EL6351
01565  9600-PGMID-ERROR.                                                EL6351
01566      EXEC CICS HANDLE CONDITION                                   EL6351
01567          PGMIDERR  (8300-SEND-TEXT)                               EL6351
01568          END-EXEC.                                                EL6351
01569                                                                   EL6351
01570      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL6351
01571      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL6351
01572      MOVE XCTL-005               TO  PGM-NAME.                    EL6351
01573      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL6351
01574      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6351
01575                                                                   EL6351
01576      GO TO 9300-XCTL.                                             EL6351
01577                                                                   EL6351
01578  9920-PRODUCER-EVALUATION.                                           CL**9
01579 ******************************************************************   CL**9
01580 *                                                                *   CL**9
01581 *       THIS AREA CONTAINS THE LOGIC NECESSARY TO CHECK THE      *   CL**9
01582 *       AUTHORIZATION OF THE USER TO 'ACCESS' A PRODUCER.        *   CL**9
01583 *       IF THE USER IS NOT AUTHORIZED AN APPROPRIATE MESSAGE     *   CL**9
01584 *       IS GENERATED.                                            *   CL**9
01585 *                                                                *   CL**9
01586 ******************************************************************   CL**9
01587                                                                      CL**9
01588      MOVE SPACES                 TO SC-PRODUCER-AUTHORIZED-SW.       CL**9
01589                                                                      CL**9
01590      IF  PI-NO-PRODUCER-SECURITY                                     CL**9
01591              OR                                                      CL**9
01592          PI-PROCESSOR-ID EQUAL 'LGXX'                                CL**9
01593          GO TO 9920-EXIT.                                            CL**9
01594                                                                      CL**9
01595      SET PI-ACCESS-NDX           TO +1                               CL**9
01596                                                                      CL**9
01597      SEARCH PI-ACCESS-CODE                                           CL**9
01598          VARYING PI-ACCESS-NDX                                       CL**9
01599                                                                      CL**9
01600          AT END                                                      CL**9
01601              MOVE 'N'            TO SC-PRODUCER-AUTHORIZED-SW        CL**9
01602                                                                      CL**9
01603          WHEN                                                        CL**9
01604              PI-ACCESS-CODE (PI-ACCESS-NDX)                          CL**9
01605                  EQUAL SC-SECURITY-ACCESS-CODE                       CL**9
01606              GO TO 9920-EXIT.                                        CL**9
01607                                                                      CL**9
01608  9920-EXIT.                                                          CL**9
01609      EXIT.                                                           CL**9
01610      EJECT                                                           CL**9
01611  9900-ERROR-FORMAT.                                               EL6351
01612      IF NOT EMI-ERRORS-COMPLETE                                   EL6351
01613          MOVE LINK-001           TO  PGM-NAME                     EL6351
01614          EXEC CICS LINK                                           EL6351
01615              PROGRAM   (PGM-NAME)                                 EL6351
01616              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL6351
01617              LENGTH    (EMI-COMM-LENGTH)                          EL6351
01618              END-EXEC.                                            EL6351
01619                                                                   EL6351
01620  9900-EXIT.                                                       EL6351
01621      EXIT.                                                        EL6351
01622                                                                   EL6351
01623  9990-ABEND.                                                      EL6351
01624      MOVE LINK-004               TO  PGM-NAME.                    EL6351
01625      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL6351
01626                                                                   EL6351
01627      EXEC CICS LINK                                               EL6351
01628          PROGRAM   (PGM-NAME)                                     EL6351
01629          COMMAREA  (EMI-LINE1)                                    EL6351
01630          LENGTH    (72)                                           EL6351
01631          END-EXEC.                                                EL6351
01632                                                                   EL6351
01633      MOVE -1                     TO  PFENTERL.                    EL6351
01634                                                                   EL6351
01635      GO TO 8200-SEND-DATAONLY.                                    EL6351
01636                                                                   EL6351
01637      GOBACK.                                                      EL6351
01638                                                                   EL6351
01639  9995-SECURITY-VIOLATION.                                         EL6351
01640                              COPY ELCSCTP.                        EL6351
01641                                                                   EL6351
01642  9995-EXIT.                                                       EL6351
01643      EXIT.                                                        EL6351
01644                                                                   EL6351
