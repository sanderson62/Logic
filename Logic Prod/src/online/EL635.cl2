00001  IDENTIFICATION DIVISION.                                         11/10/98
00002                                                                   EL635
00003  PROGRAM-ID.                 EL635 .                                 LV002
00004 *              PROGRAM CONVERTED BY                                  CL**1
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**1
00006 *              CONVERSION DATE 06/29/94 10:27:57.                    CL**1
00007 *                            VMOD=2.042                              CL**2
00008 *                                                                    CL**1
00009 ******************************************************************   CL**1
00010 ******************************************************************   CL**1
00011 *                                                                *   CL**1
00012 *         THIS IS PROGRAM IS USED BY BOTH THE CREDIT AND         *   CL**1
00013 *         MORTGAGE SYSTEMS.  ANY CHANGES MADE, SHOULD ONLY       *   CL**1
00014 *         BE MADE AFTER CONSIDERING WHAT IMPACT THEY MAY         *   CL**1
00015 *         HAVE ON EITHER SYSTEM.                                 *   CL**1
00016 *                                                                *   CL**1
00017 ******************************************************************   CL**1
00018 ******************************************************************   CL**1
00019                                                                      CL**1
00020 *AUTHOR.        LOGIC,INC.                                           CL**1
00021 *               DALLAS, TEXAS.                                       CL**1
00022                                                                      CL**1
00023 *DATE-COMPILED.                                                      CL**1
00024                                                                      CL**1
00025 *SECURITY.   *****************************************************   CL**1
00026 *            *                                                   *   CL**1
00027 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**1
00028 *            *                                                   *   CL**1
00029 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**1
00030 *                                                                *   CL**1
00031 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**1
00032 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**1
00033 *            *                                                   *   CL**1
00034 *            *****************************************************   CL**1
00035                                                                      CL**1
00036 *REMARKS.                                                            CL**1
00037 *        TRANSACTION - EXJ4 - COMPENSATION PAYMENTS/ADJUSTMENTS.     CL**1
00038                                                                      CL**1
00039  ENVIRONMENT DIVISION.                                               CL**1
00040  DATA DIVISION.                                                      CL**1
00041  EJECT                                                               CL**1
00042  WORKING-STORAGE SECTION.                                            CL**1
00043  77  FILLER  PIC X(32)  VALUE '********************************'.    CL**1
00044  77  FILLER  PIC X(32)  VALUE '*    EL635 WORKING STORAGE     *'.    CL**1
00045  77  FILLER  PIC X(32)  VALUE '**********  VMOD=2.042 *********'.    CL**2
00046                                                                      CL**1
00047      COPY ELCSCTM.                                                   CL**1
00048      COPY ELCSCRTY.                                                  CL**1
00049      COPY MPCSCRT.                                                   CL**1
00050                                                                      CL**1
00051     EJECT                                                            CL**1
00052                                                                      CL**1
00053  01  STANDARD-AREAS.                                                 CL**1
00054      12  W-APPL-SCRTY-NDX    PIC S9(4) COMP  VALUE +38.              CL**1
00055      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.            CL**1
00056      12  MAP-NAME            PIC  X(8)       VALUE 'EL635A'.         CL**1
00057      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL635S'.         CL**1
00058      12  SCREEN-NUMBER       PIC  X(4)       VALUE '633A'.           CL**1
00059      12  TRANS-ID            PIC  X(4)       VALUE 'EXJ4'.           CL**1
00060      12  EL630-TRANS-ID      PIC  X(4)       VALUE 'EXA5'.           CL**1
00061      12  EL640-TRANS-ID      PIC  X(4)       VALUE 'EXC1'.           CL**1
00062      12  EL642-TRANS-ID      PIC  X(4)       VALUE 'EXH7'.           CL**1
00063      12  EL652-TRANS-ID      PIC  X(4)       VALUE 'EXD4'.           CL**1
00064      12  EL6351-TRANS-ID     PIC  X(4)       VALUE 'EXJ5'.           CL**1
00065      12  THIS-PGM            PIC  X(8)       VALUE 'EL635'.          CL**1
00066      12  PGM-NAME            PIC  X(8).                              CL**1
00067      12  TIME-IN             PIC S9(7).                              CL**1
00068      12  TIME-OUT-R  REDEFINES  TIME-IN.                             CL**1
00069          16  FILLER          PIC  X.                                 CL**1
00070          16  TIME-OUT        PIC  99V99.                             CL**1
00071          16  FILLER          PIC  XX.                                CL**1
00072      12  EL640               PIC  X(8)       VALUE 'EL640'.          CL**1
00073      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.          CL**1
00074      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.          CL**1
00075      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.          CL**1
00076      12  XCTL-EM626          PIC  X(8)       VALUE 'EM626'.          CL**1
00077      12  XCTL-652            PIC  X(8)       VALUE 'EL652'.          CL**1
00078      12  XCTL-6351           PIC  X(8)       VALUE 'EL6351'.         CL**1
00079      12  XCTL-640            PIC  X(8)       VALUE 'EL640'.          CL**1
00080      12  XCTL-642            PIC  X(8)       VALUE 'EL642'.          CL**1
00081      12  LINK-001            PIC  X(8)       VALUE 'EL001'.          CL**1
00082      12  LINK-004            PIC  X(8)       VALUE 'EL004'.          CL**1
00083      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.        CL**1
00084      12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.         CL**1
00085      12  MPPYAJ-FILE-ID      PIC  X(8)       VALUE 'MPPYAJ'.         CL**1
00086      12  MPPRCN-FILE-ID      PIC  X(8)       VALUE 'MPPRCN'.         CL**1
00087      12  CHKQ-FILE-ID        PIC  X(8)       VALUE 'ERCHKQ'.         CL**1
00088      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.         CL**1
00089      12  RECV-FILE-ID        PIC  X(8)       VALUE 'ERRECV'.         CL**1
00090      12  RETURNED-FROM       PIC  X(8)       VALUE SPACES.           CL**1
00091      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.           CL**1
00092      12  WS-CURRENT-MDY      PIC  X(6)       VALUE SPACES.           CL**1
00093      12  WS-CURRENT-BIN-DT   PIC  XX         VALUE SPACES.           CL**1
00094      12  WS-COMMENT-FULL.                                            CL**1
00095          16  WS-COMMENT-24-POS  PIC  X(24)   VALUE SPACES.           CL**1
00096          16  FILLER             PIC  X(6)    VALUE SPACES.           CL**1
00097      12  EL630-SENT-SW       PIC  X          VALUE SPACE.            CL**1
00098          88  SENT-FROM-PENDING               VALUE 'Y'.              CL**1
00099      12  PYAJ-READ-SW        PIC  X          VALUE 'Y'.              CL**1
00100          88  PYAJ-1ST-READ                   VALUE 'Y'.              CL**1
00101      12  WARNING-SW          PIC  X          VALUE 'N'.              CL**1
00102      12  WS-SECOND-READ      PIC  X          VALUE '1'.              CL**1
00103      12  ZERO-NDX            PIC  9          VALUE ZERO.             CL**1
00104      12  WORK-SEQ-NO         PIC S9(9)  COMP-3  VALUE ZERO.          CL**1
00105      12  WS-CURSOR-POS-1     PIC S9(4)  COMP VALUE +721.             CL**1
00106      12  WS-CURSOR-POS-2     PIC S9(4)  COMP VALUE +1041.            CL**1
00107      12  WS-CURSOR-POS-3     PIC S9(4)  COMP VALUE +1361.            CL**1
00108      12  WS-EDIT-PATTERN     PIC 9(7)V99     VALUE ZERO.             CL**1
00109      12  WS-EDIT  REDEFINES  WS-EDIT-PATTERN PIC X(9).               CL**1
00110      12  WS-EDITED-AMOUNT.                                           CL**1
00111          16  WS-EDIT-AMOUNT  PIC X(9)        VALUE ZERO.             CL**1
00112          16  WS-EDIT-SIGN    PIC X           VALUE ' '.              CL**1
00113      12  WS-EMHD1            PIC  X(6)       VALUE 'EOM DT'.         CL**1
00114      12  WS-EMHD2.                                                   CL**1
00115          16  FILLER          PIC  X(20)      VALUE                   CL**1
00116                  '       CREDIT - DEBI'.                             CL**1
00117          16  FILLER          PIC  X(20)      VALUE                   CL**1
00118                  'T         TYPE    AM'.                             CL**1
00119          16  FILLER          PIC  X(20)      VALUE                   CL**1
00120                  'OUNT    G/A VOID MAI'.                             CL**1
00121          16  FILLER          PIC  X(19)      VALUE                   CL**1
00122                  'NT DT BILLED INP DT'.                              CL**1
00123      12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.            CL**1
00124          88  VALID-MTG-REC-TYPE              VALUE  'R' 'D' 'C'      CL**1
00125                                                     'S' 'T' 'U'      CL**1
00126                                                     'X' 'Y' 'Z'      CL**1
00127                                                     'F'              CL**1
00128                                                     'G'.             CL**1
00129          88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'      CL**1
00130                                                     'S' 'T' 'U'      CL**1
00131                                                     'X' 'Y' 'Z'      CL**1
00132                                                     'F'.             CL**1
00133                                                                      CL**1
00134          88  MON-VALID-REC-TYPE              VALUE  'R' 'D' 'C'      CL**1
00135                                                     'Z'.             CL**1
00136          88  ANL-VALID-REC-TYPE              VALUE  'R' 'D' 'C'      CL**1
00137                                                     'S' 'T' 'U'      CL**1
00138                                                     'X' 'Y' 'Z'.     CL**1
00139          88  NCL-VALID-REC-TYPE              VALUE  'R' 'D' 'C'      CL**1
00140                                                     'X' 'Y' 'Z'.     CL**1
00141                                                                      CL**1
00142      12  FORCE-SHOW-SW       PIC  X          VALUE SPACE.            CL**1
00143          88  FORCE-SHOW                      VALUE  'Y'.             CL**1
00144      12  OVERWRITE-AGENT-SW  PIC  X          VALUE SPACE.            CL**1
00145          88  OVERWRITE-AGENT                 VALUE  'Y'.             CL**1
00146                                                                      CL**1
00147      12  WS-RECON-SW         PIC  X          VALUE SPACE.            CL**1
00148          88  WS-RECON-ADD                    VALUE 'A'.              CL**1
00149          88  WS-RECON-UPDATE                 VALUE 'U'.              CL**1
00150          88  WS-RECON-DELETE                 VALUE 'D'.              CL**1
00151                                                                      CL**1
00152      EJECT                                                           CL**1
00153      12  WS-ERROR-CODES.                                             CL**1
00154          16  ER-0000         PIC  X(4)       VALUE '0000'.           CL**1
00155          16  ER-0008         PIC  X(4)       VALUE '0008'.           CL**1
00156          16  ER-0023         PIC  X(4)       VALUE '0023'.           CL**1
00157          16  ER-0029         PIC  X(4)       VALUE '0029'.           CL**1
00158          16  ER-0070         PIC  X(4)       VALUE '0070'.           CL**1
00159          16  ER-0587         PIC  X(4)       VALUE '0587'.           CL**1
00160          16  ER-0714         PIC  X(4)       VALUE '0714'.           CL**1
00161          16  ER-0761         PIC  X(4)       VALUE '0761'.           CL**1
00162          16  ER-2056         PIC  X(4)       VALUE '2056'.           CL**1
00163          16  ER-2231         PIC  X(4)       VALUE '2231'.           CL**1
00164          16  ER-2232         PIC  X(4)       VALUE '2232'.           CL**1
00165          16  ER-2233         PIC  X(4)       VALUE '2233'.           CL**1
00166          16  ER-2234         PIC  X(4)       VALUE '2234'.           CL**1
00167          16  ER-2235         PIC  X(4)       VALUE '2235'.           CL**1
00168          16  ER-2236         PIC  X(4)       VALUE '2236'.           CL**1
00169          16  ER-2237         PIC  X(4)       VALUE '2237'.           CL**1
00170          16  ER-2238         PIC  X(4)       VALUE '2238'.           CL**1
00171          16  ER-2239         PIC  X(4)       VALUE '2239'.           CL**1
00172          16  ER-2244         PIC  X(4)       VALUE '2244'.           CL**1
00173          16  ER-2245         PIC  X(4)       VALUE '2245'.           CL**1
00174          16  ER-2246         PIC  X(4)       VALUE '2246'.           CL**1
00175          16  ER-2370         PIC  X(4)       VALUE '2370'.           CL**1
00176          16  ER-2432         PIC  X(4)       VALUE '2432'.           CL**1
00177          16  ER-2449         PIC  X(4)       VALUE '2449'.           CL**1
00178          16  ER-2587         PIC  X(4)       VALUE '2587'.           CL**1
00179          16  ER-2588         PIC  X(4)       VALUE '2588'.           CL**1
00180          16  ER-2595         PIC  X(4)       VALUE '2595'.           CL**1
00181          16  ER-2596         PIC  X(4)       VALUE '2596'.           CL**1
00182          16  ER-2929         PIC  X(4)       VALUE '2929'.           CL**1
00183          16  ER-3020         PIC  X(4)       VALUE '3020'.           CL**1
00184          16  ER-3146         PIC  X(4)       VALUE '3146'.           CL**1
00185          16  ER-3172         PIC  X(4)       VALUE '3172'.           CL**1
00186          16  ER-3175         PIC  X(4)       VALUE '3175'.           CL**1
00187          16  ER-3177         PIC  X(4)       VALUE '3177'.           CL**1
00188          16  ER-3178         PIC  X(4)       VALUE '3178'.           CL**1
00189          16  ER-3179         PIC  X(4)       VALUE '3179'.           CL**1
00190          16  ER-3180         PIC  X(4)       VALUE '3180'.           CL**1
00191          16  ER-3183         PIC  X(4)       VALUE '3183'.           CL**1
00192          16  ER-3190         PIC  X(4)       VALUE '3190'.           CL**1
00193          16  ER-3193         PIC  X(4)       VALUE '3193'.           CL**1
00194          16  ER-3195         PIC  X(4)       VALUE '3195'.           CL**1
00195          16  ER-3196         PIC  X(4)       VALUE '3196'.           CL**1
00196          16  ER-3260         PIC  X(4)       VALUE '3260'.           CL**1
00197          16  ER-3262         PIC  X(4)       VALUE '3262'.           CL**1
00198          16  ER-3263         PIC  X(4)       VALUE '3263'.           CL**1
00199          16  ER-3264         PIC  X(4)       VALUE '3264'.           CL**1
00200          16  ER-3265         PIC  X(4)       VALUE '3265'.           CL**1
00201          16  ER-3266         PIC  X(4)       VALUE '3266'.           CL**1
00202          16  ER-7806         PIC  X(4)       VALUE '7806'.           CL**1
00203          16  ER-9094         PIC  X(4)       VALUE '9094'.           CL**1
00204          16  ER-9095         PIC  X(4)       VALUE '9095'.           CL**1
00205          16  ER-9096         PIC  X(4)       VALUE '9096'.           CL**1
00206          16  ER-9097         PIC  X(4)       VALUE '9097'.           CL**1
00207          16  ER-9179         PIC  X(4)       VALUE '9179'.           CL**1
00208          16  ER-9280         PIC  X(4)       VALUE '9280'.           CL**1
00209          16  ER-9296         PIC  X(4)       VALUE '9296'.           CL**1
00210          16  ER-9374         PIC  X(4)       VALUE '9374'.           CL**1
00211                                                                      CL**1
00212  01  WORK-AREA.                                                      CL**1
00213      12  QID.                                                        CL**1
00214          16  QID-TERM            PIC X(4)      VALUE SPACES.         CL**1
00215          16  FILLER              PIC X(4)      VALUE '635A'.         CL**1
00216      12  WS-SAVE-WS-INDX         PIC S9(4) COMP VALUE ZERO.          CL**1
00217      12  WS-EDITED-AMTS OCCURS 6 TIMES                               CL**1
00218                                 INDEXED BY WS-INDX.                  CL**1
00219          16  WS-EDITED-AMT       PIC S9(9)V99 COMP-3.                CL**1
00220                                                                      CL**1
00221      12  DATE-TEST-AREA      PIC  9(6).                              CL**1
00222      12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.                CL**1
00223          16  DATE-TEST-MM    PIC  99.                                CL**1
00224          16  DATE-TEST-DD    PIC  99.                                CL**1
00225          16  DATE-TEST-YY    PIC  99.                                CL**1
00226      12  DIVIDE-RESULT       PIC  99           VALUE ZERO.           CL**1
00227      12  DIVIDE-REMAINDER    PIC  9            VALUE ZERO.           CL**1
00228      12  PREV-BIN-MAINT-DT   PIC  XX           VALUE SPACES.         CL**1
00229      12  PREV-MAINT-DT       PIC  9(6)         VALUE ZEROS.          CL**1
00230      12  PREV-BIN-BL-DT      PIC  XX           VALUE SPACES.         CL**1
00231      12  PREV-BL-DT          PIC  9(6)         VALUE ZEROS.          CL**1
00232      12  TOTAL-ACCT-AMT      PIC S9(7)V99      VALUE ZEROS.          CL**1
00233      12  TOTAL-ACCT-NET      PIC S9(7)V99      VALUE ZEROS.          CL**1
00234      12  WS-OLD-ENTRY-AMT    PIC S9(7)V99      VALUE ZEROS.          CL**1
00235      12  WS-ENTRY-AMT        PIC S9(7)V99      VALUE ZEROS.          CL**1
00236      12  WS-WORK-BALANCE     PIC S9(7)V99      VALUE ZEROS.          CL**1
00237      12  WS-BAL-AMOUNT       PIC S9(7)V99      VALUE ZEROS.          CL**1
00238      12  WS-AR-BALANCE       PIC S9(7)V99      VALUE ZEROS.          CL**1
00239      12  WS-SAVE-INDEX-VALUE PIC S9(4) COMP    VALUE ZEROS.          CL**1
00240      12  WS-SAVE-NDX-VALUE   PIC S9(4) COMP    VALUE ZEROS.          CL**1
00241      12  WS-EOM-DTS OCCURS 3  TIMES                                  CL**1
00242                               INDEXED BY PINDEX.                     CL**1
00243          16  WS-EOM-DT               PIC XX.                         CL**1
00244      12  WS-INP-DTS OCCURS 3  TIMES                                  CL**1
00245                               INDEXED BY DINDEX.                     CL**1
00246          16  WS-INP-DT               PIC XX.                         CL**1
00247      12  WS-PREV-PF5                 PIC X.                          CL**1
00248      12  WS-ACCEPT-TABLE.                                            CL**1
00249          16  WS-ACCEPT-1 OCCURS 3 TIMES                              CL**1
00250                          INDEXED BY W-INDX                           CL**1
00251                                      PIC X.                          CL**1
00252                                                                      CL**1
00253      EJECT                                                           CL**1
00254  01  ACCESS-KEYS.                                                    CL**1
00255      12  ERPYAJ-KEY.                                                 CL**1
00256          16  PYAJ-COMP-CD            PIC  X      VALUE SPACE.        CL**1
00257          16  PYAJ-CARRIER            PIC  X      VALUE SPACES.       CL**1
00258          16  PYAJ-GROUPING           PIC  X(6)   VALUE SPACES.       CL**1
00259          16  PYAJ-FIN-RESP           PIC  X(10)  VALUE SPACES.       CL**1
00260          16  PYAJ-ACCOUNT            PIC  X(10)  VALUE SPACES.       CL**1
00261          16  PYAJ-FILE-SEQ-NO        PIC S9(8)   VALUE +0  COMP.     CL**1
00262          16  PYAJ-RECORD-TYPE        PIC  X      VALUE SPACES.       CL**1
00263                                                                      CL**1
00264      12  ERPYAJ-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.      CL**1
00265      12  ERPYAJ-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.      CL**1
00266                                                                      CL**1
00267      12  ERCHKQ-KEY.                                                 CL**1
00268          16  CHKQ-COMPANY-CD         PIC  X      VALUE SPACE.        CL**1
00269          16  CHKQ-CONTROL-NUMBER     PIC S9(8)   VALUE +0  COMP.     CL**1
00270          16  CHKQ-SEQUENCE-NUMBER    PIC S9(4)   VALUE +0  COMP.     CL**1
00271      12  ERCOMP-KEY.                                                 CL**1
00272          16  COMP-COMP-CD            PIC  X      VALUE SPACE.        CL**1
00273          16  COMP-CARRIER            PIC  X      VALUE SPACES.       CL**1
00274          16  COMP-GROUPING           PIC  X(6)   VALUE SPACES.       CL**1
00275          16  COMP-FIN-RESP           PIC  X(10)  VALUE SPACES.       CL**1
00276          16  COMP-ACCOUNT            PIC  X(10)  VALUE SPACES.       CL**1
00277          16  COMP-RECORD-TYPE        PIC  X      VALUE SPACES.       CL**1
00278      12  ERRECV-KEY.                                                 CL**1
00279          16  RECV-COMP-CD            PIC  X      VALUE SPACE.        CL**1
00280          16  RECV-TYPE               PIC  X      VALUE SPACES.       CL**1
00281          16  RECV-CARRIER            PIC  X      VALUE SPACES.       CL**1
00282          16  RECV-GROUPING           PIC  X(6)   VALUE SPACES.       CL**1
00283          16  RECV-BAL-LVL            PIC  X      VALUE SPACES.       CL**1
00284          16  RECV-ENTRY-TYPE         PIC  X      VALUE SPACES.       CL**1
00285          16  RECV-FIN-RESP           PIC  X(10)  VALUE SPACES.       CL**1
00286          16  RECV-ACCOUNT            PIC  X(10)  VALUE SPACES.       CL**1
00287          16  RECV-INVOICE            PIC  X(6)   VALUE SPACES.       CL**1
00288          16  RECV-REFERENCE          PIC  X(12)  VALUE SPACES.       CL**1
00289          16  RECV-RESPONSIBLE        PIC  X      VALUE SPACES.       CL**1
00290          16  RECV-RECORD-TYPE        PIC  X      VALUE SPACES.       CL**1
00291          16  RECV-RECORD-SEQ         PIC  S9(4)  VALUE +0  COMP.     CL**1
00292      12  ERRECV-KEY-LEN              PIC  S9(4)  COMP VALUE +51.     CL**1
00293                                                                      CL**1
00294      12  MPPRCN-KEY.                                                 CL**1
00295          16  MPPRCN-COMPANY-CD       PIC X       VALUE SPACES.       CL**1
00296          16  MPPRCN-INVOICE          PIC X(6)    VALUE SPACES.       CL**1
00297          16  MPPRCN-RECORD-SEQU      PIC S9(7)   VALUE +0  COMP-3.   CL**1
00298                                                                      CL**1
00299  EJECT                                                               CL**1
00300      COPY ELCDATE.                                                   CL**1
00301  EJECT                                                               CL**1
00302      COPY ELCLOGOF.                                                  CL**1
00303  EJECT                                                               CL**1
00304      COPY ELCATTR.                                                   CL**1
00305  EJECT                                                               CL**1
00306      COPY ELCEMIB.                                                   CL**1
00307  EJECT                                                               CL**1
00308      COPY ELCINTF.                                                   CL**1
00309      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                    CL**1
00310          16  PI-PYAJ-FILE-SW         PIC  X.                         CL**1
00311              88  END-OF-ACCT                 VALUE 'A'.              CL**1
00312              88  END-OF-FILE                 VALUE 'X'.              CL**1
00313              88  TOP-OF-FILE                 VALUE 'T'.              CL**1
00314              88  PAGE-FULL                   VALUE 'F'.              CL**1
00315              88  NO-RECORDS                  VALUE 'Y'.              CL**1
00316              88  NOT-OPEN                    VALUE 'Z'.              CL**1
00317          16  PI-PREV-FUNCTION        PIC  X.                         CL**1
00318          16  PI-SAV-FUNCTION         PIC  X.                         CL**1
00319          16  PI-SEQ-NOS.                                             CL**1
00320              20  FILLER  OCCURS 3 TIMES                              CL**1
00321                              INDEXED BY NDX.                         CL**1
00322                  24  PI-REC-TYPE     PIC  X.                         CL**1
00323                  24  PI-FILE-SEQ-NO  PIC S9(8)          COMP.        CL**1
00324                  24  PI-APPLIED      PIC  X.                         CL**1
00325                  24  PI-REFERENCE    PIC  X(12).                     CL**1
00326                  24  PI-INVOICE      PIC  X(6).                      CL**1
00327                  24  PI-AMOUNT       PIC  S9(7)V99 COMP-3.           CL**1
00328          16  PI-SAV-ENDING-PYAJ-KEY.                                 CL**1
00329              20  PI-SAV-COMP-CD      PIC  X.                         CL**1
00330              20  PI-SAV-COMP-CONTROL.                                CL**1
00331                  24  PI-SAV-CARRIER      PIC  X.                     CL**1
00332                  24  PI-SAV-GROUPING     PIC  X(6).                  CL**1
00333                  24  PI-SAV-FIN-RESP     PIC  X(10).                 CL**1
00334                  24  PI-SAV-ACCOUNT      PIC  X(10).                 CL**1
00335                  24  PI-SAV-FILE-SEQ-NO  PIC S9(8)          COMP.    CL**1
00336                  24  PI-SAV-RECORD-TYPE  PIC  X.                     CL**1
00337          16  PI-SAV-ACCT-AMT         PIC  S9(7)V99.                  CL**1
00338          16  PI-SAV-ACCT-NET         PIC  S9(7)V99.                  CL**1
00339          16  PI-TOTAL-DISPLAYED-SW   PIC  X.                         CL**1
00340              88  PI-TOTAL-DISPLAYED          VALUE 'Y'.              CL**1
00341          16  PI-SPECIAL-GROUPING-SW  PIC X.                          CL**1
00342              88  PI-SPECIAL-GROUPING         VALUE 'G'.              CL**1
00343          16  FILLER                  PIC X(60).                      CL**1
00344          16  PI-PYAJ-FILLED-SW       PIC X.                          CL**1
00345             88  REFERENCE-DISPLAYED  VALUE 'Y'.                      CL**1
00346          16  PI-PYAJ-REFERENCE       PIC X(12).                      CL**1
00347          16  PI-ACCEPT               PIC XXX.                        CL**1
00348          16  FILLER                  PIC X(421).                     CL**1
00349                                                                      CL**1
00350  EJECT                                                               CL**1
00351      COPY ELCJPFX.                                                   CL**1
00352                              PIC  X(223).                            CL**1
00353  EJECT                                                               CL**1
00354      COPY ELCAID.                                                    CL**1
00355                                                                      CL**1
00356  01  FILLER  REDEFINES  DFHAID.                                      CL**1
00357      12  FILLER              PIC  X(8).                              CL**1
00358      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.         CL**1
00359  EJECT                                                               CL**1
00360      COPY EL635S.                                                    CL**1
00361                                                                      CL**1
00362  01  MAP-EL635A  REDEFINES  EL635AI.                                 CL**1
00363      12  FILLER                  PIC  X(178).                        CL**1
00364      12  DATA-AREA       OCCURS  3 TIMES                             CL**1
00365                              INDEXED BY PINDX.                       CL**1
00366          16  COMM-LEN            PIC S9(4)              COMP.        CL**1
00367          16  COMM-ATTRB          PIC  X.                             CL**1
00368          16  COMM                PIC  X(30).                         CL**1
00369          16  NCL-COMM  REDEFINES  COMM.                              CL**1
00370              20  NCL-COMM-DTE.                                       CL**1
00371                  24  NCL-MO      PIC  XX.                            CL**1
00372                  24  NCL-DA      PIC  XX.                            CL**1
00373                  24  NCL-YR      PIC  XX.                            CL**1
00374              20  NCL-COMM-REST   PIC  X(24).                         CL**1
00375          16  RTYPE-LEN           PIC S9(4)              COMP.        CL**1
00376          16  RTYPE-ATTRB         PIC  X.                             CL**1
00377          16  RTYPE               PIC  X.                             CL**1
00378          16  AMT-LEN             PIC S9(4)              COMP.        CL**1
00379          16  AMT-ATTRB           PIC  X.                             CL**1
00380          16  AMT                 PIC S9(9)V99.                       CL**1
00381          16  AMTO  REDEFINES                                         CL**1
00382              AMT                 PIC Z(7).99-.                       CL**1
00383          16  APPLIED-LEN         PIC S9(4)              COMP.        CL**1
00384          16  APPLIED-ATTRB       PIC  X.                             CL**1
00385          16  APPLIED             PIC  X.                             CL**1
00386          16  VOID-SW-LEN         PIC S9(4)              COMP.        CL**1
00387          16  VOID-SW-ATTRB       PIC  X.                             CL**1
00388          16  VOID-SW             PIC  X.                             CL**1
00389          16  MDTE-LEN            PIC S9(4)              COMP.        CL**1
00390          16  MDTE-ATTRB          PIC  X.                             CL**1
00391          16  MDTE                PIC  9(6).                          CL**1
00392          16  BDTE-LEN            PIC S9(4)              COMP.        CL**1
00393          16  BDTE-ATTRB          PIC  X.                             CL**1
00394          16  BDTE                PIC  9(6).                          CL**1
00395          16  SDTE-LEN            PIC S9(4)              COMP.        CL**1
00396          16  SDTE-ATTRB          PIC  X.                             CL**1
00397          16  SDTE                PIC  9(6).                          CL**1
00398          16  REF-LEN             PIC S9(4)              COMP.        CL**1
00399          16  REF-ATTRB           PIC  X.                             CL**1
00400          16  REF                 PIC  X(12).                         CL**1
00401          16  INVOICE-LEN         PIC S9(4)              COMP.        CL**1
00402          16  INVOICE-ATTRB       PIC  X.                             CL**1
00403          16  INVOICE             PIC  X(6).                          CL**1
00404          16  NET-LEN             PIC S9(4)              COMP.        CL**1
00405          16  NET-ATTRB           PIC  X.                             CL**1
00406          16  NET                 PIC S9(9)V99.                       CL**1
00407          16  NETO  REDEFINES                                         CL**1
00408              NET                 PIC Z(7).99-.                       CL**1
00409          16  IDTE-LEN            PIC S9(4)              COMP.        CL**1
00410          16  IDTE-ATTRB          PIC  X.                             CL**1
00411          16  IDTE                PIC  9(6).                          CL**1
00412          16  CREDIT-LEN          PIC S9(4)              COMP.        CL**1
00413          16  CREDIT-ATTRB        PIC  X.                             CL**1
00414          16  CREDIT              PIC  X(14).                         CL**1
00415          16  DEBIT-LEN           PIC S9(4)              COMP.        CL**1
00416          16  DEBIT-ATTRB         PIC  X.                             CL**1
00417          16  DEBIT               PIC  X(14).                         CL**1
00418  EJECT                                                               CL**1
00419  LINKAGE SECTION.                                                    CL**1
00420  01  DFHCOMMAREA             PIC  X(1024).                           CL**1
00421  EJECT                                                               CL**1
00422 *01 PARMLIST       COMP.                                             CL**1
00423 *    12  FILLER              PIC S9(8).                              CL**1
00424 *    12  ERPYAJ-POINTER      PIC S9(8).                              CL**1
00425 *    12  ERCHKQ-POINTER      PIC S9(8).                              CL**1
00426 *    12  ERCOMP-POINTER      PIC S9(8).                              CL**1
00427 *    12  ERRECV-POINTER      PIC S9(8).                              CL**1
00428 *    12  MPPRCN-POINTER      PIC S9(8).                              CL**1
00429                                                                      CL**1
00430  EJECT                                                               CL**1
00431      COPY ERCPYAJ.                                                   CL**1
00432  EJECT                                                               CL**1
00433      COPY ERCCHKQ.                                                   CL**1
00434  EJECT                                                               CL**1
00435      COPY ERCCOMP.                                                   CL**1
00436  EJECT                                                               CL**1
00437      COPY ERCRECV.                                                   CL**1
00438  EJECT                                                               CL**1
00439      COPY MPCPRCN.                                                   CL**1
00440  EJECT                                                               CL**1
00441  PROCEDURE DIVISION.                                                 CL**1
00442                                                                      CL**1
00443      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.        CL**1
00444      MOVE EIBTRMID               TO  QID-TERM.                       CL**1
00445      MOVE 2                      TO  EMI-NUMBER-OF-LINES.            CL**1
00446                                                                      CL**1
00447      IF EIBCALEN = ZERO                                              CL**1
00448          GO TO 8800-UNAUTHORIZED-ACCESS.                             CL**1
00449                                                                      CL**1
00450      IF PI-RETURN-TO-PROGRAM = THIS-PGM                              CL**1
00451          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.                   CL**1
00452                                                                      CL**1
00453      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.                CL**1
00454      MOVE '5'                    TO  DC-OPTION-CODE.                 CL**1
00455                                                                      CL**1
00456      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                     CL**1
00457                                                                      CL**1
00458      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.              CL**1
00459      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.                  CL**1
00460      MOVE DC-GREG-DATE-1-MDY     TO  WS-CURRENT-MDY.                 CL**1
00461                                                                      CL**1
00462      IF MORTGAGE-SESSION                                             CL**1
00463         MOVE XCTL-EM626           TO XCTL-626                        CL**1
00464         MOVE MPPYAJ-FILE-ID       TO PYAJ-FILE-ID.                   CL**1
00465                                                                      CL**1
00466      IF PI-CALLING-PROGRAM NOT = THIS-PGM                            CL**1
00467          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                      CL**1
00468              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6       CL**1
00469              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5       CL**1
00470              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4       CL**1
00471              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3       CL**1
00472              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2       CL**1
00473              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1       CL**1
00474              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM     CL**1
00475              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM       CL**1
00476              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT         CL**1
00477          ELSE                                                        CL**1
00478              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM       CL**1
00479              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM     CL**1
00480              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1       CL**1
00481              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2       CL**1
00482              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3       CL**1
00483              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4       CL**1
00484              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5       CL**1
00485              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.      CL**1
00486                                                                      CL**1
00487      MOVE LOW-VALUES             TO  EL635AI.                        CL**1
00488                                                                      CL**1
00489      COMPUTE WORK-SEQ-NO  =  EIBTIME  *  10.                         CL**1
00490                                                                      CL**1
00491      IF RETURNED-FROM EQUAL XCTL-6351 OR                             CL**1
00492                             XCTL-640  OR                             CL**1
00493                             XCTL-642  OR                             CL**1
00494                             XCTL-652                                 CL**1
00495          PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0690-EXIT.           CL**1
00496                                                                      CL**1
00497      IF EIBTRNID EQUAL EL630-TRANS-ID                                CL**1
00498          MOVE 'T'                TO  PI-PYAJ-FILE-SW                 CL**1
00499          MOVE 'Y'                TO  EL630-SENT-SW                   CL**1
00500          MOVE ZEROS              TO  PI-SEQ-NOS                      CL**1
00501          IF ((PI-CR-CONTROL-IN-PROGRESS = SPACES) OR                 CL**1
00502              (PI-CR-FIN-RESP EQUAL 'UNKNOWN  '))                     CL**1
00503              MOVE SPACES         TO PI-CR-CONTROL-IN-PROGRESS        CL**1
00504              MOVE SPACES         TO PI-SAV-CARRIER                   CL**1
00505                                     PI-SAV-GROUPING                  CL**1
00506                                     PI-SAV-FIN-RESP                  CL**1
00507                                     PI-SAV-ACCOUNT                   CL**1
00508                                     PI-PYAJ-REFERENCE                CL**1
00509                                     PI-PYAJ-FILLED-SW                CL**1
00510              GO TO 8100-SEND-INITIAL-MAP                             CL**1
00511          ELSE                                                        CL**1
00512              MOVE DFHENTER   TO  EIBAID                              CL**1
00513              MOVE 'S'        TO  MAINTI                              CL**1
00514              MOVE PI-CR-CARRIER                                      CL**1
00515                              TO  CARRIERI                            CL**1
00516              MOVE PI-CR-GROUPING                                     CL**1
00517                              TO  GROUPI                              CL**1
00518              MOVE PI-CR-FIN-RESP                                     CL**1
00519                              TO  FINRESPI                            CL**1
00520              MOVE PI-CR-ACCOUNT                                      CL**1
00521                              TO  ACCTI                               CL**1
00522              MOVE 1          TO  CARRIERL  MAINTA                    CL**1
00523              MOVE 3          TO  GROUPL                              CL**1
00524              MOVE 6          TO  FINRESPL  ACCTL                     CL**1
00525              GO TO 0400-VALIDATE-KEY-DATA.                           CL**1
00526                                                                      CL**1
00527      IF EIBTRNID NOT = TRANS-ID                                      CL**1
00528          MOVE 'T'                TO  PI-PYAJ-FILE-SW                 CL**1
00529          MOVE ZEROS              TO  PI-SEQ-NOS                      CL**1
00530          IF (EIBTRNID NOT = EL640-TRANS-ID  AND                      CL**1
00531              EIBTRNID NOT = EL642-TRANS-ID  AND                      CL**1
00532              EIBTRNID NOT = EL652-TRANS-ID  AND                      CL**1
00533              EIBTRNID NOT = EL6351-TRANS-ID)                         CL**1
00534              GO TO 8100-SEND-INITIAL-MAP                             CL**1
00535          ELSE                                                        CL**1
00536              IF (EIBTRNID = EL6351-TRANS-ID  OR                      CL**1
00537                            EL640-TRANS-ID   OR                       CL**1
00538                            EL642-TRANS-ID)  AND                      CL**1
00539                 PI-CR-CONTROL-IN-PROGRESS = SPACES                   CL**1
00540                 GO TO 8100-SEND-INITIAL-MAP                          CL**1
00541              ELSE                                                    CL**1
00542                  MOVE DFHENTER   TO  EIBAID                          CL**1
00543                  MOVE 'S'        TO  MAINTI                          CL**1
00544                  MOVE PI-CR-CARRIER                                  CL**1
00545                                  TO  CARRIERI                        CL**1
00546                  MOVE PI-CR-GROUPING                                 CL**1
00547                                  TO  GROUPI                          CL**1
00548                  MOVE PI-CR-FIN-RESP                                 CL**1
00549                                  TO  FINRESPI                        CL**1
00550                  MOVE PI-CR-ACCOUNT                                  CL**1
00551                                  TO  ACCTI                           CL**1
00552                  MOVE 1          TO  CARRIERL  MAINTA                CL**1
00553                  MOVE 3              TO  GROUPL                      CL**1
00554                  MOVE 6              TO  FINRESPL  ACCTL             CL**1
00555                  GO TO 0400-VALIDATE-KEY-DATA.                       CL**1
00556                                                                      CL**1
00557      EXEC CICS HANDLE CONDITION                                      CL**1
00558          PGMIDERR  (9600-PGMID-ERROR)                                CL**1
00559          ERROR     (9990-ABEND)                                      CL**1
00560      END-EXEC.                                                       CL**1
00561                                                                      CL**1
00562      IF  EIBAID = DFHCLEAR                                           CL**1
00563              OR                                                      CL**1
00564          NOT DISPLAY-CAP                                             CL**1
00565          GO TO 9400-CLEAR.                                           CL**1
00566                                                                      CL**1
00567  EJECT                                                               CL**1
00568  0200-RECEIVE.                                                       CL**1
00569      IF EIBAID = DFHPA1  OR  DFHPA2  OR  DFHPA3                      CL**1
00570          MOVE ER-0008            TO  EMI-ERROR                       CL**1
00571          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**1
00572          MOVE -1                 TO  MAINTL                          CL**1
00573          GO TO 8200-SEND-DATAONLY.                                   CL**1
00574                                                                      CL**1
00575      EXEC CICS RECEIVE                                               CL**1
00576          MAP     (MAP-NAME)                                          CL**1
00577          MAPSET  (MAPSET-NAME)                                       CL**1
00578          INTO    (EL635AI)                                           CL**1
00579      END-EXEC.                                                       CL**1
00580                                                                      CL**1
00581      IF PFENTERL = ZERO                                              CL**1
00582          GO TO 0300-CHECK-PFKEYS.                                    CL**1
00583                                                                      CL**1
00584      IF (PFENTERI  IS NUMERIC)                                       CL**1
00585        AND (PFENTERI GREATER 0 AND LESS 25)                          CL**1
00586          MOVE PF-VALUES (PFENTERI)  TO  EIBAID                       CL**1
00587      ELSE                                                            CL**1
00588          MOVE ER-0029               TO  EMI-ERROR                    CL**1
00589          GO TO 0320-INPUT-ERROR.                                     CL**1
00590                                                                      CL**1
00591  0300-CHECK-PFKEYS.                                                  CL**1
00592      IF EIBAID = DFHPF23                                             CL**1
00593          GO TO 8810-PF23.                                            CL**1
00594                                                                      CL**1
00595      IF EIBAID = DFHPF24                                             CL**1
00596          GO TO 9200-RETURN-MAIN-MENU.                                CL**1
00597                                                                      CL**1
00598      IF EIBAID = DFHPF12                                             CL**1
00599          GO TO 9500-PF12.                                            CL**1
00600                                                                      CL**1
00601      IF EIBAID = DFHPF3                                              CL**1
00602          IF PI-CR-CONTROL-IN-PROGRESS NOT = SPACES                   CL**1
00603              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT         CL**1
00604              MOVE XCTL-652       TO  PGM-NAME                        CL**1
00605             IF PI-CR-ACCOUNT = LOW-VALUES                            CL**1
00606                MOVE 'G'          TO  PI-CR-TYPE                      CL**1
00607                GO TO 9300-XCTL                                       CL**1
00608             ELSE                                                     CL**1
00609                MOVE 'A'          TO  PI-CR-TYPE                      CL**1
00610                GO TO 9300-XCTL                                       CL**1
00611          ELSE                                                        CL**1
00612             MOVE ER-3020         TO  EMI-ERROR                       CL**1
00613             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               CL**1
00614             GO TO 8100-SEND-INITIAL-MAP.                             CL**1
00615                                                                      CL**1
00616      IF EIBAID = DFHPF4                                              CL**1
00617          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT             CL**1
00618          MOVE XCTL-6351          TO  PGM-NAME                        CL**1
00619          GO TO 9300-XCTL.                                            CL**1
00620                                                                      CL**1
00621      IF CREDIT-SESSION  AND EIBAID = DFHPF6                          CL**1
00622          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT             CL**1
00623          MOVE SPACES TO PI-PROGRAM-WORK-AREA                         CL**1
00624          MOVE XCTL-640           TO  PGM-NAME                        CL**1
00625          IF PI-CR-ACCOUNT = LOW-VALUES                               CL**1
00626              MOVE SPACES       TO  PI-CR-CONTROL-IN-PROGRESS         CL**1
00627              GO TO 9300-XCTL                                         CL**1
00628          ELSE                                                        CL**1
00629              MOVE 'A'          TO  PI-CR-TYPE                        CL**1
00630              GO TO 9300-XCTL.                                        CL**1
00631                                                                      CL**1
00632      IF CREDIT-SESSION  AND EIBAID = DFHPF7                          CL**1
00633         IF PI-GA-BILLING                                             CL**1
00634             PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT          CL**1
00635             MOVE SPACES TO PI-PROGRAM-WORK-AREA                      CL**1
00636             MOVE XCTL-642        TO  PGM-NAME                        CL**1
00637             IF PI-CR-ACCOUNT = LOW-VALUES                            CL**1
00638                 MOVE 'G'          TO  PI-CR-TYPE                     CL**1
00639                 GO TO 9300-XCTL                                      CL**1
00640             ELSE                                                     CL**1
00641                 MOVE SPACES      TO  PI-CR-CONTROL-IN-PROGRESS       CL**1
00642                 GO TO 9300-XCTL                                      CL**1
00643         ELSE                                                         CL**1
00644             MOVE ER-2929         TO  EMI-ERROR                       CL**1
00645             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               CL**1
00646             MOVE AL-UNBON        TO  PFENTERA                        CL**1
00647             IF PFENTERL = ZERO                                       CL**1
00648                 MOVE -1          TO  MAINTL                          CL**1
00649                 GO TO 8200-SEND-DATAONLY                             CL**1
00650             ELSE                                                     CL**1
00651                 MOVE -1          TO  PFENTERL                        CL**1
00652                 GO TO 8200-SEND-DATAONLY.                            CL**1
00653                                                                      CL**1
00654      IF EIBAID = DFHPF1  OR  DFHPF2                                  CL**1
00655          GO TO 0400-VALIDATE-KEY-DATA.                               CL**1
00656                                                                      CL**1
00657      IF EIBAID = DFHENTER                                            CL**1
00658          MOVE SPACES             TO WS-ACCEPT-1 (W-INDX)             CL**1
00659          MOVE SPACES             TO WS-PREV-PF5                      CL**1
00660          GO TO 0400-VALIDATE-KEY-DATA.                               CL**1
00661                                                                      CL**1
00662      IF EIBAID = DFHPF5                                              CL**1
00663          MOVE PI-ACCEPT          TO WS-ACCEPT-TABLE                  CL**1
00664          GO TO 0400-VALIDATE-KEY-DATA.                               CL**1
00665                                                                      CL**1
00666      IF EIBAID = DFHPF8                                              CL**1
00667          GO TO 0400-VALIDATE-KEY-DATA.                               CL**1
00668                                                                      CL**1
00669  0320-INPUT-ERROR.                                                   CL**1
00670      MOVE ER-0029                TO  EMI-ERROR.                      CL**1
00671                                                                      CL**1
00672      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
00673                                                                      CL**1
00674      MOVE AL-UNBON               TO  PFENTERA.                       CL**1
00675                                                                      CL**1
00676      IF PFENTERL = ZERO                                              CL**1
00677          MOVE -1                 TO  MAINTL                          CL**1
00678      ELSE                                                            CL**1
00679          MOVE -1                 TO  PFENTERL.                       CL**1
00680                                                                      CL**1
00681      GO TO 8200-SEND-DATAONLY.                                       CL**1
00682  EJECT                                                               CL**1
00683  0400-VALIDATE-KEY-DATA.                                             CL**1
00684      IF MODIFY-CAP  OR (EIBAID = DFHPF1 OR DFHPF2)                   CL**1
00685          NEXT SENTENCE                                               CL**1
00686        ELSE                                                          CL**1
00687         IF MAINTI NOT = 'S'                                          CL**1
00688          MOVE 'UPDATE'           TO SM-READ                          CL**1
00689          PERFORM 9995-SECURITY-VIOLATION                             CL**1
00690          IF  MORTGAGE-SESSION                                        CL**1
00691              MOVE ER-9096        TO EMI-ERROR                        CL**1
00692              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**1
00693              GO TO 8100-SEND-INITIAL-MAP                             CL**1
00694          ELSE                                                        CL**1
00695              MOVE ER-0070        TO EMI-ERROR                        CL**1
00696              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**1
00697              GO TO 8100-SEND-INITIAL-MAP.                            CL**1
00698                                                                      CL**1
00699      MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD.                 CL**1
00700                                                                      CL**1
00701      IF EIBAID   = DFHPF1                                            CL**1
00702         GO TO 4000-BROWSE-FRWD.                                      CL**1
00703                                                                      CL**1
00704      IF EIBAID   = DFHPF1   AND                                      CL**1
00705         CARRIERL = ZEROS    AND                                      CL**1
00706         GROUPL   = ZEROS    AND                                      CL**1
00707         FINRESPL = ZEROS    AND                                      CL**1
00708         ACCTL    = ZEROS                                             CL**1
00709          GO TO 4000-BROWSE-FRWD.                                     CL**1
00710                                                                      CL**1
00711      IF EIBAID   = DFHPF8                                            CL**1
00712         GO TO 1010-COMP-CHECK.                                       CL**1
00713                                                                      CL**1
00714      IF MAINTI = 'C' OR  'S'                                         CL**1
00715          MOVE AL-UANON           TO  MAINTA                          CL**1
00716          MOVE MAINTI             TO  PI-SAV-FUNCTION                 CL**1
00717      ELSE                                                            CL**1
00718          MOVE -1                 TO  MAINTL                          CL**1
00719          MOVE ER-0023            TO  EMI-ERROR                       CL**1
00720          MOVE AL-UABON           TO  MAINTA                          CL**1
00721          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                 CL**1
00722                                                                      CL**1
00723      IF MAINTI = 'C'                                                 CL**1
00724        AND PI-PREV-FUNCTION NOT = 'S'                                CL**1
00725          MOVE -1                 TO  MAINTL                          CL**1
00726          MOVE ER-2056            TO  EMI-ERROR                       CL**1
00727          MOVE AL-UABON           TO  MAINTA                          CL**1
00728          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                 CL**1
00729                                                                      CL**1
00730      IF MORTGAGE-SESSION                                             CL**1
00731         IF PI-SPECIAL-GROUPING                                       CL**1
00732            IF CARRIERL       GREATER THAN ZEROS                      CL**1
00733               OR GROUPL      GREATER THAN ZEROS                      CL**1
00734               OR FINRESPL    GREATER THAN ZEROS                      CL**1
00735               OR ACCTL       GREATER THAN ZEROS                      CL**1
00736                  MOVE ER-9179    TO EMI-ERROR                        CL**1
00737                  MOVE -1         TO CARRIERL                         CL**1
00738                  MOVE AL-UABON   TO CARRIERA                         CL**1
00739                                     GROUPA                           CL**1
00740                                     FINRESPA                         CL**1
00741                                     ACCTA                            CL**1
00742                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**1
00743                  GO TO 0410-CONT-EDIT                                CL**1
00744            ELSE                                                      CL**1
00745               GO TO 0410-CONT-EDIT                                   CL**1
00746         ELSE                                                         CL**1
00747            NEXT SENTENCE.                                            CL**1
00748                                                                      CL**1
00749      IF CARRIERL = ZEROS  AND                                        CL**1
00750         GROUPL   = ZEROS  AND                                        CL**1
00751         FINRESPL = ZEROS  AND                                        CL**1
00752         ACCTL    = ZEROS                                             CL**1
00753          MOVE -1                 TO  CARRIERL                        CL**1
00754          MOVE ER-2231            TO  EMI-ERROR                       CL**1
00755          MOVE AL-UABOF           TO  CARRIERA                        CL**1
00756                                      GROUPA                          CL**1
00757                                      FINRESPA                        CL**1
00758                                      ACCTA                           CL**1
00759          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**1
00760          GO TO 8200-SEND-DATAONLY.                                   CL**1
00761                                                                      CL**1
00762      IF CARRIERL NOT = ZEROS                                         CL**1
00763          MOVE AL-UANON           TO  CARRIERA                        CL**1
00764          MOVE CARRIERI           TO  COMP-CARRIER                    CL**1
00765                                      PI-SAV-CARRIER                  CL**1
00766                                      PI-CR-CARRIER                   CL**1
00767          IF CARRIERI NOT = ZEROS                                     CL**1
00768            AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)              CL**1
00769              MOVE ER-2587        TO  EMI-ERROR                       CL**1
00770              MOVE -1             TO  CARRIERL                        CL**1
00771              MOVE AL-UABON       TO  CARRIERA                        CL**1
00772              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**1
00773          ELSE                                                        CL**1
00774              NEXT SENTENCE                                           CL**1
00775      ELSE                                                            CL**1
00776          MOVE LOW-VALUES          TO  COMP-CARRIER                   CL**1
00777                                       PI-SAV-CARRIER.                CL**1
00778                                                                      CL**1
00779      IF CARRIERL NOT = ZEROS                                         CL**1
00780             AND                                                      CL**1
00781         CARRIERI NOT = SPACES                                        CL**1
00782          IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES             CL**1
00783              IF PI-CARRIER-SECURITY = CARRIERI                       CL**1
00784                  NEXT SENTENCE                                       CL**1
00785              ELSE                                                    CL**1
00786                  MOVE -1         TO  CARRIERL                        CL**1
00787                  MOVE ER-2370    TO  EMI-ERROR                       CL**1
00788                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.         CL**1
00789                                                                      CL**1
00790      IF GROUPL NOT = ZEROS                                           CL**1
00791          MOVE AL-UANON           TO  GROUPA                          CL**1
00792          MOVE GROUPI             TO  COMP-GROUPING                   CL**1
00793                                      PI-SAV-GROUPING                 CL**1
00794                                      PI-CR-GROUPING                  CL**1
00795      ELSE                                                            CL**1
00796          MOVE LOW-VALUES         TO  COMP-GROUPING                   CL**1
00797                                      PI-SAV-GROUPING.                CL**1
00798                                                                      CL**1
00799      IF FINRESPL NOT = ZEROS                                         CL**1
00800          MOVE AL-UANON           TO  FINRESPA                        CL**1
00801          MOVE FINRESPI           TO  COMP-FIN-RESP                   CL**1
00802                                      PI-SAV-FIN-RESP                 CL**1
00803                                      PI-CR-FIN-RESP                  CL**1
00804      ELSE                                                            CL**1
00805          MOVE LOW-VALUES         TO  COMP-FIN-RESP                   CL**1
00806                                      PI-SAV-FIN-RESP.                CL**1
00807                                                                      CL**1
00808      IF ACCTL NOT = ZEROS                                            CL**1
00809          MOVE AL-UANON           TO  ACCTA                           CL**1
00810          MOVE ACCTI              TO  COMP-ACCOUNT                    CL**1
00811                                      PI-SAV-ACCOUNT                  CL**1
00812                                      PI-CR-ACCOUNT                   CL**1
00813      ELSE                                                            CL**1
00814          MOVE LOW-VALUES         TO  COMP-ACCOUNT                    CL**1
00815                                      PI-SAV-ACCOUNT                  CL**1
00816                                      PI-CR-ACCOUNT.                  CL**1
00817                                                                      CL**1
00818  0410-CONT-EDIT.                                                     CL**1
00819                                                                      CL**1
00820      IF EMI-ERROR NOT = ZEROS                                        CL**1
00821          GO TO 8200-SEND-DATAONLY.                                   CL**1
00822                                                                      CL**1
00823      MOVE MAINTI                 TO  PI-PREV-FUNCTION.               CL**1
00824                                                                      CL**1
00825      IF MAINTI = 'S'                                                 CL**1
00826          IF EIBAID = DFHPF1  OR  DFHENTER                            CL**1
00827              GO TO 4000-BROWSE-FRWD                                  CL**1
00828          ELSE                                                        CL**1
00829              GO TO 4100-BROWSE-BKWD.                                 CL**1
00830                                                                      CL**1
00831      IF PI-SPECIAL-GROUPING                                          CL**1
00832         GO TO 0410-SKIP-COMP-EDIT.                                   CL**1
00833                                                                      CL**1
00834      IF PI-APPLIED (1) = 'N' OR                                      CL**1
00835         PI-APPLIED (2) = 'N' OR                                      CL**1
00836         PI-APPLIED (3) = 'N'                                         CL**1
00837          GO TO 0410-SKIP-COMP-EDIT.                                  CL**1
00838                                                                      CL**1
00839      MOVE SPACE                  TO  OVERWRITE-AGENT-SW.             CL**1
00840      MOVE PI-COMPANY-CD          TO  COMP-COMP-CD.                   CL**1
00841      MOVE '1'                    TO  WS-SECOND-READ.                 CL**1
00842                                                                      CL**1
00843      IF ACCTI = LOW-VALUES                                           CL**1
00844          GO TO 0405-VERIFY-G-ONLY.                                   CL**1
00845                                                                      CL**1
00846      IF ACCTI NOT = FINRESPI                                         CL**1
00847          GO TO 0410-SKIP-COMP-EDIT.                                  CL**1
00848                                                                      CL**1
00849  0405-VERIFY-A-COMP.                                                 CL**1
00850                                                                      CL**1
00851      MOVE 'A'                TO  COMP-RECORD-TYPE.                   CL**1
00852                                                                      CL**1
00853      EXEC CICS HANDLE CONDITION                                      CL**1
00854          NOTFND   (0410-NO-A-COMP)                                   CL**1
00855          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL**1
00856          END-EXEC.                                                   CL**1
00857                                                                      CL**1
00858      EXEC CICS READ                                                  CL**1
00859          DATASET  (COMP-FILE-ID)                                     CL**1
00860          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL**1
00861          RIDFLD   (ERCOMP-KEY)                                       CL**1
00862          EQUAL                                                       CL**1
00863          END-EXEC.                                                   CL**1
00864                                                                      CL**1
00865      IF COMP-FIN-RESP NOT = COMP-ACCOUNT                             CL**1
00866          NEXT SENTENCE                                               CL**1
00867      ELSE                                                            CL**1
00868          GO TO 0410-SKIP-COMP-EDIT.                                  CL**1
00869                                                                      CL**1
00870      MOVE LOW-VALUES         TO  COMP-ACCOUNT.                       CL**1
00871      MOVE 'G'                TO  COMP-RECORD-TYPE.                   CL**1
00872                                                                      CL**1
00873      EXEC CICS HANDLE CONDITION                                      CL**1
00874          NOTFND   (0410-NO-G-FOR-A)                                  CL**1
00875          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL**1
00876          END-EXEC.                                                   CL**1
00877                                                                      CL**1
00878      EXEC CICS READ                                                  CL**1
00879          DATASET  (COMP-FILE-ID)                                     CL**1
00880          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL**1
00881          RIDFLD   (ERCOMP-KEY)                                       CL**1
00882          EQUAL                                                       CL**1
00883          END-EXEC.                                                   CL**1
00884                                                                      CL**1
00885      GO TO 0410-SKIP-COMP-EDIT.                                      CL**1
00886                                                                      CL**1
00887  0410-NO-A-COMP.                                                     CL**1
00888                                                                      CL**1
00889      MOVE -1                     TO  CARRIERL.                       CL**1
00890      MOVE AL-UABON               TO  CARRIERA                        CL**1
00891                                      GROUPA                          CL**1
00892                                      FINRESPA                        CL**1
00893                                      ACCTA.                          CL**1
00894                                                                      CL**1
00895      MOVE ER-3178                TO  EMI-ERROR                       CL**1
00896                                                                      CL**1
00897      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
00898                                                                      CL**1
00899      GO TO 8200-SEND-DATAONLY.                                       CL**1
00900                                                                      CL**1
00901  0410-NO-G-FOR-A.                                                    CL**1
00902                                                                      CL**1
00903      MOVE -1                     TO  CARRIERL.                       CL**1
00904      MOVE AL-UABON               TO  CARRIERA                        CL**1
00905                                      GROUPA                          CL**1
00906                                      FINRESPA                        CL**1
00907                                      ACCTA.                          CL**1
00908                                                                      CL**1
00909      MOVE ER-3193                TO  EMI-ERROR                       CL**1
00910                                                                      CL**1
00911      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
00912                                                                      CL**1
00913      GO TO 8200-SEND-DATAONLY.                                       CL**1
00914                                                                      CL**1
00915  0405-VERIFY-G-ONLY.                                                 CL**1
00916                                                                      CL**1
00917      MOVE 'G'                TO  COMP-RECORD-TYPE.                   CL**1
00918                                                                      CL**1
00919      EXEC CICS HANDLE CONDITION                                      CL**1
00920          NOTFND   (0410-NO-G-COMP)                                   CL**1
00921          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL**1
00922          END-EXEC.                                                   CL**1
00923                                                                      CL**1
00924      EXEC CICS READ                                                  CL**1
00925          DATASET  (COMP-FILE-ID)                                     CL**1
00926          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL**1
00927          RIDFLD   (ERCOMP-KEY)                                       CL**1
00928          EQUAL                                                       CL**1
00929          END-EXEC.                                                   CL**1
00930                                                                      CL**1
00931      GO TO 0410-SKIP-COMP-EDIT.                                      CL**1
00932                                                                      CL**1
00933  0410-NO-G-COMP.                                                     CL**1
00934                                                                      CL**1
00935      MOVE -1                     TO  CARRIERL.                       CL**1
00936      MOVE AL-UABON               TO  CARRIERA                        CL**1
00937                                      GROUPA                          CL**1
00938                                      FINRESPA                        CL**1
00939                                      ACCTA.                          CL**1
00940                                                                      CL**1
00941      MOVE ER-3179                TO  EMI-ERROR.                      CL**1
00942                                                                      CL**1
00943      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
00944                                                                      CL**1
00945      GO TO 8200-SEND-DATAONLY.                                       CL**1
00946                                                                      CL**1
00947  0410-SKIP-COMP-EDIT.                                                CL**1
00948                                                                      CL**1
00949      IF MAINTI = 'C'                                                 CL**1
00950          GO TO 1000-EDIT-DATA.                                       CL**1
00951                                                                      CL**1
00952      IF EIBAID = DFHPF1                                              CL**1
00953        OR DFHENTER                                                   CL**1
00954          GO TO 4000-BROWSE-FRWD.                                     CL**1
00955                                                                      CL**1
00956      IF EIBAID = DFHPF2                                              CL**1
00957          GO TO 4100-BROWSE-BKWD.                                     CL**1
00958                                                                      CL**1
00959      EJECT                                                           CL**1
00960  0500-CREATE-TEMP-STORAGE.                                           CL**1
00961                                                                      CL**1
00962      PERFORM 0800-DELETE-TS  THRU  0890-EXIT.                        CL**1
00963                                                                      CL**1
00964      EXEC CICS WRITEQ TS                                             CL**1
00965          QUEUE  (QID)                                                CL**1
00966          FROM   (PROGRAM-INTERFACE-BLOCK)                            CL**1
00967          LENGTH (PI-COMM-LENGTH)                                     CL**1
00968      END-EXEC.                                                       CL**1
00969                                                                      CL**1
00970  0590-EXIT.                                                          CL**1
00971       EXIT.                                                          CL**1
00972                                                                      CL**1
00973  0600-RECOVER-TEMP-STORAGE.                                          CL**1
00974      EXEC CICS READQ TS                                              CL**1
00975          QUEUE  (QID)                                                CL**1
00976          INTO   (PROGRAM-INTERFACE-BLOCK)                            CL**1
00977          LENGTH (PI-COMM-LENGTH)                                     CL**1
00978      END-EXEC.                                                       CL**1
00979                                                                      CL**1
00980      PERFORM 0800-DELETE-TS THRU 0890-EXIT.                          CL**1
00981                                                                      CL**1
00982  0690-EXIT.                                                          CL**1
00983       EXIT.                                                          CL**1
00984                                                                      CL**1
00985  0800-DELETE-TS.                                                     CL**1
00986      EXEC CICS HANDLE CONDITION                                      CL**1
00987          QIDERR (0890-EXIT)                                          CL**1
00988      END-EXEC.                                                       CL**1
00989                                                                      CL**1
00990      EXEC CICS DELETEQ TS                                            CL**1
00991          QUEUE  (QID)                                                CL**1
00992      END-EXEC.                                                       CL**1
00993                                                                      CL**1
00994      EXEC CICS SYNCPOINT                                             CL**1
00995      END-EXEC.                                                       CL**1
00996                                                                      CL**1
00997  0890-EXIT.                                                          CL**1
00998       EXIT.                                                          CL**1
00999      EJECT                                                           CL**1
01000                                                                      CL**1
01001  1000-EDIT-DATA.                                                     CL**1
01002                                                                      CL**1
01003      IF NOT MODIFY-CAP                                               CL**1
01004          MOVE 'UPDATE'       TO SM-READ                              CL**1
01005          PERFORM 9995-SECURITY-VIOLATION                             CL**1
01006          MOVE ER-0070        TO EMI-ERROR                            CL**1
01007          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**1
01008          GO TO 8100-SEND-INITIAL-MAP.                                CL**1
01009                                                                      CL**1
01010      SET PINDX                   TO  1.                              CL**1
01011      SET W-INDX                  TO  1.                              CL**1
01012                                                                      CL**1
01013 ****** IF THE REFERENCE NUMBER HAS BEEN FILLED IN AUTOMATICALLY      CL**1
01014 ****** FROM THE PI-AREA OF EL630,  THE ATTRIBUTE WAS ERASED          CL**1
01015 ****** WHEN THE SCREEN WAS DISPLAYED IN 8100-SEND-INITIAL-MAP.       CL**1
01016 ****** UNLESS THE CLIENT TYPES OVER THE REFERENCE NUMBER,            CL**1
01017 ****** THE PROGRAM IS NOT AWARE THAT THE NUMBER IS DISPLAYED.        CL**1
01018                                                                      CL**1
01019      IF ((REF-LEN (PINDX) EQUAL ZEROS)                               CL**1
01020          AND (REFERENCE-DISPLAYED)                                   CL**1
01021          AND (PI-PYAJ-REFERENCE NOT EQUAL SPACES))                   CL**1
01022            MOVE PI-PYAJ-REFERENCE TO REF (PINDX)                     CL**1
01023            MOVE 12               TO REF-LEN (PINDX)                  CL**1
01024            MOVE AL-UANON         TO REF-ATTRB (PINDX)                CL**1
01025            MOVE SPACE            TO PI-PYAJ-FILLED-SW                CL**1
01026            MOVE SPACE            TO PI-PYAJ-REFERENCE.               CL**1
01027                                                                      CL**1
01028  1010-EDIT-LOOP.                                                     CL**1
01029                                                                      CL**1
01030      SET NDX                     TO  PINDX.                          CL**1
01031                                                                      CL**1
01032      SET WS-SAVE-INDEX-VALUE     TO  PINDX.                          CL**1
01033                                                                      CL**1
01034      IF COMM-LEN (PINDX)    = ZEROS                                  CL**1
01035        AND RTYPE-LEN (PINDX)   = ZEROS                               CL**1
01036        AND AMT-LEN (PINDX)     = ZEROS                               CL**1
01037        AND APPLIED-LEN (PINDX) = ZEROS                               CL**1
01038        AND VOID-SW-LEN (PINDX) = ZEROS                               CL**1
01039        AND SDTE-LEN (PINDX)    = ZEROS                               CL**1
01040        AND IDTE-LEN (PINDX)    = ZEROS                               CL**1
01041          GO TO 1020-CHECK-INVOICE.                                   CL**1
01042                                                                      CL**1
01043      IF EIBAID = DFHPF5                                              CL**1
01044          GO TO 1010-EDIT-PROCESS.                                    CL**1
01045                                                                      CL**1
01046      IF WS-PREV-PF5 = 'Y' AND                                        CL**1
01047          COMM-LEN (PINDX)    = ZEROS AND                             CL**1
01048          RTYPE-LEN (PINDX)   = ZEROS AND                             CL**1
01049          AMT-LEN (PINDX)     = ZEROS AND                             CL**1
01050          APPLIED-LEN (PINDX) = ZEROS AND                             CL**1
01051          VOID-SW-LEN (PINDX) = ZEROS AND                             CL**1
01052          SDTE-LEN (PINDX)    = ZEROS AND                             CL**1
01053          IDTE-LEN (PINDX)    = ZEROS                                 CL**1
01054          GO TO 1020-CHECK-INVOICE.                                   CL**1
01055                                                                      CL**1
01056  1010-EDIT-PROCESS.                                                  CL**1
01057                                                                      CL**1
01058      IF COMM-LEN (PINDX) NOT = ZEROS                                 CL**1
01059          MOVE AL-UANON           TO  COMM-ATTRB (PINDX)              CL**1
01060          IF PI-COMPANY-ID NOT = 'NCL'                                CL**1
01061              NEXT SENTENCE                                           CL**1
01062          ELSE                                                        CL**1
01063              IF NCL-COMM-DTE (PINDX) IS NUMERIC                      CL**1
01064                  MOVE NCL-COMM-DTE (PINDX)                           CL**1
01065                                  TO  DC-GREG-DATE-1-MDY              CL**1
01066                  MOVE '4'        TO  DC-OPTION-CODE                  CL**1
01067                  PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT          CL**1
01068                  IF NO-CONVERSION-ERROR                              CL**1
01069                      NEXT SENTENCE                                   CL**1
01070                  ELSE                                                CL**1
01071                      MOVE ER-2595                                    CL**1
01072                                  TO  EMI-ERROR                       CL**1
01073                      MOVE -1     TO  COMM-LEN (PINDX)                CL**1
01074                      MOVE AL-UABON                                   CL**1
01075                                  TO  COMM-ATTRB (PINDX)              CL**1
01076                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT      CL**1
01077              ELSE                                                    CL**1
01078                  IF NCL-COMM-DTE (PINDX) = SPACES OR LOW-VALUES      CL**1
01079                      MOVE WS-CURRENT-MDY                             CL**1
01080                                  TO  NCL-COMM-DTE (PINDX)            CL**1
01081                  ELSE                                                CL**1
01082                      MOVE COMM (PINDX)                               CL**1
01083                                  TO  WS-COMMENT-FULL                 CL**1
01084                      MOVE WS-COMMENT-24-POS                          CL**1
01085                                  TO  NCL-COMM-REST (PINDX)           CL**1
01086                      MOVE WS-CURRENT-MDY                             CL**1
01087                                  TO  NCL-COMM-DTE (PINDX)            CL**1
01088      ELSE                                                            CL**1
01089          IF PI-COMPANY-ID = 'NCL'                                    CL**1
01090            AND PI-FILE-SEQ-NO (NDX) = ZEROS                          CL**1
01091              MOVE +6             TO  COMM-LEN (PINDX)                CL**1
01092              MOVE WS-CURRENT-MDY TO  NCL-COMM-DTE (PINDX).           CL**1
01093                                                                      CL**1
01094      IF RTYPE-LEN (PINDX) NOT = ZEROS                                CL**1
01095          MOVE RTYPE (PINDX)      TO  CHECK-REC-TYPE                  CL**1
01096          IF MORTGAGE-SESSION                                         CL**1
01097             IF NOT VALID-MTG-REC-TYPE                                CL**1
01098                MOVE -1           TO  RTYPE-LEN (PINDX)               CL**1
01099                MOVE ER-2234      TO  EMI-ERROR                       CL**1
01100                MOVE AL-UABON     TO  RTYPE-ATTRB (PINDX)             CL**1
01101                PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT            CL**1
01102             ELSE                                                     CL**1
01103                MOVE AL-UANON     TO  RTYPE-ATTRB (PINDX)             CL**1
01104          ELSE                                                        CL**1
01105             IF PI-COMPANY-ID = 'MON'                                 CL**1
01106                 IF NOT MON-VALID-REC-TYPE                            CL**1
01107                    MOVE -1       TO  RTYPE-LEN (PINDX)               CL**1
01108                    MOVE ER-7806  TO  EMI-ERROR                       CL**1
01109                    MOVE AL-UABON TO  RTYPE-ATTRB (PINDX)             CL**1
01110                    PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT        CL**1
01111                 ELSE                                                 CL**1
01112                    MOVE AL-UANON TO  RTYPE-ATTRB (PINDX)             CL**1
01113             ELSE                                                     CL**1
01114                 IF PI-COMPANY-ID = 'NCL'                             CL**1
01115                     IF NOT NCL-VALID-REC-TYPE                        CL**1
01116                        MOVE -1   TO  RTYPE-LEN (PINDX)               CL**1
01117                        MOVE ER-7806                                  CL**1
01118                                  TO  EMI-ERROR                       CL**1
01119                        MOVE AL-UABON                                 CL**1
01120                                  TO  RTYPE-ATTRB (PINDX)             CL**1
01121                        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT    CL**1
01122                     ELSE                                             CL**1
01123                        MOVE AL-UANON                                 CL**1
01124                                  TO  RTYPE-ATTRB (PINDX)             CL**1
01125             ELSE                                                     CL**1
01126                 IF PI-COMPANY-ID = 'ANL'                             CL**1
01127                     IF NOT ANL-VALID-REC-TYPE                        CL**1
01128                        MOVE -1   TO  RTYPE-LEN (PINDX)               CL**1
01129                        MOVE ER-7806                                  CL**1
01130                                  TO  EMI-ERROR                       CL**1
01131                        MOVE AL-UABON                                 CL**1
01132                                  TO  RTYPE-ATTRB (PINDX)             CL**1
01133                        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT    CL**1
01134                     ELSE                                             CL**1
01135                        MOVE AL-UANON                                 CL**1
01136                                  TO  RTYPE-ATTRB (PINDX)             CL**1
01137                 ELSE                                                 CL**1
01138                     IF NOT VALID-REC-TYPE                            CL**1
01139                         MOVE -1  TO  RTYPE-LEN (PINDX)               CL**1
01140                         MOVE ER-2234                                 CL**1
01141                                  TO  EMI-ERROR                       CL**1
01142                         MOVE AL-UABON                                CL**1
01143                                  TO  RTYPE-ATTRB (PINDX)             CL**1
01144                         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT   CL**1
01145                     ELSE                                             CL**1
01146                         MOVE AL-UANON                                CL**1
01147                                  TO  RTYPE-ATTRB (PINDX)             CL**1
01148      ELSE                                                            CL**1
01149          IF PI-FILE-SEQ-NO (NDX) = ZEROS                             CL**1
01150              MOVE -1             TO  RTYPE-LEN (PINDX)               CL**1
01151              MOVE ER-2235        TO  EMI-ERROR                       CL**1
01152              MOVE AL-UABON       TO  RTYPE-ATTRB (PINDX)             CL**1
01153              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.             CL**1
01154                                                                      CL**1
01155      IF AMT-LEN (PINDX) NOT = ZEROS                                  CL**1
01156          EXEC CICS BIF DEEDIT                                        CL**1
01157              FIELD (AMT (PINDX))                                     CL**1
01158              LENGTH (11)                                             CL**1
01159          END-EXEC                                                    CL**1
01160          IF AMT(PINDX) = ZEROS                                       CL**1
01161              IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS                     CL**1
01162                  SET WS-INDX  TO PINDX                               CL**1
01163                  MOVE AMT(PINDX) TO  WS-EDITED-AMT (WS-INDX)         CL**1
01164                  GO TO 1050-INCREMENT-PINDX                          CL**1
01165              ELSE                                                    CL**1
01166                  MOVE ER-2245    TO  EMI-ERROR                       CL**1
01167                  MOVE -1         TO  AMT-LEN(PINDX)                  CL**1
01168                  MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)               CL**1
01169                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**1
01170          ELSE                                                        CL**1
01171              IF AMT (PINDX) NUMERIC                                  CL**1
01172                  SET WS-INDX  TO PINDX                               CL**1
01173                  MOVE AMT (PINDX)                                    CL**1
01174                                  TO  WS-EDITED-AMT (WS-INDX)         CL**1
01175              ELSE                                                    CL**1
01176                  MOVE ER-2245    TO  EMI-ERROR                       CL**1
01177                  MOVE -1         TO  AMT-LEN(PINDX)                  CL**1
01178                  MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)               CL**1
01179                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**1
01180      ELSE                                                            CL**1
01181          IF PI-FILE-SEQ-NO (NDX) = ZEROS                             CL**1
01182              MOVE -1             TO  AMT-LEN (PINDX)                 CL**1
01183              MOVE ER-2236        TO  EMI-ERROR                       CL**1
01184              MOVE AL-UNBON       TO  AMT-ATTRB (PINDX)               CL**1
01185              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.             CL**1
01186                                                                      CL**1
01187      IF APPLIED-LEN (PINDX) NOT = ZERO                               CL**1
01188          IF APPLIED  (PINDX) = 'A' OR 'G' OR 'O'                     CL**1
01189              NEXT SENTENCE                                           CL**1
01190          ELSE                                                        CL**1
01191              MOVE -1             TO APPLIED-LEN   (PINDX)            CL**1
01192              MOVE ER-3146        TO EMI-ERROR                        CL**1
01193              MOVE AL-UABON       TO APPLIED-ATTRB (PINDX)            CL**1
01194              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**1
01195      ELSE                                                            CL**1
01196          IF PI-FILE-SEQ-NO (NDX) = ZEROS                             CL**1
01197              MOVE -1             TO  APPLIED-LEN (PINDX)             CL**1
01198              MOVE ER-3146        TO  EMI-ERROR                       CL**1
01199              MOVE AL-UABON       TO  APPLIED-ATTRB (PINDX)           CL**1
01200              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.             CL**1
01201                                                                      CL**1
01202      IF APPLIED-LEN (PINDX) NOT = ZEROS                              CL**1
01203          IF APPLIED (PINDX) = 'A'                                    CL**1
01204             IF ACCTI = LOW-VALUES                                    CL**1
01205                MOVE -1           TO APPLIED-LEN   (PINDX)            CL**1
01206                MOVE ER-3195      TO EMI-ERROR                        CL**1
01207                MOVE AL-UABON     TO APPLIED-ATTRB (PINDX)            CL**1
01208                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.             CL**1
01209                                                                      CL**1
01210  1010-COMP-CHECK.                                                    CL**1
01211                                                                      CL**1
01212      IF EIBAID   = DFHPF8                                            CL**1
01213          IF EIBCPOSN = WS-CURSOR-POS-1                               CL**1
01214              SET PINDX TO 1                                          CL**1
01215          ELSE                                                        CL**1
01216              IF EIBCPOSN = WS-CURSOR-POS-2                           CL**1
01217                  SET PINDX TO 2                                      CL**1
01218              ELSE                                                    CL**1
01219                  IF EIBCPOSN = WS-CURSOR-POS-3                       CL**1
01220                      SET PINDX TO 3                                  CL**1
01221                  ELSE                                                CL**1
01222                      MOVE ER-3262                                    CL**1
01223                                  TO EMI-ERROR                        CL**1
01224                      MOVE -1     TO COMM-LEN (PINDX)                 CL**1
01225                      MOVE AL-UABON                                   CL**1
01226                                  TO COMM-ATTRB (PINDX)               CL**1
01227                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT        CL**1
01228                      SET PINDX UP  BY 3                              CL**1
01229                      GO TO 1050-INCREMENT-PINDX.                     CL**1
01230                                                                      CL**1
01231      MOVE PI-COMPANY-CD          TO COMP-COMP-CD.                    CL**1
01232      MOVE CARRIERI               TO COMP-CARRIER.                    CL**1
01233      MOVE GROUPI                 TO COMP-GROUPING.                   CL**1
01234      MOVE FINRESPI               TO COMP-FIN-RESP.                   CL**1
01235                                                                      CL**1
01236      IF EIBAID NOT = DFHPF8                                          CL**1
01237          IF APPLIED-LEN (PINDX) NOT = ZEROS                          CL**1
01238              IF (APPLIED (PINDX) = 'O') OR                           CL**1
01239                 (ACCTI = SPACES OR LOW-VALUES)                       CL**1
01240                  GO TO 1011-VERIFY-COMP-G                            CL**1
01241              ELSE                                                    CL**1
01242                  NEXT SENTENCE                                       CL**1
01243          ELSE                                                        CL**1
01244              IF (PI-APPLIED (NDX) = 'O') OR                          CL**1
01245                 (ACCTI = SPACES OR LOW-VALUES)                       CL**1
01246                  GO TO 1011-VERIFY-COMP-G                            CL**1
01247              ELSE                                                    CL**1
01248                  NEXT SENTENCE                                       CL**1
01249      ELSE                                                            CL**1
01250          SET NDX TO PINDX                                            CL**1
01251          IF (PI-APPLIED (NDX) = 'O') OR                              CL**1
01252             (ACCTI = SPACES OR LOW-VALUES)                           CL**1
01253              GO TO 1011-VERIFY-COMP-G.                               CL**1
01254                                                                      CL**1
01255      MOVE ACCTI                  TO COMP-ACCOUNT.                    CL**1
01256      MOVE 'A'                    TO COMP-RECORD-TYPE.                CL**1
01257                                                                      CL**1
01258      EXEC CICS HANDLE CONDITION                                      CL**1
01259          NOTFND   (1011-NO-COMP-A)                                   CL**1
01260          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL**1
01261          END-EXEC.                                                   CL**1
01262                                                                      CL**1
01263      EXEC CICS READ                                                  CL**1
01264          DATASET  (COMP-FILE-ID)                                     CL**1
01265          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL**1
01266          RIDFLD   (ERCOMP-KEY)                                       CL**1
01267          EQUAL                                                       CL**1
01268          END-EXEC.                                                   CL**1
01269                                                                      CL**1
01270  1011-CHECK-FOR-A.                                                   CL**1
01271                                                                      CL**1
01272      IF EIBAID NOT = DFHPF8                                          CL**1
01273          IF APPLIED (PINDX) = 'A'                                    CL**1
01274               GO TO 1012-CHK-VOID                                    CL**1
01275          ELSE                                                        CL**1
01276              NEXT SENTENCE                                           CL**1
01277      ELSE                                                            CL**1
01278          IF PI-APPLIED (NDX) = 'A'                                   CL**1
01279               GO TO 1025-CHECK-INVOICE.                              CL**1
01280                                                                      CL**1
01281      IF EIBAID NOT = DFHPF8                                          CL**1
01282          IF APPLIED (PINDX) = 'G'                                    CL**1
01283               GO TO 1011-VERIFY-COMP-G                               CL**1
01284          ELSE                                                        CL**1
01285              NEXT SENTENCE                                           CL**1
01286      ELSE                                                            CL**1
01287          IF PI-APPLIED (NDX) = 'G'                                   CL**1
01288               GO TO 1011-VERIFY-COMP-G.                              CL**1
01289                                                                      CL**1
01290      IF EIBAID = DFHPF8                                              CL**1
01291          GO TO 1025-CHECK-INVOICE                                    CL**1
01292      ELSE                                                            CL**1
01293          IF APPLIED (PINDX) = 'O'                                    CL**1
01294              MOVE -1             TO APPLIED-LEN   (PINDX)            CL**1
01295              MOVE ER-3196        TO EMI-ERROR                        CL**1
01296              MOVE AL-UABON       TO APPLIED-ATTRB (PINDX)            CL**1
01297              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**1
01298              GO TO 1012-CHK-VOID.                                    CL**1
01299                                                                      CL**1
01300      GO TO 1012-CHK-VOID.                                            CL**1
01301                                                                      CL**1
01302  1011-NO-COMP-A.                                                     CL**1
01303                                                                      CL**1
01304       MOVE ER-3178               TO  EMI-ERROR.                      CL**1
01305       MOVE -1                    TO  CARRIERL.                       CL**1
01306       MOVE AL-UABON              TO  CARRIERA                        CL**1
01307                                      GROUPA                          CL**1
01308                                      FINRESPA.                       CL**1
01309                                                                      CL**1
01310       PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                    CL**1
01311                                                                      CL**1
01312       GO TO 1011-CHECK-FOR-A.                                        CL**1
01313                                                                      CL**1
01314  1011-VERIFY-COMP-G.                                                 CL**1
01315                                                                      CL**1
01316      MOVE LOW-VALUES             TO  COMP-ACCOUNT.                   CL**1
01317      MOVE 'G'                    TO  COMP-RECORD-TYPE.               CL**1
01318                                                                      CL**1
01319      EXEC CICS HANDLE CONDITION                                      CL**1
01320          NOTFND   (1011-NO-COMP-G)                                   CL**1
01321          NOTOPEN  (7100-COMP-FILE-NOTOPEN)                           CL**1
01322          END-EXEC.                                                   CL**1
01323                                                                      CL**1
01324      EXEC CICS READ                                                  CL**1
01325          DATASET  (COMP-FILE-ID)                                     CL**1
01326          SET      (ADDRESS OF COMPENSATION-MASTER)                   CL**1
01327          RIDFLD   (ERCOMP-KEY)                                       CL**1
01328          EQUAL                                                       CL**1
01329          END-EXEC.                                                   CL**1
01330                                                                      CL**1
01331       GO TO 1012-CHK-VOID.                                           CL**1
01332                                                                      CL**1
01333  1011-NO-COMP-G.                                                     CL**1
01334                                                                      CL**1
01335       MOVE ER-3260               TO  EMI-ERROR.                      CL**1
01336       MOVE -1                    TO  CARRIERL.                       CL**1
01337       MOVE AL-UABON              TO  CARRIERA                        CL**1
01338                                      GROUPA                          CL**1
01339                                      FINRESPA.                       CL**1
01340                                                                      CL**1
01341       PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                    CL**1
01342                                                                      CL**1
01343  1012-CHK-VOID.                                                      CL**1
01344                                                                      CL**1
01345      IF EIBAID = DFHPF8                                              CL**1
01346          GO TO 1025-CHECK-INVOICE.                                   CL**1
01347                                                                      CL**1
01348      IF VOID-SW-LEN (PINDX) NOT = ZEROS                              CL**1
01349          IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS                         CL**1
01350            AND (PI-REC-TYPE (NDX) = 'C')                             CL**1
01351              IF VOID-SW (PINDX) = 'V'                                CL**1
01352                  MOVE AL-UANON   TO  VOID-SW-ATTRB (PINDX)           CL**1
01353              ELSE                                                    CL**1
01354                  MOVE ER-2246    TO  EMI-ERROR                       CL**1
01355                  MOVE -1         TO  VOID-SW-LEN (PINDX)             CL**1
01356                  MOVE AL-UABON   TO  VOID-SW-ATTRB (PINDX)           CL**1
01357                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT          CL**1
01358          ELSE                                                        CL**1
01359              MOVE ER-2449        TO  EMI-ERROR                       CL**1
01360              MOVE -1             TO  VOID-SW-LEN (PINDX)             CL**1
01361              MOVE AL-UABON       TO  VOID-SW-ATTRB (PINDX)           CL**1
01362              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.             CL**1
01363                                                                      CL**1
01364      IF SDTE-LEN (PINDX) = ZEROS                                     CL**1
01365          GO TO 1016-CHECK-INPUT-DATE.                                CL**1
01366                                                                      CL**1
01367      MOVE AL-UNNON               TO  SDTE-ATTRB (PINDX).             CL**1
01368                                                                      CL**1
01369      IF SDTE (PINDX) NOT NUMERIC OR                                  CL**1
01370         SDTE (PINDX) EQUAL ZEROS                                     CL**1
01371         GO TO 1015-DAY-ERROR.                                        CL**1
01372                                                                      CL**1
01373      MOVE SDTE (PINDX)           TO  DC-GREG-DATE-1-MDY.             CL**1
01374      MOVE '4'                    TO  DC-OPTION-CODE.                 CL**1
01375      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                     CL**1
01376                                                                      CL**1
01377      IF NO-CONVERSION-ERROR                                          CL**1
01378         SET PINDEX               TO  PINDX                           CL**1
01379         MOVE DC-BIN-DATE-1       TO  WS-EOM-DT (PINDEX)              CL**1
01380      ELSE                                                            CL**1
01381         GO TO 1015-DAY-ERROR.                                        CL**1
01382                                                                      CL**1
01383      MOVE SDTE (PINDX)           TO DATE-TEST-AREA.                  CL**1
01384                                                                      CL**1
01385      IF DATE-TEST-MM = 4 OR  6  OR  9  OR  11                        CL**1
01386          IF DATE-TEST-DD  NOT = 30                                   CL**1
01387              GO TO 1015-DAY-ERROR                                    CL**1
01388          ELSE                                                        CL**1
01389              GO TO 1012-CHECK-FUTURE-MO.                             CL**1
01390                                                                      CL**1
01391      IF DATE-TEST-MM = 1 OR  3  OR  5  OR  7  OR                     CL**1
01392                              8  OR  10  OR  12                       CL**1
01393          IF DATE-TEST-DD  NOT = 31                                   CL**1
01394              GO TO 1015-DAY-ERROR                                    CL**1
01395          ELSE                                                        CL**1
01396              GO TO 1012-CHECK-FUTURE-MO.                             CL**1
01397                                                                      CL**1
01398      DIVIDE DATE-TEST-YY  BY  4  GIVING  DIVIDE-RESULT               CL**1
01399          REMAINDER  DIVIDE-REMAINDER.                                CL**1
01400                                                                      CL**1
01401      IF DATE-TEST-YY = ZERO                                          CL**1
01402          IF DATE-TEST-DD = 29                                        CL**1
01403              GO TO 1012-CHECK-FUTURE-MO                              CL**1
01404          ELSE                                                        CL**1
01405              GO TO 1015-DAY-ERROR.                                   CL**1
01406                                                                      CL**1
01407      IF DIVIDE-REMAINDER NOT = ZERO                                  CL**1
01408          IF DATE-TEST-DD  NOT = 28                                   CL**1
01409              GO TO 1015-DAY-ERROR                                    CL**1
01410          ELSE                                                        CL**1
01411              GO TO 1012-CHECK-FUTURE-MO                              CL**1
01412      ELSE                                                            CL**1
01413          IF DATE-TEST-DD = 29                                        CL**1
01414              GO TO 1012-CHECK-FUTURE-MO                              CL**1
01415          ELSE                                                        CL**1
01416              GO TO 1015-DAY-ERROR.                                   CL**1
01417                                                                      CL**1
01418  1012-CHECK-FUTURE-MO.                                               CL**1
01419                                                                      CL**1
01420      IF WS-EOM-DT (PINDEX) NOT GREATER THAN PI-CR-MONTH-END-DT       CL**1
01421          GO TO 1016-CHECK-INPUT-DATE.                                CL**1
01422                                                                      CL**1
01423      MOVE PI-CR-MONTH-END-DT     TO  DC-BIN-DATE-1.                  CL**1
01424      MOVE WS-EOM-DT (PINDEX)     TO  DC-BIN-DATE-2.                  CL**1
01425      MOVE '1'                    TO  DC-OPTION-CODE.                 CL**1
01426      PERFORM 8500-DATE-CONVERT  THRU 8500-EXIT.                      CL**1
01427                                                                      CL**1
01428      IF DATE-CONVERSION-ERROR                                        CL**1
01429         GO TO 1015-DAY-ERROR.                                        CL**1
01430                                                                      CL**1
01431      IF DC-ELAPSED-MONTHS  GREATER THAN  +2                          CL**1
01432         MOVE -1                  TO  SDTE-LEN (PINDX)                CL**1
01433         MOVE AL-UNBON            TO  SDTE-ATTRB (PINDX)              CL**1
01434         MOVE ER-0761             TO  EMI-ERROR                       CL**1
01435         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                  CL**1
01436                                                                      CL**1
01437      GO TO 1016-CHECK-INPUT-DATE.                                    CL**1
01438                                                                      CL**1
01439  1015-DAY-ERROR.                                                     CL**1
01440      MOVE -1                     TO  SDTE-LEN (PINDX).               CL**1
01441      MOVE AL-UNBON               TO  SDTE-ATTRB (PINDX).             CL**1
01442      MOVE ER-0587                TO  EMI-ERROR.                      CL**1
01443                                                                      CL**1
01444      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
01445                                                                      CL**1
01446  1016-CHECK-INPUT-DATE.                                              CL**1
01447                                                                      CL**1
01448      IF IDTE-LEN (PINDX)  =  ZEROS                                   CL**1
01449          GO TO 1020-CHECK-INVOICE.                                   CL**1
01450                                                                      CL**1
01451      MOVE AL-UNNON               TO  IDTE-ATTRB (PINDX).             CL**1
01452                                                                      CL**1
01453      IF IDTE (PINDX) NOT NUMERIC OR                                  CL**1
01454         IDTE (PINDX) EQUAL ZEROS                                     CL**1
01455          GO TO 1017-DAY-ERROR.                                       CL**1
01456                                                                      CL**1
01457      MOVE IDTE (PINDX)           TO  DC-GREG-DATE-1-MDY.             CL**1
01458      MOVE '4'                    TO  DC-OPTION-CODE.                 CL**1
01459                                                                      CL**1
01460      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                     CL**1
01461                                                                      CL**1
01462      IF NO-CONVERSION-ERROR                                          CL**1
01463          SET DINDEX              TO  PINDX                           CL**1
01464          MOVE DC-BIN-DATE-1      TO  WS-INP-DT (DINDEX)              CL**1
01465          GO TO 1020-CHECK-INVOICE.                                   CL**1
01466                                                                      CL**1
01467  1017-DAY-ERROR.                                                     CL**1
01468      MOVE -1                     TO  IDTE-LEN (PINDX).               CL**1
01469      MOVE AL-UNBON               TO  IDTE-ATTRB (PINDX).             CL**1
01470      MOVE ER-0714                TO  EMI-ERROR.                      CL**1
01471                                                                      CL**1
01472      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
01473                                                                      CL**1
01474  1020-CHECK-INVOICE.                                                 CL**1
01475                                                                      CL**1
01476      IF MORTGAGE-SESSION                                             CL**1
01477         IF AMT-LEN (PINDX) NOT = ZEROS                               CL**1
01478            IF PI-FILE-SEQ-NO (NDX) = ZEROS                           CL**1
01479               IF RTYPE (PINDX) = 'R'                                 CL**1
01480                  IF INVOICE-LEN (PINDX) = ZEROS                      CL**1
01481                     MOVE -1            TO INVOICE-LEN (PINDX)        CL**1
01482                     MOVE ER-3175       TO EMI-ERROR                  CL**1
01483                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         CL**1
01484                     GO TO 1050-INCREMENT-PINDX                       CL**1
01485                  ELSE                                                CL**1
01486                     PERFORM 6500-VERIFY-RECON-HEADER THRU            CL**1
01487                                                      6590-EXIT.      CL**1
01488      IF (INVOICE (PINDX) = SPACES OR LOW-VALUES)                     CL**1
01489          GO TO 1050-INCREMENT-PINDX.                                 CL**1
01490                                                                      CL**1
01491      IF WS-ACCEPT-1 (W-INDX) = 'A'                                   CL**1
01492          GO TO 1050-INCREMENT-PINDX.                                 CL**1
01493                                                                      CL**1
01494      IF EIBAID = DFHPF5                                              CL**1
01495          MOVE 'A'                TO WS-ACCEPT-1 (W-INDX)             CL**1
01496          MOVE 'Y'                TO WS-PREV-PF5                      CL**1
01497          MOVE DFHENTER           TO EIBAID                           CL**1
01498          GO TO 1050-INCREMENT-PINDX.                                 CL**1
01499                                                                      CL**1
01500  1025-CHECK-INVOICE.                                                 CL**1
01501                                                                      CL**1
01502      MOVE PI-COMPANY-CD          TO RECV-COMP-CD.                    CL**1
01503      MOVE '1'                    TO RECV-TYPE.                       CL**1
01504      MOVE CARRIERI               TO RECV-CARRIER.                    CL**1
01505      MOVE GROUPI                 TO RECV-GROUPING.                   CL**1
01506      MOVE CO-AR-BAL-LEVEL        TO RECV-BAL-LVL.                    CL**1
01507      MOVE FINRESPI               TO RECV-FIN-RESP.                   CL**1
01508      MOVE ACCTI                  TO RECV-ACCOUNT.                    CL**1
01509                                                                      CL**1
01510      IF EIBAID = DFHPF8                                              CL**1
01511          MOVE PI-INVOICE (NDX)   TO RECV-INVOICE                     CL**1
01512          MOVE PI-REFERENCE (NDX) TO RECV-REFERENCE                   CL**1
01513          IF PI-APPLIED (NDX) = 'A'                                   CL**1
01514              MOVE '1'            TO RECV-ENTRY-TYPE                  CL**1
01515              MOVE 'A'            TO RECV-RESPONSIBLE                 CL**1
01516          ELSE                                                        CL**1
01517              MOVE LOW-VALUES     TO RECV-RESPONSIBLE                 CL**1
01518              IF PI-APPLIED (NDX) = 'G'                               CL**1
01519                  MOVE '2'        TO RECV-ENTRY-TYPE                  CL**1
01520              ELSE                                                    CL**1
01521                  MOVE '3'        TO RECV-ENTRY-TYPE                  CL**1
01522      ELSE                                                            CL**1
01523          MOVE INVOICE (PINDX)    TO RECV-INVOICE                     CL**1
01524          MOVE REF (PINDX)        TO RECV-REFERENCE                   CL**1
01525          IF APPLIED (PINDX) = 'A'                                    CL**1
01526              MOVE '1'            TO RECV-ENTRY-TYPE                  CL**1
01527              MOVE 'A'            TO RECV-RESPONSIBLE                 CL**1
01528          ELSE                                                        CL**1
01529              MOVE LOW-VALUES     TO RECV-RESPONSIBLE                 CL**1
01530              IF APPLIED (PINDX) = 'G'                                CL**1
01531                  MOVE '2'        TO RECV-ENTRY-TYPE                  CL**1
01532              ELSE                                                    CL**1
01533                  MOVE '3'        TO RECV-ENTRY-TYPE.                 CL**1
01534                                                                      CL**1
01535                                                                      CL**1
01536      MOVE ZERO                   TO RECV-RECORD-TYPE.                CL**1
01537      MOVE +0                     TO RECV-RECORD-SEQ.                 CL**1
01538                                                                      CL**1
01539      INSPECT ERRECV-KEY REPLACING ALL SPACES BY LOW-VALUES.          CL**1
01540                                                                      CL**1
01541      EXEC CICS HANDLE CONDITION                                      CL**1
01542          NOTFND   (1040-RECV-NOTFND)                                 CL**1
01543          NOTOPEN  (7200-RECV-FILE-NOTOPEN)                           CL**1
01544          END-EXEC.                                                   CL**1
01545                                                                      CL**1
01546      EXEC CICS READ                                                  CL**1
01547          DATASET  (RECV-FILE-ID)                                     CL**1
01548          SET      (ADDRESS OF ACCOUNTS-RECEIVABLE)                   CL**1
01549          RIDFLD   (ERRECV-KEY)                                       CL**1
01550          EQUAL                                                       CL**1
01551      END-EXEC.                                                       CL**1
01552                                                                      CL**1
01553      IF AR-BAL-DEBIT                                                 CL**1
01554          MOVE AR-BAL-AMOUNT      TO WS-AR-BALANCE                    CL**1
01555      ELSE                                                            CL**1
01556          COMPUTE WS-AR-BALANCE = AR-BAL-AMOUNT * -1.                 CL**1
01557                                                                      CL**1
01558      IF EIBAID = DFHPF8                                              CL**1
01559          MOVE PI-AMOUNT (NDX)    TO WS-WORK-BALANCE                  CL**1
01560          IF PI-REC-TYPE (NDX) = 'C' OR 'U'                           CL**1
01561              COMPUTE WS-WORK-BALANCE = WS-WORK-BALANCE * -1          CL**1
01562          ELSE                                                        CL**1
01563              NEXT SENTENCE                                           CL**1
01564      ELSE                                                            CL**1
01565          IF AMT (PINDX) NUMERIC                                      CL**1
01566              MOVE AMT (PINDX)    TO WS-WORK-BALANCE                  CL**1
01567              IF RTYPE (PINDX) = 'C' OR 'U'                           CL**1
01568                  COMPUTE WS-WORK-BALANCE = WS-WORK-BALANCE * -1      CL**1
01569              ELSE                                                    CL**1
01570                  NEXT SENTENCE                                       CL**1
01571          ELSE                                                        CL**1
01572              GO TO 1050-INCREMENT-PINDX.                             CL**1
01573                                                                      CL**1
01574      COMPUTE WS-BAL-AMOUNT = WS-AR-BALANCE - WS-WORK-BALANCE.        CL**1
01575                                                                      CL**1
01576      IF EIBAID = DFHPF8                                              CL**1
01577          IF WS-BAL-AMOUNT NOT = ZERO                                 CL**1
01578              MOVE AR-BAL-AMOUNT  TO WS-EDIT-PATTERN                  CL**1
01579              MOVE WS-EDIT        TO WS-EDIT-AMOUNT                   CL**1
01580              MOVE ER-3263        TO EMI-ERROR                        CL**1
01581              MOVE -1             TO COMM-LEN (PINDX)                 CL**1
01582              MOVE AL-UABON       TO COMM-ATTRB (PINDX)               CL**1
01583              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**1
01584              SET PINDX UP  BY 3                                      CL**1
01585              IF AR-BAL-CREDIT                                        CL**1
01586                  MOVE '-'        TO  WS-EDIT-SIGN                    CL**1
01587                  MOVE WS-EDITED-AMOUNT                               CL**1
01588                                  TO EMI-TEXT-VARIABLE (1)            CL**1
01589                  GO TO 1050-INCREMENT-PINDX                          CL**1
01590              ELSE                                                    CL**1
01591                  MOVE ' '        TO  WS-EDIT-SIGN                    CL**1
01592                  MOVE WS-EDITED-AMOUNT                               CL**1
01593                                  TO EMI-TEXT-VARIABLE (1)            CL**1
01594                  GO TO 1050-INCREMENT-PINDX                          CL**1
01595          ELSE                                                        CL**1
01596              MOVE ER-3265        TO EMI-ERROR                        CL**1
01597              MOVE -1             TO COMM-LEN (PINDX)                 CL**1
01598              MOVE AL-UABON       TO COMM-ATTRB (PINDX)               CL**1
01599              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**1
01600              SET PINDX UP  BY 3                                      CL**1
01601              GO TO 1050-INCREMENT-PINDX.                             CL**1
01602                                                                      CL**1
01603      IF WS-BAL-AMOUNT NOT = ZERO                                     CL**1
01604          MOVE AR-BAL-AMOUNT      TO WS-EDIT-PATTERN                  CL**1
01605          MOVE WS-EDIT            TO WS-EDIT-AMOUNT                   CL**1
01606          MOVE ER-3266            TO EMI-ERROR                        CL**1
01607          MOVE -1                 TO INVOICE-LEN (PINDX)              CL**1
01608          MOVE AL-UABON           TO INVOICE-ATTRB (PINDX)            CL**1
01609          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**1
01610          IF AR-BAL-CREDIT                                            CL**1
01611              MOVE '-'            TO  WS-EDIT-SIGN                    CL**1
01612              IF EMI-ERROR-NUMBER (1) = '3266'                        CL**1
01613                  MOVE WS-EDITED-AMOUNT                               CL**1
01614                                  TO EMI-TEXT-VARIABLE (1)            CL**1
01615                  GO TO 1050-INCREMENT-PINDX                          CL**1
01616              ELSE                                                    CL**1
01617                  MOVE WS-EDITED-AMOUNT                               CL**1
01618                                  TO EMI-TEXT-VARIABLE (2)            CL**1
01619                  GO TO 1050-INCREMENT-PINDX                          CL**1
01620          ELSE                                                        CL**1
01621              IF EMI-ERROR-NUMBER (1) = '3266'                        CL**1
01622                  MOVE WS-EDITED-AMOUNT                               CL**1
01623                                  TO EMI-TEXT-VARIABLE (1)            CL**1
01624                  GO TO 1050-INCREMENT-PINDX                          CL**1
01625              ELSE                                                    CL**1
01626                  MOVE ' '            TO  WS-EDIT-SIGN                CL**1
01627                  MOVE WS-EDITED-AMOUNT                               CL**1
01628                                  TO EMI-TEXT-VARIABLE (2)            CL**1
01629                  GO TO 1050-INCREMENT-PINDX                          CL**1
01630      ELSE                                                            CL**1
01631          GO TO 1050-INCREMENT-PINDX.                                 CL**1
01632                                                                      CL**1
01633  1040-RECV-NOTFND.                                                   CL**1
01634                                                                      CL**1
01635      IF EIBAID = DFHPF8                                              CL**1
01636          MOVE ER-3264            TO EMI-ERROR                        CL**1
01637          MOVE -1                 TO COMM-LEN (PINDX)                 CL**1
01638          MOVE AL-UABON           TO COMM-ATTRB (PINDX)               CL**1
01639          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**1
01640          SET PINDX  UP  BY  3                                        CL**1
01641          GO TO 1050-INCREMENT-PINDX.                                 CL**1
01642                                                                      CL**1
01643      MOVE ER-3180                TO EMI-ERROR.                       CL**1
01644      MOVE -1                     TO INVOICE-LEN (PINDX).             CL**1
01645      MOVE AL-UABON               TO INVOICE-ATTRB (PINDX).           CL**1
01646                                                                      CL**1
01647      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
01648                                                                      CL**1
01649  1050-INCREMENT-PINDX.                                               CL**1
01650      SET PINDX  UP  BY  1.                                           CL**1
01651      SET W-INDX                  TO  PINDX.                          CL**1
01652      MOVE WS-ACCEPT-TABLE        TO  PI-ACCEPT.                      CL**1
01653                                                                      CL**1
01654      IF PINDX  IS NOT GREATER THAN  +3                               CL**1
01655          GO TO 1010-EDIT-LOOP.                                       CL**1
01656                                                                      CL**1
01657      IF EMI-ERROR = ZEROS                                            CL**1
01658          NEXT SENTENCE                                               CL**1
01659      ELSE                                                            CL**1
01660          MOVE 'S'                TO  PI-PREV-FUNCTION                CL**1
01661          GO TO 8200-SEND-DATAONLY.                                   CL**1
01662  EJECT                                                               CL**1
01663  2000-UPDATE-THE-FILE.                                               CL**1
01664                                                                      CL**1
01665      SET PINDX                   TO  ZERO-NDX.                       CL**1
01666                                                                      CL**1
01667  2100-UPDATE-LOOP.                                                   CL**1
01668                                                                      CL**1
01669      SET PINDX  UP  BY  1.                                           CL**1
01670      SET NDX                     TO  PINDX.                          CL**1
01671                                                                      CL**1
01672      IF PINDX  IS GREATER THAN  +3                                   CL**1
01673          GO TO 2200-UPDATE-COMPLETE.                                 CL**1
01674                                                                      CL**1
01675      IF COMM-LEN    (PINDX) = ZEROS                                  CL**1
01676        AND RTYPE-LEN   (PINDX) = ZEROS                               CL**1
01677        AND AMT-LEN     (PINDX) = ZEROS                               CL**1
01678        AND APPLIED-LEN (PINDX) = ZEROS                               CL**1
01679        AND VOID-SW-LEN (PINDX) = ZEROS                               CL**1
01680        AND SDTE-LEN    (PINDX) = ZEROS                               CL**1
01681        AND REF-LEN     (PINDX) = ZEROS                               CL**1
01682        AND INVOICE-LEN (PINDX) = ZEROS                               CL**1
01683        AND IDTE-LEN    (PINDX) = ZEROS                               CL**1
01684        AND CREDIT-LEN  (PINDX) = ZEROS                               CL**1
01685        AND DEBIT-LEN   (PINDX) = ZEROS                               CL**1
01686          GO TO 2100-UPDATE-LOOP.                                     CL**1
01687                                                                      CL**1
01688      IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS                             CL**1
01689          NEXT SENTENCE                                               CL**1
01690      ELSE                                                            CL**1
01691          GO TO 2110-ADD-RECORD.                                      CL**1
01692                                                                      CL**1
01693      EXEC CICS HANDLE CONDITION                                      CL**1
01694          NOTFND  (2110-ADD-RECORD)                                   CL**1
01695      END-EXEC.                                                       CL**1
01696                                                                      CL**1
01697      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.                    CL**1
01698      MOVE PI-FILE-SEQ-NO (NDX)    TO  PYAJ-FILE-SEQ-NO.              CL**1
01699      MOVE PI-REC-TYPE (NDX)       TO  PYAJ-RECORD-TYPE.              CL**1
01700                                                                      CL**1
01701      IF MORTGAGE-SESSION                                             CL**1
01702         MOVE MPPYAJ-FILE-ID       TO PYAJ-FILE-ID.                   CL**1
01703                                                                      CL**1
01704      EXEC CICS READ                                                  CL**1
01705          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL**1
01706          DATASET  (PYAJ-FILE-ID)                                     CL**1
01707          RIDFLD   (ERPYAJ-KEY)                                       CL**1
01708          UPDATE                                                      CL**1
01709      END-EXEC.                                                       CL**1
01710                                                                      CL**1
01711      IF RTYPE-LEN (PINDX) GREATER THAN +0                            CL**1
01712         IF RTYPE (PINDX) NOT = PY-RECORD-TYPE                        CL**1
01713            PERFORM 2190-CHANGE-RECORD-TYPE THRU 2190-EXIT.           CL**1
01714                                                                      CL**1
01715      IF AMT-LEN (PINDX) NOT = ZEROS                                  CL**1
01716         SET WS-INDX                 TO PINDX                         CL**1
01717          IF WS-EDITED-AMT (WS-INDX)  = ZEROS                         CL**1
01718              IF  (PY-CHECK-WRITTEN-DT = LOW-VALUES) AND              CL**1
01719                  (PY-CHECK-ORIGIN-SW NOT = 'V')                      CL**1
01720                  GO TO 2120-DELETE-RECORD                            CL**1
01721              ELSE                                                    CL**1
01722                  IF PI-PROCESSOR-ID = 'LGXX'                         CL**1
01723                      GO TO 2120-DELETE-RECORD                        CL**1
01724              ELSE                                                    CL**1
01725                      MOVE ER-2244   TO  EMI-ERROR                    CL**1
01726                      MOVE -1        TO  AMT-LEN (PINDX)              CL**1
01727                      MOVE AL-UNBON  TO  AMT-ATTRB (PINDX)            CL**1
01728                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT      CL**1
01729                      MOVE PY-ENTRY-AMT                               CL**1
01730                                     TO  AMTO (PINDX)                 CL**1
01731                      EXEC CICS UNLOCK                                CL**1
01732                      DATASET  (PYAJ-FILE-ID)                         CL**1
01733                      END-EXEC                                        CL**1
01734                      GO TO 8200-SEND-DATAONLY.                       CL**1
01735                                                                      CL**1
01736      MOVE 'B'                    TO  JP-RECORD-TYPE.                 CL**1
01737      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                     CL**1
01738      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**1
01739                                                                      CL**1
01740      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**1
01741                                                                      CL**1
01742      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.               CL**1
01743      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.           CL**1
01744      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT.               CL**1
01745                                                                      CL**1
01746      IF COMM-LEN (PINDX) NOT = ZEROS                                 CL**1
01747          MOVE COMM (PINDX)       TO  PY-ENTRY-COMMENT.               CL**1
01748                                                                      CL**1
01749      IF AMT-LEN (PINDX) NOT = ZEROS                                  CL**1
01750          SET WS-INDX             TO  PINDX                           CL**1
01751          MOVE PY-ENTRY-AMT       TO  WS-OLD-ENTRY-AMT                CL**1
01752          MOVE WS-EDITED-AMT (WS-INDX)  TO  PY-ENTRY-AMT              CL**1
01753                                            WS-ENTRY-AMT.             CL**1
01754                                                                      CL**1
01755      IF APPLIED-LEN (PINDX) NOT = ZERO                               CL**1
01756          MOVE APPLIED (PINDX)    TO  PY-PMT-APPLIED.                 CL**1
01757                                                                      CL**1
01758      IF VOID-SW-LEN (PINDX) NOT = ZEROS                              CL**1
01759          MOVE VOID-SW (PINDX)    TO  PY-VOID-SW.                     CL**1
01760                                                                      CL**1
01761      IF SDTE-LEN (PINDX) NOT = ZEROS                                 CL**1
01762         SET PINDEX               TO  PINDX                           CL**1
01763         MOVE WS-EOM-DT (PINDEX)  TO  PY-CREDIT-SELECT-DT.            CL**1
01764                                                                      CL**1
01765      IF REF-LEN  (PINDX) NOT = ZEROS                                 CL**1
01766         MOVE REF (PINDX)         TO  PY-REF-NO.                      CL**1
01767                                                                      CL**1
01768      IF INVOICE-LEN   (PINDX) NOT = ZEROS                            CL**1
01769         MOVE INVOICE  (PINDX)    TO  PY-BIL-INV.                     CL**1
01770                                                                      CL**1
01771      IF IDTE-LEN (PINDX)  NOT =  ZEROS                               CL**1
01772          SET DINDEX              TO  PINDX                           CL**1
01773          MOVE WS-INP-DT (DINDEX)                                     CL**1
01774                                  TO  PY-INPUT-DT.                    CL**1
01775                                                                      CL**1
01776      IF CREDIT-LEN    (PINDX) NOT = ZEROS                            CL**1
01777         MOVE CREDIT   (PINDX)    TO  PY-GL-CR.                       CL**1
01778                                                                      CL**1
01779      IF DEBIT-LEN     (PINDX) NOT = ZEROS                            CL**1
01780         MOVE DEBIT    (PINDX)    TO  PY-GL-DB.                       CL**1
01781                                                                      CL**1
01782      MOVE 'C'                    TO  JP-RECORD-TYPE.                 CL**1
01783      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                     CL**1
01784      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**1
01785                                                                      CL**1
01786      IF MORTGAGE-SESSION                                             CL**1
01787         MOVE 'U'                 TO  WS-RECON-SW                     CL**1
01788         PERFORM 6600-UPDATE-RECON-HEADER THRU 6690-EXIT.             CL**1
01789                                                                      CL**1
01790      EXEC CICS REWRITE                                               CL**1
01791          DATASET  (PYAJ-FILE-ID)                                     CL**1
01792          FROM     (PENDING-PAY-ADJ)                                  CL**1
01793      END-EXEC.                                                       CL**1
01794                                                                      CL**1
01795      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**1
01796                                                                      CL**1
01797      GO TO 2100-UPDATE-LOOP.                                         CL**1
01798                                                                      CL**1
01799  EJECT                                                               CL**1
01800                                                                      CL**1
01801  2110-ADD-RECORD.                                                    CL**1
01802      EXEC CICS GETMAIN                                               CL**1
01803          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL**1
01804          LENGTH   (ERPYAJ-RECORD-LENGTH)                             CL**1
01805          INITIMG  (GETMAIN-SPACE)                                    CL**1
01806      END-EXEC.                                                       CL**1
01807                                                                      CL**1
01808      MOVE 'PY'                   TO  PY-RECORD-ID.                   CL**1
01809      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.                  CL**1
01810      MOVE PI-SAV-CARRIER         TO  PY-CARRIER.                     CL**1
01811      MOVE PI-SAV-GROUPING        TO  PY-GROUPING.                    CL**1
01812      MOVE PI-SAV-FIN-RESP        TO  PY-FIN-RESP.                    CL**1
01813      MOVE PI-SAV-ACCOUNT         TO  PY-ACCOUNT.                     CL**1
01814      MOVE RTYPE (PINDX)          TO  PY-RECORD-TYPE.                 CL**1
01815      MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.                 CL**1
01816                                                                      CL**1
01817      ADD +1                      TO  WORK-SEQ-NO.                    CL**1
01818                                                                      CL**1
01819      IF COMM-LEN (PINDX) NOT = ZEROS                                 CL**1
01820          MOVE COMM (PINDX)       TO  PY-ENTRY-COMMENT.               CL**1
01821                                                                      CL**1
01822      EXEC CICS BIF DEEDIT                                            CL**1
01823          FIELD (AMT (PINDX))                                         CL**1
01824          LENGTH (11)                                                 CL**1
01825      END-EXEC.                                                       CL**1
01826                                                                      CL**1
01827      MOVE AMT (PINDX)            TO  PY-ENTRY-AMT                    CL**1
01828                                      WS-ENTRY-AMT.                   CL**1
01829                                                                      CL**1
01830      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.               CL**1
01831      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.           CL**1
01832      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT.               CL**1
01833      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL            CL**1
01834                                      PY-CHECK-QUE-SEQUENCE.          CL**1
01835      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT             CL**1
01836                                      PY-BILLED-DATE                  CL**1
01837                                      PY-AR-DATE                      CL**1
01838                                      PY-GL-DATE                      CL**1
01839                                      PY-REPORTED-DT                  CL**1
01840                                      PY-CHECK-WRITTEN-DT.            CL**1
01841                                                                      CL**1
01842      IF SDTE (PINDX)  IS NUMERIC                                     CL**1
01843          MOVE SDTE (PINDX)       TO  DC-GREG-DATE-1-MDY              CL**1
01844          MOVE '4'                TO  DC-OPTION-CODE                  CL**1
01845          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                  CL**1
01846          IF DATE-CONVERSION-ERROR                                    CL**1
01847              IF MORTGAGE-SESSION                                     CL**1
01848                  MOVE PI-MP-MONTH-END-DT                             CL**1
01849                                  TO  PY-CREDIT-SELECT-DT             CL**1
01850              ELSE                                                    CL**1
01851                  MOVE PI-CR-MONTH-END-DT                             CL**1
01852                                  TO  PY-CREDIT-SELECT-DT             CL**1
01853          ELSE                                                        CL**1
01854              MOVE DC-BIN-DATE-1  TO  PY-CREDIT-SELECT-DT             CL**1
01855      ELSE                                                            CL**1
01856          IF MORTGAGE-SESSION                                         CL**1
01857              MOVE PI-MP-MONTH-END-DT                                 CL**1
01858                                  TO  PY-CREDIT-SELECT-DT             CL**1
01859          ELSE                                                        CL**1
01860              MOVE PI-CR-MONTH-END-DT                                 CL**1
01861                                  TO  PY-CREDIT-SELECT-DT.            CL**1
01862                                                                      CL**1
01863      IF REF-LEN  (PINDX) NOT = ZEROS                                 CL**1
01864         MOVE REF (PINDX)         TO  PY-REF-NO.                      CL**1
01865                                                                      CL**1
01866      IF INVOICE-LEN   (PINDX) NOT = ZEROS                            CL**1
01867         MOVE INVOICE  (PINDX)    TO  PY-BIL-INV.                     CL**1
01868                                                                      CL**1
01869      IF IDTE (PINDX)  IS NUMERIC                                     CL**1
01870          MOVE IDTE (PINDX)       TO  DC-GREG-DATE-1-MDY              CL**1
01871          MOVE '4'                TO  DC-OPTION-CODE                  CL**1
01872          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                  CL**1
01873          IF DATE-CONVERSION-ERROR                                    CL**1
01874              MOVE -1             TO  IDTE-LEN (PINDX)                CL**1
01875              MOVE AL-UNBON       TO  IDTE-ATTRB (PINDX)              CL**1
01876              MOVE ER-0714        TO  EMI-ERROR                       CL**1
01877              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**1
01878          ELSE                                                        CL**1
01879              MOVE DC-BIN-DATE-1  TO  PY-INPUT-DT                     CL**1
01880      ELSE                                                            CL**1
01881          MOVE WS-CURRENT-BIN-DT  TO  PY-INPUT-DT.                    CL**1
01882                                                                      CL**1
01883      IF CREDIT-LEN    (PINDX) NOT = ZEROS                            CL**1
01884         MOVE CREDIT   (PINDX)    TO  PY-GL-CR.                       CL**1
01885                                                                      CL**1
01886      IF DEBIT-LEN     (PINDX) NOT = ZEROS                            CL**1
01887         MOVE DEBIT    (PINDX)    TO  PY-GL-DB.                       CL**1
01888                                                                      CL**1
01889      MOVE APPLIED (PINDX)        TO  PY-PMT-APPLIED.                 CL**1
01890                                                                      CL**1
01891  2115-WRITE-REC.                                                     CL**1
01892                                                                      CL**1
01893      EXEC CICS HANDLE CONDITION                                      CL**1
01894          DUPREC  (2115-DUP-RECORD)                                   CL**1
01895      END-EXEC.                                                       CL**1
01896                                                                      CL**1
01897      MOVE 'A'                    TO  JP-RECORD-TYPE.                 CL**1
01898      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                     CL**1
01899      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**1
01900                                                                      CL**1
01901      IF MORTGAGE-SESSION                                             CL**1
01902         MOVE 'A'                 TO WS-RECON-SW                      CL**1
01903         PERFORM 6600-UPDATE-RECON-HEADER THRU 6690-EXIT.             CL**1
01904                                                                      CL**1
01905  2115-RETRY-WRITE.                                                   CL**1
01906                                                                      CL**1
01907      EXEC CICS WRITE                                                 CL**1
01908          DATASET  (PYAJ-FILE-ID)                                     CL**1
01909          FROM     (PENDING-PAY-ADJ)                                  CL**1
01910          RIDFLD   (PY-CONTROL-PRIMARY)                               CL**1
01911      END-EXEC.                                                       CL**1
01912                                                                      CL**1
01913      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**1
01914                                                                      CL**1
01915      GO TO 2100-UPDATE-LOOP.                                         CL**1
01916                                                                      CL**1
01917  2115-DUP-RECORD.                                                    CL**1
01918                                                                      CL**1
01919      COMPUTE PY-FILE-SEQ-NO = PY-FILE-SEQ-NO + 1.                    CL**1
01920                                                                      CL**1
01921      GO TO 2115-RETRY-WRITE.                                         CL**1
01922                                                                      CL**1
01923  EJECT                                                               CL**1
01924                                                                      CL**1
01925  2120-DELETE-RECORD.                                                 CL**1
01926      IF PY-RECORD-TYPE NOT = 'C' AND 'U'                             CL**1
01927          GO TO 2120-DELETE-CONT.                                     CL**1
01928                                                                      CL**1
01929      IF PY-CHECK-QUE-CONTROL = ZEROS                                 CL**1
01930        AND PY-CHECK-QUE-SEQUENCE = ZEROS                             CL**1
01931          GO TO 2120-DELETE-CONT.                                     CL**1
01932                                                                      CL**1
01933      EXEC CICS HANDLE CONDITION                                      CL**1
01934          NOTFND  (2120-DELETE-CONT)                                  CL**1
01935      END-EXEC.                                                       CL**1
01936                                                                      CL**1
01937      MOVE PY-COMPANY-CD          TO  CHKQ-COMPANY-CD.                CL**1
01938      MOVE PY-CHECK-QUE-CONTROL   TO  CHKQ-CONTROL-NUMBER.            CL**1
01939      MOVE PY-CHECK-QUE-SEQUENCE  TO  CHKQ-SEQUENCE-NUMBER.           CL**1
01940                                                                      CL**1
01941      EXEC CICS READ                                                  CL**1
01942          SET      (ADDRESS OF CHECK-QUE)                             CL**1
01943          DATASET  (CHKQ-FILE-ID)                                     CL**1
01944          RIDFLD   (ERCHKQ-KEY)                                       CL**1
01945          UPDATE                                                      CL**1
01946      END-EXEC.                                                       CL**1
01947                                                                      CL**1
01948      MOVE 'D'                    TO  JP-RECORD-TYPE.                 CL**1
01949      MOVE CHKQ-FILE-ID           TO  JP-FILE-ID.                     CL**1
01950      MOVE CHECK-QUE              TO  JP-RECORD-AREA.                 CL**1
01951                                                                      CL**1
01952      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**1
01953                                                                      CL**1
01954      EXEC CICS DELETE                                                CL**1
01955          DATASET  (CHKQ-FILE-ID)                                     CL**1
01956      END-EXEC.                                                       CL**1
01957                                                                      CL**1
01958  2120-DELETE-CONT.                                                   CL**1
01959      MOVE 'D'                    TO  JP-RECORD-TYPE.                 CL**1
01960      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                     CL**1
01961      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**1
01962                                                                      CL**1
01963      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**1
01964                                                                      CL**1
01965      MOVE PY-ENTRY-AMT           TO WS-ENTRY-AMT.                    CL**1
01966                                                                      CL**1
01967      IF MORTGAGE-SESSION                                             CL**1
01968         MOVE 'D'                 TO WS-RECON-SW                      CL**1
01969         PERFORM 6600-UPDATE-RECON-HEADER THRU 6690-EXIT.             CL**1
01970                                                                      CL**1
01971      EXEC CICS DELETE                                                CL**1
01972          DATASET(PYAJ-FILE-ID)                                       CL**1
01973      END-EXEC.                                                       CL**1
01974                                                                      CL**1
01975      GO TO 2100-UPDATE-LOOP.                                         CL**1
01976                                                                      CL**1
01977                                                                      CL**1
01978  2190-CHANGE-RECORD-TYPE.                                            CL**1
01979                                                                      CL**1
01980      MOVE 'D'                    TO  JP-RECORD-TYPE.                 CL**1
01981      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                     CL**1
01982      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**1
01983                                                                      CL**1
01984      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**1
01985                                                                      CL**1
01986      EXEC CICS DELETE                                                CL**1
01987          DATASET(PYAJ-FILE-ID)                                       CL**1
01988      END-EXEC.                                                       CL**1
01989                                                                      CL**1
01990      EXEC CICS GETMAIN                                               CL**1
01991          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL**1
01992          LENGTH   (ERPYAJ-RECORD-LENGTH)                             CL**1
01993          INITIMG  (GETMAIN-SPACE)                                    CL**1
01994      END-EXEC.                                                       CL**1
01995                                                                      CL**1
01996      MOVE JP-RECORD-AREA         TO PENDING-PAY-ADJ.                 CL**1
01997      MOVE ERPYAJ-KEY             TO PY-CONTROL-PRIMARY.              CL**1
01998                                                                      CL**1
01999      MOVE RTYPE (PINDX)          TO PY-RECORD-TYPE                   CL**1
02000                                     PYAJ-RECORD-TYPE.                CL**1
02001      EXEC CICS HANDLE CONDITION                                      CL**1
02002          DUPREC  (2190-DUP-RECORD)                                   CL**1
02003      END-EXEC.                                                       CL**1
02004                                                                      CL**1
02005      MOVE 'A'                    TO  JP-RECORD-TYPE.                 CL**1
02006      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.                     CL**1
02007      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.                 CL**1
02008                                                                      CL**1
02009                                                                      CL**1
02010  2190-RETRY-WRITE.                                                   CL**1
02011                                                                      CL**1
02012      EXEC CICS WRITE                                                 CL**1
02013          DATASET  (PYAJ-FILE-ID)                                     CL**1
02014          FROM     (PENDING-PAY-ADJ)                                  CL**1
02015          RIDFLD   (PY-CONTROL-PRIMARY)                               CL**1
02016      END-EXEC.                                                       CL**1
02017                                                                      CL**1
02018      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL**1
02019                                                                      CL**1
02020      EXEC CICS READ                                                  CL**1
02021          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL**1
02022          DATASET  (PYAJ-FILE-ID)                                     CL**1
02023          RIDFLD   (ERPYAJ-KEY)                                       CL**1
02024          UPDATE                                                      CL**1
02025      END-EXEC.                                                       CL**1
02026                                                                      CL**1
02027      GO TO 2190-EXIT.                                                CL**1
02028                                                                      CL**1
02029                                                                      CL**1
02030  2190-DUP-RECORD.                                                    CL**1
02031                                                                      CL**1
02032      COMPUTE PY-FILE-SEQ-NO = PY-FILE-SEQ-NO + 1.                    CL**1
02033                                                                      CL**1
02034      GO TO 2190-RETRY-WRITE.                                         CL**1
02035                                                                      CL**1
02036  2190-EXIT.                                                          CL**1
02037      EXIT.                                                           CL**1
02038                                                                      CL**1
02039  2200-UPDATE-COMPLETE.                                               CL**1
02040      IF EIBAID = DFHPF1                                              CL**1
02041          GO TO 4000-BROWSE-FRWD.                                     CL**1
02042                                                                      CL**1
02043      IF EIBAID = DFHPF2                                              CL**1
02044          GO TO 4100-BROWSE-BKWD.                                     CL**1
02045                                                                      CL**1
02046      MOVE LOW-VALUES             TO  EL635AI.                        CL**1
02047                                                                      CL**1
02048      IF WARNING-SW = 'N'                                             CL**1
02049          MOVE ER-0000            TO  EMI-ERROR                       CL**1
02050      ELSE                                                            CL**1
02051          MOVE ER-3172            TO  EMI-ERROR.                      CL**1
02052                                                                      CL**1
02053      MOVE -1                     TO  MAINTL.                         CL**1
02054                                                                      CL**1
02055      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                      CL**1
02056                                                                      CL**1
02057      GO TO 8100-SEND-INITIAL-MAP.                                    CL**1
02058  EJECT                                                               CL**1
02059  4000-BROWSE-FRWD.                                                   CL**1
02060                                                                      CL**1
02061      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.                    CL**1
02062                                                                      CL**1
02063      IF EIBAID = DFHPF1                                              CL**1
02064          IF PAGE-FULL                                                CL**1
02065              MOVE PI-FILE-SEQ-NO (3)                                 CL**1
02066                                       TO  PYAJ-FILE-SEQ-NO           CL**1
02067              MOVE HIGH-VALUES         TO  PYAJ-RECORD-TYPE           CL**1
02068              MOVE 'N'                 TO  PYAJ-READ-SW               CL**1
02069              MOVE PI-SAV-ACCT-AMT     TO  TOTAL-ACCT-AMT             CL**1
02070              MOVE PI-SAV-ACCT-NET     TO  TOTAL-ACCT-NET             CL**1
02071          ELSE                                                        CL**1
02072              IF TOP-OF-FILE                                          CL**1
02073                 MOVE SPACE            TO  PI-PYAJ-FILE-SW            CL**1
02074                 MOVE ZEROS            TO  PYAJ-FILE-SEQ-NO           CL**1
02075                 MOVE SPACES           TO  PYAJ-RECORD-TYPE           CL**1
02076              ELSE                                                    CL**1
02077                 MOVE 99999999         TO  PYAJ-FILE-SEQ-NO           CL**1
02078                 MOVE HIGH-VALUES      TO  PYAJ-RECORD-TYPE           CL**1
02079      ELSE                                                            CL**1
02080          MOVE ZEROS                   TO  PYAJ-FILE-SEQ-NO           CL**1
02081          MOVE SPACES                  TO  PYAJ-RECORD-TYPE.          CL**1
02082                                                                      CL**1
02083                                                                      CL**1
02084      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES                 CL**1
02085          MOVE PI-CARRIER-SECURITY  TO  PYAJ-CARRIER.                 CL**1
02086                                                                      CL**1
02087  4000-BROWSE-FRWD-FOR-PREV.                                          CL**1
02088                                                                      CL**1
02089                                                                      CL**1
02090      MOVE SPACE                  TO PI-REC-TYPE (1)                  CL**1
02091                                     PI-REC-TYPE (2)                  CL**1
02092                                     PI-REC-TYPE (3).                 CL**1
02093                                                                      CL**1
02094      MOVE ZEROS                  TO PI-FILE-SEQ-NO (1)               CL**1
02095                                     PI-FILE-SEQ-NO (2)               CL**1
02096                                     PI-FILE-SEQ-NO (3)               CL**1
02097                                     PI-AMOUNT (1)                    CL**1
02098                                     PI-AMOUNT (2)                    CL**1
02099                                     PI-AMOUNT (3).                   CL**1
02100                                                                      CL**1
02101      MOVE SPACE                  TO PI-APPLIED (1)                   CL**1
02102                                     PI-REFERENCE (1)                 CL**1
02103                                     PI-INVOICE (1)                   CL**1
02104                                     PI-APPLIED (2)                   CL**1
02105                                     PI-REFERENCE (2)                 CL**1
02106                                     PI-INVOICE (2)                   CL**1
02107                                     PI-APPLIED (3)                   CL**1
02108                                     PI-REFERENCE (3)                 CL**1
02109                                     PI-INVOICE (3).                  CL**1
02110                                                                      CL**1
02111      IF END-OF-FILE                                                  CL**1
02112          IF EIBAID = DFHPF1                                          CL**1
02113             IF PI-TOTAL-DISPLAYED                                    CL**1
02114                NEXT SENTENCE                                         CL**1
02115             ELSE                                                     CL**1
02116                MOVE -1                 TO  MAINTL                    CL**1
02117                MOVE ER-2237            TO  EMI-ERROR                 CL**1
02118                PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT            CL**1
02119                MOVE SPACE              TO  PI-PYAJ-FILE-SW           CL**1
02120                SET PINDX               TO  +1                        CL**1
02121                MOVE 'TOTAL'            TO  COMM (PINDX)              CL**1
02122                MOVE PI-SAV-ACCT-AMT    TO  AMTO (PINDX)              CL**1
02123                MOVE 'NET TOTAL'        TO  REF  (PINDX)              CL**1
02124                MOVE PI-SAV-ACCT-NET    TO  NETO (PINDX)              CL**1
02125                MOVE ZEROS              TO  PI-SAV-ACCT-AMT           CL**1
02126                                         PI-SAV-ACCT-NET              CL**1
02127                GO TO 8100-SEND-INITIAL-MAP.                          CL**1
02128                                                                      CL**1
02129       IF END-OF-FILE                                                 CL**1
02130          IF EIBAID = DFHPF1                                          CL**1
02131             MOVE -1             TO  MAINTL                           CL**1
02132             MOVE ER-2237        TO  EMI-ERROR                        CL**1
02133             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT               CL**1
02134             GO TO 8200-SEND-DATAONLY.                                CL**1
02135                                                                      CL**1
02136      PERFORM 5000-START-BROWSE  THRU  5030-EXIT.                     CL**1
02137                                                                      CL**1
02138      IF NO-RECORDS                                                   CL**1
02139         MOVE SPACE              TO  PI-PYAJ-FILE-SW                  CL**1
02140         MOVE LOW-VALUES         TO  EL635AO                          CL**1
02141         MOVE ER-2239            TO  EMI-ERROR                        CL**1
02142         MOVE -1                 TO  MAINTL                           CL**1
02143         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                   CL**1
02144         IF (SENT-FROM-PENDING                                        CL**1
02145             AND PI-PYAJ-REFERENCE GREATER THAN SPACES)               CL**1
02146             MOVE PI-PYAJ-REFERENCE TO  REF1O                         CL**1
02147             MOVE 12                TO  REF1L                         CL**1
02148             MOVE AL-UANON          TO  REF1A                         CL**1
02149             MOVE SPACE             TO  EL630-SENT-SW                 CL**1
02150             MOVE 'Y'               TO  PI-PYAJ-FILLED-SW             CL**1
02151             GO TO 8100-SEND-INITIAL-MAP                              CL**1
02152         ELSE                                                         CL**1
02153             GO TO 8100-SEND-INITIAL-MAP.                             CL**1
02154                                                                      CL**1
02155      IF NOT-OPEN                                                     CL**1
02156          GO TO 7000-PYAJ-FILE-NOTOPEN.                               CL**1
02157                                                                      CL**1
02158      MOVE LOW-VALUES             TO  EL635AO.                        CL**1
02159      MOVE ZEROS                  TO  PI-SEQ-NOS.                     CL**1
02160                                                                      CL**1
02161      PERFORM 6000-READ-AND-FORMAT-SCREEN  THRU  6200-EXIT.           CL**1
02162                                                                      CL**1
02163      IF NO-RECORDS                                                   CL**1
02164          MOVE SPACE              TO  PI-PYAJ-FILE-SW                 CL**1
02165          MOVE ER-2239            TO  EMI-ERROR                       CL**1
02166          MOVE -1                 TO  MAINTL                          CL**1
02167          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**1
02168          IF (SENT-FROM-PENDING                                       CL**1
02169             AND PI-PYAJ-REFERENCE GREATER THAN SPACES)               CL**1
02170             MOVE PI-PYAJ-REFERENCE TO  REF1O                         CL**1
02171             MOVE 12                TO  REF1L                         CL**1
02172             MOVE AL-UANON          TO  REF1A                         CL**1
02173             MOVE SPACE             TO  EL630-SENT-SW                 CL**1
02174             MOVE 'Y'               TO  PI-PYAJ-FILLED-SW             CL**1
02175             GO TO 8100-SEND-INITIAL-MAP                              CL**1
02176          ELSE                                                        CL**1
02177              GO TO 8100-SEND-INITIAL-MAP.                            CL**1
02178                                                                      CL**1
02179      IF END-OF-FILE                                                  CL**1
02180          IF EIBAID = DFHPF2                                          CL**1
02181              MOVE ER-2238        TO  EMI-ERROR                       CL**1
02182              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT              CL**1
02183              MOVE 'T'            TO  PI-PYAJ-FILE-SW                 CL**1
02184          ELSE                                                        CL**1
02185              MOVE ER-2237        TO  EMI-ERROR                       CL**1
02186              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.             CL**1
02187                                                                      CL**1
02188      MOVE 'S'                    TO  PI-PREV-FUNCTION                CL**1
02189                                      PI-SAV-FUNCTION.                CL**1
02190                                                                      CL**1
02191      GO TO 8100-SEND-INITIAL-MAP.                                    CL**1
02192  EJECT                                                               CL**1
02193  4100-BROWSE-BKWD.                                                   CL**1
02194      MOVE SPACE                   TO  PI-PYAJ-FILE-SW                CL**1
02195                                       PI-TOTAL-DISPLAYED-SW.         CL**1
02196      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.                    CL**1
02197      MOVE ZEROS                   TO  PYAJ-FILE-SEQ-NO.              CL**1
02198      MOVE ZEROS                   TO  PYAJ-RECORD-TYPE.              CL**1
02199                                                                      CL**1
02200      PERFORM 5000-START-BROWSE  THRU  5030-EXIT.                     CL**1
02201                                                                      CL**1
02202      IF NO-RECORDS                                                   CL**1
02203          MOVE SPACE              TO  PI-PYAJ-FILE-SW                 CL**1
02204          MOVE LOW-VALUES         TO  EL635AO                         CL**1
02205          MOVE ER-2239            TO  EMI-ERROR                       CL**1
02206          MOVE -1                 TO  MAINTL                          CL**1
02207          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL**1
02208          GO TO 8100-SEND-INITIAL-MAP.                                CL**1
02209                                                                      CL**1
02210      IF NOT-OPEN                                                     CL**1
02211          GO TO 7000-PYAJ-FILE-NOTOPEN.                               CL**1
02212                                                                      CL**1
02213      EXEC CICS READNEXT                                              CL**1
02214          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL**1
02215          DATASET  (PYAJ-FILE-ID)                                     CL**1
02216          RIDFLD   (ERPYAJ-KEY)                                       CL**1
02217      END-EXEC.                                                       CL**1
02218                                                                      CL**1
02219      PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT.                    CL**1
02220                                                                      CL**1
02221      PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT.                    CL**1
02222                                                                      CL**1
02223      PERFORM 5200-END-BROWSE  THRU  5200-EXIT.                       CL**1
02224                                                                      CL**1
02225      MOVE ZEROS                  TO  PYAJ-FILE-SEQ-NO.               CL**1
02226      MOVE SPACES                 TO  PYAJ-RECORD-TYPE.               CL**1
02227                                                                      CL**1
02228      GO TO 4000-BROWSE-FRWD-FOR-PREV.                                CL**1
02229  EJECT                                                               CL**1
02230  5000-START-BROWSE.                                                  CL**1
02231      EXEC CICS HANDLE CONDITION                                      CL**1
02232          NOTOPEN  (5010-NOT-OPEN)                                    CL**1
02233          NOTFND   (5020-NO-RECORDS)                                  CL**1
02234          ENDFILE  (5020-NO-RECORDS)                                  CL**1
02235      END-EXEC.                                                       CL**1
02236                                                                      CL**1
02237      EXEC CICS STARTBR                                               CL**1
02238          DATASET  (PYAJ-FILE-ID)                                     CL**1
02239          RIDFLD   (ERPYAJ-KEY)                                       CL**1
02240      END-EXEC.                                                       CL**1
02241                                                                      CL**1
02242      GO TO 5030-EXIT.                                                CL**1
02243                                                                      CL**1
02244  5010-NOT-OPEN.                                                      CL**1
02245      MOVE 'Z'                    TO  PI-PYAJ-FILE-SW.                CL**1
02246                                                                      CL**1
02247      GO TO 5030-EXIT.                                                CL**1
02248                                                                      CL**1
02249  5020-NO-RECORDS.                                                    CL**1
02250      MOVE ZEROS                  TO  PI-SEQ-NOS.                     CL**1
02251      MOVE 'Y'                    TO  PI-PYAJ-FILE-SW.                CL**1
02252                                                                      CL**1
02253  5030-EXIT.                                                          CL**1
02254      EXIT.                                                           CL**1
02255  EJECT                                                               CL**1
02256  5100-READ-PREVIOUS.                                                 CL**1
02257      EXEC CICS HANDLE CONDITION                                      CL**1
02258          ENDFILE  (5110-END-OF-FILE)                                 CL**1
02259          NOTFND   (5110-END-OF-FILE)                                 CL**1
02260      END-EXEC.                                                       CL**1
02261                                                                      CL**1
02262      EXEC CICS READPREV                                              CL**1
02263          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL**1
02264          DATASET  (PYAJ-FILE-ID)                                     CL**1
02265          RIDFLD   (ERPYAJ-KEY)                                       CL**1
02266      END-EXEC.                                                       CL**1
02267                                                                      CL**1
02268      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES                 CL**1
02269          IF PI-CARRIER-SECURITY = PY-CARRIER                         CL**1
02270              NEXT SENTENCE                                           CL**1
02271          ELSE                                                        CL**1
02272              GO TO 5110-END-OF-FILE.                                 CL**1
02273                                                                      CL**1
02274      GO TO 5120-EXIT.                                                CL**1
02275                                                                      CL**1
02276  5110-END-OF-FILE.                                                   CL**1
02277                                                                      CL**1
02278      MOVE ER-2238                TO  EMI-ERROR.                      CL**1
02279      MOVE -1                     TO  PFENTERL.                       CL**1
02280                                                                      CL**1
02281      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
02282                                                                      CL**1
02283      GO TO 8200-SEND-DATAONLY.                                       CL**1
02284                                                                      CL**1
02285  5120-EXIT.                                                          CL**1
02286      EXIT.                                                           CL**1
02287                                                                      CL**1
02288  5200-END-BROWSE.                                                    CL**1
02289      EXEC CICS ENDBR                                                 CL**1
02290          DATASET  (PYAJ-FILE-ID)                                     CL**1
02291      END-EXEC.                                                       CL**1
02292                                                                      CL**1
02293  5200-EXIT.                                                          CL**1
02294      EXIT.                                                           CL**1
02295  EJECT                                                               CL**1
02296  6000-READ-AND-FORMAT-SCREEN.                                        CL**1
02297      EXEC CICS HANDLE CONDITION                                      CL**1
02298          ENDFILE  (6100-END-OF-FILE)                                 CL**1
02299          NOTFND   (6100-END-OF-FILE)                                 CL**1
02300      END-EXEC.                                                       CL**1
02301                                                                      CL**1
02302      MOVE SPACE                  TO PI-REC-TYPE (1)                  CL**1
02303                                     PI-REC-TYPE (2)                  CL**1
02304                                     PI-REC-TYPE (3).                 CL**1
02305                                                                      CL**1
02306      MOVE ZEROS                  TO PI-FILE-SEQ-NO (1)               CL**1
02307                                     PI-FILE-SEQ-NO (2)               CL**1
02308                                     PI-FILE-SEQ-NO (3)               CL**1
02309                                     PI-AMOUNT (1)                    CL**1
02310                                     PI-AMOUNT (2)                    CL**1
02311                                     PI-AMOUNT (3).                   CL**1
02312                                                                      CL**1
02313      MOVE SPACE                  TO PI-APPLIED (1)                   CL**1
02314                                     PI-REFERENCE (1)                 CL**1
02315                                     PI-INVOICE (1)                   CL**1
02316                                     PI-APPLIED (2)                   CL**1
02317                                     PI-REFERENCE (2)                 CL**1
02318                                     PI-INVOICE (2)                   CL**1
02319                                     PI-APPLIED (3)                   CL**1
02320                                     PI-REFERENCE (3)                 CL**1
02321                                     PI-INVOICE (3).                  CL**1
02322                                                                      CL**1
02323      MOVE SPACE                  TO PI-TOTAL-DISPLAYED-SW.           CL**1
02324      SET PINDX                   TO ZERO-NDX.                        CL**1
02325                                                                      CL**1
02326      IF (SENT-FROM-PENDING                                           CL**1
02327             AND PI-PYAJ-REFERENCE GREATER THAN SPACES)               CL**1
02328         SET PINDX  UP  BY  1                                         CL**1
02329         MOVE 12                  TO  REF-LEN (PINDX)                 CL**1
02330         MOVE PI-PYAJ-REFERENCE   TO  REF (PINDX)                     CL**1
02331         MOVE 'Y'                 TO  PI-PYAJ-FILLED-SW               CL**1
02332         MOVE SPACE               TO  EL630-SENT-SW.                  CL**1
02333                                                                      CL**1
02334  6010-READ-NEXT.                                                     CL**1
02335                                                                      CL**1
02336      EXEC CICS READNEXT                                              CL**1
02337          SET      (ADDRESS OF PENDING-PAY-ADJ)                       CL**1
02338          DATASET  (PYAJ-FILE-ID)                                     CL**1
02339          RIDFLD   (ERPYAJ-KEY)                                       CL**1
02340      END-EXEC.                                                       CL**1
02341                                                                      CL**1
02342      IF PI-COMPANY-CD NOT = PY-COMPANY-CD                            CL**1
02343        AND PI-SAV-ENDING-PYAJ-KEY NOT = SPACES                       CL**1
02344          GO TO 6100-END-OF-FILE.                                     CL**1
02345                                                                      CL**1
02346      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES                 CL**1
02347          IF PY-CARRIER NOT = PI-CARRIER-SECURITY                     CL**1
02348              IF PI-SAV-ENDING-PYAJ-KEY NOT = SPACES                  CL**1
02349                  GO TO 6100-END-OF-FILE.                             CL**1
02350                                                                      CL**1
02351      IF PYAJ-1ST-READ                                                CL**1
02352        AND EIBAID NOT = DFHENTER                                     CL**1
02353        AND NOT PAGE-FULL                                             CL**1
02354          MOVE PY-CONTROL-PRIMARY  TO  PI-SAV-ENDING-PYAJ-KEY         CL**1
02355          MOVE PY-CARRIER          TO  PI-CR-CARRIER                  CL**1
02356          MOVE PY-GROUPING         TO  PI-CR-GROUPING                 CL**1
02357          MOVE PY-FIN-RESP         TO  PI-CR-FIN-RESP                 CL**1
02358          MOVE PY-ACCOUNT          TO  PI-CR-ACCOUNT.                 CL**1
02359                                                                      CL**1
02360      MOVE SPACE                  TO  PI-PYAJ-FILE-SW.                CL**1
02361                                                                      CL**1
02362      IF PI-COMPANY-CD   = PY-COMPANY-CD  AND                         CL**1
02363         PI-SAV-CARRIER  = PY-CARRIER     AND                         CL**1
02364         PI-SAV-GROUPING = PY-GROUPING    AND                         CL**1
02365         PI-SAV-FIN-RESP = PY-FIN-RESP    AND                         CL**1
02366         PI-SAV-ACCOUNT  = PY-ACCOUNT                                 CL**1
02367          NEXT SENTENCE                                               CL**1
02368      ELSE                                                            CL**1
02369          IF PYAJ-1ST-READ                                            CL**1
02370             AND NOT PAGE-FULL                                        CL**1
02371              MOVE 'Y'            TO  PI-PYAJ-FILE-SW                 CL**1
02372              MOVE SPACE          TO  PYAJ-READ-SW                    CL**1
02373              GO TO 6200-EXIT                                         CL**1
02374          ELSE                                                        CL**1
02375              MOVE 'A'            TO  PI-PYAJ-FILE-SW                 CL**1
02376              IF PINDX  IS LESS THAN  +3                              CL**1
02377                  SET PINDX  UP  BY  1                                CL**1
02378                  MOVE 'TOTAL'         TO  COMM (PINDX)               CL**1
02379                  MOVE TOTAL-ACCT-AMT  TO  AMTO (PINDX)               CL**1
02380                  MOVE 'NET TOTAL'     TO  REF  (PINDX)               CL**1
02381                  MOVE TOTAL-ACCT-NET  TO  NETO (PINDX)               CL**1
02382                  GO TO 6200-EXIT                                     CL**1
02383              ELSE                                                    CL**1
02384                  MOVE TOTAL-ACCT-AMT  TO  PI-SAV-ACCT-AMT            CL**1
02385                  MOVE TOTAL-ACCT-NET  TO  PI-SAV-ACCT-NET            CL**1
02386                  MOVE 'F'             TO  PI-PYAJ-FILE-SW            CL**1
02387                  GO TO 6200-EXIT.                                    CL**1
02388                                                                      CL**1
02389      IF PY-CREDIT-ACCEPT-DT NOT = LOW-VALUES                         CL**1
02390          GO TO 6000-READ-AND-FORMAT-SCREEN.                          CL**1
02391                                                                      CL**1
02392      SET PINDX  UP  BY +1.                                           CL**1
02393                                                                      CL**1
02394      SET WS-SAVE-INDEX-VALUE     TO  PINDX.                          CL**1
02395                                                                      CL**1
02396      MOVE TOTAL-ACCT-AMT      TO PI-SAV-ACCT-AMT.                    CL**1
02397      MOVE TOTAL-ACCT-NET      TO PI-SAV-ACCT-NET.                    CL**1
02398                                                                      CL**1
02399      IF PINDX  IS GREATER THAN  +3                                   CL**1
02400         MOVE 'F'                 TO  PI-PYAJ-FILE-SW                 CL**1
02401         GO TO 6200-EXIT.                                             CL**1
02402                                                                      CL**1
02403      IF PYAJ-1ST-READ                                                CL**1
02404          MOVE SPACE              TO  PYAJ-READ-SW.                   CL**1
02405                                                                      CL**1
02406      SET NDX                     TO  PINDX.                          CL**1
02407      SET WS-SAVE-NDX-VALUE       TO  NDX.                            CL**1
02408                                                                      CL**1
02409      MOVE PY-FILE-SEQ-NO         TO  PI-FILE-SEQ-NO (NDX).           CL**1
02410      MOVE PY-ENTRY-COMMENT       TO  COMM (PINDX).                   CL**1
02411      MOVE PY-ENTRY-AMT           TO  AMTO (PINDX).                   CL**1
02412                                                                      CL**1
02413      ADD PY-ENTRY-AMT            TO  TOTAL-ACCT-AMT.                 CL**1
02414                                                                      CL**1
02415      IF PY-RECORD-TYPE = 'R' OR 'D' OR 'S' OR 'T' OR 'Z'             CL**1
02416          ADD PY-ENTRY-AMT        TO  TOTAL-ACCT-NET                  CL**1
02417      ELSE                                                            CL**1
02418          SUBTRACT PY-ENTRY-AMT  FROM  TOTAL-ACCT-NET.                CL**1
02419                                                                      CL**1
02420      MOVE PY-RECORD-TYPE         TO  RTYPE (PINDX)                   CL**1
02421                                      PI-REC-TYPE (NDX).              CL**1
02422                                                                      CL**1
02423      MOVE PY-PMT-APPLIED         TO  PI-APPLIED (NDX).               CL**1
02424      MOVE PY-REF-NO              TO  PI-REFERENCE (NDX).             CL**1
02425      MOVE PY-BIL-INV             TO  PI-INVOICE (NDX).               CL**1
02426      MOVE PY-ENTRY-AMT           TO  PI-AMOUNT (NDX).                CL**1
02427                                                                      CL**1
02428      IF PY-RECORD-TYPE = 'G'                                         CL**1
02429         MOVE 'G'                 TO  PI-SPECIAL-GROUPING-SW          CL**1
02430      ELSE                                                            CL**1
02431         MOVE SPACE               TO  PI-SPECIAL-GROUPING-SW.         CL**1
02432                                                                      CL**1
02433      IF PY-PMT-APPLIED NOT = SPACE                                   CL**1
02434          MOVE PY-PMT-APPLIED     TO  APPLIED (PINDX).                CL**1
02435                                                                      CL**1
02436      IF PY-VOID-SW NOT = SPACE                                       CL**1
02437          MOVE PY-VOID-SW         TO  VOID-SW (PINDX).                CL**1
02438                                                                      CL**1
02439      IF PY-LAST-MAINT-DT = PREV-BIN-MAINT-DT                         CL**1
02440          MOVE PREV-MAINT-DT        TO  MDTE (PINDX)                  CL**1
02441      ELSE                                                            CL**1
02442          MOVE PY-LAST-MAINT-DT     TO  DC-BIN-DATE-1                 CL**1
02443                                        PREV-BIN-MAINT-DT             CL**1
02444          MOVE SPACE                TO  DC-OPTION-CODE                CL**1
02445          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                  CL**1
02446          MOVE DC-GREG-DATE-1-MDY   TO  MDTE (PINDX)                  CL**1
02447                                        PREV-MAINT-DT.                CL**1
02448                                                                      CL**1
02449      IF PY-AR-DATE NOT = LOW-VALUES                                  CL**1
02450          MOVE AL-SANOF           TO  COMM-ATTRB    (PINDX)           CL**1
02451                                      RTYPE-ATTRB   (PINDX)           CL**1
02452                                      AMT-ATTRB     (PINDX)           CL**1
02453                                      VOID-SW-ATTRB (PINDX)           CL**1
02454                                      APPLIED-ATTRB (PINDX)           CL**1
02455                                      REF-ATTRB     (PINDX)           CL**1
02456                                      INVOICE-ATTRB (PINDX)           CL**1
02457                                      IDTE-ATTRB    (PINDX)           CL**1
02458                                      CREDIT-ATTRB  (PINDX)           CL**1
02459                                      DEBIT-ATTRB   (PINDX)           CL**1
02460          IF PY-AR-DATE = PREV-BIN-BL-DT                              CL**1
02461              MOVE PREV-BL-DT           TO  BDTE (PINDX)              CL**1
02462          ELSE                                                        CL**1
02463              MOVE PY-AR-DATE           TO  DC-BIN-DATE-1             CL**1
02464                                            PREV-BIN-BL-DT            CL**1
02465              MOVE SPACE                TO  DC-OPTION-CODE            CL**1
02466              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT              CL**1
02467              MOVE DC-GREG-DATE-1-MDY   TO  BDTE (PINDX)              CL**1
02468                                            PREV-BL-DT.               CL**1
02469                                                                      CL**1
02470      IF PY-AR-DATE NOT = LOW-VALUES                                  CL**1
02471         IF MORTGAGE-SESSION                                          CL**1
02472             MOVE AL-SANON        TO  INVOICE-ATTRB (PINDX).          CL**1
02473                                                                      CL**1
02474      IF (PY-CHECK-ORIGIN-SW = SPACES OR LOW-VALUES) AND              CL**1
02475         (PY-GL-DATE = SPACES OR LOW-VALUES)                          CL**1
02476          NEXT SENTENCE                                               CL**1
02477      ELSE                                                            CL**1
02478          MOVE AL-SANOF           TO  RTYPE-ATTRB   (PINDX)           CL**1
02479                                      AMT-ATTRB     (PINDX)           CL**1
02480                                      VOID-SW-ATTRB (PINDX)           CL**1
02481                                      APPLIED-ATTRB (PINDX)           CL**1
02482                                      SDTE-ATTRB    (PINDX)           CL**1
02483                                      IDTE-ATTRB    (PINDX)           CL**1
02484                                      REF-ATTRB     (PINDX)           CL**1
02485                                      INVOICE-ATTRB (PINDX)           CL**1
02486                                      CREDIT-ATTRB  (PINDX)           CL**1
02487                                      DEBIT-ATTRB   (PINDX).          CL**1
02488                                                                      CL**1
02489      IF PI-PROCESSOR-ID = 'LGXX'                                     CL**1
02490          MOVE AL-UANOF           TO  COMM-ATTRB    (PINDX)           CL**1
02491                                      RTYPE-ATTRB   (PINDX)           CL**1
02492                                      AMT-ATTRB     (PINDX)           CL**1
02493                                      VOID-SW-ATTRB (PINDX)           CL**1
02494                                      APPLIED-ATTRB (PINDX)           CL**1
02495                                      SDTE-ATTRB    (PINDX)           CL**1
02496                                      REF-ATTRB     (PINDX)           CL**1
02497                                      INVOICE-ATTRB (PINDX)           CL**1
02498                                      IDTE-ATTRB    (PINDX)           CL**1
02499                                      CREDIT-ATTRB  (PINDX)           CL**1
02500                                      DEBIT-ATTRB   (PINDX).          CL**1
02501                                                                      CL**1
02502      IF PY-CREDIT-SELECT-DT NOT = LOW-VALUES                         CL**1
02503         MOVE PY-CREDIT-SELECT-DT  TO  DC-BIN-DATE-1                  CL**1
02504         MOVE SPACE                TO  DC-OPTION-CODE                 CL**1
02505         PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                   CL**1
02506         MOVE DC-GREG-DATE-1-MDY   TO  SDTE (PINDX).                  CL**1
02507                                                                      CL**1
02508      IF PY-INPUT-DT  NOT =  LOW-VALUES                               CL**1
02509          MOVE PY-INPUT-DT        TO  DC-BIN-DATE-1                   CL**1
02510          MOVE SPACE              TO  DC-OPTION-CODE                  CL**1
02511          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT                  CL**1
02512          MOVE DC-GREG-DATE-1-MDY                                     CL**1
02513                                  TO  IDTE (PINDX).                   CL**1
02514                                                                      CL**1
02515      MOVE PY-REF-NO              TO  REF     (PINDX).                CL**1
02516      MOVE PY-BIL-INV             TO  INVOICE (PINDX).                CL**1
02517      MOVE PY-GL-CR               TO  CREDIT  (PINDX).                CL**1
02518      MOVE PY-GL-DB               TO  DEBIT   (PINDX).                CL**1
02519                                                                      CL**1
02520      GO TO 6010-READ-NEXT.                                           CL**1
02521                                                                      CL**1
02522  6100-END-OF-FILE.                                                   CL**1
02523      MOVE 'X'                    TO  PI-PYAJ-FILE-SW.                CL**1
02524                                                                      CL**1
02525      IF PINDX  IS LESS THAN  +3                                      CL**1
02526          SET PINDX  UP  BY  1                                        CL**1
02527          MOVE 'TOTAL'            TO  COMM (PINDX)                    CL**1
02528          MOVE TOTAL-ACCT-AMT     TO  AMTO (PINDX)                    CL**1
02529          MOVE 'NET TOTAL'        TO  REF  (PINDX)                    CL**1
02530          MOVE TOTAL-ACCT-NET     TO  NETO (PINDX)                    CL**1
02531          MOVE 'Y'                TO  PI-TOTAL-DISPLAYED-SW           CL**1
02532      ELSE                                                            CL**1
02533          MOVE TOTAL-ACCT-AMT     TO  PI-SAV-ACCT-AMT                 CL**1
02534          MOVE TOTAL-ACCT-NET     TO  PI-SAV-ACCT-NET.                CL**1
02535                                                                      CL**1
02536  6200-EXIT.                                                          CL**1
02537      EXIT.                                                           CL**1
02538  EJECT                                                               CL**1
02539                                                                      CL**1
02540                                                                      CL**1
02541  6500-VERIFY-RECON-HEADER.                                           CL**1
02542                                                                      CL**1
02543      IF PY-REMIT-RECEIVED                                            CL**1
02544         GO TO 6590-EXIT.                                             CL**1
02545                                                                      CL**1
02546      MOVE PI-COMPANY-CD           TO MPPRCN-COMPANY-CD.              CL**1
02547      MOVE INVOICE (PINDX)         TO MPPRCN-INVOICE.                 CL**1
02548      MOVE +999999999              TO MPPRCN-RECORD-SEQU.             CL**1
02549                                                                      CL**1
02550      EXEC CICS HANDLE CONDITION                                      CL**1
02551          NOTFND   (6580-HEADER-NOTFND)                               CL**1
02552          END-EXEC.                                                   CL**1
02553                                                                      CL**1
02554      EXEC CICS READ                                                  CL**1
02555          DATASET   (MPPRCN-FILE-ID)                                  CL**1
02556          SET       (ADDRESS OF PAYMENT-RECONCILIATION)               CL**1
02557          RIDFLD    (MPPRCN-KEY)                                      CL**1
02558          END-EXEC.                                                   CL**1
02559                                                                      CL**1
02560      IF  NOT PI-NO-CARRIER-SECURITY                                  CL**1
02561              AND                                                     CL**1
02562          PI-CARRIER-SECURITY NOT EQUAL PR-CARRIER-A2                 CL**1
02563          MOVE -1                 TO CARRIERL                         CL**1
02564          MOVE AL-UANON           TO CARRIERA                         CL**1
02565          MOVE ER-9095            TO EMI-ERROR                        CL**1
02566          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**1
02567          GO TO 8200-SEND-DATAONLY.                                   CL**1
02568                                                                      CL**1
02569      MOVE PR-SECURITY-ACCESS-CODE                                    CL**1
02570                                  TO SC-SECURITY-ACCESS-CODE.         CL**1
02571      PERFORM 9920-PRODUCER-EVALUATION THRU 9920-EXIT.                CL**1
02572                                                                      CL**1
02573      IF  SC-PRODUCER-NOT-AUTHORIZED                                  CL**1
02574          MOVE -1                 TO ACCTL                            CL**1
02575          MOVE AL-UANON           TO ACCTA                            CL**1
02576          MOVE ER-9094            TO EMI-ERROR                        CL**1
02577          GO TO 8200-SEND-DATAONLY.                                   CL**1
02578                                                                      CL**1
02579      IF PR-HDR-POSTED                                                CL**1
02580         GO TO 6560-POST-ERROR.                                       CL**1
02581                                                                      CL**1
02582      GO TO 6590-EXIT.                                                CL**1
02583                                                                      CL**1
02584  6560-POST-ERROR.                                                    CL**1
02585                                                                      CL**1
02586      MOVE -1                     TO INVOICE-LEN (PINDX)              CL**1
02587      MOVE ER-9296                TO EMI-ERROR.                       CL**1
02588      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**1
02589                                                                      CL**1
02590      GO TO 6590-EXIT.                                                CL**1
02591                                                                      CL**1
02592  6580-HEADER-NOTFND.                                                 CL**1
02593                                                                      CL**1
02594      MOVE -1                     TO INVOICE-LEN (PINDX)              CL**1
02595      MOVE ER-9280                TO EMI-ERROR.                       CL**1
02596      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**1
02597                                                                      CL**1
02598  6590-EXIT.                                                          CL**1
02599      EXIT.                                                           CL**1
02600                                                                      CL**1
02601      EJECT                                                           CL**1
02602                                                                      CL**1
02603  6600-UPDATE-RECON-HEADER.                                           CL**1
02604                                                                      CL**1
02605      IF (PY-REMIT-RECEIVED OR PY-REMIT-IND-GROUPING)                 CL**1
02606         NEXT SENTENCE                                                CL**1
02607      ELSE                                                            CL**1
02608         GO TO 6690-EXIT.                                             CL**1
02609                                                                      CL**1
02610      MOVE PI-COMPANY-CD           TO MPPRCN-COMPANY-CD.              CL**1
02611      MOVE PY-BIL-INV              TO MPPRCN-INVOICE.                 CL**1
02612      MOVE +999999999              TO MPPRCN-RECORD-SEQU.             CL**1
02613                                                                      CL**1
02614      EXEC CICS HANDLE CONDITION                                      CL**1
02615          NOTFND   (6680-HEADER-NOTFND)                               CL**1
02616          END-EXEC.                                                   CL**1
02617                                                                      CL**1
02618      EXEC CICS READ                                                  CL**1
02619          DATASET   (MPPRCN-FILE-ID)                                  CL**1
02620          SET       (ADDRESS OF PAYMENT-RECONCILIATION)               CL**1
02621          RIDFLD    (MPPRCN-KEY)                                      CL**1
02622          UPDATE                                                      CL**1
02623          END-EXEC.                                                   CL**1
02624                                                                      CL**1
02625      IF  NOT PI-NO-CARRIER-SECURITY                                  CL**1
02626              AND                                                     CL**1
02627          PI-CARRIER-SECURITY NOT EQUAL PR-CARRIER-A2                 CL**1
02628          MOVE -1                 TO CARRIERL                         CL**1
02629          MOVE AL-UANON           TO CARRIERA                         CL**1
02630          MOVE ER-9095            TO EMI-ERROR                        CL**1
02631          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**1
02632          GO TO 8200-SEND-DATAONLY.                                   CL**1
02633                                                                      CL**1
02634      MOVE PR-SECURITY-ACCESS-CODE                                    CL**1
02635                                  TO SC-SECURITY-ACCESS-CODE.         CL**1
02636      PERFORM 9920-PRODUCER-EVALUATION THRU 9920-EXIT.                CL**1
02637                                                                      CL**1
02638      IF  SC-PRODUCER-NOT-AUTHORIZED                                  CL**1
02639          MOVE -1                 TO ACCTL                            CL**1
02640          MOVE AL-UANON           TO ACCTA                            CL**1
02641          MOVE ER-9094            TO EMI-ERROR                        CL**1
02642          GO TO 8200-SEND-DATAONLY.                                   CL**1
02643                                                                      CL**1
02644      IF PR-HDR-POSTED                                                CL**1
02645         GO TO 6660-POST-ERROR.                                       CL**1
02646                                                                      CL**1
02647      IF WS-RECON-DELETE                                              CL**1
02648         SUBTRACT WS-ENTRY-AMT  FROM PR-RECEIVED-PREMIUM.             CL**1
02649                                                                      CL**1
02650      IF WS-RECON-UPDATE                                              CL**1
02651         SUBTRACT WS-OLD-ENTRY-AMT FROM PR-RECEIVED-PREMIUM           CL**1
02652         ADD      WS-ENTRY-AMT     TO   PR-RECEIVED-PREMIUM.          CL**1
02653                                                                      CL**1
02654      IF WS-RECON-ADD                                                 CL**1
02655         ADD  WS-ENTRY-AMT        TO PR-RECEIVED-PREMIUM.             CL**1
02656                                                                      CL**1
02657      MOVE WS-CURRENT-BIN-DT      TO PR-RECEIVED-DT.                  CL**1
02658                                                                      CL**1
02659      MOVE PI-PROCESSOR-ID       TO PR-LAST-CHANGE-PROCESSOR.         CL**1
02660      MOVE EIBTIME               TO PR-LAST-CHANGE-TIME.              CL**1
02661      MOVE WS-CURRENT-BIN-DT     TO PR-LAST-CHANGE-DT.                CL**1
02662                                                                      CL**1
02663      EXEC CICS REWRITE                                               CL**1
02664          DATASET   (MPPRCN-FILE-ID)                                  CL**1
02665          FROM      (PAYMENT-RECONCILIATION)                          CL**1
02666          END-EXEC.                                                   CL**1
02667                                                                      CL**1
02668      GO TO 6690-EXIT.                                                CL**1
02669                                                                      CL**1
02670  6660-POST-ERROR.                                                    CL**1
02671                                                                      CL**1
02672      EXEC CICS SYNCPOINT ROLLBACK                                    CL**1
02673      END-EXEC.                                                       CL**1
02674                                                                      CL**1
02675      MOVE -1                     TO INVOICE-LEN (PINDX)              CL**1
02676      MOVE ER-9296                TO EMI-ERROR.                       CL**1
02677      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**1
02678                                                                      CL**1
02679      GO TO 8200-SEND-DATAONLY.                                       CL**1
02680                                                                      CL**1
02681  6680-HEADER-NOTFND.                                                 CL**1
02682                                                                      CL**1
02683      EXEC CICS SYNCPOINT ROLLBACK                                    CL**1
02684      END-EXEC.                                                       CL**1
02685                                                                      CL**1
02686      MOVE -1                     TO INVOICE-LEN (PINDX).             CL**1
02687      MOVE ER-9280                TO EMI-ERROR.                       CL**1
02688      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.                      CL**1
02689                                                                      CL**1
02690      GO TO 8200-SEND-DATAONLY.                                       CL**1
02691                                                                      CL**1
02692  6690-EXIT.                                                          CL**1
02693      EXIT.                                                           CL**1
02694                                                                      CL**1
02695      EJECT                                                           CL**1
02696                                                                      CL**1
02697  7000-PYAJ-FILE-NOTOPEN.                                             CL**1
02698      MOVE -1                     TO  MAINTL.                         CL**1
02699                                                                      CL**1
02700      IF  MORTGAGE-SESSION                                            CL**1
02701          MOVE ER-9374            TO  EMI-ERROR                       CL**1
02702                                                                      CL**1
02703      ELSE                                                            CL**1
02704          MOVE ER-2232            TO  EMI-ERROR.                      CL**1
02705                                                                      CL**1
02706      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
02707                                                                      CL**1
02708      GO TO 8200-SEND-DATAONLY.                                       CL**1
02709                                                                      CL**1
02710  7100-COMP-FILE-NOTOPEN.                                             CL**1
02711      MOVE -1                     TO  MAINTL.                         CL**1
02712      MOVE ER-2233                TO  EMI-ERROR.                      CL**1
02713                                                                      CL**1
02714      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
02715                                                                      CL**1
02716      GO TO 8200-SEND-DATAONLY.                                       CL**1
02717                                                                      CL**1
02718  7200-RECV-FILE-NOTOPEN.                                             CL**1
02719      MOVE -1                     TO  MAINTL.                         CL**1
02720      MOVE ER-3177                TO  EMI-ERROR.                      CL**1
02721                                                                      CL**1
02722      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.                     CL**1
02723                                                                      CL**1
02724      GO TO 8200-SEND-DATAONLY.                                       CL**1
02725  EJECT                                                               CL**1
02726  8100-SEND-INITIAL-MAP.                                              CL**1
02727      MOVE WS-CURRENT-DT          TO  DATEO.                          CL**1
02728      MOVE EIBTIME                TO  TIME-IN.                        CL**1
02729      MOVE TIME-OUT               TO  TIMEO.                          CL**1
02730      MOVE -1                     TO  MAINTL.                         CL**1
02731      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                       CL**1
02732      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                       CL**1
02733                                                                      CL**1
02734      IF MORTGAGE-SESSION                                             CL**1
02735         MOVE ' PRODUCER:'        TO ACCTHDGO                         CL**1
02736         MOVE AL-SABOF            TO ACCTHDGA.                        CL**1
02737                                                                      CL**1
02738      IF NOT CREDIT-SESSION                                           CL**1
02739         MOVE AL-SADOF            TO PFGABILA, PFACBILA.              CL**1
02740                                                                      CL**1
02741      IF EIBTRNID = TRANS-ID                                          CL**1
02742        OR EL640-TRANS-ID                                             CL**1
02743        OR EL642-TRANS-ID                                             CL**1
02744        OR EL652-TRANS-ID                                             CL**1
02745        OR EL6351-TRANS-ID                                            CL**1
02746        OR EL630-TRANS-ID                                             CL**1
02747          IF PI-SAV-COMP-CONTROL GREATER THAN SPACES                  CL**1
02748              MOVE PI-SAV-FUNCTION  TO  MAINTI                        CL**1
02749              MOVE PI-SAV-CARRIER   TO  CARRIERO                      CL**1
02750              MOVE PI-SAV-GROUPING  TO  GROUPO                        CL**1
02751              MOVE PI-SAV-FIN-RESP  TO  FINRESPO                      CL**1
02752              MOVE PI-SAV-ACCOUNT   TO  ACCTO                         CL**1
02753              MOVE AL-UANON         TO  MAINTA                        CL**1
02754                                        CARRIERA                      CL**1
02755                                        GROUPA                        CL**1
02756                                        FINRESPA                      CL**1
02757                                        ACCTA                         CL**1
02758          ELSE                                                        CL**1
02759              NEXT SENTENCE.                                          CL**1
02760                                                                      CL**1
02761      MOVE WS-EMHD1               TO  EMHD1O                          CL**1
02762      MOVE AL-SABOF               TO  EMHD1A                          CL**1
02763      MOVE WS-EMHD2               TO  EMHD2O                          CL**1
02764      MOVE AL-SABOF               TO  EMHD2A.                         CL**1
02765                                                                      CL**1
02766      EXEC CICS SEND                                                  CL**1
02767          MAP     (MAP-NAME)                                          CL**1
02768          MAPSET  (MAPSET-NAME)                                       CL**1
02769          FROM    (EL635AO)                                           CL**1
02770          ERASE                                                       CL**1
02771          CURSOR                                                      CL**1
02772          END-EXEC.                                                   CL**1
02773                                                                      CL**1
02774      GO TO 9100-RETURN-TRAN.                                         CL**1
02775  EJECT                                                               CL**1
02776  8200-SEND-DATAONLY.                                                 CL**1
02777      MOVE WS-CURRENT-DT          TO  DATEO.                          CL**1
02778      MOVE EIBTIME                TO  TIME-IN.                        CL**1
02779      MOVE TIME-OUT               TO  TIMEO.                          CL**1
02780      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                       CL**1
02781      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.                       CL**1
02782                                                                      CL**1
02783      IF NOT CREDIT-SESSION                                           CL**1
02784         MOVE AL-SADOF            TO PFGABILA, PFACBILA.              CL**1
02785                                                                      CL**1
02786      EXEC CICS SEND                                                  CL**1
02787          MAP     (MAP-NAME)                                          CL**1
02788          MAPSET  (MAPSET-NAME)                                       CL**1
02789          FROM    (EL635AO)                                           CL**1
02790          DATAONLY                                                    CL**1
02791          CURSOR                                                      CL**1
02792      END-EXEC.                                                       CL**1
02793                                                                      CL**1
02794      GO TO 9100-RETURN-TRAN.                                         CL**1
02795                                                                      CL**1
02796  8300-SEND-TEXT.                                                     CL**1
02797      EXEC CICS SEND TEXT                                             CL**1
02798          FROM    (LOGOFF-TEXT)                                       CL**1
02799          LENGTH  (LOGOFF-LENGTH)                                     CL**1
02800          ERASE                                                       CL**1
02801          FREEKB                                                      CL**1
02802      END-EXEC.                                                       CL**1
02803                                                                      CL**1
02804      EXEC CICS RETURN                                                CL**1
02805      END-EXEC.                                                       CL**1
02806  EJECT                                                               CL**1
02807  8400-LOG-JOURNAL-RECORD.                                            CL**1
02808 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                     CL**1
02809 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.                  CL**1
02810                                                                      CL**1
02811 *    EXEC CICS JOURNAL                                               CL**1
02812 *        JFILEID  (PI-JOURNAL-FILE-ID)                               CL**1
02813 *        JTYPEID  ('EL')                                             CL**1
02814 *        FROM     (JOURNAL-RECORD)                                   CL**1
02815 *        LENGTH   (223)                                              CL**1
02816 *        END-EXEC.                                                   CL**1
02817                                                                      CL**1
02818  8400-EXIT.                                                          CL**1
02819      EXIT.                                                           CL**1
02820                                                                      CL**1
02821  8500-DATE-CONVERT.                                                  CL**1
02822      EXEC CICS LINK                                                  CL**1
02823          PROGRAM   (LINK-CLDATCV)                                    CL**1
02824          COMMAREA  (DATE-CONVERSION-DATA)                            CL**1
02825          LENGTH    (DC-COMM-LENGTH)                                  CL**1
02826      END-EXEC.                                                       CL**1
02827                                                                      CL**1
02828  8500-EXIT.                                                          CL**1
02829      EXIT.                                                           CL**1
02830                                                                      CL**1
02831  EJECT                                                               CL**1
02832  8800-UNAUTHORIZED-ACCESS.                                           CL**1
02833      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                     CL**1
02834                                                                      CL**1
02835      GO TO 8300-SEND-TEXT.                                           CL**1
02836                                                                      CL**1
02837  8810-PF23.                                                          CL**1
02838      MOVE EIBAID                 TO  PI-ENTRY-CD-1.                  CL**1
02839      MOVE XCTL-005               TO  PGM-NAME.                       CL**1
02840                                                                      CL**1
02841      GO TO 9300-XCTL.                                                CL**1
02842                                                                      CL**1
02843  9000-RETURN-CICS.                                                   CL**1
02844      EXEC CICS RETURN                                                CL**1
02845          END-EXEC.                                                   CL**1
02846                                                                      CL**1
02847  9100-RETURN-TRAN.                                                   CL**1
02848      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.               CL**1
02849      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.           CL**1
02850                                                                      CL**1
02851      EXEC CICS RETURN                                                CL**1
02852          TRANSID   (TRANS-ID)                                        CL**1
02853          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                         CL**1
02854          LENGTH    (PI-COMM-LENGTH)                                  CL**1
02855      END-EXEC.                                                       CL**1
02856                                                                      CL**1
02857  9200-RETURN-MAIN-MENU.                                              CL**1
02858      MOVE XCTL-626               TO  PGM-NAME.                       CL**1
02859                                                                      CL**1
02860      GO TO 9300-XCTL.                                                CL**1
02861                                                                      CL**1
02862  9300-XCTL.                                                          CL**1
02863      EXEC CICS XCTL                                                  CL**1
02864          PROGRAM   (PGM-NAME)                                        CL**1
02865          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                         CL**1
02866          LENGTH    (PI-COMM-LENGTH)                                  CL**1
02867      END-EXEC.                                                       CL**1
02868                                                                      CL**1
02869  9400-CLEAR.                                                         CL**1
02870      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                       CL**1
02871                                                                      CL**1
02872      IF PI-RETURN-TO-PROGRAM = 'EL626'                               CL**1
02873          MOVE SPACES             TO  PI-CR-CONTROL-IN-PROGRESS       CL**1
02874                                      PI-SAV-COMP-CONTROL.            CL**1
02875                                                                      CL**1
02876      GO TO 9300-XCTL.                                                CL**1
02877                                                                      CL**1
02878  9500-PF12.                                                          CL**1
02879      MOVE XCTL-010               TO  PGM-NAME.                       CL**1
02880                                                                      CL**1
02881      GO TO 9300-XCTL.                                                CL**1
02882                                                                      CL**1
02883  9600-PGMID-ERROR.                                                   CL**1
02884      EXEC CICS HANDLE CONDITION                                      CL**1
02885          PGMIDERR  (8300-SEND-TEXT)                                  CL**1
02886      END-EXEC.                                                       CL**1
02887                                                                      CL**1
02888      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.             CL**1
02889      MOVE ' '                    TO  PI-ENTRY-CD-1.                  CL**1
02890      MOVE XCTL-005               TO  PGM-NAME.                       CL**1
02891      MOVE PGM-NAME               TO  LOGOFF-PGM.                     CL**1
02892      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                    CL**1
02893                                                                      CL**1
02894      GO TO 9300-XCTL.                                                CL**1
02895                                                                      CL**1
02896  9900-ERROR-FORMAT.                                                  CL**1
02897      IF NOT EMI-ERRORS-COMPLETE                                      CL**1
02898          MOVE LINK-001           TO  PGM-NAME                        CL**1
02899          EXEC CICS LINK                                              CL**1
02900              PROGRAM   (PGM-NAME)                                    CL**1
02901              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)               CL**1
02902              LENGTH    (EMI-COMM-LENGTH)                             CL**1
02903          END-EXEC.                                                   CL**1
02904                                                                      CL**1
02905  9900-EXIT.                                                          CL**1
02906      EXIT.                                                           CL**1
02907      EJECT                                                           CL**1
02908  9910-INITIALIZE-SECURITY.                                           CL**1
02909 ******************************************************************   CL**1
02910 *                                                                *   CL**1
02911 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *   CL**1
02912 *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *   CL**1
02913 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *   CL**1
02914 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *   CL**1
02915 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *   CL**1
02916 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *   CL**1
02917 *                                                                *   CL**1
02918 ******************************************************************   CL**1
02919                                                                      CL**1
02920      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'                            CL**1
02921          IF  MORTGAGE-SESSION                                        CL**1
02922              MOVE '125E'             TO SC-QUID-SYSTEM               CL**1
02923              MOVE EIBTRMID           TO SC-QUID-TERMINAL             CL**1
02924                                                                      CL**1
02925              EXEC CICS READQ TS                                      CL**1
02926                  QUEUE  (SC-QUID-KEY)                                CL**1
02927                  INTO   (SECURITY-CONTROL-E)                         CL**1
02928                  LENGTH (SC-COMM-LENGTH-E)                           CL**1
02929                  ITEM   (SC-ITEM)                                    CL**1
02930              END-EXEC                                                CL**1
02931                                                                      CL**1
02932              MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)                   CL**1
02933                                      TO PI-DISPLAY-CAP               CL**1
02934              MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)                    CL**1
02935                                      TO PI-MODIFY-CAP                CL**1
02936              IF  NOT DISPLAY-CAP                                     CL**1
02937                  MOVE 'READ'         TO SM-READ                      CL**1
02938                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT      CL**1
02939                  MOVE ER-9097        TO EMI-ERROR                    CL**1
02940                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**1
02941                  GO TO 8100-SEND-INITIAL-MAP                         CL**1
02942              ELSE                                                    CL**1
02943                  GO TO 9910-EXIT                                     CL**1
02944          ELSE                                                        CL**1
02945              EXEC CICS  READQ TS                                     CL**1
02946                  QUEUE   (PI-SECURITY-TEMP-STORE-ID)                 CL**1
02947                  INTO    (SECURITY-CONTROL)                          CL**1
02948                  LENGTH  (SC-COMM-LENGTH)                            CL**1
02949                  ITEM    (SC-ITEM)                                   CL**1
02950                  END-EXEC                                            CL**1
02951                                                                      CL**1
02952              MOVE SC-CREDIT-DISPLAY (15)                             CL**1
02953                                  TO PI-DISPLAY-CAP                   CL**1
02954              MOVE SC-CREDIT-UPDATE  (15)                             CL**1
02955                                  TO PI-MODIFY-CAP                    CL**1
02956                                                                      CL**1
02957              IF  NOT DISPLAY-CAP                                     CL**1
02958                  MOVE 'READ'     TO SM-READ                          CL**1
02959                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT      CL**1
02960                  MOVE ER-0070    TO  EMI-ERROR                       CL**1
02961                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**1
02962                  GO TO 8100-SEND-INITIAL-MAP.                        CL**1
02963                                                                      CL**1
02964  9910-EXIT.                                                          CL**1
02965      EXIT.                                                           CL**1
02966      EJECT                                                           CL**1
02967  9920-PRODUCER-EVALUATION.                                           CL**1
02968 ******************************************************************   CL**1
02969 *                                                                *   CL**1
02970 *       THIS AREA CONTAINS THE LOGIC NECESSARY TO CHECK THE      *   CL**1
02971 *       AUTHORIZATION OF THE USER TO 'ACCESS' A PRODUCER.        *   CL**1
02972 *       IF THE USER IS NOT AUTHORIZED AN APPROPRIATE MESSAGE     *   CL**1
02973 *       IS GENERATED.                                            *   CL**1
02974 *                                                                *   CL**1
02975 ******************************************************************   CL**1
02976                                                                      CL**1
02977      MOVE SPACES                 TO SC-PRODUCER-AUTHORIZED-SW.       CL**1
02978                                                                      CL**1
02979      IF  PI-NO-PRODUCER-SECURITY                                     CL**1
02980              OR                                                      CL**1
02981          PI-PROCESSOR-ID EQUAL 'LGXX'                                CL**1
02982          GO TO 9920-EXIT.                                            CL**1
02983                                                                      CL**1
02984      SET PI-ACCESS-NDX           TO +1                               CL**1
02985                                                                      CL**1
02986      SEARCH PI-ACCESS-CODE                                           CL**1
02987          VARYING PI-ACCESS-NDX                                       CL**1
02988                                                                      CL**1
02989          AT END                                                      CL**1
02990              MOVE 'N'            TO SC-PRODUCER-AUTHORIZED-SW        CL**1
02991                                                                      CL**1
02992          WHEN                                                        CL**1
02993              PI-ACCESS-CODE (PI-ACCESS-NDX)                          CL**1
02994                  EQUAL SC-SECURITY-ACCESS-CODE                       CL**1
02995              GO TO 9920-EXIT.                                        CL**1
02996                                                                      CL**1
02997  9920-EXIT.                                                          CL**1
02998      EXIT.                                                           CL**1
02999      EJECT                                                           CL**1
03000                                                                      CL**1
03001  9990-ABEND.                                                         CL**1
03002      MOVE LINK-004               TO  PGM-NAME.                       CL**1
03003      MOVE DFHEIBLK               TO  EMI-LINE1.                      CL**1
03004                                                                      CL**1
03005      EXEC CICS LINK                                                  CL**1
03006          PROGRAM   (PGM-NAME)                                        CL**1
03007          COMMAREA  (EMI-LINE1)                                       CL**1
03008          LENGTH    (72)                                              CL**1
03009      END-EXEC.                                                       CL**1
03010                                                                      CL**1
03011      MOVE -1                     TO  PFENTERL.                       CL**1
03012                                                                      CL**1
03013      GO TO 8200-SEND-DATAONLY.                                       CL**1
03014                                                                      CL**1
03015      MOVE ZEROS  TO RETURN-CODE.
03015      GOBACK.                                                         CL**1
03016                                                                      CL**1
03017  9995-SECURITY-VIOLATION.                                            CL**1
03018                              COPY ELCSCTP.                           CL**1
03019                                                                      CL**1
03020  9995-EXIT.                                                          CL**1
03021      EXIT.                                                           CL**1
