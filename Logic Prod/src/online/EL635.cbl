00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL635 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 06/29/94 10:27:57.
00007 *                            VMOD=2.042
00008 *
00009 ******************************************************************
00010 ******************************************************************
00011 *                                                                *
00012 *         THIS IS PROGRAM IS USED BY BOTH THE CREDIT AND         *
00013 *         MORTGAGE SYSTEMS.  ANY CHANGES MADE, SHOULD ONLY       *
00014 *         BE MADE AFTER CONSIDERING WHAT IMPACT THEY MAY         *
00015 *         HAVE ON EITHER SYSTEM.                                 *
00016 *                                                                *
00017 ******************************************************************
00018 ******************************************************************
00019
00020 *AUTHOR.        LOGIC,INC.
00021 *               DALLAS, TEXAS.
00022
00023 *DATE-COMPILED.
00024
00025 *SECURITY.   *****************************************************
00026 *            *                                                   *
00027 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00028 *            *                                                   *
00029 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00030 *                                                                *
00031 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00032 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00033 *            *                                                   *
00034 *            *****************************************************
00035
00036 *REMARKS.
00037 *        TRANSACTION - EXJ4 - COMPENSATION PAYMENTS/ADJUSTMENTS.
00038
00039  ENVIRONMENT DIVISION.
00040  DATA DIVISION.
00041  EJECT
00042  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00043  77  FILLER  PIC X(32)  VALUE '********************************'.
00044  77  FILLER  PIC X(32)  VALUE '*    EL635 WORKING STORAGE     *'.
00045  77  FILLER  PIC X(32)  VALUE '**********  VMOD=2.042 *********'.
00046
00047 *    COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
00048 *    COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
00049 *    COPY MPCSCRT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCSCRT                             *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        ACQUIRED BY SIGN-ON PROGRAM EL125.                      *
00008 *                                      (MP MORTGAGE PROTECTION)  *
00009 *                                                                *
00010 ******************************************************************
00011 *
00012  01  SECURITY-CONTROL-E.
00013      12  SC-COMM-LENGTH-E             PIC S9(04) VALUE +144 COMP.
00014      12  FILLER                       PIC  X(02) VALUE 'SC'.
00015      12  SC-QUID-KEY.
00016          16  SC-QUID-TERMINAL         PIC  X(04).
00017          16  SC-QUID-SYSTEM           PIC  X(04).
00018      12  SC-ITEM                      PIC S9(04) VALUE +1   COMP.
00019      12  SC-SECURITY-ACCESS-CODE      PIC  X(01).
00020      12  SC-PRODUCER-AUTHORIZED-SW    PIC  X(01).
00021          88 SC-PRODUCER-AUTHORIZED               VALUE ' '.
00022          88 SC-PRODUCER-NOT-AUTHORIZED           VALUE 'N'.
00023      12  SC-MP-CODES.
00024          16  SC-MP-AUTHORIZATION OCCURS 44 TIMES.
00025              20  SC-MP-DISPLAY        PIC  X(01).
00026              20  SC-MP-UPDATE         PIC  X(01).
00027      12  FILLER                       PIC  X(40).
00050
00051     EJECT
00052
00053  01  STANDARD-AREAS.
00054      12  W-APPL-SCRTY-NDX    PIC S9(4) COMP  VALUE +38.
00055      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
00056      12  MAP-NAME            PIC  X(8)       VALUE 'EL635A'.
00057      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL635S'.
00058      12  SCREEN-NUMBER       PIC  X(4)       VALUE '633A'.
00059      12  TRANS-ID            PIC  X(4)       VALUE 'EXJ4'.
00060      12  EL630-TRANS-ID      PIC  X(4)       VALUE 'EXA5'.
00061      12  EL640-TRANS-ID      PIC  X(4)       VALUE 'EXC1'.
00062      12  EL642-TRANS-ID      PIC  X(4)       VALUE 'EXH7'.
00063      12  EL652-TRANS-ID      PIC  X(4)       VALUE 'EXD4'.
00064      12  EL6351-TRANS-ID     PIC  X(4)       VALUE 'EXJ5'.
00065      12  THIS-PGM            PIC  X(8)       VALUE 'EL635'.
00066      12  PGM-NAME            PIC  X(8).
00067      12  TIME-IN             PIC S9(7).
00068      12  TIME-OUT-R  REDEFINES  TIME-IN.
00069          16  FILLER          PIC  X.
00070          16  TIME-OUT        PIC  99V99.
00071          16  FILLER          PIC  XX.
00072      12  EL640               PIC  X(8)       VALUE 'EL640'.
00073      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00074      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00075      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
00076      12  XCTL-EM626          PIC  X(8)       VALUE 'EM626'.
00077      12  XCTL-652            PIC  X(8)       VALUE 'EL652'.
00078      12  XCTL-6351           PIC  X(8)       VALUE 'EL6351'.
00079      12  XCTL-640            PIC  X(8)       VALUE 'EL640'.
00080      12  XCTL-642            PIC  X(8)       VALUE 'EL642'.
00081      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00082      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00083      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00084      12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.
00085      12  MPPYAJ-FILE-ID      PIC  X(8)       VALUE 'MPPYAJ'.
00086      12  MPPRCN-FILE-ID      PIC  X(8)       VALUE 'MPPRCN'.
00087      12  CHKQ-FILE-ID        PIC  X(8)       VALUE 'ERCHKQ'.
00088      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.
00089      12  RECV-FILE-ID        PIC  X(8)       VALUE 'ERRECV'.
00090      12  RETURNED-FROM       PIC  X(8)       VALUE SPACES.
00091      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.
00092      12  WS-CURRENT-MDY      PIC  X(6)       VALUE SPACES.
00093      12  WS-CURRENT-BIN-DT   PIC  XX         VALUE SPACES.
00094      12  WS-COMMENT-FULL.
00095          16  WS-COMMENT-24-POS  PIC  X(24)   VALUE SPACES.
00096          16  FILLER             PIC  X(6)    VALUE SPACES.
00097      12  EL630-SENT-SW       PIC  X          VALUE SPACE.
00098          88  SENT-FROM-PENDING               VALUE 'Y'.
00099      12  PYAJ-READ-SW        PIC  X          VALUE 'Y'.
00100          88  PYAJ-1ST-READ                   VALUE 'Y'.
00101      12  WARNING-SW          PIC  X          VALUE 'N'.
00102      12  WS-SECOND-READ      PIC  X          VALUE '1'.
00103      12  ZERO-NDX            PIC  9          VALUE ZERO.
00104      12  WORK-SEQ-NO         PIC S9(9)  COMP-3  VALUE ZERO.
00105      12  WS-CURSOR-POS-1     PIC S9(4)  COMP VALUE +721.
00106      12  WS-CURSOR-POS-2     PIC S9(4)  COMP VALUE +1041.
00107      12  WS-CURSOR-POS-3     PIC S9(4)  COMP VALUE +1361.
00108      12  WS-EDIT-PATTERN     PIC 9(7)V99     VALUE ZERO.
00109      12  WS-EDIT  REDEFINES  WS-EDIT-PATTERN PIC X(9).
00110      12  WS-EDITED-AMOUNT.
00111          16  WS-EDIT-AMOUNT  PIC X(9)        VALUE ZERO.
00112          16  WS-EDIT-SIGN    PIC X           VALUE ' '.
00113      12  WS-EMHD1            PIC  X(6)       VALUE 'EOM DT'.
00114      12  WS-EMHD2.
00115          16  FILLER          PIC  X(20)      VALUE
00116                  '       CREDIT - DEBI'.
00117          16  FILLER          PIC  X(20)      VALUE
00118                  'T         TYPE    AM'.
00119          16  FILLER          PIC  X(20)      VALUE
00120                  'OUNT    G/A VOID MAI'.
00121          16  FILLER          PIC  X(19)      VALUE
00122                  'NT DT BILLED INP DT'.
00123      12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.
00124          88  VALID-MTG-REC-TYPE              VALUE  'R' 'D' 'C'
00125                                                     'S' 'T' 'U'
00126                                                     'X' 'Y' 'Z'
00127                                                     'F'
00128                                                     'G'.
00129          88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'
00130                                                     'S' 'T' 'U'
00131                                                     'X' 'Y' 'Z'
00132                                                     'F'.
00133
00134          88  MON-VALID-REC-TYPE              VALUE  'R' 'D' 'C'
00135                                                     'Z'.
00136          88  ANL-VALID-REC-TYPE              VALUE  'R' 'D' 'C'
00137                                                     'S' 'T' 'U'
00138                                                     'X' 'Y' 'Z'.
00139          88  NCL-VALID-REC-TYPE              VALUE  'R' 'D' 'C'
00140                                                     'X' 'Y' 'Z'.
00141
00142      12  FORCE-SHOW-SW       PIC  X          VALUE SPACE.
00143          88  FORCE-SHOW                      VALUE  'Y'.
00144      12  OVERWRITE-AGENT-SW  PIC  X          VALUE SPACE.
00145          88  OVERWRITE-AGENT                 VALUE  'Y'.
00146
00147      12  WS-RECON-SW         PIC  X          VALUE SPACE.
00148          88  WS-RECON-ADD                    VALUE 'A'.
00149          88  WS-RECON-UPDATE                 VALUE 'U'.
00150          88  WS-RECON-DELETE                 VALUE 'D'.
00151
00152      EJECT
00153      12  WS-ERROR-CODES.
00154          16  ER-0000         PIC  X(4)       VALUE '0000'.
00155          16  ER-0008         PIC  X(4)       VALUE '0008'.
00156          16  ER-0023         PIC  X(4)       VALUE '0023'.
00157          16  ER-0029         PIC  X(4)       VALUE '0029'.
00158          16  ER-0070         PIC  X(4)       VALUE '0070'.
00159          16  ER-0587         PIC  X(4)       VALUE '0587'.
00160          16  ER-0714         PIC  X(4)       VALUE '0714'.
00161          16  ER-0761         PIC  X(4)       VALUE '0761'.
00162          16  ER-2056         PIC  X(4)       VALUE '2056'.
00163          16  ER-2231         PIC  X(4)       VALUE '2231'.
00164          16  ER-2232         PIC  X(4)       VALUE '2232'.
00165          16  ER-2233         PIC  X(4)       VALUE '2233'.
00166          16  ER-2234         PIC  X(4)       VALUE '2234'.
00167          16  ER-2235         PIC  X(4)       VALUE '2235'.
00168          16  ER-2236         PIC  X(4)       VALUE '2236'.
00169          16  ER-2237         PIC  X(4)       VALUE '2237'.
00170          16  ER-2238         PIC  X(4)       VALUE '2238'.
00171          16  ER-2239         PIC  X(4)       VALUE '2239'.
00172          16  ER-2244         PIC  X(4)       VALUE '2244'.
00173          16  ER-2245         PIC  X(4)       VALUE '2245'.
00174          16  ER-2246         PIC  X(4)       VALUE '2246'.
00175          16  ER-2370         PIC  X(4)       VALUE '2370'.
00176          16  ER-2432         PIC  X(4)       VALUE '2432'.
00177          16  ER-2449         PIC  X(4)       VALUE '2449'.
00178          16  ER-2587         PIC  X(4)       VALUE '2587'.
00179          16  ER-2588         PIC  X(4)       VALUE '2588'.
00180          16  ER-2595         PIC  X(4)       VALUE '2595'.
00181          16  ER-2596         PIC  X(4)       VALUE '2596'.
00182          16  ER-2929         PIC  X(4)       VALUE '2929'.
00183          16  ER-3020         PIC  X(4)       VALUE '3020'.
00184          16  ER-3146         PIC  X(4)       VALUE '3146'.
00185          16  ER-3172         PIC  X(4)       VALUE '3172'.
00186          16  ER-3175         PIC  X(4)       VALUE '3175'.
00187          16  ER-3177         PIC  X(4)       VALUE '3177'.
00188          16  ER-3178         PIC  X(4)       VALUE '3178'.
00189          16  ER-3179         PIC  X(4)       VALUE '3179'.
00190          16  ER-3180         PIC  X(4)       VALUE '3180'.
00191          16  ER-3183         PIC  X(4)       VALUE '3183'.
00192          16  ER-3190         PIC  X(4)       VALUE '3190'.
00193          16  ER-3193         PIC  X(4)       VALUE '3193'.
00194          16  ER-3195         PIC  X(4)       VALUE '3195'.
00195          16  ER-3196         PIC  X(4)       VALUE '3196'.
00196          16  ER-3260         PIC  X(4)       VALUE '3260'.
00197          16  ER-3262         PIC  X(4)       VALUE '3262'.
00198          16  ER-3263         PIC  X(4)       VALUE '3263'.
00199          16  ER-3264         PIC  X(4)       VALUE '3264'.
00200          16  ER-3265         PIC  X(4)       VALUE '3265'.
00201          16  ER-3266         PIC  X(4)       VALUE '3266'.
00202          16  ER-7806         PIC  X(4)       VALUE '7806'.
00203          16  ER-9094         PIC  X(4)       VALUE '9094'.
00204          16  ER-9095         PIC  X(4)       VALUE '9095'.
00205          16  ER-9096         PIC  X(4)       VALUE '9096'.
00206          16  ER-9097         PIC  X(4)       VALUE '9097'.
00207          16  ER-9179         PIC  X(4)       VALUE '9179'.
00208          16  ER-9280         PIC  X(4)       VALUE '9280'.
00209          16  ER-9296         PIC  X(4)       VALUE '9296'.
00210          16  ER-9374         PIC  X(4)       VALUE '9374'.
00211
00212  01  WORK-AREA.
00213      12  QID.
00214          16  QID-TERM            PIC X(4)      VALUE SPACES.
00215          16  FILLER              PIC X(4)      VALUE '635A'.
00216      12  WS-SAVE-WS-INDX         PIC S9(4) COMP VALUE ZERO.
00217      12  WS-EDITED-AMTS OCCURS 6 TIMES
00218                                 INDEXED BY WS-INDX.
00219          16  WS-EDITED-AMT       PIC S9(9)V99 COMP-3.
00220
00221      12  DATE-TEST-AREA      PIC  9(6).
00222      12  DATE-TEST-AREA-R  REDEFINES  DATE-TEST-AREA.
00223          16  DATE-TEST-MM    PIC  99.
00224          16  DATE-TEST-DD    PIC  99.
00225          16  DATE-TEST-YY    PIC  99.
00226      12  DIVIDE-RESULT       PIC  99           VALUE ZERO.
00227      12  DIVIDE-REMAINDER    PIC  9            VALUE ZERO.
00228      12  PREV-BIN-MAINT-DT   PIC  XX           VALUE SPACES.
00229      12  PREV-MAINT-DT       PIC  9(6)         VALUE ZEROS.
00230      12  PREV-BIN-BL-DT      PIC  XX           VALUE SPACES.
00231      12  PREV-BL-DT          PIC  9(6)         VALUE ZEROS.
00232      12  TOTAL-ACCT-AMT      PIC S9(7)V99      VALUE ZEROS.
00233      12  TOTAL-ACCT-NET      PIC S9(7)V99      VALUE ZEROS.
00234      12  WS-OLD-ENTRY-AMT    PIC S9(7)V99      VALUE ZEROS.
00235      12  WS-ENTRY-AMT        PIC S9(7)V99      VALUE ZEROS.
00236      12  WS-WORK-BALANCE     PIC S9(7)V99      VALUE ZEROS.
00237      12  WS-BAL-AMOUNT       PIC S9(7)V99      VALUE ZEROS.
00238      12  WS-AR-BALANCE       PIC S9(7)V99      VALUE ZEROS.
00239      12  WS-SAVE-INDEX-VALUE PIC S9(4) COMP    VALUE ZEROS.
00240      12  WS-SAVE-NDX-VALUE   PIC S9(4) COMP    VALUE ZEROS.
00241      12  WS-EOM-DTS OCCURS 3  TIMES
00242                               INDEXED BY PINDEX.
00243          16  WS-EOM-DT               PIC XX.
00244      12  WS-INP-DTS OCCURS 3  TIMES
00245                               INDEXED BY DINDEX.
00246          16  WS-INP-DT               PIC XX.
00247      12  WS-PREV-PF5                 PIC X.
00248      12  WS-ACCEPT-TABLE.
00249          16  WS-ACCEPT-1 OCCURS 3 TIMES
00250                          INDEXED BY W-INDX
00251                                      PIC X.
00252
00253      EJECT
00254  01  ACCESS-KEYS.
00255      12  ERPYAJ-KEY.
00256          16  PYAJ-COMP-CD            PIC  X      VALUE SPACE.
00257          16  PYAJ-CARRIER            PIC  X      VALUE SPACES.
00258          16  PYAJ-GROUPING           PIC  X(6)   VALUE SPACES.
00259          16  PYAJ-FIN-RESP           PIC  X(10)  VALUE SPACES.
00260          16  PYAJ-ACCOUNT            PIC  X(10)  VALUE SPACES.
00261          16  PYAJ-FILE-SEQ-NO        PIC S9(8)   VALUE +0  COMP.
00262          16  PYAJ-RECORD-TYPE        PIC  X      VALUE SPACES.
00263
00264      12  ERPYAJ-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.
00265      12  ERPYAJ-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.
00266
00267      12  ERCHKQ-KEY.
00268          16  CHKQ-COMPANY-CD         PIC  X      VALUE SPACE.
00269          16  CHKQ-CONTROL-NUMBER     PIC S9(8)   VALUE +0  COMP.
00270          16  CHKQ-SEQUENCE-NUMBER    PIC S9(4)   VALUE +0  COMP.
00271      12  ERCOMP-KEY.
00272          16  COMP-COMP-CD            PIC  X      VALUE SPACE.
00273          16  COMP-CARRIER            PIC  X      VALUE SPACES.
00274          16  COMP-GROUPING           PIC  X(6)   VALUE SPACES.
00275          16  COMP-FIN-RESP           PIC  X(10)  VALUE SPACES.
00276          16  COMP-ACCOUNT            PIC  X(10)  VALUE SPACES.
00277          16  COMP-RECORD-TYPE        PIC  X      VALUE SPACES.
00278      12  ERRECV-KEY.
00279          16  RECV-COMP-CD            PIC  X      VALUE SPACE.
00280          16  RECV-TYPE               PIC  X      VALUE SPACES.
00281          16  RECV-CARRIER            PIC  X      VALUE SPACES.
00282          16  RECV-GROUPING           PIC  X(6)   VALUE SPACES.
00283          16  RECV-BAL-LVL            PIC  X      VALUE SPACES.
00284          16  RECV-ENTRY-TYPE         PIC  X      VALUE SPACES.
00285          16  RECV-FIN-RESP           PIC  X(10)  VALUE SPACES.
00286          16  RECV-ACCOUNT            PIC  X(10)  VALUE SPACES.
00287          16  RECV-INVOICE            PIC  X(6)   VALUE SPACES.
00288          16  RECV-REFERENCE          PIC  X(12)  VALUE SPACES.
00289          16  RECV-RESPONSIBLE        PIC  X      VALUE SPACES.
00290          16  RECV-RECORD-TYPE        PIC  X      VALUE SPACES.
00291          16  RECV-RECORD-SEQ         PIC  S9(4)  VALUE +0  COMP.
00292      12  ERRECV-KEY-LEN              PIC  S9(4)  COMP VALUE +51.
00293
00294      12  MPPRCN-KEY.
00295          16  MPPRCN-COMPANY-CD       PIC X       VALUE SPACES.
00296          16  MPPRCN-INVOICE          PIC X(6)    VALUE SPACES.
00297          16  MPPRCN-RECORD-SEQU      PIC S9(7)   VALUE +0  COMP-3.
00298
00299  EJECT
00300 *    COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00301  EJECT
00302 *    COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
00303  EJECT
00304 *    COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
00305  EJECT
00306 *    COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
00070      12  FILLER                      PIC X(137)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00307  EJECT
00308 *    COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
00131      12  FILLER                          PIC X(4).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
00309      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00310          16  PI-PYAJ-FILE-SW         PIC  X.
00311              88  END-OF-ACCT                 VALUE 'A'.
00312              88  END-OF-FILE                 VALUE 'X'.
00313              88  TOP-OF-FILE                 VALUE 'T'.
00314              88  PAGE-FULL                   VALUE 'F'.
00315              88  NO-RECORDS                  VALUE 'Y'.
00316              88  NOT-OPEN                    VALUE 'Z'.
00317          16  PI-PREV-FUNCTION        PIC  X.
00318          16  PI-SAV-FUNCTION         PIC  X.
00319          16  PI-SEQ-NOS.
00320              20  FILLER  OCCURS 3 TIMES
00321                              INDEXED BY NDX.
00322                  24  PI-REC-TYPE     PIC  X.
00323                  24  PI-FILE-SEQ-NO  PIC S9(8)          COMP.
00324                  24  PI-APPLIED      PIC  X.
00325                  24  PI-REFERENCE    PIC  X(12).
00326                  24  PI-INVOICE      PIC  X(6).
00327                  24  PI-AMOUNT       PIC  S9(7)V99 COMP-3.
00328          16  PI-SAV-ENDING-PYAJ-KEY.
00329              20  PI-SAV-COMP-CD      PIC  X.
00330              20  PI-SAV-COMP-CONTROL.
00331                  24  PI-SAV-CARRIER      PIC  X.
00332                  24  PI-SAV-GROUPING     PIC  X(6).
00333                  24  PI-SAV-FIN-RESP     PIC  X(10).
00334                  24  PI-SAV-ACCOUNT      PIC  X(10).
00335                  24  PI-SAV-FILE-SEQ-NO  PIC S9(8)          COMP.
00336                  24  PI-SAV-RECORD-TYPE  PIC  X.
00337          16  PI-SAV-ACCT-AMT         PIC  S9(7)V99.
00338          16  PI-SAV-ACCT-NET         PIC  S9(7)V99.
00339          16  PI-TOTAL-DISPLAYED-SW   PIC  X.
00340              88  PI-TOTAL-DISPLAYED          VALUE 'Y'.
00341          16  PI-SPECIAL-GROUPING-SW  PIC X.
00342              88  PI-SPECIAL-GROUPING         VALUE 'G'.
00343          16  FILLER                  PIC X(60).
00344          16  PI-PYAJ-FILLED-SW       PIC X.
00345             88  REFERENCE-DISPLAYED  VALUE 'Y'.
00346          16  PI-PYAJ-REFERENCE       PIC X(12).
00347          16  PI-ACCEPT               PIC XXX.
00348          16  FILLER                  PIC X(421).
00349
00350  EJECT
00351 *    COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00352                              PIC  X(223).
00353  EJECT
00354 *    COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00355
00356  01  FILLER  REDEFINES  DFHAID.
00357      12  FILLER              PIC  X(8).
00358      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
00359  EJECT
00360 *    COPY EL635S.
       01  EL635AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  FINRESPL PIC S9(0004) COMP.
           05  FINRESPF PIC  X(0001).
           05  FILLER REDEFINES FINRESPF.
               10  FINRESPA PIC  X(0001).
           05  FINRESPI PIC  X(0010).
      *    -------------------------------
           05  ACCTHDGL PIC S9(0004) COMP.
           05  ACCTHDGF PIC  X(0001).
           05  FILLER REDEFINES ACCTHDGF.
               10  ACCTHDGA PIC  X(0001).
           05  ACCTHDGI PIC  X(0010).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  EMHD1L PIC S9(0004) COMP.
           05  EMHD1F PIC  X(0001).
           05  FILLER REDEFINES EMHD1F.
               10  EMHD1A PIC  X(0001).
           05  EMHD1I PIC  X(0006).
      *    -------------------------------
           05  EMHD2L PIC S9(0004) COMP.
           05  EMHD2F PIC  X(0001).
           05  FILLER REDEFINES EMHD2F.
               10  EMHD2A PIC  X(0001).
           05  EMHD2I PIC  X(0079).
      *    -------------------------------
           05  COMM1L PIC S9(0004) COMP.
           05  COMM1F PIC  X(0001).
           05  FILLER REDEFINES COMM1F.
               10  COMM1A PIC  X(0001).
           05  COMM1I PIC  X(0030).
      *    -------------------------------
           05  TYPE1L PIC S9(0004) COMP.
           05  TYPE1F PIC  X(0001).
           05  FILLER REDEFINES TYPE1F.
               10  TYPE1A PIC  X(0001).
           05  TYPE1I PIC  X(0001).
      *    -------------------------------
           05  AMT1L PIC S9(0004) COMP.
           05  AMT1F PIC  X(0001).
           05  FILLER REDEFINES AMT1F.
               10  AMT1A PIC  X(0001).
           05  AMT1I PIC  X(0011).
      *    -------------------------------
           05  APLID1L PIC S9(0004) COMP.
           05  APLID1F PIC  X(0001).
           05  FILLER REDEFINES APLID1F.
               10  APLID1A PIC  X(0001).
           05  APLID1I PIC  X(0001).
      *    -------------------------------
           05  VOID1L PIC S9(0004) COMP.
           05  VOID1F PIC  X(0001).
           05  FILLER REDEFINES VOID1F.
               10  VOID1A PIC  X(0001).
           05  VOID1I PIC  X(0001).
      *    -------------------------------
           05  MDT1L PIC S9(0004) COMP.
           05  MDT1F PIC  X(0001).
           05  FILLER REDEFINES MDT1F.
               10  MDT1A PIC  X(0001).
           05  MDT1I PIC  X(0006).
      *    -------------------------------
           05  BDT1L PIC S9(0004) COMP.
           05  BDT1F PIC  X(0001).
           05  FILLER REDEFINES BDT1F.
               10  BDT1A PIC  X(0001).
           05  BDT1I PIC  X(0006).
      *    -------------------------------
           05  SDT1L PIC S9(0004) COMP.
           05  SDT1F PIC  X(0001).
           05  FILLER REDEFINES SDT1F.
               10  SDT1A PIC  X(0001).
           05  SDT1I PIC  X(0006).
      *    -------------------------------
           05  REF1L PIC S9(0004) COMP.
           05  REF1F PIC  X(0001).
           05  FILLER REDEFINES REF1F.
               10  REF1A PIC  X(0001).
           05  REF1I PIC  X(0012).
      *    -------------------------------
           05  SUM1L PIC S9(0004) COMP.
           05  SUM1F PIC  X(0001).
           05  FILLER REDEFINES SUM1F.
               10  SUM1A PIC  X(0001).
           05  SUM1I PIC  X(0006).
      *    -------------------------------
           05  NETOT1L PIC S9(0004) COMP.
           05  NETOT1F PIC  X(0001).
           05  FILLER REDEFINES NETOT1F.
               10  NETOT1A PIC  X(0001).
           05  NETOT1I PIC  X(0011).
      *    -------------------------------
           05  INPDT1L PIC S9(0004) COMP.
           05  INPDT1F PIC  X(0001).
           05  FILLER REDEFINES INPDT1F.
               10  INPDT1A PIC  X(0001).
           05  INPDT1I PIC  X(0006).
      *    -------------------------------
           05  CREDIT1L PIC S9(0004) COMP.
           05  CREDIT1F PIC  X(0001).
           05  FILLER REDEFINES CREDIT1F.
               10  CREDIT1A PIC  X(0001).
           05  CREDIT1I PIC  X(0014).
      *    -------------------------------
           05  DEBIT1L PIC S9(0004) COMP.
           05  DEBIT1F PIC  X(0001).
           05  FILLER REDEFINES DEBIT1F.
               10  DEBIT1A PIC  X(0001).
           05  DEBIT1I PIC  X(0014).
      *    -------------------------------
           05  COMM2L PIC S9(0004) COMP.
           05  COMM2F PIC  X(0001).
           05  FILLER REDEFINES COMM2F.
               10  COMM2A PIC  X(0001).
           05  COMM2I PIC  X(0030).
      *    -------------------------------
           05  TYPE2L PIC S9(0004) COMP.
           05  TYPE2F PIC  X(0001).
           05  FILLER REDEFINES TYPE2F.
               10  TYPE2A PIC  X(0001).
           05  TYPE2I PIC  X(0001).
      *    -------------------------------
           05  AMT2L PIC S9(0004) COMP.
           05  AMT2F PIC  X(0001).
           05  FILLER REDEFINES AMT2F.
               10  AMT2A PIC  X(0001).
           05  AMT2I PIC  X(0011).
      *    -------------------------------
           05  APLID2L PIC S9(0004) COMP.
           05  APLID2F PIC  X(0001).
           05  FILLER REDEFINES APLID2F.
               10  APLID2A PIC  X(0001).
           05  APLID2I PIC  X(0001).
      *    -------------------------------
           05  VOID2L PIC S9(0004) COMP.
           05  VOID2F PIC  X(0001).
           05  FILLER REDEFINES VOID2F.
               10  VOID2A PIC  X(0001).
           05  VOID2I PIC  X(0001).
      *    -------------------------------
           05  MDT2L PIC S9(0004) COMP.
           05  MDT2F PIC  X(0001).
           05  FILLER REDEFINES MDT2F.
               10  MDT2A PIC  X(0001).
           05  MDT2I PIC  X(0006).
      *    -------------------------------
           05  BDT2L PIC S9(0004) COMP.
           05  BDT2F PIC  X(0001).
           05  FILLER REDEFINES BDT2F.
               10  BDT2A PIC  X(0001).
           05  BDT2I PIC  X(0006).
      *    -------------------------------
           05  SDT2L PIC S9(0004) COMP.
           05  SDT2F PIC  X(0001).
           05  FILLER REDEFINES SDT2F.
               10  SDT2A PIC  X(0001).
           05  SDT2I PIC  X(0006).
      *    -------------------------------
           05  REF2L PIC S9(0004) COMP.
           05  REF2F PIC  X(0001).
           05  FILLER REDEFINES REF2F.
               10  REF2A PIC  X(0001).
           05  REF2I PIC  X(0012).
      *    -------------------------------
           05  SUM2L PIC S9(0004) COMP.
           05  SUM2F PIC  X(0001).
           05  FILLER REDEFINES SUM2F.
               10  SUM2A PIC  X(0001).
           05  SUM2I PIC  X(0006).
      *    -------------------------------
           05  NETOT2L PIC S9(0004) COMP.
           05  NETOT2F PIC  X(0001).
           05  FILLER REDEFINES NETOT2F.
               10  NETOT2A PIC  X(0001).
           05  NETOT2I PIC  X(0011).
      *    -------------------------------
           05  INPDT2L PIC S9(0004) COMP.
           05  INPDT2F PIC  X(0001).
           05  FILLER REDEFINES INPDT2F.
               10  INPDT2A PIC  X(0001).
           05  INPDT2I PIC  X(0006).
      *    -------------------------------
           05  CREDIT2L PIC S9(0004) COMP.
           05  CREDIT2F PIC  X(0001).
           05  FILLER REDEFINES CREDIT2F.
               10  CREDIT2A PIC  X(0001).
           05  CREDIT2I PIC  X(0014).
      *    -------------------------------
           05  DEBIT2L PIC S9(0004) COMP.
           05  DEBIT2F PIC  X(0001).
           05  FILLER REDEFINES DEBIT2F.
               10  DEBIT2A PIC  X(0001).
           05  DEBIT2I PIC  X(0014).
      *    -------------------------------
           05  COMM3L PIC S9(0004) COMP.
           05  COMM3F PIC  X(0001).
           05  FILLER REDEFINES COMM3F.
               10  COMM3A PIC  X(0001).
           05  COMM3I PIC  X(0030).
      *    -------------------------------
           05  TYPE3L PIC S9(0004) COMP.
           05  TYPE3F PIC  X(0001).
           05  FILLER REDEFINES TYPE3F.
               10  TYPE3A PIC  X(0001).
           05  TYPE3I PIC  X(0001).
      *    -------------------------------
           05  AMT3L PIC S9(0004) COMP.
           05  AMT3F PIC  X(0001).
           05  FILLER REDEFINES AMT3F.
               10  AMT3A PIC  X(0001).
           05  AMT3I PIC  X(0011).
      *    -------------------------------
           05  APLID3L PIC S9(0004) COMP.
           05  APLID3F PIC  X(0001).
           05  FILLER REDEFINES APLID3F.
               10  APLID3A PIC  X(0001).
           05  APLID3I PIC  X(0001).
      *    -------------------------------
           05  VOID3L PIC S9(0004) COMP.
           05  VOID3F PIC  X(0001).
           05  FILLER REDEFINES VOID3F.
               10  VOID3A PIC  X(0001).
           05  VOID3I PIC  X(0001).
      *    -------------------------------
           05  MDT3L PIC S9(0004) COMP.
           05  MDT3F PIC  X(0001).
           05  FILLER REDEFINES MDT3F.
               10  MDT3A PIC  X(0001).
           05  MDT3I PIC  X(0006).
      *    -------------------------------
           05  BDT3L PIC S9(0004) COMP.
           05  BDT3F PIC  X(0001).
           05  FILLER REDEFINES BDT3F.
               10  BDT3A PIC  X(0001).
           05  BDT3I PIC  X(0006).
      *    -------------------------------
           05  SDT3L PIC S9(0004) COMP.
           05  SDT3F PIC  X(0001).
           05  FILLER REDEFINES SDT3F.
               10  SDT3A PIC  X(0001).
           05  SDT3I PIC  X(0006).
      *    -------------------------------
           05  REF3L PIC S9(0004) COMP.
           05  REF3F PIC  X(0001).
           05  FILLER REDEFINES REF3F.
               10  REF3A PIC  X(0001).
           05  REF3I PIC  X(0012).
      *    -------------------------------
           05  SUM3L PIC S9(0004) COMP.
           05  SUM3F PIC  X(0001).
           05  FILLER REDEFINES SUM3F.
               10  SUM3A PIC  X(0001).
           05  SUM3I PIC  X(0006).
      *    -------------------------------
           05  NETOT3L PIC S9(0004) COMP.
           05  NETOT3F PIC  X(0001).
           05  FILLER REDEFINES NETOT3F.
               10  NETOT3A PIC  X(0001).
           05  NETOT3I PIC  X(0011).
      *    -------------------------------
           05  INPDT3L PIC S9(0004) COMP.
           05  INPDT3F PIC  X(0001).
           05  FILLER REDEFINES INPDT3F.
               10  INPDT3A PIC  X(0001).
           05  INPDT3I PIC  X(0006).
      *    -------------------------------
           05  CREDIT3L PIC S9(0004) COMP.
           05  CREDIT3F PIC  X(0001).
           05  FILLER REDEFINES CREDIT3F.
               10  CREDIT3A PIC  X(0001).
           05  CREDIT3I PIC  X(0014).
      *    -------------------------------
           05  DEBIT3L PIC S9(0004) COMP.
           05  DEBIT3F PIC  X(0001).
           05  FILLER REDEFINES DEBIT3F.
               10  DEBIT3A PIC  X(0001).
           05  DEBIT3I PIC  X(0014).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0079).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
      *    -------------------------------
           05  PFGABILL PIC S9(0004) COMP.
           05  PFGABILF PIC  X(0001).
           05  FILLER REDEFINES PFGABILF.
               10  PFGABILA PIC  X(0001).
           05  PFGABILI PIC  X(0014).
      *    -------------------------------
           05  PFACBILL PIC S9(0004) COMP.
           05  PFACBILF PIC  X(0001).
           05  FILLER REDEFINES PFACBILF.
               10  PFACBILA PIC  X(0001).
           05  PFACBILI PIC  X(0016).
       01  EL635AO REDEFINES EL635AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRESPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTHDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMHD1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMHD2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT1O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APLID1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REF1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUM1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NETOT1O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INPDT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREDIT1O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEBIT1O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT2O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APLID2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REF2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUM2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NETOT2O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INPDT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREDIT2O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEBIT2O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM3O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT3O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APLID3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOID3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REF3O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUM3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NETOT3O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INPDT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREDIT3O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEBIT3O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFGABILO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFACBILO PIC  X(0016).
      *    -------------------------------
00361
00362  01  MAP-EL635A  REDEFINES  EL635AI.
00363      12  FILLER                  PIC  X(178).
00364      12  DATA-AREA       OCCURS  3 TIMES
00365                              INDEXED BY PINDX.
00366          16  COMM-LEN            PIC S9(4)              COMP.
00367          16  COMM-ATTRB          PIC  X.
00368          16  COMM                PIC  X(30).
00369          16  NCL-COMM  REDEFINES  COMM.
00370              20  NCL-COMM-DTE.
00371                  24  NCL-MO      PIC  XX.
00372                  24  NCL-DA      PIC  XX.
00373                  24  NCL-YR      PIC  XX.
00374              20  NCL-COMM-REST   PIC  X(24).
00375          16  RTYPE-LEN           PIC S9(4)              COMP.
00376          16  RTYPE-ATTRB         PIC  X.
00377          16  RTYPE               PIC  X.
00378          16  AMT-LEN             PIC S9(4)              COMP.
00379          16  AMT-ATTRB           PIC  X.
00380          16  AMT                 PIC S9(9)V99.
00381          16  AMTO  REDEFINES
00382              AMT                 PIC Z(7).99-.
00383          16  APPLIED-LEN         PIC S9(4)              COMP.
00384          16  APPLIED-ATTRB       PIC  X.
00385          16  APPLIED             PIC  X.
00386          16  VOID-SW-LEN         PIC S9(4)              COMP.
00387          16  VOID-SW-ATTRB       PIC  X.
00388          16  VOID-SW             PIC  X.
00389          16  MDTE-LEN            PIC S9(4)              COMP.
00390          16  MDTE-ATTRB          PIC  X.
00391          16  MDTE                PIC  9(6).
00392          16  BDTE-LEN            PIC S9(4)              COMP.
00393          16  BDTE-ATTRB          PIC  X.
00394          16  BDTE                PIC  9(6).
00395          16  SDTE-LEN            PIC S9(4)              COMP.
00396          16  SDTE-ATTRB          PIC  X.
00397          16  SDTE                PIC  9(6).
00398          16  REF-LEN             PIC S9(4)              COMP.
00399          16  REF-ATTRB           PIC  X.
00400          16  REF                 PIC  X(12).
00401          16  INVOICE-LEN         PIC S9(4)              COMP.
00402          16  INVOICE-ATTRB       PIC  X.
00403          16  INVOICE             PIC  X(6).
00404          16  NET-LEN             PIC S9(4)              COMP.
00405          16  NET-ATTRB           PIC  X.
00406          16  NET                 PIC S9(9)V99.
00407          16  NETO  REDEFINES
00408              NET                 PIC Z(7).99-.
00409          16  IDTE-LEN            PIC S9(4)              COMP.
00410          16  IDTE-ATTRB          PIC  X.
00411          16  IDTE                PIC  9(6).
00412          16  CREDIT-LEN          PIC S9(4)              COMP.
00413          16  CREDIT-ATTRB        PIC  X.
00414          16  CREDIT              PIC  X(14).
00415          16  DEBIT-LEN           PIC S9(4)              COMP.
00416          16  DEBIT-ATTRB         PIC  X.
00417          16  DEBIT               PIC  X(14).
00418  EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00420  01  DFHCOMMAREA             PIC  X(1024).
00421  EJECT
00422 *01 PARMLIST       COMP.
00423 *    12  FILLER              PIC S9(8).
00424 *    12  ERPYAJ-POINTER      PIC S9(8).
00425 *    12  ERCHKQ-POINTER      PIC S9(8).
00426 *    12  ERCOMP-POINTER      PIC S9(8).
00427 *    12  ERRECV-POINTER      PIC S9(8).
00428 *    12  MPPRCN-POINTER      PIC S9(8).
00429
00430  EJECT
00431 *    COPY ERCPYAJ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCPYAJ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
00008 *                                                                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
042303******************************************************************
042303*                   C H A N G E   L O G
042303*
042303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042303*-----------------------------------------------------------------
042303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042303* EFFECTIVE    NUMBER
042303*-----------------------------------------------------------------
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
042303******************************************************************
00019
00020  01  PENDING-PAY-ADJ.
00021      12  PY-RECORD-ID                     PIC XX.
00022          88  VALID-PY-ID                        VALUE 'PY'.
00023
00024      12  PY-CONTROL-PRIMARY.
00025          16  PY-COMPANY-CD                PIC X.
00026          16  PY-CARRIER                   PIC X.
00027          16  PY-GROUPING                  PIC X(6).
00028          16  PY-FIN-RESP                  PIC X(10).
00029          16  PY-ACCOUNT                   PIC X(10).
00030          16  PY-PRODUCER REDEFINES PY-ACCOUNT
00031                                           PIC X(10).
00032          16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
00033          16  PY-RECORD-TYPE               PIC X.
00034              88  PY-REMIT-RECEIVED            VALUE 'R'.
00035              88  PY-DEPOSIT                   VALUE 'D'.
00036              88  PY-CHARGE-TO-AGENT           VALUE 'C'.
00037              88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
00038              88  PY-ADJ-DEPOSIT               VALUE 'T'.
00039              88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
00040              88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
00041              88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
00042              88  PY-ADD-TO-BALANCE            VALUE 'Z'.
00043              88  PY-FICA-ENTRY                VALUE 'F'.
00044              88  PY-REMIT-IND-GROUPING        VALUE 'G'.
00045              88  PY-POLICY-FEE                VALUE 'W'.
042303             88  PY-DUE-PREM-ADJ              VALUE 'P'.
00046
00047      12  PY-PYMT-TYPE                     PIC X.
00048              88  PY-NEW-BUS-PYMT              VALUE 'B'.
00049              88  PY-REINS-PYMT                VALUE 'R'.
00050              88  PY-EXP-PYMT                  VALUE 'E'.
00051
00052      12  PY-BIL-INV                       PIC X(6).
00053      12  PY-REF-NO                        PIC X(12).
00054
00055      12  PY-LAST-MAINT-DT                 PIC XX.
00056      12  PY-LAST-MAINT-BY                 PIC X(4).
00057      12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00058
00059      12  PY-PYADJ-RECORD.
00060          16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
00061          16  PY-ENTRY-COMMENT             PIC X(30).
CIDMOD         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
CIDMOD             20  PY-GL-ACCOUNT            PIC X(10).
CIDMOD             20  PY-GL-STATE              PIC X(02).
CIDMOD             20  PY-GL-CANC-SW            PIC X(01).
CIDMOD                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
CIDMOD                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
CIDMOD             20  PY-GL-COMMENT            PIC X(10).
CIDMOD             20  FILLER      REDEFINES PY-GL-COMMENT.
CIDMOD                 24  PY-GL-CHECK-NO       PIC 9(06).
CIDMOD                 24  FILLER               PIC X(04).
CIDMOD             20  FILLER                   PIC X(07).
00074          16  PY-SAVE-ACCOUNT              PIC X(10).
00075          16  PY-SAVE-TYPE                 PIC X(01).
00076
00077          16  PY-LETTERS.
00078              20  PY-LETTER OCCURS 3 TIMES
00079                            INDEXED BY PY-LET-NDX
00080                                           PIC X(04).
00081
060205         16  PY-ERCOMP-TYPE               PIC X.
060205             88  PY-ACCOUNT-TYPE              VALUE 'A'.
060205             88  PY-GA-TYPE                   VALUE 'G'.
060205             88  PY-BANK-TYPE                 VALUE 'B'.
060205         16  FILLER                       PIC X(05).
00083
00084      12  PY-RECORD-STATUS.
00085          16  PY-CREDIT-SELECT-DT          PIC XX.
00086          16  PY-CREDIT-ACCEPT-DT          PIC XX.
00087          16  PY-BILLED-DATE               PIC XX.
00088          16  PY-REPORTED-DT               PIC XX.
00089          16  PY-PMT-APPLIED               PIC X.
00090              88  PY-ACCOUNT-PMT               VALUE 'A'.
00091              88  PY-GA-PMT                    VALUE 'G'.
00092              88  PY-OVWRITE-PMT               VALUE 'O'.
00093              88  PY-NON-AR-PMT                VALUE 'N'.
00094          16  FILLER                       PIC X(5).
00095          16  PY-INPUT-DT                  PIC XX.
00096          16  PY-CHECK-NUMBER              PIC X(6).
00097          16  PY-VOID-SW                   PIC X.
00098              88  PY-CHECK-VOIDED              VALUE 'V'.
00099          16  PY-CHECK-ORIGIN-SW           PIC X.
00100              88  PY-BILLING-CHECK             VALUE 'B'.
00101              88  PY-REFUND-CHECK              VALUE 'R'.
00102              88  PY-GA-CHECK                  VALUE 'G'.
00103              88  PY-CHECK-WRITTEN             VALUE 'W'.
00104              88  PY-CHECK-REVERSAL            VALUE 'V'.
00105          16  PY-CHECK-WRITTEN-DT          PIC XX.
00106          16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
00107          16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
00108          16  PY-BILL-FLAG                 PIC X.
00109              88  PY-BILLED                    VALUE 'B'.
00110          16  PY-AR-FLAG                   PIC X.
00111              88  PY-AR-CYCLE                  VALUE 'C'.
00112              88  PY-AR-MONTH-END              VALUE 'M'.
00113          16  PY-AR-DATE                   PIC XX.
00114
00115      12  PY-GL-CODES.
00116          16  PY-GL-DB                     PIC X(14).
00117          16  PY-GL-CR                     PIC X(14).
00118          16  PY-GL-FLAG                   PIC X.
00119          16  PY-GL-DATE                   PIC XX.
00120
00121      12  PY-CANCEL-FEE-FLAG               PIC X(2).
00122      12  FILLER                           PIC X(3).
00123 ******************************************************************
00432  EJECT
00433 *    COPY ERCCHKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE CREDIT SYSTEM      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 100  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERCHKQ                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH  = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-QUE.
00019      12  CQ-RECORD-ID                PIC XX.
00020          88  VALID-CQ-ID                     VALUE 'CQ'.
00021
00022      12  CQ-CONTROL-PRIMARY.
00023          16  CQ-COMPANY-CD           PIC X.
00024          16  CQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00025          16  CQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00026
00027      12  CQ-ENTRY-TYPE               PIC X.
00028              88  CHECK-ON-QUE           VALUE 'Q'.
00029              88  ALIGNMENT-CHECK        VALUE 'A'.
00030              88  MANUAL-CHECK           VALUE 'M'.
00031              88  SPOILED-CHECK          VALUE 'S'.
00032              88  VOIDED-CHECK           VALUE 'V'.
00033              88  PAYMENT-ABORTED        VALUE 'X'.
00034
00035      12  CQ-CREDIT-MASTER-CNTL       PIC X(50).
00036
00037      12  CQ-CREDIT-PYAJ-CNTL         REDEFINES
00038          CQ-CREDIT-MASTER-CNTL.
00039          16  CQ-PYAJ-CARRIER         PIC X.
00040          16  CQ-PYAJ-GROUPING        PIC X(6).
00041          16  CQ-PYAJ-FIN-RESP        PIC X(10).
00042          16  CQ-PYAJ-ACCOUNT         PIC X(10).
00043          16  CQ-PYAJ-SEQ             PIC S9(8)  COMP.
00044          16  CQ-PYAJ-REC-TYPE        PIC X.
00045          16  FILLER                  PIC X(18).
00046
00047      12  CQ-CREDIT-CHEK-CNTL         REDEFINES
00048          CQ-CREDIT-MASTER-CNTL.
00049          16  CQ-CHEK-CARRIER         PIC X.
00050          16  CQ-CHEK-GROUPING        PIC X(6).
00051          16  CQ-CHEK-STATE           PIC XX.
00052          16  CQ-CHEK-ACCOUNT         PIC X(10).
00053          16  CQ-CHEK-CERT-EFF-DT     PIC XX.
00054          16  CQ-CHEK-CERT-NO.
00055              20  CQ-CHEK-CERT-PRIME  PIC X(10).
00056              20  CQ-CHEK-CERT-SFX    PIC X.
00057          16  CQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00058          16  CQ-CHEK-FIN-RESP        PIC X(10).
00059          16  FILLER                  PIC X(06).
00060
00061      12  CQ-CHECK-NUMBER             PIC X(7).
00062      12  CQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00063      12  CQ-PAYMENT-TYPE             PIC X.
00064              88  CQ-BILLING-CREDIT         VALUE '1'.
00065              88  CQ-REFUND-PMT             VALUE '2'.
00066              88  CQ-CHECK-MAINT-PMT        VALUE '3'.
00067      12  CQ-VOID-INDICATOR           PIC X.
00068              88  CHECK-IS-VOID             VALUE 'V'.
00069      12  CQ-TIMES-PRINTED            PIC S9(4)       COMP.
00070      12  CQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00071      12  CQ-CHECK-BY-USER            PIC X(4).
00072      12  CQ-PRE-NUMBERING-SW         PIC X.
00073        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00074        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00075
00076      12  CQ-CHECK-WRITTEN-DT         PIC XX.
00077      12  CQ-LAST-UPDATED-BY          PIC S9(4)       COMP.
00078      12  CQ-ACCOUNT-AGENT            PIC X(10).
00079      12  CQ-CHECK-VOIDED-DT          PIC XX.
00080
00081      12  CQ-LETTERS-IND              PIC X.
00082          88  CQ-LETTERS-REQUIRED           VALUE 'Y'.
00083
00084 ******************************************************************
00434  EJECT
00435 *    COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
00436  EJECT
00437 *    COPY ERCRECV.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCRECV.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACCOUNTS RECEIVABLE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE =  300 RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERRECV                         RKP=2,LEN=53   *
00013 *       ALTERNATE PATH1 = ERRECV2  (BY CO, CARR, GROUP, AGENT,   *
00014 *                                      EMO-DT, BAL, ENTRY-TYPE,  *
00015 *                                      F.R, INVOICE, REFERENCE,  *
00016 *                                      RESPONSIBILITY, REC TYPE, *
00017 *                                      SEQ. NO.)                 *
00018 *                                                RKP=64 ,LEN=54  *
00019 *   LOG = NO                                                     *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 ******************************************************************
00022
00023
00024  01  ACCOUNTS-RECEIVABLE.
00025      12  AR-RECORD-ID                      PIC XX.
00026          88  VALID-AR-ID                      VALUE 'AR'.
00027
00028      12  AR-CONTROL-PRIMARY.
00029          16  AR-COMPANY-CD                 PIC X.
00030          16  AR-TYPE                       PIC X.
00031              88 AR-NEW-BUSINESS VALUE '1'.
00032          16  AR-CARRIER                    PIC X.
00033          16  AR-GROUPING.
00034              20 AR-GROUPING-PREFIX         PIC XXX.
00035              20 AR-GROUPING-PRIME          PIC XXX.
00036          16  AR-BAL-LEVEL                  PIC X.
00037              88  AR-BAL-LVL-REF               VALUE '1'.
00038              88  AR-BAL-LVL-BILL-REF          VALUE '1'.
00039              88  AR-BAL-LVL-BILL              VALUE '2'.
00040              88  AR-BAL-LVL-AGT-FR            VALUE '3'.
00041              88  AR-BAL-LVL-FIN-RES           VALUE '4'.
00042          16  AR-ENTRY-TYPE                 PIC X.
00043              88  AR-AGENT-ENTRY               VALUE '1'.
00044              88  AR-GA-ENTRY                  VALUE '2'.
00045              88  AR-OVWRT-ENTRY               VALUE '3'.
00046          16  AR-FIN-RES                    PIC X(10).
00047          16  AR-AGENT-NO                   PIC X(10).
00048          16  AR-INVOICE-NO                 PIC X(6).
00049          16  AR-REFERENCE                  PIC X(12).
00050          16  AR-RESPONSIBILITY             PIC X.
00051              88  AR-AGENT-RESPONSIBLE         VALUE 'A'.
00052              88  AR-GA-RESPONSIBLE            VALUE 'G'.
00053          16  AR-RECORD-TYPE                PIC X.
00054              88  AR-BALANCE                   VALUE '0'.
00055              88  AR-PREMIUM                   VALUE '1'.
00056              88  AR-COMMISSION                VALUE '2'.
00057              88  AR-PAY-ADJ                   VALUE '3'.
00058              88  AR-TRANSFER                  VALUE '4'.
00059              88  AR-WRITE-OFF                 VALUE '5'.
00060              88  AR-NOTE                      VALUE '6'.
00061          16  AR-RECORD-SEQ                 PIC S9(4)    COMP.
00062
00063      12  FILLER                            PIC X(9).
00064
00065      12  AR-ACCOUNT-AGENT-CONTROL.
00066          16  AR-ACCOUNT-AGENT.
00067              20  AR-COMPANY-CD-A1          PIC X.
00068              20  AR-CARRIER-A1             PIC X.
00069              20  AR-GROUPING-A1            PIC X(6).
00070              20  AR-AGENT-NO-A1            PIC X(10).
00071          16  AR-EOM-DT-A1                  PIC XX.
00072          16  AR-BAL-LEVEL-A1               PIC X.
00073          16  AR-ENTRY-TYPE-A1              PIC X.
00074              88  AR-ACCT-AGENT               VALUE '1'.
00075              88  AR-GEN-AGENT                VALUE '2'.
00076              88  AR-OVERWRITE-AGENT          VALUE '3'.
00077          16  AR-FIN-RES-A1                 PIC X(10).
00078          16  AR-INVOICE-A1                 PIC X(6).
00079          16  AR-REFERENCE-A1               PIC X(12).
00080          16  AR-RESPONSIBILITY-A1          PIC X.
00081              88  AR-AGENT-RESPONSIBLE-A1      VALUE 'A'.
00082              88  AR-GA-RESPONSIBLE-A1         VALUE 'G'.
00083          16  AR-RECORD-TYPE-A1             PIC X.
00084          16  AR-RECORD-SEQ-A1              PIC S9(4)    COMP.
00085
00086      12  AR-CONTROL-GA                     PIC X(10).
00087
00088      12  AR-LAST-MAINT-DT                  PIC XX.
00089      12  AR-LAST-MAINT-BY                  PIC X(4).
00090      12  AR-LST-MAINT-HHMMSS               PIC S9(6)    COMP-3.
00091      12  AR-MONTH-END-DT                   PIC XX.
00092      12  FILLER                            PIC X.
00093      12  AR-REVERSAL-INFO.
00094          16  AR-REVERSAL-ID                PIC X(4).
00095          16  AR-REVERSAL-DT                PIC XX.
00096      12  AR-UPDATE-CODE                    PIC X.
00097          88  AR-NEW-RECORD                      VALUE 'N'.
00098          88  AR-UPDATED-RECORD                  VALUE 'U'.
00099      12  AR-SYSTEM                         PIC X.
00100          88  AR-MORTGAGE-SYSTEM                 VALUE 'M'.
00101      12  AR-CSR-TEMP                       PIC X(04).
00102      12  AR-CARRIER-TEMP                   PIC X(01).
00103      12  AR-GROUPING-TEMP                  PIC X(06).
00104      12  FILLER                            PIC X(20).
00105
00106      12  AR-RECORD-BODY                    PIC X(120).
00107
00108      12  AR-PREM-COMM-RECORD   REDEFINES AR-RECORD-BODY.
00109          16  AR-P-C-TRAN-DATE              PIC XX.
00110          16  AR-P-C-STMT-DATE              PIC XX.
00111          16  AR-P-C-BATCH                  PIC X(6).
00112          16  AR-P-C-AMOUNT                 PIC S9(7)V99  COMP-3.
00113          16  AR-P-C-DB-CR                  PIC XX.
00114              88 AR-P-C-DEBIT                   VALUE 'DB'.
00115              88 AR-P-C-CREDIT                  VALUE 'CR'.
00116          16  AR-P-C-DESCRIPTION            PIC X(30).
00117          16  AR-P-C-REMITTER-CODE          PIC X.
00118              88 AR-P-C-REMITTER-PAYS           VALUE 'Y'.
00119              88 AR-P-C-ACCT-AGT-PAYS           VALUE 'Y'.
00120          16  FILLER                        PIC X(33).
00121          16  AR-P-C-REMITTER               PIC X(10).
00122          16  AR-P-C-AMOUNT-LF              PIC S9(7)V99  COMP-3.
00123          16  AR-P-C-AMOUNT-AH              PIC S9(7)V99  COMP-3.
00124          16  FILLER                        PIC X(19).
00125
00126      12  AR-PAY-ADJ-RECORD  REDEFINES AR-RECORD-BODY.
00127          16  AR-PA-TRAN-DATE               PIC XX.
00128          16  AR-PA-STMT-DATE               PIC XX.
00129          16  FILLER                        PIC X(6).
00130          16  AR-PA-AMOUNT                  PIC S9(7)V99  COMP-3.
00131          16  AR-PA-DB-CR                   PIC XX.
00132              88  AR-PA-DEBIT                   VALUE 'DB'.
00133              88  AR-PA-CREDIT                  VALUE 'CR'.
00134          16  AR-PA-SEQ-NO                  PIC S9(8)     COMP.
00135          16  AR-PA-TYPE                    PIC X.
00136          16  AR-PA-DEBIT-LEDGER            PIC X(14).
00137          16  AR-PA-CREDIT-LEDGER           PIC X(14).
00138          16  AR-PA-COMMENT                 PIC X(30).
00139          16  AR-PA-REMITTER-CODE           PIC X.
00140              88 AR-PA-REMITTER-PAYS            VALUE 'Y'.
00141              88 AR-PA-ACCT-AGT-PAYS            VALUE 'Y'.
00142          16  AR-PA-REMITTER                PIC X(10).
00143          16  AR-NOTE-COUNT                 PIC S9(5).
00144          16  FILLER                        PIC X(24).
00145
00146      12  AR-TRANSFER-RECORD REDEFINES AR-RECORD-BODY.
00147          16  AR-XRF-TRAN-DATE              PIC XX.
00148          16  AR-XRF-STMT-DATE              PIC XX.
00149          16  AR-XRF-BATCH                  PIC X(6).
00150          16  AR-XRF-AMOUNT                 PIC S9(7)V99  COMP-3.
00151          16  AR-XRF-DB-CR                  PIC XX.
00152              88  AR-XRF-DEBIT                  VALUE 'DB'.
00153              88  AR-XRF-CREDIT                 VALUE 'CR'.
00154          16  AR-BAL-DESCRIPTION            PIC X(30).
00155          16  AR-XRF-REMITTER-CODE          PIC X.
00156              88 AR-XRF-REMITTER-PAYS           VALUE 'Y'.
00157              88 AR-XRF-ACCT-AGT-PAYS           VALUE 'Y'.
00158          16  FILLER                        PIC X(33).
00159          16  AR-XRF-REMITTER               PIC X(10).
00160          16  FILLER                        PIC X(29).
00161
00162      12  AR-BALANCE-RECORD REDEFINES AR-RECORD-BODY.
00163          16  AR-FST-TRAN-DATE              PIC XX.
00164          16  AR-LST-TRAN-DATE              PIC XX.
00165          16  FILLER                        PIC X(6).
00166          16  AR-BAL-AMOUNT                 PIC S9(7)V99  COMP-3.
00167          16  AR-BAL-DB-CR                  PIC XX.
00168              88  AR-BAL-DEBIT                  VALUE 'DB'.
00169              88  AR-BAL-CREDIT                 VALUE 'CR'.
00170          16  AR-BAL-DESCRIPTION            PIC X(30).
00171          16  FILLER                        PIC X(73).
00438  EJECT
00439 *    COPY MPCPRCN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCPRCN                             *
00004 *                            VMOD=1.014                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = PAYMENT RECONCILIATION                    *
00007 *   (ANY CHANGES MADE TO THIS COPYBOOK MUST ALSO BE MADE         *
00008 *   TO MPCPEXT)                                                  *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 835  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = MPPRCN (SEE NOTE ABOVE)          RKP=2,LEN=11 *
00014 *   ALTERNATE PATH2= MPPRCN2(PRODUCER BILLING SEQU)RKP=33,LEN=44 *
00015 *   ALTERNATE PATH3= MPPRCN3(POLICY PRIMARY CNTL  )RKP=97,LEN=46 *
00016 *   ALTERNATE PATH4= MPPRCN4(BY RECORD TYPE CNTL  )RKP=163,LEN=46*
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
00021
00022  01  PAYMENT-RECONCILIATION.
00023      12  PR-RECORD-ID                      PIC XX.
00024          88  VALID-PR-ID                      VALUE 'PR'.
00025
00026 ******************************************************************
00027 *   BASE CLUSTER = MPPRCN         (BASE KEY)      RKP=2,LEN=11   *
00028 ******************************************************************
00029
00030      12  PR-CONTROL-PRIMARY.
00031          16  PR-COMPANY-CD                 PIC X.
00032          16  PR-INVOICE-NUMBER.
00033              20  PR-INVOICE-YMD.
00034                  24  PR-INVOICE-YR         PIC X.
00035                  24  PR-INVOICE-MO         PIC X.
00036                  24  PR-INVOICE-DAY        PIC X.
00037              20  PR-INVOICE-SEQU           PIC X(3).
00038                  88  PR-INDIVIDUAL-BILL       VALUE 'AAA'.
00039          16  PR-RECORD-SEQU                PIC S9(7)     COMP-3.
00040              88  PR-INVOICE-HEADER            VALUE +9999999.
00041      12  FILLER                            PIC X(20).
00042
00043 ******************************************************************
00044 * ALTERNATE PATH2 = MPPRCN2(PRODUCER BILLING SEQU)RKP=33,LEN=44  *
00045 ******************************************************************
00046
00047      12  PR-CONTROL-BY-PRODUCER.
00048          16  PR-COMPANY-CD-A2              PIC X.
00049          16  PR-CARRIER-A2                 PIC X.
00050          16  PR-GROUPING-A2.
00051              20  PR-GROUPING-PREFIX-A2     PIC X(3).
00052              20  PR-GROUPING-PRIME-A2      PIC X(3).
00053          16  PR-STATE-A2                   PIC XX.
00054          16  PR-PRODUCER-A2.
00055              20  PR-PRODUCER-PREFIX-A2     PIC X(4).
00056              20  PR-PRODUCER-PRIME-A2      PIC X(6).
00057          16  PR-PRODUCER-BILLING-SEQU-A2   PIC X(20).
00058          16  PR-CURRENT-DATE-BIN-A2        PIC X(02).
00059          16  PR-UNIQUE-KEY-A2              PIC S9(04) COMP.
00060      12  FILLER                            PIC X(20).
00061
00062 ******************************************************************
00063 *  ALTERNATE PATH3 = MPPRCN3(POLICY CONTROL SEQU)RKP=97,LEN=46   *
00064 ******************************************************************
00065
00066      12  PR-CONTROL-BY-POLICY.
00067          16  PR-COMPANY-CD-A3              PIC X.
00068          16  PR-POLICY-NO-A3.
00069              20  PR-POLICY-PRIME-A3        PIC X(18).
00070              20  PR-POLICY-SFX-A3          PIC XX.
00071          16  PR-POLICY-CNTL-PARTIAL.
00072              20  PR-CARRIER-A3             PIC X.
00073              20  PR-GROUPING-A3.
00074                  24  PR-GROUPING-PREFIX-A3 PIC X(3).
00075                  24  PR-GROUPING-PRIME-A3  PIC X(3).
00076              20  PR-STATE-A3               PIC XX.
00077              20  PR-PRODUCER-A3.
00078                  24  PR-PRODUCER-PREFIX-A3 PIC X(4).
00079                  24  PR-PRODUCER-PRIME-A3  PIC X(6).
00080              20  PR-POLICY-EFF-DT-A3       PIC XX.
00081          16  PR-CURRENT-DATE-BIN-A3        PIC X(02).
00082          16  PR-UNIQUE-KEY-A3              PIC S9(04) COMP.
00083      12  FILLER                            PIC X(20).
00084
00085 ******************************************************************
00086 *  ALTERNATE PATH4 = MPPRCN4(RECON RECORD TYPE  )RKP=163 LEN=25  *
00087 ******************************************************************
00088
00089      12  PR-CONTROL-BY-RECORD-TYPE.
00090          16  PR-COMPANY-CD-A4              PIC X.
00091          16  PR-RECORD-TYPE-A4             PIC X.
00092              88  PR-HEADER-RECORD              VALUE '1'.
00093              88  PR-DETAIL-RECORD              VALUE '2'.
00094          16  PR-CARRIER-A4                 PIC X.
00095          16  PR-GROUPING-A4.
00096              20  PR-GROUPING-PREFIX-A4     PIC X(3).
00097              20  PR-GROUPING-PRIME-A4      PIC X(3).
00098          16  PR-STATE-A4                   PIC XX.
00099          16  PR-PRODUCER-A4.
00100              20  PR-PRODUCER-PREFIX-A4     PIC X(4).
00101              20  PR-PRODUCER-PRIME-A4      PIC X(6).
00102          16  PR-CURRENT-DATE-BIN-A4        PIC X(02).
00103          16  PR-UNIQUE-KEY-A4              PIC S9(04) COMP.
00104      12  FILLER                            PIC X(20).
00105
00106 ******************************************************************
00107 *                 FILE SYNCHRONIZATION DATA                      *
00108 ******************************************************************
00109
00110      12  PR-FILE-SYNCH-DATA.
00111          16  PR-LAST-CHANGE-DT             PIC XX.
00112          16  PR-LAST-CHANGE-TIME           PIC S9(7)     COMP-3.
00113          16  PR-LAST-CHANGE-PROCESSOR      PIC X(4).
00114          16  PR-SECURITY-ACCESS-CODE       PIC X.
00115      12  FILLER                            PIC X(16).
00116
00117 ******************************************************************
00118 *              PREMIUM RECONCILIATION RECORD BODY                *
00119 ******************************************************************
00120
00121      12  PR-RECORD-BODY                    PIC X(600).
00122
00123 ******************************************************************
00124 *             PREMIUM RECONCILIATION BATCH HEADER                *
00125 ******************************************************************
00126
00127      12  PR-BATCH-HEADER  REDEFINES  PR-RECORD-BODY.
00128          16  PR-POSTING-DATA.
00129              20  PR-POSTING-DT             PIC XX.
00130              20  PR-POSTING-TIME           PIC S9(7)      COMP-3.
00131              20  PR-POSTING-PROCESSOR      PIC X(4).
00132          16  PR-SUMMARY-DATA.
00133              20  PR-DOCU-COUNTS.
00134                  24  PR-ACTUAL-DOCU-CNT    PIC S9(5)      COMP-3.
00135                  24  PR-EXPECTED-DOCU-CNT  PIC S9(5)      COMP-3.
00136              20  PR-PREMIUM-AMTS.
00137                  24  PR-RECEIVED-PREMIUM   PIC S9(7)V99   COMP-3.
00138                  24  PR-ACTUAL-PREMIUM     PIC S9(7)V99   COMP-3.
00139                  24  PR-EXPECTED-PREMIUM   PIC S9(7)V99   COMP-3.
00140              20  PR-COMPENSATION-AMTS.
00141                  24  PR-PROD-EXPECTED-COM  PIC S9(7)V99   COMP-3.
00142                  24  PR-PROD-ACTUAL-COM    PIC S9(7)V99   COMP-3.
00143                  24  PR-OVRWT-EXPECTED-COM PIC S9(7)V99   COMP-3.
00144                  24  PR-OVRWT-ACTUAL-COM   PIC S9(7)V99   COMP-3.
00145                  24  PR-REMIT-EXPECTED-COM PIC S9(7)V99   COMP-3.
00146                  24  PR-REMIT-ACTUAL-COM   PIC S9(7)V99   COMP-3.
00147          16  PR-HDR-BILLING-SEQUENCE       PIC X(01).
00148              88  PR-HDR-BILL-NAME-SEQU        VALUE '1'.
00149              88  PR-HDR-BILL-LOAN-SEQU        VALUE '2'.
00150              88  PR-HDR-BILL-PLCY-SEQU        VALUE '3'.
00151          16  PR-HDR-BILLING-DT             PIC XX.
00152          16  PR-HDR-BILL-TO-DT             PIC XX.
00153          16  PR-HDR-POSTING-STATUS         PIC X.
00154              88  PR-HDR-NEEDS-EDITING         VALUE '0'.
00155              88  PR-HDR-EDITED                VALUE '1'.
00156              88  PR-HDR-OK-TO-POST            VALUE '2'.
00157              88  PR-HDR-POSTED                VALUE '3'.
00158              88  PR-HDR-REVERSED              VALUE '4'.
00159              88  PR-HDR-POSTING               VALUE '5'.
00160          16  PR-RECEIVED-DT                PIC XX.
00161          16  PR-RESPONSIBLE-AGENT          PIC X(10).
00162          16  PR-PAC-INFORMATION.
00163              20  PR-HDR-BANK-TRANSIT-NUMBER.
00164                  24  PR-HDR-FEDERAL-NUMBER PIC X(4).
00165                  24  PR-HDR-BANK-NUMBER    PIC X(4).
00166              20  PR-HDR-BANK-ACCOUNT-NUMBER
00167                                            PIC X(20).
00168              20  PR-SIGNATURE-NAME         PIC X(25).
00169
00170 ******************************************************************
00171 *         HEADER  AGENT AND COMMISSION DATA                      *
00172 ******************************************************************
00173
00174          16  PR-HDR-1STYR-RENEW-SW           PIC X.
00175              88  PR-HDR-1STYR-COMMISSIONS            VALUE '1'.
00176              88  PR-HDR-RENEW-COMMISSIONS            VALUE '2'.
00177              88  PR-HDR-CROSS-BOUNDRIES              VALUE '3'.
00178          16  PR-HDR-COMMISSION-DATA.
00179              20  PR-HDR-REMIT-TO             PIC S9(3) COMP-3.
00180              20  PR-HDR-COMM-CHANGE-SW       PIC X.
00181                  88  PR-HDR-COMMISSION-CHANGE         VALUE 'Y'.
00182              20  PR-HDR-AGENT-INFORMATION   OCCURS   5 TIMES.
00183                  24  PR-HDR-AGENT-NUMBER     PIC X(10).
00184                  24  PR-HDR-AGENT-TYPE       PIC X.
00185                      88  PR-HDR-AGENT-GROSS           VALUE 'C'.
00186                      88  PR-HDR-AGENT-REINS           VALUE 'R'.
00187                      88  PR-HDR-AGENT-GROSS-REINS     VALUE 'D'.
00188                      88  PR-HDR-OVERWRITE-GROSS       VALUE 'O'.
00189                      88  PR-HDR-OVERWRITE-GROSS-REINS
00190                                                       VALUE 'P'.
00191                      88  PR-HDR-OVERWRITE-REINS       VALUE 'T'.
00192                      88  PR-HDR-REINS-ONLY            VALUE 'W'.
00193                  24  PR-HDR-COMMISSION-BILL-PAID PIC X(1).
00194                      88  PR-HDR-GENERATE-BILL         VALUE 'B'.
00195                      88  PR-HDR-GENERATE-PAID         VALUE 'P'.
00196                  24  PR-HDR-AGENT-COMP-1ST-YEAR  PIC S99V999.
00197                  24  PR-HDR-COMP-1ST-YEAR-TYPE   PIC X(1).
00198                      88  PR-HDR-COMP-1ST-YEAR-PERCENT
00199                                                   VALUE '1'.
00200                      88  PR-HDR-COMP-1ST-YEAR-DOLLARS
00201                                                   VALUE '2'.
00202                      88  PR-HDR-COMP-1ST-YEAR-NOT-USED
00203                                                   VALUE '3'.
00204                  24  PR-HDR-RENEW-DATA  OCCURS 6 TIMES.
00205                      28  PR-HDR-RENEW-MONTHS PIC S999    COMP-3.
00206                      28  PR-HDR-RENEW-COMM   PIC S99V999 COMP-3.
00207                      28  PR-HDR-RENEW-TYPE   PIC X(1).
00208                          88  PR-HDR-COMP-RENEW-PERCENT
00209                                                         VALUE '1'.
00210                          88  PR-HDR-COMP-RENEW-DOLLARS
00211                                                         VALUE '2'.
00212                          88  PR-HDR-COMP-RENEW-NOT-USED
00213                                                         VALUE '3'.
00214                  24  PR-HDR-COMP-RECALC-FLAG     PIC X(1).
00215                      88  PR-HDR-BYPASS-RECALC          VALUE 'N'.
00216          16  FILLER                        PIC X(189).
00217
00218 ******************************************************************
00219 *                PREMIUM RECONCILIATION DETAIL                   *
00220 ******************************************************************
00221
00222      12  PR-RECON-DETAIL  REDEFINES  PR-RECORD-BODY.
00223          16  PR-TRANSACTION-TYPE           PIC X.
00224              88  PR-BILL                      VALUE '1'.
00225              88  PR-CANCEL                    VALUE '2'.
00226              88  PR-PYMT-REVERSAL             VALUE '3'.
00227              88  PR-BILL-REVERSAL             VALUE '4'.
00228              88  PR-PAID-IN-ADVANCE           VALUE '5'.
00229              88  PR-AR-BILL-REVERSAL          VALUE '6'.
00230          16  PR-POSTING-STATUS             PIC X.
00231              88  PR-NEEDS-EDITING             VALUE 'N'.
00232              88  PR-EDITED                    VALUE 'E'.
00233              88  PR-POSTED                    VALUE 'P'.
00234              88  PR-FORCE-TO-POST             VALUE 'F'.
00235              88  PR-DELETED                   VALUE 'D'.
00236              88  PR-REVERSED                  VALUE 'R'.
00237          16  PR-POLICY-STATUS              PIC X.
00238              88  PR-P-ACTIVE                  VALUE  '1'.
00239              88  PR-P-LAPSE                   VALUE  '0'.
00240              88  PR-P-CLAIM-APPLIED           VALUE  '6'.
00241              88  PR-P-CANCEL                  VALUE  '7'.
00242              88  PR-BILLABLE                  VALUES '0' '1' '6'.
00243          16  PR-PYMT-REVERSAL-DT           PIC XX.
00244          16  PR-INSURED-NAME.
00245              20  PR-INSURED-FIRST-NAME.
00246                  24  PR-INSURED-1ST-INIT   PIC X.
00247                  24  FILLER                PIC X(9).
00248              20  PR-INSURED-MIDDLE-INITIAL PIC X.
00249              20  PR-INSURED-LAST-NAME      PIC X(15).
00250          16  PR-LOAN-NUMBER                PIC X(20).
00251          16  PR-PLAN-CODE                  PIC X(02).
00252          16  PR-PLAN-REVISION              PIC X(03).
00253          16  PR-INS-MONTH-PREMIUM          PIC S9(5)V9(6) COMP-3.
00254          16  PR-BILLING-INFORMATION.
00255              20  PR-BILLING-TYPE           PIC X.
00256                  88  PR-LIST-BILL             VALUE '1'.
00257                  88  PR-TAPE-BILL             VALUE '2'.
00258                  88  PR-TAPE-LIST-BILL        VALUE '3'.
00259                  88  PR-GROUP-BILL         VALUES ARE '1' '2' '3'.
00260                  88  PR-DIRECT-BILL           VALUE '4'.
00261                  88  PR-PAC-BILL         VALUES ARE '5' 'C' 'S'.
00262                  88  PR-CHARGE-CARD-BILL      VALUE '6'.
00263                  88  PR-PAC-REFUND            VALUE 'D'.
00264                  88  PR-CHARGE-CARD-REFUND    VALUE 'E'.
00265                  88  PR-INDIV-BILL
00266                      VALUES ARE '4' '5' '6' 'C' 'S'.
00267                  88  PR-EFT-CHECKING          VALUE 'C'.
00268                  88  PR-EFT-SAVINGS           VALUE 'S'.
00269              20  PR-BILLING-SEQUENCE       PIC X(01).
00270                  88  PR-BILL-NAME-SEQU        VALUE '1'.
00271                  88  PR-BILL-LOAN-SEQU        VALUE '2'.
00272                  88  PR-BILL-PLCY-SEQU        VALUE '3'.
00273              20  PR-BILLING-MODE           PIC X(01).
00274                  88  PR-ANNUAL                VALUE '1'.
00275                  88  PR-SEMI-ANNUAL           VALUE '2'.
00276                  88  PR-QUARTERLY             VALUE '3'.
00277                  88  PR-MONTHLY               VALUE '4'.
00278                  88  PR-BI-MONTHLY            VALUE '5'.
00279                  88  PR-SINGLE-PREM           VALUE '6'.
00280              20  PR-MONTHS-BILLED          PIC S9(03)     COMP-3.
00281              20  PR-BILLED-AMT             PIC S9(5)V99   COMP-3.
00282              20  PR-BILLED-TO-DT           PIC XX.
00283              20  PR-BILLING-DT             PIC XX.
00284              20  PR-BILLING-SW             PIC X.
00285                  88  PR-FIRST-BILLING         VALUE 'Y'.
00286                  88  PR-PLCY-PAID-IN-ADVANCE  VALUE 'A'.
00287              20  PR-BANK-TRANSIT-NUMBER.
00288                  24  PR-FEDERAL-NUMBER     PIC X(4).
00289                  24  PR-BANK-NUMBER        PIC X(4).
00290              20  PR-BANK-ACCOUNT-NUMBER    PIC X(20).
00291              20  PR-CHARGE-CARD-TYPE       PIC X(2).
00292                  88  PR-VISA                  VALUE 'VI'.
00293                  88  PR-MSTR-CARD             VALUE 'MC'.
00294                  88  PR-DINERS-CLUB           VALUE 'DN'.
00295                  88  PR-DISCOVER              VALUE 'DS'.
00296                  88  PR-CARTE-BLANCHE         VALUE 'CB'.
00297                  88  PR-AMERICAN-EXPRESS      VALUE 'AE'.
00298              20  PR-CHARGE-CARD-EXP-DT     PIC X(2).
00299              20  PR-LOAN-OFFICER           PIC X(5).
00300              20  PR-BILLING-GROUPING-CODE  PIC X(6).
00301          16  PR-BILLING-RECONCILE-DATA.
00302              20  PR-EXPECTED-DATA.
00303                  24  PR-EXPECTED-PAYMENT   PIC S9(5)V99   COMP-3.
00304                  24  PR-EXPECTED-TO-DT     PIC XX.
00305              20  PR-EXP-COMPENSATION-DATA.
00306                  24  PR-EXP-PROD-LVL-COMP  PIC S9(5)V99   COMP-3.
00307                  24  PR-EXP-OVWR-LVL-COMP  PIC S9(5)V99   COMP-3.
00308                  24  PR-EXP-REMT-LVL-COMP  PIC S9(5)V99   COMP-3.
00309              20  PR-ENTERED-DATA.
00310                  24  PR-ENTERED-PAYMENT    PIC S9(5)V99   COMP-3.
00311                  24  PR-ENTERED-OVSH       PIC S9(5)V99   COMP-3.
00312                  24  PR-ENTERED-TO-DT      PIC XX.
00313                  24  PR-ENTERED-SOURCE     PIC X.
00314                      88  PR-BILLING-PGM       VALUE 'B'.
00315                      88  PR-OPERATOR          VALUE 'O'.
00316                      88  PR-PAYMENT-PGM       VALUE 'P'.
00317              20  PR-ENT-COMPENSATION-DATA.
00318                  24  PR-ENT-PROD-LVL-COMP  PIC S9(5)V99   COMP-3.
00319                  24  PR-ENT-OVWR-LVL-COMP  PIC S9(5)V99   COMP-3.
00320                  24  PR-ENT-REMT-LVL-COMP  PIC S9(5)V99   COMP-3.
00321          16  PR-PAYMENT-TYPE               PIC X.
00322              88  PR-CHECK                     VALUE 'C'.
00323              88  PR-MONEY-ORDER               VALUE 'M'.
00324          16  PR-CHECK-NUMBER               PIC X(5).
00325          16  PR-PREVIOUS-PAYMENT-DATA.
00326              20  PR-PREV-INVOICE-NUMBER    PIC X(6).
00327              20  PR-PREV-LAST-PYMT-DT      PIC XX.
00328              20  PR-PREV-PAID-TO-DT        PIC XX.
00329              20  PR-PREV-PAYMENT-AMT       PIC S9(5)V99  COMP-3.
00330              20  PR-PREV-OVER-SHORT-AMT    PIC S9(5)V99  COMP-3.
00331              20  PR-PREV-TOTAL-PREM-RECVD  PIC S9(7)V99  COMP-3.
00332              20  PR-PREV-MONTHS-PAID       PIC S9(3)     COMP-3.
00333          16  PR-PREVIOUS-BILLING-DATA.
00334              20  PR-PREV-LAST-BILL-DT      PIC XX.
00335              20  PR-PREV-BILL-TO-DT        PIC XX.
00336              20  PR-PREV-LAST-BILL-AMT     PIC S9(5)V99  COMP-3.
00337              20  PR-PREV-BILLING-SW        PIC X.
00338              20  PR-PREV-EXIT-DT           PIC XX.
00339              20  PR-PREV-LAST-LAPSE-DT     PIC XX.
00340          16  PR-MONTH-END-DT               PIC XX.
00341          16  PR-PLAN-TYPE                  PIC X.
00342              88  PR-AH-MORT-PLAN              VALUE 'A'.
00343              88  PR-AD-D-MORT-PLAN            VALUE 'E'.
00344              88  PR-DISMEM-MORT-PLAN          VALUE 'D'.
00345              88  PR-LIFE-MORT-PLAN            VALUE 'L'.
00346          16  PR-EXPECTED-TAX               PIC S9(5)V99   COMP-3.
00347          16  PR-BILL-DAY                   PIC S99        COMP-3.
00348          16  FILLER                        PIC X(14).
00349
00350 ******************************************************************
00351 *                 AGENT AND COMMISSION DATA                      *
00352 ******************************************************************
00353
00354          16  PR-1STYR-RENEW-SW               PIC X.
00355              88  PR-1STYR-COMMISSIONS         VALUE '1'.
00356              88  PR-RENEW-COMMISSIONS         VALUE '2'.
00357              88  PR-CROSS-BOUNDRIES           VALUE '3'.
00358          16  PR-COMMISSION-DATA.
00359              20  PR-REMIT-TO                 PIC S9(3) COMP-3.
00360              20  PR-COMM-CHANGE-SW           PIC X.
00361                  88  PR-COMMISSION-CHANGE         VALUE 'Y'.
00362              20  PR-AGENT-INFORMATION   OCCURS   5 TIMES.
00363                  24  PR-AGENT-NUMBER         PIC X(10).
00364                  24  PR-AGENT-TYPE           PIC X.
00365                      88  PR-AGENT-GROSS           VALUE 'C'.
00366                      88  PR-AGENT-REINS           VALUE 'R'.
00367                      88  PR-AGENT-GROSS-REINS     VALUE 'D'.
00368                      88  PR-OVERWRITE-GROSS       VALUE 'O'.
00369                      88  PR-OVERWRITE-GROSS-REINS VALUE 'P'.
00370                      88  PR-OVERWRITE-REINS       VALUE 'T'.
00371                      88  PR-REINS-ONLY            VALUE 'W'.
00372                  24  PR-COMMISSION-BILL-PAID PIC X(1).
00373                      88  PR-GENERATE-BILL         VALUE 'B'.
00374                      88  PR-GENERATE-PAID         VALUE 'P'.
00375                  24  PR-AGENT-COMP-1ST-YEAR  PIC S99V999.
00376                  24  PR-COMP-1ST-YEAR-TYPE   PIC X(1).
00377                      88  PR-COMP-1ST-YEAR-PERCENT   VALUE '1'.
00378                      88  PR-COMP-1ST-YEAR-DOLLARS   VALUE '2'.
00379                      88  PR-COMP-1ST-YEAR-NOT-USED  VALUE '3'.
00380                  24  PR-RENEW-DATA OCCURS 6 TIMES.
00381                      28  PR-RENEW-MONTHS      PIC S999    COMP-3.
00382                      28  PR-RESNEW-COMMISSION
00383                                               PIC S99V999 COMP-3.
00384                      28  PR-COMP-RENEW-TYPE   PIC X(1).
00385                          88  PR-COMP-RENEW-PERCENT      VALUE '1'.
00386                          88  PR-COMP-RENEW-DOLLARS      VALUE '2'.
00387                          88  PR-COMP-RENEW-NOT-USED     VALUE '3'.
00388                  24  PR-COMP-RECALC-FLAG     PIC X(1).
00389                      88  PR-BYPASS-RECALC         VALUE 'N'.
00390          16  FILLER                        PIC X(94).
00391
00440  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-PAY-ADJ
                                CHECK-QUE COMPENSATION-MASTER
                                ACCOUNTS-RECEIVABLE
                                PAYMENT-RECONCILIATION.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL635' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00442
00443      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00444      MOVE EIBTRMID               TO  QID-TERM.
00445      MOVE 2                      TO  EMI-NUMBER-OF-LINES.
00446
00447      IF EIBCALEN = ZERO
00448          GO TO 8800-UNAUTHORIZED-ACCESS.
00449
00450      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00451          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM.
00452
00453      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00454      MOVE '5'                    TO  DC-OPTION-CODE.
00455
00456      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00457
00458      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.
00459      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.
00460      MOVE DC-GREG-DATE-1-MDY     TO  WS-CURRENT-MDY.
00461
00462      IF MORTGAGE-SESSION
00463         MOVE XCTL-EM626           TO XCTL-626
00464         MOVE MPPYAJ-FILE-ID       TO PYAJ-FILE-ID.
00465
00466      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00467          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00468              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00469              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00470              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00471              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00472              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00473              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00474              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00475              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00476              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
00477          ELSE
00478              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00479              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00480              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00481              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00482              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00483              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00484              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00485              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00486
00487      MOVE LOW-VALUES             TO  EL635AI.
00488
00489      COMPUTE WORK-SEQ-NO  =  EIBTIME  *  10.
00490
00491      IF RETURNED-FROM EQUAL XCTL-6351 OR
00492                             XCTL-640  OR
00493                             XCTL-642  OR
00494                             XCTL-652
00495          PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0690-EXIT.
00496
00497      IF EIBTRNID EQUAL EL630-TRANS-ID
00498          MOVE 'T'                TO  PI-PYAJ-FILE-SW
00499          MOVE 'Y'                TO  EL630-SENT-SW
00500          MOVE ZEROS              TO  PI-SEQ-NOS
00501          IF ((PI-CR-CONTROL-IN-PROGRESS = SPACES) OR
00502              (PI-CR-FIN-RESP EQUAL 'UNKNOWN  '))
00503              MOVE SPACES         TO PI-CR-CONTROL-IN-PROGRESS
00504              MOVE SPACES         TO PI-SAV-CARRIER
00505                                     PI-SAV-GROUPING
00506                                     PI-SAV-FIN-RESP
00507                                     PI-SAV-ACCOUNT
00508                                     PI-PYAJ-REFERENCE
00509                                     PI-PYAJ-FILLED-SW
00510              GO TO 8100-SEND-INITIAL-MAP
00511          ELSE
00512              MOVE DFHENTER   TO  EIBAID
00513              MOVE 'S'        TO  MAINTI
00514              MOVE PI-CR-CARRIER
00515                              TO  CARRIERI
00516              MOVE PI-CR-GROUPING
00517                              TO  GROUPI
00518              MOVE PI-CR-FIN-RESP
00519                              TO  FINRESPI
00520              MOVE PI-CR-ACCOUNT
00521                              TO  ACCTI
00522              MOVE 1          TO  CARRIERL  MAINTA
00523              MOVE 3          TO  GROUPL
00524              MOVE 6          TO  FINRESPL  ACCTL
00525              GO TO 0400-VALIDATE-KEY-DATA.
00526
00527      IF EIBTRNID NOT = TRANS-ID
00528          MOVE 'T'                TO  PI-PYAJ-FILE-SW
00529          MOVE ZEROS              TO  PI-SEQ-NOS
00530          IF (EIBTRNID NOT = EL640-TRANS-ID  AND
00531              EIBTRNID NOT = EL642-TRANS-ID  AND
00532              EIBTRNID NOT = EL652-TRANS-ID  AND
00533              EIBTRNID NOT = EL6351-TRANS-ID)
00534              GO TO 8100-SEND-INITIAL-MAP
00535          ELSE
00536              IF (EIBTRNID = EL6351-TRANS-ID  OR
00537                            EL640-TRANS-ID   OR
00538                            EL642-TRANS-ID)  AND
00539                 PI-CR-CONTROL-IN-PROGRESS = SPACES
00540                 GO TO 8100-SEND-INITIAL-MAP
00541              ELSE
00542                  MOVE DFHENTER   TO  EIBAID
00543                  MOVE 'S'        TO  MAINTI
00544                  MOVE PI-CR-CARRIER
00545                                  TO  CARRIERI
00546                  MOVE PI-CR-GROUPING
00547                                  TO  GROUPI
00548                  MOVE PI-CR-FIN-RESP
00549                                  TO  FINRESPI
00550                  MOVE PI-CR-ACCOUNT
00551                                  TO  ACCTI
00552                  MOVE 1          TO  CARRIERL  MAINTA
00553                  MOVE 3              TO  GROUPL
00554                  MOVE 6              TO  FINRESPL  ACCTL
00555                  GO TO 0400-VALIDATE-KEY-DATA.
00556
00557      
      * EXEC CICS HANDLE CONDITION
00558 *        PGMIDERR  (9600-PGMID-ERROR)
00559 *        ERROR     (9990-ABEND)
00560 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00003031' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033303331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00561
00562      IF  EIBAID = DFHCLEAR
00563              OR
00564          NOT DISPLAY-CAP
00565          GO TO 9400-CLEAR.
00566
00567  EJECT
00568  0200-RECEIVE.
00569      IF EIBAID = DFHPA1  OR  DFHPA2  OR  DFHPA3
00570          MOVE ER-0008            TO  EMI-ERROR
00571          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00572          MOVE -1                 TO  MAINTL
00573          GO TO 8200-SEND-DATAONLY.
00574
00575      
      * EXEC CICS RECEIVE
00576 *        MAP     (MAP-NAME)
00577 *        MAPSET  (MAPSET-NAME)
00578 *        INTO    (EL635AI)
00579 *    END-EXEC.
           MOVE LENGTH OF
            EL635AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003049' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL635AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00580
00581      IF PFENTERL = ZERO
00582          GO TO 0300-CHECK-PFKEYS.
00583
00584      IF (PFENTERI  IS NUMERIC)
00585        AND (PFENTERI GREATER 0 AND LESS 25)
00586          MOVE PF-VALUES (PFENTERI)  TO  EIBAID
00587      ELSE
00588          MOVE ER-0029               TO  EMI-ERROR
00589          GO TO 0320-INPUT-ERROR.
00590
00591  0300-CHECK-PFKEYS.
00592      IF EIBAID = DFHPF23
00593          GO TO 8810-PF23.
00594
00595      IF EIBAID = DFHPF24
00596          GO TO 9200-RETURN-MAIN-MENU.
00597
00598      IF EIBAID = DFHPF12
00599          GO TO 9500-PF12.
00600
00601      IF EIBAID = DFHPF3
00602          IF PI-CR-CONTROL-IN-PROGRESS NOT = SPACES
00603              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00604              MOVE XCTL-652       TO  PGM-NAME
00605             IF PI-CR-ACCOUNT = LOW-VALUES
00606                MOVE 'G'          TO  PI-CR-TYPE
00607                GO TO 9300-XCTL
00608             ELSE
00609                MOVE 'A'          TO  PI-CR-TYPE
00610                GO TO 9300-XCTL
00611          ELSE
00612             MOVE ER-3020         TO  EMI-ERROR
00613             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00614             GO TO 8100-SEND-INITIAL-MAP.
00615
00616      IF EIBAID = DFHPF4
00617          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00618          MOVE XCTL-6351          TO  PGM-NAME
00619          GO TO 9300-XCTL.
00620
00621      IF CREDIT-SESSION  AND EIBAID = DFHPF6
00622          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00623          MOVE SPACES TO PI-PROGRAM-WORK-AREA
00624          MOVE XCTL-640           TO  PGM-NAME
00625          IF PI-CR-ACCOUNT = LOW-VALUES
00626              MOVE SPACES       TO  PI-CR-CONTROL-IN-PROGRESS
00627              GO TO 9300-XCTL
00628          ELSE
00629              MOVE 'A'          TO  PI-CR-TYPE
00630              GO TO 9300-XCTL.
00631
00632      IF CREDIT-SESSION  AND EIBAID = DFHPF7
00633         IF PI-GA-BILLING
00634             PERFORM 0500-CREATE-TEMP-STORAGE THRU 0590-EXIT
00635             MOVE SPACES TO PI-PROGRAM-WORK-AREA
00636             MOVE XCTL-642        TO  PGM-NAME
00637             IF PI-CR-ACCOUNT = LOW-VALUES
00638                 MOVE 'G'          TO  PI-CR-TYPE
00639                 GO TO 9300-XCTL
00640             ELSE
00641                 MOVE SPACES      TO  PI-CR-CONTROL-IN-PROGRESS
00642                 GO TO 9300-XCTL
00643         ELSE
00644             MOVE ER-2929         TO  EMI-ERROR
00645             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00646             MOVE AL-UNBON        TO  PFENTERA
00647             IF PFENTERL = ZERO
00648                 MOVE -1          TO  MAINTL
00649                 GO TO 8200-SEND-DATAONLY
00650             ELSE
00651                 MOVE -1          TO  PFENTERL
00652                 GO TO 8200-SEND-DATAONLY.
00653
00654      IF EIBAID = DFHPF1  OR  DFHPF2
00655          GO TO 0400-VALIDATE-KEY-DATA.
00656
00657      IF EIBAID = DFHENTER
00658          MOVE SPACES             TO WS-ACCEPT-1 (W-INDX)
00659          MOVE SPACES             TO WS-PREV-PF5
00660          GO TO 0400-VALIDATE-KEY-DATA.
00661
00662      IF EIBAID = DFHPF5
00663          MOVE PI-ACCEPT          TO WS-ACCEPT-TABLE
00664          GO TO 0400-VALIDATE-KEY-DATA.
00665
00666      IF EIBAID = DFHPF8
00667          GO TO 0400-VALIDATE-KEY-DATA.
00668
00669  0320-INPUT-ERROR.
00670      MOVE ER-0029                TO  EMI-ERROR.
00671
00672      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00673
00674      MOVE AL-UNBON               TO  PFENTERA.
00675
00676      IF PFENTERL = ZERO
00677          MOVE -1                 TO  MAINTL
00678      ELSE
00679          MOVE -1                 TO  PFENTERL.
00680
00681      GO TO 8200-SEND-DATAONLY.
00682  EJECT
00683  0400-VALIDATE-KEY-DATA.
00684      IF MODIFY-CAP  OR (EIBAID = DFHPF1 OR DFHPF2)
00685          NEXT SENTENCE
00686        ELSE
00687         IF MAINTI NOT = 'S'
00688          MOVE 'UPDATE'           TO SM-READ
00689          PERFORM 9995-SECURITY-VIOLATION
00690          IF  MORTGAGE-SESSION
00691              MOVE ER-9096        TO EMI-ERROR
00692              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00693              GO TO 8100-SEND-INITIAL-MAP
00694          ELSE
00695              MOVE ER-0070        TO EMI-ERROR
00696              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00697              GO TO 8100-SEND-INITIAL-MAP.
00698
00699      MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD.
00700
00701      IF EIBAID   = DFHPF1
00702         GO TO 4000-BROWSE-FRWD.
00703
00704      IF EIBAID   = DFHPF1   AND
00705         CARRIERL = ZEROS    AND
00706         GROUPL   = ZEROS    AND
00707         FINRESPL = ZEROS    AND
00708         ACCTL    = ZEROS
00709          GO TO 4000-BROWSE-FRWD.
00710
00711      IF EIBAID   = DFHPF8
00712         GO TO 1010-COMP-CHECK.
00713
00714      IF MAINTI = 'C' OR  'S'
00715          MOVE AL-UANON           TO  MAINTA
00716          MOVE MAINTI             TO  PI-SAV-FUNCTION
00717      ELSE
00718          MOVE -1                 TO  MAINTL
00719          MOVE ER-0023            TO  EMI-ERROR
00720          MOVE AL-UABON           TO  MAINTA
00721          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00722
00723      IF MAINTI = 'C'
00724        AND PI-PREV-FUNCTION NOT = 'S'
00725          MOVE -1                 TO  MAINTL
00726          MOVE ER-2056            TO  EMI-ERROR
00727          MOVE AL-UABON           TO  MAINTA
00728          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00729
00730      IF MORTGAGE-SESSION
00731         IF PI-SPECIAL-GROUPING
00732            IF CARRIERL       GREATER THAN ZEROS
00733               OR GROUPL      GREATER THAN ZEROS
00734               OR FINRESPL    GREATER THAN ZEROS
00735               OR ACCTL       GREATER THAN ZEROS
00736                  MOVE ER-9179    TO EMI-ERROR
00737                  MOVE -1         TO CARRIERL
00738                  MOVE AL-UABON   TO CARRIERA
00739                                     GROUPA
00740                                     FINRESPA
00741                                     ACCTA
00742                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00743                  GO TO 0410-CONT-EDIT
00744            ELSE
00745               GO TO 0410-CONT-EDIT
00746         ELSE
00747            NEXT SENTENCE.
00748
00749      IF CARRIERL = ZEROS  AND
00750         GROUPL   = ZEROS  AND
00751         FINRESPL = ZEROS  AND
00752         ACCTL    = ZEROS
00753          MOVE -1                 TO  CARRIERL
00754          MOVE ER-2231            TO  EMI-ERROR
00755          MOVE AL-UABOF           TO  CARRIERA
00756                                      GROUPA
00757                                      FINRESPA
00758                                      ACCTA
00759          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00760          GO TO 8200-SEND-DATAONLY.
00761
00762      IF CARRIERL NOT = ZEROS
00763          MOVE AL-UANON           TO  CARRIERA
00764          MOVE CARRIERI           TO  COMP-CARRIER
00765                                      PI-SAV-CARRIER
00766                                      PI-CR-CARRIER
00767          IF CARRIERI NOT = ZEROS
00768            AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)
00769              MOVE ER-2587        TO  EMI-ERROR
00770              MOVE -1             TO  CARRIERL
00771              MOVE AL-UABON       TO  CARRIERA
00772              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00773          ELSE
00774              NEXT SENTENCE
00775      ELSE
00776          MOVE LOW-VALUES          TO  COMP-CARRIER
00777                                       PI-SAV-CARRIER.
00778
00779      IF CARRIERL NOT = ZEROS
00780             AND
00781         CARRIERI NOT = SPACES
00782          IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
00783              IF PI-CARRIER-SECURITY = CARRIERI
00784                  NEXT SENTENCE
00785              ELSE
00786                  MOVE -1         TO  CARRIERL
00787                  MOVE ER-2370    TO  EMI-ERROR
00788                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00789
00790      IF GROUPL NOT = ZEROS
00791          MOVE AL-UANON           TO  GROUPA
00792          MOVE GROUPI             TO  COMP-GROUPING
00793                                      PI-SAV-GROUPING
00794                                      PI-CR-GROUPING
00795      ELSE
00796          MOVE LOW-VALUES         TO  COMP-GROUPING
00797                                      PI-SAV-GROUPING.
00798
00799      IF FINRESPL NOT = ZEROS
00800          MOVE AL-UANON           TO  FINRESPA
00801          MOVE FINRESPI           TO  COMP-FIN-RESP
00802                                      PI-SAV-FIN-RESP
00803                                      PI-CR-FIN-RESP
00804      ELSE
00805          MOVE LOW-VALUES         TO  COMP-FIN-RESP
00806                                      PI-SAV-FIN-RESP.
00807
00808      IF ACCTL NOT = ZEROS
00809          MOVE AL-UANON           TO  ACCTA
00810          MOVE ACCTI              TO  COMP-ACCOUNT
00811                                      PI-SAV-ACCOUNT
00812                                      PI-CR-ACCOUNT
00813      ELSE
00814          MOVE LOW-VALUES         TO  COMP-ACCOUNT
00815                                      PI-SAV-ACCOUNT
00816                                      PI-CR-ACCOUNT.
00817
00818  0410-CONT-EDIT.
00819
00820      IF EMI-ERROR NOT = ZEROS
00821          GO TO 8200-SEND-DATAONLY.
00822
00823      MOVE MAINTI                 TO  PI-PREV-FUNCTION.
00824
00825      IF MAINTI = 'S'
00826          IF EIBAID = DFHPF1  OR  DFHENTER
00827              GO TO 4000-BROWSE-FRWD
00828          ELSE
00829              GO TO 4100-BROWSE-BKWD.
00830
00831      IF PI-SPECIAL-GROUPING
00832         GO TO 0410-SKIP-COMP-EDIT.
00833
00834      IF PI-APPLIED (1) = 'N' OR
00835         PI-APPLIED (2) = 'N' OR
00836         PI-APPLIED (3) = 'N'
00837          GO TO 0410-SKIP-COMP-EDIT.
00838
00839      MOVE SPACE                  TO  OVERWRITE-AGENT-SW.
00840      MOVE PI-COMPANY-CD          TO  COMP-COMP-CD.
00841      MOVE '1'                    TO  WS-SECOND-READ.
00842
00843      IF ACCTI = LOW-VALUES
00844          GO TO 0405-VERIFY-G-ONLY.
00845
00846      IF ACCTI NOT = FINRESPI
00847          GO TO 0410-SKIP-COMP-EDIT.
00848
00849  0405-VERIFY-A-COMP.
00850
00851      MOVE 'A'                TO  COMP-RECORD-TYPE.
00852
00853      
      * EXEC CICS HANDLE CONDITION
00854 *        NOTFND   (0410-NO-A-COMP)
00855 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00856 *        END-EXEC.
      *    MOVE '"$IJ                  ! # #00003327' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033333237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00857
00858      
      * EXEC CICS READ
00859 *        DATASET  (COMP-FILE-ID)
00860 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00861 *        RIDFLD   (ERCOMP-KEY)
00862 *        EQUAL
00863 *        END-EXEC.
      *    MOVE '&"S        E          (   #00003332' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00864
00865      IF COMP-FIN-RESP NOT = COMP-ACCOUNT
00866          NEXT SENTENCE
00867      ELSE
00868          GO TO 0410-SKIP-COMP-EDIT.
00869
00870      MOVE LOW-VALUES         TO  COMP-ACCOUNT.
00871      MOVE 'G'                TO  COMP-RECORD-TYPE.
00872
00873      
      * EXEC CICS HANDLE CONDITION
00874 *        NOTFND   (0410-NO-G-FOR-A)
00875 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00876 *        END-EXEC.
      *    MOVE '"$IJ                  ! $ #00003347' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033333437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00877
00878      
      * EXEC CICS READ
00879 *        DATASET  (COMP-FILE-ID)
00880 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00881 *        RIDFLD   (ERCOMP-KEY)
00882 *        EQUAL
00883 *        END-EXEC.
      *    MOVE '&"S        E          (   #00003352' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00884
00885      GO TO 0410-SKIP-COMP-EDIT.
00886
00887  0410-NO-A-COMP.
00888
00889      MOVE -1                     TO  CARRIERL.
00890      MOVE AL-UABON               TO  CARRIERA
00891                                      GROUPA
00892                                      FINRESPA
00893                                      ACCTA.
00894
00895      MOVE ER-3178                TO  EMI-ERROR
00896
00897      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00898
00899      GO TO 8200-SEND-DATAONLY.
00900
00901  0410-NO-G-FOR-A.
00902
00903      MOVE -1                     TO  CARRIERL.
00904      MOVE AL-UABON               TO  CARRIERA
00905                                      GROUPA
00906                                      FINRESPA
00907                                      ACCTA.
00908
00909      MOVE ER-3193                TO  EMI-ERROR
00910
00911      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00912
00913      GO TO 8200-SEND-DATAONLY.
00914
00915  0405-VERIFY-G-ONLY.
00916
00917      MOVE 'G'                TO  COMP-RECORD-TYPE.
00918
00919      
      * EXEC CICS HANDLE CONDITION
00920 *        NOTFND   (0410-NO-G-COMP)
00921 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00922 *        END-EXEC.
      *    MOVE '"$IJ                  ! % #00003393' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00923
00924      
      * EXEC CICS READ
00925 *        DATASET  (COMP-FILE-ID)
00926 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00927 *        RIDFLD   (ERCOMP-KEY)
00928 *        EQUAL
00929 *        END-EXEC.
      *    MOVE '&"S        E          (   #00003398' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00930
00931      GO TO 0410-SKIP-COMP-EDIT.
00932
00933  0410-NO-G-COMP.
00934
00935      MOVE -1                     TO  CARRIERL.
00936      MOVE AL-UABON               TO  CARRIERA
00937                                      GROUPA
00938                                      FINRESPA
00939                                      ACCTA.
00940
00941      MOVE ER-3179                TO  EMI-ERROR.
00942
00943      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00944
00945      GO TO 8200-SEND-DATAONLY.
00946
00947  0410-SKIP-COMP-EDIT.
00948
00949      IF MAINTI = 'C'
00950          GO TO 1000-EDIT-DATA.
00951
00952      IF EIBAID = DFHPF1
00953        OR DFHENTER
00954          GO TO 4000-BROWSE-FRWD.
00955
00956      IF EIBAID = DFHPF2
00957          GO TO 4100-BROWSE-BKWD.
00958
00959      EJECT
00960  0500-CREATE-TEMP-STORAGE.
00961
00962      PERFORM 0800-DELETE-TS  THRU  0890-EXIT.
00963
00964      
      * EXEC CICS WRITEQ TS
00965 *        QUEUE  (QID)
00966 *        FROM   (PROGRAM-INTERFACE-BLOCK)
00967 *        LENGTH (PI-COMM-LENGTH)
00968 *    END-EXEC.
      *    MOVE '*"                    ''   #00003438' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00969
00970  0590-EXIT.
00971       EXIT.
00972
00973  0600-RECOVER-TEMP-STORAGE.
00974      
      * EXEC CICS READQ TS
00975 *        QUEUE  (QID)
00976 *        INTO   (PROGRAM-INTERFACE-BLOCK)
00977 *        LENGTH (PI-COMM-LENGTH)
00978 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00003448' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00979
00980      PERFORM 0800-DELETE-TS THRU 0890-EXIT.
00981
00982  0690-EXIT.
00983       EXIT.
00984
00985  0800-DELETE-TS.
00986      
      * EXEC CICS HANDLE CONDITION
00987 *        QIDERR (0890-EXIT)
00988 *    END-EXEC.
      *    MOVE '"$N                   ! & #00003460' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033343630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00989
00990      
      * EXEC CICS DELETEQ TS
00991 *        QUEUE  (QID)
00992 *    END-EXEC.
      *    MOVE '*&                    #   #00003464' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00993
00994      
      * EXEC CICS SYNCPOINT
00995 *    END-EXEC.
      *    MOVE '6"                    !   #00003468' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00996
00997  0890-EXIT.
00998       EXIT.
00999      EJECT
01000
01001  1000-EDIT-DATA.
01002
01003      IF NOT MODIFY-CAP
01004          MOVE 'UPDATE'       TO SM-READ
01005          PERFORM 9995-SECURITY-VIOLATION
01006          MOVE ER-0070        TO EMI-ERROR
01007          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01008          GO TO 8100-SEND-INITIAL-MAP.
01009
01010      SET PINDX                   TO  1.
01011      SET W-INDX                  TO  1.
01012
01013 ****** IF THE REFERENCE NUMBER HAS BEEN FILLED IN AUTOMATICALLY
01014 ****** FROM THE PI-AREA OF EL630,  THE ATTRIBUTE WAS ERASED
01015 ****** WHEN THE SCREEN WAS DISPLAYED IN 8100-SEND-INITIAL-MAP.
01016 ****** UNLESS THE CLIENT TYPES OVER THE REFERENCE NUMBER,
01017 ****** THE PROGRAM IS NOT AWARE THAT THE NUMBER IS DISPLAYED.
01018
01019      IF ((REF-LEN (PINDX) EQUAL ZEROS)
01020          AND (REFERENCE-DISPLAYED)
01021          AND (PI-PYAJ-REFERENCE NOT EQUAL SPACES))
01022            MOVE PI-PYAJ-REFERENCE TO REF (PINDX)
01023            MOVE 12               TO REF-LEN (PINDX)
01024            MOVE AL-UANON         TO REF-ATTRB (PINDX)
01025            MOVE SPACE            TO PI-PYAJ-FILLED-SW
01026            MOVE SPACE            TO PI-PYAJ-REFERENCE.
01027
01028  1010-EDIT-LOOP.
01029
01030      SET NDX                     TO  PINDX.
01031
01032      SET WS-SAVE-INDEX-VALUE     TO  PINDX.
01033
01034      IF COMM-LEN (PINDX)    = ZEROS
01035        AND RTYPE-LEN (PINDX)   = ZEROS
01036        AND AMT-LEN (PINDX)     = ZEROS
01037        AND APPLIED-LEN (PINDX) = ZEROS
01038        AND VOID-SW-LEN (PINDX) = ZEROS
01039        AND SDTE-LEN (PINDX)    = ZEROS
01040        AND IDTE-LEN (PINDX)    = ZEROS
01041          GO TO 1020-CHECK-INVOICE.
01042
01043      IF EIBAID = DFHPF5
01044          GO TO 1010-EDIT-PROCESS.
01045
01046      IF WS-PREV-PF5 = 'Y' AND
01047          COMM-LEN (PINDX)    = ZEROS AND
01048          RTYPE-LEN (PINDX)   = ZEROS AND
01049          AMT-LEN (PINDX)     = ZEROS AND
01050          APPLIED-LEN (PINDX) = ZEROS AND
01051          VOID-SW-LEN (PINDX) = ZEROS AND
01052          SDTE-LEN (PINDX)    = ZEROS AND
01053          IDTE-LEN (PINDX)    = ZEROS
01054          GO TO 1020-CHECK-INVOICE.
01055
01056  1010-EDIT-PROCESS.
01057
01058      IF COMM-LEN (PINDX) NOT = ZEROS
01059          MOVE AL-UANON           TO  COMM-ATTRB (PINDX)
01060          IF PI-COMPANY-ID NOT = 'NCL'
01061              NEXT SENTENCE
01062          ELSE
01063              IF NCL-COMM-DTE (PINDX) IS NUMERIC
01064                  MOVE NCL-COMM-DTE (PINDX)
01065                                  TO  DC-GREG-DATE-1-MDY
01066                  MOVE '4'        TO  DC-OPTION-CODE
01067                  PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
01068                  IF NO-CONVERSION-ERROR
01069                      NEXT SENTENCE
01070                  ELSE
01071                      MOVE ER-2595
01072                                  TO  EMI-ERROR
01073                      MOVE -1     TO  COMM-LEN (PINDX)
01074                      MOVE AL-UABON
01075                                  TO  COMM-ATTRB (PINDX)
01076                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01077              ELSE
01078                  IF NCL-COMM-DTE (PINDX) = SPACES OR LOW-VALUES
01079                      MOVE WS-CURRENT-MDY
01080                                  TO  NCL-COMM-DTE (PINDX)
01081                  ELSE
01082                      MOVE COMM (PINDX)
01083                                  TO  WS-COMMENT-FULL
01084                      MOVE WS-COMMENT-24-POS
01085                                  TO  NCL-COMM-REST (PINDX)
01086                      MOVE WS-CURRENT-MDY
01087                                  TO  NCL-COMM-DTE (PINDX)
01088      ELSE
01089          IF PI-COMPANY-ID = 'NCL'
01090            AND PI-FILE-SEQ-NO (NDX) = ZEROS
01091              MOVE +6             TO  COMM-LEN (PINDX)
01092              MOVE WS-CURRENT-MDY TO  NCL-COMM-DTE (PINDX).
01093
01094      IF RTYPE-LEN (PINDX) NOT = ZEROS
01095          MOVE RTYPE (PINDX)      TO  CHECK-REC-TYPE
01096          IF MORTGAGE-SESSION
01097             IF NOT VALID-MTG-REC-TYPE
01098                MOVE -1           TO  RTYPE-LEN (PINDX)
01099                MOVE ER-2234      TO  EMI-ERROR
01100                MOVE AL-UABON     TO  RTYPE-ATTRB (PINDX)
01101                PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01102             ELSE
01103                MOVE AL-UANON     TO  RTYPE-ATTRB (PINDX)
01104          ELSE
01105             IF PI-COMPANY-ID = 'MON'
01106                 IF NOT MON-VALID-REC-TYPE
01107                    MOVE -1       TO  RTYPE-LEN (PINDX)
01108                    MOVE ER-7806  TO  EMI-ERROR
01109                    MOVE AL-UABON TO  RTYPE-ATTRB (PINDX)
01110                    PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01111                 ELSE
01112                    MOVE AL-UANON TO  RTYPE-ATTRB (PINDX)
01113             ELSE
01114                 IF PI-COMPANY-ID = 'NCL'
01115                     IF NOT NCL-VALID-REC-TYPE
01116                        MOVE -1   TO  RTYPE-LEN (PINDX)
01117                        MOVE ER-7806
01118                                  TO  EMI-ERROR
01119                        MOVE AL-UABON
01120                                  TO  RTYPE-ATTRB (PINDX)
01121                        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01122                     ELSE
01123                        MOVE AL-UANON
01124                                  TO  RTYPE-ATTRB (PINDX)
01125             ELSE
01126                 IF PI-COMPANY-ID = 'ANL'
01127                     IF NOT ANL-VALID-REC-TYPE
01128                        MOVE -1   TO  RTYPE-LEN (PINDX)
01129                        MOVE ER-7806
01130                                  TO  EMI-ERROR
01131                        MOVE AL-UABON
01132                                  TO  RTYPE-ATTRB (PINDX)
01133                        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01134                     ELSE
01135                        MOVE AL-UANON
01136                                  TO  RTYPE-ATTRB (PINDX)
01137                 ELSE
01138                     IF NOT VALID-REC-TYPE
01139                         MOVE -1  TO  RTYPE-LEN (PINDX)
01140                         MOVE ER-2234
01141                                  TO  EMI-ERROR
01142                         MOVE AL-UABON
01143                                  TO  RTYPE-ATTRB (PINDX)
01144                         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01145                     ELSE
01146                         MOVE AL-UANON
01147                                  TO  RTYPE-ATTRB (PINDX)
01148      ELSE
01149          IF PI-FILE-SEQ-NO (NDX) = ZEROS
01150              MOVE -1             TO  RTYPE-LEN (PINDX)
01151              MOVE ER-2235        TO  EMI-ERROR
01152              MOVE AL-UABON       TO  RTYPE-ATTRB (PINDX)
01153              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01154
01155      IF AMT-LEN (PINDX) NOT = ZEROS
01156          
      * EXEC CICS BIF DEEDIT
01157 *            FIELD (AMT (PINDX))
01158 *            LENGTH (11)
01159 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003630' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMT(PINDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01160          IF AMT(PINDX) = ZEROS
01161              IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS
01162                  SET WS-INDX  TO PINDX
01163                  MOVE AMT(PINDX) TO  WS-EDITED-AMT (WS-INDX)
01164                  GO TO 1050-INCREMENT-PINDX
01165              ELSE
01166                  MOVE ER-2245    TO  EMI-ERROR
01167                  MOVE -1         TO  AMT-LEN(PINDX)
01168                  MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)
01169                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01170          ELSE
01171              IF AMT (PINDX) NUMERIC
01172                  SET WS-INDX  TO PINDX
01173                  MOVE AMT (PINDX)
01174                                  TO  WS-EDITED-AMT (WS-INDX)
01175              ELSE
01176                  MOVE ER-2245    TO  EMI-ERROR
01177                  MOVE -1         TO  AMT-LEN(PINDX)
01178                  MOVE AL-UNBON   TO  AMT-ATTRB (PINDX)
01179                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01180      ELSE
01181          IF PI-FILE-SEQ-NO (NDX) = ZEROS
01182              MOVE -1             TO  AMT-LEN (PINDX)
01183              MOVE ER-2236        TO  EMI-ERROR
01184              MOVE AL-UNBON       TO  AMT-ATTRB (PINDX)
01185              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01186
01187      IF APPLIED-LEN (PINDX) NOT = ZERO
01188          IF APPLIED  (PINDX) = 'A' OR 'G' OR 'O'
01189              NEXT SENTENCE
01190          ELSE
01191              MOVE -1             TO APPLIED-LEN   (PINDX)
01192              MOVE ER-3146        TO EMI-ERROR
01193              MOVE AL-UABON       TO APPLIED-ATTRB (PINDX)
01194              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01195      ELSE
01196          IF PI-FILE-SEQ-NO (NDX) = ZEROS
01197              MOVE -1             TO  APPLIED-LEN (PINDX)
01198              MOVE ER-3146        TO  EMI-ERROR
01199              MOVE AL-UABON       TO  APPLIED-ATTRB (PINDX)
01200              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01201
01202      IF APPLIED-LEN (PINDX) NOT = ZEROS
01203          IF APPLIED (PINDX) = 'A'
01204             IF ACCTI = LOW-VALUES
01205                MOVE -1           TO APPLIED-LEN   (PINDX)
01206                MOVE ER-3195      TO EMI-ERROR
01207                MOVE AL-UABON     TO APPLIED-ATTRB (PINDX)
01208                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01209
01210  1010-COMP-CHECK.
01211
01212      IF EIBAID   = DFHPF8
01213          IF EIBCPOSN = WS-CURSOR-POS-1
01214              SET PINDX TO 1
01215          ELSE
01216              IF EIBCPOSN = WS-CURSOR-POS-2
01217                  SET PINDX TO 2
01218              ELSE
01219                  IF EIBCPOSN = WS-CURSOR-POS-3
01220                      SET PINDX TO 3
01221                  ELSE
01222                      MOVE ER-3262
01223                                  TO EMI-ERROR
01224                      MOVE -1     TO COMM-LEN (PINDX)
01225                      MOVE AL-UABON
01226                                  TO COMM-ATTRB (PINDX)
01227                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01228                      SET PINDX UP  BY 3
01229                      GO TO 1050-INCREMENT-PINDX.
01230
01231      MOVE PI-COMPANY-CD          TO COMP-COMP-CD.
01232      MOVE CARRIERI               TO COMP-CARRIER.
01233      MOVE GROUPI                 TO COMP-GROUPING.
01234      MOVE FINRESPI               TO COMP-FIN-RESP.
01235
01236      IF EIBAID NOT = DFHPF8
01237          IF APPLIED-LEN (PINDX) NOT = ZEROS
01238              IF (APPLIED (PINDX) = 'O') OR
01239                 (ACCTI = SPACES OR LOW-VALUES)
01240                  GO TO 1011-VERIFY-COMP-G
01241              ELSE
01242                  NEXT SENTENCE
01243          ELSE
01244              IF (PI-APPLIED (NDX) = 'O') OR
01245                 (ACCTI = SPACES OR LOW-VALUES)
01246                  GO TO 1011-VERIFY-COMP-G
01247              ELSE
01248                  NEXT SENTENCE
01249      ELSE
01250          SET NDX TO PINDX
01251          IF (PI-APPLIED (NDX) = 'O') OR
01252             (ACCTI = SPACES OR LOW-VALUES)
01253              GO TO 1011-VERIFY-COMP-G.
01254
01255      MOVE ACCTI                  TO COMP-ACCOUNT.
01256      MOVE 'A'                    TO COMP-RECORD-TYPE.
01257
01258      
      * EXEC CICS HANDLE CONDITION
01259 *        NOTFND   (1011-NO-COMP-A)
01260 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
01261 *        END-EXEC.
      *    MOVE '"$IJ                  ! '' #00003732' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033373332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01262
01263      
      * EXEC CICS READ
01264 *        DATASET  (COMP-FILE-ID)
01265 *        SET      (ADDRESS OF COMPENSATION-MASTER)
01266 *        RIDFLD   (ERCOMP-KEY)
01267 *        EQUAL
01268 *        END-EXEC.
      *    MOVE '&"S        E          (   #00003737' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01269
01270  1011-CHECK-FOR-A.
01271
01272      IF EIBAID NOT = DFHPF8
01273          IF APPLIED (PINDX) = 'A'
01274               GO TO 1012-CHK-VOID
01275          ELSE
01276              NEXT SENTENCE
01277      ELSE
01278          IF PI-APPLIED (NDX) = 'A'
01279               GO TO 1025-CHECK-INVOICE.
01280
01281      IF EIBAID NOT = DFHPF8
01282          IF APPLIED (PINDX) = 'G'
01283               GO TO 1011-VERIFY-COMP-G
01284          ELSE
01285              NEXT SENTENCE
01286      ELSE
01287          IF PI-APPLIED (NDX) = 'G'
01288               GO TO 1011-VERIFY-COMP-G.
01289
01290      IF EIBAID = DFHPF8
01291          GO TO 1025-CHECK-INVOICE
01292      ELSE
01293          IF APPLIED (PINDX) = 'O'
01294              MOVE -1             TO APPLIED-LEN   (PINDX)
01295              MOVE ER-3196        TO EMI-ERROR
01296              MOVE AL-UABON       TO APPLIED-ATTRB (PINDX)
01297              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01298              GO TO 1012-CHK-VOID.
01299
01300      GO TO 1012-CHK-VOID.
01301
01302  1011-NO-COMP-A.
01303
01304       MOVE ER-3178               TO  EMI-ERROR.
01305       MOVE -1                    TO  CARRIERL.
01306       MOVE AL-UABON              TO  CARRIERA
01307                                      GROUPA
01308                                      FINRESPA.
01309
01310       PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01311
01312       GO TO 1011-CHECK-FOR-A.
01313
01314  1011-VERIFY-COMP-G.
01315
01316      MOVE LOW-VALUES             TO  COMP-ACCOUNT.
01317      MOVE 'G'                    TO  COMP-RECORD-TYPE.
01318
01319      
      * EXEC CICS HANDLE CONDITION
01320 *        NOTFND   (1011-NO-COMP-G)
01321 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
01322 *        END-EXEC.
      *    MOVE '"$IJ                  ! ( #00003793' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033373933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01323
01324      
      * EXEC CICS READ
01325 *        DATASET  (COMP-FILE-ID)
01326 *        SET      (ADDRESS OF COMPENSATION-MASTER)
01327 *        RIDFLD   (ERCOMP-KEY)
01328 *        EQUAL
01329 *        END-EXEC.
      *    MOVE '&"S        E          (   #00003798' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01330
01331       GO TO 1012-CHK-VOID.
01332
01333  1011-NO-COMP-G.
01334
01335       MOVE ER-3260               TO  EMI-ERROR.
01336       MOVE -1                    TO  CARRIERL.
01337       MOVE AL-UABON              TO  CARRIERA
01338                                      GROUPA
01339                                      FINRESPA.
01340
01341       PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01342
01343  1012-CHK-VOID.
01344
01345      IF EIBAID = DFHPF8
01346          GO TO 1025-CHECK-INVOICE.
01347
01348      IF VOID-SW-LEN (PINDX) NOT = ZEROS
01349          IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS
01350            AND (PI-REC-TYPE (NDX) = 'C')
01351              IF VOID-SW (PINDX) = 'V'
01352                  MOVE AL-UANON   TO  VOID-SW-ATTRB (PINDX)
01353              ELSE
01354                  MOVE ER-2246    TO  EMI-ERROR
01355                  MOVE -1         TO  VOID-SW-LEN (PINDX)
01356                  MOVE AL-UABON   TO  VOID-SW-ATTRB (PINDX)
01357                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01358          ELSE
01359              MOVE ER-2449        TO  EMI-ERROR
01360              MOVE -1             TO  VOID-SW-LEN (PINDX)
01361              MOVE AL-UABON       TO  VOID-SW-ATTRB (PINDX)
01362              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01363
01364      IF SDTE-LEN (PINDX) = ZEROS
01365          GO TO 1016-CHECK-INPUT-DATE.
01366
01367      MOVE AL-UNNON               TO  SDTE-ATTRB (PINDX).
01368
01369      IF SDTE (PINDX) NOT NUMERIC OR
01370         SDTE (PINDX) EQUAL ZEROS
01371         GO TO 1015-DAY-ERROR.
01372
01373      MOVE SDTE (PINDX)           TO  DC-GREG-DATE-1-MDY.
01374      MOVE '4'                    TO  DC-OPTION-CODE.
01375      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
01376
01377      IF NO-CONVERSION-ERROR
01378         SET PINDEX               TO  PINDX
01379         MOVE DC-BIN-DATE-1       TO  WS-EOM-DT (PINDEX)
01380      ELSE
01381         GO TO 1015-DAY-ERROR.
01382
01383      MOVE SDTE (PINDX)           TO DATE-TEST-AREA.
01384
01385      IF DATE-TEST-MM = 4 OR  6  OR  9  OR  11
01386          IF DATE-TEST-DD  NOT = 30
01387              GO TO 1015-DAY-ERROR
01388          ELSE
01389              GO TO 1012-CHECK-FUTURE-MO.
01390
01391      IF DATE-TEST-MM = 1 OR  3  OR  5  OR  7  OR
01392                              8  OR  10  OR  12
01393          IF DATE-TEST-DD  NOT = 31
01394              GO TO 1015-DAY-ERROR
01395          ELSE
01396              GO TO 1012-CHECK-FUTURE-MO.
01397
01398      DIVIDE DATE-TEST-YY  BY  4  GIVING  DIVIDE-RESULT
01399          REMAINDER  DIVIDE-REMAINDER.
01400
01401      IF DATE-TEST-YY = ZERO
01402          IF DATE-TEST-DD = 29
01403              GO TO 1012-CHECK-FUTURE-MO
01404          ELSE
01405              GO TO 1015-DAY-ERROR.
01406
01407      IF DIVIDE-REMAINDER NOT = ZERO
01408          IF DATE-TEST-DD  NOT = 28
01409              GO TO 1015-DAY-ERROR
01410          ELSE
01411              GO TO 1012-CHECK-FUTURE-MO
01412      ELSE
01413          IF DATE-TEST-DD = 29
01414              GO TO 1012-CHECK-FUTURE-MO
01415          ELSE
01416              GO TO 1015-DAY-ERROR.
01417
01418  1012-CHECK-FUTURE-MO.
01419
01420      IF WS-EOM-DT (PINDEX) NOT GREATER THAN PI-CR-MONTH-END-DT
01421          GO TO 1016-CHECK-INPUT-DATE.
01422
01423      MOVE PI-CR-MONTH-END-DT     TO  DC-BIN-DATE-1.
01424      MOVE WS-EOM-DT (PINDEX)     TO  DC-BIN-DATE-2.
01425      MOVE '1'                    TO  DC-OPTION-CODE.
01426      PERFORM 8500-DATE-CONVERT  THRU 8500-EXIT.
01427
01428      IF DATE-CONVERSION-ERROR
01429         GO TO 1015-DAY-ERROR.
01430
01431      IF DC-ELAPSED-MONTHS  GREATER THAN  +2
01432         MOVE -1                  TO  SDTE-LEN (PINDX)
01433         MOVE AL-UNBON            TO  SDTE-ATTRB (PINDX)
01434         MOVE ER-0761             TO  EMI-ERROR
01435         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01436
01437      GO TO 1016-CHECK-INPUT-DATE.
01438
01439  1015-DAY-ERROR.
01440      MOVE -1                     TO  SDTE-LEN (PINDX).
01441      MOVE AL-UNBON               TO  SDTE-ATTRB (PINDX).
01442      MOVE ER-0587                TO  EMI-ERROR.
01443
01444      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01445
01446  1016-CHECK-INPUT-DATE.
01447
01448      IF IDTE-LEN (PINDX)  =  ZEROS
01449          GO TO 1020-CHECK-INVOICE.
01450
01451      MOVE AL-UNNON               TO  IDTE-ATTRB (PINDX).
01452
01453      IF IDTE (PINDX) NOT NUMERIC OR
01454         IDTE (PINDX) EQUAL ZEROS
01455          GO TO 1017-DAY-ERROR.
01456
01457      MOVE IDTE (PINDX)           TO  DC-GREG-DATE-1-MDY.
01458      MOVE '4'                    TO  DC-OPTION-CODE.
01459
01460      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
01461
01462      IF NO-CONVERSION-ERROR
01463          SET DINDEX              TO  PINDX
01464          MOVE DC-BIN-DATE-1      TO  WS-INP-DT (DINDEX)
01465          GO TO 1020-CHECK-INVOICE.
01466
01467  1017-DAY-ERROR.
01468      MOVE -1                     TO  IDTE-LEN (PINDX).
01469      MOVE AL-UNBON               TO  IDTE-ATTRB (PINDX).
01470      MOVE ER-0714                TO  EMI-ERROR.
01471
01472      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01473
01474  1020-CHECK-INVOICE.
01475
01476      IF MORTGAGE-SESSION
01477         IF AMT-LEN (PINDX) NOT = ZEROS
01478            IF PI-FILE-SEQ-NO (NDX) = ZEROS
01479               IF RTYPE (PINDX) = 'R'
01480                  IF INVOICE-LEN (PINDX) = ZEROS
01481                     MOVE -1            TO INVOICE-LEN (PINDX)
01482                     MOVE ER-3175       TO EMI-ERROR
01483                     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01484                     GO TO 1050-INCREMENT-PINDX
01485                  ELSE
01486                     PERFORM 6500-VERIFY-RECON-HEADER THRU
01487                                                      6590-EXIT.
01488      IF (INVOICE (PINDX) = SPACES OR LOW-VALUES)
01489          GO TO 1050-INCREMENT-PINDX.
01490
01491      IF WS-ACCEPT-1 (W-INDX) = 'A'
01492          GO TO 1050-INCREMENT-PINDX.
01493
01494      IF EIBAID = DFHPF5
01495          MOVE 'A'                TO WS-ACCEPT-1 (W-INDX)
01496          MOVE 'Y'                TO WS-PREV-PF5
01497          MOVE DFHENTER           TO EIBAID
01498          GO TO 1050-INCREMENT-PINDX.
01499
01500  1025-CHECK-INVOICE.
01501
01502      MOVE PI-COMPANY-CD          TO RECV-COMP-CD.
01503      MOVE '1'                    TO RECV-TYPE.
01504      MOVE CARRIERI               TO RECV-CARRIER.
01505      MOVE GROUPI                 TO RECV-GROUPING.
01506      MOVE CO-AR-BAL-LEVEL        TO RECV-BAL-LVL.
01507      MOVE FINRESPI               TO RECV-FIN-RESP.
01508      MOVE ACCTI                  TO RECV-ACCOUNT.
01509
01510      IF EIBAID = DFHPF8
01511          MOVE PI-INVOICE (NDX)   TO RECV-INVOICE
01512          MOVE PI-REFERENCE (NDX) TO RECV-REFERENCE
01513          IF PI-APPLIED (NDX) = 'A'
01514              MOVE '1'            TO RECV-ENTRY-TYPE
01515              MOVE 'A'            TO RECV-RESPONSIBLE
01516          ELSE
01517              MOVE LOW-VALUES     TO RECV-RESPONSIBLE
01518              IF PI-APPLIED (NDX) = 'G'
01519                  MOVE '2'        TO RECV-ENTRY-TYPE
01520              ELSE
01521                  MOVE '3'        TO RECV-ENTRY-TYPE
01522      ELSE
01523          MOVE INVOICE (PINDX)    TO RECV-INVOICE
01524          MOVE REF (PINDX)        TO RECV-REFERENCE
01525          IF APPLIED (PINDX) = 'A'
01526              MOVE '1'            TO RECV-ENTRY-TYPE
01527              MOVE 'A'            TO RECV-RESPONSIBLE
01528          ELSE
01529              MOVE LOW-VALUES     TO RECV-RESPONSIBLE
01530              IF APPLIED (PINDX) = 'G'
01531                  MOVE '2'        TO RECV-ENTRY-TYPE
01532              ELSE
01533                  MOVE '3'        TO RECV-ENTRY-TYPE.
01534
01535
01536      MOVE ZERO                   TO RECV-RECORD-TYPE.
01537      MOVE +0                     TO RECV-RECORD-SEQ.
01538
01539      INSPECT ERRECV-KEY REPLACING ALL SPACES BY LOW-VALUES.
01540
01541      
      * EXEC CICS HANDLE CONDITION
01542 *        NOTFND   (1040-RECV-NOTFND)
01543 *        NOTOPEN  (7200-RECV-FILE-NOTOPEN)
01544 *        END-EXEC.
      *    MOVE '"$IJ                  ! ) #00004015' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034303135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01545
01546      
      * EXEC CICS READ
01547 *        DATASET  (RECV-FILE-ID)
01548 *        SET      (ADDRESS OF ACCOUNTS-RECEIVABLE)
01549 *        RIDFLD   (ERRECV-KEY)
01550 *        EQUAL
01551 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004020' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RECV-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRECV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNTS-RECEIVABLE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01552
01553      IF AR-BAL-DEBIT
01554          MOVE AR-BAL-AMOUNT      TO WS-AR-BALANCE
01555      ELSE
01556          COMPUTE WS-AR-BALANCE = AR-BAL-AMOUNT * -1.
01557
01558      IF EIBAID = DFHPF8
01559          MOVE PI-AMOUNT (NDX)    TO WS-WORK-BALANCE
01560          IF PI-REC-TYPE (NDX) = 'C' OR 'U'
01561              COMPUTE WS-WORK-BALANCE = WS-WORK-BALANCE * -1
01562          ELSE
01563              NEXT SENTENCE
01564      ELSE
01565          IF AMT (PINDX) NUMERIC
01566              MOVE AMT (PINDX)    TO WS-WORK-BALANCE
01567              IF RTYPE (PINDX) = 'C' OR 'U'
01568                  COMPUTE WS-WORK-BALANCE = WS-WORK-BALANCE * -1
01569              ELSE
01570                  NEXT SENTENCE
01571          ELSE
01572              GO TO 1050-INCREMENT-PINDX.
01573
01574      COMPUTE WS-BAL-AMOUNT = WS-AR-BALANCE - WS-WORK-BALANCE.
01575
01576      IF EIBAID = DFHPF8
01577          IF WS-BAL-AMOUNT NOT = ZERO
01578              MOVE AR-BAL-AMOUNT  TO WS-EDIT-PATTERN
01579              MOVE WS-EDIT        TO WS-EDIT-AMOUNT
01580              MOVE ER-3263        TO EMI-ERROR
01581              MOVE -1             TO COMM-LEN (PINDX)
01582              MOVE AL-UABON       TO COMM-ATTRB (PINDX)
01583              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01584              SET PINDX UP  BY 3
01585              IF AR-BAL-CREDIT
01586                  MOVE '-'        TO  WS-EDIT-SIGN
01587                  MOVE WS-EDITED-AMOUNT
01588                                  TO EMI-TEXT-VARIABLE (1)
01589                  GO TO 1050-INCREMENT-PINDX
01590              ELSE
01591                  MOVE ' '        TO  WS-EDIT-SIGN
01592                  MOVE WS-EDITED-AMOUNT
01593                                  TO EMI-TEXT-VARIABLE (1)
01594                  GO TO 1050-INCREMENT-PINDX
01595          ELSE
01596              MOVE ER-3265        TO EMI-ERROR
01597              MOVE -1             TO COMM-LEN (PINDX)
01598              MOVE AL-UABON       TO COMM-ATTRB (PINDX)
01599              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01600              SET PINDX UP  BY 3
01601              GO TO 1050-INCREMENT-PINDX.
01602
01603      IF WS-BAL-AMOUNT NOT = ZERO
01604          MOVE AR-BAL-AMOUNT      TO WS-EDIT-PATTERN
01605          MOVE WS-EDIT            TO WS-EDIT-AMOUNT
01606          MOVE ER-3266            TO EMI-ERROR
01607          MOVE -1                 TO INVOICE-LEN (PINDX)
01608          MOVE AL-UABON           TO INVOICE-ATTRB (PINDX)
01609          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01610          IF AR-BAL-CREDIT
01611              MOVE '-'            TO  WS-EDIT-SIGN
01612              IF EMI-ERROR-NUMBER (1) = '3266'
01613                  MOVE WS-EDITED-AMOUNT
01614                                  TO EMI-TEXT-VARIABLE (1)
01615                  GO TO 1050-INCREMENT-PINDX
01616              ELSE
01617                  MOVE WS-EDITED-AMOUNT
01618                                  TO EMI-TEXT-VARIABLE (2)
01619                  GO TO 1050-INCREMENT-PINDX
01620          ELSE
01621              IF EMI-ERROR-NUMBER (1) = '3266'
01622                  MOVE WS-EDITED-AMOUNT
01623                                  TO EMI-TEXT-VARIABLE (1)
01624                  GO TO 1050-INCREMENT-PINDX
01625              ELSE
01626                  MOVE ' '            TO  WS-EDIT-SIGN
01627                  MOVE WS-EDITED-AMOUNT
01628                                  TO EMI-TEXT-VARIABLE (2)
01629                  GO TO 1050-INCREMENT-PINDX
01630      ELSE
01631          GO TO 1050-INCREMENT-PINDX.
01632
01633  1040-RECV-NOTFND.
01634
01635      IF EIBAID = DFHPF8
01636          MOVE ER-3264            TO EMI-ERROR
01637          MOVE -1                 TO COMM-LEN (PINDX)
01638          MOVE AL-UABON           TO COMM-ATTRB (PINDX)
01639          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01640          SET PINDX  UP  BY  3
01641          GO TO 1050-INCREMENT-PINDX.
01642
01643      MOVE ER-3180                TO EMI-ERROR.
01644      MOVE -1                     TO INVOICE-LEN (PINDX).
01645      MOVE AL-UABON               TO INVOICE-ATTRB (PINDX).
01646
01647      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01648
01649  1050-INCREMENT-PINDX.
01650      SET PINDX  UP  BY  1.
01651      SET W-INDX                  TO  PINDX.
01652      MOVE WS-ACCEPT-TABLE        TO  PI-ACCEPT.
01653
01654      IF PINDX  IS NOT GREATER THAN  +3
01655          GO TO 1010-EDIT-LOOP.
01656
01657      IF EMI-ERROR = ZEROS
01658          NEXT SENTENCE
01659      ELSE
01660          MOVE 'S'                TO  PI-PREV-FUNCTION
01661          GO TO 8200-SEND-DATAONLY.
01662  EJECT
01663  2000-UPDATE-THE-FILE.
01664
01665      SET PINDX                   TO  ZERO-NDX.
01666
01667  2100-UPDATE-LOOP.
01668
01669      SET PINDX  UP  BY  1.
01670      SET NDX                     TO  PINDX.
01671
01672      IF PINDX  IS GREATER THAN  +3
01673          GO TO 2200-UPDATE-COMPLETE.
01674
01675      IF COMM-LEN    (PINDX) = ZEROS
01676        AND RTYPE-LEN   (PINDX) = ZEROS
01677        AND AMT-LEN     (PINDX) = ZEROS
01678        AND APPLIED-LEN (PINDX) = ZEROS
01679        AND VOID-SW-LEN (PINDX) = ZEROS
01680        AND SDTE-LEN    (PINDX) = ZEROS
01681        AND REF-LEN     (PINDX) = ZEROS
01682        AND INVOICE-LEN (PINDX) = ZEROS
01683        AND IDTE-LEN    (PINDX) = ZEROS
01684        AND CREDIT-LEN  (PINDX) = ZEROS
01685        AND DEBIT-LEN   (PINDX) = ZEROS
01686          GO TO 2100-UPDATE-LOOP.
01687
01688      IF PI-FILE-SEQ-NO (NDX) NOT = ZEROS
01689          NEXT SENTENCE
01690      ELSE
01691          GO TO 2110-ADD-RECORD.
01692
01693      
      * EXEC CICS HANDLE CONDITION
01694 *        NOTFND  (2110-ADD-RECORD)
01695 *    END-EXEC.
      *    MOVE '"$I                   ! * #00004167' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303034313637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01696
01697      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.
01698      MOVE PI-FILE-SEQ-NO (NDX)    TO  PYAJ-FILE-SEQ-NO.
01699      MOVE PI-REC-TYPE (NDX)       TO  PYAJ-RECORD-TYPE.
01700
01701      IF MORTGAGE-SESSION
01702         MOVE MPPYAJ-FILE-ID       TO PYAJ-FILE-ID.
01703
01704      
      * EXEC CICS READ
01705 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01706 *        DATASET  (PYAJ-FILE-ID)
01707 *        RIDFLD   (ERPYAJ-KEY)
01708 *        UPDATE
01709 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004178' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01710
01711      IF RTYPE-LEN (PINDX) GREATER THAN +0
01712         IF RTYPE (PINDX) NOT = PY-RECORD-TYPE
01713            PERFORM 2190-CHANGE-RECORD-TYPE THRU 2190-EXIT.
01714
01715      IF AMT-LEN (PINDX) NOT = ZEROS
01716         SET WS-INDX                 TO PINDX
01717          IF WS-EDITED-AMT (WS-INDX)  = ZEROS
01718              IF  (PY-CHECK-WRITTEN-DT = LOW-VALUES) AND
01719                  (PY-CHECK-ORIGIN-SW NOT = 'V')
01720                  GO TO 2120-DELETE-RECORD
01721              ELSE
01722                  IF PI-PROCESSOR-ID = 'LGXX'
01723                      GO TO 2120-DELETE-RECORD
01724              ELSE
01725                      MOVE ER-2244   TO  EMI-ERROR
01726                      MOVE -1        TO  AMT-LEN (PINDX)
01727                      MOVE AL-UNBON  TO  AMT-ATTRB (PINDX)
01728                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01729                      MOVE PY-ENTRY-AMT
01730                                     TO  AMTO (PINDX)
01731                      
      * EXEC CICS UNLOCK
01732 *                    DATASET  (PYAJ-FILE-ID)
01733 *                    END-EXEC
      *    MOVE '&*                    #   #00004205' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01734                      GO TO 8200-SEND-DATAONLY.
01735
01736      MOVE 'B'                    TO  JP-RECORD-TYPE.
01737      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
01738      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01739
01740      PERFORM 8400-LOG-JOURNAL-RECORD.
01741
01742      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
01743      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
01744      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT.
01745
01746      IF COMM-LEN (PINDX) NOT = ZEROS
01747          MOVE COMM (PINDX)       TO  PY-ENTRY-COMMENT.
01748
01749      IF AMT-LEN (PINDX) NOT = ZEROS
01750          SET WS-INDX             TO  PINDX
01751          MOVE PY-ENTRY-AMT       TO  WS-OLD-ENTRY-AMT
01752          MOVE WS-EDITED-AMT (WS-INDX)  TO  PY-ENTRY-AMT
01753                                            WS-ENTRY-AMT.
01754
01755      IF APPLIED-LEN (PINDX) NOT = ZERO
01756          MOVE APPLIED (PINDX)    TO  PY-PMT-APPLIED.
01757
01758      IF VOID-SW-LEN (PINDX) NOT = ZEROS
01759          MOVE VOID-SW (PINDX)    TO  PY-VOID-SW.
01760
01761      IF SDTE-LEN (PINDX) NOT = ZEROS
01762         SET PINDEX               TO  PINDX
01763         MOVE WS-EOM-DT (PINDEX)  TO  PY-CREDIT-SELECT-DT.
01764
01765      IF REF-LEN  (PINDX) NOT = ZEROS
01766         MOVE REF (PINDX)         TO  PY-REF-NO.
01767
01768      IF INVOICE-LEN   (PINDX) NOT = ZEROS
01769         MOVE INVOICE  (PINDX)    TO  PY-BIL-INV.
01770
01771      IF IDTE-LEN (PINDX)  NOT =  ZEROS
01772          SET DINDEX              TO  PINDX
01773          MOVE WS-INP-DT (DINDEX)
01774                                  TO  PY-INPUT-DT.
01775
01776      IF CREDIT-LEN    (PINDX) NOT = ZEROS
01777         MOVE CREDIT   (PINDX)    TO  PY-GL-CR.
01778
01779      IF DEBIT-LEN     (PINDX) NOT = ZEROS
01780         MOVE DEBIT    (PINDX)    TO  PY-GL-DB.
01781
01782      MOVE 'C'                    TO  JP-RECORD-TYPE.
01783      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
01784      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01785
01786      IF MORTGAGE-SESSION
01787         MOVE 'U'                 TO  WS-RECON-SW
01788         PERFORM 6600-UPDATE-RECON-HEADER THRU 6690-EXIT.
01789
01790      
      * EXEC CICS REWRITE
01791 *        DATASET  (PYAJ-FILE-ID)
01792 *        FROM     (PENDING-PAY-ADJ)
01793 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004264' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01794
01795      PERFORM 8400-LOG-JOURNAL-RECORD.
01796
01797      GO TO 2100-UPDATE-LOOP.
01798
01799  EJECT
01800
01801  2110-ADD-RECORD.
01802      
      * EXEC CICS GETMAIN
01803 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01804 *        LENGTH   (ERPYAJ-RECORD-LENGTH)
01805 *        INITIMG  (GETMAIN-SPACE)
01806 *    END-EXEC.
      *    MOVE ',"IL                  $   #00004276' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01807
01808      MOVE 'PY'                   TO  PY-RECORD-ID.
01809      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.
01810      MOVE PI-SAV-CARRIER         TO  PY-CARRIER.
01811      MOVE PI-SAV-GROUPING        TO  PY-GROUPING.
01812      MOVE PI-SAV-FIN-RESP        TO  PY-FIN-RESP.
01813      MOVE PI-SAV-ACCOUNT         TO  PY-ACCOUNT.
01814      MOVE RTYPE (PINDX)          TO  PY-RECORD-TYPE.
01815      MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.
01816
01817      ADD +1                      TO  WORK-SEQ-NO.
01818
01819      IF COMM-LEN (PINDX) NOT = ZEROS
01820          MOVE COMM (PINDX)       TO  PY-ENTRY-COMMENT.
01821
01822      
      * EXEC CICS BIF DEEDIT
01823 *        FIELD (AMT (PINDX))
01824 *        LENGTH (11)
01825 *    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004296' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMT(PINDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01826
01827      MOVE AMT (PINDX)            TO  PY-ENTRY-AMT
01828                                      WS-ENTRY-AMT.
01829
01830      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
01831      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
01832      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT.
01833      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL
01834                                      PY-CHECK-QUE-SEQUENCE.
01835      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT
01836                                      PY-BILLED-DATE
01837                                      PY-AR-DATE
01838                                      PY-GL-DATE
01839                                      PY-REPORTED-DT
01840                                      PY-CHECK-WRITTEN-DT.
01841
01842      IF SDTE (PINDX)  IS NUMERIC
01843          MOVE SDTE (PINDX)       TO  DC-GREG-DATE-1-MDY
01844          MOVE '4'                TO  DC-OPTION-CODE
01845          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
01846          IF DATE-CONVERSION-ERROR
01847              IF MORTGAGE-SESSION
01848                  MOVE PI-MP-MONTH-END-DT
01849                                  TO  PY-CREDIT-SELECT-DT
01850              ELSE
01851                  MOVE PI-CR-MONTH-END-DT
01852                                  TO  PY-CREDIT-SELECT-DT
01853          ELSE
01854              MOVE DC-BIN-DATE-1  TO  PY-CREDIT-SELECT-DT
01855      ELSE
01856          IF MORTGAGE-SESSION
01857              MOVE PI-MP-MONTH-END-DT
01858                                  TO  PY-CREDIT-SELECT-DT
01859          ELSE
01860              MOVE PI-CR-MONTH-END-DT
01861                                  TO  PY-CREDIT-SELECT-DT.
01862
01863      IF REF-LEN  (PINDX) NOT = ZEROS
01864         MOVE REF (PINDX)         TO  PY-REF-NO.
01865
01866      IF INVOICE-LEN   (PINDX) NOT = ZEROS
01867         MOVE INVOICE  (PINDX)    TO  PY-BIL-INV.
01868
01869      IF IDTE (PINDX)  IS NUMERIC
01870          MOVE IDTE (PINDX)       TO  DC-GREG-DATE-1-MDY
01871          MOVE '4'                TO  DC-OPTION-CODE
01872          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
01873          IF DATE-CONVERSION-ERROR
01874              MOVE -1             TO  IDTE-LEN (PINDX)
01875              MOVE AL-UNBON       TO  IDTE-ATTRB (PINDX)
01876              MOVE ER-0714        TO  EMI-ERROR
01877              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01878          ELSE
01879              MOVE DC-BIN-DATE-1  TO  PY-INPUT-DT
01880      ELSE
01881          MOVE WS-CURRENT-BIN-DT  TO  PY-INPUT-DT.
01882
01883      IF CREDIT-LEN    (PINDX) NOT = ZEROS
01884         MOVE CREDIT   (PINDX)    TO  PY-GL-CR.
01885
01886      IF DEBIT-LEN     (PINDX) NOT = ZEROS
01887         MOVE DEBIT    (PINDX)    TO  PY-GL-DB.
01888
01889      MOVE APPLIED (PINDX)        TO  PY-PMT-APPLIED.
01890
01891  2115-WRITE-REC.
01892
01893      
      * EXEC CICS HANDLE CONDITION
01894 *        DUPREC  (2115-DUP-RECORD)
01895 *    END-EXEC.
      *    MOVE '"$%                   ! + #00004367' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303034333637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01896
01897      MOVE 'A'                    TO  JP-RECORD-TYPE.
01898      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
01899      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01900
01901      IF MORTGAGE-SESSION
01902         MOVE 'A'                 TO WS-RECON-SW
01903         PERFORM 6600-UPDATE-RECON-HEADER THRU 6690-EXIT.
01904
01905  2115-RETRY-WRITE.
01906
01907      
      * EXEC CICS WRITE
01908 *        DATASET  (PYAJ-FILE-ID)
01909 *        FROM     (PENDING-PAY-ADJ)
01910 *        RIDFLD   (PY-CONTROL-PRIMARY)
01911 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004381' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01912
01913      PERFORM 8400-LOG-JOURNAL-RECORD.
01914
01915      GO TO 2100-UPDATE-LOOP.
01916
01917  2115-DUP-RECORD.
01918
01919      COMPUTE PY-FILE-SEQ-NO = PY-FILE-SEQ-NO + 1.
01920
01921      GO TO 2115-RETRY-WRITE.
01922
01923  EJECT
01924
01925  2120-DELETE-RECORD.
01926      IF PY-RECORD-TYPE NOT = 'C' AND 'U'
01927          GO TO 2120-DELETE-CONT.
01928
01929      IF PY-CHECK-QUE-CONTROL = ZEROS
01930        AND PY-CHECK-QUE-SEQUENCE = ZEROS
01931          GO TO 2120-DELETE-CONT.
01932
01933      
      * EXEC CICS HANDLE CONDITION
01934 *        NOTFND  (2120-DELETE-CONT)
01935 *    END-EXEC.
      *    MOVE '"$I                   ! , #00004407' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303034343037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01936
01937      MOVE PY-COMPANY-CD          TO  CHKQ-COMPANY-CD.
01938      MOVE PY-CHECK-QUE-CONTROL   TO  CHKQ-CONTROL-NUMBER.
01939      MOVE PY-CHECK-QUE-SEQUENCE  TO  CHKQ-SEQUENCE-NUMBER.
01940
01941      
      * EXEC CICS READ
01942 *        SET      (ADDRESS OF CHECK-QUE)
01943 *        DATASET  (CHKQ-FILE-ID)
01944 *        RIDFLD   (ERCHKQ-KEY)
01945 *        UPDATE
01946 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004415' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CHKQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCHKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01947
01948      MOVE 'D'                    TO  JP-RECORD-TYPE.
01949      MOVE CHKQ-FILE-ID           TO  JP-FILE-ID.
01950      MOVE CHECK-QUE              TO  JP-RECORD-AREA.
01951
01952      PERFORM 8400-LOG-JOURNAL-RECORD.
01953
01954      
      * EXEC CICS DELETE
01955 *        DATASET  (CHKQ-FILE-ID)
01956 *    END-EXEC.
      *    MOVE '&(                    &   #00004428' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CHKQ-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01957
01958  2120-DELETE-CONT.
01959      MOVE 'D'                    TO  JP-RECORD-TYPE.
01960      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
01961      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01962
01963      PERFORM 8400-LOG-JOURNAL-RECORD.
01964
01965      MOVE PY-ENTRY-AMT           TO WS-ENTRY-AMT.
01966
01967      IF MORTGAGE-SESSION
01968         MOVE 'D'                 TO WS-RECON-SW
01969         PERFORM 6600-UPDATE-RECON-HEADER THRU 6690-EXIT.
01970
01971      
      * EXEC CICS DELETE
01972 *        DATASET(PYAJ-FILE-ID)
01973 *    END-EXEC.
      *    MOVE '&(                    &   #00004445' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01974
01975      GO TO 2100-UPDATE-LOOP.
01976
01977
01978  2190-CHANGE-RECORD-TYPE.
01979
01980      MOVE 'D'                    TO  JP-RECORD-TYPE.
01981      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
01982      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01983
01984      PERFORM 8400-LOG-JOURNAL-RECORD.
01985
01986      
      * EXEC CICS DELETE
01987 *        DATASET(PYAJ-FILE-ID)
01988 *    END-EXEC.
      *    MOVE '&(                    &   #00004460' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01989
01990      
      * EXEC CICS GETMAIN
01991 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01992 *        LENGTH   (ERPYAJ-RECORD-LENGTH)
01993 *        INITIMG  (GETMAIN-SPACE)
01994 *    END-EXEC.
      *    MOVE ',"IL                  $   #00004464' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01995
01996      MOVE JP-RECORD-AREA         TO PENDING-PAY-ADJ.
01997      MOVE ERPYAJ-KEY             TO PY-CONTROL-PRIMARY.
01998
01999      MOVE RTYPE (PINDX)          TO PY-RECORD-TYPE
02000                                     PYAJ-RECORD-TYPE.
02001      
      * EXEC CICS HANDLE CONDITION
02002 *        DUPREC  (2190-DUP-RECORD)
02003 *    END-EXEC.
      *    MOVE '"$%                   ! - #00004475' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303034343735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02004
02005      MOVE 'A'                    TO  JP-RECORD-TYPE.
02006      MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
02007      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
02008
02009
02010  2190-RETRY-WRITE.
02011
02012      
      * EXEC CICS WRITE
02013 *        DATASET  (PYAJ-FILE-ID)
02014 *        FROM     (PENDING-PAY-ADJ)
02015 *        RIDFLD   (PY-CONTROL-PRIMARY)
02016 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004486' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02017
02018      PERFORM 8400-LOG-JOURNAL-RECORD.
02019
02020      
      * EXEC CICS READ
02021 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
02022 *        DATASET  (PYAJ-FILE-ID)
02023 *        RIDFLD   (ERPYAJ-KEY)
02024 *        UPDATE
02025 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004494' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02026
02027      GO TO 2190-EXIT.
02028
02029
02030  2190-DUP-RECORD.
02031
02032      COMPUTE PY-FILE-SEQ-NO = PY-FILE-SEQ-NO + 1.
02033
02034      GO TO 2190-RETRY-WRITE.
02035
02036  2190-EXIT.
02037      EXIT.
02038
02039  2200-UPDATE-COMPLETE.
02040      IF EIBAID = DFHPF1
02041          GO TO 4000-BROWSE-FRWD.
02042
02043      IF EIBAID = DFHPF2
02044          GO TO 4100-BROWSE-BKWD.
02045
02046      MOVE LOW-VALUES             TO  EL635AI.
02047
02048      IF WARNING-SW = 'N'
02049          MOVE ER-0000            TO  EMI-ERROR
02050      ELSE
02051          MOVE ER-3172            TO  EMI-ERROR.
02052
02053      MOVE -1                     TO  MAINTL.
02054
02055      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02056
02057      GO TO 8100-SEND-INITIAL-MAP.
02058  EJECT
02059  4000-BROWSE-FRWD.
02060
02061      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.
02062
02063      IF EIBAID = DFHPF1
02064          IF PAGE-FULL
02065              MOVE PI-FILE-SEQ-NO (3)
02066                                       TO  PYAJ-FILE-SEQ-NO
02067              MOVE HIGH-VALUES         TO  PYAJ-RECORD-TYPE
02068              MOVE 'N'                 TO  PYAJ-READ-SW
02069              MOVE PI-SAV-ACCT-AMT     TO  TOTAL-ACCT-AMT
02070              MOVE PI-SAV-ACCT-NET     TO  TOTAL-ACCT-NET
02071          ELSE
02072              IF TOP-OF-FILE
02073                 MOVE SPACE            TO  PI-PYAJ-FILE-SW
02074                 MOVE ZEROS            TO  PYAJ-FILE-SEQ-NO
02075                 MOVE SPACES           TO  PYAJ-RECORD-TYPE
02076              ELSE
02077                 MOVE 99999999         TO  PYAJ-FILE-SEQ-NO
02078                 MOVE HIGH-VALUES      TO  PYAJ-RECORD-TYPE
02079      ELSE
02080          MOVE ZEROS                   TO  PYAJ-FILE-SEQ-NO
02081          MOVE SPACES                  TO  PYAJ-RECORD-TYPE.
02082
02083
02084      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
02085          MOVE PI-CARRIER-SECURITY  TO  PYAJ-CARRIER.
02086
02087  4000-BROWSE-FRWD-FOR-PREV.
02088
02089
02090      MOVE SPACE                  TO PI-REC-TYPE (1)
02091                                     PI-REC-TYPE (2)
02092                                     PI-REC-TYPE (3).
02093
02094      MOVE ZEROS                  TO PI-FILE-SEQ-NO (1)
02095                                     PI-FILE-SEQ-NO (2)
02096                                     PI-FILE-SEQ-NO (3)
02097                                     PI-AMOUNT (1)
02098                                     PI-AMOUNT (2)
02099                                     PI-AMOUNT (3).
02100
02101      MOVE SPACE                  TO PI-APPLIED (1)
02102                                     PI-REFERENCE (1)
02103                                     PI-INVOICE (1)
02104                                     PI-APPLIED (2)
02105                                     PI-REFERENCE (2)
02106                                     PI-INVOICE (2)
02107                                     PI-APPLIED (3)
02108                                     PI-REFERENCE (3)
02109                                     PI-INVOICE (3).
02110
02111      IF END-OF-FILE
02112          IF EIBAID = DFHPF1
02113             IF PI-TOTAL-DISPLAYED
02114                NEXT SENTENCE
02115             ELSE
02116                MOVE -1                 TO  MAINTL
02117                MOVE ER-2237            TO  EMI-ERROR
02118                PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02119                MOVE SPACE              TO  PI-PYAJ-FILE-SW
02120                SET PINDX               TO  +1
02121                MOVE 'TOTAL'            TO  COMM (PINDX)
02122                MOVE PI-SAV-ACCT-AMT    TO  AMTO (PINDX)
02123                MOVE 'NET TOTAL'        TO  REF  (PINDX)
02124                MOVE PI-SAV-ACCT-NET    TO  NETO (PINDX)
02125                MOVE ZEROS              TO  PI-SAV-ACCT-AMT
02126                                         PI-SAV-ACCT-NET
02127                GO TO 8100-SEND-INITIAL-MAP.
02128
02129       IF END-OF-FILE
02130          IF EIBAID = DFHPF1
02131             MOVE -1             TO  MAINTL
02132             MOVE ER-2237        TO  EMI-ERROR
02133             PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02134             GO TO 8200-SEND-DATAONLY.
02135
02136      PERFORM 5000-START-BROWSE  THRU  5030-EXIT.
02137
02138      IF NO-RECORDS
02139         MOVE SPACE              TO  PI-PYAJ-FILE-SW
02140         MOVE LOW-VALUES         TO  EL635AO
02141         MOVE ER-2239            TO  EMI-ERROR
02142         MOVE -1                 TO  MAINTL
02143         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02144         IF (SENT-FROM-PENDING
02145             AND PI-PYAJ-REFERENCE GREATER THAN SPACES)
02146             MOVE PI-PYAJ-REFERENCE TO  REF1O
02147             MOVE 12                TO  REF1L
02148             MOVE AL-UANON          TO  REF1A
02149             MOVE SPACE             TO  EL630-SENT-SW
02150             MOVE 'Y'               TO  PI-PYAJ-FILLED-SW
02151             GO TO 8100-SEND-INITIAL-MAP
02152         ELSE
02153             GO TO 8100-SEND-INITIAL-MAP.
02154
02155      IF NOT-OPEN
02156          GO TO 7000-PYAJ-FILE-NOTOPEN.
02157
02158      MOVE LOW-VALUES             TO  EL635AO.
02159      MOVE ZEROS                  TO  PI-SEQ-NOS.
02160
02161      PERFORM 6000-READ-AND-FORMAT-SCREEN  THRU  6200-EXIT.
02162
02163      IF NO-RECORDS
02164          MOVE SPACE              TO  PI-PYAJ-FILE-SW
02165          MOVE ER-2239            TO  EMI-ERROR
02166          MOVE -1                 TO  MAINTL
02167          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02168          IF (SENT-FROM-PENDING
02169             AND PI-PYAJ-REFERENCE GREATER THAN SPACES)
02170             MOVE PI-PYAJ-REFERENCE TO  REF1O
02171             MOVE 12                TO  REF1L
02172             MOVE AL-UANON          TO  REF1A
02173             MOVE SPACE             TO  EL630-SENT-SW
02174             MOVE 'Y'               TO  PI-PYAJ-FILLED-SW
02175             GO TO 8100-SEND-INITIAL-MAP
02176          ELSE
02177              GO TO 8100-SEND-INITIAL-MAP.
02178
02179      IF END-OF-FILE
02180          IF EIBAID = DFHPF2
02181              MOVE ER-2238        TO  EMI-ERROR
02182              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02183              MOVE 'T'            TO  PI-PYAJ-FILE-SW
02184          ELSE
02185              MOVE ER-2237        TO  EMI-ERROR
02186              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02187
02188      MOVE 'S'                    TO  PI-PREV-FUNCTION
02189                                      PI-SAV-FUNCTION.
02190
02191      GO TO 8100-SEND-INITIAL-MAP.
02192  EJECT
02193  4100-BROWSE-BKWD.
02194      MOVE SPACE                   TO  PI-PYAJ-FILE-SW
02195                                       PI-TOTAL-DISPLAYED-SW.
02196      MOVE PI-SAV-ENDING-PYAJ-KEY  TO  ERPYAJ-KEY.
02197      MOVE ZEROS                   TO  PYAJ-FILE-SEQ-NO.
02198      MOVE ZEROS                   TO  PYAJ-RECORD-TYPE.
02199
02200      PERFORM 5000-START-BROWSE  THRU  5030-EXIT.
02201
02202      IF NO-RECORDS
02203          MOVE SPACE              TO  PI-PYAJ-FILE-SW
02204          MOVE LOW-VALUES         TO  EL635AO
02205          MOVE ER-2239            TO  EMI-ERROR
02206          MOVE -1                 TO  MAINTL
02207          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02208          GO TO 8100-SEND-INITIAL-MAP.
02209
02210      IF NOT-OPEN
02211          GO TO 7000-PYAJ-FILE-NOTOPEN.
02212
02213      
      * EXEC CICS READNEXT
02214 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
02215 *        DATASET  (PYAJ-FILE-ID)
02216 *        RIDFLD   (ERPYAJ-KEY)
02217 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004687' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02218
02219      PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT.
02220
02221      PERFORM 5100-READ-PREVIOUS  THRU  5120-EXIT.
02222
02223      PERFORM 5200-END-BROWSE  THRU  5200-EXIT.
02224
02225      MOVE ZEROS                  TO  PYAJ-FILE-SEQ-NO.
02226      MOVE SPACES                 TO  PYAJ-RECORD-TYPE.
02227
02228      GO TO 4000-BROWSE-FRWD-FOR-PREV.
02229  EJECT
02230  5000-START-BROWSE.
02231      
      * EXEC CICS HANDLE CONDITION
02232 *        NOTOPEN  (5010-NOT-OPEN)
02233 *        NOTFND   (5020-NO-RECORDS)
02234 *        ENDFILE  (5020-NO-RECORDS)
02235 *    END-EXEC.
      *    MOVE '"$JI''                 ! . #00004705' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303034373035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02236
02237      
      * EXEC CICS STARTBR
02238 *        DATASET  (PYAJ-FILE-ID)
02239 *        RIDFLD   (ERPYAJ-KEY)
02240 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004711' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02241
02242      GO TO 5030-EXIT.
02243
02244  5010-NOT-OPEN.
02245      MOVE 'Z'                    TO  PI-PYAJ-FILE-SW.
02246
02247      GO TO 5030-EXIT.
02248
02249  5020-NO-RECORDS.
02250      MOVE ZEROS                  TO  PI-SEQ-NOS.
02251      MOVE 'Y'                    TO  PI-PYAJ-FILE-SW.
02252
02253  5030-EXIT.
02254      EXIT.
02255  EJECT
02256  5100-READ-PREVIOUS.
02257      
      * EXEC CICS HANDLE CONDITION
02258 *        ENDFILE  (5110-END-OF-FILE)
02259 *        NOTFND   (5110-END-OF-FILE)
02260 *    END-EXEC.
      *    MOVE '"$''I                  ! / #00004731' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303034373331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02261
02262      
      * EXEC CICS READPREV
02263 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
02264 *        DATASET  (PYAJ-FILE-ID)
02265 *        RIDFLD   (ERPYAJ-KEY)
02266 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004736' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02267
02268      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
02269          IF PI-CARRIER-SECURITY = PY-CARRIER
02270              NEXT SENTENCE
02271          ELSE
02272              GO TO 5110-END-OF-FILE.
02273
02274      GO TO 5120-EXIT.
02275
02276  5110-END-OF-FILE.
02277
02278      MOVE ER-2238                TO  EMI-ERROR.
02279      MOVE -1                     TO  PFENTERL.
02280
02281      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02282
02283      GO TO 8200-SEND-DATAONLY.
02284
02285  5120-EXIT.
02286      EXIT.
02287
02288  5200-END-BROWSE.
02289      
      * EXEC CICS ENDBR
02290 *        DATASET  (PYAJ-FILE-ID)
02291 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004763' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02292
02293  5200-EXIT.
02294      EXIT.
02295  EJECT
02296  6000-READ-AND-FORMAT-SCREEN.
02297      
      * EXEC CICS HANDLE CONDITION
02298 *        ENDFILE  (6100-END-OF-FILE)
02299 *        NOTFND   (6100-END-OF-FILE)
02300 *    END-EXEC.
      *    MOVE '"$''I                  ! 0 #00004771' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303034373731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02301
02302      MOVE SPACE                  TO PI-REC-TYPE (1)
02303                                     PI-REC-TYPE (2)
02304                                     PI-REC-TYPE (3).
02305
02306      MOVE ZEROS                  TO PI-FILE-SEQ-NO (1)
02307                                     PI-FILE-SEQ-NO (2)
02308                                     PI-FILE-SEQ-NO (3)
02309                                     PI-AMOUNT (1)
02310                                     PI-AMOUNT (2)
02311                                     PI-AMOUNT (3).
02312
02313      MOVE SPACE                  TO PI-APPLIED (1)
02314                                     PI-REFERENCE (1)
02315                                     PI-INVOICE (1)
02316                                     PI-APPLIED (2)
02317                                     PI-REFERENCE (2)
02318                                     PI-INVOICE (2)
02319                                     PI-APPLIED (3)
02320                                     PI-REFERENCE (3)
02321                                     PI-INVOICE (3).
02322
02323      MOVE SPACE                  TO PI-TOTAL-DISPLAYED-SW.
02324      SET PINDX                   TO ZERO-NDX.
02325
02326      IF (SENT-FROM-PENDING
02327             AND PI-PYAJ-REFERENCE GREATER THAN SPACES)
02328         SET PINDX  UP  BY  1
02329         MOVE 12                  TO  REF-LEN (PINDX)
02330         MOVE PI-PYAJ-REFERENCE   TO  REF (PINDX)
02331         MOVE 'Y'                 TO  PI-PYAJ-FILLED-SW
02332         MOVE SPACE               TO  EL630-SENT-SW.
02333
02334  6010-READ-NEXT.
02335
02336      
      * EXEC CICS READNEXT
02337 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
02338 *        DATASET  (PYAJ-FILE-ID)
02339 *        RIDFLD   (ERPYAJ-KEY)
02340 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004810' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02341
02342      IF PI-COMPANY-CD NOT = PY-COMPANY-CD
02343        AND PI-SAV-ENDING-PYAJ-KEY NOT = SPACES
02344          GO TO 6100-END-OF-FILE.
02345
02346      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
02347          IF PY-CARRIER NOT = PI-CARRIER-SECURITY
02348              IF PI-SAV-ENDING-PYAJ-KEY NOT = SPACES
02349                  GO TO 6100-END-OF-FILE.
02350
02351      IF PYAJ-1ST-READ
02352        AND EIBAID NOT = DFHENTER
02353        AND NOT PAGE-FULL
02354          MOVE PY-CONTROL-PRIMARY  TO  PI-SAV-ENDING-PYAJ-KEY
02355          MOVE PY-CARRIER          TO  PI-CR-CARRIER
02356          MOVE PY-GROUPING         TO  PI-CR-GROUPING
02357          MOVE PY-FIN-RESP         TO  PI-CR-FIN-RESP
02358          MOVE PY-ACCOUNT          TO  PI-CR-ACCOUNT.
02359
02360      MOVE SPACE                  TO  PI-PYAJ-FILE-SW.
02361
02362      IF PI-COMPANY-CD   = PY-COMPANY-CD  AND
02363         PI-SAV-CARRIER  = PY-CARRIER     AND
02364         PI-SAV-GROUPING = PY-GROUPING    AND
02365         PI-SAV-FIN-RESP = PY-FIN-RESP    AND
02366         PI-SAV-ACCOUNT  = PY-ACCOUNT
02367          NEXT SENTENCE
02368      ELSE
02369          IF PYAJ-1ST-READ
02370             AND NOT PAGE-FULL
02371              MOVE 'Y'            TO  PI-PYAJ-FILE-SW
02372              MOVE SPACE          TO  PYAJ-READ-SW
02373              GO TO 6200-EXIT
02374          ELSE
02375              MOVE 'A'            TO  PI-PYAJ-FILE-SW
02376              IF PINDX  IS LESS THAN  +3
02377                  SET PINDX  UP  BY  1
02378                  MOVE 'TOTAL'         TO  COMM (PINDX)
02379                  MOVE TOTAL-ACCT-AMT  TO  AMTO (PINDX)
02380                  MOVE 'NET TOTAL'     TO  REF  (PINDX)
02381                  MOVE TOTAL-ACCT-NET  TO  NETO (PINDX)
02382                  GO TO 6200-EXIT
02383              ELSE
02384                  MOVE TOTAL-ACCT-AMT  TO  PI-SAV-ACCT-AMT
02385                  MOVE TOTAL-ACCT-NET  TO  PI-SAV-ACCT-NET
02386                  MOVE 'F'             TO  PI-PYAJ-FILE-SW
02387                  GO TO 6200-EXIT.
02388
02389      IF PY-CREDIT-ACCEPT-DT NOT = LOW-VALUES
02390          GO TO 6000-READ-AND-FORMAT-SCREEN.
02391
02392      SET PINDX  UP  BY +1.
02393
02394      SET WS-SAVE-INDEX-VALUE     TO  PINDX.
02395
02396      MOVE TOTAL-ACCT-AMT      TO PI-SAV-ACCT-AMT.
02397      MOVE TOTAL-ACCT-NET      TO PI-SAV-ACCT-NET.
02398
02399      IF PINDX  IS GREATER THAN  +3
02400         MOVE 'F'                 TO  PI-PYAJ-FILE-SW
02401         GO TO 6200-EXIT.
02402
02403      IF PYAJ-1ST-READ
02404          MOVE SPACE              TO  PYAJ-READ-SW.
02405
02406      SET NDX                     TO  PINDX.
02407      SET WS-SAVE-NDX-VALUE       TO  NDX.
02408
02409      MOVE PY-FILE-SEQ-NO         TO  PI-FILE-SEQ-NO (NDX).
02410      MOVE PY-ENTRY-COMMENT       TO  COMM (PINDX).
02411      MOVE PY-ENTRY-AMT           TO  AMTO (PINDX).
02412
02413      ADD PY-ENTRY-AMT            TO  TOTAL-ACCT-AMT.
02414
02415      IF PY-RECORD-TYPE = 'R' OR 'D' OR 'S' OR 'T' OR 'Z'
02416          ADD PY-ENTRY-AMT        TO  TOTAL-ACCT-NET
02417      ELSE
02418          SUBTRACT PY-ENTRY-AMT  FROM  TOTAL-ACCT-NET.
02419
02420      MOVE PY-RECORD-TYPE         TO  RTYPE (PINDX)
02421                                      PI-REC-TYPE (NDX).
02422
02423      MOVE PY-PMT-APPLIED         TO  PI-APPLIED (NDX).
02424      MOVE PY-REF-NO              TO  PI-REFERENCE (NDX).
02425      MOVE PY-BIL-INV             TO  PI-INVOICE (NDX).
02426      MOVE PY-ENTRY-AMT           TO  PI-AMOUNT (NDX).
02427
02428      IF PY-RECORD-TYPE = 'G'
02429         MOVE 'G'                 TO  PI-SPECIAL-GROUPING-SW
02430      ELSE
02431         MOVE SPACE               TO  PI-SPECIAL-GROUPING-SW.
02432
02433      IF PY-PMT-APPLIED NOT = SPACE
02434          MOVE PY-PMT-APPLIED     TO  APPLIED (PINDX).
02435
02436      IF PY-VOID-SW NOT = SPACE
02437          MOVE PY-VOID-SW         TO  VOID-SW (PINDX).
02438
02439      IF PY-LAST-MAINT-DT = PREV-BIN-MAINT-DT
02440          MOVE PREV-MAINT-DT        TO  MDTE (PINDX)
02441      ELSE
02442          MOVE PY-LAST-MAINT-DT     TO  DC-BIN-DATE-1
02443                                        PREV-BIN-MAINT-DT
02444          MOVE SPACE                TO  DC-OPTION-CODE
02445          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02446          MOVE DC-GREG-DATE-1-MDY   TO  MDTE (PINDX)
02447                                        PREV-MAINT-DT.
02448
02449      IF PY-AR-DATE NOT = LOW-VALUES
02450          MOVE AL-SANOF           TO  COMM-ATTRB    (PINDX)
02451                                      RTYPE-ATTRB   (PINDX)
02452                                      AMT-ATTRB     (PINDX)
02453                                      VOID-SW-ATTRB (PINDX)
02454                                      APPLIED-ATTRB (PINDX)
02455                                      REF-ATTRB     (PINDX)
02456                                      INVOICE-ATTRB (PINDX)
02457                                      IDTE-ATTRB    (PINDX)
02458                                      CREDIT-ATTRB  (PINDX)
02459                                      DEBIT-ATTRB   (PINDX)
02460          IF PY-AR-DATE = PREV-BIN-BL-DT
02461              MOVE PREV-BL-DT           TO  BDTE (PINDX)
02462          ELSE
02463              MOVE PY-AR-DATE           TO  DC-BIN-DATE-1
02464                                            PREV-BIN-BL-DT
02465              MOVE SPACE                TO  DC-OPTION-CODE
02466              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02467              MOVE DC-GREG-DATE-1-MDY   TO  BDTE (PINDX)
02468                                            PREV-BL-DT.
02469
02470      IF PY-AR-DATE NOT = LOW-VALUES
02471         IF MORTGAGE-SESSION
02472             MOVE AL-SANON        TO  INVOICE-ATTRB (PINDX).
02473
02474      IF (PY-CHECK-ORIGIN-SW = SPACES OR LOW-VALUES) AND
02475         (PY-GL-DATE = SPACES OR LOW-VALUES)
02476          NEXT SENTENCE
02477      ELSE
02478          MOVE AL-SANOF           TO  RTYPE-ATTRB   (PINDX)
02479                                      AMT-ATTRB     (PINDX)
02480                                      VOID-SW-ATTRB (PINDX)
02481                                      APPLIED-ATTRB (PINDX)
02482                                      SDTE-ATTRB    (PINDX)
02483                                      IDTE-ATTRB    (PINDX)
02484                                      REF-ATTRB     (PINDX)
02485                                      INVOICE-ATTRB (PINDX)
02486                                      CREDIT-ATTRB  (PINDX)
02487                                      DEBIT-ATTRB   (PINDX).
02488
02489      IF PI-PROCESSOR-ID = 'LGXX'
02490          MOVE AL-UANOF           TO  COMM-ATTRB    (PINDX)
02491                                      RTYPE-ATTRB   (PINDX)
02492                                      AMT-ATTRB     (PINDX)
02493                                      VOID-SW-ATTRB (PINDX)
02494                                      APPLIED-ATTRB (PINDX)
02495                                      SDTE-ATTRB    (PINDX)
02496                                      REF-ATTRB     (PINDX)
02497                                      INVOICE-ATTRB (PINDX)
02498                                      IDTE-ATTRB    (PINDX)
02499                                      CREDIT-ATTRB  (PINDX)
02500                                      DEBIT-ATTRB   (PINDX).
02501
02502      IF PY-CREDIT-SELECT-DT NOT = LOW-VALUES
02503         MOVE PY-CREDIT-SELECT-DT  TO  DC-BIN-DATE-1
02504         MOVE SPACE                TO  DC-OPTION-CODE
02505         PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02506         MOVE DC-GREG-DATE-1-MDY   TO  SDTE (PINDX).
02507
02508      IF PY-INPUT-DT  NOT =  LOW-VALUES
02509          MOVE PY-INPUT-DT        TO  DC-BIN-DATE-1
02510          MOVE SPACE              TO  DC-OPTION-CODE
02511          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02512          MOVE DC-GREG-DATE-1-MDY
02513                                  TO  IDTE (PINDX).
02514
02515      MOVE PY-REF-NO              TO  REF     (PINDX).
02516      MOVE PY-BIL-INV             TO  INVOICE (PINDX).
02517      MOVE PY-GL-CR               TO  CREDIT  (PINDX).
02518      MOVE PY-GL-DB               TO  DEBIT   (PINDX).
02519
02520      GO TO 6010-READ-NEXT.
02521
02522  6100-END-OF-FILE.
02523      MOVE 'X'                    TO  PI-PYAJ-FILE-SW.
02524
02525      IF PINDX  IS LESS THAN  +3
02526          SET PINDX  UP  BY  1
02527          MOVE 'TOTAL'            TO  COMM (PINDX)
02528          MOVE TOTAL-ACCT-AMT     TO  AMTO (PINDX)
02529          MOVE 'NET TOTAL'        TO  REF  (PINDX)
02530          MOVE TOTAL-ACCT-NET     TO  NETO (PINDX)
02531          MOVE 'Y'                TO  PI-TOTAL-DISPLAYED-SW
02532      ELSE
02533          MOVE TOTAL-ACCT-AMT     TO  PI-SAV-ACCT-AMT
02534          MOVE TOTAL-ACCT-NET     TO  PI-SAV-ACCT-NET.
02535
02536  6200-EXIT.
02537      EXIT.
02538  EJECT
02539
02540
02541  6500-VERIFY-RECON-HEADER.
02542
02543      IF PY-REMIT-RECEIVED
02544         GO TO 6590-EXIT.
02545
02546      MOVE PI-COMPANY-CD           TO MPPRCN-COMPANY-CD.
02547      MOVE INVOICE (PINDX)         TO MPPRCN-INVOICE.
02548      MOVE +999999999              TO MPPRCN-RECORD-SEQU.
02549
02550      
      * EXEC CICS HANDLE CONDITION
02551 *        NOTFND   (6580-HEADER-NOTFND)
02552 *        END-EXEC.
      *    MOVE '"$I                   ! 1 #00005024' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303035303234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02553
02554      
      * EXEC CICS READ
02555 *        DATASET   (MPPRCN-FILE-ID)
02556 *        SET       (ADDRESS OF PAYMENT-RECONCILIATION)
02557 *        RIDFLD    (MPPRCN-KEY)
02558 *        END-EXEC.
      *    MOVE '&"S        E          (   #00005028' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MPPRCN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MPPRCN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PAYMENT-RECONCILIATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02559
02560      IF  NOT PI-NO-CARRIER-SECURITY
02561              AND
02562          PI-CARRIER-SECURITY NOT EQUAL PR-CARRIER-A2
02563          MOVE -1                 TO CARRIERL
02564          MOVE AL-UANON           TO CARRIERA
02565          MOVE ER-9095            TO EMI-ERROR
02566          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02567          GO TO 8200-SEND-DATAONLY.
02568
02569      MOVE PR-SECURITY-ACCESS-CODE
02570                                  TO SC-SECURITY-ACCESS-CODE.
02571      PERFORM 9920-PRODUCER-EVALUATION THRU 9920-EXIT.
02572
02573      IF  SC-PRODUCER-NOT-AUTHORIZED
02574          MOVE -1                 TO ACCTL
02575          MOVE AL-UANON           TO ACCTA
02576          MOVE ER-9094            TO EMI-ERROR
02577          GO TO 8200-SEND-DATAONLY.
02578
02579      IF PR-HDR-POSTED
02580         GO TO 6560-POST-ERROR.
02581
02582      GO TO 6590-EXIT.
02583
02584  6560-POST-ERROR.
02585
02586      MOVE -1                     TO INVOICE-LEN (PINDX)
02587      MOVE ER-9296                TO EMI-ERROR.
02588      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
02589
02590      GO TO 6590-EXIT.
02591
02592  6580-HEADER-NOTFND.
02593
02594      MOVE -1                     TO INVOICE-LEN (PINDX)
02595      MOVE ER-9280                TO EMI-ERROR.
02596      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
02597
02598  6590-EXIT.
02599      EXIT.
02600
02601      EJECT
02602
02603  6600-UPDATE-RECON-HEADER.
02604
02605      IF (PY-REMIT-RECEIVED OR PY-REMIT-IND-GROUPING)
02606         NEXT SENTENCE
02607      ELSE
02608         GO TO 6690-EXIT.
02609
02610      MOVE PI-COMPANY-CD           TO MPPRCN-COMPANY-CD.
02611      MOVE PY-BIL-INV              TO MPPRCN-INVOICE.
02612      MOVE +999999999              TO MPPRCN-RECORD-SEQU.
02613
02614      
      * EXEC CICS HANDLE CONDITION
02615 *        NOTFND   (6680-HEADER-NOTFND)
02616 *        END-EXEC.
      *    MOVE '"$I                   ! 2 #00005088' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303035303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02617
02618      
      * EXEC CICS READ
02619 *        DATASET   (MPPRCN-FILE-ID)
02620 *        SET       (ADDRESS OF PAYMENT-RECONCILIATION)
02621 *        RIDFLD    (MPPRCN-KEY)
02622 *        UPDATE
02623 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00005092' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MPPRCN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MPPRCN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PAYMENT-RECONCILIATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02624
02625      IF  NOT PI-NO-CARRIER-SECURITY
02626              AND
02627          PI-CARRIER-SECURITY NOT EQUAL PR-CARRIER-A2
02628          MOVE -1                 TO CARRIERL
02629          MOVE AL-UANON           TO CARRIERA
02630          MOVE ER-9095            TO EMI-ERROR
02631          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02632          GO TO 8200-SEND-DATAONLY.
02633
02634      MOVE PR-SECURITY-ACCESS-CODE
02635                                  TO SC-SECURITY-ACCESS-CODE.
02636      PERFORM 9920-PRODUCER-EVALUATION THRU 9920-EXIT.
02637
02638      IF  SC-PRODUCER-NOT-AUTHORIZED
02639          MOVE -1                 TO ACCTL
02640          MOVE AL-UANON           TO ACCTA
02641          MOVE ER-9094            TO EMI-ERROR
02642          GO TO 8200-SEND-DATAONLY.
02643
02644      IF PR-HDR-POSTED
02645         GO TO 6660-POST-ERROR.
02646
02647      IF WS-RECON-DELETE
02648         SUBTRACT WS-ENTRY-AMT  FROM PR-RECEIVED-PREMIUM.
02649
02650      IF WS-RECON-UPDATE
02651         SUBTRACT WS-OLD-ENTRY-AMT FROM PR-RECEIVED-PREMIUM
02652         ADD      WS-ENTRY-AMT     TO   PR-RECEIVED-PREMIUM.
02653
02654      IF WS-RECON-ADD
02655         ADD  WS-ENTRY-AMT        TO PR-RECEIVED-PREMIUM.
02656
02657      MOVE WS-CURRENT-BIN-DT      TO PR-RECEIVED-DT.
02658
02659      MOVE PI-PROCESSOR-ID       TO PR-LAST-CHANGE-PROCESSOR.
02660      MOVE EIBTIME               TO PR-LAST-CHANGE-TIME.
02661      MOVE WS-CURRENT-BIN-DT     TO PR-LAST-CHANGE-DT.
02662
02663      
      * EXEC CICS REWRITE
02664 *        DATASET   (MPPRCN-FILE-ID)
02665 *        FROM      (PAYMENT-RECONCILIATION)
02666 *        END-EXEC.
           MOVE LENGTH OF
            PAYMENT-RECONCILIATION
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005137' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MPPRCN-FILE-ID, 
                 PAYMENT-RECONCILIATION, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02667
02668      GO TO 6690-EXIT.
02669
02670  6660-POST-ERROR.
02671
02672      
      * EXEC CICS SYNCPOINT ROLLBACK
02673 *    END-EXEC.
      *    MOVE '6"R                   !   #00005146' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02674
02675      MOVE -1                     TO INVOICE-LEN (PINDX)
02676      MOVE ER-9296                TO EMI-ERROR.
02677      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
02678
02679      GO TO 8200-SEND-DATAONLY.
02680
02681  6680-HEADER-NOTFND.
02682
02683      
      * EXEC CICS SYNCPOINT ROLLBACK
02684 *    END-EXEC.
      *    MOVE '6"R                   !   #00005157' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02685
02686      MOVE -1                     TO INVOICE-LEN (PINDX).
02687      MOVE ER-9280                TO EMI-ERROR.
02688      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
02689
02690      GO TO 8200-SEND-DATAONLY.
02691
02692  6690-EXIT.
02693      EXIT.
02694
02695      EJECT
02696
02697  7000-PYAJ-FILE-NOTOPEN.
02698      MOVE -1                     TO  MAINTL.
02699
02700      IF  MORTGAGE-SESSION
02701          MOVE ER-9374            TO  EMI-ERROR
02702
02703      ELSE
02704          MOVE ER-2232            TO  EMI-ERROR.
02705
02706      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02707
02708      GO TO 8200-SEND-DATAONLY.
02709
02710  7100-COMP-FILE-NOTOPEN.
02711      MOVE -1                     TO  MAINTL.
02712      MOVE ER-2233                TO  EMI-ERROR.
02713
02714      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02715
02716      GO TO 8200-SEND-DATAONLY.
02717
02718  7200-RECV-FILE-NOTOPEN.
02719      MOVE -1                     TO  MAINTL.
02720      MOVE ER-3177                TO  EMI-ERROR.
02721
02722      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02723
02724      GO TO 8200-SEND-DATAONLY.
02725  EJECT
02726  8100-SEND-INITIAL-MAP.
02727      MOVE WS-CURRENT-DT          TO  DATEO.
02728      MOVE EIBTIME                TO  TIME-IN.
02729      MOVE TIME-OUT               TO  TIMEO.
02730      MOVE -1                     TO  MAINTL.
02731      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
02732      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
02733
02734      IF MORTGAGE-SESSION
02735         MOVE ' PRODUCER:'        TO ACCTHDGO
02736         MOVE AL-SABOF            TO ACCTHDGA.
02737
02738      IF NOT CREDIT-SESSION
02739         MOVE AL-SADOF            TO PFGABILA, PFACBILA.
02740
02741      IF EIBTRNID = TRANS-ID
02742        OR EL640-TRANS-ID
02743        OR EL642-TRANS-ID
02744        OR EL652-TRANS-ID
02745        OR EL6351-TRANS-ID
02746        OR EL630-TRANS-ID
02747          IF PI-SAV-COMP-CONTROL GREATER THAN SPACES
02748              MOVE PI-SAV-FUNCTION  TO  MAINTI
02749              MOVE PI-SAV-CARRIER   TO  CARRIERO
02750              MOVE PI-SAV-GROUPING  TO  GROUPO
02751              MOVE PI-SAV-FIN-RESP  TO  FINRESPO
02752              MOVE PI-SAV-ACCOUNT   TO  ACCTO
02753              MOVE AL-UANON         TO  MAINTA
02754                                        CARRIERA
02755                                        GROUPA
02756                                        FINRESPA
02757                                        ACCTA
02758          ELSE
02759              NEXT SENTENCE.
02760
02761      MOVE WS-EMHD1               TO  EMHD1O
02762      MOVE AL-SABOF               TO  EMHD1A
02763      MOVE WS-EMHD2               TO  EMHD2O
02764      MOVE AL-SABOF               TO  EMHD2A.
02765
02766      
      * EXEC CICS SEND
02767 *        MAP     (MAP-NAME)
02768 *        MAPSET  (MAPSET-NAME)
02769 *        FROM    (EL635AO)
02770 *        ERASE
02771 *        CURSOR
02772 *        END-EXEC.
           MOVE LENGTH OF
            EL635AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005240' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL635AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02773
02774      GO TO 9100-RETURN-TRAN.
02775  EJECT
02776  8200-SEND-DATAONLY.
02777      MOVE WS-CURRENT-DT          TO  DATEO.
02778      MOVE EIBTIME                TO  TIME-IN.
02779      MOVE TIME-OUT               TO  TIMEO.
02780      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
02781      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
02782
02783      IF NOT CREDIT-SESSION
02784         MOVE AL-SADOF            TO PFGABILA, PFACBILA.
02785
02786      
      * EXEC CICS SEND
02787 *        MAP     (MAP-NAME)
02788 *        MAPSET  (MAPSET-NAME)
02789 *        FROM    (EL635AO)
02790 *        DATAONLY
02791 *        CURSOR
02792 *    END-EXEC.
           MOVE LENGTH OF
            EL635AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005260' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL635AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02793
02794      GO TO 9100-RETURN-TRAN.
02795
02796  8300-SEND-TEXT.
02797      
      * EXEC CICS SEND TEXT
02798 *        FROM    (LOGOFF-TEXT)
02799 *        LENGTH  (LOGOFF-LENGTH)
02800 *        ERASE
02801 *        FREEKB
02802 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005271' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02803
02804      
      * EXEC CICS RETURN
02805 *    END-EXEC.
      *    MOVE '.(                    &   #00005278' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02806  EJECT
02807  8400-LOG-JOURNAL-RECORD.
02808 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
02809 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.
02810
02811 *    EXEC CICS JOURNAL
02812 *        JFILEID  (PI-JOURNAL-FILE-ID)
02813 *        JTYPEID  ('EL')
02814 *        FROM     (JOURNAL-RECORD)
02815 *        LENGTH   (223)
02816 *        END-EXEC.
02817
02818  8400-EXIT.
02819      EXIT.
02820
02821  8500-DATE-CONVERT.
02822      
      * EXEC CICS LINK
02823 *        PROGRAM   (LINK-CLDATCV)
02824 *        COMMAREA  (DATE-CONVERSION-DATA)
02825 *        LENGTH    (DC-COMM-LENGTH)
02826 *    END-EXEC.
      *    MOVE '."C                   ''   #00005296' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-CLDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02827
02828  8500-EXIT.
02829      EXIT.
02830
02831  EJECT
02832  8800-UNAUTHORIZED-ACCESS.
02833      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
02834
02835      GO TO 8300-SEND-TEXT.
02836
02837  8810-PF23.
02838      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
02839      MOVE XCTL-005               TO  PGM-NAME.
02840
02841      GO TO 9300-XCTL.
02842
02843  9000-RETURN-CICS.
02844      
      * EXEC CICS RETURN
02845 *        END-EXEC.
      *    MOVE '.(                    &   #00005318' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02846
02847  9100-RETURN-TRAN.
02848      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
02849      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
02850
02851      
      * EXEC CICS RETURN
02852 *        TRANSID   (TRANS-ID)
02853 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02854 *        LENGTH    (PI-COMM-LENGTH)
02855 *    END-EXEC.
      *    MOVE '.(CT                  &   #00005325' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02856
02857  9200-RETURN-MAIN-MENU.
02858      MOVE XCTL-626               TO  PGM-NAME.
02859
02860      GO TO 9300-XCTL.
02861
02862  9300-XCTL.
02863      
      * EXEC CICS XCTL
02864 *        PROGRAM   (PGM-NAME)
02865 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02866 *        LENGTH    (PI-COMM-LENGTH)
02867 *    END-EXEC.
      *    MOVE '.$C                   $   #00005337' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02868
02869  9400-CLEAR.
02870      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
02871
02872      IF PI-RETURN-TO-PROGRAM = 'EL626'
02873          MOVE SPACES             TO  PI-CR-CONTROL-IN-PROGRESS
02874                                      PI-SAV-COMP-CONTROL.
02875
02876      GO TO 9300-XCTL.
02877
02878  9500-PF12.
02879      MOVE XCTL-010               TO  PGM-NAME.
02880
02881      GO TO 9300-XCTL.
02882
02883  9600-PGMID-ERROR.
02884      
      * EXEC CICS HANDLE CONDITION
02885 *        PGMIDERR  (8300-SEND-TEXT)
02886 *    END-EXEC.
      *    MOVE '"$L                   ! 3 #00005358' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303035333538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02887
02888      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
02889      MOVE ' '                    TO  PI-ENTRY-CD-1.
02890      MOVE XCTL-005               TO  PGM-NAME.
02891      MOVE PGM-NAME               TO  LOGOFF-PGM.
02892      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
02893
02894      GO TO 9300-XCTL.
02895
02896  9900-ERROR-FORMAT.
02897      IF NOT EMI-ERRORS-COMPLETE
02898          MOVE LINK-001           TO  PGM-NAME
02899          
      * EXEC CICS LINK
02900 *            PROGRAM   (PGM-NAME)
02901 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
02902 *            LENGTH    (EMI-COMM-LENGTH)
02903 *        END-EXEC.
      *    MOVE '."C                   ''   #00005373' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02904
02905  9900-EXIT.
02906      EXIT.
02907      EJECT
02908  9910-INITIALIZE-SECURITY.
02909 ******************************************************************
02910 *                                                                *
02911 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
02912 *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *
02913 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
02914 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
02915 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
02916 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
02917 *                                                                *
02918 ******************************************************************
02919
02920      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'
02921          IF  MORTGAGE-SESSION
02922              MOVE '125E'             TO SC-QUID-SYSTEM
02923              MOVE EIBTRMID           TO SC-QUID-TERMINAL
02924
02925              
      * EXEC CICS READQ TS
02926 *                QUEUE  (SC-QUID-KEY)
02927 *                INTO   (SECURITY-CONTROL-E)
02928 *                LENGTH (SC-COMM-LENGTH-E)
02929 *                ITEM   (SC-ITEM)
02930 *            END-EXEC
      *    MOVE '*$II   L              ''   #00005399' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SC-QUID-KEY, 
                 SECURITY-CONTROL-E, 
                 SC-COMM-LENGTH-E, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02931
02932              MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)
02933                                      TO PI-DISPLAY-CAP
02934              MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)
02935                                      TO PI-MODIFY-CAP
02936              IF  NOT DISPLAY-CAP
02937                  MOVE 'READ'         TO SM-READ
02938                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02939                  MOVE ER-9097        TO EMI-ERROR
02940                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02941                  GO TO 8100-SEND-INITIAL-MAP
02942              ELSE
02943                  GO TO 9910-EXIT
02944          ELSE
02945              
      * EXEC CICS  READQ TS
02946 *                QUEUE   (PI-SECURITY-TEMP-STORE-ID)
02947 *                INTO    (SECURITY-CONTROL)
02948 *                LENGTH  (SC-COMM-LENGTH)
02949 *                ITEM    (SC-ITEM)
02950 *                END-EXEC
      *    MOVE '*$II   L              ''   #00005419' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02951
02952              MOVE SC-CREDIT-DISPLAY (15)
02953                                  TO PI-DISPLAY-CAP
02954              MOVE SC-CREDIT-UPDATE  (15)
02955                                  TO PI-MODIFY-CAP
02956
02957              IF  NOT DISPLAY-CAP
02958                  MOVE 'READ'     TO SM-READ
02959                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02960                  MOVE ER-0070    TO  EMI-ERROR
02961                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02962                  GO TO 8100-SEND-INITIAL-MAP.
02963
02964  9910-EXIT.
02965      EXIT.
02966      EJECT
02967  9920-PRODUCER-EVALUATION.
02968 ******************************************************************
02969 *                                                                *
02970 *       THIS AREA CONTAINS THE LOGIC NECESSARY TO CHECK THE      *
02971 *       AUTHORIZATION OF THE USER TO 'ACCESS' A PRODUCER.        *
02972 *       IF THE USER IS NOT AUTHORIZED AN APPROPRIATE MESSAGE     *
02973 *       IS GENERATED.                                            *
02974 *                                                                *
02975 ******************************************************************
02976
02977      MOVE SPACES                 TO SC-PRODUCER-AUTHORIZED-SW.
02978
02979      IF  PI-NO-PRODUCER-SECURITY
02980              OR
02981          PI-PROCESSOR-ID EQUAL 'LGXX'
02982          GO TO 9920-EXIT.
02983
02984      SET PI-ACCESS-NDX           TO +1
02985
02986      SEARCH PI-ACCESS-CODE
02987          VARYING PI-ACCESS-NDX
02988
02989          AT END
02990              MOVE 'N'            TO SC-PRODUCER-AUTHORIZED-SW
02991
02992          WHEN
02993              PI-ACCESS-CODE (PI-ACCESS-NDX)
02994                  EQUAL SC-SECURITY-ACCESS-CODE
02995              GO TO 9920-EXIT.
02996
02997  9920-EXIT.
02998      EXIT.
02999      EJECT
03000
03001  9990-ABEND.
03002      MOVE LINK-004               TO  PGM-NAME.
03003      MOVE DFHEIBLK               TO  EMI-LINE1.
03004
03005      
      * EXEC CICS LINK
03006 *        PROGRAM   (PGM-NAME)
03007 *        COMMAREA  (EMI-LINE1)
03008 *        LENGTH    (72)
03009 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005479' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03010
03011      MOVE -1                     TO  PFENTERL.
03012
03013      GO TO 8200-SEND-DATAONLY.
03014
03015      MOVE ZEROS  TO RETURN-CODE.
03015      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL635' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
03016
03017  9995-SECURITY-VIOLATION.
03018 *                            COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005510' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
03019
03020  9995-EXIT.
03021      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL635' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0410-NO-A-COMP,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0410-NO-G-FOR-A,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 0410-NO-G-COMP,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0890-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1011-NO-COMP-A,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1011-NO-COMP-G,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 1040-RECV-NOTFND,
                     7200-RECV-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 2110-ADD-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 2115-DUP-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 2120-DELETE-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 2190-DUP-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 5010-NOT-OPEN,
                     5020-NO-RECORDS,
                     5020-NO-RECORDS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 5110-END-OF-FILE,
                     5110-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 6100-END-OF-FILE,
                     6100-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 6580-HEADER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 6680-HEADER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL635' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
