00001  IDENTIFICATION DIVISION.                                         10/18/96
00002                                                                   EL636
00003  PROGRAM-ID.                 EL636 .                                 LV026
00004 *              PROGRAM CONVERTED BY                                  CL*24
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*24
00006 *              CONVERSION DATE 12/13/94 13:34:15.                    CL*24
00007 *                            VMOD=2.026                              CL*26
00008 *                                                                 EL636
00008 *                                                                 EL636
00009 *AUTHOR.     LOGIC,INC.                                              CL*24
00010 *            DALLAS, TEXAS.                                          CL*24
00011                                                                   EL636
00012 *DATE-COMPILED.                                                      CL*24
00013 *SECURITY.   *****************************************************   CL*24
00014 *            *                                                   *   CL*24
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*24
00016 *            *                                                   *   CL*24
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*24
00018 *                                                                *   CL*24
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*24
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*24
00021 *            *                                                   *   CL*24
00022 *            *****************************************************   CL*24
00023                                                                   EL636
00024 *REMARKS. TRANSACTION - EXJA - COMMISSION CHECK WORK FILE            CL*24
00025 *                              MAINTENANCE.                          CL*24
00026                                                                   EL636
00027  ENVIRONMENT DIVISION.                                            EL636
00028                                                                   EL636
00029      EJECT                                                        EL636
00030  DATA DIVISION.                                                   EL636
00031  WORKING-STORAGE SECTION.                                         EL636
00032                                                                   EL636
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL636
00034  77  FILLER  PIC X(32)  VALUE '*    EL636 WORKING STORAGE     *'. EL636
00035  77  FILLER  PIC X(32)  VALUE '************ V/M 2.026 *********'.    CL*26
00036                                                                   EL636
00037     EJECT                                                         EL636
00038                                                                   EL636
00039                              COPY ELCSCTM.                           CL*13
00040                              COPY ELCSCRTY.                          CL*13
00041                                                                   EL636
00042     EJECT                                                         EL636
00043                                                                   EL636
00044 ******************************************************************EL636
00045 *                                                                *EL636
00046 *              S T A N D A R D   A R E A S                       *EL636
00047 *                                                                *EL636
00048 ******************************************************************EL636
00049                                                                   EL636
00050  01  STANDARD-AREAS.                                              EL636
00051      12  SC-ITEM                     PIC S9(4)   VALUE +1   COMP. EL636
00052      12  GETMAIN-SPACE               PIC X       VALUE SPACE.     EL636
00053      12  EL636A                      PIC X(8)    VALUE 'EL636A'.  EL636
00054      12  MAPSET-EL636S               PIC X(8)    VALUE 'EL636S'.  EL636
00055      12  TRANS-EXJA                  PIC X(4)    VALUE 'EXJA'.    EL636
00056      12  THIS-PGM                    PIC X(8)    VALUE 'EL636 '.  EL636
00057      12  PGM-NAME                    PIC X(8).                    EL636
00058      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.       CL*11
00059      12  TIME-IN                     PIC S9(7).                   EL636
00060      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL636
00061          16  FILLER                  PIC X.                       EL636
00062          16  TIME-OUT                PIC 99V99.                   EL636
00063          16  FILLER                  PIC X(2).                    EL636
00064      12  LINK-EL001                  PIC X(8)    VALUE 'EL001'.   EL636
00065      12  LINK-EL004                  PIC X(8)    VALUE 'EL004'.   EL636
00066      12  XCTL-EL005                  PIC X(8)    VALUE 'EL005'.   EL636
00067      12  XCTL-EL6361                 PIC X(8)    VALUE 'EL6361'.     CL*11
00068      12  XCTL-EL010                  PIC X(8)    VALUE 'EL010'.   EL636
00069      12  XCTL-EL626                  PIC X(8)    VALUE 'EL626'.   EL636
00070      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL636
00071      12  FILE-ID-ERCMCK              PIC X(8)    VALUE 'ERCMCK'.  EL636
00072      12  FILE-ID-ERCKWK              PIC X(8)    VALUE 'ERCKWK '. EL636
00073      12  FILE-ID-ERCOMP              PIC X(8)    VALUE 'ERCOMP '. EL636
00074      12  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL '.    CL*13
00075      12  WS-CURRENT-DT               PIC X(8)    VALUE SPACES.    EL636
00076      12  WS-CURRENT-BIN-DT           PIC XX      VALUE SPACES.    EL636
00077      12  QID.                                                     EL636
00078          16  QID-TERM                PIC X(4)    VALUE SPACES.    EL636
00079          16  FILLER                  PIC X(4)    VALUE '125D'.    EL636
00080      12  WS-TIME                     PIC 9(6)    VALUE ZEROS.     EL636
00081      12  WS-HR-MINS-SECS REDEFINES WS-TIME.                       EL636
00082          16  WS-HR-MINS              PIC 99V99.                   EL636
00083          16  FILLER                  PIC XX.                      EL636
00084      12  CLIENT-HER                  PIC X(3)    VALUE 'HER'.        CL*11
00085      12  CLIENT-LGX                  PIC X(3)    VALUE 'LGX'.        CL*11
00086      12  ERCOMP-SW                   PIC X       VALUE ' '.          CL*26
00087                                                                   EL636
00088      EJECT                                                        EL636
00089                                                                   EL636
00090  01  WORK-AREA.                                                   EL636
00091      12  DEEDIT-FIELD                PIC X(12).                   EL636
00092      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(10)V99.  EL636
00093      12  WS-SEQUENCE-NO              PIC S9(4)    VALUE +0 COMP.  EL636
00094      12  WS-DETAMT OCCURS 5 TIMES    PIC S9(7)V99.                EL636
00095      12  WS-DETAIL-AMOUNT            PIC S9(7)V99 VALUE +0 COMP-3.EL636
00096      12  WS-SPACE-AGENT-SW           PIC X        VALUE SPACE.       CL*17
00097      12  WS-UPDATE-SW                PIC X        VALUE SPACE.    EL636
00098          88  WS-ENTRIES-UPDATED                   VALUE 'Y'.      EL636
00099      12  WS-SUB1                     PIC S9(4)    VALUE +0 COMP.  EL636
00100      12  WS-SUB2                     PIC S9(4)    VALUE +0 COMP.  EL636
00101      12  WS-BROWSE-SW                PIC X        VALUE SPACE.    EL636
00102          88  WS-BROWSE-STARTED                    VALUE 'Y'.      EL636
00103      12  WS-FIRST-DETAIL-SW          PIC X        VALUE SPACE.       CL**8
00104          88  FIRST-DETAIL-READ                    VALUE 'Y'.         CL**8
00105      12  WS-HEADER-INFO.                                          EL636
00106          16  WS-PAYEE-NAME           PIC X(30)    VALUE SPACES.   EL636
00107          16  WS-PAYEE-ADDRESS-1      PIC X(30)    VALUE SPACES.   EL636
00108          16  WS-PAYEE-ADDRESS-2      PIC X(30)    VALUE SPACES.   EL636
00109          16  WS-PAYEE-CITY-ST        PIC X(30)    VALUE SPACES.   EL636
00110          16  WS-PAYEE-ZIP-CODE.                                   EL636
00111              20  WS-PAYEE-ZIP        PIC X(5)     VALUE SPACES.   EL636
00112              20  WS-PAYEE-ZIP-EXT    PIC X(4)     VALUE SPACES.   EL636
00113          16  WS-AMOUNT-PAID          PIC S9(7)V99 VALUE +0 COMP-3.EL636
00114          16  WS-TOTAL-ENTRIES        PIC S9(3)    VALUE +0 COMP-3.EL636
00115      12  WS-SEQ-NINES                PIC S9(4)    VALUE +9999        CL**7
00116                                                   COMP.              CL**7
00117      12  WS-PAY-SEQ-NUM              PIC S9(4)    VALUE +0 COMP.     CL**7
00118      12  WS-NOFIRST                  PIC S9(4)    VALUE +0 COMP.     CL**8
00119      12  WS-DELETE-SEQ-NO            PIC S9(4)    VALUE +0 COMP.     CL**8
00120      12  WS-DELETE-AMOUNT            PIC S9(7)V99 VALUE +0 COMP-3.   CL**8
00121      12  WS-DELETE-COUNT             PIC S9(3)    VALUE +0 COMP-3.   CL**8
00122      12  WS-HOLD-AMOUNT              PIC S9(7)V99 VALUE +0 COMP-3.   CL*11
00123      12  WS-HOLD-COUNT               PIC S9(3)    VALUE +0 COMP-3.   CL*11
00124      12  WS-OFF-HOLD-AMOUNT          PIC S9(7)V99 VALUE +0 COMP-3.   CL*11
00125      12  WS-OFF-HOLD-COUNT           PIC S9(3)    VALUE +0 COMP-3.   CL*11
00126      12  WS-SAVE-ACTION-CODE         PIC X        VALUE SPACES.      CL**8
00127          88 SHOW-FUNCTION            VALUE 'S'.                      CL**8
00128      12  WS-HOLD-MAINT OCCURS 5 TIMES    PIC X.                      CL*11
00129      12  FILLER        PIC X(16) VALUE 'ZZZZZZZZZZZZZZZZ'.           CL*13
00130      12  HOLD-CSR                 PIC X(04)  VALUE LOW-VALUES.       CL*13
00131                                                                   EL636
00132      EJECT                                                        EL636
00133                                                                   EL636
00134 ******************************************************************EL636
00135 *                                                                *EL636
00136 *                E R R O R   M E S S A G E S                     *EL636
00137 *                                                                *EL636
00138 ******************************************************************EL636
00139                                                                   EL636
00140  01  ERROR-MESSAGES.                                              EL636
00141      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL636
00142      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL636
00143      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL636
00144      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL636
00145      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL636
00146      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL636
00147      12  ER-0194                 PIC X(4)  VALUE '0194'.          EL636
00148      12  ER-0195                 PIC X(4)  VALUE '0195'.          EL636
00149      12  ER-1883                 PIC X(4)  VALUE '1883'.             CL*13
00150      12  ER-1999                 PIC X(4)  VALUE '1999'.             CL*11
00151      12  ER-2056                 PIC X(4)  VALUE '2056'.          EL636
00152      12  ER-2132                 PIC X(4)  VALUE '2132'.          EL636
00153      12  ER-2223                 PIC X(4)  VALUE '2223'.          EL636
00154      12  ER-2251                 PIC X(4)  VALUE '2251'.          EL636
00155      12  ER-2252                 PIC X(4)  VALUE '2252'.          EL636
00156      12  ER-2800                 PIC X(4)  VALUE '2800'.          EL636
00157      12  ER-2869                 PIC X(4)  VALUE '2869'.             CL*16
00158      12  ER-3139                 PIC X(4)  VALUE '3139'.             CL**3
00159      12  ER-3146                 PIC X(4)  VALUE '3146'.             CL*20
00160      12  ER-3147                 PIC X(4)  VALUE '3147'.          EL636
00161      12  ER-3148                 PIC X(4)  VALUE '3148'.          EL636
00162      12  ER-3149                 PIC X(4)  VALUE '3149'.          EL636
00163      12  ER-3158                 PIC X(4)  VALUE '3158'.          EL636
00164      12  ER-3159                 PIC X(4)  VALUE '3159'.          EL636
00165      12  ER-3160                 PIC X(4)  VALUE '3160'.          EL636
00166      12  ER-3161                 PIC X(4)  VALUE '3161'.          EL636
00167      12  ER-3162                 PIC X(4)  VALUE '3162'.          EL636
00168      12  ER-3163                 PIC X(4)  VALUE '3163'.          EL636
00169      12  ER-3164                 PIC X(4)  VALUE '3164'.          EL636
00170      12  ER-3165                 PIC X(4)  VALUE '3165'.          EL636
00171      12  ER-3167                 PIC X(4)  VALUE '3167'.             CL**2
00172      12  ER-3169                 PIC X(4)  VALUE '3169'.             CL**4
00173      12  ER-3173                 PIC X(4)  VALUE '3173'.             CL**6
00174      12  ER-3179                 PIC X(4)  VALUE '3179'.             CL*17
00175      12  ER-3189                 PIC X(4)  VALUE '3189'.             CL*17
00176      12  ER-3190                 PIC X(4)  VALUE '3190'.             CL*20
00177      12  ER-3198                 PIC X(4)  VALUE '3198'.             CL*26
00178      12  ER-3256                 PIC X(4)  VALUE '3256'.             CL*21
00179      12  ER-7043                 PIC X(4)  VALUE '7043'.             CL**7
00180      12  ER-7082                 PIC X(4)  VALUE '7082'.             CL*17
00181      12  ER-7804                 PIC X(4)  VALUE '7804'.             CL*11
00182                                                                   EL636
00183      EJECT                                                        EL636
00184                                                                   EL636
00185 ******************************************************************EL636
00186 *                                                                *EL636
00187 *              A C C E S S   K E Y S                             *EL636
00188 *                                                                *EL636
00189 ******************************************************************EL636
00190                                                                   EL636
00191  01  ACCESS-KEYS.                                                 EL636
00192                                                                   EL636
00193      12  ELCNTL-KEY.                                                 CL*13
00194           16  CNTL-COMP-ID           PIC X(3)       VALUE SPACES.    CL*13
00195           16  CNTL-REC-TYPE          PIC X          VALUE SPACES.    CL*13
00196           16  CNTL-ACCESS            PIC X(4)       VALUE SPACES.    CL*13
00197           16  CNTL-SEQ-NO            PIC S9(4)     VALUE +0  COMP.   CL*13
00198      12  ERCMCK-KEY.                                              EL636
00199          16  ERCMCK-COMPANY-CD       PIC X          VALUE SPACES. EL636
00200          16  ERCMCK-CSR              PIC X(4)   VALUE LOW-VALUES.    CL*13
00201          16  ERCMCK-CARRIER          PIC X          VALUE SPACES. EL636
00202          16  ERCMCK-GROUPING         PIC X(6)       VALUE SPACES. EL636
00203          16  ERCMCK-PAYEE            PIC X(10)      VALUE SPACES. EL636
00204          16  ERCMCK-PAYEE-SEQ        PIC S9(4) COMP VALUE +0.        CL**7
00205          16  ERCMCK-SEQUENCE-NO      PIC S9(4) COMP VALUE +0.     EL636
00206                                                                      CL**4
00207      12  SAVE-ERCMCK-CONTROL.                                        CL**4
00208          16  SVCMCK-COMPANY-CD       PIC X          VALUE SPACES.    CL**4
00209          16  SVCMCK-CSR              PIC X(4)    VALUE LOW-VALUES.   CL*13
00210          16  SVCMCK-CARRIER          PIC X          VALUE SPACES.    CL**4
00211          16  SVCMCK-GROUPING         PIC X(6)       VALUE SPACES.    CL**4
00212          16  SVCMCK-PAYEE            PIC X(10)      VALUE SPACES.    CL**4
00213          16  SVCMCK-PAYEE-SEQ        PIC S9(4) COMP VALUE +0.        CL**7
00214          16  SVCMCK-SEQUENCE-NO      PIC S9(4) COMP VALUE +0.        CL**4
00215                                                                   EL636
00216      12  ERCKWK-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.   EL636
00217                                                                   EL636
00218      12  ERCKWK-KEY.                                              EL636
00219          16  ERCKWK-COMPANY-CD       PIC X           VALUE SPACES.EL636
00220          16  ERCKWK-CSR              PIC X(4)   VALUE LOW-VALUES.    CL*13
00221          16  ERCKWK-CARRIER          PIC X           VALUE SPACES.EL636
00222          16  ERCKWK-GROUPING         PIC X(6)        VALUE SPACES.EL636
00223          16  ERCKWK-PAYEE            PIC X(10)       VALUE SPACES.EL636
00224          16  ERCKWK-PAYEE-SEQ        PIC S9(4)  COMP VALUE +0.       CL**7
00225          16  ERCKWK-SEQUENCE-NO      PIC S9(4)  COMP VALUE +0.    EL636
00226                                                                   EL636
00227      12  ERCKWK-COMPARE-KEY          PIC X(26)       VALUE SPACE.    CL*13
00228                                                                   EL636
00229      12  SAVE-ERCKWK-CONTROL.                                     EL636
00230          16  SVCKWK-COMPANY-CD       PIC X           VALUE SPACES.EL636
00231          16  SVCKWK-CSR              PIC X(4)  VALUE LOW-VALUES.     CL*13
00232          16  SVCKWK-CARRIER          PIC X           VALUE SPACES.EL636
00233          16  SVCKWK-GROUPING         PIC X(6)        VALUE SPACES.EL636
00234          16  SVCKWK-PAYEE            PIC X(10)       VALUE SPACES.EL636
00235          16  SVCKWK-PAYEE-SEQ        PIC S9(4) COMP  VALUE +0.       CL**7
00236          16  SVCKWK-SEQUENCE-NO      PIC S9(4) COMP  VALUE +0.       CL**7
00237                                                                      CL**7
00238      12  SVCKWK-COMPARE-KEY          PIC X(26)       VALUE SPACE.    CL*13
00239                                                                   EL636
00240      12  SAVE-ERCKWK-RECORD.                                         CL*21
00241          16  FILLER                  PIC X(26).                      CL*21
00242          16  SAVE-CKWK-SEQ-NO        PIC S9(4) COMP  VALUE +0.       CL*21
00243          16  FILLER                  PIC X(172).                     CL*21
00244                                                                      CL*21
00245                                                                      CL*21
00246                                                                      CL*21
00247      12  ERCMCK-RECORD-LENGTH        PIC S9(4) COMP VALUE +2000.     CL*11
00248                                                                   EL636
00249      12  ERCOMP-KEY.                                              EL636
00250          16  ERCOMP-COMP-CD      PIC X     VALUE SPACE.           EL636
00251          16  ERCOMP-CARRIER      PIC X     VALUE SPACES.          EL636
00252          16  ERCOMP-GROUPING     PIC X(6)  VALUE SPACES.          EL636
00253          16  ERCOMP-FIN-RESP     PIC X(10) VALUE SPACES.          EL636
00254          16  ERCOMP-ACCT         PIC X(10) VALUE SPACES.          EL636
00255          16  ERCOMP-RECORD-TYPE  PIC X     VALUE SPACES.          EL636
00256                                                                   EL636
00257      EJECT                                                        EL636
00258                                                                   EL636
00259                              COPY ELCDATE.                           CL*13
00260                                                                   EL636
00261      EJECT                                                        EL636
00262                              COPY ELCLOGOF.                          CL*13
00263                                                                   EL636
00264      EJECT                                                        EL636
00265                              COPY ELCATTR.                           CL*13
00266                                                                   EL636
00267      EJECT                                                        EL636
00268                              COPY ELCEMIB.                           CL*13
00269                                                                   EL636
00270      EJECT                                                        EL636
00271                              COPY ELCINTF.                           CL*13
00272      12  FILLER  REDEFINES PI-PROGRAM-WORK-AREA.                  EL636
00273          16  PI-MAINT-FUNCTION       PIC X.                       EL636
00274              88  PI-ADD-FUNCTION         VALUE 'A'.               EL636
00275              88  PI-CHG-FUNCTION         VALUE 'C'.               EL636
00276              88  PI-DEL-FUNCTION         VALUE 'D'.               EL636
00277              88  PI-SHOW-FUNCTION        VALUE 'S'.               EL636
00278          16  PI-ERCKWK-KEY.                                       EL636
00279              20  PI-ERCKWK-COMP-CD   PIC X.                       EL636
00280              20  PI-ERCKWK-CSR       PIC X(4).                       CL*13
00281              20  PI-ERCKWK-CARRIER   PIC X.                       EL636
00282              20  PI-ERCKWK-GROUPING  PIC X(6).                    EL636
00283              20  PI-ERCKWK-PAYEE     PIC X(10).                   EL636
00284              20  PI-ERCKWK-PAYEE-SEQ PIC S9(4)   COMP.               CL**7
00285              20  PI-ERCKWK-SEQ-NO    PIC S9(4)   COMP.            EL636
00286          16  PI-SAVE-ERCKWK-KEY.                                  EL636
00287              20  PI-SVCKWK-COMP-CD   PIC X.                       EL636
00288              20  PI-SVCKWK-CSR       PIC X(4).                       CL*13
00289              20  PI-SVCKWK-CARRIER   PIC X.                       EL636
00290              20  PI-SVCKWK-GROUPING  PIC X(6).                    EL636
00291              20  PI-SVCKWK-PAYEE     PIC X(10).                   EL636
00292              20  PI-SVCKWK-PAYEE-SEQ PIC S9(4)   COMP.               CL**7
00293              20  PI-SVCKWK-SEQ-NO    PIC S9(4)   COMP.            EL636
00294          16  PI-STUB-KEY OCCURS 5    PIC X(26).                      CL*13
00295          16  PI-STUB-ACCT OCCURS 5   PIC X(10).                      CL*21
00296          16  PI-NO-ENTRIES-DISPLAYED PIC S9.                      EL636
00297          16  PI-PREV-FUNCTION        PIC X.                          CL**7
00298          16  PI-LAST-STUB-KEY.                                       CL**8
00299              20  PI-LAST.                                            CL*13
00300                  22  FILLER          PIC X.                          CL*13
00301                  22  PI-LAST-STUB-CSR   PIC  X(4).                   CL*13
00302                  22  FILLER          PIC X(19).                      CL*13
00303              20  PI-LAST-STUB-SEQ-NO PIC S9(4)   COMP.               CL**8
00304          16  FILLER                  PIC X(379).                     CL*24
00305                                                                   EL636
00306      EJECT                                                        EL636
00307                              COPY ELCJPFX.                           CL*13
00308                              PIC X(1583).                         EL636
00309                                                                   EL636
00310      EJECT                                                        EL636
00311                              COPY ELCAID.                            CL*13
00312  01  FILLER    REDEFINES DFHAID.                                  EL636
00313      12  FILLER              PIC X(8).                            EL636
00314      12  PF-VALUES           PIC X       OCCURS 24.                  CL*15
00315                                                                   EL636
00316      EJECT                                                        EL636
00317                              COPY EL636S.                            CL*13
00318                                                                   EL636
00319  01  DISPLAY-MAP REDEFINES EL636AI.                               EL636
00320      12  FILLER                  PIC X(355).                         CL*13
00321      12  CK-STUB-INFO OCCURS 5 TIMES.                             EL636
00322          16  CK-MAINT-LEN        PIC S9(4)   COMP.                EL636
00323          16  CK-MAINT-ATTRB      PIC X.                           EL636
00324          16  CK-MAINT            PIC X.                           EL636
00325          16  CK-COMNTS-LEN       PIC S9(4)   COMP.                EL636
00326          16  CK-COMNTS-ATTRB     PIC X.                           EL636
00327          16  CK-COMNTS           PIC X(21).                          CL**2
00328          16  CK-AGENT-LEN        PIC S9(4)   COMP.                EL636
00329          16  CK-AGENT-ATTRB      PIC X.                           EL636
00330          16  CK-AGENT            PIC X(10).                       EL636
00331          16  CK-INVREF-LEN       PIC S9(4)   COMP.                EL636
00332          16  CK-INVREF-ATTRB     PIC X.                           EL636
00333          16  CK-INVREF           PIC X(12).                       EL636
00334          16  CK-LEDGER-LEN       PIC S9(4)   COMP.                EL636
00335          16  CK-LEDGER-ATTRB     PIC X.                           EL636
00336          16  CK-LEDGER           PIC X(14).                       EL636
00337          16  CK-AGOCD-LEN        PIC S9(4)   COMP.                   CL*20
00338          16  CK-AGOCD-ATTRB      PIC X.                              CL*20
00339          16  CK-AGOCD            PIC X(1).                           CL*20
00340          16  CK-DETAMT-LEN       PIC S9(4)   COMP.                EL636
00341          16  CK-DETAMT-ATTRB     PIC X.                           EL636
00342          16  CK-DETAMT-IN        PIC X(12).                       EL636
00343          16  CK-DETAMT-OUT REDEFINES CK-DETAMT-IN                 EL636
00344                                  PIC ZZZZ,ZZZ.99-.                EL636
00345      EJECT                                                        EL636
00346  LINKAGE SECTION.                                                 EL636
00347  01  DFHCOMMAREA             PIC X(1024).                         EL636
00348                                                                   EL636
00349      EJECT                                                        EL636
00350 *01 PARMLIST .                                                       CL*24
00351 *    02  FILLER              PIC S9(8)   COMP.                       CL*24
00352 *    02  ERCKWK-POINTER      PIC S9(8)   COMP.                       CL*24
00353 *    02  ERCMCK-POINTER      PIC S9(8)   COMP.                       CL*24
00354 *    02  ERCOMP-POINTER      PIC S9(8)   COMP.                       CL*24
00355 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL*24
00356                                                                   EL636
00357      EJECT                                                        EL636
00358                                                                   EL636
00359                               COPY ERCCKWK.                          CL*13
00360      EJECT                                                        EL636
00361                                                                   EL636
00362                               COPY ERCCMCK.                          CL*13
00363      EJECT                                                        EL636
00364                                                                   EL636
00365                               COPY ERCCOMP.                          CL*13
00366      EJECT                                                           CL**4
00367                               COPY ELCCNTL.                          CL*24
00368                                                                      CL**4
00369                                                                   EL636
00370  PROCEDURE DIVISION.                                              EL636
00371                                                                   EL636
00372      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL636
00373      MOVE 2                      TO EMI-NUMBER-OF-LINES.             CL**7
00374                                                                   EL636
00375      IF EIBCALEN = 0                                              EL636
00376          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL636
00377                                                                   EL636
00378      MOVE EIBTRMID               TO QID-TERM.                     EL636
00379      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL636
00380      MOVE '5'                    TO DC-OPTION-CODE.               EL636
00381      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL636
00382      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL636
00383      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                EL636
00384      MOVE ZEROS                  TO WS-NOFIRST.                      CL**8
00385                                                                   EL636
00386      IF PI-RETURN-TO-PROGRAM = THIS-PGM                              CL*11
00387          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM                    CL*11
00388      ELSE                                                            CL*11
00389          MOVE SPACES             TO RETURNED-FROM.                   CL*11
00390                                                                      CL*11
00391      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL636
00392          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL636
00393              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL636
00394              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL636
00395              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL636
00396              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL636
00397              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL636
00398              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL636
00399              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL636
00400              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL636
00401          ELSE                                                     EL636
00402              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL636
00403              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL636
00404              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL636
00405              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL636
00406              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL636
00407              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL636
00408              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL636
00409              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL636
00410                                                                   EL636
00411      MOVE LOW-VALUES             TO EL636AI.                      EL636
00412                                                                   EL636
00413      IF EIBTRNID NOT = TRANS-EXJA                                 EL636
00414          IF RETURNED-FROM EQUAL XCTL-EL6361                          CL*11
00415              GO TO 5000-DISPLAY-PAYEE                                CL*11
00416          ELSE                                                        CL*11
00417              MOVE SPACES              TO PI-PROGRAM-WORK-AREA        CL*11
00418              MOVE PI-COMPANY-CD       TO PI-ERCKWK-COMP-CD           CL*11
00419              MOVE +0                  TO PI-ERCKWK-SEQ-NO            CL*11
00420              MOVE +0                  TO PI-NO-ENTRIES-DISPLAYED     CL*21
00421              GO TO 8100-SEND-INITIAL-MAP.                            CL*20
00422                                                                   EL636
00423      EXEC CICS HANDLE CONDITION                                   EL636
00424          PGMIDERR  (9600-PGMID-ERROR)                             EL636
00425          ERROR     (9990-ABEND)                                   EL636
00426      END-EXEC.                                                    EL636
00427                                                                   EL636
00428      IF EIBAID = DFHCLEAR                                         EL636
00429          GO TO 9400-CLEAR.                                        EL636
00430                                                                   EL636
00431      IF PI-PROCESSOR-ID = 'LGXX'                                  EL636
00432          GO TO 0200-RECEIVE.                                      EL636
00433                                                                   EL636
00434      EXEC CICS READQ TS                                           EL636
00435          QUEUE  (QID)                                             EL636
00436          INTO   (SECURITY-CONTROL)                                EL636
00437          LENGTH (SC-COMM-LENGTH)                                  EL636
00438          ITEM   (SC-ITEM)                                         EL636
00439      END-EXEC.                                                    EL636
00440                                                                   EL636
00441      MOVE SC-CREDIT-DISPLAY (6)   TO PI-DISPLAY-CAP.                 CL**2
00442      MOVE SC-CREDIT-UPDATE  (6)   TO PI-MODIFY-CAP.                  CL**2
00443                                                                   EL636
00444      IF NOT DISPLAY-CAP                                           EL636
00445          MOVE 'READ'          TO SM-READ                          EL636
00446          PERFORM 9995-SECURITY-VIOLATION                          EL636
00447          MOVE ER-0070         TO  EMI-ERROR                       EL636
00448          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL636
00449          GO TO 8100-SEND-INITIAL-MAP.                             EL636
00450                                                                   EL636
00451      EJECT                                                        EL636
00452                                                                   EL636
00453 ******************************************************************EL636
00454 *                                                                *EL636
00455 *              R E C E I V E   M A P                             *EL636
00456 *                                                                *EL636
00457 ******************************************************************EL636
00458                                                                   EL636
00459  0200-RECEIVE.                                                    EL636
00460                                                                   EL636
00461      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL636
00462          MOVE ER-0008            TO EMI-ERROR                     EL636
00463          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL636
00464          MOVE -1                 TO MAINTL                        EL636
00465          GO TO 8200-SEND-DATAONLY.                                EL636
00466                                                                   EL636
00467      EXEC CICS RECEIVE                                            EL636
00468          MAP      (EL636A)                                        EL636
00469          MAPSET   (MAPSET-EL636S)                                 EL636
00470          INTO     (EL636AI)                                       EL636
00471      END-EXEC.                                                    EL636
00472                                                                   EL636
00473                                                                   EL636
00474      IF PFENTERL GREATER THAN ZERO                                EL636
00475         IF EIBAID NOT = DFHENTER                                  EL636
00476            MOVE ER-0004          TO EMI-ERROR                     EL636
00477            MOVE AL-UNBOF         TO PFENTERA                      EL636
00478            MOVE -1               TO PFENTERL                      EL636
00479            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL636
00480            GO TO 8200-SEND-DATAONLY                               EL636
00481         ELSE                                                      EL636
00482            IF PFENTERI NUMERIC AND                                EL636
00483               PFENTERI GREATER 0 AND LESS 25                         CL*10
00484               MOVE PF-VALUES (PFENTERI) TO EIBAID                 EL636
00485            ELSE                                                   EL636
00486               MOVE ER-0029       TO EMI-ERROR                     EL636
00487               MOVE AL-UNBOF      TO PFENTERA                      EL636
00488               MOVE -1            TO PFENTERL                      EL636
00489               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            EL636
00490               GO TO 8200-SEND-DATAONLY.                           EL636
00491                                                                   EL636
00492      EJECT                                                        EL636
00493                                                                   EL636
00494 ******************************************************************EL636
00495 *                                                                *EL636
00496 *              C H E C K   P F K E Y S                           *EL636
00497 *                                                                *EL636
00498 ******************************************************************EL636
00499                                                                   EL636
00500  0300-CHECK-PFKEYS.                                               EL636
00501                                                                   EL636
00502      IF EIBAID = DFHPF23                                          EL636
00503          GO TO 8810-PF23.                                         EL636
00504                                                                   EL636
00505      IF EIBAID = DFHPF24                                          EL636
00506          GO TO 9200-RETURN-MAIN-MENU.                             EL636
00507                                                                   EL636
00508      IF EIBAID = DFHPF12                                          EL636
00509          GO TO 9500-PF12.                                         EL636
00510                                                                   EL636
00511      IF EIBAID = DFHPF1                                           EL636
00512          GO TO 6000-DISPLAY-NEXT-PAYEE.                           EL636
00513                                                                   EL636
00514      IF EIBAID = DFHPF2                                           EL636
00515          GO TO 6100-DISPLAY-PREV-PAYEE.                           EL636
00516                                                                   EL636
00517      IF EIBAID = DFHPF3                                           EL636
00518          IF PI-LAST-STUB-KEY NOT EQUAL SPACES                        CL**8
00519              GO TO 5000-DISPLAY-PAYEE.                               CL**8
00520                                                                   EL636
00521      IF EIBAID = DFHPF4                                           EL636
00522          IF PI-STUB-KEY (1) NOT EQUAL SPACES                         CL**8
00523              GO TO 6600-DISPLAY-PREV-ENTRIES.                        CL**8
00524                                                                   EL636
00525      IF EIBAID = DFHPF5                                           EL636
00526          GO TO 7000-RELEASE-ENTRIES.                              EL636
00527                                                                   EL636
00528      IF EIBAID = DFHPF6                                           EL636
00529          MOVE LOW-VALUES         TO EL636AI                       EL636
00530          MOVE 'A'                TO MAINTI                        EL636
00531                                     PI-MAINT-FUNCTION             EL636
00532          MOVE AL-UANON           TO MAINTA                        EL636
00533          GO TO 8100-SEND-INITIAL-MAP.                             EL636
00534                                                                   EL636
00535      IF EIBAID = DFHPF7                                           EL636
00536         IF MAINTI = 'D'                                           EL636
00537            GO TO 1000-EDIT-MAP.                                   EL636
00538                                                                      CL*11
00539      IF EIBAID = DFHPF8                                              CL*11
00540          GO TO 8820-PF08.                                            CL*12
00541                                                                   EL636
00542      IF EIBAID = DFHENTER                                         EL636
00543          GO TO 1000-EDIT-MAP.                                     EL636
00544                                                                   EL636
00545      MOVE ER-0008 TO EMI-ERROR.                                   EL636
00546      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
00547      MOVE -1                     TO PFENTERL.                     EL636
00548                                                                   EL636
00549      GO TO 8200-SEND-DATAONLY.                                    EL636
00550                                                                   EL636
00551      EJECT                                                        EL636
00552                                                                   EL636
00553 ******************************************************************EL636
00554 *                                                                *EL636
00555 *                  E D I T    M A P                              *EL636
00556 *                                                                *EL636
00557 ******************************************************************EL636
00558                                                                   EL636
00559  1000-EDIT-MAP.                                                   EL636
00560                                                                   EL636
00561      IF MAINTI = 'C' OR 'D' OR 'S' OR 'A' OR 'V'                     CL**4
00562         MOVE MAINTI              TO PI-MAINT-FUNCTION             EL636
00563         MOVE AL-UANON            TO MAINTA                        EL636
00564      ELSE                                                         EL636
00565         MOVE ER-0023             TO EMI-ERROR                     EL636
00566         MOVE -1                  TO MAINTL                        EL636
00567         MOVE AL-UABON            TO MAINTA                        EL636
00568         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL636
00569         GO TO 8200-SEND-DATAONLY.                                 EL636
00570                                                                   EL636
00571      IF PAYSEQL GREATER THAN 0                                       CL**7
00572           MOVE PAYSEQI           TO WS-PAY-SEQ-NUM                   CL**7
00573      ELSE                                                            CL**7
00574           MOVE +1                TO WS-PAY-SEQ-NUM.                  CL**7
00575                                                                      CL**8
00576      IF MAINTI = 'S' AND                                             CL**8
00577         NOFIRSTL GREATER THAN 0                                      CL**8
00578         IF NOFIRSTI NUMERIC                                          CL**8
00579             MOVE NOFIRSTI          TO WS-NOFIRST                     CL**8
00580         ELSE                                                         CL**8
00581             MOVE -1                  TO NOFIRSTL                     CL**8
00582             MOVE ER-2223             TO EMI-ERROR                    CL**8
00583             MOVE AL-UNBON            TO NOFIRSTA                     CL**8
00584             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                CL**8
00585                                                                      CL**7
00586      IF MAINTI = 'C' OR 'D' OR 'V'                                   CL**4
00587         IF PI-COMPANY-CD   = PI-SVCKWK-COMP-CD  AND                  CL**7
00588            CSRI            = PI-SVCKWK-CSR      AND                  CL*13
00589            CARI            = PI-SVCKWK-CARRIER  AND                  CL**7
00590            GROUPI          = PI-SVCKWK-GROUPING AND                  CL**7
00591            PAYEEI          = PI-SVCKWK-PAYEE    AND                  CL**7
00592            WS-PAY-SEQ-NUM  = PI-SVCKWK-PAYEE-SEQ                     CL**7
00593            NEXT SENTENCE                                          EL636
00594         ELSE                                                      EL636
00595            MOVE ER-2056             TO EMI-ERROR                  EL636
00596            MOVE -1                  TO MAINTL                     EL636
00597            MOVE AL-UABON            TO MAINTA                     EL636
00598            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL636
00599            GO TO 8200-SEND-DATAONLY.                              EL636
00600                                                                   EL636
00601      IF MAINTI = 'D'                                              EL636
00602         IF EIBAID = DFHPF7                                        EL636
00603            MOVE MAINTI              TO PI-MAINT-FUNCTION          EL636
00604            MOVE AL-UANON            TO MAINTA                     EL636
00605         ELSE                                                      EL636
00606            MOVE ER-3147             TO EMI-ERROR                  EL636
00607            MOVE -1                  TO MAINTL                     EL636
00608            MOVE AL-UABON            TO MAINTA                     EL636
00609            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL636
00610            GO TO 8200-SEND-DATAONLY.                              EL636
00611                                                                   EL636
00612      IF NOT MODIFY-CAP                                            EL636
00613         IF MAINTI = 'S'                                           EL636
00614            MOVE MAINTI              TO PI-MAINT-FUNCTION          EL636
00615         ELSE                                                      EL636
00616            MOVE 'UPDATE'            TO SM-READ                    EL636
00617            PERFORM 9995-SECURITY-VIOLATION                        EL636
00618            MOVE ER-0070             TO EMI-ERROR                  EL636
00619            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL636
00620            GO TO 8100-SEND-INITIAL-MAP.                           EL636
00621                                                                   EL636
00622      IF MAINTI = 'S' OR 'D' OR 'V'                                   CL**4
00623         GO TO 1050-EDIT-COMPLETE.                                    CL**4
00624                                                                      CL**4
00625      IF CARL GREATER THAN ZERO                                    EL636
00626         MOVE AL-UANON            TO CARA                          EL636
00627      ELSE                                                         EL636
00628         MOVE -1                  TO CARL                          EL636
00629         MOVE ER-0194             TO EMI-ERROR                     EL636
00630         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL636
00631                                                                   EL636
00632      IF GROUPL GREATER THAN ZEROS                                 EL636
00633         MOVE AL-UANON            TO GROUPA                        EL636
00634      ELSE                                                         EL636
00635         MOVE -1                  TO GROUPL                        EL636
00636         MOVE ER-0195             TO EMI-ERROR                     EL636
00637         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL636
00638                                                                   EL636
00639      IF PAYEEL GREATER THAN ZEROS                                 EL636
00640         MOVE AL-UANON            TO PAYEEA                        EL636
00641      ELSE                                                         EL636
00642         MOVE -1                  TO PAYEEL                        EL636
00643         MOVE ER-3148             TO EMI-ERROR                     EL636
00644         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 EL636
00645                                                                      CL*13
00646      IF CSRL GREATER THAN ZERO                                       CL*13
00647         MOVE AL-UANON            TO CSRA                             CL*13
00648         PERFORM 1600-READ-CNTL-FILE  THRU 1690-EXIT.                 CL*13
00649                                                                      CL*13
00650                                                                   EL636
00651      IF PAYTOL  GREATER THAN ZEROS                                EL636
00652         MOVE AL-UANON            TO PAYTOA.                       EL636
00653                                                                   EL636
00654      IF ADDRS1L GREATER THAN ZEROS                                EL636
00655         MOVE AL-UANON            TO ADDRS1A.                      EL636
00656                                                                   EL636
00657      IF ADDRS2L GREATER THAN ZEROS                                EL636
00658         MOVE AL-UANON            TO ADDRS2A.                      EL636
00659                                                                   EL636
00660      IF CITYSTL GREATER THAN ZEROS                                EL636
00661         MOVE AL-UANON            TO CITYSTA.                      EL636
00662                                                                   EL636
00663      IF ZIPL    GREATER THAN ZEROS                                EL636
00664         MOVE AL-UANON            TO ZIPA.                         EL636
00665                                                                   EL636
00666      IF ZIPEXTL GREATER THAN ZEROS                                EL636
00667         MOVE AL-UANON            TO ZIPEXTL.                      EL636
00668                                                                   EL636
00669      MOVE +0                     TO WS-SUB1.                      EL636
00670                                                                   EL636
00671  1025-EDIT-STUB-INFO.                                             EL636
00672                                                                   EL636
00673      ADD +1                      TO WS-SUB1.                      EL636
00674                                                                   EL636
00675      IF WS-SUB1 GREATER THAN +5                                   EL636
00676         GO TO 1050-EDIT-COMPLETE.                                 EL636
00677                                                                   EL636
00678      IF CK-MAINT-LEN (WS-SUB1) NOT GREATER THAN ZEROS             EL636
00679          IF CK-AGENT-LEN (WS-SUB1)   GREATER THAN ZERO  OR           CL**8
00680             CK-COMNTS-LEN (WS-SUB1)  GREATER THAN ZERO  OR           CL*11
00681             CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZERO  OR           CL**8
00682             CK-LEDGER-LEN (WS-SUB1)  GREATER THAN ZERO  OR           CL**8
00683             CK-AGOCD-LEN  (WS-SUB1)  GREATER THAN ZERO  OR           CL*20
00684             CK-DETAMT-LEN (WS-SUB1)  GREATER THAN ZERO               CL**8
00685              NEXT SENTENCE                                           CL**8
00686          ELSE                                                        CL**8
00687              GO TO 1025-EDIT-STUB-INFO.                              CL**8
00688                                                                   EL636
00689      IF PI-COMPANY-ID = 'NSL' AND                                    CL*25
00690          CK-MAINT (WS-SUB1) = 'H'                                    CL*25
00691              MOVE ER-0023        TO EMI-ERROR                        CL*25
00692              MOVE -1             TO CK-MAINT-LEN    (WS-SUB1)        CL*25
00693              MOVE AL-UABON       TO CK-MAINT-ATTRB  (WS-SUB1)        CL*25
00694              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*25
00695      ELSE                                                         EL636
00696          IF CK-MAINT (WS-SUB1) = 'A' OR 'C' OR 'D' OR 'I'            CL*25
00697                                      OR 'R' OR 'N' OR 'H'            CL*25
00698              MOVE AL-UANON       TO CK-MAINT-ATTRB  (WS-SUB1)        CL*25
00699          ELSE                                                        CL*25
00700              MOVE ER-0023        TO EMI-ERROR                        CL*25
00701              MOVE -1             TO CK-MAINT-LEN    (WS-SUB1)        CL*25
00702              MOVE AL-UABON       TO CK-MAINT-ATTRB  (WS-SUB1)        CL*25
00703              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*25
00704                                                                      CL*11
00705      IF (CK-MAINT-LEN (WS-SUB1) GREATER THAN ZEROS) AND              CL*11
00706             (CK-MAINT (WS-SUB1) = 'H')                               CL*11
00707          IF CK-AGENT-LEN (WS-SUB1)   GREATER THAN ZERO  OR           CL*11
00708             CK-COMNTS-LEN (WS-SUB1)  GREATER THAN ZERO  OR           CL*11
00709             CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZERO  OR           CL*11
00710             CK-LEDGER-LEN (WS-SUB1)  GREATER THAN ZERO  OR           CL*11
00711             CK-AGOCD-LEN  (WS-SUB1)  GREATER THAN ZERO  OR           CL*20
00712             CK-DETAMT-LEN (WS-SUB1)  GREATER THAN ZERO               CL*11
00713               MOVE ER-7804          TO EMI-ERROR                     CL*11
00714               MOVE -1               TO CK-MAINT-LEN    (WS-SUB1)     CL*11
00715               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               CL*18
00716          ELSE                                                        CL*18
00717               GO TO 1025-EDIT-STUB-INFO.                             CL*22
00718                                                                   EL636
00719      IF CK-MAINT        (WS-SUB1) = 'D'                              CL*17
00720          GO TO 1025-EDIT-STUB-INFO.                                  CL*17
00721                                                                   EL636
00722 ******    TO DISTINGUISH BETWEEN AN R OR I USED AS A CHANGE OR       CL*21
00723 ******    AN ADDITION CHANGE I TO J AND R TO S FOR AN ADD            CL*21
00724      IF CK-MAINT (WS-SUB1) = 'I' AND                                 CL*21
00725         WS-SUB1 GREATER THAN PI-NO-ENTRIES-DISPLAYED                 CL*21
00726          MOVE 'J' TO CK-MAINT (WS-SUB1).                             CL*21
00727                                                                      CL*21
00728      IF CK-MAINT (WS-SUB1) = 'R' AND                                 CL*21
00729         WS-SUB1 GREATER THAN PI-NO-ENTRIES-DISPLAYED                 CL*21
00730          MOVE 'S' TO CK-MAINT (WS-SUB1).                             CL*21
00731                                                                      CL*21
00732 **********************                                               CL*21
00733                                                                      CL**2
00734      IF CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZEROS                  CL*17
00735          IF CK-MAINT  (WS-SUB1) = 'R' OR 'I' OR 'J' OR 'S'           CL*21
00736              NEXT SENTENCE                                           CL*17
00737          ELSE                                                        CL*17
00738            MOVE ER-3189          TO EMI-ERROR                        CL*17
00739            MOVE -1               TO CK-INVREF-LEN   (WS-SUB1)        CL*17
00740            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**6
00741                                                                      CL**6
00742      IF CK-MAINT  (WS-SUB1) = 'R' OR 'I' OR 'J' OR 'S'               CL*21
00743          IF CK-INVREF (WS-SUB1)  NOT EQUAL LOW-VALUES                CL*21
00744              NEXT SENTENCE                                           CL*20
00745          ELSE                                                        CL*20
00746            MOVE ER-3256          TO EMI-ERROR                        CL*21
00747            MOVE -1               TO CK-INVREF-LEN   (WS-SUB1)        CL*20
00748            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL*20
00749                                                                      CL*20
00750      IF CK-MAINT        (WS-SUB1) = 'N' AND                          CL**6
00751         MAINTI                    = ('A' OR 'C')                     CL**7
00752         IF CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZEROS               CL**6
00753            MOVE ER-3173          TO EMI-ERROR                        CL**6
00754            MOVE -1               TO CK-INVREF-LEN   (WS-SUB1)        CL**6
00755            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                 CL**2
00756                                                                      CL*11
00757      IF CK-AGOCD-LEN (WS-SUB1) GREATER THAN ZERO                     CL*20
00758          IF CK-AGOCD (WS-SUB1)  EQUAL 'A' OR 'G' OR 'O'              CL*20
00759              NEXT SENTENCE                                           CL*20
00760          ELSE                                                        CL*20
00761              MOVE ER-3146          TO EMI-ERROR                      CL*20
00762              MOVE -1               TO CK-AGOCD-LEN   (WS-SUB1)       CL*20
00763              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*11
00764                                                                      CL*20
00765      IF CK-AGOCD (WS-SUB1) EQUAL 'A'                                 CL*20
00766          IF CK-AGENT (WS-SUB1) GREATER THAN SPACES                   CL*20
00767              NEXT SENTENCE                                           CL*20
00768          ELSE                                                        CL*20
00769              MOVE ER-3190          TO EMI-ERROR                      CL*20
00770              MOVE -1               TO CK-AGENT-LEN   (WS-SUB1)       CL*20
00771              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
00772                                                                      CL*20
00773      IF CK-MAINT    (WS-SUB1) = 'A' OR 'J' OR 'S'                    CL*21
00774          IF CK-AGOCD-LEN  (WS-SUB1)  GREATER THAN ZERO               CL*20
00775              NEXT SENTENCE                                           CL*20
00776          ELSE                                                        CL*20
00777              MOVE ER-3146          TO EMI-ERROR                      CL*20
00778              MOVE -1               TO CK-AGOCD-LEN   (WS-SUB1)       CL*20
00779              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
00780                                                                      CL*20
00781      IF CK-MAINT    (WS-SUB1) = 'R' OR 'I' OR 'J' OR 'S'             CL*21
00782          IF CK-AGOCD-LEN  (WS-SUB1)  GREATER THAN ZERO               CL*20
00783              NEXT SENTENCE                                           CL*20
00784          ELSE                                                        CL*20
00785              IF CK-AGOCD (WS-SUB1) = SPACE OR LOW-VALUE              CL*20
00786                  NEXT SENTENCE                                       CL*20
00787              ELSE                                                    CL*20
00788                  MOVE ER-3146      TO EMI-ERROR                      CL*20
00789                  MOVE -1           TO CK-AGOCD-LEN   (WS-SUB1)       CL*20
00790                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL*20
00791                                                                   EL636
00792      IF CK-LEDGER-LEN (WS-SUB1) GREATER THAN ZEROS                EL636
00793         MOVE AL-UANON            TO  CK-LEDGER-LEN  (WS-SUB1).    EL636
00794                                                                   EL636
00795      IF CK-MAINT (WS-SUB1)     = 'A' OR 'C' OR 'N'                   CL**8
00796          IF PI-COMPANY-ID = 'CRI'                                    CL*26
00797              IF CK-LEDGER-LEN (WS-SUB1) GREATER THAN ZEROS           CL**7
00798                 PERFORM 1500-VERIFY-LEDGER THRU 1590-EXIT            CL**7
00799              ELSE                                                    CL**7
00800                  MOVE ER-3149    TO EMI-ERROR                        CL**7
00801                  MOVE -1         TO CK-LEDGER-LEN    (WS-SUB1)       CL**7
00802                  MOVE AL-UABON   TO CK-LEDGER-ATTRB  (WS-SUB1)       CL**7
00803                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.           CL**7
00804                                                                      CL**2
00805      IF CK-MAINT (WS-SUB1)     = 'R'  AND MAINTI = 'A'               CL**2
00806          IF PI-COMPANY-ID = 'CRI'                                    CL*26
00807              IF CK-LEDGER-LEN (WS-SUB1) GREATER THAN ZEROS           CL**7
00808                 PERFORM 1500-VERIFY-LEDGER THRU 1590-EXIT            CL**7
00809              ELSE                                                    CL**7
00810                 MOVE ER-3149     TO EMI-ERROR                        CL**7
00811                 MOVE -1          TO CK-LEDGER-LEN    (WS-SUB1)       CL**7
00812                 MOVE AL-UABON    TO CK-LEDGER-ATTRB  (WS-SUB1)       CL**7
00813                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            CL**7
00814                                                                   EL636
00815      IF CK-DETAMT-LEN (WS-SUB1)  GREATER THAN ZEROS               EL636
00816         MOVE CK-DETAMT-IN (WS-SUB1) TO DEEDIT-FIELD               EL636
00817         PERFORM 8600-DEEDIT                                       EL636
00818         IF DEEDIT-FIELD-V2 NUMERIC AND                               CL*21
00819            DEEDIT-FIELD-V2 NOT EQUAL ZERO                            CL*21
00820            MOVE DEEDIT-FIELD-V2  TO WS-DETAMT       (WS-SUB1)     EL636
00821            MOVE AL-UNNON         TO CK-DETAMT-ATTRB (WS-SUB1)     EL636
00822         ELSE                                                      EL636
00823            MOVE ER-2223          TO EMI-ERROR                     EL636
00824            MOVE -1               TO CK-DETAMT-LEN   (WS-SUB1)     EL636
00825            MOVE AL-UNBON         TO CK-DETAMT-ATTRB (WS-SUB1)     EL636
00826            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL636
00827                                                                      CL*19
00828      IF CK-MAINT (WS-SUB1) = 'A' OR 'N' OR 'J' OR 'S'                CL*21
00829         IF CK-DETAMT-LEN (WS-SUB1) GREATER THAN ZEROS             EL636
00830            NEXT SENTENCE                                          EL636
00831         ELSE                                                      EL636
00832            MOVE ER-3158          TO EMI-ERROR                     EL636
00833            MOVE -1               TO CK-DETAMT-LEN   (WS-SUB1)     EL636
00834            MOVE AL-UABON         TO CK-DETAMT-ATTRB (WS-SUB1)     EL636
00835            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.              EL636
00836                                                                   EL636
00837      IF CK-MAINT (WS-SUB1)     = 'N'                                 CL*20
00838          GO TO 1025-EDIT-STUB-INFO.                                  CL*20
00839                                                                      CL*20
00840      IF CK-MAINT (WS-SUB1) = 'A' OR 'J' OR 'S'                       CL*21
00841          IF CK-AGENT-LEN (WS-SUB1) GREATER THAN ZERO                 CL*21
00842              IF CK-AGENT (WS-SUB1) = SPACES OR LOW-VALUES            CL*21
00843                  MOVE '*'        TO  WS-SPACE-AGENT-SW               CL*21
00844                  PERFORM 1480-VERIFY-GA-ONLY  THRU  1490-EXIT        CL*21
00845                  GO TO 1040-STUB-EDIT-COMPLETE                       CL*21
00846              ELSE                                                    CL*21
00847                  PERFORM 1400-VERIFY-AGENT  THRU  1470-EXIT          CL*21
00848                  GO TO 1040-STUB-EDIT-COMPLETE                       CL*21
00849          ELSE                                                        CL*20
00850                  PERFORM 1480-VERIFY-GA-ONLY  THRU   1490-EXIT       CL*21
00851                  GO TO 1040-STUB-EDIT-COMPLETE.                      CL*21
00852                                                                      CL*21
00853      IF CK-MAINT (WS-SUB1) = 'C' OR 'I' OR 'R'                       CL*21
00854          IF CK-AGENT-LEN (WS-SUB1) GREATER THAN ZERO                 CL*21
00855              IF CK-AGENT (WS-SUB1) = SPACES OR LOW-VALUES            CL*21
00856                  MOVE '*'        TO  WS-SPACE-AGENT-SW               CL*21
00857                  PERFORM 1480-VERIFY-GA-ONLY  THRU  1490-EXIT        CL*21
00858                  GO TO 1040-STUB-EDIT-COMPLETE                       CL*21
00859              ELSE                                                    CL*21
00860                  PERFORM 1400-VERIFY-AGENT  THRU  1470-EXIT          CL*21
00861                  GO TO 1040-STUB-EDIT-COMPLETE                       CL*21
00862          ELSE                                                        CL*21
00863              IF CK-AGOCD-LEN (WS-SUB1) GREATER THAN ZERO             CL*21
00864                  MOVE PI-STUB-ACCT (WS-SUB1)                         CL*21
00865                                  TO  CK-AGENT (WS-SUB1)              CL*21
00866                  IF PI-STUB-ACCT (WS-SUB1) = SPACES OR LOW-VALUES    CL*21
00867                      MOVE '*'    TO  WS-SPACE-AGENT-SW               CL*21
00868                      PERFORM 1480-VERIFY-GA-ONLY  THRU  1490-EXIT    CL*21
00869                      GO TO 1040-STUB-EDIT-COMPLETE                   CL*21
00870                  ELSE                                                CL*21
00871                      PERFORM 1400-VERIFY-AGENT  THRU  1470-EXIT      CL*21
00872                      GO TO 1040-STUB-EDIT-COMPLETE                   CL*21
00873      ELSE                                                            CL*17
00874          GO TO 1040-STUB-EDIT-COMPLETE.                              CL*21
00875                                                                      CL*21
00876  1040-STUB-EDIT-COMPLETE.                                            CL*21
00877                                                                      CL*21
00878      IF CK-MAINT (WS-SUB1) = 'J'                                     CL*21
00879          MOVE 'I'               TO CK-MAINT (WS-SUB1).               CL*21
00880                                                                      CL*21
00881      IF CK-MAINT (WS-SUB1) = 'S'                                     CL*21
00882          MOVE 'R'               TO CK-MAINT (WS-SUB1).               CL*21
00883                                                                      CL**7
00884      GO TO 1025-EDIT-STUB-INFO.                                   EL636
00885                                                                   EL636
00886  1050-EDIT-COMPLETE.                                              EL636
00887                                                                   EL636
00888      IF CSRL GREATER THAN ZERO                                       CL*13
00889          MOVE CSRI              TO PI-ERCKWK-CSR                     CL*13
00890      ELSE                                                            CL*13
00891          MOVE HOLD-CSR          TO PI-ERCKWK-CSR                     CL*13
00892                                    CSRI.                             CL*13
00893                                                                      CL*13
00894      IF EMI-FATAL-CTR NOT = ZEROS                                    CL**7
00895          GO TO 8200-SEND-DATAONLY.                                   CL**7
00896                                                                   EL636
00897      MOVE PI-COMPANY-CD          TO PI-ERCKWK-COMP-CD.            EL636
00898      MOVE CARI                   TO PI-ERCKWK-CARRIER.            EL636
00899      MOVE GROUPI                 TO PI-ERCKWK-GROUPING.           EL636
00900      MOVE PAYEEI                 TO PI-ERCKWK-PAYEE.              EL636
00901      MOVE WS-PAY-SEQ-NUM         TO PI-ERCKWK-PAYEE-SEQ.             CL**7
00902                                                                   EL636
00903      IF MAINTI = 'A'                                              EL636
00904         GO TO 2000-ADD-PAYEE.                                     EL636
00905                                                                   EL636
00906      IF MAINTI = 'C'                                              EL636
00907         GO TO 3000-CHANGE-PAYEE.                                  EL636
00908                                                                   EL636
00909      IF MAINTI = 'D'                                              EL636
00910         GO TO 4000-DELETE-PAYEE.                                  EL636
00911                                                                   EL636
00912      IF MAINTI = 'S'                                              EL636
00913         MOVE +0                  TO PI-ERCKWK-SEQ-NO              EL636
00914         GO TO 5000-DISPLAY-PAYEE.                                 EL636
00915                                                                      CL**4
00916      IF MAINTI = 'V'                                                 CL**4
00917         GO TO 7500-VOID-COMM-CHECK.                                  CL**4
00918                                                                   EL636
00919      EJECT                                                        EL636
00920                                                                   EL636
00921 ******************************************************************EL636
00922 *                                                                *EL636
00923 *      V E R I F Y   A G E N T  IS ON   C O M P.   M A S T E R   *   CL*11
00924 *                                                                *EL636
00925 ******************************************************************EL636
00926                                                                   EL636
00927  1400-VERIFY-AGENT.                                               EL636
00928                                                                   EL636
00929      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.               EL636
00930      MOVE CARI                   TO ERCOMP-CARRIER.               EL636
00931      MOVE GROUPI                 TO ERCOMP-GROUPING.              EL636
00932      MOVE PAYEEI                 TO ERCOMP-FIN-RESP.              EL636
00933                                                                      CL*21
00934      IF CK-AGOCD (WS-SUB1) = 'O'                                     CL*21
00935          GO TO 1430-SECOND-READ.                                     CL*21
00936                                                                      CL*21
00937      MOVE CK-AGENT (WS-SUB1)     TO ERCOMP-ACCT.                  EL636
00938      MOVE 'A'                    TO ERCOMP-RECORD-TYPE.           EL636
00939                                                                      CL**7
00940  1420-FIRST-READ.                                                    CL**7
00941                                                                      CL**7
00942      EXEC CICS HANDLE CONDITION                                      CL**7
00943          NOTFND   (1420-FIRST-NOTFND)                                CL*21
00944          END-EXEC.                                                   CL**7
00945                                                                      CL**7
00946      EXEC CICS READ                                                  CL**7
00947           DATASET   (FILE-ID-ERCOMP)                                 CL**7
00948           SET       (ADDRESS OF COMPENSATION-MASTER)                 CL*24
00949           RIDFLD    (ERCOMP-KEY)                                     CL**7
00950      END-EXEC.                                                       CL**7
00951                                                                      CL**7
00952      IF   CO-CSR-CODE  =  SPACES                                     CL*13
00953          MOVE LOW-VALUES         TO CO-CSR-CODE.                     CL*13
00954                                                                      CL*20
00955      MOVE CO-CSR-CODE            TO HOLD-CSR.                        CL*13
00956                                                                      CL*21
00957      IF CK-AGOCD (WS-SUB1) = 'A'                                     CL*21
00958          GO TO 1470-EXIT                                             CL*21
00959      ELSE                                                            CL*21
00960          GO TO 1430-SECOND-READ.                                     CL*21
00961                                                                      CL*21
00962  1420-FIRST-NOTFND.                                                  CL*21
00963                                                                      CL*21
00964      MOVE -1                     TO PAYEEL                           CL*21
00965      MOVE ER-2869                TO EMI-ERROR                        CL*21
00966      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                        CL*21
00967      GO TO 1470-EXIT.                                                CL*17
00968                                                                      CL**7
00969  1430-SECOND-READ.                                                   CL**7
00970                                                                      CL*20
00971      EXEC CICS HANDLE CONDITION                                      CL**7
00972          NOTFND   (1440-SECOND-NOTFND)                               CL**7
00973          END-EXEC.                                                   CL**7
00974                                                                      CL*11
00975      MOVE LOW-VALUES             TO ERCOMP-ACCT.                     CL*11
00976      MOVE 'G'                    TO ERCOMP-RECORD-TYPE.              CL*11
00977                                                                      CL**7
00978      EXEC CICS READ                                                  CL**7
00979           DATASET   (FILE-ID-ERCOMP)                                 CL**7
00980           SET       (ADDRESS OF COMPENSATION-MASTER)                 CL*24
00981           RIDFLD    (ERCOMP-KEY)                                     CL**7
00982      END-EXEC.                                                       CL**7
00983      IF   CO-CSR-CODE  =  SPACES                                     CL*13
00984          MOVE LOW-VALUES         TO CO-CSR-CODE.                     CL*13
00985                                                                      CL*20
00986      MOVE CO-CSR-CODE            TO HOLD-CSR.                        CL*13
00987      GO TO 1470-EXIT.                                                CL*17
00988                                                                      CL**7
00989  1440-SECOND-NOTFND.                                                 CL**7
00990                                                                      CL**7
00991      MOVE -1                     TO PAYEEL.                          CL*21
00992      MOVE ER-7043                TO EMI-ERROR.                       CL**7
00993      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
00994      GO TO 1470-EXIT.                                                CL*26
00995 ******************************************************************   CL*26
00996 *      V E R I F Y   C O M P.   M A S T E R    IS ACH            *   CL*26
00997 ******************************************************************   CL*26
00998 *                                                                *   CL*26
00999  1450-ACH-READ.                                                      CL*26
01000                                                                      CL*26
01001      EXEC CICS HANDLE CONDITION                                      CL*26
01002          NOTFND   (1440-SECOND-NOTFND)                               CL*26
01003          END-EXEC.                                                   CL*26
01004                                                                   EL636
01005      EXEC CICS READ                                                  CL*26
01006           DATASET   (FILE-ID-ERCOMP)                                 CL*26
01007           SET       (ADDRESS OF COMPENSATION-MASTER)                 CL*26
01008           RIDFLD    (ERCOMP-KEY)                                     CL*26
01009      END-EXEC.                                                       CL*26
01010                                                                      CL*26
01011 *COMPENSATION MASTER IS ACH PROCESSING WHEN CO-ACH-STATUS = 'A'      CL*26
01012      MOVE SPACE                  TO ERCOMP-SW.                       CL*26
01013      IF   CO-ACH-STATUS    = 'A'                                     CL*26
01014          MOVE 'P'                TO ERCOMP-SW.                       CL*26
01015                                                                      CL*26
01016      GO TO 1470-EXIT.                                                CL*26
01017  1470-EXIT.                                                          CL*17
01018      EXIT.                                                           CL*17
01019      EJECT                                                           CL*17
01020                                                                      CL*17
01021 ******************************************************************   CL*17
01022 *                                                                *   CL*17
01023 *      V E R I F Y   G A   ONLY IS ON   C O M P.   M A S T E R   *   CL*17
01024 *                                                                *   CL*17
01025 ******************************************************************   CL*17
01026  1480-VERIFY-GA-ONLY.                                                CL*17
01027                                                                      CL*17
01028      IF WS-SPACE-AGENT-SW   =  '*'                                   CL*17
01029          NEXT SENTENCE                                               CL*17
01030      ELSE                                                            CL*17
01031          IF CK-AGENT (WS-SUB1)  =  SPACES OR LOW-VALUES              CL*17
01032              NEXT SENTENCE                                           CL*17
01033          ELSE                                                        CL*17
01034              GO TO 1489-CLEAR.                                       CL*17
01035                                                                      CL*17
01036      IF CK-MAINT (WS-SUB1)  =  'N'                                   CL*17
01037          GO TO 1489-CLEAR.                                           CL*17
01038                                                                      CL*17
01039      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.                  CL*17
01040      MOVE CARI                   TO ERCOMP-CARRIER.                  CL*17
01041      MOVE GROUPI                 TO ERCOMP-GROUPING.                 CL*17
01042      MOVE PAYEEI                 TO ERCOMP-FIN-RESP.                 CL*17
01043      MOVE LOW-VALUES             TO ERCOMP-ACCT.                     CL*17
01044      MOVE 'G'                    TO ERCOMP-RECORD-TYPE.              CL*17
01045                                                                      CL*17
01046      EXEC CICS HANDLE CONDITION                                      CL*17
01047          NOTFND   (1485-NO-COMP-MSTR)                                CL*17
01048          END-EXEC.                                                   CL*17
01049                                                                      CL*17
01050      EXEC CICS READ                                                  CL*17
01051           DATASET   (FILE-ID-ERCOMP)                                 CL*17
01052           SET       (ADDRESS OF COMPENSATION-MASTER)                 CL*24
01053           RIDFLD    (ERCOMP-KEY)                                     CL*17
01054      END-EXEC.                                                       CL*17
01055                                                                      CL*17
01056      IF CO-AR-BAL-LEVEL  =  '4'                                      CL*17
01057         IF CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZEROS               CL*17
01058             MOVE -1             TO CK-AGENT-LEN (WS-SUB1)            CL*17
01059             MOVE ER-3164        TO EMI-ERROR                         CL*17
01060             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 CL*17
01061             GO TO 1489-CLEAR                                         CL*17
01062         ELSE                                                         CL*17
01063             GO TO 1489-CLEAR                                         CL*17
01064      ELSE                                                            CL*17
01065          IF CO-AR-BAL-LEVEL  =  '3'                                  CL*17
01066              MOVE -1             TO CK-AGENT-LEN (WS-SUB1)           CL*17
01067              MOVE ER-7082        TO EMI-ERROR                        CL*17
01068              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*17
01069              GO TO 1489-CLEAR                                        CL*21
01070             IF CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZEROS           CL*17
01071                 MOVE -1          TO CK-AGENT-LEN (WS-SUB1)           CL*17
01072                 MOVE ER-3164     TO EMI-ERROR                        CL*17
01073                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             CL*17
01074                 GO TO 1489-CLEAR                                     CL*17
01075             ELSE                                                     CL*17
01076                 GO TO 1489-CLEAR                                     CL*17
01077          ELSE                                                        CL*17
01078              IF CO-AR-BAL-LEVEL  =  '1'  OR '2'                      CL*17
01079                  MOVE -1         TO CK-AGENT-LEN (WS-SUB1)           CL*17
01080                  MOVE ER-7082    TO EMI-ERROR                        CL*17
01081                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL*17
01082                  GO TO 1489-CLEAR                                    CL*21
01083              ELSE                                                    CL*17
01084                  GO TO 1489-CLEAR.                                   CL*17
01085                                                                      CL*17
01086  1485-NO-COMP-MSTR.                                                  CL*17
01087                                                                      CL*17
01088      MOVE -1                     TO PAYEEL.                          CL*21
01089      MOVE ER-3179                TO EMI-ERROR.                       CL*17
01090      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*17
01091                                                                      CL*17
01092  1489-CLEAR.                                                         CL*17
01093      MOVE  SPACES                TO  WS-SPACE-AGENT-SW.              CL*17
01094                                                                      CL*17
01095  1490-EXIT.                                                       EL636
01096      EXIT.                                                        EL636
01097                                                                      CL*17
01098                                                                   EL636
01099      EJECT                                                        EL636
01100                                                                   EL636
01101 ******************************************************************EL636
01102 *                                                                *EL636
01103 *          V E R I F Y   L E D G E R   N U M B E R               *EL636
01104 *                                                                *EL636
01105 ******************************************************************EL636
01106                                                                   EL636
01107  1500-VERIFY-LEDGER.                                              EL636
01108  1590-EXIT.                                                       EL636
01109       EXIT.                                                       EL636
01110                                                                   EL636
01111     EJECT                                                         EL636
01112                                                                   EL636
01113  1600-READ-CNTL-FILE.                                                CL*13
01114 *****  NCL REQUIRES A CSR CODE WHICH IS VALIDATED TO THE  *****      CL*20
01115 *****  USER IDENTIFICATION FILE. CSR CODES CAN BE WITHOUT *****      CL*20
01116 *****  VALIDATION.                                        *****      CL*20
01117                                                                      CL*20
01118      IF   PI-COMPANY-ID  =  'NCL'                                    CL*13
01119          NEXT SENTENCE                                               CL*13
01120      ELSE                                                            CL*13
01121          GO TO 1690-EXIT.                                            CL*13
01122                                                                      CL*20
01123      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                   CL*13
01124      MOVE '2'                    TO  CNTL-REC-TYPE.                  CL*13
01125      MOVE CSRI                   TO  CNTL-ACCESS.                    CL*13
01126      MOVE ZEROS                  TO  CNTL-SEQ-NO.                    CL*13
01127                                                                      CL*20
01128      EXEC CICS HANDLE CONDITION                                      CL*13
01129          NOTFND  (1680-NOT-FOUND)                                    CL*13
01130          ERROR   (9990-ABEND)                                        CL*13
01131      END-EXEC.                                                       CL*13
01132                                                                      CL*20
01133      EXEC CICS READ                                                  CL*13
01134          DATASET  (CNTL-FILE-ID)                                     CL*13
01135          SET      (ADDRESS OF CONTROL-FILE)                          CL*24
01136          RIDFLD   (ELCNTL-KEY)                                       CL*13
01137      END-EXEC.                                                       CL*13
01138                                                                      CL*20
01139      GO TO 1690-EXIT.                                                CL*13
01140                                                                      CL*20
01141  1680-NOT-FOUND.                                                     CL*13
01142      MOVE ER-1883                TO EMI-ERROR.                       CL*13
01143      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*13
01144      MOVE -1                     TO CSRL.                            CL*13
01145      MOVE AL-UABON               TO CSRA.                            CL*13
01146                                                                      CL*13
01147  1690-EXIT.                                                          CL*13
01148       EXIT.                                                          CL*13
01149                                                                      CL*13
01150     EJECT                                                            CL*13
01151 ******************************************************************EL636
01152 *                                                                *EL636
01153 *               A D D   P A Y E E                                *EL636
01154 *                                                                *EL636
01155 ******************************************************************EL636
01156                                                                   EL636
01157  2000-ADD-PAYEE.                                                  EL636
01158                                                                   EL636
01159      EXEC CICS GETMAIN                                            EL636
01160          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01161          LENGTH  (ERCKWK-RECORD-LENGTH)                           EL636
01162          INITIMG (GETMAIN-SPACE)                                  EL636
01163      END-EXEC.                                                    EL636
01164                                                                   EL636
01165      IF PI-ERCKWK-PAYEE-SEQ = +1                                     CL**7
01166          ADD +1 TO PI-ERCKWK-PAYEE-SEQ                               CL**7
01167      ELSE                                                            CL**7
01168          MOVE +2 TO PI-ERCKWK-PAYEE-SEQ.                             CL**7
01169                                                                      CL**7
01170  2010-READ-FOR-UPDATE.                                               CL**7
01171      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.            EL636
01172      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.                      CL*13
01173      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.               EL636
01174      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.              EL636
01175      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.                 EL636
01176      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.                CL**7
01177      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.           EL636
01178                                                                   EL636
01179  2025-ADD-HEADER.                                                 EL636
01180                                                                   EL636
01181      MOVE 'CW'                   TO CW-RECORD-ID.                 EL636
01182      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.           EL636
01183      MOVE '0'                    TO CW-RECORD-TYPE.               EL636
01184      MOVE WS-CURRENT-BIN-DT      TO CW-RECORDED-DT.               EL636
01185      MOVE PI-PROCESSOR-ID        TO CW-RECORDED-BY.               EL636
01186      MOVE EIBTIME                TO CW-LAST-MAINT-HHMMSS.         EL636
01187                                                                   EL636
01188      IF PAYTOL  GREATER THAN ZEROS                                EL636
01189         MOVE PAYTOI              TO CW-PAYEE-NAME.                EL636
01190                                                                   EL636
01191      IF ADDRS1L GREATER THAN ZEROS                                EL636
01192         MOVE ADDRS1I             TO CW-ADDRESS-1.                 EL636
01193                                                                   EL636
01194      IF ADDRS2L GREATER THAN ZEROS                                EL636
01195         MOVE ADDRS2I             TO CW-ADDRESS-2.                 EL636
01196                                                                   EL636
01197      IF CITYSTL GREATER THAN ZEROS                                EL636
01198         MOVE CITYSTI             TO CW-PAYEE-CITY-ST.             EL636
01199                                                                   EL636
01200      IF ZIPL    GREATER THAN ZEROS                                EL636
01201         MOVE ZIPI                TO CW-PAYEE-ZIP.                 EL636
01202                                                                   EL636
01203      IF ZIPEXTL GREATER THAN ZEROS                                EL636
01204         MOVE ZIPEXTI             TO CW-PAYEE-ZIP-EXT.             EL636
01205                                                                   EL636
01206      MOVE +0                     TO CW-TOTAL-COMMISSION           EL636
01207                                     CW-TOTAL-ENTRIES.             EL636
01208                                                                   EL636
01209      MOVE LOW-VALUES             TO CW-CREDIT-SELECT-DT           EL636
01210                                     CW-CREDIT-ACCEPT-DT           EL636
01211                                     CW-RELEASE-DT.                EL636
01212                                                                   EL636
01213  2030-WRITE-HEADER.                                                  CL**7
01214      EXEC CICS HANDLE CONDITION                                   EL636
01215          DUPREC    (2060-DUPLICATE-CHECK)                            CL**7
01216      END-EXEC.                                                    EL636
01217                                                                   EL636
01218      EXEC CICS WRITE                                              EL636
01219           FROM    (CHECK-WORK-RECORDS)                            EL636
01220           DATASET (FILE-ID-ERCKWK)                                EL636
01221           RIDFLD  (ERCKWK-KEY)                                    EL636
01222      END-EXEC.                                                    EL636
01223                                                                   EL636
01224      MOVE +0                     TO WS-AMOUNT-PAID                EL636
01225                                     WS-SEQUENCE-NO                EL636
01226                                     WS-TOTAL-ENTRIES.             EL636
01227                                                                   EL636
01228      MOVE +0                     TO WS-SUB1.                      EL636
01229                                                                   EL636
01230  2050-ADD-DETAIL-ENTRIES.                                         EL636
01231                                                                   EL636
01232      ADD +1                      TO WS-SUB1.                      EL636
01233                                                                   EL636
01234      IF WS-SUB1 GREATER THAN +5                                   EL636
01235         GO TO 2090-NEW-PAYEE-PROCESSED.                           EL636
01236                                                                   EL636
01237      IF CK-MAINT-LEN (WS-SUB1) NOT GREATER THAN +0                EL636
01238         GO TO 2050-ADD-DETAIL-ENTRIES.                            EL636
01239                                                                   EL636
01240      MOVE SPACES                 TO CHECK-WORK-RECORDS.           EL636
01241      MOVE 'CW'                   TO CW-RECORD-ID.                 EL636
01242      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.           EL636
01243      ADD  +1                     TO WS-SEQUENCE-NO.               EL636
01244      MOVE WS-SEQUENCE-NO         TO CW-SEQUENCE-NO.               EL636
01245      MOVE CW-SEQUENCE-NO         TO ERCKWK-SEQUENCE-NO.           EL636
01246      MOVE '1'                    TO CW-RECORD-TYPE.               EL636
01247      MOVE WS-CURRENT-BIN-DT      TO CW-RECORDED-DT.               EL636
01248      MOVE PI-PROCESSOR-ID        TO CW-RECORDED-BY.               EL636
01249      MOVE EIBTIME                TO CW-LAST-MAINT-HHMMSS.         EL636
01250                                                                   EL636
01251      MOVE LOW-VALUES             TO CW-CREDIT-SELECT-DT           EL636
01252                                     CW-CREDIT-ACCEPT-DT           EL636
01253                                     CW-RELEASE-DT.                EL636
01254                                                                   EL636
01255      MOVE CK-MAINT (WS-SUB1)     TO CW-LAST-MAINT-APPLIED.           CL*11
01256                                                                      CL*11
01257      IF CK-COMNTS-LEN  (WS-SUB1) GREATER THAN ZEROS               EL636
01258         MOVE CK-COMNTS (WS-SUB1) TO CW-COMMENT.                   EL636
01259                                                                   EL636
01260      IF CK-AGENT-LEN   (WS-SUB1) GREATER THAN ZEROS               EL636
01261         MOVE CK-AGENT  (WS-SUB1) TO CW-ACCT-AGENT.                EL636
01262                                                                   EL636
01263      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS                  CL**2
01264         IF CK-MAINT    (WS-SUB1) = 'R'                               CL**2
01265            MOVE CK-INVREF (WS-SUB1) TO CW-REFERENCE.                 CL**2
01266                                                                      CL**2
01267      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS                  CL**2
01268         IF CK-MAINT    (WS-SUB1) = 'I'                               CL**2
01269            MOVE CK-INVREF (WS-SUB1) TO CW-INVOICE.                   CL**2
01270                                                                      CL**2
01271      IF CK-LEDGER-LEN  (WS-SUB1) GREATER THAN ZEROS               EL636
01272         MOVE CK-LEDGER (WS-SUB1) TO CW-LEDGER-NO.                 EL636
01273                                                                      CL**2
01274      IF CK-AGOCD-LEN   (WS-SUB1) GREATER THAN ZEROS                  CL*20
01275         MOVE CK-AGOCD  (WS-SUB1) TO CW-PMT-APPLIED.                  CL*20
01276                                                                      CL*20
01277      MOVE 'C'                    TO CW-PAYMENT-TYPE.                 CL**8
01278                                                                   EL636
01279      IF CK-DETAMT-LEN  (WS-SUB1) GREATER THAN ZEROS               EL636
01280         MOVE WS-DETAMT (WS-SUB1) TO CW-DETAIL-AMOUNT                 CL*11
01281      ELSE                                                            CL*11
01282         MOVE ZEROS               TO CW-DETAIL-AMOUNT.                CL*11
01283                                                                   EL636
01284      ADD +1                      TO WS-TOTAL-ENTRIES.             EL636
01285                                                                      CL*25
01286      IF CK-MAINT (WS-SUB1) NOT EQUAL 'H'                             CL*11
01287          ADD WS-DETAMT (WS-SUB1) TO WS-AMOUNT-PAID.                  CL*11
01288                                                                      CL*11
01289      IF CW-NON-AR-ITEM EQUAL 'Y'                                     CL*18
01290          NEXT SENTENCE                                               CL*11
01291      ELSE                                                            CL*11
01292         IF CK-MAINT (WS-SUB1) = 'N'                                  CL*11
01293             MOVE 'Y'             TO CW-NON-AR-ITEM                   CL*18
01294             MOVE 'N'             TO CW-PMT-APPLIED                   CL*20
01295         ELSE                                                         CL*11
01296             MOVE 'N'             TO CW-NON-AR-ITEM.                  CL*18
01297                                                                   EL636
01298      EXEC CICS HANDLE CONDITION                                   EL636
01299           DUPREC  (2080-DUPLICATE-CHECK)                          EL636
01300      END-EXEC.                                                    EL636
01301                                                                   EL636
01302      EXEC CICS WRITE                                              EL636
01303           FROM    (CHECK-WORK-RECORDS)                            EL636
01304           DATASET (FILE-ID-ERCKWK)                                EL636
01305           RIDFLD  (ERCKWK-KEY)                                    EL636
01306      END-EXEC.                                                    EL636
01307                                                                   EL636
01308      MOVE ERCKWK-KEY             TO  PI-STUB-KEY     (WS-SUB1).   EL636
01309      MOVE 'A'                    TO  PI-PREV-FUNCTION.               CL**7
01310                                                                   EL636
01311      GO TO 2050-ADD-DETAIL-ENTRIES.                               EL636
01312                                                                      CL**7
01313  2060-DUPLICATE-CHECK.                                               CL**7
01314      ADD +1 TO PI-ERCKWK-PAYEE-SEQ.                                  CL**7
01315      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.                CL**7
01316      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.              CL**7
01317      GO TO 2030-WRITE-HEADER.                                        CL**7
01318                                                                   EL636
01319  2080-DUPLICATE-CHECK.                                            EL636
01320                                                                   EL636
01321      EXEC CICS SYNCPOINT ROLLBACK                                 EL636
01322      END-EXEC.                                                    EL636
01323                                                                   EL636
01324      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL636
01325      MOVE ER-3159                TO EMI-ERROR.                    EL636
01326      MOVE -1                     TO CARL.                         EL636
01327      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
01328      GO TO 8200-SEND-DATAONLY.                                    EL636
01329                                                                   EL636
01330  2090-NEW-PAYEE-PROCESSED.                                        EL636
01331                                                                   EL636
01332      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO               CL**2
01333                                     PI-ERCKWK-SEQ-NO.                CL**2
01334                                                                   EL636
01335      EXEC CICS HANDLE CONDITION                                   EL636
01336          NOTFND    (2095-HEADER-ROLLBACK)                            CL**7
01337      END-EXEC.                                                    EL636
01338                                                                   EL636
01339      EXEC CICS READ                                               EL636
01340          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01341          DATASET (FILE-ID-ERCKWK)                                 EL636
01342          RIDFLD  (ERCKWK-KEY)                                     EL636
01343          UPDATE                                                   EL636
01344      END-EXEC.                                                    EL636
01345                                                                   EL636
01346      MOVE WS-AMOUNT-PAID          TO   CW-TOTAL-COMMISSION.       EL636
01347      MOVE WS-TOTAL-ENTRIES        TO   CW-TOTAL-ENTRIES.          EL636
01348                                                                   EL636
01349      IF CW-TOTAL-COMMISSION = ZERO                                   CL**7
01350          GO TO 2095-HEADER-ROLLBACK.                                 CL**7
01351                                                                      CL**7
01352      EXEC CICS REWRITE                                            EL636
01353           FROM    (CHECK-WORK-RECORDS)                            EL636
01354           DATASET (FILE-ID-ERCKWK)                                EL636
01355      END-EXEC.                                                    EL636
01356                                                                   EL636
01357      IF EMI-WARNING-CTR = ZEROS                                      CL**7
01358          MOVE ER-0000                TO  EMI-ERROR                   CL**7
01359          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**7
01360                                                                      CL**7
01361      GO TO 5000-DISPLAY-PAYEE.                                    EL636
01362                                                                   EL636
01363  2095-HEADER-ROLLBACK.                                               CL**7
01364                                                                   EL636
01365      EXEC CICS SYNCPOINT ROLLBACK                                 EL636
01366      END-EXEC.                                                    EL636
01367                                                                   EL636
01368      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL636
01369      MOVE ER-3160                TO EMI-ERROR.                    EL636
01370      MOVE -1                     TO CARL.                         EL636
01371      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
01372      GO TO 8200-SEND-DATAONLY.                                    EL636
01373                                                                   EL636
01374                                                                   EL636
01375      EJECT                                                        EL636
01376                                                                   EL636
01377 ******************************************************************EL636
01378 *                                                                *EL636
01379 *              C H A N G E   P A Y E E                           *EL636
01380 *                                                                *EL636
01381 ******************************************************************EL636
01382                                                                   EL636
01383  3000-CHANGE-PAYEE.                                               EL636
01384                                                                   EL636
01385      MOVE 'C'                    TO PI-MAINT-FUNCTION.            EL636
01386      MOVE +0                     TO WS-SUB1                          CL**8
01387                                     WS-DELETE-COUNT                  CL**8
01388                                     WS-DELETE-AMOUNT                 CL**8
01389                                     WS-DELETE-SEQ-NO                 CL*11
01390                                     WS-HOLD-COUNT                    CL*11
01391                                     WS-HOLD-AMOUNT                   CL*11
01392                                     WS-OFF-HOLD-COUNT                CL*11
01393                                     WS-OFF-HOLD-AMOUNT.              CL*11
01394      MOVE SPACE                  TO WS-UPDATE-SW.                 EL636
01395                                                                   EL636
01396  3010-CHANGE-ENTRIES.                                             EL636
01397                                                                   EL636
01398      ADD +1                      TO WS-SUB1.                      EL636
01399                                                                   EL636
01400      IF WS-SUB1 GREATER THAN +5                                   EL636
01401         IF  WS-DELETE-COUNT GREATER THAN +0                          CL**8
01402             PERFORM 3400-RESEQ-SLUG-LINES THRU 3499-EXIT             CL**8
01403             GO TO 3050-UPDATE-HEADER                                 CL**8
01404         ELSE                                                         CL**8
01405             GO TO 3050-UPDATE-HEADER.                                CL**8
01406                                                                   EL636
01407      IF CK-MAINT-LEN    (WS-SUB1) GREATER THAN ZEROS OR           EL636
01408         CK-COMNTS-LEN   (WS-SUB1) GREATER THAN ZEROS OR           EL636
01409         CK-AGENT-LEN    (WS-SUB1) GREATER THAN ZEROS OR           EL636
01410         CK-INVREF-LEN   (WS-SUB1) GREATER THAN ZEROS OR              CL**2
01411         CK-LEDGER-LEN   (WS-SUB1) GREATER THAN ZEROS OR           EL636
01412         CK-AGOCD-LEN    (WS-SUB1) GREATER THAN ZEROS OR              CL*20
01413         CK-DETAMT-LEN   (WS-SUB1) GREATER THAN ZEROS              EL636
01414         NEXT SENTENCE                                             EL636
01415      ELSE                                                         EL636
01416         GO TO 3010-CHANGE-ENTRIES.                                EL636
01417                                                                   EL636
01418      IF CK-MAINT-LEN (WS-SUB1) NOT GREATER THAN ZEROS             EL636
01419         GO TO 3010-CHANGE-ENTRIES.                                EL636
01420                                                                   EL636
01421      IF CK-MAINT (WS-SUB1) = 'A' OR 'N'                              CL**8
01422         PERFORM 3200-ADD-DETAIL-ENTRY THRU 3290-EXIT              EL636
01423         GO TO 3010-CHANGE-ENTRIES.                                EL636
01424                                                                      CL**2
01425      IF CK-MAINT (WS-SUB1) = 'R' OR 'I'                              CL**2
01426         IF WS-SUB1 GREATER THAN PI-NO-ENTRIES-DISPLAYED              CL**2
01427            PERFORM 3200-ADD-DETAIL-ENTRY THRU 3290-EXIT              CL**2
01428            GO TO 3010-CHANGE-ENTRIES.                                CL**2
01429                                                                   EL636
01430      IF WS-SUB1 GREATER THAN PI-NO-ENTRIES-DISPLAYED              EL636
01431         GO TO 3010-CHANGE-ENTRIES.                                EL636
01432                                                                   EL636
01433      MOVE PI-STUB-KEY (WS-SUB1)  TO ERCKWK-KEY.                   EL636
01434                                                                   EL636
01435      EXEC CICS HANDLE CONDITION                                   EL636
01436          NOTFND    (3010-CHANGE-ENTRIES)                          EL636
01437      END-EXEC.                                                    EL636
01438                                                                   EL636
01439      EXEC CICS READ                                               EL636
01440          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01441          DATASET (FILE-ID-ERCKWK)                                 EL636
01442          RIDFLD  (ERCKWK-KEY)                                     EL636
01443          UPDATE                                                   EL636
01444      END-EXEC.                                                    EL636
01445                                                                   EL636
01446      IF CW-RELEASE-DT GREATER THAN LOW-VALUES                        CL**8
01447          GO TO 3085-RELEASE-ERROR.                                   CL**8
01448                                                                      CL**8
01449      IF CK-MAINT  (WS-SUB1) = 'D'                                 EL636
01450         MOVE 'Y'                 TO WS-UPDATE-SW                     CL**8
01451         PERFORM 3100-DELETE-ENTRY THRU 3190-EXIT                  EL636
01452         GO TO 3010-CHANGE-ENTRIES.                                EL636
01453                                                                   EL636
01454      IF CK-MAINT (WS-SUB1) = 'H'                                     CL*11
01455         IF CW-LAST-MAINT-APPLIED EQUAL 'H'                           CL*11
01456             NEXT SENTENCE                                            CL*11
01457         ELSE                                                         CL*11
01458             MOVE 'Y'                 TO WS-UPDATE-SW                 CL*11
01459             ADD CW-DETAIL-AMOUNT     TO WS-HOLD-AMOUNT               CL*11
01460             ADD +1                   TO WS-HOLD-COUNT                CL*11
01461      ELSE                                                            CL*11
01462         IF CW-LAST-MAINT-APPLIED EQUAL 'H'                           CL*11
01463             MOVE 'Y'                 TO WS-UPDATE-SW                 CL*11
01464             ADD CW-DETAIL-AMOUNT     TO WS-OFF-HOLD-AMOUNT           CL*11
01465             ADD +1                   TO WS-OFF-HOLD-COUNT.           CL*11
01466                                                                      CL*11
01467      IF CK-MAINT (WS-SUB1) = 'C' AND                                 CL*26
01468         CW-LAST-MAINT-APPLIED = 'N'                                  CL*26
01469          MOVE -1              TO CK-MAINT-LEN (WS-SUB1)              CL*26
01470          MOVE ER-3198         TO EMI-ERROR                           CL*26
01471          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT                  CL*26
01472          GO TO 8200-SEND-DATAONLY.                                   CL*26
01473                                                                      CL*26
01474      MOVE CK-MAINT (WS-SUB1)     TO CW-LAST-MAINT-APPLIED.           CL*26
01475                                                                      CL*11
01476      IF (CK-COMNTS-LEN  (WS-SUB1) GREATER THAN ZEROS)                CL*11
01477         MOVE 'Y'                 TO WS-UPDATE-SW                  EL636
01478         MOVE CK-COMNTS (WS-SUB1) TO CW-COMMENT.                   EL636
01479                                                                   EL636
01480      IF CK-AGENT-LEN   (WS-SUB1) GREATER THAN ZEROS               EL636
01481         MOVE 'Y'                 TO WS-UPDATE-SW                  EL636
01482         MOVE CK-AGENT  (WS-SUB1) TO CW-ACCT-AGENT.                EL636
01483                                                                   EL636
01484      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS                  CL**2
01485         IF CK-MAINT    (WS-SUB1) = 'R'                               CL**2
01486            MOVE CK-INVREF (WS-SUB1) TO CW-REFERENCE.                 CL**2
01487                                                                      CL**2
01488      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS                  CL**2
01489         IF CK-MAINT    (WS-SUB1) = 'I'                               CL**2
01490            MOVE CK-INVREF (WS-SUB1) TO CW-INVOICE.                   CL**2
01491                                                                      CL**2
01492      IF CK-LEDGER-LEN  (WS-SUB1) GREATER THAN ZEROS               EL636
01493         MOVE CK-LEDGER (WS-SUB1) TO CW-LEDGER-NO.                 EL636
01494                                                                      CL**2
01495      IF CK-AGOCD-LEN   (WS-SUB1) GREATER THAN ZEROS                  CL*20
01496         MOVE CK-AGOCD  (WS-SUB1) TO CW-PMT-APPLIED.                  CL*20
01497                                                                      CL*20
01498      MOVE 'C'                    TO CW-PAYMENT-TYPE.                 CL**8
01499                                                                   EL636
01500      IF CK-DETAMT-LEN  (WS-SUB1) GREATER THAN ZEROS               EL636
01501         MOVE CW-DETAIL-AMOUNT    TO WS-DETAIL-AMOUNT              EL636
01502         MOVE WS-DETAMT (WS-SUB1) TO CW-DETAIL-AMOUNT.             EL636
01503                                                                   EL636
01504      EXEC CICS REWRITE                                            EL636
01505           FROM    (CHECK-WORK-RECORDS)                            EL636
01506           DATASET (FILE-ID-ERCKWK)                                EL636
01507      END-EXEC.                                                    EL636
01508                                                                   EL636
01509      IF CK-DETAMT-LEN  (WS-SUB1) GREATER THAN ZEROS               EL636
01510         PERFORM 3300-UPDATE-TOTAL-COMMISSION THRU 3390-EXIT.      EL636
01511                                                                   EL636
01512      GO TO 3010-CHANGE-ENTRIES.                                   EL636
01513                                                                   EL636
01514  3050-UPDATE-HEADER.                                              EL636
01515                                                                   EL636
01516      IF PAYTOL          GREATER THAN ZEROS OR                     EL636
01517         ADDRS1L         GREATER THAN ZEROS OR                     EL636
01518         ADDRS2L         GREATER THAN ZEROS OR                     EL636
01519         CITYSTL         GREATER THAN ZEROS OR                     EL636
01520         ZIPL            GREATER THAN ZEROS OR                     EL636
01521         ZIPEXTL         GREATER THAN ZEROS OR                     EL636
01522         WS-ENTRIES-UPDATED                                        EL636
01523         NEXT SENTENCE                                             EL636
01524      ELSE                                                         EL636
01525         GO TO 3080-CHANGES-PROCESSED.                             EL636
01526                                                                   EL636
01527      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.            EL636
01528      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.                      CL*13
01529      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.               EL636
01530      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.              EL636
01531      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.                 EL636
01532      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.                CL**7
01533      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.           EL636
01534                                                                   EL636
01535      EXEC CICS HANDLE CONDITION                                   EL636
01536          NOTFND    (3090-CHECKS-NOTFND)                           EL636
01537          ENDFILE   (3090-CHECKS-NOTFND)                           EL636
01538      END-EXEC.                                                    EL636
01539                                                                   EL636
01540      EXEC CICS READ                                               EL636
01541          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01542          DATASET (FILE-ID-ERCKWK)                                 EL636
01543          RIDFLD  (ERCKWK-KEY)                                     EL636
01544          UPDATE                                                   EL636
01545      END-EXEC.                                                    EL636
01546                                                                      CL*20
01547      IF CW-RELEASE-DT GREATER THAN LOW-VALUES                        CL*20
01548          GO TO 3085-RELEASE-ERROR.                                   CL*20
01549                                                                   EL636
01550      IF PAYTOL  GREATER THAN ZEROS                                EL636
01551         MOVE PAYTOI              TO CW-PAYEE-NAME.                EL636
01552                                                                   EL636
01553      IF ADDRS1L GREATER THAN ZEROS                                EL636
01554         MOVE ADDRS1I             TO CW-ADDRESS-1.                 EL636
01555                                                                   EL636
01556      IF ADDRS2L GREATER THAN ZEROS                                EL636
01557         MOVE ADDRS2I             TO CW-ADDRESS-2.                 EL636
01558                                                                   EL636
01559      IF CITYSTL GREATER THAN ZEROS                                EL636
01560         MOVE CITYSTI             TO CW-PAYEE-CITY-ST.             EL636
01561                                                                   EL636
01562      IF ZIPL    GREATER THAN ZEROS                                EL636
01563         MOVE ZIPI                TO CW-PAYEE-ZIP.                 EL636
01564                                                                   EL636
01565      IF ZIPEXTL GREATER THAN ZEROS                                EL636
01566         MOVE ZIPEXTI             TO CW-PAYEE-ZIP-EXT.             EL636
01567                                                                   EL636
01568      IF WS-DELETE-COUNT GREATER THAN +0                              CL**8
01569          SUBTRACT WS-DELETE-AMOUNT    FROM CW-TOTAL-COMMISSION       CL**8
01570          SUBTRACT WS-DELETE-COUNT     FROM CW-TOTAL-ENTRIES.         CL**8
01571                                                                      CL**8
01572      IF WS-HOLD-COUNT GREATER THAN +0                                CL*11
01573          SUBTRACT WS-HOLD-AMOUNT      FROM CW-TOTAL-COMMISSION.      CL*11
01574                                                                      CL*11
01575      IF WS-OFF-HOLD-COUNT GREATER THAN +0                            CL*11
01576          ADD WS-OFF-HOLD-AMOUNT  TO  CW-TOTAL-COMMISSION.            CL*11
01577                                                                      CL*11
01578                                                                      CL**8
01579      MOVE WS-CURRENT-BIN-DT      TO  CW-RECORDED-DT.              EL636
01580      MOVE PI-PROCESSOR-ID        TO  CW-RECORDED-BY.              EL636
01581      MOVE EIBTIME                TO  CW-LAST-MAINT-HHMMSS.        EL636
01582                                                                   EL636
01583      EXEC CICS REWRITE                                            EL636
01584           FROM    (CHECK-WORK-RECORDS)                            EL636
01585           DATASET (FILE-ID-ERCKWK)                                EL636
01586      END-EXEC.                                                    EL636
01587                                                                   EL636
01588  3080-CHANGES-PROCESSED.                                          EL636
01589                                                                      CL**2
01590      IF PI-NO-ENTRIES-DISPLAYED GREATER THAN ZEROS                   CL**2
01591          MOVE PI-STUB-KEY (1)        TO PI-ERCKWK-KEY                CL**8
01592          IF WS-DELETE-COUNT  GREATER THAN +0   AND                   CL**8
01593             WS-DELETE-COUNT  EQUAL PI-NO-ENTRIES-DISPLAYED           CL**8
01594             IF  CW-TOTAL-ENTRIES GREATER THAN +5 AND                 CL**8
01595                 PI-ERCKWK-SEQ-NO GREATER THAN +5                     CL**8
01596                 SUBTRACT +5 FROM PI-ERCKWK-SEQ-NO                    CL**8
01597             ELSE                                                     CL**8
01598                 MOVE ZEROS TO    PI-ERCKWK-SEQ-NO.                   CL**8
01599                                                                   EL636
01600      IF EMI-WARNING-CTR = ZEROS                                      CL**7
01601          MOVE ER-0000                TO EMI-ERROR                    CL**7
01602          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**7
01603                                                                      CL**7
01604      GO TO 5000-DISPLAY-PAYEE.                                    EL636
01605                                                                   EL636
01606  3085-RELEASE-ERROR.                                                 CL**8
01607                                                                      CL**8
01608      EXEC CICS UNLOCK                                                CL**8
01609           DATASET (FILE-ID-ERCKWK)                                   CL**8
01610      END-EXEC.                                                       CL**8
01611                                                                      CL**8
01612      MOVE ER-3139                TO EMI-ERROR.                       CL**8
01613      MOVE -1                     TO MAINTL.                          CL**8
01614      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**8
01615      GO TO 8200-SEND-DATAONLY.                                       CL**8
01616  3090-CHECKS-NOTFND.                                              EL636
01617                                                                   EL636
01618      EXEC CICS SYNCPOINT ROLLBACK                                 EL636
01619      END-EXEC.                                                    EL636
01620                                                                   EL636
01621      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL636
01622      MOVE ER-3161                TO EMI-ERROR.                    EL636
01623      MOVE -1                     TO CARL.                         EL636
01624      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
01625      GO TO 8200-SEND-DATAONLY.                                    EL636
01626                                                                   EL636
01627      EJECT                                                        EL636
01628                                                                   EL636
01629 ******************************************************************EL636
01630 *                                                                *EL636
01631 *              D E L E T E   E N T R Y                           *EL636
01632 *                                                                *EL636
01633 ******************************************************************EL636
01634                                                                   EL636
01635  3100-DELETE-ENTRY.                                               EL636
01636                                                                   EL636
01637      IF CW-LAST-MAINT-APPLIED NOT = 'H'                              CL*25
01638          ADD  CW-DETAIL-AMOUNT   TO WS-DELETE-AMOUNT.                CL*25
01639                                                                      CL*25
01640      ADD  +1                     TO WS-DELETE-COUNT.                 CL**8
01641                                                                      CL**8
01642      IF  WS-DELETE-COUNT EQUAL +1                                    CL**8
01643          MOVE ERCKWK-SEQUENCE-NO TO WS-DELETE-SEQ-NO.                CL**8
01644                                                                   EL636
01645      EXEC CICS DELETE                                             EL636
01646           DATASET   (FILE-ID-ERCKWK)                              EL636
01647      END-EXEC.                                                    EL636
01648                                                                   EL636
01649  3190-EXIT.                                                       EL636
01650      EXIT.                                                        EL636
01651                                                                   EL636
01652      EJECT                                                        EL636
01653                                                                   EL636
01654 ******************************************************************EL636
01655 *                                                                *EL636
01656 *           A D D   D E T A I L   E N T R Y                      *EL636
01657 *                                                                *EL636
01658 ******************************************************************EL636
01659                                                                   EL636
01660  3200-ADD-DETAIL-ENTRY.                                           EL636
01661                                                                   EL636
01662      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.            EL636
01663      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.                      CL*13
01664      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.               EL636
01665      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.              EL636
01666      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.                 EL636
01667      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.                CL**7
01668      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.           EL636
01669                                                                   EL636
01670      EXEC CICS HANDLE CONDITION                                   EL636
01671          NOTFND    (3280-HEADER-NOTFND)                           EL636
01672      END-EXEC.                                                    EL636
01673                                                                   EL636
01674      EXEC CICS READ                                               EL636
01675          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01676          DATASET (FILE-ID-ERCKWK)                                 EL636
01677          RIDFLD  (ERCKWK-KEY)                                     EL636
01678          UPDATE                                                   EL636
01679      END-EXEC.                                                    EL636
01680                                                                      CL**8
01681      IF CW-RELEASE-DT GREATER THAN LOW-VALUES                        CL**8
01682          GO TO 3285-RELEASE-ERROR.                                   CL**8
01683                                                                   EL636
01684      ADD WS-DETAMT (WS-SUB1)     TO CW-TOTAL-COMMISSION.          EL636
01685      ADD +1                      TO CW-TOTAL-ENTRIES.             EL636
01686      MOVE CW-TOTAL-ENTRIES       TO WS-TOTAL-ENTRIES.             EL636
01687                                                                   EL636
01688      MOVE WS-CURRENT-BIN-DT      TO  CW-RECORDED-DT.              EL636
01689      MOVE PI-PROCESSOR-ID        TO  CW-RECORDED-BY.              EL636
01690      MOVE EIBTIME                TO  CW-LAST-MAINT-HHMMSS.        EL636
01691                                                                   EL636
01692      EXEC CICS REWRITE                                            EL636
01693           FROM    (CHECK-WORK-RECORDS)                            EL636
01694           DATASET (FILE-ID-ERCKWK)                                EL636
01695      END-EXEC.                                                    EL636
01696                                                                   EL636
01697      EXEC CICS GETMAIN                                            EL636
01698          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01699          LENGTH  (ERCKWK-RECORD-LENGTH)                           EL636
01700          INITIMG (GETMAIN-SPACE)                                  EL636
01701      END-EXEC.                                                    EL636
01702                                                                   EL636
01703      MOVE 'CW'                   TO CW-RECORD-ID.                 EL636
01704      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.           EL636
01705                                                                   EL636
01706      MOVE WS-TOTAL-ENTRIES       TO CW-SEQUENCE-NO                EL636
01707                                     ERCKWK-SEQUENCE-NO.           EL636
01708                                                                   EL636
01709      MOVE '1'                    TO CW-RECORD-TYPE.               EL636
01710      MOVE WS-CURRENT-BIN-DT      TO CW-RECORDED-DT.               EL636
01711      MOVE PI-PROCESSOR-ID        TO CW-RECORDED-BY.               EL636
01712      MOVE EIBTIME                TO CW-LAST-MAINT-HHMMSS.         EL636
01713                                                                   EL636
01714      MOVE CK-MAINT     (WS-SUB1) TO CW-LAST-MAINT-APPLIED.           CL*11
01715                                                                      CL*11
01716      IF CK-COMNTS-LEN  (WS-SUB1) GREATER THAN ZEROS               EL636
01717         MOVE CK-COMNTS (WS-SUB1) TO CW-COMMENT.                   EL636
01718                                                                   EL636
01719      IF CK-AGENT-LEN   (WS-SUB1) GREATER THAN ZEROS               EL636
01720         MOVE CK-AGENT  (WS-SUB1) TO CW-ACCT-AGENT.                EL636
01721                                                                   EL636
01722      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS                  CL**2
01723         IF CK-MAINT    (WS-SUB1) = 'R'                               CL**2
01724            MOVE CK-INVREF (WS-SUB1) TO CW-REFERENCE.                 CL**2
01725                                                                      CL**2
01726      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS                  CL**2
01727         IF CK-MAINT    (WS-SUB1) = 'I'                               CL**2
01728            MOVE CK-INVREF (WS-SUB1) TO CW-INVOICE.                   CL**2
01729                                                                      CL**2
01730      IF CK-LEDGER-LEN  (WS-SUB1) GREATER THAN ZEROS               EL636
01731         MOVE CK-LEDGER (WS-SUB1) TO CW-LEDGER-NO.                 EL636
01732                                                                      CL*20
01733      IF CK-AGOCD-LEN   (WS-SUB1) GREATER THAN ZEROS                  CL*20
01734         MOVE CK-AGOCD  (WS-SUB1) TO CW-PMT-APPLIED.                  CL*20
01735                                                                      CL**2
01736      MOVE 'C'                    TO CW-PAYMENT-TYPE.                 CL**8
01737                                                                   EL636
01738      IF CK-DETAMT-LEN  (WS-SUB1) GREATER THAN ZEROS               EL636
01739         MOVE WS-DETAMT (WS-SUB1) TO CW-DETAIL-AMOUNT.             EL636
01740                                                                   EL636
01741      MOVE LOW-VALUES             TO CW-CREDIT-SELECT-DT           EL636
01742                                     CW-CREDIT-ACCEPT-DT           EL636
01743                                     CW-RELEASE-DT.                EL636
01744                                                                      CL*11
01745      IF CW-NON-AR-ITEM EQUAL 'Y'                                     CL*18
01746          NEXT SENTENCE                                               CL*11
01747      ELSE                                                            CL*11
01748         IF CK-MAINT (WS-SUB1) = 'N'                                  CL*11
01749             MOVE 'Y'                TO CW-NON-AR-ITEM                CL*18
01750             MOVE 'N'                TO CW-PMT-APPLIED                CL*26
01751         ELSE                                                         CL*11
01752             MOVE 'N'                TO CW-NON-AR-ITEM.               CL*18
01753                                                                   EL636
01754      EXEC CICS HANDLE CONDITION                                   EL636
01755           DUPREC  (3270-DUP-ENTRY)                                EL636
01756      END-EXEC.                                                    EL636
01757                                                                   EL636
01758  3250-WRITE-DETAIL-ENTRY.                                         EL636
01759                                                                   EL636
01760      EXEC CICS WRITE                                              EL636
01761           FROM    (CHECK-WORK-RECORDS)                            EL636
01762           DATASET (FILE-ID-ERCKWK)                                EL636
01763           RIDFLD  (ERCKWK-KEY)                                    EL636
01764      END-EXEC.                                                    EL636
01765                                                                      CL**7
01766      MOVE 'A'                   TO PI-PREV-FUNCTION.                 CL**7
01767                                                                   EL636
01768      GO TO 3290-EXIT.                                             EL636
01769                                                                   EL636
01770  3270-DUP-ENTRY.                                                  EL636
01771                                                                   EL636
01772      ADD +1                      TO CW-SEQUENCE-NO.               EL636
01773      MOVE CW-SEQUENCE-NO         TO ERCKWK-SEQUENCE-NO.           EL636
01774      GO TO 3250-WRITE-DETAIL-ENTRY.                               EL636
01775                                                                   EL636
01776  3280-HEADER-NOTFND.                                              EL636
01777                                                                   EL636
01778      EXEC CICS SYNCPOINT ROLLBACK                                 EL636
01779      END-EXEC.                                                    EL636
01780                                                                   EL636
01781      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL636
01782      MOVE ER-3161                TO EMI-ERROR.                    EL636
01783      MOVE -1                     TO CARL.                         EL636
01784      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**8
01785      GO TO 8200-SEND-DATAONLY.                                       CL**8
01786                                                                      CL**8
01787  3285-RELEASE-ERROR.                                                 CL**8
01788                                                                      CL**8
01789      EXEC CICS UNLOCK                                                CL**8
01790           DATASET (FILE-ID-ERCKWK)                                   CL**8
01791      END-EXEC.                                                       CL**8
01792                                                                      CL**8
01793      MOVE ER-3139                TO EMI-ERROR.                       CL**8
01794      MOVE -1                     TO MAINTL.                          CL**8
01795      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
01796      GO TO 8200-SEND-DATAONLY.                                    EL636
01797                                                                   EL636
01798  3290-EXIT.                                                       EL636
01799      EXIT.                                                        EL636
01800                                                                   EL636
01801      EJECT                                                        EL636
01802                                                                   EL636
01803 ******************************************************************EL636
01804 *                                                                *EL636
01805 *         U P D A T E   T O T A L   C O M M I S S I O N          *EL636
01806 *                                                                *EL636
01807 ******************************************************************EL636
01808                                                                   EL636
01809  3300-UPDATE-TOTAL-COMMISSION.                                    EL636
01810                                                                   EL636
01811      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.            EL636
01812      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.                      CL*13
01813      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.               EL636
01814      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.              EL636
01815      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.                 EL636
01816      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.                CL**7
01817      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.           EL636
01818                                                                   EL636
01819      EXEC CICS HANDLE CONDITION                                   EL636
01820          NOTFND    (3380-HEADER-NOTFND)                           EL636
01821      END-EXEC.                                                    EL636
01822                                                                   EL636
01823      EXEC CICS READ                                               EL636
01824          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01825          DATASET (FILE-ID-ERCKWK)                                 EL636
01826          RIDFLD  (ERCKWK-KEY)                                     EL636
01827          UPDATE                                                   EL636
01828      END-EXEC.                                                    EL636
01829                                                                   EL636
01830      SUBTRACT WS-DETAIL-AMOUNT    FROM CW-TOTAL-COMMISSION.       EL636
01831      ADD      WS-DETAMT (WS-SUB1) TO   CW-TOTAL-COMMISSION.       EL636
01832                                                                   EL636
01833      MOVE WS-CURRENT-BIN-DT      TO  CW-RECORDED-DT.              EL636
01834      MOVE PI-PROCESSOR-ID        TO  CW-RECORDED-BY.              EL636
01835      MOVE EIBTIME                TO  CW-LAST-MAINT-HHMMSS.        EL636
01836                                                                   EL636
01837      EXEC CICS REWRITE                                            EL636
01838           FROM    (CHECK-WORK-RECORDS)                            EL636
01839           DATASET (FILE-ID-ERCKWK)                                EL636
01840      END-EXEC.                                                    EL636
01841                                                                   EL636
01842      GO TO 3390-EXIT.                                             EL636
01843                                                                   EL636
01844  3380-HEADER-NOTFND.                                              EL636
01845                                                                   EL636
01846      EXEC CICS SYNCPOINT ROLLBACK                                 EL636
01847      END-EXEC.                                                    EL636
01848                                                                   EL636
01849      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL636
01850      MOVE ER-3161                TO EMI-ERROR.                    EL636
01851      MOVE -1                     TO CARL.                         EL636
01852      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
01853      GO TO 8200-SEND-DATAONLY.                                    EL636
01854                                                                   EL636
01855  3390-EXIT.                                                       EL636
01856      EXIT.                                                           CL**8
01857      EJECT                                                           CL**8
01858 ******************************************************************   CL**8
01859 *                                                                *   CL**8
01860 *             R E S E Q U E N C E   S L U G   L I N E S          *   CL**8
01861 *                                                                *   CL**8
01862 ******************************************************************   CL**8
01863                                                                      CL**8
01864  3400-RESEQ-SLUG-LINES.                                              CL**8
01865                                                                      CL**8
01866      EXEC CICS GETMAIN                                               CL*11
01867          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01868          LENGTH  (ERCKWK-RECORD-LENGTH)                              CL*11
01869          INITIMG (GETMAIN-SPACE)                                     CL*11
01870      END-EXEC.                                                       CL*11
01871                                                                      CL*11
01872      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.             CL**8
01873      MOVE WS-DELETE-SEQ-NO       TO ERCKWK-SEQUENCE-NO               CL**8
01874                                     WS-SEQUENCE-NO.                  CL**8
01875                                                                      CL**8
01876      EXEC CICS HANDLE CONDITION                                      CL**8
01877          NOTFND    (3490-FINISHED)                                   CL**8
01878          ENDFILE   (3490-FINISHED)                                   CL**8
01879      END-EXEC.                                                       CL**8
01880                                                                      CL**8
01881  3410-BROWSE-CHECK-WORK-FILE.                                        CL**8
01882                                                                      CL**8
01883      EXEC CICS STARTBR                                               CL**8
01884          DATASET (FILE-ID-ERCKWK)                                    CL**8
01885          RIDFLD  (ERCKWK-KEY)                                        CL**8
01886      END-EXEC.                                                       CL**8
01887                                                                      CL**8
01888  3420-READ-CHECK-WORK-FILE.                                          CL**8
01889                                                                      CL**8
01890      EXEC CICS READNEXT                                              CL**8
01891          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01892          DATASET (FILE-ID-ERCKWK)                                    CL**8
01893          RIDFLD  (ERCKWK-KEY)                                        CL**8
01894      END-EXEC.                                                       CL**8
01895                                                                      CL**8
01896      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL*13
01897          GO TO      3490-FINISHED.                                   CL*13
01898                                                                      CL*13
01899      IF CW-COMPANY-CD = SVCKWK-COMPANY-CD AND                        CL**8
01900         CW-CARRIER    = SVCKWK-CARRIER    AND                        CL**8
01901         CW-CSR        = SVCKWK-CSR        AND                        CL*13
01902         CW-GROUPING   = SVCKWK-GROUPING   AND                        CL**8
01903         CW-PAYEE      = SVCKWK-PAYEE      AND                        CL**8
01904         CW-PAYEE-SEQ  = SVCKWK-PAYEE-SEQ                             CL**8
01905         NEXT SENTENCE                                                CL**8
01906      ELSE                                                            CL**8
01907         GO TO 3490-FINISHED.                                         CL**8
01908                                                                      CL**8
01909      IF CW-HEADER OR CW-TEXT                                         CL*11
01910          GO TO 3420-READ-CHECK-WORK-FILE.                            CL**8
01911                                                                      CL**8
01912      EXEC CICS ENDBR                                                 CL**8
01913           DATASET (FILE-ID-ERCKWK)                                   CL**8
01914      END-EXEC.                                                       CL**8
01915                                                                      CL**8
01916      EXEC CICS READ                                                  CL**8
01917          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01918          DATASET (FILE-ID-ERCKWK)                                    CL**8
01919          RIDFLD  (ERCKWK-KEY)                                        CL**8
01920          UPDATE                                                      CL**8
01921      END-EXEC.                                                       CL**8
01922                                                                      CL**8
01923      MOVE CHECK-WORK-RECORDS     TO SAVE-ERCKWK-RECORD.              CL*21
01924                                                                      CL*21
01925      EXEC CICS DELETE                                                CL**8
01926           DATASET (FILE-ID-ERCKWK)                                   CL**8
01927      END-EXEC.                                                       CL**8
01928                                                                      CL**8
01929      MOVE SAVE-ERCKWK-RECORD     TO CHECK-WORK-RECORDS.              CL*21
01930                                                                      CL*21
01931      MOVE WS-SEQUENCE-NO         TO SAVE-CKWK-SEQ-NO                 CL*21
01932                                     ERCKWK-SEQUENCE-NO.              CL**8
01933                                                                      CL**8
01934      EXEC CICS HANDLE CONDITION                                      CL**8
01935           DUPREC  (9990-ABEND)                                       CL**8
01936      END-EXEC.                                                       CL**8
01937                                                                      CL**8
01938      EXEC CICS WRITE                                                 CL**8
01939           FROM    (SAVE-ERCKWK-RECORD)                               CL*21
01940           DATASET (FILE-ID-ERCKWK)                                   CL**8
01941           RIDFLD  (ERCKWK-KEY)                                       CL**8
01942      END-EXEC.                                                       CL**8
01943                                                                      CL**8
01944      ADD +1                      TO WS-SEQUENCE-NO                   CL**8
01945                                     ERCKWK-SEQUENCE-NO.              CL**8
01946      GO TO 3410-BROWSE-CHECK-WORK-FILE.                              CL**8
01947                                                                      CL**8
01948  3490-FINISHED.                                                      CL**8
01949                                                                      CL**8
01950      MOVE SAVE-ERCKWK-CONTROL  TO ERCKWK-KEY.                        CL**8
01951                                                                      CL**8
01952      EXEC CICS ENDBR                                                 CL**8
01953           DATASET (FILE-ID-ERCKWK)                                   CL**8
01954      END-EXEC.                                                       CL**8
01955                                                                      CL**8
01956  3499-EXIT.                                                          CL**8
01957      EXIT.                                                        EL636
01958                                                                   EL636
01959      EJECT                                                        EL636
01960 ******************************************************************EL636
01961 *                                                                *EL636
01962 *             D E L E T E   P A Y E E                            *EL636
01963 *                                                                *EL636
01964 ******************************************************************EL636
01965                                                                   EL636
01966  4000-DELETE-PAYEE.                                               EL636
01967                                                                   EL636
01968      MOVE 'D'                    TO PI-MAINT-FUNCTION.            EL636
01969                                                                   EL636
01970      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.            EL636
01971      MOVE CSRI                   TO ERCKWK-CSR.                      CL*13
01972      MOVE CARI                   TO ERCKWK-CARRIER.               EL636
01973      MOVE GROUPI                 TO ERCKWK-GROUPING.              EL636
01974      MOVE PAYEEI                 TO ERCKWK-PAYEE.                 EL636
01975      MOVE PAYSEQI                TO ERCKWK-PAYEE-SEQ.                CL**7
01976      MOVE +0                     TO ERCKWK-SEQUENCE-NO.           EL636
01977                                                                   EL636
01978      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.          EL636
01979                                                                   EL636
01980      EXEC CICS HANDLE CONDITION                                   EL636
01981          NOTFND    (4080-PAYEE-NOTFND)                            EL636
01982          ENDFILE   (4070-PAYEE-DELETED)                           EL636
01983      END-EXEC.                                                    EL636
01984                                                                   EL636
01985  4010-READ-CHECK-WORK-FILE.                                       EL636
01986                                                                   EL636
01987      EXEC CICS STARTBR                                            EL636
01988          DATASET (FILE-ID-ERCKWK)                                 EL636
01989          RIDFLD  (ERCKWK-KEY)                                     EL636
01990      END-EXEC.                                                    EL636
01991                                                                   EL636
01992      EXEC CICS READNEXT                                           EL636
01993          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
01994          DATASET (FILE-ID-ERCKWK)                                 EL636
01995          RIDFLD  (ERCKWK-KEY)                                     EL636
01996      END-EXEC.                                                    EL636
01997                                                                   EL636
01998      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL*13
01999          GO TO      4070-PAYEE-DELETED.                              CL*13
02000                                                                      CL*13
02001      IF CW-COMPANY-CD = SVCKWK-COMPANY-CD AND                     EL636
02002         CW-CSR        = SVCKWK-CSR        AND                        CL*13
02003         CW-CARRIER    = SVCKWK-CARRIER    AND                     EL636
02004         CW-GROUPING   = SVCKWK-GROUPING   AND                     EL636
02005         CW-PAYEE      = SVCKWK-PAYEE      AND                        CL**7
02006         CW-PAYEE-SEQ  = SVCKWK-PAYEE-SEQ                             CL**7
02007         NEXT SENTENCE                                             EL636
02008      ELSE                                                         EL636
02009         GO TO 4070-PAYEE-DELETED.                                 EL636
02010                                                                      CL*20
02011      IF CW-RELEASE-DT GREATER THAN LOW-VALUES                        CL*20
02012          GO TO 4090-PAYEE-RELEASED.                                  CL*20
02013                                                                   EL636
02014      EXEC CICS ENDBR                                              EL636
02015           DATASET (FILE-ID-ERCKWK)                                EL636
02016      END-EXEC.                                                    EL636
02017                                                                   EL636
02018      EXEC CICS READ                                               EL636
02019          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02020          DATASET (FILE-ID-ERCKWK)                                 EL636
02021          RIDFLD  (ERCKWK-KEY)                                     EL636
02022          UPDATE                                                   EL636
02023      END-EXEC.                                                    EL636
02024                                                                   EL636
02025      EXEC CICS DELETE                                             EL636
02026           DATASET (FILE-ID-ERCKWK)                                EL636
02027      END-EXEC.                                                    EL636
02028                                                                   EL636
02029                                                                   EL636
02030      GO TO 4010-READ-CHECK-WORK-FILE.                             EL636
02031                                                                   EL636
02032  4070-PAYEE-DELETED.                                              EL636
02033                                                                   EL636
02034      MOVE LOW-VALUES             TO  EL636AI.                     EL636
02035      MOVE SVCKWK-CSR             TO  CSRO.                           CL*13
02036      MOVE SVCKWK-CARRIER         TO  CARO.                        EL636
02037      MOVE SVCKWK-GROUPING        TO  GROUPO.                      EL636
02038      MOVE SVCKWK-PAYEE           TO  PAYEEO.                      EL636
02039      MOVE SVCKWK-PAYEE-SEQ       TO  PAYSEQO.                        CL**7
02040                                                                   EL636
02041      MOVE AL-UANON               TO  CSRA                            CL*13
02042                                      CARA                            CL*13
02043                                      GROUPA                       EL636
02044                                      PAYEEA                          CL**7
02045                                      PAYSEQA.                        CL**7
02046                                                                   EL636
02047      MOVE ER-0000                TO  EMI-ERROR.                   EL636
02048      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02049      MOVE -1                     TO  MAINTL.                      EL636
02050      GO TO 8100-SEND-INITIAL-MAP.                                 EL636
02051                                                                   EL636
02052  4080-PAYEE-NOTFND.                                               EL636
02053                                                                   EL636
02054      MOVE ER-3160                TO EMI-ERROR.                    EL636
02055      MOVE -1                     TO CARL.                         EL636
02056      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*20
02057      GO TO 8200-SEND-DATAONLY.                                       CL*20
02058                                                                      CL*20
02059  4090-PAYEE-RELEASED.                                                CL*20
02060                                                                      CL*20
02061      EXEC CICS UNLOCK                                                CL*20
02062           DATASET (FILE-ID-ERCKWK)                                   CL*20
02063      END-EXEC.                                                       CL*20
02064                                                                      CL*20
02065      MOVE ER-3139                TO EMI-ERROR.                       CL*20
02066      MOVE -1                     TO MAINTL.                          CL*20
02067      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02068      GO TO 8200-SEND-DATAONLY.                                    EL636
02069                                                                   EL636
02070      EJECT                                                        EL636
02071                                                                   EL636
02072 ******************************************************************EL636
02073 *                                                                *EL636
02074 *               D I S P L A Y   P A Y E E                        *EL636
02075 *                                                                *EL636
02076 ******************************************************************EL636
02077                                                                   EL636
02078  5000-DISPLAY-PAYEE.                                              EL636
02079                                                                   EL636
02080      MOVE 'Y' TO WS-FIRST-DETAIL-SW.                                 CL**8
02081                                                                      CL**8
02082      MOVE PI-ERCKWK-COMP-CD      TO ERCKWK-COMPANY-CD.            EL636
02083      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.                      CL*13
02084      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.               EL636
02085      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.              EL636
02086      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.                 EL636
02087      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.                CL**7
02088      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.           EL636
02089                                                                   EL636
02090      IF EIBAID = DFHPF3                                           EL636
02091          MOVE PI-LAST-STUB-KEY    TO ERCKWK-KEY                      CL**8
02092                                      PI-ERCKWK-KEY                   CL**8
02093          MOVE ZEROS               TO ERCKWK-SEQUENCE-NO              CL**8
02094                                      PI-ERCKWK-SEQ-NO                CL**8
02095          MOVE PI-LAST-STUB-SEQ-NO TO WS-NOFIRST.                     CL**8
02096                                                                   EL636
02097      EXEC CICS HANDLE CONDITION                                   EL636
02098          NOTFND    (5090-CHECKS-NOTFND)                           EL636
02099          ENDFILE   (5090-CHECKS-NOTFND)                           EL636
02100      END-EXEC.                                                    EL636
02101                                                                   EL636
02102      EXEC CICS READ                                               EL636
02103          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02104          DATASET (FILE-ID-ERCKWK)                                 EL636
02105          RIDFLD  (ERCKWK-KEY)                                     EL636
02106      END-EXEC.                                                    EL636
02107                                                                   EL636
02108      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL*13
02109          GO TO      5090-CHECKS-NOTFND.                              CL*13
02110                                                                      CL*13
02111  5020-FOUND.                                                         CL**7
02112      IF NOT CW-HEADER                                             EL636
02113         GO TO 5090-CHECKS-NOTFND.                                 EL636
02114                                                                   EL636
02115      MOVE +1 TO WS-SUB1.                                             CL*11
02116  5000-SAVE-MAINT.                                                    CL*11
02117                                                                      CL*11
02118      IF WS-SUB1 LESS THAN +6                                         CL*11
02119          MOVE CK-MAINT (WS-SUB1) TO WS-HOLD-MAINT (WS-SUB1)          CL*11
02120          ADD +1 TO WS-SUB1                                           CL*11
02121          GO TO 5000-SAVE-MAINT.                                      CL*11
02122                                                                      CL*11
02123      MOVE MAINTI TO WS-SAVE-ACTION-CODE.                             CL**8
02124      MOVE LOW-VALUES             TO  EL636AI.                     EL636
02125      MOVE 'C'                    TO  MAINTI.                      EL636
02126      MOVE AL-UANON               TO  MAINTA.                      EL636
02127      MOVE CW-CSR                 TO  CSRO.                           CL*13
02128      MOVE AL-UANON               TO  CSRA.                           CL*13
02129      MOVE CW-CARRIER             TO  CARO.                        EL636
02130      MOVE AL-UANON               TO  CARA.                        EL636
02131      MOVE CW-GROUPING            TO  GROUPO.                      EL636
02132      MOVE AL-UANON               TO  GROUPA.                      EL636
02133      MOVE CW-PAYEE               TO  PAYEEO.                      EL636
02134      MOVE AL-UANON               TO  PAYEEA.                      EL636
02135      MOVE CW-PAYEE-SEQ           TO  PAYSEQO.                        CL**7
02136      MOVE AL-UANON               TO  PAYSEQA.                        CL**7
02137      MOVE CW-PAYEE-NAME          TO  PAYTOO.                      EL636
02138      MOVE AL-UANON               TO  PAYTOA.                         CL**7
02139      MOVE CW-ADDRESS-1           TO  ADDRS1O.                     EL636
02140      MOVE AL-UANON               TO  ADDRS1A.                        CL**7
02141      MOVE CW-ADDRESS-2           TO  ADDRS2O.                     EL636
02142      MOVE AL-UANON               TO  ADDRS2A.                        CL**7
02143      MOVE CW-PAYEE-CITY-ST       TO  CITYSTO.                     EL636
02144      MOVE AL-UANON               TO  CITYSTA.                        CL**7
02145      MOVE CW-PAYEE-ZIP           TO  ZIPO.                        EL636
02146      MOVE AL-UANON               TO  ZIPA.                           CL**7
02147      MOVE CW-PAYEE-ZIP-EXT       TO  ZIPEXTO.                     EL636
02148      MOVE AL-UANON               TO  ZIPEXTA.                        CL**7
02149                                                                      CL*11
02150                                                                      CL*12
02151      MOVE AL-SANON               TO  PFKEY8A.                        CL*12
02152                                                                   EL636
02153      IF CW-RECORDED-BY = 'OFFL'                                   EL636
02154         MOVE 'OFF-LINE'          TO  MAINTBYO                     EL636
02155      ELSE                                                         EL636
02156         MOVE CW-RECORDED-BY      TO  MAINTBYO.                    EL636
02157                                                                   EL636
02158      MOVE CW-LAST-MAINT-HHMMSS   TO  WS-TIME.                     EL636
02159      MOVE WS-HR-MINS             TO  MAINTATO.                    EL636
02160      MOVE CW-RECORDED-DT         TO  DC-BIN-DATE-1.               EL636
02161      MOVE SPACE                  TO  DC-OPTION-CODE.              EL636
02162      PERFORM 8500-DATE-CONVERT.                                   EL636
02163                                                                   EL636
02164      IF NO-CONVERSION-ERROR                                       EL636
02165         MOVE DC-GREG-DATE-1-EDIT TO  MAINTONO.                    EL636
02166                                                                   EL636
02167      MOVE CW-RELEASE-DT          TO  DC-BIN-DATE-1.               EL636
02168      MOVE SPACE                  TO  DC-OPTION-CODE.              EL636
02169      PERFORM 8500-DATE-CONVERT.                                   EL636
02170                                                                   EL636
02171      IF NO-CONVERSION-ERROR                                       EL636
02172         MOVE DC-GREG-DATE-1-EDIT TO  RELDTO.                      EL636
02173                                                                   EL636
02174      MOVE CW-TOTAL-COMMISSION    TO  CHKAMTO.                     EL636
02175      MOVE CW-TOTAL-ENTRIES       TO  NOENTRSO.                    EL636
02176      MOVE AL-UNNON               TO  NOFIRSTA.                       CL**8
02177      MOVE ZEROS                  TO  NOFIRSTO                        CL**8
02178                                      NOLASTO.                        CL**8
02179                                                                   EL636
02180      MOVE PI-ERCKWK-KEY          TO  PI-LAST-STUB-KEY.               CL**8
02181                                                                      CL**8
02182      IF SHOW-FUNCTION  AND                                           CL**8
02183          WS-NOFIRST GREATER +0                                       CL**8
02184          IF WS-NOFIRST GREATER THAN CW-TOTAL-ENTRIES                 CL**8
02185              IF CW-TOTAL-ENTRIES GREATER THAN +5                     CL**8
02186                  SUBTRACT +5 FROM CW-TOTAL-ENTRIES                   CL**8
02187                                     GIVING  PI-ERCKWK-SEQ-NO         CL**8
02188              ELSE                                                    CL**8
02189                  MOVE +1 TO  PI-ERCKWK-SEQ-NO                        CL**8
02190          ELSE                                                        CL**8
02191              MOVE WS-NOFIRST TO  PI-ERCKWK-SEQ-NO.                   CL**8
02192                                                                      CL**8
02193      IF  EIBAID = DFHPF3                                             CL**8
02194          IF WS-NOFIRST LESS THAN CW-TOTAL-ENTRIES                    CL**8
02195              ADD +1 WS-NOFIRST GIVING PI-ERCKWK-SEQ-NO               CL**9
02196          ELSE                                                        CL**8
02197              MOVE WS-NOFIRST          TO PI-ERCKWK-SEQ-NO.           CL**8
02198                                                                      CL**7
02199      MOVE PI-ERCKWK-KEY          TO SAVE-ERCKWK-CONTROL           EL636
02200                                     PI-SAVE-ERCKWK-KEY.           EL636
02201      EXEC CICS HANDLE CONDITION                                   EL636
02202          NOTFND    (5070-CHECK-PROCESSED)                         EL636
02203          ENDFILE   (5060-END-OF-FILE)                             EL636
02204      END-EXEC.                                                    EL636
02205                                                                   EL636
02206      IF WS-BROWSE-SW  =  'Y'                                         CL*14
02207          GO TO 5049-SET.                                             CL*14
02208      MOVE SPACE                  TO WS-BROWSE-SW.                 EL636
02209                                                                   EL636
02210      EXEC CICS STARTBR                                            EL636
02211          DATASET (FILE-ID-ERCKWK)                                 EL636
02212          RIDFLD  (PI-ERCKWK-KEY)                                  EL636
02213      END-EXEC.                                                    EL636
02214                                                                   EL636
02215  5049-SET.                                                           CL*14
02216      MOVE +0                     TO WS-SUB1                          CL**8
02217                                     PI-NO-ENTRIES-DISPLAYED.         CL**8
02218      MOVE 'Y'                    TO WS-BROWSE-SW.                 EL636
02219                                                                   EL636
02220  5050-READ-CHECK-WORK-FILE.                                          CL**7
02221                                                                   EL636
02222      EXEC CICS READNEXT                                           EL636
02223          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02224          DATASET (FILE-ID-ERCKWK)                                 EL636
02225          RIDFLD  (PI-ERCKWK-KEY)                                  EL636
02226      END-EXEC.                                                    EL636
02227                                                                   EL636
02228      IF CW-COMPANY-CD = SVCKWK-COMPANY-CD AND                     EL636
02229         CW-CSR        = SVCKWK-CSR        AND                        CL*13
02230         CW-CARRIER    = SVCKWK-CARRIER    AND                     EL636
02231         CW-GROUPING   = SVCKWK-GROUPING   AND                     EL636
02232         CW-PAYEE      = SVCKWK-PAYEE      AND                        CL**7
02233         CW-PAYEE-SEQ  = SVCKWK-PAYEE-SEQ                             CL**7
02234         NEXT SENTENCE                                             EL636
02235      ELSE                                                         EL636
02236         GO TO 5070-CHECK-PROCESSED.                               EL636
02237                                                                   EL636
02238      IF CW-HEADER OR CW-TEXT                                         CL*11
02239         GO TO 5050-READ-CHECK-WORK-FILE                              CL**8
02240      ELSE                                                            CL**8
02241          IF  FIRST-DETAIL-READ                                       CL**8
02242              MOVE SPACES TO WS-FIRST-DETAIL-SW                       CL**8
02243              MOVE AL-UNNON       TO  NOFIRSTA                        CL**8
02244              MOVE CW-SEQUENCE-NO TO  NOFIRSTO.                       CL**8
02245                                                                   EL636
02246      ADD +1                      TO  WS-SUB1.                     EL636
02247                                                                   EL636
02248      IF  WS-SUB1 GREATER THAN +5                                  EL636
02249          GO TO 5070-CHECK-PROCESSED.                              EL636
02250                                                                      CL**8
02251      MOVE CW-SEQUENCE-NO         TO  NOLASTO.                        CL**8
02252                                                                   EL636
02253      MOVE CW-COMMENT             TO  CK-COMNTS      (WS-SUB1).    EL636
02254      MOVE CW-ACCT-AGENT          TO  CK-AGENT       (WS-SUB1).    EL636
02255                                                                   EL636
02256      IF WS-HOLD-MAINT (WS-SUB1) EQUAL 'I'                            CL*11
02257         MOVE CW-INVOICE          TO  CK-INVREF      (WS-SUB1)     EL636
02258      ELSE                                                         EL636
02259         IF WS-HOLD-MAINT (WS-SUB1) EQUAL 'R'                         CL*11
02260             MOVE CW-REFERENCE    TO  CK-INVREF      (WS-SUB1)        CL*11
02261         ELSE                                                         CL*11
02262             IF CW-INVOICE GREATER THAN SPACES                        CL*11
02263                 MOVE CW-INVOICE   TO  CK-INVREF     (WS-SUB1)        CL*11
02264             ELSE                                                     CL*11
02265                 MOVE CW-REFERENCE TO  CK-INVREF     (WS-SUB1).       CL*11
02266                                                                   EL636
02267      MOVE CW-LEDGER-NO           TO  CK-LEDGER      (WS-SUB1).    EL636
02268      MOVE CW-PMT-APPLIED         TO  CK-AGOCD       (WS-SUB1).       CL*20
02269      MOVE CW-DETAIL-AMOUNT       TO  CK-DETAMT-OUT  (WS-SUB1).    EL636
02270                                                                   EL636
02271      MOVE PI-ERCKWK-KEY          TO  PI-STUB-KEY     (WS-SUB1)       CL**8
02272                                      PI-LAST-STUB-KEY.               CL**8
02273      MOVE CW-ACCT-AGENT          TO  PI-STUB-ACCT   (WS-SUB1).       CL*21
02274                                                                      CL*11
02275      IF CW-LAST-MAINT-APPLIED EQUAL 'H'                              CL*11
02276          MOVE CW-LAST-MAINT-APPLIED                                  CL*11
02277                                  TO  CK-MAINT (WS-SUB1)              CL*11
02278          MOVE 'THIS ITEM ON HOLD'                                    CL*11
02279                                  TO  CK-COMNTS       (WS-SUB1)       CL*11
02280          MOVE +0                 TO  CK-COMNTS-LEN   (WS-SUB1)       CL*11
02281          MOVE AL-UNBOF           TO  CK-DETAMT-ATTRB (WS-SUB1)       CL*11
02282          MOVE AL-UABOF           TO  CK-COMNTS-ATTRB (WS-SUB1)       CL*11
02283                                      CK-MAINT-ATTRB  (WS-SUB1)       CL*11
02284                                      CK-AGENT-ATTRB  (WS-SUB1)       CL*11
02285                                      CK-INVREF-ATTRB (WS-SUB1)       CL*11
02286                                      CK-LEDGER-ATTRB (WS-SUB1)       CL*20
02287                                      CK-AGOCD-ATTRB  (WS-SUB1).      CL*20
02288                                                                   EL636
02289      MOVE WS-SUB1                TO  PI-NO-ENTRIES-DISPLAYED.     EL636
02290                                                                   EL636
02291      GO TO 5050-READ-CHECK-WORK-FILE.                                CL**7
02292                                                                   EL636
02293  5060-END-OF-FILE.                                                EL636
02294                                                                   EL636
02295      MOVE ER-2251                TO EMI-ERROR.                    EL636
02296      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02297                                                                   EL636
02298  5070-CHECK-PROCESSED.                                            EL636
02299                                                                   EL636
02300      IF WS-BROWSE-STARTED                                         EL636
02301         EXEC CICS ENDBR                                           EL636
02302              DATASET (FILE-ID-ERCKWK)                             EL636
02303         END-EXEC.                                                 EL636
02304                                                                   EL636
02305      IF EIBAID = DFHPF3                                              CL**7
02306          MOVE ' '                TO  PI-PREV-FUNCTION.               CL**7
02307                                                                      CL**7
02308      MOVE 'S'                    TO  PI-MAINT-FUNCTION.           EL636
02309      MOVE -1                     TO  MAINTL.                      EL636
02310      GO TO 8100-SEND-INITIAL-MAP.                                 EL636
02311                                                                   EL636
02312  5090-CHECKS-NOTFND.                                              EL636
02313                                                                   EL636
02314      IF WS-BROWSE-STARTED                                            CL**7
02315         EXEC CICS ENDBR                                              CL**7
02316              DATASET (FILE-ID-ERCKWK)                                CL**7
02317         END-EXEC.                                                    CL**7
02318                                                                      CL**9
02319      EXEC CICS HANDLE CONDITION                                      CL**9
02320          NOTFND    (5095-CHECKS-NOTFND)                              CL**9
02321          ENDFILE   (5095-CHECKS-NOTFND)                              CL**9
02322      END-EXEC.                                                       CL**9
02323                                                                      CL**7
02324      MOVE SPACE                  TO WS-BROWSE-SW.                    CL*14
02325                                                                      CL*14
02326      EXEC CICS STARTBR                                               CL**7
02327          DATASET (FILE-ID-ERCKWK)                                    CL**7
02328          RIDFLD  (PI-ERCKWK-KEY)                                     CL**7
02329          GTEQ                                                        CL**7
02330      END-EXEC.                                                       CL**7
02331                                                                      CL*14
02332      MOVE 'Y'                    TO WS-BROWSE-SW.                    CL*14
02333                                                                      CL**7
02334      EXEC CICS READNEXT                                              CL**7
02335          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02336          DATASET (FILE-ID-ERCKWK)                                    CL**7
02337          RIDFLD  (PI-ERCKWK-KEY)                                     CL**7
02338      END-EXEC.                                                       CL**7
02339                                                                      CL**7
02340      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL*13
02341          GO TO      5095-CHECKS-NOTFND                               CL*14
02342      ELSE                                                            CL*14
02343          GO TO      5020-FOUND.                                      CL*14
02344                                                                      CL*13
02345                                                                      CL**7
02346  5095-CHECKS-NOTFND.                                                 CL**7
02347      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL636
02348      MOVE ER-3160                TO EMI-ERROR.                    EL636
02349      MOVE -1                     TO CARL.                         EL636
02350      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02351      GO TO 8200-SEND-DATAONLY.                                    EL636
02352                                                                   EL636
02353      EJECT                                                        EL636
02354                                                                   EL636
02355 ******************************************************************EL636
02356 *                                                                *EL636
02357 *            D I S P L A Y   N E X T   P A Y E E                 *EL636
02358 *                                                                *EL636
02359 ******************************************************************EL636
02360                                                                   EL636
02361  6000-DISPLAY-NEXT-PAYEE.                                         EL636
02362                                                                   EL636
02363      MOVE PI-LAST-STUB-KEY       TO PI-ERCKWK-KEY.                   CL**8
02364      MOVE PI-COMPANY-CD          TO PI-ERCKWK-COMP-CD.            EL636
02365                                                                      CL*20
02366      IF PI-LAST-STUB-CSR = SPACES                                    CL*20
02367          MOVE LOW-VALUES         TO PI-LAST-STUB-CSR.                CL*20
02368                                                                      CL*20
02369      MOVE  PI-LAST-STUB-CSR      TO PI-ERCKWK-CSR                    CL*20
02370                                                                      CL*20
02371      MOVE +9999                  TO PI-ERCKWK-SEQ-NO.             EL636
02372                                                                   EL636
02373      EXEC CICS HANDLE CONDITION                                   EL636
02374          ENDFILE   (6060-END-OF-FILE)                             EL636
02375      END-EXEC.                                                    EL636
02376                                                                   EL636
02377  6010-READ-NEXT-PAYEE.                                            EL636
02378                                                                   EL636
02379      EXEC CICS READ                                               EL636
02380          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02381          DATASET (FILE-ID-ERCKWK)                                 EL636
02382          RIDFLD  (PI-ERCKWK-KEY)                                  EL636
02383          GTEQ                                                     EL636
02384      END-EXEC.                                                    EL636
02385                                                                   EL636
02386      IF CW-COMPANY-CD NOT = PI-COMPANY-CD                         EL636
02387         GO TO 6060-END-OF-FILE.                                   EL636
02388                                                                   EL636
02389      MOVE  CW-CSR                TO  PI-ERCKWK-CSR.                  CL*13
02390      MOVE  CW-CARRIER            TO  PI-ERCKWK-CARRIER.           EL636
02391      MOVE  CW-GROUPING           TO  PI-ERCKWK-GROUPING.          EL636
02392      MOVE  CW-PAYEE              TO  PI-ERCKWK-PAYEE.             EL636
02393      MOVE  CW-PAYEE-SEQ          TO  PI-ERCKWK-PAYEE-SEQ.            CL**7
02394      MOVE  CW-SEQUENCE-NO        TO  PI-ERCKWK-SEQ-NO.            EL636
02395                                                                   EL636
02396      GO TO 5000-DISPLAY-PAYEE.                                    EL636
02397                                                                   EL636
02398  6060-END-OF-FILE.                                                EL636
02399                                                                   EL636
02400      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL636
02401      MOVE ER-2251                TO EMI-ERROR.                    EL636
02402      MOVE -1                     TO CARL.                         EL636
02403      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02404      GO TO 8200-SEND-DATAONLY.                                    EL636
02405                                                                   EL636
02406      EJECT                                                        EL636
02407                                                                   EL636
02408 ******************************************************************EL636
02409 *                                                                *EL636
02410 *            D I S P L A Y   P R E V   P A Y E E                 *EL636
02411 *                                                                *EL636
02412 ******************************************************************EL636
02413                                                                   EL636
02414  6100-DISPLAY-PREV-PAYEE.                                         EL636
02415                                                                   EL636
02416      IF PI-LAST-STUB-KEY EQUAL SPACES                                CL**8
02417          GO TO 6160-END-OF-FILE.                                     CL**8
02418                                                                   EL636
02419      MOVE PI-LAST-STUB-KEY       TO PI-ERCKWK-KEY.                   CL**8
02420                                                                      CL**8
02421      IF PI-ERCKWK-SEQ-NO GREATER THAN +5                             CL**8
02422          MOVE +1 TO PI-ERCKWK-SEQ-NO                                 CL**8
02423          GO TO 5000-DISPLAY-PAYEE.                                   CL**8
02424                                                                      CL**8
02425      MOVE PI-COMPANY-CD          TO PI-ERCKWK-COMP-CD.            EL636
02426      MOVE +0000                  TO PI-ERCKWK-SEQ-NO.             EL636
02427                                                                   EL636
02428      EXEC CICS HANDLE CONDITION                                   EL636
02429          ENDFILE   (6160-END-OF-FILE)                             EL636
02430      END-EXEC.                                                    EL636
02431                                                                   EL636
02432      EXEC CICS STARTBR                                            EL636
02433          DATASET (FILE-ID-ERCKWK)                                 EL636
02434          RIDFLD  (PI-ERCKWK-KEY)                                  EL636
02435      END-EXEC.                                                    EL636
02436                                                                   EL636
02437      MOVE PI-ERCKWK-KEY          TO SVCKWK-COMPARE-KEY.              CL**2
02438                                                                      CL**2
02439      EXEC CICS READNEXT                                              CL**2
02440          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02441          DATASET (FILE-ID-ERCKWK)                                    CL**2
02442          RIDFLD  (PI-ERCKWK-KEY)                                     CL**2
02443      END-EXEC.                                                       CL**2
02444                                                                      CL**2
02445      EXEC CICS RESETBR                                               CL**2
02446          DATASET (FILE-ID-ERCKWK)                                    CL**2
02447          RIDFLD  (PI-ERCKWK-KEY)                                     CL**2
02448      END-EXEC.                                                       CL**2
02449                                                                   EL636
02450  6110-READ-PREV-ENTRY.                                            EL636
02451                                                                   EL636
02452      EXEC CICS READPREV                                           EL636
02453          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02454          DATASET (FILE-ID-ERCKWK)                                 EL636
02455          RIDFLD  (PI-ERCKWK-KEY)                                  EL636
02456      END-EXEC.                                                    EL636
02457                                                                      CL*13
02458      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL*13
02459          GO TO      6160-END-OF-FILE.                                CL*13
02460                                                                   EL636
02461      MOVE PI-ERCKWK-KEY          TO ERCKWK-COMPARE-KEY.              CL**2
02462                                                                      CL**2
02463      IF ERCKWK-COMPARE-KEY LESS THAN SVCKWK-COMPARE-KEY              CL**2
02464         NEXT SENTENCE                                                CL**2
02465      ELSE                                                            CL**2
02466         GO TO 6110-READ-PREV-ENTRY.                               EL636
02467                                                                   EL636
02468      MOVE +1                     TO PI-ERCKWK-SEQ-NO.             EL636
02469                                                                   EL636
02470      EXEC CICS ENDBR                                              EL636
02471           DATASET (FILE-ID-ERCKWK)                                EL636
02472      END-EXEC.                                                    EL636
02473                                                                   EL636
02474      IF PI-ERCKWK-COMP-CD NOT = PI-COMPANY-CD                     EL636
02475         GO TO 6160-END-OF-FILE                                    EL636
02476      ELSE                                                         EL636
02477         GO TO 5000-DISPLAY-PAYEE.                                 EL636
02478                                                                   EL636
02479  6160-END-OF-FILE.                                                EL636
02480                                                                   EL636
02481      MOVE SPACE                  TO PI-MAINT-FUNCTION.            EL636
02482      MOVE ER-2252                TO EMI-ERROR.                    EL636
02483      MOVE -1                     TO CARL.                         EL636
02484      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02485      GO TO 8200-SEND-DATAONLY.                                    EL636
02486                                                                   EL636
02487      EJECT                                                        EL636
02488                                                                   EL636
02489 ******************************************************************EL636
02490 *                                                                *EL636
02491 *         D I S P L A Y   P R E V   E N T E R I E S              *EL636
02492 *                                                                *EL636
02493 ******************************************************************EL636
02494                                                                   EL636
02495  6600-DISPLAY-PREV-ENTRIES.                                       EL636
02496                                                                   EL636
02497      IF PI-NO-ENTRIES-DISPLAYED GREATER THAN ZEROS                   CL**8
02498          MOVE PI-STUB-KEY (1)        TO PI-ERCKWK-KEY                CL**8
02499                                         SAVE-ERCKWK-CONTROL          CL**8
02500      ELSE                                                            CL**8
02501          MOVE PI-LAST-STUB-KEY       TO PI-ERCKWK-KEY                CL**8
02502          GO TO 5000-DISPLAY-PAYEE.                                   CL**8
02503                                                                   EL636
02504      MOVE +0                     TO WS-SUB1.                      EL636
02505                                                                   EL636
02506      EXEC CICS STARTBR                                            EL636
02507          DATASET (FILE-ID-ERCKWK)                                 EL636
02508          RIDFLD  (PI-ERCKWK-KEY)                                  EL636
02509      END-EXEC.                                                    EL636
02510                                                                   EL636
02511      MOVE PI-ERCKWK-KEY          TO SAVE-ERCKWK-CONTROL.             CL**2
02512                                                                      CL**2
02513      EXEC CICS READNEXT                                              CL**2
02514          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02515          DATASET (FILE-ID-ERCKWK)                                    CL**2
02516          RIDFLD  (PI-ERCKWK-KEY)                                     CL**2
02517      END-EXEC.                                                       CL**2
02518                                                                      CL**2
02519      EXEC CICS RESETBR                                               CL**2
02520          DATASET (FILE-ID-ERCKWK)                                    CL**2
02521          RIDFLD  (PI-ERCKWK-KEY)                                     CL**2
02522      END-EXEC.                                                       CL**2
02523                                                                      CL**2
02524  6610-READ-PREV-ENTRY.                                            EL636
02525                                                                   EL636
02526      EXEC CICS READPREV                                           EL636
02527          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02528          DATASET (FILE-ID-ERCKWK)                                 EL636
02529          RIDFLD  (PI-ERCKWK-KEY)                                  EL636
02530      END-EXEC.                                                    EL636
02531                                                                   EL636
02532      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL*13
02533          GO TO      5000-DISPLAY-PAYEE.                              CL*13
02534                                                                      CL*13
02535      IF PI-ERCKWK-PAYEE GREATER THAN SVCKWK-PAYEE                    CL**2
02536         GO TO 6610-READ-PREV-ENTRY.                               EL636
02537                                                                   EL636
02538      IF PI-ERCKWK-SEQ-NO = ZEROS                                  EL636
02539         GO TO 6660-PREV-ENTRIES-PRIMED.                           EL636
02540                                                                   EL636
02541      ADD +1                      TO WS-SUB1.                      EL636
02542                                                                   EL636
02543      IF WS-SUB1 GREATER +5                                           CL**2
02544         GO TO 6660-PREV-ENTRIES-PRIMED.                           EL636
02545                                                                   EL636
02546      GO TO 6610-READ-PREV-ENTRY.                                  EL636
02547                                                                   EL636
02548  6660-PREV-ENTRIES-PRIMED.                                        EL636
02549                                                                   EL636
02550      EXEC CICS ENDBR                                              EL636
02551           DATASET (FILE-ID-ERCKWK)                                EL636
02552      END-EXEC.                                                    EL636
02553                                                                   EL636
02554      GO TO 5000-DISPLAY-PAYEE.                                    EL636
02555                                                                   EL636
02556      EJECT                                                        EL636
02557                                                                   EL636
02558 ******************************************************************EL636
02559 *                                                                *EL636
02560 *    R E L E A S E   E N T E R I E S   T O   CHECK-FILE          *EL636
02561 *                                                                *EL636
02562 ******************************************************************EL636
02563                                                                   EL636
02564  7000-RELEASE-ENTRIES.                                            EL636
02565      MOVE ' '                    TO ERCOMP-SW.                       CL*26
02566                                                                   EL636
02567      EXEC CICS GETMAIN                                            EL636
02568          SET     (ADDRESS OF COMM-CHECK-RECORDS)                     CL*24
02569          LENGTH  (ERCMCK-RECORD-LENGTH)                           EL636
02570          INITIMG (GETMAIN-SPACE)                                  EL636
02571      END-EXEC.                                                    EL636
02572                                                                   EL636
02573      EXEC CICS HANDLE CONDITION                                   EL636
02574          NOTFND    (7090-CHECKS-NOTFND)                           EL636
02575          ENDFILE   (7090-CHECKS-NOTFND)                           EL636
02576      END-EXEC.                                                    EL636
02577                                                                   EL636
02578      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.            EL636
02579      MOVE CSRI                   TO ERCKWK-CSR.                      CL*13
02580      MOVE CARI                   TO ERCKWK-CARRIER.               EL636
02581      MOVE GROUPI                 TO ERCKWK-GROUPING.              EL636
02582      MOVE PAYEEI                 TO ERCKWK-PAYEE.                 EL636
02583      MOVE PAYSEQI                TO ERCKWK-PAYEE-SEQ.                CL**7
02584      MOVE +0                     TO ERCKWK-SEQUENCE-NO.           EL636
02585                                                                   EL636
02586      EXEC CICS READ                                               EL636
02587          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02588          DATASET (FILE-ID-ERCKWK)                                 EL636
02589          RIDFLD  (ERCKWK-KEY)                                     EL636
02590          UPDATE                                                   EL636
02591      END-EXEC.                                                    EL636
02592                                                                   EL636
02593      IF CW-RELEASE-DT GREATER THAN LOW-VALUES                        CL**3
02594         GO TO 7095-RELEASE-ERROR.                                    CL**3
02595                                                                      CL**3
02596      IF CW-TOTAL-COMMISSION NOT GREATER THAN ZEROS                EL636
02597         GO TO 7085-COMMISSION-ERROR.                              EL636
02598                                                                   EL636
02599      IF CW-PAYEE-NAME = SPACES OR                                 EL636
02600         CW-PAYEE-NAME = 'NEED MAILING NAME'                       EL636
02601         GO TO 7075-PAYEE-NAME-ERROR.                              EL636
02602                                                                   EL636
02603      MOVE WS-CURRENT-BIN-DT      TO  CW-RELEASE-DT.               EL636
02604      MOVE WS-CURRENT-BIN-DT      TO  CW-RECORDED-DT.                 CL*23
02605      MOVE PI-PROCESSOR-ID        TO  CW-RECORDED-BY.                 CL*23
02606      MOVE EIBTIME                TO  CW-LAST-MAINT-HHMMSS.           CL*23
02607                                                                   EL636
02608      MOVE +0 TO WS-SEQUENCE-NO.                                   EL636
02609                                                                   EL636
02610      MOVE CW-PAYEE-NAME          TO WS-PAYEE-NAME.                EL636
02611      MOVE CW-ADDRESS-1           TO WS-PAYEE-ADDRESS-1.           EL636
02612      MOVE CW-ADDRESS-2           TO WS-PAYEE-ADDRESS-2.           EL636
02613      MOVE CW-PAYEE-CITY-ST       TO WS-PAYEE-CITY-ST.             EL636
02614      MOVE CW-PAYEE-ZIP           TO WS-PAYEE-ZIP.                 EL636
02615      MOVE CW-PAYEE-ZIP-EXT       TO WS-PAYEE-ZIP-EXT.             EL636
02616                                                                   EL636
02617      MOVE CW-TOTAL-COMMISSION    TO WS-AMOUNT-PAID.               EL636
02618      MOVE CW-TOTAL-ENTRIES       TO WS-TOTAL-ENTRIES.             EL636
02619                                                                   EL636
02620      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.          EL636
02621                                                                   EL636
02622      MOVE +1                     TO WS-SUB2.                      EL636
02623                                                                   EL636
02624      EXEC CICS REWRITE                                            EL636
02625           FROM    (CHECK-WORK-RECORDS)                            EL636
02626           DATASET (FILE-ID-ERCKWK)                                EL636
02627      END-EXEC.                                                    EL636
02628                                                                   EL636
02629      EXEC CICS HANDLE CONDITION                                   EL636
02630          NOTFND    (7070-CHECK-PROCESSED)                         EL636
02631          ENDFILE   (7070-CHECK-PROCESSED)                         EL636
02632      END-EXEC.                                                    EL636
02633                                                                   EL636
02634  7010-READ-CHECK-WORK-FILE.                                       EL636
02635                                                                   EL636
02636      EXEC CICS STARTBR                                            EL636
02637          DATASET (FILE-ID-ERCKWK)                                 EL636
02638          RIDFLD  (ERCKWK-KEY)                                     EL636
02639      END-EXEC.                                                    EL636
02640                                                                   EL636
02641  7020-READ-NEXT-DETAIL.                                           EL636
02642                                                                   EL636
02643      EXEC CICS READNEXT                                           EL636
02644          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02645          DATASET (FILE-ID-ERCKWK)                                 EL636
02646          RIDFLD  (ERCKWK-KEY)                                     EL636
02647      END-EXEC.                                                    EL636
02648                                                                   EL636
02649      IF ERCKWK-KEY = SAVE-ERCKWK-CONTROL                          EL636
02650         GO TO 7020-READ-NEXT-DETAIL.                              EL636
02651                                                                   EL636
02652      IF CW-COMPANY-CD = SVCKWK-COMPANY-CD AND                     EL636
02653         CW-CSR        = SVCKWK-CSR        AND                        CL*13
02654         CW-CARRIER    = SVCKWK-CARRIER    AND                     EL636
02655         CW-GROUPING   = SVCKWK-GROUPING   AND                     EL636
02656         CW-PAYEE      = SVCKWK-PAYEE      AND                        CL**7
02657         CW-PAYEE-SEQ  = SVCKWK-PAYEE-SEQ                             CL**7
02658         NEXT SENTENCE                                             EL636
02659      ELSE                                                         EL636
02660         GO TO 7080-CHECK-ERROR.                                   EL636
02661                                                                   EL636
02662      IF CW-TEXT                                                      CL*11
02663         GO TO 7020-READ-NEXT-DETAIL.                                 CL*11
02664                                                                      CL*26
02665      IF ERCOMP-SW  IS EQUAL      TO ' '                              CL*26
02666         MOVE CW-COMPANY-CD       TO ERCOMP-COMP-CD                   CL*26
02667         MOVE CW-CARRIER          TO ERCOMP-CARRIER                   CL*26
02668         MOVE CW-GROUPING         TO ERCOMP-GROUPING                  CL*26
02669         MOVE CW-PAYEE            TO ERCOMP-FIN-RESP                  CL*26
02670         IF CW-PMT-APPLIED = 'O' OR 'G'                               CL*26
02671            MOVE LOW-VALUES       TO ERCOMP-ACCT                      CL*26
02672            MOVE 'G'              TO ERCOMP-RECORD-TYPE               CL*26
02673            PERFORM 1450-ACH-READ    THRU 1470-EXIT                   CL*26
02674         ELSE                                                         CL*26
02675            MOVE CW-ACCT-AGENT    TO ERCOMP-ACCT                      CL*26
02676            MOVE 'A'              TO ERCOMP-RECORD-TYPE               CL*26
02677            PERFORM 1450-ACH-READ    THRU 1470-EXIT                   CL*26
02678         END-IF                                                       CL*26
02679      END-IF.                                                         CL*26
02680                                                                      CL*26
02681                                                                      CL*11
02682      MOVE CW-COMMENT             TO  CK-STUB-COMMENT  (WS-SUB2).  EL636
02683      MOVE CW-ACCT-AGENT          TO  CK-ACCT-AGENT    (WS-SUB2).  EL636
02684      MOVE CW-INVOICE             TO  CK-INVOICE       (WS-SUB2).  EL636
02685      MOVE CW-REFERENCE           TO  CK-REFERENCE     (WS-SUB2).  EL636
02686      MOVE CW-LEDGER-NO           TO  CK-LEDGER-NO     (WS-SUB2).  EL636
02687      MOVE CW-LAST-MAINT-APPLIED                                      CL*11
02688                              TO  CK-LAST-MAINT-APPLIED (WS-SUB2).    CL*11
02689                                                                      CL*11
02690      MOVE CW-NON-AR-ITEM         TO  CK-NON-AR-ITEM   (WS-SUB2).     CL*11
02691      MOVE CW-PMT-APPLIED         TO  CK-PYAJ-PMT-APPLIED (WS-SUB2)   CL*20
02692                                                                      CL**2
02693      MOVE 'C'                    TO  CK-PAYMENT-TYPE  (WS-SUB2).     CL**8
02694                                                                      CL**2
02695      MOVE CW-DETAIL-AMOUNT       TO  CK-DETAIL-AMT    (WS-SUB2).  EL636
02696                                                                   EL636
02697      MOVE CW-CSR                 TO  CK-CKWK-CSR      (WS-SUB2).     CL*13
02698      MOVE CW-CARRIER             TO  CK-CKWK-CARRIER  (WS-SUB2).  EL636
02699      MOVE CW-GROUPING            TO  CK-CKWK-GROUPING (WS-SUB2).  EL636
02700      MOVE CW-PAYEE               TO  CK-CKWK-PAYEE    (WS-SUB2).  EL636
02701      MOVE CW-PAYEE-SEQ           TO  CK-CKWK-PAYEE-SEQ (WS-SUB2).    CL**7
02702      MOVE CW-SEQUENCE-NO         TO  CK-CKWK-SEQ-NO   (WS-SUB2).  EL636
02703                                                                   EL636
02704      EXEC CICS ENDBR                                              EL636
02705           DATASET (FILE-ID-ERCKWK)                                EL636
02706      END-EXEC.                                                    EL636
02707                                                                   EL636
02708      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.          EL636
02709                                                                   EL636
02710      EXEC CICS READ                                               EL636
02711          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02712          DATASET (FILE-ID-ERCKWK)                                 EL636
02713          RIDFLD  (ERCKWK-KEY)                                     EL636
02714          UPDATE                                                   EL636
02715      END-EXEC.                                                    EL636
02716                                                                   EL636
02717      MOVE WS-CURRENT-BIN-DT      TO  CW-RELEASE-DT.               EL636
02718                                                                   EL636
02719      EXEC CICS REWRITE                                            EL636
02720           FROM    (CHECK-WORK-RECORDS)                            EL636
02721           DATASET (FILE-ID-ERCKWK)                                EL636
02722      END-EXEC.                                                    EL636
02723                                                                   EL636
02724      SUBTRACT +1                 FROM WS-TOTAL-ENTRIES.           EL636
02725                                                                   EL636
02726      IF WS-TOTAL-ENTRIES = +0                                     EL636
02727         GO TO 7070-CHECK-PROCESSED.                               EL636
02728                                                                   EL636
02729      ADD +1                      TO   WS-SUB2.                    EL636
02730                                                                   EL636
02731      IF WS-SUB2 NOT GREATER THAN +15                              EL636
02732         GO TO 7010-READ-CHECK-WORK-FILE.                          EL636
02733                                                                   EL636
02734      MOVE ZEROS                  TO  CK-AMOUNT-PAID.              EL636
02735      MOVE 'D'                    TO  CK-RECORD-TYPE.                 CL*11
02736      PERFORM 7400-WRITE-COMM-CHECK-RECORD THRU 7490-EXIT.            CL*11
02737                                                                   EL636
02738      MOVE SPACES                 TO COMM-CHECK-RECORDS.           EL636
02739                                                                   EL636
02740      MOVE +1                     TO WS-SUB2.                      EL636
02741                                                                   EL636
02742      GO TO 7010-READ-CHECK-WORK-FILE.                             EL636
02743                                                                   EL636
02744  7070-CHECK-PROCESSED.                                            EL636
02745                                                                   EL636
02746      MOVE WS-AMOUNT-PAID         TO  CK-AMOUNT-PAID.              EL636
02747                                                                   EL636
02748      MOVE 'D'                    TO  CK-RECORD-TYPE.                 CL*11
02749      PERFORM 7400-WRITE-COMM-CHECK-RECORD THRU 7490-EXIT.            CL*11
02750                                                                      CL*11
02751      PERFORM 7100-WRITE-COMM-CHECK-TEXT THRU 7190-EXIT.              CL*11
02752                                                                   EL636
02753      IF PI-NO-ENTRIES-DISPLAYED GREATER THAN ZEROS                   CL**4
02754         MOVE PI-STUB-KEY (1)        TO PI-ERCKWK-KEY.                CL**4
02755                                                                   EL636
02756      MOVE ER-0000                TO  EMI-ERROR.                   EL636
02757      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02758      GO TO 5000-DISPLAY-PAYEE.                                       CL**4
02759                                                                   EL636
02760  7075-PAYEE-NAME-ERROR.                                           EL636
02761                                                                   EL636
02762      EXEC CICS UNLOCK                                             EL636
02763           DATASET (FILE-ID-ERCKWK)                                EL636
02764      END-EXEC.                                                    EL636
02765                                                                   EL636
02766      MOVE ER-3163                TO EMI-ERROR.                    EL636
02767      MOVE -1                     TO PAYTOL.                       EL636
02768      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02769      GO TO 8200-SEND-DATAONLY.                                    EL636
02770                                                                   EL636
02771  7080-CHECK-ERROR.                                                EL636
02772                                                                   EL636
02773      EXEC CICS SYNCPOINT ROLLBACK                                 EL636
02774      END-EXEC.                                                    EL636
02775                                                                   EL636
02776      MOVE ER-3162                TO EMI-ERROR.                    EL636
02777      MOVE -1                     TO CARL.                         EL636
02778      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02779      GO TO 8200-SEND-DATAONLY.                                    EL636
02780                                                                   EL636
02781  7085-COMMISSION-ERROR.                                           EL636
02782                                                                   EL636
02783      EXEC CICS UNLOCK                                             EL636
02784           DATASET (FILE-ID-ERCKWK)                                EL636
02785      END-EXEC.                                                    EL636
02786                                                                   EL636
02787      MOVE AL-SABOF               TO CHKAMTA.                      EL636
02788      MOVE ER-3165                TO EMI-ERROR.                    EL636
02789      MOVE -1                     TO CARL.                         EL636
02790      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02791      GO TO 8200-SEND-DATAONLY.                                    EL636
02792                                                                   EL636
02793  7090-CHECKS-NOTFND.                                              EL636
02794                                                                   EL636
02795      MOVE ER-3160                TO EMI-ERROR.                    EL636
02796      MOVE -1                     TO CARL.                         EL636
02797      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**3
02798      GO TO 8200-SEND-DATAONLY.                                       CL**3
02799                                                                      CL**3
02800  7095-RELEASE-ERROR.                                                 CL**3
02801                                                                      CL**3
02802      EXEC CICS UNLOCK                                                CL**3
02803           DATASET (FILE-ID-ERCKWK)                                   CL**3
02804      END-EXEC.                                                       CL**3
02805                                                                      CL**3
02806      MOVE ER-3139                TO EMI-ERROR.                       CL**3
02807      MOVE -1                     TO PAYTOL.                          CL**3
02808      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL636
02809      GO TO 8200-SEND-DATAONLY.                                    EL636
02810                                                                   EL636
02811      EJECT                                                        EL636
02812                                                                   EL636
02813 ******************************************************************EL636
02814 *                                                                *EL636
02815 *         W R I T E   C H E C K   S T U B   T E X T              *   CL*11
02816 *                                                                *EL636
02817 ******************************************************************EL636
02818                                                                   EL636
02819  7100-WRITE-COMM-CHECK-TEXT.                                         CL*11
02820                                                                      CL*11
02821      EXEC CICS HANDLE CONDITION                                      CL*11
02822          NOTFND    (7190-EXIT)                                       CL*24
02823          ENDFILE   (7190-EXIT)                                       CL*24
02824      END-EXEC.                                                       CL*11
02825                                                                      CL*11
02826      MOVE +0                     TO WS-SUB2.                         CL*11
02827      MOVE SAVE-ERCKWK-CONTROL    TO ERCKWK-KEY.                      CL*11
02828      MOVE +8999                  TO ERCKWK-SEQUENCE-NO.              CL*11
02829                                                                      CL*11
02830  7110-READ-TEXT-LOOP.                                                CL*11
02831      ADD +1                      TO WS-SUB2.                         CL*11
02832      ADD +1                      TO ERCKWK-SEQUENCE-NO.              CL*11
02833                                                                      CL*11
02834      EXEC CICS READ                                                  CL*11
02835          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
02836          DATASET (FILE-ID-ERCKWK)                                    CL*11
02837          RIDFLD  (ERCKWK-KEY)                                        CL*11
02838          UPDATE                                                      CL*11
02839      END-EXEC.                                                       CL*11
02840                                                                      CL*11
02841      MOVE WS-CURRENT-BIN-DT      TO  CW-RELEASE-DT.                  CL*11
02842      MOVE CW-STUB-TEXT           TO  CK-STUB-TEXT (WS-SUB2).         CL*11
02843      MOVE SPACES                 TO  CK-STUB-FILL-AREA.              CL*15
02844                                                                      CL*11
02845      IF ERCOMP-SW IS EQUAL       TO  'P'                             CL*26
02846         MOVE ERCOMP-SW           TO  CK-ACH-PAYMENT                  CL*26
02847      ELSE                                                            CL*26
02848         MOVE 'N'                 TO  CK-ACH-PAYMENT.                 CL*26
02849                                                                      CL*26
02850                                                                      CL*26
02851      EXEC CICS REWRITE                                               CL*11
02852           FROM    (CHECK-WORK-RECORDS)                               CL*11
02853           DATASET (FILE-ID-ERCKWK)                                   CL*11
02854      END-EXEC.                                                       CL*11
02855                                                                      CL*11
02856      IF WS-SUB2 LESS THAN +3                                         CL*11
02857         GO TO 7110-READ-TEXT-LOOP.                                   CL*11
02858                                                                      CL*11
02859      MOVE ZEROS                  TO  CK-AMOUNT-PAID.                 CL*11
02860      MOVE 'T'                    TO  CK-RECORD-TYPE.                 CL*11
02861      PERFORM 7400-WRITE-COMM-CHECK-RECORD THRU 7490-EXIT.            CL*11
02862                                                                      CL*11
02863      MOVE SPACES                 TO COMM-CHECK-RECORDS.              CL*11
02864                                                                      CL*11
02865  7180-TEXT-PROCESSED.                                                CL*11
02866                                                                      CL*11
02867  7190-EXIT.                                                          CL*11
02868       EXIT.                                                          CL*24
02869                                                                      CL*11
02870      EJECT                                                           CL*11
02871                                                                      CL*11
02872 ******************************************************************   CL*11
02873 *                                                                *   CL*11
02874 *         W R I T E   C O M M I S S I O N   C H E C K S          *   CL*11
02875 *                                                                *   CL*11
02876 ******************************************************************   CL*11
02877                                                                      CL*11
02878  7400-WRITE-COMM-CHECK-RECORD.                                       CL*11
02879                                                                   EL636
02880      ADD +1                      TO  WS-SEQUENCE-NO.              EL636
02881      MOVE WS-SEQUENCE-NO         TO  CK-SEQUENCE-NO.              EL636
02882                                                                   EL636
02883      MOVE 'CK'                   TO  CK-RECORD-ID.                EL636
02884      MOVE CW-COMPANY-CD          TO  CK-COMPANY-CD.               EL636
02885      MOVE CW-CSR                 TO  CK-CSR.                         CL*13
02886      MOVE CW-CARRIER             TO  CK-CARRIER.                  EL636
02887      MOVE CW-GROUPING            TO  CK-GROUPING.                 EL636
02888      MOVE CW-PAYEE               TO  CK-PAYEE.                    EL636
02889      MOVE CW-PAYEE-SEQ           TO  CK-PAYEE-SEQ.                   CL**7
02890      MOVE WS-CURRENT-BIN-DT      TO  CK-RECORDED-DT.              EL636
02891      MOVE CW-AR-STATEMENT-DT     TO  CK-AR-STATEMENT-DT.             CL*11
02892 *    MOVE CW-PMT-APPLIED         TO  CK-PMT-APPLIED.                 CL*20
02893      MOVE PI-PROCESSOR-ID        TO  CK-RECORDED-BY.              EL636
02894      MOVE EIBTIME                TO  CK-LAST-MAINT-HHMMSS.        EL636
02895      MOVE WS-PAYEE-NAME          TO  CK-PAYEE-NAME.               EL636
02896      MOVE WS-PAYEE-ADDRESS-1     TO  CK-PAYEE-ADDRESS-1.          EL636
02897      MOVE WS-PAYEE-ADDRESS-2     TO  CK-PAYEE-ADDRESS-2.          EL636
02898      MOVE WS-PAYEE-CITY-ST       TO  CK-PAYEE-CITY-ST.            EL636
02899      MOVE WS-PAYEE-ZIP-CODE      TO  CK-PAYEE-ZIP-CODE.           EL636
02900                                                                   EL636
02901      MOVE ZEROS                  TO  CK-QUE-CONTROL-NUMBER        EL636
02902                                      CK-QUE-SEQ-NO.               EL636
02903                                                                   EL636
02904      MOVE LOW-VALUES             TO  CK-CHECK-WRITTEN-DT          EL636
02905                                      CK-CREDIT-SELECT-DT          EL636
02906                                      CK-CREDIT-ACCEPT-DT          EL636
02907                                      CK-VOID-DT.                  EL636
02908                                                                      CL*26
02909      IF ERCOMP-SW IS EQUAL       TO  'P'                             CL*26
02910         MOVE ERCOMP-SW           TO  CK-ACH-PAYMENT                  CL*26
02911      ELSE                                                            CL*26
02912         MOVE 'N'                 TO  CK-ACH-PAYMENT.                 CL*26
02913                                                                   EL636
02914      EXEC CICS HANDLE CONDITION                                   EL636
02915           DUPREC  (7480-DUPREC)                                      CL*11
02916      END-EXEC.                                                    EL636
02917                                                                   EL636
02918  7425-WRITE-COMM-CHECK-RECORD.                                       CL*11
02919                                                                   EL636
02920      EXEC CICS WRITE                                              EL636
02921           DATASET   (FILE-ID-ERCMCK)                              EL636
02922           FROM      (COMM-CHECK-RECORDS)                          EL636
02923           RIDFLD    (CK-CONTROL-PRIMARY)                          EL636
02924      END-EXEC.                                                    EL636
02925                                                                   EL636
02926      GO TO 7490-EXIT.                                                CL*11
02927                                                                   EL636
02928  7480-DUPREC.                                                        CL*11
02929                                                                   EL636
02930      ADD +1                      TO CK-SEQUENCE-NO.               EL636
02931      GO TO 7425-WRITE-COMM-CHECK-RECORD.                             CL*11
02932                                                                   EL636
02933  7490-EXIT.                                                          CL*11
02934       EXIT.                                                          CL*24
02935                                                                   EL636
02936      EJECT                                                        EL636
02937                                                                      CL**4
02938 ******************************************************************   CL**4
02939 *                                                                *   CL**4
02940 *           V O I D   C O M M I S I O N   C H E C K              *   CL**4
02941 *                                                                *   CL**4
02942 ******************************************************************   CL**4
02943                                                                      CL**4
02944  7500-VOID-COMM-CHECK.                                               CL**4
02945                                                                      CL**4
02946      MOVE 'V'                    TO PI-MAINT-FUNCTION.               CL**4
02947                                                                      CL**4
02948      MOVE PI-COMPANY-CD          TO ERCMCK-COMPANY-CD.               CL**4
02949      MOVE CSRI                   TO ERCMCK-CSR.                      CL*13
02950      MOVE CARI                   TO ERCMCK-CARRIER.                  CL**4
02951      MOVE GROUPI                 TO ERCMCK-GROUPING.                 CL**4
02952      MOVE PAYEEI                 TO ERCMCK-PAYEE.                    CL**4
02953      MOVE PAYSEQI                TO ERCMCK-PAYEE-SEQ.                CL**7
02954      MOVE +0                     TO ERCMCK-SEQUENCE-NO.              CL**4
02955                                                                      CL**4
02956      EXEC CICS HANDLE CONDITION                                      CL**4
02957          NOTFND    (7550-VOID-WORK-RECS)                             CL**4
02958          ENDFILE   (7550-VOID-WORK-RECS)                             CL**4
02959      END-EXEC.                                                       CL**4
02960                                                                      CL**4
02961  7510-READ-COMM-CHECK.                                               CL**4
02962                                                                      CL**4
02963      EXEC CICS STARTBR                                               CL**4
02964          DATASET (FILE-ID-ERCMCK)                                    CL**4
02965          RIDFLD  (ERCMCK-KEY)                                        CL**4
02966      END-EXEC.                                                       CL**4
02967                                                                      CL**4
02968  7520-PROCESS-COMM-CHECK.                                            CL**4
02969                                                                      CL**4
02970      EXEC CICS READNEXT                                              CL**4
02971          SET     (ADDRESS OF COMM-CHECK-RECORDS)                     CL*24
02972          DATASET (FILE-ID-ERCMCK)                                    CL**4
02973          RIDFLD  (ERCMCK-KEY)                                        CL**4
02974      END-EXEC.                                                       CL**4
02975                                                                      CL*13
02976      IF PI-COMPANY-CD  NOT =  CK-COMPANY-CD                          CL*15
02977          GO TO      7550-VOID-WORK-RECS.                             CL*13
02978                                                                      CL**4
02979                                                                      CL**4
02980      MOVE ERCMCK-KEY             TO  SAVE-ERCMCK-CONTROL.            CL**4
02981                                                                      CL**4
02982      IF CK-COMPANY-CD = PI-SVCKWK-COMP-CD  AND                       CL**4
02983         CK-CSR        = PI-SVCKWK-CSR      AND                       CL*15
02984         CK-CARRIER    = PI-SVCKWK-CARRIER  AND                       CL**4
02985         CK-GROUPING   = PI-SVCKWK-GROUPING AND                       CL**4
02986         CK-PAYEE      = PI-SVCKWK-PAYEE    AND                       CL**7
02987         CK-PAYEE-SEQ  = PI-SVCKWK-PAYEE-SEQ                          CL**7
02988         NEXT SENTENCE                                                CL**4
02989      ELSE                                                            CL**4
02990         EXEC CICS ENDBR                                              CL**4
02991              DATASET (FILE-ID-ERCMCK)                                CL**4
02992         END-EXEC                                                     CL**4
02993         GO TO 7550-VOID-WORK-RECS.                                   CL**4
02994                                                                      CL**4
02995      IF CK-QUE-CONTROL-NUMBER = ZEROS                                CL**4
02996         NEXT SENTENCE                                                CL**4
02997      ELSE                                                            CL**4
02998         GO TO 7580-VOID-ERROR.                                       CL**4
02999                                                                      CL**4
03000      EXEC CICS ENDBR                                                 CL**4
03001           DATASET (FILE-ID-ERCMCK)                                   CL**4
03002      END-EXEC.                                                       CL**4
03003                                                                      CL**4
03004      EXEC CICS READ                                                  CL**4
03005          SET     (ADDRESS OF COMM-CHECK-RECORDS)                     CL*24
03006          DATASET (FILE-ID-ERCMCK)                                    CL**4
03007          RIDFLD  (ERCMCK-KEY)                                        CL**4
03008          UPDATE                                                      CL**4
03009      END-EXEC.                                                       CL**4
03010                                                                      CL**4
03011      EXEC CICS DELETE                                                CL**4
03012           DATASET (FILE-ID-ERCMCK)                                   CL**4
03013      END-EXEC.                                                       CL**4
03014                                                                      CL**4
03015      GO TO 7510-READ-COMM-CHECK.                                     CL**4
03016                                                                      CL**4
03017  7550-VOID-WORK-RECS.                                                CL**4
03018                                                                      CL**4
03019      EXEC CICS HANDLE CONDITION                                      CL**4
03020          NOTFND    (7570-COMM-CHECK-VOIDED)                          CL**4
03021          ENDFILE   (7570-COMM-CHECK-VOIDED)                          CL**4
03022      END-EXEC.                                                       CL**4
03023                                                                      CL**4
03024      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.               CL**4
03025      MOVE CSRI                   TO ERCKWK-CSR.                      CL*13
03026      MOVE CARI                   TO ERCKWK-CARRIER.                  CL**4
03027      MOVE GROUPI                 TO ERCKWK-GROUPING.                 CL**4
03028      MOVE PAYEEI                 TO ERCKWK-PAYEE.                    CL**4
03029      MOVE PAYSEQI                TO ERCKWK-PAYEE-SEQ.                CL**7
03030      MOVE +0                     TO ERCKWK-SEQUENCE-NO.              CL**4
03031                                                                      CL**4
03032  7560-READ-CHECK-WORK-FILE.                                          CL**4
03033                                                                      CL**4
03034      EXEC CICS STARTBR                                               CL**4
03035          DATASET (FILE-ID-ERCKWK)                                    CL**4
03036          RIDFLD  (ERCKWK-KEY)                                        CL**4
03037      END-EXEC.                                                       CL**4
03038                                                                      CL**4
03039      MOVE 'Y'                    TO WS-BROWSE-SW.                    CL*24
03040                                                                      CL*24
03041  7565-PROCESS-WORK-FILE.                                             CL**4
03042                                                                      CL**4
03043      EXEC CICS READNEXT                                              CL**4
03044          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
03045          DATASET (FILE-ID-ERCKWK)                                    CL**4
03046          RIDFLD  (ERCKWK-KEY)                                        CL**4
03047      END-EXEC.                                                       CL**4
03048                                                                      CL**4
03049      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD                          CL*13
03050          GO TO      7570-COMM-CHECK-VOIDED.                          CL*13
03051                                                                      CL*13
03052      IF ERCKWK-KEY = SAVE-ERCKWK-CONTROL                             CL**4
03053         GO TO 7565-PROCESS-WORK-FILE.                                CL**4
03054                                                                      CL**4
03055      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.             CL**4
03056                                                                      CL**4
03057      IF CW-COMPANY-CD = PI-SVCKWK-COMP-CD  AND                       CL**4
03058         CW-CSR        = PI-SVCKWK-CSR      AND                       CL*13
03059         CW-CARRIER    = PI-SVCKWK-CARRIER  AND                       CL**4
03060         CW-GROUPING   = PI-SVCKWK-GROUPING AND                       CL**4
03061         CW-PAYEE      = PI-SVCKWK-PAYEE    AND                       CL**7
03062         CW-PAYEE-SEQ  = PI-SVCKWK-PAYEE-SEQ                          CL**7
03063         NEXT SENTENCE                                                CL**4
03064      ELSE                                                            CL**4
03065         EXEC CICS ENDBR                                              CL**4
03066              DATASET (FILE-ID-ERCKWK)                                CL**4
03067         END-EXEC                                                     CL**4
03068         MOVE 'N'                 TO WS-BROWSE-SW                     CL*24
03069         GO TO 7570-COMM-CHECK-VOIDED.                                CL**4
03070                                                                      CL**4
03071      EXEC CICS ENDBR                                                 CL**4
03072           DATASET (FILE-ID-ERCKWK)                                   CL**4
03073      END-EXEC.                                                       CL**4
03074                                                                      CL**4
03075      EXEC CICS READ                                                  CL**4
03076          SET     (ADDRESS OF CHECK-WORK-RECORDS)                     CL*24
03077          DATASET (FILE-ID-ERCKWK)                                    CL**4
03078          RIDFLD  (ERCKWK-KEY)                                        CL**4
03079          UPDATE                                                      CL**4
03080      END-EXEC.                                                       CL**4
03081                                                                      CL**4
03082      MOVE LOW-VALUES             TO  CW-RELEASE-DT.                  CL**4
03083                                                                      CL**4
03084      EXEC CICS REWRITE                                               CL**4
03085           FROM    (CHECK-WORK-RECORDS)                               CL**4
03086           DATASET (FILE-ID-ERCKWK)                                   CL**4
03087      END-EXEC.                                                       CL**4
03088                                                                      CL**4
03089      GO TO 7560-READ-CHECK-WORK-FILE.                                CL**4
03090                                                                      CL**4
03091  7570-COMM-CHECK-VOIDED.                                             CL**4
03092                                                                      CL*24
03093      IF WS-BROWSE-SW = 'Y'                                           CL*24
03094          MOVE 'N'                TO  WS-BROWSE-SW                    CL*24
03095          EXEC CICS ENDBR                                             CL*24
03096               DATASET (FILE-ID-ERCKWK)                               CL*24
03097          END-EXEC.                                                   CL*24
03098                                                                      CL**4
03099      IF PI-NO-ENTRIES-DISPLAYED GREATER THAN ZEROS                   CL**4
03100         MOVE PI-STUB-KEY (1)        TO PI-ERCKWK-KEY.                CL**4
03101                                                                      CL**4
03102      MOVE ER-0000                TO EMI-ERROR.                       CL**4
03103      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
03104      GO TO 5000-DISPLAY-PAYEE.                                       CL**4
03105                                                                      CL**4
03106  7580-VOID-ERROR.                                                    CL**4
03107                                                                      CL**4
03108      EXEC CICS ENDBR                                                 CL**4
03109           DATASET (FILE-ID-ERCMCK)                                   CL**4
03110      END-EXEC.                                                       CL**4
03111                                                                      CL**4
03112      MOVE ER-3169                TO EMI-ERROR.                       CL**4
03113      MOVE -1                     TO CARL.                            CL**4
03114      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
03115      GO TO 8200-SEND-DATAONLY.                                       CL**4
03116                                                                      CL**4
03117      EJECT                                                           CL**4
03118                                                                      CL**4
03119 ******************************************************************EL636
03120 *                                                                *EL636
03121 *             S E N D    I N I T I A L   M A P                   *EL636
03122 *                                                                *EL636
03123 ******************************************************************EL636
03124                                                                   EL636
03125  8100-SEND-INITIAL-MAP.                                           EL636
03126                                                                   EL636
03127      MOVE WS-CURRENT-DT          TO DATEO.                        EL636
03128      MOVE EIBTIME                TO TIME-IN.                      EL636
03129      MOVE TIME-OUT               TO TIMEO.                        EL636
03130      MOVE -1                     TO MAINTL.                       EL636
03131                                                                   EL636
03132      MOVE EMI-MESSAGE-AREA (1)   TO ERMESG1O.                        CL**7
03133      MOVE EMI-MESSAGE-AREA (2)   TO ERMESG2O.                        CL**7
03134                                                                   EL636
03135                                                                      CL*12
03136      MOVE AL-SANON               TO  PFKEY8A.                        CL*12
03137                                                                      CL*11
03138      EXEC CICS SEND                                               EL636
03139          MAP      (EL636A)                                        EL636
03140          MAPSET   (MAPSET-EL636S)                                 EL636
03141          FROM     (EL636AI)                                       EL636
03142          ERASE                                                    EL636
03143          CURSOR                                                   EL636
03144      END-EXEC.                                                    EL636
03145                                                                   EL636
03146      GO TO 9100-RETURN-TRAN.                                      EL636
03147                                                                   EL636
03148      EJECT                                                        EL636
03149                                                                   EL636
03150 ******************************************************************EL636
03151 *                                                                *EL636
03152 *              S E N D    D A T A O N L Y                        *EL636
03153 *                                                                *EL636
03154 ******************************************************************EL636
03155                                                                   EL636
03156  8200-SEND-DATAONLY.                                              EL636
03157                                                                   EL636
03158      MOVE WS-CURRENT-DT          TO DATEO.                        EL636
03159      MOVE EIBTIME                TO TIME-IN.                      EL636
03160      MOVE TIME-OUT               TO TIMEO.                        EL636
03161                                                                   EL636
03162      MOVE EMI-MESSAGE-AREA (1)   TO ERMESG1O.                        CL**7
03163      MOVE EMI-MESSAGE-AREA (2)   TO ERMESG2O.                        CL**7
03164                                                                   EL636
03165      EXEC CICS SEND                                               EL636
03166           MAP      (EL636A)                                       EL636
03167           MAPSET   (MAPSET-EL636S)                                EL636
03168           FROM     (EL636AI)                                      EL636
03169           DATAONLY                                                EL636
03170           CURSOR                                                  EL636
03171      END-EXEC.                                                    EL636
03172                                                                   EL636
03173      GO TO 9100-RETURN-TRAN.                                      EL636
03174                                                                   EL636
03175      EJECT                                                        EL636
03176                                                                   EL636
03177  8300-SEND-TEXT.                                                  EL636
03178      EXEC CICS SEND TEXT                                          EL636
03179          FROM     (LOGOFF-TEXT)                                   EL636
03180          LENGTH   (LOGOFF-LENGTH)                                 EL636
03181          ERASE                                                    EL636
03182          FREEKB                                                   EL636
03183      END-EXEC.                                                    EL636
03184                                                                   EL636
03185      EXEC CICS RETURN                                             EL636
03186      END-EXEC.                                                    EL636
03187                                                                   EL636
03188                                                                   EL636
03189  8400-LOG-JOURNAL-RECORD.                                         EL636
03190      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL636
03191      MOVE THIS-PGM                TO JP-PROGRAM-ID.               EL636
03192                                                                   EL636
03193 *    EXEC CICS JOURNAL                                            EL636
03194 *        JFILEID     (PI-JOURNAL-FILE-ID)                         EL636
03195 *        JTYPEID     ('EL')                                       EL636
03196 *        FROM        (JOURNAL-RECORD)                             EL636
03197 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   EL636
03198 *        END-EXEC.                                                EL636
03199                                                                   EL636
03200  8500-DATE-CONVERT.                                               EL636
03201      EXEC CICS LINK                                               EL636
03202          PROGRAM  (LINK-ELDATCV)                                  EL636
03203          COMMAREA (DATE-CONVERSION-DATA)                          EL636
03204          LENGTH   (DC-COMM-LENGTH)                                EL636
03205      END-EXEC.                                                    EL636
03206                                                                   EL636
03207  8500-EXIT.                                                       EL636
03208      EXIT.                                                        EL636
03209                                                                   EL636
03210      EJECT                                                        EL636
03211                                                                   EL636
03212  8600-DEEDIT.                                                     EL636
03213                                                                   EL636
03214      EXEC CICS BIF DEEDIT                                         EL636
03215           FIELD   (DEEDIT-FIELD)                                  EL636
03216           LENGTH  (12)                                            EL636
03217      END-EXEC.                                                    EL636
03218                                                                   EL636
03219  8800-UNAUTHORIZED-ACCESS.                                        EL636
03220      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL636
03221      GO TO 8300-SEND-TEXT.                                        EL636
03222                                                                   EL636
03223  8810-PF23.                                                       EL636
03224      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL636
03225      MOVE XCTL-EL005             TO PGM-NAME.                     EL636
03226      GO TO 9300-XCTL.                                                CL*11
03227                                                                      CL*11
03228  8820-PF08.                                                          CL*11
03229      MOVE PI-LAST-STUB-KEY       TO PI-ERCKWK-KEY.                   CL*11
03230      MOVE ZEROS                  TO PI-ERCKWK-SEQ-NO.                CL*11
03231      MOVE EIBAID                 TO PI-ENTRY-CD-1.                   CL*11
03232      MOVE XCTL-EL6361            TO PGM-NAME.                        CL*11
03233      GO TO 9300-XCTL.                                             EL636
03234                                                                   EL636
03235  9200-RETURN-MAIN-MENU.                                           EL636
03236      MOVE XCTL-EL626             TO PGM-NAME.                     EL636
03237      GO TO 9300-XCTL.                                             EL636
03238                                                                   EL636
03239  9100-RETURN-TRAN.                                                EL636
03240                                                                   EL636
03241      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL636
03242      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.         EL636
03243                                                                   EL636
03244      EXEC CICS RETURN                                             EL636
03245          TRANSID    (TRANS-EXJA)                                  EL636
03246          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL636
03247          LENGTH     (PI-COMM-LENGTH)                              EL636
03248      END-EXEC.                                                    EL636
03249                                                                   EL636
03250  9300-XCTL.                                                       EL636
03251      EXEC CICS XCTL                                               EL636
03252          PROGRAM    (PGM-NAME)                                    EL636
03253          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL636
03254          LENGTH     (PI-COMM-LENGTH)                              EL636
03255      END-EXEC.                                                    EL636
03256                                                                   EL636
03257  9400-CLEAR.                                                      EL636
03258      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL636
03259      GO TO 9300-XCTL.                                             EL636
03260                                                                   EL636
03261  9500-PF12.                                                       EL636
03262      MOVE XCTL-EL010             TO PGM-NAME.                     EL636
03263      GO TO 9300-XCTL.                                             EL636
03264                                                                   EL636
03265  9600-PGMID-ERROR.                                                EL636
03266                                                                   EL636
03267      EXEC CICS HANDLE CONDITION                                   EL636
03268          PGMIDERR    (8300-SEND-TEXT)                             EL636
03269      END-EXEC.                                                    EL636
03270                                                                   EL636
03271      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL636
03272      MOVE ' '                    TO PI-ENTRY-CD-1.                EL636
03273      MOVE XCTL-EL005             TO PGM-NAME.                     EL636
03274      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL636
03275      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL636
03276      GO TO 9300-XCTL.                                             EL636
03277                                                                   EL636
03278  9900-ERROR-FORMAT.                                               EL636
03279                                                                   EL636
03280      IF NOT EMI-ERRORS-COMPLETE                                   EL636
03281          MOVE LINK-EL001         TO PGM-NAME                      EL636
03282          EXEC CICS LINK                                           EL636
03283              PROGRAM    (PGM-NAME)                                EL636
03284              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL636
03285              LENGTH     (EMI-COMM-LENGTH)                         EL636
03286          END-EXEC.                                                EL636
03287                                                                   EL636
03288  9900-EXIT.                                                       EL636
03289      EXIT.                                                        EL636
03290                                                                   EL636
03291  9990-ABEND.                                                      EL636
03292      MOVE LINK-EL004             TO PGM-NAME.                     EL636
03293      MOVE DFHEIBLK               TO EMI-LINE1.                    EL636
03294      EXEC CICS LINK                                               EL636
03295          PROGRAM   (PGM-NAME)                                     EL636
03296          COMMAREA  (EMI-LINE1)                                    EL636
03297          LENGTH    (72)                                           EL636
03298      END-EXEC.                                                    EL636
03299                                                                   EL636
03300      MOVE -1                     TO PFENTERL.                     EL636
03301                                                                   EL636
03302      GO TO 8200-SEND-DATAONLY.                                    EL636
03303                                                                   EL636
03304      EJECT                                                        EL636
03305                                                                   EL636
03306  9995-SECURITY-VIOLATION.                                         EL636
03307                              COPY ELCSCTP.                        EL636
03308                                                                   EL636
03309  9995-EXIT.                                                       EL636
03310      EXIT.                                                        EL636
03311                                                                   EL636
