00001  IDENTIFICATION DIVISION.                                         03/08/96
00002                                                                   EL602
00003  PROGRAM-ID.                 EL602 .                                 LV015
00004 *              PROGRAM CONVERTED BY                                  CL*15
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*15
00006 *              CONVERSION DATE 02/12/96 16:56:43.                    CL*15
00007 *                            VMOD=2.015                              CL*15
00008 *                                                                 EL602
00009 *AUTHOR.        LOGIC,INC.                                           CL*15
00010 *               DALLAS, TEXAS.                                       CL*15
00011                                                                   EL602
00012 *DATE-COMPILED.                                                      CL*15
00013                                                                   EL602
00014 *SECURITY.   *****************************************************   CL*15
00015 *            *                                                   *   CL*15
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*15
00017 *            *                                                   *   CL*15
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*15
00019 *                                                                *   CL*15
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*15
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*15
00022 *            *                                                   *   CL*15
00023 *            *****************************************************   CL*15
00024                                                                   EL602
00025 *REMARKS.                                                            CL**2
00026 *        TRANSACTION - EXA2 - MORTALITY TABLE CONTROLS.              CL**4
00027                                                                   EL602
010413******************************************************************
010413*                   C H A N G E   L O G
010413*
010413* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010413*-----------------------------------------------------------------
010413*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010413* EFFECTIVE    NUMBER
010413*-----------------------------------------------------------------
010413* 010413  CR2012072400002  PEMA  INCREASE MAX # TBLS ALLOWED
010413******************************************************************
00028  ENVIRONMENT DIVISION.                                            EL602
00029  DATA DIVISION.                                                   EL602
00030                                  EJECT                               CL**4
00031  WORKING-STORAGE SECTION.                                         EL602
00032  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.         CL*15
00033  77  FILLER  PIC X(032) VALUE '********************************'.    CL**4
00034  77  FILLER  PIC X(032) VALUE '*    EL602 WORKING STORAGE     *'.    CL**4
00035  77  FILLER  PIC X(032) VALUE '************ V/M 2.015 *********'.    CL*15
00036                                                                   EL602
00037                                  EJECT                               CL**4
00038  01  W-PROGRAM-WORK-AREA.                                            CL**4
00039      12  FILLER                  PIC  X(17)                          CL**4
00040                              VALUE 'PROGRAM WORK AREA'.              CL**4
00041      12  SC-ITEM-CL-CR           PIC S9(04) COMP  VALUE +1.          CL*13
00042      12  W-APPL-SCRTY-NDX        PIC S9(04) COMP  VALUE +4.          CL*13
00043      12  W-ASKTIME-CTR           PIC S9(04) COMP  VALUE ZEROS.       CL**4
00044      12  W-JOURNAL-LENGTH        PIC S9(04) COMP  VALUE ZEROS.       CL**4
00045      12  W-LINE-NUMBER           PIC S9(04) COMP  VALUE ZEROS.       CL**4
00046      12  W-NEXT-TABLE-LINE       PIC S9(04) COMP  VALUE ZEROS.       CL**4
00047      12  W-REAL-TABLE-LINE       PIC S9(04) COMP  VALUE ZEROS.       CL**4
00048      12  W-WORK-CTR              PIC S9(04) COMP.                    CL**4
00049      12  W-WORK-FIELD            PIC S9(03) COMP-3.                  CL**4
00050                                                                   EL602
00051      12  W-CHECK-PFKEYS          PIC  9(02).                         CL**4
00052      12  W-DISPLAY-LINE-NUMBER.                                      CL**4
010413         16  W-EDITED-LINE-NUMB  PIC  Z99.                           CL**4
010413*        16  FILLER              PIC  X(01)       VALUE '.'.         CL**4
00055      12  W-HOLD-INTEREST         PIC V9(04)       VALUE ZEROS.       CL**4
00056      12  W-HOLD-JOINT-FACTOR     PIC  9(01)V9(04) VALUE ZEROS.       CL**4
00057      12  W-HOLD-RESERVE-ADJ      PIC  9(01)V9(04) VALUE ZEROS.       CL**4
00058      12  W-LAST-ERROR            PIC  9(04)       VALUE 9999.        CL**4
00059                                                                   EL602
00060      12  W-CALL-PGM              PIC  X(08).                         CL**4
00061      12  W-COMP-CD-R.                                                CL**4
00062          16  FILLER              PIC  X(01).                         CL**4
00063          16  W-COMP-CD-X         PIC  X(01).                         CL**4
00064      12  W-COMP-CD   REDEFINES                                       CL**4
00065          W-COMP-CD-R             PIC S9(04)               COMP.      CL**4
00066                                                                   EL602
00067      12  W-DEEDIT-FIELD          PIC  X(15).                         CL**4
00068      12  W-DEEDIT-FIELD-V0 REDEFINES                                 CL**4
00069          W-DEEDIT-FIELD          PIC S9(15).                         CL**4
00070      12  W-DEEDIT-FIELD-V1 REDEFINES                                 CL**4
00071          W-DEEDIT-FIELD          PIC S9(14)V9(01).                   CL**4
00072      12  W-DEEDIT-FIELD-V2 REDEFINES                                 CL**4
00073          W-DEEDIT-FIELD          PIC S9(13)V9(02).                   CL**4
00074      12  W-DEEDIT-FIELD-V5 REDEFINES                                 CL**4
00075          W-DEEDIT-FIELD          PIC S9(10)V9(05).                   CL**4
00076                                                                   EL602
00077      12  W-INTEREST-N            PIC V9(04).                         CL**4
00078      12  W-INTEREST-R   REDEFINES  W-INTEREST-N.                     CL**4
00079          16  FILLER              PIC  X(01).                         CL**4
00080          16  W-INT               PIC  X(02).                         CL**4
00081          16  FILLER              PIC  X(01).                         CL**4
00082                                                                   EL602
00083      12  W-MORT-CODE.                                                CL**4
00084          16  W-MORT-TBL          PIC  X(01).                         CL**4
00085          16  W-MORT-INT          PIC  X(02).                         CL**4
00086          16  W-MORT-TYP          PIC  X(01).                         CL**4
00087      12  W-SAVE-DATE             PIC  X(08)  VALUE SPACES.           CL**4
00088      12  W-SAVE-BIN-DATE         PIC  X(02)  VALUE SPACES.           CL**4
00089      12  W-TEMP-LINE             PIC  X(53).                         CL**4
00090                                                                      CL**4
00091      12  W-TIME-IN               PIC S9(07).                         CL**4
00092      12  W-TIME-OUT-R REDEFINES W-TIME-IN.                           CL**4
00093          16  FILLER              PIC  X(01).                         CL**4
00094          16  W-TIME-OUT          PIC  9(02)V9(02).                   CL**4
00095          16  FILLER              PIC  X(02).                         CL**4
00096                                                                      CL**4
00097                                  EJECT                               CL**4
00098  01  W-PROGRAM-KEY-AREAS.                                            CL**4
00099      12  FILLER                  PIC  X(10)  VALUE 'KEY AREAS:'.     CL**4
00100                                                                      CL**4
00101      12  W-QUID-KEY.                                                 CL**4
00102          16  W-QUID-TERMINAL     PIC  X(04).                         CL**4
00103          16  W-QUID-MAP-NUM      PIC  X(04)  VALUE '602A'.           CL**4
00104                                                                      CL**4
00105      12  W-WORKING-CNTL-KEY.                                         CL**4
00106          16  W-CNTL-COMPANY-ID   PIC  X(03).                         CL**4
00107          16  W-CNTL-RECORD-TYPE  PIC  X(01)  VALUE '7'.              CL**4
00108          16  FILLER              PIC  X(04)  VALUE SPACES.           CL**4
00109          16  W-CNTL-SEQUENCE-NO  PIC S9(04)   COMP.                  CL**4
00110                                  EJECT                               CL**4
00111  01  W-PROGRAM-SWITCHES-AND-TESTS.                                   CL**4
00112      12  FILLER                  PIC  X(15)                          CL**4
00113                                  VALUE 'SWITCHES/TESTS:'.            CL**4
00114                                                                      CL**4
00115      12  W-CHECK-MAINT           PIC  X(01).                         CL**4
00116          88  W-VALID-OPTION      VALUE 'A' 'C' 'D' 'S' 'I'.          CL**4
00117      12  W-EXCHANGE-MADE-IND     PIC  X(01)  VALUE ' '.              CL**4
00118          88  W-EXCHANGE-MADE                 VALUE 'Y'.              CL**4
00119      12  W-FIRST-TIME-SW         PIC  X(01)  VALUE ' '.              CL**4
00120          88  W-FIRST-TIME                    VALUE 'Y'.              CL**4
00121      12  W-LAST-MORT-READ-IND    PIC  X(01)  VALUE ' '.              CL**4
00122          88  W-LAST-MORT-RECORD-READ         VALUE 'Y'.              CL**4
00123      12  W-VALID-JOINT-CODE-IND  PIC  X(01).                         CL**4
00124          88  W-VALID-JOINT-CODE         VALUE 'A' 'V'                CL**4
00125                                         SPACES LOW-VALUES.           CL**4
00126      12  W-VALID-TYPE-IND        PIC  X(05).                         CL**4
00127          88  W-TYPE-VALID-C             VALUE 'J' 'S'.               CL**4
00128          88  W-TYPE-VALID-M             VALUE 'J' 'S' 'C'.           CL**4
00129      12  W-SUPPORTED-TABLES-IND  PIC  X(05).                         CL**4
00130          88  W-TABLE-SUPPORTED          VALUE '41CSO' '58CET'        CL**4
00131                                               '58CSO' '60CSG'        CL**4
00132                                               '58FSO' '80MSO'        CL**4
00125                                               '58CSO' '58FSO'          000
CIDMOD                                              '58UET' '58USO'          000
CIDMOD                                              '80CSO' '80UET'          000
CIDMOD                                              '80FSO' '80MET'        CL**4
00134                                               '80FET' '80GBT'        CL*11
101005                                              '80USO' '01CSO'        CL*11
00135                                               'ZERO ' 'XXXXX'.       CL**4
00136          88  W-NULL-ENTRY               VALUE 'ZERO '.               CL**7
00137  01  FILLER                      PIC  X(23)                          CL**4
00138                              VALUE 'PROGRAM INTERFACE START'.        CL**4
00139      COPY ELCINTF.                                                   CL**4
00140      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                    CL**4
00141          16  PI-CURSOR           PIC S9(08)           COMP.          CL**4
00142          16  PI-FIRST-CNTL-KEY   PIC  X(10).                         CL**4
00143          16  PI-FIRST-TIME-IND   PIC  X(01).                         CL**4
00144              88  PI-FIRST-TIME               VALUE LOW-VALUES.       CL**4
00145              88  PI-NOT-FIRST-TIME           VALUE 'N'.              CL**4
00146          16  PI-LAST-CNTL-KEY    PIC  X(10).                         CL**4
00147          16  PI-LAST-INSERTED-LINE                                   CL**4
00148                                  PIC S9(04)           COMP.          CL**4
00149          16  PI-LAST-MNT-DATE    PIC  X(02).                         CL**4
00150          16  PI-LAST-MNT-DATE-ALPHA                                  CL**4
00151                                  PIC  X(08).                         CL**4
00152          16  PI-LAST-MNT-PROCESSOR                                   CL**4
010413                                 PIC  X(04).                         CL**4
00154          16  PI-LAST-MNT-TIME    PIC S9(06)           COMP.          CL**4
00155          16  PI-LAST-MORT-TBL    PIC S9(04)           COMP.          CL**4
00156          16  PI-LAST-SEQ-NO      PIC S9(04)           COMP.          CL**4
00157          16  PI-MAINT            PIC  X(01).                         CL**4
00158          16  PI-MODIFICATIONS-MADE-IND                               CL**4
00159                                  PIC  X(01).                         CL**4
00160              88  PI-MODIFICATIONS-MADE       VALUE 'Y'.              CL**4
00161          16  PI-PASS-SW          PIC  X(01).                         CL**4
00162              88  PI-2ND-TIME-PAST-END        VALUE 'Y'.              CL**5
00163          16  PI-TABLE-LINE       PIC S9(04)           COMP.          CL**4
00164          16  PI-TOTAL-MORT-LINES PIC S9(04)           COMP.          CL**4
00165          16  PI-WORK-TABLE-LINE  PIC S9(04)           COMP.          CL**4
00166          16  FILLER              PIC X(583).                         CL*15
00167                                  EJECT                               CL**4
00168  01  FILLER                      PIC  X(17)                          CL**4
00169                              VALUE 'PROGRAM MAP START'.              CL**4
00170      COPY EL602S.                                                    CL**4
00171                                  EJECT                               CL**4
00172  01  W-MAP-IN REDEFINES EL602AI.                                     CL**4
010413     12  FILLER                  PIC  X(79).                         CL**4
00174      12  W-TABLE-INPUT OCCURS  14  TIMES                             CL**4
00175                              INDEXED BY W-TBLI-NDX                   CL**4
00176                                         W-TBLI-NDX2                  CL**4
00177                                         W-DUPTBL-NDX.                CL**4
00178          16  W-LINE-L            PIC S9(04)               COMP.      CL**4
00179          16  W-LINE-A            PIC  X(01).                         CL**4
00180          16  W-LINE-I            PIC  X(03).                         CL**4
00181          16  W-TABLE-INDAT.                                          CL**4
00182              20  W-TABLE-L       PIC S9(04)               COMP.      CL**4
00183              20  W-TABLE-A       PIC  X(01).                         CL**4
00184              20  W-TABLE-I       PIC  X(05).                         CL**4
00185              20  W-TBLTP-L       PIC S9(04)               COMP.      CL**4
00186              20  W-TBLTP-A       PIC  X(01).                         CL**4
00187              20  W-TBLTP-I       PIC  X(01).                         CL**4
00188              20  W-INTR-L        PIC S9(04)               COMP.      CL**4
00189              20  W-INTR-A        PIC  X(01).                         CL**4
00190              20  W-INTR-I        PIC  9(01)V9(04).                   CL**4
00191              20  W-ANAL-L        PIC S9(04)               COMP.      CL**4
00192              20  W-ANAL-A        PIC  X(01).                         CL**4
00193              20  W-ANAL-I        PIC  X(02).                         CL**4
00194              20  W-RSADJ-L       PIC S9(04)               COMP.      CL**4
00195              20  W-RSADJ-A       PIC  X(01).                         CL**4
00196              20  W-RSADJ-I       PIC  9(02)V9(04).                   CL**4
00197              20  W-ADJDI-L       PIC S9(04)               COMP.      CL**4
00198              20  W-ADJDI-A       PIC  X(01).                         CL**4
00199              20  W-ADJDI-I       PIC  X(01).                         CL**4
00200              20  W-JNTFC-L       PIC S9(04)               COMP.      CL**4
00201              20  W-JNTFC-A       PIC  X(01).                         CL**4
00202              20  W-JNTFC-I       PIC  9(02)V9(04).                   CL**4
00203              20  W-JNTCD-L       PIC S9(04)               COMP.      CL**4
00204              20  W-JNTCD-A       PIC  X(01).                         CL**4
00205              20  W-JNTCD-I       PIC  X(01).                         CL**4
00206              20  W-PCQ-L         PIC S9(04)               COMP.      CL**4
00207              20  W-PCQ-A         PIC  X(01).                         CL**4
00208              20  W-PCQ-I         PIC  X(01).                         CL**4
00209              20  W-MORTC-L       PIC S9(04)               COMP.      CL**4
00210              20  W-MORTC-A       PIC  X(01).                         CL**4
00211              20  W-MORTC-I       PIC  X(04).                         CL**4
00212              20  W-COMM-L        PIC S9(04)               COMP.      CL**4
00213              20  W-COMM-A        PIC  X(01).                         CL**4
00214              20  W-COMM-I        PIC  X(15).                         CL**4
00215  01  W-MAPOUT REDEFINES EL602AI.                                     CL**4
00216      12  FILLER                  PIC  X(70).                         CL**4
010413     12  W-LINSEX1-O             PIC  X(03).                         CL**4
00218      12  FILLER                  PIC  X(03).                         CL**4
010413     12  W-LINSEX2-O             PIC  X(03).                         CL**4
00220      12  W-TABLE-OUTPUT OCCURS 14  TIMES                             CL**4
00221                              INDEXED BY W-TBLO-NDX.                  CL**4
00222          16  FILLER              PIC  X(03).                         CL**4
00223          16  W-LINE-O            PIC  X(03).                         CL**4
00224          16  FILLER              PIC  X(03).                         CL**4
00225          16  W-TABLE-O           PIC  X(05).                         CL**4
00226          16  FILLER              PIC  X(03).                         CL**4
00227          16  W-TBLTP-O           PIC  X(01).                         CL**4
00228          16  FILLER              PIC  X(03).                         CL**4
00229          16  W-INTR-O            PIC  .9(04).                        CL**4
00230          16  W-INTR-X-O REDEFINES W-INTR-O                           CL**4
00231                                  PIC  X(05).                         CL**4
00232          16  FILLER              PIC  X(03).                         CL**4
00233          16  W-ANAL-O            PIC  X(02).                         CL**4
00234          16  FILLER              PIC  X(03).                         CL**4
00235          16  W-RSADJ-O           PIC  9(01).9(04).                   CL**4
00236          16  W-RSADJ-X-O REDEFINES W-RSADJ-O                         CL**4
00237                                  PIC  X(06).                         CL**4
00238          16  FILLER              PIC  X(03).                         CL**4
00239          16  W-ADJDI-O           PIC  X(01).                         CL**4
00240          16  FILLER              PIC  X(03).                         CL**4
00241          16  W-JNTFC-O           PIC  9(01).9(04).                   CL**4
00242          16  W-JNTFC-X-O REDEFINES W-JNTFC-O                         CL**4
00243                                  PIC  X(06).                         CL**4
00244          16  FILLER              PIC  X(03).                         CL**4
00245          16  W-JNTCD-O           PIC  X(01).                         CL**4
00246          16  FILLER              PIC  X(03).                         CL**4
00247          16  W-PCQ-O             PIC  X(01).                         CL**4
00248          16  FILLER              PIC  X(03).                         CL**4
00249          16  W-MORTC-O           PIC  X(04).                         CL**4
00250          16  FILLER              PIC  X(03).                         CL**4
00251          16  W-COMM-O            PIC  X(15).                         CL**4
00252                                  EJECT                               CL**4
00253  01  FILLER                      PIC  X(15)                          CL**4
00254                              VALUE 'MORTALITY TABLE'.                CL**4
00255  01  W-MORTALITY-TABLE.                                              CL**4
010413     12  W-MORT-TBL-LINE OCCURS 153 TIMES
00257                           INDEXED BY W-MORT-NDX                      CL**4
00258                                      W-MORT-NDX2.                    CL**4
00259          16  W-TABLE             PIC  X(05).                         CL**4
00260          16  W-TABLE-TYPE        PIC  X(01).                         CL**4
00261              88  W-MT-JOINT                 VALUE 'J'.               CL**4
00262              88  W-MT-SINGLE                VALUE 'S'.               CL**4
00263              88  W-MT-COMBINED              VALUE 'C'.               CL**4
00264              88  W-MT-TYPE-VALID-C          VALUE 'J' 'S'.           CL**4
00265              88  W-MT-TYPE-VALID-M          VALUE 'J' 'S' 'C'.       CL**4
00266          16  W-INTEREST          PIC SV9(04)           COMP-3.       CL**4
00267          16  W-AGE-METHOD        PIC  X(02).                         CL**4
00268              88  W-AGE-LAST                 VALUE 'AL'.              CL**4
00269              88  W-AGE-NEAR                 VALUE 'AN'.              CL**4
00270          16  W-RESERVE-ADJUSTMENT                                    CL**4
00271                                  PIC S9(01)V9(04)      COMP-3.       CL**4
00272          16  W-ADJUSTMENT-DIRECTION                                  CL**4
00273                                  PIC  X(01).                         CL**4
00274              88  W-MT-MINUS                 VALUE '-'.               CL**4
00275              88  W-MT-PLUS                  VALUE '+'.               CL**4
00276          16  W-JOINT-FACTOR      PIC S9(01)V9(04)      COMP-3.       CL**4
00277          16  W-JOINT-CODE        PIC  X(01).                         CL**4
00278              88  W-MT-VALID-JOINT-CODE      VALUE 'A' 'V'.           CL**4
00279          16  W-PC-Q              PIC  X(01).                         CL**4
00280              88  W-MT-VALID-PC-Q            VALUE 'Y' 'N' ' '.       CL**4
00281          16  W-MORTALITY-CODE    PIC  X(04).                         CL**4
00282          16  W-COMMENTS          PIC  X(15).                         CL**4
00283          16  FILLER              PIC  X(14).                         CL**4
00284  01  FILLER REDEFINES W-MORTALITY-TABLE.                             CL**4
010413     12  W-MORT-RECORD    OCCURS 17 TIMES
00286                           INDEXED BY W-MORTR-NDX                     CL**4
00287                                  PIC X(477).                         CL**4
00288                                  EJECT                               CL**4
00289  01  CONSTANT-AREAS.                                                 CL**4
00290      12  FILLER                  PIC  X(17)                          CL**4
00291                              VALUE 'PROGRAM CONSTANTS'.              CL**4
00292      12  W-CNTL-LENGTH           PIC S9(04)  VALUE +750   COMP.      CL*10
00293      12  W-CNTL-JOURNAL-LENGTH   PIC S9(04)  VALUE +773   COMP.      CL*10
00294      12  W-ITEM                  PIC S9(04)  VALUE +1     COMP.      CL**4
010413     12  W-MORT-TBL-LENGTH       PIC S9(04)  VALUE +8109  COMP.      CL**4
00296      12  W-ZERO                  PIC S9(04)  VALUE +0     COMP.      CL**4
00297                                                                      CL**4
00298      12  W-CNTL-FILE-ID          PIC  X(08)  VALUE 'ELCNTL'.         CL**4
00299      12  W-GETMAIN-SPACE         PIC  X(01)  VALUE SPACE.            CL**4
00300      12  W-MAP.                                                      CL**4
00301          16  FILLER              PIC  X(02)  VALUE 'EL'.             CL**4
00302          16  W-MAP-NUM           PIC  X(03)  VALUE '602'.            CL**4
00303          16  FILLER              PIC  X(03)  VALUE 'A'.              CL**4
00304      12  W-MAPSET                PIC  X(08)  VALUE 'EL602S'.         CL**4
00305      12  W-THIS-PGM              PIC  X(08)  VALUE 'EL602'.          CL**4
00306      12  W-TRANSACTION           PIC  X(04)  VALUE 'EXA2'.           CL**4
00307      12  W-XCTL-005              PIC  X(08)  VALUE 'EL005'.          CL**4
00308      12  W-XCTL-010              PIC  X(08)  VALUE 'EL010'.          CL**4
00309      12  W-XCTL-EL126            PIC  X(08)  VALUE 'EL126'.          CL**4
00310      12  W-XCTL-EL626            PIC  X(08)  VALUE 'EL626'.          CL**4
00311      12  W-XCTL-EM626            PIC  X(08)  VALUE 'EM626'.          CL**4
00312      12  W-XCTL-GL800            PIC  X(08)  VALUE 'GL800'.          CL**4
00313      12  W-LINK-CLDATCV          PIC  X(08)  VALUE 'ELDATCV'.        CL**4
00314      12  W-LINK-001              PIC  X(08)  VALUE 'EL001'.          CL**4
00315      12  W-LINK-004              PIC  X(08)  VALUE 'EL004'.          CL**4
00316                                  EJECT                               CL**4
00317  01  ERROR-MESSAGES.                                                 CL**4
00318      12  ER-0000                 PIC  X(04)  VALUE '0000'.           CL**4
00319      12  ER-0004                 PIC  X(04)  VALUE '0004'.           CL**4
00320      12  ER-0007                 PIC  X(04)  VALUE '0007'.           CL**4
00321      12  ER-0008                 PIC  X(04)  VALUE '0008'.           CL**4
00322      12  ER-0023                 PIC  X(04)  VALUE '0023'.           CL**4
00323      12  ER-0029                 PIC  X(04)  VALUE '0029'.           CL**4
00324      12  ER-0042                 PIC  X(04)  VALUE '0042'.           CL**4
00325      12  ER-0043                 PIC  X(04)  VALUE '0043'.           CL**4
00326      12  ER-0068                 PIC  X(04)  VALUE '0068'.           CL**4
00327      12  ER-0070                 PIC  X(04)  VALUE '0070'.           CL**4
00328      12  ER-1698                 PIC  X(04)  VALUE '1698'.           CL**4
00329      12  ER-1699                 PIC  X(04)  VALUE '1699'.           CL**4
00330      12  ER-2000                 PIC  X(04)  VALUE '2000'.           CL**4
00331      12  ER-2002                 PIC  X(04)  VALUE '2002'.           CL**4
00332      12  ER-2349                 PIC  X(04)  VALUE '2349'.           CL**4
00333      12  ER-2350                 PIC  X(04)  VALUE '2350'.           CL**4
00334      12  ER-7008                 PIC  X(04)  VALUE '7008'.           CL**4
00335      12  ER-7693                 PIC  X(04)  VALUE '7693'.           CL**4
00336      12  ER-7694                 PIC  X(04)  VALUE '7694'.           CL**4
00337      12  ER-7695                 PIC  X(04)  VALUE '7695'.           CL**4
00338      12  ER-7696                 PIC  X(04)  VALUE '7696'.           CL**4
00339      12  ER-7697                 PIC  X(04)  VALUE '7697'.           CL**4
00340      12  ER-7698                 PIC  X(04)  VALUE '7698'.           CL**4
00341      12  ER-7699                 PIC  X(04)  VALUE '7699'.           CL**4
00342      12  ER-7700                 PIC  X(04)  VALUE '7700'.           CL**4
00343      12  ER-7701                 PIC  X(04)  VALUE '7701'.           CL**4
00344      12  ER-7702                 PIC  X(04)  VALUE '7702'.           CL**4
00345      12  ER-7703                 PIC  X(04)  VALUE '7703'.           CL**4
00346      12  ER-7704                 PIC  X(04)  VALUE '7704'.           CL**4
00347      12  ER-7705                 PIC  X(04)  VALUE '7705'.           CL**4
00348      12  ER-7706                 PIC  X(04)  VALUE '7706'.           CL**4
00349      12  ER-7707                 PIC  X(04)  VALUE '7707'.           CL**4
00350      12  ER-7708                 PIC  X(04)  VALUE '7708'.           CL**4
00351      12  ER-7709                 PIC  X(04)  VALUE '7709'.           CL**4
00352      12  ER-7710                 PIC  X(04)  VALUE '7710'.           CL**4
00353      12  ER-7711                 PIC  X(04)  VALUE '7711'.           CL**4
00354      12  ER-7712                 PIC  X(04)  VALUE '7712'.           CL**4
00355      12  ER-7713                 PIC  X(04)  VALUE '7713'.           CL**4
00356      12  ER-7714                 PIC  X(04)  VALUE '7714'.           CL**4
00357      12  ER-7715                 PIC  X(04)  VALUE '7715'.           CL**4
00358      12  ER-7716                 PIC  X(04)  VALUE '7716'.           CL**4
00359      12  ER-7747                 PIC  X(04)  VALUE '7747'.           CL*14
00360      12  ER-9096                 PIC  X(04)  VALUE '9096'.           CL**4
00361      12  ER-9097                 PIC  X(04)  VALUE '9097'.           CL**4
00362      12  ER-9129                 PIC  X(04)  VALUE '9129'.           CL**4
00363      12  ER-9196                 PIC  X(04)  VALUE '9196'.           CL**4
00364      12  ER-9299                 PIC  X(04)  VALUE '9299'.           CL**4
00365                                  EJECT                               CL**4
00366      COPY ELCAID.                                                    CL**4
00367  01  FILLER  REDEFINES  DFHAID.                                   EL602
00368      12  FILLER                  PIC  X(08).                         CL**4
00369      12  PF-VALUES               PIC  X(01) OCCURS 2 TIMES.          CL**4
00370                                  EJECT                               CL**4
00371      COPY ELCATTR.                                                   CL**4
00372                                  EJECT                               CL**4
00373      COPY ELCDATE.                                                   CL**4
00374                                  EJECT                               CL**4
00375      COPY ELCEMIB.                                                   CL**4
00376                                  EJECT                               CL**4
00377      COPY ELCJPFX.                                                   CL**4
00378                                  PIC  X(750).                        CL*10
00379                                  EJECT                               CL**4
00380      COPY ELCLOGOF.                                                  CL**4
00381                                  EJECT                               CL**4
00382      COPY ELCSCTM.                                                   CL**4
00383                                  EJECT                               CL**4
00384      COPY ELCSCRTY.                                                  CL**4
00385                                  EJECT                               CL*13
00386      COPY MPCSCRT.                                                   CL*13
00387                                  EJECT                               CL**4
00388  LINKAGE SECTION.                                                 EL602
00389                                                                   EL602
00390  01  DFHCOMMAREA                 PIC  X(01024).                      CL**4
00391                                                                   EL602
00392 *01  PARMLIST.                                                       CL*15
00393 *    12  FILLER                  PIC S9(08)               COMP.      CL*15
00394 *    12  L-CNTL-POINTER          PIC S9(08)               COMP.      CL*15
00395                                  EJECT                               CL**4
00396      COPY ELCCNTL.                                                   CL**4
00397                                  EJECT                               CL**4
00398  PROCEDURE DIVISION.                                              EL602
00399                                                                      CL**4
00400      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.         CL**4
00401      MOVE EIBTRMID               TO W-QUID-TERMINAL.                 CL**4
00402                                                                      CL*15
00403      MOVE LOW-VALUES             TO EL602AO.                         CL*15
00404                                                                   EL602
00405      EXEC CICS HANDLE CONDITION                                   EL602
00406          MAPFAIL       (0030-INITIAL-PROCESS)                        CL**4
00407          NOTOPEN       (8000-NOT-OPEN)                               CL**4
00408          NOTFND        (8010-NOT-FOUND)                              CL**4
00409          PGMIDERR      (9700-PGMID-ERROR)                            CL**4
00410          ERROR         (9800-ABEND)                                  CL**4
00411      END-EXEC.                                                    EL602
00412                                                                   EL602
00413  0005-GET-CURRENT-DATE.                                              CL**4
00414                                                                   EL602
00415      MOVE EIBDATE                TO DC-JULIAN-YYDDD.                 CL**4
00416      MOVE '5'                    TO DC-OPTION-CODE.                  CL**4
00417      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.                  CL**4
00418      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.                     CL**4
00419      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE.                 CL**4
00420                                                                   EL602
00421  0010-SET-UP-ERROR-PROCESS.                                          CL**4
00422                                                                      CL**4
00423      MOVE 2                      TO EMI-NUMBER-OF-LINES.             CL**8
00424      MOVE 2                      TO EMI-SWITCH2.                     CL**8
00425                                                                      CL**4
00426  0015-START-SECURITY-PROCESS.                                        CL**4
00427                                                                      CL**4
00428      IF  EIBCALEN = ZERO                                             CL**4
00429          MOVE UNACCESS-MSG       TO LOGOFF-MSG                       CL**4
00430          PERFORM 8300-SEND-TEXT THRU 8300-EXIT                       CL**4
00431          GO TO 9000-RETURN-TRANS.                                    CL**4
00432                                  EJECT                               CL**4
00433  0025-UPDATE-CALL-HIER.                                              CL**4
00434                                                                      CL**4
00435      IF  PI-CALLING-PROGRAM NOT = W-THIS-PGM                         CL**4
00436                                                                      CL**4
00437          IF  PI-RETURN-TO-PROGRAM NOT EQUAL W-THIS-PGM               CL**4
00438              MOVE PI-SAVED-PROGRAM-5                                 CL**4
00439                                  TO PI-SAVED-PROGRAM-6               CL**4
00440              MOVE PI-SAVED-PROGRAM-4                                 CL**4
00441                                  TO PI-SAVED-PROGRAM-5               CL**4
00442              MOVE PI-SAVED-PROGRAM-3                                 CL**4
00443                                  TO PI-SAVED-PROGRAM-4               CL**4
00444              MOVE PI-SAVED-PROGRAM-2                                 CL**4
00445                                  TO PI-SAVED-PROGRAM-3               CL**4
00446              MOVE PI-SAVED-PROGRAM-1                                 CL**4
00447                                  TO PI-SAVED-PROGRAM-2               CL**4
00448              MOVE PI-RETURN-TO-PROGRAM                               CL**4
00449                                  TO PI-SAVED-PROGRAM-1               CL**4
00450              MOVE PI-CALLING-PROGRAM                                 CL**4
00451                                  TO PI-RETURN-TO-PROGRAM             CL**4
00452              MOVE W-THIS-PGM     TO PI-CALLING-PROGRAM               CL**4
00453              MOVE LOW-VALUES     TO PI-PROGRAM-WORK-AREA             CL**4
00454              MOVE ZEROS          TO PI-WORK-TABLE-LINE               CL**4
00455                                     PI-TABLE-LINE                    CL**4
00456              MOVE -14            TO PI-LAST-MORT-TBL                 CL**4
00457              MOVE SPACES         TO PI-PASS-SW                       CL**4
00458                                                                      CL**4
00459          ELSE                                                        CL**4
00460              MOVE PI-RETURN-TO-PROGRAM                               CL**4
00461                                  TO PI-CALLING-PROGRAM               CL**4
00462              MOVE PI-SAVED-PROGRAM-1                                 CL**4
00463                                  TO PI-RETURN-TO-PROGRAM             CL**4
00464              MOVE PI-SAVED-PROGRAM-2                                 CL**4
00465                                  TO PI-SAVED-PROGRAM-1               CL**4
00466              MOVE PI-SAVED-PROGRAM-3                                 CL**4
00467                                  TO PI-SAVED-PROGRAM-2               CL**4
00468              MOVE PI-SAVED-PROGRAM-4                                 CL**4
00469                                  TO PI-SAVED-PROGRAM-3               CL**4
00470              MOVE PI-SAVED-PROGRAM-5                                 CL**4
00471                                  TO PI-SAVED-PROGRAM-4               CL**4
00472              MOVE PI-SAVED-PROGRAM-6                                 CL**4
00473                                  TO PI-SAVED-PROGRAM-5               CL**4
00474              MOVE SPACES         TO PI-SAVED-PROGRAM-6.              CL**4
00475                                                                      CL**4
00476      IF  EIBTRNID EQUAL W-TRANSACTION                                CL**4
00477                                                                      CL**4
00478          IF  NOT DISPLAY-CAP                                         CL**9
00479              MOVE PI-RETURN-TO-PROGRAM                               CL**4
00480                                  TO W-CALL-PGM                       CL**4
00481              EXEC CICS XCTL                                          CL**4
00482                  PROGRAM  (W-CALL-PGM)                               CL**4
00483                  COMMAREA (PROGRAM-INTERFACE-BLOCK)                  CL**4
00484                  LENGTH   (PI-COMM-LENGTH)                           CL**4
00485              END-EXEC                                                CL**4
00486                                                                      CL**4
00487          ELSE                                                        CL**4
00488              IF  EIBAID = DFHCLEAR                                   CL**4
00489                  MOVE PI-RETURN-TO-PROGRAM                           CL**4
00490                                  TO W-CALL-PGM                       CL**4
00491                  GO TO 9400-XCTL                                     CL**4
00492                                                                      CL**4
00493              ELSE                                                    CL**4
00494                  GO TO 0035-RECEIVE.                                 CL**4
00495                                                                      CL**4
00496  0030-INITIAL-PROCESS.                                               CL**4
00497                                                                      CL**4
00498      PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT.                CL**4
00499      GO TO 0700-SET-UP-WORKING-MORT-TBL.                             CL**4
00500                                                                      CL**4
00501  0035-RECEIVE.                                                       CL**4
00502                                                                      CL**4
00503      IF  EIBAID = DFHPA1                                             CL**4
00504             OR                                                       CL**4
00505          EIBAID = DFHPA2                                             CL**4
00506             OR                                                       CL**4
00507          EIBAID = DFHPA3                                             CL**4
00508          MOVE ER-0008            TO EMI-ERROR                        CL**4
00509          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00510          MOVE -1                 TO MAINTL                           CL**4
00511          GO TO 8200-SEND-DATAONLY.                                EL602
00512                                                                   EL602
00513      EXEC CICS RECEIVE                                            EL602
00514          MAP (W-MAP)                                                 CL**4
00515          MAPSET (W-MAPSET)                                           CL**4
00516          INTO (EL602AI)                                              CL**4
00517      END-EXEC.                                                    EL602
00518                                                                   EL602
00519      PERFORM 0200-RECOVER-TEMP-STORAGE THRU 0200-EXIT.               CL**4
00520                                                                   EL602
00521                                  EJECT                               CL**4
00522  0040-CHECK-PF-FIELD-KEY.                                            CL**4
00523                                                                   EL602
00524      IF  ENTERPFL GREATER ZERO                                       CL**4
00525          PERFORM 0500-TRANSFORM-PF-FIELD THRU 0500-EXIT.             CL**4
00526                                                                   EL602
00527      IF  EIBAID = DFHPF23                                            CL**4
00528          MOVE W-XCTL-005         TO W-CALL-PGM                       CL**4
00529          MOVE EIBAID             TO PI-ENTRY-CD-1                    CL**4
00530          GO TO 9400-XCTL.                                            CL**4
00531                                                                   EL602
00532      IF  EIBAID = DFHPF24                                            CL**4
00533          GO TO 8090-RETURN-MAIN-MENU.                                CL**4
00534                                                                      CL**4
00535      IF  EIBAID = DFHPF12                                            CL**8
00536          MOVE W-XCTL-010         TO W-CALL-PGM                       CL**8
00537          GO TO 9400-XCTL.                                            CL**8
00538                                                                      CL**8
00539      IF  EIBAID = DFHPF1                                             CL**4
00540          GO TO 1000-GET-NEXT-14-TABLES.                              CL**4
00541                                                                      CL**4
00542      IF  EIBAID = DFHPF2                                             CL**4
00543          GO TO 1050-GET-LAST-14-TABLES.                              CL**4
00544                                                                      CL**4
00545      IF  EIBAID = DFHPF3                                             CL**4
00546          PERFORM 0800-RECREATE-MORT-RECORDS THRU 0800-EXIT           CL**4
00547          MOVE ER-7707            TO EMI-ERROR                        CL**4
00548          MOVE -1                 TO MAINTL                           CL**4
00549          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00550          MOVE LOW-VALUES         TO PI-PROGRAM-WORK-AREA             CL**4
00551          MOVE ZEROS              TO PI-WORK-TABLE-LINE               CL**4
00552                                     PI-TABLE-LINE                    CL**4
00553          MOVE -14                TO PI-LAST-MORT-TBL                 CL**4
00554          MOVE SPACES             TO PI-PASS-SW                       CL**4
00555          GO TO 0700-SET-UP-WORKING-MORT-TBL.                         CL**4
00556                                                                      CL**4
00557      IF  EIBAID NOT = DFHENTER                                       CL**4
00558              AND                                                     CL**8
00559          EIBAID NOT = DFHPF11                                        CL**8
00560          MOVE ER-0008            TO EMI-ERROR                        CL**4
00561          MOVE -1                 TO MAINTL                           CL**4
00562          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00563          GO TO 8200-SEND-DATAONLY.                                   CL**4
00564                                                                      CL**4
00565  0100-MAINLINE.                                                      CL**4
00566                                                                      CL**4
00567      GO TO 1100-PROCESS-INPUT.                                       CL**4
00568                                  EJECT                               CL**4
00569  0200-RECOVER-TEMP-STORAGE.                                          CL**4
00570                                                                   EL602
00571      EXEC CICS HANDLE CONDITION                                   EL602
00572          NOTFND  (0240-TS-PROBLEMS)                                  CL**4
00573          QIDERR  (0240-TS-PROBLEMS)                                  CL**4
00574          INVREQ  (0240-TS-PROBLEMS)                                  CL**4
00575          ITEMERR (0240-TS-PROBLEMS)                                  CL**4
00576      END-EXEC.                                                       CL**4
00577                                                                      CL**4
00578      EXEC CICS READQ TS                                              CL**4
00579          QUEUE   (W-QUID-KEY)                                        CL**4
00580          INTO    (W-MORTALITY-TABLE)                                 CL**4
00581          LENGTH  (W-MORT-TBL-LENGTH)                                 CL**4
00582          ITEM    (W-ITEM)                                            CL**4
00583      END-EXEC.                                                    EL602
00584                                                                   EL602
00585  0200-EXIT.                                                          CL**4
00586      EXIT.                                                           CL**4
00587                                                                      CL**4
00588  0240-TS-PROBLEMS.                                                   CL**4
00589                                                                      CL**4
00590      MOVE ER-9129                TO EMI-ERROR.                       CL**4
00591      MOVE -1                     TO MAINTL.                          CL**4
00592      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00593      GO TO 8100-SEND-INITIAL-MAP.                                    CL**4
00594                                                                      CL**4
00595  0240-EXIT.                                                          CL**4
00596      EXIT.                                                           CL**4
00597                                                                      CL**4
00598  0250-CREATE-TEMP-STORAGE.                                           CL**4
00599                                                                      CL**4
00600      EXEC CICS HANDLE CONDITION                                      CL**4
00601          QIDERR  (0250-CONTINUE)                                     CL**4
00602          INVREQ  (0240-TS-PROBLEMS)                                  CL**4
00603          ITEMERR (0240-TS-PROBLEMS)                                  CL**4
00604      END-EXEC.                                                       CL**4
00605                                                                      CL**4
00606      EXEC CICS DELETEQ TS                                            CL**4
00607          QUEUE   (W-QUID-KEY)                                        CL**4
00608      END-EXEC.                                                       CL**4
00609                                                                      CL**4
00610  0250-CONTINUE.                                                      CL**4
00611                                                                      CL**4
00612      MOVE EIBCPOSN           TO PI-CURSOR                            CL**4
00613                                                                      CL**4
00614      EXEC CICS WRITEQ TS                                             CL**4
00615          QUEUE   (W-QUID-KEY)                                        CL**4
00616          FROM    (W-MORTALITY-TABLE)                                 CL**4
00617          LENGTH  (W-MORT-TBL-LENGTH)                                 CL**4
00618          ITEM    (W-ITEM)                                            CL**4
00619      END-EXEC.                                                       CL**4
00620                                                                      CL**4
00621  0250-EXIT.                                                          CL**4
00622      EXIT.                                                           CL**4
00623                                  EJECT                               CL**4
00624  0500-TRANSFORM-PF-FIELD.                                            CL**4
00625                                                                      CL**4
00626      IF  EIBAID NOT = DFHENTER                                       CL**4
00627          MOVE ER-0004            TO EMI-ERROR                        CL**4
00628          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00629          MOVE -1                 TO ENTERPFL                         CL**4
00630          GO TO 8200-SEND-DATAONLY.                                   CL**4
00631                                                                      CL**4
00632      IF  ENTERPFI NOT NUMERIC                                        CL**4
00633          MOVE ER-0029            TO EMI-ERROR                        CL**4
00634          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00635          MOVE -1                 TO ENTERPFL                         CL**4
00636          GO TO 8200-SEND-DATAONLY.                                   CL**4
00637                                                                      CL**4
00638      IF  ENTERPFI LESS 1 OR GREATER 24                               CL**4
00639          MOVE ER-0029            TO EMI-ERROR                        CL**4
00640          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00641          MOVE -1                 TO ENTERPFL                         CL**4
00642          GO TO 8200-SEND-DATAONLY.                                   CL**4
00643                                                                      CL**4
00644      MOVE ENTERPFI               TO W-CHECK-PFKEYS.                  CL**4
00645      MOVE PF-VALUES (W-CHECK-PFKEYS)                                 CL**4
00646                                  TO EIBAID.                          CL**4
00647                                                                      CL**4
00648  0500-EXIT.                                                          CL**4
00649      EXIT.                                                           CL**4
00650                                  EJECT                               CL**4
00651  0700-SET-UP-WORKING-MORT-TBL.                                       CL**4
00652                                                                      CL**4
00653      MOVE SPACES                 TO W-WORKING-CNTL-KEY.              CL**4
00654      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.               CL**4
00655      MOVE '7'                    TO W-CNTL-RECORD-TYPE.              CL**4
00656      MOVE +0                     TO W-CNTL-SEQUENCE-NO               CL**4
00657                                     PI-TOTAL-MORT-LINES.             CL**4
00658      SET W-MORT-NDX              TO W-ZERO.                          CL**4
00659      MOVE LOW-VALUES             TO W-MORTALITY-TABLE.               CL**4
00660                                                                      CL**4
00661      EXEC CICS HANDLE CONDITION                                      CL**4
00662          ENDFILE   (0790-LAST-RECORD-PROCESSED)                      CL**4
00663          NOTFND    (0790-LAST-RECORD-PROCESSED)                      CL**4
00664          NOTOPEN   (8000-NOT-OPEN)                                   CL**4
00665      END-EXEC.                                                       CL**4
00666                                                                      CL**4
00667      PERFORM 0720-GET-MORT-CNTL-RECORD THRU 0720-EXIT                CL**4
00668              UNTIL                                                   CL**4
00669          W-LAST-MORT-RECORD-READ.                                    CL**4
00670                                                                      CL**4
00671      IF  PI-TOTAL-MORT-LINES NOT GREATER THAN +0                     CL**4
00672          MOVE 'A'                TO MAINTO                           CL**4
00673          MOVE AL-UANON           TO MAINTA                           CL**4
00674                                     PI-MAINT                         CL**4
00675          MOVE -1                 TO W-TABLE-L (1)                    CL**4
00676          GO TO 8100-SEND-INITIAL-MAP                                 CL**4
00677                                                                      CL**4
00678      ELSE                                                            CL**4
00679          MOVE 'S'                TO MAINTI                           CL**4
00680                                     PI-MAINT                         CL**4
00681          GO TO 1500-PROCESS-SHOWS.                                   CL**4
00682                                  EJECT                               CL**4
00683  0720-GET-MORT-CNTL-RECORD.                                          CL**4
00684                                                                      CL**4
00685      EXEC CICS READ                                               EL602
00686          DATASET  ('ELCNTL')                                      EL602
00687          SET      (ADDRESS OF CONTROL-FILE)                          CL*15
00688          RIDFLD   (W-WORKING-CNTL-KEY)                               CL**4
00689      END-EXEC.                                                    EL602
00690                                                                   EL602
00691      IF  CF-COMPANY-ID NOT EQUAL PI-COMPANY-ID                       CL**4
00692              OR                                                      CL**4
00693          NOT CF-MORTALITY-MASTER                                     CL**4
00694          MOVE 'Y'                TO W-LAST-MORT-READ-IND             CL**4
00695          GO TO 0720-EXIT.                                            CL**4
00696                                                                   EL602
00697      IF  PI-FIRST-CNTL-KEY EQUAL LOW-VALUES                          CL**4
00698          MOVE W-WORKING-CNTL-KEY TO PI-FIRST-CNTL-KEY                CL**4
00699                                                                   EL602
00700          MOVE CF-LAST-MAINT-DT   TO DC-BIN-DATE-1                    CL**4
00701          MOVE ' '                TO DC-OPTION-CODE                   CL**4
00702          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT               CL**4
00703          MOVE DC-GREG-DATE-1-EDIT                                    CL**4
00704                                  TO PI-LAST-MNT-DATE-ALPHA           CL**4
00705                                                                   EL602
00706          MOVE CF-LAST-MAINT-BY   TO PI-LAST-MNT-PROCESSOR            CL**4
00707          MOVE CF-LAST-MAINT-DT   TO PI-LAST-MNT-DATE                 CL**4
00708          MOVE CF-LAST-MAINT-HHMMSS                                   CL**4
00709                                  TO PI-LAST-MNT-TIME.                CL**4
00710                                                                   EL602
00711      MOVE W-WORKING-CNTL-KEY     TO PI-LAST-CNTL-KEY.                CL**4
00712                                                                   EL602
00713      PERFORM 0780-MOVE-MORT-TBL-LINE THRU 0780-EXIT                  CL**4
00714              VARYING                                                 CL**4
00715          CF-MORT-NDX FROM 1 BY 1                                     CL**4
00716              UNTIL                                                   CL**4
00717          CF-MORT-NDX GREATER THAN +9                                 CL**4
00718              OR                                                      CL**4
00719          CF-MORT-TABLE-LINE (CF-MORT-NDX) EQUAL LOW-VALUES           CL**4
00720              OR                                                      CL**4
00721          CF-MORT-TABLE (CF-MORT-NDX) NOT GREATER THAN                CL**4
00722              SPACES                                                  CL**4
00723                                                                   EL602
00724      ADD +1                      TO W-CNTL-SEQUENCE-NO.              CL**4
00725                                                                   EL602
00726  0720-EXIT.                                                          CL**4
00727      EXIT.                                                           CL**4
00728                                                                   EL602
00729  0780-MOVE-MORT-TBL-LINE.                                            CL**4
00730                                                                   EL602
00731      SET W-MORT-NDX UP BY +1.                                        CL**4
00732                                                                   EL602
010413     IF  W-MORT-NDX GREATER THAN +153
00734          MOVE ER-7693            TO EMI-ERROR                        CL**4
00735          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
00736          MOVE -1                 TO ENTERPFL                         CL**4
00737          GO TO 8200-SEND-DATAONLY.                                   CL**4
00738                                                                   EL602
00739      ADD +1                      TO PI-TOTAL-MORT-LINES.             CL**4
00740                                                                   EL602
00741      MOVE CF-MORT-TABLE (CF-MORT-NDX)                                CL**4
00742                                  TO W-TABLE (W-MORT-NDX).            CL**4
00743      MOVE CF-MORT-TABLE-TYPE (CF-MORT-NDX)                           CL**4
00744                                  TO W-TABLE-TYPE (W-MORT-NDX).       CL**4
00745      MOVE CF-MORT-INTEREST (CF-MORT-NDX)                             CL**4
00746                                  TO W-INTEREST (W-MORT-NDX).         CL**4
00747      MOVE CF-MORT-AGE-METHOD (CF-MORT-NDX)                           CL**4
00748                                  TO W-AGE-METHOD (W-MORT-NDX).       CL**4
00749      MOVE CF-MORT-RESERVE-ADJUSTMENT (CF-MORT-NDX)                   CL**4
00750          TO W-RESERVE-ADJUSTMENT (W-MORT-NDX).                       CL**4
00751      MOVE CF-MORT-ADJUSTMENT-DIRECTION (CF-MORT-NDX)                 CL**4
00752          TO W-ADJUSTMENT-DIRECTION (W-MORT-NDX).                     CL**4
00753      MOVE CF-MORT-JOINT-FACTOR (CF-MORT-NDX)                         CL**4
00754          TO W-JOINT-FACTOR (W-MORT-NDX).                             CL**4
00755      MOVE CF-MORT-JOINT-CODE (CF-MORT-NDX)                           CL**4
00756                                  TO W-JOINT-CODE (W-MORT-NDX).       CL**4
00757      MOVE CF-MORT-PC-Q (CF-MORT-NDX)                                 CL**4
00758                                  TO W-PC-Q (W-MORT-NDX).             CL**4
00759      MOVE CF-MORT-TABLE-CODE (CF-MORT-NDX)                           CL**4
00760          TO W-MORTALITY-CODE (W-MORT-NDX).                           CL**4
00761      MOVE CF-MORT-COMMENTS (CF-MORT-NDX)                             CL**4
00762                                  TO W-COMMENTS (W-MORT-NDX).         CL**4
00763                                                                   EL602
00764  0780-EXIT.                                                          CL**4
00765      EXIT.                                                           CL**4
00766                                  EJECT                               CL**4
00767  0790-LAST-RECORD-PROCESSED.                                         CL**4
00768                                                                      CL**4
00769      MOVE 'Y'                    TO W-LAST-MORT-READ-IND.            CL**4
00770      GO TO 0720-EXIT.                                                CL**4
00771                                                                      CL**4
00772  0790-EXIT.                                                          CL**4
00773      EXIT.                                                           CL**4
00774                                  EJECT                               CL**4
00775  0800-RECREATE-MORT-RECORDS.                                         CL**4
00776                                                                      CL**4
00777      IF  PI-TOTAL-MORT-LINES GREATER THAN ZERO                       CL**4
00778          PERFORM 0850-SORT-BY-MORT-CODE THRU 0850-EXIT.              CL**4
00779                                                                      CL**4
00780      IF  PI-FIRST-CNTL-KEY GREATER THAN LOW-VALUES                   CL**4
00781          MOVE PI-FIRST-CNTL-KEY  TO W-WORKING-CNTL-KEY               CL**4
00782          PERFORM 0820-DELETE-RECORDS THRU 0820-EXIT                  CL**4
00783          MOVE LOW-VALUES         TO PI-FIRST-CNTL-KEY                CL**4
00784                                     PI-LAST-CNTL-KEY.                CL**4
00785                                                                      CL**4
00786      IF  PI-TOTAL-MORT-LINES NOT GREATER THAN +0                     CL**4
00787          MOVE 'N'                TO PI-FIRST-TIME-IND                CL**4
00788          MOVE LOW-VALUES         TO PI-MODIFICATIONS-MADE-IND        CL**4
00789          GO TO 0800-EXIT.                                            CL**4
00790                                                                      CL**4
00791      MOVE SPACES                 TO W-WORKING-CNTL-KEY.              CL**4
00792      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.               CL**4
00793      MOVE '7'                    TO W-CNTL-RECORD-TYPE.              CL**4
00794      MOVE +0                     TO W-CNTL-SEQUENCE-NO.              CL**4
00795                                                                      CL**4
00796      EXEC CICS HANDLE CONDITION                                      CL**4
00797          DUPREC (0810-DUPLICATE-RECORD)                              CL**4
00798      END-EXEC.                                                    EL602
00799                                                                   EL602
00800      EXEC CICS GETMAIN                                            EL602
00801          SET    (ADDRESS OF CONTROL-FILE)                            CL*15
00802          LENGTH (W-CNTL-LENGTH)                                      CL**4
00803      END-EXEC.                                                    EL602
00804                                                                   EL602
00805  0800-CONTINUE.                                                      CL**4
00806                                                                   EL602
00807      PERFORM 0840-CREATE-NEW-RECORDS THRU 0840-EXIT                  CL**4
00808              VARYING                                                 CL**4
00809          W-MORTR-NDX FROM 1 BY 1                                     CL**4
00810              UNTIL                                                   CL**4
010413         W-MORTR-NDX GREATER THAN +17
00812              OR                                                      CL**4
00813          W-MORT-RECORD (W-MORTR-NDX) EQUAL LOW-VALUES.               CL**4
00814                                                                   EL602
00815      MOVE LOW-VALUES             TO PI-PROGRAM-WORK-AREA             CL**4
00816                                     W-MORTALITY-TABLE.               CL**4
00817      MOVE ZEROS                  TO PI-WORK-TABLE-LINE               CL**4
00818                                     PI-TABLE-LINE.                   CL**4
00819      MOVE -14                    TO PI-LAST-MORT-TBL.                CL**4
00820      MOVE SPACES                 TO PI-PASS-SW.                      CL**4
00821      GO TO 0700-SET-UP-WORKING-MORT-TBL.                             CL**4
00822                                                                   EL602
00823  0800-EXIT.                                                          CL**4
00824      EXIT.                                                           CL**4
00825                                  EJECT                               CL**4
00826  0810-DUPLICATE-RECORD.                                              CL**4
00827                                                                   EL602
00828      MOVE ER-7714                TO EMI-ERROR.                       CL**4
00829      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
00830      MOVE -1                     TO MAINTL.                          CL**4
00831      GO TO 8200-SEND-DATAONLY.                                       CL**4
00832                                                                   EL602
00833  0810-EXIT.                                                          CL**4
00834      EXIT.                                                           CL**4
00835                                  EJECT                               CL**4
00836  0820-DELETE-RECORDS.                                                CL**4
00837                                                                   EL602
00838      EXEC CICS DELETE                                                CL**4
00839          DATASET  (W-CNTL-FILE-ID)                                   CL**4
00840          RIDFLD   (W-WORKING-CNTL-KEY)                               CL**4
00841      END-EXEC.                                                    EL602
00842                                                                   EL602
00843      ADD +1                      TO W-CNTL-SEQUENCE-NO.              CL**4
00844                                                                   EL602
00845      IF  W-WORKING-CNTL-KEY NOT GREATER THAN PI-LAST-CNTL-KEY        CL**4
00846          GO TO 0820-DELETE-RECORDS.                                  CL**4
00847                                                                   EL602
00848  0820-EXIT.                                                          CL**4
00849      EXIT.                                                        EL602
00850                                  EJECT                               CL**4
00851  0840-CREATE-NEW-RECORDS.                                            CL**4
00852                                                                   EL602
00853      MOVE W-MORT-RECORD (W-MORTR-NDX)                                CL**4
00854                                  TO CF-MORTALITY-MASTER-REC.         CL**4
00855      MOVE W-WORKING-CNTL-KEY     TO CF-CONTROL-PRIMARY.              CL**4
00856      MOVE 'CF'                   TO CF-RECORD-ID.                    CL**4
00857      MOVE CONTROL-FILE           TO JP-RECORD-AREA.                  CL**4
00858      MOVE PI-LAST-MNT-PROCESSOR  TO CF-LAST-MAINT-BY.                CL**4
00859      MOVE PI-LAST-MNT-DATE       TO CF-LAST-MAINT-DT.                CL**4
00860      MOVE PI-LAST-MNT-TIME       TO CF-LAST-MAINT-HHMMSS.            CL**4
00861                                                                   EL602
00862      EXEC CICS WRITE                                                 CL**4
00863          DATASET  (W-CNTL-FILE-ID)                                   CL**4
00864          FROM     (CONTROL-FILE)                                     CL**4
00865          RIDFLD   (W-WORKING-CNTL-KEY)                               CL**4
00866      END-EXEC.                                                    EL602
00867                                                                   EL602
00868      MOVE 'A'                    TO JP-RECORD-TYPE.                  CL**4
00869      PERFORM 8400-LOG-JOURNAL THRU 8400-EXIT.                        CL**4
00870      ADD +1                      TO W-CNTL-SEQUENCE-NO.              CL**4
00871                                                                      CL**4
00872  0840-EXIT.                                                          CL**4
00873      EXIT.                                                           CL**4
00874                                  EJECT                               CL**4
00875  0850-SORT-BY-MORT-CODE.                                             CL**4
00876                                                                      CL**4
00877      COMPUTE W-NEXT-TABLE-LINE = PI-TOTAL-MORT-LINES.                CL**4
00878                                                                      CL**4
00879  0850-CONTINUE.                                                      CL**4
00880                                                                      CL**4
00881      SET W-MORT-NDX2            TO +1.                               CL**4
00882      MOVE SPACES                TO W-EXCHANGE-MADE-IND.              CL**4
00883                                                                      CL**4
00884      PERFORM 0855-BUBBLE THRU 0855-EXIT                              CL**4
00885              VARYING                                                 CL**4
00886          W-MORT-NDX FROM +1 BY +1                                    CL**4
00887              UNTIL                                                   CL**4
00888          W-MORT-NDX EQUAL W-NEXT-TABLE-LINE.                         CL**4
00889                                                                      CL**4
00890      IF  W-EXCHANGE-MADE                                             CL**4
00891          SUBTRACT +1 FROM W-NEXT-TABLE-LINE                          CL**4
00892          GO TO 0850-CONTINUE.                                        CL**4
00893                                                                      CL**4
00894  0850-EXIT.                                                          CL**4
00895      EXIT.                                                           CL**4
00896                                  EJECT                               CL**4
00897  0855-BUBBLE.                                                        CL**4
00898                                                                      CL**4
00899      SET W-MORT-NDX2 UP BY +1.                                       CL**4
00900                                                                      CL**4
00901      IF  W-MORTALITY-CODE (W-MORT-NDX) EQUAL LOW-VALUES              CL**4
00902              OR                                                      CL**4
00903          W-MORTALITY-CODE (W-MORT-NDX) GREATER THAN                  CL**4
00904          W-MORTALITY-CODE (W-MORT-NDX2)                              CL**4
00905          MOVE W-MORT-TBL-LINE (W-MORT-NDX)                           CL**4
00906                                  TO W-TEMP-LINE                      CL**4
00907          MOVE W-MORT-TBL-LINE (W-MORT-NDX2)                          CL**4
00908              TO W-MORT-TBL-LINE (W-MORT-NDX)                         CL**4
00909          MOVE W-TEMP-LINE                                            CL**4
00910              TO W-MORT-TBL-LINE (W-MORT-NDX2)                        CL**4
00911          MOVE 'Y'                TO W-EXCHANGE-MADE-IND.             CL**4
00912                                                                      CL**4
00913  0855-EXIT.                                                          CL**4
00914      EXIT.                                                           CL**4
00915                                  EJECT                               CL**4
00916  1000-GET-NEXT-14-TABLES.                                            CL**4
00917                                                                      CL**4
00918      MOVE 'S'                    TO MAINTI                           CL**4
00919                                     PI-MAINT.                        CL**4
00920                                                                      CL**4
00921      IF  LINSEL1L GREATER THAN ZEROS                                 CL**4
00922          PERFORM 1020-LINSEL1-EDIT THRU 1020-EXIT                    CL**4
00923                                                                      CL**4
00924          IF  W-TABLE (LINSEL1I) GREATER THAN LOW-VALUES              CL**4
00925              COMPUTE PI-LAST-MORT-TBL = LINSEL1I - 1                 CL**4
00926              MOVE ZEROS          TO LINSEL1O                         CL**4
00927              MOVE AL-UNNOF       TO LINSEL1A                         CL**4
00928                                                                      CL**4
00929          ELSE                                                        CL**4
00930              COMPUTE PI-LAST-MORT-TBL =                              CL**4
00931                  PI-TOTAL-MORT-LINES - 1                             CL**4
00932                                                                      CL**4
00933      ELSE                                                            CL**4
00934          ADD +14                 TO PI-LAST-MORT-TBL                 CL**4
00935                                                                      CL**4
010413         IF  PI-LAST-MORT-TBL GREATER THAN +153
00937              MOVE +0             TO PI-LAST-MORT-TBL.                CL**4
00938                                                                      CL**4
00939                                                                      CL**4
00940  1000-START-LOOP.                                                    CL**4
00941                                                                      CL**4
00942      COMPUTE PI-WORK-TABLE-LINE                                      CL**4
00943          = PI-LAST-MORT-TBL + 1.                                     CL**4
00944                                                                      CL**4
00945      SET W-MORT-NDX              TO PI-WORK-TABLE-LINE.              CL**4
00946                                                                      CL**4
00947      IF  W-TABLE (W-MORT-NDX) EQUAL LOW-VALUES                       CL**4
00948                                                                      CL**4
00949          IF  PI-TOTAL-MORT-LINES GREATER THAN +1                     CL**4
00950                                                                      CL**4
00951              IF  PI-2ND-TIME-PAST-END                                CL**5
00952                  MOVE +0         TO PI-LAST-MORT-TBL                 CL**5
00953                  MOVE ' '        TO PI-PASS-SW                       CL**5
00954                  GO TO 1000-START-LOOP                               CL**5
00955                                                                      CL**5
00956              ELSE                                                    CL**5
00957                  COMPUTE PI-LAST-MORT-TBL =                          CL**4
00958                      PI-TOTAL-MORT-LINES - 1                         CL**4
00959                  MOVE 'Y'        TO PI-PASS-SW                       CL**4
00960                  GO TO 1000-START-LOOP                               CL**4
00961                                                                      CL**4
00962          ELSE                                                        CL**4
00963              MOVE ER-1699        TO EMI-ERROR                        CL**4
00964              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00965              MOVE -1             TO MAINTL                           CL**4
00966              GO TO 8200-SEND-DATAONLY                                CL**4
00967                                                                      CL**4
00968      ELSE                                                            CL**4
00969          PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT              CL**4
00970          GO TO 8200-SEND-DATAONLY.                                   CL**4
00971                                                                      CL**4
00972  1000-EXIT.                                                          CL**4
00973      EXIT.                                                           CL**4
00974                                  EJECT                               CL**4
00975  1020-LINSEL1-EDIT.                                                  CL**4
00976                                                                      CL**4
00977      IF  LINSEL1L GREATER THAN +0                                    CL**4
00978                                                                      CL**4
00979          IF  LINSEL1I NUMERIC                                        CL**4
00980                 AND                                                  CL**4
00981              LINSEL1I NOT LESS THAN +01                              CL**4
00982                 AND                                                  CL**4
010413             LINSEL1I NOT GREATER THAN +153                          CL**4
00984              NEXT SENTENCE                                           CL**4
00985                                                                      CL**4
00986          ELSE                                                        CL**4
00987              MOVE ER-7706        TO EMI-ERROR                        CL**4
00988              MOVE -1             TO LINSEL1L                         CL**4
00989              MOVE AL-UNNON       TO LINSEL1A                         CL**4
00990              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
00991              GO TO 8200-SEND-DATAONLY.                               CL**4
00992                                                                      CL**4
00993  1020-EXIT.                                                          CL**4
00994      EXIT.                                                           CL**4
00995                                  EJECT                               CL**4
00996  1050-GET-LAST-14-TABLES.                                            CL**4
00997                                                                      CL**4
00998      MOVE 'S'                    TO MAINTI                           CL**4
00999                                     PI-MAINT.                        CL**4
01000                                                                      CL**4
01001      SET W-MORT-NDX              TO +1.                              CL**4
01002                                                                      CL**4
01003      IF  W-TABLE (W-MORT-NDX) EQUAL LOW-VALUES                       CL**4
01004          MOVE ER-1698            TO EMI-ERROR                        CL**4
01005          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
01006          MOVE -1                 TO MAINTL                           CL**4
01007          GO TO 8200-SEND-DATAONLY.                                   CL**4
01008                                                                      CL**4
01009      IF  LINSEL1L GREATER THAN ZEROS                                 CL**4
01010          PERFORM 1020-LINSEL1-EDIT THRU 1020-EXIT                    CL**4
01011                                                                      CL**4
01012          IF  W-TABLE (LINSEL1I) GREATER THAN LOW-VALUES              CL**4
01013              COMPUTE PI-LAST-MORT-TBL = LINSEL1I - 15                CL**4
01014              MOVE ZEROS          TO LINSEL1O                         CL**4
01015              MOVE AL-UANOF       TO LINSEL1A                         CL**4
01016                                                                      CL**4
01017          ELSE                                                        CL**4
01018              COMPUTE PI-LAST-MORT-TBL =                              CL**4
01019                  PI-TOTAL-MORT-LINES - 15                            CL**4
01020              PERFORM 1060-GET-LAST-MORT-TBL THRU 1060-EXIT           CL**4
01021                                                                      CL**4
01022      ELSE                                                            CL**4
01023          SUBTRACT +14 FROM PI-LAST-MORT-TBL.                         CL**4
01024                                                                      CL**4
01025      IF  PI-LAST-MORT-TBL LESS THAN +0                               CL**4
01026                                                                      CL**4
01027          IF  PI-LAST-MORT-TBL GREATER THAN -15                       CL**4
01028              MOVE +0             TO PI-LAST-MORT-TBL                 CL**4
01029                                                                      CL**4
01030          ELSE                                                        CL**4
01031              COMPUTE PI-LAST-MORT-TBL =                              CL**4
01032                  PI-TOTAL-MORT-LINES - 15.                           CL**4
01033                                                                      CL**4
01034      COMPUTE PI-WORK-TABLE-LINE                                      CL**4
01035          = PI-LAST-MORT-TBL + 1.                                     CL**4
01036                                                                      CL**4
01037      PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT.                 CL**4
01038                                                                      CL**4
01039      GO TO 8200-SEND-DATAONLY.                                       CL**4
01040                                                                      CL**4
01041  1050-EXIT.                                                          CL**4
01042      EXIT.                                                           CL**4
01043                                  EJECT                               CL**4
01044  1060-GET-LAST-MORT-TBL.                                             CL**4
01045                                                                      CL**4
01046      SET W-MORT-NDX              TO +1                               CL**4
01047      SEARCH W-MORT-TBL-LINE                                          CL**4
01048          VARYING W-MORT-NDX                                          CL**4
01049                                                                      CL**4
01050          AT END                                                      CL**4
01051              MOVE +0             TO PI-LAST-MORT-TBL                 CL**4
01052                                                                      CL**4
01053          WHEN                                                        CL**4
01054              W-TABLE (W-MORT-NDX) EQUAL LOW-VALUES                   CL**4
01055              SET PI-LAST-MORT-TBL                                    CL**4
01056                                  TO W-MORT-NDX                       CL**4
01057              SUBTRACT +15 FROM PI-LAST-MORT-TBL.                     CL**4
01058                                                                      CL**4
01059  1060-EXIT.                                                          CL**4
01060      EXIT.                                                           CL**4
01061                                  EJECT                               CL**4
01062  1100-PROCESS-INPUT.                                                 CL**4
01063                                                                      CL**4
01064      IF  MAINTL EQUAL ZEROS                                          CL**4
01065                                                                      CL**4
01066          IF  PI-MAINT GREATER THAN LOW-VALUES                        CL**4
01067              MOVE PI-MAINT       TO MAINTI                           CL**4
01068                                                                      CL**4
01069          ELSE                                                        CL**4
01070              IF  PI-LAST-MORT-TBL GREATER +0                         CL**4
01071                  MOVE 'S'        TO PI-MAINT                         CL**4
01072                                     MAINTI                           CL**4
01073                  COMPUTE PI-WORK-TABLE-LINE                          CL**4
01074                      = PI-LAST-MORT-TBL + 1                          CL**4
01075                                                                      CL**4
01076              ELSE                                                    CL**4
01077                  MOVE 'S'        TO PI-MAINT                         CL**4
01078                                     MAINTI                           CL**4
01079                  MOVE +0         TO PI-LAST-MORT-TBL                 CL**4
01080                  MOVE +1         TO PI-WORK-TABLE-LINE               CL**4
01081                                                                      CL**4
01082      ELSE                                                            CL**4
01083          MOVE MAINTI             TO W-CHECK-MAINT                    CL**4
01084          MOVE AL-UANON           TO MAINTA                           CL**4
01085                                                                      CL**4
01086          IF  NOT W-VALID-OPTION                                      CL**4
01087              MOVE -1             TO MAINTL                           CL**4
01088              MOVE AL-UABON       TO MAINTA                           CL**4
01089              MOVE ER-0023        TO EMI-ERROR                        CL**4
01090              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01091              GO TO 8200-SEND-DATAONLY                                CL**4
01092                                                                      CL**4
01093          ELSE                                                        CL**4
01094              MOVE MAINTI         TO PI-MAINT.                        CL**4
01095                                                                      CL**4
01096      IF  MAINTI EQUAL 'S'                                            CL**4
01097          PERFORM 1500-PROCESS-SHOWS THRU 1500-EXIT                   CL**4
01098                                                                      CL**4
01099      ELSE                                                            CL**4
01100          IF  NOT MODIFY-CAP                                          CL**9
01101              MOVE 'UPDATE'       TO SM-READ                          CL**4
01102              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**4
01103              MOVE ER-0070        TO EMI-ERROR                        CL**9
01104              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01105              MOVE AL-UANON       TO MAINTA                           CL**4
01106              MOVE -1             TO MAINTL                           CL**4
01107              GO TO 8200-SEND-DATAONLY                                CL**4
01108                                                                      CL**4
01109          ELSE                                                        CL**4
01110              IF  MAINTI EQUAL 'A'                                    CL**8
01111                  GO TO 1200-PROCESS-ADDS                             CL**4
01112                                                                      CL**4
01113              ELSE                                                    CL**4
01114                  IF  MAINTI EQUAL 'C'                                CL**4
01115                      GO TO 1300-PROCESS-CHANGES                      CL**4
01116                                                                      CL**4
01117                  ELSE                                                CL**4
01118                      IF  MAINTI EQUAL 'D'                            CL**4
01119                          GO TO 1400-PROCESS-DELETE.                  CL**4
01120                                                                      CL**4
01121  1100-EXIT.                                                          CL**4
01122      EXIT.                                                           CL**4
01123                                  EJECT                               CL**4
01124  1200-PROCESS-ADDS.                                                  CL**4
01125                                                                      CL**4
01126      IF  PI-TOTAL-MORT-LINES NOT EQUAL +0                            CL**4
01127          MOVE 'C'                TO MAINTI                           CL**4
01128          GO TO 1300-PROCESS-CHANGES.                                 CL**4
01129                                                                      CL**4
01130      PERFORM 2000-EDIT-MORT-TBL-DATA THRU 2000-EXIT                  CL**4
01131              VARYING                                                 CL**4
01132          W-TBLI-NDX FROM +1 BY +1                                    CL**4
01133              UNTIL                                                   CL**4
01134          W-TBLI-NDX GREATER THAN +14                                 CL**4
01135              OR                                                      CL**4
01136          W-TABLE-I (W-TBLI-NDX) EQUAL LOW-VALUES.                    CL**4
01137                                                                      CL**4
01138      IF  NOT EMI-NO-ERRORS                                           CL**4
01139              AND                                                     CL**4
01140          EMI-FATAL-CTR GREATER THAN ZEROS                            CL**8
01141          GO TO 8200-SEND-DATAONLY.                                   CL**4
01142                                                                      CL**4
01143      MOVE +0                     TO PI-LAST-MORT-TBL.                CL**4
01144      SET W-MORT-NDX              TO PI-LAST-MORT-TBL.                CL**4
01145                                                                      CL**4
01146      PERFORM 2500-UPDATE-WORKING-MORT-TBL THRU 2500-EXIT             CL**4
01147              VARYING                                                 CL**4
01148          W-TBLI-NDX FROM +1 BY +1                                    CL**4
01149              UNTIL                                                   CL**4
01150          W-TBLI-NDX GREATER THAN +14                                 CL**4
01151              OR                                                      CL**4
01152          W-TABLE-I (W-TBLI-NDX) EQUAL LOW-VALUES.                    CL**4
01153                                                                      CL**4
01154      IF  NOT EMI-NO-ERRORS                                           CL**4
01155              AND                                                     CL**4
01156          EMI-FATAL-CTR GREATER THAN ZEROS                            CL**8
01157          GO TO 8200-SEND-DATAONLY.                                   CL**4
01158                                                                      CL**4
01159      IF  W-MORT-NDX GREATER THAN PI-TOTAL-MORT-LINES                 CL**4
01160          SET PI-TOTAL-MORT-LINES TO W-MORT-NDX.                      CL**4
01161                                                                      CL**4
01162      MOVE ER-7708                TO EMI-ERROR.                       CL**4
01163      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01164                                                                      CL**4
01165      IF  PI-MODIFICATIONS-MADE                                       CL**4
01166          EXEC CICS ASKTIME END-EXEC                                  CL**4
01167          MOVE EIBTIME            TO PI-LAST-MNT-TIME                 CL**4
01168          MOVE PI-PROCESSOR-ID    TO PI-LAST-MNT-PROCESSOR            CL**4
01169          MOVE W-SAVE-BIN-DATE    TO PI-LAST-MNT-DATE                 CL**4
01170          MOVE W-SAVE-DATE        TO PI-LAST-MNT-DATE-ALPHA.          CL**4
01171                                                                      CL**4
01172      GO TO 1500-PROCESS-SHOWS.                                       CL**4
01173                                                                      CL**4
01174  1200-EXIT.                                                          CL**4
01175      EXIT.                                                           CL**4
01176                                  EJECT                               CL**4
01177  1300-PROCESS-CHANGES.                                               CL**4
01178                                                                      CL**4
01179      PERFORM 2000-EDIT-MORT-TBL-DATA THRU 2000-EXIT                  CL**4
01180              VARYING                                                 CL**4
01181          W-TBLI-NDX FROM +1 BY +1                                    CL**4
01182              UNTIL                                                   CL**4
01183          W-TBLI-NDX GREATER THAN +14.                                CL**4
01184                                                                      CL**4
01185      IF  NOT EMI-NO-ERRORS                                           CL**4
01186              AND                                                     CL**4
01187          EMI-FATAL-CTR GREATER THAN ZEROS                            CL**8
01188          GO TO 8200-SEND-DATAONLY.                                   CL**4
01189                                                                      CL**4
01190      IF  PI-LAST-MORT-TBL LESS THAN +0                               CL**4
01191          MOVE +0                 TO PI-LAST-MORT-TBL.                CL**4
01192                                                                      CL**4
01193      SET W-MORT-NDX              TO PI-LAST-MORT-TBL.                CL**4
01194                                                                      CL**4
01195      PERFORM 2500-UPDATE-WORKING-MORT-TBL THRU 2500-EXIT             CL**4
01196              VARYING                                                 CL**4
01197          W-TBLI-NDX FROM +1 BY +1                                    CL**4
01198              UNTIL                                                   CL**4
01199          W-TBLI-NDX GREATER THAN +14.                                CL**4
01200                                                                      CL**4
01201      IF  NOT EMI-NO-ERRORS                                           CL**4
01202              AND                                                     CL**4
01203          EMI-FATAL-CTR GREATER THAN ZEROS                            CL**8
01204          GO TO 8200-SEND-DATAONLY.                                   CL**4
01205                                                                      CL**4
01206      IF  W-MORT-NDX GREATER THAN PI-TOTAL-MORT-LINES                 CL**4
01207          SET PI-TOTAL-MORT-LINES TO W-MORT-NDX.                      CL**4
01208                                                                      CL**4
01209      MOVE ER-7708                TO EMI-ERROR.                       CL**4
01210      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01211                                                                      CL**4
01212      IF  PI-MODIFICATIONS-MADE                                       CL**4
01213          EXEC CICS ASKTIME END-EXEC                                  CL**4
01214          MOVE EIBTIME            TO PI-LAST-MNT-TIME                 CL**4
01215          MOVE PI-PROCESSOR-ID    TO PI-LAST-MNT-PROCESSOR            CL**4
01216          MOVE W-SAVE-BIN-DATE    TO PI-LAST-MNT-DATE                 CL**4
01217          MOVE W-SAVE-DATE        TO PI-LAST-MNT-DATE-ALPHA.          CL**4
01218                                                                      CL**4
01219      GO TO 1500-PROCESS-SHOWS.                                       CL**4
01220                                                                      CL**4
01221  1300-EXIT.                                                          CL**4
01222      EXIT.                                                           CL**4
01223                                  EJECT                               CL**4
01224  1400-PROCESS-DELETE.                                                CL**4
01225                                                                      CL**4
01226      PERFORM 1420-CHECK-REQUEST THRU 1420-EXIT.                      CL**4
01227                                                                      CL**4
01228      IF  NOT EMI-NO-ERRORS                                           CL**4
01229              AND                                                     CL**4
01230          EMI-FATAL-CTR GREATER THAN ZEROS                            CL**8
01231          GO TO 8200-SEND-DATAONLY.                                   CL**4
01232                                                                      CL**4
01233      PERFORM 1440-DO-DELETE THRU 1440-EXIT.                          CL**4
01234                                                                      CL**4
01235      IF  NOT EMI-NO-ERRORS                                           CL**4
01236              AND                                                     CL**4
01237          EMI-FATAL-CTR GREATER THAN ZEROS                            CL**8
01238          GO TO 8200-SEND-DATAONLY.                                   CL**4
01239                                                                      CL**4
01240      MOVE ER-7708                TO EMI-ERROR.                       CL**4
01241      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01242                                                                      CL**4
01243      IF  PI-MODIFICATIONS-MADE                                       CL**4
01244          EXEC CICS ASKTIME END-EXEC.                                 CL**4
01245          MOVE EIBTIME            TO PI-LAST-MNT-TIME                 CL**4
01246          MOVE PI-PROCESSOR-ID    TO PI-LAST-MNT-PROCESSOR            CL**4
01247          MOVE W-SAVE-BIN-DATE    TO PI-LAST-MNT-DATE                 CL**4
01248          MOVE W-SAVE-DATE        TO PI-LAST-MNT-DATE-ALPHA.          CL**4
01249                                                                      CL**4
01250      GO TO 1500-PROCESS-SHOWS.                                       CL**4
01251                                                                      CL**4
01252  1400-EXIT.                                                          CL**4
01253      EXIT.                                                           CL**4
01254                                  EJECT                               CL**4
01255  1420-CHECK-REQUEST.                                                 CL**4
01256                                                                      CL**4
01257      IF  LINSEL1L NOT GREATER THAN ZEROS                             CL**4
01258          MOVE ER-7706            TO EMI-ERROR                        CL**4
01259          MOVE -1                 TO LINSEL1L                         CL**4
01260          MOVE AL-UABON           TO LINSEL1A                         CL**4
01261          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
01262          GO TO 1420-EXIT.                                            CL**4
01263                                                                      CL**4
01264      EXEC CICS BIF DEEDIT                                            CL**4
01265          FIELD   (LINSEL1I)                                          CL**4
01266          LENGTH  (2)                                                 CL**4
01267          END-EXEC                                                    CL**4
01268                                                                      CL**4
01269      COMPUTE W-NEXT-TABLE-LINE                                       CL**4
01270          = PI-LAST-MORT-TBL + 15                                     CL**4
01271                                                                      CL**4
01272      IF  LINSEL1I NUMERIC                                            CL**4
01273             AND                                                      CL**4
01274          LINSEL1I NOT LESS THAN +01                                  CL**4
01275             AND                                                      CL**4
010413         LINSEL1I NOT GREATER THAN +153                              CL**4
01277             AND                                                      CL**4
01278          LINSEL1I GREATER THAN PI-LAST-MORT-TBL                      CL**4
01279             AND                                                      CL**4
01280          LINSEL1I LESS THAN W-NEXT-TABLE-LINE                        CL**4
01281          SET W-TBLI-NDX      TO LINSEL1I                             CL**4
01282          SET W-TBLI-NDX DOWN BY PI-LAST-MORT-TBL                     CL**4
01283                                                                      CL**4
01284          IF  W-TABLE-I (W-TBLI-NDX) EQUAL LOW-VALUES                 CL**4
01285              MOVE ER-9196    TO EMI-ERROR                            CL**4
01286              MOVE -1         TO LINSEL1L                             CL**4
01287              MOVE AL-UABON   TO LINSEL1A                             CL**4
01288              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01289                                                                      CL**4
01290          ELSE                                                        CL**4
01291              NEXT SENTENCE                                           CL**4
01292                                                                      CL**4
01293      ELSE                                                            CL**4
01294          MOVE ER-7694        TO EMI-ERROR                            CL**4
01295          MOVE -1             TO LINSEL1L                             CL**4
01296          MOVE AL-UABON       TO LINSEL1A                             CL**4
01297          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**4
01298                                                                      CL**4
01299      IF  LINSEL2L NOT GREATER THAN ZEROS                             CL**4
01300          COMPUTE W-WORK-CTR = LINSEL1I + 1                           CL**4
01301          GO TO 1420-EXIT.                                            CL**4
01302                                                                      CL**4
01303      EXEC CICS BIF DEEDIT                                            CL**4
01304          FIELD   (LINSEL2I)                                          CL**4
01305          LENGTH  (2)                                                 CL**4
01306          END-EXEC                                                    CL**4
01307                                                                      CL**4
01308      IF  LINSEL2I NUMERIC                                            CL**4
01309             AND                                                      CL**4
01310          LINSEL2I NOT LESS THAN +01                                  CL**4
01311             AND                                                      CL**4
010413         LINSEL2I NOT GREATER THAN +153                              CL**4
01313             AND                                                      CL**4
01314          LINSEL2I NOT LESS THAN LINSEL1I                             CL**4
01315             AND                                                      CL**4
01316          LINSEL2I NOT GREATER THAN PI-TOTAL-MORT-LINES               CL**4
01317          COMPUTE W-WORK-CTR = LINSEL2I + 1                           CL**4
01318                                                                      CL**4
01319      ELSE                                                            CL**4
01320          MOVE ER-7716        TO EMI-ERROR                            CL**4
01321          MOVE -1             TO LINSEL2L                             CL**4
01322          MOVE AL-UABON       TO LINSEL2A                             CL**4
01323          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**4
01324                                                                      CL**4
01325  1420-EXIT.                                                          CL**4
01326      EXIT.                                                           CL**4
01327                                  EJECT                               CL**4
01328  1440-DO-DELETE.                                                     CL**4
01329                                                                      CL**4
01330      SET W-MORT-NDX              TO LINSEL1I.                        CL**4
01331      SET W-MORT-NDX2             TO W-WORK-CTR.                      CL**4
01332                                                                      CL**4
01333      COMPUTE PI-TOTAL-MORT-LINES                                     CL**4
01334          = PI-TOTAL-MORT-LINES - (W-WORK-CTR - LINSEL1I).            CL**4
01335                                                                      CL**4
01336      PERFORM 1460-BUBBLE-TABLES THRU 1460-EXIT                       CL**4
01337              VARYING                                                 CL**4
01338          W-MORT-NDX FROM W-MORT-NDX BY +1                            CL**4
01339              UNTIL                                                   CL**4
01340          W-MORT-NDX GREATER THAN PI-TOTAL-MORT-LINES.                CL**4
01341                                                                      CL**4
01342      PERFORM 1470-CLEAN-REST THRU 1470-EXIT                          CL**4
01343              VARYING                                                 CL**4
01344          W-MORT-NDX FROM W-MORT-NDX BY +1                            CL**4
01345              UNTIL                                                   CL**4
01346          W-MORT-NDX GREATER THAN W-WORK-CTR.                         CL**4
01347                                                                      CL**4
01348      MOVE 'Y'                    TO PI-MODIFICATIONS-MADE-IND.       CL**4
01349                                                                      CL**4
01350  1440-EXIT.                                                          CL**4
01351      EXIT.                                                           CL**4
01352                                  EJECT                               CL**4
01353  1460-BUBBLE-TABLES.                                                 CL**4
01354                                                                      CL**4
01355      MOVE W-MORT-TBL-LINE (W-MORT-NDX2)                              CL**4
01356          TO W-MORT-TBL-LINE (W-MORT-NDX).                            CL**4
01357      MOVE LOW-VALUES                                                 CL**4
01358          TO W-MORT-TBL-LINE (W-MORT-NDX2).                           CL**4
01359      SET W-MORT-NDX2 UP BY +1.                                       CL**4
01360                                                                      CL**4
01361  1460-EXIT.                                                          CL**4
01362      EXIT.                                                           CL**4
01363                                                                      CL**4
01364  1470-CLEAN-REST.                                                    CL**4
01365                                                                      CL**4
01366      MOVE LOW-VALUES                                                 CL**4
01367          TO W-MORT-TBL-LINE (W-MORT-NDX).                            CL**4
01368                                                                      CL**4
01369  1470-EXIT.                                                          CL**4
01370      EXIT.                                                           CL**4
01371                                  EJECT                               CL**4
01372  1500-PROCESS-SHOWS.                                                 CL**4
01373                                                                      CL**4
01374      PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT.                 CL**4
01375                                                                      CL**4
01376      GO TO 8100-SEND-INITIAL-MAP.                                    CL**4
01377                                                                      CL**4
01378  1500-EXIT.                                                          CL**4
01379      EXIT.                                                           CL**4
01380                                  EJECT                               CL**4
01381  2000-EDIT-MORT-TBL-DATA.                                            CL**4
01382                                                                      CL**4
01383      IF  W-TABLE-INDAT (W-TBLI-NDX) EQUAL LOW-VALUES                 CL**4
01384          GO TO 2000-EXIT.                                            CL**4
01385                                                                      CL**4
01386      MOVE +0                     TO W-HOLD-INTEREST                  CL**4
01387                                     W-HOLD-RESERVE-ADJ               CL**4
01388                                     W-HOLD-JOINT-FACTOR.             CL**4
01389                                                                      CL**4
01390      PERFORM 2020-EDIT-TABLE                THRU 2020-EXIT.          CL**4
01391      PERFORM 2030-EDIT-TABLE-TYPE           THRU 2030-EXIT.          CL**4
01392      PERFORM 2040-EDIT-INTEREST             THRU 2040-EXIT.          CL**4
01393      PERFORM 2050-EDIT-AGE-METHOD           THRU 2050-EXIT.          CL**4
01394      PERFORM 2060-EDIT-RESERVE-ADJUSTMENT   THRU 2060-EXIT.          CL**4
01395      PERFORM 2070-EDIT-ADJUSTMENT-DIRECTION THRU 2070-EXIT.          CL**4
01396      PERFORM 2080-EDIT-JOINT-FACTOR         THRU 2080-EXIT.          CL**4
01397      PERFORM 2090-EDIT-JOINT-CDE            THRU 2090-EXIT.          CL**4
01398      PERFORM 2100-EDIT-PC-Q                 THRU 2100-EXIT.          CL**4
01399      PERFORM 2110-EDIT-MORT-CDE             THRU 2110-EXIT.          CL**4
01400      PERFORM 2120-EDIT-COMMENTS             THRU 2120-EXIT.          CL**4
01401                                                                      CL**4
01402  2000-EXIT.                                                          CL**4
01403      EXIT.                                                           CL**4
01404                                  EJECT                               CL**4
01405  2020-EDIT-TABLE.                                                    CL**4
01406                                                                      CL**4
01407       IF  W-TABLE-L (W-TBLI-NDX) GREATER THAN ZEROS                  CL**4
01408           MOVE W-TABLE-I (W-TBLI-NDX)                                CL**4
01409                                  TO W-SUPPORTED-TABLES-IND           CL**4
01410                                                                      CL**4
01411           IF  W-TABLE-SUPPORTED                                      CL**4
01412               MOVE AL-UANON      TO W-TABLE-A (W-TBLI-NDX)           CL**4
01413           ELSE                                                       CL**4
01414               MOVE ER-7695       TO EMI-ERROR                        CL**4
01415               MOVE AL-UABON      TO W-TABLE-A (W-TBLI-NDX)           CL**4
01416               MOVE -1            TO W-TABLE-L (W-TBLI-NDX)           CL**4
01417               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               CL**4
01418       ELSE                                                           CL**4
01419           MOVE ER-7712           TO EMI-ERROR                        CL**4
01420           MOVE AL-UABON          TO W-TABLE-A (W-TBLI-NDX)           CL**4
01421           MOVE -1                TO W-TABLE-L (W-TBLI-NDX)           CL**4
01422           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                  CL**4
01423                                                                      CL**4
01424      GO TO 2020-EXIT.                                                CL**4
01425                                                                      CL**4
01426  2020-EXIT.                                                          CL**4
01427      EXIT.                                                           CL**4
01428                                  EJECT                               CL**4
01429  2030-EDIT-TABLE-TYPE.                                               CL**4
01430                                                                      CL**4
01431      IF  W-NULL-ENTRY                                                CL**5
01432          MOVE AL-UANON          TO W-TBLTP-A (W-TBLI-NDX)            CL**4
01433          MOVE SPACES            TO W-TBLTP-I (W-TBLI-NDX)            CL**4
01434          GO TO 2030-EXIT.                                            CL**4
01435                                                                      CL**4
01436      IF  W-TBLTP-L (W-TBLI-NDX) GREATER THAN +0                      CL**4
01437          MOVE W-TBLTP-I (W-TBLI-NDX)                                 CL**4
01438                                  TO W-VALID-TYPE-IND                 CL**4
01439                                                                      CL**4
01440          IF  MORTGAGE-SESSION                                        CL**4
01441                  AND                                                 CL**4
01442              W-TYPE-VALID-M                                          CL**4
01443                  OR                                                  CL**4
01444              W-TYPE-VALID-C                                          CL**4
01445              MOVE AL-UANON       TO W-TBLTP-A (W-TBLI-NDX)           CL**4
01446              GO TO 2030-EXIT.                                        CL**4
01447                                                                      CL**4
01448      MOVE ER-7697                TO EMI-ERROR                        CL**4
01449      MOVE -1                     TO W-TBLTP-L (W-TBLI-NDX)           CL**4
01450      MOVE AL-UABON               TO W-TBLTP-A (W-TBLI-NDX)           CL**4
01451      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01452                                                                      CL**4
01453  2030-EXIT.                                                          CL**4
01454      EXIT.                                                           CL**4
01455                                  EJECT                               CL**4
01456  2040-EDIT-INTEREST.                                                 CL**4
01457                                                                      CL**5
01458      IF  W-NULL-ENTRY                                                CL**5
01459          MOVE AL-UANON          TO W-INTR-A (W-TBLI-NDX)             CL**4
01460          MOVE 1.0000            TO W-INTR-I (W-TBLI-NDX)             CL**4
01461          GO TO 2040-EXIT.                                            CL**4
01462                                                                      CL**4
01463      IF  W-INTR-L (W-TBLI-NDX) GREATER THAN +0                       CL**4
01464          EXEC CICS BIF DEEDIT                                        CL**4
01465              FIELD   (W-INTR-I (W-TBLI-NDX))                         CL**4
01466              LENGTH  (5)                                             CL**4
01467              END-EXEC                                                CL**4
01468                                                                      CL**4
01469          IF  W-INTR-I (W-TBLI-NDX) NUMERIC                           CL**4
01470              MOVE W-INTR-I(W-TBLI-NDX)                               CL**4
01471                                  TO W-HOLD-INTEREST                  CL**4
01472              MOVE AL-UNNON       TO W-INTR-A (W-TBLI-NDX)            CL**4
01473              GO TO 2040-EXIT.                                        CL**4
01474                                                                      CL**4
01475      MOVE ER-7698                TO EMI-ERROR                        CL**4
01476      MOVE -1                     TO W-INTR-L (W-TBLI-NDX)            CL**4
01477      MOVE AL-UNBON               TO W-INTR-A (W-TBLI-NDX)            CL**4
01478      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01479                                                                      CL**4
01480  2040-EXIT.                                                          CL**4
01481      EXIT.                                                           CL**4
01482                                  EJECT                               CL**4
01483  2050-EDIT-AGE-METHOD.                                               CL**4
01484                                                                      CL**5
01485      IF  W-NULL-ENTRY                                                CL**5
01486          MOVE AL-UANON          TO W-ANAL-A (W-TBLI-NDX)             CL**4
01487          MOVE SPACES            TO W-ANAL-I (W-TBLI-NDX)             CL**4
01488          GO TO 2050-EXIT.                                            CL**4
01489                                                                      CL**4
01490      IF  W-ANAL-L (W-TBLI-NDX) GREATER THAN +0                       CL**4
01491          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AL' OR 'AN'                CL**4
01492              MOVE AL-UANON       TO W-ANAL-A (W-TBLI-NDX)            CL**4
01493              GO TO 2050-CHECK-COMPANY.                               CL**4
01494                                                                      CL**4
01495      MOVE ER-7699                TO EMI-ERROR.                       CL**4
01496      MOVE -1                     TO W-ANAL-L (W-TBLI-NDX).           CL**4
01497      MOVE AL-UABON               TO W-ANAL-A (W-TBLI-NDX).           CL**4
01498      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01499      GO TO 2050-EXIT.                                                CL**4
01500                                                                      CL**4
01501  2050-CHECK-COMPANY.                                                 CL**4
01502                                                                      CL**4
01503      EXEC CICS HANDLE CONDITION                                      CL**4
01504          ENDFILE   (2051-MASTER-CNTL-NOT-FOUND)                      CL**4
01505          NOTFND    (2051-MASTER-CNTL-NOT-FOUND)                      CL**4
01506      END-EXEC.                                                       CL**4
01507                                                                      CL**4
01508      MOVE SPACES                 TO W-WORKING-CNTL-KEY.              CL**4
01509      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.               CL**4
01510      MOVE '1'                    TO W-CNTL-RECORD-TYPE.              CL**4
01511      MOVE +0                     TO W-CNTL-SEQUENCE-NO.              CL**4
01512                                                                      CL**4
01513      EXEC CICS READ                                               EL602
01514          DATASET(W-CNTL-FILE-ID)                                     CL**4
01515          SET    (ADDRESS OF CONTROL-FILE)                            CL*15
01516          RIDFLD (W-WORKING-CNTL-KEY)                                 CL**4
01517      END-EXEC.                                                    EL602
01518                                                                   EL602
01519      IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AN'                            CL**4
01520               AND                                                    CL**4
01521          CF-USE-ALL-AGE-LAST                                         CL**4
01522               OR                                                     CL**4
01523          W-ANAL-I (W-TBLI-NDX) EQUAL 'AL'                            CL**4
01524               AND                                                    CL**4
01525          CF-USE-ALL-AGE-NEAR                                         CL**4
01526          MOVE ER-7715            TO EMI-ERROR                        CL**4
01527          MOVE -1                 TO W-ANAL-L (W-TBLI-NDX)            CL**4
01528          MOVE AL-UABON           TO W-ANAL-A (W-TBLI-NDX)            CL**4
01529          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
01530          GO TO 2050-EXIT                                             CL**4
01531                                                                   EL602
01532      ELSE                                                            CL**4
01533          MOVE AL-UANON           TO W-ANAL-A (W-TBLI-NDX).           CL**4
01534                                                                   EL602
01535  2050-EXIT.                                                          CL**4
01536      EXIT.                                                        EL602
01537                                  EJECT                               CL**4
01538  2051-MASTER-CNTL-NOT-FOUND.                                         CL**4
01539                                                                      CL**4
01540      MOVE ER-9299                TO EMI-ERROR.                       CL**4
01541      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01542      MOVE -1                     TO ENTERPFL.                        CL**4
01543      GO TO 8200-SEND-DATAONLY.                                       CL**4
01544                                                                      CL**4
01545  2051-EXIT.                                                          CL**4
01546      EXIT.                                                           CL**4
01547                                  EJECT                               CL**4
01548  2060-EDIT-RESERVE-ADJUSTMENT.                                       CL**4
01549                                                                      CL**5
01550      IF  W-NULL-ENTRY                                                CL**5
01551          MOVE AL-UANON          TO W-RSADJ-A (W-TBLI-NDX)            CL**4
01552          MOVE 1.0000            TO W-RSADJ-I (W-TBLI-NDX)            CL**4
01553          GO TO 2060-EXIT.                                            CL**4
01554                                                                      CL**4
01555      IF  W-RSADJ-L (W-TBLI-NDX) GREATER THAN +0                      CL**4
01556          EXEC CICS BIF DEEDIT                                        CL**4
01557              FIELD   (W-RSADJ-I (W-TBLI-NDX))                        CL**4
01558              LENGTH  (6)                                             CL**4
01559              END-EXEC                                                CL**4
01560                                                                      CL**4
01561          IF  W-RSADJ-I (W-TBLI-NDX) NUMERIC                          CL**4
01562              MOVE AL-UNNON       TO W-RSADJ-A (W-TBLI-NDX)           CL**4
01563                                                                      CL**4
01564              IF  W-RSADJ-I (W-TBLI-NDX) EQUAL ZEROS                  CL**4
01565                  MOVE +1.0       TO W-RSADJ-I (W-TBLI-NDX)           CL**4
01566                                     W-HOLD-RESERVE-ADJ               CL**4
01567                  GO TO 2060-EXIT                                     CL**4
01568                                                                      CL**4
01569              ELSE                                                    CL**4
01570                  MOVE W-RSADJ-I (W-TBLI-NDX)                         CL**4
01571                                  TO W-HOLD-RESERVE-ADJ               CL**4
01572                  GO TO 2060-EXIT.                                    CL**4
01573                                                                      CL**4
01574      MOVE ER-7700                TO EMI-ERROR.                       CL**4
01575      MOVE -1                     TO W-RSADJ-L (W-TBLI-NDX).          CL**4
01576      MOVE AL-UNBON               TO W-RSADJ-A (W-TBLI-NDX).          CL**4
01577      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
01578                                                                      CL**4
01579  2060-EXIT.                                                          CL**4
01580      EXIT.                                                           CL**4
01581                                  EJECT                               CL**4
01582  2070-EDIT-ADJUSTMENT-DIRECTION.                                     CL**4
01583                                                                      CL**5
01584      IF  W-NULL-ENTRY                                                CL**5
01585          MOVE AL-UANON          TO W-ADJDI-A (W-TBLI-NDX)            CL**4
01586          MOVE SPACES            TO W-ADJDI-I (W-TBLI-NDX)            CL**4
01587          GO TO 2070-EXIT.                                            CL**4
01588                                                                      CL**4
01589      IF  W-ADJDI-L (W-TBLI-NDX) GREATER THAN +0                      CL**4
01590          IF  W-ADJDI-I (W-TBLI-NDX) EQUAL '+' OR '-' OR              CL**4
01591                                           SPACES     OR              CL**4
01592                                           LOW-VALUES                 CL**4
01593              MOVE AL-UANON   TO W-ADJDI-A (W-TBLI-NDX)               CL**4
01594                                                                      CL**4
01595          ELSE                                                        CL**4
01596              MOVE ER-7701    TO EMI-ERROR                            CL**4
01597              MOVE -1         TO W-ADJDI-L (W-TBLI-NDX)               CL**4
01598              MOVE AL-UABON   TO W-ADJDI-A (W-TBLI-NDX)               CL**4
01599              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**4
01600                                                                      CL**4
01601  2070-EXIT.                                                          CL**4
01602      EXIT.                                                           CL**4
01603                                  EJECT                               CL**4
01604  2080-EDIT-JOINT-FACTOR.                                             CL**4
01605                                                                      CL**4
01606      IF  W-NULL-ENTRY                                                CL**5
01607          MOVE AL-UANON          TO W-JNTFC-A (W-TBLI-NDX)            CL**4
01608          MOVE 1.0000            TO W-JNTFC-I (W-TBLI-NDX)            CL**4
01609          GO TO 2080-EXIT.                                            CL**4
01610                                                                      CL**4
01611      IF  W-JNTFC-L (W-TBLI-NDX) GREATER THAN +0                      CL**4
01612          EXEC CICS BIF DEEDIT                                        CL**4
01613              FIELD   (W-JNTFC-I (W-TBLI-NDX))                        CL**4
01614              LENGTH  (6)                                             CL**4
01615              END-EXEC                                                CL**4
01616                                                                      CL**4
01617          IF  W-JNTFC-I (W-TBLI-NDX) NOT NUMERIC                      CL**4
01618              MOVE ER-2000        TO EMI-ERROR                        CL**4
01619              MOVE -1             TO W-JNTFC-L (W-TBLI-NDX)           CL**4
01620              MOVE AL-UNBON       TO W-JNTFC-A (W-TBLI-NDX)           CL**4
01621              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01622              GO TO 2080-EXIT                                         CL**4
01623                                                                      CL**4
01624          ELSE                                                        CL**4
01625              MOVE W-JNTFC-I (W-TBLI-NDX)                             CL**4
01626                                  TO W-HOLD-JOINT-FACTOR              CL**4
01627                                                                      CL**4
01628      ELSE                                                            CL**4
01629          MOVE +1.0               TO W-HOLD-JOINT-FACTOR              CL**4
01630                                     W-JNTFC-I (W-TBLI-NDX).          CL**4
01631                                                                      CL**4
01632      IF  W-TBLTP-I (W-TBLI-NDX) EQUAL 'J'                            CL**4
01633              OR                                                      CL**4
01634          W-TBLTP-I (W-TBLI-NDX) EQUAL 'C'                            CL**4
01635                                                                      CL**4
01636          IF  W-HOLD-JOINT-FACTOR NOT EQUAL +1                        CL**4
01637              MOVE ER-2000        TO EMI-ERROR                        CL**4
01638              MOVE -1             TO W-JNTFC-L (W-TBLI-NDX)           CL**4
01639              MOVE AL-UNBON       TO W-JNTFC-A (W-TBLI-NDX)           CL**4
01640              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01641              GO TO 2080-EXIT                                         CL**4
01642                                                                      CL**4
01643          ELSE                                                        CL**4
01644              MOVE AL-UNNON                                           CL**4
01645                          TO W-JNTFC-A (W-TBLI-NDX)                   CL**4
01646              GO TO 2080-EXIT                                         CL**4
01647                                                                      CL**4
01648      ELSE                                                            CL**4
01649          IF  W-HOLD-JOINT-FACTOR LESS THAN +1                        CL**4
01650              MOVE ER-7710                                            CL**4
01651                          TO EMI-ERROR                                CL**4
01652              MOVE -1     TO W-JNTFC-L (W-TBLI-NDX)                   CL**4
01653              MOVE AL-UNBON                                           CL**4
01654                          TO W-JNTFC-A (W-TBLI-NDX)                   CL**4
01655              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01656              GO TO 2080-EXIT                                         CL**4
01657                                                                      CL**4
01658          ELSE                                                        CL**4
01659              MOVE AL-UNNON                                           CL**4
01660                          TO W-JNTFC-A (W-TBLI-NDX).                  CL**4
01661                                                                      CL**4
01662  2080-EXIT.                                                          CL**4
01663      EXIT.                                                           CL**4
01664                                  EJECT                               CL**4
01665  2090-EDIT-JOINT-CDE.                                                CL**4
01666                                                                      CL**5
01667      IF  W-NULL-ENTRY                                                CL**5
01668          MOVE AL-UANON          TO W-JNTCD-A (W-TBLI-NDX)            CL**4
01669          MOVE SPACES            TO W-JNTCD-I (W-TBLI-NDX)            CL**4
01670          GO TO 2090-EXIT.                                            CL**4
01671                                                                      CL**4
01672      IF  W-JNTCD-L (W-TBLI-NDX) GREATER THAN +0                      CL**4
01673          MOVE W-JNTCD-I (W-TBLI-NDX)                                 CL**4
01674                                  TO W-VALID-JOINT-CODE-IND           CL**4
01675                                                                      CL**4
01676          IF  W-VALID-JOINT-CODE                                      CL**4
01677              MOVE AL-UANON       TO W-JNTCD-A (W-TBLI-NDX)           CL**4
01678                                                                      CL**4
01679          ELSE                                                        CL**4
01680              MOVE ER-7711        TO EMI-ERROR                        CL**4
01681              MOVE -1             TO W-JNTCD-L (W-TBLI-NDX)           CL**4
01682              MOVE AL-UABON       TO W-JNTCD-A (W-TBLI-NDX)           CL**4
01683              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**4
01684                                                                      CL**4
01685  2090-EXIT.                                                          CL**4
01686      EXIT.                                                           CL**4
01687                                  EJECT                               CL**4
01688  2100-EDIT-PC-Q.                                                     CL**4
01689                                                                      CL**5
01690      IF  W-NULL-ENTRY                                                CL**5
01691          MOVE AL-UANON          TO W-PCQ-A (W-TBLI-NDX)              CL**4
01692          MOVE SPACES            TO W-PCQ-I (W-TBLI-NDX)              CL**4
01693          GO TO 2100-EXIT.                                            CL**4
01694                                                                      CL**4
01695      IF  W-PCQ-L (W-TBLI-NDX) GREATER THAN +0                        CL**4
01696                                                                      CL**4
01697          IF  W-PCQ-I (W-TBLI-NDX) EQUAL 'Y'                          CL**4
01698                  OR                                                  CL**4
01699              W-PCQ-I (W-TBLI-NDX) EQUAL SPACES                       CL**4
01700              MOVE AL-UANON       TO W-PCQ-A (W-TBLI-NDX)             CL**4
01701                                                                      CL**4
01702          ELSE                                                        CL**4
01703              MOVE ER-7702        TO EMI-ERROR                        CL**4
01704              MOVE -1             TO W-PCQ-L (W-TBLI-NDX)             CL**4
01705              MOVE AL-UABON       TO W-PCQ-A (W-TBLI-NDX)             CL**4
01706              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01707              GO TO 2100-EXIT.                                        CL**4
01708                                                                      CL**4
01709      IF  W-HOLD-RESERVE-ADJ NOT EQUAL +1.0                           CL**4
01710              AND                                                     CL**4
01711          W-PCQ-I (W-TBLI-NDX) EQUAL 'Y'                              CL**4
01712          MOVE ER-7705            TO EMI-ERROR                        CL**4
01713          MOVE -1                 TO W-RSADJ-L (W-TBLI-NDX)           CL**4
01714          MOVE AL-UABON           TO W-RSADJ-A (W-TBLI-NDX)           CL**4
01715          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL**4
01716                                                                      CL**4
01717  2100-EXIT.                                                          CL**4
01718      EXIT.                                                           CL**4
01719                                  EJECT                               CL**4
01720  2110-EDIT-MORT-CDE.                                                 CL**4
01721                                                                      CL**4
01722      IF  W-MORTC-I (W-TBLI-NDX) GREATER THAN SPACES                  CL**4
01723          MOVE W-MORTC-I (W-TBLI-NDX) TO W-MORT-CODE                  CL*14
01724          GO TO 2110-CHECK-AGAINST-WORK-TABLE.                        CL**4
01725                                                                      CL**5
01726      IF  W-NULL-ENTRY                                                CL**5
01727          MOVE AL-UANON          TO W-MORTC-A (W-TBLI-NDX)            CL**5
01728          MOVE 'ZERO'            TO W-MORTC-I (W-TBLI-NDX)            CL**5
01729          GO TO 2110-EXIT.                                            CL**5
01730                                                                      CL**4
01731      IF  W-INTR-L (W-TBLI-NDX) GREATER THAN +0                       CL**4
01732          MOVE W-INTR-I (W-TBLI-NDX)                                  CL**4
01733                                  TO W-INTEREST-N                     CL**4
01734          MOVE W-INT              TO W-MORT-INT                       CL**4
01735          MOVE ZERO               TO W-MORT-TYP                       CL**4
01736          MOVE '*'                TO W-MORT-TBL                       CL**4
01737                                                                      CL**4
01738      ELSE                                                            CL**4
01739          MOVE ER-7703            TO EMI-ERROR                        CL**4
01740          MOVE -1                 TO W-INTR-L (W-TBLI-NDX)            CL**4
01741          MOVE AL-UABON           TO W-INTR-A (W-TBLI-NDX)            CL**4
01742          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
01743          GO TO 2110-EXIT.                                            CL**4
01744                                                                      CL**4
01745      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '41CSO'                        CL**4
01746                                                                      CL**4
01747          IF  W-HOLD-RESERVE-ADJ EQUAL 1.30                           CL**4
01748              MOVE 'B'            TO W-MORT-TBL                       CL**4
01749                                                                      CL**4
01750          ELSE                                                        CL**4
01751              MOVE 'A'            TO W-MORT-TBL.                      CL**4
01752                                                                      CL**4
01753      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '60CSG'                        CL**4
01754                                                                      CL**4
01755          IF  W-HOLD-RESERVE-ADJ EQUAL 1.30                           CL**4
01756              MOVE 'I'            TO W-MORT-TBL                       CL**4
01757                                                                      CL**4
01758          ELSE                                                        CL**4
01759              MOVE 'G'            TO W-MORT-TBL.                      CL**4
01760                                                                      CL**4
01761      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '58CSO'                        CL**4
01762                                                                      CL**4
01763          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AN'                        CL**4
01764                                                                      CL**4
01765              IF  W-HOLD-RESERVE-ADJ EQUAL 1.30                       CL**4
01766                  MOVE 'E'        TO W-MORT-TBL                       CL**4
01767                                                                      CL**4
01768              ELSE                                                    CL**4
01769                  MOVE 'C'        TO W-MORT-TBL                       CL**4
01770                                                                      CL**4
01771          ELSE                                                        CL**4
01772              IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AL'                    CL**4
01773                                                                      CL**4
01774                  IF  W-HOLD-RESERVE-ADJ EQUAL 1.15                   CL**4
01775                      MOVE 'J'    TO W-MORT-TBL                       CL**4
01776                                                                      CL**4
01777                  ELSE                                                CL**4
01778                      IF  W-HOLD-RESERVE-ADJ EQUAL 1.30               CL**4
01779                          MOVE 'F'                                    CL**4
01780                                  TO W-MORT-TBL                       CL**4
01781                                                                      CL**4
01782                      ELSE                                            CL**4
01783                          IF  W-TBLTP-I (W-TBLI-NDX) EQUAL 'J'        CL**4
01784                              MOVE 'P'                                CL**4
01785                                  TO W-MORT-TBL                       CL**4
01786                                                                      CL**4
01787                          ELSE                                        CL**4
01788                              MOVE 'D'                                CL**4
01789                                  TO W-MORT-TBL.                      CL**4
01790                                                                      CL**4
01791                                                                      CL**4
01792      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '58CET'                        CL**4
01793                                                                      CL**4
01794          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AN'                        CL**4
01795              MOVE 'H'            TO W-MORT-TBL                       CL**4
01796                                                                      CL**4
01797          ELSE                                                        CL**4
01798              MOVE 'L'            TO W-MORT-TBL.                      CL**4
01799                                                                      CL**4
01800      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80MSO'                        CL**4
01801                                                                      CL**7
01802          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AL'                        CL**7
01803              MOVE 'S'            TO W-MORT-TBL                       CL**7
01804                                                                      CL**7
01805          ELSE                                                        CL**7
01806              MOVE 'Q'            TO W-MORT-TBL.                      CL**7
01807                                                                      CL**4
01808      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80FSO'                        CL**4
01809                                                                      CL**7
01810          IF  W-ANAL-I (W-TBLI-NDX) EQUAL 'AL'                        CL**7
01811              MOVE 'T'            TO W-MORT-TBL                       CL**7
01812                                                                      CL**7
01813          ELSE                                                        CL**7
01814              MOVE 'R'            TO W-MORT-TBL.                      CL**7
01815                                                                      CL**4
01816      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80MET'                        CL**4
01817          MOVE 'U'                TO W-MORT-TBL.                      CL**4
01818                                                                      CL**4
01819      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80FET'                        CL**4
01820          MOVE 'V'                TO W-MORT-TBL.                      CL**4
01821                                                                      CL*11
01822      IF  W-TABLE-I (W-TBLI-NDX) EQUAL '80GBT'                        CL*11
01823          MOVE 'W'                TO W-MORT-TBL.                      CL*11
01824                                                                      CL**7
01825      IF  W-TABLE-I (W-TBLI-NDX) EQUAL 'XXXXX'                        CL**7
01826          MOVE 'X'                TO W-MORT-TBL.                      CL**7
01827                                                                      CL**4
01828      IF  W-MORT-TBL EQUAL '*'                                        CL**4
01829          MOVE ER-7704            TO EMI-ERROR                        CL**4
01830          MOVE -1                 TO W-MORTC-L (W-TBLI-NDX)           CL**4
01831          MOVE AL-UABON           TO W-MORTC-A (W-TBLI-NDX)           CL**4
01832          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**4
01833          GO TO 2110-EXIT.                                            CL**4
01834                                                                      CL**4
01835      MOVE W-MORT-CODE            TO W-MORTC-O (W-TBLI-NDX).          CL**4
01836                                                                      CL**4
01837  2110-CHECK-AGAINST-WORK-TABLE.                                      CL**4
01838                                                                      CL*14
01839      IF  W-MORT-CODE NOT = 'ZERO'                                    CL*14
01840        IF  W-MORT-TYP NOT = '0'                                      CL*14
01841            MOVE ER-7747          TO EMI-ERROR                        CL*14
01842            MOVE -1               TO W-MORTC-L (W-TBLI-NDX)           CL*14
01843            MOVE AL-UABON         TO W-MORTC-A (W-TBLI-NDX)           CL*14
01844            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  CL*14
01845            GO TO 2110-EXIT.                                          CL*14
01846                                                                      CL**4
01847      MOVE AL-UANON               TO W-MORTC-A (W-TBLI-NDX)           CL**4
01848      MOVE +4                     TO W-MORTC-L (W-TBLI-NDX)           CL**4
01849                                                                      CL**4
01850      SET W-DUPTBL-NDX            TO +1.                              CL**4
01851      SEARCH  W-TABLE-INPUT                                           CL**4
01852          VARYING W-DUPTBL-NDX                                        CL**4
01853                                                                      CL**4
01854          WHEN                                                        CL**4
01855              W-MORTC-L (W-DUPTBL-NDX) EQUAL ZEROS                    CL**4
01856              NEXT SENTENCE                                           CL**4
01857                                                                      CL**4
01858          WHEN                                                        CL**4
01859              W-MORTC-I (W-DUPTBL-NDX) EQUAL                          CL**4
01860                  W-MORTC-I (W-TBLI-NDX)                              CL**4
01861                  AND                                                 CL**4
01862              W-DUPTBL-NDX NOT EQUAL W-TBLI-NDX                       CL**4
01863                  AND                                                 CL**5
01864              W-TBLTP-I (W-DUPTBL-NDX) EQUAL                          CL**5
01865                  W-TBLTP-I (W-TBLI-NDX)                              CL**5
01866              MOVE ER-7696        TO EMI-ERROR                        CL**4
01867              MOVE AL-UABON       TO W-MORTC-A (W-DUPTBL-NDX)         CL**4
01868                                     W-MORTC-A (W-TBLI-NDX)           CL**4
01869              MOVE -1             TO W-MORTC-L (W-TBLI-NDX)           CL**4
01870              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01871              GO TO 2110-EXIT.                                        CL**4
01872                                                                      CL**4
01873      COMPUTE W-NEXT-TABLE-LINE = PI-LAST-MORT-TBL + 15.              CL**4
01874      SET W-MORT-NDX              TO +1.                              CL**4
01875                                                                      CL**4
01876      SEARCH  W-MORT-TBL-LINE                                         CL**4
01877          VARYING W-MORT-NDX                                          CL**4
01878                                                                      CL**4
01879          WHEN                                                        CL**4
01880              W-MORTALITY-CODE (W-MORT-NDX) EQUAL LOW-VALUES          CL**4
01881              NEXT SENTENCE                                           CL**4
01882                                                                      CL**4
01883          WHEN                                                        CL**4
01884              W-MORTALITY-CODE (W-MORT-NDX)                           CL**4
01885                  EQUAL W-MORTC-I (W-TBLI-NDX)                        CL**4
01886                  AND                                                 CL**6
01887              W-TABLE-TYPE (W-MORT-NDX) EQUAL                         CL**6
01888                  W-TBLTP-I (W-TBLI-NDX)                              CL**6
01889                  AND                                                 CL**4
01890              (W-MORT-NDX NOT GREATER THAN PI-LAST-MORT-TBL           CL**4
01891                  OR                                                  CL**4
01892              W-MORT-NDX NOT LESS THAN W-NEXT-TABLE-LINE)             CL**4
01893              MOVE ER-7696        TO EMI-ERROR                        CL**4
01894              MOVE AL-UABON       TO W-MORTC-A (W-TBLI-NDX)           CL**4
01895              MOVE -1             TO W-MORTC-L (W-TBLI-NDX)           CL**4
01896              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
01897              GO TO 2110-EXIT.                                        CL**4
01898                                                                      CL**4
01899  2110-EXIT.                                                          CL**4
01900      EXIT.                                                           CL**4
01901                                                                      CL**4
01902  2120-EDIT-COMMENTS.                                                 CL**4
01903                                                                      CL**4
01904      IF W-NULL-ENTRY                                                 CL**4
01905          MOVE AL-UANON          TO W-COMM-A (W-TBLI-NDX)             CL**4
01906          MOVE 'NULL ENTRY'      TO W-COMM-I (W-TBLI-NDX)             CL**4
01907          GO TO 2120-EXIT.                                            CL**4
01908                                                                      CL**4
01909      IF  W-COMM-L (W-TBLI-NDX) GREATER THAN +0                       CL**4
01910          MOVE AL-UANON           TO W-COMM-A (W-TBLI-NDX)            CL**4
01911                                                                      CL**4
01912      ELSE                                                            CL**4
01913          MOVE SPACES             TO W-COMM-I (W-TBLI-NDX)            CL**4
01914          MOVE AL-UANON           TO W-COMM-A (W-TBLI-NDX).           CL**4
01915                                                                      CL**4
01916  2120-EXIT.                                                          CL**4
01917      EXIT.                                                           CL**4
01918                                  EJECT                               CL**4
01919  2500-UPDATE-WORKING-MORT-TBL.                                       CL**4
01920                                                                      CL**4
01921      IF  W-TABLE-INDAT (W-TBLI-NDX) EQUAL LOW-VALUES                 CL**4
01922          GO TO 2500-EXIT.                                            CL**4
01923                                                                      CL**4
01924      SET W-MORT-NDX UP BY +1.                                        CL**4
01925                                                                      CL**4
01926      IF  W-TABLE-I (W-TBLI-NDX) NOT EQUAL W-TABLE (W-MORT-NDX)       CL**4
01927          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01928          MOVE W-TABLE-I (W-TBLI-NDX)                                 CL**4
01929              TO W-TABLE (W-MORT-NDX).                                CL**4
01930                                                                      CL**4
01931      IF  W-TBLTP-I (W-TBLI-NDX) NOT EQUAL                            CL**4
01932              W-TABLE-TYPE (W-MORT-NDX)                               CL**4
01933          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01934          MOVE W-TBLTP-I (W-TBLI-NDX)                                 CL**4
01935              TO W-TABLE-TYPE (W-MORT-NDX).                           CL**4
01936                                                                      CL**4
01937      IF  W-INTEREST (W-MORT-NDX) NOT NUMERIC                         CL**4
01938              OR                                                      CL**4
01939          W-INTR-I (W-TBLI-NDX) NOT EQUAL W-INTEREST (W-MORT-NDX)     CL**4
01940          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01941          MOVE W-INTR-I (W-TBLI-NDX)                                  CL**4
01942              TO W-INTEREST (W-MORT-NDX).                             CL**4
01943                                                                      CL**4
01944      IF  W-ANAL-I (W-TBLI-NDX) NOT EQUAL                             CL**4
01945              W-AGE-METHOD (W-MORT-NDX)                               CL**4
01946          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01947          MOVE W-ANAL-I (W-TBLI-NDX)                                  CL**4
01948              TO W-AGE-METHOD (W-MORT-NDX).                           CL**4
01949                                                                      CL**4
01950      IF  W-RESERVE-ADJUSTMENT (W-MORT-NDX) NOT NUMERIC               CL**4
01951              OR                                                      CL**4
01952          W-RSADJ-I (W-TBLI-NDX) NOT EQUAL                            CL**4
01953              W-RESERVE-ADJUSTMENT (W-MORT-NDX)                       CL**4
01954          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01955          MOVE W-RSADJ-I (W-TBLI-NDX)                                 CL**4
01956              TO W-RESERVE-ADJUSTMENT (W-MORT-NDX).                   CL**4
01957                                                                      CL**4
01958      IF  W-ADJDI-I (W-TBLI-NDX) NOT EQUAL                            CL**4
01959              W-ADJUSTMENT-DIRECTION (W-MORT-NDX)                     CL**4
01960          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01961          MOVE W-ADJDI-I (W-TBLI-NDX)                                 CL**4
01962              TO W-ADJUSTMENT-DIRECTION (W-MORT-NDX).                 CL**4
01963                                                                      CL**4
01964      IF  W-JOINT-FACTOR (W-MORT-NDX) NOT NUMERIC                     CL**4
01965              OR                                                      CL**4
01966          W-JNTFC-I (W-TBLI-NDX) NOT EQUAL                            CL**4
01967              W-JOINT-FACTOR (W-MORT-NDX)                             CL**4
01968          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01969          MOVE W-JNTFC-I (W-TBLI-NDX)                                 CL**4
01970              TO W-JOINT-FACTOR (W-MORT-NDX).                         CL**4
01971                                                                      CL**4
01972      IF  W-JNTCD-I (W-TBLI-NDX) NOT EQUAL                            CL**4
01973              W-JOINT-CODE (W-MORT-NDX)                               CL**4
01974          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01975          MOVE W-JNTCD-I (W-TBLI-NDX)                                 CL**4
01976              TO W-JOINT-CODE (W-MORT-NDX).                           CL**4
01977                                                                      CL**4
01978      IF  W-PCQ-I (W-TBLI-NDX) NOT EQUAL                              CL**4
01979              W-PC-Q (W-MORT-NDX)                                     CL**4
01980          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01981          MOVE W-PCQ-I (W-TBLI-NDX)                                   CL**4
01982              TO W-PC-Q (W-MORT-NDX).                                 CL**4
01983                                                                      CL**4
01984      IF  W-MORTC-I (W-TBLI-NDX) NOT EQUAL                            CL**4
01985              W-MORTALITY-CODE (W-MORT-NDX)                           CL**4
01986          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01987          MOVE W-MORTC-I (W-TBLI-NDX)                                 CL**4
01988              TO W-MORTALITY-CODE (W-MORT-NDX).                       CL**4
01989                                                                      CL**4
01990      IF  W-COMM-I (W-TBLI-NDX) NOT EQUAL                             CL**4
01991              W-COMMENTS (W-MORT-NDX)                                 CL**4
01992          MOVE 'Y'                TO PI-MODIFICATIONS-MADE-IND        CL**4
01993          MOVE W-COMM-I (W-TBLI-NDX)                                  CL**4
01994              TO W-COMMENTS (W-MORT-NDX).                             CL**4
01995                                                                      CL**4
01996  2500-EXIT.                                                          CL**4
01997      EXIT.                                                           CL**4
01998                                  EJECT                               CL**4
01999  5000-MOVE-MORT-TBL-DATA.                                            CL**4
02000                                                                      CL**4
02001      MOVE ZEROS                  TO LINSEL1L                         CL**4
02002      MOVE LOW-VALUES             TO W-LINSEX1-O                      CL**4
02003                                     W-LINSEX2-O.                     CL**4
02004      MOVE AL-UNNOF               TO LINSEL1A.                        CL**4
02005                                                                      CL**4
02006      MOVE AL-UANON               TO MAINTA.                          CL**4
02007                                                                      CL**4
02008      MOVE -1                     TO MAINTL.                          CL**4
02009      MOVE 'S'                    TO MAINTI                           CL**4
02010                                     PI-MAINT.                        CL**4
02011                                                                      CL**4
02012      MOVE PI-LAST-MNT-PROCESSOR  TO MAINTBYO                         CL**4
02013                                     PI-UPDATE-BY.                    CL**4
02014      MOVE PI-LAST-MNT-DATE-ALPHA TO MAINTDTO.                        CL**4
02015      MOVE PI-LAST-MNT-TIME       TO W-TIME-IN                        CL**4
02016                                     PI-UPDATE-HHMMSS.                CL**4
02017      MOVE W-TIME-OUT             TO MAINTTMO.                        CL**4
02018                                                                      CL**4
02019      IF  PI-LAST-MORT-TBL LESS THAN +0                               CL**4
02020          MOVE +0                 TO PI-LAST-MORT-TBL.                CL**4
02021                                                                      CL**4
02022      COMPUTE W-NEXT-TABLE-LINE                                       CL**4
02023          = PI-LAST-MORT-TBL + 14.                                    CL**4
02024      SET W-MORT-NDX              TO PI-LAST-MORT-TBL.                CL**4
02025      SET W-MORT-NDX UP BY +1.                                        CL**4
02026                                                                      CL**4
02027      PERFORM 5020-PROCESS-TABLE-DATA THRU 5020-EXIT                  CL**4
02028              VARYING                                                 CL**4
02029          W-MORT-NDX FROM W-MORT-NDX BY +1                            CL**4
02030              UNTIL                                                   CL**4
010413         W-MORT-NDX GREATER THAN +153
02032              OR                                                      CL**4
02033          W-MORT-NDX GREATER THAN W-NEXT-TABLE-LINE.                  CL**4
02034                                                                      CL**4
02035      MOVE -1                     TO MAINTL.                          CL**4
02036                                                                      CL**4
02037  5000-EXIT.                                                          CL**4
02038      EXIT.                                                           CL**4
02039                                  EJECT                               CL**4
02040  5020-PROCESS-TABLE-DATA.                                            CL**4
02041                                                                      CL**4
02042      SET W-TBLO-NDX              TO W-MORT-NDX.                      CL**4
02043      SET W-TBLO-NDX DOWN BY PI-LAST-MORT-TBL.                        CL**4
02044      SET W-LINE-NUMBER           TO W-MORT-NDX.                      CL**4
02045                                                                      CL**4
02046      IF  W-LINE-NUMBER LESS THAN +1                                  CL**4
02047          MOVE +1                 TO W-LINE-NUMBER                    CL**4
02048          SET W-MORT-NDX                                              CL**4
02049              W-TBLO-NDX          TO +1.                              CL**4
02050                                                                      CL**4
02051      MOVE W-LINE-NUMBER          TO W-EDITED-LINE-NUMB.              CL**4
02052      MOVE W-DISPLAY-LINE-NUMBER  TO W-LINE-O (W-TBLO-NDX).           CL**4
02053      MOVE AL-SABON               TO W-LINE-A (W-TBLO-NDX).           CL**4
02054                                                                      CL**4
02055      IF  W-TABLE (W-MORT-NDX) GREATER THAN LOW-VALUES                CL**4
02056          MOVE W-TABLE (W-MORT-NDX)                                   CL**4
02057                                  TO W-TABLE-O (W-TBLO-NDX)           CL**4
02058          MOVE W-TABLE-TYPE (W-MORT-NDX)                              CL**4
02059                                  TO W-TBLTP-O (W-TBLO-NDX)           CL**4
02060          MOVE W-INTEREST (W-MORT-NDX)                                CL**4
02061                                  TO W-INTR-O (W-TBLO-NDX)            CL**4
02062          MOVE W-AGE-METHOD (W-MORT-NDX)                              CL**4
02063                                  TO W-ANAL-O (W-TBLO-NDX)            CL**4
02064          MOVE W-RESERVE-ADJUSTMENT (W-MORT-NDX)                      CL**4
02065                                  TO W-RSADJ-O (W-TBLO-NDX)           CL**4
02066          MOVE W-ADJUSTMENT-DIRECTION (W-MORT-NDX)                    CL**4
02067                                  TO W-ADJDI-O (W-TBLO-NDX)           CL**4
02068          MOVE W-JOINT-FACTOR (W-MORT-NDX)                            CL**4
02069                                  TO W-JNTFC-O (W-TBLO-NDX)           CL**4
02070          MOVE W-JOINT-CODE (W-MORT-NDX)                              CL**4
02071                                  TO W-JNTCD-O (W-TBLO-NDX)           CL**4
02072          MOVE W-PC-Q (W-MORT-NDX)                                    CL**4
02073                                  TO W-PCQ-O (W-TBLO-NDX)             CL**4
02074          MOVE W-MORTALITY-CODE (W-MORT-NDX)                          CL**4
02075                                  TO W-MORTC-O (W-TBLO-NDX)           CL**4
02076          MOVE W-COMMENTS (W-MORT-NDX)                                CL**4
02077                                  TO W-COMM-O (W-TBLO-NDX)            CL**4
02078                                                                      CL**4
02079          PERFORM 5040-SET-TABLE-LINE-ATTRB THRU 5040-EXIT            CL**4
02080                                                                      CL**4
02081      ELSE                                                            CL**4
02082          MOVE SPACES             TO W-TABLE-O (W-TBLO-NDX)           CL**4
02083                                     W-TBLTP-O (W-TBLO-NDX)           CL**4
02084                                     W-INTR-X-O (W-TBLO-NDX)          CL**4
02085                                     W-ANAL-O (W-TBLO-NDX)            CL**4
02086                                     W-RSADJ-X-O (W-TBLO-NDX)         CL**4
02087                                     W-ADJDI-O (W-TBLO-NDX)           CL**4
02088                                     W-JNTFC-X-O (W-TBLO-NDX)         CL**4
02089                                     W-JNTCD-O (W-TBLO-NDX)           CL**4
02090                                     W-PCQ-O (W-TBLO-NDX)             CL**4
02091                                     W-MORTC-O (W-TBLO-NDX)           CL**4
02092                                     W-COMM-O (W-TBLO-NDX)            CL**4
02093                                     W-LINE-O (W-TBLO-NDX)            CL**4
02094          MOVE AL-UANOF           TO W-TABLE-A (W-TBLO-NDX)           CL**4
02095                                     W-TBLTP-A (W-TBLO-NDX)           CL**4
02096                                     W-INTR-A (W-TBLO-NDX)            CL**4
02097                                     W-ANAL-A (W-TBLO-NDX)            CL**4
02098                                     W-RSADJ-A (W-TBLO-NDX)           CL**4
02099                                     W-ADJDI-A (W-TBLO-NDX)           CL**4
02100                                     W-JNTFC-A (W-TBLO-NDX)           CL**4
02101                                     W-JNTCD-A (W-TBLO-NDX)           CL**4
02102                                     W-PCQ-A (W-TBLO-NDX)             CL**4
02103                                     W-MORTC-A (W-TBLO-NDX)           CL**4
02104                                     W-COMM-A (W-TBLO-NDX)            CL**4
02105                                     W-LINE-O (W-TBLO-NDX).           CL**4
02106                                                                      CL**4
02107  5020-EXIT.                                                          CL**4
02108      EXIT.                                                           CL**4
02109                                                                      CL**4
02110  5040-SET-TABLE-LINE-ATTRB.                                          CL**4
02111                                                                      CL**4
02112      MOVE AL-UANON               TO W-TABLE-A (W-TBLO-NDX)           CL**4
02113                                     W-TBLTP-A (W-TBLO-NDX)           CL**4
02114                                     W-ANAL-A (W-TBLO-NDX)            CL**4
02115                                     W-ADJDI-A (W-TBLO-NDX)           CL**4
02116                                     W-JNTCD-A (W-TBLO-NDX)           CL**4
02117                                     W-PCQ-A (W-TBLO-NDX)             CL**4
02118                                     W-MORTC-A (W-TBLO-NDX)           CL**4
02119                                     W-COMM-A (W-TBLO-NDX).           CL**4
02120                                                                      CL**4
02121      MOVE AL-UNNON               TO W-INTR-A (W-TBLO-NDX)            CL**4
02122                                     W-RSADJ-A (W-TBLO-NDX)           CL**4
02123                                     W-JNTFC-A (W-TBLO-NDX).          CL**4
02124                                                                      CL**4
02125      MOVE AL-SABON               TO W-LINE-A (W-TBLO-NDX).           CL**4
02126                                                                      CL**4
02127  5040-EXIT.                                                          CL**4
02128       EXIT.                                                          CL**4
02129                                  EJECT                               CL**4
02130 ******************************************************************   CL**4
02131 *   COMMON ERROR HANDLING ROUTINES.                              *   CL**4
02132 ******************************************************************   CL**4
02133                                                                      CL**4
02134  8000-NOT-OPEN.                                                      CL**4
02135                                                                      CL**4
02136      MOVE ER-0042                TO EMI-ERROR.                       CL**4
02137      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
02138      MOVE -1                     TO ENTERPFL.                        CL**4
02139                                                                      CL**4
02140      IF  EIBTRNID NOT = W-TRANSACTION                                CL**4
02141          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
02142                                                                      CL**4
02143      GO TO 8200-SEND-DATAONLY.                                       CL**4
02144                                                                      CL**4
02145  8010-NOT-FOUND.                                                     CL**4
02146                                                                      CL**4
02147      MOVE ER-0043                TO EMI-ERROR.                       CL**4
02148      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**4
02149      MOVE -1                     TO ENTERPFL.                        CL**4
02150                                                                      CL**4
02151      IF  EIBTRNID NOT = W-TRANSACTION                                CL**4
02152          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
02153                                                                      CL**4
02154      GO TO 8200-SEND-DATAONLY.                                       CL**4
02155                                                                      CL**4
02156  8090-RETURN-MAIN-MENU.                                              CL**4
02157                                                                      CL**4
02158      IF  CREDIT-SESSION                                              CL**4
02159          MOVE W-XCTL-EL626      TO W-CALL-PGM                        CL**4
02160                                                                      CL**4
02161      ELSE                                                            CL**4
02162          IF  CLAIM-SESSION                                           CL**4
02163              MOVE W-XCTL-EL126  TO W-CALL-PGM                        CL**4
02164                                                                      CL**4
02165          ELSE                                                        CL**4
02166              IF  MORTGAGE-SESSION                                    CL**4
02167                  MOVE W-XCTL-EM626                                   CL**4
02168                                 TO W-CALL-PGM                        CL**4
02169                                                                      CL**4
02170              ELSE                                                    CL**4
02171                  IF  GENERAL-LEDGER-SESSION                          CL**4
02172                      MOVE W-XCTL-GL800                               CL**4
02173                                 TO W-CALL-PGM.                       CL**4
02174                                                                      CL**4
02175      GO TO 9400-XCTL.                                                CL**4
02176                                  EJECT                               CL**4
02177  8100-SEND-INITIAL-MAP.                                           EL602
02178 ******************************************************************   CL**4
02179 *                                                                *   CL**4
02180 *       THIS LOGIC SENDS THE INITIAL MAP.  IT WILL LOOK FOR      *   CL**4
02181 *       THE MAP DATA UNDER THE NAMES LISTED BELOW AND FOUND      *   CL**4
02182 *       IN THE WORK AREA SECTION OF WORKING STORAGE.             *   CL**4
02183 *                                                                *   CL**4
02184 ******************************************************************   CL**4
02185                                                                      CL**4
02186      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.                   CL**4
02187                                                                      CL**4
02188      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                        CL**4
02189      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                        CL**4
02190                                                                      CL**4
02191      IF  EMI-ERROR NOT EQUAL ER-9129                                 CL**4
02192          PERFORM 0250-CREATE-TEMP-STORAGE THRU 0250-EXIT.            CL**4
02193                                                                   EL602
02194      EXEC CICS SEND                                               EL602
02195          MAP    (W-MAP)                                              CL**4
02196          MAPSET (W-MAPSET)                                           CL**4
02197          FROM   (EL602AI)                                            CL**4
02198          ERASE                                                    EL602
02199          FREEKB                                                      CL**4
02200          CURSOR                                                   EL602
02201      END-EXEC.                                                    EL602
02202                                                                   EL602
02203      GO TO 9000-RETURN-TRANS.                                        CL**4
02204                                                                   EL602
02205  8100-EXIT.                                                          CL**4
02206      EXIT.                                                           CL**4
02207                                  EJECT                               CL**4
02208  8200-SEND-DATAONLY.                                              EL602
02209 ******************************************************************   CL**4
02210 *                                                                *   CL**4
02211 *       THIS LOGIC SENDS THE UPDATED VERSION OF THE MAP, USING   *   CL**4
02212 *       THE FIELDS LISTED BELOW WHICH SHOULD BE FOUND IN THE     *   CL**4
02213 *       WORK AREA OF WORKING STORAGE.                            *   CL**4
02214 *                                                                *   CL**4
02215 ******************************************************************   CL**4
02216                                                                      CL**4
02217      IF  EIBTRNID NOT EQUAL W-TRANSACTION                            CL**4
02218          GO TO 8100-SEND-INITIAL-MAP.                                CL**4
02219                                                                      CL**4
02220      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.                   CL**4
02221                                                                      CL**4
02222      IF  EIBAID = DFHPF11                                            CL**8
02223          MOVE 'Y'                TO EMI-ROLL-SWITCH                  CL**8
02224          PERFORM 9900-ERROR-FORMAT.                                  CL**8
02225                                                                      CL**8
02226      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                        CL**4
02227      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                        CL**4
02228                                                                      CL**4
02229      IF  EMI-ERROR NOT EQUAL ER-9129                                 CL**4
02230          PERFORM 0250-CREATE-TEMP-STORAGE THRU 0250-EXIT.            CL**4
02231                                                                   EL602
02232      EXEC CICS SEND                                               EL602
02233          MAP    (W-MAP)                                              CL**4
02234          MAPSET (W-MAPSET)                                           CL**4
02235          FROM   (EL602AI)                                            CL**4
02236          DATAONLY                                                 EL602
02237          FREEKB                                                      CL**4
02238          CURSOR                                                   EL602
02239      END-EXEC.                                                    EL602
02240                                                                   EL602
02241      GO TO 9000-RETURN-TRANS.                                        CL**4
02242                                                                      CL**4
02243  8200-EXIT.                                                          CL**4
02244      EXIT.                                                           CL**4
02245                                  EJECT                               CL**4
02246  8300-SEND-TEXT.                                                  EL602
02247 *****************************************************************    CL**4
02248 *    THIS PARAGRAPH SENDS THE COMMON LOGOFF MESSAGE.            *    CL**4
02249 *****************************************************************    CL**4
02250                                                                      CL**4
02251      EXEC CICS SEND TEXT                                          EL602
02252          FROM    (LOGOFF-TEXT)                                    EL602
02253          LENGTH  (LOGOFF-LENGTH)                                  EL602
02254          ERASE                                                    EL602
02255          FREEKB                                                   EL602
02256      END-EXEC.                                                    EL602
02257                                                                   EL602
02258  8300-EXIT.                                                          CL**4
02259      EXIT.                                                           CL**4
02260                                  EJECT                               CL**4
02261  8400-LOG-JOURNAL.                                                   CL**4
02262 ******************************************************************   CL**4
02263 *                                                                *   CL**4
02264 *       THIS LOGIC CREATES THE REQUIRED JOURNAL ENTRIES FOR      *   CL**4
02265 *       DATA BASE UPDATES.                                       *   CL**4
02266 *                                                                *   CL**4
02267 *       THE FOLLOWING FIELD ARE REQUIRED IN WORKING-STORAGE:     *   CL**4
02268 *                                                                *   CL**4
02269 *       W-CNTL-FILE-ID   PIC  X(08)                              *   CL**4
02270 *       W-JOURNAL-LENGTH PIC S9(04) COMP.                        *   CL**4
02271 *       W-THIS-PGM       PIC  X(08)                              *   CL**4
02272 *                                                                *   CL**4
02273 ******************************************************************   CL**4
02274                                                                      CL**4
02275      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                      CL**4
02276      MOVE W-CNTL-FILE-ID         TO JP-FILE-ID.                      CL**4
02277      MOVE W-THIS-PGM             TO JP-PROGRAM-ID.                   CL**4
02278                                                                      CL**4
pemuni*    IF  PI-JOURNAL-FILE-ID NOT = ZERO                               CL**4
pemuni*        EXEC CICS JOURNAL                                           CL**4
pemuni*            JFILEID (PI-JOURNAL-FILE-ID)                            CL**4
pemuni*            JTYPEID ('MP')                                          CL**4
pemnui*            FROM    (JOURNAL-RECORD)                                CL**4
pemuni*            LENGTH  (W-JOURNAL-LENGTH)                              CL**4
pemuni*        END-EXEC.                                                   CL**4
02286                                                                      CL**4
02287  8400-EXIT.                                                          CL**4
02288      EXIT.                                                           CL**4
02289                                  EJECT                               CL**4
02290  9000-RETURN-TRANS.                                                  CL**4
02291 *****************************************************************    CL**4
02292 *     THIS PARAGRAPH CAUSES THE PROGRAM TO EXIT TO A            *    CL**4
02293 *     TRANSACTION.                                              *    CL**4
02294 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        *    CL**4
02295 *     W-TRANSACTION          PIC  X(04)  VALUE 'XXXX'.          *    CL**4
02296 *****************************************************************    CL**4
02297                                                                      CL**4
02298      MOVE EMI-ERROR-NUMBER(1)     TO PI-LAST-ERROR-NO.               CL**4
02299                                                                      CL**4
02300      EXEC CICS RETURN                                             EL602
02301          TRANSID  (W-TRANSACTION)                                    CL**4
02302          COMMAREA (PROGRAM-INTERFACE-BLOCK)                          CL**4
02303          LENGTH   (PI-COMM-LENGTH)                                   CL**4
02304      END-EXEC.                                                       CL**4
02305                                                                      CL**4
02306  9000-EXIT.                                                          CL**4
02307      EXIT.                                                           CL**4
02308                                  EJECT                               CL**4
02309  9400-XCTL.                                                          CL**4
02310 *****************************************************************    CL**4
02311 *    THIS PARAGRAPH TRANSFERS CONTROL TO INDICATED PROGRAM.     *    CL**4
02312 *    PROGRAM MUST RESIDE IN W-CALL-PGM.                         *    CL**4
02313 *****************************************************************    CL**4
02314                                                                      CL**4
02315      IF  PI-MODIFICATIONS-MADE                                       CL**4
02316                                                                      CL**4
02317          IF  PI-FIRST-TIME                                           CL**4
02318                                                                      CL**4
02319              IF  W-MORTALITY-TABLE EQUAL LOW-VALUES                  CL**4
02320                  PERFORM 0200-RECOVER-TEMP-STORAGE                   CL**4
02321                      THRU 0200-EXIT                                  CL**4
02322                  MOVE 'N'            TO PI-FIRST-TIME-IND            CL**4
02323                  MOVE ER-7713        TO EMI-ERROR                    CL**4
02324                  MOVE -1             TO ENTERPFL                     CL**4
02325                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**4
02326                  PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT      CL**4
02327                  GO TO 8100-SEND-INITIAL-MAP                         CL**4
02328                                                                      CL**4
02329              ELSE                                                    CL**4
02330                  MOVE 'N'            TO PI-FIRST-TIME-IND            CL**4
02331                  MOVE ER-7713        TO EMI-ERROR                    CL**4
02332                  MOVE -1             TO ENTERPFL                     CL**4
02333                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**4
02334                  PERFORM 5000-MOVE-MORT-TBL-DATA THRU 5000-EXIT      CL**4
02335                  GO TO 8100-SEND-INITIAL-MAP.                        CL**4
02336                                                                      CL**4
02337      EXEC CICS XCTL                                                  CL**4
02338          PROGRAM  (W-CALL-PGM)                                       CL**4
02339          COMMAREA (PROGRAM-INTERFACE-BLOCK)                          CL**4
02340          LENGTH   (PI-COMM-LENGTH)                                   CL**4
02341      END-EXEC.                                                       CL**4
02342                                                                      CL**4
02343  9400-EXIT.                                                          CL**4
02344      EXIT.                                                           CL**4
02345                                  EJECT                               CL**4
02346  9500-LINK-DATE-CONVERT.                                             CL**4
02347 *****************************************************************    CL**4
02348 *    THIS PARAGRAPH 'CALLS' THE UTILITY DATE PROCESSOR.         *    CL**4
02349 *****************************************************************    CL**4
02350                                                                      CL**4
02351      EXEC CICS LINK                                                  CL**4
02352          PROGRAM    ('ELDATCV')                                      CL**4
02353          COMMAREA   (DATE-CONVERSION-DATA)                           CL**4
02354          LENGTH     (DC-COMM-LENGTH)                                 CL**4
02355          END-EXEC.                                                   CL**4
02356                                                                      CL**4
02357  9500-EXIT.                                                          CL**4
02358      EXIT.                                                           CL**4
02359                                  EJECT                               CL**4
02360  9600-FORMAT-DATE-TIME.                                              CL**4
02361 *****************************************************************    CL**4
02362 *     THIS LOGIC UPDATES THE DATE/TIME INFO ON GIVEN MAP        *    CL**4
02363 *****************************************************************    CL**4
02364                                                                      CL**4
02365      MOVE PI-COMPANY-ID          TO COMPANYO.                        CL**4
02366      MOVE W-SAVE-DATE            TO RUNDTEO.                         CL**4
02367                                                                      CL**4
02368      EXEC CICS ASKTIME                                               CL**4
02369      END-EXEC.                                                    EL602
02370                                                                   EL602
02371      MOVE EIBTIME                TO W-TIME-IN.                       CL**4
02372      MOVE W-TIME-OUT             TO RUNTIMEO.                        CL**4
02373 *    MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.            CL**4
02374                                                                   EL602
02375  9600-EXIT.                                                          CL**4
02376      EXIT.                                                           CL**4
02377                                  EJECT                               CL**4
02378  9700-PGMID-ERROR.                                                   CL**4
02379 *****************************************************************    CL**4
02380 *     THIS PARAGRAPH TRANSFERS CONTROL TO EL005 LOGOFF.         *    CL**4
02381 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        *    CL**4
02382 *     W-CALL-PGM             PIC  X(08)  VALUE 'EL000   '.      *    CL**4
02383 *     W-THIS-PGM             PIC  X(08).                        *    CL**4
02384 *     W-XCTL-005             PIC  X(08)  VALUE 'EL005   '.      *    CL**4
02385 *****************************************************************    CL**4
02386                                                                   EL602
02387      EXEC CICS  HANDLE CONDITION                                     CL**4
02388          PGMIDERR  (8300-SEND-TEXT)                               EL602
02389          END-EXEC.                                                   CL**4
02390                                                                   EL602
02391      MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.              CL**4
02392      MOVE ' '                    TO PI-ENTRY-CD-1.                   CL**4
02393      MOVE W-XCTL-005             TO W-CALL-PGM                       CL**4
02394                                     LOGOFF-PGM.                      CL**4
02395      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                     CL**4
02396                                                                   EL602
02397      GO TO 9400-XCTL.                                                CL**4
02398                                                                   EL602
02399  9700-EXIT.                                                       EL602
02400      EXIT.                                                        EL602
02401                                  EJECT                               CL**4
02402  9800-ABEND.                                                         CL**4
02403 *****************************************************************    CL**4
02404 *     THIS PARAGRAPH LINKS TO A COMMON ABEND ROUTINE.           *    CL**4
02405 *     THE FOLLOWING FIELDS ARE NEEDED IN WORKING-STORAGE        *    CL**4
02406 *     W-LINK-004             PIC  X(08)  VALUE 'EL004   '.      *    CL**4
02407 *****************************************************************    CL**4
02408                                                                   EL602
02409      MOVE W-LINK-004             TO W-CALL-PGM.                      CL**4
02410      MOVE DFHEIBLK               TO EMI-LINE1                        CL**4
02411                                                                      CL**4
02412      EXEC CICS  LINK                                                 CL**4
02413          PROGRAM   (W-CALL-PGM)                                      CL**4
02414          COMMAREA  (EMI-LINE1)                                       CL**4
02415          LENGTH    (72)                                              CL**4
02416          END-EXEC.                                                   CL**4
02417                                                                      CL**4
02418      GO TO 8200-SEND-DATAONLY.                                       CL**4
02419                                                                      CL**4
02420  9800-EXIT.                                                          CL**4
02421      EXIT.                                                           CL**4
02422                                  EJECT                               CL**4
02423  9900-ERROR-FORMAT.                                               EL602
02424                                                                      CL**4
02425      IF  EMI-ERRORS-COMPLETE                                         CL**8
02426          GO TO 9900-EXIT.                                            CL**8
02427                                                                      CL**4
02428      IF  EIBAID = DFHPF11                                            CL**8
02429          NEXT SENTENCE                                               CL**8
02430                                                                      CL**4
02431      ELSE                                                            CL**8
02432          IF  EMI-ERROR EQUAL W-LAST-ERROR                            CL**8
02433              GO TO 9900-EXIT                                         CL**8
02434                                                                      CL**8
02435          ELSE                                                        CL**8
02436              MOVE EMI-ERROR TO W-LAST-ERROR.                         CL**8
02437                                                                      CL**8
02438                                                                      CL**8
02439      MOVE W-LINK-001          TO W-CALL-PGM                          CL**8
02440                                                                      CL**8
02441      EXEC CICS LINK                                                  CL**8
02442          PROGRAM    (W-CALL-PGM)                                     CL**8
02443          COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)                  CL**8
02444          LENGTH     (EMI-COMM-LENGTH)                                CL**8
02445      END-EXEC.                                                       CL**8
02446                                                                   EL602
02447  9900-EXIT.                                                       EL602
02448      EXIT.                                                        EL602
02449                                                                      CL**4
02450                                  EJECT                               CL**4
02451  9910-INITIALIZE-SECURITY.                                           CL**4
02452                                                                      CL**4
02453      IF  MORTGAGE-SESSION                                            CL*13
02454          PERFORM 9915-INITIALIZE-CONV THRU 9915-EXIT                 CL*13
02455          GO TO 9910-EXIT.                                            CL*13
02456                                                                      CL*13
02457      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'                            CL**4
02458          EXEC CICS  READQ TS                                         CL**9
02459              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                     CL**9
02460              INTO    (SECURITY-CONTROL)                              CL**9
02461              LENGTH  (SC-COMM-LENGTH)                                CL**9
02462              ITEM    (SC-ITEM-CL-CR)                                 CL**9
02463              END-EXEC                                                CL**9
02464                                                                      CL**4
02465          MOVE SC-CREDIT-DISPLAY (09)                                 CL**9
02466                              TO PI-DISPLAY-CAP                       CL**9
02467          MOVE SC-CREDIT-UPDATE  (09)                                 CL**9
02468                              TO PI-MODIFY-CAP                        CL**9
02469                                                                      CL**9
02470          IF  NOT DISPLAY-CAP                                         CL**9
02471              MOVE 'READ'     TO SM-READ                              CL**9
02472              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL**4
02473              MOVE ER-0070    TO  EMI-ERROR                           CL**9
02474              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**4
02475              MOVE AL-UANON       TO MAINTA                           CL**9
02476              MOVE -1             TO MAINTL                           CL**9
02477              GO TO 8100-SEND-INITIAL-MAP.                            CL**4
02478                                                                      CL**4
02479  9910-EXIT.                                                          CL**4
02480      EXIT.                                                           CL**4
02481                                  EJECT                               CL**4
02482  9915-INITIALIZE-CONV.                                               CL*13
02483 ******************************************************************   CL**4
02484 *                                                                *   CL**4
02485 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *   CL*13
02486 *       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *   CL*13
02487 *       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *   CL*13
02488 *       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *   CL*13
02489 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *   CL*13
02490 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *   CL*13
02491 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *   CL*13
02492 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *   CL*13
02493 *                                                                *   CL**4
02494 *       NOTE:  THE CARRIER/GRP/STATE/PRODUCER SECURITY DATA      *   CL*13
02495 *       IS ALSO PROVIDED BY THIS LOGIC.                          *   CL*13
02496 *                                                                *   CL**4
02497 ******************************************************************   CL**4
02498                                                                      CL*13
02499      IF  PI-PROCESSOR-ID NOT EQUAL 'LGXX'                            CL*13
02500          MOVE '125E'             TO SC-QUID-SYSTEM                   CL*13
02501          MOVE EIBTRMID           TO SC-QUID-TERMINAL                 CL*13
02502                                                                      CL*13
02503          EXEC CICS READQ TS                                          CL*13
02504              QUEUE  (SC-QUID-KEY)                                    CL*13
02505              INTO   (SECURITY-CONTROL-E)                             CL*13
02506              LENGTH (SC-COMM-LENGTH-E)                               CL*13
02507              ITEM   (SC-ITEM)                                        CL*13
02508          END-EXEC                                                    CL*13
02509                                                                      CL*13
02510          MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)                       CL*13
02511                                  TO PI-DISPLAY-CAP                   CL*13
02512          MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)                        CL*13
02513                                  TO PI-MODIFY-CAP                    CL*13
02514                                                                      CL*13
02515          IF  NOT DISPLAY-CAP                                         CL*13
02516              MOVE 'READ'         TO SM-READ                          CL*13
02517              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT          CL*13
02518              MOVE ER-9097        TO EMI-ERROR                        CL*13
02519              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL*13
02520              PERFORM 8100-SEND-INITIAL-MAP.                          CL*13
02521                                                                      CL*13
02522  9915-EXIT.                                                          CL*13
02523      EXIT.                                                           CL*13
02524                                  EJECT                               CL*13
02525  9995-SECURITY-VIOLATION.                                            CL*13
02526                                                                      CL**4
02527      MOVE EIBDATE          TO SM-JUL-DATE.                           CL**4
02528      MOVE EIBTRMID         TO SM-TERMID.                             CL**4
02529      MOVE W-THIS-PGM       TO SM-PGM.                                CL**4
02530      MOVE EIBTIME          TO W-TIME-IN.                             CL**4
02531      MOVE W-TIME-OUT       TO SM-TIME.                               CL**4
02532      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.                       CL**4
02533                                                                   EL602
02534      EXEC CICS LINK                                               EL602
02535           PROGRAM  ('EL003')                                         CL**4
02536           COMMAREA (SECURITY-MESSAGE)                                CL**4
02537           LENGTH   (80)                                              CL**4
02538      END-EXEC.                                                    EL602
02539                                                                   EL602
02540  9995-EXIT.                                                       EL602
02541      EXIT.                                                           CL**4
02542                                  EJECT                               CL**4
02543  9999-GOBACK.                                                        CL**4
02544      GOBACK.                                                         CL**4
02545                                                                      CL**4
02546  9999-EXIT.                                                          CL**4
02547      EXIT.                                                           CL**4
