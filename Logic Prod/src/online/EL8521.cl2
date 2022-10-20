00001  IDENTIFICATION DIVISION.                                         11/12/96
00002                                                                   EL8521
00003  PROGRAM-ID.                 EL8521.                                 LV008
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/14/96 08:06:29.                    CL**7
00007 *                            VMOD=2.008                              CL**8
00008 *                                                                 EL8521
00008 *                                                                 EL8521
00009 *AUTHOR.     LOGIC,INC.                                              CL**7
00010 *            DALLAS, TEXAS.                                          CL**7
00011                                                                   EL8521
00012 *DATE-COMPILED.                                                      CL**7
00013 *SECURITY.   *****************************************************   CL**7
00014 *            *                                                   *   CL**7
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00016 *            *                                                   *   CL**7
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00018 *                                                                *   CL**7
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00021 *            *                                                   *   CL**7
00022 *            *****************************************************   CL**7
00023                                                                   EL8521
00024 *REMARKS. TRANSACTION - EXJ6 - ACCOUNTS RECEIVABLE                   CL**7
00025 *                              REQUEST FILE SELECTION.               CL**7
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
122002******************************************************************
00026                                                                   EL8521
00027  ENVIRONMENT DIVISION.                                            EL8521
00028                                                                   EL8521
00029      EJECT                                                        EL8521
00030  DATA DIVISION.                                                   EL8521
00031  WORKING-STORAGE SECTION.                                         EL8521
00032                                                                   EL8521
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL8521
00034  77  FILLER  PIC X(32)  VALUE '*   EL8521 WORKING STORAGE     *'. EL8521
00035  77  FILLER  PIC X(32)  VALUE '************VMOD=2.008 *********'.    CL**8
00036                                                                   EL8521
00037     EJECT                                                         EL8521
00038                                                                   EL8521
00039  77  FIRST-ERROR-SCAN-SW     PIC X         VALUE 'N'.                CL**3
00040      88  FIRST-ERROR-NOT-FOUND             VALUE 'N'.                CL**3
00041  77  UPDATE-RQST-SW          PIC X         VALUE 'N'.                CL**4
00042      88  UPDATE-RQST-DT                    VALUE 'Y'.                CL**4
00043  77  UPDATE-STMT-SW          PIC X         VALUE 'N'.                CL**4
00044      88  UPDATE-STMT-DT                    VALUE 'Y'.                CL**4
00045  77  UPDATE-STATUS-SW        PIC X         VALUE 'N'.                CL**4
00046      88  UPDATE-STATUS                     VALUE 'Y'.                CL**4
00047                              COPY ELCSCTM.                           CL**6
00048                              COPY ELCSCRTY.                          CL**6
00049                                                                   EL8521
00050     EJECT                                                         EL8521
00051                                                                   EL8521
00052 ******************************************************************EL8521
00053 *                                                                *EL8521
00054 *              S T A N D A R D   A R E A S                       *EL8521
00055 *                                                                *EL8521
00056 ******************************************************************EL8521
00057                                                                   EL8521
00058  01  STANDARD-AREAS.                                              EL8521
00059      12  SC-ITEM                 PIC S9(4)   VALUE +1    COMP.    EL8521
00060      12  QID.                                                     EL8521
00061          16  QID-TERM            PIC X(4)      VALUE SPACES.      EL8521
00062          16  FILLER              PIC X(4)      VALUE '125D'.      EL8521
00063      12  GETMAIN-SPACE           PIC X       VALUE SPACE.         EL8521
00064      12  EL852B                  PIC X(8)    VALUE 'EL852B'.      EL8521
00065      12  EL852C                  PIC X(8)    VALUE 'EL852C'.      EL8521
00066      12  EL852D                  PIC X(8)    VALUE 'EL852D'.      EL8521
00067      12  MAPSET-EL8521S          PIC X(8)    VALUE 'EL8521S'.     EL8521
00068      12  TRANS-EXJ6              PIC X(4)    VALUE 'EXJ6'.        EL8521
00069      12  THIS-PGM                PIC X(8)    VALUE 'EL8521'.      EL8521
00070      12  PGM-NAME                PIC X(8).                        EL8521
00071      12  TIME-IN                 PIC S9(7).                       EL8521
00072      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL8521
00073          16  FILLER              PIC X.                           EL8521
00074          16  TIME-OUT            PIC 99V99.                       EL8521
00075          16  FILLER              PIC X(2).                        EL8521
00076      12  LINK-EL001              PIC X(8)    VALUE 'EL001'.       EL8521
00077      12  LINK-EL004              PIC X(8)    VALUE 'EL004'.       EL8521
00078      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.       EL8521
00079      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.       EL8521
00080      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.       EL8521
00081      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.     EL8521
00082      12  FILE-ID-ERRQST          PIC X(8)    VALUE 'ERRQST'.      EL8521
00083      12  FILE-ID-ERACCT          PIC X(8)    VALUE 'ERACCT'.      EL8521
00084      12  FILE-ID-ERCOMP          PIC X(8)    VALUE 'ERCOMP'.      EL8521
00085      12  FILE-ID-ERSUMM          PIC X(8)    VALUE 'ERSUMM'.      EL8521
00086      12  FILE-ID-ERPNDB          PIC X(8)    VALUE 'ERPNDB'.         CL**8
00087      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.        EL8521
00088      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.        EL8521
00089      12  WS-DATA-SELECTED-SW     PIC X       VALUE SPACE.         EL8521
00090          88 WS-DATA-SELECTED                 VALUE 'Y'.           EL8521
00091      12  WS-OPTION-COUNTER       PIC S999    VALUE ZEROS COMP-3.  EL8521
00092      12  WS-MAINT-FUNCTION       PIC X       VALUE SPACE.         EL8521
00093          88 WS-BROWSE-FUNCTION               VALUE 'B'.           EL8521
00094          88 WS-SUBMIT-FUNCTION               VALUE 'S'.           EL8521
00095          88 WS-RESUBMIT-FUNCTION             VALUE 'R'.           EL8521
00096      12  WS-BROWSE-STARTED-SW    PIC X       VALUE SPACE.         EL8521
00097          88 WS-BROWSE-STARTED                VALUE 'Y'.           EL8521
00098      12  WS-SUB1                 PIC S9(4)   VALUE ZEROS COMP.    EL8521
00099      12  WS-AM-NAME              PIC X(30)   VALUE SPACES.        EL8521
00100      12  WS-FIN-RESP-NAME        PIC X(30)   VALUE SPACES.        EL8521
00101      12  WS-RQST-ACCT-CONTROL    PIC X(20)   VALUE SPACES.        EL8521
00102      12  WS-RQST-FIN-RESP-CNTL   PIC X(18)   VALUE SPACES.        EL8521
00103      12  WS-SUM-NAME             PIC X(30)   VALUE SPACES.        EL8521
00104      12  WS-SAVE-RQST-DT         PIC XX      VALUE LOW-VALUES.       CL**4
00105      12  WS-SAVE-STMT-DT         PIC XX      VALUE LOW-VALUES.       CL**4
00106      12  WS-SAVE-STATUS          PIC X       VALUE SPACES.           CL**4
00107      12  WS-TRAILER-ONLY-SW      PIC X       VALUE 'Y'.              CL**8
00108          88 WS-TRAILER-ONLY                  VALUE 'Y'.              CL**8
00109      12  WS-NEW-ACCT-MAST-SW     PIC X.                              CL**8
00110          88 WS-NEW-ACCT-MAST                 VALUE 'Y'.              CL**8
00111      12  WS-ACCOUNT-MASTER-SW    PIC X.                              CL**8
00112          88 WS-ACCOUNT-MASTER                VALUE 'Y'.              CL**8
00113      12  WS-NO-ACCOUNT-MASTER-SW PIC X.                              CL**8
00114          88 WS-NO-ACCOUNT-MASTER             VALUE 'Y'.              CL**8
00115      12  WS-AGT-ERROR-SW         PIC X.                              CL**8
00116          88 WS-AGT-ERROR                     VALUE 'Y'.              CL**8
00117      12  WS-FIN-RESP-ERROR-SW    PIC X.                              CL**8
00118          88 WS-FIN-RESP-ERROR                VALUE 'Y'.              CL**8
00119      12  WS-AGT-COMP-ERROR-SW    PIC X.                              CL**8
00120          88 WS-AGT-COMP-ERROR                VALUE 'Y'.              CL**8
00121      12  WS-FIN-RESP-COMP-ERROR-SW                                   CL**8
00122                                  PIC X.                              CL**8
00123          88 WS-FIN-RESP-COMP-ERROR           VALUE 'Y'.              CL**8
00124      12  WS-ACCESS-ERPNDB.                                           CL**8
00125          16  WS-ACCESS-CO-ID     PIC X.                              CL**8
00126          16  WS-ACCESS-BATCH     PIC X(6).                           CL**8
00127          16  WS-ACCESS-SEQ       PIC S9(4)   COMP.                   CL**8
00128          16  WS-ACCESS-CHG-SEQ   PIC S9(4)   COMP.                   CL**8
00129      12  WS-PREV-PNDB.                                               CL**8
00130          16  WS-PREV-COMPANY     PIC X.                              CL**8
00131          16  WS-PREV-BATCH       PIC X(6).                           CL**8
00132      12  WS-SAVE-AM-CONTROLS.                                        CL**8
00133          16  WS-SAVE-CARRIER     PIC X.                              CL**8
00134          16  WS-SAVE-GROUPING    PIC X(6).                           CL**8
00135          16  WS-SAVE-STATE       PIC X(2).                           CL**8
00136          16  WS-SAVE-ACCT-AGENT  PIC X(10).                          CL**8
00137          16  WS-SAVE-FIN-RESP    PIC X(10).                          CL**8
00138      12  WS-SAVE-AM-EFF-DT       PIC XX      VALUE LOW-VALUES.       CL**8
00139      12  WS-SAVE-AM-EXP-DT       PIC XX      VALUE LOW-VALUES.       CL**8
00140      12  WS-SAVE-SUMMARY         PIC X(6).                           CL**8
00141                                                                   EL8521
00142      EJECT                                                        EL8521
00143                                                                   EL8521
00144 ******************************************************************EL8521
00145 *                                                                *EL8521
00146 *                E R R O R   M E S S A G E S                     *EL8521
00147 *                                                                *EL8521
00148 ******************************************************************EL8521
00149                                                                   EL8521
00150  01  ERROR-MESSAGES.                                              EL8521
00151      12  ER-0000                 PIC X(4)  VALUE '0000'.          EL8521
00152      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL8521
00153      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL8521
00154      12  ER-0023                 PIC X(4)  VALUE '0023'.          EL8521
00155      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL8521
00156      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL8521
00157      12  ER-2211                 PIC X(4)  VALUE '2211'.             CL**8
00158      12  ER-2132                 PIC X(4)  VALUE '2132'.          EL8521
00159      12  ER-2134                 PIC X(4)  VALUE '2134'.          EL8521
00160      12  ER-2210                 PIC X(4)  VALUE '2210'.          EL8521
00161      12  ER-2242                 PIC X(4)  VALUE '2242'.             CL**8
00162      12  ER-2251                 PIC X(4)  VALUE '2251'.          EL8521
00163      12  ER-2252                 PIC X(4)  VALUE '2252'.          EL8521
00164      12  ER-2800                 PIC X(4)  VALUE '2800'.          EL8521
00165      12  ER-2919                 PIC X(4)  VALUE '2919'.          EL8521
00166      12  ER-2935                 PIC X(4)  VALUE '2935'.          EL8521
00167      12  ER-3131                 PIC X(4)  VALUE '3131'.          EL8521
00168      12  ER-3132                 PIC X(4)  VALUE '3132'.          EL8521
00169      12  ER-3134                 PIC X(4)  VALUE '3134'.          EL8521
00170      12  ER-3136                 PIC X(4)  VALUE '3136'.          EL8521
00171      12  ER-3191                 PIC X(4)  VALUE '3191'.             CL**5
00172                                                                   EL8521
00173      EJECT                                                        EL8521
00174                                                                   EL8521
00175 ******************************************************************EL8521
00176 *                                                                *EL8521
00177 *              A C C E S S   K E Y S                             *EL8521
00178 *                                                                *EL8521
00179 ******************************************************************EL8521
00180                                                                   EL8521
00181  01  ACCESS-KEYS.                                                 EL8521
00182                                                                   EL8521
00183      12  ERRQST-KEY.                                              EL8521
00184          16  ERRQST-COMPANY-CD       PIC X     VALUE SPACE.       EL8521
00185          16  ERRQST-ENTRY-BATCH      PIC X(6)  VALUE SPACES.      EL8521
00186                                                                   EL8521
00187      12  ERRQST-ALT-KEY1.                                         EL8521
00188          16  ERRQST-ACCT-CONTROL.                                 EL8521
00189              20 ERRQST-COMPANY-CD-A1 PIC X     VALUE SPACES.      EL8521
00190              20 ERRQST-CARRIER-A1    PIC X     VALUE SPACES.      EL8521
00191              20 ERRQST-GROUPING-A1   PIC X(6)  VALUE SPACES.      EL8521
00192              20 ERRQST-STATE-A1      PIC XX    VALUE SPACES.      EL8521
00193              20 ERRQST-ACCOUNT-A1    PIC X(10) VALUE SPACES.      EL8521
00194          16  ERRQST-REFERENCE-A1     PIC X(12) VALUE SPACES.      EL8521
00195          16  ERRQST-BATCH-A1         PIC X(6)  VALUE SPACES.      EL8521
00196                                                                   EL8521
00197      12  ERRQST-ALT-KEY2.                                         EL8521
00198          16  ERRQST-FIN-RESP-CONTROL.                             EL8521
00199              20 ERRQST-COMPANY-CD-A2 PIC X     VALUE SPACES.      EL8521
00200              20 ERRQST-CARRIER-A2    PIC X     VALUE SPACES.      EL8521
00201              20 ERRQST-GROUPING-A2   PIC X(6)  VALUE SPACES.      EL8521
00202              20 ERRQST-FIN-RESP-A2   PIC X(10) VALUE SPACES.      EL8521
00203          16  ERRQST-ACCOUNT-A2       PIC X(10) VALUE SPACES.      EL8521
00204          16  ERRQST-REFERENCE-A2     PIC X(12) VALUE SPACES.      EL8521
00205          16  ERRQST-BATCH-A2         PIC X(6)  VALUE SPACES.      EL8521
00206                                                                   EL8521
00207      12  ERRQST-ALT-KEY3.                                         EL8521
00208          16  ERRQST-COMPANY-CD-A3    PIC X     VALUE SPACES.      EL8521
00209          16  ERRQST-CARRIER-A3       PIC X     VALUE SPACES.      EL8521
00210          16  ERRQST-GROUPING-A3      PIC X     VALUE SPACES.      EL8521
00211          16  ERRQST-ACCOUNT-AGENT    PIC X     VALUE SPACES.      EL8521
00212          16  ERRQST-BATCH-A3         PIC X     VALUE SPACES.      EL8521
00213                                                                   EL8521
00214      12  ERRQST-ALT-KEY4.                                         EL8521
00215          16  ERRQST-COMPANY-CD-A4    PIC X     VALUE SPACES.      EL8521
00216          16  ERRQST-SUMMARY-CODE     PIC X(6)  VALUE SPACES.      EL8521
00217          16  ERRQST-ACCOUNT-A4       PIC X(10) VALUE SPACES.      EL8521
00218          16  ERRQST-REFERENCE-A4     PIC X(12) VALUE SPACES.      EL8521
00219          16  ERRQST-BATCH-A4         PIC X(6)  VALUE SPACES.      EL8521
00220                                                                   EL8521
00221      12  ERRQST-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.   EL8521
00222      12  ERRQST-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.   EL8521
00223                                                                   EL8521
00224      12  ERCOMP-KEY.                                              EL8521
00225          16  ERCOMP-COMP-CD          PIC X     VALUE SPACE.       EL8521
00226          16  ERCOMP-CARRIER          PIC X     VALUE SPACES.      EL8521
00227          16  ERCOMP-GROUPING         PIC X(6)  VALUE SPACES.      EL8521
00228          16  ERCOMP-FIN-RESP         PIC X(10) VALUE SPACES.      EL8521
00229          16  ERCOMP-ACCOUNT          PIC X(10) VALUE SPACES.      EL8521
00230          16  ERCOMP-RECORD-TYPE      PIC X     VALUE SPACES.      EL8521
00231                                                                   EL8521
00232      12  ERACCT-KEY.                                              EL8521
00233          16  ERACCT-COMP-KEY.                                     EL8521
00234              20  ERACCT-COMP-CD     PIC X     VALUE SPACES.       EL8521
00235              20  ERACCT-CARRIER     PIC X     VALUE SPACES.       EL8521
00236              20  ERACCT-GROUPING    PIC X(6)  VALUE SPACES.       EL8521
00237              20  ERACCT-STATE       PIC XX    VALUE SPACES.       EL8521
00238              20  ERACCT-ACCOUNT     PIC X(10) VALUE SPACES.       EL8521
00239          16  ACCT-EXP-DATE          PIC XX    VALUE SPACES.       EL8521
00240          16  FILLER                 PIC X(4)  VALUE LOW-VALUES.   EL8521
00241                                                                   EL8521
00242      12  ERACCT-SAVE-KEY            PIC X(20) VALUE SPACES.       EL8521
00243                                                                   EL8521
00244      12  ERSUMM-KEY.                                              EL8521
00245          16  ERSUMM-COMPANY-CD      PIC X     VALUE SPACE.        EL8521
00246          16  ERSUMM-SUMMARY         PIC X(6)  VALUE SPACE.        EL8521
00247          16  ERSUMM-CARRIER         PIC X     VALUE SPACE.        EL8521
00248          16  ERSUMM-GROUP           PIC X(6)  VALUE SPACE.        EL8521
00249          16  ERSUMM-FIN-RESP        PIC X(10) VALUE SPACE.        EL8521
00250          16  ERSUMM-ACCT-AGENT      PIC X(10) VALUE SPACE.        EL8521
00251                                                                   EL8521
00252      12  ERPNDB-KEY.                                                 CL**8
00253          16  ERPNDB-COMPANY-CD      PIC X     VALUE SPACE.           CL**8
00254          16  ERPNDB-BATCH           PIC X(6)  VALUE SPACE.           CL**8
00255          16  ERPNDB-BATCH-SEQ       PIC S9(4) VALUE ZEROS COMP.      CL**8
00256          16  ERPNDB-BATCH-CHG-SEQ   PIC S9(4) VALUE ZEROS COMP.      CL**8
00257      EJECT                                                        EL8521
00258                                                                   EL8521
00259                              COPY ELCDATE.                           CL**6
00260                                                                   EL8521
00261      EJECT                                                        EL8521
00262                              COPY ELCLOGOF.                          CL**6
00263                                                                   EL8521
00264      EJECT                                                        EL8521
00265                              COPY ELCATTR.                           CL**6
00266                                                                   EL8521
00267      EJECT                                                        EL8521
00268                              COPY ELCEMIB.                           CL**8
00269                                                                   EL8521
00270      EJECT                                                        EL8521
00271                              COPY ELCINTF.                           CL**6
00272                              COPY ELC852PI.                          CL**8
00273      EJECT                                                        EL8521
00274                              COPY ELCJPFX.                           CL**6
00275                              PIC X(223).                          EL8521
00276                                                                   EL8521
00277      EJECT                                                        EL8521
00278                              COPY ELCAID.                            CL**6
00279  01  FILLER    REDEFINES DFHAID.                                  EL8521
00280      12  FILLER              PIC X(8).                            EL8521
00281      12  PF-VALUES           PIC X       OCCURS 2.                EL8521
00282                                                                   EL8521
00283      EJECT                                                        EL8521
00284                              COPY EL8521S.                           CL**6
00285  01  DISPLAY-MAP REDEFINES EL852BI.                               EL8521
00286      12  FILLER                  PIC X(136).                      EL8521
00287      12  AR-REQUESTS OCCURS 10 TIMES.                             EL8521
00288          16  AR-SEQ-LEN          PIC S9(4)   COMP.                EL8521
00289          16  AR-SEQ-ATTRB        PIC X.                           EL8521
00290          16  AR-SEQ              PIC 99.                          EL8521
00291          16  AR-CAR-LEN          PIC S9(4)   COMP.                EL8521
00292          16  AR-CAR-ATTRB        PIC X.                           EL8521
00293          16  AR-CAR              PIC X.                           EL8521
00294          16  AR-GRP-LEN          PIC S9(4)   COMP.                EL8521
00295          16  AR-GRP-ATTRB        PIC X.                           EL8521
00296          16  AR-GRP              PIC X(6).                        EL8521
00297          16  AR-ST-LEN           PIC S9(4)   COMP.                EL8521
00298          16  AR-ST-ATTRB         PIC X.                           EL8521
00299          16  AR-ST               PIC XX.                          EL8521
00300          16  AR-ACCT-LEN         PIC S9(4)   COMP.                EL8521
00301          16  AR-ACCT-ATTRB       PIC X.                           EL8521
00302          16  AR-ACCT             PIC X(10).                       EL8521
00303          16  AR-REF-LEN          PIC S9(4)   COMP.                EL8521
00304          16  AR-REF-ATTRB        PIC X.                           EL8521
00305          16  AR-REF              PIC X(12).                       EL8521
00306          16  AR-BATCH-LEN        PIC S9(4)   COMP.                EL8521
00307          16  AR-BATCH-ATTRB      PIC X.                           EL8521
00308          16  AR-BATCH            PIC X(6).                        EL8521
00309          16  AR-SUM-LEN          PIC S9(4)   COMP.                EL8521
00310          16  AR-SUM-ATTRB        PIC X.                           EL8521
00311          16  AR-SUM              PIC X(6).                        EL8521
00312          16  AR-ENTRY-DT-LEN     PIC S9(4)   COMP.                EL8521
00313          16  AR-ENTRY-DT-ATTRB   PIC X.                           EL8521
00314          16  AR-ENTRY-DT         PIC X(6).                        EL8521
00315          16  AR-REQST-DT-LEN     PIC S9(4)   COMP.                EL8521
00316          16  AR-REQST-DT-ATTRB   PIC X.                           EL8521
00317          16  AR-REQST-DT         PIC X(6).                        EL8521
00318          16  AR-STMT-DT-LEN      PIC S9(4)   COMP.                EL8521
00319          16  AR-STMT-DT-ATTRB    PIC X.                           EL8521
00320          16  AR-STMT-DT          PIC X(6).                        EL8521
00321          16  AR-STATUS-LEN       PIC S9(4)   COMP.                EL8521
00322          16  AR-STATUS-ATTRB     PIC X.                           EL8521
00323          16  AR-STATUS           PIC X.                           EL8521
00324      12  AR-SELECT-LEN           PIC S9(4)   COMP.                EL8521
00325      12  AR-SELECT-ATTRB         PIC X.                           EL8521
00326      12  AR-SELECT               PIC 99.                          EL8521
00327      12  AR-SELECT-SPACES  REDEFINES  AR-SELECT                      CL**8
00328                                  PIC XX.                             CL**8
00329                                                                   EL8521
00330      EJECT                                                        EL8521
00331  LINKAGE SECTION.                                                 EL8521
00332  01  DFHCOMMAREA             PIC X(1024).                         EL8521
00333      EJECT                                                        EL8521
00334                              COPY ERCRQST.                           CL**6
00335      EJECT                                                        EL8521
00336                              COPY ERCCOMP.                           CL**6
00337      EJECT                                                        EL8521
00338                              COPY ERCACCT.                           CL**6
00339      EJECT                                                        EL8521
00340                              COPY ERCSUMM.                           CL**6
00341      EJECT                                                           CL**8
00342                              COPY ERCPNDB.                           CL**8
00343      EJECT                                                        EL8521
00344                                                                   EL8521
00345  PROCEDURE DIVISION.                                              EL8521
00346                                                                   EL8521
00347      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL8521
00348      MOVE 1                      TO EMI-NUMBER-OF-LINES.          EL8521
00349                                                                   EL8521
00350      IF EIBCALEN = 0                                              EL8521
00351          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL8521
00352                                                                   EL8521
00353      MOVE EIBTRMID               TO QID-TERM.                     EL8521
00354      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL8521
00355      MOVE '5'                    TO DC-OPTION-CODE.               EL8521
00356      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL8521
00357      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL8521
00358      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.                EL8521
00359                                                                   EL8521
00360      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL8521
00361          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL8521
00362              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL8521
00363              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL8521
00364              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL8521
00365              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL8521
00366              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL8521
00367              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL8521
00368              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL8521
00369              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL8521
00370          ELSE                                                     EL8521
00371              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL8521
00372              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL8521
00373              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL8521
00374              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL8521
00375              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL8521
00376              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL8521
00377              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL8521
00378              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL8521
00379                                                                   EL8521
00380      MOVE LOW-VALUES             TO EL852BI.                      EL8521
00381                                                                   EL8521
00382      EXEC CICS HANDLE CONDITION                                   EL8521
00383          PGMIDERR  (9600-PGMID-ERROR)                             EL8521
00384          ERROR     (9990-ABEND)                                   EL8521
00385      END-EXEC.                                                    EL8521
00386                                                                   EL8521
00387      IF EIBTRNID NOT = TRANS-EXJ6                                 EL8521
00388         IF PI-MAP-NAME = EL852D                                   EL8521
00389            MOVE PI-SELECT-KEY      TO PI-ACCESS-KEY               EL8521
00390            GO TO 3200-DISPLAY-BATCH-REQUEST.                      EL8521
00391                                                                   EL8521
00392      IF EIBTRNID NOT = TRANS-EXJ6                                 EL8521
00393         MOVE PI-SELECT-KEY         TO PI-ACCESS-KEY               EL8521
00394         GO TO 3000-DISPLAY-REQUESTS.                              EL8521
00395                                                                   EL8521
00396      IF EIBAID = DFHCLEAR                                         EL8521
00397         EXEC CICS ASKTIME                                         EL8521
00398         END-EXEC                                                  EL8521
00399         IF PI-MAP-NAME = EL852D                                   EL8521
00400            IF PI-REQUEST-KEY (1) NOT = SPACES                     EL8521
00401               MOVE PI-PREV-MAP-NAME TO PI-MAP-NAME                EL8521
00402               MOVE SPACES           TO PI-PREV-MAP-NAME           EL8521
00403               MOVE PI-REQUEST-KEY (1) TO PI-ACCESS-KEY            EL8521
00404               GO TO 3000-DISPLAY-REQUESTS.                        EL8521
00405                                                                   EL8521
00406      IF EIBAID = DFHCLEAR                                         EL8521
00407          GO TO 9400-CLEAR.                                        EL8521
00408                                                                   EL8521
00409      IF PI-PROCESSOR-ID = 'LGXX'                                  EL8521
00410          GO TO 0200-RECEIVE.                                      EL8521
00411                                                                   EL8521
00412      EXEC CICS READQ TS                                           EL8521
00413          QUEUE  (QID)                                             EL8521
00414          INTO   (SECURITY-CONTROL)                                EL8521
00415          LENGTH (SC-COMM-LENGTH)                                  EL8521
00416          ITEM   (SC-ITEM)                                         EL8521
00417      END-EXEC.                                                    EL8521
00418                                                                   EL8521
00419      MOVE SC-CREDIT-DISPLAY (3)   TO PI-DISPLAY-CAP.              EL8521
00420      MOVE SC-CREDIT-UPDATE  (3)   TO PI-MODIFY-CAP.               EL8521
00421                                                                   EL8521
00422      IF NOT DISPLAY-CAP                                           EL8521
00423          MOVE 'READ'          TO SM-READ                          EL8521
00424          PERFORM 9995-SECURITY-VIOLATION                          EL8521
00425          MOVE ER-0070         TO  EMI-ERROR                       EL8521
00426          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL8521
00427          GO TO 8100-SEND-INITIAL-MAP.                             EL8521
00428                                                                   EL8521
00429      EJECT                                                        EL8521
00430                                                                   EL8521
00431 ******************************************************************EL8521
00432 *                                                                *EL8521
00433 *              R E C E I V E   M A P S                           *EL8521
00434 *                                                                *EL8521
00435 ******************************************************************EL8521
00436                                                                   EL8521
00437  0200-RECEIVE.                                                    EL8521
00438                                                                   EL8521
00439      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL8521
00440          MOVE ER-0008            TO EMI-ERROR                     EL8521
00441          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL8521
00442          IF PI-MAP-NAME = EL852B                                  EL8521
00443             MOVE -1              TO BPFENTRL                      EL8521
00444             GO TO 8200-SEND-DATAONLY                              EL8521
00445          ELSE                                                     EL8521
00446             IF PI-MAP-NAME = EL852C                               EL8521
00447                MOVE -1           TO CPFENTRL                      EL8521
00448                GO TO 8200-SEND-DATAONLY                           EL8521
00449             ELSE                                                  EL8521
00450                MOVE -1           TO DPFENTRL                      EL8521
00451                GO TO 8200-SEND-DATAONLY.                          EL8521
00452                                                                   EL8521
00453      EXEC CICS RECEIVE                                            EL8521
00454          MAP      (PI-MAP-NAME)                                   EL8521
00455          MAPSET   (MAPSET-EL8521S)                                EL8521
00456          INTO     (EL852BI)                                       EL8521
00457      END-EXEC.                                                    EL8521
00458                                                                   EL8521
00459      IF PI-MAP-NAME = EL852B                                      EL8521
00460          IF BPFENTRL GREATER THAN ZERO                            EL8521
00461              IF EIBAID NOT = DFHENTER                             EL8521
00462                  MOVE ER-0004    TO EMI-ERROR                     EL8521
00463                  MOVE AL-UNBOF   TO BPFENTRA                      EL8521
00464                  MOVE -1         TO BPFENTRL                      EL8521
00465                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL8521
00466                  GO TO 8200-SEND-DATAONLY                         EL8521
00467              ELSE                                                 EL8521
00468                  IF BPFENTRI NUMERIC AND                          EL8521
00469                     BPFENTRI GREATER 0 AND LESS 25                   CL**4
00470                      MOVE PF-VALUES (BPFENTRI) TO EIBAID          EL8521
00471                  ELSE                                             EL8521
00472                      MOVE ER-0029  TO EMI-ERROR                   EL8521
00473                      MOVE AL-UNBOF TO BPFENTRA                    EL8521
00474                      MOVE -1       TO BPFENTRL                    EL8521
00475                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL8521
00476                      GO TO 8200-SEND-DATAONLY.                    EL8521
00477                                                                   EL8521
00478      IF PI-MAP-NAME = EL852C                                      EL8521
00479          IF CPFENTRL GREATER THAN ZERO                            EL8521
00480              IF EIBAID NOT = DFHENTER                             EL8521
00481                  MOVE ER-0004    TO EMI-ERROR                     EL8521
00482                  MOVE AL-UNBOF   TO CPFENTRA                      EL8521
00483                  MOVE -1         TO CPFENTRL                      EL8521
00484                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL8521
00485                  GO TO 8200-SEND-DATAONLY                         EL8521
00486              ELSE                                                 EL8521
00487                  IF CPFENTRI NUMERIC AND                          EL8521
00488                     CPFENTRI GREATER 0 AND LESS 25                   CL**4
00489                      MOVE PF-VALUES (CPFENTRI) TO EIBAID          EL8521
00490                  ELSE                                             EL8521
00491                      MOVE ER-0029  TO EMI-ERROR                   EL8521
00492                      MOVE AL-UNBOF TO BPFENTRA                    EL8521
00493                      MOVE -1       TO BPFENTRL                    EL8521
00494                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL8521
00495                      GO TO 8200-SEND-DATAONLY.                    EL8521
00496                                                                   EL8521
00497      IF PI-MAP-NAME = EL852D                                      EL8521
00498          IF CPFENTRL GREATER THAN ZERO                            EL8521
00499              IF EIBAID NOT = DFHENTER                             EL8521
00500                  MOVE ER-0004    TO EMI-ERROR                     EL8521
00501                  MOVE AL-UNBOF   TO DPFENTRA                      EL8521
00502                  MOVE -1         TO DPFENTRL                      EL8521
00503                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL8521
00504                  GO TO 8200-SEND-DATAONLY                         EL8521
00505              ELSE                                                 EL8521
00506                  IF CPFENTRI NUMERIC AND                          EL8521
00507                     CPFENTRI GREATER 0 AND LESS 25                   CL**4
00508                      MOVE PF-VALUES (DPFENTRI) TO EIBAID          EL8521
00509                  ELSE                                             EL8521
00510                      MOVE ER-0029  TO EMI-ERROR                   EL8521
00511                      MOVE AL-UNBOF TO DPFENTRA                    EL8521
00512                      MOVE -1       TO DPFENTRL                    EL8521
00513                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT     EL8521
00514                      GO TO 8200-SEND-DATAONLY.                    EL8521
00515                                                                   EL8521
00516      EJECT                                                        EL8521
00517                                                                   EL8521
00518 ******************************************************************EL8521
00519 *                                                                *EL8521
00520 *              C H E C K   P F K E Y S                           *EL8521
00521 *                                                                *EL8521
00522 ******************************************************************EL8521
00523                                                                   EL8521
00524  0300-CHECK-PFKEYS.                                               EL8521
00525                                                                   EL8521
00526      IF EIBAID = DFHPF23                                          EL8521
00527          GO TO 8810-PF23.                                         EL8521
00528                                                                   EL8521
00529      IF EIBAID = DFHPF24                                          EL8521
00530          GO TO 9200-RETURN-MAIN-MENU.                             EL8521
00531                                                                   EL8521
00532      IF EIBAID = DFHPF12                                          EL8521
00533          GO TO 9500-PF12.                                         EL8521
00534                                                                   EL8521
00535      IF EIBAID = DFHENTER                                         EL8521
00536          GO TO 1000-EDIT-MAPS.                                    EL8521
00537                                                                   EL8521
00538      IF PI-MAP-NAME = EL852D                                      EL8521
00539         MOVE ER-0008 TO EMI-ERROR                                 EL8521
00540         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL8521
00541         MOVE -1                   TO DPFENTRL                     EL8521
00542         GO TO 8200-SEND-DATAONLY.                                 EL8521
00543                                                                   EL8521
00544      IF EIBAID = DFHPF1 OR DFHPF3                                    CL**3
00545         IF PI-END-OF-FILE                                         EL8521
00546            MOVE ER-2251 TO EMI-ERROR                              EL8521
00547            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL8521
00548            GO TO 8200-SEND-DATAONLY                               EL8521
00549         ELSE                                                      EL8521
00550            GO TO 3000-DISPLAY-REQUESTS.                           EL8521
00551                                                                   EL8521
00552      IF EIBAID = DFHPF2 OR DFHPF4                                    CL**3
00553         IF PI-TOP-OF-FILE                                         EL8521
00554            MOVE ER-2252 TO EMI-ERROR                              EL8521
00555            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL8521
00556            GO TO 8200-SEND-DATAONLY                               EL8521
00557         ELSE                                                      EL8521
00558            GO TO 3100-DISPLAY-PREV-REQUESTS.                      EL8521
00559                                                                      CL**8
00560      IF EIBAID = DFHPF5                                              CL**8
00561          PERFORM 5000-EDIT-BATCH  THRU  5999-EXIT                    CL**8
00562          GO TO 8200-SEND-DATAONLY.                                   CL**8
00563                                                                   EL8521
00564      MOVE ER-0008 TO EMI-ERROR.                                   EL8521
00565      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
00566                                                                   EL8521
00567      IF PI-MAP-NAME = EL852B                                      EL8521
00568         MOVE -1                  TO BPFENTRL.                     EL8521
00569                                                                   EL8521
00570      IF PI-MAP-NAME = EL852C                                      EL8521
00571         MOVE -1                  TO CPFENTRL.                     EL8521
00572                                                                   EL8521
00573      IF PI-MAP-NAME = EL852D                                      EL8521
00574         MOVE -1                  TO DPFENTRL.                     EL8521
00575                                                                   EL8521
00576      GO TO 8200-SEND-DATAONLY.                                    EL8521
00577                                                                   EL8521
00578      EJECT                                                        EL8521
00579                                                                   EL8521
00580 ******************************************************************EL8521
00581 *                                                                *EL8521
00582 *                  E D I T    M A P S                            *EL8521
00583 *                                                                *EL8521
00584 ******************************************************************EL8521
00585                                                                   EL8521
00586  1000-EDIT-MAPS.                                                  EL8521
00587                                                                   EL8521
00588      IF PI-MAP-NAME = EL852D                                      EL8521
00589         GO TO 1200-EDIT-BATCH-DISPLAY.                            EL8521
00590                                                                   EL8521
00591  1100-EDIT-DISPLAY-REQUESTS.                                      EL8521
00592                                                                   EL8521
00593      MOVE +0                     TO WS-SUB1.                      EL8521
00594                                                                   EL8521
00595  1110-EDIT-REQUEST.                                               EL8521
00596                                                                   EL8521
00597      ADD  +1                     TO WS-SUB1.                      EL8521
00598                                                                   EL8521
00599      IF WS-SUB1 GREATER THAN +10                                  EL8521
00600         IF AR-SELECT-LEN GREATER THAN ZEROS                       EL8521
00601            GO TO 1170-EDIT-SELECTION                              EL8521
00602         ELSE                                                      EL8521
00603            GO TO 3000-DISPLAY-REQUESTS.                           EL8521
00604                                                                   EL8521
00605      IF AR-REQST-DT-LEN (WS-SUB1) GREATER THAN ZEROS              EL8521
00606         IF AR-REQST-DT  (WS-SUB1) = SPACES                        EL8521
00607            MOVE LOW-VALUES          TO WS-SAVE-RQST-DT               CL**4
00608            MOVE PI-REQUEST-KEY (WS-SUB1) TO PI-ACCESS-KEY         EL8521
00609            MOVE 'Y'                 TO UPDATE-RQST-SW                CL**4
00610         ELSE                                                      EL8521
00611            MOVE ER-3131             TO EMI-ERROR                  EL8521
00612            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL8521
00613            GO TO 8200-SEND-DATAONLY.                              EL8521
00614                                                                      CL**4
00615      IF PI-PROCESSOR-ID = 'LGXX'                                     CL**4
00616          IF AR-STMT-DT-LEN (WS-SUB1) GREATER THAN ZEROS              CL**4
00617              MOVE PI-REQUEST-KEY (WS-SUB1) TO PI-ACCESS-KEY          CL**4
00618              MOVE 'Y'                 TO UPDATE-STMT-SW              CL**4
00619                  IF AR-STMT-DT  (WS-SUB1) = SPACES                   CL**4
00620                      MOVE LOW-VALUES    TO WS-SAVE-STMT-DT           CL**4
00621                  ELSE                                                CL**4
00622                      MOVE '3'           TO DC-OPTION-CODE            CL**4
00623                      MOVE AR-STMT-DT (WS-SUB1)                       CL**4
00624                                         TO DC-GREG-DATE-1-YMD        CL**4
00625                      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT        CL**4
00626                          IF NO-CONVERSION-ERROR                      CL**4
00627                              MOVE DC-BIN-DATE-1                      CL**4
00628                                          TO WS-SAVE-STMT-DT          CL**4
00629                          ELSE                                        CL**4
00630                              MOVE ER-3131             TO EMI-ERROR   CL**4
00631                              PERFORM 9900-ERROR-FORMAT               CL**4
00632                                                    THRU 9900-EXIT    CL**4
00633                              GO TO 8200-SEND-DATAONLY.               CL**4
00634                                                                      CL**4
00635      IF AR-STATUS-LEN (WS-SUB1) GREATER THAN ZEROS                   CL**5
00636          IF AR-STATUS   (WS-SUB1) NOT = SPACES                       CL**8
00637              MOVE ER-3191        TO EMI-ERROR                        CL**8
00638              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**8
00639              GO TO 8200-SEND-DATAONLY                                CL**8
00640          ELSE                                                        CL**8
00641              MOVE PI-REQUEST-KEY (WS-SUB1) TO PI-ACCESS-KEY          CL**8
00642              MOVE 'Y'            TO UPDATE-STATUS-SW                 CL**8
00643              MOVE SPACES         TO WS-SAVE-STATUS                   CL**8
00644              MOVE WS-SUB1        TO AR-SELECT                        CL**8
00645              PERFORM 5000-EDIT-BATCH  THRU  5999-EXIT                CL**8
00646              MOVE ' '            TO UPDATE-STATUS-SW                 CL**8
00647              MOVE SPACES         TO AR-SELECT-SPACES                 CL**8
00648              GO TO 1110-EDIT-REQUEST.                                CL**8
00649                                                                      CL**4
00650      IF UPDATE-RQST-DT OR UPDATE-STMT-DT                             CL**8
00651          PERFORM 2000-UPDATE-REQUEST THRU 2090-EXIT                  CL**4
00652          MOVE ' '                TO UPDATE-RQST-SW                   CL**8
00653          MOVE ' '                TO UPDATE-STMT-SW.                  CL**8
00654                                                                   EL8521
00655      GO TO 1110-EDIT-REQUEST.                                     EL8521
00656                                                                   EL8521
00657  1170-EDIT-SELECTION.                                             EL8521
00658                                                                   EL8521
00659      IF AR-SELECT NOT NUMERIC                                     EL8521
00660         MOVE ER-3132             TO EMI-ERROR                     EL8521
00661         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL8521
00662         GO TO 8200-SEND-DATAONLY.                                 EL8521
00663                                                                   EL8521
00664      IF AR-SELECT GREATER THAN +0 AND LESS THAN +11               EL8521
00665         NEXT SENTENCE                                             EL8521
00666      ELSE                                                         EL8521
00667         MOVE ER-3132             TO EMI-ERROR                     EL8521
00668         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                  EL8521
00669         GO TO 8200-SEND-DATAONLY.                                 EL8521
00670                                                                   EL8521
00671      MOVE PI-REQUEST-KEY (AR-SELECT) TO PI-ACCESS-KEY.            EL8521
00672      MOVE PI-MAP-NAME                TO PI-PREV-MAP-NAME.         EL8521
00673      MOVE EL852D                     TO PI-MAP-NAME.              EL8521
00674      GO TO 3200-DISPLAY-BATCH-REQUEST.                            EL8521
00675                                                                   EL8521
00676      EJECT                                                        EL8521
00677                                                                   EL8521
00678  1200-EDIT-BATCH-DISPLAY.                                         EL8521
00679                                                                   EL8521
00680      IF DREQSTL GREATER THAN ZEROS                                EL8521
00681         IF DREQSTI = SPACES                                       EL8521
00682            PERFORM 2000-UPDATE-REQUEST THRU 2090-EXIT             EL8521
00683         ELSE                                                      EL8521
00684            MOVE ER-3131             TO EMI-ERROR                  EL8521
00685            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               EL8521
00686            MOVE -1                  TO DPFENTRL                   EL8521
00687            GO TO 8200-SEND-DATAONLY.                              EL8521
00688                                                                   EL8521
00689      GO TO 3200-DISPLAY-BATCH-REQUEST.                            EL8521
00690                                                                   EL8521
00691      EJECT                                                        EL8521
00692                                                                   EL8521
00693 ******************************************************************EL8521
00694 *                                                                *EL8521
00695 *              U P D A T E   R E Q U E S T                       *EL8521
00696 *                                                                *EL8521
00697 *    1.  VOID THE REQUEST BY RE-SETTING THE REQUEST DATE         *EL8521
00698 *        TO LOW-VALUES.                                          *EL8521
00699 *                                                                *EL8521
00700 *    2.  SPACE OUT THE REQUEST METHOD AND REQUESTOR.             *EL8521
00701 *                                                                *EL8521
00702 ******************************************************************EL8521
00703                                                                   EL8521
00704  2000-UPDATE-REQUEST.                                             EL8521
00705                                                                   EL8521
00706       IF NOT MODIFY-CAP                                           EL8521
00707          MOVE 'UPDATE'           TO SM-READ                       EL8521
00708          PERFORM 9995-SECURITY-VIOLATION                          EL8521
00709          MOVE ER-0070            TO EMI-ERROR                     EL8521
00710          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL8521
00711          GO TO 8100-SEND-INITIAL-MAP.                             EL8521
00712                                                                   EL8521
00713      EXEC CICS HANDLE CONDITION                                   EL8521
00714          NOTFND  (2080-REQUEST-NOTFND)                            EL8521
00715      END-EXEC.                                                    EL8521
00716                                                                   EL8521
00717      EXEC CICS READ                                               EL8521
00718          SET     (ADDRESS OF AR-REQUEST-RECORD)                      CL**7
00719          DATASET (PI-FILE-ID)                                     EL8521
00720          RIDFLD  (PI-ACCESS-KEY)                                  EL8521
00721      END-EXEC.                                                    EL8521
00722                                                                   EL8521
00723      IF PI-PROCESSOR-ID NOT = 'LGXX'                                 CL**4
00724         IF RQ-STMT-DT NOT = LOW-VALUES                               CL**4
00725             GO TO 2070-REQUEST-DT-ERROR.                             CL**4
00726                                                                   EL8521
00727      MOVE RQ-CONTROL-PRIMARY    TO ERRQST-KEY.                    EL8521
00728                                                                   EL8521
00729      EXEC CICS READ                                               EL8521
00730          SET     (ADDRESS OF AR-REQUEST-RECORD)                      CL**7
00731          DATASET (FILE-ID-ERRQST)                                 EL8521
00732          RIDFLD  (ERRQST-KEY)                                     EL8521
00733          UPDATE                                                   EL8521
00734      END-EXEC.                                                    EL8521
00735                                                                   EL8521
00736      MOVE 'B'                    TO JP-RECORD-TYPE.               EL8521
00737      MOVE AR-REQUEST-RECORD      TO JP-RECORD-AREA.               EL8521
00738                                                                   EL8521
00739      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL8521
00740                                                                   EL8521
00741      IF UPDATE-RQST-DT                                               CL**4
00742          MOVE WS-SAVE-RQST-DT    TO RQ-REQUEST-DT.                   CL**4
00743                                                                      CL**4
00744      IF UPDATE-STMT-DT                                               CL**4
00745          MOVE WS-SAVE-STMT-DT    TO RQ-STMT-DT.                      CL**4
00746                                                                      CL**4
00747      IF UPDATE-STATUS                                                CL**4
00748          MOVE WS-SAVE-STATUS     TO RQ-STATUS.                       CL**4
00749                                                                      CL**4
00750      MOVE SPACES                 TO RQ-PROCESSOR-ID.              EL8521
00751      MOVE SPACE                  TO RQ-REQUEST-METHOD.            EL8521
00752                                                                   EL8521
00753      MOVE 'C'                    TO JP-RECORD-TYPE.               EL8521
00754      MOVE AR-REQUEST-RECORD      TO JP-RECORD-AREA.               EL8521
00755                                                                   EL8521
00756      EXEC CICS REWRITE                                            EL8521
00757          DATASET (FILE-ID-ERRQST)                                 EL8521
00758          FROM    (AR-REQUEST-RECORD)                              EL8521
00759      END-EXEC.                                                    EL8521
00760                                                                   EL8521
00761      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL8521
00762                                                                   EL8521
00763      GO TO 2090-EXIT.                                             EL8521
00764                                                                   EL8521
00765  2070-REQUEST-DT-ERROR.                                           EL8521
00766                                                                   EL8521
00767      MOVE ER-3136                TO EMI-ERROR.                    EL8521
00768      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
00769                                                                   EL8521
00770      IF PI-MAP-NAME = EL852D                                      EL8521
00771         MOVE -1                  TO DPFENTRL                      EL8521
00772      ELSE                                                         EL8521
00773         MOVE -1                  TO BPFENTRL.                     EL8521
00774                                                                   EL8521
00775      GO TO 8200-SEND-DATAONLY.                                    EL8521
00776                                                                   EL8521
00777  2080-REQUEST-NOTFND.                                             EL8521
00778                                                                   EL8521
00779      MOVE ER-2132                TO EMI-ERROR.                    EL8521
00780      IF PI-MAP-NAME = EL852D                                      EL8521
00781         MOVE -1                  TO DPFENTRL                      EL8521
00782      ELSE                                                         EL8521
00783         MOVE -1                  TO BPFENTRL.                     EL8521
00784      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
00785      GO TO 8200-SEND-DATAONLY.                                    EL8521
00786                                                                   EL8521
00787  2090-EXIT.                                                       EL8521
00788      EXIT.                                                        EL8521
00789      EJECT                                                        EL8521
00790                                                                   EL8521
00791 ******************************************************************EL8521
00792 *                                                                *EL8521
00793 *           D I S P L A Y   R E Q U E S T S                      *EL8521
00794 *                                                                *EL8521
00795 ******************************************************************EL8521
00796                                                                   EL8521
00797  3000-DISPLAY-REQUESTS.                                           EL8521
00798                                                                   EL8521
00799      EXEC CICS HANDLE CONDITION                                   EL8521
00800          NOTFND  (3080-REQUEST-NOTFND)                            EL8521
00801          ENDFILE (3080-REQUEST-NOTFND)                            EL8521
00802      END-EXEC.                                                    EL8521
00803                                                                   EL8521
00804      IF EIBTRNID = TRANS-EXJ6                                     EL8521
00805         IF EIBAID = DFHPF1                                        EL8521
00806            MOVE PI-REQUEST-KEY (10) TO PI-ACCESS-KEY              EL8521
00807         ELSE                                                      EL8521
00808            MOVE PI-REQUEST-KEY (1)  TO PI-ACCESS-KEY.             EL8521
00809                                                                   EL8521
00810      EXEC CICS STARTBR                                            EL8521
00811          DATASET (PI-FILE-ID)                                     EL8521
00812          RIDFLD  (PI-ACCESS-KEY)                                  EL8521
00813      END-EXEC.                                                    EL8521
00814                                                                   EL8521
00815      MOVE SPACE                  TO PI-END-OF-FILE-SW             EL8521
00816                                     PI-TOP-OF-FILE-SW.            EL8521
00817                                                                   EL8521
00818      IF PI-OPTION-ONE-SELECTED                                    EL8521
00819         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY2.              EL8521
00820                                                                   EL8521
00821      IF PI-OPTION-TWO-SELECTED                                    EL8521
00822         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY1.              EL8521
00823                                                                   EL8521
00824      IF PI-OPTION-THREE-SELECTED                                  EL8521
00825         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY4.              EL8521
00826                                                                   EL8521
00827      IF PI-OPTION-FOUR-SELECTED                                   EL8521
00828         MOVE PI-SELECT-KEY       TO ERRQST-KEY.                   EL8521
00829                                                                   EL8521
00830      MOVE 'N'                    TO FIRST-ERROR-SCAN-SW.             CL**3
00831      MOVE +0                     TO WS-SUB1.                      EL8521
00832                                                                   EL8521
00833  3010-READ-REQUEST-FILE.                                          EL8521
00834                                                                   EL8521
00835      EXEC CICS HANDLE CONDITION                                   EL8521
00836          ENDFILE (3060-END-OF-FILE)                               EL8521
00837      END-EXEC.                                                    EL8521
00838                                                                   EL8521
00839      EXEC CICS READNEXT                                           EL8521
00840          SET     (ADDRESS OF AR-REQUEST-RECORD)                      CL**7
00841          DATASET (PI-FILE-ID)                                     EL8521
00842          RIDFLD  (PI-ACCESS-KEY)                                  EL8521
00843      END-EXEC.                                                    EL8521
00844                                                                   EL8521
00845      IF EIBAID = DFHPF1                                           EL8521
00846         IF PI-REQUEST-KEY (10) = PI-ACCESS-KEY                    EL8521
00847            GO TO 3010-READ-REQUEST-FILE.                             CL**3
00848                                                                      CL**3
00849      IF EIBAID = DFHPF3                                              CL**3
00850         IF PI-REQUEST-KEY (1) = PI-ACCESS-KEY                        CL**3
00851            GO TO 3010-READ-REQUEST-FILE.                          EL8521
00852                                                                   EL8521
00853      IF RQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL8521
00854         GO TO 3060-END-OF-FILE.                                   EL8521
00855                                                                   EL8521
00856      IF PI-OPTION-ONE-SELECTED                                    EL8521
00857         MOVE RQ-CONTROL-BY-FIN-RESP TO WS-RQST-FIN-RESP-CNTL      EL8521
00858         IF WS-RQST-FIN-RESP-CNTL GREATER THAN                     EL8521
00859               ERRQST-FIN-RESP-CONTROL                             EL8521
00860            GO TO 3060-END-OF-FILE.                                EL8521
00861                                                                   EL8521
00862      IF PI-OPTION-TWO-SELECTED                                    EL8521
00863         MOVE RQ-CONTROL-BY-ACCT-REF TO WS-RQST-ACCT-CONTROL       EL8521
00864         IF WS-RQST-ACCT-CONTROL GREATER THAN ERRQST-ACCT-CONTROL  EL8521
00865            GO TO 3060-END-OF-FILE.                                EL8521
00866                                                                   EL8521
00867      IF PI-OPTION-TWO-SELECTED                                    EL8521
00868         IF ERRQST-REFERENCE-A1 GREATER THAN LOW-VALUES            EL8521
00869            IF  ERRQST-REFERENCE-A1 = RQ-REFERENCE-A1              EL8521
00870                NEXT SENTENCE                                      EL8521
00871            ELSE                                                   EL8521
00872                GO TO 3010-READ-REQUEST-FILE                       EL8521
00873         ELSE                                                      EL8521
00874            NEXT SENTENCE.                                         EL8521
00875                                                                   EL8521
00876      IF PI-OPTION-THREE-SELECTED                                  EL8521
00877         IF ERRQST-SUMMARY-CODE = RQ-SUMMARY-CODE                  EL8521
00878            NEXT SENTENCE                                          EL8521
00879         ELSE                                                      EL8521
00880            GO TO 3010-READ-REQUEST-FILE.                          EL8521
00881                                                                   EL8521
00882      IF PI-OPTION-FOUR-SELECTED AND EIBAID = DFHPF3                  CL**3
00883          IF RQ-STATUS NOT EQUAL 'E' AND FIRST-ERROR-NOT-FOUND        CL**3
00884              GO TO 3010-READ-REQUEST-FILE                            CL**3
00885          ELSE                                                        CL**3
00886              MOVE 'Y' TO FIRST-ERROR-SCAN-SW.                        CL**3
00887                                                                      CL**3
00888      ADD +1                      TO WS-SUB1.                      EL8521
00889                                                                   EL8521
00890      IF WS-SUB1 = +1                                              EL8521
00891         IF PI-OPTION-ONE-SELECTED                                 EL8521
00892            PERFORM 4100-READ-COMP-MASTER THRU 4190-EXIT           EL8521
00893            MOVE WS-FIN-RESP-NAME TO BNAMEO                        EL8521
00894            MOVE RQ-CARRIER-A2    TO BCARRO                        EL8521
00895            MOVE RQ-GROUPING-A2   TO BGROUPO                       EL8521
00896            MOVE RQ-FIN-RESP-A2   TO BAGENTO.                      EL8521
00897                                                                   EL8521
00898      IF WS-SUB1 = +1                                              EL8521
00899         IF PI-OPTION-TWO-SELECTED                                 EL8521
00900            PERFORM 4000-READ-ACCOUNT-MASTER THRU 4090-EXIT        EL8521
00901            MOVE WS-AM-NAME          TO CNAMEO                     EL8521
00902            MOVE RQ-CARRIER-A1       TO CCARRO                     EL8521
00903            MOVE RQ-GROUPING-A1      TO CGROUPO                    EL8521
00904            MOVE RQ-STATE-A1         TO CSTO                       EL8521
00905            MOVE RQ-ACCOUNT-A1       TO CACCTO.                    EL8521
00906                                                                   EL8521
00907      IF WS-SUB1 = +1                                              EL8521
00908         IF PI-OPTION-THREE-SELECTED                               EL8521
00909            PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT              EL8521
00910            MOVE WS-SUM-NAME      TO BNAMEO                        EL8521
00911            MOVE 'SUM:'           TO BCARHDGO                      EL8521
00912            MOVE AL-SADOF         TO BAGTHDGA                      EL8521
00913            MOVE RQ-SUMMARY-CODE  TO BGRPHDGO.                     EL8521
00914                                                                   EL8521
00915      IF WS-SUB1 = +1                                              EL8521
00916         IF PI-OPTION-FOUR-SELECTED                                EL8521
00917            MOVE AL-SADOF         TO BCARHDGA                      EL8521
00918                                     BAGTHDGA                      EL8521
00919                                     BGRPHDGA                      EL8521
00920                                     BNAMHDGA                         CL**3
00921      IF WS-SUB1 = +1                                                 CL**3
00922         IF PI-OPTION-FOUR-SELECTED                                   CL**3
00923             IF EIBAID = DFHPF3                                       CL**3
00924                 MOVE 'SCAN ERRORED BATCHES'   TO BNAMEO              CL**3
00925             ELSE                                                     CL**3
00926                 MOVE 'BATCH DISPLAY'          TO BNAMEO.             CL**3
00927                                                                   EL8521
00928      IF WS-SUB1 GREATER THAN +10                                  EL8521
00929         GO TO 3070-DISPLAY-PROCESSED.                             EL8521
00930                                                                   EL8521
00931      IF WS-SUB1 = +1                                              EL8521
00932         MOVE PI-ACCESS-KEY       TO PI-1ST-REQUEST-KEY.           EL8521
00933                                                                   EL8521
00934      MOVE PI-ACCESS-KEY          TO PI-REQUEST-KEY (WS-SUB1).     EL8521
00935                                                                   EL8521
00936      MOVE WS-SUB1                TO AR-SEQ          (WS-SUB1).    EL8521
00937                                                                   EL8521
00938      IF NOT PI-OPTION-TWO-SELECTED                                EL8521
00939         MOVE RQ-CARRIER-A1       TO AR-CAR          (WS-SUB1)     EL8521
00940         MOVE RQ-GROUPING-A1      TO AR-GRP          (WS-SUB1)     EL8521
00941         MOVE RQ-STATE-A1         TO AR-ST           (WS-SUB1)     EL8521
00942         MOVE RQ-ACCOUNT-A1       TO AR-ACCT         (WS-SUB1)     EL8521
00943      ELSE                                                         EL8521
00944         MOVE RQ-CARRIER-A2       TO AR-CAR          (WS-SUB1)     EL8521
00945         MOVE RQ-GROUPING-A2      TO AR-GRP          (WS-SUB1)     EL8521
00946         MOVE RQ-FIN-RESP-A2      TO AR-ACCT         (WS-SUB1).    EL8521
00947                                                                   EL8521
00948      MOVE RQ-REFERENCE-A1        TO AR-REF          (WS-SUB1).    EL8521
00949      MOVE RQ-BATCH-A1            TO AR-BATCH        (WS-SUB1).    EL8521
00950      MOVE RQ-SUMMARY-CODE        TO AR-SUM          (WS-SUB1).    EL8521
00951      MOVE RQ-STATUS              TO AR-STATUS       (WS-SUB1).    EL8521
00952                                                                   EL8521
00953      MOVE SPACE                  TO DC-OPTION-CODE.               EL8521
00954      MOVE RQ-ENTRY-DT            TO DC-BIN-DATE-1.                EL8521
00955      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL8521
00956                                                                   EL8521
00957      IF DATE-CONVERSION-ERROR                                     EL8521
00958         MOVE 'DT ERR'            TO AR-ENTRY-DT (WS-SUB1)         EL8521
00959      ELSE                                                         EL8521
00960         MOVE DC-GREG-DATE-1-MDY  TO AR-ENTRY-DT       (WS-SUB1).  EL8521
00961                                                                   EL8521
00962      IF RQ-REQUEST-DT GREATER THAN LOW-VALUES                     EL8521
00963         MOVE SPACE                  TO DC-OPTION-CODE             EL8521
00964         MOVE RQ-REQUEST-DT          TO DC-BIN-DATE-1              EL8521
00965         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  EL8521
00966         IF DATE-CONVERSION-ERROR                                  EL8521
00967            MOVE 'DT ERR'            TO AR-REQST-DT    (WS-SUB1)   EL8521
00968         ELSE                                                      EL8521
00969            MOVE DC-GREG-DATE-1-MDY  TO AR-REQST-DT    (WS-SUB1)      CL**2
00970      ELSE                                                            CL**2
00971         MOVE LOW-VALUES             TO AR-REQST-DT    (WS-SUB1).     CL**2
00972                                                                   EL8521
00973      MOVE AL-UANOF                 TO AR-REQST-DT-ATTRB (WS-SUB1)    CL**6
00974                                       AR-STATUS-ATTRB (WS-SUB1).     CL**6
00975                                                                   EL8521
00976      IF PI-PROCESSOR-ID = 'LGXX'                                     CL**4
00977          MOVE AL-UANOF             TO AR-STMT-DT-ATTRB (WS-SUB1).    CL**6
00978 *                                     AR-STATUS-ATTRB (WS-SUB1).     CL**6
00979                                                                      CL**4
00980      IF RQ-STMT-DT GREATER THAN LOW-VALUES                        EL8521
00981         MOVE SPACE                  TO DC-OPTION-CODE             EL8521
00982         MOVE RQ-STMT-DT             TO DC-BIN-DATE-1              EL8521
00983         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  EL8521
00984         IF DATE-CONVERSION-ERROR                                  EL8521
00985            MOVE 'DT ERR'            TO AR-STMT-DT  (WS-SUB1)      EL8521
00986         ELSE                                                      EL8521
00987            MOVE DC-GREG-DATE-1-MDY  TO AR-STMT-DT  (WS-SUB1)         CL**2
00988      ELSE                                                            CL**2
00989         MOVE LOW-VALUES             TO AR-STMT-DT  (WS-SUB1).        CL**2
00990                                                                   EL8521
00991      GO TO 3010-READ-REQUEST-FILE.                                EL8521
00992                                                                   EL8521
00993  3060-END-OF-FILE.                                                EL8521
00994                                                                   EL8521
00995      MOVE 'Y'                    TO PI-END-OF-FILE-SW.            EL8521
00996      MOVE ER-2251                TO EMI-ERROR.                    EL8521
00997      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
00998                                                                   EL8521
00999  3070-DISPLAY-PROCESSED.                                          EL8521
01000                                                                   EL8521
01001      EXEC CICS ENDBR                                              EL8521
01002          DATASET (PI-FILE-ID)                                     EL8521
01003      END-EXEC.                                                    EL8521
01004                                                                   EL8521
01005      MOVE -1                     TO AR-SELECT-LEN.                EL8521
01006      GO TO 8100-SEND-INITIAL-MAP.                                 EL8521
01007                                                                   EL8521
01008  3080-REQUEST-NOTFND.                                             EL8521
01009                                                                   EL8521
01010      MOVE ER-2132                TO EMI-ERROR.                    EL8521
01011      MOVE -1                     TO AR-SELECT-LEN.                EL8521
01012      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
01013      GO TO 8200-SEND-DATAONLY.                                    EL8521
01014                                                                   EL8521
01015  3090-EXIT.                                                       EL8521
01016       EXIT.                                                       EL8521
01017                                                                   EL8521
01018      EJECT                                                        EL8521
01019                                                                   EL8521
01020 ******************************************************************EL8521
01021 *                                                                *EL8521
01022 *        D I S P L A Y   P R E V.   R E Q U E S T S              *EL8521
01023 *                                                                *EL8521
01024 ******************************************************************EL8521
01025                                                                   EL8521
01026  3100-DISPLAY-PREV-REQUESTS.                                      EL8521
01027                                                                   EL8521
01028      EXEC CICS HANDLE CONDITION                                   EL8521
01029          NOTFND  (3180-REQUEST-NOTFND)                            EL8521
01030          ENDFILE (3180-REQUEST-NOTFND)                            EL8521
01031      END-EXEC.                                                    EL8521
01032                                                                   EL8521
01033      MOVE PI-REQUEST-KEY (1)    TO PI-ACCESS-KEY.                 EL8521
01034                                                                   EL8521
01035      EXEC CICS STARTBR                                            EL8521
01036          DATASET (PI-FILE-ID)                                     EL8521
01037          RIDFLD  (PI-ACCESS-KEY)                                  EL8521
01038      END-EXEC.                                                    EL8521
01039                                                                   EL8521
01040      MOVE 'N'                    TO FIRST-ERROR-SCAN-SW.             CL**3
01041      MOVE SPACE                  TO PI-END-OF-FILE-SW             EL8521
01042                                     PI-TOP-OF-FILE-SW.            EL8521
01043                                                                   EL8521
01044      IF PI-OPTION-ONE-SELECTED                                    EL8521
01045         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY2.              EL8521
01046                                                                   EL8521
01047      IF PI-OPTION-TWO-SELECTED                                    EL8521
01048         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY1.              EL8521
01049                                                                   EL8521
01050      IF PI-OPTION-THREE-SELECTED                                  EL8521
01051         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY4.              EL8521
01052                                                                   EL8521
01053      IF PI-OPTION-FOUR-SELECTED                                   EL8521
01054         MOVE PI-SELECT-KEY       TO ERRQST-KEY.                   EL8521
01055                                                                   EL8521
01056                                                                   EL8521
01057      MOVE +11                    TO WS-SUB1.                      EL8521
01058                                                                   EL8521
01059  3110-READ-REQUEST-FILE.                                          EL8521
01060                                                                   EL8521
01061      EXEC CICS HANDLE CONDITION                                   EL8521
01062          ENDFILE (3160-END-OF-FILE)                               EL8521
01063      END-EXEC.                                                    EL8521
01064                                                                   EL8521
01065      EXEC CICS READPREV                                           EL8521
01066          SET     (ADDRESS OF AR-REQUEST-RECORD)                      CL**7
01067          DATASET (PI-FILE-ID)                                     EL8521
01068          RIDFLD  (PI-ACCESS-KEY)                                  EL8521
01069      END-EXEC.                                                    EL8521
01070                                                                   EL8521
01071      IF RQ-COMPANY-CD NOT = PI-COMPANY-CD                         EL8521
01072         GO TO 3160-END-OF-FILE.                                   EL8521
01073                                                                   EL8521
01074      IF PI-REQUEST-KEY (1) = PI-ACCESS-KEY                        EL8521
01075         GO TO 3110-READ-REQUEST-FILE.                             EL8521
01076                                                                   EL8521
01077      IF PI-OPTION-ONE-SELECTED                                    EL8521
01078         MOVE RQ-CONTROL-BY-FIN-RESP TO WS-RQST-FIN-RESP-CNTL      EL8521
01079         IF WS-RQST-FIN-RESP-CNTL LESS THAN                        EL8521
01080               ERRQST-FIN-RESP-CONTROL                             EL8521
01081            GO TO 3160-END-OF-FILE.                                EL8521
01082                                                                   EL8521
01083      IF PI-OPTION-TWO-SELECTED                                    EL8521
01084         MOVE RQ-CONTROL-BY-ACCT-REF TO WS-RQST-ACCT-CONTROL       EL8521
01085         IF WS-RQST-ACCT-CONTROL LESS THAN ERRQST-ACCT-CONTROL     EL8521
01086            GO TO 3160-END-OF-FILE.                                EL8521
01087                                                                   EL8521
01088      IF PI-OPTION-TWO-SELECTED                                    EL8521
01089         IF ERRQST-REFERENCE-A1 GREATER THAN LOW-VALUES            EL8521
01090            IF ERRQST-REFERENCE-A1 = RQ-REFERENCE-A1               EL8521
01091               NEXT SENTENCE                                       EL8521
01092            ELSE                                                   EL8521
01093               GO TO 3010-READ-REQUEST-FILE.                       EL8521
01094                                                                   EL8521
01095      IF PI-OPTION-THREE-SELECTED                                  EL8521
01096         IF ERRQST-SUMMARY-CODE = RQ-SUMMARY-CODE                  EL8521
01097            NEXT SENTENCE                                          EL8521
01098         ELSE                                                      EL8521
01099            GO TO 3110-READ-REQUEST-FILE.                          EL8521
01100                                                                   EL8521
01101      IF PI-OPTION-FOUR-SELECTED AND EIBAID = DFHPF4                  CL**3
01102          IF RQ-STATUS NOT EQUAL 'E' AND FIRST-ERROR-NOT-FOUND        CL**3
01103              GO TO 3110-READ-REQUEST-FILE                            CL**3
01104          ELSE                                                        CL**3
01105              MOVE 'Y' TO FIRST-ERROR-SCAN-SW.                        CL**3
01106                                                                      CL**3
01107      SUBTRACT +1                 FROM WS-SUB1.                    EL8521
01108                                                                   EL8521
01109      IF WS-SUB1 LESS THAN +1                                      EL8521
01110         GO TO 3170-DISPLAY-PROCESSED.                             EL8521
01111                                                                   EL8521
01112      IF WS-SUB1 = +10                                             EL8521
01113         IF PI-OPTION-ONE-SELECTED                                 EL8521
01114            PERFORM 4100-READ-COMP-MASTER THRU 4190-EXIT           EL8521
01115            MOVE WS-FIN-RESP-NAME TO BNAMEO                        EL8521
01116            MOVE RQ-CARRIER-A2    TO BCARRO                        EL8521
01117            MOVE RQ-GROUPING-A2   TO BGROUPO                       EL8521
01118            MOVE RQ-FIN-RESP-A2   TO BAGENTO.                      EL8521
01119                                                                   EL8521
01120      IF WS-SUB1 = +10                                             EL8521
01121         IF PI-OPTION-TWO-SELECTED                                 EL8521
01122            PERFORM 4000-READ-ACCOUNT-MASTER THRU 4090-EXIT        EL8521
01123            MOVE WS-AM-NAME          TO CNAMEO                     EL8521
01124            MOVE RQ-CARRIER-A1       TO CCARRO                     EL8521
01125            MOVE RQ-GROUPING-A1      TO CGROUPO                    EL8521
01126            MOVE RQ-STATE-A1         TO CSTO                       EL8521
01127            MOVE RQ-ACCOUNT-A1       TO CACCTO.                    EL8521
01128                                                                   EL8521
01129      IF WS-SUB1 = +10                                             EL8521
01130         IF PI-OPTION-THREE-SELECTED                               EL8521
01131            PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT              EL8521
01132            MOVE WS-SUM-NAME      TO BNAMEO                        EL8521
01133            MOVE 'SUM:'           TO BCARHDGO                      EL8521
01134            MOVE AL-SADOF         TO BAGTHDGA                      EL8521
01135            MOVE RQ-SUMMARY-CODE  TO BGRPHDGO.                     EL8521
01136                                                                   EL8521
01137      IF WS-SUB1 = +10                                             EL8521
01138         IF PI-OPTION-FOUR-SELECTED                                EL8521
01139            MOVE AL-SADOF         TO BCARHDGA                      EL8521
01140                                     BAGTHDGA                      EL8521
01141                                     BGRPHDGA                      EL8521
01142                                     BNAMHDGA.                     EL8521
01143                                                                      CL**3
01144      IF WS-SUB1 = +10                                                CL**3
01145         IF PI-OPTION-FOUR-SELECTED                                   CL**3
01146            IF EIBAID = DFHPF4                                        CL**3
01147                MOVE 'SCAN ERRORED BATCHES'   TO BNAMEO               CL**3
01148            ELSE                                                      CL**3
01149                MOVE 'BATCH DISPLAY'          TO BNAMEO.              CL**3
01150                                                                   EL8521
01151      MOVE PI-ACCESS-KEY          TO PI-REQUEST-KEY (WS-SUB1).     EL8521
01152                                                                   EL8521
01153      MOVE WS-SUB1                TO AR-SEQ    (WS-SUB1).          EL8521
01154                                                                   EL8521
01155      IF NOT PI-OPTION-TWO-SELECTED                                EL8521
01156         MOVE RQ-CARRIER-A1       TO AR-CAR    (WS-SUB1)           EL8521
01157         MOVE RQ-GROUPING-A1      TO AR-GRP    (WS-SUB1)           EL8521
01158         MOVE RQ-STATE-A1         TO AR-ST     (WS-SUB1)           EL8521
01159         MOVE RQ-ACCOUNT-A1       TO AR-ACCT   (WS-SUB1)           EL8521
01160      ELSE                                                         EL8521
01161         MOVE RQ-CARRIER-A2       TO AR-CAR    (WS-SUB1)           EL8521
01162         MOVE RQ-GROUPING-A2      TO AR-GRP    (WS-SUB1)           EL8521
01163         MOVE RQ-FIN-RESP-A2      TO AR-ACCT   (WS-SUB1).          EL8521
01164                                                                   EL8521
01165      MOVE RQ-REFERENCE-A1        TO AR-REF    (WS-SUB1).          EL8521
01166      MOVE RQ-BATCH-A1            TO AR-BATCH  (WS-SUB1).          EL8521
01167      MOVE RQ-SUMMARY-CODE        TO AR-SUM    (WS-SUB1).          EL8521
01168      MOVE RQ-STATUS              TO AR-STATUS (WS-SUB1).          EL8521
01169                                                                   EL8521
01170      MOVE SPACE                  TO DC-OPTION-CODE.               EL8521
01171      MOVE RQ-ENTRY-DT            TO DC-BIN-DATE-1.                EL8521
01172      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.                    EL8521
01173                                                                   EL8521
01174      IF DATE-CONVERSION-ERROR                                     EL8521
01175         MOVE 'DT ERR'            TO AR-ENTRY-DT (WS-SUB1)         EL8521
01176      ELSE                                                         EL8521
01177         MOVE DC-GREG-DATE-1-MDY  TO AR-ENTRY-DT (WS-SUB1).        EL8521
01178                                                                   EL8521
01179      IF RQ-REQUEST-DT GREATER THAN LOW-VALUES                     EL8521
01180         MOVE SPACE                  TO DC-OPTION-CODE             EL8521
01181         MOVE RQ-REQUEST-DT          TO DC-BIN-DATE-1              EL8521
01182         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  EL8521
01183         IF DATE-CONVERSION-ERROR                                  EL8521
01184            MOVE 'DT ERR'            TO AR-REQST-DT (WS-SUB1)      EL8521
01185         ELSE                                                      EL8521
01186            MOVE DC-GREG-DATE-1-MDY  TO AR-REQST-DT    (WS-SUB1)      CL**2
01187      ELSE                                                            CL**2
01188         MOVE SPACES                 TO AR-REQST-DT    (WS-SUB1).     CL**4
01189                                                                      CL**2
01190                                                                   EL8521
01191      MOVE AL-UANOF                 TO AR-REQST-DT-ATTRB (WS-SUB1).EL8521
01192                                                                      CL**4
01193      IF PI-PROCESSOR-ID = 'LGXX'                                     CL**4
01194          MOVE AL-UANOF             TO AR-STMT-DT-ATTRB (WS-SUB1)     CL**4
01195                                       AR-STATUS-ATTRB (WS-SUB1).     CL**4
01196                                                                   EL8521
01197      IF RQ-STMT-DT GREATER THAN LOW-VALUES                        EL8521
01198         MOVE SPACE                  TO DC-OPTION-CODE             EL8521
01199         MOVE RQ-STMT-DT             TO DC-BIN-DATE-1              EL8521
01200         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  EL8521
01201         IF DATE-CONVERSION-ERROR                                  EL8521
01202            MOVE 'DT ERR'            TO AR-STMT-DT  (WS-SUB1)      EL8521
01203         ELSE                                                      EL8521
01204            MOVE DC-GREG-DATE-1-MDY  TO AR-STMT-DT  (WS-SUB1)         CL**2
01205      ELSE                                                            CL**2
01206         MOVE SPACES                 TO AR-STMT-DT  (WS-SUB1).        CL**4
01207                                                                      CL**2
01208                                                                   EL8521
01209      GO TO 3110-READ-REQUEST-FILE.                                EL8521
01210                                                                   EL8521
01211  3160-END-OF-FILE.                                                EL8521
01212                                                                   EL8521
01213      MOVE 'Y'                    TO PI-TOP-OF-FILE-SW.            EL8521
01214      MOVE ER-2252                TO EMI-ERROR.                    EL8521
01215      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
01216                                                                   EL8521
01217  3170-DISPLAY-PROCESSED.                                          EL8521
01218                                                                   EL8521
01219      EXEC CICS ENDBR                                              EL8521
01220          DATASET (PI-FILE-ID)                                     EL8521
01221      END-EXEC.                                                    EL8521
01222                                                                   EL8521
01223      MOVE -1                     TO AR-SELECT-LEN.                EL8521
01224                                                                   EL8521
01225      IF WS-SUB1 LESS +1                                              CL**2
01226         GO TO 8100-SEND-INITIAL-MAP                               EL8521
01227      ELSE                                                         EL8521
01228         MOVE PI-REQUEST-KEY (1)  TO PI-ACCESS-KEY                 EL8521
01229         GO TO 8200-SEND-DATAONLY.                                 EL8521
01230                                                                   EL8521
01231  3180-REQUEST-NOTFND.                                             EL8521
01232                                                                   EL8521
01233      MOVE ER-2132                TO EMI-ERROR.                    EL8521
01234      MOVE -1                     TO AR-SELECT-LEN.                EL8521
01235      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
01236      GO TO 8200-SEND-DATAONLY.                                    EL8521
01237                                                                   EL8521
01238  3190-EXIT.                                                       EL8521
01239       EXIT.                                                       EL8521
01240                                                                   EL8521
01241      EJECT                                                        EL8521
01242                                                                   EL8521
01243 ******************************************************************EL8521
01244 *                                                                *EL8521
01245 *        D I S P L A Y   B A T C H   R E Q U E S T               *EL8521
01246 *                                                                *EL8521
01247 ******************************************************************EL8521
01248                                                                   EL8521
01249  3200-DISPLAY-BATCH-REQUEST.                                      EL8521
01250                                                                   EL8521
01251      EXEC CICS HANDLE CONDITION                                   EL8521
01252          NOTFND  (3280-REQUEST-NOTFND)                            EL8521
01253          ENDFILE (3280-REQUEST-NOTFND)                            EL8521
01254      END-EXEC.                                                    EL8521
01255                                                                   EL8521
01256      EXEC CICS READ                                               EL8521
01257          SET     (ADDRESS OF AR-REQUEST-RECORD)                      CL**7
01258          DATASET (PI-FILE-ID)                                     EL8521
01259          RIDFLD  (PI-ACCESS-KEY)                                  EL8521
01260      END-EXEC.                                                    EL8521
01261                                                                   EL8521
01262      MOVE RQ-ENTRY-BATCH         TO DBATCHO.                      EL8521
01263      MOVE RQ-CARRIER-A1          TO DCARO.                        EL8521
01264      MOVE RQ-GROUPING-A1         TO DGROUPO.                      EL8521
01265      MOVE RQ-STATE-A1            TO DSTO.                         EL8521
01266      MOVE RQ-ACCOUNT-A1          TO DACCTO.                       EL8521
01267      MOVE RQ-FIN-RESP-A2         TO DFRESPO.                      EL8521
01268      MOVE RQ-ACCT-AGENT-A2       TO DAGENTO.                      EL8521
01269      MOVE RQ-REFERENCE-A2        TO DREFO.                        EL8521
01270      MOVE RQ-SUMMARY-CODE        TO DSUMO.                        EL8521
01271      MOVE RQ-STATUS              TO DSTATUSO.                     EL8521
01272      MOVE RQ-PROCESSOR-ID        TO DRQSTRO.                      EL8521
01273      MOVE RQ-REQUEST-METHOD      TO DTYPEO.                       EL8521
01274                                                                   EL8521
01275      IF RQ-ENTRY-DT GREATER THAN LOW-VALUES                       EL8521
01276         MOVE SPACE                  TO DC-OPTION-CODE             EL8521
01277         MOVE RQ-ENTRY-DT            TO DC-BIN-DATE-1              EL8521
01278         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  EL8521
01279         IF DATE-CONVERSION-ERROR                                  EL8521
01280            MOVE 'DT ERR'            TO DENTRYO                    EL8521
01281         ELSE                                                      EL8521
01282            MOVE DC-GREG-DATE-1-EDIT TO DENTRYO.                   EL8521
01283                                                                   EL8521
01284      IF RQ-REQUEST-DT GREATER THAN LOW-VALUES                     EL8521
01285         MOVE SPACE                  TO DC-OPTION-CODE             EL8521
01286         MOVE RQ-REQUEST-DT          TO DC-BIN-DATE-1              EL8521
01287         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  EL8521
01288         IF DATE-CONVERSION-ERROR                                  EL8521
01289            MOVE 'DT ERR'            TO DREQSTO                    EL8521
01290         ELSE                                                      EL8521
01291            MOVE DC-GREG-DATE-1-EDIT TO DREQSTO.                   EL8521
01292                                                                   EL8521
01293      IF RQ-STMT-DT GREATER THAN LOW-VALUES                        EL8521
01294         MOVE SPACE                  TO DC-OPTION-CODE             EL8521
01295         MOVE RQ-STMT-DT             TO DC-BIN-DATE-1              EL8521
01296         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  EL8521
01297         IF DATE-CONVERSION-ERROR                                  EL8521
01298            MOVE 'DT ERR'            TO DSTMTDTO                   EL8521
01299         ELSE                                                      EL8521
01300            MOVE DC-GREG-DATE-1-EDIT TO DSTMTDTO.                  EL8521
01301                                                                   EL8521
01302      IF RQ-MO-END-DT GREATER THAN LOW-VALUES                      EL8521
01303         MOVE SPACE                  TO DC-OPTION-CODE             EL8521
01304         MOVE RQ-MO-END-DT           TO DC-BIN-DATE-1              EL8521
01305         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  EL8521
01306         IF DATE-CONVERSION-ERROR                                  EL8521
01307            MOVE 'DT ERR'            TO DEOMDTO                    EL8521
01308         ELSE                                                      EL8521
01309            MOVE DC-GREG-DATE-1-EDIT TO DEOMDTO.                   EL8521
01310                                                                   EL8521
01311      IF RQ-REVERSAL-DT GREATER THAN LOW-VALUES                    EL8521
01312         MOVE SPACE                  TO DC-OPTION-CODE             EL8521
01313         MOVE RQ-REVERSAL-DT         TO DC-BIN-DATE-1              EL8521
01314         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                  EL8521
01315         IF DATE-CONVERSION-ERROR                                  EL8521
01316            MOVE 'DT ERR'            TO DVOIDDTO                   EL8521
01317         ELSE                                                      EL8521
01318            MOVE DC-GREG-DATE-1-EDIT TO DVOIDDTO.                  EL8521
01319                                                                   EL8521
01320       MOVE -1                       TO DPFENTRL.                  EL8521
01321       GO TO 8100-SEND-INITIAL-MAP.                                EL8521
01322                                                                   EL8521
01323  3280-REQUEST-NOTFND.                                             EL8521
01324                                                                   EL8521
01325       MOVE SPACE                 TO PI-OPTION.                    EL8521
01326       MOVE ER-2132               TO EMI-ERROR.                    EL8521
01327       MOVE -1                    TO DPFENTRL.                     EL8521
01328       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   EL8521
01329       GO TO 8100-SEND-INITIAL-MAP.                                EL8521
01330                                                                   EL8521
01331      EJECT                                                        EL8521
01332 ******************************************************************EL8521
01333 *                                                                *EL8521
01334 *             R E A D   A C C O U N T   M A S T E R              *EL8521
01335 *                                                                *EL8521
01336 ******************************************************************EL8521
01337                                                                   EL8521
01338  4000-READ-ACCOUNT-MASTER.                                        EL8521
01339                                                                   EL8521
01340      MOVE LOW-VALUES             TO ERACCT-KEY.                   EL8521
01341      MOVE RQ-COMPANY-CD          TO ERACCT-COMP-CD.               EL8521
01342      MOVE RQ-CARRIER-A1          TO ERACCT-CARRIER.               EL8521
01343      MOVE RQ-GROUPING-A1         TO ERACCT-GROUPING.              EL8521
01344      MOVE RQ-STATE-A1            TO ERACCT-STATE                  EL8521
01345      MOVE RQ-ACCOUNT-A1          TO ERACCT-ACCOUNT.               EL8521
01346      MOVE ERACCT-KEY             TO ERACCT-SAVE-KEY.              EL8521
01347                                                                   EL8521
01348      EXEC CICS HANDLE CONDITION                                   EL8521
01349          NOTFND   (4080-ACCOUNT-NOTFND)                           EL8521
01350      END-EXEC.                                                    EL8521
01351                                                                   EL8521
01352      EXEC CICS STARTBR                                            EL8521
01353          DATASET (FILE-ID-ERACCT)                                 EL8521
01354          RIDFLD  (ERACCT-KEY)                                     EL8521
01355      END-EXEC.                                                    EL8521
01356                                                                   EL8521
01357      MOVE 'Y'                    TO WS-BROWSE-STARTED-SW.         EL8521
01358                                                                   EL8521
01359      EXEC CICS HANDLE CONDITION                                   EL8521
01360          NOTFND   (4070-ACCOUNT-PROCESSED)                        EL8521
01361          ENDFILE  (4070-ACCOUNT-PROCESSED)                        EL8521
01362      END-EXEC.                                                    EL8521
01363                                                                   EL8521
01364  4250-READ-LOOP.                                                  EL8521
01365                                                                   EL8521
01366      EXEC CICS READNEXT                                           EL8521
01367          DATASET   (FILE-ID-ERACCT)                               EL8521
01368          SET       (ADDRESS OF ACCOUNT-MASTER)                       CL**7
01369          RIDFLD    (ERACCT-KEY)                                   EL8521
01370      END-EXEC.                                                    EL8521
01371                                                                   EL8521
01372      MOVE AM-NAME                TO WS-AM-NAME.                   EL8521
01373                                                                   EL8521
01374      IF ERACCT-KEY = ERACCT-SAVE-KEY                              EL8521
01375         GO TO 4250-READ-LOOP.                                     EL8521
01376                                                                   EL8521
01377  4070-ACCOUNT-PROCESSED.                                          EL8521
01378                                                                   EL8521
01379      IF WS-BROWSE-STARTED                                         EL8521
01380         EXEC CICS ENDBR                                           EL8521
01381             DATASET (FILE-ID-ERACCT)                              EL8521
01382         END-EXEC.                                                 EL8521
01383                                                                   EL8521
01384      GO TO 4090-EXIT.                                             EL8521
01385                                                                   EL8521
01386  4080-ACCOUNT-NOTFND.                                             EL8521
01387                                                                   EL8521
01388      MOVE ER-2210                TO EMI-ERROR.                    EL8521
01389      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
01390                                                                   EL8521
01391  4090-EXIT.                                                       EL8521
01392      EXIT.                                                        EL8521
01393                                                                   EL8521
01394      EJECT                                                        EL8521
01395                                                                   EL8521
01396                                                                   EL8521
01397 ******************************************************************EL8521
01398 *                                                                *EL8521
01399 *       R E A D   C O M P E N S A T I O N   M A S T E R          *EL8521
01400 *                                                                *EL8521
01401 ******************************************************************EL8521
01402                                                                   EL8521
01403  4100-READ-COMP-MASTER.                                           EL8521
01404                                                                   EL8521
01405      EXEC CICS HANDLE CONDITION                                   EL8521
01406          NOTFND   (4170-COMP-MASTER-NOTFND)                       EL8521
01407          END-EXEC.                                                EL8521
01408                                                                   EL8521
01409      MOVE RQ-COMPANY-CD          TO ERCOMP-COMP-CD.               EL8521
01410                                                                   EL8521
01411      IF PI-ZERO-CARRIER OR                                        EL8521
01412         PI-ZERO-CAR-GROUP                                         EL8521
01413         MOVE ZEROS               TO ERCOMP-CARRIER                EL8521
01414      ELSE                                                         EL8521
01415         MOVE RQ-CARRIER-A2       TO ERCOMP-CARRIER.               EL8521
01416                                                                   EL8521
01417      IF PI-ZERO-GROUPING OR                                       EL8521
01418         PI-ZERO-CAR-GROUP                                         EL8521
01419         MOVE ZEROS               TO ERCOMP-GROUPING               EL8521
01420      ELSE                                                         EL8521
01421         MOVE RQ-GROUPING-A2      TO ERCOMP-GROUPING.              EL8521
01422                                                                   EL8521
01423      MOVE RQ-FIN-RESP-A2         TO ERCOMP-FIN-RESP.              EL8521
01424      MOVE RQ-ACCT-AGENT-A2       TO ERCOMP-ACCOUNT.               EL8521
01425      MOVE 'A'                    TO ERCOMP-RECORD-TYPE.           EL8521
01426                                                                   EL8521
01427      EXEC CICS READ                                               EL8521
01428           DATASET   (FILE-ID-ERCOMP)                              EL8521
01429           SET       (ADDRESS OF COMPENSATION-MASTER)                 CL**7
01430           RIDFLD    (ERCOMP-KEY)                                  EL8521
01431      END-EXEC.                                                    EL8521
01432                                                                   EL8521
01433      MOVE CO-MAIL-NAME           TO WS-FIN-RESP-NAME.             EL8521
01434                                                                   EL8521
01435      GO TO 4190-EXIT.                                             EL8521
01436                                                                   EL8521
01437  4170-COMP-MASTER-NOTFND.                                         EL8521
01438                                                                   EL8521
01439      MOVE ER-2800                TO EMI-ERROR.                    EL8521
01440      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
01441                                                                   EL8521
01442  4190-EXIT.                                                       EL8521
01443      EXIT.                                                        EL8521
01444                                                                   EL8521
01445      EJECT                                                        EL8521
01446                                                                   EL8521
01447 ******************************************************************EL8521
01448 *                                                                *EL8521
01449 *           R E A D   S U M M A R Y   H E A D E R                *EL8521
01450 *                                                                *EL8521
01451 ******************************************************************EL8521
01452                                                                   EL8521
01453  4200-READ-SUM-FILE.                                              EL8521
01454                                                                   EL8521
01455      MOVE LOW-VALUES             TO ERSUMM-KEY.                   EL8521
01456      MOVE RQ-COMPANY-CD          TO ERSUMM-COMPANY-CD.            EL8521
01457      MOVE RQ-SUMMARY-CODE        TO ERSUMM-SUMMARY.               EL8521
01458                                                                   EL8521
01459      EXEC CICS HANDLE CONDITION                                   EL8521
01460          NOTFND   (4280-SUMMARY-NOTFND)                           EL8521
01461      END-EXEC.                                                    EL8521
01462                                                                   EL8521
01463      EXEC CICS READ                                               EL8521
01464          DATASET   (FILE-ID-ERSUMM)                               EL8521
01465          SET       (ADDRESS OF SUMM-CROSS-REFERENCE)                 CL**7
01466          RIDFLD    (ERSUMM-KEY)                                   EL8521
01467      END-EXEC.                                                    EL8521
01468                                                                   EL8521
01469      MOVE SX-SUMM-OR-AGT-NAME    TO WS-SUM-NAME.                  EL8521
01470                                                                   EL8521
01471      GO TO 4290-EXIT.                                             EL8521
01472                                                                   EL8521
01473  4280-SUMMARY-NOTFND.                                             EL8521
01474                                                                   EL8521
01475      MOVE ER-3134                TO EMI-ERROR.                    EL8521
01476      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL8521
01477      MOVE SPACES                 TO WS-SUM-NAME.                  EL8521
01478                                                                   EL8521
01479  4290-EXIT.                                                       EL8521
01480      EXIT.                                                           CL**8
01481                                                                      CL**8
01482      EJECT                                                           CL**8
01483                                                                      CL**8
01484 ******************************************************************   CL**8
01485 *                                                                *   CL**8
01486 *   S E L E C T   B A T C H    T O    E D I T                    *   CL**8
01487 *                                                                *   CL**8
01488 ******************************************************************   CL**8
01489                                                                      CL**8
01490  5000-EDIT-BATCH.                                                    CL**8
01491                                                                      CL**8
01492      IF AR-SELECT NOT NUMERIC                                        CL**8
01493         MOVE ER-3132             TO EMI-ERROR                        CL**8
01494         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**8
01495         GO TO 8200-SEND-DATAONLY.                                    CL**8
01496                                                                      CL**8
01497      IF AR-SELECT GREATER THAN +0 AND LESS THAN +11                  CL**8
01498         NEXT SENTENCE                                                CL**8
01499      ELSE                                                            CL**8
01500         MOVE ER-3132             TO EMI-ERROR                        CL**8
01501         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                     CL**8
01502         GO TO 8200-SEND-DATAONLY.                                    CL**8
01503                                                                      CL**8
01504      MOVE LOW-VALUES             TO WS-PREV-PNDB.                    CL**8
01505      MOVE SPACES                 TO WS-SAVE-AM-CONTROLS              CL**8
01506                                     WS-SAVE-SUMMARY.                 CL**8
01507      MOVE 'N'                    TO WS-AGT-ERROR-SW                  CL**8
01508                                     WS-FIN-RESP-ERROR-SW             CL**8
01509                                     WS-AGT-COMP-ERROR-SW             CL**8
01510                                     WS-FIN-RESP-COMP-ERROR-SW        CL**8
01511                                     WS-NO-ACCOUNT-MASTER-SW.         CL**8
01512      MOVE 'Y'                    TO WS-NEW-ACCT-MAST-SW              CL**8
01513                                     WS-ACCOUNT-MASTER-SW.            CL**8
01514                                                                      CL**8
01515      MOVE LOW-VALUES             TO ERPNDB-KEY.                      CL**8
01516      MOVE PI-REQUEST-KEY (AR-SELECT)                                 CL**8
01517                                  TO WS-ACCESS-ERPNDB.                CL**8
01518      MOVE +1                     TO WS-ACCESS-SEQ.                   CL**8
01519      MOVE ZEROS                  TO WS-ACCESS-CHG-SEQ.               CL**8
01520      MOVE WS-ACCESS-ERPNDB       TO ERPNDB-KEY.                      CL**8
01521                                                                      CL**8
01522      EXEC CICS HANDLE CONDITION                                      CL**8
01523          NOTFND (5100-NOT-FOUND)                                     CL**8
01524          ENDFILE (5100-NOT-FOUND)                                    CL**8
01525      END-EXEC.                                                       CL**8
01526                                                                      CL**8
01527      EXEC CICS STARTBR                                               CL**8
01528          DATASET (FILE-ID-ERPNDB)                                    CL**8
01529          RIDFLD  (ERPNDB-KEY)                                        CL**8
01530      END-EXEC.                                                       CL**8
01531                                                                      CL**8
01532  5010-READ-PNDB.                                                     CL**8
01533                                                                      CL**8
01534      EXEC CICS READNEXT                                              CL**8
01535          DATASET   (FILE-ID-ERPNDB)                                  CL**8
01536          SET       (ADDRESS OF PENDING-BUSINESS)                     CL**8
01537          RIDFLD    (ERPNDB-KEY)                                      CL**8
01538      END-EXEC.                                                       CL**8
01539                                                                      CL**8
01540      IF WS-PREV-PNDB = LOW-VALUES                                    CL**8
01541          MOVE PB-CONTROL-PRIMARY TO WS-PREV-PNDB                     CL**8
01542          IF (WS-PREV-COMPANY NOT = PB-COMPANY-CD)  OR                CL**8
01543             (WS-PREV-BATCH   NOT = PB-ENTRY-BATCH)                   CL**8
01544              GO TO 5100-NOT-FOUND.                                   CL**8
01545                                                                      CL**8
01546      IF (PB-COMPANY-CD NOT = WS-PREV-COMPANY) OR                     CL**8
01547         (PB-ENTRY-BATCH NOT = WS-PREV-BATCH)                         CL**8
01548          PERFORM 6000-UPDATE-REQUEST  THRU  6999-EXIT                CL**8
01549          GO TO 5999-EXIT.                                            CL**8
01550                                                                      CL**8
01551      IF PB-MAILING-DATA                                              CL**8
01552          GO TO 5010-READ-PNDB.                                       CL**8
01553                                                                      CL**8
01554      IF PB-ISSUE                                                     CL**8
01555          IF PB-I-ENTRY-STATUS = '5' OR '9' OR 'D' OR 'V'             CL**8
122002                             OR 'M'
01556              GO TO 5010-READ-PNDB.                                   CL**8
01557                                                                      CL**8
01558      IF PB-CANCELLATION                                              CL**8
01559          IF PB-I-ENTRY-STATUS = '9' OR 'D' OR 'V'                    CL**8
01560              GO TO 5010-READ-PNDB.                                   CL**8
01561                                                                      CL**8
01562      IF PB-BATCH-TRAILER                                             CL**8
01563          PERFORM 6000-UPDATE-REQUEST  THRU  6999-EXIT                CL**8
01564          GO TO 5999-EXIT.                                            CL**8
01565                                                                      CL**8
01566      MOVE 'N'                    TO WS-TRAILER-ONLY-SW.              CL**8
01567                                                                      CL**8
01568      IF WS-NEW-ACCT-MAST                                             CL**8
01569          PERFORM 7000-ACCOUNT-MATCH  THRU  7999-EXIT                 CL**8
01570              GO TO 5010-READ-PNDB.                                   CL**8
01571                                                                      CL**8
01572      IF NOT WS-ACCOUNT-MASTER                                        CL**8
01573          PERFORM 7000-ACCOUNT-MATCH  THRU  7999-EXIT                 CL**8
01574              GO TO 5010-READ-PNDB.                                   CL**8
01575                                                                      CL**8
01576      IF (PB-CERT-EFF-DT LESS THAN WS-SAVE-AM-EFF-DT) OR              CL**8
01577         (PB-CERT-EFF-DT GREATER THAN WS-SAVE-AM-EXP-DT)              CL**8
01578          PERFORM 7000-ACCOUNT-MATCH  THRU  7999-EXIT                 CL**8
01579              GO TO 5010-READ-PNDB.                                   CL**8
01580                                                                      CL**8
01581      GO TO 5010-READ-PNDB.                                           CL**8
01582                                                                      CL**8
01583  5100-NOT-FOUND.                                                     CL**8
01584                                                                      CL**8
01585      MOVE ER-2242                TO EMI-ERROR.                       CL**8
01586      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**8
01587      GO TO 8200-SEND-DATAONLY.                                       CL**8
01588                                                                      CL**8
01589  5999-EXIT.                                                          CL**8
01590                                                                      CL**8
01591      EJECT                                                           CL**8
01592                                                                      CL**8
01593 ******************************************************************   CL**8
01594 *                                                                *   CL**8
01595 *   E D I T   B A T C H    A N D   R E B U I D     R E Q U E S T *   CL**8
01596 *                                                                *   CL**8
01597 ******************************************************************   CL**8
01598                                                                      CL**8
01599  6000-UPDATE-REQUEST.                                                CL**8
01600                                                                      CL**8
01601     EXEC CICS ENDBR                                                  CL**8
01602         DATASET (FILE-ID-ERPNDB)                                     CL**8
01603     END-EXEC.                                                        CL**8
01604                                                                      CL**8
01605      IF WS-TRAILER-ONLY                                              CL**8
01606          MOVE ER-2211            TO EMI-ERROR                        CL**8
01607          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL**8
01608          GO TO 8200-SEND-DATAONLY.                                   CL**8
01609                                                                      CL**8
01610      IF WS-NO-ACCOUNT-MASTER   OR                                    CL**8
01611         WS-AGT-ERROR           OR                                    CL**8
01612         WS-FIN-RESP-ERROR                                            CL**8
01613          GO TO 6400-UPDATE.                                          CL**8
01614                                                                      CL**8
01615      EXEC CICS HANDLE CONDITION                                      CL**8
01616          NOTFND   (6100-NO-ACCT-AGENT-COMP)                          CL**8
01617          END-EXEC.                                                   CL**8
01618                                                                      CL**8
01619      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.                  CL**8
01620                                                                      CL**8
01621      IF PI-ZERO-CARRIER OR                                           CL**8
01622         PI-ZERO-CAR-GROUP                                            CL**8
01623         MOVE ZEROS               TO ERCOMP-CARRIER                   CL**8
01624      ELSE                                                            CL**8
01625         MOVE WS-SAVE-CARRIER     TO ERCOMP-CARRIER.                  CL**8
01626                                                                      CL**8
01627      IF PI-ZERO-GROUPING OR                                          CL**8
01628         PI-ZERO-CAR-GROUP                                            CL**8
01629         MOVE ZEROS               TO ERCOMP-GROUPING                  CL**8
01630      ELSE                                                            CL**8
01631         MOVE WS-SAVE-GROUPING    TO ERCOMP-GROUPING.                 CL**8
01632                                                                      CL**8
01633      MOVE WS-SAVE-FIN-RESP       TO ERCOMP-FIN-RESP.                 CL**8
01634      MOVE WS-SAVE-ACCT-AGENT     TO ERCOMP-ACCOUNT.                  CL**8
01635      MOVE 'A'                    TO ERCOMP-RECORD-TYPE.              CL**8
01636                                                                      CL**8
01637      EXEC CICS READ                                                  CL**8
01638           DATASET   (FILE-ID-ERCOMP)                                 CL**8
01639           SET       (ADDRESS OF COMPENSATION-MASTER)                 CL**8
01640           RIDFLD    (ERCOMP-KEY)                                     CL**8
01641      END-EXEC.                                                       CL**8
01642                                                                      CL**8
01643      MOVE CO-AR-SUMMARY-CODE     TO WS-SAVE-SUMMARY.                 CL**8
01644      GO TO 6200-TEST-FIN-RESP-COMP.                                  CL**8
01645                                                                      CL**8
01646  6100-NO-ACCT-AGENT-COMP.                                            CL**8
01647                                                                      CL**8
01648      MOVE 'Y'                    TO WS-AGT-COMP-ERROR-SW.            CL**8
01649                                                                      CL**8
01650  6200-TEST-FIN-RESP-COMP.                                            CL**8
01651                                                                      CL**8
01652      IF WS-SAVE-ACCT-AGENT = WS-SAVE-FIN-RESP                        CL**8
01653          GO TO 6400-UPDATE.                                          CL**8
01654                                                                      CL**8
01655      MOVE LOW-VALUES             TO CO-RESP-NO.                      CL**8
01656      MOVE 'G'                    TO CO-TYPE.                         CL**8
01657                                                                      CL**8
01658      EXEC CICS HANDLE CONDITION                                      CL**8
01659          NOTFND   (6300-NO-FIN-RESP-COMP)                            CL**8
01660          END-EXEC.                                                   CL**8
01661                                                                      CL**8
01662      EXEC CICS READ                                                  CL**8
01663           DATASET   (FILE-ID-ERCOMP)                                 CL**8
01664           SET       (ADDRESS OF COMPENSATION-MASTER)                 CL**8
01665           RIDFLD    (ERCOMP-KEY)                                     CL**8
01666      END-EXEC.                                                       CL**8
01667                                                                      CL**8
01668      GO TO 6400-UPDATE.                                              CL**8
01669                                                                      CL**8
01670  6300-NO-FIN-RESP-COMP.                                              CL**8
01671                                                                      CL**8
01672      MOVE 'Y'                    TO WS-FIN-RESP-COMP-ERROR-SW.       CL**8
01673                                                                      CL**8
01674  6400-UPDATE.                                                        CL**8
01675                                                                      CL**8
01676      MOVE PI-REQUEST-KEY (AR-SELECT)                                 CL**8
01677                                  TO ERRQST-KEY.                      CL**8
01678                                                                      CL**8
01679      EXEC CICS READ                                                  CL**8
01680          SET     (ADDRESS OF AR-REQUEST-RECORD)                      CL**8
01681          DATASET (FILE-ID-ERRQST)                                    CL**8
01682          RIDFLD  (ERRQST-KEY)                                        CL**8
01683          UPDATE                                                      CL**8
01684      END-EXEC.                                                       CL**8
01685                                                                      CL**8
01686      IF NOT WS-NO-ACCOUNT-MASTER   AND                               CL**8
01687         NOT WS-AGT-ERROR           AND                               CL**8
01688         NOT WS-FIN-RESP-ERROR      AND                               CL**8
01689         NOT WS-AGT-COMP-ERROR      AND                               CL**8
01690         NOT WS-FIN-RESP-COMP-ERROR                                   CL**8
01691          MOVE ' '                TO RQ-STATUS                        CL**8
01692                                     AR-STATUS (AR-SELECT)            CL**8
01693      ELSE                                                            CL**8
01694          MOVE 'E'                TO RQ-STATUS                        CL**8
01695                                     AR-STATUS (AR-SELECT).           CL**8
01696                                                                      CL**8
01697      MOVE WS-SAVE-SUMMARY        TO RQ-SUMMARY-CODE.                 CL**8
01698                                                                      CL**8
01699      IF WS-AGT-ERROR OR WS-NO-ACCOUNT-MASTER                         CL**8
01700          MOVE 'UNKNOWN'          TO RQ-ACCT-AGENT-A2                 CL**8
01701                                     RQ-ACCT-AGENT-A3                 CL**8
01702      ELSE                                                            CL**8
01703          MOVE WS-SAVE-ACCT-AGENT TO RQ-ACCT-AGENT-A2                 CL**8
01704                                     RQ-ACCT-AGENT-A3.                CL**8
01705                                                                      CL**8
01706      IF WS-FIN-RESP-ERROR OR WS-NO-ACCOUNT-MASTER                    CL**8
01707          MOVE 'UNKNOWN'          TO RQ-FIN-RESP-A2                   CL**8
01708      ELSE                                                            CL**8
01709          MOVE WS-SAVE-FIN-RESP   TO RQ-FIN-RESP-A2.                  CL**8
01710                                                                      CL**8
01711      EXEC CICS REWRITE                                               CL**8
01712          DATASET (FILE-ID-ERRQST)                                    CL**8
01713          FROM    (AR-REQUEST-RECORD)                                 CL**8
01714      END-EXEC.                                                       CL**8
01715                                                                      CL**8
01716      MOVE ER-0000                TO EMI-ERROR.                       CL**8
01717      MOVE -1                     TO AR-SELECT-LEN.                   CL**8
01718      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**8
01719                                                                      CL**8
01720  6999-EXIT.                                                          CL**8
01721      EXIT.                                                           CL**8
01722                                                                      CL**8
01723      EJECT                                                           CL**8
01724                                                                      CL**8
01725 ******************************************************************   CL**8
01726 *                                                                *   CL**8
01727 *   M A T C H   T O   A C C O U N T   M A S T E R                *   CL**8
01728 *                                                                *   CL**8
01729 ******************************************************************   CL**8
01730                                                                      CL**8
01731  7000-ACCOUNT-MATCH.                                                 CL**8
01732                                                                      CL**8
01733      MOVE LOW-VALUES             TO ERACCT-KEY.                      CL**8
01734      MOVE PB-COMPANY-CD          TO ERACCT-COMP-CD.                  CL**8
01735      MOVE PB-SV-CARRIER          TO ERACCT-CARRIER.                  CL**8
01736      MOVE PB-SV-GROUPING         TO ERACCT-GROUPING.                 CL**8
01737      MOVE PB-SV-STATE            TO ERACCT-STATE.                    CL**8
01738      MOVE PB-ACCOUNT             TO ERACCT-ACCOUNT.                  CL**8
01739      MOVE PB-CERT-EFF-DT         TO ACCT-EXP-DATE.                   CL**8
01740                                                                      CL**8
01741      EXEC CICS HANDLE CONDITION                                      CL**8
01742          ENDFILE  (7100-NO-ACCOUNT-MASTER)                           CL**8
01743      END-EXEC.                                                       CL**8
01744                                                                      CL**8
01745      EXEC CICS STARTBR                                               CL**8
01746          DATASET (FILE-ID-ERACCT)                                    CL**8
01747          RIDFLD  (ERACCT-KEY)                                        CL**8
01748      END-EXEC.                                                       CL**8
01749                                                                      CL**8
01750  7050-ACCOUNT-MASTER-READNEXT.                                       CL**8
01751                                                                      CL**8
01752      EXEC CICS READNEXT                                              CL**8
01753          DATASET   (FILE-ID-ERACCT)                                  CL**8
01754          SET       (ADDRESS OF ACCOUNT-MASTER)                       CL**8
01755          RIDFLD    (ERACCT-KEY)                                      CL**8
01756      END-EXEC.                                                       CL**8
01757                                                                      CL**8
01758      IF PB-COMPANY-CD NOT = AM-COMPANY-CD-A1 OR                      CL**8
01759         PB-CARRIER    NOT = AM-VG-CARRIER    OR                      CL**8
01760         PB-GROUPING   NOT = AM-VG-GROUPING   OR                      CL**8
01761         PB-STATE      NOT = AM-VG-STATE      OR                      CL**8
01762         PB-ACCOUNT    NOT = AM-VG-ACCOUNT                            CL**8
01763          MOVE 'N'                TO WS-ACCOUNT-MASTER-SW             CL**8
01764          GO TO 7900-ENDBR.                                           CL**8
01765                                                                      CL**8
01766      IF (PB-CERT-EFF-DT = AM-EFFECTIVE-DT OR                         CL**8
01767          PB-CERT-EFF-DT GREATER THAN AM-EFFECTIVE-DT) AND            CL**8
01768         PB-CERT-EFF-DT LESS THAN AM-EXPIRATION-DT                    CL**8
01769          NEXT SENTENCE                                               CL**8
01770      ELSE                                                            CL**8
01771          GO TO 7050-ACCOUNT-MASTER-READNEXT.                         CL**8
01772                                                                      CL**8
01773      IF WS-NEW-ACCT-MAST                                             CL**8
01774          MOVE AM-CARRIER         TO WS-SAVE-CARRIER                  CL**8
01775          MOVE AM-GROUPING        TO WS-SAVE-GROUPING                 CL**8
01776          MOVE AM-STATE           TO WS-SAVE-STATE                    CL**8
01777          MOVE AM-AGT (01)        TO WS-SAVE-ACCT-AGENT               CL**8
01778          MOVE AM-AGT (AM-REMIT-TO)                                   CL**8
01779                                  TO WS-SAVE-FIN-RESP                 CL**8
01780          MOVE AM-EFFECTIVE-DT    TO WS-SAVE-AM-EFF-DT                CL**8
01781          MOVE AM-EXPIRATION-DT   TO WS-SAVE-AM-EXP-DT                CL**8
01782          MOVE 'N'                TO WS-NEW-ACCT-MAST-SW              CL**8
01783          GO TO 7900-ENDBR.                                           CL**8
01784                                                                      CL**8
01785      IF AM-AGT (01) NOT = WS-SAVE-ACCT-AGENT                         CL**8
01786          MOVE SPACES             TO WS-SAVE-ACCT-AGENT               CL**8
01787          MOVE 'Y'                TO WS-AGT-ERROR-SW.                 CL**8
01788                                                                      CL**8
01789      IF AM-AGT (AM-REMIT-TO) NOT = WS-SAVE-FIN-RESP                  CL**8
01790          MOVE SPACES             TO WS-SAVE-FIN-RESP                 CL**8
01791          MOVE 'Y'                TO WS-FIN-RESP-ERROR-SW.            CL**8
01792                                                                      CL**8
01793      GO TO 7900-ENDBR.                                               CL**8
01794                                                                      CL**8
01795  7100-NO-ACCOUNT-MASTER.                                             CL**8
01796                                                                      CL**8
01797      MOVE 'Y'                    TO WS-NO-ACCOUNT-MASTER-SW.         CL**8
01798      MOVE SPACES                 TO WS-SAVE-ACCT-AGENT               CL**8
01799                                     WS-SAVE-FIN-RESP.                CL**8
01800                                                                      CL**8
01801  7900-ENDBR.                                                         CL**8
01802      EXEC CICS ENDBR                                                 CL**8
01803          DATASET (FILE-ID-ERACCT)                                    CL**8
01804      END-EXEC.                                                       CL**8
01805                                                                      CL**8
01806  7999-EXIT.                                                          CL**8
01807      EXIT.                                                        EL8521
01808                                                                   EL8521
01809      EJECT                                                        EL8521
01810                                                                   EL8521
01811 ******************************************************************EL8521
01812 *                                                                *EL8521
01813 *            S  E N D    I N I T I A L   M A P                   *EL8521
01814 *                                                                *EL8521
01815 ******************************************************************EL8521
01816                                                                   EL8521
01817  8100-SEND-INITIAL-MAP.                                           EL8521
01818                                                                   EL8521
01819      MOVE EIBTIME                    TO TIME-IN.                  EL8521
01820                                                                   EL8521
01821      IF PI-MAP-NAME = EL852B                                      EL8521
01822         MOVE EMI-MESSAGE-AREA (1)    TO BERMSGO                   EL8521
01823         MOVE WS-CURRENT-DT           TO BDATEO                    EL8521
01824         MOVE TIME-OUT                TO BTIMEO                    EL8521
01825         MOVE -1                      TO AR-SELECT-LEN.            EL8521
01826                                                                   EL8521
01827      IF PI-MAP-NAME = EL852C                                      EL8521
01828         MOVE EMI-MESSAGE-AREA (1)    TO CERMSGO                   EL8521
01829         MOVE WS-CURRENT-DT           TO CDATEO                    EL8521
01830         MOVE TIME-OUT                TO CTIMEO                    EL8521
01831         MOVE -1                      TO AR-SELECT-LEN.            EL8521
01832                                                                   EL8521
01833      IF PI-MAP-NAME = EL852D                                      EL8521
01834         MOVE EMI-MESSAGE-AREA (1)    TO DERMSGO                   EL8521
01835         MOVE WS-CURRENT-DT           TO DDATEO                    EL8521
01836         MOVE TIME-OUT                TO DTIMEO                    EL8521
01837         MOVE -1                      TO DPFENTRL.                 EL8521
01838                                                                   EL8521
01839                                                                   EL8521
01840      EXEC CICS SEND                                               EL8521
01841          MAP      (PI-MAP-NAME)                                   EL8521
01842          MAPSET   (MAPSET-EL8521S)                                EL8521
01843          FROM     (EL852BI)                                       EL8521
01844          ERASE                                                    EL8521
01845          CURSOR                                                   EL8521
01846      END-EXEC.                                                    EL8521
01847                                                                   EL8521
01848      GO TO 9100-RETURN-TRAN.                                      EL8521
01849                                                                   EL8521
01850      EJECT                                                        EL8521
01851                                                                   EL8521
01852 ******************************************************************EL8521
01853 *                                                                *EL8521
01854 *              S E N D    D A T A O N L Y                        *EL8521
01855 *                                                                *EL8521
01856 ******************************************************************EL8521
01857                                                                   EL8521
01858  8200-SEND-DATAONLY.                                              EL8521
01859                                                                   EL8521
01860      MOVE EIBTIME                TO TIME-IN.                      EL8521
01861                                                                   EL8521
01862      IF PI-MAP-NAME = EL852B                                      EL8521
01863         MOVE EMI-MESSAGE-AREA (1)    TO BERMSGO                   EL8521
01864         MOVE WS-CURRENT-DT           TO BDATEO                    EL8521
01865         MOVE TIME-OUT                TO BTIMEO                    EL8521
01866         MOVE -1                      TO AR-SELECT-LEN.            EL8521
01867                                                                   EL8521
01868      IF PI-MAP-NAME = EL852C                                      EL8521
01869         MOVE EMI-MESSAGE-AREA (1)    TO CERMSGO                   EL8521
01870         MOVE WS-CURRENT-DT           TO CDATEO                    EL8521
01871         MOVE TIME-OUT                TO CTIMEO                    EL8521
01872         MOVE -1                      TO AR-SELECT-LEN.            EL8521
01873                                                                   EL8521
01874      IF PI-MAP-NAME = EL852D                                      EL8521
01875         MOVE EMI-MESSAGE-AREA (1)    TO DERMSGO                   EL8521
01876         MOVE WS-CURRENT-DT           TO DDATEO                    EL8521
01877         MOVE TIME-OUT                TO DTIMEO                    EL8521
01878         MOVE -1                      TO DPFENTRL.                 EL8521
01879                                                                   EL8521
01880                                                                   EL8521
01881      EXEC CICS SEND                                               EL8521
01882           MAP      (PI-MAP-NAME)                                  EL8521
01883           MAPSET   (MAPSET-EL8521S)                               EL8521
01884           FROM     (EL852BI)                                      EL8521
01885           DATAONLY                                                EL8521
01886           CURSOR                                                  EL8521
01887      END-EXEC.                                                    EL8521
01888                                                                   EL8521
01889      GO TO 9100-RETURN-TRAN.                                      EL8521
01890                                                                   EL8521
01891      EJECT                                                        EL8521
01892                                                                   EL8521
01893  8300-SEND-TEXT.                                                  EL8521
01894      EXEC CICS SEND TEXT                                          EL8521
01895          FROM     (LOGOFF-TEXT)                                   EL8521
01896          LENGTH   (LOGOFF-LENGTH)                                 EL8521
01897          ERASE                                                    EL8521
01898          FREEKB                                                   EL8521
01899      END-EXEC.                                                    EL8521
01900                                                                   EL8521
01901      EXEC CICS RETURN                                             EL8521
01902      END-EXEC.                                                    EL8521
01903                                                                   EL8521
01904                                                                   EL8521
01905  8400-LOG-JOURNAL-RECORD.                                         EL8521
01906      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL8521
01907      MOVE THIS-PGM                TO JP-PROGRAM-ID.               EL8521
01908                                                                   EL8521
01909 *    EXEC CICS JOURNAL                                            EL8521
01910 *        JFILEID     (PI-JOURNAL-FILE-ID)                         EL8521
01911 *        JTYPEID     ('EL')                                       EL8521
01912 *        FROM        (JOURNAL-RECORD)                             EL8521
01913 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   EL8521
01914 *        END-EXEC.                                                EL8521
01915                                                                   EL8521
01916  8500-DATE-CONVERT.                                               EL8521
01917      EXEC CICS LINK                                               EL8521
01918          PROGRAM  (LINK-ELDATCV)                                  EL8521
01919          COMMAREA (DATE-CONVERSION-DATA)                          EL8521
01920          LENGTH   (DC-COMM-LENGTH)                                EL8521
01921      END-EXEC.                                                    EL8521
01922                                                                   EL8521
01923  8500-EXIT.                                                       EL8521
01924      EXIT.                                                        EL8521
01925                                                                   EL8521
01926      EJECT                                                        EL8521
01927                                                                   EL8521
01928  8800-UNAUTHORIZED-ACCESS.                                        EL8521
01929      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL8521
01930      GO TO 8300-SEND-TEXT.                                        EL8521
01931                                                                   EL8521
01932  8810-PF23.                                                       EL8521
01933      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL8521
01934      MOVE XCTL-EL005             TO PGM-NAME.                     EL8521
01935      GO TO 9300-XCTL.                                             EL8521
01936                                                                   EL8521
01937  9200-RETURN-MAIN-MENU.                                           EL8521
01938      MOVE XCTL-EL626             TO PGM-NAME.                     EL8521
01939      GO TO 9300-XCTL.                                             EL8521
01940                                                                   EL8521
01941  9000-RETURN-CICS.                                                EL8521
01942      EXEC CICS RETURN                                             EL8521
01943      END-EXEC.                                                    EL8521
01944                                                                   EL8521
01945  9100-RETURN-TRAN.                                                EL8521
01946                                                                   EL8521
01947      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL8521
01948      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.         EL8521
01949                                                                   EL8521
01950      EXEC CICS RETURN                                             EL8521
01951          TRANSID    (TRANS-EXJ6)                                  EL8521
01952          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL8521
01953          LENGTH     (PI-COMM-LENGTH)                              EL8521
01954      END-EXEC.                                                    EL8521
01955                                                                   EL8521
01956  9300-XCTL.                                                       EL8521
01957      EXEC CICS XCTL                                               EL8521
01958          PROGRAM    (PGM-NAME)                                    EL8521
01959          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL8521
01960          LENGTH     (PI-COMM-LENGTH)                              EL8521
01961      END-EXEC.                                                    EL8521
01962                                                                   EL8521
01963  9400-CLEAR.                                                      EL8521
01964      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME                      EL8521
01965      GO TO 9300-XCTL.                                             EL8521
01966                                                                   EL8521
01967  9500-PF12.                                                       EL8521
01968      MOVE XCTL-EL010             TO PGM-NAME.                     EL8521
01969      GO TO 9300-XCTL.                                             EL8521
01970                                                                   EL8521
01971  9600-PGMID-ERROR.                                                EL8521
01972                                                                   EL8521
01973      EXEC CICS HANDLE CONDITION                                   EL8521
01974          PGMIDERR    (8300-SEND-TEXT)                             EL8521
01975      END-EXEC.                                                    EL8521
01976                                                                   EL8521
01977      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL8521
01978      MOVE ' '                    TO PI-ENTRY-CD-1.                EL8521
01979      MOVE XCTL-EL005             TO PGM-NAME.                     EL8521
01980      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL8521
01981      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL8521
01982      GO TO 9300-XCTL.                                             EL8521
01983                                                                   EL8521
01984  9900-ERROR-FORMAT.                                               EL8521
01985                                                                   EL8521
01986      IF NOT EMI-ERRORS-COMPLETE                                   EL8521
01987          MOVE LINK-EL001         TO PGM-NAME                      EL8521
01988          EXEC CICS LINK                                           EL8521
01989              PROGRAM    (PGM-NAME)                                EL8521
01990              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL8521
01991              LENGTH     (EMI-COMM-LENGTH)                         EL8521
01992          END-EXEC.                                                EL8521
01993                                                                   EL8521
01994  9900-EXIT.                                                       EL8521
01995      EXIT.                                                        EL8521
01996                                                                   EL8521
01997  9990-ABEND.                                                      EL8521
01998      MOVE LINK-EL004             TO PGM-NAME.                     EL8521
01999      MOVE DFHEIBLK               TO EMI-LINE1.                    EL8521
02000      EXEC CICS LINK                                               EL8521
02001          PROGRAM   (PGM-NAME)                                     EL8521
02002          COMMAREA  (EMI-LINE1)                                    EL8521
02003          LENGTH    (72)                                           EL8521
02004      END-EXEC.                                                    EL8521
02005                                                                   EL8521
02006      IF PI-MAP-NAME = EL852B                                      EL8521
02007         MOVE -1                  TO BPFENTRL.                     EL8521
02008                                                                   EL8521
02009      IF PI-MAP-NAME = EL852C                                      EL8521
02010         MOVE -1                  TO CPFENTRL.                     EL8521
02011                                                                   EL8521
02012      IF PI-MAP-NAME = EL852D                                      EL8521
02013         MOVE -1                  TO DPFENTRL.                     EL8521
02014                                                                   EL8521
02015      GO TO 8200-SEND-DATAONLY.                                    EL8521
02016                                                                   EL8521
02017      GOBACK.                                                      EL8521
02018                                                                   EL8521
02019      EJECT                                                        EL8521
02020                                                                   EL8521
02021  9995-SECURITY-VIOLATION.                                         EL8521
02022                              COPY ELCSCTP.                        EL8521
02023                                                                   EL8521
02024  9995-EXIT.                                                       EL8521
02025      EXIT.                                                        EL8521
02026                                                                   EL8521
