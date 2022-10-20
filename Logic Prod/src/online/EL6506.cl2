00001  ID DIVISION.                                                     01/14/97
00002                                                                   EL6506
00003  PROGRAM-ID.                 EL6506.                                 LV028
00004 *              PROGRAM CONVERTED BY                                  CL*26
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*26
00006 *              CONVERSION DATE 02/12/96 08:09:40.                    CL*26
00007 *                            VMOD=2.028                              CL*28
00008 *                                                                 EL6506
00009 *AUTHOR.     LOGIC,INC.                                              CL*26
00010 *            DALLAS, TEXAS.                                          CL*26
00011                                                                   EL6506
00012 *DATE-COMPILED.                                                      CL*26
00013 *SECURITY.   *****************************************************   CL*26
00014 *            *                                                   *   CL*26
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*26
00016 *            *                                                   *   CL*26
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*26
00018 *                                                                *   CL*26
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*26
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*26
00021 *            *                                                   *   CL*26
00022 *            *****************************************************   CL*26
00023 *                                                                    CL**5
00024 *REMARKS.    TRANSACTION - EXG6 - ACCOUNT MAINT (MISC. ACCT DATA).   CL**5
00025 *                                                                    CL**5
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL6506AO FILLER
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
101101******************************************************************

00026  ENVIRONMENT DIVISION.                                            EL6506
00027                                                                   EL6506
00028      EJECT                                                        EL6506
00029  DATA DIVISION.                                                   EL6506
00030  WORKING-STORAGE SECTION.                                         EL6506
00031  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.      CL*26
00032  77  FILLER  PIC X(32)  VALUE '********************************'. EL6506
00033  77  FILLER  PIC X(32)  VALUE '*    EL6506 WORKING STORAGE    *'. EL6506
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.028 *********'.    CL*28
00035                                                                   EL6506
00036  01  WS-DATE-AREA.                                                EL6506
00037      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL6506
00038      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.       CL*12
00039                                                                   EL6506
00040  01  STANDARD-AREAS.                                              EL6506
PEMMOD     12  WS-YYYYMMDD             PIC X(8).                           CL*54
PEMMOD     12  WS-COMP-DATE REDEFINES WS-YYYYMMDD                          CL*54
PEMMOD                                 PIC 9(8).                           CL*54
00041      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.     CL*13
00042      12  MAP-NAME                    PIC X(8)    VALUE 'EL6506A'. EL6506
00043      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6506S'. EL6506
00044      12  SCREEN-NUMBER               PIC X(4)    VALUE '650G'.    EL6506
00045      12  TRANS-ID                    PIC X(4)    VALUE 'EXG6'.    EL6506
00046      12  THIS-PGM                    PIC X(8)    VALUE 'EL6506'.  EL6506
00047      12  PGM-NAME                    PIC X(8)    VALUE SPACES.       CL*26
00048      12  TIME-IN                     PIC S9(7)   VALUE ZEROS.        CL*26
00049      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6506
00050          16  FILLER                  PIC X.                       EL6506
00051          16  TIME-OUT                PIC 99V99.                   EL6506
00052          16  FILLER                  PIC XX.                         CL*12
00053      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL6506
00054      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL6506
00055      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.   EL6506
00056      12  XCTL-650                    PIC X(8)    VALUE 'EL650'.   EL6506
00057      12  XCTL-6501                   PIC X(8)    VALUE 'EL6501'.  EL6506
00058      12  XCTL-6502                   PIC X(8)    VALUE 'EL6502'.  EL6506
00059      12  XCTL-6503                   PIC X(8)    VALUE 'EL6503'.  EL6506
00060      12  XCTL-6504                   PIC X(8)    VALUE 'EL6504'.  EL6506
00061      12  XCTL-6505                   PIC X(8)    VALUE 'EL6505'.     CL**7
00062      12  XCTL-6507                   PIC X(8)    VALUE 'EL6507'.     CL*15
00063      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL6506
00064      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL6506
00065      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL6506
00066      12  FILE-ID                     PIC X(8)    VALUE SPACES.    EL6506
00067      12  ERACCT-FILE                 PIC X(8)    VALUE 'ERACCT'.  EL6506
00068      12  ERRATE-FILE                 PIC X(8)    VALUE 'ERRATE'.  EL6506
00069      12  ELCNTL-FILE                 PIC X(8)    VALUE 'ELCNTL'.  EL6506
00070      12  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.    EL6506
00071      12  YMD-CURRENT-SAVE            PIC X(6)    VALUE SPACES.    EL6506
00072                                                                   EL6506
00073      12  ERACCT-LENGTH               PIC S9(4)   VALUE +2023 COMP.EL6506
00074      12  ELCNTL-LENGTH               PIC S9(4)   VALUE +527  COMP.EL6506
00075      12  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.EL6506
00076      12  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.EL6506
00077      12  SUB1                        PIC S9(4)   VALUE +0    COMP.EL6506
00078      12  SUB2                        PIC S9(4)   VALUE +0    COMP.EL6506
00079                                                                   EL6506
00080      12  DEEDIT-FIELD                PIC X(15).                   EL6506
00081      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6506
00082      12  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(13)V99.   EL6506
00083      12  DEEDIT-FIELD-V2  REDEFINES DEEDIT-FIELD PIC S9(13)V99.      CL*20
00084      12  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V999.     CL*20
00085      12  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).    CL**2
00086      12  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5). EL6506
00087      12  DEEDIT-FIELD-V6  REDEFINES DEEDIT-FIELD PIC S9(9)V9(6).  EL6506
00088                                                                   EL6506
00089      12  WS-EDIT-FIELD-CONV          PIC S9(4)   VALUE +0.        EL6506
00090      12  WS-TOT-PERCENT              PIC S9V9999 VALUE +0.        EL6506
00091      12  WS-NUM-RANGES               PIC S9      VALUE +0.           CL*20
00092      12  WS-OB-MODE-CODE             PIC X.                          CL*21
00093          88  VALID-OB-MODE              VALUE ' ' 'M' 'Q' 'S' 'A'.   CL*21
00094                                                                   EL6506
00095      12  RATE-KEY.                                                EL6506
00096          16  RATE-COMP-CD            PIC X.                       EL6506
00097          16  RATE-STATE              PIC XX.                      EL6506
00098          16  RATE-CLASS              PIC XX.                      EL6506
00099          16  RATE-DEV                PIC XXX.                     EL6506
00100          16  FILLER                  PIC X(20).                   EL6506
00101                                                                      CL*20
00102      12  WS-ACCOUNT-ALLOWANCE OCCURS 5 TIMES                         CL*20
00103                                      PIC S9(5)V99 VALUE ZEROS.       CL*26
00104                                                                      CL*20
00105      12  WS-BEGIN-RANGE       OCCURS 5 TIMES                         CL*20
00106                                      PIC S9(5)    VALUE ZEROS.       CL*26
00107                                                                      CL*20
00108      12  WS-END-RANGE         OCCURS 5 TIMES                         CL*20
00109                                      PIC S9(5)    VALUE ZEROS.       CL*26
00110                                                                   EL6506
00111      12  WS-LF-DEV-PERCENT           PIC S9V9(6)  VALUE ZEROS.    EL6506
00112      12  WS-AH-DEV-PERCENT           PIC S9V9(6)  VALUE ZEROS.    EL6506
00113                                                                   EL6506
00114      12  WS-OB-LF-RATE               PIC S99V9(5) VALUE ZEROS.    EL6506
00115      12  WS-OB-JNTLF-RATE            PIC S99V9(5) VALUE ZEROS.    EL6506
00116      12  WS-OB-AH-RATE               PIC S99V9(5) VALUE ZEROS.    EL6506
00117                                                                   EL6506
00118      12  WS-TOL-PREM                 PIC S999V99    VALUE ZEROS.     CL**2
00119      12  WS-TOL-REF                  PIC S999V99    VALUE ZEROS.     CL**2
00120      12  WS-TOL-REF-PCT              PIC S9V9(4)    VALUE ZEROS.     CL*28
00121      12  WS-TOL-CLM                  PIC S999V99    VALUE ZEROS.     CL**2
00122      12  WS-OVR-SHT-AMT              PIC S999V99    VALUE ZEROS.     CL*28
00123      12  WS-OVR-SHT-PCT              PIC S9V9(4)    VALUE ZEROS.     CL*28
00124      12  WS-LF-EXP-PERCENT           PIC S9(3)V9(4) VALUE ZEROS.     CL**2
00125      12  WS-AH-EXP-PERCENT           PIC S9(3)V9(4) VALUE ZEROS.     CL**2
00126      12  SV-MAX-MON-BEN              PIC S9(7)      VALUE ZEROS.     CL**5
00127      12  SV-MAX-TOT-BEN              PIC S9(7)      VALUE ZEROS.     CL**5
00128      12  WS-MAX-MON-BEN              PIC S9(7)      VALUE ZEROS.     CL**5
00131      12  WS-MAX-TOT-BEN              PIC S9(7)      VALUE ZEROS.     CL**5
00134      EJECT                                                        EL6506
00135      12  ERROR-MESSAGES.                                          EL6506
00136          16  ER-0000                 PIC X(4)    VALUE '0000'.    EL6506
00137          16  ER-0002                 PIC X(4)    VALUE '0002'.    EL6506
00138          16  ER-0004                 PIC X(4)    VALUE '0004'.    EL6506
00139          16  ER-0008                 PIC X(4)    VALUE '0008'.    EL6506
00140          16  ER-0029                 PIC X(4)    VALUE '0029'.    EL6506
00141          16  ER-0068                 PIC X(4)    VALUE '0068'.    EL6506
00142          16  ER-0070                 PIC X(4)    VALUE '0070'.    EL6506
00143          16  ER-2039                 PIC X(4)    VALUE '2039'.    EL6506
00144          16  ER-2154                 PIC X(4)    VALUE '2154'.    EL6506
00145          16  ER-2165                 PIC X(4)    VALUE '2165'.    EL6506
00146          16  ER-2168                 PIC X(4)    VALUE '2168'.    EL6506
00147          16  ER-2169                 PIC X(4)    VALUE '2169'.    EL6506
00148          16  ER-2170                 PIC X(4)    VALUE '2170'.    EL6506
00149          16  ER-2223                 PIC X(4)    VALUE '2223'.       CL*20
00150          16  ER-2572                 PIC X(4)    VALUE '2572'.    EL6506
00151          16  ER-2591                 PIC X(4)    VALUE '2591'.       CL*21
00152          16  ER-3043                 PIC X(4)    VALUE '3043'.       CL*20
00153          16  ER-3124                 PIC X(4)    VALUE '3124'.    EL6506
00154          16  ER-3125                 PIC X(4)    VALUE '3125'.    EL6506
00155          16  ER-3126                 PIC X(4)    VALUE '3126'.    EL6506
00156          16  ER-3127                 PIC X(4)    VALUE '3127'.       CL**5
00157          16  ER-3128                 PIC X(4)    VALUE '3128'.       CL**5
00158          16  ER-3779                 PIC X(4)    VALUE '3779'.       CL*22
00159          16  ER-7320                 PIC X(4)    VALUE '7320'.       CL**4
00160          16  ER-7531                 PIC X(4)    VALUE '7531'.    EL6506
00161          16  ER-0627                 PIC X(4)    VALUE '0627'.       CL**6
00162                                                                   EL6506
00163      12  ELCNTL-KEY.                                              EL6506
00164          16  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.    EL6506
00165          16  CNTL-REC-TYPE           PIC X       VALUE SPACES.    EL6506
00166          16  CNTL-ACCESS             PIC X(4)    VALUE SPACES.    EL6506
00167          16  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.  EL6506
00168                                                                   EL6506
00169      12  WS-BROWSE-STARTED-SW        PIC X       VALUE SPACES.       CL*24
00170          88  WS-BROWSE-STARTED                   VALUE 'Y'.          CL*24
00171      12  WS-SAVE-REPORT-CODE1        PIC X(10)   VALUE SPACES.       CL*14
00172      12  WS-SAVE-REPORT-CODE2        PIC X(10)   VALUE SPACES.       CL*14
00173      12  WS-REPORT-CODE-CAPTION.                                     CL*12
00174          16  WS-REPORT-CD-CAPTION    PIC X(14)   VALUE SPACES.       CL*12
00175          16  FILLER                  PIC X       VALUE ':'.          CL*12
00176                                                                      CL*12
00177      EJECT                                                        EL6506
00178                            COPY ELCSCTM.                             CL**5
00179      EJECT                                                        EL6506
00180                            COPY ELCSCRTY.                            CL**5
00181      EJECT                                                        EL6506
00182                            COPY ELCLOGOF.                            CL*12
00183      EJECT                                                        EL6506
00184                            COPY ELCDATE.                             CL*12
00185      EJECT                                                        EL6506
00186                            COPY ELCATTR.                             CL*12
00187      EJECT                                                           CL**5
00188                            COPY ELCEMIB.                             CL*12
00189      EJECT                                                           CL**5
00190                            COPY ELCINTF.                             CL*12
00191                            COPY ELC650PI.                            CL*15
00192                            COPY ELCJPFX.                             CL*12
00193                                          PIC X(2000).                CL*12
00194                                                                   EL6506
00195      EJECT                                                        EL6506
00196                            COPY ELCAID.                              CL*21
00197  01  FILLER    REDEFINES DFHAID.                                  EL6506
00198      12  FILLER                      PIC X(8).                    EL6506
00199      12  PF-VALUES                   PIC X       OCCURS 2.        EL6506
00200                                                                   EL6506
00201      EJECT                                                        EL6506
00202                            COPY EL6506S.                             CL*21
00203                                                                      CL*20
CIDMOD 01  MAP-B REDEFINES EL6506AO.                                         000
101101     12  FILLER                      PIC X(257).                       000
CIDMOD     12  FLC-ACCOUNT-LENGTH          PIC S9(4)   COMP.                 000
CIDMOD     12  FLC-ACCOUNT-ATTRB           PIC X.                            000
CIDMOD     12  FLC-ACCOUNT-INPUT           PIC X(226).                       000
CIDMOD                                                                       000
00204      EJECT                                                        EL6506
00205  LINKAGE SECTION.                                                 EL6506
00206  01  DFHCOMMAREA                     PIC X(1500).                    CL*13
00207                                                                   EL6506
00208      EJECT                                                        EL6506
00209 *01 PARMLIST .                                                       CL*26
00210 *    02  FILLER                      PIC S9(8)   COMP.               CL*26
00211 *    02  ERACCT-POINTER              PIC S9(8)   COMP.               CL*26
00212 *    02  ERRATE-POINTER              PIC S9(8)   COMP.               CL*26
00213 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.               CL*26
00214                                                                   EL6506
00215                            COPY ERCACCT.                             CL*21
00216      EJECT                                                        EL6506
00217                            COPY ERCRATE.                             CL*21
00218      EJECT                                                        EL6506
00219                            COPY ELCCNTL.                             CL*21
00220      EJECT                                                        EL6506
00221                                                                   EL6506
00222  PROCEDURE DIVISION.                                              EL6506
00223      CONTINUE.                                                       CL*26
00224                                                                   EL6506
00225      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6506
00226      MOVE '5'                    TO  DC-OPTION-CODE.              EL6506
00227      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6506
00228      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL6506
00229      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL6506
00230      MOVE DC-GREG-DATE-1-YMD     TO  YMD-CURRENT-SAVE.            EL6506
00231                                                                   EL6506
00232      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6506
00233      MOVE +1                     TO  EMI-NUMBER-OF-LINES.            CL*24
00234                                                                   EL6506
00235      IF EIBCALEN = 0                                              EL6506
00236          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6506
00237                                                                   EL6506
00238      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6506
00239          IF PI-CALLING-PROGRAM = XCTL-6501                           CL*23
00240              MOVE PI-CALLING-PROGRAM                                 CL*23
00241                                  TO  PI-RETURN-TO-PROGRAM            CL*23
00242              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM              CL*23
00243          ELSE                                                        CL*23
00244              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM.             CL*23
00245                                                                   EL6506
00246      MOVE LOW-VALUES             TO  EL6506AI.                    EL6506
00247                                                                   EL6506
00248      IF EIBTRNID NOT = TRANS-ID                                   EL6506
00249          MOVE PI-MAINT           TO  MAINTYPO                     EL6506
00250          MOVE AL-UANON           TO  MAINTYPA                     EL6506
00251          MOVE -1                 TO  MAINTYPL                     EL6506
00252          IF PI-MAINT = 'S' OR 'C'                                 EL6506
00253              GO TO 4000-SHOW                                      EL6506
00254          ELSE                                                     EL6506
00255              IF PI-MAINT = 'A'                                    EL6506
00256                  MOVE 'C'            TO  PI-MAINT                 EL6506
00257                  GO TO 4000-SHOW                                  EL6506
00258              ELSE                                                 EL6506
00259                  GO TO 8100-SEND-INITIAL-MAP.                     EL6506
00260                                                                   EL6506
00261      EXEC CICS HANDLE CONDITION                                   EL6506
00262          PGMIDERR  (9600-PGMID-ERROR)                             EL6506
00263          ERROR     (9990-ABEND)                                   EL6506
00264      END-EXEC.                                                    EL6506
00265                                                                   EL6506
00266      IF EIBAID = DFHCLEAR                                         EL6506
00267          GO TO 9400-CLEAR.                                        EL6506
00268                                                                   EL6506
00269      EJECT                                                        EL6506
00270  0200-RECEIVE.                                                    EL6506
00271      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6506
00272          MOVE ER-0008            TO  EMI-ERROR                    EL6506
00273          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6506
00274          MOVE -1                 TO  PFENTERL                     EL6506
00275          GO TO 8200-SEND-DATAONLY.                                EL6506
00276                                                                   EL6506
00277      EXEC CICS RECEIVE                                            EL6506
00278          MAP      (MAP-NAME)                                      EL6506
00279          MAPSET   (MAPSET-NAME)                                   EL6506
00280          INTO     (EL6506AI)                                      EL6506
00281      END-EXEC.                                                    EL6506
00282                                                                   EL6506
00283      IF PFENTERL = 0                                              EL6506
00284          GO TO 0300-CHECK-PFKEYS.                                 EL6506
00285      IF EIBAID NOT = DFHENTER                                     EL6506
00286          MOVE ER-0004            TO  EMI-ERROR                    EL6506
00287          GO TO 0320-INPUT-ERROR.                                  EL6506
00288      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)      CL*13
00289          MOVE PF-VALUES (PFENTERI) TO  EIBAID                     EL6506
00290      ELSE                                                         EL6506
00291          MOVE ER-0029            TO  EMI-ERROR                    EL6506
00292          GO TO 0320-INPUT-ERROR.                                  EL6506
00293                                                                   EL6506
00294      EJECT                                                        EL6506
00295  0300-CHECK-PFKEYS.                                               EL6506
00296      IF EIBAID = DFHPF23                                          EL6506
00297          GO TO 8810-PF23.                                         EL6506
00298      IF EIBAID = DFHPF24                                          EL6506
00299          GO TO 9200-RETURN-MAIN-MENU.                             EL6506
00300      IF EIBAID = DFHPF12                                          EL6506
00301          GO TO 9500-PF12.                                         EL6506
00302      IF EIBAID = DFHPF5                                              CL*18
00303          MOVE XCTL-6502          TO  PGM-NAME                        CL*18
00304          GO TO 9300-XCTL.                                            CL*18
00305      IF EIBAID = DFHPF7                                           EL6506
00306          MOVE XCTL-6504          TO  PGM-NAME                     EL6506
00307          GO TO 9300-XCTL.                                         EL6506
00308      IF EIBAID = DFHPF8                                           EL6506
00309          MOVE XCTL-6501          TO  PGM-NAME                     EL6506
00310          GO TO 9300-XCTL.                                         EL6506
00311      IF EIBAID = DFHPF9                                              CL**7
00312          MOVE XCTL-6505          TO  PGM-NAME                        CL**7
00313          GO TO 9300-XCTL.                                            CL**7
00314 *    IF EIBAID = DFHPF10                                             CL*19
00315 *       MOVE PI-ACCT-CCGSA-KEY   TO  PI-PLAN-KEY                     CL*19
00316 *       MOVE ZEROS               TO  PI-PLAN-BEN-TYPE                CL*19
00317 *                                    PI-PLAN-BEN                     CL*19
00318 *                                    PI-PLAN-REVISION                CL*19
00319 *       MOVE XCTL-6507           TO  PGM-NAME                        CL*19
00320 *       GO TO 9300-XCTL.                                             CL*19
00321                                                                      CL*15
00322      IF EIBAID = DFHENTER                                         EL6506
00323          GO TO 0330-CHECK-MAINTYP.                                EL6506
00324                                                                   EL6506
00325      MOVE ER-0029                TO  EMI-ERROR.                   EL6506
00326  0320-INPUT-ERROR.                                                EL6506
00327      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6506
00328      MOVE AL-UNBON               TO  PFENTERA.                    EL6506
00329      MOVE -1                     TO  PFENTERL.                    EL6506
00330      GO TO 8200-SEND-DATAONLY.                                    EL6506
00331                                                                   EL6506
00332  EJECT                                                               CL*20
00333  0330-CHECK-MAINTYP.                                              EL6506
00334      IF MAINTYPL GREATER ZERO                                     EL6506
00335          IF MAINTYPI = 'S' OR 'C' OR 'A'                          EL6506
00336              MOVE AL-UANON       TO  MAINTYPA                     EL6506
00337              MOVE MAINTYPI       TO  PI-MAINT                     EL6506
00338          ELSE                                                     EL6506
00339              MOVE -1             TO  MAINTYPL                     EL6506
00340              MOVE AL-UABON       TO  MAINTYPA                     EL6506
00341              MOVE ER-2039        TO  EMI-ERROR                    EL6506
00342              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6506
00343              GO TO 8200-SEND-DATAONLY                             EL6506
00344      ELSE                                                         EL6506
00345          MOVE -1                 TO  MAINTYPL                     EL6506
00346          MOVE AL-UABON           TO  MAINTYPA                     EL6506
00347          MOVE ER-2039            TO  EMI-ERROR                    EL6506
00348          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6506
00349          GO TO 8200-SEND-DATAONLY.                                EL6506
00350                                                                   EL6506
00351      IF PI-MAINT = 'S'                                            EL6506
00352          GO TO 4000-SHOW.                                         EL6506
00353                                                                   EL6506
00354      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.                EL6506
00355                                                                      CL*12
00356      IF EMI-ERROR NOT = ZEROS                                     EL6506
00357          MOVE -1                 TO  MAINTYPL                     EL6506
00358          GO TO 8200-SEND-DATAONLY.                                EL6506
00359                                                                   EL6506
00360      GO TO 4200-MAINT.                                            EL6506
00361                                                                   EL6506
00362      EJECT                                                        EL6506
00363                                                                   EL6506
00364  4000-SHOW.                                                       EL6506
00365      IF LCP-ONCTR-01 =  0                                            CL*26
00366          ADD 1 TO LCP-ONCTR-01                                       CL*26
00367          PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.               CL*12
00368                                                                      CL*12
00369      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6506
00370      MOVE LOW-VALUES             TO  EL6506AO.                    EL6506
00371      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL6506
00372                                                                   EL6506
00373      EJECT                                                        EL6506
00374  4200-MAINT.                                                      EL6506
00375      IF NOT MODIFY-CAP                                            EL6506
00376          MOVE 'UPDATE'       TO SM-READ                           EL6506
00377          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL6506
00378          MOVE ER-0070             TO  EMI-ERROR                   EL6506
00379          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6506
00380          GO TO 8100-SEND-INITIAL-MAP.                             EL6506
00381                                                                   EL6506
00382      PERFORM 7000-EDIT THRU 7099-EXIT.                            EL6506
00383                                                                   EL6506
00384      IF EMI-NO-ERRORS                                             EL6506
00385          NEXT SENTENCE                                            EL6506
00386      ELSE                                                         EL6506
00387          IF EMI-FORCABLE OR EMI-FATAL                                CL**9
00388             GO TO 8200-SEND-DATAONLY.                                CL**9
00389                                                                   EL6506
00390      PERFORM 7300-READ-ERACCT-UPDATE THRU 7300-EXIT.              EL6506
00391                                                                   EL6506
00392      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.                 CL*12
00393      MOVE ERACCT-FILE            TO  FILE-ID.                     EL6506
00394                                                                   EL6506
00395      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT.              EL6506
00396                                                                   EL6506
00397      IF AM-LAST-MAINT-USER   = PI-UPDATE-BY OR                       CL*13
00398         AM-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6506
00399          NEXT SENTENCE                                            EL6506
00400      ELSE                                                         EL6506
00401          EXEC CICS UNLOCK                                         EL6506
00402               DATASET  (ERACCT-FILE)                              EL6506
00403          END-EXEC                                                 EL6506
00404          MOVE ER-0068            TO  EMI-ERROR                    EL6506
00405          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6506
00406          PERFORM 7100-READ-ERACCT  THRU 7100-EXIT                 EL6506
00407          MOVE LOW-VALUES         TO  EL6506AO                     EL6506
00408          MOVE -1                 TO  MAINTYPL                     EL6506
00409          MOVE 'S'                TO  PI-MAINT                     EL6506
00410          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL6506
00411                                                                   EL6506
00412      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.          EL6506
00413      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.        EL6506
00414      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6506
00415      MOVE '5'                    TO  DC-OPTION-CODE.              EL6506
00416      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL6506
00417                                                                   EL6506
00418      EJECT                                                           CL*20
00419      EXEC CICS LINK                                               EL6506
00420          PROGRAM (PGM-NAME)                                          CL*13
00421          COMMAREA(DATE-CONVERSION-DATA)                           EL6506
00422          LENGTH  (DC-COMM-LENGTH)                                    CL*13
00423      END-EXEC.                                                    EL6506
00424                                                                   EL6506
00425      MOVE DC-BIN-DATE-1          TO  AM-LAST-MAINT-DT             EL6506
00426                                      BIN-CURRENT-SAVE.            EL6506
00427      MOVE 'B'                    TO  JP-RECORD-TYPE               EL6506
00428      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6506
00429      PERFORM 8400-LOG-JOURNAL-RECORD.                                CL*12
00430      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.              EL6506
00431                                                                   EL6506
00432      EXEC CICS REWRITE                                            EL6506
00433          DATASET  (ERACCT-FILE)                                   EL6506
00434          FROM     (ACCOUNT-MASTER)                                EL6506
00435      END-EXEC.                                                    EL6506
00436                                                                   EL6506
00437      MOVE 'C'                    TO  JP-RECORD-TYPE               EL6506
00438      MOVE ERACCT-FILE            TO  FILE-ID.                     EL6506
00439      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6506
00440      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6506
00441      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6506
00442      MOVE ER-0000                TO  EMI-ERROR.                   EL6506
00443      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6506
00444                                                                   EL6506
00445      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6506
00446      MOVE LOW-VALUES             TO  EL6506AO.                    EL6506
00447      MOVE 'C'                    TO  PI-MAINT.                    EL6506
00448                                                                   EL6506
00449      EJECT                                                        EL6506
00450                                                                   EL6506
00451  5000-BUILD-INITIAL-SCREEN.                                       EL6506
00452                                                                   EL6506
00453      MOVE AM-CAL-TABLE           TO TABLEO.                       EL6506
00454      MOVE AL-UANON               TO TABLEA.                       EL6506
00455      MOVE -1                     TO TABLEL.                       EL6506
00456                                                                   EL6506
00457      IF AM-LF-OB-RATE NUMERIC                                     EL6506
00458         MOVE AM-LF-OB-RATE       TO OBLFRTO.                      EL6506
00459      MOVE AM-LF-DEVIATION        TO DEVCDLFO.                     EL6506
00460      MOVE AL-UANON               TO DEVCDLFA.                     EL6506
00461      MOVE -1                     TO DEVCDLFL.                     EL6506
00462      MOVE AM-LF-DEVIATION-PCT    TO DEVPCTLO.                     EL6506
00463                                                                   EL6506
00464      IF AM-LF-OB-RATE-JNT NUMERIC                                 EL6506
00465         MOVE AM-LF-OB-RATE-JNT   TO OBJLFRTO.                     EL6506
00466      MOVE AM-AH-DEVIATION        TO DEVCDAHO.                     EL6506
00467      MOVE AL-UANON               TO DEVCDAHA.                     EL6506
00468      MOVE -1                     TO DEVCDAHL.                     EL6506
00469      MOVE AM-AH-DEVIATION-PCT    TO DEVPCTAO.                     EL6506
00470                                                                   EL6506
00471      IF AM-AH-OB-RATE NUMERIC                                     EL6506
00472         MOVE AM-AH-OB-RATE       TO OBAHRTO.                      EL6506
00473                                                                   EL6506
00474      IF AM-OB-PAYMENT-MODE = ' ' OR 'M' OR 'Q' OR 'S' OR 'A'         CL*21
00475         MOVE AM-OB-PAYMENT-MODE  TO OBMODEO                          CL*21
00476      ELSE                                                            CL*21
00477         MOVE SPACE               TO OBMODEO.                         CL*21
00478                                                                      CL*21
00479      MOVE AM-TOL-PREM            TO ISSTOLO.                      EL6506
00480      MOVE AM-TOL-REF             TO REFTOLO.                      EL6506
00481      MOVE AM-TOL-CLM             TO CLAMTOLO.                     EL6506
00482                                                                   EL6506
00483      IF AM-TOL-REF-PCT NUMERIC                                       CL*28
00484         MOVE AM-TOL-REF-PCT TO REFPCTO.                              CL*28
00485                                                                      CL*28
00486      IF AM-OVR-SHT-AMT NUMERIC                                       CL*28
00487          MOVE AM-OVR-SHT-AMT TO OVSAMTO                              CL*28
00488      END-IF.                                                         CL*28
00489                                                                      CL*28
00490      IF AM-OVR-SHT-PCT NUMERIC                                       CL*28
00491         MOVE AM-OVR-SHT-PCT TO OVSPCTO                               CL*28
00492      END-IF.                                                         CL*28
00493                                                                      CL**2
CIDMOD     IF AM-LF-RPT021-EXP-PCT NUMERIC                                   000
CIDMOD        MOVE AM-LF-RPT021-EXP-PCT TO LFEXP21O.                         000
CIDMOD                                                                  EL6506
00494      IF AM-AH-RPT021-EXP-PCT NUMERIC                                 CL**2
00495         MOVE AM-AH-RPT021-EXP-PCT TO AHEXP21O.                       CL*13
00496                                                                   EL6506
00497      MOVE AM-RPT045A-SWITCH      TO ECS45SWO.                        CL**3
00498                                                                      CL*24
00499      IF AM-MAX-MON-BEN NUMERIC                                       CL**5
00500          MOVE AM-MAX-MON-BEN     TO MAXMONBO
           END-IF
00501 *        MOVE WS-MAX-MON-BEN     TO MAXMONBO.                        CL**5
00502                                                                      CL**5
00503      IF AM-MAX-TOT-BEN NUMERIC                                       CL**5
00504          MOVE AM-MAX-TOT-BEN     TO MAXTOTBO
           END-IF
00506                                                                      CL**5
00507      MOVE AM-EARN-METHOD-R       TO EMREDO.                       EL6506
00508      MOVE AM-EARN-METHOD-L       TO EMLEVO.                       EL6506
00509      MOVE AM-EARN-METHOD-A       TO EMAHO.                        EL6506
00510                                                                   EL6506
00511      MOVE PI-MAINT               TO  MAINTYPO.                    EL6506
00512      MOVE -1                     TO  MAINTYPL.                    EL6506
00513      MOVE AL-UANON               TO  MAINTYPA.                    EL6506
00514                                                                   EL6506
00515      IF PI-COMPANY-ID EQUAL 'FLC' OR 'LGX'                           CL*20
00516          NEXT SENTENCE                                               CL*20
00517      ELSE                                                            CL*20
CIDMOD         MOVE AL-SANOF           TO FLC-ACCOUNT-ATTRB                  000
CIDMOD                                    HEADAA                           CL*20
00519                                     HEADB1A                          CL*20
00520                                     HEADB2A                          CL*20
00521                                     RNGBEG1A                         CL*20
00522                                     RNGBEG2A                         CL*20
00523                                     RNGBEG3A                         CL*20
00524                                     RNGBEG4A                         CL*20
00525                                     RNGBEG5A                         CL*20
00526                                     RNGEND1A                         CL*20
00527                                     RNGEND2A                         CL*20
00528                                     RNGEND3A                         CL*20
00529                                     RNGEND4A                         CL*20
00530                                     RNGEND5A                         CL*20
00531                                     ALLAMT1A                         CL*20
00532                                     ALLAMT2A                         CL*20
00533                                     ALLAMT3A                         CL*20
00534                                     ALLAMT4A                         CL*20
00535                                     ALLAMT5A                         CL*20
00536          GO TO 5098-CONTINUE-BUILD.                                  CL*20
00537                                                                      CL*20
00538      MOVE '--- ACCOUNT ALLOWANCE ---'                                CL*20
00539                                 TO HEADAO.                           CL*20
00540      MOVE '- NET PREMIUM RANGE -' TO HEADB1O.                        CL*20
00541      MOVE ' ALLOWANCE'           TO HEADB2O.                         CL*20
00542      MOVE '='                    TO ALLEQU1O                         CL*20
00543                                     ALLEQU2O                         CL*20
00544                                     ALLEQU3O                         CL*20
00545                                     ALLEQU4O                         CL*20
00546                                     ALLEQU5O.                        CL*20
00547                                                                      CL*20
00548      MOVE '-'                    TO ALLDES1O                         CL*20
00549                                     ALLDES2O                         CL*20
00550                                     ALLDES3O                         CL*20
00551                                     ALLDES4O                         CL*20
00552                                     ALLDES5O.                        CL*20
00553                                                                      CL*20
00554      IF  AM-ALLOW-BEGIN-RANGE (1) NUMERIC                            CL*20
00555        AND (AM-ALLOW-BEGIN-RANGE (1) GREATER  ZERO                   CL*20
00556        OR  AM-ALLOW-END-RANGE (1) GREATER  ZERO)                     CL*20
00557          MOVE AM-ALLOW-BEGIN-RANGE (1)  TO RNGBEG1O.                 CL*20
00558                                                                      CL*20
00559      IF (AM-ALLOW-END-RANGE (1) NUMERIC                              CL*20
00560         AND AM-ALLOW-END-RANGE (1) GREATER ZERO)                     CL*20
00561          MOVE AM-ALLOW-END-RANGE (1)  TO RNGEND1O.                   CL*20
00562                                                                      CL*20
00563      IF (AM-ALLOW-BEGIN-RANGE (2) NUMERIC                            CL*20
00564        AND AM-ALLOW-BEGIN-RANGE (2) GREATER ZERO)                    CL*20
00565          MOVE AM-ALLOW-BEGIN-RANGE (2)  TO RNGBEG2O.                 CL*20
00566                                                                      CL*20
00567      IF (AM-ALLOW-END-RANGE (2) NUMERIC                              CL*20
00568        AND AM-ALLOW-END-RANGE (2) GREATER ZERO)                      CL*20
00569          MOVE AM-ALLOW-END-RANGE (2)  TO RNGEND2O.                   CL*20
00570                                                                      CL*20
00571      IF (AM-ALLOW-BEGIN-RANGE (3) NUMERIC                            CL*20
00572        AND AM-ALLOW-BEGIN-RANGE (3) GREATER ZERO)                    CL*20
00573          MOVE AM-ALLOW-BEGIN-RANGE (3)  TO RNGBEG3O.                 CL*20
00574                                                                      CL*20
00575      IF (AM-ALLOW-END-RANGE (3) NUMERIC                              CL*20
00576        AND AM-ALLOW-END-RANGE (3) GREATER ZERO)                      CL*20
00577          MOVE AM-ALLOW-END-RANGE (3)  TO RNGEND3O.                   CL*20
00578                                                                      CL*20
00579      IF (AM-ALLOW-BEGIN-RANGE (4) NUMERIC                            CL*20
00580        AND AM-ALLOW-BEGIN-RANGE (4) GREATER  ZERO)                   CL*20
00581          MOVE AM-ALLOW-BEGIN-RANGE (4)  TO RNGBEG4O.                 CL*20
00582                                                                      CL*20
00583      IF (AM-ALLOW-END-RANGE (4) NUMERIC                              CL*20
00584        AND AM-ALLOW-END-RANGE (4) GREATER ZERO)                      CL*20
00585          MOVE AM-ALLOW-END-RANGE (4)  TO RNGEND4O.                   CL*20
00586                                                                      CL*20
00587      IF (AM-ALLOW-BEGIN-RANGE (5) NUMERIC                            CL*20
00588        AND AM-ALLOW-BEGIN-RANGE (5) GREATER  ZERO)                   CL*20
00589          MOVE AM-ALLOW-BEGIN-RANGE (5)  TO RNGBEG5O.                 CL*20
00590                                                                      CL*20
00591      IF (AM-ALLOW-END-RANGE (5) NUMERIC                              CL*20
00592        AND AM-ALLOW-END-RANGE (5) GREATER ZERO)                      CL*20
00593          MOVE AM-ALLOW-END-RANGE (5)  TO RNGEND5O.                   CL*20
00594                                                                      CL*20
00595      IF (AM-ALLOWANCE-AMT (1) NUMERIC                                CL*20
00596         AND AM-ALLOWANCE-AMT (1) GREATER ZERO)                       CL*20
00597          MOVE AM-ALLOWANCE-AMT (1)  TO ALLAMT1O.                     CL*20
00598                                                                      CL*20
00599      IF (AM-ALLOWANCE-AMT (2) NUMERIC                                CL*20
00600         AND AM-ALLOWANCE-AMT (2) GREATER ZERO)                       CL*20
00601          MOVE AM-ALLOWANCE-AMT (2)  TO ALLAMT2O.                     CL*20
00602                                                                      CL*20
00603      IF (AM-ALLOWANCE-AMT (3) NUMERIC                                CL*20
00604         AND AM-ALLOWANCE-AMT (3) GREATER ZERO)                       CL*20
00605          MOVE AM-ALLOWANCE-AMT (3)  TO ALLAMT3O.                     CL*20
00606                                                                      CL*20
00607      IF (AM-ALLOWANCE-AMT (4) NUMERIC                                CL*20
00608         AND AM-ALLOWANCE-AMT (4) GREATER ZERO)                       CL*20
00609          MOVE AM-ALLOWANCE-AMT (4)  TO ALLAMT4O.                     CL*20
00610                                                                      CL*20
00611      IF (AM-ALLOWANCE-AMT (5) NUMERIC                                CL*20
00612         AND AM-ALLOWANCE-AMT (5) GREATER ZERO)                       CL*20
00613          MOVE AM-ALLOWANCE-AMT (5)  TO ALLAMT5O.                     CL*20
00614                                                                      CL*20
00615                                                                      CL*20
00616  5098-CONTINUE-BUILD.                                                CL*20
00617                                                                      CL*20
00618      GO TO 8100-SEND-INITIAL-MAP.                                 EL6506
00619                                                                   EL6506
00620  5099-EXIT.                                                       EL6506
00621      EXIT.                                                        EL6506
00622      EJECT                                                        EL6506
00623  6000-CHECK-FOR-UPDATE.                                           EL6506
00624                                                                   EL6506
00625      IF TABLEL GREATER ZEROS                                      EL6506
00626         MOVE TABLEI              TO AM-CAL-TABLE.                 EL6506
00627                                                                   EL6506
00628      IF OBLFRTL GREATER ZEROS                                     EL6506
00629         MOVE WS-OB-LF-RATE       TO AM-LF-OB-RATE.                EL6506
00630                                                                      CL*18
00631      IF DEVCDLFL GREATER ZEROS                                    EL6506
00632         MOVE DEVCDLFI            TO AM-LF-DEVIATION.              EL6506
00633                                                                      CL*18
00634      IF DEVPCTLL GREATER ZEROS                                    EL6506
00635         MOVE WS-LF-DEV-PERCENT   TO AM-LF-DEVIATION-PCT.          EL6506
00636                                                                   EL6506
00637      IF OBJLFRTL GREATER ZEROS                                    EL6506
00638         MOVE WS-OB-JNTLF-RATE    TO AM-LF-OB-RATE-JNT.            EL6506
00639                                                                      CL*13
00640      IF DEVCDAHL GREATER ZEROS                                    EL6506
00641         MOVE DEVCDAHI            TO AM-AH-DEVIATION.              EL6506
00642                                                                      CL*18
00643      IF DEVPCTAL GREATER ZEROS                                    EL6506
00644         MOVE WS-AH-DEV-PERCENT   TO AM-AH-DEVIATION-PCT.          EL6506
00645                                                                   EL6506
00646      IF OBAHRTL GREATER ZEROS                                     EL6506
00647         MOVE WS-OB-AH-RATE       TO AM-AH-OB-RATE.                EL6506
00648                                                                   EL6506
00649      IF OBMODEL GREATER ZEROS                                        CL*21
00650         MOVE OBMODEI             TO AM-OB-PAYMENT-MODE.              CL*21
00651                                                                      CL*21
00652      IF ISSTOLL GREATER ZEROS                                     EL6506
00653         MOVE WS-TOL-PREM         TO AM-TOL-PREM.                  EL6506
00654                                                                   EL6506
00655      IF OVSAMTL > +0                                                 CL*28
00656          MOVE  WS-OVR-SHT-AMT TO AM-OVR-SHT-AMT                      CL*28
00657      END-IF.                                                         CL*28
00658                                                                      CL*28
00659      IF OVSPCTL > +0                                                 CL*28
00660          MOVE WS-OVR-SHT-PCT      TO AM-OVR-SHT-PCT                  CL*28
00661      END-IF.                                                         CL*28
00662                                                                      CL*28
00663      IF EMREDL GREATER ZEROS                                      EL6506
00664         MOVE EMREDI              TO AM-EARN-METHOD-R.             EL6506
00665                                                                   EL6506
00666      IF REFTOLL GREATER ZEROS                                     EL6506
00667         MOVE WS-TOL-REF          TO AM-TOL-REF.                   EL6506
00668                                                                      CL*28
00669      IF REFPCTL GREATER ZEROS                                        CL*28
00670         MOVE WS-TOL-REF-PCT      TO AM-TOL-REF-PCT.                  CL*28
00671                                                                      CL*13
00672      IF EMLEVL GREATER ZEROS                                      EL6506
00673         MOVE EMLEVI              TO AM-EARN-METHOD-L.             EL6506
00674                                                                   EL6506
00675      IF CLAMTOLL GREATER ZEROS                                    EL6506
00676         MOVE WS-TOL-CLM          TO AM-TOL-CLM.                   EL6506
00677                                                                   EL6506
00678      IF LFEXP21L GREATER ZEROS                                       CL**2
00679         MOVE WS-LF-EXP-PERCENT   TO AM-LF-RPT021-EXP-PCT.            CL**2
00680                                                                      CL**2
00681      IF AHEXP21L GREATER ZEROS                                       CL**2
00682         MOVE WS-AH-EXP-PERCENT   TO AM-AH-RPT021-EXP-PCT.            CL**2
00683                                                                      CL**3
00684      IF ECS45SWL GREATER ZEROS                                       CL**3
00685         MOVE ECS45SWI            TO AM-RPT045A-SWITCH.               CL**3
00686                                                                      CL**5
00687      IF MAXMONBL GREATER ZEROS                                       CL**5
00688         MOVE SV-MAX-MON-BEN      TO AM-MAX-MON-BEN.                  CL**5
00689                                                                      CL*18
00690      IF MAXTOTBL GREATER ZEROS                                       CL**5
00691         MOVE SV-MAX-TOT-BEN      TO AM-MAX-TOT-BEN.                  CL**5
00692                                                                   EL6506
00693      IF EMAHL GREATER ZEROS                                       EL6506
00694         MOVE EMAHI               TO AM-EARN-METHOD-A.             EL6506
00695                                                                   EL6506
00696      IF AM-ALLOWANCE-AMT (1) NOT NUMERIC                             CL*20
00697         MOVE ZEROS               TO AM-ALLOWANCE-AMT (1).            CL*20
00698                                                                      CL*20
00699      IF AM-ALLOWANCE-AMT (2) NOT NUMERIC                             CL*20
00700         MOVE ZEROS               TO AM-ALLOWANCE-AMT (2).            CL*20
00701                                                                      CL*20
00702      IF AM-ALLOWANCE-AMT (3) NOT NUMERIC                             CL*20
00703         MOVE ZEROS               TO AM-ALLOWANCE-AMT (3).            CL*20
00704                                                                      CL*20
00705      IF AM-ALLOWANCE-AMT (4) NOT NUMERIC                             CL*20
00706         MOVE ZEROS               TO AM-ALLOWANCE-AMT (4).            CL*20
00707                                                                      CL*20
00708      IF AM-ALLOWANCE-AMT (5) NOT NUMERIC                             CL*20
00709         MOVE ZEROS               TO AM-ALLOWANCE-AMT (5).            CL*20
00710                                                                      CL*20
00711      IF PI-COMPANY-ID EQUAL 'FLC' OR 'LGX'                           CL*20
00712         NEXT SENTENCE                                                CL*20
00713      ELSE                                                            CL*20
00714         GO TO 6049-EXIT.                                             CL*20
00715                                                                      CL*20
00716      IF RNGBEG1L GREATER ZEROS                                       CL*20
00717         MOVE WS-BEGIN-RANGE (1)    TO AM-ALLOW-BEGIN-RANGE (1).      CL*20
00718                                                                      CL*20
00719      IF RNGBEG2L GREATER ZEROS                                       CL*20
00720         MOVE WS-BEGIN-RANGE (2)    TO AM-ALLOW-BEGIN-RANGE (2).      CL*20
00721                                                                      CL*20
00722      IF RNGBEG3L GREATER ZEROS                                       CL*20
00723         MOVE WS-BEGIN-RANGE (3)    TO AM-ALLOW-BEGIN-RANGE (3).      CL*20
00724                                                                      CL*20
00725      IF RNGBEG4L GREATER ZEROS                                       CL*20
00726         MOVE WS-BEGIN-RANGE (4)    TO AM-ALLOW-BEGIN-RANGE (4).      CL*20
00727                                                                      CL*20
00728      IF RNGBEG5L GREATER ZEROS                                       CL*20
00729         MOVE WS-BEGIN-RANGE (5)    TO AM-ALLOW-BEGIN-RANGE (5).      CL*20
00730                                                                      CL*20
00731      IF RNGEND1L GREATER ZEROS                                       CL*20
00732         MOVE WS-END-RANGE (1)      TO AM-ALLOW-END-RANGE (1).        CL*20
00733                                                                      CL*20
00734      IF RNGEND2L GREATER ZEROS                                       CL*20
00735         MOVE WS-END-RANGE (2)      TO AM-ALLOW-END-RANGE (2).        CL*20
00736                                                                      CL*20
00737      IF RNGEND3L GREATER ZEROS                                       CL*20
00738         MOVE WS-END-RANGE (3)      TO AM-ALLOW-END-RANGE (3).        CL*20
00739                                                                      CL*20
00740      IF RNGEND4L GREATER ZEROS                                       CL*20
00741         MOVE WS-END-RANGE (4)      TO AM-ALLOW-END-RANGE (4).        CL*20
00742                                                                      CL*20
00743      IF RNGEND5L GREATER ZEROS                                       CL*20
00744         MOVE WS-END-RANGE (5)      TO AM-ALLOW-END-RANGE (5).        CL*20
00745                                                                      CL*20
00746      IF ALLAMT1L GREATER ZEROS                                       CL*20
00747         MOVE WS-ACCOUNT-ALLOWANCE (1) TO AM-ALLOWANCE-AMT (1).       CL*20
00748                                                                      CL*20
00749      IF ALLAMT2L GREATER ZEROS                                       CL*20
00750         MOVE WS-ACCOUNT-ALLOWANCE (2) TO AM-ALLOWANCE-AMT (2).       CL*20
00751                                                                      CL*20
00752      IF ALLAMT3L GREATER ZEROS                                       CL*20
00753         MOVE WS-ACCOUNT-ALLOWANCE (3) TO AM-ALLOWANCE-AMT (3).       CL*20
00754                                                                      CL*20
00755      IF ALLAMT4L GREATER ZEROS                                       CL*20
00756         MOVE WS-ACCOUNT-ALLOWANCE (4) TO AM-ALLOWANCE-AMT (4).       CL*20
00757                                                                      CL*20
00758      IF ALLAMT5L GREATER ZEROS                                       CL*20
00759         MOVE WS-ACCOUNT-ALLOWANCE (5) TO AM-ALLOWANCE-AMT (5).       CL*20
00760                                                                      CL*20
00761  6049-EXIT.                                                       EL6506
00762      EXIT.                                                        EL6506
00763      EJECT                                                        EL6506
00764                                                                   EL6506
00765  7000-EDIT.                                                       EL6506
00766                                                                      CL**7
00767      IF OBLFRTL GREATER ZEROS                                     EL6506
00768          MOVE OBLFRTI                TO DEEDIT-FIELD-V5           EL6506
00769          PERFORM 8600-DEEDIT                                      EL6506
00770          IF DEEDIT-FIELD-V5 NUMERIC                               EL6506
00771              MOVE DEEDIT-FIELD-V5        TO WS-OB-LF-RATE         EL6506
00772                                             OBLFRTO               EL6506
00773          ELSE                                                     EL6506
00774              MOVE -1                     TO OBLFRTL               EL6506
00775              MOVE AL-UABON               TO OBLFRTA               EL6506
00776              MOVE ER-3125                TO EMI-ERROR             EL6506
00777              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
00778                                                                   EL6506
00779      IF OBJLFRTL GREATER ZEROS                                    EL6506
00780          MOVE OBJLFRTI                TO DEEDIT-FIELD-V5          EL6506
00781          PERFORM 8600-DEEDIT                                      EL6506
00782          IF DEEDIT-FIELD-V5 NUMERIC                               EL6506
00783              MOVE DEEDIT-FIELD-V5        TO WS-OB-JNTLF-RATE      EL6506
00784                                             OBJLFRTO              EL6506
00785          ELSE                                                     EL6506
00786              MOVE -1                     TO OBJLFRTL              EL6506
00787              MOVE AL-UABON               TO OBJLFRTA              EL6506
00788              MOVE ER-3125                TO EMI-ERROR             EL6506
00789              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
00790                                                                   EL6506
00791      IF OBAHRTL GREATER ZEROS                                     EL6506
00792          MOVE OBAHRTI                TO DEEDIT-FIELD-V5           EL6506
00793          PERFORM 8600-DEEDIT                                      EL6506
00794          IF DEEDIT-FIELD-V5 NUMERIC                               EL6506
00795              MOVE DEEDIT-FIELD-V5        TO WS-OB-AH-RATE         EL6506
00796                                             OBAHRTO               EL6506
00797          ELSE                                                     EL6506
00798              MOVE -1                     TO OBAHRTL               EL6506
00799              MOVE AL-UABON               TO OBAHRTA               EL6506
00800              MOVE ER-3125                TO EMI-ERROR             EL6506
00801              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*21
00802                                                                      CL*21
00803      IF OBMODEL GREATER ZEROS                                        CL*21
00804          MOVE OBMODEI                    TO WS-OB-MODE-CODE          CL*21
00805          IF NOT VALID-OB-MODE                                        CL*21
00806              MOVE -1                     TO OBMODEL                  CL*21
00807              MOVE AL-UABON               TO OBMODEA                  CL*21
00808              MOVE ER-2591                TO EMI-ERROR                CL*21
00809              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
00810                                                                   EL6506
00811  EJECT                                                               CL*20
00812  7020-CHECK-LIFE-DEVIATION.                                       EL6506
00813                                                                      CL*18
00814      IF DEVPCTLL GREATER ZEROS                                    EL6506
00815          MOVE DEVPCTLI               TO DEEDIT-FIELD-V6           EL6506
00816          PERFORM 8600-DEEDIT                                      EL6506
00817          IF DEEDIT-FIELD-V6 NUMERIC                               EL6506
00818              MOVE DEEDIT-FIELD-V6        TO WS-LF-DEV-PERCENT     EL6506
00819                                             DEVPCTLO              EL6506
00820          ELSE                                                     EL6506
00821              MOVE -1                     TO DEVPCTLL              EL6506
00822              MOVE AL-UABON               TO DEVPCTLA              EL6506
00823              MOVE ER-3126                TO EMI-ERROR             EL6506
00824              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
00825                                                                   EL6506
00826      IF TABLEL GREATER ZERO                                       EL6506
00827          NEXT SENTENCE                                            EL6506
00828      ELSE                                                         EL6506
00829          IF DEVCDLFL NOT GREATER ZEROS OR                         EL6506
00830             DEVCDLFI = SPACES                                     EL6506
00831                GO TO 7030-CHECK-AH-DEVIATION.                     EL6506
00832                                                                   EL6506
00833      MOVE LOW-VALUES             TO RATE-KEY.                     EL6506
00834                                                                   EL6506
00835      MOVE PI-COMPANY-CD          TO RATE-COMP-CD.                 EL6506
00836      MOVE PI-ACCT-STATE          TO RATE-STATE.                   EL6506
00837      IF TABLEL NOT GREATER ZERO                                   EL6506
00838          IF MAINTYPI = 'A'                                        EL6506
00839              MOVE ZEROS          TO TABLEI                        EL6506
00840              MOVE 2              TO TABLEL.                       EL6506
00841      MOVE TABLEI                 TO RATE-CLASS.                   EL6506
00842      MOVE DEVCDLFI               TO RATE-DEV.                     EL6506
00843                                                                   EL6506
00844      EXEC CICS HANDLE CONDITION                                   EL6506
00845          NOTFND   (7029-LIFE-DEV-ERROR)                           EL6506
00846      END-EXEC.                                                    EL6506
00847                                                                   EL6506
00848      EXEC CICS READ                                               EL6506
00849          GTEQ                                                     EL6506
00850          DATASET   (ERRATE-FILE)                                  EL6506
00851          SET       (ADDRESS OF RATE-RECORD)                          CL*26
00852          RIDFLD    (RATE-KEY)                                     EL6506
00853      END-EXEC.                                                    EL6506
00854                                                                   EL6506
00855      CONTINUE.                                                       CL*26
00856                                                                   EL6506
00857      IF RT-COMPANY-CD = RATE-COMP-CD AND                             CL*12
00858         RT-ST-CODE    = RATE-STATE   AND                             CL*12
00859         RT-ST-CLASS   = RATE-CLASS   AND                             CL*12
00860         RT-ST-DEV     = RATE-DEV                                     CL*12
00861            GO TO 7030-CHECK-AH-DEVIATION.                            CL*12
00862                                                                   EL6506
00863  7029-LIFE-DEV-ERROR.                                             EL6506
00864      MOVE -1                     TO DEVCDLFL                      EL6506
00865                                     TABLEL.                       EL6506
00866      MOVE AL-UABON               TO DEVCDLFA                      EL6506
00867                                     TABLEA.                       EL6506
00868      MOVE ER-2154                TO EMI-ERROR.                    EL6506
00869      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6506
00870                                                                   EL6506
00871  EJECT                                                               CL*20
00872  7030-CHECK-AH-DEVIATION.                                         EL6506
00873                                                                      CL*18
00874      IF DEVPCTAL GREATER ZEROS                                    EL6506
00875          MOVE DEVPCTAI               TO DEEDIT-FIELD-V6           EL6506
00876          PERFORM 8600-DEEDIT                                      EL6506
00877          IF DEEDIT-FIELD-V6 NUMERIC                               EL6506
00878              MOVE DEEDIT-FIELD-V6        TO WS-AH-DEV-PERCENT     EL6506
00879                                             DEVPCTAO              EL6506
00880          ELSE                                                     EL6506
00881              MOVE -1                     TO DEVPCTAL              EL6506
00882              MOVE AL-UABON               TO DEVPCTAA              EL6506
00883              MOVE ER-3126                TO EMI-ERROR             EL6506
00884              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
00885                                                                   EL6506
00886      IF TABLEL GREATER ZERO                                       EL6506
00887          NEXT SENTENCE                                            EL6506
00888      ELSE                                                         EL6506
00889          IF DEVCDAHL NOT GREATER ZEROS OR                         EL6506
00890             DEVCDAHI = SPACES                                     EL6506
00891                GO TO 7040-CHECK-TOLERANCES.                       EL6506
00892                                                                   EL6506
00893      MOVE LOW-VALUES             TO RATE-KEY.                     EL6506
00894                                                                   EL6506
00895      MOVE PI-COMPANY-CD          TO RATE-COMP-CD.                 EL6506
00896      MOVE PI-ACCT-STATE          TO RATE-STATE.                   EL6506
00897      IF TABLEL NOT GREATER ZERO                                   EL6506
00898          IF MAINTYPI = 'A'                                        EL6506
00899              MOVE ZEROS          TO TABLEI                        EL6506
00900              MOVE 2              TO TABLEL.                       EL6506
00901      MOVE TABLEI                 TO RATE-CLASS.                   EL6506
00902      MOVE DEVCDAHI               TO RATE-DEV.                     EL6506
00903                                                                   EL6506
00904      EXEC CICS HANDLE CONDITION                                   EL6506
00905          NOTFND   (7039-AH-DEV-ERROR)                             EL6506
00906      END-EXEC.                                                    EL6506
00907                                                                   EL6506
00908      EXEC CICS READ                                               EL6506
00909          GTEQ                                                     EL6506
00910          DATASET   (ERRATE-FILE)                                  EL6506
00911          SET       (ADDRESS OF RATE-RECORD)                          CL*26
00912          RIDFLD    (RATE-KEY)                                     EL6506
00913      END-EXEC.                                                    EL6506
00914                                                                   EL6506
00915      CONTINUE.                                                       CL*26
00916                                                                   EL6506
00917      IF RT-COMPANY-CD = RATE-COMP-CD AND                             CL*12
00918         RT-ST-CODE    = RATE-STATE   AND                             CL*12
00919         RT-ST-CLASS   = RATE-CLASS   AND                             CL*12
00920         RT-ST-DEV     = RATE-DEV                                     CL*12
00921            GO TO 7040-CHECK-TOLERANCES.                              CL*12
00922                                                                   EL6506
00923  7039-AH-DEV-ERROR.                                               EL6506
00924                                                                      CL*18
00925      MOVE -1                     TO DEVCDAHL                      EL6506
00926                                     TABLEL.                       EL6506
00927      MOVE AL-UABON               TO DEVCDAHA                      EL6506
00928                                     TABLEA.                       EL6506
00929      MOVE ER-2154                TO EMI-ERROR.                    EL6506
00930      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6506
00931  EJECT                                                               CL*20
00932                                                                   EL6506
00933  7040-CHECK-TOLERANCES.                                           EL6506
00934                                                                      CL*18
00935      IF ISSTOLL GREATER ZEROS                                     EL6506
00936          MOVE ISSTOLI                TO DEEDIT-FIELD-V1           EL6506
00937          PERFORM 8600-DEEDIT                                      EL6506
00938          IF DEEDIT-FIELD-V1 NUMERIC                               EL6506
00939              MOVE DEEDIT-FIELD-V1        TO WS-TOL-PREM           EL6506
00940                                             ISSTOLO               EL6506
00941          ELSE                                                     EL6506
00942              MOVE -1                     TO ISSTOLL               EL6506
00943              MOVE AL-UABON               TO ISSTOLA               EL6506
00944              MOVE ER-2168                TO EMI-ERROR             EL6506
00945              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
00946                                                                   EL6506
00947      IF OVSAMTL GREATER ZEROS                                        CL*28
00948          MOVE OVSAMTI                TO DEEDIT-FIELD-V1              CL*28
00949          PERFORM 8600-DEEDIT                                         CL*28
00950          IF DEEDIT-FIELD-V1 NUMERIC                                  CL*28
00951              MOVE DEEDIT-FIELD-V1        TO WS-OVR-SHT-AMT           CL*28
00952                                             OVSAMTO                  CL*28
00953          ELSE                                                        CL*28
00954              MOVE -1                     TO OVSAMTL                  CL*28
00955              MOVE AL-UABON               TO OVSAMTA                  CL*28
00956              MOVE ER-2168                TO EMI-ERROR                CL*28
00957              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*28
00958                                                                      CL*28
00959      IF OVSPCTL GREATER ZEROS                                        CL*28
00960          MOVE OVSPCTI                TO DEEDIT-FIELD-V4              CL*28
00961          PERFORM 8600-DEEDIT                                         CL*28
00962          IF DEEDIT-FIELD-V4 NUMERIC                                  CL*28
00963              MOVE DEEDIT-FIELD-V4        TO WS-OVR-SHT-PCT           CL*28
00964                                             OVSPCTO                  CL*28
00965          ELSE                                                        CL*28
00966              MOVE -1                     TO OVSPCTL                  CL*28
00967              MOVE AL-UABON               TO OVSPCTA                  CL*28
00968              MOVE ER-2169                TO EMI-ERROR                CL*28
00969              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*28
00970                                                                      CL*28
00971      IF REFTOLL GREATER ZEROS                                     EL6506
00972          MOVE REFTOLI                TO DEEDIT-FIELD-V1           EL6506
00973          PERFORM 8600-DEEDIT                                      EL6506
00974          IF DEEDIT-FIELD-V1 NUMERIC                               EL6506
00975              MOVE DEEDIT-FIELD-V1        TO WS-TOL-REF            EL6506
00976                                             REFTOLO               EL6506
00977          ELSE                                                     EL6506
00978              MOVE -1                     TO REFTOLL               EL6506
00979              MOVE AL-UABON               TO REFTOLA               EL6506
00980              MOVE ER-2169                TO EMI-ERROR                CL*28
00981              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*28
00982                                                                      CL*28
00983      IF REFPCTL GREATER ZEROS                                        CL*28
00984          MOVE REFPCTI                TO DEEDIT-FIELD-V4              CL*28
00985          PERFORM 8600-DEEDIT                                         CL*28
00986          IF DEEDIT-FIELD-V4 NUMERIC                                  CL*28
00987              MOVE DEEDIT-FIELD-V4        TO WS-TOL-REF-PCT           CL*28
00988                                             REFPCTO                  CL*28
00989          ELSE                                                        CL*28
00990              MOVE -1                     TO REFPCTL                  CL*28
00991              MOVE AL-UABON               TO REFPCTA                  CL*28
00992              MOVE ER-2169                TO EMI-ERROR             EL6506
00993              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
00994                                                                   EL6506
00995      IF CLAMTOLL GREATER ZEROS                                    EL6506
00996          MOVE CLAMTOLI               TO DEEDIT-FIELD-V1           EL6506
00997          PERFORM 8600-DEEDIT                                      EL6506
00998          IF DEEDIT-FIELD-V1 NUMERIC                               EL6506
00999              MOVE DEEDIT-FIELD-V1        TO WS-TOL-CLM            EL6506
01000                                             CLAMTOLO              EL6506
01001          ELSE                                                     EL6506
01002              MOVE -1                     TO CLAMTOLL              EL6506
01003              MOVE AL-UABON               TO CLAMTOLA              EL6506
01004              MOVE ER-2170                TO EMI-ERROR             EL6506
01005              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
01006                                                                   EL6506
01007  EJECT                                                               CL*20
01008  7045-CHECK-TOLERANCES.                                           EL6506
01009                                                                      CL*18
01010      IF LFEXP21L GREATER ZEROS                                       CL**2
01011          MOVE LFEXP21I                   TO DEEDIT-FIELD-V4          CL**2
01012          PERFORM 8600-DEEDIT                                      EL6506
01013          IF DEEDIT-FIELD-V4 NUMERIC                               EL6506
01014              MOVE DEEDIT-FIELD-V4        TO WS-LF-EXP-PERCENT        CL**2
01015                                             LFEXP21O                 CL**2
01016          ELSE                                                     EL6506
01017              MOVE -1                     TO LFEXP21L                 CL**2
01018              MOVE AL-UABON               TO LFEXP21A                 CL**2
01019              MOVE ER-7531                TO EMI-ERROR                CL**2
01020              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**2
01021                                                                      CL**2
01022      IF AHEXP21L GREATER ZEROS                                       CL**2
01023          MOVE AHEXP21I                   TO DEEDIT-FIELD-V4          CL**2
01024          PERFORM 8600-DEEDIT                                         CL**2
01025          IF DEEDIT-FIELD-V4 NUMERIC                                  CL**2
01026              MOVE DEEDIT-FIELD-V4        TO WS-AH-EXP-PERCENT        CL**2
01027                                             AHEXP21O                 CL**2
01028          ELSE                                                        CL**2
01029              MOVE -1                     TO AHEXP21L                 CL**2
01030              MOVE AL-UABON               TO AHEXP21A                 CL**2
01031              MOVE ER-7531                TO EMI-ERROR             EL6506
01032              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**4
01033                                                                      CL**4
01034      IF ECS45SWL GREATER ZEROS                                       CL**4
01035          IF ECS45SWI = 'N' OR 'Y'                                    CL**4
01036              NEXT SENTENCE                                           CL**4
01037          ELSE                                                        CL**4
01038              MOVE -1                     TO ECS45SWL                 CL**4
01039              MOVE AL-UABON               TO ECS45SWA                 CL*24
CIDMOD             MOVE AL-UABON               TO EMREDA                     000
01040              MOVE ER-7320                TO EMI-ERROR                CL*24
01041              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
01042                                                                   EL6506
01043  EJECT                                                               CL*20
01044  7050-CHECK-REFUND-METHODS.                                       EL6506
01045                                                                      CL*18
01046      IF EMREDL GREATER ZEROS                                      EL6506
01047          IF EMREDI = ' ' OR 'A' OR 'C' OR 'P' OR                     CL*27
01048                      'R' OR 'M' OR 'S'                               CL*27
01049              NEXT SENTENCE                                        EL6506
01050          ELSE                                                     EL6506
01051              MOVE -1                     TO EMREDL                EL6506
01052              MOVE AL-UABON               TO EMREDA                EL6506
01053              MOVE ER-2165                TO EMI-ERROR             EL6506
01054              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
01055                                                                   EL6506
01056      IF EMLEVL GREATER ZEROS                                      EL6506
01057          IF EMLEVI = ' ' OR 'A' OR 'C' OR 'P' OR                     CL*27
01058                      'R' OR 'M' OR 'S'                               CL*27
01059              NEXT SENTENCE                                        EL6506
01060          ELSE                                                     EL6506
01061              MOVE -1                     TO EMLEVL                EL6506
01062              MOVE AL-UABON               TO EMLEVA                EL6506
01063              MOVE ER-2165                TO EMI-ERROR             EL6506
01064              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
01065                                                                   EL6506
01066      IF EMAHL GREATER ZEROS                                       EL6506
01067          IF EMAHI = ' ' OR 'A' OR 'C' OR 'P' OR                      CL*27
01068                     'R' OR 'M' OR 'N' OR 'S'                         CL*27
01069              NEXT SENTENCE                                        EL6506
01070          ELSE                                                     EL6506
01071              MOVE -1                     TO EMAHL                 EL6506
01072              MOVE AL-UABON               TO EMAHA                 EL6506
CIDMOD             MOVE ER-2165                TO EMI-ERROR                  000
CIDMOD*            MOVE ER-3779                TO EMI-ERROR                CL*22
01074              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6506
01075                                                                      CL**5
01076  EJECT                                                               CL*20
01077  7060-CHECK-INS-LIMITS.                                              CL**5
01078                                                                      CL*18
01079      IF MAXMONBL GREATER ZEROS                                       CL**5
01080          MOVE MAXMONBI                   TO DEEDIT-FIELD
01081          PERFORM 8600-DEEDIT                                         CL**5
01082          IF DEEDIT-FIELD-V0 NUMERIC                                  CL**5
01083              MOVE DEEDIT-FIELD-V0        TO SV-MAX-MON-BEN           CL**5
01085              MOVE SV-MAX-MON-BEN         TO MAXMONBO                 CL**5
01086          ELSE                                                        CL**5
01087              MOVE -1                     TO MAXMONBL                 CL**5
01088              MOVE AL-UABON               TO MAXMONBA                 CL**5
01089              MOVE ER-3127                TO EMI-ERROR                CL**5
01090              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**5
01091                                                                      CL**5
01092      IF MAXTOTBL GREATER ZEROS                                       CL**5
01093          MOVE MAXTOTBI                   TO DEEDIT-FIELD-V0          CL**5
01094          PERFORM 8600-DEEDIT                                         CL**5
01095          IF DEEDIT-FIELD-V0 NUMERIC                                  CL**5
01096              MOVE DEEDIT-FIELD-V0        TO SV-MAX-TOT-BEN           CL*26
01098              MOVE SV-MAX-TOT-BEN         TO MAXTOTBO                 CL**5
01099          ELSE                                                        CL**5
01100              MOVE -1                     TO MAXTOTBL                 CL**5
01101              MOVE AL-UABON               TO MAXTOTBA                 CL**5
01102              MOVE ER-3128                TO EMI-ERROR                CL**5
01103              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**5
01104                                                                      CL*20
01105  EJECT                                                               CL*20
01106  7070-CHECK-ALLOWANCE.                                               CL*20
01107                                                                      CL*20
01108      IF PI-COMPANY-ID EQUAL 'FLC' OR 'LGX'                           CL*20
01109          NEXT SENTENCE                                               CL*20
01110      ELSE                                                            CL*20
01111          GO TO 7099-EXIT.                                            CL*20
01112                                                                      CL*20
01113      MOVE ZEROS                     TO WS-ACCOUNT-ALLOWANCE (1)      CL*20
01114                                        WS-ACCOUNT-ALLOWANCE (2)      CL*20
01115                                        WS-ACCOUNT-ALLOWANCE (3)      CL*20
01116                                        WS-ACCOUNT-ALLOWANCE (4)      CL*20
01117                                        WS-ACCOUNT-ALLOWANCE (5)      CL*20
01118                                        WS-BEGIN-RANGE (1)            CL*20
01119                                        WS-BEGIN-RANGE (2)            CL*20
01120                                        WS-BEGIN-RANGE (3)            CL*20
01121                                        WS-BEGIN-RANGE (4)            CL*20
01122                                        WS-BEGIN-RANGE (5)            CL*20
01123                                        WS-END-RANGE (1)              CL*20
01124                                        WS-END-RANGE (2)              CL*20
01125                                        WS-END-RANGE (3)              CL*20
01126                                        WS-END-RANGE (4)              CL*20
01127                                        WS-END-RANGE (5).             CL*20
01128                                                                      CL*20
01129      PERFORM 7100-READ-ERACCT       THRU 7100-EXIT.                  CL*20
01130                                                                      CL*20
01131      IF (AM-ALLOW-BEGIN-RANGE (1) NUMERIC                            CL*20
01132        AND AM-ALLOW-BEGIN-RANGE (1) GREATER  ZERO)                   CL*20
01133          MOVE AM-ALLOW-BEGIN-RANGE (1)  TO WS-BEGIN-RANGE (1).       CL*20
01134                                                                      CL*20
01135      IF (AM-ALLOW-END-RANGE (1) NUMERIC                              CL*20
01136         AND AM-ALLOW-END-RANGE (1) GREATER ZERO)                     CL*20
01137          MOVE AM-ALLOW-END-RANGE (1)  TO WS-END-RANGE (1).           CL*20
01138                                                                      CL*20
01139      IF (AM-ALLOW-BEGIN-RANGE (2) NUMERIC                            CL*20
01140        AND AM-ALLOW-BEGIN-RANGE (2) GREATER ZERO)                    CL*20
01141          MOVE AM-ALLOW-BEGIN-RANGE (2)  TO WS-BEGIN-RANGE (2).       CL*20
01142                                                                      CL*20
01143      IF (AM-ALLOW-END-RANGE (2) NUMERIC                              CL*20
01144        AND AM-ALLOW-END-RANGE (2) GREATER ZERO)                      CL*20
01145          MOVE AM-ALLOW-END-RANGE (2)  TO WS-END-RANGE (2).           CL*20
01146                                                                      CL*20
01147      IF AM-ALLOW-BEGIN-RANGE (3) NUMERIC                             CL*20
01148          MOVE AM-ALLOW-BEGIN-RANGE (3)  TO WS-BEGIN-RANGE (3).       CL*20
01149                                                                      CL*20
01150      IF (AM-ALLOW-END-RANGE (3) NUMERIC                              CL*20
01151        AND AM-ALLOW-END-RANGE (3) GREATER ZERO)                      CL*20
01152          MOVE AM-ALLOW-END-RANGE (3)  TO WS-END-RANGE (3).           CL*20
01153                                                                      CL*20
01154      IF (AM-ALLOW-BEGIN-RANGE (4) NUMERIC                            CL*20
01155        AND AM-ALLOW-BEGIN-RANGE (4) GREATER  ZERO)                   CL*20
01156          MOVE AM-ALLOW-BEGIN-RANGE (4)  TO WS-BEGIN-RANGE (4).       CL*20
01157                                                                      CL*20
01158      IF (AM-ALLOW-END-RANGE (4) NUMERIC                              CL*20
01159        AND AM-ALLOW-END-RANGE (4) GREATER ZERO)                      CL*20
01160          MOVE AM-ALLOW-END-RANGE (4)  TO WS-END-RANGE (4).           CL*20
01161                                                                      CL*20
01162      IF (AM-ALLOW-BEGIN-RANGE (5) NUMERIC                            CL*20
01163        AND AM-ALLOW-BEGIN-RANGE (5) GREATER  ZERO)                   CL*20
01164          MOVE AM-ALLOW-BEGIN-RANGE (5)  TO WS-BEGIN-RANGE (5).       CL*20
01165                                                                      CL*20
01166      IF (AM-ALLOW-END-RANGE (5) NUMERIC                              CL*20
01167        AND AM-ALLOW-END-RANGE (5) GREATER ZERO)                      CL*20
01168          MOVE AM-ALLOW-END-RANGE (5)  TO WS-END-RANGE (5).           CL*20
01169                                                                      CL*20
01170      IF (AM-ALLOWANCE-AMT  (1) NUMERIC                               CL*20
01171        AND AM-ALLOWANCE-AMT  (1) GREATER ZERO)                       CL*20
01172          MOVE AM-ALLOWANCE-AMT (1)  TO WS-ACCOUNT-ALLOWANCE (1).     CL*20
01173                                                                      CL*20
01174      IF (AM-ALLOWANCE-AMT  (2) NUMERIC                               CL*20
01175        AND AM-ALLOWANCE-AMT  (2) GREATER ZERO)                       CL*20
01176          MOVE AM-ALLOWANCE-AMT (2)  TO WS-ACCOUNT-ALLOWANCE (2).     CL*20
01177                                                                      CL*20
01178      IF (AM-ALLOWANCE-AMT  (3) NUMERIC                               CL*20
01179        AND AM-ALLOWANCE-AMT  (3) GREATER ZERO)                       CL*20
01180          MOVE AM-ALLOWANCE-AMT (3)  TO WS-ACCOUNT-ALLOWANCE (3).     CL*20
01181                                                                      CL*20
01182      IF (AM-ALLOWANCE-AMT  (4) NUMERIC                               CL*20
01183        AND AM-ALLOWANCE-AMT  (4) GREATER ZERO)                       CL*20
01184          MOVE AM-ALLOWANCE-AMT (4)  TO WS-ACCOUNT-ALLOWANCE (4).     CL*20
01185                                                                      CL*20
01186      IF (AM-ALLOWANCE-AMT  (5) NUMERIC                               CL*20
01187        AND AM-ALLOWANCE-AMT  (5) GREATER ZERO)                       CL*20
01188          MOVE AM-ALLOWANCE-AMT (5)  TO WS-ACCOUNT-ALLOWANCE (5).     CL*20
01189                                                                      CL*20
01190      IF RNGBEG1L GREATER ZEROS                                       CL*20
01191          MOVE RNGBEG1I              TO DEEDIT-FIELD-V0               CL*20
01192          PERFORM 8600-DEEDIT                                         CL*20
01193          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01194              MOVE DEEDIT-FIELD-V0   TO WS-BEGIN-RANGE (1)            CL*20
01195                                        RNGBEG1O                      CL*21
01196          ELSE                                                        CL*20
01197              MOVE ZEROS             TO WS-BEGIN-RANGE (1)            CL*20
01198              MOVE -1                TO RNGBEG1L                      CL*20
01199              MOVE AL-UABON          TO RNGBEG1A                      CL*20
01200              MOVE ER-2223           TO EMI-ERROR                     CL*20
01201              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01202                                                                      CL*20
01203      IF RNGEND1L GREATER ZEROS                                       CL*20
01204          MOVE RNGEND1I              TO DEEDIT-FIELD-V0               CL*20
01205          PERFORM 8600-DEEDIT                                         CL*20
01206          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01207              MOVE DEEDIT-FIELD-V0   TO WS-END-RANGE (1)              CL*20
01208                                        RNGEND1O                      CL*21
01209          ELSE                                                        CL*20
01210              MOVE ZEROS             TO WS-END-RANGE (1)              CL*20
01211              MOVE -1                TO RNGEND1L                      CL*20
01212              MOVE AL-UABON          TO RNGEND1A                      CL*20
01213              MOVE ER-2223           TO EMI-ERROR                     CL*20
01214              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01215                                                                      CL*20
01216      IF RNGBEG2L GREATER ZEROS                                       CL*20
01217          MOVE RNGBEG2I              TO DEEDIT-FIELD-V0               CL*20
01218          PERFORM 8600-DEEDIT                                         CL*20
01219          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01220              MOVE DEEDIT-FIELD-V0   TO WS-BEGIN-RANGE (2)            CL*20
01221                                        RNGBEG2O                      CL*21
01222          ELSE                                                        CL*20
01223              MOVE ZEROS             TO WS-BEGIN-RANGE (2)            CL*20
01224              MOVE -1                TO RNGBEG2L                      CL*20
01225              MOVE AL-UABON          TO RNGBEG2A                      CL*20
01226              MOVE ER-2223           TO EMI-ERROR                     CL*20
01227              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01228                                                                      CL*20
01229      IF RNGEND2L GREATER ZEROS                                       CL*20
01230          MOVE RNGEND2I              TO DEEDIT-FIELD-V0               CL*20
01231          PERFORM 8600-DEEDIT                                         CL*20
01232          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01233              MOVE DEEDIT-FIELD-V0   TO WS-END-RANGE (2)              CL*20
01234                                        RNGEND2O                      CL*21
01235          ELSE                                                        CL*20
01236              MOVE ZEROS             TO WS-END-RANGE (2)              CL*20
01237              MOVE -1                TO RNGEND2L                      CL*20
01238              MOVE AL-UABON          TO RNGEND2A                      CL*20
01239              MOVE ER-2223           TO EMI-ERROR                     CL*20
01240              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01241                                                                      CL*20
01242      IF RNGBEG3L GREATER ZEROS                                       CL*20
01243          MOVE RNGBEG3I              TO DEEDIT-FIELD-V0               CL*20
01244          PERFORM 8600-DEEDIT                                         CL*20
01245          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01246              MOVE DEEDIT-FIELD-V0   TO WS-BEGIN-RANGE (3)            CL*20
01247                                        RNGBEG3O                      CL*21
01248          ELSE                                                        CL*20
01249              MOVE ZEROS             TO WS-BEGIN-RANGE (3)            CL*20
01250              MOVE -1                TO RNGBEG3L                      CL*20
01251              MOVE AL-UABON          TO RNGBEG3A                      CL*20
01252              MOVE ER-2223           TO EMI-ERROR                     CL*20
01253              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01254                                                                      CL*20
01255      IF RNGEND3L GREATER ZEROS                                       CL*20
01256          MOVE RNGEND3I              TO DEEDIT-FIELD-V0               CL*20
01257          PERFORM 8600-DEEDIT                                         CL*20
01258          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01259              MOVE DEEDIT-FIELD-V0   TO WS-END-RANGE (3)              CL*20
01260                                        RNGEND3O                      CL*21
01261          ELSE                                                        CL*20
01262              MOVE ZEROS             TO WS-END-RANGE (3)              CL*20
01263              MOVE -1                TO RNGEND3L                      CL*20
01264              MOVE AL-UABON          TO RNGEND3A                      CL*20
01265              MOVE ER-2223           TO EMI-ERROR                     CL*20
01266              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01267                                                                      CL*20
01268      IF RNGBEG4L GREATER ZEROS                                       CL*20
01269          MOVE RNGBEG4I              TO DEEDIT-FIELD-V0               CL*20
01270          PERFORM 8600-DEEDIT                                         CL*20
01271          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01272              MOVE DEEDIT-FIELD-V0   TO WS-BEGIN-RANGE (4)            CL*20
01273                                        RNGBEG4O                      CL*21
01274          ELSE                                                        CL*20
01275              MOVE ZEROS             TO WS-BEGIN-RANGE (4)            CL*20
01276              MOVE -1                TO RNGBEG4L                      CL*20
01277              MOVE AL-UABON          TO RNGBEG4A                      CL*20
01278              MOVE ER-2223           TO EMI-ERROR                     CL*20
01279              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01280                                                                      CL*20
01281      IF RNGEND4L GREATER ZEROS                                       CL*20
01282          MOVE RNGEND4I              TO DEEDIT-FIELD-V0               CL*20
01283          PERFORM 8600-DEEDIT                                         CL*20
01284          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01285              MOVE DEEDIT-FIELD-V0   TO WS-END-RANGE (4)              CL*20
01286                                        RNGEND4O                      CL*21
01287          ELSE                                                        CL*20
01288              MOVE ZEROS             TO WS-END-RANGE (4)              CL*20
01289              MOVE -1                TO RNGEND4L                      CL*20
01290              MOVE AL-UABON          TO RNGEND4A                      CL*20
01291              MOVE ER-2223           TO EMI-ERROR                     CL*20
01292              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01293                                                                      CL*20
01294      IF RNGBEG5L GREATER ZEROS                                       CL*20
01295          MOVE RNGBEG5I              TO DEEDIT-FIELD-V0               CL*20
01296          PERFORM 8600-DEEDIT                                         CL*20
01297          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01298              MOVE DEEDIT-FIELD-V0   TO WS-BEGIN-RANGE (5)            CL*20
01299                                        RNGBEG5O                      CL*21
01300          ELSE                                                        CL*20
01301              MOVE ZEROS             TO WS-BEGIN-RANGE (5)            CL*20
01302              MOVE -1                TO RNGBEG5L                      CL*20
01303              MOVE AL-UABON          TO RNGBEG5A                      CL*20
01304              MOVE ER-2223           TO EMI-ERROR                     CL*20
01305              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01306                                                                      CL*20
01307      IF RNGEND5L GREATER ZEROS                                       CL*20
01308          MOVE RNGEND5I              TO DEEDIT-FIELD-V0               CL*20
01309          PERFORM 8600-DEEDIT                                         CL*20
01310          IF DEEDIT-FIELD-V0 NUMERIC                                  CL*20
01311              MOVE DEEDIT-FIELD-V0   TO WS-END-RANGE (5)              CL*20
01312                                        RNGEND5O                      CL*21
01313          ELSE                                                        CL*20
01314              MOVE ZEROS             TO WS-END-RANGE (5)              CL*20
01315              MOVE -1                TO RNGEND5L                      CL*20
01316              MOVE AL-UABON          TO RNGEND5A                      CL*20
01317              MOVE ER-2223           TO EMI-ERROR                     CL*20
01318              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01319                                                                      CL*20
01320      IF ALLAMT1L GREATER ZEROS                                       CL*20
01321          MOVE ALLAMT1I              TO DEEDIT-FIELD                  CL*20
01322          PERFORM 8600-DEEDIT                                         CL*20
01323          IF DEEDIT-FIELD-V2 NUMERIC                                  CL*20
01324              MOVE DEEDIT-FIELD-V2   TO WS-ACCOUNT-ALLOWANCE (1)      CL*20
01325                                        ALLAMT1O                      CL*21
01326          ELSE                                                        CL*20
01327              MOVE ZEROS             TO WS-ACCOUNT-ALLOWANCE (1)      CL*20
01328              MOVE -1                TO ALLAMT1L                      CL*20
01329              MOVE AL-UABON          TO ALLAMT1A                      CL*20
01330              MOVE ER-2223           TO EMI-ERROR                     CL*20
01331              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01332                                                                      CL*20
01333      IF ALLAMT2L GREATER ZEROS                                       CL*20
01334          MOVE ALLAMT2I              TO DEEDIT-FIELD                  CL*20
01335          PERFORM 8600-DEEDIT                                         CL*20
01336          IF DEEDIT-FIELD-V2 NUMERIC                                  CL*20
01337              MOVE DEEDIT-FIELD-V2   TO WS-ACCOUNT-ALLOWANCE (2)      CL*20
01338                                        ALLAMT2O                      CL*21
01339          ELSE                                                        CL*20
01340              MOVE ZEROS             TO WS-ACCOUNT-ALLOWANCE (2)      CL*20
01341              MOVE -1                TO ALLAMT2L                      CL*20
01342              MOVE AL-UABON          TO ALLAMT2A                      CL*20
01343              MOVE ER-2223           TO EMI-ERROR                     CL*20
01344              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01345                                                                      CL*20
01346      IF ALLAMT3L GREATER ZEROS                                       CL*20
01347          MOVE ALLAMT3I              TO DEEDIT-FIELD                  CL*20
01348          PERFORM 8600-DEEDIT                                         CL*20
01349          IF DEEDIT-FIELD-V2 NUMERIC                                  CL*20
01350              MOVE DEEDIT-FIELD-V2   TO WS-ACCOUNT-ALLOWANCE (3)      CL*20
01351                                        ALLAMT3O                      CL*21
01352          ELSE                                                        CL*20
01353              MOVE ZEROS             TO WS-ACCOUNT-ALLOWANCE (3)      CL*20
01354              MOVE -1                TO ALLAMT3L                      CL*20
01355              MOVE AL-UABON          TO ALLAMT3A                      CL*20
01356              MOVE ER-2223           TO EMI-ERROR                     CL*20
01357              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01358                                                                      CL*20
01359      IF ALLAMT4L GREATER ZEROS                                       CL*20
01360          MOVE ALLAMT4I              TO DEEDIT-FIELD                  CL*20
01361          PERFORM 8600-DEEDIT                                         CL*20
01362          IF DEEDIT-FIELD-V2 NUMERIC                                  CL*20
01363              MOVE DEEDIT-FIELD-V2   TO WS-ACCOUNT-ALLOWANCE (4)      CL*20
01364                                        ALLAMT4O                      CL*21
01365          ELSE                                                        CL*20
01366              MOVE ZEROS             TO WS-ACCOUNT-ALLOWANCE (4)      CL*20
01367              MOVE -1                TO ALLAMT4L                      CL*20
01368              MOVE AL-UABON          TO ALLAMT4A                      CL*20
01369              MOVE ER-2223           TO EMI-ERROR                     CL*20
01370              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01371                                                                      CL*20
01372      IF ALLAMT5L GREATER ZEROS                                       CL*20
01373          MOVE ALLAMT5I              TO DEEDIT-FIELD                  CL*20
01374          PERFORM 8600-DEEDIT                                         CL*20
01375          IF DEEDIT-FIELD-V2 NUMERIC                                  CL*20
01376              MOVE DEEDIT-FIELD-V2   TO WS-ACCOUNT-ALLOWANCE (5)      CL*20
01377                                        ALLAMT5O                      CL*21
01378          ELSE                                                        CL*20
01379              MOVE ZEROS             TO WS-ACCOUNT-ALLOWANCE (5)      CL*20
01380              MOVE -1                TO ALLAMT5L                      CL*20
01381              MOVE AL-UABON          TO ALLAMT5A                      CL*20
01382              MOVE ER-2223           TO EMI-ERROR                     CL*20
01383              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL*20
01384                                                                      CL*20
01385      PERFORM 7500-EDIT-RANGES       THRU 7505-EXIT.                  CL*20
01386                                                                      CL**5
01387  7099-EXIT.                                                       EL6506
01388      EXIT.                                                        EL6506
01389      EJECT                                                        EL6506
01390                                                                   EL6506
01391  7100-READ-ERACCT.                                                EL6506
01392                                                                      CL*18
01393      EXEC CICS READ                                               EL6506
01394           DATASET  (ERACCT-FILE)                                  EL6506
01395           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*26
01396           RIDFLD   (PI-ACCT-KEY)                                  EL6506
01397      END-EXEC.                                                    EL6506
01398                                                                   EL6506
01399      CONTINUE.                                                       CL*26
01400                                                                   EL6506
01401      MOVE AM-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL6506
01402      MOVE AM-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL6506
01403                                                                   EL6506
01404  7100-EXIT.                                                       EL6506
01405      EXIT.                                                        EL6506
01406      EJECT                                                        EL6506
01407                                                                   EL6506
01408  7200-DEEDIT.                                                     EL6506
01409      EXEC CICS BIF                                                EL6506
01410           DEEDIT                                                  EL6506
01411           FIELD  (DEEDIT-FIELD)                                   EL6506
01412           LENGTH (15)                                             EL6506
01413      END-EXEC.                                                    EL6506
01414                                                                   EL6506
01415  7200-EXIT.                                                       EL6506
01416      EXIT.                                                        EL6506
01417      EJECT                                                        EL6506
01418  7300-READ-ERACCT-UPDATE.                                         EL6506
01419      EXEC CICS READ                                               EL6506
01420           DATASET  (ERACCT-FILE)                                  EL6506
01421           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*26
01422           RIDFLD   (PI-ACCT-KEY)                                  EL6506
01423           UPDATE                                                  EL6506
01424      END-EXEC.                                                    EL6506
01425                                                                   EL6506
01426      CONTINUE.                                                       CL*26
01427                                                                   EL6506
01428  7300-EXIT.                                                       EL6506
01429      EXIT.                                                        EL6506
01430      EJECT                                                           CL*20
01431  7500-EDIT-RANGES.                                                   CL*20
01432                                                                      CL*20
01433 *    IF WS-BEGIN-RANGE (1) GREATER THAN WS-END-RANGE (1)             CL*20
01434 *        MOVE -1                TO RNGBEG1L                          CL*20
01435 *        MOVE AL-UABON          TO RNGBEG1A                          CL*20
01436 *        MOVE ER-3043           TO EMI-ERROR                         CL*20
01437 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*20
01438 *                                                                    CL*20
01439 *    IF WS-BEGIN-RANGE (2) GREATER THAN WS-END-RANGE (2)             CL*20
01440 *        MOVE -1                TO RNGBEG2L                          CL*20
01441 *        MOVE AL-UABON          TO RNGBEG2A                          CL*20
01442 *        MOVE ER-3043           TO EMI-ERROR                         CL*20
01443 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*20
01444 *                                                                    CL*20
01445 *    IF WS-BEGIN-RANGE (3) GREATER THAN WS-END-RANGE (3)             CL*20
01446 *        MOVE -1                TO RNGBEG3L                          CL*20
01447 *        MOVE AL-UABON          TO RNGBEG3A                          CL*20
01448 *        MOVE ER-3043           TO EMI-ERROR                         CL*20
01449 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*20
01450 *                                                                    CL*20
01451 *    IF WS-BEGIN-RANGE (4) GREATER THAN WS-END-RANGE (4)             CL*20
01452 *        MOVE -1                TO RNGBEG4L                          CL*20
01453 *        MOVE AL-UABON          TO RNGBEG4A                          CL*20
01454 *        MOVE ER-3043           TO EMI-ERROR                         CL*20
01455 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*20
01456 *                                                                    CL*20
01457 *    IF WS-BEGIN-RANGE (5) GREATER THAN WS-END-RANGE (5)             CL*20
01458 *        MOVE -1                TO RNGBEG5L                          CL*20
01459 *        MOVE AL-UABON          TO RNGBEG5A                          CL*20
01460 *        MOVE ER-3043           TO EMI-ERROR                         CL*20
01461 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*20
01462 *                                                                    CL*20
01463 *    IF (WS-BEGIN-RANGE (2) NOT EQUAL ZERO                           CL*20
01464 *      AND WS-END-RANGE (1) NOT LESS THAN WS-BEGIN-RANGE (2))        CL*20
01465 *        MOVE -1                TO RNGEND1L                          CL*20
01466 *        MOVE AL-UABON          TO RNGEND1A                          CL*20
01467 *        MOVE ER-3043           TO EMI-ERROR                         CL*20
01468 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*20
01469 *                                                                    CL*20
01470 *    IF (WS-BEGIN-RANGE (3) NOT EQUAL ZERO                           CL*20
01471 *      AND WS-END-RANGE (2) NOT LESS THAN WS-BEGIN-RANGE (3))        CL*20
01472 *        MOVE -1                TO RNGEND2L                          CL*20
01473 *        MOVE AL-UABON          TO RNGEND2A                          CL*20
01474 *        MOVE ER-3043           TO EMI-ERROR                         CL*20
01475 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*20
01476 *                                                                    CL*20
01477 *    IF (WS-BEGIN-RANGE (4) NOT EQUAL ZERO                           CL*20
01478 *      AND WS-END-RANGE (3) NOT LESS THAN WS-BEGIN-RANGE (4))        CL*20
01479 *        MOVE -1                TO RNGEND3L                          CL*20
01480 *        MOVE AL-UABON          TO RNGEND3A                          CL*20
01481 *        MOVE ER-3043           TO EMI-ERROR                         CL*20
01482 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*20
01483 *                                                                    CL*20
01484 *    IF (WS-BEGIN-RANGE (5) NOT EQUAL ZERO                           CL*20
01485 *      AND WS-END-RANGE (4) NOT LESS THAN WS-BEGIN-RANGE (5))        CL*20
01486 *        MOVE -1                TO RNGEND4L                          CL*20
01487 *        MOVE AL-UABON          TO RNGEND4A                          CL*20
01488 *        MOVE ER-3043           TO EMI-ERROR                         CL*20
01489 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                   CL*20
01490                                                                      CL*20
01491      IF WS-ACCOUNT-ALLOWANCE (5) GREATER THAN ZERO                   CL*20
01492         AND (WS-ACCOUNT-ALLOWANCE (5) NOT GREATER THAN               CL*20
01493                              WS-ACCOUNT-ALLOWANCE (4)                CL*20
01494         OR WS-ACCOUNT-ALLOWANCE (4) NOT GREATER THAN                 CL*20
01495                              WS-ACCOUNT-ALLOWANCE (3)                CL*20
01496         OR WS-ACCOUNT-ALLOWANCE (3) NOT GREATER THAN                 CL*20
01497                              WS-ACCOUNT-ALLOWANCE (2)                CL*20
01498         OR WS-ACCOUNT-ALLOWANCE (2) NOT GREATER THAN                 CL*20
01499                              WS-ACCOUNT-ALLOWANCE (1))               CL*20
01500          MOVE -1                TO ALLAMT1L                          CL*20
01501          MOVE AL-UABON          TO ALLAMT1A                          CL*20
01502          MOVE ER-3043           TO EMI-ERROR                         CL*20
01503          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*20
01504          GO TO 7501-CONTINUE.                                        CL*20
01505                                                                      CL*20
01506      IF WS-ACCOUNT-ALLOWANCE (4) GREATER THAN ZERO                   CL*20
01507         AND (WS-ACCOUNT-ALLOWANCE (4) NOT GREATER THAN               CL*20
01508                              WS-ACCOUNT-ALLOWANCE (3)                CL*20
01509         OR WS-ACCOUNT-ALLOWANCE (3) NOT GREATER THAN                 CL*20
01510                              WS-ACCOUNT-ALLOWANCE (2)                CL*20
01511         OR WS-ACCOUNT-ALLOWANCE (2) NOT GREATER THAN                 CL*20
01512                              WS-ACCOUNT-ALLOWANCE (1))               CL*20
01513          MOVE -1                TO ALLAMT4L                          CL*20
01514          MOVE AL-UABON          TO ALLAMT4A                          CL*20
01515          MOVE ER-3043           TO EMI-ERROR                         CL*20
01516          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*20
01517          GO TO 7501-CONTINUE.                                        CL*20
01518                                                                      CL*20
01519      IF WS-ACCOUNT-ALLOWANCE (3) GREATER THAN ZERO                   CL*20
01520         AND (WS-ACCOUNT-ALLOWANCE (3) NOT GREATER THAN               CL*20
01521                              WS-ACCOUNT-ALLOWANCE (2)                CL*20
01522         OR WS-ACCOUNT-ALLOWANCE (2) NOT GREATER THAN                 CL*20
01523                              WS-ACCOUNT-ALLOWANCE (1))               CL*20
01524          MOVE -1                TO ALLAMT1L                          CL*20
01525          MOVE AL-UABON          TO ALLAMT1A                          CL*20
01526          MOVE ER-3043           TO EMI-ERROR                         CL*20
01527          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*20
01528          GO TO 7501-CONTINUE.                                        CL*20
01529                                                                      CL*20
01530      IF (WS-ACCOUNT-ALLOWANCE (2) GREATER THAN ZERO                  CL*20
01531         AND WS-ACCOUNT-ALLOWANCE (2) NOT GREATER THAN                CL*20
01532                              WS-ACCOUNT-ALLOWANCE (1))               CL*20
01533          MOVE -1                TO ALLAMT1L                          CL*20
01534          MOVE AL-UABON          TO ALLAMT1A                          CL*20
01535          MOVE ER-3043           TO EMI-ERROR                         CL*20
01536          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*20
01537          GO TO 7501-CONTINUE.                                        CL*20
01538                                                                      CL*20
01539  7501-CONTINUE.                                                      CL*20
01540                                                                      CL*20
01541      IF WS-END-RANGE (5)  GREATER THAN ZERO                          CL*20
01542         AND (WS-END-RANGE (5) NOT GREATER THAN                       CL*20
01543                             WS-BEGIN-RANGE (5)                       CL*20
01544         OR  WS-BEGIN-RANGE (5) NOT GREATER THAN                      CL*20
01545                             WS-END-RANGE (4)                         CL*20
01546         OR  WS-END-RANGE (4) NOT GREATER THAN                        CL*20
01547                             WS-BEGIN-RANGE (4)                       CL*20
01548         OR  WS-BEGIN-RANGE (4) NOT GREATER THAN                      CL*20
01549                             WS-END-RANGE (3)                         CL*20
01550         OR  WS-END-RANGE (3) NOT GREATER THAN                        CL*20
01551                             WS-BEGIN-RANGE (3)                       CL*20
01552         OR  WS-BEGIN-RANGE (3) NOT GREATER THAN                      CL*20
01553                             WS-END-RANGE (2)                         CL*20
01554         OR  WS-END-RANGE (2) NOT GREATER THAN                        CL*20
01555                             WS-BEGIN-RANGE (2)                       CL*20
01556         OR  WS-BEGIN-RANGE (2) NOT GREATER THAN                      CL*20
01557                             WS-END-RANGE (1)                         CL*20
01558         OR  WS-END-RANGE (1) NOT GREATER THAN                        CL*20
01559                             WS-BEGIN-RANGE (1))                      CL*20
01560          MOVE -1                TO RNGBEG5L                          CL*20
01561          MOVE AL-UABON          TO RNGBEG5A                          CL*20
01562          MOVE ER-3043           TO EMI-ERROR                         CL*20
01563          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*20
01564          GO TO 7505-EXIT.                                            CL*20
01565                                                                      CL*20
01566      IF WS-END-RANGE (4)  GREATER THAN ZERO                          CL*20
01567         AND (WS-END-RANGE (4) NOT GREATER THAN                       CL*20
01568                             WS-BEGIN-RANGE (4)                       CL*20
01569         OR  WS-BEGIN-RANGE (4) NOT GREATER THAN                      CL*20
01570                             WS-END-RANGE (3)                         CL*20
01571         OR  WS-END-RANGE (3) NOT GREATER THAN                        CL*20
01572                             WS-BEGIN-RANGE (3)                       CL*20
01573         OR  WS-BEGIN-RANGE (3) NOT GREATER THAN                      CL*20
01574                             WS-END-RANGE (2)                         CL*20
01575         OR  WS-END-RANGE (2) NOT GREATER THAN                        CL*20
01576                             WS-BEGIN-RANGE (2)                       CL*20
01577         OR  WS-BEGIN-RANGE (2) NOT GREATER THAN                      CL*20
01578                             WS-END-RANGE (1)                         CL*20
01579         OR  WS-END-RANGE (1) NOT GREATER THAN                        CL*20
01580                             WS-BEGIN-RANGE (1))                      CL*20
01581          MOVE -1                TO RNGBEG4L                          CL*20
01582          MOVE AL-UABON          TO RNGBEG4A                          CL*20
01583          MOVE ER-3043           TO EMI-ERROR                         CL*20
01584          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*20
01585          GO TO 7505-EXIT.                                            CL*20
01586                                                                      CL*20
01587      IF WS-END-RANGE (3)  GREATER THAN ZERO                          CL*20
01588         AND (WS-END-RANGE (3) NOT GREATER THAN                       CL*20
01589                             WS-BEGIN-RANGE (3)                       CL*20
01590         OR  WS-BEGIN-RANGE (3) NOT GREATER THAN                      CL*20
01591                             WS-END-RANGE (2)                         CL*20
01592         OR  WS-END-RANGE (2) NOT GREATER THAN                        CL*20
01593                             WS-BEGIN-RANGE (2)                       CL*20
01594         OR  WS-BEGIN-RANGE (2) NOT GREATER THAN                      CL*20
01595                             WS-END-RANGE (1)                         CL*20
01596         OR  WS-END-RANGE (1) NOT GREATER THAN                        CL*20
01597                             WS-BEGIN-RANGE (1))                      CL*20
01598          MOVE -1                TO RNGBEG3L                          CL*20
01599          MOVE AL-UABON          TO RNGBEG3A                          CL*20
01600          MOVE ER-3043           TO EMI-ERROR                         CL*20
01601          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*20
01602          GO TO 7505-EXIT.                                            CL*20
01603                                                                      CL*20
01604      IF WS-END-RANGE (2)  GREATER THAN ZERO                          CL*20
01605         AND (WS-END-RANGE (2) NOT GREATER THAN                       CL*20
01606                             WS-BEGIN-RANGE (2)                       CL*20
01607         OR  WS-BEGIN-RANGE (2) NOT GREATER THAN                      CL*20
01608                             WS-END-RANGE (1)                         CL*20
01609         OR  WS-END-RANGE (1) NOT GREATER THAN                        CL*20
01610                             WS-BEGIN-RANGE (1))                      CL*20
01611          MOVE -1                TO RNGBEG2L                          CL*20
01612          MOVE AL-UABON          TO RNGBEG2A                          CL*20
01613          MOVE ER-3043           TO EMI-ERROR                         CL*20
01614          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*20
01615          GO TO 7505-EXIT.                                            CL*20
01616                                                                      CL*20
01617  7505-EXIT.                                                          CL*20
01618                                                                      CL*24
01619  EJECT                                                               CL*24
01620  7800-COMPANY-REC-READ.                                           EL6506
01621      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6506
01622      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6506
01623      MOVE '1'                    TO  CNTL-REC-TYPE.               EL6506
01624      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6506
01625      EXEC CICS HANDLE CONDITION                                   EL6506
01626          NOTFND   (7880-NO-COMP)                                  EL6506
01627      END-EXEC.                                                    EL6506
01628                                                                   EL6506
01629      EXEC CICS READ                                               EL6506
01630          DATASET   (ELCNTL-FILE)                                  EL6506
01631          SET       (ADDRESS OF CONTROL-FILE)                         CL*26
01632          RIDFLD    (ELCNTL-KEY)                                   EL6506
01633      END-EXEC.                                                       CL*12
01634                                                                   EL6506
01635      CONTINUE.                                                       CL*26
01636                                                                   EL6506
01637      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES                     EL6506
01638          MOVE ER-2572            TO  EMI-ERROR                    EL6506
01639          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6506
01640                                                                      CL*12
01641      GO TO 7899-EXIT.                                             EL6506
01642                                                                   EL6506
01643  7880-NO-COMP.                                                    EL6506
01644                                                                      CL*18
01645      MOVE ER-0002                TO  EMI-ERROR                    EL6506
01646      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6506
01647                                                                      CL*12
01648  7899-EXIT.                                                       EL6506
01649      EXIT.                                                        EL6506
01650      EJECT                                                        EL6506
01651  8000-UPDATE-MAINT-DATE.                                          EL6506
01652                                                                      CL*18
01653      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6506
01654      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6506
01655      MOVE '1'                    TO  CNTL-REC-TYPE.               EL6506
01656      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6506
01657                                                                   EL6506
01658      EXEC CICS HANDLE CONDITION                                   EL6506
01659          NOTFND   (8000-EXIT)                                     EL6506
01660      END-EXEC.                                                    EL6506
01661                                                                   EL6506
01662      EXEC CICS READ                                               EL6506
01663          UPDATE                                                   EL6506
01664          DATASET   (ELCNTL-FILE)                                  EL6506
01665          SET       (ADDRESS OF CONTROL-FILE)                         CL*26
01666          RIDFLD    (ELCNTL-KEY)                                   EL6506
01667      END-EXEC.                                                    EL6506
01668                                                                   EL6506
01669      CONTINUE.                                                       CL*26
01670                                                                   EL6506
01671      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL6506
01672      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL6506
01673      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6506
01674      MOVE ELCNTL-FILE            TO  FILE-ID.                     EL6506
01675      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6506
01676                                                                   EL6506
01677      MOVE BIN-CURRENT-SAVE       TO  CF-ACCOUNT-MSTR-MAINT-DT.    EL6506
01678                                                                   EL6506
01679      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL6506
01680      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL6506
01681      MOVE ELCNTL-FILE            TO  FILE-ID.                     EL6506
01682                                                                   EL6506
01683      EXEC CICS REWRITE                                            EL6506
01684          DATASET   (ELCNTL-FILE)                                  EL6506
01685          FROM      (CONTROL-FILE)                                    CL*12
01686      END-EXEC.                                                    EL6506
01687                                                                   EL6506
01688      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6506
01689      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6506
01690                                                                   EL6506
01691  8000-EXIT.                                                       EL6506
01692       EXIT.                                                       EL6506
01693      EJECT                                                        EL6506
01694                                                                   EL6506
01695  8100-SEND-INITIAL-MAP.                                           EL6506
01696      MOVE SAVE-DATE              TO  DATEO.                       EL6506
01697      MOVE EIBTIME                TO  TIME-IN.                     EL6506
01698      MOVE TIME-OUT               TO  TIMEO.                       EL6506
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01699      MOVE -1                     TO  PFENTERL.                    EL6506
01700      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6506
01701                                                                   EL6506
01702      MOVE PI-LIFE-OVERRIDE-L6    TO  LFHEAD1O                        CL**2
01703                                      LFHEAD2O.                       CL**2
01704                                                                      CL**2
01705      MOVE PI-AH-OVERRIDE-L6      TO  AHHEAD1O                     EL6506
01706                                      AHHEAD2O                     EL6506
01707                                      AHHEAD3O                        CL**2
01708                                      AHHEAD4O.                       CL**2
01709                                                                      CL**2
PEMMOD     SET T-INDEX                 TO PI-LINE-SELECTED
PEMMOD
PEMMOD     IF PI-2ND-PAGE
PEMMOD        SET T-INDEX UP BY 8
PEMMOD     ELSE
PEMMOD        IF PI-3RD-PAGE
PEMMOD           SET T-INDEX UP BY 16
PEMMOD        ELSE
PEMMOD           IF PI-LST-PAGE
PEMMOD              SET T-INDEX UP BY 24
PEMMOD           END-IF
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     MOVE PI-ACCT-CARRIER        TO CARRI
PEMMOD     MOVE PI-ACCT-GROUPING       TO GROUPINI
PEMMOD     MOVE PI-ACCT-STATE          TO STATEI
PEMMOD     MOVE PI-ACCT-ACCOUNT        TO ACCOUNTI
PEMMOD
PEMMOD     IF PI-ACCT-EXP-DT = LOW-VALUES
PEMMOD        MOVE SPACES              TO EXPDTEI
PEMMOD     ELSE
PEMMOD        IF PI-ACCT-EXP-DT NOT = HIGH-VALUES
PEMMOD           MOVE PI-ACCT-EXP-DT   TO DC-BIN-DATE-1
PEMMOD           MOVE SPACE            TO DC-OPTION-CODE
PEMMOD           PERFORM 9700-LINK-DATE-CONVERT
PEMMOD                                 THRU 9700-EXIT
PEMMOD           MOVE DC-GREG-DATE-1-EDIT
PEMMOD                                 TO EXPDTEI
PEMMOD        ELSE
PEMMOD           MOVE 999999           TO EXPDTEO
PEMMOD           INSPECT EXPDTEI CONVERTING SPACES TO '/'
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     MOVE EXPDTEI (1:2)          TO WS-YYYYMMDD (5:2)
PEMMOD     MOVE EXPDTEI (4:2)          TO WS-YYYYMMDD (7:2)
PEMMOD     MOVE EXPDTEI (6:2)          TO WS-YYYYMMDD (3:2)
PEMMOD     IF EXPDTEI (7:1) NOT = '9'
PEMMOD        MOVE '20'                TO WS-YYYYMMDD (1:2)
PEMMOD     ELSE
PEMMOD        MOVE '19'                TO WS-YYYYMMDD (1:2)
PEMMOD     END-IF
PEMMOD
PEMMOD     MOVE PI-BIN-EFF-DT (T-INDEX)
PEMMOD                                 TO DC-BIN-DATE-1
PEMMOD     MOVE SPACE                  TO DC-OPTION-CODE
PEMMOD     PERFORM 9700-LINK-DATE-CONVERT
PEMMOD                                 THRU 9700-EXIT
PEMMOD     MOVE DC-GREG-DATE-1-EDIT    TO EFFDTEI
PEMMOD
PEMMOD     IF (PI-PROCESSOR-ID NOT = 'PEMA')
020816        AND (PI-COMPANY-ID NOT = 'DCC' and 'VPP')
PEMMOD        MOVE AL-SADON            TO EMREDA
PEMMOD                                    EMLEVA
PEMMOD                                    EMAHA
PEMMOD     END-IF
PEMMOD
01710      EXEC CICS SEND                                               EL6506
01711          MAP      (MAP-NAME)                                      EL6506
01712          MAPSET   (MAPSET-NAME)                                   EL6506
01713          FROM     (EL6506AO)                                      EL6506
01714          ERASE                                                    EL6506
01715          CURSOR                                                   EL6506
01716      END-EXEC.                                                    EL6506
01717                                                                   EL6506
01718      GO TO 9100-RETURN-TRAN.                                      EL6506
01719                                                                   EL6506
01720  EJECT                                                               CL*20
01721  8200-SEND-DATAONLY.                                              EL6506
01722      MOVE SAVE-DATE              TO  DATEO.                       EL6506
01723      MOVE EIBTIME                TO  TIME-IN.                     EL6506
01724      MOVE TIME-OUT               TO  TIMEO.                       EL6506
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01725      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O                     EL6506
01726                                                                   EL6506
01727      MOVE PI-LIFE-OVERRIDE-L6    TO  LFHEAD1O.                    EL6506
01728      MOVE PI-AH-OVERRIDE-L6      TO  AHHEAD1O                     EL6506
01729                                      AHHEAD2O                     EL6506
01730                                      AHHEAD3O.                    EL6506
01731                                                                   EL6506
PEMMOD     IF (PI-PROCESSOR-ID NOT = 'PEMA')
020816        AND (PI-COMPANY-ID NOT = 'DCC' and 'VPP')
PEMMOD        MOVE AL-SADON            TO EMREDA
PEMMOD                                    EMLEVA
PEMMOD                                    EMAHA
PEMMOD     END-IF
PEMMOD
01732      EXEC CICS SEND                                               EL6506
01733          MAP      (MAP-NAME)                                      EL6506
01734          MAPSET   (MAPSET-NAME)                                   EL6506
01735          FROM     (EL6506AO)                                      EL6506
01736          DATAONLY                                                 EL6506
01737          CURSOR                                                   EL6506
01738      END-EXEC.                                                    EL6506
01739                                                                   EL6506
01740      GO TO 9100-RETURN-TRAN.                                      EL6506
01741                                                                   EL6506
01742  EJECT                                                               CL*20
01743  8300-SEND-TEXT.                                                  EL6506
01744      EXEC CICS SEND TEXT                                          EL6506
01745          FROM     (LOGOFF-TEXT)                                   EL6506
01746          LENGTH   (LOGOFF-LENGTH)                                 EL6506
01747          ERASE                                                    EL6506
01748          FREEKB                                                   EL6506
01749      END-EXEC.                                                    EL6506
01750                                                                   EL6506
01751      EXEC CICS RETURN                                             EL6506
01752      END-EXEC.                                                    EL6506
01753                                                                   EL6506
01754  EJECT                                                               CL*20
01755  8400-LOG-JOURNAL-RECORD.                                         EL6506
01756      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL6506
01757      MOVE FILE-ID                TO  JP-FILE-ID.                  EL6506
01758      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL6506
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO                             EL6506
pemuni*        EXEC CICS JOURNAL                                        EL6506
pemuni*            JFILEID     (PI-JOURNAL-FILE-ID)                     EL6506
pemuni*            JTYPEID     ('ER')                                   EL6506
pemuni*            FROM        (JOURNAL-RECORD)                         EL6506
pemuni*            LENGTH      (WS-JOURNAL-FILE-LENGTH)                 EL6506
pemuni*        END-EXEC.                                                EL6506
01766                                                                   EL6506
01767  8600-DEEDIT.                                                     EL6506
01768      EXEC CICS BIF DEEDIT                                         EL6506
01769           FIELD(DEEDIT-FIELD)                                     EL6506
01770           LENGTH(15)                                              EL6506
01771      END-EXEC.                                                    EL6506
01772                                                                   EL6506
01773  EJECT                                                               CL*20
01774  8800-UNAUTHORIZED-ACCESS.                                        EL6506
01775      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL6506
01776      GO TO 8300-SEND-TEXT.                                        EL6506
01777                                                                   EL6506
01778  8810-PF23.                                                       EL6506
01779      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6506
01780      MOVE XCTL-005               TO  PGM-NAME.                    EL6506
01781      GO TO 9300-XCTL.                                             EL6506
01782                                                                   EL6506
01783  EJECT                                                               CL*20
01784  9000-RETURN-CICS.                                                EL6506
01785      EXEC CICS RETURN                                             EL6506
01786      END-EXEC.                                                    EL6506
01787                                                                   EL6506
01788  9100-RETURN-TRAN.                                                EL6506
01789      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6506
01790      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL6506
01791      EXEC CICS RETURN                                             EL6506
01792          TRANSID    (TRANS-ID)                                    EL6506
01793          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6506
01794          LENGTH     (WS-COMM-LENGTH)                                 CL*13
01795      END-EXEC.                                                    EL6506
01796                                                                   EL6506
01797  9200-RETURN-MAIN-MENU.                                           EL6506
01798      MOVE XCTL-626               TO  PGM-NAME.                    EL6506
01799      GO TO 9300-XCTL.                                             EL6506
01800                                                                   EL6506
01801  EJECT                                                               CL*20
01802  9300-XCTL.                                                       EL6506
01803      EXEC CICS XCTL                                               EL6506
01804          PROGRAM    (PGM-NAME)                                    EL6506
01805          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6506
01806          LENGTH     (WS-COMM-LENGTH)                                 CL*13
01807      END-EXEC.                                                    EL6506
01808                                                                   EL6506
01809  9400-CLEAR.                                                      EL6506
01810      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                       CL*13
01811      GO TO 9300-XCTL.                                             EL6506
01812                                                                   EL6506
01813  9500-PF12.                                                       EL6506
01814      MOVE XCTL-010               TO  PGM-NAME.                    EL6506
01815      GO TO 9300-XCTL.                                             EL6506
01816                                                                   EL6506
01817  9600-PGMID-ERROR.                                                EL6506
01818      EXEC CICS HANDLE CONDITION                                   EL6506
01819          PGMIDERR    (8300-SEND-TEXT)                             EL6506
01820      END-EXEC.                                                    EL6506
01821                                                                   EL6506
01822      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL6506
01823      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL6506
01824      MOVE XCTL-005               TO  PGM-NAME.                    EL6506
01825      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL6506
01826      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6506
01827      GO TO 9300-XCTL.                                             EL6506
01828                                                                   EL6506
01829  9700-LINK-DATE-CONVERT.                                          EL6506
01830      EXEC CICS LINK                                               EL6506
01831          PROGRAM    ('ELDATCV')                                   EL6506
01832          COMMAREA   (DATE-CONVERSION-DATA)                        EL6506
01833          LENGTH     (DC-COMM-LENGTH)                              EL6506
01834      END-EXEC.                                                    EL6506
01835                                                                   EL6506
01836  9700-EXIT.                                                       EL6506
01837      EXIT.                                                        EL6506
01838                                                                   EL6506
01839  EJECT                                                               CL*20
01840  9900-ERROR-FORMAT.                                               EL6506
01841      IF NOT EMI-ERRORS-COMPLETE                                   EL6506
01842          MOVE LINK-001           TO  PGM-NAME                     EL6506
01843          EXEC CICS LINK                                           EL6506
01844              PROGRAM    (PGM-NAME)                                EL6506
01845              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6506
01846              LENGTH     (EMI-COMM-LENGTH)                         EL6506
01847          END-EXEC.                                                EL6506
01848                                                                   EL6506
01849  9900-EXIT.                                                       EL6506
01850      EXIT.                                                        EL6506
01851                                                                   EL6506
01852  9990-ABEND.                                                      EL6506
01853      MOVE LINK-004               TO  PGM-NAME.                    EL6506
01854      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL6506
01855      EXEC CICS LINK                                               EL6506
01856          PROGRAM   (PGM-NAME)                                     EL6506
01857          COMMAREA  (EMI-LINE1)                                    EL6506
01858          LENGTH    (72)                                           EL6506
01859      END-EXEC.                                                    EL6506
01860                                                                   EL6506
01861      GO TO 8200-SEND-DATAONLY.                                    EL6506
01862      GOBACK.                                                      EL6506
01863                                                                   EL6506
01864  9995-SECURITY-VIOLATION.                                         EL6506
01865             COPY ELCSCTP.                                         EL6506
01866  9995-EXIT.                                                       EL6506
01867       EXIT.                                                       EL6506

