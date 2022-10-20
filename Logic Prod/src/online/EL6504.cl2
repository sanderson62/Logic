00001  ID DIVISION.                                                     02/26/96
00002                                                                   EL6504
00003  PROGRAM-ID.                 EL6504.                                 LV011
00004 *              PROGRAM CONVERTED BY                                  CL*11
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*11
00006 *              CONVERSION DATE 02/12/96 08:07:16.                    CL*11
00007 *                            VMOD=2.011                              CL*11
00008 *                                                                 EL6504
00009 *AUTHOR.     LOGIC,INC.                                              CL*11
00010 *            DALLAS, TEXAS.                                          CL*11
00011                                                                   EL6504
00012 *DATE-COMPILED.                                                      CL*11
00013 *SECURITY.   *****************************************************   CL*11
00014 *            *                                                   *   CL*11
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*11
00016 *            *                                                   *   CL*11
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*11
00018 *                                                                *   CL*11
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*11
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*11
00021 *            *                                                   *   CL*11
00022 *            *****************************************************   CL*11
00023 *                                                                 EL6504
00024 *REMARKS.    TRANSACTION - EXC8 - ACCOUNT MAINT (REIN & COMM).       CL*11
00023 *                                                                 EL6504
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101******************************************************************

00025                                                                   EL6504
00026  ENVIRONMENT DIVISION.                                            EL6504
00027                                                                   EL6504
00028      EJECT                                                        EL6504
00029  DATA DIVISION.                                                   EL6504
00030  WORKING-STORAGE SECTION.                                         EL6504
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6504
00032  77  FILLER  PIC X(32)  VALUE '*    EL6504 WORKING STORAGE    *'. EL6504
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.011 *********'.    CL*11
00034                                                                   EL6504
00035  01  WS-DATE-AREA.                                                EL6504
00036      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL6504
00037      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.       CL**5
00038                                                                   EL6504
00039  01  STANDARD-AREAS.                                              EL6504
PEMMOD     12  WS-YYYYMMDD             PIC X(8).                           CL*54
PEMMOD     12  WS-COMP-DATE REDEFINES WS-YYYYMMDD                          CL*54
PEMMOD                                 PIC 9(8).                           CL*54
00040      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.     CL**5
00041      12  MAP-NAME                    PIC X(8)    VALUE 'EL6504A'. EL6504
00042      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6504S'. EL6504
00043      12  SCREEN-NUMBER               PIC X(4)    VALUE '650E'.    EL6504
00044      12  TRANS-ID                    PIC X(4)    VALUE 'EXC8'.    EL6504
00045      12  THIS-PGM                    PIC X(8)    VALUE 'EL6504'.  EL6504
00046      12  PGM-NAME                    PIC X(8)    VALUE SPACES.       CL*11
00047      12  TIME-IN                     PIC S9(7)   VALUE ZEROS.        CL*11
00048      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6504
00049          16  FILLER                  PIC X.                       EL6504
00050          16  TIME-OUT                PIC 99V99.                   EL6504
00051          16  FILLER                  PIC XX.                         CL**5
00052      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL6504
00053      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL6504
00054      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.   EL6504
00055      12  XCTL-650                    PIC X(8)    VALUE 'EL650'.   EL6504
00056      12  XCTL-6501                   PIC X(8)    VALUE 'EL6501'.  EL6504
00057      12  XCTL-6502                   PIC X(8)    VALUE 'EL6502'.  EL6504
00058      12  XCTL-6503                   PIC X(8)    VALUE 'EL6503'.  EL6504
00059      12  XCTL-6505                   PIC X(8)    VALUE 'EL6505'.     CL**4
00060      12  XCTL-6506                   PIC X(8)    VALUE 'EL6506'.  EL6504
00061      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL6504
00062      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL6504
00063      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL6504
00064      12  FILE-ID                     PIC X(8)    VALUE SPACES.    EL6504
00065      12  ERACCT-FILE                 PIC X(8)    VALUE 'ERACCT'.  EL6504
00066      12  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.  EL6504
00067      12  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.    EL6504
00068      12  YMD-CURRENT-SAVE            PIC X(6)    VALUE SPACES.    EL6504
00069                                                                   EL6504
00070      12  ERACCT-LENGTH               PIC S9(4)   VALUE +2023 COMP.EL6504
00071      12  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.EL6504
00072      12  ELCNTL-LENGTH               PIC S9(4)   VALUE +527  COMP.EL6504
00073      12  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.EL6504
00074      12  SUB1                        PIC S9(4)   VALUE +0    COMP.EL6504
00075      12  SUB2                        PIC S9(4)   VALUE +0    COMP.EL6504
00076                                                                   EL6504
00077      12  DEEDIT-FIELD                PIC X(15).                   EL6504
00078      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).      EL6504
00079      12  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V999.  EL6504
00080      12  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9999. EL6504
00081                                                                   EL6504
00082      12  WS-EDIT-FIELD-CONV          PIC S9(4)   VALUE +0.        EL6504
00083      12  WS-TOT-PERCENT              PIC S9V9999 VALUE +0.        EL6504
00084                                                                   EL6504
00085      12  WS-VALID-EARNINGS           PIC X       VALUE SPACE.     EL6504
00086          88  VALID-EARNINGS                      VALUE ' ' 'R' 'P'EL6504
00087                                                       'A' 'C' 'M'.EL6504
00088                                                                   EL6504
00089      12  WS-LF-FEE                   PIC S9V9999 VALUE ZEROS.     EL6504
00090      12  WS-LF-TAX                   PIC S9V9999 VALUE ZEROS.     EL6504
00091      12  WS-AH-FEE                   PIC S9V9999 VALUE ZEROS.     EL6504
00092      12  WS-AH-TAX                   PIC S9V9999 VALUE ZEROS.     EL6504
00093      12  WS-AH-R78                   PIC S9V9999 VALUE ZEROS.     EL6504
00094      12  WS-AH-PRATA                 PIC S9V9999 VALUE ZEROS.     EL6504
00095      12  WS-LF-RET                   PIC S9V9999 VALUE ZEROS.     EL6504
00096      12  WS-AH-RET                   PIC S9V9999 VALUE ZEROS.     EL6504
00097      12  WS-LF-LOSS                  PIC SV999   VALUE ZEROS.     EL6504
00098      12  WS-AH-LOSS                  PIC SV999   VALUE ZEROS.     EL6504
00099      12  WS-LF-PCT-1                 PIC S9V9999 VALUE ZEROS.        CL**9
00100      12  WS-LF-PCT-2                 PIC S9V9999 VALUE ZEROS.        CL**9
00101      12  WS-LF-PCT-3                 PIC S9V9999 VALUE ZEROS.        CL**9
00102      12  WS-AH-PCT-1                 PIC S9V9999 VALUE ZEROS.        CL**9
00103      12  WS-AH-PCT-2                 PIC S9V9999 VALUE ZEROS.        CL**9
00104      12  WS-AH-PCT-3                 PIC S9V9999 VALUE ZEROS.        CL**9
00105      12  WS-LF-THRU-1                PIC S9(7)   VALUE ZEROS.        CL**9
00106      12  WS-LF-THRU-2                PIC S9(7)   VALUE ZEROS.        CL**9
00107      12  WS-LF-THRU-3                PIC S9(7)   VALUE ZEROS.        CL**9
00108      12  WS-AH-THRU-1                PIC S9(7)   VALUE ZEROS.        CL**9
00109      12  WS-AH-THRU-2                PIC S9(7)   VALUE ZEROS.        CL**9
00110      12  WS-AH-THRU-3                PIC S9(7)   VALUE ZEROS.        CL**9
00111      12  WS-QUALIFY-LIMIT            PIC S9(7)   VALUE ZEROS.        CL**9
00112                                                                   EL6504
00113      EJECT                                                        EL6504
00114      12  ERROR-MESSAGES.                                          EL6504
00115          16  ER-0000                 PIC X(4)    VALUE '0000'.    EL6504
00116          16  ER-0002                 PIC X(4)    VALUE '0002'.    EL6504
00117          16  ER-0004                 PIC X(4)    VALUE '0004'.    EL6504
00118          16  ER-0008                 PIC X(4)    VALUE '0008'.    EL6504
00119          16  ER-0029                 PIC X(4)    VALUE '0029'.    EL6504
00120          16  ER-0068                 PIC X(4)    VALUE '0068'.    EL6504
00121          16  ER-0070                 PIC X(4)    VALUE '0070'.    EL6504
00122          16  ER-0620                 PIC X(4)    VALUE '0620'.    EL6504
00123          16  ER-0621                 PIC X(4)    VALUE '0621'.    EL6504
00124          16  ER-0626                 PIC X(4)    VALUE '0626'.    EL6504
00125          16  ER-2039                 PIC X(4)    VALUE '2039'.    EL6504
00126          16  ER-2081                 PIC X(4)    VALUE '2081'.    EL6504
00127          16  ER-2082                 PIC X(4)    VALUE '2082'.    EL6504
00128          16  ER-2083                 PIC X(4)    VALUE '2083'.    EL6504
00129          16  ER-2084                 PIC X(4)    VALUE '2084'.    EL6504
00130          16  ER-2085                 PIC X(4)    VALUE '2085'.    EL6504
00131          16  ER-2086                 PIC X(4)    VALUE '2086'.    EL6504
00132          16  ER-2098                 PIC X(4)    VALUE '2098'.    EL6504
00133          16  ER-2099                 PIC X(4)    VALUE '2099'.    EL6504
00134          16  ER-2100                 PIC X(4)    VALUE '2100'.    EL6504
00135          16  ER-2101                 PIC X(4)    VALUE '2101'.    EL6504
00136          16  ER-2102                 PIC X(4)    VALUE '2102'.    EL6504
00137          16  ER-2103                 PIC X(4)    VALUE '2103'.    EL6504
00138          16  ER-2161                 PIC X(4)    VALUE '2161'.    EL6504
00139          16  ER-2162                 PIC X(4)    VALUE '2162'.    EL6504
00140          16  ER-2163                 PIC X(4)    VALUE '2163'.    EL6504
00141          16  ER-2164                 PIC X(4)    VALUE '2164'.    EL6504
00142          16  ER-2165                 PIC X(4)    VALUE '2165'.    EL6504
00143          16  ER-2171                 PIC X(4)    VALUE '2171'.    EL6504
00144          16  ER-2572                 PIC X(4)    VALUE '2572'.    EL6504
00145                                                                   EL6504
00146      12  ELCNTL-KEY.                                              EL6504
00147          16  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.    EL6504
00148          16  CNTL-REC-TYPE           PIC X       VALUE SPACES.    EL6504
00149          16  CNTL-ACCESS             PIC X(4)    VALUE SPACES.    EL6504
00150          16  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.  EL6504
00151                                                                   EL6504
00152      EJECT                                                        EL6504
00153                            COPY ELCSCTM.                             CL**9
00154      EJECT                                                        EL6504
00155                            COPY ELCSCRTY.                            CL**9
00156      EJECT                                                        EL6504
00157                            COPY ELCLOGOF.                            CL**9
00158                                                                   EL6504
00159      EJECT                                                        EL6504
00160                            COPY ELCDATE.                             CL**9
00161                                                                   EL6504
00162      EJECT                                                        EL6504
00163                            COPY ELCATTR.                             CL**9
00164                                                                   EL6504
00165      EJECT                                                        EL6504
00166                            COPY ELCEMIB.                             CL**9
00167                                                                   EL6504
00168      EJECT                                                        EL6504
00169                            COPY ELCINTF.                             CL**9
00170                            COPY ELC650PI.                            CL**9
00171                                                                      CL**6
00172                            COPY ELCJPFX.                             CL**9
00173                                      PIC X(2000).                 EL6504
00174                                                                   EL6504
00175      EJECT                                                        EL6504
00176                            COPY ELCAID.                              CL**9
00177  01  FILLER    REDEFINES DFHAID.                                  EL6504
00178      12  FILLER                      PIC X(8).                    EL6504
00179      12  PF-VALUES                   PIC X       OCCURS 2.        EL6504
00180                                                                   EL6504
00181      EJECT                                                        EL6504
00182                            COPY EL6504S.                             CL**9
00183                                                                   EL6504
00184      EJECT                                                        EL6504
00185  LINKAGE SECTION.                                                 EL6504
00186  01  DFHCOMMAREA                     PIC X(1500).                    CL**5
00187                                                                   EL6504
00188      EJECT                                                        EL6504
00189 *01 PARMLIST .                                                       CL*11
00190 *    02  FILLER                      PIC S9(8)   COMP.               CL*11
00191 *    02  ERACCT-POINTER              PIC S9(8)   COMP.               CL*11
00192 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.               CL*11
00193                                                                   EL6504
00194                            COPY ERCACCT.                             CL**9
00195      EJECT                                                        EL6504
00196                            COPY ELCCNTL.                             CL**9
00197      EJECT                                                        EL6504
00198                                                                   EL6504
00199  PROCEDURE DIVISION.                                              EL6504
00200      CONTINUE.                                                       CL*11
00201                                                                   EL6504
00202      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6504
00203      MOVE '5'                    TO  DC-OPTION-CODE.              EL6504
00204      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               EL6504
00205      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL6504
00206      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL6504
00207      MOVE DC-GREG-DATE-1-YMD     TO  YMD-CURRENT-SAVE.            EL6504
00208                                                                   EL6504
00209      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL6504
00210      IF EIBCALEN = 0                                              EL6504
00211          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6504
00212                                                                   EL6504
00213      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6504
00214          IF PI-CALLING-PROGRAM = XCTL-6501                           CL*10
00215              MOVE PI-CALLING-PROGRAM                                 CL*10
00216                                  TO  PI-RETURN-TO-PROGRAM            CL*10
00217              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM              CL*10
00218          ELSE                                                        CL*10
00219              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM.             CL*10
00220                                                                   EL6504
00221      MOVE LOW-VALUES             TO  EL6504AI.                    EL6504
00222                                                                   EL6504
00223      IF EIBTRNID NOT = TRANS-ID                                   EL6504
00224          MOVE PI-MAINT           TO  MAINTYPO                     EL6504
00225          MOVE AL-UANON           TO  MAINTYPA                     EL6504
00226          MOVE -1                 TO  MAINTYPL                     EL6504
00227                                                                   EL6504
00228          IF PI-MAINT = 'S' OR 'C'                                 EL6504
00229              GO TO 4000-SHOW                                      EL6504
00230          ELSE                                                     EL6504
00231              IF PI-MAINT = 'A'                                    EL6504
00232                  MOVE 'C'            TO  PI-MAINT                 EL6504
00233                  GO TO 4000-SHOW                                  EL6504
00234              ELSE                                                 EL6504
00235                  GO TO 8100-SEND-INITIAL-MAP.                     EL6504
00236                                                                   EL6504
00237      EXEC CICS HANDLE CONDITION                                   EL6504
00238          PGMIDERR  (9600-PGMID-ERROR)                             EL6504
00239          ERROR     (9990-ABEND)                                   EL6504
00240      END-EXEC.                                                    EL6504
00241                                                                   EL6504
00242      IF EIBAID = DFHCLEAR                                         EL6504
00243          GO TO 9400-CLEAR.                                        EL6504
00244                                                                   EL6504
00245      EJECT                                                        EL6504
00246  0200-RECEIVE.                                                    EL6504
00247      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6504
00248          MOVE ER-0008            TO  EMI-ERROR                    EL6504
00249          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6504
00250          MOVE -1                 TO  PFENTERL                     EL6504
00251          GO TO 8200-SEND-DATAONLY.                                EL6504
00252                                                                   EL6504
00253      EXEC CICS RECEIVE                                            EL6504
00254          MAP      (MAP-NAME)                                      EL6504
00255          MAPSET   (MAPSET-NAME)                                   EL6504
00256          INTO     (EL6504AI)                                      EL6504
00257      END-EXEC.                                                    EL6504
00258                                                                   EL6504
00259      IF PFENTERL = 0                                              EL6504
00260          GO TO 0300-CHECK-PFKEYS.                                 EL6504
00261      IF EIBAID NOT = DFHENTER                                     EL6504
00262          MOVE ER-0004            TO  EMI-ERROR                    EL6504
00263          GO TO 0320-INPUT-ERROR.                                  EL6504
00264      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)      CL**5
00265          MOVE PF-VALUES (PFENTERI) TO  EIBAID                     EL6504
00266      ELSE                                                         EL6504
00267          MOVE ER-0029            TO  EMI-ERROR                    EL6504
00268          GO TO 0320-INPUT-ERROR.                                  EL6504
00269      EJECT                                                        EL6504
00270  0300-CHECK-PFKEYS.                                               EL6504
00271      IF EIBAID = DFHPF23                                          EL6504
00272          GO TO 8810-PF23.                                         EL6504
00273      IF EIBAID = DFHPF24                                          EL6504
00274          GO TO 9200-RETURN-MAIN-MENU.                             EL6504
00275      IF EIBAID = DFHPF12                                          EL6504
00276          GO TO 9500-PF12.                                         EL6504
00277                                                                      CL**8
00278      IF EIBAID = DFHPF5                                              CL**8
00279          MOVE XCTL-6502          TO  PGM-NAME                        CL**8
00280          GO TO 9300-XCTL.                                            CL**8
00281                                                                      CL**5
00282      IF EIBAID = DFHPF7                                           EL6504
00283          MOVE XCTL-6501          TO  PGM-NAME                     EL6504
00284          GO TO 9300-XCTL.                                         EL6504
00285                                                                      CL**5
00286      IF EIBAID = DFHPF8                                           EL6504
00287          MOVE XCTL-6506          TO  PGM-NAME                     EL6504
00288          GO TO 9300-XCTL.                                            CL**4
00289                                                                      CL**5
00290      IF EIBAID = DFHPF9                                              CL**4
00291          MOVE XCTL-6505          TO  PGM-NAME                        CL**4
00292          GO TO 9300-XCTL.                                         EL6504
00293                                                                      CL**5
00294      IF EIBAID = DFHENTER                                         EL6504
00295          GO TO 0330-CHECK-MAINTYP.                                EL6504
00296                                                                      CL**5
00297      MOVE ER-0029                TO  EMI-ERROR.                   EL6504
00298  0320-INPUT-ERROR.                                                EL6504
00299      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6504
00300      MOVE AL-UNBON               TO  PFENTERA.                    EL6504
00301      MOVE -1                     TO  PFENTERL.                    EL6504
00302      GO TO 8200-SEND-DATAONLY.                                    EL6504
00303                                                                   EL6504
00304  0330-CHECK-MAINTYP.                                              EL6504
00305      IF MAINTYPL GREATER ZERO                                        CL**5
00306          IF MAINTYPI = 'S' OR 'C' OR 'A'                          EL6504
00307              MOVE AL-UANON       TO  MAINTYPA                     EL6504
00308              MOVE MAINTYPI       TO  PI-MAINT                     EL6504
00309          ELSE                                                     EL6504
00310              MOVE -1             TO  MAINTYPL                     EL6504
00311              MOVE AL-UABON       TO  MAINTYPA                     EL6504
00312              MOVE ER-2039        TO  EMI-ERROR                    EL6504
00313              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT             EL6504
00314              GO TO 8200-SEND-DATAONLY                             EL6504
00315      ELSE                                                         EL6504
00316          MOVE -1                 TO  MAINTYPL                     EL6504
00317          MOVE AL-UABON           TO  MAINTYPA                     EL6504
00318          MOVE ER-2039            TO  EMI-ERROR                    EL6504
00319          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6504
00320          GO TO 8200-SEND-DATAONLY.                                EL6504
00321                                                                   EL6504
00322      IF PI-MAINT = 'S'                                            EL6504
00323          GO TO 4000-SHOW.                                         EL6504
00324                                                                   EL6504
00325      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.                EL6504
00326                                                                      CL**5
00327      IF EMI-ERROR NOT = ZEROS                                     EL6504
00328          MOVE -1                 TO  MAINTYPL                     EL6504
00329          GO TO 8200-SEND-DATAONLY.                                EL6504
00330                                                                   EL6504
00331      GO TO 4200-MAINT.                                            EL6504
00332                                                                   EL6504
00333      EJECT                                                        EL6504
00334                                                                   EL6504
00335  4000-SHOW.                                                       EL6504
00336      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6504
00337      MOVE LOW-VALUES             TO  EL6504AO.                    EL6504
00338      GO TO 5000-BUILD-INITIAL-SCREEN.                             EL6504
00339                                                                   EL6504
00340      EJECT                                                        EL6504
00341                                                                   EL6504
00342  4200-MAINT.                                                      EL6504
00343      IF NOT MODIFY-CAP                                            EL6504
00344          MOVE 'UPDATE'            TO SM-READ                         CL**5
00345          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           EL6504
00346          MOVE ER-0070             TO  EMI-ERROR                   EL6504
00347          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6504
00348          GO TO 8100-SEND-INITIAL-MAP.                             EL6504
00349                                                                   EL6504
00350      PERFORM 7000-EDIT THRU 7099-EXIT.                            EL6504
00351                                                                   EL6504
00352      IF EMI-NO-ERRORS                                             EL6504
00353          NEXT SENTENCE                                            EL6504
00354      ELSE                                                         EL6504
00355          GO TO 8200-SEND-DATAONLY.                                EL6504
00356                                                                   EL6504
00357      PERFORM 7300-READ-ERACCT-UPDATE THRU 7300-EXIT.              EL6504
00358                                                                   EL6504
00359      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.              EL6504
00360      MOVE ERACCT-FILE            TO  FILE-ID.                     EL6504
00361                                                                   EL6504
00362      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT.              EL6504
00363                                                                   EL6504
00364      IF AM-LAST-MAINT-USER   = PI-UPDATE-BY OR                       CL**5
00365         AM-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS                   EL6504
00366          NEXT SENTENCE                                            EL6504
00367      ELSE                                                         EL6504
00368          EXEC CICS UNLOCK                                         EL6504
00369               DATASET  (ERACCT-FILE)                              EL6504
00370          END-EXEC                                                 EL6504
00371          MOVE ER-0068            TO  EMI-ERROR                    EL6504
00372          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 EL6504
00373          PERFORM 7100-READ-ERACCT  THRU 7100-EXIT                 EL6504
00374          MOVE LOW-VALUES         TO  EL6504AO                     EL6504
00375          MOVE -1                 TO  MAINTYPL                     EL6504
00376          MOVE 'S'                TO  PI-MAINT                     EL6504
00377          GO TO 5000-BUILD-INITIAL-SCREEN.                         EL6504
00378                                                                   EL6504
00379      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.          EL6504
00380      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.        EL6504
00381      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL6504
00382      MOVE '5'                    TO  DC-OPTION-CODE.              EL6504
00383      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL6504
00384                                                                   EL6504
00385      EXEC CICS LINK                                               EL6504
00386          PROGRAM (PGM-NAME)                                          CL**5
00387          COMMAREA(DATE-CONVERSION-DATA)                           EL6504
00388          LENGTH  (DC-COMM-LENGTH)                                    CL**5
00389      END-EXEC.                                                    EL6504
00390                                                                   EL6504
00391      MOVE DC-BIN-DATE-1          TO  AM-LAST-MAINT-DT             EL6504
00392                                      BIN-CURRENT-SAVE.            EL6504
00393 *    MOVE YMD-CURRENT-SAVE       TO  ACC-LST-75-UPD.              EL6504
00394      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL6504
00395      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6504
00396      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6504
00397      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.              EL6504
00398                                                                   EL6504
00399      EXEC CICS REWRITE                                            EL6504
00400          DATASET  (ERACCT-FILE)                                   EL6504
00401          FROM     (ACCOUNT-MASTER)                                EL6504
00402      END-EXEC.                                                    EL6504
00403                                                                   EL6504
00404      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL6504
00405      MOVE ERACCT-FILE            TO  FILE-ID.                     EL6504
00406      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6504
00407      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6504
00408      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.               EL6504
00409      MOVE ER-0000                TO  EMI-ERROR.                   EL6504
00410      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6504
00411                                                                   EL6504
00412      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.                     EL6504
00413                                                                      CL**5
00414      MOVE LOW-VALUES             TO  EL6504AO.                    EL6504
00415      MOVE 'C'                    TO  PI-MAINT.                    EL6504
00416                                                                   EL6504
00417      EJECT                                                        EL6504
00418  5000-BUILD-INITIAL-SCREEN.                                       EL6504
00419      MOVE AM-REI-PRT-ST          TO  TAXOPTO.                     EL6504
00420      MOVE AM-REI-PRT-OW          TO  PRTOPTO.                     EL6504
00421      MOVE AM-REI-MORT            TO  MORTALO.                     EL6504
00422                                                                   EL6504
00423      MOVE AL-UANON               TO  TAXOPTA                         CL**9
00424                                      PRTOPTA                         CL**9
00425                                      MORTALA.                        CL**9
00426                                                                      CL**9
00427      IF AM-REI-78-PCT NUMERIC                                        CL**9
00428       IF AM-REI-78-PCT NOT = ZEROS                                   CL**9
00429          MOVE AM-REI-78-PCT      TO  ARULEO                       EL6504
00430          MOVE AL-UNNON           TO  ARULEA.                      EL6504
00431                                                                   EL6504
00432      IF AM-REI-PR-PCT NUMERIC                                        CL**9
00433       IF AM-REI-PR-PCT NOT = ZEROS                                   CL**9
00434          MOVE AM-REI-PR-PCT      TO  ARATAO                       EL6504
00435          MOVE AL-UNNON           TO  ARATAA.                      EL6504
00436                                                                   EL6504
00437      IF AM-REI-FEE-LF NUMERIC                                        CL**9
00438       IF AM-REI-FEE-LF NOT = ZEROS                                   CL**9
00439          MOVE AM-REI-FEE-LF      TO  LCFEEO                       EL6504
00440          MOVE AL-UNNON           TO  LCFEEA.                      EL6504
00441                                                                   EL6504
00442      IF AM-REI-PE-LF  NOT = SPACES                                EL6504
00443          MOVE AM-REI-PE-LF       TO  LCBASISO                     EL6504
00444          MOVE AL-UANON           TO  LCBASISA.                    EL6504
00445                                                                   EL6504
00446      IF AM-REI-LF-TAX NUMERIC                                        CL**9
00447       IF AM-REI-LF-TAX NOT = ZEROS                                   CL**9
00448          MOVE AM-REI-LF-TAX      TO  LTAXO                        EL6504
00449          MOVE AL-UNNON           TO  LTAXA.                       EL6504
00450                                                                   EL6504
00451      IF AM-REI-FEE-AH NUMERIC                                        CL**9
00452       IF AM-REI-FEE-AH NOT = ZEROS                                   CL**9
00453          MOVE AM-REI-FEE-AH      TO  ACFEEO                       EL6504
00454          MOVE AL-UNNON           TO  ACFEEA.                      EL6504
00455                                                                   EL6504
00456      IF AM-REI-PE-AH  NOT = SPACES                                EL6504
00457          MOVE AM-REI-PE-AH       TO  ACBASISO                     EL6504
00458          MOVE AL-UANON           TO  ACBASISA.                    EL6504
00459                                                                   EL6504
00460      IF AM-REI-AH-TAX NOT = ZEROS                                 EL6504
00461          MOVE AM-REI-AH-TAX      TO  ATAXO                        EL6504
00462          MOVE AL-UNNON           TO  ATAXA.                       EL6504
00463                                                                   EL6504
00464      IF AM-REI-GROUP-A  NOT = SPACES                              EL6504
00465          MOVE AM-REI-GROUP-A     TO  GROUPAO.                     EL6504
00466          MOVE AL-UANON           TO  GROUPAA.                     EL6504
00467                                                                   EL6504
00468      IF AM-REI-GROUP-B  NOT = SPACES                              EL6504
00469          MOVE AM-REI-GROUP-B     TO  GROUPBO.                     EL6504
00470          MOVE AL-UANON           TO  GROUPBA.                     EL6504
00471                                                                   EL6504
CIDMOD     IF AM-RETRO-PREM-P-E = ZERO                                       000
CIDMOD         MOVE SPACE              TO AM-RETRO-PREM-P-E.                 000
CIDMOD     IF AM-RETRO-CLMS-P-I = ZERO                                       000
CIDMOD         MOVE SPACE              TO AM-RETRO-CLMS-P-I.                 000
CIDMOD                                                                       000
00472      MOVE AM-RET-Y-N             TO  YNRETROO.                       CL**9
00473      MOVE AM-RETRO-PREM-P-E      TO  PERETROO.                       CL**9
00474      MOVE AM-RET-P-E             TO  PECOMMO.                        CL**9
00475      MOVE AM-RETRO-CLMS-P-I      TO  PICLAIMO.                       CL**9
00476                                                                      CL**9
00477      MOVE AL-UANON               TO  YNRETROA                        CL**9
00478                                      PERETROA                        CL**9
00479                                      PECOMMA                         CL**9
00480                                      PICLAIMA.                       CL**9
00481                                                                      CL**9
00482      IF AM-RETRO-POOL NOT = SPACES                                   CL**9
00483          MOVE AM-RETRO-POOL      TO  GPRETROO                        CL**9
00484          MOVE AL-UANON           TO  GPRETROA.                    EL6504
00485                                                                   EL6504
00486      IF AM-RETRO-QUALIFY-LIMIT NOT NUMERIC                           CL**9
00487          MOVE ZEROS              TO  AM-RETRO-QUALIFY-LIMIT.         CL**9
00488                                                                   EL6504
00489      IF AM-RETRO-QUALIFY-LIMIT NOT = ZEROS                           CL**9
00490          MOVE AM-RETRO-QUALIFY-LIMIT                                 CL**9
00491                                  TO  RELIMITO                        CL**9
00492          MOVE AL-UNNON           TO  RELIMITA.                       CL**9
00493                                                                   EL6504
00494      IF AM-RET-MIN-LOSS-L NOT NUMERIC OR                          EL6504
00495         AM-RET-MIN-LOSS-L = ZEROS                                 EL6504
00496          NEXT SENTENCE                                            EL6504
00497      ELSE                                                         EL6504
00498          MOVE AM-RET-MIN-LOSS-L  TO  LFLOSSO                      EL6504
00499          MOVE AL-UNNON           TO  LFLOSSA.                     EL6504
00500                                                                   EL6504
00501      IF AM-RET-MIN-LOSS-A NOT NUMERIC OR                          EL6504
00502         AM-RET-MIN-LOSS-A = ZEROS                                 EL6504
00503          NEXT SENTENCE                                            EL6504
00504      ELSE                                                         EL6504
00505          MOVE AM-RET-MIN-LOSS-A  TO  AHLOSSO                      EL6504
00506          MOVE AL-UNNON           TO  AHLOSSA.                     EL6504
00507                                                                   EL6504
00508      IF AM-RET-EARN-R NOT = SPACES                                EL6504
00509          MOVE AM-RET-EARN-R      TO  REDEARNO                     EL6504
00510          MOVE AL-UANON           TO  REDEARNA.                    EL6504
00511                                                                   EL6504
00512      IF AM-RET-EARN-L NOT = SPACES                                EL6504
00513          MOVE AM-RET-EARN-L      TO  LEVEARNO                     EL6504
00514          MOVE AL-UANON           TO  LEVEARNA.                    EL6504
00515                                                                   EL6504
00516      IF AM-RET-EARN-A NOT = SPACES                                EL6504
00517          MOVE AM-RET-EARN-A      TO  AHEARNO                      EL6504
00518          MOVE AL-UANON           TO  AHEARNA.                     EL6504
00519                                                                   EL6504
00520      IF AM-RET-BEG-EARN-R NOT = SPACES                            EL6504
00521          MOVE AM-RET-BEG-EARN-R  TO  BEGREDO                      EL6504
00522          MOVE AL-UANON           TO  BEGREDA.                     EL6504
00523                                                                   EL6504
00524      IF AM-RET-BEG-EARN-L NOT = SPACES                            EL6504
00525          MOVE AM-RET-BEG-EARN-L  TO  BEGLEVO                      EL6504
00526          MOVE AL-UANON           TO  BEGLEVA.                     EL6504
00527                                                                   EL6504
00528      IF AM-RET-BEG-EARN-A NOT = SPACES                            EL6504
00529          MOVE AM-RET-BEG-EARN-A  TO  BEGAHO                       EL6504
00530          MOVE AL-UANON           TO  BEGAHA.                      EL6504
00531                                                                   EL6504
00532      IF AM-RET-ST-TAX-USE NOT = SPACES                            EL6504
00533          MOVE AM-RET-ST-TAX-USE  TO  TAXOPO                       EL6504
00534          MOVE AL-UANON           TO  TAXOPA.                      EL6504
00535                                                                   EL6504
00536      IF AM-LF-RET NOT = ZEROS                                        CL**9
00537          MOVE AM-LF-RET          TO  LFRETENO                        CL**9
00538          MOVE AL-UNNON           TO  LFRETENA.                       CL**9
00539                                                                      CL**9
00540      IF AM-AH-RET NOT = ZEROS                                        CL**9
00541          MOVE AM-AH-RET          TO  AHRETENO                        CL**9
00542          MOVE AL-UNNON           TO  AHRETENA.                       CL**9
00543                                                                      CL**9
00544 *    MOVE AM-RETRO-RET-METHOD-LF TO  LFMETHDO.                       CL**9
00545 *    MOVE AL-UANON               TO  LFMETHDA.                       CL**9
00546 *    MOVE AM-RETRO-RET-BASIS-LF  TO  LFBASISO.                       CL**9
00547 *    MOVE AL-UANON               TO  LFBASISA.                       CL**9
00548                                                                      CL**9
00549      IF AM-RETRO-RET-PCT-LF (1) NOT NUMERIC                          CL**9
00550          MOVE ZEROS              TO  AM-RETRO-RET-PCT-LF (1).        CL**9
00551      IF AM-RETRO-RET-PCT-LF (2) NOT NUMERIC                          CL**9
00552          MOVE ZEROS              TO  AM-RETRO-RET-PCT-LF (2).        CL**9
00553      IF AM-RETRO-RET-PCT-LF (3) NOT NUMERIC                          CL**9
00554          MOVE ZEROS              TO  AM-RETRO-RET-PCT-LF (3).        CL**9
00555      IF AM-RETRO-RET-THRU-LF (1) NOT NUMERIC                         CL**9
00556          MOVE ZEROS              TO  AM-RETRO-RET-THRU-LF (1).       CL**9
00557      IF AM-RETRO-RET-THRU-LF (2) NOT NUMERIC                         CL**9
00558          MOVE ZEROS              TO  AM-RETRO-RET-THRU-LF (2).       CL**9
00559      IF AM-RETRO-RET-THRU-LF (3) NOT NUMERIC                         CL**9
00560          MOVE ZEROS              TO  AM-RETRO-RET-THRU-LF (3).       CL**9
00561                                                                      CL**9
00562 *    IF AM-RETRO-RET-PCT-LF (1) NOT = ZEROS                          CL**9
00563 *        MOVE AM-RETRO-RET-PCT-LF (1)  TO  LFPCT1O                   CL**9
00564 *        MOVE AL-UNNON                 TO  LFPCT1A.                  CL**9
00565 *                                                                    CL**9
00566 *    IF AM-RETRO-RET-THRU-LF (1) NOT = ZEROS                         CL**9
00567 *        MOVE AM-RETRO-RET-THRU-LF (1) TO  LFTHRU1O                  CL**9
00568 *        MOVE AL-UNNON                 TO  LFTHRU1A.                 CL**9
00569 *                                                                    CL**9
00570 *    IF AM-RETRO-RET-PCT-LF (2) NOT = ZEROS                          CL**9
00571 *        MOVE AM-RETRO-RET-PCT-LF (2)  TO  LFPCT2O                   CL**9
00572 *        MOVE AL-UNNON                 TO  LFPCT2A.                  CL**9
00573 *                                                                    CL**9
00574 *    IF AM-RETRO-RET-THRU-LF (2) NOT = ZEROS                         CL**9
00575 *        MOVE AM-RETRO-RET-THRU-LF (2) TO  LFTHRU2O                  CL**9
00576 *        MOVE AL-UNNON                 TO  LFTHRU2A.                 CL**9
00577 *                                                                    CL**9
00578 *    IF AM-RETRO-RET-PCT-LF (3) NOT = ZEROS                          CL**9
00579 *        MOVE AM-RETRO-RET-PCT-LF (3)  TO  LFPCT3O                   CL**9
00580 *        MOVE AL-UNNON                 TO  LFPCT3A.                  CL**9
00581 *                                                                    CL**9
00582 *    IF AM-RETRO-RET-THRU-LF (3) NOT = ZEROS                         CL**9
00583 *        MOVE AM-RETRO-RET-THRU-LF (3) TO  LFTHRU3O                  CL**9
00584 *        MOVE AL-UNNON                 TO  LFTHRU3A.                 CL**9
00585 *                                                                    CL**9
00586 *    MOVE AM-RETRO-RET-METHOD-AH TO  AHMETHDO.                       CL**9
00587 *    MOVE AL-UANON               TO  AHMETHDA.                       CL**9
00588 *    MOVE AM-RETRO-RET-BASIS-AH  TO  AHBASISO.                       CL**9
00589 *    MOVE AL-UANON               TO  AHBASISA.                       CL**9
00590                                                                      CL**9
00591      IF AM-RETRO-RET-PCT-AH (1) NOT NUMERIC                          CL**9
00592          MOVE ZEROS              TO  AM-RETRO-RET-PCT-AH (1).        CL**9
00593      IF AM-RETRO-RET-PCT-AH (2) NOT NUMERIC                          CL**9
00594          MOVE ZEROS              TO  AM-RETRO-RET-PCT-AH (2).        CL**9
00595      IF AM-RETRO-RET-PCT-AH (3) NOT NUMERIC                          CL**9
00596          MOVE ZEROS              TO  AM-RETRO-RET-PCT-AH (3).        CL**9
00597      IF AM-RETRO-RET-THRU-AH (1) NOT NUMERIC                         CL**9
00598          MOVE ZEROS              TO  AM-RETRO-RET-THRU-AH (1).       CL**9
00599      IF AM-RETRO-RET-THRU-AH (2) NOT NUMERIC                         CL**9
00600          MOVE ZEROS              TO  AM-RETRO-RET-THRU-AH (2).       CL**9
00601      IF AM-RETRO-RET-THRU-AH (3) NOT NUMERIC                         CL**9
00602          MOVE ZEROS              TO  AM-RETRO-RET-THRU-AH (3).       CL**9
00603                                                                      CL**9
00604 *    IF AM-RETRO-RET-PCT-AH (1) NOT = ZEROS                          CL**9
00605 *        MOVE AM-RETRO-RET-PCT-AH (1)  TO  AHPCT1O                   CL**9
00606 *        MOVE AL-UNNON                 TO  AHPCT1A.                  CL**9
00607 *                                                                    CL**9
00608 *    IF AM-RETRO-RET-THRU-AH (1) NOT = ZEROS                         CL**9
00609 *        MOVE AM-RETRO-RET-THRU-AH (1) TO  AHTHRU1O                  CL**9
00610 *        MOVE AL-UNNON                 TO  AHTHRU1A.                 CL**9
00611 *                                                                    CL**9
00612 *    IF AM-RETRO-RET-PCT-AH (2) NOT = ZEROS                          CL**9
00613 *        MOVE AM-RETRO-RET-PCT-AH (2)  TO  AHPCT2O                   CL**9
00614 *        MOVE AL-UNNON                 TO  AHPCT2A.                  CL**9
00615 *                                                                    CL**9
00616 *    IF AM-RETRO-RET-THRU-AH (2) NOT = ZEROS                         CL**9
00617 *        MOVE AM-RETRO-RET-THRU-AH (2) TO  AHTHRU2O                  CL**9
00618 *        MOVE AL-UNNON                 TO  AHTHRU2A.                 CL**9
00619 *                                                                    CL**9
00620 *    IF AM-RETRO-RET-PCT-AH (3) NOT = ZEROS                          CL**9
00621 *        MOVE AM-RETRO-RET-PCT-AH (3)  TO  AHPCT3O                   CL**9
00622 *        MOVE AL-UNNON                 TO  AHPCT3A.                  CL**9
00623 *                                                                    CL**9
00624 *    IF AM-RETRO-RET-THRU-AH (3) NOT = ZEROS                         CL**9
00625 *        MOVE AM-RETRO-RET-THRU-AH (3) TO  AHTHRU3O                  CL**9
00626 *        MOVE AL-UNNON                 TO  AHTHRU3A.                 CL**9
00627                                                                      CL**9
00628                                                                      CL**5
00629      IF AM-FLI-BANK-BALANCE    NUMERIC AND                           CL**5
00630         AM-FLI-BANK-1ST-6-PREM NUMERIC AND                        EL6504
00631         AM-FLI-BANK-CAP-AMT    NUMERIC                               CL**5
00632            MOVE AL-SADOF            TO  COM1A                        CL**5
00633                                         COM2A                        CL**5
00634                                         COM3A                        CL**5
00635                                         COM4A                        CL**5
00636                                         COM5A                        CL**5
00637          ELSE                                                        CL**5
00638            MOVE AM-COMMENT-LINE (1)  TO  COM1O                       CL**5
00639            MOVE AM-COMMENT-LINE (2)  TO  COM2O                       CL**5
00640            MOVE AM-COMMENT-LINE (3)  TO  COM3O                       CL**5
00641            MOVE AM-COMMENT-LINE (4)  TO  COM4O                       CL**5
00642            MOVE AM-COMMENT-LINE (5)  TO  COM5O                       CL**5
00643            MOVE AL-UANON             TO  COM1A                       CL**5
00644                                          COM2A                    EL6504
00645                                          COM3A                    EL6504
00646                                          COM4A                    EL6504
00647                                          COM5A.                   EL6504
00648                                                                   EL6504
00649      MOVE PI-MAINT               TO  MAINTYPO.                    EL6504
00650      MOVE AL-UANON               TO  MAINTYPA.                    EL6504
00651      MOVE -1                     TO  MAINTYPL.                    EL6504
00652                                                                   EL6504
00653      GO TO 8100-SEND-INITIAL-MAP.                                 EL6504
00654                                                                   EL6504
00655  5099-EXIT.                                                       EL6504
00656      EXIT.                                                        EL6504
00657      EJECT                                                        EL6504
00658  6000-CHECK-FOR-UPDATE.                                           EL6504
00659      IF LCFEEL GREATER ZERO                                       EL6504
00660          MOVE WS-LF-FEE          TO  AM-REI-FEE-LF.               EL6504
00661 *        MOVE LCFEEI             TO  AM-REI-FEE-LF.               EL6504
00662                                                                   EL6504
00663      IF LCBASISL GREATER ZERO                                     EL6504
00664          MOVE LCBASISI           TO  AM-REI-PE-LF.                EL6504
00665                                                                   EL6504
00666      IF LTAXL GREATER ZERO                                        EL6504
00667          MOVE WS-LF-TAX          TO  AM-REI-LF-TAX.               EL6504
00668 *        MOVE LTAXI              TO  AM-REI-LF-TAX.               EL6504
00669                                                                   EL6504
00670      IF ACFEEL GREATER ZERO                                       EL6504
00671          MOVE WS-AH-FEE          TO  AM-REI-FEE-AH.               EL6504
00672 *        MOVE ACFEEI             TO  AM-REI-FEE-AH.               EL6504
00673                                                                   EL6504
00674      IF ACBASISL GREATER ZERO                                     EL6504
00675          MOVE ACBASISI           TO  AM-REI-PE-AH.                EL6504
00676                                                                   EL6504
00677      IF ATAXL GREATER ZERO                                        EL6504
00678          MOVE WS-AH-TAX          TO  AM-REI-AH-TAX.               EL6504
00679 *        MOVE ATAXI              TO  AM-REI-AH-TAX.               EL6504
00680                                                                   EL6504
00681      IF ARULEL GREATER ZERO                                       EL6504
00682          MOVE WS-AH-R78          TO  AM-REI-78-PCT.               EL6504
00683 *        MOVE ARULEI             TO  AM-REI-78-PCT.               EL6504
00684                                                                   EL6504
00685      IF ARATAL GREATER ZERO                                       EL6504
00686          MOVE WS-AH-PRATA        TO  AM-REI-PR-PCT.               EL6504
00687 *        MOVE ARATAI             TO  AM-REI-PR-PCT.               EL6504
00688                                                                   EL6504
00689      IF TAXOPTL GREATER ZERO                                      EL6504
00690          MOVE TAXOPTI            TO  AM-REI-PRT-ST.               EL6504
00691                                                                   EL6504
00692      IF PRTOPTL GREATER ZERO                                      EL6504
00693          MOVE PRTOPTI            TO  AM-REI-PRT-OW.               EL6504
00694                                                                   EL6504
00695      IF GROUPAL GREATER ZERO                                      EL6504
00696          MOVE GROUPAI            TO  AM-REI-GROUP-A.              EL6504
00697                                                                   EL6504
00698      IF GROUPBL GREATER ZERO                                      EL6504
00699          MOVE GROUPBI            TO  AM-REI-GROUP-B.              EL6504
00700                                                                   EL6504
00701      IF MORTALL GREATER ZERO                                      EL6504
00702          MOVE MORTALI            TO  AM-REI-MORT.                 EL6504
00703                                                                   EL6504
00704      IF YNRETROL GREATER ZERO                                        CL**5
00705         MOVE YNRETROI            TO  AM-RET-Y-N.                  EL6504
00706                                                                   EL6504
00707      IF PERETROL GREATER ZERO                                        CL**5
00708         MOVE PERETROI            TO  AM-RETRO-PREM-P-E.              CL**9
00709                                                                   EL6504
00710      IF PECOMML GREATER ZERO                                         CL**9
00711         MOVE PECOMMI             TO  AM-RET-P-E.                     CL**9
00712                                                                   EL6504
00713      IF PICLAIML GREATER ZERO                                        CL**9
00714         MOVE PICLAIMI            TO  AM-RETRO-CLMS-P-I.              CL**9
00715                                                                   EL6504
00716      IF GPRETROL GREATER ZERO                                        CL**5
00717         MOVE GPRETROI            TO  AM-RETRO-POOL.                  CL**9
00718                                                                      CL**9
00719      IF RELIMITL GREATER ZERO                                        CL**9
00720         MOVE WS-QUALIFY-LIMIT    TO  AM-RETRO-QUALIFY-LIMIT.         CL**9
00721                                                                   EL6504
00722      IF LFLOSSL GREATER ZERO                                         CL**5
00723          MOVE WS-LF-LOSS         TO  AM-RET-MIN-LOSS-L.           EL6504
00724 *        MOVE LFLOSSI            TO  AM-RET-MIN-LOSS-L.           EL6504
00725                                                                   EL6504
00726      IF AHLOSSL GREATER ZERO                                         CL**5
00727          MOVE WS-AH-LOSS         TO  AM-RET-MIN-LOSS-A.           EL6504
00728 *        MOVE AHLOSSI            TO  AM-RET-MIN-LOSS-A.           EL6504
00729                                                                   EL6504
00730      IF REDEARNL GREATER ZERO                                        CL**5
00731          MOVE REDEARNI           TO  AM-RET-EARN-R.               EL6504
00732                                                                   EL6504
00733      IF LEVEARNL GREATER ZERO                                        CL**5
00734          MOVE LEVEARNI           TO  AM-RET-EARN-L.               EL6504
00735                                                                   EL6504
00736      IF AHEARNL GREATER ZERO                                         CL**5
00737          MOVE AHEARNI            TO  AM-RET-EARN-A.               EL6504
00738                                                                   EL6504
00739      IF BEGREDL GREATER ZERO                                         CL**5
00740          MOVE BEGREDI            TO  AM-RET-BEG-EARN-R.           EL6504
00741                                                                   EL6504
00742      IF BEGLEVL GREATER ZERO                                         CL**5
00743          MOVE BEGLEVI            TO  AM-RET-BEG-EARN-L.           EL6504
00744                                                                   EL6504
00745      IF BEGAHL GREATER ZERO                                          CL**5
00746          MOVE BEGAHI             TO  AM-RET-BEG-EARN-A.           EL6504
00747                                                                   EL6504
00748      IF TAXOPL GREATER ZERO                                          CL**5
00749          MOVE TAXOPI             TO  AM-RET-ST-TAX-USE.           EL6504
00750                                                                   EL6504
00751      IF LFRETENL GREATER ZERO                                        CL**9
00752         MOVE WS-LF-RET           TO  AM-LF-RET.                      CL**9
00753 *       MOVE LFRETENI            TO  AM-LF-RET.                      CL**9
00754                                                                      CL**9
00755 *    IF LFMETHDL GREATER ZERO                                        CL**9
00756 *        MOVE LFMETHDI           TO  AM-RETRO-RET-METHOD-LF.         CL**9
00757 *                                                                    CL**9
00758 *    IF LFBASISL GREATER ZERO                                        CL**9
00759 *        MOVE LFBASISI           TO  AM-RETRO-RET-BASIS-LF.          CL**9
00760 *                                                                    CL**9
00761 *    IF LFPCT1L GREATER ZERO                                         CL**9
00762 *        MOVE WS-LF-PCT-1        TO  AM-RETRO-RET-PCT-LF (1).        CL**9
00763 *                                                                    CL**9
00764 *    IF LFTHRU1L GREATER ZERO                                        CL**9
00765 *        MOVE WS-LF-THRU-1       TO  AM-RETRO-RET-THRU-LF (1).       CL**9
00766 *                                                                    CL**9
00767 *    IF LFPCT2L GREATER ZERO                                         CL**9
00768 *        MOVE WS-LF-PCT-2        TO  AM-RETRO-RET-PCT-LF (2).        CL**9
00769 *                                                                    CL**9
00770 *    IF LFTHRU2L GREATER ZERO                                        CL**9
00771 *        MOVE WS-LF-THRU-2       TO  AM-RETRO-RET-THRU-LF (2).       CL**9
00772 *                                                                    CL**9
00773 *    IF LFPCT3L GREATER ZERO                                         CL**9
00774 *        MOVE WS-LF-PCT-3        TO  AM-RETRO-RET-PCT-LF (3).        CL**9
00775 *                                                                    CL**9
00776 *    IF LFTHRU3L GREATER ZERO                                        CL**9
00777 *        MOVE WS-LF-THRU-3       TO  AM-RETRO-RET-THRU-LF (3).       CL**9
00778                                                                      CL**9
00779      IF AHRETENL GREATER ZERO                                        CL**9
00780         MOVE WS-AH-RET           TO  AM-AH-RET.                      CL**9
00781 *       MOVE AHRETENI            TO  AM-AH-RET.                      CL**9
00782                                                                      CL**9
00783 *    IF AHMETHDL GREATER ZERO                                        CL**9
00784 *        MOVE AHMETHDI           TO  AM-RETRO-RET-METHOD-AH.         CL**9
00785 *                                                                    CL**9
00786 *    IF AHBASISL GREATER ZERO                                        CL**9
00787 *        MOVE AHBASISI           TO  AM-RETRO-RET-BASIS-AH.          CL**9
00788 *                                                                    CL**9
00789 *    IF AHPCT1L GREATER ZERO                                         CL**9
00790 *        MOVE WS-AH-PCT-1        TO  AM-RETRO-RET-PCT-AH (1).        CL**9
00791 *                                                                    CL**9
00792 *    IF AHTHRU1L GREATER ZERO                                        CL**9
00793 *        MOVE WS-AH-THRU-1       TO  AM-RETRO-RET-THRU-AH (1).       CL**9
00794 *                                                                    CL**9
00795 *    IF AHPCT2L GREATER ZERO                                         CL**9
00796 *        MOVE WS-AH-PCT-2        TO  AM-RETRO-RET-PCT-AH (2).        CL**9
00797 *                                                                    CL**9
00798 *    IF AHTHRU2L GREATER ZERO                                        CL**9
00799 *        MOVE WS-AH-THRU-2       TO  AM-RETRO-RET-THRU-AH (2).       CL**9
00800 *                                                                    CL**9
00801 *    IF AHPCT3L GREATER ZERO                                         CL**9
00802 *        MOVE WS-AH-PCT-3        TO  AM-RETRO-RET-PCT-AH (3).        CL**9
00803 *                                                                    CL**9
00804 *    IF AHTHRU3L GREATER ZERO                                        CL**9
00805 *        MOVE WS-AH-THRU-3       TO  AM-RETRO-RET-THRU-AH (3).       CL**9
00806 *                                                                    CL**9
00807      IF COM1L GREATER ZERO                                        EL6504
00808          MOVE COM1I              TO  AM-COMMENT-LINE (1).         EL6504
00809                                                                   EL6504
00810      IF COM2L GREATER ZERO                                        EL6504
00811          MOVE COM2I              TO  AM-COMMENT-LINE (2).         EL6504
00812                                                                   EL6504
00813      IF COM3L GREATER ZERO                                        EL6504
00814          MOVE COM3I              TO  AM-COMMENT-LINE (3).         EL6504
00815                                                                   EL6504
00816      IF COM4L GREATER ZERO                                        EL6504
00817          MOVE COM4I              TO  AM-COMMENT-LINE (4).         EL6504
00818                                                                   EL6504
00819      IF COM5L GREATER ZERO                                        EL6504
00820          MOVE COM5I              TO  AM-COMMENT-LINE (5).         EL6504
00821                                                                   EL6504
00822                                                                   EL6504
00823  6049-EXIT.                                                       EL6504
00824      EXIT.                                                        EL6504
00825      EJECT                                                        EL6504
00826  7000-EDIT.                                                       EL6504
00827      IF LCFEEL GREATER ZERO                                       EL6504
00828          MOVE LCFEEI                 TO  DEEDIT-FIELD             EL6504
00829          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6504
00830          IF DEEDIT-FIELD-V4 NUMERIC                               EL6504
00831              MOVE DEEDIT-FIELD-V4    TO  WS-LF-FEE                EL6504
00832              MOVE AL-UNNON           TO  LCFEEA                   EL6504
00833          ELSE                                                     EL6504
00834              MOVE -1                 TO  LCFEEL                   EL6504
00835              MOVE AL-UNBON           TO  LCFEEA                   EL6504
00836              MOVE ER-2081            TO  EMI-ERROR                EL6504
00837              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00838                                                                   EL6504
00839      IF LCBASISL GREATER ZERO                                     EL6504
00840          IF LCBASISI = SPACES                                     EL6504
00841              NEXT SENTENCE                                        EL6504
00842          ELSE                                                     EL6504
00843              IF LCBASISI = 'P' OR 'E' OR ' '                      EL6504
00844                  MOVE AL-UANON       TO  LCBASISA                 EL6504
00845              ELSE                                                 EL6504
00846                  MOVE -1             TO  LCBASISL                 EL6504
00847                  MOVE AL-UABON       TO  LCBASISA                 EL6504
00848                  MOVE ER-2098        TO  EMI-ERROR                EL6504
00849                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL6504
00850                                                                   EL6504
00851      IF LTAXL GREATER ZERO                                        EL6504
00852          MOVE LTAXI                  TO  DEEDIT-FIELD             EL6504
00853          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6504
00854          IF DEEDIT-FIELD-V4 NUMERIC                               EL6504
00855              MOVE DEEDIT-FIELD-V4    TO  WS-LF-TAX                EL6504
00856              MOVE AL-UNNON           TO  LTAXA                    EL6504
00857          ELSE                                                     EL6504
00858              MOVE -1                 TO  LTAXL                    EL6504
00859              MOVE AL-UNBON           TO  LTAXA                    EL6504
00860              MOVE ER-2082            TO  EMI-ERROR                EL6504
00861              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00862                                                                   EL6504
00863      IF TAXOPTL GREATER ZERO                                      EL6504
00864          IF TAXOPTI = 'Y' OR 'N' OR ' ' OR 'F'                       CL**3
00865              MOVE AL-UANON       TO  TAXOPTA                      EL6504
00866          ELSE                                                     EL6504
00867              MOVE -1             TO  TAXOPTL                      EL6504
00868              MOVE AL-UABON       TO  TAXOPTA                      EL6504
00869              MOVE ER-2100        TO  EMI-ERROR                    EL6504
00870              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00871                                                                   EL6504
00872      IF PRTOPTL GREATER ZERO                                      EL6504
00873          IF PRTOPTI = 'A' OR 'F' OR 'Y' OR ' '                    EL6504
00874              MOVE AL-UANON       TO  PRTOPTA                      EL6504
00875          ELSE                                                     EL6504
00876              MOVE -1             TO  PRTOPTL                      EL6504
00877              MOVE AL-UABON       TO  PRTOPTA                      EL6504
00878              MOVE ER-2101        TO  EMI-ERROR                    EL6504
00879              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00880                                                                   EL6504
00881      IF GROUPAL GREATER ZERO                                      EL6504
00882          MOVE AL-UANON           TO  GROUPAA.                     EL6504
00883                                                                   EL6504
00884      IF GROUPBL GREATER ZERO                                      EL6504
00885          MOVE AL-UANON           TO  GROUPBA.                     EL6504
00886                                                                   EL6504
00887      IF MORTALL GREATER ZERO                                      EL6504
00888          PERFORM 7400-EDIT-MORTALITY                              EL6504
00889             THRU 7499-EXIT.                                       EL6504
00890                                                                   EL6504
00891      IF ACFEEL GREATER ZERO                                       EL6504
00892          MOVE ACFEEI                 TO  DEEDIT-FIELD             EL6504
00893          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6504
00894          IF DEEDIT-FIELD-V4 NUMERIC                               EL6504
00895              MOVE DEEDIT-FIELD-V4    TO  WS-AH-FEE                EL6504
00896              MOVE AL-UNNON           TO  ACFEEA                   EL6504
00897          ELSE                                                     EL6504
00898              MOVE -1                 TO  ACFEEL                   EL6504
00899              MOVE AL-UNBON           TO  ACFEEA                   EL6504
00900              MOVE ER-2083            TO  EMI-ERROR                EL6504
00901              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00902                                                                   EL6504
00903      IF ACBASISL GREATER ZERO                                     EL6504
00904          IF ACBASISI = SPACES                                     EL6504
00905              NEXT SENTENCE                                        EL6504
00906          ELSE                                                     EL6504
00907              IF ACBASISI = 'P' OR 'E' OR ' '                      EL6504
00908                  MOVE AL-UANON       TO  ACBASISA                 EL6504
00909              ELSE                                                 EL6504
00910                  MOVE -1             TO  ACBASISL                 EL6504
00911                  MOVE AL-UABON       TO  ACBASISA                 EL6504
00912                  MOVE ER-2099        TO  EMI-ERROR                EL6504
00913                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.        EL6504
00914                                                                   EL6504
00915      IF ATAXL GREATER ZERO                                        EL6504
00916          MOVE ATAXI                  TO  DEEDIT-FIELD             EL6504
00917          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6504
00918          IF DEEDIT-FIELD-V4 NUMERIC                               EL6504
00919              MOVE DEEDIT-FIELD-V4    TO  WS-AH-TAX                EL6504
00920              MOVE AL-UNNON           TO  ATAXA                    EL6504
00921          ELSE                                                     EL6504
00922              MOVE -1                 TO  ATAXL                    EL6504
00923              MOVE AL-UNBON           TO  ATAXA                    EL6504
00924              MOVE ER-2084            TO  EMI-ERROR                EL6504
00925              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00926                                                                   EL6504
00927      IF ARULEL GREATER ZERO                                       EL6504
00928          MOVE ARULEI                 TO  DEEDIT-FIELD             EL6504
00929          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6504
00930          IF DEEDIT-FIELD-V4 NUMERIC                               EL6504
00931              MOVE DEEDIT-FIELD-V4    TO  WS-AH-R78                EL6504
00932              MOVE AL-UNNON           TO  ARULEA                   EL6504
00933              ADD WS-AH-R78           TO  WS-TOT-PERCENT           EL6504
00934          ELSE                                                     EL6504
00935              MOVE -1                 TO  ARULEL                   EL6504
00936              MOVE AL-UNBON           TO  ARULEA                   EL6504
00937              MOVE ER-2085            TO  EMI-ERROR                EL6504
00938              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00939                                                                   EL6504
00940      IF ARATAL GREATER ZERO                                       EL6504
00941          MOVE ARATAI                 TO  DEEDIT-FIELD             EL6504
00942          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6504
00943          IF DEEDIT-FIELD-V4 NUMERIC                               EL6504
00944              MOVE DEEDIT-FIELD-V4    TO  WS-AH-PRATA              EL6504
00945              MOVE AL-UNNON           TO  ARATAA                   EL6504
00946              ADD WS-AH-PRATA         TO  WS-TOT-PERCENT           EL6504
00947              IF WS-TOT-PERCENT = +1 OR ZERO                       EL6504
00948                  NEXT SENTENCE                                    EL6504
00949              ELSE                                                 EL6504
00950                  MOVE -1             TO  ARULEL                   EL6504
00951                  MOVE AL-UNBON       TO  ARULEA                   EL6504
00952                                          ARATAA                   EL6504
00953                  MOVE ER-2102        TO  EMI-ERROR                EL6504
00954                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6504
00955          ELSE                                                     EL6504
00956              MOVE -1             TO  ARATAL                       EL6504
00957              MOVE AL-UNBON       TO  ARATAA                       EL6504
00958              MOVE ER-2086        TO  EMI-ERROR                    EL6504
00959              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00960                                                                   EL6504
00961      IF YNRETROL GREATER ZERO                                        CL**5
00962          IF YNRETROI = 'Y' OR 'N' OR 'Q' OR 'S' OR                EL6504
00963                        'G' OR 'I' OR ' '                          EL6504
00964              NEXT SENTENCE                                        EL6504
00965          ELSE                                                     EL6504
00966              MOVE -1             TO  YNRETROL                     EL6504
00967              MOVE AL-UABON       TO  YNRETROA                     EL6504
00968              MOVE ER-2161        TO  EMI-ERROR                    EL6504
00969              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00970                                                                   EL6504
00971      IF PECOMML GREATER ZERO                                         CL**9
00972          IF PECOMMI = 'P' OR 'E' OR ' '                              CL**9
00973              NEXT SENTENCE                                           CL**9
00974          ELSE                                                        CL**9
00975              MOVE ER-2162        TO  EMI-ERROR                       CL**9
00976              MOVE -1             TO  PECOMML                         CL**9
00977              MOVE AL-UABON       TO  PECOMMA                         CL**9
00978              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                CL**9
00979      ELSE                                                            CL**9
00980          IF YNRETROI NOT = 'N'                                       CL**9
00981              MOVE ER-2171        TO  EMI-ERROR                       CL**9
00982              MOVE -1             TO  PECOMML                         CL**9
00983              MOVE AL-UABON       TO  PECOMMA                         CL**9
00984              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
00985                                                                      CL**9
00986      IF PERETROL GREATER ZERO                                        CL**5
00987          IF PERETROI = 'P' OR 'E' OR ' '                          EL6504
00988              NEXT SENTENCE                                        EL6504
00989          ELSE                                                     EL6504
00990              MOVE ER-2162        TO  EMI-ERROR                    EL6504
00991              MOVE -1             TO  PERETROL                     EL6504
00992              MOVE AL-UABON       TO  PERETROA                     EL6504
00993              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
00994                                                                   EL6504
00995      IF PICLAIML GREATER ZERO                                        CL**9
00996          IF PICLAIMI = 'P' OR 'I' OR ' '                             CL**9
00997              NEXT SENTENCE                                           CL**9
00998          ELSE                                                     EL6504
00999              MOVE ER-2162        TO  EMI-ERROR                       CL**9
01000              MOVE -1             TO  PICLAIML                        CL**9
01001              MOVE AL-UABON       TO  PICLAIMA                        CL**9
01002              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01003                                                                   EL6504
01004      IF GPRETROL GREATER ZERO                                     EL6504
01005          MOVE AL-UANON           TO  GPRETROA.                    EL6504
01006                                                                      CL**9
01007      IF RELIMITL GREATER ZERO                                        CL**9
01008          MOVE RELIMITI               TO  DEEDIT-FIELD                CL**9
01009          PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01010          IF DEEDIT-FIELD-V0 NUMERIC                                  CL**9
01011              MOVE DEEDIT-FIELD-V0    TO  WS-QUALIFY-LIMIT            CL**9
01012              MOVE AL-UNNON           TO  RELIMITA                    CL**9
01013          ELSE                                                        CL**9
01014              MOVE -1                 TO  RELIMITL                    CL**9
01015              MOVE AL-UNBON           TO  RELIMITA                    CL**9
01016              MOVE ER-0620            TO  EMI-ERROR                   CL**9
01017              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01018                                                                   EL6504
01019      IF LFLOSSL GREATER ZERO                                      EL6504
01020          MOVE LFLOSSI                TO  DEEDIT-FIELD             EL6504
01021          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6504
01022          IF DEEDIT-FIELD-V3 NUMERIC                               EL6504
01023              MOVE DEEDIT-FIELD-V3    TO  WS-LF-LOSS               EL6504
01024              MOVE AL-UNNON           TO  LFLOSSA                  EL6504
01025          ELSE                                                     EL6504
01026              MOVE -1                 TO  LFLOSSL                  EL6504
01027              MOVE AL-UNBON           TO  LFLOSSA                  EL6504
01028              MOVE ER-0620            TO  EMI-ERROR                EL6504
01029              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01030                                                                   EL6504
01031      IF AHLOSSL GREATER ZERO                                      EL6504
01032          MOVE AHLOSSI                TO  DEEDIT-FIELD             EL6504
01033          PERFORM 7200-DEEDIT THRU 7200-EXIT                       EL6504
01034          IF DEEDIT-FIELD-V3 NUMERIC                               EL6504
01035              MOVE DEEDIT-FIELD-V3    TO  WS-AH-LOSS               EL6504
01036              MOVE AL-UNNON           TO  AHLOSSA                  EL6504
01037          ELSE                                                     EL6504
01038              MOVE -1                 TO  AHLOSSL                  EL6504
01039              MOVE AL-UNBON           TO  AHLOSSA                  EL6504
01040              MOVE ER-0621            TO  EMI-ERROR                EL6504
01041              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01042                                                                   EL6504
01043      IF REDEARNL GREATER ZERO                                     EL6504
01044          MOVE REDEARNI           TO  WS-VALID-EARNINGS            EL6504
01045          IF VALID-EARNINGS                                        EL6504
01046              MOVE AL-UANON       TO  REDEARNA                     EL6504
01047          ELSE                                                     EL6504
01048              MOVE -1             TO  REDEARNL                     EL6504
01049              MOVE AL-UABON       TO  REDEARNA                     EL6504
01050              MOVE ER-2165        TO  EMI-ERROR                    EL6504
01051              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01052                                                                   EL6504
01053      IF LEVEARNL GREATER ZERO                                     EL6504
01054          MOVE LEVEARNI           TO  WS-VALID-EARNINGS            EL6504
01055          IF VALID-EARNINGS                                        EL6504
01056              MOVE AL-UANON       TO  LEVEARNA                     EL6504
01057          ELSE                                                     EL6504
01058              MOVE -1             TO  LEVEARNL                     EL6504
01059              MOVE AL-UABON       TO  LEVEARNA                     EL6504
01060              MOVE ER-2165        TO  EMI-ERROR                    EL6504
01061              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01062                                                                   EL6504
01063      IF AHEARNL GREATER ZERO                                      EL6504
01064          MOVE AHEARNI            TO  WS-VALID-EARNINGS            EL6504
01065          IF VALID-EARNINGS                                        EL6504
01066              MOVE AL-UANON       TO  AHEARNA                      EL6504
01067          ELSE                                                     EL6504
01068              MOVE -1             TO  AHEARNL                      EL6504
01069              MOVE AL-UABON       TO  AHEARNA                      EL6504
01070              MOVE ER-2165        TO  EMI-ERROR                    EL6504
01071              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01072                                                                   EL6504
01073      IF BEGREDL GREATER ZERO                                      EL6504
01074          MOVE BEGREDI            TO  WS-VALID-EARNINGS            EL6504
01075          IF VALID-EARNINGS                                        EL6504
01076              MOVE AL-UANON       TO  BEGREDA                      EL6504
01077          ELSE                                                     EL6504
01078              MOVE -1             TO  BEGREDL                      EL6504
01079              MOVE AL-UABON       TO  BEGREDA                      EL6504
01080              MOVE ER-2165        TO  EMI-ERROR                    EL6504
01081              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01082                                                                   EL6504
01083      IF BEGLEVL GREATER ZERO                                      EL6504
01084          MOVE BEGLEVI            TO  WS-VALID-EARNINGS            EL6504
01085          IF VALID-EARNINGS                                        EL6504
01086              MOVE AL-UANON       TO  BEGLEVA                      EL6504
01087          ELSE                                                     EL6504
01088              MOVE -1             TO  BEGLEVL                      EL6504
01089              MOVE AL-UABON       TO  BEGLEVA                      EL6504
01090              MOVE ER-2165        TO  EMI-ERROR                    EL6504
01091              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01092                                                                   EL6504
01093      IF BEGAHL GREATER ZERO                                       EL6504
01094          MOVE BEGAHI             TO  WS-VALID-EARNINGS            EL6504
01095          IF VALID-EARNINGS                                        EL6504
01096              MOVE AL-UANON       TO  BEGAHA                       EL6504
01097          ELSE                                                     EL6504
01098              MOVE -1             TO  BEGAHL                       EL6504
01099              MOVE AL-UABON       TO  BEGAHA                       EL6504
01100              MOVE ER-2165        TO  EMI-ERROR                    EL6504
01101              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01102                                                                   EL6504
01103      IF TAXOPL GREATER ZERO                                       EL6504
01104          IF TAXOPI = 'P' OR 'E' OR 'Y' OR ' '                     EL6504
01105              MOVE AL-UANON       TO  TAXOPA                       EL6504
01106          ELSE                                                     EL6504
01107              MOVE -1             TO  TAXOPL                       EL6504
01108              MOVE AL-UABON       TO  TAXOPA                       EL6504
01109              MOVE ER-0626        TO  EMI-ERROR                    EL6504
01110              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.            EL6504
01111                                                                   EL6504
01112      IF LFRETENL GREATER ZERO                                        CL**9
01113          MOVE LFRETENI               TO  DEEDIT-FIELD                CL**9
01114          PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01115          IF DEEDIT-FIELD-V4 NUMERIC                                  CL**9
01116              MOVE DEEDIT-FIELD-V4    TO  WS-LF-RET                   CL**9
01117              IF WS-LF-RET GREATER 1                                  CL**9
01118                  MOVE -1             TO  LFRETENL                    CL**9
01119                  MOVE AL-UNBON       TO  LFRETENA                    CL**9
01120                  MOVE ER-2163        TO  EMI-ERROR                   CL**9
01121                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**9
01122              ELSE                                                    CL**9
01123                  MOVE AL-UNNON       TO  LFRETENA                    CL**9
01124          ELSE                                                        CL**9
01125              MOVE -1             TO  LFRETENL                        CL**9
01126              MOVE AL-UNBON       TO  LFRETENA                        CL**9
01127              MOVE ER-2163        TO  EMI-ERROR                       CL**9
01128              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01129                                                                      CL**9
01130 *    IF LFMETHDL GREATER ZERO                                        CL**9
01131 *        IF LFMETHDI = 'P' OR 'S' OR ' '                             CL**9
01132 *            NEXT SENTENCE                                           CL**9
01133 *        ELSE                                                        CL**9
01134 *            MOVE ER-2162        TO  EMI-ERROR                       CL**9
01135 *            MOVE -1             TO  LFMETHDL                        CL**9
01136 *            MOVE AL-UABON       TO  LFMETHDA                        CL**9
01137 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01138 *                                                                    CL**9
01139 *    IF LFBASISL GREATER ZERO                                        CL**9
01140 *        IF LFBASISI = 'P' OR 'E' OR ' '                             CL**9
01141 *            NEXT SENTENCE                                           CL**9
01142 *        ELSE                                                        CL**9
01143 *            MOVE ER-2162        TO  EMI-ERROR                       CL**9
01144 *            MOVE -1             TO  LFBASISL                        CL**9
01145 *            MOVE AL-UABON       TO  LFBASISA                        CL**9
01146 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01147 *                                                                    CL**9
01148 *    IF LFPCT1L GREATER ZERO                                         CL**9
01149 *        MOVE LFPCT1I                TO  DEEDIT-FIELD                CL**9
01150 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01151 *        IF DEEDIT-FIELD-V4 NUMERIC                                  CL**9
01152 *            MOVE DEEDIT-FIELD-V4    TO  WS-LF-PCT-1                 CL**9
01153 *            IF WS-LF-PCT-1 GREATER 1                                CL**9
01154 *                MOVE -1             TO  LFPCT1L                     CL**9
01155 *                MOVE AL-UNBON       TO  LFPCT1A                     CL**9
01156 *                MOVE ER-2163        TO  EMI-ERROR                   CL**9
01157 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**9
01158 *            ELSE                                                    CL**9
01159 *                MOVE AL-UNNON       TO  LFPCT1A                     CL**9
01160 *        ELSE                                                        CL**9
01161 *            MOVE -1             TO  LFPCT1L                         CL**9
01162 *            MOVE AL-UNBON       TO  LFPCT1A                         CL**9
01163 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01164 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01165 *                                                                    CL**9
01166 *    IF LFTHRU1L GREATER ZERO                                        CL**9
01167 *        MOVE LFTHRU1I               TO  DEEDIT-FIELD                CL**9
01168 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01169 *        IF DEEDIT-FIELD-V0 NUMERIC                                  CL**9
01170 *            MOVE DEEDIT-FIELD-V0    TO  WS-LF-THRU-1                CL**9
01171 *            MOVE AL-UNNON           TO  LFTHRU1A                    CL**9
01172 *        ELSE                                                        CL**9
01173 *            MOVE -1             TO  LFTHRU1L                        CL**9
01174 *            MOVE AL-UNBON       TO  LFTHRU1A                        CL**9
01175 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01176 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01177 *                                                                    CL**9
01178 *    IF LFPCT2L GREATER ZERO                                         CL**9
01179 *        MOVE LFPCT2I                TO  DEEDIT-FIELD                CL**9
01180 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01181 *        IF DEEDIT-FIELD-V4 NUMERIC                                  CL**9
01182 *            MOVE DEEDIT-FIELD-V4    TO  WS-LF-PCT-2                 CL**9
01183 *            IF WS-LF-PCT-2 GREATER 1                                CL**9
01184 *                MOVE -1             TO  LFPCT2L                     CL**9
01185 *                MOVE AL-UNBON       TO  LFPCT2A                     CL**9
01186 *                MOVE ER-2163        TO  EMI-ERROR                   CL**9
01187 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**9
01188 *            ELSE                                                    CL**9
01189 *                MOVE AL-UNNON       TO  LFPCT2A                     CL**9
01190 *        ELSE                                                        CL**9
01191 *            MOVE -1             TO  LFPCT2L                         CL**9
01192 *            MOVE AL-UNBON       TO  LFPCT2A                         CL**9
01193 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01194 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01195 *                                                                    CL**9
01196 *    IF LFTHRU2L GREATER ZERO                                        CL**9
01197 *        MOVE LFTHRU2I               TO  DEEDIT-FIELD                CL**9
01198 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01199 *        IF DEEDIT-FIELD-V0 NUMERIC                                  CL**9
01200 *            MOVE DEEDIT-FIELD-V0    TO  WS-LF-THRU-2                CL**9
01201 *            MOVE AL-UNNON           TO  LFTHRU2A                    CL**9
01202 *        ELSE                                                        CL**9
01203 *            MOVE -1             TO  LFTHRU2L                        CL**9
01204 *            MOVE AL-UNBON       TO  LFTHRU2A                        CL**9
01205 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01206 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01207 *                                                                    CL**9
01208 *    IF LFPCT3L GREATER ZERO                                         CL**9
01209 *        MOVE LFPCT3I                TO  DEEDIT-FIELD                CL**9
01210 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01211 *        IF DEEDIT-FIELD-V4 NUMERIC                                  CL**9
01212 *            MOVE DEEDIT-FIELD-V4    TO  WS-LF-PCT-3                 CL**9
01213 *            IF WS-LF-PCT-3 GREATER 1                                CL**9
01214 *                MOVE -1             TO  LFPCT3L                     CL**9
01215 *                MOVE AL-UNBON       TO  LFPCT3A                     CL**9
01216 *                MOVE ER-2163        TO  EMI-ERROR                   CL**9
01217 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**9
01218 *            ELSE                                                    CL**9
01219 *                MOVE AL-UNNON       TO  LFPCT3A                     CL**9
01220 *        ELSE                                                        CL**9
01221 *            MOVE -1             TO  LFPCT3L                         CL**9
01222 *            MOVE AL-UNBON       TO  LFPCT3A                         CL**9
01223 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01224 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01225 *                                                                    CL**9
01226 *    IF LFTHRU3L GREATER ZERO                                        CL**9
01227 *        MOVE LFTHRU3I               TO  DEEDIT-FIELD                CL**9
01228 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01229 *        IF DEEDIT-FIELD-V0 NUMERIC                                  CL**9
01230 *            MOVE DEEDIT-FIELD-V0    TO  WS-LF-THRU-3                CL**9
01231 *            MOVE AL-UNNON           TO  LFTHRU3A                    CL**9
01232 *        ELSE                                                        CL**9
01233 *            MOVE -1             TO  LFTHRU3L                        CL**9
01234 *            MOVE AL-UNBON       TO  LFTHRU3A                        CL**9
01235 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01236 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01237                                                                      CL**9
01238      IF AHRETENL GREATER ZERO                                        CL**9
01239          MOVE AHRETENI               TO  DEEDIT-FIELD                CL**9
01240          PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01241          IF DEEDIT-FIELD-V4 NUMERIC                                  CL**9
01242              MOVE DEEDIT-FIELD-V4    TO  WS-AH-RET                   CL**9
01243              IF WS-AH-RET GREATER 1                                  CL**9
01244                  MOVE -1             TO  AHRETENL                    CL**9
01245                  MOVE AL-UNBON       TO  AHRETENA                    CL**9
01246                  MOVE ER-2164        TO  EMI-ERROR                   CL**9
01247                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**9
01248              ELSE                                                    CL**9
01249                  MOVE AL-UNNON       TO  AHRETENA                    CL**9
01250          ELSE                                                        CL**9
01251              MOVE -1             TO  AHRETENL                        CL**9
01252              MOVE AL-UNBON       TO  AHRETENA                        CL**9
01253              MOVE ER-2164        TO  EMI-ERROR                       CL**9
01254              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01255                                                                      CL**9
01256 *    IF AHMETHDL GREATER ZERO                                        CL**9
01257 *        IF AHMETHDI = 'P' OR 'S' OR ' '                             CL**9
01258 *            NEXT SENTENCE                                           CL**9
01259 *        ELSE                                                        CL**9
01260 *            MOVE ER-2162        TO  EMI-ERROR                       CL**9
01261 *            MOVE -1             TO  AHMETHDL                        CL**9
01262 *            MOVE AL-UABON       TO  AHMETHDA                        CL**9
01263 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01264 *                                                                    CL**9
01265 *    IF AHBASISL GREATER ZERO                                        CL**9
01266 *        IF AHBASISI = 'P' OR 'E' OR ' '                             CL**9
01267 *            NEXT SENTENCE                                           CL**9
01268 *        ELSE                                                        CL**9
01269 *            MOVE ER-2162        TO  EMI-ERROR                       CL**9
01270 *            MOVE -1             TO  AHBASISL                        CL**9
01271 *            MOVE AL-UABON       TO  AHBASISA                        CL**9
01272 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01273 *                                                                    CL**9
01274 *    IF AHPCT1L GREATER ZERO                                         CL**9
01275 *        MOVE AHPCT1I                TO  DEEDIT-FIELD                CL**9
01276 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01277 *        IF DEEDIT-FIELD-V4 NUMERIC                                  CL**9
01278 *            MOVE DEEDIT-FIELD-V4    TO  WS-AH-PCT-1                 CL**9
01279 *            IF WS-AH-PCT-1 GREATER 1                                CL**9
01280 *                MOVE -1             TO  AHPCT1L                     CL**9
01281 *                MOVE AL-UNBON       TO  AHPCT1A                     CL**9
01282 *                MOVE ER-2163        TO  EMI-ERROR                   CL**9
01283 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**9
01284 *            ELSE                                                    CL**9
01285 *                MOVE AL-UNNON       TO  AHPCT1A                     CL**9
01286 *        ELSE                                                        CL**9
01287 *            MOVE -1             TO  AHPCT1L                         CL**9
01288 *            MOVE AL-UNBON       TO  AHPCT1A                         CL**9
01289 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01290 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01291 *                                                                    CL**9
01292 *    IF AHTHRU1L GREATER ZERO                                        CL**9
01293 *        MOVE AHTHRU1I               TO  DEEDIT-FIELD                CL**9
01294 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01295 *        IF DEEDIT-FIELD-V0 NUMERIC                                  CL**9
01296 *            MOVE DEEDIT-FIELD-V0    TO  WS-AH-THRU-1                CL**9
01297 *            MOVE AL-UNNON           TO  AHTHRU1A                    CL**9
01298 *        ELSE                                                        CL**9
01299 *            MOVE -1             TO  AHTHRU1L                        CL**9
01300 *            MOVE AL-UNBON       TO  AHTHRU1A                        CL**9
01301 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01302 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01303 *                                                                    CL**9
01304 *    IF AHPCT2L GREATER ZERO                                         CL**9
01305 *        MOVE AHPCT2I                TO  DEEDIT-FIELD                CL**9
01306 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01307 *        IF DEEDIT-FIELD-V4 NUMERIC                                  CL**9
01308 *            MOVE DEEDIT-FIELD-V4    TO  WS-AH-PCT-2                 CL**9
01309 *            IF WS-AH-PCT-2 GREATER 1                                CL**9
01310 *                MOVE -1             TO  AHPCT2L                     CL**9
01311 *                MOVE AL-UNBON       TO  AHPCT2A                     CL**9
01312 *                MOVE ER-2163        TO  EMI-ERROR                   CL**9
01313 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**9
01314 *            ELSE                                                    CL**9
01315 *                MOVE AL-UNNON       TO  AHPCT2A                     CL**9
01316 *        ELSE                                                        CL**9
01317 *            MOVE -1             TO  AHPCT2L                         CL**9
01318 *            MOVE AL-UNBON       TO  AHPCT2A                         CL**9
01319 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01320 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01321 *                                                                    CL**9
01322 *    IF AHTHRU2L GREATER ZERO                                        CL**9
01323 *        MOVE AHTHRU2I               TO  DEEDIT-FIELD                CL**9
01324 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01325 *        IF DEEDIT-FIELD-V0 NUMERIC                                  CL**9
01326 *            MOVE DEEDIT-FIELD-V0    TO  WS-AH-THRU-2                CL**9
01327 *            MOVE AL-UNNON           TO  AHTHRU2A                    CL**9
01328 *        ELSE                                                        CL**9
01329 *            MOVE -1             TO  AHTHRU2L                        CL**9
01330 *            MOVE AL-UNBON       TO  AHTHRU2A                        CL**9
01331 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01332 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01333 *                                                                    CL**9
01334 *    IF AHPCT3L GREATER ZERO                                         CL**9
01335 *        MOVE AHPCT3I                TO  DEEDIT-FIELD                CL**9
01336 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01337 *        IF DEEDIT-FIELD-V4 NUMERIC                                  CL**9
01338 *            MOVE DEEDIT-FIELD-V4    TO  WS-AH-PCT-3                 CL**9
01339 *            IF WS-AH-PCT-3 GREATER 1                                CL**9
01340 *                MOVE -1             TO  AHPCT3L                     CL**9
01341 *                MOVE AL-UNBON       TO  AHPCT3A                     CL**9
01342 *                MOVE ER-2163        TO  EMI-ERROR                   CL**9
01343 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            CL**9
01344 *            ELSE                                                    CL**9
01345 *                MOVE AL-UNNON       TO  AHPCT3A                     CL**9
01346 *        ELSE                                                        CL**9
01347 *            MOVE -1             TO  AHPCT3L                         CL**9
01348 *            MOVE AL-UNBON       TO  AHPCT3A                         CL**9
01349 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01350 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01351 *                                                                    CL**9
01352 *    IF AHTHRU3L GREATER ZERO                                        CL**9
01353 *        MOVE AHTHRU3I               TO  DEEDIT-FIELD                CL**9
01354 *        PERFORM 7200-DEEDIT THRU 7200-EXIT                          CL**9
01355 *        IF DEEDIT-FIELD-V0 NUMERIC                                  CL**9
01356 *            MOVE DEEDIT-FIELD-V0    TO  WS-AH-THRU-3                CL**9
01357 *            MOVE AL-UNNON           TO  AHTHRU3A                    CL**9
01358 *        ELSE                                                        CL**9
01359 *            MOVE -1             TO  LFTHRU3L                        CL**9
01360 *            MOVE AL-UNBON       TO  LFTHRU3A                        CL**9
01361 *            MOVE ER-2163        TO  EMI-ERROR                       CL**9
01362 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.               CL**9
01363 *                                                                    CL**9
01364      IF COM1L GREATER ZERO                                        EL6504
01365          MOVE AL-UANON           TO  COM1A.                       EL6504
01366                                                                   EL6504
01367      IF COM2L GREATER ZERO                                        EL6504
01368          MOVE AL-UANON           TO  COM2A.                       EL6504
01369                                                                   EL6504
01370      IF COM3L GREATER ZERO                                        EL6504
01371          MOVE AL-UANON           TO  COM3A.                       EL6504
01372                                                                   EL6504
01373      IF COM4L GREATER ZERO                                        EL6504
01374          MOVE AL-UANON           TO  COM4A.                       EL6504
01375                                                                   EL6504
01376      IF COM5L GREATER ZERO                                        EL6504
01377          MOVE AL-UANON           TO  COM5A.                       EL6504
01378                                                                   EL6504
01379  7099-EXIT.                                                       EL6504
01380      EXIT.                                                        EL6504
01381      EJECT                                                        EL6504
01382  7100-READ-ERACCT.                                                EL6504
01383      EXEC CICS READ                                               EL6504
01384           DATASET  (ERACCT-FILE)                                  EL6504
01385           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*11
01386           RIDFLD   (PI-ACCT-KEY)                                  EL6504
01387      END-EXEC.                                                    EL6504
01388                                                                   EL6504
01389      CONTINUE.                                                       CL*11
01390                                                                   EL6504
01391      MOVE AM-LAST-MAINT-USER     TO  PI-UPDATE-BY.                EL6504
01392      MOVE AM-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.            EL6504
01393                                                                   EL6504
01394  7100-EXIT.                                                       EL6504
01395      EXIT.                                                        EL6504
01396      EJECT                                                        EL6504
01397  7200-DEEDIT.                                                     EL6504
01398      EXEC CICS BIF                                                EL6504
01399           DEEDIT                                                  EL6504
01400           FIELD  (DEEDIT-FIELD)                                   EL6504
01401           LENGTH (15)                                             EL6504
01402      END-EXEC.                                                    EL6504
01403                                                                   EL6504
01404  7200-EXIT.                                                       EL6504
01405      EXIT.                                                        EL6504
01406      EJECT                                                        EL6504
01407  7300-READ-ERACCT-UPDATE.                                         EL6504
01408      EXEC CICS READ                                               EL6504
01409           DATASET  (ERACCT-FILE)                                  EL6504
01410           SET      (ADDRESS OF ACCOUNT-MASTER)                       CL*11
01411           RIDFLD   (PI-ACCT-KEY)                                  EL6504
01412           UPDATE                                                  EL6504
01413      END-EXEC.                                                    EL6504
01414                                                                   EL6504
01415      CONTINUE.                                                       CL*11
01416                                                                   EL6504
01417  7300-EXIT.                                                       EL6504
01418      EXIT.                                                        EL6504
01419      EJECT                                                        EL6504
01420  7400-EDIT-MORTALITY.                                             EL6504
01421      IF MORTALI = SPACES                                          EL6504
01422          GO TO 7499-EXIT.                                         EL6504
01423                                                                   EL6504
01424      MOVE SPACES                 TO  ELCNTL-KEY.                     CL**7
01425      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6504
01426      MOVE '7'                    TO  CNTL-REC-TYPE.               EL6504
01427      MOVE +0                     TO  CNTL-SEQ-NO.                    CL**7
01428                                                                   EL6504
01429      EXEC CICS HANDLE CONDITION                                   EL6504
01430          NOTFND  (7490-NOT-FOUND)                                 EL6504
01431          ENDFILE (7490-NOT-FOUND)                                    CL**7
01432      END-EXEC.                                                    EL6504
01433                                                                   EL6504
01434  7405-READ-MORTALITY.                                                CL**2
01435                                                                      CL**7
01436      EXEC CICS READ                                               EL6504
01437           DATASET  (CNTL-FILE-ID)                                 EL6504
01438           SET      (ADDRESS OF CONTROL-FILE)                         CL*11
01439           RIDFLD   (ELCNTL-KEY)                                   EL6504
01440      END-EXEC.                                                    EL6504
01441                                                                   EL6504
01442      CONTINUE.                                                       CL*11
01443                                                                   EL6504
01444      IF PI-COMPANY-ID NOT EQUAL  TO CF-COMPANY-ID                    CL**7
01445          OR CF-RECORD-TYPE NOT EQUAL '7'                             CL**7
01446          GO TO 7490-NOT-FOUND                                        CL**7
01447      ELSE                                                            CL**7
01448          MOVE +1                 TO  SUB2                            CL**7
01449          GO TO 7410-SEARCH-MORTAL-TABLE.                             CL**7
01450                                                                   EL6504
01451  7410-SEARCH-MORTAL-TABLE.                                        EL6504
01452                                                                      CL**7
01453      IF CF-MORT-TABLE-CODE (SUB2) = MORTALI                       EL6504
01454          GO TO 7499-EXIT.                                         EL6504
01455                                                                   EL6504
01456      IF CF-MORT-TABLE-CODE (SUB2) = LOW-VALUES                       CL**7
01457             OR                                                       CL**7
01458         CF-MORT-TABLE-CODE (SUB2) GREATER THAN MORTALI               CL**7
01459         GO TO 7490-NOT-FOUND.                                        CL**7
01460                                                                      CL**7
01461      ADD +1                      TO  SUB2.                        EL6504
01462                                                                   EL6504
01463      IF SUB2 GREATER +9                                              CL**7
01464          ADD +1                  TO CNTL-SEQ-NO                      CL**7
01465          GO TO 7405-READ-MORTALITY                                   CL**7
01466      ELSE                                                         EL6504
01467          GO TO 7410-SEARCH-MORTAL-TABLE.                          EL6504
01468                                                                   EL6504
01469  7490-NOT-FOUND.                                                  EL6504
01470      MOVE -1                     TO  MORTALL.                     EL6504
01471      MOVE AL-UABON               TO  MORTALA.                     EL6504
01472      MOVE ER-2103                TO  EMI-ERROR                    EL6504
01473      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL**5
01474                                                                   EL6504
01475  7499-EXIT.                                                       EL6504
01476      EXIT.                                                        EL6504
01477      EJECT                                                        EL6504
01478  7800-COMPANY-REC-READ.                                           EL6504
01479      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6504
01480      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6504
01481      MOVE '1'                    TO  CNTL-REC-TYPE.               EL6504
01482      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6504
01483      EXEC CICS HANDLE CONDITION                                   EL6504
01484          NOTFND   (7880-NO-COMP)                                  EL6504
01485      END-EXEC.                                                    EL6504
01486                                                                   EL6504
01487      EXEC CICS READ                                               EL6504
01488          DATASET   (CNTL-FILE-ID)                                 EL6504
01489          SET       (ADDRESS OF CONTROL-FILE)                         CL*11
01490          RIDFLD    (ELCNTL-KEY)                                   EL6504
01491      END-EXEC.                                                    EL6504
01492                                                                   EL6504
01493      CONTINUE.                                                       CL*11
01494                                                                   EL6504
01495      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES                     EL6504
01496          MOVE ER-2572            TO  EMI-ERROR                    EL6504
01497          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                EL6504
01498                                                                   EL6504
01499      GO TO 7899-EXIT.                                             EL6504
01500                                                                   EL6504
01501  7880-NO-COMP.                                                    EL6504
01502      MOVE ER-0002                TO  EMI-ERROR                    EL6504
01503      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    EL6504
01504                                                                   EL6504
01505  7899-EXIT.                                                       EL6504
01506      EXIT.                                                        EL6504
01507      EJECT                                                        EL6504
01508                                                                   EL6504
01509  8000-UPDATE-MAINT-DATE.                                          EL6504
01510      MOVE SPACES                 TO  ELCNTL-KEY.                  EL6504
01511                                                                   EL6504
01512      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL6504
01513      MOVE '1'                    TO  CNTL-REC-TYPE.               EL6504
01514      MOVE +0                     TO  CNTL-SEQ-NO.                 EL6504
01515                                                                   EL6504
01516      EXEC CICS HANDLE CONDITION                                   EL6504
01517          NOTFND   (8000-EXIT)                                     EL6504
01518      END-EXEC.                                                    EL6504
01519                                                                   EL6504
01520      EXEC CICS READ                                               EL6504
01521          UPDATE                                                   EL6504
01522          DATASET   (CNTL-FILE-ID)                                 EL6504
01523          SET       (ADDRESS OF CONTROL-FILE)                         CL*11
01524          RIDFLD    (ELCNTL-KEY)                                   EL6504
01525      END-EXEC.                                                    EL6504
01526                                                                   EL6504
01527      CONTINUE.                                                       CL*11
01528                                                                   EL6504
01529      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL6504
01530      MOVE 'B'                    TO  JP-RECORD-TYPE.              EL6504
01531      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6504
01532      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL6504
01533      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6504
01534                                                                   EL6504
01535      MOVE BIN-CURRENT-SAVE       TO  CF-ACCOUNT-MSTR-MAINT-DT.    EL6504
01536                                                                   EL6504
01537      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.              EL6504
01538      MOVE 'C'                    TO  JP-RECORD-TYPE.              EL6504
01539      MOVE CNTL-FILE-ID           TO  FILE-ID.                     EL6504
01540                                                                   EL6504
01541      EXEC CICS REWRITE                                            EL6504
01542          DATASET   (CNTL-FILE-ID)                                 EL6504
01543          FROM      (CONTROL-FILE)                                 EL6504
01544      END-EXEC.                                                    EL6504
01545                                                                   EL6504
01546      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.      EL6504
01547      PERFORM 8400-LOG-JOURNAL-RECORD.                             EL6504
01548                                                                   EL6504
01549  8000-EXIT.                                                       EL6504
01550       EXIT.                                                       EL6504
01551      EJECT                                                        EL6504
01552                                                                   EL6504
01553  8100-SEND-INITIAL-MAP.                                           EL6504
01554      MOVE SAVE-DATE              TO  DATEO.                       EL6504
01555      MOVE EIBTIME                TO  TIME-IN.                     EL6504
01556      MOVE TIME-OUT               TO  TIMEO.                       EL6504
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01557      MOVE -1                     TO  PFENTERL.                    EL6504
01558      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.                    EL6504
01559                                                                   EL6504
01560      MOVE PI-LIFE-OVERRIDE-L6    TO  LFREIHDO                        CL**9
01561                                      LFRETHDO.                       CL**9
01562      MOVE PI-AH-OVERRIDE-L6      TO  AHREIHDO                        CL**9
01563                                      AHRETHDO.                       CL**9
01564      MOVE PI-LIFE-OVERRIDE-L2    TO  LRETHD1O.                       CL**9
01565      MOVE PI-AH-OVERRIDE-L2      TO  ARETHD1O                        CL**9
01566                                      ARETHD2O                        CL**9
01567                                      ARETHD3O.                       CL**9
01568                                                                   EL6504
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
PEMMOD     IF PI-PROCESSOR-ID NOT = 'PEMA'
PEMMOD        MOVE AL-SADON            TO LTAXA
PEMMOD                                    ATAXA
PEMMOD     END-IF
PEMMOD
01569      EXEC CICS SEND                                               EL6504
01570          MAP      (MAP-NAME)                                      EL6504
01571          MAPSET   (MAPSET-NAME)                                   EL6504
01572          FROM     (EL6504AO)                                      EL6504
01573          ERASE                                                    EL6504
01574          CURSOR                                                   EL6504
01575      END-EXEC.                                                    EL6504
01576                                                                   EL6504
01577      GO TO 9100-RETURN-TRAN.                                      EL6504
01578                                                                   EL6504
01579  8200-SEND-DATAONLY.                                              EL6504
01580      MOVE SAVE-DATE              TO  DATEO.                       EL6504
01581      MOVE EIBTIME                TO  TIME-IN.                     EL6504
01582      MOVE TIME-OUT               TO  TIMEO.                       EL6504
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01583      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O                     EL6504
01584                                                                   EL6504
01585      MOVE PI-LIFE-OVERRIDE-L6    TO  LFREIHDO                        CL**9
01586                                      LFRETHDO.                       CL**9
01587      MOVE PI-AH-OVERRIDE-L6      TO  AHREIHDO                        CL**9
01588                                      AHRETHDO.                       CL**9
01589      MOVE PI-LIFE-OVERRIDE-L2    TO  LRETHD1O.                       CL**9
01590      MOVE PI-AH-OVERRIDE-L2      TO  ARETHD1O                        CL**9
01591                                      ARETHD2O                        CL**9
01592                                      ARETHD3O.                       CL**9
01593                                                                   EL6504
PEMMOD     IF PI-PROCESSOR-ID NOT = 'PEMA'
PEMMOD        MOVE AL-SADON            TO LTAXA
PEMMOD                                    ATAXA
PEMMOD     END-IF
PEMMOD
01594      EXEC CICS SEND                                               EL6504
01595          MAP      (MAP-NAME)                                      EL6504
01596          MAPSET   (MAPSET-NAME)                                   EL6504
01597          FROM     (EL6504AO)                                      EL6504
01598          DATAONLY                                                 EL6504
01599          ERASEAUP                                                 EL6504
01600          CURSOR                                                   EL6504
01601      END-EXEC.                                                    EL6504
01602                                                                   EL6504
01603      GO TO 9100-RETURN-TRAN.                                      EL6504
01604                                                                   EL6504
01605  8300-SEND-TEXT.                                                  EL6504
01606      EXEC CICS SEND TEXT                                          EL6504
01607          FROM     (LOGOFF-TEXT)                                   EL6504
01608          LENGTH   (LOGOFF-LENGTH)                                 EL6504
01609          ERASE                                                    EL6504
01610          FREEKB                                                   EL6504
01611      END-EXEC.                                                    EL6504
01612                                                                   EL6504
01613      EXEC CICS RETURN                                             EL6504
01614      END-EXEC.                                                    EL6504
01615                                                                   EL6504
01616  8400-LOG-JOURNAL-RECORD.                                         EL6504
01617      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.                  EL6504
01618      MOVE FILE-ID                TO  JP-FILE-ID.                  EL6504
01619      MOVE THIS-PGM               TO  JP-PROGRAM-ID.               EL6504
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO                             EL6504
pemuni*        EXEC CICS JOURNAL                                        EL6504
pemuni*            JFILEID     (PI-JOURNAL-FILE-ID)                     EL6504
pemuni*            JTYPEID     ('ER')                                   EL6504
pemuni*            FROM        (JOURNAL-RECORD)                         EL6504
pemuni*            LENGTH      (WS-JOURNAL-FILE-LENGTH)                 EL6504
pemuni*        END-EXEC.                                                EL6504
01627                                                                   EL6504
01628  8800-UNAUTHORIZED-ACCESS.                                        EL6504
01629      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.                  EL6504
01630      GO TO 8300-SEND-TEXT.                                        EL6504
01631                                                                   EL6504
01632  8810-PF23.                                                       EL6504
01633      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL6504
01634      MOVE XCTL-005               TO  PGM-NAME.                    EL6504
01635      GO TO 9300-XCTL.                                             EL6504
01636                                                                   EL6504
01637  9000-RETURN-CICS.                                                EL6504
01638      EXEC CICS RETURN                                             EL6504
01639      END-EXEC.                                                    EL6504
01640                                                                   EL6504
01641  9100-RETURN-TRAN.                                                   CL**7
01642      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL6504
01643      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL6504
01644      EXEC CICS RETURN                                             EL6504
01645          TRANSID    (TRANS-ID)                                    EL6504
01646          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6504
01647          LENGTH     (WS-COMM-LENGTH)                                 CL**5
01648      END-EXEC.                                                    EL6504
01649                                                                   EL6504
01650  9200-RETURN-MAIN-MENU.                                           EL6504
01651      MOVE XCTL-626               TO  PGM-NAME.                    EL6504
01652      GO TO 9300-XCTL.                                             EL6504
01653                                                                   EL6504
01654  9300-XCTL.                                                       EL6504
01655      EXEC CICS XCTL                                               EL6504
01656          PROGRAM    (PGM-NAME)                                    EL6504
01657          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6504
01658          LENGTH     (WS-COMM-LENGTH)                                 CL**5
01659      END-EXEC.                                                    EL6504
01660                                                                   EL6504
01661  9400-CLEAR.                                                      EL6504
01662      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.                    EL6504
01663      GO TO 9300-XCTL.                                             EL6504
01664                                                                   EL6504
01665  9500-PF12.                                                       EL6504
01666      MOVE XCTL-010               TO  PGM-NAME.                    EL6504
01667      GO TO 9300-XCTL.                                             EL6504
01668                                                                   EL6504
01669  9600-PGMID-ERROR.                                                EL6504
01670      EXEC CICS HANDLE CONDITION                                   EL6504
01671          PGMIDERR    (8300-SEND-TEXT)                             EL6504
01672      END-EXEC.                                                    EL6504
01673                                                                   EL6504
01674      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.          EL6504
01675      MOVE ' '                    TO  PI-ENTRY-CD-1.               EL6504
01676      MOVE XCTL-005               TO  PGM-NAME.                    EL6504
01677      MOVE PGM-NAME               TO  LOGOFF-PGM.                  EL6504
01678      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL6504
01679      GO TO 9300-XCTL.                                             EL6504
01680                                                                   EL6504
01681  9700-LINK-DATE-CONVERT.                                          EL6504
01682                                                                   EL6504
01683      EXEC CICS LINK                                               EL6504
01684          PROGRAM    ('ELDATCV')                                   EL6504
01685          COMMAREA   (DATE-CONVERSION-DATA)                        EL6504
01686          LENGTH     (DC-COMM-LENGTH)                              EL6504
01687      END-EXEC.                                                    EL6504
01688                                                                   EL6504
01689  9700-EXIT.                                                       EL6504
01690      EXIT.                                                        EL6504
01691                                                                   EL6504
01692  9900-ERROR-FORMAT.                                               EL6504
01693      IF NOT EMI-ERRORS-COMPLETE                                   EL6504
01694          MOVE LINK-001           TO  PGM-NAME                     EL6504
01695          EXEC CICS LINK                                           EL6504
01696              PROGRAM    (PGM-NAME)                                EL6504
01697              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6504
01698              LENGTH     (EMI-COMM-LENGTH)                         EL6504
01699          END-EXEC.                                                EL6504
01700                                                                   EL6504
01701  9900-EXIT.                                                       EL6504
01702      EXIT.                                                        EL6504
01703                                                                   EL6504
01704  9990-ABEND.                                                      EL6504
01705      MOVE LINK-004               TO  PGM-NAME.                    EL6504
01706      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL6504
01707      EXEC CICS LINK                                               EL6504
01708          PROGRAM   (PGM-NAME)                                     EL6504
01709          COMMAREA  (EMI-LINE1)                                    EL6504
01710          LENGTH    (72)                                           EL6504
01711      END-EXEC.                                                    EL6504
01712                                                                   EL6504
01713      GO TO 8200-SEND-DATAONLY.                                    EL6504
01714                                                                   EL6504
01715      GOBACK.                                                      EL6504
01716                                                                   EL6504
01717  9995-SECURITY-VIOLATION.                                         EL6504
01718             COPY ELCSCTP.                                         EL6504
01719  9995-EXIT.                                                       EL6504
01720       EXIT.                                                       EL6504
