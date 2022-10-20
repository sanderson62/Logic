00001  ID DIVISION.                                                     03/08/96
00002                                                                   EL604
00003  PROGRAM-ID.                 EL604.                                  LV015
00004 *              PROGRAM CONVERTED BY                                  CL*15
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*15
00006 *              CONVERSION DATE 02/12/96 16:59:34.                    CL*15
00007 *                            VMOD=2.015.                             CL*15
00008 *                                                                 EL604
00008 *                                                                 EL604
00009 *AUTHOR.     LOGIC,INC.                                              CL*15
00010 *            DALLAS, TEXAS.                                          CL*15
00011                                                                   EL604
00012 *DATE-COMPILED.                                                      CL*15
00013 *SECURITY.   *****************************************************   CL*15
00014 *            *                                                   *   CL*15
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*15
00016 *            *                                                   *   CL*15
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*15
00018 *                                                                *   CL*15
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*15
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*15
00021 *            *                                                   *   CL*15
00022 *            *****************************************************   CL*15
00023                                                                   EL604
00024 *REMARKS.    TRANSACTION - EXA9 - REPORT CUSTOMIZATION.              CL*15
00025                                                                   EL604
121307******************************************************************
121307*                   C H A N G E   L O G
121307*
121307* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121307*-----------------------------------------------------------------
121307*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121307* EFFECTIVE    NUMBER
121307*-----------------------------------------------------------------
121307* 121307  CR2007090700001  PEMA  ADD ACCT STATUS OF "C"
121307******************************************************************
00026  ENVIRONMENT DIVISION.                                            EL604
00027                                                                   EL604
00028      EJECT                                                        EL604
00029  DATA DIVISION.                                                   EL604
00030  WORKING-STORAGE SECTION.                                         EL604
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL604
00032  77  FILLER  PIC X(32)  VALUE '*    EL604  WORKING STORAGE    *'. EL604
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.015 *********'.    CL*15

00035                                  COPY ELCSCTM.                       CL*14
00037                                  COPY ELCSCRTY.                      CL*14
092308                                 COPY MPCSCRT.

00040  01  WS-DATE-AREA.                                                EL604
00041      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL604
00042      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.    EL604
00043                                                                   EL604
00044  01  STANDARD-AREAS.                                              EL604
092308     12  W-APPL-SCRTY-NDX    PIC S9(04) COMP   VALUE +30.
092308     12  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.
00046      12  WS-TEST-CARRIER             PIC X       VALUE SPACES.    EL604
00047      12  WS-TEST-STATE               PIC XX      VALUE SPACES.    EL604
00048      12  WS-TEST-BUSTYP              PIC 99      VALUE ZEROS.        CL**2
00049      12  WS-TEST-BEN                 PIC XX      VALUE SPACES.       CL**2
00050          88  INVALID-BENEFIT-CODE       VALUE '00'                   CL**6
00051                                               '90' THRU '99'.        CL**2
00052      12  WS-TEST-SEQ                 PIC X       VALUE SPACES.       CL*14
00053          88  VALID-ACCT-SEQ           VALUES  '1' '2' '3'            CL*14
00054                                               '4' '5' '6'.           CL*14
00055      12  WS-TEST-LEN                 PIC S9(4)   VALUE +0.        EL604
00056      12  WS-TEST-ATTRB               PIC X       VALUE SPACES.    EL604
00057      12  WS-SUB                      PIC S9(4)   VALUE +0 COMP.   EL604
00058      12  WS-BROWSE-SW                PIC X       VALUE SPACES.       CL**6
00059      12  WS-SEQ-AREA.                                             EL604
00060          16  WS-SEQ-NO OCCURS 6 TIMES PIC X.                      EL604
00061      12  WS-LO-EFF                   PIC XX      VALUE LOW-VALUES.   CL**6
00062      12  WS-HI-EFF                   PIC XX      VALUE LOW-VALUES.   CL**6
00063      12  WS-LO-ENT                   PIC XX      VALUE LOW-VALUES.   CL**6
00064      12  WS-HI-ENT                   PIC XX      VALUE LOW-VALUES.   CL**6
00065      12  WS-LO-LOSS                  PIC S9(3)V99 VALUE +0.       EL604
00066      12  WS-HI-LOSS                  PIC S9(3)V99 VALUE +0.       EL604
00067      12  GETMAIN-SPACE               PIC X       VALUE SPACE.     EL604
00068      12  MAP-NAME                    PIC X(8)    VALUE 'EL604A'.  EL604
00069      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL604S'.  EL604
00070      12  SCREEN-NUMBER               PIC X(4)    VALUE '604A'.    EL604
00071      12  TRANS-ID                    PIC X(4)    VALUE 'EXA9'.    EL604
00072      12  THIS-PGM                    PIC X(8)    VALUE 'EL604'.   EL604
00073      12  PGM-NAME                    PIC X(8).                    EL604
00074      12  TIME-IN                     PIC S9(7).                   EL604
00075      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL604
00076          16  FILLER                  PIC X.                       EL604
00077          16  TIME-OUT                PIC 99V99.                   EL604
00078          16  FILLER                  PIC XX.                         CL**6
00079      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL604
00080      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL604
00081      12  XCTL-EL126                  PIC X(8)    VALUE 'EL126'.      CL**4
00082      12  XCTL-EL6041                 PIC X(8)    VALUE 'EL6041'.     CL*14
00083      12  XCTL-EL626                  PIC X(8)    VALUE 'EL626'.      CL**4
00084      12  XCTL-EM626                  PIC X(8)    VALUE 'EM626'.      CL**4
00085      12  XCTL-GL800                  PIC X(8)    VALUE 'GL800'.      CL**4
00086      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL604
00087      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL604
00088      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL604
00089      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.  EL604
00090      12  WS-JOURNAL-RECORD-LENGTH PIC S9(4)   COMP VALUE +750.       CL*13
00091                                                                   EL604
00092      12  DEEDIT-FIELD                PIC X(15).                   EL604
00093      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).     EL604
00094      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.  EL604
00095                                                                   EL604
00096      12  RETURN-FROM                 PIC X(8).                    EL604
00097      12  WS-REPORT-FOUND-SW          PIC X       VALUE 'N'.       EL604
00098          88  REPORT-WAS-FOUND                    VALUE 'Y'.       EL604
00099          88  REPORT-WAS-NOT-FOUND                VALUE 'N'.       EL604
00100                                                                   EL604
00101      EJECT                                                        EL604
00102  01   ERROR-MESSAGES.                                             EL604
00103      12  ER-0000                     PIC  X(4)   VALUE '0000'.    EL604
00104      12  ER-0004                     PIC  X(4)   VALUE '0004'.    EL604
00105      12  ER-0021                     PIC  X(4)   VALUE '0021'.    EL604
00106      12  ER-0023                     PIC  X(4)   VALUE '0023'.    EL604
00107      12  ER-0028                     PIC  X(4)   VALUE '0028'.    EL604
00108      12  ER-0029                     PIC  X(4)   VALUE '0029'.    EL604
00109      12  ER-0050                     PIC  X(4)   VALUE '0050'.    EL604
00110      12  ER-0068                     PIC  X(4)   VALUE '0068'.    EL604
00111      12  ER-0070                     PIC  X(4)   VALUE '0070'.    EL604
00112      12  ER-0132                     PIC  X(4)   VALUE '0132'.    EL604
00113      12  ER-0138                     PIC  X(4)   VALUE '0138'.    EL604
00114      12  ER-0139                     PIC  X(4)   VALUE '0139'.    EL604
00115      12  ER-0142                     PIC  X(4)   VALUE '0142'.    EL604
00116      12  ER-2178                     PIC  X(4)   VALUE '2178'.    EL604
00117      12  ER-2237                     PIC  X(4)   VALUE '2237'.    EL604
00118      12  ER-2238                     PIC  X(4)   VALUE '2238'.    EL604
00119      12  ER-2845                     PIC  X(4)   VALUE '2845'.    EL604
00120      12  ER-2848                     PIC  X(4)   VALUE '2848'.    EL604
00121      12  ER-7008                     PIC  X(4)   VALUE '7008'.    EL604
00122      12  ER-7123                     PIC  X(4)   VALUE '7123'.    EL604
00123      12  ER-7125                     PIC  X(4)   VALUE '7125'.    EL604
00124      12  ER-7680                     PIC  X(4)   VALUE '7680'.    EL604
00125      12  ER-7681                     PIC  X(4)   VALUE '7681'.    EL604
00126      12  ER-7682                     PIC  X(4)   VALUE '7682'.    EL604
00127      12  ER-7683                     PIC  X(4)   VALUE '7683'.    EL604
00128      12  ER-7684                     PIC  X(4)   VALUE '7684'.    EL604
00129      12  ER-7685                     PIC  X(4)   VALUE '7685'.    EL604
00130      12  ER-7745                     PIC  X(4)   VALUE '7745'.       CL*14
00131      12  ER-7746                     PIC  X(4)   VALUE '7746'.       CL*14
00132      12  ER-9096                     PIC  X(4)   VALUE '9096'.       CL**4
00133      12  ER-9097                     PIC  X(4)   VALUE '9097'.       CL**4
00134                                                                   EL604
00135      EJECT                                                        EL604
00136                                                                   EL604
00137  01  ACCESS-KEYS.                                                 EL604
00138      12  ELCNTL-KEY.                                              EL604
00139          16  ELCNTL-COMPANY-ID          PIC  XXX.                 EL604
00140          16  ELCNTL-RECORD-TYPE         PIC  X.                   EL604
00141          16  FILLER                     PIC  X.                   EL604
00142          16  ELCNTL-REPORT              PIC  999.                 EL604
00143          16  ELCNTL-SEQ-NO              PIC  S9(4)   COMP.        EL604
00144                                                                   EL604
00145      12  ELCNTL-TEST-KEY.                                         EL604
00146          16  ELCNTL-TEST-COMP-ID        PIC  XXX.                 EL604
00147          16  ELCNTL-TEST-REC-TYPE       PIC  X.                   EL604
00148          16  ELCNTL-ACCESS-KEY.                                   EL604
00149              20  FILLER                 PIC  XXX.                 EL604
00150              20  ELCNTL-TEST-CAR        PIC  X.                   EL604
00151          16  ELCNTL-ACCESS-KEY1 REDEFINES ELCNTL-ACCESS-KEY.      EL604
00152              20  ELCNTL-TEST-STATE      PIC  XX.                     CL**3
00153              20  FILLER                 PIC  XX.                  EL604
00154          16  ELCNTL-ACCESS-KEY2 REDEFINES ELCNTL-ACCESS-KEY.      EL604
00155              20  FILLER                 PIC  XX.                  EL604
00156              20  ELCNTL-TEST-BEN        PIC  XX.                     CL**2
00157          16  ELCNTL-ACCESS-KEY3 REDEFINES ELCNTL-ACCESS-KEY.         CL**2
00158              20  FILLER                 PIC  XX.                     CL**2
00159              20  ELCNTL-TEST-BUSTYP     PIC  99.                     CL**2
00160          16  ELCNTL-TEST-SEQ-NO         PIC  S9(4)   COMP.        EL604
00161                                                                   EL604
00162      12  ELCNTL-LENGTH           PIC S9(4) COMP VALUE +750.          CL*13
00163                                                                   EL604
00164      EJECT                                                        EL604
00165                                                                   EL604
00166                                  COPY ELCDATE.                       CL*14
00167      EJECT                                                        EL604
00168                                  COPY ELCLOGOF.                      CL*14
00169      EJECT                                                        EL604
00170                                  COPY ELCATTR.                       CL*14
00171      EJECT                                                        EL604
00172                                  COPY ELCEMIB.                       CL*14
00173      EJECT                                                           CL*14
00174                                  COPY ELCINTF.                       CL*14
00175                                                                      CL*14
00176      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL604
00177          16  PI-LAST-REPORT        PIC 9(3).                      EL604
00178          16  FILLER                PIC X(637).                       CL*15
00179      EJECT                                                        EL604
00180                              COPY ELCJPFX.                           CL*14
00181                              PIC X(750).                             CL*13
00182      EJECT                                                        EL604
00183                              COPY ELCAID.                            CL*14
00184                                                                   EL604
00185  01  FILLER    REDEFINES DFHAID.                                  EL604
00186      12  FILLER              PIC X(8).                            EL604
00187      12  PF-VALUES           PIC X       OCCURS 24 TIMES.         EL604
00188                                                                   EL604
00189      EJECT                                                        EL604
00190                                  COPY EL604S.                        CL*14
00191      EJECT                                                        EL604
00192                                                                   EL604
00193  LINKAGE SECTION.                                                 EL604
00194  01  DFHCOMMAREA             PIC X(1024).                         EL604
00195                                                                   EL604
00196      EJECT                                                        EL604
00197                                                                   EL604
00198 *01 PARMLIST .                                                       CL*15
00199 *    12  FILLER                      PIC S9(8)   COMP.               CL*15
00200 *    12  ELCNTL-POINTER              PIC S9(8)   COMP.               CL*15
00201                                                                   EL604
00202      EJECT                                                        EL604
00203                                                                   EL604
00204                              COPY ELCCNTL.                           CL*14
00205                                                                   EL604
00206      EJECT                                                        EL604
00207  PROCEDURE DIVISION.                                              EL604
00208                                                                   EL604
00209      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL604
00210      MOVE '5'                    TO DC-OPTION-CODE.               EL604
00211      PERFORM 9700-DATE-LINK.                                      EL604
00212      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL604
00213      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL604
00214                                                                   EL604
00215      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL604
00216      MOVE 2                      TO EMI-NUMBER-OF-LINES.          EL604
00217                                                                   EL604
00218      IF EIBCALEN = 0                                              EL604
00219          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL604
00220                                                                   EL604
00221      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL604
00222          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL604
00223              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL604
00224              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL604
00225              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL604
00226              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL604
00227              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL604
00228              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL604
00229              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL604
00230              MOVE THIS-PGM             TO PI-CALLING-PROGRAM         CL**6
00231              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT         CL*12
00232          ELSE                                                     EL604
00233              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM             EL604
00234              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL604
00235              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL604
00236              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL604
00237              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL604
00238              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL604
00239              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL604
00240              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL604
00241              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL604
00242                                                                   EL604
00243      EXEC CICS    HANDLE    CONDITION                             EL604
00244           PGMIDERR          (9600-PGMID-ERROR)                    EL604
00245           ERROR             (9990-ABEND)                          EL604
00246      END-EXEC.                                                    EL604
00247                                                                   EL604
00248      IF  EIBTRNID NOT = TRANS-ID                                  EL604
00249          MOVE LOW-VALUES         TO EL604AI                       EL604
00250          GO TO 8100-SEND-INITIAL-MAP.                             EL604
00251                                                                   EL604
00252      IF  EIBAID = DFHCLEAR                                           CL**4
00253              OR                                                      CL**4
00254          NOT DISPLAY-CAP
00255          GO TO 9400-CLEAR.                                        EL604
00256                                                                   EL604
00257      EJECT                                                        EL604
00258                                                                   EL604
00259  0200-RECEIVE.                                                    EL604
00260      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL604
00261         MOVE ER-7008            TO EMI-ERROR                      EL604
00262         PERFORM 9900-ERROR-FORMAT                                 EL604
00263         MOVE -1                 TO MAINTL                         EL604
00264         GO TO 8200-SEND-DATAONLY.                                 EL604
00265                                                                   EL604
00266      EXEC CICS RECEIVE                                            EL604
00267          MAP      (MAP-NAME)                                      EL604
00268          MAPSET   (MAPSET-NAME)                                   EL604
00269          INTO     (EL604AI)                                       EL604
00270      END-EXEC.                                                    EL604
00271                                                                   EL604
00272      IF  PFKEYL = +0                                              EL604
00273          GO TO 0300-CHECK-PFKEYS.                                 EL604
00274                                                                   EL604
00275      IF  EIBAID NOT = DFHENTER                                    EL604
00276          MOVE ER-0004            TO EMI-ERROR                     EL604
00277          GO TO 0320-INPUT-ERROR.                                  EL604
00278                                                                   EL604
00279      IF PFKEYI NOT NUMERIC                                        EL604
00280         MOVE ER-0029     TO EMI-ERROR                             EL604
00281         GO TO 0320-INPUT-ERROR.                                   EL604
00282                                                                   EL604
00283      IF PFKEYI GREATER 01 AND LESS 25                                CL**6
00284         MOVE PF-VALUES (PFKEYI) TO EIBAID                         EL604
00285      ELSE                                                         EL604
00286         MOVE ER-0029        TO EMI-ERROR                          EL604
00287         GO TO 0320-INPUT-ERROR.                                   EL604
00288                                                                   EL604
00289  0300-CHECK-PFKEYS.                                               EL604
00290      IF EIBAID = DFHPF23                                          EL604
00291          GO TO 8810-PF23.                                         EL604
00292                                                                   EL604
00293      IF EIBAID = DFHPF24                                          EL604
00294          GO TO 9200-RETURN-MAIN-MENU.                             EL604
00295                                                                   EL604
00296      IF EIBAID = DFHPF12                                          EL604
00297          GO TO 9500-PF12.                                         EL604
00298                                                                   EL604
00299      IF MAINTL GREATER THAN +0 AND                                EL604
00300               EIBAID NOT = DFHENTER                               EL604
00301         MOVE -1             TO  MAINTL                            EL604
00302         MOVE  ER-0050       TO  EMI-ERROR                         EL604
00303         PERFORM 9900-ERROR-FORMAT                                 EL604
00304         GO TO 8200-SEND-DATAONLY.                                 EL604
00305                                                                   EL604
00306      IF  EIBAID = DFHPF1                                          EL604
00307          PERFORM 7000-BROWSE-FWRD-NEXT-ACCOUNT THRU 7090-EXIT     EL604
00308          GO TO 4000-SHOW-REPORT.                                     CL*14
00309                                                                   EL604
00310      IF  EIBAID = DFHPF2                                          EL604
00311          PERFORM 7100-BROWSE-BWRD-NEXT-ACCOUNT THRU 7190-EXIT     EL604
00312          GO TO 4000-SHOW-REPORT.                                     CL*14
00313                                                                      CL*14
00314      IF EIBAID = DFHPF3                                              CL*14
00315          GO TO 8900-PF03.                                            CL*14
00316                                                                   EL604
00317      IF EIBAID = DFHENTER                                         EL604
00318          GO TO 0400-EDIT-INPUT-DATA.                              EL604
00319                                                                   EL604
00320      MOVE ER-0029                TO EMI-ERROR.                    EL604
00321                                                                   EL604
00322  0320-INPUT-ERROR.                                                EL604
00323      PERFORM 9900-ERROR-FORMAT.                                   EL604
00324      MOVE AL-UNBON               TO PFKEYA.                       EL604
00325      IF PFKEYL = 0                                                EL604
00326          MOVE -1                 TO MAINTL                        EL604
00327      ELSE                                                         EL604
00328          MOVE -1                 TO PFKEYL.                       EL604
00329                                                                   EL604
00330      GO TO 8200-SEND-DATAONLY.                                    EL604
00331                                                                   EL604
00332      EJECT                                                        EL604
00333                                                                   EL604
00334  0400-EDIT-INPUT-DATA.                                            EL604
00335      IF MAINTI = 'S'                                                 CL**4
00336         GO TO 4000-SHOW-REPORT.                                      CL*14
00337                                                                      CL*14
00338      IF   NOT MODIFY-CAP
00339           MOVE 'UPDATE'       TO SM-READ                             CL**4
00340           PERFORM 9995-SECURITY-VIOLATION                            CL**4
00341           MOVE ER-0070        TO EMI-ERROR                           CL**4
00342           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                   CL**4
00343           GO TO 8100-SEND-INITIAL-MAP.                               CL**4
00344                                                                   EL604
00345      IF MAINTI = 'A'                                              EL604
00346         GO TO 1000-ADD-REPORT.                                    EL604
00347                                                                   EL604
00348      IF MAINTI = 'C'                                              EL604
00349         GO TO 2000-CHANGE-REPORT.                                 EL604
00350                                                                   EL604
00351      IF MAINTI = 'D'                                              EL604
00352         GO TO 2500-DELETE-REPORT.                                 EL604
00353                                                                      CL*14
00354      IF MAINTI = 'K'                                                 CL*14
00355         GO TO 3000-COPY-REPORT.                                      CL*14
00356                                                                   EL604
00357      MOVE  ER-0023            TO EMI-ERROR                        EL604
00358      MOVE -1                  TO MAINTL                           EL604
00359      MOVE AL-UABON            TO MAINTA                           EL604
00360      PERFORM 9900-ERROR-FORMAT                                    EL604
00361      GO TO 8200-SEND-DATAONLY.                                    EL604
00362                                                                   EL604
00363      EJECT                                                        EL604
00364                                                                   EL604
00365  1000-ADD-REPORT.                                                 EL604
00366      IF REPORTL GREATER +0 AND                                    EL604
00367         REPORTI NUMERIC                                           EL604
00368         NEXT SENTENCE                                             EL604
00369      ELSE                                                         EL604
00370         MOVE ER-7680     TO EMI-ERROR                             EL604
00371         MOVE -1          TO REPORTL                               EL604
00372         MOVE AL-UNBON    TO REPORTA                               EL604
00373         PERFORM 9900-ERROR-FORMAT                                 EL604
00374         GO TO 8200-SEND-DATAONLY.                                 EL604
00375                                                                   EL604
00376      MOVE SPACES            TO ELCNTL-KEY.                        EL604
00377      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.                 EL604
00378      MOVE REPORTI           TO ELCNTL-REPORT.                     EL604
00379      MOVE 'C'               TO ELCNTL-RECORD-TYPE.                EL604
00380      MOVE +0                TO ELCNTL-SEQ-NO.                     EL604
00381                                                                   EL604
00382      EXEC CICS HANDLE CONDITION                                   EL604
00383           NOTFND     (1100-EDIT-REPORT-SCREEN)                    EL604
00384      END-EXEC.                                                    EL604
00385                                                                   EL604
00386      EXEC CICS READ                                               EL604
00387           DATASET    (ELCNTL-FILE-ID)                             EL604
00388           RIDFLD     (ELCNTL-KEY)                                 EL604
00389           SET        (ADDRESS OF CONTROL-FILE)                       CL*15
00390      END-EXEC.                                                    EL604
00391                                                                   EL604
00392      MOVE -1                     TO REPORTL.                      EL604
00393      MOVE  ER-0132               TO  EMI-ERROR.                   EL604
00394      PERFORM 9900-ERROR-FORMAT.                                   EL604
00395      GO TO 8200-SEND-DATAONLY.                                    EL604
00396                                                                   EL604
00397  1100-EDIT-REPORT-SCREEN.                                         EL604
00398      IF STATUSL GREATER +0                                        EL604
121307        IF STATUSI = 'A' OR 'I' OR 'B' OR 'C'
00400            NEXT SENTENCE                                          EL604
00401         ELSE                                                      EL604
00402            MOVE ER-7681            TO EMI-ERROR                   EL604
00403            MOVE -1                 TO STATUSL                     EL604
00404            MOVE AL-UNBON           TO STATUSA                     EL604
00405            PERFORM 9900-ERROR-FORMAT                              EL604
00406      ELSE                                                         EL604
00407         IF MAINTI = 'A'                                           EL604
00408            MOVE ER-7681            TO EMI-ERROR                   EL604
00409            MOVE -1                 TO STATUSL                     EL604
00410            MOVE AL-UNBON           TO STATUSA                     EL604
00411            PERFORM 9900-ERROR-FORMAT.                             EL604
00412                                                                   EL604
00413      IF CARSEQL GREATER THAN +0                                   EL604
00414         INSPECT CARSEQI CONVERTING SPACES TO ZEROS                   CL*15
00415         IF CARSEQI NUMERIC AND                                    EL604
00416            CARSEQI LESS THAN 7                                    EL604
00417            NEXT SENTENCE                                          EL604
00418         ELSE                                                      EL604
00419            MOVE ER-7682             TO EMI-ERROR                  EL604
00420            MOVE -1                  TO CARSEQL                    EL604
00421            MOVE AL-UNBON            TO CARSEQA                    EL604
00422            PERFORM 9900-ERROR-FORMAT.                             EL604
00423                                                                   EL604
00424      IF CAR1L GREATER THAN +0 AND                                 EL604
00425         CAR1I NOT = SPACES                                        EL604
00426         MOVE CAR1I         TO WS-TEST-CARRIER                        CL**6
00427         MOVE CAR1L         TO WS-TEST-LEN                            CL**6
00428         MOVE CAR1A         TO WS-TEST-ATTRB                          CL**6
00429         PERFORM 5000-VERIFY-CARRIER THRU 5019-EXIT                EL604
00430         MOVE WS-TEST-LEN   TO CAR1L                                  CL**6
00431         MOVE WS-TEST-ATTRB TO CAR1A.                              EL604
00432                                                                   EL604
00433      IF CAR2L GREATER THAN +0 AND                                 EL604
00434         CAR2I NOT = SPACES                                        EL604
00435         MOVE CAR2I         TO WS-TEST-CARRIER                        CL**6
00436         MOVE CAR2L         TO WS-TEST-LEN                            CL**6
00437         MOVE CAR2A         TO WS-TEST-ATTRB                          CL**6
00438         PERFORM 5000-VERIFY-CARRIER THRU 5019-EXIT                EL604
00439         MOVE WS-TEST-LEN   TO CAR2L                                  CL**6
00440         MOVE WS-TEST-ATTRB TO CAR2A.                              EL604
00441                                                                   EL604
00442      IF CAR3L GREATER THAN +0 AND                                 EL604
00443         CAR3I NOT = SPACES                                        EL604
00444         MOVE CAR3I         TO WS-TEST-CARRIER                        CL**6
00445         MOVE CAR3L         TO WS-TEST-LEN                            CL**6
00446         MOVE CAR3A         TO WS-TEST-ATTRB                          CL**6
00447         PERFORM 5000-VERIFY-CARRIER THRU 5019-EXIT                EL604
00448         MOVE WS-TEST-LEN   TO CAR3L                                  CL**6
00449         MOVE WS-TEST-ATTRB TO CAR3A.                              EL604
00450                                                                   EL604
00451      IF GRPSEQL GREATER THAN +0                                   EL604
00452         INSPECT GRPSEQI CONVERTING SPACES TO ZEROS                   CL*15
00453         IF GRPSEQI NUMERIC AND                                    EL604
00454            GRPSEQI LESS THAN 7                                    EL604
00455            NEXT SENTENCE                                          EL604
00456         ELSE                                                      EL604
00457            MOVE ER-7682             TO EMI-ERROR                  EL604
00458            MOVE -1                  TO GRPSEQL                    EL604
00459            MOVE AL-UNBON            TO GRPSEQA                    EL604
00460            PERFORM 9900-ERROR-FORMAT.                             EL604
00461                                                                   EL604
00462      IF STSEQL GREATER THAN +0                                    EL604
00463         INSPECT STSEQI CONVERTING SPACES TO ZEROS                    CL*15
00464         IF STSEQI NUMERIC AND                                     EL604
00465            STSEQI LESS THAN 7                                     EL604
00466            NEXT SENTENCE                                          EL604
00467         ELSE                                                      EL604
00468            MOVE ER-7682             TO EMI-ERROR                  EL604
00469            MOVE -1                  TO STSEQL                     EL604
00470            MOVE AL-UNBON            TO STSEQA                     EL604
00471            PERFORM 9900-ERROR-FORMAT.                             EL604
00472                                                                   EL604
00473      IF ST1L GREATER THAN +0 AND                                  EL604
00474         ST1I NOT = SPACES AND ZEROS                               EL604
00475         MOVE ST1I            TO WS-TEST-STATE                        CL**6
00476         MOVE ST1L            TO WS-TEST-LEN                          CL**6
00477         MOVE ST1A            TO WS-TEST-ATTRB                        CL**6
00478         PERFORM 5020-VERIFY-STATE THRU 5039-EXIT                  EL604
00479         MOVE WS-TEST-LEN     TO ST1L                                 CL**6
00480         MOVE WS-TEST-ATTRB   TO ST1A.                                CL**6
00481                                                                   EL604
00482      IF ST2L GREATER THAN +0 AND                                  EL604
00483         ST2I NOT = SPACES AND ZEROS                               EL604
00484         MOVE ST2I            TO WS-TEST-STATE                        CL**6
00485         MOVE ST2L            TO WS-TEST-LEN                          CL**6
00486         MOVE ST2A            TO WS-TEST-ATTRB                        CL**6
00487         PERFORM 5020-VERIFY-STATE THRU 5039-EXIT                  EL604
00488         MOVE WS-TEST-LEN     TO ST2L                                 CL**6
00489         MOVE WS-TEST-ATTRB   TO ST2A.                                CL**6
00490                                                                   EL604
00491      IF ST3L GREATER THAN +0 AND                                  EL604
00492         ST3I NOT = SPACES AND ZEROS                               EL604
00493         MOVE ST3I            TO WS-TEST-STATE                        CL**6
00494         MOVE ST3L            TO WS-TEST-LEN                          CL**6
00495         MOVE ST3A            TO WS-TEST-ATTRB                        CL**6
00496         PERFORM 5020-VERIFY-STATE THRU 5039-EXIT                  EL604
00497         MOVE WS-TEST-LEN     TO ST3L                                 CL**6
00498         MOVE WS-TEST-ATTRB   TO ST3A.                                CL**6
00499                                                                   EL604
00500      IF BUSSEQL GREATER THAN +0                                   EL604
00501         INSPECT BUSSEQI CONVERTING SPACES TO ZEROS                   CL*15
00502         IF BUSSEQI NUMERIC AND                                    EL604
00503            BUSSEQI LESS THAN 7                                    EL604
00504            NEXT SENTENCE                                          EL604
00505         ELSE                                                      EL604
00506            MOVE ER-7682             TO EMI-ERROR                  EL604
00507            MOVE -1                  TO BUSSEQL                    EL604
00508            MOVE AL-UNBON            TO BUSSEQA                    EL604
00509            PERFORM 9900-ERROR-FORMAT.                             EL604
00510                                                                   EL604
00511      IF BUS1L GREATER THAN +0                                     EL604
00512         INSPECT BUS1I CONVERTING SPACES TO ZEROS                     CL*15
00513         IF BUS1I NUMERIC AND                                      EL604
00514            BUS1I NOT = '00'                                       EL604
00515            MOVE BUS1I         TO WS-TEST-BUSTYP                      CL**6
00516            MOVE BUS1L         TO WS-TEST-LEN                         CL**6
00517            MOVE BUS1A         TO WS-TEST-ATTRB                       CL**6
00518            PERFORM 5060-VERIFY-BUS-TYPE THRU 5079-EXIT            EL604
00519            MOVE WS-TEST-LEN   TO BUS1L                               CL**6
00520            MOVE WS-TEST-ATTRB TO BUS1A                            EL604
00521         ELSE                                                      EL604
00522            IF BUS1I NOT NUMERIC                                   EL604
00523               MOVE ER-2178      TO EMI-ERROR                      EL604
00524               MOVE -1           TO BUS1L                          EL604
00525               MOVE AL-UNBON     TO BUS1A                          EL604
00526               PERFORM 9900-ERROR-FORMAT.                          EL604
00527                                                                   EL604
00528      IF BUS2L GREATER THAN +0                                     EL604
00529         INSPECT BUS2I CONVERTING SPACES TO ZEROS                     CL*15
00530         IF BUS2I NUMERIC AND                                      EL604
00531                  BUS2I NOT = '00'                                 EL604
00532            MOVE BUS2I          TO WS-TEST-BUSTYP                     CL**6
00533            MOVE BUS2L          TO WS-TEST-LEN                        CL**6
00534            MOVE BUS2A          TO WS-TEST-ATTRB                      CL**6
00535            PERFORM 5060-VERIFY-BUS-TYPE THRU 5079-EXIT            EL604
00536            MOVE WS-TEST-LEN    TO BUS2L                              CL**6
00537            MOVE WS-TEST-ATTRB  TO BUS2A                              CL**6
00538         ELSE                                                      EL604
00539            IF BUS2I NOT NUMERIC                                   EL604
00540               MOVE ER-2178      TO EMI-ERROR                      EL604
00541               MOVE -1           TO BUS2L                          EL604
00542               MOVE AL-UNBON     TO BUS2A                          EL604
00543               PERFORM 9900-ERROR-FORMAT.                          EL604
00544                                                                   EL604
00545      IF BUS3L GREATER THAN +0                                     EL604
00546         INSPECT BUS3I CONVERTING SPACES TO ZEROS                     CL*15
00547         IF BUS3I NUMERIC AND                                      EL604
00548            BUS3I NOT = '00'                                       EL604
00549            MOVE BUS3I         TO WS-TEST-BUSTYP                      CL**6
00550            MOVE BUS3L         TO WS-TEST-LEN                         CL**6
00551            MOVE BUS3A         TO WS-TEST-ATTRB                       CL**6
00552            PERFORM 5060-VERIFY-BUS-TYPE THRU 5079-EXIT            EL604
00553            MOVE WS-TEST-LEN   TO BUS3L                               CL**6
00554            MOVE WS-TEST-ATTRB TO BUS3A                            EL604
00555         ELSE                                                      EL604
00556            IF BUS3I NOT NUMERIC                                   EL604
00557               MOVE ER-2178      TO EMI-ERROR                      EL604
00558               MOVE -1           TO BUS3L                          EL604
00559               MOVE AL-UNBON     TO BUS3A                          EL604
00560               PERFORM 9900-ERROR-FORMAT.                          EL604
00561                                                                   EL604
00562      IF LBEN1L GREATER THAN +0                                    EL604
00563         MOVE LBEN1I           TO WS-TEST-BEN                         CL**6
00564         IF NOT INVALID-BENEFIT-CODE                                  CL**2
00565            MOVE LBEN1L        TO WS-TEST-LEN                         CL**6
00566            MOVE LBEN1A        TO WS-TEST-ATTRB                       CL**6
00567            MOVE SPACES        TO ELCNTL-TEST-KEY                     CL**6
00568            MOVE '4'           TO ELCNTL-TEST-REC-TYPE                CL**6
00569            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT                 EL604
00570            MOVE WS-TEST-LEN   TO LBEN1L                              CL**6
00571            MOVE WS-TEST-ATTRB TO LBEN1A                           EL604
00572         ELSE                                                      EL604
00573            MOVE ER-7125       TO EMI-ERROR                           CL**6
00574            MOVE -1            TO LBEN1L                              CL**6
00575            MOVE AL-UABON      TO LBEN1A                              CL**6
00576            PERFORM 9900-ERROR-FORMAT.                                CL**2
00577                                                                   EL604
00578      IF LBEN2L GREATER THAN +0                                    EL604
00579         MOVE LBEN2I           TO WS-TEST-BEN                         CL**6
00580         IF NOT INVALID-BENEFIT-CODE                                  CL**2
00581            MOVE LBEN2L        TO WS-TEST-LEN                         CL**6
00582            MOVE LBEN2A        TO WS-TEST-ATTRB                       CL**6
00583            MOVE SPACES        TO ELCNTL-TEST-KEY                     CL**6
00584            MOVE '4'           TO ELCNTL-TEST-REC-TYPE                CL**6
00585            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT                 EL604
00586            MOVE WS-TEST-LEN   TO LBEN2L                              CL**6
00587            MOVE WS-TEST-ATTRB TO LBEN2A                           EL604
00588         ELSE                                                      EL604
00589            MOVE ER-7125       TO EMI-ERROR                           CL**6
00590            MOVE -1            TO LBEN2L                              CL**6
00591            MOVE AL-UABON      TO LBEN2A                              CL**6
00592            PERFORM 9900-ERROR-FORMAT.                                CL**2
00593                                                                   EL604
00594      IF LBEN3L GREATER THAN +0                                    EL604
00595         MOVE LBEN3I           TO WS-TEST-BEN                         CL**6
00596         IF NOT INVALID-BENEFIT-CODE                                  CL**2
00597            MOVE LBEN3L        TO WS-TEST-LEN                         CL**6
00598            MOVE LBEN3A        TO WS-TEST-ATTRB                       CL**6
00599            MOVE SPACES        TO ELCNTL-TEST-KEY                     CL**6
00600            MOVE '4'           TO ELCNTL-TEST-REC-TYPE                CL**6
00601            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT                 EL604
00602            MOVE WS-TEST-LEN   TO LBEN3L                              CL**6
00603            MOVE WS-TEST-ATTRB TO LBEN3A                           EL604
00604         ELSE                                                      EL604
00605            MOVE ER-7125       TO EMI-ERROR                           CL**6
00606            MOVE -1            TO LBEN3L                              CL**6
00607            MOVE AL-UABON      TO LBEN3A                              CL**6
00608            PERFORM 9900-ERROR-FORMAT.                                CL**2
00609                                                                   EL604
00610      IF ABEN1L GREATER THAN +0                                    EL604
00611         MOVE ABEN1I           TO WS-TEST-BEN                         CL**6
00612         IF NOT INVALID-BENEFIT-CODE                                  CL**2
00613            MOVE ABEN1L        TO WS-TEST-LEN                         CL**6
00614            MOVE ABEN1A        TO WS-TEST-ATTRB                       CL**6
00615            MOVE SPACES        TO ELCNTL-TEST-KEY                     CL**6
00616            MOVE '5'           TO ELCNTL-TEST-REC-TYPE                CL**6
00617            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT                 EL604
00618            MOVE WS-TEST-LEN   TO ABEN1L                              CL**6
00619            MOVE WS-TEST-ATTRB TO ABEN1A                           EL604
00620         ELSE                                                      EL604
00621            MOVE ER-7125       TO EMI-ERROR                           CL**6
00622            MOVE -1            TO ABEN1L                              CL**6
00623            MOVE AL-UABON      TO ABEN1A                              CL**6
00624            PERFORM 9900-ERROR-FORMAT.                                CL**2
00625                                                                   EL604
00626      IF ABEN2L GREATER THAN +0                                    EL604
00627         MOVE ABEN2I           TO WS-TEST-BEN                         CL**6
00628         IF NOT INVALID-BENEFIT-CODE                                  CL**2
00629            MOVE ABEN2L        TO WS-TEST-LEN                         CL**6
00630            MOVE ABEN2A        TO WS-TEST-ATTRB                       CL**6
00631            MOVE SPACES        TO ELCNTL-TEST-KEY                     CL**6
00632            MOVE '5'           TO ELCNTL-TEST-REC-TYPE                CL**6
00633            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT                 EL604
00634            MOVE WS-TEST-LEN   TO ABEN2L                              CL**6
00635            MOVE WS-TEST-ATTRB TO ABEN2A                           EL604
00636         ELSE                                                      EL604
00637            MOVE ER-7125       TO EMI-ERROR                           CL**6
00638            MOVE -1            TO ABEN2L                              CL**6
00639            MOVE AL-UABON      TO ABEN2A                              CL**6
00640            PERFORM 9900-ERROR-FORMAT.                                CL**2
00641                                                                   EL604
00642      IF ABEN3L GREATER THAN +0                                    EL604
00643         MOVE ABEN3I           TO WS-TEST-BEN                         CL**6
00644         IF NOT INVALID-BENEFIT-CODE                                  CL**2
00645            MOVE ABEN3L        TO WS-TEST-LEN                         CL**6
00646            MOVE ABEN3A        TO WS-TEST-ATTRB                       CL**6
00647            MOVE SPACES        TO ELCNTL-TEST-KEY                     CL**6
00648            MOVE '5'           TO ELCNTL-TEST-REC-TYPE                CL**6
00649            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT                 EL604
00650            MOVE WS-TEST-LEN   TO ABEN3L                              CL**6
00651            MOVE WS-TEST-ATTRB TO ABEN3A                           EL604
00652         ELSE                                                      EL604
00653            MOVE ER-7125       TO EMI-ERROR                           CL**6
00654            MOVE -1            TO ABEN3L                              CL**6
00655            MOVE AL-UABON      TO ABEN3A                              CL**6
00656            PERFORM 9900-ERROR-FORMAT.                                CL**2
00657                                                                   EL604
00658      IF ACCTSEQL GREATER THAN +0                                  EL604
00659         INSPECT ACCTSEQI CONVERTING SPACES TO ZEROS                  CL*15
00660         IF ACCTSEQI NUMERIC AND                                   EL604
00661            ACCTSEQI LESS THAN 7                                   EL604
00662            NEXT SENTENCE                                          EL604
00663         ELSE                                                      EL604
00664            MOVE ER-7682             TO EMI-ERROR                  EL604
00665            MOVE -1                  TO ACCTSEQL                   EL604
00666            MOVE AL-UNBON            TO ACCTSEQA                   EL604
00667            PERFORM 9900-ERROR-FORMAT.                             EL604
00668                                                                   EL604
00669      IF AGNTSEQL GREATER THAN +0                                     CL*10
00670         INSPECT AGNTSEQI CONVERTING SPACES TO ZEROS                  CL*15
00671         IF AGNTSEQI NUMERIC AND                                      CL*10
00672            AGNTSEQI LESS THAN 7                                      CL*10
00673            NEXT SENTENCE                                             CL*10
00674         ELSE                                                         CL*10
00675            MOVE ER-7682             TO EMI-ERROR                     CL*10
00676            MOVE -1                  TO AGNTSEQL                      CL*10
00677            MOVE AL-UNBON            TO AGNTSEQA                      CL*10
00678            PERFORM 9900-ERROR-FORMAT.                                CL*10
00679                                                                      CL*10
00680      IF REINSEQL GREATER THAN +0                                     CL**7
00681         INSPECT REINSEQI CONVERTING SPACES TO ZEROS                  CL*15
00682         IF REINSEQI NUMERIC AND                                      CL**7
00683            REINSEQI LESS THAN 7                                      CL**7
00684            NEXT SENTENCE                                             CL**7
00685         ELSE                                                         CL**7
00686            MOVE ER-7682             TO EMI-ERROR                     CL**7
00687            MOVE -1                  TO REINSEQL                      CL**7
00688            MOVE AL-UNBON            TO REINSEQA                      CL**7
00689            PERFORM 9900-ERROR-FORMAT.                                CL**7
00690                                                                      CL**7
00691      IF RPT1SEQL GREATER THAN +0                                  EL604
00692         INSPECT RPT1SEQI CONVERTING SPACES TO ZEROS                  CL*15
00693         IF RPT1SEQI NUMERIC AND                                   EL604
00694            RPT1SEQI LESS THAN 7                                   EL604
00695            NEXT SENTENCE                                          EL604
00696         ELSE                                                      EL604
00697            MOVE ER-7682             TO EMI-ERROR                  EL604
00698            MOVE -1                  TO RPT1SEQL                   EL604
00699            MOVE AL-UNBON            TO RPT1SEQA                   EL604
00700            PERFORM 9900-ERROR-FORMAT.                             EL604
00701                                                                   EL604
00702      IF RPT2SEQL GREATER THAN +0                                  EL604
00703         INSPECT RPT2SEQI CONVERTING SPACES TO ZEROS                  CL*15
00704         IF RPT2SEQI NUMERIC AND                                   EL604
00705            RPT2SEQI LESS THAN 7                                   EL604
00706            NEXT SENTENCE                                          EL604
00707         ELSE                                                      EL604
00708            MOVE ER-7682             TO EMI-ERROR                  EL604
00709            MOVE -1                  TO RPT2SEQL                   EL604
00710            MOVE AL-UNBON            TO RPT2SEQA                   EL604
00711            PERFORM 9900-ERROR-FORMAT.                             EL604
00712                                                                   EL604
00713      IF USR1SEQL GREATER THAN +0                                  EL604
00714         INSPECT USR1SEQI CONVERTING SPACES TO ZEROS                  CL*15
00715         IF USR1SEQI NUMERIC AND                                   EL604
00716            USR1SEQI LESS THAN 7                                   EL604
00717            NEXT SENTENCE                                          EL604
00718         ELSE                                                      EL604
00719            MOVE ER-7682             TO EMI-ERROR                  EL604
00720            MOVE -1                  TO USR1SEQL                   EL604
00721            MOVE AL-UNBON            TO USR1SEQA                   EL604
00722            PERFORM 9900-ERROR-FORMAT.                             EL604
00723                                                                   EL604
00724      IF USR2SEQL GREATER THAN +0                                  EL604
00725         INSPECT USR2SEQI CONVERTING SPACES TO ZEROS                  CL*15
00726         IF USR2SEQI NUMERIC AND                                   EL604
00727            USR2SEQI LESS THAN 7                                   EL604
00728            NEXT SENTENCE                                          EL604
00729         ELSE                                                      EL604
00730            MOVE ER-7682             TO EMI-ERROR                  EL604
00731            MOVE -1                  TO USR2SEQL                   EL604
00732            MOVE AL-UNBON            TO USR2SEQA                   EL604
00733            PERFORM 9900-ERROR-FORMAT.                             EL604
00734                                                                   EL604
00735      IF USR3SEQL GREATER THAN +0                                  EL604
00736         INSPECT USR3SEQI CONVERTING SPACES TO ZEROS                  CL*15
00737         IF USR3SEQI NUMERIC AND                                   EL604
00738            USR3SEQI LESS THAN 7                                   EL604
00739            NEXT SENTENCE                                          EL604
00740         ELSE                                                      EL604
00741            MOVE ER-7682             TO EMI-ERROR                  EL604
00742            MOVE -1                  TO USR3SEQL                   EL604
00743            MOVE AL-UNBON            TO USR3SEQA                   EL604
00744            PERFORM 9900-ERROR-FORMAT.                             EL604
00745                                                                   EL604
00746      IF USR4SEQL GREATER THAN +0                                  EL604
00747         INSPECT USR4SEQI CONVERTING SPACES TO ZEROS                  CL*15
00748         IF USR4SEQI NUMERIC AND                                   EL604
00749            USR4SEQI LESS THAN 7                                   EL604
00750            NEXT SENTENCE                                          EL604
00751         ELSE                                                      EL604
00752            MOVE ER-7682             TO EMI-ERROR                  EL604
00753            MOVE -1                  TO USR4SEQL                   EL604
00754            MOVE AL-UNBON            TO USR4SEQA                   EL604
00755            PERFORM 9900-ERROR-FORMAT.                             EL604
00756                                                                   EL604
00757      IF USR5SEQL GREATER THAN +0                                  EL604
00758         INSPECT USR5SEQI CONVERTING SPACES TO ZEROS                  CL*15
00759         IF USR5SEQI NUMERIC AND                                   EL604
00760            USR5SEQI LESS THAN 7                                   EL604
00761            NEXT SENTENCE                                          EL604
00762         ELSE                                                      EL604
00763            MOVE ER-7682             TO EMI-ERROR                  EL604
00764            MOVE -1                  TO USR5SEQL                   EL604
00765            MOVE AL-UNBON            TO USR5SEQA                   EL604
00766            PERFORM 9900-ERROR-FORMAT.                                CL*14
00767                                                                      CL*14
00768      IF EXPRPTL GREATER THAN +0                                      CL*14
00769         IF EXPRPTI EQUAL 'Y' OR 'N'                                  CL*14
00770             NEXT SENTENCE                                            CL*14
00771         ELSE                                                         CL*14
00772            MOVE ER-7745             TO EMI-ERROR                     CL*14
00773            MOVE -1                  TO EXPRPTL                       CL*14
00774            MOVE AL-UNBON            TO EXPRPTA                       CL*14
00775            PERFORM 9900-ERROR-FORMAT.                             EL604
00776                                                                   EL604
00777      IF NOT EMI-NO-ERRORS                                         EL604
00778         GO TO 8200-SEND-DATAONLY.                                 EL604
00779                                                                   EL604
00780      MOVE SPACES TO WS-SEQ-AREA.                                  EL604
00781                                                                   EL604
00782      IF CARSEQL GREATER THAN +0 AND                               EL604
00783         CARSEQI NOT = 0                                           EL604
00784         IF WS-SEQ-NO (CARSEQI) NOT = 'X'                          EL604
00785            MOVE 'X'              TO WS-SEQ-NO (CARSEQI)              CL**6
00786         ELSE                                                      EL604
00787            MOVE ER-7682          TO EMI-ERROR                     EL604
00788            MOVE -1               TO CARSEQL                       EL604
00789            MOVE AL-UNBON         TO CARSEQA                       EL604
00790            PERFORM 9900-ERROR-FORMAT.                             EL604
00791                                                                   EL604
00792      IF GRPSEQL GREATER THAN +0 AND                               EL604
00793         GRPSEQI NOT = 0                                           EL604
00794         IF WS-SEQ-NO (GRPSEQI) NOT = 'X'                          EL604
00795            MOVE 'X'              TO WS-SEQ-NO (GRPSEQI)              CL**6
00796         ELSE                                                      EL604
00797            MOVE ER-7682          TO EMI-ERROR                     EL604
00798            MOVE -1               TO GRPSEQL                       EL604
00799            MOVE AL-UNBON         TO GRPSEQA                       EL604
00800            PERFORM 9900-ERROR-FORMAT.                                CL**7
00801                                                                      CL**7
00802      IF REINSEQL GREATER THAN +0 AND                                 CL**7
00803         REINSEQI NOT = 0                                             CL**7
00804         IF WS-SEQ-NO (REINSEQI) NOT = 'X'                            CL**7
00805            MOVE 'X'              TO WS-SEQ-NO (REINSEQI)             CL**7
00806         ELSE                                                         CL**7
00807            MOVE ER-7682          TO EMI-ERROR                        CL**7
00808            MOVE -1               TO REINSEQL                         CL**7
00809            MOVE AL-UNBON         TO REINSEQA                         CL**7
00810            PERFORM 9900-ERROR-FORMAT.                             EL604
00811                                                                   EL604
00812      IF STSEQL GREATER THAN +0 AND                                EL604
00813         STSEQI NOT = 0                                            EL604
00814         IF WS-SEQ-NO (STSEQI) NOT = 'X'                           EL604
00815            MOVE 'X'              TO WS-SEQ-NO (STSEQI)               CL**6
00816         ELSE                                                      EL604
00817            MOVE ER-7682          TO EMI-ERROR                     EL604
00818            MOVE -1               TO STSEQL                        EL604
00819            MOVE AL-UNBON         TO STSEQA                        EL604
00820            PERFORM 9900-ERROR-FORMAT.                             EL604
00821                                                                   EL604
00822      IF BUSSEQL GREATER THAN +0 AND                               EL604
00823         BUSSEQI NOT = 0                                           EL604
00824         IF WS-SEQ-NO (BUSSEQI) NOT = 'X'                          EL604
00825            MOVE 'X'              TO WS-SEQ-NO (BUSSEQI)              CL**6
00826         ELSE                                                      EL604
00827            MOVE ER-7682          TO EMI-ERROR                     EL604
00828            MOVE -1               TO BUSSEQL                       EL604
00829            MOVE AL-UNBON         TO BUSSEQA                       EL604
00830            PERFORM 9900-ERROR-FORMAT.                             EL604
00831                                                                   EL604
00832      IF ACCTSEQL GREATER THAN +0 AND                              EL604
00833         ACCTSEQI NOT = 0                                          EL604
00834         IF WS-SEQ-NO (ACCTSEQI) NOT = 'X'                         EL604
00835            MOVE 'X'              TO WS-SEQ-NO (ACCTSEQI)             CL**6
00836         ELSE                                                      EL604
00837            MOVE ER-7682          TO EMI-ERROR                     EL604
00838            MOVE -1               TO ACCTSEQL                      EL604
00839            MOVE AL-UNBON         TO ACCTSEQA                      EL604
00840            PERFORM 9900-ERROR-FORMAT.                                CL*10
00841                                                                      CL*10
00842      IF AGNTSEQL GREATER THAN +0 AND                                 CL*10
00843         AGNTSEQI NOT = 0                                             CL*10
00844         IF WS-SEQ-NO (AGNTSEQI) NOT = 'X'                            CL*10
00845            MOVE 'X'              TO WS-SEQ-NO (AGNTSEQI)             CL*10
00846         ELSE                                                         CL*10
00847            MOVE ER-7682          TO EMI-ERROR                        CL*10
00848            MOVE -1               TO AGNTSEQL                         CL*10
00849            MOVE AL-UNBON         TO AGNTSEQA                         CL*10
00850            PERFORM 9900-ERROR-FORMAT.                             EL604
00851                                                                   EL604
00852      IF RPT1SEQL GREATER THAN +0 AND                              EL604
00853         RPT1SEQI NOT = 0                                          EL604
00854         IF WS-SEQ-NO (RPT1SEQI) NOT = 'X'                         EL604
00855            MOVE 'X'              TO WS-SEQ-NO (RPT1SEQI)             CL**6
00856         ELSE                                                      EL604
00857            MOVE ER-7682          TO EMI-ERROR                     EL604
00858            MOVE -1               TO RPT1SEQL                      EL604
00859            MOVE AL-UNBON         TO RPT1SEQA                      EL604
00860            PERFORM 9900-ERROR-FORMAT.                             EL604
00861                                                                   EL604
00862      IF RPT2SEQL GREATER THAN +0 AND                              EL604
00863         RPT2SEQI NOT = 0                                          EL604
00864         IF WS-SEQ-NO (RPT2SEQI) NOT = 'X'                         EL604
00865            MOVE 'X'              TO WS-SEQ-NO (RPT2SEQI)             CL**6
00866         ELSE                                                      EL604
00867            MOVE ER-7682          TO EMI-ERROR                     EL604
00868            MOVE -1               TO RPT2SEQL                      EL604
00869            MOVE AL-UNBON         TO RPT2SEQA                      EL604
00870            PERFORM 9900-ERROR-FORMAT.                             EL604
00871                                                                   EL604
00872      IF USR1SEQL GREATER THAN +0 AND                              EL604
00873         USR1SEQI NOT = 0                                          EL604
00874         IF WS-SEQ-NO (USR1SEQI) NOT = 'X'                         EL604
00875            MOVE 'X'              TO WS-SEQ-NO (USR1SEQI)             CL**6
00876         ELSE                                                      EL604
00877            MOVE ER-7682          TO EMI-ERROR                     EL604
00878            MOVE -1               TO USR1SEQL                      EL604
00879            MOVE AL-UNBON         TO USR1SEQA                      EL604
00880            PERFORM 9900-ERROR-FORMAT.                             EL604
00881                                                                   EL604
00882      IF USR2SEQL GREATER THAN +0 AND                              EL604
00883         USR2SEQI NOT = 0                                          EL604
00884         IF WS-SEQ-NO (USR2SEQI) NOT = 'X'                         EL604
00885            MOVE 'X'              TO WS-SEQ-NO (USR2SEQI)             CL**6
00886         ELSE                                                      EL604
00887            MOVE ER-7682          TO EMI-ERROR                     EL604
00888            MOVE -1               TO USR2SEQL                      EL604
00889            MOVE AL-UNBON         TO USR2SEQA                      EL604
00890            PERFORM 9900-ERROR-FORMAT.                             EL604
00891                                                                   EL604
00892      IF USR3SEQL GREATER THAN +0 AND                              EL604
00893         USR3SEQI NOT = 0                                          EL604
00894         IF WS-SEQ-NO (USR3SEQI) NOT = 'X'                         EL604
00895            MOVE 'X'              TO WS-SEQ-NO (USR3SEQI)             CL**6
00896         ELSE                                                      EL604
00897            MOVE ER-7682          TO EMI-ERROR                     EL604
00898            MOVE -1               TO USR3SEQL                      EL604
00899            MOVE AL-UNBON         TO USR3SEQA                      EL604
00900            PERFORM 9900-ERROR-FORMAT.                             EL604
00901                                                                   EL604
00902      IF USR4SEQL GREATER THAN +0 AND                              EL604
00903         USR4SEQI NOT = 0                                          EL604
00904         IF WS-SEQ-NO (USR4SEQI) NOT = 'X'                         EL604
00905            MOVE 'X'              TO WS-SEQ-NO (USR4SEQI)             CL**6
00906         ELSE                                                      EL604
00907            MOVE ER-7682          TO EMI-ERROR                     EL604
00908            MOVE -1               TO USR4SEQL                      EL604
00909            MOVE AL-UNBON         TO USR4SEQA                      EL604
00910            PERFORM 9900-ERROR-FORMAT.                             EL604
00911                                                                   EL604
00912      IF USR5SEQL GREATER THAN +0 AND                              EL604
00913         USR5SEQI NOT = 0                                          EL604
00914         IF WS-SEQ-NO (USR5SEQI) NOT = 'X'                         EL604
00915            MOVE 'X'              TO WS-SEQ-NO (USR5SEQI)             CL**6
00916         ELSE                                                      EL604
00917            MOVE ER-7682          TO EMI-ERROR                     EL604
00918            MOVE -1               TO USR5SEQL                      EL604
00919            MOVE AL-UNBON         TO USR5SEQA                      EL604
00920            PERFORM 9900-ERROR-FORMAT.                             EL604
00921                                                                   EL604
00922      IF NOT EMI-NO-ERRORS                                         EL604
00923         GO TO 8200-SEND-DATAONLY                                  EL604
00924                                                                   EL604
00925      MOVE +0 TO WS-LO-LOSS WS-HI-LOSS.                            EL604
00926                                                                   EL604
00927      IF  LOLOSSL GREATER THAN +0                                  EL604
00928          MOVE LOLOSSI                TO DEEDIT-FIELD              EL604
00929          PERFORM 8600-DEEDIT                                      EL604
00930          IF  DEEDIT-FIELD-V0  NOT NUMERIC                            CL**6
00931              MOVE -1             TO LOLOSSL                       EL604
00932              MOVE AL-UNBON       TO LOLOSSA                       EL604
00933              MOVE  ER-7683       TO EMI-ERROR                     EL604
00934              PERFORM 9900-ERROR-FORMAT                            EL604
00935          ELSE                                                     EL604
00936              MOVE DEEDIT-FIELD-V2     TO WS-LO-LOSS.              EL604
00937                                                                   EL604
00938      IF  HILOSSL GREATER THAN +0                                  EL604
00939          MOVE HILOSSI                TO DEEDIT-FIELD              EL604
00940          PERFORM 8600-DEEDIT                                      EL604
00941          IF  DEEDIT-FIELD-V0     NOT NUMERIC                      EL604
00942              MOVE -1             TO HILOSSL                       EL604
00943              MOVE AL-UNBON       TO HILOSSA                       EL604
00944              MOVE  ER-7683       TO EMI-ERROR                     EL604
00945              PERFORM 9900-ERROR-FORMAT                            EL604
00946          ELSE                                                     EL604
00947              MOVE DEEDIT-FIELD-V2     TO WS-HI-LOSS.              EL604
00948                                                                   EL604
00949      IF MAINTI = 'A'                                              EL604
00950         IF WS-LO-LOSS GREATER THAN WS-HI-LOSS                     EL604
00951            MOVE ER-7683             TO EMI-ERROR                  EL604
00952            MOVE -1                  TO LOLOSSL                    EL604
00953            MOVE AL-UNBON            TO LOLOSSA                    EL604
00954            PERFORM 9900-ERROR-FORMAT.                             EL604
00955                                                                   EL604
00956      MOVE LOW-VALUES TO WS-LO-EFF     WS-HI-EFF                   EL604
00957                         WS-LO-ENT     WS-HI-ENT.                  EL604
00958                                                                   EL604
00959      IF LOEFFDTL GREATER THAN +0                                  EL604
00960         MOVE LOEFFDTI               TO DEEDIT-FIELD               EL604
00961         PERFORM 8600-DEEDIT                                       EL604
00962         IF DEEDIT-FIELD-V0     NOT NUMERIC                        EL604
00963            MOVE -1             TO LOEFFDTL                        EL604
00964            MOVE AL-UNBON       TO LOEFFDTA                        EL604
00965            MOVE  ER-0021       TO EMI-ERROR                       EL604
00966            PERFORM 9900-ERROR-FORMAT                              EL604
00967         ELSE                                                      EL604
00968            IF DEEDIT-FIELD-V0 = ZEROS                             EL604
00969               MOVE LOW-VALUES      TO WS-LO-EFF                      CL**6
00970            ELSE                                                   EL604
00971               MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY          EL604
00972               MOVE '4'             TO DC-OPTION-CODE              EL604
00973               PERFORM 9700-DATE-LINK                              EL604
00974               IF NO-CONVERSION-ERROR                              EL604
00975                  MOVE DC-BIN-DATE-1 TO WS-LO-EFF                  EL604
00976               ELSE                                                EL604
00977                  MOVE ER-0021       TO EMI-ERROR                  EL604
00978                  MOVE -1            TO LOEFFDTL                   EL604
00979                  MOVE AL-UNBON      TO LOEFFDTA                   EL604
00980                  PERFORM 9900-ERROR-FORMAT.                       EL604
00981                                                                   EL604
00982      IF HIEFFDTL GREATER THAN +0                                  EL604
00983         MOVE HIEFFDTI               TO DEEDIT-FIELD               EL604
00984         PERFORM 8600-DEEDIT                                       EL604
00985         IF DEEDIT-FIELD-V0     NOT NUMERIC                        EL604
00986            MOVE -1             TO HIEFFDTL                        EL604
00987            MOVE AL-UNBON       TO HIEFFDTA                        EL604
00988            MOVE  ER-0021       TO EMI-ERROR                       EL604
00989            PERFORM 9900-ERROR-FORMAT                              EL604
00990         ELSE                                                      EL604
00991            IF DEEDIT-FIELD-V0 = ZEROS                             EL604
00992               MOVE LOW-VALUES TO WS-HI-EFF                        EL604
00993            ELSE                                                   EL604
00994               MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY          EL604
00995               MOVE '4'             TO DC-OPTION-CODE              EL604
00996               PERFORM 9700-DATE-LINK                              EL604
00997               IF NO-CONVERSION-ERROR                              EL604
00998                  MOVE DC-BIN-DATE-1 TO WS-HI-EFF                  EL604
00999               ELSE                                                EL604
01000                  IF DEEDIT-FIELD-V0 = 999999 OR 9999999 OR        EL604
01001                                           99999999                EL604
01002                     MOVE HIGH-VALUES TO WS-HI-EFF                 EL604
01003                  ELSE                                             EL604
01004                     MOVE ER-0021       TO EMI-ERROR               EL604
01005                     MOVE -1            TO HIEFFDTL                EL604
01006                     MOVE AL-UNBON      TO HIEFFDTA                EL604
01007                     PERFORM 9900-ERROR-FORMAT.                    EL604
01008                                                                   EL604
01009      IF MAINTI = 'A'                                              EL604
01010         IF WS-LO-EFF GREATER THAN WS-HI-EFF                       EL604
01011            MOVE ER-7684            TO EMI-ERROR                   EL604
01012            MOVE -1                 TO LOEFFDTL                    EL604
01013            MOVE AL-UNBON           TO LOEFFDTA                    EL604
01014            PERFORM 9900-ERROR-FORMAT.                             EL604
01015                                                                   EL604
01016      IF LOENTDTL GREATER THAN +0                                  EL604
01017         MOVE LOENTDTI               TO DEEDIT-FIELD               EL604
01018         PERFORM 8600-DEEDIT                                       EL604
01019         IF DEEDIT-FIELD-V0     NOT NUMERIC                        EL604
01020            MOVE -1             TO LOENTDTL                        EL604
01021            MOVE AL-UNBON       TO LOENTDTA                        EL604
01022            MOVE  ER-0021       TO EMI-ERROR                       EL604
01023            PERFORM 9900-ERROR-FORMAT                              EL604
01024         ELSE                                                      EL604
01025            IF DEEDIT-FIELD-V0 = ZEROS                             EL604
01026               MOVE LOW-VALUES TO WS-LO-ENT                        EL604
01027            ELSE                                                   EL604
01028               MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY          EL604
01029               MOVE '4'             TO DC-OPTION-CODE              EL604
01030               PERFORM 9700-DATE-LINK                              EL604
01031               IF NO-CONVERSION-ERROR                              EL604
01032                  MOVE DC-BIN-DATE-1 TO WS-LO-ENT                  EL604
01033               ELSE                                                EL604
01034                  MOVE ER-0021       TO EMI-ERROR                  EL604
01035                  MOVE -1            TO LOENTDTL                   EL604
01036                  MOVE AL-UNBON      TO LOENTDTA                   EL604
01037                  PERFORM 9900-ERROR-FORMAT.                       EL604
01038                                                                   EL604
01039      IF HIENTDTL GREATER THAN +0                                  EL604
01040         MOVE HIENTDTI               TO DEEDIT-FIELD               EL604
01041         PERFORM 8600-DEEDIT                                       EL604
01042         IF DEEDIT-FIELD-V0     NOT NUMERIC                        EL604
01043            MOVE -1             TO HIENTDTL                        EL604
01044            MOVE AL-UNBON       TO HIENTDTA                        EL604
01045            MOVE  ER-0021       TO EMI-ERROR                       EL604
01046            PERFORM 9900-ERROR-FORMAT                              EL604
01047          ELSE                                                     EL604
01048            IF DEEDIT-FIELD-V0 = ZEROS                             EL604
01049               MOVE LOW-VALUES TO WS-LO-ENT                        EL604
01050            ELSE                                                   EL604
01051               MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY          EL604
01052               MOVE '4'             TO DC-OPTION-CODE              EL604
01053               PERFORM 9700-DATE-LINK                              EL604
01054               IF NO-CONVERSION-ERROR                              EL604
01055                  MOVE DC-BIN-DATE-1 TO WS-HI-ENT                  EL604
01056               ELSE                                                EL604
01057                  IF DEEDIT-FIELD-V0 = 999999 OR 9999999 OR        EL604
01058                                           99999999                EL604
01059                     MOVE HIGH-VALUES TO WS-HI-ENT                 EL604
01060                  ELSE                                             EL604
01061                     MOVE ER-0021       TO EMI-ERROR               EL604
01062                     MOVE -1            TO HIENTDTL                EL604
01063                     MOVE AL-UNBON      TO HIENTDTA                EL604
01064                     PERFORM 9900-ERROR-FORMAT.                    EL604
01065                                                                   EL604
01066      IF MAINTI = 'A'                                              EL604
01067         IF WS-LO-ENT GREATER THAN WS-HI-ENT                       EL604
01068            MOVE ER-7684            TO EMI-ERROR                   EL604
01069            MOVE -1                 TO LOENTDTL                    EL604
01070            MOVE AL-UNBON           TO LOENTDTA                    EL604
01071            PERFORM 9900-ERROR-FORMAT.                             EL604
01072                                                                   EL604
01073      IF NOT EMI-NO-ERRORS                                         EL604
01074         GO TO 8200-SEND-DATAONLY.                                 EL604
01075                                                                   EL604
01076  1200-ADD-REPORT-RECORD.                                          EL604
01077     EXEC CICS GETMAIN                                             EL604
01078          SET       (ADDRESS OF CONTROL-FILE)                         CL*15
01079          LENGTH    (ELCNTL-LENGTH)                                EL604
01080          INITIMG   (GETMAIN-SPACE)                                EL604
01081      END-EXEC.                                                    EL604
01082                                                                   EL604
01083      MOVE ZEROS                 TO CF-CARRIER-OPT-SEQ             EL604
01084                                    CF-GROUP-OPT-SEQ               EL604
01085                                    CF-STATE-OPT-SEQ               EL604
01086                                    CF-ACCOUNT-OPT-SEQ             EL604
01087                                    CF-AGENT-OPT-SEQ                  CL*10
01088                                    CF-REINS-OPT-SEQ                  CL**7
01089                                    CF-BUS-TYP-OPT-SEQ             EL604
01090                                    CF-LF-TYP-OPT-SEQ              EL604
01091                                    CF-AH-TYP-OPT-SEQ              EL604
01092                                    CF-REPTCD1-OPT-SEQ             EL604
01093                                    CF-REPTCD2-OPT-SEQ             EL604
01094                                    CF-USER1-OPT-SEQ               EL604
01095                                    CF-USER2-OPT-SEQ               EL604
01096                                    CF-USER3-OPT-SEQ               EL604
01097                                    CF-USER4-OPT-SEQ               EL604
01098                                    CF-USER5-OPT-SEQ.              EL604
01099      MOVE +0                    TO CF-SEL-LO-LOSS-RATIO           EL604
01100                                    CF-SEL-HI-LOSS-RATIO.          EL604
01101                                                                   EL604
01102      MOVE LOW-VALUES            TO CF-SEL-LO-ENTRY-DATE           EL604
01103                                    CF-SEL-HI-ENTRY-DATE           EL604
01104                                    CF-SEL-LO-EFFECTIVE-DATE       EL604
01105                                    CF-SEL-HI-EFFECTIVE-DATE.      EL604
01106                                                                   EL604
01107  1500-BUILD-RECORD.                                               EL604
01108                                                                      CL*11
01109      MOVE 'CF'                  TO CF-RECORD-ID                   EL604
01110      MOVE PI-COMPANY-ID         TO CF-COMPANY-ID                  EL604
01111      MOVE 'C'                   TO CF-RECORD-TYPE                 EL604
01112      MOVE REPORTI               TO CF-CUSTOM-REPORT-NO            EL604
01113      MOVE +0                    TO CF-SEQUENCE-NO.                EL604
01114                                                                   EL604
01115      MOVE SAVE-BIN-DATE         TO CF-LAST-MAINT-DT               EL604
01116      MOVE PI-PROCESSOR-ID       TO CF-LAST-MAINT-BY               EL604
01117      MOVE EIBTIME               TO CF-LAST-MAINT-HHMMSS.          EL604
01118                                                                      CL*11
01119      IF CF-CARRIER-OPT-SEQ NOT NUMERIC                               CL*11
01120         MOVE ZEROS              TO CF-CARRIER-OPT-SEQ.               CL*11
01121                                                                      CL*11
01122      IF CF-GROUP-OPT-SEQ        NOT NUMERIC                          CL*11
01123         MOVE ZEROS              TO CF-GROUP-OPT-SEQ.                 CL*11
01124                                                                      CL*11
01125      IF CF-STATE-OPT-SEQ        NOT NUMERIC                          CL*11
01126         MOVE ZEROS              TO CF-STATE-OPT-SEQ.                 CL*11
01127      IF CF-ACCOUNT-OPT-SEQ      NOT NUMERIC                          CL*11
01128         MOVE ZEROS              TO CF-ACCOUNT-OPT-SEQ.               CL*11
01129      IF CF-AGENT-OPT-SEQ        NOT NUMERIC                          CL*11
01130         MOVE ZEROS              TO CF-AGENT-OPT-SEQ.                 CL*11
01131      IF CF-REINS-OPT-SEQ        NOT NUMERIC                          CL*11
01132         MOVE ZEROS              TO CF-REINS-OPT-SEQ.                 CL*11
01133      IF CF-BUS-TYP-OPT-SEQ      NOT NUMERIC                          CL*11
01134         MOVE ZEROS              TO CF-BUS-TYP-OPT-SEQ.               CL*11
01135      IF CF-LF-TYP-OPT-SEQ       NOT NUMERIC                          CL*11
01136         MOVE ZEROS              TO CF-LF-TYP-OPT-SEQ.                CL*11
01137      IF CF-AH-TYP-OPT-SEQ       NOT NUMERIC                          CL*11
01138         MOVE ZEROS              TO CF-AH-TYP-OPT-SEQ.                CL*11
01139      IF CF-REPTCD1-OPT-SEQ      NOT NUMERIC                          CL*11
01140         MOVE ZEROS              TO CF-REPTCD1-OPT-SEQ.               CL*11
01141      IF CF-REPTCD2-OPT-SEQ      NOT NUMERIC                          CL*11
01142         MOVE ZEROS              TO CF-REPTCD2-OPT-SEQ.               CL*11
01143      IF CF-USER1-OPT-SEQ        NOT NUMERIC                          CL*11
01144         MOVE ZEROS              TO CF-USER1-OPT-SEQ.                 CL*11
01145      IF CF-USER2-OPT-SEQ        NOT NUMERIC                          CL*11
01146         MOVE ZEROS              TO CF-USER2-OPT-SEQ.                 CL*11
01147      IF CF-USER3-OPT-SEQ        NOT NUMERIC                          CL*11
01148         MOVE ZEROS              TO CF-USER3-OPT-SEQ.                 CL*11
01149      IF CF-USER4-OPT-SEQ        NOT NUMERIC                          CL*11
01150         MOVE ZEROS              TO CF-USER4-OPT-SEQ.                 CL*11
01151      IF CF-USER5-OPT-SEQ        NOT NUMERIC                          CL*11
01152         MOVE ZEROS              TO CF-USER5-OPT-SEQ.                 CL*11
01153      IF CF-SEL-LO-LOSS-RATIO    NOT NUMERIC                          CL*11
01154         MOVE +0                 TO CF-SEL-LO-LOSS-RATIO.             CL*11
01155      IF CF-SEL-HI-LOSS-RATIO    NOT NUMERIC                          CL*11
01156         MOVE +0                 TO CF-SEL-HI-LOSS-RATIO.             CL*11
01157                                                                   EL604
01158      IF STATUSL GREATER THAN +0                                   EL604
01159         MOVE STATUSI            TO CF-ACCOUNT-MASTER-STATUS.         CL**9
01160      IF CARSEQL GREATER THAN +0                                   EL604
01161         MOVE CARSEQI            TO CF-CARRIER-OPT-SEQ.            EL604
01162      IF CAR1L GREATER THAN +0                                     EL604
01163         MOVE CAR1I              TO CF-CARRIER-SELECT (1).         EL604
01164      IF CAR2L GREATER THAN +0                                     EL604
01165         MOVE CAR2I              TO CF-CARRIER-SELECT (2).         EL604
01166      IF CAR3L GREATER THAN +0                                     EL604
01167         MOVE CAR3I              TO CF-CARRIER-SELECT (3).         EL604
01168      IF GRPSEQL GREATER THAN +0                                   EL604
01169         MOVE GRPSEQI            TO CF-GROUP-OPT-SEQ.              EL604
01170      IF GRP1L GREATER THAN +0                                     EL604
01171         MOVE GRP1I              TO CF-GROUP-SELECT (1).           EL604
01172      IF GRP2L GREATER THAN +0                                     EL604
01173         MOVE GRP2I              TO CF-GROUP-SELECT (2).           EL604
01174      IF GRP3L GREATER THAN +0                                     EL604
01175         MOVE GRP3I              TO CF-GROUP-SELECT (3).           EL604
01176      IF STSEQL GREATER THAN +0                                    EL604
01177         MOVE STSEQI             TO CF-STATE-OPT-SEQ.              EL604
01178      IF ST1L GREATER THAN +0                                      EL604
01179         MOVE ST1I               TO CF-STATE-SELECT (1).           EL604
01180      IF ST2L GREATER THAN +0                                      EL604
01181         MOVE ST2I               TO CF-STATE-SELECT (2).           EL604
01182      IF ST3L GREATER THAN +0                                      EL604
01183         MOVE ST3I               TO CF-STATE-SELECT (3).           EL604
01184      IF ACCTSEQL GREATER THAN +0                                  EL604
01185         MOVE ACCTSEQI           TO CF-ACCOUNT-OPT-SEQ.            EL604
01186      IF ACCT1L GREATER THAN +0                                    EL604
01187         MOVE ACCT1I             TO CF-ACCOUNT-SELECT (1).         EL604
01188      IF ACCT2L GREATER THAN +0                                    EL604
01189         MOVE ACCT2I             TO CF-ACCOUNT-SELECT (2).         EL604
01190      IF ACCT3L GREATER THAN +0                                    EL604
01191         MOVE ACCT3I             TO CF-ACCOUNT-SELECT (3).         EL604
01192      IF AGNTSEQL GREATER THAN +0                                     CL*10
01193         MOVE AGNTSEQI           TO CF-AGENT-OPT-SEQ.                 CL*10
01194      IF AGNT1L GREATER THAN +0                                       CL*10
01195         MOVE AGNT1I             TO CF-AGENT-SELECT (1).              CL*10
01196      IF AGNT2L GREATER THAN +0                                       CL*10
01197         MOVE AGNT2I             TO CF-AGENT-SELECT (2).              CL*10
01198      IF AGNT3L GREATER THAN +0                                       CL*10
01199         MOVE AGNT3I             TO CF-AGENT-SELECT (3).              CL*10
01200      IF REINSEQL GREATER THAN +0                                     CL**7
01201         MOVE REINSEQI           TO CF-REINS-OPT-SEQ.                 CL**7
01202      IF REIN1L GREATER THAN +0                                       CL**7
01203         MOVE REIN1I             TO CF-REINS-SELECT (1).              CL**7
01204      IF REIN2L GREATER THAN +0                                       CL**7
01205         MOVE REIN2I             TO CF-REINS-SELECT (2).              CL**7
01206      IF REIN3L GREATER THAN +0                                       CL**7
01207         MOVE REIN3I             TO CF-REINS-SELECT (3).              CL**7
01208      IF BUSSEQL GREATER THAN +0                                   EL604
01209         MOVE BUSSEQI            TO CF-BUS-TYP-OPT-SEQ.            EL604
01210      IF BUS1L GREATER THAN +0                                     EL604
01211         MOVE BUS1I              TO CF-BUS-TYP-SELECT (1).         EL604
01212      IF BUS2L GREATER THAN +0                                     EL604
01213         MOVE BUS2I              TO CF-BUS-TYP-SELECT (2).         EL604
01214      IF BUS3L GREATER THAN +0                                     EL604
01215         MOVE BUS3I              TO CF-BUS-TYP-SELECT (3).         EL604
01216      IF LBENSEQL GREATER THAN +0                                  EL604
01217         MOVE LBENSEQI           TO CF-LF-TYP-OPT-SEQ.             EL604
01218      IF LBEN1L GREATER THAN +0                                    EL604
01219         MOVE LBEN1I             TO CF-BUS-LF-SELECT (1).          EL604
01220      IF LBEN2L GREATER THAN +0                                    EL604
01221         MOVE LBEN2I             TO CF-BUS-LF-SELECT (2).          EL604
01222      IF LBEN3L GREATER THAN +0                                    EL604
01223         MOVE LBEN3I             TO CF-BUS-LF-SELECT (3).          EL604
01224      IF ABENSEQL GREATER THAN +0                                  EL604
01225         MOVE ABENSEQI           TO CF-AH-TYP-OPT-SEQ.             EL604
01226      IF ABEN1L GREATER THAN +0                                    EL604
01227         MOVE ABEN1I             TO CF-BUS-AH-SELECT (1).          EL604
01228      IF ABEN2L GREATER THAN +0                                    EL604
01229         MOVE ABEN2I             TO CF-BUS-AH-SELECT (2).          EL604
01230      IF ABEN3L GREATER THAN +0                                    EL604
01231         MOVE ABEN3I             TO CF-BUS-AH-SELECT (3).          EL604
01232      IF RPT1SEQL GREATER THAN +0                                  EL604
01233         MOVE RPT1SEQI           TO CF-REPTCD1-OPT-SEQ.            EL604
01234      IF RPTCD11L GREATER THAN +0                                  EL604
01235         MOVE RPTCD11I           TO CF-REPTCD1-SELECT (1).         EL604
01236      IF RPTCD12L GREATER THAN +0                                  EL604
01237         MOVE RPTCD12I           TO CF-REPTCD1-SELECT (2).         EL604
01238      IF RPTCD13L GREATER THAN +0                                  EL604
01239         MOVE RPTCD13I           TO CF-REPTCD1-SELECT (3).         EL604
01240      IF RPT2SEQL GREATER THAN +0                                  EL604
01241         MOVE RPT2SEQI           TO CF-REPTCD2-OPT-SEQ.               CL**5
01242      IF RPTCD21L GREATER THAN +0                                  EL604
01243         MOVE RPTCD21I           TO CF-REPTCD2-SELECT (1).         EL604
01244      IF RPTCD22L GREATER THAN +0                                  EL604
01245         MOVE RPTCD22I           TO CF-REPTCD2-SELECT (2).         EL604
01246      IF RPTCD23L GREATER THAN +0                                  EL604
01247         MOVE RPTCD23I           TO CF-REPTCD2-SELECT (3).         EL604
01248      IF USR1SEQL GREATER THAN +0                                  EL604
01249         MOVE USR1SEQI           TO CF-USER1-OPT-SEQ.              EL604
01250      IF USR11L GREATER THAN +0                                    EL604
01251         MOVE USR11I             TO CF-USER1-SELECT (1).           EL604
01252      IF USR12L GREATER THAN +0                                    EL604
01253         MOVE USR12I             TO CF-USER1-SELECT (2).           EL604
01254      IF USR13L GREATER THAN +0                                    EL604
01255         MOVE USR13I             TO CF-USER1-SELECT (3).           EL604
01256      IF USR2SEQL GREATER THAN +0                                  EL604
01257         MOVE USR2SEQI           TO CF-USER2-OPT-SEQ.              EL604
01258      IF USR21L GREATER THAN +0                                    EL604
01259         MOVE USR21I             TO CF-USER2-SELECT (1).           EL604
01260      IF USR22L GREATER THAN +0                                    EL604
01261         MOVE USR22I             TO CF-USER2-SELECT (2).           EL604
01262      IF USR23L GREATER THAN +0                                    EL604
01263         MOVE USR23I             TO CF-USER2-SELECT (3).           EL604
01264      IF USR3SEQL GREATER THAN +0                                  EL604
01265         MOVE USR3SEQI           TO CF-USER3-OPT-SEQ.              EL604
01266      IF USR31L GREATER THAN +0                                    EL604
01267         MOVE USR31I             TO CF-USER3-SELECT (1).           EL604
01268      IF USR32L GREATER THAN +0                                    EL604
01269         MOVE USR32I             TO CF-USER3-SELECT (2).           EL604
01270      IF USR33L GREATER THAN +0                                    EL604
01271         MOVE USR33I             TO CF-USER3-SELECT (3).           EL604
01272      IF USR4SEQL GREATER THAN +0                                  EL604
01273         MOVE USR4SEQI           TO CF-USER4-OPT-SEQ.              EL604
01274      IF USR41L GREATER THAN +0                                    EL604
01275         MOVE USR41I             TO CF-USER4-SELECT (1).           EL604
01276      IF USR42L GREATER THAN +0                                    EL604
01277         MOVE USR42I             TO CF-USER4-SELECT (2).           EL604
01278      IF USR43L GREATER THAN +0                                    EL604
01279         MOVE USR43I             TO CF-USER4-SELECT (3).           EL604
01280      IF USR5SEQL GREATER THAN +0                                  EL604
01281         MOVE USR5SEQI           TO CF-USER5-OPT-SEQ.              EL604
01282      IF USR51L GREATER THAN +0                                    EL604
01283         MOVE USR51I             TO CF-USER5-SELECT (1).           EL604
01284      IF USR52L GREATER THAN +0                                    EL604
01285         MOVE USR52I             TO CF-USER5-SELECT (2).           EL604
01286      IF USR53L GREATER THAN +0                                    EL604
01287         MOVE USR53I             TO CF-USER5-SELECT (3).           EL604
01288      IF LOLOSSL GREATER THAN +0                                   EL604
01289         MOVE WS-LO-LOSS         TO CF-SEL-LO-LOSS-RATIO.          EL604
01290      IF HILOSSL GREATER THAN +0                                   EL604
01291         MOVE WS-HI-LOSS         TO CF-SEL-HI-LOSS-RATIO.          EL604
01292      IF LOENTDTL GREATER THAN +0                                  EL604
01293         MOVE WS-LO-ENT          TO CF-SEL-LO-ENTRY-DATE.          EL604
01294      IF HIENTDTL GREATER THAN +0                                  EL604
01295         MOVE WS-HI-ENT          TO CF-SEL-HI-ENTRY-DATE.          EL604
01296      IF LOEFFDTL GREATER THAN +0                                  EL604
01297         MOVE WS-LO-EFF          TO CF-SEL-LO-EFFECTIVE-DATE.      EL604
01298      IF HIEFFDTL GREATER THAN +0                                  EL604
01299         MOVE WS-HI-EFF          TO CF-SEL-HI-EFFECTIVE-DATE.      EL604
01300      IF EXPRPTL GREATER THAN +0                                      CL*14
01301         MOVE EXPRPTI            TO CF-EXCEPTION-LIST-IND.            CL*14
01302                                                                      CL*14
01303      IF CF-EXCEPTION-LIST-IND EQUAL 'Y'                              CL*14
01304         IF CF-ACCOUNT-OPT-USED                                       CL*14
01305             NEXT SENTENCE                                            CL*14
01306         ELSE                                                         CL*14
01307            MOVE ER-7746             TO EMI-ERROR                     CL*14
01308            MOVE -1                  TO ACCTSEQL                      CL*14
01309            MOVE AL-UNBON            TO ACCTSEQA                      CL*14
01310            PERFORM 9900-ERROR-FORMAT                                 CL*14
01311            GO TO 8200-SEND-DATAONLY.                                 CL*14
01312                                                                   EL604
01313  1600-WRITE-REPORT-RECORD.                                        EL604
01314                                                                      CL*11
01315      EXEC CICS WRITE                                              EL604
01316           DATASET(ELCNTL-FILE-ID)                                 EL604
01317           FROM   (CONTROL-FILE)                                   EL604
01318           RIDFLD (ELCNTL-KEY)                                     EL604
01319      END-EXEC.                                                    EL604
01320                                                                   EL604
01321      MOVE ER-0000      TO EMI-ERROR.                              EL604
01322      PERFORM 9900-ERROR-FORMAT.                                   EL604
01323      GO TO 4000-SHOW-REPORT.                                         CL*14
01324                                                                   EL604
01325  2000-CHANGE-REPORT.                                              EL604
01326      IF REPORTI NOT NUMERIC                                       EL604
01327         MOVE ER-7680            TO EMI-ERROR                      EL604
01328         MOVE -1                 TO REPORTL                        EL604
01329         MOVE AL-UNBON           TO REPORTA                        EL604
01330         PERFORM 9900-ERROR-FORMAT                                 EL604
01331         GO TO 8200-SEND-DATAONLY.                                 EL604
01332                                                                   EL604
01333      IF REPORTI NOT = PI-LAST-REPORT                              EL604
01334         MOVE ER-0138            TO EMI-ERROR                      EL604
01335         MOVE -1                 TO REPORTL                        EL604
01336         MOVE AL-UNBON           TO REPORTA                        EL604
01337         PERFORM 9900-ERROR-FORMAT                                 EL604
01338         GO TO 8200-SEND-DATAONLY.                                 EL604
01339                                                                   EL604
01340      MOVE SPACES                 TO ELCNTL-KEY                    EL604
01341      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            EL604
01342      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.           EL604
01343      MOVE REPORTI                TO ELCNTL-REPORT.                EL604
01344      MOVE +0                     TO ELCNTL-SEQ-NO.                EL604
01345                                                                   EL604
01346      EXEC CICS HANDLE CONDITION                                   EL604
01347           NOTFND     (2100-NOT-FOUND)                             EL604
01348      END-EXEC.                                                    EL604
01349                                                                   EL604
01350      EXEC CICS READ                                               EL604
01351           DATASET    (ELCNTL-FILE-ID)                             EL604
01352           SET        (ADDRESS OF CONTROL-FILE)                       CL*15
01353           RIDFLD     (ELCNTL-KEY)                                 EL604
01354      END-EXEC.                                                    EL604
01355                                                                   EL604
01356      IF (CF-LAST-MAINT-BY NOT = PI-UPDATE-BY) OR                  EL604
01357         (CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS)             EL604
01358         MOVE ER-0068          TO EMI-ERROR                        EL604
01359         MOVE -1               TO MAINTL                           EL604
01360         PERFORM 9900-ERROR-FORMAT                                 EL604
01361         GO TO 8200-SEND-DATAONLY.                                 EL604
01362                                                                   EL604
01363      PERFORM 1100-EDIT-REPORT-SCREEN.                             EL604
01364                                                                   EL604
01365      IF NOT EMI-NO-ERRORS                                         EL604
01366         GO TO 8200-SEND-DATAONLY.                                 EL604
01367                                                                   EL604
01368      EXEC CICS READ                                               EL604
01369           DATASET    (ELCNTL-FILE-ID)                             EL604
01370           SET        (ADDRESS OF CONTROL-FILE)                       CL*15
01371           RIDFLD     (ELCNTL-KEY)                                 EL604
01372           UPDATE                                                  EL604
01373      END-EXEC.                                                    EL604
01374                                                                   EL604
01375      PERFORM 1500-BUILD-RECORD.                                   EL604
01376                                                                   EL604
01377      MOVE SPACES TO WS-SEQ-AREA.                                  EL604
01378                                                                   EL604
01379      IF CF-CARRIER-OPT-SEQ NOT = 0                                EL604
01380         IF WS-SEQ-NO (CF-CARRIER-OPT-SEQ) NOT = 'X'               EL604
01381            MOVE 'X'     TO WS-SEQ-NO (CF-CARRIER-OPT-SEQ)         EL604
01382         ELSE                                                      EL604
01383            MOVE ER-7682          TO EMI-ERROR                     EL604
01384            MOVE -1               TO CARSEQL                       EL604
01385            MOVE AL-UNBON         TO CARSEQA                       EL604
01386            PERFORM 9900-ERROR-FORMAT.                             EL604
01387                                                                   EL604
01388      IF CF-GROUP-OPT-SEQ NOT = 0                                  EL604
01389         IF WS-SEQ-NO (CF-GROUP-OPT-SEQ) NOT = 'X'                 EL604
01390            MOVE 'X'     TO WS-SEQ-NO (CF-GROUP-OPT-SEQ)           EL604
01391         ELSE                                                      EL604
01392            MOVE ER-7682          TO EMI-ERROR                     EL604
01393            MOVE -1               TO GRPSEQL                       EL604
01394            MOVE AL-UNBON         TO GRPSEQA                       EL604
01395            PERFORM 9900-ERROR-FORMAT.                             EL604
01396                                                                   EL604
01397      IF CF-STATE-OPT-SEQ NOT = 0                                  EL604
01398         IF WS-SEQ-NO (CF-STATE-OPT-SEQ) NOT = 'X'                 EL604
01399            MOVE 'X'     TO WS-SEQ-NO (CF-STATE-OPT-SEQ)           EL604
01400         ELSE                                                      EL604
01401            MOVE ER-7682          TO EMI-ERROR                     EL604
01402            MOVE -1               TO STSEQL                        EL604
01403            MOVE AL-UNBON         TO STSEQA                        EL604
01404            PERFORM 9900-ERROR-FORMAT.                             EL604
01405                                                                   EL604
01406      IF CF-BUS-TYP-OPT-SEQ NOT = 0                                EL604
01407         IF WS-SEQ-NO (CF-BUS-TYP-OPT-SEQ) NOT = 'X'               EL604
01408            MOVE 'X'     TO WS-SEQ-NO (CF-BUS-TYP-OPT-SEQ)         EL604
01409         ELSE                                                      EL604
01410            MOVE ER-7682          TO EMI-ERROR                     EL604
01411            MOVE -1               TO BUSSEQL                       EL604
01412            MOVE AL-UNBON         TO BUSSEQA                       EL604
01413            PERFORM 9900-ERROR-FORMAT.                             EL604
01414                                                                   EL604
01415      IF CF-LF-TYP-OPT-SEQ NOT = 0                                 EL604
01416         IF WS-SEQ-NO (CF-LF-TYP-OPT-SEQ) NOT = 'X'                EL604
01417            MOVE 'X'     TO WS-SEQ-NO (CF-LF-TYP-OPT-SEQ)          EL604
01418         ELSE                                                      EL604
01419            MOVE ER-7682          TO EMI-ERROR                     EL604
01420            MOVE -1               TO LBENSEQL                      EL604
01421            MOVE AL-UNBON         TO LBENSEQA                      EL604
01422            PERFORM 9900-ERROR-FORMAT.                             EL604
01423                                                                   EL604
01424      IF CF-AH-TYP-OPT-SEQ NOT = 0                                 EL604
01425         IF WS-SEQ-NO (CF-AH-TYP-OPT-SEQ) NOT = 'X'                EL604
01426            MOVE 'X'     TO WS-SEQ-NO (CF-AH-TYP-OPT-SEQ)          EL604
01427         ELSE                                                      EL604
01428            MOVE ER-7682          TO EMI-ERROR                     EL604
01429            MOVE -1               TO ABENSEQL                      EL604
01430            MOVE AL-UNBON         TO ABENSEQA                      EL604
01431            PERFORM 9900-ERROR-FORMAT.                             EL604
01432                                                                   EL604
01433      IF CF-ACCOUNT-OPT-SEQ NOT = 0                                EL604
01434         IF WS-SEQ-NO (CF-ACCOUNT-OPT-SEQ) NOT = 'X'               EL604
01435            MOVE 'X'     TO WS-SEQ-NO (CF-ACCOUNT-OPT-SEQ)         EL604
01436         ELSE                                                      EL604
01437            MOVE ER-7682          TO EMI-ERROR                     EL604
01438            MOVE -1               TO ACCTSEQL                      EL604
01439            MOVE AL-UNBON         TO ACCTSEQA                      EL604
01440            PERFORM 9900-ERROR-FORMAT.                             EL604
01441                                                                   EL604
01442      IF CF-AGENT-OPT-SEQ NOT NUMERIC                                 CL*10
01443          MOVE ZEROS              TO CF-AGENT-OPT-SEQ.                CL*10
01444                                                                      CL*10
01445      IF CF-AGENT-OPT-SEQ NOT = 0                                     CL*10
01446         IF WS-SEQ-NO (CF-AGENT-OPT-SEQ) NOT = 'X'                    CL*10
01447            MOVE 'X'     TO WS-SEQ-NO (CF-AGENT-OPT-SEQ)              CL*10
01448         ELSE                                                         CL*10
01449            MOVE ER-7682          TO EMI-ERROR                        CL*10
01450            MOVE -1               TO AGNTSEQL                         CL*10
01451            MOVE AL-UNBON         TO AGNTSEQA                         CL*10
01452            PERFORM 9900-ERROR-FORMAT.                                CL*10
01453                                                                      CL*10
01454      IF CF-REINS-OPT-SEQ NOT NUMERIC                                 CL**8
01455          MOVE ZEROS              TO CF-REINS-OPT-SEQ.                CL**8
01456                                                                      CL**8
01457      IF CF-REINS-OPT-SEQ NOT = 0                                     CL**7
01458         IF WS-SEQ-NO (CF-REINS-OPT-SEQ) NOT = 'X'                    CL**7
01459            MOVE 'X'     TO WS-SEQ-NO (CF-REINS-OPT-SEQ)              CL**7
01460         ELSE                                                         CL**7
01461            MOVE ER-7682          TO EMI-ERROR                        CL**7
01462            MOVE -1               TO REINSEQL                         CL**7
01463            MOVE AL-UNBON         TO REINSEQA                         CL**7
01464            PERFORM 9900-ERROR-FORMAT.                                CL**7
01465                                                                      CL**7
01466      IF CF-REPTCD1-OPT-SEQ NOT = 0                                EL604
01467         IF WS-SEQ-NO (CF-REPTCD1-OPT-SEQ) NOT = 'X'               EL604
01468            MOVE 'X'     TO WS-SEQ-NO (CF-REPTCD1-OPT-SEQ)         EL604
01469         ELSE                                                      EL604
01470            MOVE ER-7682          TO EMI-ERROR                     EL604
01471            MOVE -1               TO RPT1SEQL                      EL604
01472            MOVE AL-UNBON         TO RPT1SEQA                      EL604
01473            PERFORM 9900-ERROR-FORMAT.                             EL604
01474                                                                   EL604
01475      IF CF-REPTCD2-OPT-SEQ NOT = 0                                EL604
01476         IF WS-SEQ-NO (CF-REPTCD2-OPT-SEQ) NOT = 'X'               EL604
01477            MOVE 'X'     TO WS-SEQ-NO (CF-REPTCD2-OPT-SEQ)         EL604
01478         ELSE                                                      EL604
01479            MOVE ER-7682          TO EMI-ERROR                     EL604
01480            MOVE -1               TO RPT2SEQL                      EL604
01481            MOVE AL-UNBON         TO RPT2SEQA                      EL604
01482            PERFORM 9900-ERROR-FORMAT.                             EL604
01483                                                                   EL604
01484      IF CF-USER1-OPT-SEQ NOT = 0                                  EL604
01485         IF WS-SEQ-NO (CF-USER1-OPT-SEQ) NOT = 'X'                 EL604
01486            MOVE 'X'     TO WS-SEQ-NO (CF-USER1-OPT-SEQ)           EL604
01487         ELSE                                                      EL604
01488            MOVE ER-7682          TO EMI-ERROR                     EL604
01489            MOVE -1               TO USR1SEQL                      EL604
01490            MOVE AL-UNBON         TO USR1SEQA                      EL604
01491            PERFORM 9900-ERROR-FORMAT.                             EL604
01492                                                                   EL604
01493      IF CF-USER2-OPT-SEQ NOT = 0                                  EL604
01494         IF WS-SEQ-NO (CF-USER2-OPT-SEQ) NOT = 'X'                 EL604
01495            MOVE 'X'     TO WS-SEQ-NO (CF-USER2-OPT-SEQ)           EL604
01496         ELSE                                                      EL604
01497            MOVE ER-7682          TO EMI-ERROR                     EL604
01498            MOVE -1               TO USR2SEQL                      EL604
01499            MOVE AL-UNBON         TO USR2SEQA                      EL604
01500            PERFORM 9900-ERROR-FORMAT.                             EL604
01501                                                                   EL604
01502      IF CF-USER3-OPT-SEQ NOT = 0                                  EL604
01503         IF WS-SEQ-NO (CF-USER3-OPT-SEQ) NOT = 'X'                 EL604
01504            MOVE 'X'     TO WS-SEQ-NO (CF-USER3-OPT-SEQ)           EL604
01505         ELSE                                                      EL604
01506            MOVE ER-7682          TO EMI-ERROR                     EL604
01507            MOVE -1               TO USR3SEQL                      EL604
01508            MOVE AL-UNBON         TO USR3SEQA                      EL604
01509            PERFORM 9900-ERROR-FORMAT.                             EL604
01510                                                                   EL604
01511      IF CF-USER4-OPT-SEQ NOT = 0                                  EL604
01512         IF WS-SEQ-NO (CF-USER4-OPT-SEQ) NOT = 'X'                 EL604
01513            MOVE 'X'     TO WS-SEQ-NO (CF-USER4-OPT-SEQ)           EL604
01514         ELSE                                                      EL604
01515            MOVE ER-7682          TO EMI-ERROR                     EL604
01516            MOVE -1               TO USR4SEQL                      EL604
01517            MOVE AL-UNBON         TO USR4SEQA                      EL604
01518            PERFORM 9900-ERROR-FORMAT.                             EL604
01519                                                                   EL604
01520      IF CF-USER5-OPT-SEQ NOT = 0                                  EL604
01521         IF WS-SEQ-NO (CF-USER5-OPT-SEQ) NOT = 'X'                 EL604
01522            MOVE 'X'     TO WS-SEQ-NO (CF-USER5-OPT-SEQ)           EL604
01523         ELSE                                                      EL604
01524            MOVE ER-7682          TO EMI-ERROR                     EL604
01525            MOVE -1               TO USR5SEQL                      EL604
01526            MOVE AL-UNBON         TO USR5SEQA                      EL604
01527            PERFORM 9900-ERROR-FORMAT.                             EL604
01528                                                                   EL604
01529      IF CF-SEL-LO-LOSS-RATIO GREATER THAN CF-SEL-HI-LOSS-RATIO    EL604
01530         MOVE ER-7683             TO EMI-ERROR                     EL604
01531         MOVE -1                  TO LOLOSSL                       EL604
01532         MOVE AL-UNBON            TO LOLOSSA                       EL604
01533         PERFORM 9900-ERROR-FORMAT.                                EL604
01534                                                                   EL604
01535      IF CF-SEL-LO-ENTRY-DATE GREATER THAN CF-SEL-HI-ENTRY-DATE    EL604
01536         MOVE ER-7684             TO EMI-ERROR                     EL604
01537         MOVE -1                  TO LOENTDTL                      EL604
01538         MOVE AL-UNBON            TO LOENTDTA                      EL604
01539         PERFORM 9900-ERROR-FORMAT.                                EL604
01540                                                                   EL604
01541      IF CF-SEL-LO-EFFECTIVE-DATE GREATER THAN                     EL604
01542                           CF-SEL-HI-EFFECTIVE-DATE                EL604
01543         MOVE ER-7684             TO EMI-ERROR                     EL604
01544         MOVE -1                  TO LOEFFDTL                      EL604
01545         MOVE AL-UNBON            TO LOEFFDTA                      EL604
01546         PERFORM 9900-ERROR-FORMAT.                                EL604
01547                                                                   EL604
01548      IF CF-EXCEPTION-LIST-IND EQUAL 'Y'                              CL*14
01549         IF CF-ACCOUNT-OPT-USED                                       CL*14
01550             NEXT SENTENCE                                            CL*14
01551         ELSE                                                         CL*14
01552            MOVE ER-7746             TO EMI-ERROR                     CL*14
01553            MOVE -1                  TO ACCTSEQL                      CL*14
01554            MOVE AL-UNBON            TO ACCTSEQA                      CL*14
01555            PERFORM 9900-ERROR-FORMAT.                                CL*14
01556                                                                      CL*14
01557      IF NOT EMI-NO-ERRORS                                         EL604
01558         EXEC CICS UNLOCK                                          EL604
01559              DATASET   (ELCNTL-FILE-ID)                           EL604
01560         END-EXEC                                                  EL604
01561         GO TO 8200-SEND-DATAONLY.                                 EL604
01562                                                                   EL604
01563      EXEC CICS REWRITE                                            EL604
01564           DATASET   (ELCNTL-FILE-ID)                              EL604
01565           FROM      (CONTROL-FILE)                                EL604
01566      END-EXEC.                                                    EL604
01567                                                                   EL604
01568      MOVE ER-0000      TO EMI-ERROR.                              EL604
01569      PERFORM 9900-ERROR-FORMAT.                                   EL604
01570      GO TO 4000-SHOW-REPORT.                                         CL*14
01571                                                                   EL604
01572  2100-NOT-FOUND.                                                  EL604
01573      MOVE ER-0139              TO EMI-ERROR.                      EL604
01574      MOVE -1                   TO REPORTL.                        EL604
01575      MOVE AL-UNBON             TO REPORTA.                        EL604
01576      PERFORM 9900-ERROR-FORMAT.                                   EL604
01577      GO TO 8200-SEND-DATAONLY.                                    EL604
01578                                                                   EL604
01579      EJECT                                                        EL604
01580                                                                   EL604
01581  2500-DELETE-REPORT.                                              EL604
01582      IF REPORTI NOT NUMERIC                                       EL604
01583         MOVE ER-7680            TO EMI-ERROR                      EL604
01584         MOVE -1                 TO REPORTL                        EL604
01585         MOVE AL-UNBON           TO REPORTA                        EL604
01586         PERFORM 9900-ERROR-FORMAT                                 EL604
01587         GO TO 8200-SEND-DATAONLY.                                 EL604
01588                                                                   EL604
01589      IF REPORTI NOT = PI-LAST-REPORT                              EL604
01590         MOVE ER-0138            TO EMI-ERROR                      EL604
01591         MOVE -1                 TO REPORTL                        EL604
01592         MOVE AL-UNBON           TO REPORTA                        EL604
01593         PERFORM 9900-ERROR-FORMAT                                 EL604
01594         GO TO 8200-SEND-DATAONLY.                                 EL604
01595                                                                   EL604
01596      EXEC CICS HANDLE CONDITION                                   EL604
01597          NOTFND   (2890-NOT-FOUND)                                EL604
01598      END-EXEC.                                                    EL604
01599                                                                   EL604
01600      MOVE SPACES                 TO ELCNTL-KEY.                   EL604
01601                                                                   EL604
01602      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            EL604
01603      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.           EL604
01604      MOVE REPORTI                TO ELCNTL-REPORT.                EL604
01605      MOVE +0                     TO ELCNTL-SEQ-NO.                EL604
01606                                                                   EL604
01607      EXEC CICS DELETE                                             EL604
01608           DATASET   (ELCNTL-FILE-ID)                              EL604
01609           RIDFLD    (ELCNTL-KEY)                                  EL604
01610      END-EXEC.                                                    EL604
01611                                                                   EL604
01612      MOVE LOW-VALUES TO EL604AI.                                  EL604
01613      MOVE ER-0000      TO EMI-ERROR.                              EL604
01614      PERFORM 9900-ERROR-FORMAT.                                   EL604
01615      GO TO 8100-SEND-INITIAL-MAP.                                 EL604
01616                                                                   EL604
01617  2890-NOT-FOUND.                                                  EL604
01618      MOVE  ER-0142               TO EMI-ERROR.                    EL604
01619      MOVE -1                     TO REPORTL.                      EL604
01620      MOVE AL-UNBON               TO REPORTA.                      EL604
01621      PERFORM 9900-ERROR-FORMAT.                                   EL604
01622      GO TO 8200-SEND-DATAONLY.                                    EL604
01623                                                                   EL604
01624      EJECT                                                           CL*14
01625  3000-COPY-REPORT.                                                   CL*14
01626      IF REPORTL GREATER +0 AND                                       CL*14
01627         REPORTI NUMERIC                                              CL*14
01628         NEXT SENTENCE                                                CL*14
01629      ELSE                                                            CL*14
01630         MOVE ER-7680     TO EMI-ERROR                                CL*14
01631         MOVE -1          TO REPORTL                                  CL*14
01632         MOVE AL-UNBON    TO REPORTA                                  CL*14
01633         PERFORM 9900-ERROR-FORMAT                                    CL*14
01634         GO TO 8200-SEND-DATAONLY.                                    CL*14
01635                                                                      CL*14
01636      IF CPYRPTL GREATER +0 AND                                       CL*14
01637         CPYRPTI NUMERIC                                              CL*14
01638         NEXT SENTENCE                                                CL*14
01639      ELSE                                                            CL*14
01640         MOVE ER-7680     TO EMI-ERROR                                CL*14
01641         MOVE -1          TO CPYRPTL                                  CL*14
01642         MOVE AL-UNBON    TO CPYRPTA                                  CL*14
01643         PERFORM 9900-ERROR-FORMAT                                    CL*14
01644         GO TO 8200-SEND-DATAONLY.                                    CL*14
01645                                                                      CL*14
01646      MOVE SPACES            TO ELCNTL-KEY.                           CL*14
01647      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.                    CL*14
01648      MOVE REPORTI           TO ELCNTL-REPORT.                        CL*14
01649      MOVE 'C'               TO ELCNTL-RECORD-TYPE.                   CL*14
01650      MOVE +0                TO ELCNTL-SEQ-NO.                        CL*14
01651                                                                      CL*14
01652      EXEC CICS HANDLE CONDITION                                      CL*14
01653           NOTFND     (3100-CONTINUE)                                 CL*14
01654      END-EXEC.                                                       CL*14
01655                                                                      CL*14
01656      EXEC CICS READ                                                  CL*14
01657           DATASET    (ELCNTL-FILE-ID)                                CL*14
01658           RIDFLD     (ELCNTL-KEY)                                    CL*14
01659           SET        (ADDRESS OF CONTROL-FILE)                       CL*15
01660      END-EXEC.                                                       CL*14
01661                                                                      CL*14
01662      MOVE -1                     TO REPORTL.                         CL*14
01663      MOVE  ER-0132               TO  EMI-ERROR.                      CL*14
01664      PERFORM 9900-ERROR-FORMAT.                                      CL*14
01665      GO TO 8200-SEND-DATAONLY.                                       CL*14
01666                                                                      CL*14
01667  3100-CONTINUE.                                                      CL*14
01668      MOVE SPACES            TO ELCNTL-KEY.                           CL*14
01669      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.                    CL*14
01670      MOVE CPYRPTI           TO ELCNTL-REPORT.                        CL*14
01671      MOVE 'C'               TO ELCNTL-RECORD-TYPE.                   CL*14
01672      MOVE +0                TO ELCNTL-SEQ-NO.                        CL*14
01673                                                                      CL*14
01674      EXEC CICS HANDLE CONDITION                                      CL*14
01675           NOTFND     (3900-NOT-FOUND)                                CL*14
01676      END-EXEC.                                                       CL*14
01677                                                                      CL*14
01678      EXEC CICS READ                                                  CL*14
01679           DATASET    (ELCNTL-FILE-ID)                                CL*14
01680           RIDFLD     (ELCNTL-KEY)                                    CL*14
01681           SET        (ADDRESS OF CONTROL-FILE)                       CL*15
01682      END-EXEC.                                                       CL*14
01683                                                                      CL*14
01684      MOVE SPACES            TO ELCNTL-KEY.                           CL*14
01685      MOVE 'CF'                  TO CF-RECORD-ID                      CL*14
01686      MOVE PI-COMPANY-ID         TO CF-COMPANY-ID                     CL*14
01687                                    ELCNTL-COMPANY-ID.                CL*14
01688      MOVE 'C'                   TO CF-RECORD-TYPE                    CL*14
01689                                    ELCNTL-RECORD-TYPE.               CL*14
01690      MOVE REPORTI               TO CF-CUSTOM-REPORT-NO               CL*14
01691                                    ELCNTL-REPORT.                    CL*14
01692      MOVE +0                    TO CF-SEQUENCE-NO                    CL*14
01693                                    ELCNTL-SEQ-NO.                    CL*14
01694                                                                      CL*14
01695      MOVE SAVE-BIN-DATE         TO CF-LAST-MAINT-DT                  CL*14
01696      MOVE PI-PROCESSOR-ID       TO CF-LAST-MAINT-BY                  CL*14
01697      MOVE EIBTIME               TO CF-LAST-MAINT-HHMMSS.             CL*14
01698                                                                      CL*14
01699      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT             CL*14
01700                                           REPORTO.                   CL*14
01701                                                                      CL*14
01702      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.               CL*14
01703      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.           CL*14
01704                                                                      CL*14
01705      EXEC CICS WRITE                                                 CL*14
01706           DATASET(ELCNTL-FILE-ID)                                    CL*14
01707           FROM   (CONTROL-FILE)                                      CL*14
01708           RIDFLD (ELCNTL-KEY)                                        CL*14
01709      END-EXEC.                                                       CL*14
01710                                                                      CL*14
01711      MOVE ER-0000      TO EMI-ERROR.                                 CL*14
01712      PERFORM 9900-ERROR-FORMAT.                                      CL*14
01713      GO TO 4000-SHOW-REPORT.                                         CL*14
01714                                                                      CL*14
01715  3900-NOT-FOUND.                                                     CL*14
01716      MOVE  ER-0142               TO EMI-ERROR.                       CL*14
01717      MOVE -1                     TO CPYRPTL.                         CL*14
01718      MOVE AL-UNBON               TO CPYRPTA.                         CL*14
01719      PERFORM 9900-ERROR-FORMAT.                                      CL*14
01720      GO TO 8200-SEND-DATAONLY.                                       CL*14
01721                                                                      CL*14
01722  4000-SHOW-REPORT.                                                   CL*14
01723      IF MAINTI = 'S'                                              EL604
01724         IF REPORTL GREATER THAN +0 AND                            EL604
01725            REPORTI NUMERIC                                        EL604
01726            NEXT SENTENCE                                          EL604
01727         ELSE                                                      EL604
01728            MOVE ER-7680     TO EMI-ERROR                          EL604
01729            MOVE -1          TO REPORTL                            EL604
01730            MOVE AL-UNBON    TO REPORTA                            EL604
01731            PERFORM 9900-ERROR-FORMAT                              EL604
01732            GO TO 8200-SEND-DATAONLY.                              EL604
01733                                                                   EL604
01734      IF MAINTI = 'S'                                              EL604
01735         MOVE SPACES              TO ELCNTL-KEY                    EL604
01736         MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID             EL604
01737         MOVE 'C'                 TO ELCNTL-RECORD-TYPE            EL604
01738         MOVE REPORTI             TO ELCNTL-REPORT                 EL604
01739         MOVE +0                  TO ELCNTL-SEQ-NO.                EL604
01740                                                                   EL604
01741      EXEC CICS HANDLE CONDITION                                   EL604
01742           NOTFND     (4100-NOT-FOUND)                                CL*14
01743      END-EXEC.                                                    EL604
01744                                                                   EL604
01745      EXEC CICS READ                                               EL604
01746           DATASET     (ELCNTL-FILE-ID)                            EL604
01747           RIDFLD      (ELCNTL-KEY)                                EL604
01748           SET         (ADDRESS OF CONTROL-FILE)                      CL*15
01749      END-EXEC.                                                    EL604
01750                                                                   EL604
01751      MOVE LOW-VALUES                   TO EL604AI.                EL604
01752                                                                   EL604
01753      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT          EL604
01754                                           REPORTO.                EL604
01755      MOVE CF-ACCOUNT-MASTER-STATUS     TO STATUSO.                   CL*10
01756      IF CF-CARRIER-OPT-SEQ NOT = 0                                EL604
01757         MOVE CF-CARRIER-OPT-SEQ        TO CARSEQO.                EL604
01758      MOVE CF-CARRIER-SELECT (1)        TO CAR1O.                  EL604
01759      MOVE CF-CARRIER-SELECT (2)        TO CAR2O.                  EL604
01760      MOVE CF-CARRIER-SELECT (3)        TO CAR3O.                  EL604
01761      IF CF-GROUP-OPT-SEQ NOT = 0                                  EL604
01762         MOVE CF-GROUP-OPT-SEQ          TO GRPSEQO.                EL604
01763      MOVE CF-GROUP-SELECT (1)          TO GRP1O.                  EL604
01764      MOVE CF-GROUP-SELECT (2)          TO GRP2O.                  EL604
01765      MOVE CF-GROUP-SELECT (3)          TO GRP3O.                  EL604
01766      IF CF-STATE-OPT-SEQ NOT = 0                                  EL604
01767         MOVE CF-STATE-OPT-SEQ          TO STSEQO.                 EL604
01768      MOVE CF-STATE-SELECT (1)          TO ST1O.                   EL604
01769      MOVE CF-STATE-SELECT (2)          TO ST2O.                   EL604
01770      MOVE CF-STATE-SELECT (3)          TO ST3O.                   EL604
01771      IF CF-ACCOUNT-OPT-SEQ NOT = 0                                EL604
01772         MOVE CF-ACCOUNT-OPT-SEQ        TO ACCTSEQO.               EL604
01773      MOVE CF-ACCOUNT-SELECT (1)        TO ACCT1O.                 EL604
01774      MOVE CF-ACCOUNT-SELECT (2)        TO ACCT2O.                 EL604
01775      MOVE CF-ACCOUNT-SELECT (3)        TO ACCT3O.                 EL604
01776      IF CF-AGENT-OPT-SEQ NOT NUMERIC                                 CL*10
01777         MOVE ZEROS                     TO CF-AGENT-OPT-SEQ.          CL*10
01778      IF CF-AGENT-OPT-SEQ NOT = 0                                     CL*10
01779         MOVE CF-AGENT-OPT-SEQ          TO AGNTSEQO.                  CL*10
01780      MOVE CF-AGENT-SELECT (1)          TO AGNT1O.                    CL*10
01781      MOVE CF-AGENT-SELECT (2)          TO AGNT2O.                    CL*10
01782      MOVE CF-AGENT-SELECT (3)          TO AGNT3O.                    CL*10
01783                                                                      CL**8
01784      IF CF-REINS-OPT-SEQ NOT NUMERIC                                 CL**8
01785          MOVE ZEROS              TO CF-REINS-OPT-SEQ.                CL**8
01786                                                                      CL**8
01787      IF CF-REINS-OPT-SEQ NOT = 0                                     CL**7
01788         MOVE CF-REINS-OPT-SEQ          TO REINSEQO.                  CL**7
01789                                                                      CL**8
01790      MOVE CF-REINS-SELECT (1)          TO REIN1O.                    CL**7
01791      MOVE CF-REINS-SELECT (2)          TO REIN2O.                    CL**7
01792      MOVE CF-REINS-SELECT (3)          TO REIN3O.                    CL**7
01793                                                                      CL**8
01794      IF CF-BUS-TYP-OPT-SEQ NOT = 0                                EL604
01795         MOVE CF-BUS-TYP-OPT-SEQ        TO BUSSEQO.                EL604
01796      IF CF-BUS-TYP-SELECT (1) NOT = ZEROS                         EL604
01797         MOVE CF-BUS-TYP-SELECT (1)     TO BUS1O.                  EL604
01798      IF CF-BUS-TYP-SELECT (2) NOT = ZEROS                         EL604
01799         MOVE CF-BUS-TYP-SELECT (2)     TO BUS2O.                  EL604
01800      IF CF-BUS-TYP-SELECT (3) NOT = ZEROS                         EL604
01801         MOVE CF-BUS-TYP-SELECT (3)     TO BUS3O.                  EL604
01802      IF CF-LF-TYP-OPT-SEQ NOT = 0                                 EL604
01803         MOVE CF-LF-TYP-OPT-SEQ         TO LBENSEQO.               EL604
01804      IF CF-BUS-LF-SELECT (1) NOT = ZEROS                          EL604
01805         MOVE CF-BUS-LF-SELECT (1)      TO LBEN1O.                 EL604
01806      IF CF-BUS-LF-SELECT (2) NOT = ZEROS                          EL604
01807         MOVE CF-BUS-LF-SELECT (2)      TO LBEN2O.                 EL604
01808      IF CF-BUS-LF-SELECT (3) NOT = ZEROS                          EL604
01809         MOVE CF-BUS-LF-SELECT (3)      TO LBEN3O.                 EL604
01810      IF CF-AH-TYP-OPT-SEQ NOT = 0                                 EL604
01811         MOVE CF-AH-TYP-OPT-SEQ         TO ABENSEQO.               EL604
01812      IF CF-BUS-AH-SELECT (1) NOT = ZEROS                          EL604
01813         MOVE CF-BUS-AH-SELECT (1)      TO ABEN1O.                 EL604
01814      IF CF-BUS-AH-SELECT (2) NOT = ZEROS                          EL604
01815         MOVE CF-BUS-AH-SELECT (2)      TO ABEN2O.                 EL604
01816      IF CF-BUS-AH-SELECT (3) NOT = ZEROS                          EL604
01817         MOVE CF-BUS-AH-SELECT (3)      TO ABEN3O.                 EL604
01818      IF CF-REPTCD1-OPT-SEQ NOT = 0                                EL604
01819         MOVE CF-REPTCD1-OPT-SEQ        TO RPT1SEQO.               EL604
01820      MOVE CF-REPTCD1-SELECT (1)        TO RPTCD11O.               EL604
01821      MOVE CF-REPTCD1-SELECT (2)        TO RPTCD12O.               EL604
01822      MOVE CF-REPTCD1-SELECT (3)        TO RPTCD13O.               EL604
01823      IF CF-REPTCD2-OPT-SEQ NOT = 0                                EL604
01824         MOVE CF-REPTCD2-OPT-SEQ        TO RPT2SEQO.               EL604
01825      MOVE CF-REPTCD2-SELECT (1)        TO RPTCD21O.               EL604
01826      MOVE CF-REPTCD2-SELECT (2)        TO RPTCD22O.               EL604
01827      MOVE CF-REPTCD2-SELECT (3)        TO RPTCD23O.               EL604
01828      IF CF-USER1-OPT-SEQ NOT = 0                                  EL604
01829         MOVE CF-USER1-OPT-SEQ          TO USR1SEQO.               EL604
01830      MOVE CF-USER1-SELECT (1)          TO USR11O.                 EL604
01831      MOVE CF-USER1-SELECT (2)          TO USR12O.                 EL604
01832      MOVE CF-USER1-SELECT (3)          TO USR13O.                 EL604
01833      IF CF-USER2-OPT-SEQ NOT = 0                                  EL604
01834         MOVE CF-USER2-OPT-SEQ          TO USR2SEQO.               EL604
01835      MOVE CF-USER2-SELECT (1)          TO USR21O.                 EL604
01836      MOVE CF-USER2-SELECT (2)          TO USR22O.                 EL604
01837      MOVE CF-USER2-SELECT (3)          TO USR23O.                 EL604
01838      IF CF-USER3-OPT-SEQ NOT = 0                                  EL604
01839         MOVE CF-USER3-OPT-SEQ          TO USR3SEQO.               EL604
01840      MOVE CF-USER3-SELECT (1)          TO USR31O.                 EL604
01841      MOVE CF-USER3-SELECT (2)          TO USR32O.                 EL604
01842      MOVE CF-USER3-SELECT (3)          TO USR33O.                 EL604
01843      IF CF-USER4-OPT-SEQ NOT = 0                                  EL604
01844         MOVE CF-USER4-OPT-SEQ          TO USR4SEQO.               EL604
01845      MOVE CF-USER4-SELECT (1)          TO USR41O.                 EL604
01846      MOVE CF-USER4-SELECT (2)          TO USR42O.                 EL604
01847      MOVE CF-USER4-SELECT (3)          TO USR43O.                 EL604
01848      IF CF-USER5-OPT-SEQ NOT = 0                                  EL604
01849         MOVE CF-USER5-OPT-SEQ          TO USR5SEQO.               EL604
01850      MOVE CF-USER5-SELECT (1)          TO USR51O.                 EL604
01851      MOVE CF-USER5-SELECT (2)          TO USR52O.                 EL604
01852      MOVE CF-USER5-SELECT (3)          TO USR53O.                 EL604
pemuni     IF CF-SEL-LO-LOSS-RATIO    NOT NUMERIC
pemuni        MOVE +0                 TO CF-SEL-LO-LOSS-RATIO
pemuni     end-if
pemuni     IF CF-SEL-HI-LOSS-RATIO    NOT NUMERIC
pemuni        MOVE +0                 TO CF-SEL-HI-LOSS-RATIO
pemuni     end-if
01853      MOVE CF-SEL-LO-LOSS-RATIO         TO LOLOSSO.                EL604
01854      MOVE CF-SEL-HI-LOSS-RATIO         TO HILOSSO.                EL604
01855                                                                   EL604
01856      IF CF-SEL-LO-ENTRY-DATE NOT = LOW-VALUES                     EL604
01857         MOVE CF-SEL-LO-ENTRY-DATE TO DC-BIN-DATE-1                EL604
01858         MOVE ' '                  TO DC-OPTION-CODE               EL604
01859         PERFORM 9700-DATE-LINK                                    EL604
01860         IF NO-CONVERSION-ERROR                                    EL604
01861            MOVE DC-GREG-DATE-1-EDIT     TO LOENTDTO.              EL604
01862                                                                   EL604
01863      IF CF-SEL-HI-ENTRY-DATE = LOW-VALUES                         EL604
01864         NEXT SENTENCE                                             EL604
01865      ELSE                                                         EL604
01866      IF CF-SEL-HI-ENTRY-DATE = HIGH-VALUES                        EL604
01867         MOVE '99/99/99'                TO HIENTDTO                EL604
01868      ELSE                                                         EL604
01869         MOVE CF-SEL-HI-ENTRY-DATE TO DC-BIN-DATE-1                EL604
01870         MOVE ' '                  TO DC-OPTION-CODE               EL604
01871         PERFORM 9700-DATE-LINK                                    EL604
01872         IF NO-CONVERSION-ERROR                                    EL604
01873            MOVE DC-GREG-DATE-1-EDIT     TO HIENTDTO.              EL604
01874                                                                   EL604
01875      IF CF-SEL-LO-EFFECTIVE-DATE = LOW-VALUES                     EL604
01876         MOVE SPACES                    TO LOEFFDTO                EL604
01877      ELSE                                                         EL604
01878         MOVE CF-SEL-LO-EFFECTIVE-DATE TO DC-BIN-DATE-1            EL604
01879         MOVE ' '                  TO DC-OPTION-CODE               EL604
01880         PERFORM 9700-DATE-LINK                                    EL604
01881         IF NO-CONVERSION-ERROR                                    EL604
01882            MOVE DC-GREG-DATE-1-EDIT     TO LOEFFDTO.              EL604
01883                                                                   EL604
01884      IF CF-SEL-HI-EFFECTIVE-DATE = LOW-VALUES                     EL604
01885         MOVE SPACES                    TO HIEFFDTO                EL604
01886      ELSE                                                         EL604
01887      IF CF-SEL-HI-EFFECTIVE-DATE = HIGH-VALUES                    EL604
01888         MOVE '99/99/99'                TO HIEFFDTO                EL604
01889      ELSE                                                         EL604
01890         MOVE CF-SEL-HI-EFFECTIVE-DATE TO DC-BIN-DATE-1            EL604
01891         MOVE ' '                  TO DC-OPTION-CODE               EL604
01892         PERFORM 9700-DATE-LINK                                    EL604
01893         IF NO-CONVERSION-ERROR                                    EL604
01894            MOVE DC-GREG-DATE-1-EDIT     TO HIEFFDTO.              EL604
01895                                                                   EL604
01896      MOVE CF-EXCEPTION-LIST-IND TO EXPRPTO.                          CL*14
01897                                                                      CL*14
01898      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.            EL604
01899      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.        EL604
01900                                                                   EL604
01901      GO TO 8100-SEND-INITIAL-MAP.                                 EL604
01902                                                                   EL604
01903  4100-NOT-FOUND.                                                     CL*14
01904      MOVE  ER-0142               TO EMI-ERROR.                    EL604
01905      MOVE -1                     TO REPORTL.                      EL604
01906      MOVE AL-UNBON               TO REPORTA.                      EL604
01907      PERFORM 9900-ERROR-FORMAT.                                   EL604
01908      GO TO 8200-SEND-DATAONLY.                                    EL604
01909                                                                   EL604
01910  5000-VERIFY-CARRIER.                                             EL604
01911      EXEC CICS HANDLE CONDITION                                   EL604
01912           NOTFND    (5010-NOT-FOUND)                              EL604
01913           ENDFILE   (5010-NOT-FOUND)                              EL604
01914      END-EXEC.                                                    EL604
01915                                                                   EL604
01916      MOVE SPACES TO ELCNTL-TEST-KEY.                              EL604
01917      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.              EL604
01918      MOVE '6'                TO ELCNTL-TEST-REC-TYPE.             EL604
01919      MOVE WS-TEST-CARRIER    TO ELCNTL-TEST-CAR.                  EL604
01920      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.               EL604
01921                                                                   EL604
01922      EXEC CICS READ                                               EL604
01923           DATASET    (ELCNTL-FILE-ID)                             EL604
01924           RIDFLD     (ELCNTL-TEST-KEY)                            EL604
01925           SET        (ADDRESS OF CONTROL-FILE)                       CL*15
01926      END-EXEC.                                                    EL604
01927                                                                   EL604
01928      GO TO 5019-EXIT.                                             EL604
01929                                                                   EL604
01930  5010-NOT-FOUND.                                                  EL604
01931      MOVE ER-2845           TO EMI-ERROR                          EL604
01932      MOVE -1                TO WS-TEST-LEN.                       EL604
01933      MOVE AL-UNBON          TO WS-TEST-ATTRB.                     EL604
01934      PERFORM 9900-ERROR-FORMAT.                                   EL604
01935                                                                   EL604
01936  5019-EXIT.                                                       EL604
01937      EXIT.                                                        EL604
01938                                                                   EL604
01939  5020-VERIFY-STATE.                                               EL604
01940      EXEC CICS HANDLE CONDITION                                   EL604
01941           NOTFND    (5030-NOT-FOUND)                              EL604
01942           ENDFILE   (5030-NOT-FOUND)                              EL604
01943      END-EXEC.                                                    EL604
01944                                                                   EL604
01945      MOVE SPACES TO ELCNTL-TEST-KEY.                              EL604
01946      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.              EL604
01947      MOVE '3'                TO ELCNTL-TEST-REC-TYPE.             EL604
01948      MOVE WS-TEST-STATE      TO ELCNTL-TEST-STATE.                EL604
01949      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.               EL604
01950                                                                   EL604
01951      EXEC CICS READ                                               EL604
01952           DATASET    (ELCNTL-FILE-ID)                             EL604
01953           RIDFLD     (ELCNTL-TEST-KEY)                            EL604
01954           SET        (ADDRESS OF CONTROL-FILE)                       CL*15
01955      END-EXEC.                                                    EL604
01956                                                                   EL604
01957      GO TO 5039-EXIT.                                             EL604
01958                                                                   EL604
01959  5030-NOT-FOUND.                                                  EL604
01960      MOVE ER-2848           TO EMI-ERROR                          EL604
01961      MOVE -1                TO WS-TEST-LEN.                       EL604
01962      MOVE AL-UNBON          TO WS-TEST-ATTRB.                     EL604
01963      PERFORM 9900-ERROR-FORMAT.                                   EL604
01964                                                                   EL604
01965  5039-EXIT.                                                       EL604
01966      EXIT.                                                        EL604
01967                                                                   EL604
01968  5040-VERIFY-BEN.                                                 EL604
01969      IF WS-TEST-BEN = SPACES                                         CL**6
01970          GO TO 5059-EXIT.                                            CL**6
01971                                                                      CL**6
01972      MOVE SPACES TO WS-BROWSE-SW.                                 EL604
01973                                                                   EL604
01974      EXEC CICS HANDLE CONDITION                                   EL604
01975           NOTFND    (5050-NOT-FOUND)                              EL604
01976           ENDFILE   (5050-NOT-FOUND)                              EL604
01977      END-EXEC.                                                    EL604
01978                                                                   EL604
01979      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.              EL604
01980      MOVE WS-TEST-BEN        TO ELCNTL-TEST-BEN.                  EL604
01981      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.               EL604
01982                                                                   EL604
01983      EXEC CICS STARTBR                                            EL604
01984           DATASET    (ELCNTL-FILE-ID)                             EL604
01985           RIDFLD     (ELCNTL-TEST-KEY)                            EL604
01986      END-EXEC.                                                    EL604
01987                                                                   EL604
01988      MOVE 'Y'    TO WS-BROWSE-SW.                                 EL604
01989                                                                   EL604
01990      EXEC CICS READNEXT                                           EL604
01991           DATASET    (ELCNTL-FILE-ID)                             EL604
01992           RIDFLD     (ELCNTL-TEST-KEY)                            EL604
01993           SET        (ADDRESS OF CONTROL-FILE)                       CL*15
01994      END-EXEC.                                                    EL604
01995                                                                   EL604
01996      MOVE +1  TO WS-SUB.                                          EL604
01997                                                                   EL604
01998  5041-SEARCH-BEN-TYPE.                                            EL604
01999      IF WS-SUB  GREATER THAN +8                                   EL604
02000         GO TO 5050-NOT-FOUND.                                     EL604
02001                                                                   EL604
02002      IF CF-BENEFIT-CODE (WS-SUB) = WS-TEST-BEN                       CL**2
02003         GO TO 5055-ENDBR                                          EL604
02004      ELSE                                                         EL604
02005         ADD +1 TO WS-SUB                                          EL604
02006         GO TO 5041-SEARCH-BEN-TYPE.                               EL604
02007                                                                   EL604
02008  5050-NOT-FOUND.                                                  EL604
02009      MOVE ER-7685           TO EMI-ERROR                          EL604
02010      MOVE -1                TO WS-TEST-LEN.                       EL604
02011      MOVE AL-UNBON          TO WS-TEST-ATTRB.                     EL604
02012      PERFORM 9900-ERROR-FORMAT.                                   EL604
02013                                                                   EL604
02014  5055-ENDBR.                                                      EL604
02015      IF WS-BROWSE-SW = 'Y'                                        EL604
02016         EXEC CICS ENDBR                                           EL604
02017              DATASET    (ELCNTL-FILE-ID)                          EL604
02018         END-EXEC.                                                 EL604
02019                                                                   EL604
02020  5059-EXIT.                                                       EL604
02021      EXIT.                                                        EL604
02022                                                                   EL604
02023  5060-VERIFY-BUS-TYPE.                                            EL604
02024      MOVE SPACES TO WS-BROWSE-SW.                                 EL604
02025                                                                   EL604
02026      EXEC CICS HANDLE CONDITION                                   EL604
02027           NOTFND    (5070-NOT-FOUND)                              EL604
02028           ENDFILE   (5070-NOT-FOUND)                              EL604
02029      END-EXEC.                                                    EL604
02030                                                                   EL604
02031      MOVE SPACES             TO ELCNTL-TEST-KEY.                  EL604
02032      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.              EL604
02033      MOVE '8'                TO ELCNTL-TEST-REC-TYPE.             EL604
02034      MOVE WS-TEST-BUSTYP     TO ELCNTL-TEST-BUSTYP.                  CL**2
02035      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.               EL604
02036                                                                   EL604
02037      EXEC CICS STARTBR                                            EL604
02038           DATASET    (ELCNTL-FILE-ID)                             EL604
02039           RIDFLD     (ELCNTL-TEST-KEY)                            EL604
02040      END-EXEC.                                                    EL604
02041                                                                   EL604
02042      MOVE 'Y' TO WS-BROWSE-SW.                                    EL604
02043                                                                   EL604
02044      EXEC CICS READNEXT                                           EL604
02045           DATASET    (ELCNTL-FILE-ID)                             EL604
02046           RIDFLD     (ELCNTL-TEST-KEY)                            EL604
02047           SET        (ADDRESS OF CONTROL-FILE)                       CL*15
02048      END-EXEC.                                                    EL604
02049                                                                   EL604
02050      COMPUTE WS-SUB = WS-TEST-BUSTYP -                               CL**2
02051                     (ELCNTL-TEST-BUSTYP - 20).                       CL**2
02052                                                                   EL604
02053      IF ELCNTL-TEST-BUSTYP = 99                                      CL**2
02054         SUBTRACT +1 FROM WS-SUB.                                  EL604
02055                                                                   EL604
02056      IF CF-BUSINESS-TITLE (WS-SUB) = SPACES                       EL604
02057         GO TO 5070-NOT-FOUND.                                     EL604
02058                                                                   EL604
02059      GO TO 5075-ENDBR.                                            EL604
02060                                                                   EL604
02061  5070-NOT-FOUND.                                                  EL604
02062                                                                   EL604
02063      MOVE ER-2178           TO EMI-ERROR                          EL604
02064      MOVE -1                TO WS-TEST-LEN.                       EL604
02065      MOVE AL-UNBON          TO WS-TEST-ATTRB.                     EL604
02066      PERFORM 9900-ERROR-FORMAT.                                   EL604
02067                                                                   EL604
02068  5075-ENDBR.                                                      EL604
02069      IF WS-BROWSE-SW = 'Y'                                        EL604
02070         EXEC CICS ENDBR                                           EL604
02071              DATASET    (ELCNTL-FILE-ID)                          EL604
02072         END-EXEC.                                                 EL604
02073                                                                   EL604
02074  5079-EXIT.                                                       EL604
02075      EXIT.                                                        EL604
02076      EJECT                                                        EL604
02077                                                                   EL604
02078  7000-BROWSE-FWRD-NEXT-ACCOUNT.                                   EL604
02079      MOVE SPACES            TO ELCNTL-KEY.                        EL604
02080      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.                 EL604
02081      MOVE 'C'               TO ELCNTL-RECORD-TYPE.                EL604
02082      MOVE +1                TO ELCNTL-SEQ-NO.                     EL604
02083                                                                   EL604
02084      IF REPORTL GREATER THAN +0                                   EL604
02085         MOVE REPORTI            TO ELCNTL-REPORT                  EL604
02086      ELSE                                                         EL604
02087         MOVE ZEROS              TO ELCNTL-REPORT.                 EL604
02088                                                                   EL604
02089      MOVE 'N'                    TO WS-REPORT-FOUND-SW.           EL604
02090                                                                   EL604
02091      EXEC CICS HANDLE CONDITION                                   EL604
02092          NOTFND   (7080-END-OF-SEARCH)                            EL604
02093      END-EXEC.                                                    EL604
02094                                                                   EL604
02095      EXEC CICS STARTBR                                            EL604
02096          DATASET  (ELCNTL-FILE-ID)                                EL604
02097          RIDFLD   (ELCNTL-KEY)                                    EL604
02098      END-EXEC.                                                    EL604
02099                                                                   EL604
02100      EXEC CICS HANDLE CONDITION                                   EL604
02101          NOTFND   (7070-END-OF-BROWSE)                            EL604
02102          ENDFILE  (7070-END-OF-BROWSE)                            EL604
02103      END-EXEC.                                                    EL604
02104                                                                   EL604
02105  7010-READ-FWRD-NEXT-RECORD.                                      EL604
02106      EXEC CICS READNEXT                                           EL604
02107          DATASET  (ELCNTL-FILE-ID)                                EL604
02108          RIDFLD   (ELCNTL-KEY)                                    EL604
02109          SET      (ADDRESS OF CONTROL-FILE)                          CL*15
02110      END-EXEC.                                                    EL604
02111                                                                   EL604
02112      IF ELCNTL-COMPANY-ID = PI-COMPANY-ID AND                     EL604
02113         ELCNTL-RECORD-TYPE = 'C'                                  EL604
02114         NEXT SENTENCE                                             EL604
02115      ELSE                                                         EL604
02116         GO TO 7070-END-OF-BROWSE.                                 EL604
02117                                                                   EL604
02118      IF ELCNTL-SEQ-NO = +0                                           CL*14
02119          NEXT SENTENCE                                               CL*14
02120      ELSE                                                            CL*14
02121          GO TO 7010-READ-FWRD-NEXT-RECORD.                           CL*14
02122                                                                      CL*14
02123      MOVE 'Y'                    TO WS-REPORT-FOUND-SW.           EL604
02124                                                                   EL604
02125  7070-END-OF-BROWSE.                                              EL604
02126      EXEC CICS ENDBR                                              EL604
02127          DATASET  (ELCNTL-FILE-ID)                                EL604
02128      END-EXEC.                                                    EL604
02129                                                                   EL604
02130       IF REPORT-WAS-NOT-FOUND                                     EL604
02131          NEXT SENTENCE                                            EL604
02132       ELSE                                                        EL604
02133          GO TO 7090-EXIT.                                         EL604
02134                                                                   EL604
02135  7080-END-OF-SEARCH.                                              EL604
02136      MOVE -1                     TO MAINTL.                       EL604
02137      MOVE  ER-2237               TO EMI-ERROR.                    EL604
02138      PERFORM 9900-ERROR-FORMAT.                                   EL604
02139      GO TO 8200-SEND-DATAONLY.                                    EL604
02140                                                                   EL604
02141  7090-EXIT.                                                       EL604
02142      EXIT.                                                        EL604
02143                                                                   EL604
02144      EJECT                                                        EL604
02145  7100-BROWSE-BWRD-NEXT-ACCOUNT.                                   EL604
02146      MOVE 'N'                    TO WS-REPORT-FOUND-SW.           EL604
02147                                                                   EL604
02148      MOVE SPACES                 TO ELCNTL-KEY.                   EL604
02149      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            EL604
02150      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.           EL604
02151      MOVE +0                     TO ELCNTL-SEQ-NO.                EL604
02152                                                                   EL604
02153      IF REPORTL GREATER THAN +0                                   EL604
02154         MOVE REPORTI            TO ELCNTL-REPORT                  EL604
02155      ELSE                                                         EL604
02156         MOVE PI-LAST-REPORT     TO ELCNTL-REPORT.                 EL604
02157                                                                   EL604
02158      EXEC CICS HANDLE CONDITION                                   EL604
02159          NOTFND   (7180-END-OF-SEARCH)                            EL604
02160      END-EXEC.                                                    EL604
02161                                                                   EL604
02162      EXEC CICS STARTBR                                            EL604
02163          DATASET  (ELCNTL-FILE-ID)                                EL604
02164          RIDFLD   (ELCNTL-KEY)                                    EL604
02165      END-EXEC.                                                    EL604
02166                                                                   EL604
02167      EXEC CICS HANDLE CONDITION                                   EL604
02168          NOTFND   (7170-END-OF-BROWSE)                            EL604
02169          ENDFILE  (7170-END-OF-BROWSE)                            EL604
02170      END-EXEC.                                                    EL604
02171                                                                   EL604
02172  7110-READ-FWRD-NEXT-RECORD.                                      EL604
02173      EXEC CICS READNEXT                                           EL604
02174          DATASET  (ELCNTL-FILE-ID)                                EL604
02175          RIDFLD   (ELCNTL-KEY)                                    EL604
02176          SET      (ADDRESS OF CONTROL-FILE)                          CL*15
02177      END-EXEC.                                                    EL604
02178                                                                   EL604
02179      EXEC CICS READPREV                                              CL*14
02180          DATASET  (ELCNTL-FILE-ID)                                   CL*14
02181          RIDFLD   (ELCNTL-KEY)                                       CL*14
02182          SET      (ADDRESS OF CONTROL-FILE)                          CL*15
02183      END-EXEC.                                                       CL*14
02184                                                                      CL*14
02185  7110-READ-BWRD-NEXT-RECORD.                                      EL604
02186      EXEC CICS READPREV                                           EL604
02187          DATASET  (ELCNTL-FILE-ID)                                EL604
02188          RIDFLD   (ELCNTL-KEY)                                    EL604
02189          SET      (ADDRESS OF CONTROL-FILE)                          CL*15
02190      END-EXEC.                                                    EL604
02191                                                                   EL604
02192      IF ELCNTL-COMPANY-ID = PI-COMPANY-ID AND                     EL604
02193         ELCNTL-RECORD-TYPE = 'C'                                  EL604
02194         NEXT SENTENCE                                             EL604
02195      ELSE                                                         EL604
02196         GO TO 7170-END-OF-BROWSE.                                 EL604
02197                                                                      CL*14
02198      IF ELCNTL-SEQ-NO = +0                                           CL*14
02199          NEXT SENTENCE                                               CL*14
02200      ELSE                                                            CL*14
02201          GO TO 7110-READ-BWRD-NEXT-RECORD.                           CL*14
02202                                                                   EL604
02203      MOVE 'Y'                    TO WS-REPORT-FOUND-SW.           EL604
02204                                                                   EL604
02205  7170-END-OF-BROWSE.                                              EL604
02206      EXEC CICS ENDBR                                              EL604
02207          DATASET  (ELCNTL-FILE-ID)                                EL604
02208      END-EXEC.                                                    EL604
02209                                                                   EL604
02210      IF REPORT-WAS-NOT-FOUND                                      EL604
02211         NEXT SENTENCE                                             EL604
02212      ELSE                                                         EL604
02213         GO TO 7190-EXIT.                                          EL604
02214                                                                   EL604
02215  7180-END-OF-SEARCH.                                              EL604
02216      MOVE -1                     TO MAINTL.                       EL604
02217      MOVE  ER-2238               TO EMI-ERROR.                    EL604
02218      PERFORM 9900-ERROR-FORMAT.                                   EL604
02219      GO TO 8200-SEND-DATAONLY.                                    EL604
02220                                                                   EL604
02221   7190-EXIT.                                                      EL604
02222      EXIT.                                                        EL604
02223                                                                   EL604
02224      EJECT                                                        EL604
02225  8100-SEND-INITIAL-MAP.                                           EL604
02226      MOVE SAVE-DATE              TO RUNDTEO.                      EL604
02227      MOVE EIBTIME                TO TIME-IN.                      EL604
02228      MOVE TIME-OUT               TO RUNTIMEO.                     EL604
02229      MOVE SPACES                 TO MAINTO.                       EL604
02230      MOVE -1                     TO MAINTL.                       EL604
02231      MOVE PI-LIFE-OVERRIDE-L2    TO LBENTPO.                      EL604
02232      MOVE PI-AH-OVERRIDE-L2      TO ABENTPO.                      EL604
02233      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL604
02234      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL604
02235                                                                   EL604
02236  8150-SEND-INITIAL-MAP.                                           EL604
02237      EXEC CICS SEND                                               EL604
02238          MAP      (MAP-NAME)                                      EL604
02239          MAPSET   (MAPSET-NAME)                                   EL604
02240          FROM     (EL604AO)                                       EL604
02241          ERASE                                                    EL604
02242          CURSOR                                                   EL604
02243      END-EXEC.                                                    EL604
02244                                                                   EL604
02245      GO TO 9100-RETURN-TRAN.                                      EL604
02246                                                                   EL604
02247  8200-SEND-DATAONLY.                                              EL604
02248      MOVE SAVE-DATE              TO RUNDTEO                       EL604
02249      MOVE EIBTIME                TO TIME-IN.                      EL604
02250      MOVE TIME-OUT               TO RUNTIMEO                      EL604
02251      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL604
02252      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                     EL604
02253      EXEC CICS SEND                                               EL604
02254          MAP      (MAP-NAME)                                      EL604
02255          MAPSET   (MAPSET-NAME)                                   EL604
02256          FROM     (EL604AO)                                       EL604
02257          DATAONLY                                                 EL604
02258          CURSOR                                                   EL604
02259      END-EXEC.                                                    EL604
02260                                                                   EL604
02261      GO TO 9100-RETURN-TRAN.                                      EL604
02262                                                                   EL604
02263      EJECT                                                        EL604
02264  8300-SEND-TEXT.                                                  EL604
02265      EXEC CICS SEND TEXT                                          EL604
02266          FROM     (LOGOFF-TEXT)                                   EL604
02267          LENGTH   (LOGOFF-LENGTH)                                 EL604
02268          ERASE                                                    EL604
02269          FREEKB                                                   EL604
02270      END-EXEC.                                                    EL604
02271                                                                   EL604
02272      EXEC CICS RETURN                                             EL604
02273      END-EXEC.                                                    EL604
02274                                                                   EL604
02275      EJECT                                                        EL604
02276  8400-LOG-JOURNAL-RECORD.                                         EL604
02277      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL604
02278      MOVE ELCNTL-FILE-ID         TO JP-FILE-ID.                   EL604
02279      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL604
pemuni*    EXEC CICS JOURNAL                                            EL604
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)                         EL604
pemuni*        JTYPEID     ('CR')                                       EL604
pemuni*        FROM        (JOURNAL-RECORD)                             EL604
pemuni*        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   EL604
pemuni*    END-EXEC.                                                    EL604
02286                                                                   EL604
02287  8400-EXIT.                                                       EL604
02288      EXIT.                                                        EL604
02289                                                                   EL604
02290  8600-DEEDIT.                                                     EL604
02291      EXEC CICS BIF DEEDIT                                         EL604
02292           FIELD   (DEEDIT-FIELD)                                  EL604
02293           LENGTH  (15)                                            EL604
02294       END-EXEC.                                                   EL604
02295                                                                   EL604
02296  8800-UNAUTHORIZED-ACCESS.                                        EL604
02297      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL604
02298      GO TO 8300-SEND-TEXT.                                        EL604
02299                                                                   EL604
02300  8810-PF23.                                                       EL604
02301      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL604
02302      MOVE XCTL-005               TO PGM-NAME.                     EL604
02303      GO TO 9300-XCTL.                                                CL*14
02304                                                                      CL*14
02305  8900-PF03.                                                          CL*14
02306      MOVE XCTL-EL6041            TO PGM-NAME.                        CL*14
02307      GO TO 9300-XCTL.                                             EL604
02308                                                                   EL604
02309  9000-RETURN-CICS.                                                EL604
02310      EXEC CICS RETURN                                             EL604
02311      END-EXEC.                                                    EL604
02312                                                                   EL604
02313  9100-RETURN-TRAN.                                                EL604
02314      MOVE    EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.          EL604
02315      MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.      EL604
02316      EXEC CICS RETURN                                             EL604
02317          TRANSID    (TRANS-ID)                                    EL604
02318          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL604
02319          LENGTH     (PI-COMM-LENGTH)                              EL604
02320      END-EXEC.                                                    EL604
02321                                                                   EL604
02322  9200-RETURN-MAIN-MENU.                                           EL604
02323                                                                      CL**4
02324      IF  CREDIT-SESSION                                              CL**4
02325          MOVE XCTL-EL626         TO PGM-NAME                         CL**4
02326                                                                      CL**4
02327      ELSE                                                            CL**4
02328          IF  CLAIM-SESSION                                           CL**4
02329              MOVE XCTL-EL126     TO PGM-NAME                         CL**4
02330                                                                      CL**4
02331          ELSE                                                        CL**4
02332              IF  MORTGAGE-SESSION                                    CL**4
02333                  MOVE XCTL-EM626 TO PGM-NAME                         CL**4
02334                                                                      CL**4
02335              ELSE                                                    CL**4
02336                  IF  GENERAL-LEDGER-SESSION                          CL**4
02337                      MOVE XCTL-GL800                                 CL**4
02338                                  TO PGM-NAME.                        CL**4
02339                                                                      CL**4
02340      GO TO 9300-XCTL.                                             EL604
02341                                                                   EL604
02342  9300-XCTL.                                                       EL604
02343      EXEC CICS XCTL                                               EL604
02344          PROGRAM    (PGM-NAME)                                    EL604
02345          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL604
02346          LENGTH     (PI-COMM-LENGTH)                              EL604
02347      END-EXEC.                                                    EL604
02348                                                                   EL604
02349  9400-CLEAR.                                                      EL604
02350      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL604
02351      GO TO 9300-XCTL.                                             EL604
02352                                                                   EL604
02353  9500-PF12.                                                       EL604
02354      MOVE XCTL-010               TO PGM-NAME.                     EL604
02355      GO TO 9300-XCTL.                                             EL604
02356                                                                   EL604
02357  9600-PGMID-ERROR.                                                EL604
02358      EXEC CICS HANDLE CONDITION                                   EL604
02359          PGMIDERR    (8300-SEND-TEXT)                             EL604
02360      END-EXEC.                                                    EL604
02361                                                                   EL604
02362      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL604
02363      MOVE ' '                    TO PI-ENTRY-CD-1.                EL604
02364      MOVE XCTL-005               TO PGM-NAME.                     EL604
02365      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL604
02366      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL604
02367      GO TO 9300-XCTL.                                             EL604
02368                                                                   EL604
02369  9700-DATE-LINK.                                                  EL604
02370      MOVE LINK-ELDATCV           TO PGM-NAME                      EL604
02371      EXEC CICS LINK                                               EL604
02372          PROGRAM    (PGM-NAME)                                    EL604
02373          COMMAREA   (DATE-CONVERSION-DATA)                        EL604
02374          LENGTH     (DC-COMM-LENGTH)                              EL604
02375      END-EXEC.                                                    EL604
02376                                                                   EL604
02377  9900-ERROR-FORMAT.                                               EL604
02378      IF NOT EMI-ERRORS-COMPLETE                                   EL604
02379          MOVE LINK-001           TO PGM-NAME                      EL604
02380          EXEC CICS LINK                                           EL604
02381              PROGRAM    (PGM-NAME)                                EL604
02382              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL604
02383              LENGTH     (EMI-COMM-LENGTH)                         EL604
02384          END-EXEC.                                                EL604
02385                                                                   EL604
02386  9900-EXIT.                                                       EL604
02387      EXIT.                                                        EL604
02388                                                                   EL604
02389  9990-ABEND.                                                      EL604
02390      MOVE LINK-004               TO PGM-NAME.                     EL604
02391      MOVE DFHEIBLK               TO EMI-LINE1                     EL604
02392                                                                   EL604
02393      EXEC CICS LINK                                               EL604
02394          PROGRAM   (PGM-NAME)                                     EL604
02395          COMMAREA  (EMI-LINE1)                                    EL604
02396          LENGTH    (72)                                           EL604
02397      END-EXEC.                                                    EL604
02398                                                                   EL604
02399      MOVE -1                     TO MAINTL.                       EL604
02400      GO TO 8200-SEND-DATAONLY.                                    EL604
02401                                                                      CL**4
02402      EJECT                                                           CL**4
092308 9910-INITIALIZE-SECURITY.                                        
      ******************************************************************
      *                                                                *
      *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
      *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *
      *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
      *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
      *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
      *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
      *                                                                *
      ******************************************************************
                                                                        
           IF  PI-PROCESSOR-ID NOT = 'LGXX'                             
               IF  MORTGAGE-SESSION                                     
                   MOVE '125E'             TO SC-QUID-SYSTEM            
                   MOVE EIBTRMID           TO SC-QUID-TERMINAL          
                                                                        
                   EXEC CICS READQ TS                                   
                       QUEUE  (SC-QUID-KEY)                             
                       INTO   (SECURITY-CONTROL-E)                      
                       LENGTH (SC-COMM-LENGTH-E)                        
                       ITEM   (SC-ITEM)                                 
                   END-EXEC                                             
                                                                        
                   MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)                
                                           TO PI-DISPLAY-CAP            
                   MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)                 
                                           TO PI-MODIFY-CAP             
                                                                        
                   IF  NOT DISPLAY-CAP                                  
                       MOVE 'READ'         TO SM-READ                   
                       PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT   
                       MOVE ER-9097        TO EMI-ERROR                 
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
                       GO TO 8100-SEND-INITIAL-MAP                      
                   ELSE                                                 
                       GO TO 9910-EXIT                                  
               ELSE                                                     
                   EXEC CICS  READQ TS                                  
                       QUEUE   (PI-SECURITY-TEMP-STORE-ID)              
                       INTO    (SECURITY-CONTROL)                       
                       LENGTH  (SC-COMM-LENGTH)                         
                       ITEM    (SC-ITEM-CL-CR)                          
                       END-EXEC                                         

                   MOVE SC-CREDIT-DISPLAY (30)
                                       TO PI-DISPLAY-CAP                
                   MOVE SC-CREDIT-UPDATE  (30)
                                       TO PI-MODIFY-CAP                 

                   IF  NOT DISPLAY-CAP                                  
                       MOVE 'READ'     TO SM-READ                       
                       PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT   
                       MOVE ER-0070    TO  EMI-ERROR                    
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         
                       GO TO 8100-SEND-INITIAL-MAP.                     

092308 9910-EXIT.                                                       
           EXIT.                                                        

02432  9995-SECURITY-VIOLATION.                                         EL604
02433             COPY ELCSCTP.                                         EL604
02434                                                                   EL604
02435  9995-EXIT.                                                       EL604
02436       EXIT.                                                       EL604
