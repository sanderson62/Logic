00001  ID DIVISION.                                                     03/08/96
00002                                                                   EL6041
00003  PROGRAM-ID.                 EL6041.                                 LV004
00004 *              PROGRAM CONVERTED BY                                  CL**4
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**4
00006 *              CONVERSION DATE 02/12/96 17:00:55.                    CL**4
00007 *                            VMOD=2.004.                             CL**4
00008 *                                                                 EL6041
00008 *                                                                 EL6041
00009 *AUTHOR.     LOGIC,INC.                                              CL**4
00010 *            DALLAS, TEXAS.                                          CL**4
00011                                                                   EL6041
00012 *DATE-COMPILED.                                                      CL**4
00013 *SECURITY.   *****************************************************   CL**4
00014 *            *                                                   *   CL**4
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**4
00016 *            *                                                   *   CL**4
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**4
00018 *                                                                *   CL**4
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**4
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**4
00021 *            *                                                   *   CL**4
00022 *            *****************************************************   CL**4
00023                                                                   EL6041
00024 *REMARKS.    TRANSACTION - EX67 - REPORT CUSTOMIZATION.              CL**4
00025                                                                   EL6041
00026  ENVIRONMENT DIVISION.                                            EL6041
00027                                                                   EL6041
00028      EJECT                                                        EL6041
00029  DATA DIVISION.                                                   EL6041
00030  WORKING-STORAGE SECTION.                                         EL6041
00031  77  FILLER  PIC X(32)  VALUE '********************************'. EL6041
00032  77  FILLER  PIC X(32)  VALUE '*    EL6041  WORKING STORAGE   *'. EL6041
00033  77  FILLER  PIC X(32)  VALUE '************VMOD=2.004 *********'.    CL**4
00034                                                                   EL6041
00035                                  COPY ELCSCTM.                    EL6041
00036      EJECT                                                        EL6041
00037                                  COPY ELCSCRTY.                   EL6041
00038                                                                   EL6041
00039      EJECT                                                        EL6041
00040  01  WS-DATE-AREA.                                                EL6041
00041      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.    EL6041
00042      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.    EL6041
00043                                                                   EL6041
00044  01  STANDARD-AREAS.                                              EL6041
00045      12  SC-ITEM                     PIC S9(4) COMP VALUE +1.     EL6041
00046      12  WS-TEST-CARRIER             PIC X       VALUE SPACES.    EL6041
00047      12  WS-TEST-STATE               PIC XX      VALUE SPACES.    EL6041
00048      12  WS-TEST-BUSTYP              PIC 99      VALUE ZEROS.     EL6041
00049      12  WS-TEST-BEN                 PIC XX      VALUE SPACES.    EL6041
00050          88  INVALID-BENEFIT-CODE       VALUE '00'                EL6041
00051                                               '90' THRU '99'.     EL6041
00052      12  WS-TEST-LEN                 PIC S9(4)   VALUE +0.        EL6041
00053      12  WS-TEST-ATTRB               PIC X       VALUE SPACES.    EL6041
00054      12  WS-SUB                      PIC S9(4)   VALUE +0 COMP.   EL6041
00055      12  WS-BROWSE-SW                PIC X       VALUE SPACES.    EL6041
00056      12  WS-SEQ-AREA.                                             EL6041
00057          16  WS-SEQ-NO OCCURS 6 TIMES PIC X.                      EL6041
00058      12  WS-LO-EFF                   PIC XX      VALUE LOW-VALUES.EL6041
00059      12  WS-HI-EFF                   PIC XX      VALUE LOW-VALUES.EL6041
00060      12  WS-LO-ENT                   PIC XX      VALUE LOW-VALUES.EL6041
00061      12  WS-HI-ENT                   PIC XX      VALUE LOW-VALUES.EL6041
00062      12  WS-LO-LOSS                  PIC S9(3)V99 VALUE +0.       EL6041
00063      12  WS-HI-LOSS                  PIC S9(3)V99 VALUE +0.       EL6041
00064      12  GETMAIN-SPACE               PIC X       VALUE SPACE.     EL6041
00065      12  MAP-NAME                    PIC X(8)    VALUE 'EL6041A'. EL6041
00066      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6041S'. EL6041
00067      12  SCREEN-NUMBER               PIC X(4)    VALUE '604B'.    EL6041
00068      12  TRANS-ID                    PIC X(4)    VALUE 'EX67'.    EL6041
00069      12  THIS-PGM                    PIC X(8)    VALUE 'EL6041'.  EL6041
00070      12  PGM-NAME                    PIC X(8).                    EL6041
00071      12  TIME-IN                     PIC S9(7).                   EL6041
00072      12  TIME-OUT-R  REDEFINES TIME-IN.                           EL6041
00073          16  FILLER                  PIC X.                       EL6041
00074          16  TIME-OUT                PIC 99V99.                   EL6041
00075          16  FILLER                  PIC XX.                      EL6041
00076      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.   EL6041
00077      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.   EL6041
00078      12  XCTL-EL126                  PIC X(8)    VALUE 'EL126'.   EL6041
00079      12  XCTL-EL626                  PIC X(8)    VALUE 'EL626'.   EL6041
00080      12  XCTL-EM626                  PIC X(8)    VALUE 'EM626'.   EL6041
00081      12  XCTL-GL800                  PIC X(8)    VALUE 'GL800'.   EL6041
00082      12  LINK-001                    PIC X(8)    VALUE 'EL001'.   EL6041
00083      12  LINK-004                    PIC X(8)    VALUE 'EL004'.   EL6041
00084      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'. EL6041
00085      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.  EL6041
00086      12  WS-JOURNAL-RECORD-LENGTH PIC S9(4)   COMP VALUE +750.    EL6041
00087                                                                   EL6041
00088      12  DEEDIT-FIELD                PIC X(15).                   EL6041
00089      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).     EL6041
00090      12  DEEDIT-FIELD-V1 REDEFINES DEEDIT-FIELD   PIC S9(14)V9.   EL6041
00091      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.  EL6041
00092                                                                   EL6041
00093      12  RETURN-FROM                 PIC X(8).                    EL6041
00094      12  WS-REPORT-FOUND-SW          PIC X       VALUE 'N'.       EL6041
00095          88  REPORT-WAS-FOUND                    VALUE 'Y'.       EL6041
00096          88  REPORT-WAS-NOT-FOUND                VALUE 'N'.       EL6041
00097                                                                   EL6041
00098  01  WS-MISC-WORK.                                                EL6041
00099      12  WS-COMBINED-LIFE-AH-OPT.                                 EL6041
00100          16  WS-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.    EL6041
00101          16  WS-SINGLE-MO-PREM-PCT      PIC S9(02).               EL6041
00102          16  WS-EARN-PREM-DECLINE       PIC S9(02).               EL6041
00103          16  WS-CANCELLATION-RATIO      PIC S9(02).               EL6041
00104          16  WS-RETENTION-LIMIT         PIC S9(07).                  CL**3
00105                                                                   EL6041
00106      12  WS-LIFE-OPT.                                             EL6041
00107          16  WS-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.    EL6041
00108          16  WS-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.    EL6041
00109          16  WS-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.    EL6041
00110          16  WS-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.    EL6041
00111          16  WS-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.    EL6041
00112          16  WS-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.    EL6041
00113          16  WS-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.    EL6041
00114          16  WS-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.    EL6041
00115          16  WS-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.    EL6041
00116          16  WS-LF-AVG-AGE-MAX          PIC S9(02).               EL6041
00117                                                                   EL6041
00118      12  WS-AH-OPT.                                               EL6041
00119          16  WS-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.    EL6041
00120          16  WS-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.    EL6041
00121          16  WS-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.    EL6041
00122          16  WS-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.    EL6041
00123          16  WS-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.    EL6041
00124          16  WS-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.    EL6041
00125          16  WS-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.    EL6041
00126          16  WS-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.    EL6041
00127          16  WS-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.    EL6041
00128          16  WS-AH-AVG-AGE-MAX          PIC S9(02).               EL6041
00129                                                                   EL6041
00130      EJECT                                                        EL6041
00131  01   ERROR-MESSAGES.                                             EL6041
00132      12  ER-0000                     PIC  X(4)   VALUE '0000'.    EL6041
00133      12  ER-0004                     PIC  X(4)   VALUE '0004'.    EL6041
00134      12  ER-0021                     PIC  X(4)   VALUE '0021'.    EL6041
00135      12  ER-0023                     PIC  X(4)   VALUE '0023'.    EL6041
00136      12  ER-0028                     PIC  X(4)   VALUE '0028'.    EL6041
00137      12  ER-0029                     PIC  X(4)   VALUE '0029'.    EL6041
00138      12  ER-0050                     PIC  X(4)   VALUE '0050'.    EL6041
00139      12  ER-0068                     PIC  X(4)   VALUE '0068'.    EL6041
00140      12  ER-0070                     PIC  X(4)   VALUE '0070'.    EL6041
00141      12  ER-0132                     PIC  X(4)   VALUE '0132'.    EL6041
00142      12  ER-0138                     PIC  X(4)   VALUE '0138'.    EL6041
00143      12  ER-0139                     PIC  X(4)   VALUE '0139'.    EL6041
00144      12  ER-0142                     PIC  X(4)   VALUE '0142'.    EL6041
00145      12  ER-2001                     PIC  X(4)   VALUE '2001'.       CL**2
00146      12  ER-2178                     PIC  X(4)   VALUE '2178'.    EL6041
00147      12  ER-2223                     PIC  X(4)   VALUE '2223'.       CL**2
00148      12  ER-2237                     PIC  X(4)   VALUE '2237'.    EL6041
00149      12  ER-2238                     PIC  X(4)   VALUE '2238'.    EL6041
00150      12  ER-2845                     PIC  X(4)   VALUE '2845'.    EL6041
00151      12  ER-2848                     PIC  X(4)   VALUE '2848'.    EL6041
00152      12  ER-7008                     PIC  X(4)   VALUE '7008'.    EL6041
00153      12  ER-7123                     PIC  X(4)   VALUE '7123'.    EL6041
00154      12  ER-7125                     PIC  X(4)   VALUE '7125'.    EL6041
00155      12  ER-7354                     PIC  X(4)   VALUE '7354'.       CL**2
00156      12  ER-7680                     PIC  X(4)   VALUE '7680'.    EL6041
00157      12  ER-7681                     PIC  X(4)   VALUE '7681'.    EL6041
00158      12  ER-7682                     PIC  X(4)   VALUE '7682'.    EL6041
00159      12  ER-7683                     PIC  X(4)   VALUE '7683'.    EL6041
00160      12  ER-7684                     PIC  X(4)   VALUE '7684'.    EL6041
00161      12  ER-7685                     PIC  X(4)   VALUE '7685'.    EL6041
00162      12  ER-9096                     PIC  X(4)   VALUE '9096'.    EL6041
00163      12  ER-9097                     PIC  X(4)   VALUE '9097'.    EL6041
00164                                                                   EL6041
00165      EJECT                                                        EL6041
00166                                                                   EL6041
00167  01  ACCESS-KEYS.                                                 EL6041
00168      12  ELCNTL-KEY.                                              EL6041
00169          16  ELCNTL-COMPANY-ID          PIC  XXX.                 EL6041
00170          16  ELCNTL-RECORD-TYPE         PIC  X.                   EL6041
00171          16  FILLER                     PIC  X.                   EL6041
00172          16  ELCNTL-REPORT              PIC  999.                 EL6041
00173          16  ELCNTL-SEQ-NO              PIC  S9(4)   COMP.        EL6041
00174                                                                   EL6041
00175      12  ELCNTL-TEST-KEY.                                         EL6041
00176          16  ELCNTL-TEST-COMP-ID        PIC  XXX.                 EL6041
00177          16  ELCNTL-TEST-REC-TYPE       PIC  X.                   EL6041
00178          16  ELCNTL-ACCESS-KEY.                                   EL6041
00179              20  FILLER                 PIC  XXX.                 EL6041
00180              20  ELCNTL-TEST-CAR        PIC  X.                   EL6041
00181          16  ELCNTL-ACCESS-KEY1 REDEFINES ELCNTL-ACCESS-KEY.      EL6041
00182              20  ELCNTL-TEST-STATE      PIC  XX.                  EL6041
00183              20  FILLER                 PIC  XX.                  EL6041
00184          16  ELCNTL-ACCESS-KEY2 REDEFINES ELCNTL-ACCESS-KEY.      EL6041
00185              20  FILLER                 PIC  XX.                  EL6041
00186              20  ELCNTL-TEST-BEN        PIC  XX.                  EL6041
00187          16  ELCNTL-ACCESS-KEY3 REDEFINES ELCNTL-ACCESS-KEY.      EL6041
00188              20  FILLER                 PIC  XX.                  EL6041
00189              20  ELCNTL-TEST-BUSTYP     PIC  99.                  EL6041
00190          16  ELCNTL-TEST-SEQ-NO         PIC  S9(4)   COMP.        EL6041
00191                                                                   EL6041
00192      12  ELCNTL-LENGTH           PIC S9(4) COMP VALUE +750.       EL6041
00193                                                                   EL6041
00194      EJECT                                                        EL6041
00195                                  COPY ELCDATE.                    EL6041
00196      EJECT                                                        EL6041
00197                                  COPY ELCLOGOF.                   EL6041
00198      EJECT                                                        EL6041
00199                                  COPY ELCATTR.                    EL6041
00200      EJECT                                                        EL6041
00201                                  COPY ELCEMIB.                    EL6041
00202      EJECT                                                        EL6041
00203                                  COPY ELCINTF.                    EL6041
00204                                                                   EL6041
00205      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                   EL6041
00206          16  PI-LAST-REPORT        PIC 9(3).                      EL6041
00207          16  FILLER                PIC X(637).                       CL**4
00208      EJECT                                                        EL6041
00209                              COPY ELCJPFX.                        EL6041
00210                              PIC X(750).                          EL6041
00211      EJECT                                                        EL6041
00212                              COPY ELCAID.                         EL6041
00213                                                                   EL6041
00214  01  FILLER    REDEFINES DFHAID.                                  EL6041
00215      12  FILLER              PIC X(8).                            EL6041
00216      12  PF-VALUES           PIC X       OCCURS 24 TIMES.         EL6041
00217                                                                   EL6041
00218      EJECT                                                        EL6041
00219                                  COPY EL6041S.                    EL6041
00220      EJECT                                                        EL6041
00221                                                                   EL6041
00222  LINKAGE SECTION.                                                 EL6041
00223  01  DFHCOMMAREA             PIC X(1024).                         EL6041
00224                                                                   EL6041
00225      EJECT                                                        EL6041
00226                                                                   EL6041
00227 *01 PARMLIST .                                                       CL**4
00228 *    12  FILLER                      PIC S9(8)   COMP.               CL**4
00229 *    12  ELCNTL-POINTER              PIC S9(8)   COMP.               CL**4
00230                                                                   EL6041
00231      EJECT                                                        EL6041
00232                                                                   EL6041
00233                              COPY ELCCNTL.                        EL6041
00234                                                                   EL6041
00235      EJECT                                                        EL6041
00236  PROCEDURE DIVISION.                                              EL6041
00237                                                                   EL6041
00238      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL6041
00239      MOVE '5'                    TO DC-OPTION-CODE.               EL6041
00240      PERFORM 9700-DATE-LINK.                                      EL6041
00241      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL6041
00242      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL6041
00243                                                                   EL6041
00244      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL6041
00245      MOVE 1                      TO EMI-NUMBER-OF-LINES.             CL**3
00246                                                                   EL6041
00247      IF EIBCALEN = 0                                              EL6041
00248          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL6041
00249                                                                   EL6041
00250      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL6041
00251          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL6041
00252              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      EL6041
00253              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      EL6041
00254              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      EL6041
00255              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      EL6041
00256              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      EL6041
00257              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      EL6041
00258              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    EL6041
00259              MOVE THIS-PGM             TO PI-CALLING-PROGRAM      EL6041
00260              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT      EL6041
00261          ELSE                                                     EL6041
00262              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM             EL6041
00263              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      EL6041
00264              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    EL6041
00265              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      EL6041
00266              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      EL6041
00267              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      EL6041
00268              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      EL6041
00269              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      EL6041
00270              MOVE SPACES               TO PI-SAVED-PROGRAM-6.     EL6041
00271                                                                   EL6041
00272      EXEC CICS    HANDLE    CONDITION                             EL6041
00273           PGMIDERR          (9600-PGMID-ERROR)                    EL6041
00274           ERROR             (9990-ABEND)                          EL6041
00275      END-EXEC.                                                    EL6041
00276                                                                   EL6041
00277      IF  EIBTRNID NOT = TRANS-ID                                  EL6041
00278          MOVE LOW-VALUES         TO EL6041AI                      EL6041
00279          GO TO 8100-SEND-INITIAL-MAP.                             EL6041
00280                                                                   EL6041
00281      IF  EIBAID = DFHCLEAR                                        EL6041
00282              OR                                                   EL6041
00283          NOT SYSTEM-DISPLAY-CAP                                   EL6041
00284          GO TO 9400-CLEAR.                                        EL6041
00285                                                                   EL6041
00286      EJECT                                                        EL6041
00287                                                                   EL6041
00288  0200-RECEIVE.                                                    EL6041
00289      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL6041
00290         MOVE ER-7008            TO EMI-ERROR                      EL6041
00291         PERFORM 9900-ERROR-FORMAT                                 EL6041
00292         MOVE -1                 TO MAINTL                         EL6041
00293         GO TO 8200-SEND-DATAONLY.                                 EL6041
00294                                                                   EL6041
00295      EXEC CICS RECEIVE                                            EL6041
00296          MAP      (MAP-NAME)                                      EL6041
00297          MAPSET   (MAPSET-NAME)                                   EL6041
00298          INTO     (EL6041AI)                                      EL6041
00299      END-EXEC.                                                    EL6041
00300                                                                   EL6041
00301      IF  PFKEYL = +0                                              EL6041
00302          GO TO 0300-CHECK-PFKEYS.                                 EL6041
00303                                                                   EL6041
00304      IF  EIBAID NOT = DFHENTER                                    EL6041
00305          MOVE ER-0004            TO EMI-ERROR                     EL6041
00306          GO TO 0320-INPUT-ERROR.                                  EL6041
00307                                                                   EL6041
00308      IF PFKEYI NOT NUMERIC                                        EL6041
00309         MOVE ER-0029     TO EMI-ERROR                             EL6041
00310         GO TO 0320-INPUT-ERROR.                                   EL6041
00311                                                                   EL6041
00312      IF PFKEYI GREATER 01 AND LESS 25                             EL6041
00313         MOVE PF-VALUES (PFKEYI) TO EIBAID                         EL6041
00314      ELSE                                                         EL6041
00315         MOVE ER-0029        TO EMI-ERROR                          EL6041
00316         GO TO 0320-INPUT-ERROR.                                   EL6041
00317                                                                   EL6041
00318  0300-CHECK-PFKEYS.                                               EL6041
00319      IF EIBAID = DFHPF23                                          EL6041
00320          GO TO 8810-PF23.                                         EL6041
00321                                                                   EL6041
00322      IF EIBAID = DFHPF24                                          EL6041
00323          GO TO 9200-RETURN-MAIN-MENU.                             EL6041
00324                                                                   EL6041
00325      IF EIBAID = DFHPF12                                          EL6041
00326          GO TO 9500-PF12.                                         EL6041
00327                                                                   EL6041
00328      IF MAINTL GREATER THAN +0 AND                                EL6041
00329               EIBAID NOT = DFHENTER                               EL6041
00330         MOVE -1             TO  MAINTL                            EL6041
00331         MOVE  ER-0050       TO  EMI-ERROR                         EL6041
00332         PERFORM 9900-ERROR-FORMAT                                 EL6041
00333         GO TO 8200-SEND-DATAONLY.                                 EL6041
00334                                                                   EL6041
00335      IF  EIBAID = DFHPF1                                          EL6041
00336          PERFORM 7000-BROWSE-FWRD-NEXT-ACCOUNT THRU 7090-EXIT     EL6041
00337          GO TO 4000-SHOW-REPORT.                                  EL6041
00338                                                                   EL6041
00339      IF  EIBAID = DFHPF2                                          EL6041
00340          PERFORM 7100-BROWSE-BWRD-NEXT-ACCOUNT THRU 7190-EXIT     EL6041
00341          GO TO 4000-SHOW-REPORT.                                  EL6041
00342                                                                   EL6041
00343      IF EIBAID = DFHENTER                                         EL6041
00344          GO TO 0400-EDIT-INPUT-DATA.                              EL6041
00345                                                                   EL6041
00346      MOVE ER-0029                TO EMI-ERROR.                    EL6041
00347                                                                   EL6041
00348  0320-INPUT-ERROR.                                                EL6041
00349      PERFORM 9900-ERROR-FORMAT.                                   EL6041
00350      MOVE AL-UNBON               TO PFKEYA.                       EL6041
00351      IF PFKEYL = 0                                                EL6041
00352          MOVE -1                 TO MAINTL                        EL6041
00353      ELSE                                                         EL6041
00354          MOVE -1                 TO PFKEYL.                       EL6041
00355                                                                   EL6041
00356      GO TO 8200-SEND-DATAONLY.                                    EL6041
00357                                                                   EL6041
00358      EJECT                                                        EL6041
00359                                                                   EL6041
00360  0400-EDIT-INPUT-DATA.                                            EL6041
00361      IF MAINTI = 'S'                                              EL6041
00362         GO TO 4000-SHOW-REPORT.                                   EL6041
00363                                                                   EL6041
00364      IF   NOT SYSTEM-MODIFY-CAP                                   EL6041
00365           MOVE 'UPDATE'       TO SM-READ                          EL6041
00366           PERFORM 9995-SECURITY-VIOLATION                         EL6041
00367           MOVE ER-0070        TO EMI-ERROR                        EL6041
00368           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                EL6041
00369           GO TO 8100-SEND-INITIAL-MAP.                            EL6041
00370                                                                   EL6041
00371      IF MAINTI = 'A'                                              EL6041
00372         GO TO 1000-ADD-REPORT.                                    EL6041
00373                                                                   EL6041
00374      IF MAINTI = 'C'                                              EL6041
00375         GO TO 2000-CHANGE-REPORT.                                 EL6041
00376                                                                   EL6041
00377      IF MAINTI = 'D'                                              EL6041
00378         GO TO 2500-DELETE-REPORT.                                 EL6041
00379                                                                   EL6041
00380      IF MAINTI = 'K'                                              EL6041
00381         GO TO 3000-COPY-REPORT.                                   EL6041
00382                                                                   EL6041
00383      MOVE  ER-0023            TO EMI-ERROR                        EL6041
00384      MOVE -1                  TO MAINTL                           EL6041
00385      MOVE AL-UABON            TO MAINTA                           EL6041
00386      PERFORM 9900-ERROR-FORMAT                                    EL6041
00387      GO TO 8200-SEND-DATAONLY.                                    EL6041
00388                                                                   EL6041
00389      EJECT                                                        EL6041
00390                                                                   EL6041
00391  1000-ADD-REPORT.                                                 EL6041
00392      IF REPORTL GREATER +0 AND                                    EL6041
00393         REPORTI NUMERIC                                           EL6041
00394         NEXT SENTENCE                                             EL6041
00395      ELSE                                                         EL6041
00396         MOVE ER-7680     TO EMI-ERROR                             EL6041
00397         MOVE -1          TO REPORTL                               EL6041
00398         MOVE AL-UNBON    TO REPORTA                               EL6041
00399         PERFORM 9900-ERROR-FORMAT                                 EL6041
00400         GO TO 8200-SEND-DATAONLY.                                 EL6041
00401                                                                   EL6041
00402      MOVE SPACES            TO ELCNTL-KEY.                        EL6041
00403      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.                 EL6041
00404      MOVE REPORTI           TO ELCNTL-REPORT.                     EL6041
00405      MOVE 'C'               TO ELCNTL-RECORD-TYPE.                EL6041
00406      MOVE +0002             TO ELCNTL-SEQ-NO.                     EL6041
00407                                                                   EL6041
00408      EXEC CICS HANDLE CONDITION                                   EL6041
00409           NOTFND     (1100-EDIT-REPORT-SCREEN)                    EL6041
00410      END-EXEC.                                                    EL6041
00411                                                                   EL6041
00412      EXEC CICS READ                                               EL6041
00413           DATASET    (ELCNTL-FILE-ID)                             EL6041
00414           RIDFLD     (ELCNTL-KEY)                                 EL6041
00415           SET        (ADDRESS OF CONTROL-FILE)                       CL**4
00416      END-EXEC.                                                    EL6041
00417                                                                   EL6041
00418      MOVE -1                     TO REPORTL.                      EL6041
00419      MOVE  ER-0132               TO  EMI-ERROR.                   EL6041
00420      PERFORM 9900-ERROR-FORMAT.                                   EL6041
00421      GO TO 8200-SEND-DATAONLY.                                    EL6041
00422                                                                   EL6041
00423  1100-EDIT-REPORT-SCREEN.                                         EL6041
00424      IF LTONEYL GREATER +0                                        EL6041
00425         IF LTONEYI = 'Y' OR 'N'                                   EL6041
00426            NEXT SENTENCE                                          EL6041
00427         ELSE                                                      EL6041
00428            MOVE ER-2001            TO EMI-ERROR                      CL**2
00429            MOVE -1                 TO LTONEYL                     EL6041
00430            MOVE AL-UABON           TO LTONEYA                     EL6041
00431            PERFORM 9900-ERROR-FORMAT.                                CL**2
00432                                                                      CL**2
00433      IF ZERMPDL GREATER +0                                           CL**2
00434         IF ZERMPDI = 'A' OR 'B' OR 'C'                               CL**2
00435            NEXT SENTENCE                                             CL**2
00436         ELSE                                                         CL**2
00437            MOVE ER-7354            TO EMI-ERROR                      CL**2
00438            MOVE -1                 TO ZERMPDL                        CL**2
00439            MOVE AL-UABON           TO ZERMPDA                        CL**2
00440            PERFORM 9900-ERROR-FORMAT.                             EL6041
00441                                                                   EL6041
00442      IF ISSCNTL GREATER THAN +0                                   EL6041
00443          MOVE ISSCNTI TO DEEDIT-FIELD                             EL6041
00444          PERFORM 8600-DEEDIT                                      EL6041
00445          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00446              MOVE -1 TO ISSCNTL                                   EL6041
00447              MOVE AL-UNBON TO ISSCNTA                             EL6041
00448              MOVE ER-2223 TO EMI-ERROR                               CL**2
00449              PERFORM 9900-ERROR-FORMAT                            EL6041
00450          ELSE                                                     EL6041
00451              MOVE DEEDIT-FIELD-V0 TO WS-ISS-COUNT-DIFF.           EL6041
00452                                                                   EL6041
00453      IF SMPRMPL GREATER THAN +0                                   EL6041
00454          MOVE SMPRMPI TO DEEDIT-FIELD                             EL6041
00455          PERFORM 8600-DEEDIT                                      EL6041
00456          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00457              MOVE -1 TO SMPRMPL                                   EL6041
00458              MOVE AL-UNBON TO SMPRMPA                             EL6041
00459              MOVE ER-2223 TO EMI-ERROR                               CL**2
00460              PERFORM 9900-ERROR-FORMAT                            EL6041
00461          ELSE                                                     EL6041
00462              MOVE DEEDIT-FIELD-V0 TO WS-SINGLE-MO-PREM-PCT.       EL6041
00463                                                                   EL6041
00464      IF EPRMDCL GREATER THAN +0                                   EL6041
00465          MOVE EPRMDCI TO DEEDIT-FIELD                             EL6041
00466          PERFORM 8600-DEEDIT                                      EL6041
00467          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00468              MOVE -1 TO EPRMDCL                                   EL6041
00469              MOVE AL-UNBON TO EPRMDCA                             EL6041
00470              MOVE ER-2223 TO EMI-ERROR                               CL**2
00471              PERFORM 9900-ERROR-FORMAT                            EL6041
00472          ELSE                                                     EL6041
00473              MOVE DEEDIT-FIELD-V0 TO WS-EARN-PREM-DECLINE.        EL6041
00474                                                                   EL6041
00475      IF CNCRATL GREATER THAN +0                                   EL6041
00476          MOVE CNCRATI TO DEEDIT-FIELD                             EL6041
00477          PERFORM 8600-DEEDIT                                      EL6041
00478          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00479              MOVE -1 TO CNCRATL                                   EL6041
00480              MOVE AL-UNBON TO CNCRATA                             EL6041
00481              MOVE ER-2223 TO EMI-ERROR                               CL**2
00482              PERFORM 9900-ERROR-FORMAT                            EL6041
00483          ELSE                                                     EL6041
00484              MOVE DEEDIT-FIELD-V0 TO WS-CANCELLATION-RATIO.       EL6041
00485                                                                   EL6041
00486      IF RETLIML GREATER THAN +0                                      CL**3
00487          MOVE RETLIMI TO DEEDIT-FIELD                                CL**3
00488          PERFORM 8600-DEEDIT                                         CL**3
00489          IF DEEDIT-FIELD-V0  NOT NUMERIC                             CL**3
00490              MOVE -1 TO RETLIML                                      CL**3
00491              MOVE AL-UNBON TO RETLIMA                                CL**3
00492              MOVE ER-2223 TO EMI-ERROR                               CL**3
00493              PERFORM 9900-ERROR-FORMAT                               CL**3
00494          ELSE                                                        CL**3
00495              MOVE DEEDIT-FIELD-V0 TO WS-RETENTION-LIMIT.             CL**3
00496                                                                      CL**3
00497      IF LLSRATL GREATER THAN +0                                   EL6041
00498          MOVE LLSRATI TO DEEDIT-FIELD                             EL6041
00499          PERFORM 8600-DEEDIT                                      EL6041
00500          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00501              MOVE -1 TO LLSRATL                                   EL6041
00502              MOVE AL-UNBON TO LLSRATA                             EL6041
00503              MOVE ER-2223 TO EMI-ERROR                               CL**2
00504              PERFORM 9900-ERROR-FORMAT                            EL6041
00505          ELSE                                                     EL6041
00506              MOVE DEEDIT-FIELD-V0 TO WS-LF-LOSS-RATIO-PCT.        EL6041
00507                                                                   EL6041
00508      IF LLTLRSL GREATER THAN +0                                   EL6041
00509          MOVE LLTLRSI TO DEEDIT-FIELD                             EL6041
00510          PERFORM 8600-DEEDIT                                      EL6041
00511          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00512              MOVE -1 TO LLTLRSL                                   EL6041
00513              MOVE AL-UNBON TO LLTLRSA                             EL6041
00514              MOVE ER-2223 TO EMI-ERROR                               CL**2
00515              PERFORM 9900-ERROR-FORMAT                            EL6041
00516          ELSE                                                     EL6041
00517              MOVE DEEDIT-FIELD-V0 TO WS-LF-LTM-LOSS-RATIO.        EL6041
00518                                                                   EL6041
00519      IF LPDPROL GREATER THAN +0                                   EL6041
00520          MOVE LPDPROI TO DEEDIT-FIELD                             EL6041
00521          PERFORM 8600-DEEDIT                                      EL6041
00522          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00523              MOVE -1 TO LPDPROL                                   EL6041
00524              MOVE AL-UNBON TO LPDPROA                             EL6041
00525              MOVE ER-2223 TO EMI-ERROR                               CL**2
00526              PERFORM 9900-ERROR-FORMAT                            EL6041
00527          ELSE                                                     EL6041
00528              MOVE DEEDIT-FIELD-V0 TO WS-LF-PERIOD-PROFIT.         EL6041
00529                                                                   EL6041
00530      IF LLTPRPL GREATER THAN +0                                   EL6041
00531          MOVE LLTPRPI TO DEEDIT-FIELD                             EL6041
00532          PERFORM 8600-DEEDIT                                      EL6041
00533          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00534              MOVE -1 TO LLTPRPL                                   EL6041
00535              MOVE AL-UNBON TO LLTPRPA                             EL6041
00536              MOVE ER-2223 TO EMI-ERROR                               CL**2
00537              PERFORM 9900-ERROR-FORMAT                            EL6041
00538          ELSE                                                     EL6041
00539              MOVE DEEDIT-FIELD-V1 TO WS-LF-LTM-PROFIT-PCT.        EL6041
00540                                                                   EL6041
00541      IF LLTIDCL GREATER THAN +0                                   EL6041
00542          MOVE LLTIDCI TO DEEDIT-FIELD                             EL6041
00543          PERFORM 8600-DEEDIT                                      EL6041
00544          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00545              MOVE -1 TO LLTIDCL                                   EL6041
00546              MOVE AL-UNBON TO LLTIDCA                             EL6041
00547              MOVE ER-2223 TO EMI-ERROR                               CL**2
00548              PERFORM 9900-ERROR-FORMAT                            EL6041
00549          ELSE                                                     EL6041
00550              MOVE DEEDIT-FIELD-V1 TO WS-LF-LTM-INFORCE-DECR.      EL6041
00551                                                                   EL6041
00552      IF LLTTRML GREATER THAN +0                                   EL6041
00553          MOVE LLTTRMI TO DEEDIT-FIELD                             EL6041
00554          PERFORM 8600-DEEDIT                                      EL6041
00555          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00556              MOVE -1 TO LLTTRML                                   EL6041
00557              MOVE AL-UNBON TO LLTTRMA                             EL6041
00558              MOVE ER-2223 TO EMI-ERROR                               CL**2
00559              PERFORM 9900-ERROR-FORMAT                            EL6041
00560          ELSE                                                     EL6041
00561              MOVE DEEDIT-FIELD-V1 TO WS-LF-LTM-TERM-CHG.          EL6041
00562                                                                   EL6041
00563      IF LTRMAVL GREATER THAN +0                                   EL6041
00564          MOVE LTRMAVI TO DEEDIT-FIELD                             EL6041
00565          PERFORM 8600-DEEDIT                                      EL6041
00566          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00567              MOVE -1 TO LTRMAVL                                   EL6041
00568              MOVE AL-UNBON TO LTRMAVA                             EL6041
00569              MOVE ER-2223 TO EMI-ERROR                               CL**2
00570              PERFORM 9900-ERROR-FORMAT                            EL6041
00571          ELSE                                                     EL6041
00572              MOVE DEEDIT-FIELD-V1 TO WS-LF-TERM-AVG-WEIGHTED.     EL6041
00573                                                                   EL6041
00574      IF LLTAGPL GREATER THAN +0                                   EL6041
00575          MOVE LLTAGPI TO DEEDIT-FIELD                             EL6041
00576          PERFORM 8600-DEEDIT                                      EL6041
00577          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00578              MOVE -1 TO LLTAGPL                                   EL6041
00579              MOVE AL-UNBON TO LLTAGPA                             EL6041
00580              MOVE ER-2223 TO EMI-ERROR                               CL**2
00581              PERFORM 9900-ERROR-FORMAT                            EL6041
00582          ELSE                                                     EL6041
00583              MOVE DEEDIT-FIELD-V1 TO WS-LF-LTM-AGE-PCT.           EL6041
00584                                                                   EL6041
00585      IF LAVAGWL GREATER THAN +0                                   EL6041
00586          MOVE LAVAGWI TO DEEDIT-FIELD                             EL6041
00587          PERFORM 8600-DEEDIT                                      EL6041
00588          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00589              MOVE -1 TO LAVAGWL                                   EL6041
00590              MOVE AL-UNBON TO LAVAGWA                             EL6041
00591              MOVE ER-2223 TO EMI-ERROR                               CL**2
00592              PERFORM 9900-ERROR-FORMAT                            EL6041
00593          ELSE                                                     EL6041
00594              MOVE DEEDIT-FIELD-V1 TO WS-LF-AGE-AVG-WEIGHTED.      EL6041
00595                                                                   EL6041
00596      IF LAVAGML GREATER THAN +0                                   EL6041
00597          MOVE LAVAGMI TO DEEDIT-FIELD                             EL6041
00598          PERFORM 8600-DEEDIT                                      EL6041
00599          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00600              MOVE -1 TO LAVAGML                                   EL6041
00601              MOVE AL-UNBON TO LAVAGMA                             EL6041
00602              MOVE ER-2223 TO EMI-ERROR                               CL**2
00603              PERFORM 9900-ERROR-FORMAT                            EL6041
00604          ELSE                                                     EL6041
00605              MOVE DEEDIT-FIELD-V0 TO WS-LF-AVG-AGE-MAX.           EL6041
00606                                                                   EL6041
00607      IF ALSRATL GREATER THAN +0                                   EL6041
00608          MOVE ALSRATI TO DEEDIT-FIELD                             EL6041
00609          PERFORM 8600-DEEDIT                                      EL6041
00610          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00611              MOVE -1 TO ALSRATL                                   EL6041
00612              MOVE AL-UNBON TO ALSRATA                             EL6041
00613              MOVE ER-2223 TO EMI-ERROR                               CL**2
00614              PERFORM 9900-ERROR-FORMAT                            EL6041
00615          ELSE                                                     EL6041
00616              MOVE DEEDIT-FIELD-V0 TO WS-AH-LOSS-RATIO-PCT.        EL6041
00617                                                                   EL6041
00618      IF ALTLRSL GREATER THAN +0                                   EL6041
00619          MOVE ALTLRSI TO DEEDIT-FIELD                             EL6041
00620          PERFORM 8600-DEEDIT                                      EL6041
00621          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00622              MOVE -1 TO ALTLRSL                                   EL6041
00623              MOVE AL-UNBON TO ALTLRSA                             EL6041
00624              MOVE ER-2223 TO EMI-ERROR                               CL**2
00625              PERFORM 9900-ERROR-FORMAT                            EL6041
00626          ELSE                                                     EL6041
00627              MOVE DEEDIT-FIELD-V0 TO WS-AH-LTM-LOSS-RATIO.        EL6041
00628                                                                   EL6041
00629      IF APDPROL GREATER THAN +0                                   EL6041
00630          MOVE APDPROI TO DEEDIT-FIELD                             EL6041
00631          PERFORM 8600-DEEDIT                                      EL6041
00632          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00633              MOVE -1 TO APDPROL                                   EL6041
00634              MOVE AL-UNBON TO APDPROA                             EL6041
00635              MOVE ER-2223 TO EMI-ERROR                               CL**2
00636              PERFORM 9900-ERROR-FORMAT                            EL6041
00637          ELSE                                                     EL6041
00638              MOVE DEEDIT-FIELD-V0 TO WS-AH-PERIOD-PROFIT.         EL6041
00639                                                                   EL6041
00640      IF ALTPRPL GREATER THAN +0                                   EL6041
00641          MOVE ALTPRPI TO DEEDIT-FIELD                             EL6041
00642          PERFORM 8600-DEEDIT                                      EL6041
00643          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00644              MOVE -1 TO ALTPRPL                                   EL6041
00645              MOVE AL-UNBON TO ALTPRPA                             EL6041
00646              MOVE ER-2223 TO EMI-ERROR                               CL**2
00647              PERFORM 9900-ERROR-FORMAT                            EL6041
00648          ELSE                                                     EL6041
00649              MOVE DEEDIT-FIELD-V1 TO WS-AH-LTM-PROFIT-PCT.        EL6041
00650                                                                   EL6041
00651      IF ALTIDCL GREATER THAN +0                                   EL6041
00652          MOVE ALTIDCI TO DEEDIT-FIELD                             EL6041
00653          PERFORM 8600-DEEDIT                                      EL6041
00654          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00655              MOVE -1 TO ALTIDCL                                   EL6041
00656              MOVE AL-UNBON TO ALTIDCA                             EL6041
00657              MOVE ER-2223 TO EMI-ERROR                               CL**2
00658              PERFORM 9900-ERROR-FORMAT                            EL6041
00659          ELSE                                                     EL6041
00660              MOVE DEEDIT-FIELD-V1 TO WS-AH-LTM-INFORCE-DECR.      EL6041
00661                                                                   EL6041
00662      IF ALTTRML GREATER THAN +0                                   EL6041
00663          MOVE ALTTRMI TO DEEDIT-FIELD                             EL6041
00664          PERFORM 8600-DEEDIT                                      EL6041
00665          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00666              MOVE -1 TO ALTTRML                                   EL6041
00667              MOVE AL-UNBON TO ALTTRMA                             EL6041
00668              MOVE ER-2223 TO EMI-ERROR                               CL**2
00669              PERFORM 9900-ERROR-FORMAT                            EL6041
00670          ELSE                                                     EL6041
00671              MOVE DEEDIT-FIELD-V1 TO WS-AH-LTM-TERM-CHG.          EL6041
00672                                                                   EL6041
00673      IF ATRMAVL GREATER THAN +0                                   EL6041
00674          MOVE ATRMAVI TO DEEDIT-FIELD                             EL6041
00675          PERFORM 8600-DEEDIT                                      EL6041
00676          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00677              MOVE -1 TO ATRMAVL                                   EL6041
00678              MOVE AL-UNBON TO ATRMAVA                             EL6041
00679              MOVE ER-2223 TO EMI-ERROR                               CL**2
00680              PERFORM 9900-ERROR-FORMAT                            EL6041
00681          ELSE                                                     EL6041
00682              MOVE DEEDIT-FIELD-V1 TO WS-AH-TERM-AVG-WEIGHTED.     EL6041
00683                                                                   EL6041
00684      IF ALTAGPL GREATER THAN +0                                   EL6041
00685          MOVE ALTAGPI TO DEEDIT-FIELD                             EL6041
00686          PERFORM 8600-DEEDIT                                      EL6041
00687          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00688              MOVE -1 TO ALTAGPL                                   EL6041
00689              MOVE AL-UNBON TO ALTAGPA                             EL6041
00690              MOVE ER-2223 TO EMI-ERROR                               CL**2
00691              PERFORM 9900-ERROR-FORMAT                            EL6041
00692          ELSE                                                     EL6041
00693              MOVE DEEDIT-FIELD-V1 TO WS-AH-LTM-AGE-PCT.           EL6041
00694                                                                   EL6041
00695      IF AAVAGWL GREATER THAN +0                                   EL6041
00696          MOVE AAVAGWI TO DEEDIT-FIELD                             EL6041
00697          PERFORM 8600-DEEDIT                                      EL6041
00698          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00699              MOVE -1 TO AAVAGWL                                   EL6041
00700              MOVE AL-UNBON TO AAVAGWA                             EL6041
00701              MOVE ER-2223 TO EMI-ERROR                               CL**2
00702              PERFORM 9900-ERROR-FORMAT                            EL6041
00703          ELSE                                                     EL6041
00704              MOVE DEEDIT-FIELD-V1 TO WS-AH-AGE-AVG-WEIGHTED.      EL6041
00705                                                                   EL6041
00706      IF AAVAGML GREATER THAN +0                                   EL6041
00707          MOVE AAVAGMI TO DEEDIT-FIELD                             EL6041
00708          PERFORM 8600-DEEDIT                                      EL6041
00709          IF DEEDIT-FIELD-V0  NOT NUMERIC                          EL6041
00710              MOVE -1 TO AAVAGML                                   EL6041
00711              MOVE AL-UNBON TO AAVAGMA                             EL6041
00712              MOVE ER-2223 TO EMI-ERROR                               CL**2
00713              PERFORM 9900-ERROR-FORMAT                            EL6041
00714          ELSE                                                     EL6041
00715              MOVE DEEDIT-FIELD-V0 TO WS-AH-AVG-AGE-MAX.           EL6041
00716                                                                   EL6041
00717      IF NOT EMI-NO-ERRORS                                         EL6041
00718         GO TO 8200-SEND-DATAONLY.                                 EL6041
00719                                                                   EL6041
00720  1200-ADD-REPORT-RECORD.                                          EL6041
00721     EXEC CICS GETMAIN                                             EL6041
00722          SET       (ADDRESS OF CONTROL-FILE)                         CL**4
00723          LENGTH    (ELCNTL-LENGTH)                                EL6041
00724          INITIMG   (GETMAIN-SPACE)                                EL6041
00725      END-EXEC.                                                    EL6041
00726                                                                   EL6041
00727      MOVE 'N'                   TO CF-ACCOUNTS-LT-ONE-YEAR.       EL6041
00728                                                                   EL6041
00729      MOVE ZEROS                 TO CF-ISS-COUNT-DIFF              EL6041
00730                                    CF-SINGLE-MO-PREM-PCT          EL6041
00731                                    CF-EARN-PREM-DECR-PCT          EL6041
00732                                    CF-CANCELLATION-RATIO          EL6041
00733                                    CF-RETENTION-LIMIT                CL**3
00734                                    CF-LF-LOSS-RATIO-PCT           EL6041
00735                                    CF-LF-LTM-LOSS-RATIO           EL6041
00736                                    CF-LF-PERIOD-PROFIT            EL6041
00737                                    CF-LF-LTM-PROFIT-PCT           EL6041
00738                                    CF-LF-LTM-INFORCE-DECR         EL6041
00739                                    CF-LF-LTM-TERM-CHG             EL6041
00740                                    CF-LF-TERM-AVG-WEIGHTED        EL6041
00741                                    CF-LF-LTM-AGE-PCT              EL6041
00742                                    CF-LF-AGE-AVG-WEIGHTED         EL6041
00743                                    CF-LF-AVG-AGE-MAX              EL6041
00744                                    CF-AH-LOSS-RATIO-PCT           EL6041
00745                                    CF-AH-LTM-LOSS-RATIO           EL6041
00746                                    CF-AH-PERIOD-PROFIT            EL6041
00747                                    CF-AH-LTM-PROFIT-PCT           EL6041
00748                                    CF-AH-LTM-INFORCE-DECR         EL6041
00749                                    CF-AH-LTM-TERM-CHG             EL6041
00750                                    CF-AH-TERM-AVG-WEIGHTED        EL6041
00751                                    CF-AH-LTM-AGE-PCT              EL6041
00752                                    CF-AH-AGE-AVG-WEIGHTED         EL6041
00753                                    CF-AH-AVG-AGE-MAX.             EL6041
00754                                                                   EL6041
00755  1500-BUILD-RECORD.                                               EL6041
00756                                                                   EL6041
00757      MOVE 'CF'                  TO CF-RECORD-ID                   EL6041
00758      MOVE PI-COMPANY-ID         TO CF-COMPANY-ID                  EL6041
00759      MOVE 'C'                   TO CF-RECORD-TYPE                 EL6041
00760      MOVE REPORTI               TO CF-CUSTOM-REPORT-NO            EL6041
00761      MOVE +0002                 TO CF-SEQUENCE-NO.                EL6041
00762                                                                   EL6041
00763      MOVE SAVE-BIN-DATE         TO CF-LAST-MAINT-DT               EL6041
00764      MOVE PI-PROCESSOR-ID       TO CF-LAST-MAINT-BY               EL6041
00765      MOVE EIBTIME               TO CF-LAST-MAINT-HHMMSS.          EL6041
00766                                                                   EL6041
00767      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT          EL6041
00768                                           REPORTO.                EL6041
00769                                                                   EL6041
00770      IF CF-ISS-COUNT-DIFF NOT NUMERIC                             EL6041
00771           MOVE ZEROS TO CF-ISS-COUNT-DIFF.                        EL6041
00772                                                                   EL6041
00773      IF CF-SINGLE-MO-PREM-PCT NOT NUMERIC                         EL6041
00774          MOVE ZEROS TO CF-SINGLE-MO-PREM-PCT.                     EL6041
00775                                                                   EL6041
00776      IF CF-EARN-PREM-DECR-PCT NOT NUMERIC                         EL6041
00777          MOVE ZEROS TO CF-SINGLE-MO-PREM-PCT.                     EL6041
00778                                                                   EL6041
00779      IF CF-CANCELLATION-RATIO NOT NUMERIC                         EL6041
00780          MOVE ZEROS TO CF-CANCELLATION-RATIO.                     EL6041
00781                                                                      CL**3
00782      IF CF-RETENTION-LIMIT NOT NUMERIC                               CL**3
00783           MOVE ZEROS TO CF-RETENTION-LIMIT.                          CL**3
00784                                                                   EL6041
00785      IF CF-LF-LOSS-RATIO-PCT NOT NUMERIC                          EL6041
00786          MOVE ZEROS TO CF-LF-LOSS-RATIO-PCT.                      EL6041
00787                                                                   EL6041
00788      IF CF-LF-LTM-LOSS-RATIO NOT NUMERIC                          EL6041
00789          MOVE ZEROS TO CF-LF-LTM-LOSS-RATIO.                      EL6041
00790                                                                   EL6041
00791      IF CF-LF-PERIOD-PROFIT NOT NUMERIC                           EL6041
00792          MOVE ZEROS TO CF-LF-PERIOD-PROFIT.                       EL6041
00793                                                                   EL6041
00794      IF CF-LF-LTM-PROFIT-PCT NOT NUMERIC                          EL6041
00795          MOVE ZEROS TO CF-LF-LTM-PROFIT-PCT.                      EL6041
00796                                                                   EL6041
00797      IF CF-LF-LTM-INFORCE-DECR NOT NUMERIC                        EL6041
00798          MOVE ZEROS TO CF-LF-LTM-INFORCE-DECR.                    EL6041
00799                                                                   EL6041
00800      IF CF-LF-LTM-TERM-CHG NOT NUMERIC                            EL6041
00801          MOVE ZEROS TO CF-LF-LTM-TERM-CHG.                        EL6041
00802                                                                   EL6041
00803      IF CF-LF-TERM-AVG-WEIGHTED NOT NUMERIC                       EL6041
00804          MOVE ZEROS TO CF-LF-TERM-AVG-WEIGHTED.                   EL6041
00805                                                                   EL6041
00806      IF CF-LF-LTM-AGE-PCT NOT NUMERIC                             EL6041
00807          MOVE ZEROS TO CF-LF-LTM-AGE-PCT.                         EL6041
00808                                                                   EL6041
00809      IF CF-LF-AGE-AVG-WEIGHTED NOT NUMERIC                        EL6041
00810          MOVE ZEROS TO CF-LF-AGE-AVG-WEIGHTED.                    EL6041
00811                                                                   EL6041
00812      IF CF-LF-AVG-AGE-MAX NOT NUMERIC                             EL6041
00813          MOVE ZEROS TO CF-LF-AVG-AGE-MAX.                         EL6041
00814                                                                   EL6041
00815      IF CF-AH-LOSS-RATIO-PCT NOT NUMERIC                          EL6041
00816          MOVE ZEROS TO CF-AH-LOSS-RATIO-PCT.                      EL6041
00817                                                                   EL6041
00818      IF CF-AH-LTM-LOSS-RATIO NOT NUMERIC                          EL6041
00819          MOVE ZEROS TO CF-AH-LTM-LOSS-RATIO.                      EL6041
00820                                                                   EL6041
00821      IF CF-AH-PERIOD-PROFIT NOT NUMERIC                           EL6041
00822          MOVE ZEROS TO CF-AH-PERIOD-PROFIT.                       EL6041
00823                                                                   EL6041
00824      IF CF-AH-LTM-PROFIT-PCT NOT NUMERIC                          EL6041
00825          MOVE ZEROS TO CF-AH-LTM-PROFIT-PCT.                      EL6041
00826                                                                   EL6041
00827      IF CF-AH-LTM-INFORCE-DECR NOT NUMERIC                        EL6041
00828          MOVE ZEROS TO CF-AH-LTM-INFORCE-DECR.                    EL6041
00829                                                                   EL6041
00830      IF CF-AH-LTM-TERM-CHG NOT NUMERIC                            EL6041
00831          MOVE ZEROS TO CF-AH-LTM-TERM-CHG.                        EL6041
00832                                                                   EL6041
00833      IF CF-AH-TERM-AVG-WEIGHTED NOT NUMERIC                       EL6041
00834          MOVE ZEROS TO CF-AH-TERM-AVG-WEIGHTED.                   EL6041
00835                                                                   EL6041
00836      IF CF-AH-LTM-AGE-PCT NOT NUMERIC                             EL6041
00837          MOVE ZEROS TO CF-AH-LTM-AGE-PCT.                         EL6041
00838                                                                   EL6041
00839      IF CF-AH-AGE-AVG-WEIGHTED NOT NUMERIC                        EL6041
00840          MOVE ZEROS TO CF-AH-AGE-AVG-WEIGHTED.                    EL6041
00841                                                                   EL6041
00842      IF CF-AH-AVG-AGE-MAX NOT NUMERIC                             EL6041
00843          MOVE ZEROS TO CF-AH-AVG-AGE-MAX.                         EL6041
00844                                                                   EL6041
00845      IF LTONEYL GREATER THAN +0                                   EL6041
00846          MOVE LTONEYI TO  CF-ACCOUNTS-LT-ONE-YEAR.                EL6041
00847                                                                      CL**2
00848      IF ZERMPDL GREATER THAN +0                                      CL**2
00849          MOVE ZERMPDI TO  CF-ACCT-ZERO-MONTH-PRODUCTION.             CL**2
00850                                                                   EL6041
00851      IF ISSCNTL GREATER THAN +0                                   EL6041
00852          MOVE WS-ISS-COUNT-DIFF TO  CF-ISS-COUNT-DIFF.            EL6041
00853                                                                   EL6041
00854      IF SMPRMPL GREATER THAN +0                                   EL6041
00855          MOVE WS-SINGLE-MO-PREM-PCT TO  CF-SINGLE-MO-PREM-PCT.    EL6041
00856                                                                   EL6041
00857      IF EPRMDCL GREATER THAN +0                                   EL6041
00858          MOVE WS-EARN-PREM-DECLINE TO  CF-EARN-PREM-DECR-PCT.     EL6041
00859                                                                   EL6041
00860      IF CNCRATL GREATER THAN +0                                   EL6041
00861          MOVE WS-CANCELLATION-RATIO TO  CF-CANCELLATION-RATIO.    EL6041
00862                                                                      CL**3
00863      IF RETLIML GREATER THAN +0                                      CL**3
00864          MOVE WS-RETENTION-LIMIT TO CF-RETENTION-LIMIT.              CL**3
00865                                                                   EL6041
00866      IF LLSRATL GREATER THAN +0                                   EL6041
00867          MOVE WS-LF-LOSS-RATIO-PCT TO  CF-LF-LOSS-RATIO-PCT.      EL6041
00868                                                                   EL6041
00869      IF LLTLRSL GREATER THAN +0                                   EL6041
00870          MOVE WS-LF-LTM-LOSS-RATIO TO  CF-LF-LTM-LOSS-RATIO.      EL6041
00871                                                                   EL6041
00872      IF LPDPROL GREATER THAN +0                                   EL6041
00873          MOVE WS-LF-PERIOD-PROFIT TO  CF-LF-PERIOD-PROFIT.        EL6041
00874                                                                   EL6041
00875      IF LLTPRPL GREATER THAN +0                                   EL6041
00876          MOVE WS-LF-LTM-PROFIT-PCT TO  CF-LF-LTM-PROFIT-PCT.      EL6041
00877                                                                   EL6041
00878      IF LLTIDCL GREATER THAN +0                                   EL6041
00879          MOVE WS-LF-LTM-INFORCE-DECR TO CF-LF-LTM-INFORCE-DECR.   EL6041
00880                                                                   EL6041
00881      IF LLTTRML GREATER THAN +0                                   EL6041
00882          MOVE WS-LF-LTM-TERM-CHG TO  CF-LF-LTM-TERM-CHG.          EL6041
00883                                                                   EL6041
00884      IF LTRMAVL GREATER THAN +0                                   EL6041
00885          MOVE WS-LF-TERM-AVG-WEIGHTED                             EL6041
00886                                  TO  CF-LF-TERM-AVG-WEIGHTED.     EL6041
00887                                                                   EL6041
00888      IF LLTAGPL GREATER THAN +0                                   EL6041
00889          MOVE WS-LF-LTM-AGE-PCT TO  CF-LF-LTM-AGE-PCT.            EL6041
00890                                                                   EL6041
00891      IF LAVAGWL GREATER THAN +0                                   EL6041
00892          MOVE WS-LF-AGE-AVG-WEIGHTED TO  CF-LF-AGE-AVG-WEIGHTED.  EL6041
00893                                                                   EL6041
00894      IF LAVAGML GREATER THAN +0                                   EL6041
00895          MOVE WS-LF-AVG-AGE-MAX TO  CF-LF-AVG-AGE-MAX.            EL6041
00896                                                                   EL6041
00897      IF ALSRATL GREATER THAN +0                                   EL6041
00898          MOVE WS-AH-LOSS-RATIO-PCT TO  CF-AH-LOSS-RATIO-PCT.      EL6041
00899                                                                   EL6041
00900      IF ALTLRSL GREATER THAN +0                                   EL6041
00901          MOVE WS-AH-LTM-LOSS-RATIO TO  CF-AH-LTM-LOSS-RATIO.      EL6041
00902                                                                   EL6041
00903      IF APDPROL GREATER THAN +0                                   EL6041
00904          MOVE WS-AH-PERIOD-PROFIT TO  CF-AH-PERIOD-PROFIT.        EL6041
00905                                                                   EL6041
00906      IF ALTPRPL GREATER THAN +0                                   EL6041
00907          MOVE WS-AH-LTM-PROFIT-PCT TO  CF-AH-LTM-PROFIT-PCT.      EL6041
00908                                                                   EL6041
00909      IF ALTIDCL GREATER THAN +0                                   EL6041
00910          MOVE WS-AH-LTM-INFORCE-DECR TO  CF-AH-LTM-INFORCE-DECR.  EL6041
00911                                                                   EL6041
00912      IF ALTTRML GREATER THAN +0                                   EL6041
00913          MOVE WS-AH-LTM-TERM-CHG TO  CF-AH-LTM-TERM-CHG.          EL6041
00914                                                                   EL6041
00915      IF ATRMAVL GREATER THAN +0                                   EL6041
00916          MOVE WS-AH-TERM-AVG-WEIGHTED                             EL6041
00917                                  TO  CF-AH-TERM-AVG-WEIGHTED.     EL6041
00918                                                                   EL6041
00919      IF ALTAGPL GREATER THAN +0                                   EL6041
00920          MOVE WS-AH-LTM-AGE-PCT TO  CF-AH-LTM-AGE-PCT.            EL6041
00921                                                                   EL6041
00922      IF AAVAGWL GREATER THAN +0                                   EL6041
00923          MOVE WS-AH-AGE-AVG-WEIGHTED                              EL6041
00924                                  TO  CF-AH-AGE-AVG-WEIGHTED.      EL6041
00925                                                                   EL6041
00926      IF AAVAGML GREATER THAN +0                                   EL6041
00927          MOVE WS-AH-AVG-AGE-MAX TO  CF-AH-AVG-AGE-MAX.            EL6041
00928                                                                   EL6041
00929      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.            EL6041
00930      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.        EL6041
00931                                                                   EL6041
00932  1600-WRITE-REPORT-RECORD.                                        EL6041
00933                                                                   EL6041
00934      EXEC CICS WRITE                                              EL6041
00935           DATASET(ELCNTL-FILE-ID)                                 EL6041
00936           FROM   (CONTROL-FILE)                                   EL6041
00937           RIDFLD (ELCNTL-KEY)                                     EL6041
00938      END-EXEC.                                                    EL6041
00939                                                                   EL6041
00940      MOVE ER-0000      TO EMI-ERROR.                              EL6041
00941      PERFORM 9900-ERROR-FORMAT.                                   EL6041
00942      GO TO 4000-SHOW-REPORT.                                      EL6041
00943                                                                   EL6041
00944  2000-CHANGE-REPORT.                                              EL6041
00945      IF REPORTI NOT NUMERIC                                       EL6041
00946         MOVE ER-7680            TO EMI-ERROR                      EL6041
00947         MOVE -1                 TO REPORTL                        EL6041
00948         MOVE AL-UNBON           TO REPORTA                        EL6041
00949         PERFORM 9900-ERROR-FORMAT                                 EL6041
00950         GO TO 8200-SEND-DATAONLY.                                 EL6041
00951                                                                   EL6041
00952      IF REPORTI NOT = PI-LAST-REPORT                              EL6041
00953         MOVE ER-0138            TO EMI-ERROR                      EL6041
00954         MOVE -1                 TO REPORTL                        EL6041
00955         MOVE AL-UNBON           TO REPORTA                        EL6041
00956         PERFORM 9900-ERROR-FORMAT                                 EL6041
00957         GO TO 8200-SEND-DATAONLY.                                 EL6041
00958                                                                   EL6041
00959      MOVE SPACES                 TO ELCNTL-KEY                    EL6041
00960      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            EL6041
00961      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.           EL6041
00962      MOVE REPORTI                TO ELCNTL-REPORT.                EL6041
00963      MOVE +0002                  TO ELCNTL-SEQ-NO.                EL6041
00964                                                                   EL6041
00965      EXEC CICS HANDLE CONDITION                                   EL6041
00966           NOTFND     (2100-NOT-FOUND)                             EL6041
00967      END-EXEC.                                                    EL6041
00968                                                                   EL6041
00969      EXEC CICS READ                                               EL6041
00970           DATASET    (ELCNTL-FILE-ID)                             EL6041
00971           SET        (ADDRESS OF CONTROL-FILE)                       CL**4
00972           RIDFLD     (ELCNTL-KEY)                                 EL6041
00973      END-EXEC.                                                    EL6041
00974                                                                   EL6041
00975      IF (CF-LAST-MAINT-BY NOT = PI-UPDATE-BY) OR                  EL6041
00976         (CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS)             EL6041
00977         MOVE ER-0068          TO EMI-ERROR                        EL6041
00978         MOVE -1               TO MAINTL                           EL6041
00979         PERFORM 9900-ERROR-FORMAT                                 EL6041
00980         GO TO 8200-SEND-DATAONLY.                                 EL6041
00981                                                                   EL6041
00982      PERFORM 1100-EDIT-REPORT-SCREEN.                             EL6041
00983                                                                   EL6041
00984      IF NOT EMI-NO-ERRORS                                         EL6041
00985         GO TO 8200-SEND-DATAONLY.                                 EL6041
00986                                                                   EL6041
00987      EXEC CICS READ                                               EL6041
00988           DATASET    (ELCNTL-FILE-ID)                             EL6041
00989           SET        (ADDRESS OF CONTROL-FILE)                       CL**4
00990           RIDFLD     (ELCNTL-KEY)                                 EL6041
00991           UPDATE                                                  EL6041
00992      END-EXEC.                                                    EL6041
00993                                                                   EL6041
00994      PERFORM 1500-BUILD-RECORD.                                   EL6041
00995                                                                   EL6041
00996      IF NOT EMI-NO-ERRORS                                         EL6041
00997         EXEC CICS UNLOCK                                          EL6041
00998              DATASET   (ELCNTL-FILE-ID)                           EL6041
00999         END-EXEC                                                  EL6041
01000         GO TO 8200-SEND-DATAONLY.                                 EL6041
01001                                                                   EL6041
01002      EXEC CICS REWRITE                                            EL6041
01003           DATASET   (ELCNTL-FILE-ID)                              EL6041
01004           FROM      (CONTROL-FILE)                                EL6041
01005      END-EXEC.                                                    EL6041
01006                                                                   EL6041
01007      MOVE ER-0000      TO EMI-ERROR.                              EL6041
01008      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01009      GO TO 4000-SHOW-REPORT.                                      EL6041
01010                                                                   EL6041
01011  2100-NOT-FOUND.                                                  EL6041
01012      MOVE ER-0139              TO EMI-ERROR.                      EL6041
01013      MOVE -1                   TO REPORTL.                        EL6041
01014      MOVE AL-UNBON             TO REPORTA.                        EL6041
01015      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01016      GO TO 8200-SEND-DATAONLY.                                    EL6041
01017                                                                   EL6041
01018      EJECT                                                        EL6041
01019                                                                   EL6041
01020  2500-DELETE-REPORT.                                              EL6041
01021      IF REPORTI NOT NUMERIC                                       EL6041
01022         MOVE ER-7680            TO EMI-ERROR                      EL6041
01023         MOVE -1                 TO REPORTL                        EL6041
01024         MOVE AL-UNBON           TO REPORTA                        EL6041
01025         PERFORM 9900-ERROR-FORMAT                                 EL6041
01026         GO TO 8200-SEND-DATAONLY.                                 EL6041
01027                                                                   EL6041
01028      IF REPORTI NOT = PI-LAST-REPORT                              EL6041
01029         MOVE ER-0138            TO EMI-ERROR                      EL6041
01030         MOVE -1                 TO REPORTL                        EL6041
01031         MOVE AL-UNBON           TO REPORTA                        EL6041
01032         PERFORM 9900-ERROR-FORMAT                                 EL6041
01033         GO TO 8200-SEND-DATAONLY.                                 EL6041
01034                                                                   EL6041
01035      EXEC CICS HANDLE CONDITION                                   EL6041
01036          NOTFND   (2890-NOT-FOUND)                                EL6041
01037      END-EXEC.                                                    EL6041
01038                                                                   EL6041
01039      MOVE SPACES                 TO ELCNTL-KEY.                   EL6041
01040                                                                   EL6041
01041      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            EL6041
01042      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.           EL6041
01043      MOVE REPORTI                TO ELCNTL-REPORT.                EL6041
01044      MOVE +0002                  TO ELCNTL-SEQ-NO.                EL6041
01045                                                                   EL6041
01046      EXEC CICS DELETE                                             EL6041
01047           DATASET   (ELCNTL-FILE-ID)                              EL6041
01048           RIDFLD    (ELCNTL-KEY)                                  EL6041
01049      END-EXEC.                                                    EL6041
01050                                                                   EL6041
01051      MOVE LOW-VALUES TO EL6041AI.                                 EL6041
01052      MOVE ER-0000      TO EMI-ERROR.                              EL6041
01053      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01054      GO TO 8100-SEND-INITIAL-MAP.                                 EL6041
01055                                                                   EL6041
01056  2890-NOT-FOUND.                                                  EL6041
01057      MOVE  ER-0142               TO EMI-ERROR.                    EL6041
01058      MOVE -1                     TO REPORTL.                      EL6041
01059      MOVE AL-UNBON               TO REPORTA.                      EL6041
01060      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01061      GO TO 8200-SEND-DATAONLY.                                    EL6041
01062                                                                   EL6041
01063      EJECT                                                        EL6041
01064  3000-COPY-REPORT.                                                EL6041
01065      IF REPORTL GREATER +0 AND                                    EL6041
01066         REPORTI NUMERIC                                           EL6041
01067         NEXT SENTENCE                                             EL6041
01068      ELSE                                                         EL6041
01069         MOVE ER-7680     TO EMI-ERROR                             EL6041
01070         MOVE -1          TO REPORTL                               EL6041
01071         MOVE AL-UNBON    TO REPORTA                               EL6041
01072         PERFORM 9900-ERROR-FORMAT                                 EL6041
01073         GO TO 8200-SEND-DATAONLY.                                 EL6041
01074                                                                   EL6041
01075      IF CPYRPTL GREATER +0 AND                                    EL6041
01076         CPYRPTI NUMERIC                                           EL6041
01077         NEXT SENTENCE                                             EL6041
01078      ELSE                                                         EL6041
01079         MOVE ER-7680     TO EMI-ERROR                             EL6041
01080         MOVE -1          TO CPYRPTL                               EL6041
01081         MOVE AL-UNBON    TO CPYRPTA                               EL6041
01082         PERFORM 9900-ERROR-FORMAT                                 EL6041
01083         GO TO 8200-SEND-DATAONLY.                                 EL6041
01084                                                                   EL6041
01085      MOVE SPACES            TO ELCNTL-KEY.                        EL6041
01086      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.                 EL6041
01087      MOVE REPORTI           TO ELCNTL-REPORT.                     EL6041
01088      MOVE 'C'               TO ELCNTL-RECORD-TYPE.                EL6041
01089      MOVE +0002             TO ELCNTL-SEQ-NO.                     EL6041
01090                                                                   EL6041
01091      EXEC CICS HANDLE CONDITION                                   EL6041
01092           NOTFND     (3100-CONTINUE)                              EL6041
01093      END-EXEC.                                                    EL6041
01094                                                                   EL6041
01095      EXEC CICS READ                                               EL6041
01096           DATASET    (ELCNTL-FILE-ID)                             EL6041
01097           RIDFLD     (ELCNTL-KEY)                                 EL6041
01098           SET        (ADDRESS OF CONTROL-FILE)                       CL**4
01099      END-EXEC.                                                    EL6041
01100                                                                   EL6041
01101      MOVE -1                     TO REPORTL.                      EL6041
01102      MOVE  ER-0132               TO  EMI-ERROR.                   EL6041
01103      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01104      GO TO 8200-SEND-DATAONLY.                                    EL6041
01105                                                                   EL6041
01106  3100-CONTINUE.                                                   EL6041
01107      MOVE SPACES            TO ELCNTL-KEY.                        EL6041
01108      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.                 EL6041
01109      MOVE CPYRPTI           TO ELCNTL-REPORT.                     EL6041
01110      MOVE 'C'               TO ELCNTL-RECORD-TYPE.                EL6041
01111      MOVE +0002             TO ELCNTL-SEQ-NO.                     EL6041
01112                                                                   EL6041
01113      EXEC CICS HANDLE CONDITION                                   EL6041
01114           NOTFND     (3900-NOT-FOUND)                             EL6041
01115      END-EXEC.                                                    EL6041
01116                                                                   EL6041
01117      EXEC CICS READ                                               EL6041
01118           DATASET    (ELCNTL-FILE-ID)                             EL6041
01119           RIDFLD     (ELCNTL-KEY)                                 EL6041
01120           SET        (ADDRESS OF CONTROL-FILE)                       CL**4
01121      END-EXEC.                                                    EL6041
01122                                                                   EL6041
01123      MOVE SPACES            TO ELCNTL-KEY.                        EL6041
01124      MOVE 'CF'                  TO CF-RECORD-ID                   EL6041
01125      MOVE PI-COMPANY-ID         TO CF-COMPANY-ID                  EL6041
01126                                    ELCNTL-COMPANY-ID.             EL6041
01127      MOVE 'C'                   TO CF-RECORD-TYPE                 EL6041
01128                                    ELCNTL-RECORD-TYPE.            EL6041
01129      MOVE REPORTI               TO CF-CUSTOM-REPORT-NO            EL6041
01130                                    ELCNTL-REPORT.                 EL6041
01131      MOVE +0002                 TO CF-SEQUENCE-NO                 EL6041
01132                                    ELCNTL-SEQ-NO.                 EL6041
01133                                                                   EL6041
01134      MOVE SAVE-BIN-DATE         TO CF-LAST-MAINT-DT               EL6041
01135      MOVE PI-PROCESSOR-ID       TO CF-LAST-MAINT-BY               EL6041
01136      MOVE EIBTIME               TO CF-LAST-MAINT-HHMMSS.          EL6041
01137                                                                   EL6041
01138      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT          EL6041
01139                                           REPORTO.                EL6041
01140                                                                   EL6041
01141      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.            EL6041
01142      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.        EL6041
01143                                                                   EL6041
01144      EXEC CICS WRITE                                              EL6041
01145           DATASET(ELCNTL-FILE-ID)                                 EL6041
01146           FROM   (CONTROL-FILE)                                   EL6041
01147           RIDFLD (ELCNTL-KEY)                                     EL6041
01148      END-EXEC.                                                    EL6041
01149                                                                   EL6041
01150      MOVE ER-0000      TO EMI-ERROR.                              EL6041
01151      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01152      GO TO 4000-SHOW-REPORT.                                      EL6041
01153                                                                   EL6041
01154  3900-NOT-FOUND.                                                  EL6041
01155      MOVE  ER-0142               TO EMI-ERROR.                    EL6041
01156      MOVE -1                     TO CPYRPTL.                      EL6041
01157      MOVE AL-UNBON               TO CPYRPTA.                      EL6041
01158      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01159      GO TO 8200-SEND-DATAONLY.                                    EL6041
01160                                                                   EL6041
01161  4000-SHOW-REPORT.                                                EL6041
01162      IF MAINTI = 'S'                                              EL6041
01163         IF REPORTL GREATER THAN +0 AND                            EL6041
01164            REPORTI NUMERIC                                        EL6041
01165            NEXT SENTENCE                                          EL6041
01166         ELSE                                                      EL6041
01167            MOVE ER-7680     TO EMI-ERROR                          EL6041
01168            MOVE -1          TO REPORTL                            EL6041
01169            MOVE AL-UNBON    TO REPORTA                            EL6041
01170            PERFORM 9900-ERROR-FORMAT                              EL6041
01171            GO TO 8200-SEND-DATAONLY.                              EL6041
01172                                                                   EL6041
01173      IF MAINTI = 'S'                                              EL6041
01174         MOVE SPACES              TO ELCNTL-KEY                    EL6041
01175         MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID             EL6041
01176         MOVE 'C'                 TO ELCNTL-RECORD-TYPE            EL6041
01177         MOVE REPORTI             TO ELCNTL-REPORT                 EL6041
01178         MOVE +0002               TO ELCNTL-SEQ-NO.                EL6041
01179                                                                   EL6041
01180      EXEC CICS HANDLE CONDITION                                   EL6041
01181           NOTFND     (4100-NOT-FOUND)                             EL6041
01182      END-EXEC.                                                    EL6041
01183                                                                   EL6041
01184      EXEC CICS READ                                               EL6041
01185           DATASET     (ELCNTL-FILE-ID)                            EL6041
01186           RIDFLD      (ELCNTL-KEY)                                EL6041
01187           SET         (ADDRESS OF CONTROL-FILE)                      CL**4
01188      END-EXEC.                                                    EL6041
01189                                                                   EL6041
01190      MOVE LOW-VALUES                   TO EL6041AI.               EL6041
01191                                                                   EL6041
01192      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT          EL6041
01193                                           REPORTO.                EL6041
01194                                                                   EL6041
01195      MOVE CF-ACCOUNTS-LT-ONE-YEAR       TO  LTONEYO.                 CL**2
01196      MOVE CF-ACCT-ZERO-MONTH-PRODUCTION TO  ZERMPDO.                 CL**2
01197      MOVE CF-ISS-COUNT-DIFF            TO  ISSCNTO.               EL6041
01198      MOVE CF-SINGLE-MO-PREM-PCT        TO  SMPRMPO.               EL6041
01199      MOVE CF-EARN-PREM-DECR-PCT        TO  EPRMDCO.               EL6041
01200      MOVE CF-CANCELLATION-RATIO        TO  CNCRATO.               EL6041
01201                                                                      CL**3
01202      IF CF-RETENTION-LIMIT NUMERIC                                   CL**3
01203          MOVE CF-RETENTION-LIMIT       TO  RETLIMO.                  CL**3
01204                                                                   EL6041
01205      MOVE CF-LF-LOSS-RATIO-PCT         TO  LLSRATO.               EL6041
01206      MOVE CF-LF-LTM-LOSS-RATIO         TO  LLTLRSO.               EL6041
01207      MOVE CF-LF-PERIOD-PROFIT          TO  LPDPROO.               EL6041
01208      MOVE CF-LF-LTM-PROFIT-PCT         TO  LLTPRPO.               EL6041
01209      MOVE CF-LF-LTM-INFORCE-DECR       TO  LLTIDCO.               EL6041
01210      MOVE CF-LF-LTM-TERM-CHG           TO  LLTTRMO.               EL6041
01211      MOVE CF-LF-TERM-AVG-WEIGHTED      TO  LTRMAVO.               EL6041
01212      MOVE CF-LF-LTM-AGE-PCT            TO  LLTAGPO.               EL6041
01213      MOVE CF-LF-AGE-AVG-WEIGHTED       TO  LAVAGWO.               EL6041
01214      MOVE CF-LF-AVG-AGE-MAX            TO  LAVAGMO.               EL6041
01215                                                                   EL6041
01216      MOVE CF-AH-LOSS-RATIO-PCT         TO  ALSRATO.               EL6041
01217      MOVE CF-AH-LTM-LOSS-RATIO         TO  ALTLRSO.               EL6041
01218      MOVE CF-AH-PERIOD-PROFIT          TO  APDPROO.               EL6041
01219      MOVE CF-AH-LTM-PROFIT-PCT         TO  ALTPRPO.               EL6041
01220      MOVE CF-AH-LTM-INFORCE-DECR       TO  ALTIDCO.               EL6041
01221      MOVE CF-AH-LTM-TERM-CHG           TO  ALTTRMO.               EL6041
01222      MOVE CF-AH-TERM-AVG-WEIGHTED      TO  ATRMAVO.               EL6041
01223      MOVE CF-AH-LTM-AGE-PCT            TO  ALTAGPO.               EL6041
01224      MOVE CF-AH-AGE-AVG-WEIGHTED       TO  AAVAGWO.               EL6041
01225      MOVE CF-AH-AVG-AGE-MAX            TO  AAVAGMO.               EL6041
01226                                                                   EL6041
01227      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.            EL6041
01228      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.        EL6041
01229                                                                   EL6041
01230      GO TO 8100-SEND-INITIAL-MAP.                                 EL6041
01231                                                                   EL6041
01232  4100-NOT-FOUND.                                                  EL6041
01233      MOVE  ER-0142               TO EMI-ERROR.                    EL6041
01234      MOVE -1                     TO REPORTL.                      EL6041
01235      MOVE AL-UNBON               TO REPORTA.                      EL6041
01236      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01237      GO TO 8200-SEND-DATAONLY.                                    EL6041
01238                                                                   EL6041
01239  5000-VERIFY-CARRIER.                                             EL6041
01240      EXEC CICS HANDLE CONDITION                                   EL6041
01241           NOTFND    (5010-NOT-FOUND)                              EL6041
01242           ENDFILE   (5010-NOT-FOUND)                              EL6041
01243      END-EXEC.                                                    EL6041
01244                                                                   EL6041
01245      MOVE SPACES TO ELCNTL-TEST-KEY.                              EL6041
01246      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.              EL6041
01247      MOVE '6'                TO ELCNTL-TEST-REC-TYPE.             EL6041
01248      MOVE WS-TEST-CARRIER    TO ELCNTL-TEST-CAR.                  EL6041
01249      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.               EL6041
01250                                                                   EL6041
01251      EXEC CICS READ                                               EL6041
01252           DATASET    (ELCNTL-FILE-ID)                             EL6041
01253           RIDFLD     (ELCNTL-TEST-KEY)                            EL6041
01254           SET        (ADDRESS OF CONTROL-FILE)                       CL**4
01255      END-EXEC.                                                    EL6041
01256                                                                   EL6041
01257      GO TO 5019-EXIT.                                             EL6041
01258                                                                   EL6041
01259  5010-NOT-FOUND.                                                  EL6041
01260      MOVE ER-2845           TO EMI-ERROR                          EL6041
01261      MOVE -1                TO WS-TEST-LEN.                       EL6041
01262      MOVE AL-UNBON          TO WS-TEST-ATTRB.                     EL6041
01263      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01264                                                                   EL6041
01265  5019-EXIT.                                                       EL6041
01266      EXIT.                                                        EL6041
01267                                                                   EL6041
01268  5020-VERIFY-STATE.                                               EL6041
01269      EXEC CICS HANDLE CONDITION                                   EL6041
01270           NOTFND    (5030-NOT-FOUND)                              EL6041
01271           ENDFILE   (5030-NOT-FOUND)                              EL6041
01272      END-EXEC.                                                    EL6041
01273                                                                   EL6041
01274      MOVE SPACES TO ELCNTL-TEST-KEY.                              EL6041
01275      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.              EL6041
01276      MOVE '3'                TO ELCNTL-TEST-REC-TYPE.             EL6041
01277      MOVE WS-TEST-STATE      TO ELCNTL-TEST-STATE.                EL6041
01278      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.               EL6041
01279                                                                   EL6041
01280      EXEC CICS READ                                               EL6041
01281           DATASET    (ELCNTL-FILE-ID)                             EL6041
01282           RIDFLD     (ELCNTL-TEST-KEY)                            EL6041
01283           SET        (ADDRESS OF CONTROL-FILE)                       CL**4
01284      END-EXEC.                                                    EL6041
01285                                                                   EL6041
01286      GO TO 5039-EXIT.                                             EL6041
01287                                                                   EL6041
01288  5030-NOT-FOUND.                                                  EL6041
01289      MOVE ER-2848           TO EMI-ERROR                          EL6041
01290      MOVE -1                TO WS-TEST-LEN.                       EL6041
01291      MOVE AL-UNBON          TO WS-TEST-ATTRB.                     EL6041
01292      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01293                                                                   EL6041
01294  5039-EXIT.                                                       EL6041
01295      EXIT.                                                        EL6041
01296                                                                   EL6041
01297  5040-VERIFY-BEN.                                                 EL6041
01298      IF WS-TEST-BEN = SPACES                                      EL6041
01299          GO TO 5059-EXIT.                                         EL6041
01300                                                                   EL6041
01301      MOVE SPACES TO WS-BROWSE-SW.                                 EL6041
01302                                                                   EL6041
01303      EXEC CICS HANDLE CONDITION                                   EL6041
01304           NOTFND    (5050-NOT-FOUND)                              EL6041
01305           ENDFILE   (5050-NOT-FOUND)                              EL6041
01306      END-EXEC.                                                    EL6041
01307                                                                   EL6041
01308      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.              EL6041
01309      MOVE WS-TEST-BEN        TO ELCNTL-TEST-BEN.                  EL6041
01310      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.               EL6041
01311                                                                   EL6041
01312      EXEC CICS STARTBR                                            EL6041
01313           DATASET    (ELCNTL-FILE-ID)                             EL6041
01314           RIDFLD     (ELCNTL-TEST-KEY)                            EL6041
01315      END-EXEC.                                                    EL6041
01316                                                                   EL6041
01317      MOVE 'Y'    TO WS-BROWSE-SW.                                 EL6041
01318                                                                   EL6041
01319      EXEC CICS READNEXT                                           EL6041
01320           DATASET    (ELCNTL-FILE-ID)                             EL6041
01321           RIDFLD     (ELCNTL-TEST-KEY)                            EL6041
01322           SET        (ADDRESS OF CONTROL-FILE)                       CL**4
01323      END-EXEC.                                                    EL6041
01324                                                                   EL6041
01325      MOVE +1  TO WS-SUB.                                          EL6041
01326                                                                   EL6041
01327  5041-SEARCH-BEN-TYPE.                                            EL6041
01328      IF WS-SUB  GREATER THAN +8                                   EL6041
01329         GO TO 5050-NOT-FOUND.                                     EL6041
01330                                                                   EL6041
01331      IF CF-BENEFIT-CODE (WS-SUB) = WS-TEST-BEN                    EL6041
01332         GO TO 5055-ENDBR                                          EL6041
01333      ELSE                                                         EL6041
01334         ADD +1 TO WS-SUB                                          EL6041
01335         GO TO 5041-SEARCH-BEN-TYPE.                               EL6041
01336                                                                   EL6041
01337  5050-NOT-FOUND.                                                  EL6041
01338      MOVE ER-7685           TO EMI-ERROR                          EL6041
01339      MOVE -1                TO WS-TEST-LEN.                       EL6041
01340      MOVE AL-UNBON          TO WS-TEST-ATTRB.                     EL6041
01341      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01342                                                                   EL6041
01343  5055-ENDBR.                                                      EL6041
01344      IF WS-BROWSE-SW = 'Y'                                        EL6041
01345         EXEC CICS ENDBR                                           EL6041
01346              DATASET    (ELCNTL-FILE-ID)                          EL6041
01347         END-EXEC.                                                 EL6041
01348                                                                   EL6041
01349  5059-EXIT.                                                       EL6041
01350      EXIT.                                                        EL6041
01351                                                                   EL6041
01352  5060-VERIFY-BUS-TYPE.                                            EL6041
01353      MOVE SPACES TO WS-BROWSE-SW.                                 EL6041
01354                                                                   EL6041
01355      EXEC CICS HANDLE CONDITION                                   EL6041
01356           NOTFND    (5070-NOT-FOUND)                              EL6041
01357           ENDFILE   (5070-NOT-FOUND)                              EL6041
01358      END-EXEC.                                                    EL6041
01359                                                                   EL6041
01360      MOVE SPACES             TO ELCNTL-TEST-KEY.                  EL6041
01361      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.              EL6041
01362      MOVE '8'                TO ELCNTL-TEST-REC-TYPE.             EL6041
01363      MOVE WS-TEST-BUSTYP     TO ELCNTL-TEST-BUSTYP.               EL6041
01364      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.               EL6041
01365                                                                   EL6041
01366      EXEC CICS STARTBR                                            EL6041
01367           DATASET    (ELCNTL-FILE-ID)                             EL6041
01368           RIDFLD     (ELCNTL-TEST-KEY)                            EL6041
01369      END-EXEC.                                                    EL6041
01370                                                                   EL6041
01371      MOVE 'Y' TO WS-BROWSE-SW.                                    EL6041
01372                                                                   EL6041
01373      EXEC CICS READNEXT                                           EL6041
01374           DATASET    (ELCNTL-FILE-ID)                             EL6041
01375           RIDFLD     (ELCNTL-TEST-KEY)                            EL6041
01376           SET        (ADDRESS OF CONTROL-FILE)                       CL**4
01377      END-EXEC.                                                    EL6041
01378                                                                   EL6041
01379      COMPUTE WS-SUB = WS-TEST-BUSTYP -                            EL6041
01380                     (ELCNTL-TEST-BUSTYP - 20).                    EL6041
01381                                                                   EL6041
01382      IF ELCNTL-TEST-BUSTYP = 99                                   EL6041
01383         SUBTRACT +1 FROM WS-SUB.                                  EL6041
01384                                                                   EL6041
01385      IF CF-BUSINESS-TITLE (WS-SUB) = SPACES                       EL6041
01386         GO TO 5070-NOT-FOUND.                                     EL6041
01387                                                                   EL6041
01388      GO TO 5075-ENDBR.                                            EL6041
01389                                                                   EL6041
01390  5070-NOT-FOUND.                                                  EL6041
01391                                                                   EL6041
01392      MOVE ER-2178           TO EMI-ERROR                          EL6041
01393      MOVE -1                TO WS-TEST-LEN.                       EL6041
01394      MOVE AL-UNBON          TO WS-TEST-ATTRB.                     EL6041
01395      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01396                                                                   EL6041
01397  5075-ENDBR.                                                      EL6041
01398      IF WS-BROWSE-SW = 'Y'                                        EL6041
01399         EXEC CICS ENDBR                                           EL6041
01400              DATASET    (ELCNTL-FILE-ID)                          EL6041
01401         END-EXEC.                                                 EL6041
01402                                                                   EL6041
01403  5079-EXIT.                                                       EL6041
01404      EXIT.                                                        EL6041
01405      EJECT                                                        EL6041
01406                                                                   EL6041
01407  7000-BROWSE-FWRD-NEXT-ACCOUNT.                                   EL6041
01408      MOVE SPACES            TO ELCNTL-KEY.                        EL6041
01409      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.                 EL6041
01410      MOVE 'C'               TO ELCNTL-RECORD-TYPE.                EL6041
01411      MOVE +0003             TO ELCNTL-SEQ-NO.                     EL6041
01412                                                                   EL6041
01413      IF REPORTL GREATER THAN +0                                   EL6041
01414         MOVE REPORTI            TO ELCNTL-REPORT                  EL6041
01415      ELSE                                                         EL6041
01416         MOVE ZEROS              TO ELCNTL-REPORT.                 EL6041
01417                                                                   EL6041
01418      MOVE 'N'                    TO WS-REPORT-FOUND-SW.           EL6041
01419                                                                   EL6041
01420      EXEC CICS HANDLE CONDITION                                   EL6041
01421          NOTFND   (7080-END-OF-SEARCH)                            EL6041
01422      END-EXEC.                                                    EL6041
01423                                                                   EL6041
01424      EXEC CICS STARTBR                                            EL6041
01425          DATASET  (ELCNTL-FILE-ID)                                EL6041
01426          RIDFLD   (ELCNTL-KEY)                                    EL6041
01427      END-EXEC.                                                    EL6041
01428                                                                   EL6041
01429      EXEC CICS HANDLE CONDITION                                   EL6041
01430          NOTFND   (7070-END-OF-BROWSE)                            EL6041
01431          ENDFILE  (7070-END-OF-BROWSE)                            EL6041
01432      END-EXEC.                                                    EL6041
01433                                                                   EL6041
01434  7010-READ-FWRD-NEXT-RECORD.                                      EL6041
01435      EXEC CICS READNEXT                                           EL6041
01436          DATASET  (ELCNTL-FILE-ID)                                EL6041
01437          RIDFLD   (ELCNTL-KEY)                                    EL6041
01438          SET      (ADDRESS OF CONTROL-FILE)                          CL**4
01439      END-EXEC.                                                    EL6041
01440                                                                   EL6041
01441      IF ELCNTL-COMPANY-ID = PI-COMPANY-ID AND                     EL6041
01442         ELCNTL-RECORD-TYPE = 'C'                                  EL6041
01443         NEXT SENTENCE                                             EL6041
01444      ELSE                                                         EL6041
01445         GO TO 7070-END-OF-BROWSE.                                 EL6041
01446                                                                   EL6041
01447      IF ELCNTL-SEQ-NO = +0002                                     EL6041
01448         NEXT SENTENCE                                             EL6041
01449      ELSE                                                         EL6041
01450         GO TO 7010-READ-FWRD-NEXT-RECORD.                         EL6041
01451                                                                   EL6041
01452      MOVE 'Y'                    TO WS-REPORT-FOUND-SW.           EL6041
01453                                                                   EL6041
01454  7070-END-OF-BROWSE.                                              EL6041
01455      EXEC CICS ENDBR                                              EL6041
01456          DATASET  (ELCNTL-FILE-ID)                                EL6041
01457      END-EXEC.                                                    EL6041
01458                                                                   EL6041
01459       IF REPORT-WAS-NOT-FOUND                                     EL6041
01460          NEXT SENTENCE                                            EL6041
01461       ELSE                                                        EL6041
01462          GO TO 7090-EXIT.                                         EL6041
01463                                                                   EL6041
01464  7080-END-OF-SEARCH.                                              EL6041
01465      MOVE -1                     TO MAINTL.                       EL6041
01466      MOVE  ER-2237               TO EMI-ERROR.                    EL6041
01467      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01468      GO TO 8200-SEND-DATAONLY.                                    EL6041
01469                                                                   EL6041
01470  7090-EXIT.                                                       EL6041
01471      EXIT.                                                        EL6041
01472                                                                   EL6041
01473      EJECT                                                        EL6041
01474  7100-BROWSE-BWRD-NEXT-ACCOUNT.                                   EL6041
01475      MOVE 'N'                    TO WS-REPORT-FOUND-SW.           EL6041
01476                                                                   EL6041
01477      MOVE SPACES                 TO ELCNTL-KEY.                   EL6041
01478      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.            EL6041
01479      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.           EL6041
01480      MOVE +0                     TO ELCNTL-SEQ-NO.                EL6041
01481                                                                   EL6041
01482      IF REPORTL GREATER THAN +0                                   EL6041
01483         MOVE REPORTI            TO ELCNTL-REPORT                  EL6041
01484      ELSE                                                         EL6041
01485         MOVE PI-LAST-REPORT     TO ELCNTL-REPORT.                 EL6041
01486                                                                   EL6041
01487      EXEC CICS HANDLE CONDITION                                   EL6041
01488          NOTFND   (7180-END-OF-SEARCH)                            EL6041
01489      END-EXEC.                                                    EL6041
01490                                                                   EL6041
01491      EXEC CICS STARTBR                                            EL6041
01492          DATASET  (ELCNTL-FILE-ID)                                EL6041
01493          RIDFLD   (ELCNTL-KEY)                                    EL6041
01494      END-EXEC.                                                    EL6041
01495                                                                   EL6041
01496      EXEC CICS HANDLE CONDITION                                   EL6041
01497          NOTFND   (7170-END-OF-BROWSE)                            EL6041
01498          ENDFILE  (7170-END-OF-BROWSE)                            EL6041
01499      END-EXEC.                                                    EL6041
01500                                                                   EL6041
01501  7110-READ-FWRD-NEXT-RECORD.                                      EL6041
01502      EXEC CICS READNEXT                                           EL6041
01503          DATASET  (ELCNTL-FILE-ID)                                EL6041
01504          RIDFLD   (ELCNTL-KEY)                                    EL6041
01505          SET      (ADDRESS OF CONTROL-FILE)                          CL**4
01506      END-EXEC.                                                    EL6041
01507                                                                   EL6041
01508      EXEC CICS READPREV                                           EL6041
01509          DATASET  (ELCNTL-FILE-ID)                                EL6041
01510          RIDFLD   (ELCNTL-KEY)                                    EL6041
01511          SET      (ADDRESS OF CONTROL-FILE)                          CL**4
01512      END-EXEC.                                                    EL6041
01513                                                                   EL6041
01514  7110-READ-BWRD-NEXT-RECORD.                                      EL6041
01515      EXEC CICS READPREV                                           EL6041
01516          DATASET  (ELCNTL-FILE-ID)                                EL6041
01517          RIDFLD   (ELCNTL-KEY)                                    EL6041
01518          SET      (ADDRESS OF CONTROL-FILE)                          CL**4
01519      END-EXEC.                                                    EL6041
01520                                                                   EL6041
01521      IF ELCNTL-COMPANY-ID = PI-COMPANY-ID AND                     EL6041
01522         ELCNTL-RECORD-TYPE = 'C'                                  EL6041
01523         NEXT SENTENCE                                             EL6041
01524      ELSE                                                         EL6041
01525         GO TO 7170-END-OF-BROWSE.                                 EL6041
01526                                                                   EL6041
01527      IF ELCNTL-SEQ-NO = +0002                                     EL6041
01528         NEXT SENTENCE                                             EL6041
01529      ELSE                                                         EL6041
01530         GO TO 7110-READ-BWRD-NEXT-RECORD.                         EL6041
01531                                                                   EL6041
01532      MOVE 'Y'                    TO WS-REPORT-FOUND-SW.           EL6041
01533                                                                   EL6041
01534  7170-END-OF-BROWSE.                                              EL6041
01535      EXEC CICS ENDBR                                              EL6041
01536          DATASET  (ELCNTL-FILE-ID)                                EL6041
01537      END-EXEC.                                                    EL6041
01538                                                                   EL6041
01539      IF REPORT-WAS-NOT-FOUND                                      EL6041
01540         NEXT SENTENCE                                             EL6041
01541      ELSE                                                         EL6041
01542         GO TO 7190-EXIT.                                          EL6041
01543                                                                   EL6041
01544  7180-END-OF-SEARCH.                                              EL6041
01545      MOVE -1                     TO MAINTL.                       EL6041
01546      MOVE  ER-2238               TO EMI-ERROR.                    EL6041
01547      PERFORM 9900-ERROR-FORMAT.                                   EL6041
01548      GO TO 8200-SEND-DATAONLY.                                    EL6041
01549                                                                   EL6041
01550   7190-EXIT.                                                      EL6041
01551      EXIT.                                                        EL6041
01552                                                                   EL6041
01553      EJECT                                                        EL6041
01554  8100-SEND-INITIAL-MAP.                                           EL6041
01555      MOVE SAVE-DATE              TO RUNDTEO.                      EL6041
01556      MOVE EIBTIME                TO TIME-IN.                      EL6041
01557      MOVE TIME-OUT               TO RUNTIMEO.                     EL6041
01558      MOVE SPACES                 TO MAINTO.                       EL6041
01559      MOVE -1                     TO MAINTL.                       EL6041
01560      MOVE PI-LIFE-OVERRIDE-L6    TO LBENTPO.                      EL6041
01561      MOVE PI-AH-OVERRIDE-L6      TO ABENTPO.                      EL6041
01562      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL6041
01563 *****MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                        CL**3
01564                                                                   EL6041
01565  8150-SEND-INITIAL-MAP.                                           EL6041
01566      EXEC CICS SEND                                               EL6041
01567          MAP      (MAP-NAME)                                      EL6041
01568          MAPSET   (MAPSET-NAME)                                   EL6041
01569          FROM     (EL6041AO)                                      EL6041
01570          ERASE                                                    EL6041
01571          CURSOR                                                   EL6041
01572      END-EXEC.                                                    EL6041
01573                                                                   EL6041
01574      GO TO 9100-RETURN-TRAN.                                      EL6041
01575                                                                   EL6041
01576  8200-SEND-DATAONLY.                                              EL6041
01577      MOVE SAVE-DATE              TO RUNDTEO                       EL6041
01578      MOVE EIBTIME                TO TIME-IN.                      EL6041
01579      MOVE TIME-OUT               TO RUNTIMEO                      EL6041
01580      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.                     EL6041
01581 *****MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.                        CL**3
01582      EXEC CICS SEND                                               EL6041
01583          MAP      (MAP-NAME)                                      EL6041
01584          MAPSET   (MAPSET-NAME)                                   EL6041
01585          FROM     (EL6041AO)                                      EL6041
01586          DATAONLY                                                 EL6041
01587          CURSOR                                                   EL6041
01588      END-EXEC.                                                    EL6041
01589                                                                   EL6041
01590      GO TO 9100-RETURN-TRAN.                                      EL6041
01591                                                                   EL6041
01592      EJECT                                                        EL6041
01593  8300-SEND-TEXT.                                                  EL6041
01594      EXEC CICS SEND TEXT                                          EL6041
01595          FROM     (LOGOFF-TEXT)                                   EL6041
01596          LENGTH   (LOGOFF-LENGTH)                                 EL6041
01597          ERASE                                                    EL6041
01598          FREEKB                                                   EL6041
01599      END-EXEC.                                                    EL6041
01600                                                                   EL6041
01601      EXEC CICS RETURN                                             EL6041
01602      END-EXEC.                                                    EL6041
01603                                                                   EL6041
01604      EJECT                                                        EL6041
01605  8400-LOG-JOURNAL-RECORD.                                         EL6041
01606      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL6041
01607      MOVE ELCNTL-FILE-ID         TO JP-FILE-ID.                   EL6041
01608      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL6041
pemuni*    EXEC CICS JOURNAL                                            EL6041
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)                         EL6041
pemuni*        JTYPEID     ('CR')                                       EL6041
pemuni*        FROM        (JOURNAL-RECORD)                             EL6041
pemuni*        LENGTH      (WS-JOURNAL-RECORD-LENGTH)                   EL6041
pemuni*    END-EXEC.                                                    EL6041
01615                                                                   EL6041
01616  8400-EXIT.                                                       EL6041
01617      EXIT.                                                        EL6041
01618                                                                   EL6041
01619  8600-DEEDIT.                                                     EL6041
01620      EXEC CICS BIF DEEDIT                                         EL6041
01621           FIELD   (DEEDIT-FIELD)                                  EL6041
01622           LENGTH  (15)                                            EL6041
01623       END-EXEC.                                                   EL6041
01624                                                                   EL6041
01625  8800-UNAUTHORIZED-ACCESS.                                        EL6041
01626      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL6041
01627      GO TO 8300-SEND-TEXT.                                        EL6041
01628                                                                   EL6041
01629  8810-PF23.                                                       EL6041
01630      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL6041
01631      MOVE XCTL-005               TO PGM-NAME.                     EL6041
01632      GO TO 9300-XCTL.                                             EL6041
01633                                                                   EL6041
01634  9000-RETURN-CICS.                                                EL6041
01635      EXEC CICS RETURN                                             EL6041
01636      END-EXEC.                                                    EL6041
01637                                                                   EL6041
01638  9100-RETURN-TRAN.                                                EL6041
01639      MOVE    EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.          EL6041
01640      MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.      EL6041
01641      EXEC CICS RETURN                                             EL6041
01642          TRANSID    (TRANS-ID)                                    EL6041
01643          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6041
01644          LENGTH     (PI-COMM-LENGTH)                              EL6041
01645      END-EXEC.                                                    EL6041
01646                                                                   EL6041
01647  9200-RETURN-MAIN-MENU.                                           EL6041
01648                                                                   EL6041
01649      IF  CREDIT-SESSION                                           EL6041
01650          MOVE XCTL-EL626         TO PGM-NAME                      EL6041
01651                                                                   EL6041
01652      ELSE                                                         EL6041
01653          IF  CLAIM-SESSION                                        EL6041
01654              MOVE XCTL-EL126     TO PGM-NAME                      EL6041
01655                                                                   EL6041
01656          ELSE                                                     EL6041
01657              IF  MORTGAGE-SESSION                                 EL6041
01658                  MOVE XCTL-EM626 TO PGM-NAME                      EL6041
01659                                                                   EL6041
01660              ELSE                                                 EL6041
01661                  IF  GENERAL-LEDGER-SESSION                       EL6041
01662                      MOVE XCTL-GL800                              EL6041
01663                                  TO PGM-NAME.                     EL6041
01664                                                                   EL6041
01665      GO TO 9300-XCTL.                                             EL6041
01666                                                                   EL6041
01667  9300-XCTL.                                                       EL6041
01668      EXEC CICS XCTL                                               EL6041
01669          PROGRAM    (PGM-NAME)                                    EL6041
01670          COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     EL6041
01671          LENGTH     (PI-COMM-LENGTH)                              EL6041
01672      END-EXEC.                                                    EL6041
01673                                                                   EL6041
01674  9400-CLEAR.                                                      EL6041
01675      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL6041
01676      GO TO 9300-XCTL.                                             EL6041
01677                                                                   EL6041
01678  9500-PF12.                                                       EL6041
01679      MOVE XCTL-010               TO PGM-NAME.                     EL6041
01680      GO TO 9300-XCTL.                                             EL6041
01681                                                                   EL6041
01682  9600-PGMID-ERROR.                                                EL6041
01683      EXEC CICS HANDLE CONDITION                                   EL6041
01684          PGMIDERR    (8300-SEND-TEXT)                             EL6041
01685      END-EXEC.                                                    EL6041
01686                                                                   EL6041
01687      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL6041
01688      MOVE ' '                    TO PI-ENTRY-CD-1.                EL6041
01689      MOVE XCTL-005               TO PGM-NAME.                     EL6041
01690      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL6041
01691      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL6041
01692      GO TO 9300-XCTL.                                             EL6041
01693                                                                   EL6041
01694  9700-DATE-LINK.                                                  EL6041
01695      MOVE LINK-ELDATCV           TO PGM-NAME                      EL6041
01696      EXEC CICS LINK                                               EL6041
01697          PROGRAM    (PGM-NAME)                                    EL6041
01698          COMMAREA   (DATE-CONVERSION-DATA)                        EL6041
01699          LENGTH     (DC-COMM-LENGTH)                              EL6041
01700      END-EXEC.                                                    EL6041
01701                                                                   EL6041
01702  9900-ERROR-FORMAT.                                               EL6041
01703      IF NOT EMI-ERRORS-COMPLETE                                   EL6041
01704          MOVE LINK-001           TO PGM-NAME                      EL6041
01705          EXEC CICS LINK                                           EL6041
01706              PROGRAM    (PGM-NAME)                                EL6041
01707              COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           EL6041
01708              LENGTH     (EMI-COMM-LENGTH)                         EL6041
01709          END-EXEC.                                                EL6041
01710                                                                   EL6041
01711  9900-EXIT.                                                       EL6041
01712      EXIT.                                                        EL6041
01713                                                                   EL6041
01714  9990-ABEND.                                                      EL6041
01715      MOVE LINK-004               TO PGM-NAME.                     EL6041
01716      MOVE DFHEIBLK               TO EMI-LINE1                     EL6041
01717                                                                   EL6041
01718      EXEC CICS LINK                                               EL6041
01719          PROGRAM   (PGM-NAME)                                     EL6041
01720          COMMAREA  (EMI-LINE1)                                    EL6041
01721          LENGTH    (72)                                           EL6041
01722      END-EXEC.                                                    EL6041
01723                                                                   EL6041
01724      MOVE -1                     TO MAINTL.                       EL6041
01725      GO TO 8200-SEND-DATAONLY.                                    EL6041
01726                                                                   EL6041
01727      EJECT                                                        EL6041
01728  9910-INITIALIZE-SECURITY.                                        EL6041
01729 ******************************************************************EL6041
01730 *                                                                *EL6041
01731 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *EL6041
01732 *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *EL6041
01733 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *EL6041
01734 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *EL6041
01735 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *EL6041
01736 *       ERROR CONDITION AND EXITS THE PROGRAM.                   *EL6041
01737 *                                                                *EL6041
01738 ******************************************************************EL6041
01739                                                                   EL6041
01740      IF  PI-PROCESSOR-ID NOT = 'LGXX'                             EL6041
01741           MOVE 'Y'                     TO PI-DISPLAY-CAP          EL6041
01742                                           PI-MODIFY-CAP           EL6041
01743           IF  NOT SYSTEM-DISPLAY-CAP                              EL6041
01744                  MOVE 'READ'         TO SM-READ                   EL6041
01745                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT   EL6041
01746                  MOVE ER-9097        TO EMI-ERROR                 EL6041
01747                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT         EL6041
01748                  GO TO 8100-SEND-INITIAL-MAP                      EL6041
01749                                                                   EL6041
01750           ELSE                                                    EL6041
01751               GO TO 9910-EXIT.                                    EL6041
01752                                                                   EL6041
01753  9910-EXIT.                                                       EL6041
01754      EXIT.                                                        EL6041
01755                                                                   EL6041
01756  9995-SECURITY-VIOLATION.                                         EL6041
01757             COPY ELCSCTP.                                         EL6041
01758                                                                   EL6041
01759  9995-EXIT.                                                       EL6041
01760       EXIT.                                                       EL6041
01761                                                                   EL6041
