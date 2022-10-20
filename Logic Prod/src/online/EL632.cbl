00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL632 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/10/96 11:56:51.
00007 *                            VMOD=2.008
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *                                                                *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024
00025 *REMARKS.
00026 *        TRANSACTION - EXB4 - CLAIMS AND RESERVES
00027 *                             (CLAIM PAYMENTS).
00028
00029  ENVIRONMENT DIVISION.
00030  DATA DIVISION.
00031  EJECT
00032  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00033  77  FILLER  PIC X(32)  VALUE '********************************'.
00034  77  FILLER  PIC X(32)  VALUE '*    EL632 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.008 *********'.
00036
00037 *                            COPY ELCSCTM.
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
00038 *                            COPY ELCSCRTY.
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
00039
00040     EJECT
00041
00042  01  STANDARD-AREAS.
00043      12  SC-ITEM             PIC S9(4) COMP VALUE +1.
00044      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
00045      12  EL632A              PIC  X(8)       VALUE 'EL632A'.
00046      12  EL632B              PIC  X(8)       VALUE 'EL632B'.
00047      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL632S'.
00048      12  SCREEN-NUMBER       PIC  X(4)       VALUE '632B'.
00049      12  TRANS-ID            PIC  X(4)       VALUE 'EXB4'.
00050      12  THIS-PGM            PIC  X(8)       VALUE 'EL632 '.
00051      12  PGM-NAME            PIC  X(8).
00052      12  TIME-IN             PIC S9(7).
00053      12  TIME-OUT-R  REDEFINES TIME-IN.
00054          16  FILLER          PIC  X.
00055          16  TIME-OUT        PIC  99V99.
00056          16  FILLER          PIC  XX.
00057      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00058      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00059      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
00060      12  XCTL-6321           PIC  X(8)       VALUE 'EL6321'.
00061      12  XCTL-127            PIC  X(8)       VALUE 'EL127'.
00062      12  XCTL-1273           PIC  X(8)       VALUE 'EL1273'.
00063      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00064      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00065      12  LINK-053            PIC  X(8)       VALUE 'EL053'.
00066      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00067      12  ELCNTL-FILE-ID      PIC  X(8)       VALUE 'ELCNTL'.
00068      12  ERACCT-FILE-ID      PIC  X(8)       VALUE 'ERACCT2'.
00069      12  ERPNDC-FILE-ID      PIC  X(8)       VALUE 'ERPNDC'.
00070      12  RETURNED-FROM       PIC  X(8)       VALUE SPACES.
00071      12  PNDC-EDIT-PASS-AREA-LEN
00072                              PIC S9(4)       VALUE +553 COMP.
00073      12  SUB                 PIC  9(3)       COMP-3 VALUE ZEROS.
00074      12  QID.
00075          16  QID-TERM        PIC  X(4).
00076          16  FILLER          PIC  X(4)       VALUE '632A'.
00077      12  WS-ERR-CODE.
00078          16  FILLER          PIC  99         VALUE ZEROS.
00079          16  WS-ERROR-SUB    PIC  99         VALUE ZEROS.
00080      12  WS-ERROR-CODES.
00081          16  ER-0000         PIC  X(4)       VALUE '0000'.
00082          16  ER-0004         PIC  X(4)       VALUE '0004'.
00083          16  ER-0008         PIC  X(4)       VALUE '0008'.
00084          16  ER-0023         PIC  X(4)       VALUE '0023'.
00085          16  ER-0029         PIC  X(4)       VALUE '0029'.
00086          16  ER-0070         PIC  X(4)       VALUE '0070'.
00087          16  ER-0194         PIC  X(4)       VALUE '0194'.
00088          16  ER-0195         PIC  X(4)       VALUE '0195'.
00089          16  ER-0196         PIC  X(4)       VALUE '0196'.
00090          16  ER-0197         PIC  X(4)       VALUE '0197'.
00091          16  ER-0203         PIC  X(4)       VALUE '0203'.
00092          16  ER-0209         PIC  X(4)       VALUE '0209'.
00093          16  ER-0587         PIC  X(4)       VALUE '0587'.
00094          16  ER-2167         PIC  X(4)       VALUE '2167'.
00095          16  ER-2208         PIC  X(4)       VALUE '2208'.
00096          16  ER-2209         PIC  X(4)       VALUE '2209'.
00097          16  ER-2210         PIC  X(4)       VALUE '2210'.
00098          16  ER-2220         PIC  X(4)       VALUE '2220'.
00099          16  ER-2223         PIC  X(4)       VALUE '2223'.
00100          16  ER-2226         PIC  X(4)       VALUE '2226'.
00101          16  ER-2237         PIC  X(4)       VALUE '2237'.
00102          16  ER-2238         PIC  X(4)       VALUE '2238'.
00103          16  ER-2243         PIC  X(4)       VALUE '2243'.
00104          16  ER-2370         PIC  X(4)       VALUE '2270'.
00105          16  ER-2371         PIC  X(4)       VALUE '2271'.
00106          16  ER-2445         PIC  X(4)       VALUE '2445'.
00107          16  ER-2446         PIC  X(4)       VALUE '2446'.
00108          16  ER-2447         PIC  X(4)       VALUE '2447'.
00109          16  ER-2448         PIC  X(4)       VALUE '2448'.
00110          16  ER-2451         PIC  X(4)       VALUE '2451'.
00111          16  ER-2452         PIC  X(4)       VALUE '2452'.
00112          16  ER-2454         PIC  X(4)       VALUE '2454'.
00113          16  ER-2455         PIC  X(4)       VALUE '2455'.
00114          16  ER-2457         PIC  X(4)       VALUE '2457'.
00115          16  ER-2458         PIC  X(4)       VALUE '2458'.
00116          16  ER-2460         PIC  X(4)       VALUE '2460'.
00117          16  ER-2461         PIC  X(4)       VALUE '2461'.
00118          16  ER-2462         PIC  X(4)       VALUE '2462'.
00119          16  ER-2463         PIC  X(4)       VALUE '2463'.
00120          16  ER-2464         PIC  X(4)       VALUE '2464'.
00121          16  ER-2465         PIC  X(4)       VALUE '2465'.
00122          16  ER-2467         PIC  X(4)       VALUE '2467'.
00123          16  ER-2468         PIC  X(4)       VALUE '2468'.
00124          16  ER-2469         PIC  X(4)       VALUE '2469'.
00125          16  ER-2470         PIC  X(4)       VALUE '2470'.
00126          16  ER-2563         PIC  X(4)       VALUE '2563'.
00127          16  ER-2574         PIC  X(4)       VALUE '2574'.
00128          16  ER-2600         PIC  X(4)       VALUE '2600'.
00129          16  ER-2761         PIC  X(4)       VALUE '2761'.
00130          16  ER-2800         PIC  X(4)       VALUE '2800'.
00131          16  ER-2815         PIC  X(4)       VALUE '2815'.
00132          16  ER-2853         PIC  X(4)       VALUE '2853'.
00133  EJECT
00134  01  WORK-AREAS.
00135      12  EL632A-HEADING          PIC X(38) VALUE
00136          'PAID DT  PAID  TO    DAYS   AGE  CAUSE'.
00137      12  WS-ACLMTP1.
00138          16  FILLER              PIC  X(13)  VALUE
00139                  'CLAIM TYPES: '.
00140          16  WS-ACLM-TYP-1       PIC  X(6).
00141          16  FILLER              PIC  X(5)   VALUE '(1), '.
00142          16  WS-ACLM-TYP-3       PIC  X(6).
00143          16  FILLER              PIC  X(6)   VALUE '-OB(3)'.
00144      12  WS-ACLMTP2.
00145          16  WS-ACLM-TYP-2       PIC  X(6).
00146          16  FILLER              PIC  X(5)   VALUE '(2), '.
00147          16  WS-ACLM-TYP-4       PIC  X(6).
00148          16  FILLER              PIC  X(6)   VALUE '-OB(4)'.
00149      12  WS-EL632-EOM-DATE       PIC  XX     VALUE LOW-VALUES.
00150      12  WS-DATE-TEST.
00151          16  WS-DATE-MO          PIC  99     VALUE ZEROS.
00152          16  WS-DATE-DA          PIC  99     VALUE ZEROS.
00153          16  FILLER              PIC  99     VALUE ZEROS.
00154      12  WS-CURRENT-DT           PIC  X(8)   VALUE SPACES.
00155      12  WS-CURRENT-BIN-DT       PIC  XX     VALUE SPACES.
00156      12  WS-CARRIER-SW           PIC  X      VALUE SPACES.
00157      12  WS-SV-CARRIER           PIC  X      VALUE SPACES.
00158      12  WS-SV-GROUPING          PIC  X(6)   VALUE SPACES.
00159      12  WS-SV-STATE             PIC  XX     VALUE SPACES.
00160      12  WS-MAINT                PIC  X      VALUE SPACE.
00161      12  WS-CONVERTED-EFFDT      PIC  XX     VALUE SPACES.
00162      12  WS-CONVERTED-INCUR      PIC  XX     VALUE SPACES.
00163      12  WS-CONVERTED-REPORT     PIC  XX     VALUE SPACES.
00164      12  WS-CONVERTED-PAID       PIC  XX     VALUE SPACES.
00165      12  WS-CONVERTED-PTHRU      PIC  XX     VALUE SPACES.
00166      12  WS-VALID-CLAIM-TYPES    PIC  X      VALUE SPACE.
00167          88  VALID-CLAIM-TYPE                VALUE '1' '2' '3'
00168                                                    '4'.
00169      12  WS-VALID-PAY-TYPES      PIC  X      VALUE SPACE.
00170          88  VALID-PAY-TYPE                  VALUE '1' '2' '3'
00171                                                    '4' '5' '6'
00172                                                    '9'.
00173      12  WS-VALID-FORCE-CD       PIC  X      VALUE SPACE.
00174          88  VALID-FORCE-CD                  VALUE '6' '7' '8'.
CIDMOD     12  WS-DEEDIT-FIELD         PIC S9(9)V99.
00175      12  WS-DEEDIT-FIELD1        PIC  X(11)  VALUE ZEROS.
00176      12  WS-DEEDIT-FIELDV0  REDEFINES  WS-DEEDIT-FIELD1
00177                                  PIC  S9(11).
00178      12  WS-RESERVE-CONTROLS.
00179          16  WS-MANUAL-SW        PIC  X.
00180              88  WS-MANUAL-RESERVES-USED     VALUE '1'.
00181          16  CF-FUTURE-SW        PIC  X.
00182              88  WS-FUTURE-RESERVES-USED     VALUE '1'.
00183          16  CF-PTC-SW           PIC  X.
00184              88  WS-PAY-TO-CURRENT-USED      VALUE '1'.
00185          16  CF-IBNR-SW          PIC  X.
00186              88  WS-IBNR-RESERVES-USED       VALUE '1'.
00187  EJECT
00188  01  ACCESS-KEYS.
00189      12  ELCNTL-KEY.
00190          16  CNTL-COMP-ID        PIC  X(3)   VALUE SPACES.
00191          16  CNTL-REC-TYPE       PIC  X      VALUE SPACES.
00192          16  CNTL-ACCESS.
00193              20  CNTL-STATE      PIC  XX     VALUE SPACES.
00194              20  FILLER          PIC  X      VALUE SPACES.
00195              20  CNTL-CARRIER    PIC  X      VALUE SPACES.
00196          16  CNTL-SEQ            PIC S9(4)   VALUE +0   COMP.
00197      12  ERACCT-KEY.
00198          16  ERACCT-COMP-KEY.
00199              20  ACCT-CO         PIC  X      VALUE SPACES.
00200              20  ACCT-CARRIER    PIC  X      VALUE SPACES.
00201              20  ACCT-GROUPING   PIC  X(6)   VALUE SPACES.
00202              20  ACCT-STATE      PIC  XX     VALUE SPACES.
00203              20  ACCT-ACCOUNT    PIC  X(10)  VALUE SPACES.
00204          16  ACCT-EXP-DATE       PIC  X(6)   VALUE SPACES.
00205      12  ERACCT-SAVE-KEY         PIC  X(20)  VALUE SPACES.
00206      12  ERPNDC-KEY.
00207          16  PNDC-COMPANY-CD     PIC  X      VALUE SPACE.
00208          16  PNDC-CARRIER        PIC  X      VALUE SPACE.
00209          16  PNDC-GROUPING       PIC  X(6)   VALUE SPACES.
00210          16  PNDC-STATE          PIC  XX     VALUE SPACES.
00211          16  PNDC-ACCOUNT        PIC  X(10)  VALUE SPACES.
00212          16  PNDC-CERT-EFF-DT    PIC  XX     VALUE SPACES.
00213          16  PNDC-CERT-NO.
00214              20  PNDC-CERT-PRIME PIC  X(10)  VALUE SPACES.
00215              20  PNDC-CERT-SFX   PIC  X      VALUE SPACE.
00216          16  PNDC-CLAIM-NO       PIC  X(7)   VALUE SPACES.
00217          16  PNDC-CHECK-NO       PIC  X(7)   VALUE SPACES.
00218          16  PNDC-RECORD-TYPE    PIC  X      VALUE '1'.
00219          16  PNDC-RECORD-SEQ     PIC S9(4)   VALUE +0   COMP.
00220  EJECT
00221 *                                    COPY ELCDATE.
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
00222  EJECT
00223 *                                    COPY ELCLOGOF.
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
00224  EJECT
00225 *                                    COPY ELCATTR.
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
00226  EJECT
00227 *                                    COPY ELCEMIB.
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
00228  EJECT
00229 *                                    COPY ELCINTF.
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
00230      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00231          16  PI-MAP-NAME                 PIC  X(8).
00232          16  PI-FIRST-TIME-SW            PIC  X.
00233              88  PI-FIRST-TIME               VALUE SPACE.
00234          16  PI-EOF-SW                   PIC  X.
00235              88  PI-FILE-EOF                 VALUE 'Y'.
00236          16  PI-ERPNDC-KEY.
00237              20  PI-PNDC-COMP-CD         PIC  X.
00238              20  PI-PNDC-CARRIER         PIC  X.
00239              20  PI-PNDC-GROUPING        PIC  X(6).
00240              20  PI-PNDC-STATE           PIC  XX.
00241              20  PI-PNDC-ACCOUNT         PIC  X(10).
00242              20  PI-PNDC-CERT-EFF-DT     PIC  XX.
00243              20  PI-PNDC-CERT-NO.
00244                  24  PI-PNDC-PRIME       PIC  X(10).
00245                  24  PI-PNDC-SFX         PIC  X.
00246              20  PI-PNDC-CLAIM-NO        PIC  X(7).
00247              20  PI-PNDC-CHECK-NO        PIC  X(7).
00248              20  PI-PNDC-REC-TYP         PIC  X.
00249              20  PI-RECORD-SEQUENCE      PIC S9(4)     COMP.
00250          16  PI-PREV-ERPNDC-KEY          PIC  X(50).
00251          16  PI-MAINT                    PIC  X.
00252          16  FILLER                      PIC  X(529).
00253  EJECT
00254  01  PNDC-EDIT-PASS-AREA.
00255      12  PNDC-RECORD                     PIC  X(500).
00256      12  WK-PC-WORK-AREA.
00257          16  WK-PC-CNTL-RECORD-FOUND-SW  PIC  X.
00258          16  WK-PC-LAST-CARRIER          PIC  X.
00259          16  WK-PC-CERT-ACCESS-CNTL      PIC  X.
00260          16  WK-PC-CO-CLAIM-REJECT-SW    PIC  X.
00261          16  WK-PC-CLAIM-SYSTEM-SW       PIC  X.
00262          16  WK-PC-CO-TOL-CLAIM          PIC S999V99 COMP-3.
00263          16  WK-PC-RESERVE-CONTROLS      PIC  X(4).
00264          16  WK-PC-CREDIT-EDIT-CONTROLS  PIC  X(12).
00265      12  WK-PC-RECORD-ADDRESSES.
00266          16  WK-PC-ACCT-ADDR             PIC S9(8)     COMP.
00267          16  WK-PC-STATE-ADDR            PIC S9(8)     COMP.
00268      12  WK-MISC.
00269          16  WK-PC-REM-TRM-CALC-OPTION   PIC X.
00270          16  FILLER                      PIC X(20).
00271
00272  EJECT
00273 *                            COPY ELCJPFX.
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
00274                              PIC  X(500).
00275  EJECT
00276 *                            COPY ELCAID.
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
00277
00278  01  FILLER    REDEFINES DFHAID.
00279      12  FILLER              PIC  X(8).
00280      12  PF-VALUES           PIC  X      OCCURS 2 TIMES.
00281  EJECT
00282 *                            COPY EL632S.
       01  EL632AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  AMAINTL PIC S9(0004) COMP.
           05  AMAINTF PIC  X(0001).
           05  FILLER REDEFINES AMAINTF.
               10  AMAINTA PIC  X(0001).
           05  AMAINTI PIC  X(0001).
      *    -------------------------------
           05  ACARHDGL PIC S9(0004) COMP.
           05  ACARHDGF PIC  X(0001).
           05  FILLER REDEFINES ACARHDGF.
               10  ACARHDGA PIC  X(0001).
           05  ACARHDGI PIC  X(0005).
      *    -------------------------------
           05  AGRPHDGL PIC S9(0004) COMP.
           05  AGRPHDGF PIC  X(0001).
           05  FILLER REDEFINES AGRPHDGF.
               10  AGRPHDGA PIC  X(0001).
           05  AGRPHDGI PIC  X(0005).
      *    -------------------------------
           05  ASTHDGL PIC S9(0004) COMP.
           05  ASTHDGF PIC  X(0001).
           05  FILLER REDEFINES ASTHDGF.
               10  ASTHDGA PIC  X(0001).
           05  ASTHDGI PIC  X(0005).
      *    -------------------------------
           05  ACARIERL PIC S9(0004) COMP.
           05  ACARIERF PIC  X(0001).
           05  FILLER REDEFINES ACARIERF.
               10  ACARIERA PIC  X(0001).
           05  ACARIERI PIC  X(0001).
      *    -------------------------------
           05  AGROUPL PIC S9(0004) COMP.
           05  AGROUPF PIC  X(0001).
           05  FILLER REDEFINES AGROUPF.
               10  AGROUPA PIC  X(0001).
           05  AGROUPI PIC  X(0006).
      *    -------------------------------
           05  ASTATEL PIC S9(0004) COMP.
           05  ASTATEF PIC  X(0001).
           05  FILLER REDEFINES ASTATEF.
               10  ASTATEA PIC  X(0001).
           05  ASTATEI PIC  X(0002).
      *    -------------------------------
           05  AACCTL PIC S9(0004) COMP.
           05  AACCTF PIC  X(0001).
           05  FILLER REDEFINES AACCTF.
               10  AACCTA PIC  X(0001).
           05  AACCTI PIC  X(0010).
      *    -------------------------------
           05  ACERTL PIC S9(0004) COMP.
           05  ACERTF PIC  X(0001).
           05  FILLER REDEFINES ACERTF.
               10  ACERTA PIC  X(0001).
           05  ACERTI PIC  X(0010).
      *    -------------------------------
           05  ACRTSFXL PIC S9(0004) COMP.
           05  ACRTSFXF PIC  X(0001).
           05  FILLER REDEFINES ACRTSFXF.
               10  ACRTSFXA PIC  X(0001).
           05  ACRTSFXI PIC  X(0001).
      *    -------------------------------
           05  AEFFDTEL PIC S9(0004) COMP.
           05  AEFFDTEF PIC  X(0001).
           05  FILLER REDEFINES AEFFDTEF.
               10  AEFFDTEA PIC  X(0001).
           05  AEFFDTEI PIC  9(6).
      *    -------------------------------
           05  ACLAIML PIC S9(0004) COMP.
           05  ACLAIMF PIC  X(0001).
           05  FILLER REDEFINES ACLAIMF.
               10  ACLAIMA PIC  X(0001).
           05  ACLAIMI PIC  X(0007).
      *    -------------------------------
           05  ACTYPEL PIC S9(0004) COMP.
           05  ACTYPEF PIC  X(0001).
           05  FILLER REDEFINES ACTYPEF.
               10  ACTYPEA PIC  X(0001).
           05  ACTYPEI PIC  X(0001).
      *    -------------------------------
           05  ACHECKL PIC S9(0004) COMP.
           05  ACHECKF PIC  X(0001).
           05  FILLER REDEFINES ACHECKF.
               10  ACHECKA PIC  X(0001).
           05  ACHECKI PIC  X(0007).
      *    -------------------------------
           05  AHDINGL PIC S9(0004) COMP.
           05  AHDINGF PIC  X(0001).
           05  FILLER REDEFINES AHDINGF.
               10  AHDINGA PIC  X(0001).
           05  AHDINGI PIC  X(0038).
      *    -------------------------------
           05  APAYMNTL PIC S9(0004) COMP.
           05  APAYMNTF PIC  X(0001).
           05  FILLER REDEFINES APAYMNTF.
               10  APAYMNTA PIC  X(0001).
           05  APAYMNTI PIC  S9(9)V9(2).
      *    -------------------------------
           05  APTYPEL PIC S9(0004) COMP.
           05  APTYPEF PIC  X(0001).
           05  FILLER REDEFINES APTYPEF.
               10  APTYPEA PIC  X(0001).
           05  APTYPEI PIC  X(0001).
      *    -------------------------------
           05  AINCURL PIC S9(0004) COMP.
           05  AINCURF PIC  X(0001).
           05  FILLER REDEFINES AINCURF.
               10  AINCURA PIC  X(0001).
           05  AINCURI PIC  9(6).
      *    -------------------------------
           05  AREPORTL PIC S9(0004) COMP.
           05  AREPORTF PIC  X(0001).
           05  FILLER REDEFINES AREPORTF.
               10  AREPORTA PIC  X(0001).
           05  AREPORTI PIC  9(6).
      *    -------------------------------
           05  APAIDL PIC S9(0004) COMP.
           05  APAIDF PIC  X(0001).
           05  FILLER REDEFINES APAIDF.
               10  APAIDA PIC  X(0001).
           05  APAIDI PIC  9(6).
      *    -------------------------------
           05  APTHRUL PIC S9(0004) COMP.
           05  APTHRUF PIC  X(0001).
           05  FILLER REDEFINES APTHRUF.
               10  APTHRUA PIC  X(0001).
           05  APTHRUI PIC  9(6).
      *    -------------------------------
           05  ADAYSL PIC S9(0004) COMP.
           05  ADAYSF PIC  X(0001).
           05  FILLER REDEFINES ADAYSF.
               10  ADAYSA PIC  X(0001).
           05  ADAYSI PIC  X(0003).
      *    -------------------------------
           05  AAGEL PIC S9(0004) COMP.
           05  AAGEF PIC  X(0001).
           05  FILLER REDEFINES AAGEF.
               10  AAGEA PIC  X(0001).
           05  AAGEI PIC  99.
      *    -------------------------------
           05  ACAUSEL PIC S9(0004) COMP.
           05  ACAUSEF PIC  X(0001).
           05  FILLER REDEFINES ACAUSEF.
               10  ACAUSEA PIC  X(0001).
           05  ACAUSEI PIC  X(0006).
      *    -------------------------------
           05  AFORCEL PIC S9(0004) COMP.
           05  AFORCEF PIC  X(0001).
           05  FILLER REDEFINES AFORCEF.
               10  AFORCEA PIC  X(0001).
           05  AFORCEI PIC  X(0001).
      *    -------------------------------
           05  AREMBENL PIC S9(0004) COMP.
           05  AREMBENF PIC  X(0001).
           05  FILLER REDEFINES AREMBENF.
               10  AREMBENA PIC  X(0001).
           05  AREMBENI PIC  X(0010).
      *    -------------------------------
           05  AREMTRML PIC S9(0004) COMP.
           05  AREMTRMF PIC  X(0001).
           05  FILLER REDEFINES AREMTRMF.
               10  AREMTRMA PIC  X(0001).
           05  AREMTRMI PIC  X(0003).
      *    -------------------------------
           05  ACLMTP1L PIC S9(0004) COMP.
           05  ACLMTP1F PIC  X(0001).
           05  FILLER REDEFINES ACLMTP1F.
               10  ACLMTP1A PIC  X(0001).
           05  ACLMTP1I PIC  X(0036).
      *    -------------------------------
           05  ACLMTP2L PIC S9(0004) COMP.
           05  ACLMTP2F PIC  X(0001).
           05  FILLER REDEFINES ACLMTP2F.
               10  ACLMTP2A PIC  X(0001).
           05  ACLMTP2I PIC  X(0023).
      *    -------------------------------
           05  AEOMDTL PIC S9(0004) COMP.
           05  AEOMDTF PIC  X(0001).
           05  FILLER REDEFINES AEOMDTF.
               10  AEOMDTA PIC  X(0001).
           05  AEOMDTI PIC  X(0008).
      *    -------------------------------
           05  AERMSG1L PIC S9(0004) COMP.
           05  AERMSG1F PIC  X(0001).
           05  FILLER REDEFINES AERMSG1F.
               10  AERMSG1A PIC  X(0001).
           05  AERMSG1I PIC  X(0076).
      *    -------------------------------
           05  AERMSG2L PIC S9(0004) COMP.
           05  AERMSG2F PIC  X(0001).
           05  FILLER REDEFINES AERMSG2F.
               10  AERMSG2A PIC  X(0001).
           05  AERMSG2I PIC  X(0076).
      *    -------------------------------
           05  AERMSG3L PIC S9(0004) COMP.
           05  AERMSG3F PIC  X(0001).
           05  FILLER REDEFINES AERMSG3F.
               10  AERMSG3A PIC  X(0001).
           05  AERMSG3I PIC  X(0076).
      *    -------------------------------
           05  APFNTERL PIC S9(0004) COMP.
           05  APFNTERF PIC  X(0001).
           05  FILLER REDEFINES APFNTERF.
               10  APFNTERA PIC  X(0001).
           05  APFNTERI PIC  99.
       01  EL632AO REDEFINES EL632AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGRPHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACRTSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEFFDTEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACHECKO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHDINGO PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAYMNTO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AINCURO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREPORTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APAIDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APTHRUO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADAYSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACAUSEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFORCEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREMBENO PIC  ZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREMTRMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLMTP1O PIC  X(0036).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLMTP2O PIC  X(0023).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEOMDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AERMSG1O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AERMSG2O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AERMSG3O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFNTERO PIC  X(0002).
      *    -------------------------------
       01  EL632BI REDEFINES EL632AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  BMAINTL PIC S9(0004) COMP.
           05  BMAINTF PIC  X(0001).
           05  FILLER REDEFINES BMAINTF.
               10  BMAINTA PIC  X(0001).
           05  BMAINTI PIC  X(0001).
      *    -------------------------------
           05  BCARHDGL PIC S9(0004) COMP.
           05  BCARHDGF PIC  X(0001).
           05  FILLER REDEFINES BCARHDGF.
               10  BCARHDGA PIC  X(0001).
           05  BCARHDGI PIC  X(0005).
      *    -------------------------------
           05  BGRPHDGL PIC S9(0004) COMP.
           05  BGRPHDGF PIC  X(0001).
           05  FILLER REDEFINES BGRPHDGF.
               10  BGRPHDGA PIC  X(0001).
           05  BGRPHDGI PIC  X(0005).
      *    -------------------------------
           05  BSTHDGL PIC S9(0004) COMP.
           05  BSTHDGF PIC  X(0001).
           05  FILLER REDEFINES BSTHDGF.
               10  BSTHDGA PIC  X(0001).
           05  BSTHDGI PIC  X(0005).
      *    -------------------------------
           05  BCARIERL PIC S9(0004) COMP.
           05  BCARIERF PIC  X(0001).
           05  FILLER REDEFINES BCARIERF.
               10  BCARIERA PIC  X(0001).
           05  BCARIERI PIC  X(0001).
      *    -------------------------------
           05  BGROUPL PIC S9(0004) COMP.
           05  BGROUPF PIC  X(0001).
           05  FILLER REDEFINES BGROUPF.
               10  BGROUPA PIC  X(0001).
           05  BGROUPI PIC  X(0006).
      *    -------------------------------
           05  BSTATEL PIC S9(0004) COMP.
           05  BSTATEF PIC  X(0001).
           05  FILLER REDEFINES BSTATEF.
               10  BSTATEA PIC  X(0001).
           05  BSTATEI PIC  X(0002).
      *    -------------------------------
           05  BACCTL PIC S9(0004) COMP.
           05  BACCTF PIC  X(0001).
           05  FILLER REDEFINES BACCTF.
               10  BACCTA PIC  X(0001).
           05  BACCTI PIC  X(0010).
      *    -------------------------------
           05  BCERTL PIC S9(0004) COMP.
           05  BCERTF PIC  X(0001).
           05  FILLER REDEFINES BCERTF.
               10  BCERTA PIC  X(0001).
           05  BCERTI PIC  X(0010).
      *    -------------------------------
           05  BCRTSFXL PIC S9(0004) COMP.
           05  BCRTSFXF PIC  X(0001).
           05  FILLER REDEFINES BCRTSFXF.
               10  BCRTSFXA PIC  X(0001).
           05  BCRTSFXI PIC  X(0001).
      *    -------------------------------
           05  BEFFDTEL PIC S9(0004) COMP.
           05  BEFFDTEF PIC  X(0001).
           05  FILLER REDEFINES BEFFDTEF.
               10  BEFFDTEA PIC  X(0001).
           05  BEFFDTEI PIC  9(6).
      *    -------------------------------
           05  BCLAIML PIC S9(0004) COMP.
           05  BCLAIMF PIC  X(0001).
           05  FILLER REDEFINES BCLAIMF.
               10  BCLAIMA PIC  X(0001).
           05  BCLAIMI PIC  X(0007).
      *    -------------------------------
           05  BCLMTYPL PIC S9(0004) COMP.
           05  BCLMTYPF PIC  X(0001).
           05  FILLER REDEFINES BCLMTYPF.
               10  BCLMTYPA PIC  X(0001).
           05  BCLMTYPI PIC  X(0001).
      *    -------------------------------
           05  BFUTUREL PIC S9(0004) COMP.
           05  BFUTUREF PIC  X(0001).
           05  FILLER REDEFINES BFUTUREF.
               10  BFUTUREA PIC  X(0001).
           05  BFUTUREI PIC  S9(9)V9(2).
      *    -------------------------------
           05  BPAYCURL PIC S9(0004) COMP.
           05  BPAYCURF PIC  X(0001).
           05  FILLER REDEFINES BPAYCURF.
               10  BPAYCURA PIC  X(0001).
           05  BPAYCURI PIC  S9(9)V9(2).
      *    -------------------------------
           05  BIBNRL PIC S9(0004) COMP.
           05  BIBNRF PIC  X(0001).
           05  FILLER REDEFINES BIBNRF.
               10  BIBNRA PIC  X(0001).
           05  BIBNRI PIC  S9(9)V9(2).
      *    -------------------------------
           05  BMANUALL PIC S9(0004) COMP.
           05  BMANUALF PIC  X(0001).
           05  FILLER REDEFINES BMANUALF.
               10  BMANUALA PIC  X(0001).
           05  BMANUALI PIC  S9(9)V9(2).
      *    -------------------------------
           05  BFORCEL PIC S9(0004) COMP.
           05  BFORCEF PIC  X(0001).
           05  FILLER REDEFINES BFORCEF.
               10  BFORCEA PIC  X(0001).
           05  BFORCEI PIC  X(0001).
      *    -------------------------------
           05  BEOMDTL PIC S9(0004) COMP.
           05  BEOMDTF PIC  X(0001).
           05  FILLER REDEFINES BEOMDTF.
               10  BEOMDTA PIC  X(0001).
           05  BEOMDTI PIC  X(0008).
      *    -------------------------------
           05  BERMSG1L PIC S9(0004) COMP.
           05  BERMSG1F PIC  X(0001).
           05  FILLER REDEFINES BERMSG1F.
               10  BERMSG1A PIC  X(0001).
           05  BERMSG1I PIC  X(0076).
      *    -------------------------------
           05  BERMSG2L PIC S9(0004) COMP.
           05  BERMSG2F PIC  X(0001).
           05  FILLER REDEFINES BERMSG2F.
               10  BERMSG2A PIC  X(0001).
           05  BERMSG2I PIC  X(0076).
      *    -------------------------------
           05  BERMSG3L PIC S9(0004) COMP.
           05  BERMSG3F PIC  X(0001).
           05  FILLER REDEFINES BERMSG3F.
               10  BERMSG3A PIC  X(0001).
           05  BERMSG3I PIC  X(0076).
      *    -------------------------------
           05  BPFNTERL PIC S9(0004) COMP.
           05  BPFNTERF PIC  X(0001).
           05  FILLER REDEFINES BPFNTERF.
               10  BPFNTERA PIC  X(0001).
           05  BPFNTERI PIC  99.
       01  EL632BO REDEFINES EL632AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRPHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCRTSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFDTEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLMTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFUTUREO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPAYCURO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIBNRO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMANUALO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFORCEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEOMDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG1O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG2O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG3O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFNTERO PIC  X(0002).
      *    -------------------------------
00283  EJECT
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
00285  01  DFHCOMMAREA             PIC  X(1024).
00286  EJECT
00287 *                            COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
031808
031808         16  FILLER                         PIC X(82).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
00498          16  FILLER                             PIC  X(240).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
00607              20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
011410         16  FILLER                         PIC X(187).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
082603             20  FILLER                     PIC X(11).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
092705         16  FILLER                         PIC X(448).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
00288  EJECT
00289 *                            COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
090803     12  FILLER                            PIC X(5).
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00290  EJECT
00291 *                            COPY ERCPNDC.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDC                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING CLAIM TRANSACTIONS                *
00008 *                      PAYMENTS, RESERVES, EXPENSES              *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 500  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPNDC                         RKP=2,LEN=50   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
00019
00020  01  PENDING-CLAIMS.
00021      12  PC-RECORD-ID                     PIC XX.
00022          88  VALID-PC-ID                      VALUE 'PC'.
00023
00024      12  PC-CONTROL-PRIMARY.
00025          16  PC-COMPANY-CD                PIC X.
00026          16  PC-CARRIER                   PIC X.
00027          16  PC-GROUPING.
00028              20  PC-GROUPING-PREFIX       PIC XXX.
00029              20  PC-GROUPING-PRIME        PIC XXX.
00030          16  PC-STATE                     PIC XX.
00031          16  PC-ACCOUNT.
00032              20  PC-ACCOUNT-PREFIX        PIC X(4).
00033              20  PC-ACCOUNT-PRIME         PIC X(6).
00034          16  PC-CERT-EFF-DT               PIC XX.
00035          16  PC-CERT-NO.
00036              20  PC-CERT-PRIME            PIC X(10).
00037              20  PC-CERT-SFX              PIC X.
00038          16  PC-CLAIM-NO                  PIC X(7).
00039          16  PC-CHECK-NO                  PIC X(7).
00040
00041          16  PC-RECORD-TYPE               PIC X.
00042              88  PC-CLAIMS                    VALUE '1'.
00043              88  PC-RESERVES                  VALUE '2'.
00044          16  PC-RECORD-SEQUENCE           PIC S9(4)     COMP.
00045
00046      12  PC-LAST-MAINT-DT                 PIC XX.
00047      12  PC-LAST-MAINT-BY                 PIC X(4).
00048      12  PC-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00049
00050      12  PC-CLAIM-RECORD.
00051          16  PC-CLAIM-TYPE                PIC X.
00052              88  PC-LF-CLAIM                  VALUE '1'.
00053              88  PC-AH-CLAIM                  VALUE '2'.
00054              88  PC-OB-LF-CLAIM               VALUE '3'.
00055              88  PC-OB-AH-CLAIM               VALUE '4'.
00056          16  PC-PAYMENT-DT                PIC XX.
00057          16  PC-PAID-THRU-DT              PIC XX.
00058          16  PC-REPORTED-DT               PIC XX.
00059          16  PC-INCURRED-DT               PIC XX.
00060          16  PC-NO-OF-DAYS-PAID           PIC S9(3)     COMP-3.
00061          16  PC-CLAIM-PAYMENT             PIC S9(7)V99  COMP-3.
00062          16  PC-AGE-AT-CLAIM              PIC 99.
00063          16  FILLER                       PIC XX.
00064          16  PC-PAYMENT-TYPE              PIC X.
00065              88  PC-PARTIAL-PAYMENT           VALUE '1'.
00066              88  PC-FINAL-PAYMENT             VALUE '2'.
00067              88  PC-LUMP-SUM-PAYMENT          VALUE '3'.
00068              88  PC-ADDITIONAL-PAYMENT        VALUE '4'.
00069              88  PC-CHARGEBLE-EXPENSE         VALUE '5'.
00070              88  PC-NON-CHARGEBLE-EXPENSE     VALUE '6'.
00071              88  PC-VOIDED-PAYMENT            VALUE '9'.
00072
00073          16  PC-FUTURE-RESERVE-AMT        PIC S9(7)V99  COMP-3.
00074          16  PC-IBNR-RESERVE-AMT          PIC S9(7)V99  COMP-3.
00075          16  PC-PTC-RESERVE-AMT           PIC S9(7)V99  COMP-3.
00076          16  PC-MANUAL-RESERVE-AMT        PIC S9(7)V99  COMP-3.
00077
00078          16  PC-SV-CARRIER                PIC X.
00079          16  PC-SV-GROUPING               PIC X(6).
00080          16  PC-SV-STATE                  PIC XX.
00081
00082          16  PC-VOID-SW                   PIC X.
00083              88  PC-PUT-CERT-INFORCE          VALUE '1'.
00084
00085          16  PC-CAUSE-CODE                PIC X(6).
00086          16  FILLER                       PIC X(48).
00087
00088          16  PC-CLAIMED-CERT-DATA.
00089              20  PC-CC-INSURED-NAME.
00090                  24  PC-CC-LAST-NAME      PIC X(15).
00091                  24  PC-CC-INITIALS       PIC XX.
00092              20  PC-CC-INSURED-AGE        PIC S99.
00093              20  PC-CC-INSURED-SEX        PIC X.
00094              20  PC-CC-ORIG-TERM          PIC S999        COMP-3.
00095              20  PC-CC-LF-BENEFIT-CD      PIC XX.
00096              20  PC-CC-LIFE-BENEFIT-AMT   PIC S9(9)V99    COMP-3.
00097              20  PC-CC-ALT-LF-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00098              20  PC-CC-LIFE-PREMIUM       PIC S9(7)V99    COMP-3.
00099              20  PC-CC-AH-BENEFIT-CD      PIC XX.
00100              20  PC-CC-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00101              20  PC-CC-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00102              20  PC-CC-RATE-CLASS         PIC XX.
00103              20  PC-CC-RATE-DEV           PIC XXX.
00104              20  PC-CC-OB-FLAG            PIC X.
00105                  88  PC-CC-OB                VALUE 'B'.
00106              20  PC-CC-AH-POLICY-STATUS   PIC X.
00107                  88  PC-CCA-POLICY-IS-ACTIVE        VALUE '1' '3'
00108                                                '4' '5' '9' '2'.
00109                  88  PC-CCA-NORMAL-ENTRY            VALUE '1'.
00110                  88  PC-CCA-POLICY-PENDING          VALUE '2'.
00111                  88  PC-CCA-POLICY-IS-RESTORE       VALUE '3'.
00112                  88  PC-CCA-CONVERSION-ENTRY        VALUE '4'.
00113                  88  PC-CCA-POLICY-IS-REISSUE       VALUE '5'.
00114                  88  PC-CCA-LUMP-SUM-DISAB          VALUE '6'.
00115                  88  PC-CCA-DEATH-CLAIM-APPLIED     VALUE '7'.
00116                  88  PC-CCA-CANCEL-APPLIED          VALUE '8'.
00117                  88  PC-CCA-REIN-ONLY               VALUE '9'.
00118              20  PC-CC-LF-POLICY-STATUS   PIC X.
00119                  88  PC-CCL-POLICY-IS-ACTIVE        VALUE '1' '3'
00120                                                '4' '5' '9' '2'.
00121                  88  PC-CCL-NORMAL-ENTRY            VALUE '1'.
00122                  88  PC-CCL-POLICY-PENDING          VALUE '2'.
00123                  88  PC-CCL-POLICY-IS-RESTORE       VALUE '3'.
00124                  88  PC-CCL-CONVERSION-ENTRY        VALUE '4'.
00125                  88  PC-CCL-POLICY-IS-REISSUE       VALUE '5'.
00126                  88  PC-CCL-LUMP-SUM-DISAB          VALUE '6'.
00127                  88  PC-CCL-DEATH-CLAIM-APPLIED     VALUE '7'.
00128                  88  PC-CCL-CANCEL-APPLIED          VALUE '8'.
00129                  88  PC-CCL-REIN-ONLY               VALUE '9'.
00130              20  PC-CC-PAY-FREQUENCY      PIC 99.
00131              20  PC-CC-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00132              20  PC-CC-SOC-SEC-NO         PIC X(11).
00133              20  PC-CC-MEMBER-NO          PIC X(12).
00134              20  PC-CC-INT-CODE           PIC X.
00135                  88  PC-CC-ADD-ON                  VALUE 'A'.
00136                  88  PC-CC-SIMPLE                  VALUE 'S'.
00137              20  PC-CC-CAPPED-TERM        PIC 999.
00138              20  PC-CC-PRIOR-LUMP-PMT     PIC S9(7)V99  COMP-3.
00139              20  PC-CC-PRIOR-DEATH-AMT    PIC S9(9)V99  COMP-3.
00140              20  PC-CC-CANCEL-DT          PIC XX.
00141              20  PC-CC-DEATH-DT           PIC XX.
00142              20  PC-CC-SETTLEMENT-DT      PIC XX.
00143              20  PC-CC-PRIOR-STATUS       PIC X.
00144              20  PC-CC-CERT-ENTRY-STATUS  PIC X.
00145          16  PC-TRLR-SEQ-NO               PIC S9(4)     COMP.
070105         16  PC-CC-CLP-STATE              PIC XX.
070105         16  FILLER                       PIC X(14).
00147          16  PC-REMAINING-BENEFIT         PIC S9(9)V99  COMP-3.
00148          16  PC-REMAINING-TERM            PIC S9(3)     COMP-3.
00149          16  FILLER                       PIC X(34).
00150
00151      12  PC-RECORD-STATUS.
00152          16  PC-CREDIT-SELECT-DT          PIC XX.
00153          16  PC-CREDIT-ACCEPT-DT          PIC XX.
00154          16  FILLER                       PIC XX.
00155          16  PC-FATAL-FLAG                PIC X.
00156              88  PC-FATAL-ERRORS             VALUE 'X'.
00157          16  PC-FORCE-CODE                PIC X.
00158              88  PC-FORCE-OFF                VALUE ' ' '0'.
00159              88  PC-CLAIM-FORCE              VALUE '6' '7'
00160                                                      '8'.
00161          16  PC-FORCE-ER-CD               PIC X.
00162              88  PC-FORCE-ERRORS             VALUE 'F'.
00163              88  PC-UNFORCED-ERRORS          VALUE 'X'.
00164          16  PC-WARN-ER-CD                PIC X.
00165              88  PC-WARNING-ERRORS           VALUE 'W'.
00166          16  PC-LF-OVERRIDE-L1            PIC X.
00167          16  PC-AH-OVERRIDE-L1            PIC X.
00168          16  FILLER                       PIC X(17).
00169          16  PC-CERT-UPDATE-SW            PIC X.
00170              88  PC-CERT-DATA-CAPTURED       VALUE '1'.
00171          16  PC-COMPANY-ID                PIC XXX.
00172          16  PC-INPUT-DT                  PIC XX.
00173
00174      12  PC-ERROR-FLAGS.
00175          16  PC-STANDARD-ERRORS.
00176              20  PC-STD-ERROR-FLAGS   OCCURS 25 TIMES PIC X.
00177          16  PC-TRANSACTION-ERRORS.
00178              20  PC-TRN-ERROR-FLAGS   OCCURS 75 TIMES PIC X.
00179
00180      12  PC-ERR-FLAGS-R REDEFINES  PC-ERROR-FLAGS.
00181          16  PC-ERR-FLAG              OCCURS 100 TIMES PIC X.
00182
00183      12  FILLER                           PIC X(25).
00184
00185 ******************************************************************
00292  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                ACCOUNT-MASTER PENDING-CLAIMS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL632' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00294      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00295
00296      IF EIBCALEN  = ZERO
00297          GO TO 8800-UNAUTHORIZED-ACCESS.
00298
00299      MOVE 3                      TO  EMI-NUMBER-OF-LINES.
00300      MOVE 2                      TO  EMI-SWITCH2.
00301      MOVE EIBTRMID               TO  QID-TERM.
00302      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00303      MOVE '5'                    TO  DC-OPTION-CODE.
00304
00305      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00306
00307      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.
00308      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.
00309      MOVE PI-CALLING-PROGRAM     TO  RETURNED-FROM.
00310
00311      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00312          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00313              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00314              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00315              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00316              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00317              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00318              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00319              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00320              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00321          ELSE
00322              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00323              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00324              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00325              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00326              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00327              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00328              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00329              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00330
00331      MOVE LOW-VALUES             TO  EL632AI.
00332
00333      IF EIBTRNID NOT = TRANS-ID
00334          IF RETURNED-FROM = XCTL-6321
00335              GO TO 5000-BROWSE-CLAIMS-FILE
00336          ELSE
00337              IF RETURNED-FROM = XCTL-127 OR XCTL-1273
00338                  PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0690-EXIT
00339                  IF PI-CONTROL-IN-PROGRESS = SPACES
00340                      MOVE EL632A  TO  PI-MAP-NAME
00341                      GO TO 8100-SEND-INITIAL-MAP
00342                  ELSE
00343                      GO TO 5000-BROWSE-CLAIMS-FILE
00344              ELSE
00345                  MOVE EL632A     TO  PI-MAP-NAME
00346                  GO TO 8100-SEND-INITIAL-MAP.
00347
00348      MOVE PI-COMPANY-CD          TO  PNDC-COMPANY-CD
00349                                      PI-PNDC-COMP-CD.
00350
00351      
      * EXEC CICS HANDLE CONDITION
00352 *        PGMIDERR  (9600-PGMID-ERROR)
00353 *        ERROR     (9990-ABEND)
00354 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00003883' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033383833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00355
00356      IF EIBAID = DFHCLEAR
00357          GO TO 9400-CLEAR.
00358
00359      IF PI-PROCESSOR-ID = 'LGXX'
00360          GO TO 0200-RECEIVE.
00361
00362      
      * EXEC CICS READQ TS
00363 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00364 *        INTO   (SECURITY-CONTROL)
00365 *        LENGTH (SC-COMM-LENGTH)
00366 *        ITEM   (SC-ITEM)
00367 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003894' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00368
00369      MOVE SC-CREDIT-DISPLAY (14)  TO PI-DISPLAY-CAP.
00370      MOVE SC-CREDIT-UPDATE  (14)  TO PI-MODIFY-CAP.
00371
00372      IF NOT DISPLAY-CAP
00373          MOVE 'READ'          TO SM-READ
00374          PERFORM 9995-SECURITY-VIOLATION
00375          MOVE ER-0070         TO  EMI-ERROR
00376          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00377          GO TO 8100-SEND-INITIAL-MAP.
00378
00379  EJECT
00380  0200-RECEIVE.
00381      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00382          MOVE ER-0008            TO  EMI-ERROR
00383          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00384          IF PI-MAP-NAME = EL632A
00385              MOVE -1             TO  APFNTERL
00386              GO TO 8200-SEND-DATAONLY
00387          ELSE
00388              MOVE -1             TO  BPFNTERL
00389              GO TO 8200-SEND-DATAONLY.
00390
00391      
      * EXEC CICS RECEIVE
00392 *        MAP     (PI-MAP-NAME)
00393 *        MAPSET  (MAPSET-NAME)
00394 *        INTO    (EL632AI)
00395 *    END-EXEC.
           MOVE LENGTH OF
            EL632AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003923' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL632AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00396
00397      IF PI-MAP-NAME = EL632A
00398          IF APFNTERL IS GREATER ZERO
00399              IF EIBAID NOT = DFHENTER
00400                  MOVE ER-0004    TO  EMI-ERROR
00401                  MOVE AL-UNBOF   TO  APFNTERA
00402                  MOVE -1         TO  APFNTERL
00403                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00404                  GO TO 8200-SEND-DATAONLY
00405              ELSE
00406                  IF APFNTERI NUMERIC   AND
00407                     APFNTERI GREATER 0 AND
00408                     APFNTERI LESS 25
00409                      MOVE PF-VALUES (APFNTERI)  TO  EIBAID
00410                  ELSE
00411                      MOVE ER-0029   TO  EMI-ERROR
00412                      MOVE AL-UNBOF  TO  APFNTERA
00413                      MOVE -1        TO  APFNTERL
00414                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00415                      GO TO 8200-SEND-DATAONLY
00416          ELSE
00417              NEXT SENTENCE
00418      ELSE
00419          IF PI-MAP-NAME = EL632B
00420              IF BPFNTERL IS GREATER ZERO
00421                  IF EIBAID NOT = DFHENTER
00422                      MOVE ER-0004   TO  EMI-ERROR
00423                      MOVE AL-UNBOF  TO  BPFNTERA
00424                      MOVE -1        TO  BPFNTERL
00425                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00426                      GO TO 8200-SEND-DATAONLY
00427                  ELSE
00428                      IF BPFNTERI IS NUMERIC
00429                        AND  BPFNTERI IS GREATER 0
00430                        AND  BPFNTERI IS LESS 25
00431                          MOVE PF-VALUES (BPFNTERI)  TO  EIBAID
00432                      ELSE
00433                          MOVE ER-0029   TO  EMI-ERROR
00434                          MOVE AL-UNBOF  TO  BPFNTERA
00435                          MOVE -1        TO  BPFNTERL
00436                          PERFORM 9900-ERROR-FORMAT
00437                              THRU  9900-EXIT
00438                          GO TO 8200-SEND-DATAONLY.
00439
00440  0300-CHECK-PFKEYS.
00441      IF EIBAID = DFHPF23
00442          GO TO 8810-PF23.
00443
00444      IF EIBAID = DFHPF24
00445          GO TO 9200-RETURN-MAIN-MENU.
00446
00447      IF EIBAID = DFHPF12
00448          GO TO 9500-PF12.
00449
00450      IF EIBAID = DFHPF1 OR DFHPF2 OR DFHPF9
00451          MOVE SPACE              TO  PI-MAINT
00452          GO TO 5000-BROWSE-CLAIMS-FILE.
00453
00454      IF EIBAID = DFHPF3
00455          MOVE LOW-VALUES         TO  EL632AO
00456          MOVE EL632A             TO  PI-MAP-NAME
00457          GO TO 8100-SEND-INITIAL-MAP.
00458
00459      IF EIBAID = DFHPF4
00460          MOVE LOW-VALUES         TO  EL632BO
00461          MOVE EL632B             TO  PI-MAP-NAME
00462          GO TO 8110-SEND-INITIAL-MAPB.
00463
00464      IF EIBAID = DFHPF6
00465        AND PI-MAP-NAME = EL632A
00466          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT
00467          MOVE XCTL-1273          TO  PGM-NAME
00468          GO TO 9300-XCTL.
00469
00470      IF EIBAID = DFHPF7
00471        AND PI-MAP-NAME = EL632A
00472          PERFORM 0500-CREATE-TEMP-STORAGE  THRU  0590-EXIT
00473          MOVE XCTL-127           TO  PGM-NAME
00474          GO TO 9300-XCTL.
00475
00476      IF EIBAID = DFHPF10
00477          MOVE XCTL-6321          TO  PGM-NAME
00478          GO TO 9300-XCTL.
00479
00480      IF EIBAID = DFHENTER  OR  DFHPF11
00481          GO TO 1000-EDIT-DATA.
00482
00483      MOVE ER-0029                TO  EMI-ERROR.
00484
00485  0320-INPUT-ERROR.
00486      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00487
00488      IF PI-MAP-NAME = EL632A
00489          MOVE -1                 TO  APFNTERL
00490      ELSE
00491          MOVE -1                 TO  BPFNTERL.
00492
00493      GO TO 8200-SEND-DATAONLY.
00494  EJECT
00495  0500-CREATE-TEMP-STORAGE.
00496      
      * EXEC CICS WRITEQ TS
00497 *        QUEUE   (QID)
00498 *        FROM    (PROGRAM-INTERFACE-BLOCK)
00499 *        LENGTH  (PI-COMM-LENGTH)
00500 *    END-EXEC.
      *    MOVE '*"                    ''   #00004028' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00501
00502  0590-EXIT.
00503       EXIT.
00504
00505  0600-RECOVER-TEMP-STORAGE.
00506      
      * EXEC CICS READQ TS
00507 *        QUEUE   (QID)
00508 *        INTO    (PROGRAM-INTERFACE-BLOCK)
00509 *        LENGTH  (PI-COMM-LENGTH)
00510 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00004038' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00511
00512      PERFORM 0800-DELETE-TS  THRU  0890-EXIT.
00513
00514  0690-EXIT.
00515       EXIT.
00516
00517  0800-DELETE-TS.
00518      
      * EXEC CICS HANDLE CONDITION
00519 *        QIDERR  (0890-EXIT)
00520 *    END-EXEC.
      *    MOVE '"$N                   ! # #00004050' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034303530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00521
00522      
      * EXEC CICS DELETEQ TS
00523 *        QUEUE  (QID)
00524 *    END-EXEC.
      *    MOVE '*&                    #   #00004054' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00525
00526  0890-EXIT.
00527       EXIT.
00528  EJECT
00529  1000-EDIT-DATA.
00530      IF MODIFY-CAP
00531          NEXT SENTENCE
00532        ELSE
00533      IF AMAINTI NOT = 'S'
00534          MOVE 'UPDATE'       TO SM-READ
00535          PERFORM 9995-SECURITY-VIOLATION
00536          MOVE ER-0070        TO EMI-ERROR
00537          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00538          GO TO 8100-SEND-INITIAL-MAP.
00539
00540      IF PI-MAP-NAME = EL632B
00541          GO TO 1100-EDIT-RESERVE-DATA.
00542
00543      MOVE PI-ERPNDC-KEY          TO  PI-PREV-ERPNDC-KEY.
00544
00545      IF PI-HAS-CLAS-IC-CLAIM
00546        AND  AMAINTI NOT = 'S'
00547          MOVE ER-2243            TO  EMI-ERROR
00548          MOVE -1                 TO  AMAINTL
00549          MOVE AL-UABON           TO  AMAINTA
00550          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00551          GO TO 8200-SEND-DATAONLY.
00552
00553      IF AMAINTI = 'S' OR 'C' OR 'A' OR 'D' OR 'K'
00554          MOVE AMAINTI            TO  WS-MAINT
00555      ELSE
00556          MOVE ER-0023            TO  EMI-ERROR
00557          MOVE -1                 TO  AMAINTL
00558          MOVE AL-UABON           TO  AMAINTA
00559          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00560          GO TO 8200-SEND-DATAONLY.
00561
00562      IF AMAINTI = 'K'
00563          IF PI-MAINT = 'S'  OR  'K'
00564              NEXT SENTENCE
00565          ELSE
00566              MOVE ER-2167        TO  EMI-ERROR
00567              MOVE -1             TO  AMAINTL
00568              MOVE AL-UABON       TO  AMAINTA
00569              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00570              GO TO 8200-SEND-DATAONLY.
00571
00572      IF ACARIERL GREATER ZEROS
00573          MOVE AL-UANON           TO  ACARIERA
00574          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1200-EXIT
00575          MOVE ACARIERI           TO  PI-PNDC-CARRIER
00576      ELSE
00577          IF NOT ST-ACCNT-CNTL  AND  NOT ACCNT-CNTL
00578              MOVE -1             TO  ACARIERL
00579              MOVE AL-UABON       TO  ACARIERA
00580              MOVE ER-0194        TO  EMI-ERROR
00581              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00582
00583      IF PI-CARRIER-SECURITY GREATER SPACES
00584          IF ACARIERL GREATER ZEROS
00585              IF ACARIERI = PI-CARRIER-SECURITY
00586                  NEXT SENTENCE
00587              ELSE
00588                  MOVE -1         TO  ACARIERL
00589                  MOVE AL-UABON   TO  ACARIERA
00590                  MOVE ER-2370    TO  EMI-ERROR
00591                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00592
00593      IF AGROUPL GREATER ZEROS
00594          MOVE AL-UANON           TO  AGROUPA
00595          MOVE AGROUPI            TO  PI-PNDC-GROUPING
00596      ELSE
00597          IF CARR-GROUP-ST-ACCNT-CNTL
00598              MOVE -1             TO  AGROUPL
00599              MOVE AL-UABON       TO  AGROUPA
00600              MOVE ER-0195        TO  EMI-ERROR
00601              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00602
00603      IF ASTATEL GREATER ZEROS
00604          MOVE AL-UANON           TO  ASTATEA
00605          PERFORM 1300-VERIFY-STATE-ID  THRU  1300-EXIT
00606          MOVE ASTATEI            TO  PI-PNDC-STATE
00607      ELSE
00608          IF NOT ACCNT-CNTL  AND  NOT CARR-ACCNT-CNTL
00609              MOVE -1             TO  ASTATEL
00610              MOVE AL-UABON       TO  ASTATEA
00611              MOVE ER-0196        TO  EMI-ERROR
00612              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00613
00614      IF AACCTL GREATER ZEROS
00615          MOVE AL-UANON           TO  AACCTA
00616          PERFORM 1400-VERIFY-ACCOUNT  THRU  1400-EXIT
00617          MOVE AACCTI             TO  PI-PNDC-ACCOUNT
00618      ELSE
00619          MOVE -1                 TO  AACCTL
00620          MOVE AL-UABON           TO  AACCTA
00621          MOVE ER-0197            TO  EMI-ERROR
00622          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00623
00624      IF PI-ACCOUNT-SECURITY GREATER SPACES
00625          IF AACCTL GREATER ZEROS
00626              IF AACCTI = PI-ACCOUNT-SECURITY
00627                      NEXT SENTENCE
00628                  ELSE
00629                      MOVE -1        TO  AACCTL
00630                      MOVE AL-UABON  TO  AACCTA
00631                      MOVE ER-2371   TO  EMI-ERROR
00632                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00633
00634      IF ACERTL NOT = ZEROS
00635          MOVE AL-UANON           TO  ACERTA
00636          MOVE ACERTI             TO  PI-PNDC-PRIME
00637      ELSE
00638          MOVE ER-0203            TO  EMI-ERROR
00639          MOVE -1                 TO  ACERTL
00640          MOVE AL-UABON           TO  ACERTA
00641          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00642
00643      IF ACRTSFXL NOT = ZEROS
00644          MOVE ACRTSFXI           TO  PI-PNDC-SFX
00645          MOVE AL-UANON           TO  ACRTSFXA
00646      ELSE
00647          MOVE SPACES             TO  PI-PNDC-SFX.
00648
00649      IF AEFFDTEL NOT = ZEROS
00650          MOVE AL-UNNON           TO  AEFFDTEA
00651          IF AEFFDTEI  IS NUMERIC
00652              MOVE 4              TO  DC-OPTION-CODE
00653              MOVE AEFFDTEI       TO  DC-GREG-DATE-1-MDY
00654              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00655              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-EFFDT
00656              IF NO-CONVERSION-ERROR
00657                  MOVE WS-CONVERTED-EFFDT  TO  PI-PNDC-CERT-EFF-DT
00658              ELSE
00659                  MOVE -1         TO  AEFFDTEL
00660                  MOVE ER-2226    TO  EMI-ERROR
00661                  MOVE AL-UNBON   TO  AEFFDTEA
00662                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00663          ELSE
00664              MOVE -1             TO  AEFFDTEL
00665              MOVE ER-2223        TO  EMI-ERROR
00666              MOVE AL-UNBON       TO  AEFFDTEA
00667              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00668      ELSE
00669          MOVE -1                 TO  AEFFDTEL
00670          MOVE ER-2220            TO  EMI-ERROR
00671          MOVE AL-UNBON           TO  AEFFDTEA
00672          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00673
00674      IF ACLAIML NOT = ZEROS
00675          MOVE AL-UANON           TO  ACLAIMA
00676          MOVE ACLAIMI            TO  PI-PNDC-CLAIM-NO
00677      ELSE
00678          IF ACHECKL NOT = ZEROS
00679              MOVE ACHECKI        TO  ACLAIMI
00680                                      PI-PNDC-CLAIM-NO
00681              MOVE AL-UANON       TO  ACLAIMA
00682          ELSE
00683              MOVE ER-0209        TO  EMI-ERROR
00684              MOVE -1             TO  ACLAIML
00685              MOVE AL-UABON       TO  ACLAIMA
00686              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00687
00688      IF ACTYPEL NOT = ZEROS
00689          MOVE ACTYPEI            TO  WS-VALID-CLAIM-TYPES
00690          IF VALID-CLAIM-TYPE
00691              MOVE AL-UANON       TO  ACTYPEA
00692          ELSE
00693              MOVE ER-2445        TO  EMI-ERROR
00694              MOVE -1             TO  ACTYPEL
00695              MOVE AL-UABON       TO  ACTYPEA
00696              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00697      ELSE
00698          IF AMAINTI = 'A'
00699              MOVE ER-2446        TO  EMI-ERROR
00700              MOVE -1             TO  ACTYPEL
00701              MOVE AL-UABON       TO  ACTYPEA
00702              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00703
00704      IF ACHECKL NOT = ZEROS
00705          MOVE AL-UANON           TO  ACHECKA
00706          MOVE ACHECKI            TO  PI-PNDC-CHECK-NO
00707      ELSE
00708          MOVE ER-2447            TO  EMI-ERROR
00709          MOVE -1                 TO  ACHECKL
00710          MOVE AL-UABON           TO  ACHECKA
00711          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00712
00713      IF AMAINTI IS = 'S'  OR  'D'
00714          GO TO 1000-EDIT-COMPLETE.
00715
00716      IF APAYMNTL NOT = ZEROS
00717          MOVE AL-UNNON           TO  APAYMNTA
CIDMOD         MOVE APAYMNTI           TO  WS-DEEDIT-FIELD
CIDMOD         PERFORM 8600-DEEDIT  THRU  8600-EXIT
CIDMOD         MOVE WS-DEEDIT-FIELD    TO  APAYMNTI
00722      ELSE
00723          IF AMAINTI = 'A'
00724              MOVE ER-2448        TO  EMI-ERROR
00725              MOVE -1             TO  APAYMNTL
00726              MOVE AL-UNBON       TO  APAYMNTA
00727              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00728
00729      IF APTYPEL NOT = ZEROS
00730          MOVE APTYPEI            TO  WS-VALID-PAY-TYPES
00731          IF VALID-PAY-TYPE
00732              MOVE AL-UANON       TO  APTYPEA
00733          ELSE
00734              MOVE ER-2853        TO  EMI-ERROR
00735              MOVE -1             TO  APTYPEL
00736              MOVE AL-UABON       TO  APTYPEA
00737              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00738      ELSE
00739          IF AMAINTI = 'A'
00740              MOVE ER-2853        TO  EMI-ERROR
00741              MOVE -1             TO  APTYPEL
00742              MOVE AL-UABON       TO  APTYPEA
00743              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00744
00745      IF AINCURL NOT = ZEROS
00746          MOVE AL-UNNON           TO  AINCURA
00747          IF AINCURI  IS NUMERIC
00748              MOVE 4              TO  DC-OPTION-CODE
00749              MOVE AINCURI        TO  DC-GREG-DATE-1-MDY
00750              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00751              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-INCUR
00752              IF NO-CONVERSION-ERROR
00753                  NEXT SENTENCE
00754              ELSE
00755                  MOVE -1         TO  AINCURL
00756                  MOVE ER-2452    TO  EMI-ERROR
00757                  MOVE AL-UNBON   TO  AINCURA
00758                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00759          ELSE
00760              MOVE -1             TO  AINCURL
00761              MOVE ER-2451        TO  EMI-ERROR
00762              MOVE AL-UNBON       TO  AINCURA
00763              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00764
00765      IF AREPORTL NOT = ZEROS
00766          MOVE AL-UNNON           TO  AREPORTA
00767          IF AREPORTI  IS NUMERIC
00768              MOVE 4              TO  DC-OPTION-CODE
00769              MOVE AREPORTI       TO  DC-GREG-DATE-1-MDY
00770              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00771              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-REPORT
00772              IF NO-CONVERSION-ERROR
00773                  NEXT SENTENCE
00774              ELSE
00775                  MOVE -1         TO  AREPORTL
00776                  MOVE ER-2455    TO  EMI-ERROR
00777                  MOVE AL-UNBON   TO  AREPORTA
00778                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00779          ELSE
00780              MOVE -1             TO  AREPORTL
00781              MOVE ER-2454        TO  EMI-ERROR
00782              MOVE AL-UNBON       TO  AREPORTA
00783              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00784
00785      IF APAIDL NOT = ZEROS
00786          MOVE AL-UNNON           TO  APAIDA
00787          IF APAIDI  IS NUMERIC
00788              MOVE 4              TO  DC-OPTION-CODE
00789              MOVE APAIDI         TO  DC-GREG-DATE-1-MDY
00790              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00791              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-PAID
00792              IF NO-CONVERSION-ERROR
00793                  NEXT SENTENCE
00794              ELSE
00795                  MOVE -1         TO  APAIDL
00796                  MOVE ER-2458    TO  EMI-ERROR
00797                  MOVE AL-UNBON   TO  APAIDA
00798                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00799          ELSE
00800              MOVE -1             TO  APAIDL
00801              MOVE ER-2457        TO  EMI-ERROR
00802              MOVE AL-UNBON       TO  APAIDA
00803              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00804
00805      IF APTHRUL NOT EQUAL ZEROS
00806         MOVE AL-UNNON           TO  APTHRUA
00807         IF APTHRUI  IS NUMERIC
00808            MOVE '4'            TO  DC-OPTION-CODE
00809            MOVE APTHRUI        TO  DC-GREG-DATE-1-MDY
00810            PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00811            MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-PTHRU
00812            IF NO-CONVERSION-ERROR
00813               IF PI-USES-PAID-TO
00814                  MOVE '6'            TO  DC-OPTION-CODE
00815                  MOVE APTHRUI        TO  DC-GREG-DATE-1-MDY
00816                  MOVE -1             TO DC-ELAPSED-DAYS
00817                  MOVE +0             TO DC-ELAPSED-MONTHS
00818                  PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00819                  MOVE DC-BIN-DATE-2  TO  WS-CONVERTED-PTHRU
00820               ELSE
00821                  NEXT SENTENCE
00822            ELSE
00823               MOVE -1         TO  APTHRUL
00824               MOVE ER-2461    TO  EMI-ERROR
00825               MOVE AL-UNBON   TO  APTHRUA
00826               PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00827         ELSE
00828            MOVE -1             TO  APTHRUL
00829            MOVE ER-2460        TO  EMI-ERROR
00830            MOVE AL-UNBON       TO  APTHRUA
00831            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00832
00833      IF ADAYSL NOT = ZEROS
00834          IF ADAYSI  IS NUMERIC
00835              MOVE AL-UNNON       TO  ADAYSA
00836          ELSE
00837              MOVE ER-2463        TO  EMI-ERROR
00838              MOVE -1             TO  ADAYSL
00839              MOVE AL-UNBON       TO  ADAYSA
00840              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00841      ELSE
00842          IF AMAINTI = 'A'
00843            AND (ACTYPEI = '2'  OR  '4')
00844            AND (APTYPEI NOT = '5'  AND  '6')
00845              MOVE ER-2462        TO  EMI-ERROR
00846              MOVE -1             TO  ADAYSL
00847              MOVE AL-UNBON       TO  ADAYSA
00848              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00849
00850      IF AAGEL NOT = ZEROS
00851          IF AAGEI  IS NUMERIC
00852              MOVE AL-UNNON       TO  AAGEA
00853          ELSE
00854              MOVE ER-2464        TO  EMI-ERROR
00855              MOVE -1             TO  AAGEL
00856              MOVE AL-UNBON       TO  AAGEA
00857              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00858
00859      IF ACAUSEL NOT = ZEROS
00860          MOVE AL-UANON           TO  ACAUSEA.
00861
00862      IF AFORCEL NOT = ZEROS
00863          MOVE AL-UANON           TO  AFORCEA.
00864
00865      IF AEOMDTL NOT = ZEROS
00866          MOVE AL-UNNON           TO  AEOMDTA
00867          MOVE AEOMDTI            TO  WS-DEEDIT-FIELD1
00868          
      * EXEC CICS BIF DEEDIT
00869 *            FIELD   (WS-DEEDIT-FIELD1)
00870 *            LENGTH  (11)
00871 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004399' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD1, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00872          MOVE WS-DEEDIT-FIELDV0   TO  AEOMDTO
00873                                       DC-GREG-DATE-1-MDY
00874          INSPECT AEOMDTO CONVERTING SPACES TO '/'
00875          MOVE 4                   TO  DC-OPTION-CODE
00876          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00877          MOVE DC-GREG-DATE-1-MDY  TO  WS-DATE-TEST
00878          IF (DATE-CONVERSION-ERROR)
00879            OR  (DC-DAYS-IN-MONTH NOT = WS-DATE-DA)
00880              MOVE -1              TO  AEOMDTL
00881              MOVE ER-0587         TO  EMI-ERROR
00882              MOVE AL-UNBON        TO  AEOMDTA
00883              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00884          ELSE
00885              MOVE DC-BIN-DATE-1   TO  WS-EL632-EOM-DATE.
00886
00887  1000-EDIT-COMPLETE.
00888      MOVE AMAINTI                TO  PI-MAINT.
00889
00890      IF PI-MAINT = 'C'
00891          IF PI-ERPNDC-KEY = PI-PREV-ERPNDC-KEY
00892              NEXT SENTENCE
00893          ELSE
00894              MOVE ER-2761          TO  EMI-ERROR
00895              MOVE -1               TO  ACARIERL
00896              MOVE AL-UABON         TO  ACARIERA
00897                                        AGROUPA
00898                                        ASTATEA
00899                                        AACCTA
00900                                        ACERTA
00901                                        ACRTSFXA
00902                                        AEFFDTEA
00903                                        ACLAIMA
00904                                        ACHECKA
00905              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00906
00907      IF AMAINTI = 'S'
00908          MOVE ZEROS              TO  PI-RECORD-SEQUENCE
00909          MOVE '1'                TO  PI-PNDC-REC-TYP
00910          GO TO 5000-BROWSE-CLAIMS-FILE.
00911
00912      IF EMI-ERROR NOT = ZEROS
00913          MOVE PI-PREV-ERPNDC-KEY  TO  PI-ERPNDC-KEY
00914          GO TO 8200-SEND-DATAONLY.
00915
00916      IF AMAINTI = 'D'
00917          GO TO 4000-DELETE-RECORD.
00918
00919      IF AMAINTI = 'A'
00920          GO TO 2000-ADD-RECORD.
00921
00922      IF AMAINTI = 'C'
00923          GO TO 3000-CHANGE-RECORD.
00924
00925      IF AMAINTI = 'K'
00926          PERFORM 3500-REWRITE-RECORD  THRU  3900-EXIT
00927          GO TO 3000-CHANGE-RECORD.
00928  EJECT
00929  1100-EDIT-RESERVE-DATA.
00930      MOVE PI-ERPNDC-KEY          TO  PI-PREV-ERPNDC-KEY.
00931      MOVE SPACES                 TO  PI-PNDC-CHECK-NO.
00932
00933      IF PI-HAS-CLAS-IC-CLAIM
00934        AND  BMAINTI NOT = 'S'
00935          MOVE ER-2243            TO  EMI-ERROR
00936          MOVE -1                 TO  BMAINTL
00937          MOVE AL-UABON           TO  BMAINTA
00938          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00939          GO TO 1100-EDIT-COMPLETE.
00940
00941      IF BMAINTI = 'S'  OR  'C'  OR  'A'
00942        OR  'D'  OR  'K'
00943          MOVE BMAINTI            TO  WS-MAINT
00944      ELSE
00945          MOVE ER-0023            TO  EMI-ERROR
00946          MOVE -1                 TO  BMAINTL
00947          MOVE AL-UABON           TO  BMAINTA
00948          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00949          GO TO 1100-EDIT-COMPLETE.
00950
00951      IF BMAINTI = 'K'
00952          IF PI-MAINT = 'S'  OR  'K'
00953              NEXT SENTENCE
00954          ELSE
00955              MOVE ER-2167        TO  EMI-ERROR
00956              MOVE -1             TO  BMAINTL
00957              MOVE AL-UABON       TO  BMAINTA
00958              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00959              GO TO 8200-SEND-DATAONLY.
00960
00961      IF BCARIERL GREATER ZEROS
00962          MOVE AL-UANON           TO  BCARIERA
00963          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1200-EXIT
00964          MOVE BCARIERI           TO  PI-PNDC-CARRIER
00965      ELSE
00966          IF NOT ST-ACCNT-CNTL  AND  NOT ACCNT-CNTL
00967              MOVE -1             TO  BCARIERL
00968              MOVE AL-UABON       TO  BCARIERA
00969              MOVE ER-0194        TO  EMI-ERROR
00970              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00971
00972      IF BGROUPL GREATER ZEROS
00973          MOVE AL-UANON           TO  BGROUPA
00974          MOVE BGROUPI            TO  PI-PNDC-GROUPING
00975      ELSE
00976          IF CARR-GROUP-ST-ACCNT-CNTL
00977              MOVE -1             TO  BGROUPL
00978              MOVE AL-UABON       TO  BGROUPA
00979              MOVE ER-0195        TO  EMI-ERROR
00980              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00981
00982      IF BSTATEL GREATER ZEROS
00983          MOVE AL-UANON           TO  BSTATEA
00984          PERFORM 1300-VERIFY-STATE-ID  THRU  1300-EXIT
00985          MOVE BSTATEI            TO  PI-PNDC-STATE
00986      ELSE
00987          IF NOT ACCNT-CNTL  AND  NOT CARR-ACCNT-CNTL
00988              MOVE -1             TO  BSTATEL
00989              MOVE AL-UABON       TO  BSTATEA
00990              MOVE ER-0196        TO  EMI-ERROR
00991              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00992
00993      IF BMAINTI = 'S'  OR  'A'  OR  'C'
00994          IF BACCTL GREATER ZEROS
00995              MOVE AL-UANON           TO  BACCTA
00996              PERFORM 1400-VERIFY-ACCOUNT  THRU  1400-EXIT
00997              MOVE BACCTI             TO  PI-PNDC-ACCOUNT
00998          ELSE
00999              MOVE -1                 TO  BACCTL
01000              MOVE AL-UABON           TO  BACCTA
01001              MOVE ER-0197            TO  EMI-ERROR
01002              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01003
01004      IF PI-COMPANY-ID = 'RIG'
01005        AND  BMAINTI NOT = 'D'
01006          MOVE AM-CARRIER         TO  BCARIERI
01007          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1200-EXIT
01008          MOVE LOW-VALUES TO BCARIERI.
01009
01010      IF BCERTL NOT = ZEROS
01011          MOVE AL-UANON           TO  BCERTA
01012          MOVE BCERTI             TO  PI-PNDC-PRIME
01013      ELSE
01014          MOVE ER-0203            TO  EMI-ERROR
01015          MOVE -1                 TO  BCERTL
01016          MOVE AL-UABON           TO  BCERTA
01017          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01018
01019      IF BCRTSFXL NOT = ZEROS
01020          MOVE BCRTSFXI           TO  PI-PNDC-SFX
01021          MOVE AL-UANON           TO  BCRTSFXA.
01022
01023      IF BEFFDTEL NOT = ZEROS
01024          MOVE AL-UNNON           TO  BEFFDTEA
01025          IF BEFFDTEI  IS NUMERIC
01026              MOVE 4              TO  DC-OPTION-CODE
01027              MOVE BEFFDTEI       TO  DC-GREG-DATE-1-MDY
01028              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
01029              MOVE DC-BIN-DATE-1  TO  WS-CONVERTED-EFFDT
01030              IF NO-CONVERSION-ERROR
01031                  MOVE WS-CONVERTED-EFFDT
01032                                  TO  PI-PNDC-CERT-EFF-DT
01033              ELSE
01034                  MOVE -1         TO  BEFFDTEL
01035                  MOVE ER-2226    TO  EMI-ERROR
01036                  MOVE AL-UNBON   TO  BEFFDTEA
01037                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01038          ELSE
01039              MOVE -1             TO  BEFFDTEL
01040              MOVE ER-2223        TO  EMI-ERROR
01041              MOVE AL-UNBON       TO  BEFFDTEA
01042              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01043      ELSE
01044          MOVE -1                 TO  BEFFDTEL
01045          MOVE ER-2220            TO  EMI-ERROR
01046          MOVE AL-UNBON           TO  BEFFDTEA
01047          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01048
01049      IF BCLAIML NOT = ZEROS
01050          MOVE AL-UANON           TO  BCLAIMA
01051          MOVE BCLAIMI            TO  PI-PNDC-CLAIM-NO
01052      ELSE
01053          MOVE ER-0209            TO  EMI-ERROR
01054          MOVE -1                 TO  BCLAIML
01055          MOVE AL-UABON           TO  BCLAIMA
01056          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01057
01058      IF BCLMTYPL NOT = ZEROS
01059          MOVE BCLMTYPI           TO  WS-VALID-CLAIM-TYPES
01060          IF VALID-CLAIM-TYPE
01061              MOVE AL-UANON       TO  BCLMTYPA
01062          ELSE
01063              MOVE ER-2445        TO  EMI-ERROR
01064              MOVE -1             TO  BCLMTYPL
01065              MOVE AL-UABON       TO  BCLMTYPA
01066              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01067      ELSE
01068          IF BMAINTI = 'A'
01069              MOVE ER-2446        TO  EMI-ERROR
01070              MOVE -1             TO  BCLMTYPL
01071              MOVE AL-UABON       TO  BCLMTYPA
01072              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01073
01074      IF BMAINTI = 'S'  OR  'D'
01075          GO TO 1100-EDIT-COMPLETE.
01076
01077      IF BFUTUREL NOT =  ZEROS
01078          IF WS-FUTURE-RESERVES-USED
CIDMOD             MOVE BFUTUREI         TO  WS-DEEDIT-FIELD
CIDMOD             PERFORM 8600-DEEDIT  THRU  8600-EXIT
CIDMOD             MOVE WS-DEEDIT-FIELD  TO  BFUTUREI
01083              MOVE AL-UNNON         TO  BFUTUREA
01084          ELSE
01085              MOVE ER-2467          TO  EMI-ERROR
01086              MOVE -1               TO  BFUTUREL
01087              MOVE AL-UNBON         TO  BFUTUREA
01088              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01089
01090      IF BPAYCURL NOT = ZEROS
01091          IF WS-PAY-TO-CURRENT-USED
CIDMOD             MOVE BPAYCURI         TO  WS-DEEDIT-FIELD
CIDMOD             PERFORM 8600-DEEDIT  THRU  8600-EXIT
CIDMOD             MOVE WS-DEEDIT-FIELD  TO  BPAYCURI
01096              MOVE AL-UNNON         TO  BPAYCURA
01097          ELSE
01098              MOVE ER-2468          TO  EMI-ERROR
01099              MOVE -1               TO  BPAYCURL
01100              MOVE AL-UNBON         TO  BPAYCURA
01101              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01102
01103      IF BIBNRL NOT = ZEROS
01104          IF WS-IBNR-RESERVES-USED
CIDMOD             MOVE BIBNRI           TO  WS-DEEDIT-FIELD
CIDMOD             PERFORM 8600-DEEDIT  THRU  8600-EXIT
CIDMOD             MOVE WS-DEEDIT-FIELD  TO  BIBNRI
01109              MOVE AL-UNNON         TO  BIBNRA
01110          ELSE
01111              MOVE ER-2469          TO  EMI-ERROR
01112              MOVE -1               TO  BIBNRL
01113              MOVE AL-UNBON         TO  BIBNRA
01114              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01115
01116      IF BMANUALL NOT = ZEROS
01117          IF WS-MANUAL-RESERVES-USED
CIDMOD             MOVE BMANUALI         TO  WS-DEEDIT-FIELD
CIDMOD             PERFORM 8600-DEEDIT  THRU  8600-EXIT
CIDMOD             MOVE WS-DEEDIT-FIELD  TO  BMANUALI
01122              MOVE AL-UNNON         TO  BMANUALA
01123          ELSE
01124              MOVE ER-2470          TO  EMI-ERROR
01125              MOVE -1               TO  BMANUALL
01126              MOVE AL-UNBON         TO  BMANUALA
01127              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01128
01129      IF BFORCEL NOT = ZEROS
01130          MOVE BFORCEI            TO  WS-VALID-FORCE-CD
01131          IF VALID-FORCE-CD
01132              MOVE AL-UANON       TO  BFORCEA
01133          ELSE
01134              MOVE ER-2815        TO  EMI-ERROR
01135              MOVE -1             TO  BFORCEL
01136              MOVE AL-UABON       TO  BFORCEA
01137              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01138
01139      IF BEOMDTL NOT = ZEROS
01140          MOVE AL-UNNON           TO  BEOMDTA
01141          MOVE BEOMDTI            TO  WS-DEEDIT-FIELD1
01142          
      * EXEC CICS BIF DEEDIT
01143 *             FIELD   (WS-DEEDIT-FIELD1)
01144 *             LENGTH  (11)
01145 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004669' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD1, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01146          MOVE WS-DEEDIT-FIELDV0  TO  BEOMDTO
01147                                      DC-GREG-DATE-1-MDY
01148          INSPECT BEOMDTO CONVERTING SPACES TO '/'
01149          MOVE 4                  TO  DC-OPTION-CODE
01150          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
01151          MOVE DC-GREG-DATE-1-MDY  TO  WS-DATE-TEST
01152          IF (DATE-CONVERSION-ERROR)
01153            OR (DC-DAYS-IN-MONTH NOT = WS-DATE-DA)
01154              MOVE -1             TO  BEOMDTL
01155              MOVE ER-0587        TO  EMI-ERROR
01156              MOVE AL-UNBON       TO  BEOMDTA
01157              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01158          ELSE
01159              MOVE DC-BIN-DATE-1  TO  WS-EL632-EOM-DATE.
01160
01161  1100-EDIT-COMPLETE.
01162      MOVE BMAINTI                TO  PI-MAINT.
01163
01164      IF PI-MAINT = 'C'
01165          IF PI-ERPNDC-KEY = PI-PREV-ERPNDC-KEY
01166              NEXT SENTENCE
01167          ELSE
01168              MOVE ER-2761        TO  EMI-ERROR
01169              MOVE -1             TO  BCARIERL
01170              MOVE AL-UABON       TO  BCARIERA
01171                                      BGROUPA
01172                                      BSTATEA
01173                                      BACCTA
01174                                      BCERTA
01175                                      BCRTSFXA
01176                                      BEFFDTEA
01177                                      BCLAIMA
01178            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01179
01180      IF EMI-ERROR NOT = ZEROS
01181          MOVE PI-PREV-ERPNDC-KEY  TO  PI-ERPNDC-KEY
01182          GO TO 8200-SEND-DATAONLY.
01183
01184      IF BMAINTI = 'A'
01185          GO TO 2000-ADD-RECORD.
01186
01187      IF BMAINTI = 'C'
01188          GO TO 3000-CHANGE-RECORD.
01189
01190      IF BMAINTI = 'K'
01191          PERFORM 3500-REWRITE-RECORD  THRU  3900-EXIT
01192          GO TO 3000-CHANGE-RECORD.
01193
01194      IF BMAINTI = 'D'
01195          GO TO 4000-DELETE-RECORD.
01196
01197      IF AMAINTI = 'S'
01198          MOVE ZEROS              TO  PI-RECORD-SEQUENCE
01199          MOVE '2'                TO  PI-PNDC-REC-TYP
01200          MOVE SPACES             TO  PI-PNDC-CHECK-NO
01201          GO TO 5000-BROWSE-CLAIMS-FILE.
01202  EJECT
01203  1200-VERIFY-CARRIER-ID.
01204      MOVE SPACES                 TO  ELCNTL-KEY.
01205      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01206      MOVE '6'                    TO  CNTL-REC-TYPE.
01207
01208      IF WS-CARRIER-SW = 'Y'
01209          MOVE WS-SV-CARRIER      TO  CNTL-CARRIER
01210      ELSE
01211          IF PI-MAP-NAME = EL632A
01212              MOVE ACARIERI       TO  CNTL-CARRIER
01213          ELSE
01214              MOVE BCARIERI       TO  CNTL-CARRIER.
01215
01216      MOVE +0                     TO  CNTL-SEQ.
01217
01218      
      * EXEC CICS HANDLE CONDITION
01219 *        NOTFND  (1200-NO-CARRIER)
01220 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00004745' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01221
01222      
      * EXEC CICS READ
01223 *        DATASET  (ELCNTL-FILE-ID)
01224 *        SET      (ADDRESS OF CONTROL-FILE)
01225 *        RIDFLD   (ELCNTL-KEY)
01226 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004749' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01227
01228      MOVE CF-RESERVE-CONTROLS    TO  WS-RESERVE-CONTROLS.
01229
01230      GO TO 1200-EXIT.
01231
01232  1200-NO-CARRIER.
01233      MOVE ER-2208                TO  EMI-ERROR.
01234
01235      IF PI-MAP-NAME = EL632A
01236          MOVE -1                 TO  ACARIERL
01237          MOVE AL-UABON           TO  ACARIERA
01238      ELSE
01239          MOVE -1                 TO  BCARIERL
01240          MOVE AL-UABON           TO  BCARIERA.
01241
01242      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01243
01244  1200-EXIT.
01245      EXIT.
01246  EJECT
01247  1300-VERIFY-STATE-ID.
01248      MOVE SPACES                 TO  ELCNTL-KEY.
01249      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01250      MOVE '3'                    TO  CNTL-REC-TYPE.
01251
01252      IF PI-MAP-NAME = EL632A
01253          MOVE ASTATEI            TO  CNTL-STATE
01254      ELSE
01255          MOVE BSTATEI            TO  CNTL-STATE.
01256
01257      MOVE +0                     TO  CNTL-SEQ.
01258
01259      
      * EXEC CICS HANDLE CONDITION
01260 *        NOTFND  (1300-NO-STATE)
01261 *    END-EXEC.
      *    MOVE '"$I                   ! % #00004786' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034373836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01262
01263      
      * EXEC CICS READ
01264 *        DATASET  (ELCNTL-FILE-ID)
01265 *        SET      (ADDRESS OF CONTROL-FILE)
01266 *        RIDFLD   (ELCNTL-KEY)
01267 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004790' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01268
01269      GO TO 1300-EXIT.
01270
01271  1300-NO-STATE.
01272      MOVE ER-2209                TO  EMI-ERROR.
01273
01274      IF PI-MAP-NAME = EL632A
01275          MOVE -1                 TO  ASTATEL
01276          MOVE AL-UABON           TO  ASTATEA
01277      ELSE
01278          MOVE -1                 TO  BSTATEL
01279          MOVE AL-UABON           TO  BSTATEA.
01280
01281      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01282
01283  1300-EXIT.
01284      EXIT.
01285  EJECT
01286  1400-VERIFY-ACCOUNT.
01287      IF PI-MAP-NAME = EL632B
01288          GO TO 1400-MAPB-DATA.
01289
01290      IF AMAINTI = 'D'  OR  'S'
01291          GO TO 1400-EXIT.
01292
01293      IF ACARIERL GREATER ZEROS
01294          MOVE ACARIERI           TO  ACCT-CARRIER
01295      ELSE
01296          MOVE SPACES             TO  ACCT-CARRIER.
01297
01298      IF AGROUPL GREATER ZEROS
01299          MOVE AGROUPI            TO  ACCT-GROUPING
01300      ELSE
01301          MOVE SPACES             TO  ACCT-GROUPING.
01302
01303      IF ASTATEL GREATER ZEROS
01304          MOVE ASTATEI            TO  ACCT-STATE
01305      ELSE
01306          MOVE SPACES             TO  ACCT-STATE.
01307
01308      MOVE AACCTI                 TO  ACCT-ACCOUNT.
01309
01310      GO TO 1400-READ-ACCOUNT.
01311
01312  1400-MAPB-DATA.
01313      IF BMAINTI = 'D'  OR  'S'
01314          GO TO 1400-EXIT.
01315
01316      IF BCARIERL GREATER ZEROS
01317          MOVE BCARIERI           TO  ACCT-CARRIER
01318      ELSE
01319          MOVE SPACES             TO  ACCT-CARRIER.
01320
01321      IF BGROUPL GREATER ZEROS
01322          MOVE BGROUPI            TO  ACCT-GROUPING
01323      ELSE
01324          MOVE SPACES             TO  ACCT-GROUPING.
01325
01326      IF BSTATEL GREATER ZEROS
01327          MOVE BSTATEI            TO  ACCT-STATE
01328      ELSE
01329          MOVE SPACES             TO  ACCT-STATE.
01330
01331      MOVE BACCTI                 TO  ACCT-ACCOUNT.
01332
01333  1400-READ-ACCOUNT.
01334      MOVE PI-COMPANY-CD          TO  ACCT-CO.
01335      MOVE LOW-VALUES             TO  ACCT-EXP-DATE.
01336
01337      
      * EXEC CICS HANDLE CONDITION
01338 *        NOTFND  (1400-ACCOUNT-INVALID)
01339 *    END-EXEC.
      *    MOVE '"$I                   ! & #00004864' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034383634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01340
01341      
      * EXEC CICS READ
01342 *        GTEQ
01343 *        DATASET  (ERACCT-FILE-ID)
01344 *        SET      (ADDRESS OF ACCOUNT-MASTER)
01345 *        RIDFLD   (ERACCT-KEY)
01346 *    END-EXEC.
      *    MOVE '&"S        G          (   #00004868' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01347
01348      MOVE AM-CONTROL-BY-VAR-GRP  TO  ERACCT-SAVE-KEY.
01349
01350      IF ERACCT-COMP-KEY NOT = ERACCT-SAVE-KEY
01351          GO TO 1400-ACCOUNT-INVALID.
01352
01353      MOVE AM-CARRIER             TO  WS-SV-CARRIER.
01354      MOVE AM-GROUPING            TO  WS-SV-GROUPING.
01355      MOVE AM-STATE               TO  WS-SV-STATE.
01356
01357      IF ST-ACCNT-CNTL  OR  ACCNT-CNTL
01358          MOVE 'Y'                TO  WS-CARRIER-SW
01359          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1200-EXIT.
01360
01361      GO TO 1400-EXIT.
01362
01363  1400-ACCOUNT-INVALID.
01364      IF PI-MAP-NAME = EL632A
01365          IF CARR-GROUP-ST-ACCNT-CNTL
01366              MOVE -1                     TO  ACARIERL
01367              MOVE AL-UABON               TO  ACARIERA
01368                                              AGROUPA
01369                                              ASTATEA
01370                                              AACCTA
01371          ELSE
01372              IF ST-ACCNT-CNTL
01373                  MOVE -1                 TO  ASTATEL
01374                  MOVE AL-UABON           TO  ASTATEA
01375                                              AACCTA
01376              ELSE
01377                  IF CARR-ST-ACCNT-CNTL
01378                      MOVE -1             TO  ACARIERL
01379                      MOVE AL-UABON       TO  ACARIERA
01380                                              ASTATEA
01381                                              AACCTA
01382                  ELSE
01383                      IF ACCNT-CNTL
01384                          MOVE -1         TO  AACCTL
01385                          MOVE AL-UABON   TO  AACCTA
01386                      ELSE
01387                          MOVE -1         TO  ACARIERL
01388                          MOVE AL-UABON   TO  ACARIERA
01389                                              AACCTA
01390      ELSE
01391          IF CARR-GROUP-ST-ACCNT-CNTL
01392              MOVE -1                     TO  BCARIERL
01393              MOVE AL-UABON               TO  BCARIERA
01394                                              BGROUPA
01395                                              BSTATEA
01396                                              BACCTA
01397          ELSE
01398              IF ST-ACCNT-CNTL
01399                  MOVE -1                 TO  BSTATEL
01400                  MOVE AL-UABON           TO  BSTATEA
01401                                              BACCTA
01402              ELSE
01403                  IF CARR-ST-ACCNT-CNTL
01404                      MOVE -1             TO  BCARIERL
01405                      MOVE AL-UABON       TO  BCARIERA
01406                                              BSTATEA
01407                                              BACCTA
01408                  ELSE
01409                      IF ACCNT-CNTL
01410                          MOVE -1         TO  BACCTL
01411                          MOVE AL-UABON   TO  BACCTA
01412                      ELSE
01413                          MOVE -1         TO  BCARIERL
01414                          MOVE AL-UABON   TO  BCARIERA
01415                                              BACCTA.
01416
01417      MOVE ER-2210                TO  EMI-ERROR.
01418
01419      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01420
01421  1400-EXIT.
01422      EXIT.
01423  EJECT
01424  2000-ADD-RECORD.
01425      
      * EXEC CICS HANDLE CONDITION
01426 *        DUPREC  (2900-DUPLICATE-RECORD)
01427 *    END-EXEC.
      *    MOVE '"$%                   ! '' #00004952' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01428
01429      
      * EXEC CICS GETMAIN
01430 *        SET      (ADDRESS OF PENDING-CLAIMS)
01431 *        LENGTH   (500)
01432 *        INITIMG  (GETMAIN-SPACE)
01433 *    END-EXEC.
           MOVE 500
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004956' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-CLAIMS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01434
01435      MOVE PI-COMPANY-CD          TO  PC-COMPANY-CD.
01436      MOVE PI-COMPANY-ID          TO  PC-COMPANY-ID.
01437      MOVE 'PC'                   TO  PC-RECORD-ID.
01438      MOVE ZEROS                  TO  PC-RECORD-SEQUENCE.
01439
01440      IF PI-MAP-NAME = EL632B
01441          GO TO 2100-ADD-RESERVE-RECORD.
01442
01443      MOVE '1'                    TO  PC-RECORD-TYPE.
01444
01445      IF ACARIERL NOT = ZEROS
01446          MOVE ACARIERI           TO  PC-CARRIER.
01447
01448      IF AGROUPL NOT = ZEROS
01449          MOVE AGROUPI            TO  PC-GROUPING.
01450
01451      IF ASTATEL NOT = ZEROS
01452          MOVE ASTATEI            TO  PC-STATE.
01453
01454      IF AACCTL NOT = ZEROS
01455          MOVE AACCTI             TO  PC-ACCOUNT.
01456
01457      MOVE WS-CONVERTED-EFFDT     TO  PC-CERT-EFF-DT.
01458      MOVE ACERTI                 TO  PC-CERT-PRIME.
01459
01460      IF ACRTSFXL NOT = ZEROS
01461          MOVE ACRTSFXI           TO  PC-CERT-SFX.
01462
01463      MOVE ACLAIMI                TO  PC-CLAIM-NO.
01464      MOVE ACHECKI                TO  PC-CHECK-NO.
01465      MOVE ACTYPEI                TO  PC-CLAIM-TYPE.
01466      MOVE APAYMNTI               TO  PC-CLAIM-PAYMENT.
01467      MOVE APTYPEI                TO  PC-PAYMENT-TYPE.
01468
01469      IF AINCURL NOT = ZEROS
01470          MOVE WS-CONVERTED-INCUR  TO  PC-INCURRED-DT
01471      ELSE
01472          MOVE LOW-VALUES          TO  PC-INCURRED-DT.
01473
01474      IF AREPORTL NOT = ZEROS
01475          MOVE WS-CONVERTED-REPORT     TO  PC-REPORTED-DT
01476      ELSE
01477          IF AINCURL NOT = ZEROS
01478              MOVE WS-CONVERTED-INCUR  TO  PC-REPORTED-DT
01479          ELSE
01480              MOVE LOW-VALUES          TO  PC-REPORTED-DT.
01481
01482      IF APAIDL NOT =  ZEROS
01483          MOVE WS-CONVERTED-PAID  TO  PC-PAYMENT-DT
01484      ELSE
01485          MOVE LOW-VALUES         TO  PC-PAYMENT-DT.
01486
01487      IF APTHRUL NOT = ZEROS
01488          MOVE WS-CONVERTED-PTHRU  TO  PC-PAID-THRU-DT
01489      ELSE
01490          MOVE LOW-VALUES          TO  PC-PAID-THRU-DT.
01491
01492      IF ADAYSL NOT = ZEROS
01493          MOVE ADAYSI             TO  PC-NO-OF-DAYS-PAID
01494      ELSE
01495          MOVE ZEROS              TO  PC-NO-OF-DAYS-PAID.
01496
01497      IF AAGEL NOT = ZEROS
01498          MOVE AAGEI              TO  PC-AGE-AT-CLAIM
01499      ELSE
01500          MOVE ZEROS              TO  PC-AGE-AT-CLAIM.
01501
01502      IF ACAUSEL NOT = ZEROS
01503          MOVE ACAUSEI            TO  PC-CAUSE-CODE.
01504
01505      IF AFORCEL NOT = ZEROS
01506          MOVE AFORCEI            TO  PC-FORCE-CODE.
01507
01508      MOVE ZEROS                  TO  PC-FUTURE-RESERVE-AMT
01509                                      PC-IBNR-RESERVE-AMT
01510                                      PC-PTC-RESERVE-AMT
01511                                      PC-MANUAL-RESERVE-AMT
01512                                      PC-REMAINING-BENEFIT
01513                                      PC-REMAINING-TERM.
01514
01515      GO TO 2200-WRITE-THE-RECORD.
01516
01517  2100-ADD-RESERVE-RECORD.
01518      MOVE '2'                    TO  PC-RECORD-TYPE.
01519
01520      IF BCARIERL NOT = ZEROS
01521          MOVE BCARIERI           TO  PC-CARRIER.
01522
01523      IF BGROUPL NOT = ZEROS
01524          MOVE BGROUPI            TO  PC-GROUPING.
01525
01526      IF BSTATEL NOT = ZEROS
01527          MOVE BSTATEI            TO  PC-STATE.
01528
01529      IF BACCTL NOT = ZEROS
01530          MOVE BACCTI             TO  PC-ACCOUNT.
01531
01532      MOVE WS-CONVERTED-EFFDT     TO  PC-CERT-EFF-DT.
01533      MOVE BCERTI                 TO  PC-CERT-PRIME.
01534
01535      IF BCRTSFXL NOT = ZEROS
01536          MOVE BCRTSFXI           TO  PC-CERT-SFX.
01537
01538      MOVE BCLAIMI                TO  PC-CLAIM-NO.
01539      MOVE BCLMTYPI               TO  PC-CLAIM-TYPE.
01540
01541      IF BFUTUREL NOT = ZEROS
01542          MOVE BFUTUREI           TO  PC-FUTURE-RESERVE-AMT
01543      ELSE
01544          MOVE ZEROS              TO  PC-FUTURE-RESERVE-AMT.
01545
01546      IF BPAYCURL NOT = ZEROS
01547          MOVE BPAYCURI           TO  PC-PTC-RESERVE-AMT
01548      ELSE
01549          MOVE ZEROS              TO  PC-PTC-RESERVE-AMT.
01550
01551      IF BIBNRL NOT = ZEROS
01552          MOVE BIBNRI             TO  PC-IBNR-RESERVE-AMT
01553      ELSE
01554          MOVE ZEROS              TO  PC-IBNR-RESERVE-AMT.
01555
01556      IF BMANUALL NOT = ZEROS
01557          MOVE BMANUALI           TO  PC-MANUAL-RESERVE-AMT
01558      ELSE
01559          MOVE ZEROS              TO  PC-MANUAL-RESERVE-AMT.
01560
01561      IF BFORCEL NOT = ZEROS
01562          MOVE BFORCEI            TO  PC-FORCE-CODE.
01563
01564      MOVE ZEROS                  TO  PC-CLAIM-PAYMENT.
01565
01566  2200-WRITE-THE-RECORD.
01567      MOVE WS-SV-CARRIER          TO  PC-SV-CARRIER.
01568      MOVE WS-SV-GROUPING         TO  PC-SV-GROUPING.
01569      MOVE WS-SV-STATE            TO  PC-SV-STATE.
01570      MOVE ZEROS                  TO  PC-CC-PRIOR-DEATH-AMT
01571                                      PC-CC-PRIOR-LUMP-PMT.
01572      MOVE PI-PROCESSOR-ID        TO  PC-LAST-MAINT-BY.
01573      MOVE EIBTIME                TO  PC-LAST-MAINT-HHMMSS.
01574      MOVE WS-CURRENT-BIN-DT      TO  PC-LAST-MAINT-DT
01575                                      PC-INPUT-DT.
01576      MOVE LOW-VALUES             TO  PC-CREDIT-ACCEPT-DT.
01577      MOVE PI-CR-MONTH-END-DT     TO  PC-CREDIT-SELECT-DT.
01578
01579      IF WS-EL632-EOM-DATE NOT = SPACES AND ZEROS
01580        AND  LOW-VALUES
01581          MOVE WS-EL632-EOM-DATE  TO  PC-CREDIT-SELECT-DT.
01582
01583      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.
01584      MOVE 'A'                    TO  JP-RECORD-TYPE
01585      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.
01586      MOVE PC-CONTROL-PRIMARY     TO  PI-ERPNDC-KEY.
01587
01588      PERFORM 9800-LINK-CLAIMS-EDIT.
01589
01590      MOVE LOW-VALUES             TO  EL632AI.
01591
01592      PERFORM 7000-FORMAT-ERRORS  THRU  7090-EXIT.
01593
01594      PERFORM 6000-FORMAT-RECORD  THRU  6900-EXIT.
01595
01596      IF PC-CLAIMS
01597          MOVE 'C'                TO  AMAINTI
01598          MOVE AL-UANON           TO  AMAINTA
01599          IF NOT PC-FATAL-ERRORS
01600            AND  (PC-FORCE-ER-CD = 'F'  OR  ' ')
01601              MOVE -1             TO  AMAINTL
01602      ELSE
01603          MOVE 'C'                TO  BMAINTI
01604          MOVE AL-UANON           TO  BMAINTA
01605          IF NOT PC-FATAL-ERRORS
01606            AND  (PC-FORCE-ER-CD = 'F'  OR  ' ')
01607              MOVE -1             TO  BMAINTL.
01608
01609  2300-WRITE.
01610      
      * EXEC CICS WRITE
01611 *        DATASET  (ERPNDC-FILE-ID)
01612 *        FROM     (PENDING-CLAIMS)
01613 *        RIDFLD   (PC-CONTROL-PRIMARY)
01614 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-CLAIMS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005137' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PENDING-CLAIMS, 
                 DFHEIV11, 
                 PC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01615
01616      PERFORM 8400-LOG-JOURNAL-RECORD.
01617
01618      IF NOT EMI-NO-ERRORS
01619          GO TO 8200-SEND-DATAONLY.
01620
01621      MOVE ER-0000                TO  EMI-ERROR
01622
01623      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01624
01625      GO TO 8100-SEND-INITIAL-MAP.
01626
01627  2900-DUPLICATE-RECORD.
01628      MOVE 1                      TO  EMI-SUB
01629                                      EMI-SWITCH1
01630                                      EMI-SWITCH-AREA-1
01631                                      EMI-SWITCH-AREA-2
01632      MOVE SPACES                 TO  EMI-ERROR-LINES.
01633
01634      IF PI-MAP-NAME = EL632A
01635          MOVE 'A'                TO  AMAINTI
01636          MOVE 1                  TO  AMAINTL
01637          MOVE ER-2563            TO  EMI-ERROR
01638          MOVE -1                 TO  ACHECKL
01639          MOVE AL-UABON           TO  AACCTA
01640                                      ACERTA
01641                                      ACRTSFXA
01642                                      AEFFDTEA
01643                                      ACLAIMA
01644                                      ACHECKA
01645      ELSE
01646          MOVE 'A'                TO  BMAINTI
01647          MOVE 1                  TO  BMAINTL
01648          MOVE ER-2574            TO  EMI-ERROR
01649          MOVE -1                 TO  BCARIERL
01650          MOVE AL-UABON           TO  BACCTA
01651                                      BCERTA
01652                                      BCRTSFXA
01653                                      BEFFDTEA
01654                                      BCLAIMA.
01655
01656      IF CARR-GROUP-ST-ACCNT-CNTL
01657          IF PI-MAP-NAME = EL632A
01658              MOVE AL-UABON       TO  ACARIERA
01659                                      AGROUPA
01660                                      ASTATEA
01661          ELSE
01662              MOVE AL-UABON       TO  BCARIERA
01663                                      BGROUPA
01664                                      BSTATEA.
01665
01666      IF CARR-ST-ACCNT-CNTL
01667          IF PI-MAP-NAME = EL632A
01668              MOVE AL-UABON       TO  ACARIERA
01669                                      ASTATEA
01670          ELSE
01671              MOVE AL-UABON       TO  BCARIERA
01672                                      BSTATEA.
01673
01674      IF CARR-ACCNT-CNTL
01675          IF PI-MAP-NAME = EL632A
01676              MOVE AL-UABON       TO  ACARIERA
01677          ELSE
01678              MOVE AL-UABON       TO  BCARIERA.
01679
01680      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01681
01682      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.
01683
01684      GO TO 8200-SEND-DATAONLY.
01685  EJECT
01686  3000-CHANGE-RECORD.
01687      
      * EXEC CICS HANDLE CONDITION
01688 *        NOTFND  (3400-RECORD-NOTFND)
01689 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00005214' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035323134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01690
01691      
      * EXEC CICS READ
01692 *        DATASET  (ERPNDC-FILE-ID)
01693 *        SET      (ADDRESS OF PENDING-CLAIMS)
01694 *        RIDFLD   (PI-ERPNDC-KEY)
01695 *        UPDATE
01696 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005218' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-CLAIMS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01697
01698      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.
01699      MOVE 'B'                    TO  JP-RECORD-TYPE
01700      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.
01701
01702      PERFORM 8400-LOG-JOURNAL-RECORD.
01703
01704      IF PI-MAP-NAME = EL632B
01705          GO TO 3100-CHANGE-RESERVE-DATA.
01706
01707      IF ACTYPEL NOT = ZEROS
01708          MOVE ACTYPEI            TO  PC-CLAIM-TYPE.
01709
01710      IF APAYMNTL NOT = ZEROS
01711          MOVE APAYMNTI           TO  PC-CLAIM-PAYMENT.
01712
01713      IF APTYPEL NOT = ZEROS
01714          MOVE APTYPEI            TO  PC-PAYMENT-TYPE.
01715
01716      IF AINCURL NOT = ZEROS
01717          MOVE WS-CONVERTED-INCUR  TO  PC-INCURRED-DT.
01718
01719      IF AREPORTL NOT = ZEROS
01720          MOVE WS-CONVERTED-REPORT  TO  PC-REPORTED-DT.
01721
01722      IF APAIDL NOT = ZEROS
01723          MOVE WS-CONVERTED-PAID  TO  PC-PAYMENT-DT.
01724
01725      IF APTHRUL NOT = ZEROS
01726          MOVE WS-CONVERTED-PTHRU  TO  PC-PAID-THRU-DT.
01727
01728      IF ADAYSL NOT = ZEROS
01729          MOVE ADAYSI             TO  PC-NO-OF-DAYS-PAID.
01730
01731      IF AAGEL NOT = ZEROS
01732          MOVE AAGEI              TO  PC-AGE-AT-CLAIM.
01733
01734      IF ACAUSEL NOT = ZEROS
01735          MOVE ACAUSEI            TO  PC-CAUSE-CODE.
01736
01737      IF AFORCEL NOT = ZEROS
01738          MOVE AFORCEI            TO  PC-FORCE-CODE.
01739
01740      GO TO 3200-REWRITE-THE-RECORD.
01741  EJECT
01742  3100-CHANGE-RESERVE-DATA.
01743      IF BFUTUREL NOT = ZEROS
01744          MOVE BFUTUREI           TO  PC-FUTURE-RESERVE-AMT.
01745
01746      IF BPAYCURL NOT = ZEROS
01747          MOVE BPAYCURI           TO  PC-PTC-RESERVE-AMT.
01748
01749      IF BIBNRL NOT = ZEROS
01750          MOVE BIBNRI             TO  PC-IBNR-RESERVE-AMT.
01751
01752      IF BMANUALL NOT = ZEROS
01753          MOVE BMANUALI           TO  PC-MANUAL-RESERVE-AMT.
01754
01755      IF BFORCEL NOT = ZEROS
01756          MOVE BFORCEI            TO  PC-FORCE-CODE.
01757
01758  3200-REWRITE-THE-RECORD.
01759      IF WS-EL632-EOM-DATE NOT = SPACES AND ZEROS
01760        AND  LOW-VALUES
01761          MOVE WS-EL632-EOM-DATE   TO  PC-CREDIT-SELECT-DT.
01762
01763      MOVE PI-PROCESSOR-ID        TO  PC-LAST-MAINT-BY.
01764      MOVE EIBTIME                TO  PC-LAST-MAINT-HHMMSS.
01765      MOVE WS-CURRENT-BIN-DT      TO  PC-LAST-MAINT-DT.
01766      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.
01767      MOVE 'C'                    TO  JP-RECORD-TYPE
01768      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.
01769      MOVE PC-CONTROL-PRIMARY     TO  PI-ERPNDC-KEY.
01770
01771      PERFORM 9800-LINK-CLAIMS-EDIT.
01772
01773      MOVE LOW-VALUES             TO  EL632AI.
01774
01775      PERFORM 7000-FORMAT-ERRORS  THRU  7090-EXIT.
01776
01777      PERFORM 6000-FORMAT-RECORD  THRU  6900-EXIT.
01778
01779      IF PC-CLAIMS
01780          MOVE 'C'                TO  AMAINTI
01781                                      PI-MAINT
01782          MOVE AL-UANON           TO  AMAINTA
01783          IF NOT PC-FATAL-ERRORS
01784            AND  (PC-FORCE-ER-CD = 'F'  OR  ' ')
01785              MOVE -1             TO  AMAINTL
01786      ELSE
01787          MOVE 'C'                TO  BMAINTI
01788                                      PI-MAINT
01789          MOVE AL-UANON           TO  BMAINTA
01790          IF NOT PC-FATAL-ERRORS
01791            AND  (PC-FORCE-ER-CD = 'F'  OR  ' ')
01792              MOVE -1             TO  BMAINTL.
01793
01794      
      * EXEC CICS REWRITE
01795 *        DATASET  (ERPNDC-FILE-ID)
01796 *        FROM     (PENDING-CLAIMS)
01797 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-CLAIMS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005321' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PENDING-CLAIMS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01798
01799      PERFORM 8400-LOG-JOURNAL-RECORD.
01800
01801      IF NOT EMI-NO-ERRORS
01802          IF PI-MAP-NAME = EL632A
01803              MOVE 'C'            TO  AMAINTI
01804              GO TO 8200-SEND-DATAONLY
01805          ELSE
01806              MOVE 'C'            TO  BMAINTI
01807              GO TO 8200-SEND-DATAONLY.
01808
01809      MOVE ER-0000                TO  EMI-ERROR.
01810
01811      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01812
01813      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.
01814
01815      GO TO 8100-SEND-INITIAL-MAP.
01816
01817  3400-RECORD-NOTFND.
01818      MOVE ER-2465                TO  EMI-ERROR.
01819      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.
01820
01821      IF PI-MAP-NAME = EL632A
01822          MOVE -1                 TO  ACARIERL
01823      ELSE
01824          MOVE -1                 TO  BCARIERL.
01825
01826      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01827
01828      GO TO 8200-SEND-DATAONLY.
01829  EJECT
01830  3500-REWRITE-RECORD.
01831
01832      
      * EXEC CICS HANDLE CONDITION
01833 *        NOTFND  (3600-DELETE-OLDREC)
01834 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00005359' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035333539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01835
01836      
      * EXEC CICS READ
01837 *        DATASET  (ERPNDC-FILE-ID)
01838 *        SET      (ADDRESS OF PENDING-CLAIMS)
01839 *        RIDFLD   (PI-ERPNDC-KEY)
01840 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005363' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-CLAIMS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01841
01842      GO TO 3800-DUPLICATE-RECORD.
01843
01844  3600-DELETE-OLDREC.
01845      
      * EXEC CICS HANDLE CONDITION
01846 *        NOTFND  (3700-REBUILD-KEY)
01847 *    END-EXEC.
      *    MOVE '"$I                   ! * #00005372' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035333732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01848
01849      
      * EXEC CICS READ
01850 *        DATASET  (ERPNDC-FILE-ID)
01851 *        SET      (ADDRESS OF PENDING-CLAIMS)
01852 *        RIDFLD   (PI-PREV-ERPNDC-KEY)
01853 *        UPDATE
01854 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005376' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-PREV-ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-CLAIMS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01855
01856      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.
01857      MOVE 'D'                    TO  JP-RECORD-TYPE.
01858      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.
01859
01860      
      * EXEC CICS DELETE
01861 *         DATASET  (ERPNDC-FILE-ID)
01862 *    END-EXEC.
      *    MOVE '&(                    &   #00005387' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01863
01864      PERFORM 8400-LOG-JOURNAL-RECORD.
01865
01866  3700-REBUILD-KEY.
01867      
      * EXEC CICS GETMAIN
01868 *        SET      (ADDRESS OF PENDING-CLAIMS)
01869 *        LENGTH   (500)
01870 *        INITIMG  (GETMAIN-SPACE)
01871 *    END-EXEC.
           MOVE 500
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00005394' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-CLAIMS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01872
01873      MOVE JP-RECORD-AREA         TO  PENDING-CLAIMS.
01874
01875      IF PI-MAP-NAME = EL632B
01876          GO TO 3750-REWRITE-RESERVE-REC.
01877
01878      IF ACARIERL NOT = ZEROS
01879          MOVE ACARIERI           TO  PC-CARRIER.
01880
01881      IF AGROUPL NOT = ZEROS
01882          MOVE AGROUPI            TO  PC-GROUPING.
01883
01884      IF ASTATEL NOT = ZEROS
01885          MOVE ASTATEI            TO  PC-STATE.
01886
01887      IF AACCTL NOT = ZEROS
01888          MOVE AACCTI             TO  PC-ACCOUNT.
01889
01890      IF  AEFFDTEL NOT = ZEROS
01891          MOVE WS-CONVERTED-EFFDT  TO  PC-CERT-EFF-DT.
01892
01893      IF  ACERTL NOT = ZEROS
01894          MOVE ACERTI             TO  PC-CERT-PRIME.
01895
01896      IF ACRTSFXL NOT = ZEROS
01897          MOVE ACRTSFXI           TO  PC-CERT-SFX.
01898
01899      MOVE ACLAIMI                TO  PC-CLAIM-NO.
01900      MOVE ACHECKI                TO  PC-CHECK-NO.
01901
01902      GO TO 3790-WRITE-NEW-RECORD.
01903
01904  3750-REWRITE-RESERVE-REC.
01905      IF BCARIERL NOT = ZEROS
01906          MOVE BCARIERI           TO  PC-CARRIER.
01907
01908      IF BGROUPL NOT = ZEROS
01909          MOVE BGROUPI            TO  PC-GROUPING.
01910
01911      IF BSTATEL NOT = ZEROS
01912          MOVE BSTATEI            TO  PC-STATE.
01913
01914      IF BACCTL NOT = ZEROS
01915          MOVE BACCTI             TO  PC-ACCOUNT.
01916
01917      IF BEFFDTEL NOT = ZEROS
01918          MOVE WS-CONVERTED-EFFDT  TO  PC-CERT-EFF-DT.
01919
01920      IF BCERTL NOT = ZEROS
01921          MOVE BCERTI             TO  PC-CERT-PRIME.
01922
01923      IF BCRTSFXL NOT = ZEROS
01924          MOVE BCRTSFXI           TO  PC-CERT-SFX.
01925
01926      MOVE BCLAIMI                TO  PC-CLAIM-NO.
01927
01928  3790-WRITE-NEW-RECORD.
01929      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.
01930      MOVE 'A'                    TO  JP-RECORD-TYPE.
01931      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.
01932      MOVE PC-CONTROL-PRIMARY     TO  PI-ERPNDC-KEY.
01933
01934      
      * EXEC CICS WRITE
01935 *        DATASET  (ERPNDC-FILE-ID)
01936 *        FROM     (PENDING-CLAIMS)
01937 *        RIDFLD   (PC-CONTROL-PRIMARY)
01938 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-CLAIMS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005461' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PENDING-CLAIMS, 
                 DFHEIV11, 
                 PC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01939
01940      PERFORM 8400-LOG-JOURNAL-RECORD.
01941
01942      GO TO 3900-EXIT.
01943
01944  3800-DUPLICATE-RECORD.
01945      MOVE ER-2563                TO  EMI-ERROR.
01946
01947      IF PI-MAP-NAME = EL632A
01948          MOVE AL-UABON           TO  ACARIERA
01949                                      AGROUPA
01950                                      ASTATEA
01951                                      AACCTA
01952                                      ACERTA
01953                                      ACRTSFXA
01954                                      AEFFDTEA
01955                                      ACLAIMA
01956                                      ACTYPEA
01957                                      ACHECKA
01958          MOVE -1                 TO  ACARIERL
01959      ELSE
01960          MOVE AL-UABON           TO  BCARIERA
01961                                      BGROUPA
01962                                      BSTATEA
01963                                      BACCTA
01964                                      BEFFDTEA
01965                                      BCERTA
01966                                      BCRTSFXA
01967                                      BCLAIMA
01968          MOVE -1                 TO  BCARIERL.
01969
01970      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01971
01972      GO TO 8200-SEND-DATAONLY.
01973
01974  3900-EXIT.
01975       EXIT.
01976  EJECT
01977  4000-DELETE-RECORD.
01978      
      * EXEC CICS HANDLE CONDITION
01979 *        NOTFND  (4900-RECORD-NOTFND)
01980 *    END-EXEC.
      *    MOVE '"$I                   ! + #00005505' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035353035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01981
01982      MOVE 'D'                    TO  JP-RECORD-TYPE.
01983      MOVE PENDING-CLAIMS         TO  JP-RECORD-AREA.
01984
01985      PERFORM 8400-LOG-JOURNAL-RECORD.
01986
01987      
      * EXEC CICS DELETE
01988 *        DATASET  (ERPNDC-FILE-ID)
01989 *        RIDFLD   (PI-ERPNDC-KEY)
01990 *    END-EXEC.
      *    MOVE '&(  R                 &   #00005514' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 PI-ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01991
01992      MOVE LOW-VALUES             TO  EL632AO.
01993      MOVE ER-0000                TO  EMI-ERROR
01994
01995      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01996
01997      GO TO 8100-SEND-INITIAL-MAP.
01998
01999  4900-RECORD-NOTFND.
02000      MOVE ER-2465                TO  EMI-ERROR.
02001      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.
02002
02003      IF PI-MAP-NAME = EL632A
02004          MOVE -1                 TO  ACARIERL
02005      ELSE
02006          MOVE -1                 TO  BCARIERL.
02007
02008      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02009
02010      GO TO 8200-SEND-DATAONLY.
02011  EJECT
02012  5000-BROWSE-CLAIMS-FILE.
02013      
      * EXEC CICS HANDLE CONDITION
02014 *        NOTFND   (5800-NO-RECORD)
02015 *        ENDFILE  (5900-END-OF-FILE)
02016 *    END-EXEC.
      *    MOVE '"$I''                  ! , #00005540' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02017
02018      MOVE PI-ERPNDC-KEY          TO  ERPNDC-KEY.
02019
02020      IF EIBAID = DFHPF2
02021          GO TO 5100-BROWSE-BKWD.
02022
02023  5010-READ-LOOP.
02024      IF EIBAID = DFHPF1  OR  DFHPF9
02025          ADD +1                  TO  PNDC-RECORD-SEQ.
02026
02027      
      * EXEC CICS READ
02028 *        DATASET  (ERPNDC-FILE-ID)
02029 *        SET      (ADDRESS OF PENDING-CLAIMS)
02030 *        RIDFLD   (ERPNDC-KEY)
02031 *        GTEQ
02032 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005554' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-CLAIMS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02033
02034      IF PC-COMPANY-CD NOT = PI-COMPANY-CD
02035          GO TO 5900-END-OF-FILE.
02036
02037      IF EIBAID = DFHPF1  OR  DFHPF9
02038          NEXT SENTENCE
02039      ELSE
02040          GO TO 5020-PROCESS-CLAIMS-RECORD.
02041
02042      IF PI-NO-CARRIER-SECURITY
02043          IF PI-NO-ACCOUNT-SECURITY
02044              GO TO 5020-PROCESS-CLAIMS-RECORD.
02045
02046      IF PI-CARRIER-SECURITY GREATER SPACES
02047          IF PI-CARRIER-SECURITY = PC-CARRIER
02048              NEXT SENTENCE
02049          ELSE
02050              MOVE PC-CONTROL-PRIMARY  TO  ERPNDC-KEY
02051              GO TO 5010-READ-LOOP.
02052
02053      IF PI-ACCOUNT-SECURITY GREATER SPACES
02054          IF PI-ACCOUNT-SECURITY = PC-ACCOUNT
02055              NEXT SENTENCE
02056          ELSE
02057              MOVE PC-CONTROL-PRIMARY  TO  ERPNDC-KEY
02058              GO TO 5010-READ-LOOP.
02059
02060  5020-PROCESS-CLAIMS-RECORD.
02061      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES
02062          IF WS-MAINT = 'S'
02063              GO TO 5800-NO-RECORD
02064          ELSE
02065              MOVE PC-CONTROL-PRIMARY  TO  ERPNDC-KEY
02066              GO TO 5010-READ-LOOP.
02067
02068      IF WS-MAINT = 'S'
02069          IF PNDC-CARRIER     = PC-CARRIER      AND
02070             PNDC-GROUPING    = PC-GROUPING     AND
02071             PNDC-STATE       = PC-STATE        AND
02072             PNDC-ACCOUNT     = PC-ACCOUNT      AND
02073             PNDC-CERT-EFF-DT = PC-CERT-EFF-DT  AND
02074             PNDC-CERT-NO     = PC-CERT-NO      AND
02075             PNDC-CLAIM-NO    = PC-CLAIM-NO     AND
02076             PNDC-CHECK-NO    = PC-CHECK-NO     AND
02077             PNDC-RECORD-TYPE = PC-RECORD-TYPE
02078              GO TO 5200-FORMAT-SCREEN
02079          ELSE
02080              GO TO 5800-NO-RECORD.
02081
02082      IF EIBAID = DFHPF9
02083          IF PC-FATAL-ERRORS
02084            OR PC-UNFORCED-ERRORS
02085              NEXT SENTENCE
02086          ELSE
02087              MOVE PC-CONTROL-PRIMARY  TO  ERPNDC-KEY
02088              GO TO 5010-READ-LOOP.
02089
02090      GO TO 5200-FORMAT-SCREEN.
02091  EJECT
02092  5100-BROWSE-BKWD.
02093      
      * EXEC CICS STARTBR
02094 *        DATASET  (ERPNDC-FILE-ID)
02095 *        RIDFLD   (ERPNDC-KEY)
02096 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005620' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02097
02098      
      * EXEC CICS READPREV
02099 *        DATASET  (ERPNDC-FILE-ID)
02100 *        SET      (ADDRESS OF PENDING-CLAIMS)
02101 *        RIDFLD   (ERPNDC-KEY)
02102 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005625' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-CLAIMS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02103
02104  5110-READ-LOOP.
02105      IF PI-FILE-EOF
02106          MOVE SPACE              TO  PI-EOF-SW
02107      ELSE
02108          
      * EXEC CICS READPREV
02109 *            DATASET  (ERPNDC-FILE-ID)
02110 *            SET      (ADDRESS OF PENDING-CLAIMS)
02111 *            RIDFLD   (ERPNDC-KEY)
02112 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005635' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-CLAIMS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02113
02114      IF PC-COMPANY-CD NOT = PI-COMPANY-CD
02115          GO TO 5900-END-OF-FILE.
02116
02117      IF PI-NO-CARRIER-SECURITY
02118          IF PI-NO-ACCOUNT-SECURITY
02119              GO TO 5120-PROCESS-CLAIMS-RECORD.
02120
02121      IF PI-CARRIER-SECURITY GREATER SPACES
02122          IF PI-CARRIER-SECURITY = PC-CARRIER
02123              NEXT SENTENCE
02124          ELSE
02125              GO TO 5110-READ-LOOP.
02126
02127      IF PI-ACCOUNT-SECURITY GREATER SPACES
02128          IF PI-ACCOUNT-SECURITY = PC-ACCOUNT
02129              NEXT SENTENCE
02130          ELSE
02131              GO TO 5110-READ-LOOP.
02132
02133  5120-PROCESS-CLAIMS-RECORD.
02134      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES
02135          GO TO 5110-READ-LOOP.
02136
02137  5200-FORMAT-SCREEN.
02138      MOVE PC-CONTROL-PRIMARY     TO  PI-ERPNDC-KEY.
02139      MOVE PC-SV-CARRIER          TO  PI-CARRIER.
02140      MOVE PC-SV-GROUPING         TO  PI-GROUPING.
02141      MOVE PC-SV-STATE            TO  PI-STATE.
02142      MOVE PC-ACCOUNT             TO  PI-ACCOUNT.
02143      MOVE PC-CERT-NO             TO  PI-CERT-NO.
02144      MOVE PC-CERT-EFF-DT         TO  PI-CERT-EFF-DT.
02145      MOVE LOW-VALUES             TO  EL632AI.
02146
02147      PERFORM 7000-FORMAT-ERRORS  THRU  7090-EXIT.
02148
02149      PERFORM 6000-FORMAT-RECORD  THRU  6900-EXIT.
02150
02151      IF PC-CLAIMS
02152          IF NOT PI-HAS-CLAS-IC-CLAIM
02153              MOVE 'C'            TO  AMAINTI
02154              MOVE AL-UANON       TO  AMAINTA
02155              IF NOT PC-FATAL-ERRORS
02156                AND (PC-FORCE-ER-CD = 'F' OR  ' ')
02157                  MOVE -1         TO  AMAINTL
02158              ELSE
02159                  NEXT SENTENCE
02160          ELSE
02161              MOVE 'S'            TO  AMAINTI
02162              MOVE AL-UANON       TO  AMAINTA
02163              IF NOT PC-FATAL-ERRORS
02164                AND (PC-FORCE-ER-CD = 'F' OR  ' ')
02165                  MOVE -1         TO  AMAINTL
02166              ELSE
02167                  NEXT SENTENCE
02168      ELSE
02169          IF NOT PI-HAS-CLAS-IC-CLAIM
02170              MOVE 'C'            TO  BMAINTI
02171              MOVE AL-UANON       TO  BMAINTA
02172              IF NOT PC-FATAL-ERRORS
02173                AND (PC-FORCE-ER-CD = 'F' OR  ' ')
02174                  MOVE -1         TO  BMAINTL
02175              ELSE
02176                  NEXT SENTENCE
02177          ELSE
02178              MOVE 'S'            TO  BMAINTI
02179              MOVE AL-UANON       TO  BMAINTA
02180              IF NOT PC-FATAL-ERRORS
02181                AND (PC-FORCE-ER-CD = 'F' OR  ' ')
02182                  MOVE -1         TO  BMAINTL.
02183
02184      GO TO 8100-SEND-INITIAL-MAP.
02185
02186  5800-NO-RECORD.
02187      IF EIBAID = DFHPF1 OR  DFHPF9
02188          GO TO 5900-END-OF-FILE.
02189
02190      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.
02191      MOVE ER-2465                TO  EMI-ERROR.
02192      MOVE PI-PREV-ERPNDC-KEY     TO  PI-ERPNDC-KEY.
02193
02194      IF PI-MAP-NAME = EL632A
02195          MOVE -1                 TO  ACARIERL
02196      ELSE
02197          MOVE -1                 TO  BCARIERL.
02198
02199      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02200
02201      GO TO 8200-SEND-DATAONLY.
02202
02203  5900-END-OF-FILE.
02204      IF EIBAID = DFHPF1 OR  DFHPF9
02205          MOVE 'Y'                TO  PI-EOF-SW
02206          MOVE -1                 TO  AMAINTL
02207          MOVE ER-2237            TO  EMI-ERROR
02208      ELSE
02209          MOVE SPACES             TO  PI-ERPNDC-KEY
02210          MOVE ER-2238            TO  EMI-ERROR.
02211
02212      IF PI-MAP-NAME = EL632A
02213          MOVE -1                 TO  ACARIERL
02214      ELSE
02215          MOVE -1                 TO  BCARIERL.
02216
02217      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
02218
02219      IF PI-MAP-NAME = EL632A
02220          IF EMI-ERROR = ER-2238 OR  ER-2237
02221              GO TO 8100-SEND-INITIAL-MAP.
02222
02223      IF PI-MAP-NAME = EL632B
02224          IF EMI-ERROR = ER-2238 OR  ER-2237
02225              GO TO 8110-SEND-INITIAL-MAPB.
02226
02227      GO TO 8200-SEND-DATAONLY.
02228  EJECT
02229  6000-FORMAT-RECORD.
02230      IF PC-RESERVES
02231          GO TO 6500-FORMAT-RESERVE-DATA.
02232
02233      MOVE EL632A                 TO  PI-MAP-NAME.
02234      MOVE PC-ACCOUNT             TO  AACCTO.
02235
02236      IF CARR-GROUP-ST-ACCNT-CNTL
02237          MOVE PC-CARRIER               TO  ACARIERO
02238          MOVE PC-GROUPING              TO  AGROUPO
02239          MOVE PC-STATE                 TO  ASTATEO
02240      ELSE
02241          IF CARR-ST-ACCNT-CNTL
02242              MOVE PC-CARRIER           TO  ACARIERO
02243              MOVE PC-STATE             TO  ASTATEO
02244          ELSE
02245              IF CARR-ACCNT-CNTL
02246                  MOVE PC-CARRIER       TO  ACARIERO
02247              ELSE
02248                  IF ST-ACCNT-CNTL
02249                      MOVE PC-STATE     TO  ASTATEO.
02250
02251      MOVE PC-CERT-EFF-DT         TO  DC-BIN-DATE-1.
02252      MOVE SPACE                  TO  DC-OPTION-CODE.
02253
02254      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
02255
02256      MOVE DC-GREG-DATE-1-MDY     TO  AEFFDTEO.
02257      MOVE PC-CERT-PRIME          TO  ACERTI.
02258
02259      IF PC-CERT-SFX NOT = SPACES
02260          MOVE PC-CERT-SFX        TO  ACRTSFXO.
02261
02262      MOVE PC-CLAIM-NO            TO  ACLAIMO.
02263      MOVE PC-CHECK-NO            TO  ACHECKO.
02264
02265      IF WS-MAINT = 'S'
02266          MOVE AL-UANON           TO  ACARIERA
02267                                      AGROUPA
02268                                      ASTATEA
02269                                      AACCTA
02270                                      AEFFDTEA
02271                                      ACERTA
02272                                      ACRTSFXA
02273                                      ACLAIMA
02274                                      ACHECKA
02275      ELSE
02276          MOVE AL-SANON           TO  ACARIERA
02277                                      AGROUPA
02278                                      ASTATEA
02279                                      AACCTA
02280                                      AEFFDTEA
02281                                      ACERTA
02282                                      ACRTSFXA
02283                                      ACLAIMA
02284                                      ACHECKA.
02285
02286      MOVE PC-CLAIM-TYPE          TO  ACTYPEO.
02287      MOVE PC-CLAIM-PAYMENT       TO  APAYMNTO.
02288      MOVE PC-PAYMENT-TYPE        TO  APTYPEO.
02289
02290      IF PC-INCURRED-DT NOT = LOW-VALUES
02291          MOVE PC-INCURRED-DT      TO  DC-BIN-DATE-1
02292          MOVE SPACE               TO  DC-OPTION-CODE
02293          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02294          MOVE DC-GREG-DATE-1-MDY  TO  AINCURO.
02295
02296      IF PC-REPORTED-DT NOT = LOW-VALUES
02297          MOVE PC-REPORTED-DT      TO  DC-BIN-DATE-1
02298          MOVE SPACE               TO  DC-OPTION-CODE
02299          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02300          MOVE DC-GREG-DATE-1-MDY  TO  AREPORTO.
02301
02302      IF PC-PAYMENT-DT NOT = LOW-VALUES
02303          MOVE PC-PAYMENT-DT       TO  DC-BIN-DATE-1
02304          MOVE SPACE               TO  DC-OPTION-CODE
02305          PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02306          MOVE DC-GREG-DATE-1-MDY  TO  APAIDO.
02307
02308      IF PC-PAID-THRU-DT NOT = LOW-VALUES
02309         IF NOT PI-USES-PAID-TO
02310            MOVE PC-PAID-THRU-DT     TO  DC-BIN-DATE-1
02311            MOVE SPACE               TO  DC-OPTION-CODE
02312            PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02313            MOVE DC-GREG-DATE-1-MDY  TO  APTHRUO
02314         ELSE
02315            MOVE PC-PAID-THRU-DT     TO  DC-BIN-DATE-1
02316            MOVE '6'                 TO  DC-OPTION-CODE
02317            MOVE +1                  TO  DC-ELAPSED-DAYS
02318            MOVE +0                  TO  DC-ELAPSED-MONTHS
02319            PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02320            MOVE DC-GREG-DATE-1-MDY  TO  APTHRUO.
02321
02322      MOVE AL-UANON               TO  AFORCEA.
02323      MOVE PC-FORCE-CODE          TO  AFORCEO.
02324      MOVE PC-NO-OF-DAYS-PAID     TO  ADAYSO.
02325
02326      IF PC-AGE-AT-CLAIM NOT = ZEROS
02327          MOVE PC-AGE-AT-CLAIM    TO  AAGEO.
02328
02329      IF PC-CAUSE-CODE NOT = SPACES
02330          MOVE PC-CAUSE-CODE      TO  ACAUSEO.
02331
02332      MOVE PC-REMAINING-BENEFIT   TO  AREMBENO.
02333      MOVE PC-REMAINING-TERM      TO  AREMTRMO.
02334
02335      IF PC-CREDIT-SELECT-DT NOT = SPACES AND  ZEROS
02336        AND  LOW-VALUES
02337         MOVE PC-CREDIT-SELECT-DT  TO  DC-BIN-DATE-1
02338         MOVE SPACES               TO  DC-OPTION-CODE
02339         PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02340         MOVE DC-GREG-DATE-1-MDY   TO  AEOMDTO
02341         INSPECT AEOMDTO CONVERTING SPACES TO '/'.
02342
02343      GO TO 6900-EXIT.
02344
02345  6500-FORMAT-RESERVE-DATA.
02346      MOVE EL632B                 TO  PI-MAP-NAME.
02347      MOVE PC-ACCOUNT             TO  BACCTO.
02348
02349      IF CARR-GROUP-ST-ACCNT-CNTL
02350          MOVE PC-CARRIER            TO  BCARIERO
02351          MOVE PC-GROUPING           TO  BGROUPO
02352          MOVE PC-STATE              TO  BSTATEO
02353      ELSE
02354          IF CARR-ST-ACCNT-CNTL
02355              MOVE PC-CARRIER        TO  BCARIERO
02356              MOVE PC-STATE          TO  BSTATEO
02357          ELSE
02358              IF CARR-ACCNT-CNTL
02359                  MOVE PC-CARRIER    TO  BCARIERO
02360              ELSE
02361                  IF ST-ACCNT-CNTL
02362                      MOVE PC-STATE  TO  BSTATEO.
02363
02364      MOVE PC-CERT-EFF-DT         TO  DC-BIN-DATE-1.
02365      MOVE SPACE                  TO  DC-OPTION-CODE.
02366
02367      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
02368
02369      MOVE DC-GREG-DATE-1-MDY     TO  BEFFDTEO.
02370      MOVE PC-CERT-PRIME          TO  BCERTI.
02371
02372      IF PC-CERT-SFX NOT = SPACES
02373          MOVE PC-CERT-SFX        TO  BCRTSFXO.
02374
02375      MOVE PC-CLAIM-NO            TO  BCLAIMO.
02376
02377      IF WS-MAINT = 'S'
02378          MOVE AL-UANON           TO  BCARIERA
02379                                      BGROUPA
02380                                      BSTATEA
02381                                      BACCTA
02382                                      BEFFDTEA
02383                                      BCERTA
02384                                      BCRTSFXA
02385                                      BCLAIMA
02386      ELSE
02387          MOVE AL-SANON           TO  BCARIERA
02388                                      BGROUPA
02389                                      BSTATEA
02390                                      BACCTA
02391                                      BEFFDTEA
02392                                      BCERTA
02393                                      BCRTSFXA
02394                                      BCLAIMA.
02395
02396      IF PC-FUTURE-RESERVE-AMT NOT = ZEROS
02397          MOVE PC-FUTURE-RESERVE-AMT  TO  BFUTUREO.
02398
02399      IF PC-IBNR-RESERVE-AMT NOT = ZEROS
02400          MOVE PC-IBNR-RESERVE-AMT  TO  BIBNRO.
02401
02402      IF PC-PTC-RESERVE-AMT NOT = ZEROS
02403          MOVE PC-PTC-RESERVE-AMT  TO  BPAYCURO.
02404
02405      IF PC-MANUAL-RESERVE-AMT NOT = ZEROS
02406          MOVE PC-MANUAL-RESERVE-AMT  TO  BMANUALO.
02407
02408      IF PC-FORCE-CODE NOT = SPACES
02409          MOVE PC-FORCE-CODE      TO  BFORCEO.
02410
02411      IF PC-CREDIT-SELECT-DT NOT = SPACES AND  ZEROS
02412        AND  LOW-VALUES
02413         MOVE PC-CREDIT-SELECT-DT  TO  DC-BIN-DATE-1
02414         MOVE SPACES               TO  DC-OPTION-CODE
02415         PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
02416         MOVE DC-GREG-DATE-1-MDY   TO  BEOMDTO
02417         INSPECT BEOMDTO CONVERTING SPACES TO '/'.
02418
02419  6900-EXIT.
02420      EXIT.
02421  EJECT
02422  7000-FORMAT-ERRORS.
02423      IF PC-ERROR-FLAGS = SPACES
02424          GO TO 7090-EXIT.
02425
02426      MOVE ER-2800                TO  WS-ERR-CODE.
02427      MOVE 1                      TO  SUB.
02428
02429  7010-ERR-LOOP.
02430      IF PC-ERR-FLAG (SUB) NOT = SPACES
02431          MOVE SUB                TO  WS-ERROR-SUB
02432          MOVE WS-ERR-CODE        TO  EMI-ERROR
02433          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02434          PERFORM 7200-SET-ATTRBS  THRU  7290-EXIT.
02435
02436      ADD 1                       TO  SUB.
02437
02438      IF SUB  IS LESS THAN  101
02439          GO TO 7010-ERR-LOOP.
02440
02441  7050-SET-ERROR-FLAGS.
02442      IF EMI-FATAL-CTR NOT = ZEROS
02443          MOVE 'X'                TO  PC-FATAL-FLAG.
02444
02445      IF EMI-FORCABLE-CTR NOT = ZEROS
02446          IF PC-CLAIM-FORCE
02447            AND  EMI-FATAL-CTR = ZEROS
02448              MOVE 'F'            TO  PC-FORCE-ER-CD
02449              MOVE ER-2600        TO  EMI-ERROR
02450              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
02451          ELSE
02452              MOVE 'X'            TO  PC-FORCE-ER-CD.
02453
02454      IF EMI-WARNING-CTR NOT = ZEROS
02455          MOVE 'W'                TO  PC-WARN-ER-CD.
02456
02457  7090-EXIT.
02458      EXIT.
02459
02460  7200-SET-ATTRBS.
02461 **2801**
02462      IF SUB = 1
02463          MOVE AL-UNBON           TO  APAYMNTA
02464          MOVE -1                 TO  APAYMNTL
02465      ELSE
02466
02467 **2802**
02468      IF SUB = 2
02469          MOVE AL-UNBON           TO  AINCURA
02470          MOVE -1                 TO  AINCURL
02471      ELSE
02472
02473 **2803**
02474      IF SUB = 3
02475          MOVE AL-UNBON           TO  APAIDA
02476          MOVE -1                 TO  APAIDL
02477      ELSE
02478
02479 **2804**
02480      IF SUB = 4
02481          MOVE AL-UNBON           TO  AREPORTA
02482          MOVE -1                 TO  AREPORTL
02483      ELSE
02484
02485 **2805**
02486      IF SUB = 5
02487          MOVE AL-UNBON           TO  APTHRUA
02488          MOVE -1                 TO  APTHRUL
02489      ELSE
02490
02491 **2806**
02492      IF SUB = 6
02493          MOVE AL-UNBON           TO  AINCURA
02494          MOVE -1                 TO  AINCURL
02495      ELSE
02496
02497 **2807**
02498      IF SUB = 7
02499          MOVE AL-UNBON           TO  APAIDA
02500          MOVE -1                 TO  APAIDL
02501      ELSE
02502
02503 **2808**
02504      IF SUB = 8
02505          MOVE AL-UNBON           TO  APAIDA
02506          MOVE -1                 TO  APAIDL
02507      ELSE
02508
02509 **2809**
02510      IF SUB = 9
02511          MOVE AL-UNBON           TO  AREPORTA
02512          MOVE -1                 TO  AREPORTL
02513      ELSE
02514
02515 **2810**
02516      IF SUB = 10
02517          MOVE AL-UNBON           TO  AREPORTA
02518          MOVE -1                 TO  AREPORTL
02519      ELSE
02520
02521 **2811**
02522      IF SUB = 11
02523          MOVE AL-UNBON           TO  APTHRUA
02524          MOVE -1                 TO  APTHRUL
02525      ELSE
02526
02527 **2812**
02528      IF SUB = 12
02529          MOVE AL-UNBON           TO  ADAYSA
02530          MOVE -1                 TO  ADAYSL
02531      ELSE
02532
02533 **2813**
02534      IF SUB = 13
02535          MOVE AL-UNBON           TO  BIBNRA
02536          MOVE -1                 TO  BIBNRL
02537      ELSE
02538
02539 **2814**
02540      IF SUB = 14
02541          MOVE AL-UNBON           TO  BPAYCURA
02542          MOVE -1                 TO  BPAYCURL
02543      ELSE
02544
02545 **2815**
02546      IF SUB = 15
02547          MOVE AL-UNBON           TO  BFUTUREA
02548          MOVE -1                 TO  BFUTUREL
02549      ELSE
02550
02551 **2816**
02552 *    IF SUB = 16
02553 *        MOVE AL-UNBON           TO
02554 *        MOVE -1                 TO
02555 *    ELSE
02556
02557 **2817**
02558      IF SUB = 17
02559          MOVE AL-UNBON           TO  ACTYPEA
02560          MOVE -1                 TO  ACTYPEL
02561      ELSE
02562
02563 **2818**
02564      IF SUB = 18
02565          MOVE AL-UNBON           TO  ACLAIMA
02566          MOVE -1                 TO  ACLAIML
02567      ELSE
02568
02569 **2819**
02570      IF SUB = 19
02571          MOVE AL-UNBON           TO  ACLAIMA
02572          MOVE -1                 TO  ACLAIML
02573      ELSE
02574
02575 **2820**
02576      IF SUB = 20
02577          MOVE AL-UNBON           TO  ACLAIMA
02578          MOVE -1                 TO  ACLAIML
02579      ELSE
02580
02581 **2821**
02582      IF SUB = 21
02583          MOVE AL-UNBON           TO  ACLAIMA
02584          MOVE -1                 TO  ACLAIML
02585      ELSE
02586
02587 **2822**
02588      IF SUB = 22
02589          MOVE AL-UNBON           TO  ACLAIMA
02590          MOVE -1                 TO  ACLAIML
02591      ELSE
02592
02593 **2823**
02594      IF SUB = 23
02595          MOVE AL-UNBON           TO  APAYMNTA
02596          MOVE -1                 TO  APAYMNTL
02597      ELSE
02598
02599 **2824**
02600      IF SUB = 24
02601          MOVE AL-UNBON           TO  ACLAIMA
02602          MOVE -1                 TO  ACLAIML
02603      ELSE
02604
02605 **2825**
02606      IF SUB = 25
02607          MOVE AL-UNBON           TO  ACTYPEA
02608          MOVE -1                 TO  ACTYPEL
02609      ELSE
02610
02611 **2826**
02612      IF SUB = 26
02613          MOVE AL-UNBON           TO  ACLAIMA
02614          MOVE -1                 TO  ACLAIML
02615      ELSE
02616
02617 **2827**
02618      IF SUB = 27
02619          MOVE AL-UNBON           TO  ACLAIMA
02620          MOVE -1                 TO  ACLAIML
02621      ELSE
02622
02623 **2828**
02624      IF SUB = 28
02625          MOVE AL-UNBON           TO  ACLAIMA
02626          MOVE -1                 TO  ACLAIML
02627      ELSE
02628
02629 **2829**
02630      IF SUB = 29
02631          MOVE AL-UNBON           TO  ACLAIMA
02632          MOVE -1                 TO  ACLAIML
02633      ELSE
02634
02635 **2830**
02636      IF SUB = 30
02637          MOVE AL-UNBON           TO  ACLAIMA
02638          MOVE -1                 TO  ACLAIML
02639      ELSE
02640
02641 **2831**
02642      IF SUB = 31
02643          MOVE AL-UNBON           TO  ACLAIMA
02644          MOVE -1                 TO  ACLAIML
02645      ELSE
02646
02647 **2832**
02648      IF SUB = 32
02649          MOVE AL-UNBON           TO  ACLAIMA
02650          MOVE -1                 TO  ACLAIML
02651      ELSE
02652
02653 **2833**
02654      IF SUB = 33
02655          IF PI-MAP-NAME = EL632A
02656              MOVE AL-SABOF       TO  ACERTA
02657              MOVE -1             TO  ACERTL
02658          ELSE
02659              MOVE AL-SABOF       TO  BCERTA
02660              MOVE -1             TO  BCERTL
02661      ELSE
02662
02663 **2834**
02664      IF SUB = 34
02665          IF PI-MAP-NAME = EL632A
02666              MOVE AL-SABOF       TO  ACERTA
02667              MOVE -1             TO  ACERTL
02668          ELSE
02669              MOVE AL-SABOF       TO  BCERTA
02670              MOVE -1             TO  BCERTL
02671      ELSE
02672
02673 **2835**
02674      IF SUB = 35
02675          MOVE AL-UNBON           TO  APTHRUA
02676          MOVE -1                 TO  APTHRUL
02677      ELSE
02678
02679 **2836**
02680      IF SUB = 36
02681          MOVE AL-UNBON           TO  APTHRUA
02682          MOVE -1                 TO  APTHRUL
02683      ELSE
02684
02685 **2837**
02686      IF SUB = 37
02687          MOVE AL-UNBON           TO  APTHRUA
02688          MOVE -1                 TO  APTHRUL
02689      ELSE
02690
02691 **2838**
02692      IF SUB = 38
02693          MOVE -1                 TO  AMAINTL
02694      ELSE
02695
02696 **2839**
02697      IF SUB = 39
02698          MOVE -1                 TO  AMAINTL
02699      ELSE
02700
02701 **2840**
02702      IF SUB = 40
02703          IF PI-MAP-NAME = EL632A
02704              MOVE -1             TO  AMAINTL
02705          ELSE
02706              MOVE -1             TO  BMAINTL
02707      ELSE
02708
02709 **2841**
02710      IF SUB = 41
02711          IF PI-MAP-NAME = EL632A
02712              MOVE -1             TO  AMAINTL
02713          ELSE
02714              MOVE -1             TO  BMAINTL
02715      ELSE
02716
02717 **2842**
02718      IF SUB = 42
02719          IF PI-MAP-NAME = EL632A
02720              MOVE -1             TO  AMAINTL
02721          ELSE
02722              MOVE -1             TO  BMAINTL
02723      ELSE
02724
02725 **2843**
02726      IF SUB = 43
02727          IF PI-MAP-NAME = EL632A
02728              MOVE -1             TO  AMAINTL
02729          ELSE
02730              MOVE -1             TO  BMAINTL
02731      ELSE
02732
02733 **2844**
02734      IF SUB = 44
02735          IF PI-MAP-NAME = EL632A
02736              MOVE AL-UABON       TO  ACARIERA
02737              MOVE -1             TO  ACARIERL
02738          ELSE
02739              MOVE AL-UABON       TO  BCARIERA
02740              MOVE -1             TO  BCARIERL
02741      ELSE
02742
02743 **2845**
02744      IF SUB = 45
02745          IF PI-MAP-NAME = EL632A
02746              MOVE AL-UABON       TO  ACARIERA
02747              MOVE -1             TO  ACARIERL
02748          ELSE
02749              MOVE AL-UABON       TO  BCARIERA
02750              MOVE -1             TO  BCARIERL
02751      ELSE
02752
02753 **2846**
02754      IF SUB = 46
02755          IF PI-MAP-NAME = EL632A
02756              MOVE AL-UABON       TO  AGROUPA
02757              MOVE -1             TO  AGROUPL
02758          ELSE
02759              MOVE AL-UABON       TO  BGROUPA
02760              MOVE -1             TO  BGROUPL
02761      ELSE
02762
02763 **2847**
02764      IF SUB = 47
02765          IF PI-MAP-NAME = EL632A
02766              MOVE AL-UABON       TO  ASTATEA
02767              MOVE -1             TO  ASTATEL
02768          ELSE
02769              MOVE AL-UABON       TO  BSTATEA
02770              MOVE -1             TO  BSTATEL
02771      ELSE
02772
02773 **2848**
02774      IF SUB = 48
02775          IF PI-MAP-NAME = EL632A
02776              MOVE AL-UABON       TO  ASTATEA
02777              MOVE -1             TO  ASTATEL
02778          ELSE
02779              MOVE AL-UABON       TO  BSTATEA
02780              MOVE -1             TO  BSTATEL
02781      ELSE
02782
02783 **2849**
02784      IF SUB = 49
02785          IF PI-MAP-NAME = EL632A
02786              MOVE AL-UABON       TO  AACCTA
02787              MOVE -1             TO  AACCTL
02788          ELSE
02789              MOVE AL-UABON       TO  BACCTA
02790              MOVE -1             TO  BACCTL
02791      ELSE
02792
02793 **2850**
02794      IF SUB = 50
02795          IF PI-MAP-NAME = EL632A
02796              MOVE AL-UABON       TO  AACCTA
02797              MOVE -1             TO  AACCTL
02798          ELSE
02799              MOVE AL-UABON       TO  BACCTA
02800              MOVE -1             TO  BACCTL
02801      ELSE
02802
02803 **2851**
02804      IF SUB = 51
02805          IF PI-MAP-NAME = EL632A
02806              MOVE AL-UABON       TO  AACCTA
02807              MOVE -1             TO  AACCTL
02808          ELSE
02809              MOVE AL-UABON       TO  BACCTA
02810              MOVE -1             TO  BACCTL
02811      ELSE
02812
02813 **2852**
02814      IF SUB = 52
02815          IF PI-MAP-NAME = EL632A
02816              MOVE AL-UABON       TO  ACLAIMA
02817              MOVE -1             TO  ACLAIML
02818          ELSE
02819              MOVE AL-UABON       TO  BCLAIMA
02820              MOVE -1             TO  BCLAIML
02821      ELSE
02822
02823 **2853**
02824      IF SUB = 53
02825          MOVE AL-UABON           TO  APTYPEA
02826          MOVE -1                 TO  APTYPEL
02827      ELSE
02828
02829 **2854**
02830      IF SUB = 54
02831          MOVE AL-UNBON           TO  BMANUALA
02832          MOVE -1                 TO  BMANUALL
02833      ELSE
02834
02835 **2855**
02836      IF SUB = 55
02837          MOVE AL-UABON           TO  APTYPEA
02838          MOVE -1                 TO  APTYPEL
02839      ELSE
02840
02841 **2856**
02842      IF SUB = 56
02843          MOVE AL-UABON           TO  APTYPEA
02844          MOVE -1                 TO  APTYPEL
02845      ELSE
02846
02847 **2857**
02848      IF SUB = 57
02849          MOVE AL-UABON           TO  ACTYPEA
02850          MOVE -1                 TO  ACTYPEL
02851      ELSE
02852
02853 **2858**
02854      IF SUB = 58
02855          MOVE AL-UABON           TO  ACTYPEA
02856          MOVE -1                 TO  ACTYPEL
02857      ELSE
02858
02859 **2859**
02860      IF SUB = 59
02861          MOVE -1                 TO  AMAINTL
02862      ELSE
02863
02864 **2860**
02865      IF SUB = 60
02866          MOVE AL-UABON           TO  APTYPEA
02867          MOVE -1                 TO  APTYPEL
02868      ELSE
02869
02870 **2861**
02871      IF SUB = 61
02872          MOVE AL-UNBON           TO  APAYMNTA
02873          MOVE -1                 TO  APAYMNTL
02874      ELSE
02875
02876 **2862**
02877      IF SUB = 62
02878          MOVE -1                 TO  AMAINTL
02879      ELSE
02880
02881 **2863**
02882      IF SUB = 63
02883          MOVE -1                 TO  AMAINTL
02884      ELSE
02885
02886 **2864**
02887      IF SUB = 64
02888          MOVE AL-UABON           TO  APTYPEA
02889          MOVE -1                 TO  APTYPEL
02890      ELSE
02891
02892 **2865**
02893      IF SUB = 65
02894          MOVE AL-UNBON           TO  APAYMNTA
02895          MOVE -1                 TO  APAYMNTL
02896      ELSE
02897
02898 **2866**
02899      IF SUB = 66
02900          MOVE AL-UNBON           TO  APAYMNTA
02901          MOVE -1                 TO  APAYMNTL
02902      ELSE
02903
02904 **2867**
02905      IF SUB = 67
02906          MOVE -1                 TO  AMAINTL
02907      ELSE
02908
02909 **2868**
02910      IF SUB = 68
02911          MOVE AL-UNBON           TO  APAIDA
02912          MOVE -1                 TO  APAIDL.
02913
02914  7290-EXIT.
02915      EXIT.
02916  EJECT
02917  8100-SEND-INITIAL-MAP.
02918      IF PI-MAP-NAME = EL632A
02919          NEXT SENTENCE
02920      ELSE
02921          GO TO 8110-SEND-INITIAL-MAPB.
02922
02923      MOVE WS-CURRENT-DT          TO  ADATEO.
02924      MOVE EIBTIME                TO  TIME-IN.
02925      MOVE TIME-OUT               TO  ATIMEO.
02926      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-ACLM-TYP-1
02927                                      WS-ACLM-TYP-3.
02928      MOVE PI-AH-OVERRIDE-L6      TO  WS-ACLM-TYP-2
02929                                      WS-ACLM-TYP-4.
02930      MOVE WS-ACLMTP1             TO  ACLMTP1O.
02931      MOVE WS-ACLMTP2             TO  ACLMTP2O.
02932      MOVE -1                     TO  AMAINTL.
02933      MOVE EMI-MESSAGE-AREA (1)   TO  AERMSG1O.
02934      MOVE EMI-MESSAGE-AREA (2)   TO  AERMSG2O.
02935      MOVE EMI-MESSAGE-AREA (3)   TO  AERMSG3O.
02936
02937      IF CARR-GROUP-ST-ACCNT-CNTL
02938          NEXT SENTENCE
02939      ELSE
02940          IF ST-ACCNT-CNTL
02941              MOVE AL-SADOF              TO  ACARHDGA
02942                                             AGRPHDGA
02943              MOVE AL-SANOF              TO  ACARIERA
02944                                             AGROUPA
02945          ELSE
02946              IF CARR-ST-ACCNT-CNTL
02947                  MOVE AL-SADOF          TO  AGRPHDGA
02948                  MOVE AL-SANOF          TO  AGROUPA
02949              ELSE
02950                  IF ACCNT-CNTL
02951                      MOVE AL-SADOF      TO  ACARHDGA
02952                                             AGRPHDGA
02953                                             ASTHDGA
02954                      MOVE AL-SANOF      TO  ACARIERA
02955                                             AGROUPA
02956                                             ASTATEA
02957                  ELSE
02958                      IF CARR-ACCNT-CNTL
02959                          MOVE AL-SADOF  TO  AGRPHDGA
02960                                             ASTHDGA
02961                          MOVE AL-SANOF  TO  AGROUPA
02962                                             ASTATEA.
02963
02964      IF PI-USES-PAID-TO
02965         MOVE EL632A-HEADING   TO AHDINGO.
02966
02967      
      * EXEC CICS SEND
02968 *        MAP     (PI-MAP-NAME)
02969 *        MAPSET  (MAPSET-NAME)
02970 *        FROM    (EL632AO)
02971 *        ERASE
02972 *        CURSOR
02973 *    END-EXEC.
           MOVE LENGTH OF
            EL632AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006494' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL632AO, 
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
           
02974
02975      GO TO 9100-RETURN-TRAN.
02976  EJECT
02977  8110-SEND-INITIAL-MAPB.
02978      MOVE WS-CURRENT-DT          TO  BDATEO.
02979      MOVE EIBTIME                TO  TIME-IN.
02980      MOVE TIME-OUT               TO  BTIMEO.
02981      MOVE -1                     TO  BMAINTL.
02982      MOVE EMI-MESSAGE-AREA (1)   TO  BERMSG1O.
02983      MOVE EMI-MESSAGE-AREA (2)   TO  BERMSG2O.
02984      MOVE EMI-MESSAGE-AREA (3)   TO  BERMSG3O.
02985
02986      IF CARR-GROUP-ST-ACCNT-CNTL
02987          NEXT SENTENCE
02988      ELSE
02989          IF ST-ACCNT-CNTL
02990              MOVE AL-SADOF              TO  BCARHDGA
02991                                             BGRPHDGA
02992              MOVE AL-SANOF              TO  BCARIERA
02993                                             BGROUPA
02994          ELSE
02995              IF CARR-ST-ACCNT-CNTL
02996                  MOVE AL-SADOF          TO  BGRPHDGA
02997                  MOVE AL-SANOF          TO  BGROUPA
02998              ELSE
02999                  IF ACCNT-CNTL
03000                      MOVE AL-SADOF      TO  BCARHDGA
03001                                             BGRPHDGA
03002                                             BSTHDGA
03003                      MOVE AL-SANOF      TO  BCARIERA
03004                                             BGROUPA
03005                                             BSTATEA
03006                  ELSE
03007                      IF CARR-ACCNT-CNTL
03008                          MOVE AL-SADOF  TO  BGRPHDGA
03009                                             BSTHDGA
03010                          MOVE AL-SANOF  TO  BGROUPA
03011                                             BSTATEA.
03012
03013      
      * EXEC CICS SEND
03014 *        MAP     (PI-MAP-NAME)
03015 *        MAPSET  (MAPSET-NAME)
03016 *        FROM    (EL632BO)
03017 *        ERASE
03018 *        CURSOR
03019 *    END-EXEC.
           MOVE LENGTH OF
            EL632BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006540' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL632BO, 
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
           
03020
03021      GO TO 9100-RETURN-TRAN.
03022  EJECT
03023  8200-SEND-DATAONLY.
03024      IF EIBAID = DFHPF11
03025          MOVE 'Y'                TO  EMI-ROLL-SWITCH
03026          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
03027
03028      IF PI-MAP-NAME EQUAL EL632A
03029         IF PI-USES-PAID-TO
03030            MOVE EL632A-HEADING   TO AHDINGO.
03031
03032      IF PI-MAP-NAME = EL632A
03033          MOVE WS-CURRENT-DT         TO  ADATEO
03034          MOVE EIBTIME               TO  TIME-IN
03035          MOVE TIME-OUT              TO  ATIMEO
03036          MOVE PI-LIFE-OVERRIDE-L6   TO  WS-ACLM-TYP-1
03037                                         WS-ACLM-TYP-3
03038          MOVE PI-AH-OVERRIDE-L6     TO  WS-ACLM-TYP-2
03039                                         WS-ACLM-TYP-4
03040          MOVE WS-ACLMTP1            TO  ACLMTP1O
03041          MOVE WS-ACLMTP2            TO  ACLMTP2O
03042          MOVE EMI-MESSAGE-AREA (1)  TO  AERMSG1O
03043          MOVE EMI-MESSAGE-AREA (2)  TO  AERMSG2O
03044          MOVE EMI-MESSAGE-AREA (3)  TO  AERMSG3O
03045          
      * EXEC CICS SEND
03046 *            MAP     (PI-MAP-NAME)
03047 *            MAPSET  (MAPSET-NAME)
03048 *            FROM    (EL632AO)
03049 *            DATAONLY
03050 *            ERASEAUP
03051 *            CURSOR
03052 *        END-EXEC
           MOVE LENGTH OF
            EL632AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00006572' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL632AO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03053      ELSE
03054          MOVE WS-CURRENT-DT         TO  BDATEO
03055          MOVE EIBTIME               TO  TIME-IN
03056          MOVE TIME-OUT              TO  BTIMEO
03057          MOVE EMI-MESSAGE-AREA (1)  TO  BERMSG1O
03058          MOVE EMI-MESSAGE-AREA (2)  TO  BERMSG2O
03059          MOVE EMI-MESSAGE-AREA (3)  TO  BERMSG3O
03060          
      * EXEC CICS SEND
03061 *            MAP     (PI-MAP-NAME)
03062 *            MAPSET  (MAPSET-NAME)
03063 *            FROM    (EL632BO)
03064 *            DATAONLY
03065 *            ERASEAUP
03066 *            CURSOR
03067 *        END-EXEC.
           MOVE LENGTH OF
            EL632BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00006587' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL632BO, 
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
           
03068
03069      GO TO 9100-RETURN-TRAN.
03070
03071  8300-SEND-TEXT.
03072      
      * EXEC CICS SEND TEXT
03073 *        FROM    (LOGOFF-TEXT)
03074 *        LENGTH  (LOGOFF-LENGTH)
03075 *        ERASE
03076 *        FREEKB
03077 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006599' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353939' TO DFHEIV0(25:11)
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
           
03078
03079      
      * EXEC CICS RETURN
03080 *    END-EXEC.
      *    MOVE '.(                    &   #00006606' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03081
03082  8400-LOG-JOURNAL-RECORD.
03083      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
03084      MOVE ERPNDC-FILE-ID         TO  JP-FILE-ID.
03085      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
03086
03087 *    EXEC CICS JOURNAL
03088 *        JFILEID  (PI-JOURNAL-FILE-ID)
03089 *        JTYPEID  ('EL')
03090 *        FROM     (JOURNAL-RECORD)
03091 *        LENGTH   (523)
03092 *    END-EXEC.
03093
03094  8500-DATE-CONVERT.
03095      MOVE LINK-CLDATCV           TO  PGM-NAME.
03096
03097      
      * EXEC CICS LINK
03098 *        PROGRAM   (PGM-NAME)
03099 *        COMMAREA  (DATE-CONVERSION-DATA)
03100 *        LENGTH    (DC-COMM-LENGTH)
03101 *    END-EXEC.
      *    MOVE '."C                   ''   #00006624' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03102
03103  8500-EXIT.
03104      EXIT.
03105
CIDMOD 8600-DEEDIT.
CIDMOD     
      * EXEC CICS BIF DEEDIT
CIDMOD*        FIELD   (WS-DEEDIT-FIELD)
CIDMOD*        LENGTH  (11)
CIDMOD*    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006634' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD 8600-EXIT.
CIDMOD     EXIT.
03114
03115  8800-UNAUTHORIZED-ACCESS.
03116      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
03117
03118      GO TO 8300-SEND-TEXT.
03119
03120  8810-PF23.
03121      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
03122      MOVE XCTL-005               TO  PGM-NAME.
03123
03124      GO TO 9300-XCTL.
03125
03126  9100-RETURN-TRAN.
03127      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
03128      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
03129
03130      
      * EXEC CICS RETURN
03131 *        TRANSID   (TRANS-ID)
03132 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
03133 *        LENGTH    (PI-COMM-LENGTH)
03134 *    END-EXEC.
      *    MOVE '.(CT                  &   #00006657' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03135
03136  9200-RETURN-MAIN-MENU.
03137      MOVE XCTL-626               TO  PGM-NAME.
03138
03139      GO TO 9300-XCTL.
03140
03141  9300-XCTL.
03142      
      * EXEC CICS XCTL
03143 *        PROGRAM   (PGM-NAME)
03144 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
03145 *        LENGTH    (PI-COMM-LENGTH)
03146 *    END-EXEC.
      *    MOVE '.$C                   $   #00006669' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03147
03148  9400-CLEAR.
03149      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
03150
03151      GO TO 9300-XCTL.
03152
03153  9500-PF12.
03154      MOVE XCTL-010               TO  PGM-NAME.
03155
03156      GO TO 9300-XCTL.
03157
03158  9600-PGMID-ERROR.
03159      
      * EXEC CICS HANDLE CONDITION
03160 *        PGMIDERR  (8300-SEND-TEXT)
03161 *    END-EXEC.
      *    MOVE '"$L                   ! - #00006686' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303036363836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03162
03163      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
03164      MOVE ' '                    TO  PI-ENTRY-CD-1.
03165      MOVE XCTL-005               TO  PGM-NAME.
03166      MOVE PGM-NAME               TO  LOGOFF-PGM.
03167      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
03168
03169      GO TO 9300-XCTL.
03170
03171  9800-LINK-CLAIMS-EDIT.
03172      MOVE PENDING-CLAIMS         TO  PNDC-RECORD.
03173      MOVE ZEROS                  TO  WK-PC-ACCT-ADDR
03174                                      WK-PC-STATE-ADDR.
03175      MOVE SPACES                 TO  WK-PC-CNTL-RECORD-FOUND-SW.
03176      MOVE LINK-053               TO  PGM-NAME.
03177
03178      
      * EXEC CICS LINK
03179 *        PROGRAM   (PGM-NAME)
03180 *        COMMAREA  (PNDC-EDIT-PASS-AREA)
03181 *        LENGTH    (PNDC-EDIT-PASS-AREA-LEN)
03182 *    END-EXEC.
      *    MOVE '."C                   ''   #00006705' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PNDC-EDIT-PASS-AREA, 
                 PNDC-EDIT-PASS-AREA-LEN, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03183
03184      MOVE PNDC-RECORD            TO  PENDING-CLAIMS.
03185
03186  9900-ERROR-FORMAT.
03187      IF NOT EMI-ERRORS-COMPLETE
03188          MOVE LINK-001           TO  PGM-NAME
03189          
      * EXEC CICS LINK
03190 *            PROGRAM   (PGM-NAME)
03191 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
03192 *            LENGTH    (EMI-COMM-LENGTH)
03193 *        END-EXEC.
      *    MOVE '."C                   ''   #00006716' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03194
03195  9900-EXIT.
03196      EXIT.
03197
03198  9990-ABEND.
03199      MOVE LINK-004               TO  PGM-NAME.
03200      MOVE DFHEIBLK               TO  EMI-LINE1
03201
03202      
      * EXEC CICS LINK
03203 *        PROGRAM   (PGM-NAME)
03204 *        COMMAREA  (EMI-LINE1)
03205 *        LENGTH    (72)
03206 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00006729' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03207
03208      IF PI-MAP-NAME = EL632A
03209          MOVE -1                 TO  AMAINTL
03210      ELSE
03211          MOVE -1                 TO  BMAINTL.
03212
03213      GO TO 8200-SEND-DATAONLY.
03214
03215      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL632' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
03216
03217  9995-SECURITY-VIOLATION.
03218 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00006762' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373632' TO DFHEIV0(25:11)
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
03219
03220  9995-EXIT.
03221      EXIT.
03222

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL632' TO DFHEIV1
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
               GO TO 0890-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1200-NO-CARRIER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1300-NO-STATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1400-ACCOUNT-INVALID
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 2900-DUPLICATE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 3400-RECORD-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3600-DELETE-OLDREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 3700-REBUILD-KEY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4900-RECORD-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5800-NO-RECORD,
                     5900-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL632' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
