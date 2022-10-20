00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL930.
00004 *                            VMOD=2.001.
00005
00006  AUTHOR.     LOGIC,INC.
00007              DALLAS, TEXAS.
00008
00009  DATE-COMPILED.
00010  SECURITY.   *****************************************************
00011              *                                                   *
00012              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00013              *                                                   *
00014              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00015              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00016              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00017              *                                                   *
00018              *****************************************************
00019
00020  REMARKS.    TRANSACTION - EXI1 - NEW BUSINESS - DATA ENTRY.
00021
00022  ENVIRONMENT DIVISION.
00023
00024      EJECT
00025  DATA DIVISION.
00026  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00027  77  FILLER  PIC X(32)  VALUE '********************************'.
00028  77  FILLER  PIC X(32)  VALUE '*    EL930 WORKING STORAGE     *'.
00029  77  FILLER  PIC X(32)  VALUE '************ V/M 2.001 *********'.
00030
00031 *                            COPY ELCSCTM.
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
00032
00033 *                            COPY ELCSCRTY.
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
00034
00035  01  STANDARD-AREAS.
00036      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.
00037      12  GETMAIN-SPACE           PIC X       VALUE SPACE.
00038      12  MAP-NAME                PIC X(8)    VALUE 'EL930A  '.
00039      12  MAPSET-NAME             PIC X(8)    VALUE 'EL930S  '.
00040      12  SCREEN-NUMBER           PIC X(4)    VALUE '930A'.
00041      12  TRANS-ID                PIC X(4)    VALUE 'EXI1'.
00042      12  EDIT-TRANS              PIC X(4)    VALUE 'EXBT'.
00043      12  PASS-AREA-LEN           PIC S9(4)   COMP VALUE +16.
00044      12  THIS-PGM                PIC X(8)    VALUE 'EL930   '.
00045      12  PGM-NAME                PIC X(8).
00046      12  TIME-IN                 PIC S9(7).
00047      12  TIME-OUT-R  REDEFINES TIME-IN.
00048          16  FILLER              PIC X.
00049          16  TIME-OUT            PIC 99V99.
00050          16  FILLER              PIC X(2).
00051      12  XCTL-005                PIC X(8)    VALUE 'EL005   '.
00052      12  XCTL-010                PIC X(8)    VALUE 'EL010   '.
00053      12  XCTL-626                PIC X(8)    VALUE 'EL626   '.
00054      12  XCTL-9301               PIC X(8)    VALUE 'EL9301  '.
00055      12  XCTL-9302               PIC X(8)    VALUE 'EL9302  '.
00056      12  XCTL-633                PIC X(8)    VALUE 'EL633   '.
00057      12  LINK-001                PIC X(8)    VALUE 'EL001   '.
00058      12  LINK-004                PIC X(8)    VALUE 'EL004   '.
00059      12  LINK-CLDATCV            PIC X(8)    VALUE 'ELDATCV '.
00060      12  ELCNTL-FILE-ID          PIC X(8)    VALUE 'ELCNTL  '.
00061      12  ERPNDT-FILE-ID          PIC X(8)    VALUE 'ERPNDT  '.
00062      12  ELCERT-FILE-ID          PIC X(8)    VALUE 'ELCERT  '.
00063      12  ERNOTE-FILE-ID          PIC X(8)    VALUE 'ERNOTE  '.
00064      12  ERMAIL-FILE-ID          PIC X(8)    VALUE 'ERMAIL  '.
00065      12  ERPNDM-FILE-ID          PIC X(8)    VALUE 'ERPNDM  '.
00066      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.
00067      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.
00068      12  WS-SYNC-CNTR            PIC S9(3)   VALUE +0 COMP-3.
00069
00070      12  WS-RECORD-LENGTHS   COMP.
00071         16  WS-ERNOTE-RECORD-LENGTH  PIC S9(4)   VALUE +500.
00072         16  WS-ERPNDT-RECORD-LENGTH  PIC S9(4)   VALUE +585.
00073         16  WS-ERMAIL-RECORD-LENGTH  PIC S9(4)   VALUE +250.
00074         16  WS-ERPNDM-RECORD-LENGTH  PIC S9(4)   VALUE +250.
00075         16  WS-ELCERT-RECORD-LENGTH  PIC S9(4)   VALUE +450.
00076         16  WS-ELCNTL-RECORD-LENGTH  PIC S9(4)   VALUE +504.
00077         16  WS-JOURNAL-RECORD-LENGTH PIC S9(4)   VALUE ZEROS.
00078
00079  01  BATCH-TO-PROCESS.
00080      05  EDIT-COMPANY-CD         PIC X.
00081      05  EDIT-BATCH              PIC X(6).
00082      05  EDIT-COMPANY-ID         PIC XXX.
00083      05  EDIT-RESTART-BATCH      PIC X(6).
00084
00085      EJECT
00086
00087  01  ERROR-MESSAGES.
00088      12  ER-0008                 PIC X(4)    VALUE '0008'.
00089      12  ER-0023                 PIC X(4)    VALUE '0023'.
00090      12  ER-0029                 PIC X(4)    VALUE '0029'.
00091      12  ER-0070                 PIC X(4)    VALUE '0070'.
00092      12  ER-0194                 PIC X(4)    VALUE '0194'.
00093      12  ER-0195                 PIC X(4)    VALUE '0195'.
00094      12  ER-0196                 PIC X(4)    VALUE '0196'.
00095      12  ER-0197                 PIC X(4)    VALUE '0197'.
00096      12  ER-0340                 PIC X(4)    VALUE '0340'.
00097      12  ER-2201                 PIC X(4)    VALUE '2201'.
00098      12  ER-2208                 PIC X(4)    VALUE '2208'.
00099      12  ER-2209                 PIC X(4)    VALUE '2209'.
00100      12  ER-2210                 PIC X(4)    VALUE '2210'.
00101      12  ER-2211                 PIC X(4)    VALUE '2211'.
00102      12  ER-2212                 PIC X(4)    VALUE '2212'.
00103      12  ER-2213                 PIC X(4)    VALUE '2213'.
00104      12  ER-2214                 PIC X(4)    VALUE '2214'.
00105      12  ER-2215                 PIC X(4)    VALUE '2215'.
00106      12  ER-2216                 PIC X(4)    VALUE '2216'.
00107      12  ER-2229                 PIC X(4)    VALUE '2229'.
00108      12  ER-2242                 PIC X(4)    VALUE '2242'.
00109      12  ER-2248                 PIC X(4)    VALUE '2248'.
00110      12  ER-2370                 PIC X(4)    VALUE '2370'.
00111      12  ER-2371                 PIC X(4)    VALUE '2371'.
00112      12  ER-2402                 PIC X(4)    VALUE '2402'.
00113      12  ER-2422                 PIC X(4)    VALUE '2422'.
00114
00115      EJECT
00116
00117  01  ACCESS-KEYS.
00118      12  ELCNTL-KEY.
00119          16  CNTL-COMP-ID        PIC X(3)  VALUE SPACES.
00120          16  CNTL-REC-TYPE       PIC X     VALUE SPACES.
00121          16  CNTL-ACCESS.
00122              20  CNTL-STATE      PIC XX    VALUE SPACES.
00123              20  FILLER          PIC X     VALUE SPACES.
00124              20  CNTL-CARRIER    PIC X     VALUE SPACES.
00125          16  CNTL-SEQ            PIC S9(4) VALUE +0 COMP.
00126
00127      12  ERPNDT-KEY.
00128          16  PNDT-COMP-CD        PIC X     VALUE SPACE.
00129          16  PNDT-ENTRY-BATCH    PIC X(6)  VALUE SPACES.
00130          16  PNDT-BATCH-SEQ      PIC S9(4) VALUE +0 COMP.
00131          16  PNDT-BATCH-CHG-SEQ  PIC S9(4) VALUE +0 COMP.
00132
00133      12  ELCERT-KEY.
00134          16  CERT-COMPANY-CD     PIC X     VALUE SPACES.
00135          16  CERT-CARRIER        PIC X     VALUE SPACES.
00136          16  CERT-GROUPING       PIC X(6)  VALUE SPACES.
00137          16  CERT-STATE          PIC XX    VALUE SPACES.
00138          16  FILLER              PIC X(23) VALUE SPACES.
00139
00140  01  FILLER.
00141      12  WS-DEEDIT-FIELD         PIC S9(8)V99.
00142      12  WS-DT-DEEDIT-FIELD REDEFINES
00143          WS-DEEDIT-FIELD         PIC X(10).
00144      12  QUESTION-MARKS          PIC X(6)  VALUE '??????'.
00145      12  WS-BATCH-NO             PIC 9(6)  VALUE ZEROS.
00146      12  WS-OBAL                 PIC S9(8)V99  VALUE ZEROS.
00147      12  WS-OCNT                 PIC S9(5)     VALUE ZEROS.
00148      12  WS-DELETE-CNT           PIC S9(5)     VALUE ZEROS.
00149      12  WS-PF1-SW               PIC X         VALUE SPACES.
00150          88  WS-PF1                            VALUE 'Y'.
00151
00152      12  WS-SAV-PNDT-KEY.
00153          16  WS-SAV-COMP-CD        PIC X.
00154          16  WS-SAV-ENTRY-BATCH    PIC X(6).
00155          16  WS-SAV-BATCH-SEQ      PIC S9(4) COMP.
00156          16  WS-SAV-BATCH-CHG-SEQ  PIC S9(4) COMP.
00157
00158      12  DATE-TEST-AREA          PIC 9(6).
00159      12  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.
00160          16  DATE-TEST-MM        PIC 99.
00161          16  DATE-TEST-DD        PIC 99.
00162          16  DATE-TEST-YY        PIC 99.
00163      12  DIVIDE-RESULT           PIC 99.
00164      12  DIVIDE-REMAINDER        PIC 9.
00165      12  WS-CERT-NOTE-SW         PIC X   VALUE ' '.
00166          88 CERT-NOTES-ARE-PRESENT  VALUE 'Y'.
00167      12  WS-CERT-ADDRESS-SW      PIC X   VALUE ' '.
00168          88 CERT-ADDRESS-PRESENT     VALUE 'Y'.
00169      12  WS-PRM-HEADER.
00170          16  WS-PRM-OVERRIDE     PIC XX    VALUE SPACES.
00171          16  FILLER              PIC X(8)  VALUE '-PREMIUM'.
00172      12  WS-REFUND-HEADER.
00173          16  WS-REFUND-OVERRIDE  PIC XX    VALUE SPACES.
00174          16  FILLER              PIC X(7)  VALUE '-REFUND'.
00175
00176      EJECT
00177
00178 *                            COPY ELCDATE.
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
00179
00180      EJECT
00181 *                            COPY ELCLOGOF.
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
00182
00183      EJECT
00184 *                            COPY ELCATTR.
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
00185
00186      EJECT
00187 *                                  COPY ELCEMIB.
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
00188
00189      EJECT
00190 *                            COPY ELCINTF.
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
00191 *    COPY ELC930PI.
00001
00002 ******************************************************************
00003 *                            ELC930PI                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE DATA  *
00007 *    ENTRY SUB-SYSTEM.                                           *
00008 *                                                                *
00009 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK.                   *
00010 *                                                                *
00011 *               EL930 - EL9301 - EL9302                          *
00012 ******************************************************************
00013
00014      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00015          16  PI-AM-NAME                  PIC X(30).
00016          16  PI-MAP-NAME                 PIC X(8).
00017          16  PI-BATCH-AMOUNTS    COMP-3.
00018              20  PI-LF-ISS-REMITTED      PIC S9(8)V99.
00019              20  PI-LF-ISS-ENTERED       PIC S9(8)V99.
00020              20  PI-LF-CAN-REMITTED      PIC S9(8)V99.
00021              20  PI-LF-CAN-ENTERED       PIC S9(8)V99.
00022              20  PI-AH-ISS-REMITTED      PIC S9(8)V99.
00023              20  PI-AH-ISS-ENTERED       PIC S9(8)V99.
00024              20  PI-AH-CAN-REMITTED      PIC S9(8)V99.
00025              20  PI-AH-CAN-ENTERED       PIC S9(8)V99.
00026              20  PI-ISS-CNT-REMITTED     PIC S9(5).
00027              20  PI-ISS-CNT-ENTERED      PIC S9(5).
00028              20  PI-CAN-CNT-REMITTED     PIC S9(5).
00029              20  PI-CAN-CNT-ENTERED      PIC S9(5).
00030          16  PI-MAINT-FUNC               PIC X.
00031          16  PI-ERROR-SW                 PIC X.
00032              88  PI-DATA-ERRORS              VALUE 'Y'.
00033          16  PI-UPDATE-SW                PIC X.
00034              88  PI-DATA-UPDATED             VALUE 'Y'.
00035          16  PI-DISPLAY-SW               PIC X.
00036              88  PI-LAST-FUNC-DISPLAY        VALUE 'Y'.
00037          16  PI-SAVE-CALLING-PGM         PIC X(8).
00038          16  PI-LAST-SEQ-NO-ADDED        PIC S9(4) COMP.
00039          16  PI-NEXT-DISPLAY-SEQ-NO      PIC S9(4) COMP.
00040          16  PI-SAV-CARRIER              PIC X.
00041          16  PI-SAV-GROUPING             PIC X(6).
00042          16  PI-SAV-STATE                PIC XX.
00043          16  PI-SAV-ACCOUNT              PIC X(10).
00044          16  PI-SAV-CERT-EFF-DT          PIC XX.
00045          16  PI-SAV-CERT-NO.
00046              20  PI-SAV-CERT-PRIME       PIC X(14).
00047              20  PI-SAV-CERT-SFX         PIC X.
00048          16  PI-SAV-ENDING-ERPNDT-KEY.
00049              20  PI-SAV-COMP-CD          PIC X.
00050              20  PI-SAV-ENTRY-BATCH      PIC X(6).
00051              20  PI-SAV-BATCH-SEQ        PIC S9(4) COMP.
00052              20  PI-SAV-BATCH-CHG-SEQ    PIC S9(4) COMP.
00053          16  PI-VERIFY-DELETE-SW         PIC X.
00054              88  PI-DELETE-IS-OK             VALUE 'Y'.
00055          16  PI-EL930-FIRST-TIME-SW      PIC X.
00056              88  PI-EL930-FIRST-TIME         VALUE SPACE.
00057          16  PI-CREDIT-EDIT-CONTROLS.
00058              20  PI-MIN-PREMIUM          PIC S9(3)V99  COMP-3.
00059              20  PI-MIN-AGE              PIC 99.
00060              20  PI-DEFAULT-AGE          PIC 99.
00061              20  PI-MIN-TERM             PIC S9(3)     COMP-3.
00062              20  PI-MAX-TERM             PIC S9(3)     COMP-3.
00063              20  PI-DEFAULT-SEX          PIC X.
00064              20  PI-JOINT-AGE-INPUT      PIC X.
00065                  88 PI-JOINT-AGE-IS-INPUT       VALUE '1'.
00066              20  PI-BIRTH-DATE-INPUT     PIC X.
00067                  88 PI-BIRTH-DATE-IS-INPUT      VALUE '1'.
00068          16  PI-KEYED-SWITCHES.
00069              20  PI-ISS-SUFFIX-KEYED-SW  PIC X.
00070                  88  PI-ISS-SUFFIX-KEYED     VALUE 'Y'.
00071              20  PI-CAN-SUFFIX-KEYED-SW  PIC X.
00072                  88  PI-CAN-SUFFIX-KEYED     VALUE 'Y'.
00073              20  PI-IG-KEYED-SW          PIC X.
00074                  88  PI-IG-KEYED             VALUE 'Y'.
00075              20  PI-APR-KEYED-SW         PIC X.
00076                  88  PI-APR-KEYED            VALUE 'Y'.
00077              20  PI-FREQ-KEYED-SW        PIC X.
00078                  88  PI-FREQ-KEYED           VALUE 'Y'.
00079              20  PI-SIG-KEYED-SW         PIC X.
00080                  88  PI-SIG-KEYED            VALUE 'Y'.
00081              20  PI-LFRT-KEYED-SW        PIC X.
00082                  88  PI-LFRT-KEYED           VALUE 'Y'.
00083              20  PI-AHRT-KEYED-SW        PIC X.
00084                  88  PI-AHRT-KEYED           VALUE 'Y'.
00085              20  PI-SSNUM-KEYED-SW       PIC X.
00086                  88  PI-SSNUM-KEYED          VALUE 'Y'.
00087              20  PI-JNT-SSNUM-KEYED-SW   PIC X.
00088                  88  PI-JNT-SSNUM-KEYED      VALUE 'Y'.
00089              20  PI-MEMBER-KEYED-SW      PIC X.
00090                  88  PI-MEMBER-KEYED         VALUE 'Y'.
00091              20  PI-MODE-KEYED-SW        PIC X.
00092                  88  PI-MODE-KEYED           VALUE 'Y'.
00093              20  PI-PMTS-KEYED-SW        PIC X.
00094                  88  PI-PMTS-KEYED           VALUE 'Y'.
00095              20  PI-LN-OFFICER-KEYED-SW  PIC X.
00096                  88  PI-LN-OFFICER-KEYED     VALUE 'Y'.
00097              20  PI-ENTRY-KEYED-SW       PIC X.
00098                  88  PI-ENTRY-KEYED          VALUE 'Y'.
00099              20  PI-RINCD-KEYED-SW       PIC X.
00100                  88  PI-RINCD-KEYED          VALUE 'Y'.
00101              20  PI-RTCLS-KEYED-SW       PIC X.
00102                  88  PI-RTCLS-KEYED          VALUE 'Y'.
00103              20  PI-LNTRM-KEYED-SW       PIC X.
00104                  88  PI-LNTRM-KEYED          VALUE 'Y'.
00105              20  PI-EXPIR-KEYED-SW       PIC X.
00106                  88  PI-EXPIR-KEYED          VALUE 'Y'.
00107              20  PI-PMT-KEYED-SW         PIC X.
00108                  88  PI-PMT-KEYED            VALUE 'Y'.
00109              20  PI-1ST-PMT-KEYED-SW     PIC X.
00110                  88  PI-1ST-PMT-KEYED        VALUE 'Y'.
00111              20  PI-DAYS-KEYED-SW        PIC X.
00112                  88  PI-DAYS-KEYED           VALUE 'Y'.
00113              20  PI-SKPCD-KEYED-SW       PIC X.
00114                  88  PI-SKPCD-KEYED          VALUE 'Y'.
00115              20  PI-JNT-AGE-KEYED-SW     PIC X.
00116                  88  PI-JNT-AGE-KEYED        VALUE 'Y'.
00117              20  PI-JNT-NAME-KEYED-SW    PIC X.
00118                  88  PI-JNT-NAME-KEYED       VALUE 'Y'.
00119              20  PI-ISS-LIVES-KEYED-SW   PIC X.
00120                  88  PI-ISS-LIVES-KEYED      VALUE 'Y'.
00121              20  PI-CAN-LIVES-KEYED-SW   PIC X.
00122                  88  PI-CAN-LIVES-KEYED      VALUE 'Y'.
00123              20  PI-PAYEE-KEYED-SW       PIC X.
00124                  88  PI-PAYEE-KEYED          VALUE 'Y'.
00125              20  PI-CHK-REQ-KEYED-SW     PIC X.
00126                  88  PI-CHK-REQ-KEYED        VALUE 'Y'.
00127              20  PI-ZIP4-KEYED-SW        PIC X.
00128                  88  PI-ZIP4-KEYED           VALUE 'Y'.
00129              20  PI-POLICY-KEYED-SW      PIC X.
00130                  88  PI-POLICY-KEYED         VALUE 'Y'.
00131              20  PI-EXPIRE-KEYED-SW      PIC X.
00132                  88  PI-EXPIRE-KEYED         VALUE 'Y'.
00133              20  PI-CRIT-PERD-KEYED-SW    PIC X.
00134                  88  PI-CRIT-PERD-KEYED      VALUE 'Y'.
00135              20  PI-BENEFICIARY-KEYED-SW PIC X.
00136                  88  PI-BENEFICIARY-KEYED    VALUE 'Y'.
00137              20  PI-PHONE-KEYED-SW       PIC X.
00138                  88  PI-PHONE-KEYED          VALUE 'Y'.
00139              20  PI-ALT-BEN-KEYED-SW     PIC X.
00140                  88  PI-ALT-BEN-KEYED        VALUE 'Y'.
00141              20  PI-ALT-PREM-KEYED-SW    PIC X.
00142                  88  PI-ALT-PREM-KEYED       VALUE 'Y'.
00143          16  PI-ACCT-LOW-EFF-DT          PIC XX.
00144          16  PI-ACCT-HIGH-EXP-DT         PIC XX.
00145          16  PI-BATCH-EOF-SW             PIC X.
00146              88  PI-BATCH-EOF                VALUE 'Y'.
00147          16  PI-NB-MONTH-END-DT          PIC XX.
00148          16  PI-ISSUE-ADDED-SW           PIC X.
00149              88  PI-ISSUE-ADDED              VALUE 'Y'.
00150
00151      EJECT
00192
00193      EJECT
00194 *                            COPY ELCJPFX.
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
00195                              PIC X(585).
00196
00197      EJECT
00198 *                            COPY ELCAID.
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
00199  01  FILLER    REDEFINES DFHAID.
00200      12  FILLER              PIC X(8).
00201      12  PF-VALUES           PIC X       OCCURS 2.
00202
00203      EJECT
00204 *                            COPY EL930S.
       01  EL930AI.
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
           05  BATCHL PIC S9(0004) COMP.
           05  BATCHF PIC  X(0001).
           05  FILLER REDEFINES BATCHF.
               10  BATCHA PIC  X(0001).
           05  BATCHI PIC  X(0006).
      *    -------------------------------
           05  LFPHDGL PIC S9(0004) COMP.
           05  LFPHDGF PIC  X(0001).
           05  FILLER REDEFINES LFPHDGF.
               10  LFPHDGA PIC  X(0001).
           05  LFPHDGI PIC  X(0010).
      *    -------------------------------
           05  AHPHDGL PIC S9(0004) COMP.
           05  AHPHDGF PIC  X(0001).
           05  FILLER REDEFINES AHPHDGF.
               10  AHPHDGA PIC  X(0001).
           05  AHPHDGI PIC  X(0010).
      *    -------------------------------
           05  LFRHDGL PIC S9(0004) COMP.
           05  LFRHDGF PIC  X(0001).
           05  FILLER REDEFINES LFRHDGF.
               10  LFRHDGA PIC  X(0001).
           05  LFRHDGI PIC  X(0009).
      *    -------------------------------
           05  AHRHDGL PIC S9(0004) COMP.
           05  AHRHDGF PIC  X(0001).
           05  FILLER REDEFINES AHRHDGF.
               10  AHRHDGA PIC  X(0001).
           05  AHRHDGI PIC  X(0009).
      *    -------------------------------
           05  EISSCNTL PIC S9(0004) COMP.
           05  EISSCNTF PIC  X(0001).
           05  FILLER REDEFINES EISSCNTF.
               10  EISSCNTA PIC  X(0001).
           05  EISSCNTI PIC  S9(6).
      *    -------------------------------
           05  ELFISSL PIC S9(0004) COMP.
           05  ELFISSF PIC  X(0001).
           05  FILLER REDEFINES ELFISSF.
               10  ELFISSA PIC  X(0001).
           05  ELFISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  EAHISSL PIC S9(0004) COMP.
           05  EAHISSF PIC  X(0001).
           05  FILLER REDEFINES EAHISSF.
               10  EAHISSA PIC  X(0001).
           05  EAHISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  ECANCNTL PIC S9(0004) COMP.
           05  ECANCNTF PIC  X(0001).
           05  FILLER REDEFINES ECANCNTF.
               10  ECANCNTA PIC  X(0001).
           05  ECANCNTI PIC  S9(6).
      *    -------------------------------
           05  ELFCANL PIC S9(0004) COMP.
           05  ELFCANF PIC  X(0001).
           05  FILLER REDEFINES ELFCANF.
               10  ELFCANA PIC  X(0001).
           05  ELFCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  EAHCANL PIC S9(0004) COMP.
           05  EAHCANF PIC  X(0001).
           05  FILLER REDEFINES EAHCANF.
               10  EAHCANA PIC  X(0001).
           05  EAHCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  AISSCNTL PIC S9(0004) COMP.
           05  AISSCNTF PIC  X(0001).
           05  FILLER REDEFINES AISSCNTF.
               10  AISSCNTA PIC  X(0001).
           05  AISSCNTI PIC  S9(6).
      *    -------------------------------
           05  ALFISSL PIC S9(0004) COMP.
           05  ALFISSF PIC  X(0001).
           05  FILLER REDEFINES ALFISSF.
               10  ALFISSA PIC  X(0001).
           05  ALFISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  AAHISSL PIC S9(0004) COMP.
           05  AAHISSF PIC  X(0001).
           05  FILLER REDEFINES AAHISSF.
               10  AAHISSA PIC  X(0001).
           05  AAHISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  ACANCNTL PIC S9(0004) COMP.
           05  ACANCNTF PIC  X(0001).
           05  FILLER REDEFINES ACANCNTF.
               10  ACANCNTA PIC  X(0001).
           05  ACANCNTI PIC  S9(6).
      *    -------------------------------
           05  ALFCANL PIC S9(0004) COMP.
           05  ALFCANF PIC  X(0001).
           05  FILLER REDEFINES ALFCANF.
               10  ALFCANA PIC  X(0001).
           05  ALFCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  AAHCANL PIC S9(0004) COMP.
           05  AAHCANF PIC  X(0001).
           05  FILLER REDEFINES AAHCANF.
               10  AAHCANA PIC  X(0001).
           05  AAHCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  OISSCNTL PIC S9(0004) COMP.
           05  OISSCNTF PIC  X(0001).
           05  FILLER REDEFINES OISSCNTF.
               10  OISSCNTA PIC  X(0001).
           05  OISSCNTI PIC  S9(6).
      *    -------------------------------
           05  OLFISSL PIC S9(0004) COMP.
           05  OLFISSF PIC  X(0001).
           05  FILLER REDEFINES OLFISSF.
               10  OLFISSA PIC  X(0001).
           05  OLFISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  OAHISSL PIC S9(0004) COMP.
           05  OAHISSF PIC  X(0001).
           05  FILLER REDEFINES OAHISSF.
               10  OAHISSA PIC  X(0001).
           05  OAHISSI PIC  S9(10)V9(2).
      *    -------------------------------
           05  OCANCNTL PIC S9(0004) COMP.
           05  OCANCNTF PIC  X(0001).
           05  FILLER REDEFINES OCANCNTF.
               10  OCANCNTA PIC  X(0001).
           05  OCANCNTI PIC  S9(6).
      *    -------------------------------
           05  OLFCANL PIC S9(0004) COMP.
           05  OLFCANF PIC  X(0001).
           05  FILLER REDEFINES OLFCANF.
               10  OLFCANA PIC  X(0001).
           05  OLFCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  OAHCANL PIC S9(0004) COMP.
           05  OAHCANF PIC  X(0001).
           05  FILLER REDEFINES OAHCANF.
               10  OAHCANA PIC  X(0001).
           05  OAHCANI PIC  S9(8)V9(2).
      *    -------------------------------
           05  MNTHNDTL PIC S9(0004) COMP.
           05  MNTHNDTF PIC  X(0001).
           05  FILLER REDEFINES MNTHNDTF.
               10  MNTHNDTA PIC  X(0001).
           05  MNTHNDTI PIC  X(0008).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0076).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0076).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL930AO REDEFINES EL930AI.
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
           05  BATCHO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPHDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPHDGO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFRHDGO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRHDGO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EISSCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ELFISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EAHISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ECANCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ELFCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EAHCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AISSCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACANCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AAHCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OISSCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OLFISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OAHISSO PIC  Z(8).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OCANCNTO PIC  Z(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OLFCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OAHCANO PIC  Z(6).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MNTHNDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00205
00206      EJECT
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
00208  01  DFHCOMMAREA             PIC X(1024).
00209
00210      EJECT
00211  01  PARMLIST      COMP.
00212      02  FILLER              PIC S9(8).
00213      02  ELCNTL-POINTER      PIC S9(8).
00214      02  ERPNDT-POINTER      PIC S9(8).
00215      02  ELCERT-POINTER      PIC S9(8).
00216      02  ERNOTE-POINTER      PIC S9(8).
00217      02  ERMAIL-POINTER      PIC S9(8).
00218      02  ERPNDM-POINTER      PIC S9(8).
00219
00220 *                            COPY ELCCNTL.
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
00221      EJECT
00222 *                            COPY ERCPNDB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
122002******************************************************************
00032
00033  01  PENDING-BUSINESS.
00034      12  PB-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  PB-CONTROL-PRIMARY.
00038          16  PB-COMPANY-CD                PIC X.
00039          16  PB-ENTRY-BATCH               PIC X(6).
00040          16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  PB-CONTROL-BY-ACCOUNT.
00044          16  PB-COMPANY-CD-A1             PIC X.
00045          16  PB-CARRIER                   PIC X.
00046          16  PB-GROUPING.
00047              20  PB-GROUPING-PREFIX       PIC XXX.
00048              20  PB-GROUPING-PRIME        PIC XXX.
00049          16  PB-STATE                     PIC XX.
00050          16  PB-ACCOUNT.
00051              20  PB-ACCOUNT-PREFIX        PIC X(4).
00052              20  PB-ACCOUNT-PRIME         PIC X(6).
00053          16  PB-CERT-EFF-DT               PIC XX.
00054          16  PB-CERT-NO.
00055              20  PB-CERT-PRIME            PIC X(10).
00056              20  PB-CERT-SFX              PIC X.
00057          16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  PB-RECORD-TYPE               PIC X.
00060              88  PB-MAILING-DATA                VALUE '0'.
00061              88  PB-ISSUE                       VALUE '1'.
00062              88  PB-CANCELLATION                VALUE '2'.
00063              88  PB-BATCH-TRAILER               VALUE '9'.
00064
00065      12  PB-CONTROL-BY-ORIG-BATCH.
00066          16  PB-ORIGINAL-COMPANY-CD       PIC X.
00067          16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  PB-CONTROL-BY-CSR.
00072          16  PB-CSR-COMPANY-CD            PIC X.
00073          16  PB-CSR-ID                    PIC X(4).
00074          16  PB-CSR-ENTRY-BATCH           PIC X(6).
00075          16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  PB-LAST-MAINT-DT                 PIC XX.
00082      12  PB-LAST-MAINT-BY                 PIC X(4).
00083      12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  PB-RECORD-BODY                   PIC X(375).
00086
00087      12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
00088          16  PB-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  PB-I-NAME.
00091              20  PB-I-INSURED-LAST-NAME   PIC X(15).
00092              20  PB-I-INSURED-FIRST-NAME.
00093                  24  PB-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  PB-I-INSURED-MIDDLE-INIT PIC X.
00096          16  PB-I-AGE                     PIC S99   COMP-3.
00097          16  PB-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  PB-I-BIRTHDAY                PIC XX.
00099          16  PB-I-INSURED-SEX             PIC X.
00100              88  PB-SEX-MALE     VALUE 'M'.
00101              88  PB-SEX-FEMALE   VALUE 'F'.
00102
00103          16  PB-I-LF-TERM                 PIC S999   COMP-3.
00104          16  PB-I-AH-TERM                 PIC S999   COMP-3.
00105          16  PB-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  PB-I-SKIP-CODE               PIC X.
00108              88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  PB-SKIP-JULY              VALUE '1'.
00110              88  PB-SKIP-AUGUST            VALUE '2'.
00111              88  PB-SKIP-SEPTEMBER         VALUE '3'.
00112              88  PB-SKIP-JULY-AUG          VALUE '4'.
00113              88  PB-SKIP-AUG-SEPT          VALUE '5'.
00114              88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  PB-SKIP-JUNE              VALUE '8'.
00117              88  PB-SKIP-JUNE-JULY         VALUE '9'.
00118              88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  PB-I-TERM-TYPE               PIC X.
00121              88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  PB-PAID-WEEKLY            VALUE 'W'.
00123              88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  PB-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  PB-PAID-13-YEARLY         VALUE 'T'.
00126          16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  PB-I-POLICY-FORM-NO          PIC X(12).
00128          16  PB-I-DATA-ENTRY-SW           PIC X.
00129              88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
011410         16  FILLER                       PIC X.
00136
00137          16  PB-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  PB-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  PB-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  PB-I-AMOUNT-FINANCED REDEFINES
100703                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  PB-I-UNPAID-CASH-PRICE REDEFINES
100703                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  PB-I-CLP-AMOUNT REDEFINES
100703                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  PB-I-LF-CALC-FLAG            PIC X.
00148              88 PB-COMP-LF-PREM               VALUE '?'.
00149          16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  PB-I-LF-ABBR                 PIC XXX.
00157          16  PB-I-LF-INPUT-CD             PIC XX.
00158
00159          16  PB-I-AH-BENEFIT-CD           PIC XX.
00160              88  PB-VALID-AH                 VALUE '01' THRU '89'.
00161              88  PB-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  PB-I-AH-CALC-FLAG            PIC X.
00166              88 PB-COMP-AH-PREM                  VALUE '?'.
00167          16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
00169          16  PB-I-AH-POLICY-FEE           PIC S9(3)V99   COMP-3.
00170          16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  PB-I-AH-ABBR                 PIC XXX.
00173          16  PB-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  PB-I-SPECIAL-REIN-CODE       PIC X.
00176          16  PB-I-REIN-TABLE              PIC XXX.
00177          16  PB-I-BUSINESS-TYPE           PIC 99.
00178          16  PB-I-INDV-GRP-CD             PIC X.
00179          16  PB-I-MORT-CODE.
00180              20  PB-I-TABLE               PIC X.
00181              20  PB-I-INTEREST            PIC XX.
00182              20  PB-I-MORT-TYP            PIC X.
00183          16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  PB-I-INDV-GRP-OVRD           PIC X.
00187          16  PB-I-RATE-CLASS-OVRD         PIC XX.
00188          16  PB-I-SIG-SW                  PIC X.
00189              88  PB-POLICY-SIGNED             VALUE 'Y'.
00190          16  PB-I-RATE-CLASS              PIC XX.
00191          16  PB-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  PB-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  PB-I-BENEFIT-TYPE            PIC XXX.
00199          16  PB-I-OB-FLAG                 PIC X.
00200              88  PB-I-OB                      VALUE 'B'.
00201              88  PB-I-SUMMARY                 VALUE 'Z'.
00202          16  PB-I-ENTRY-STATUS            PIC X.
00203              88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  PB-I-NORMAL-ENTRY            VALUE '1'.
00206              88  PB-I-POLICY-PENDING          VALUE '2'.
00207              88  PB-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  PB-I-REIN-ONLY               VALUE '9'.
00210              88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  PB-I-INT-CODE                PIC X.
00215              88  PB-ADD-ON-INTEREST           VALUE 'A'.
00216              88  PB-SIMPLE-INTEREST           VALUE 'S'.
00217          16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  PB-I-SOC-SEC-NO              PIC X(11).
00219          16  PB-I-MEMBER-NO               PIC X(12).
00220          16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  PB-I-OLD-LOF                 PIC XXX.
00222          16  PB-I-LF-EXPIRE-DT            PIC XX.
00223          16  PB-I-AH-EXPIRE-DT            PIC XX.
00224          16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  PB-I-LIFE-INDICATOR          PIC X.
00227              88  PB-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  PB-I-LIVES                   PIC S9(7)       COMP-3.
00229          16  PB-I-MAIL-ADDRS-SW           PIC X.
00230              88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  PB-I-1ST-PMT-DT              PIC XX.
00233          16  PB-I-JOINT-INSURED.
00234              20 PB-I-JOINT-LAST-NAME      PIC X(15).
00235              20 PB-I-JOINT-FIRST-NAME.
00236                 24  PB-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 PB-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  PB-I-BENEFICIARY-NAME.
100703             20  PB-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  PB-I-LAST-ADD-ON-DT          PIC XX.
011904         16  PB-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES PB-I-REFERENCE.
011904             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  PB-I-CLP-STATE           PIC XX.
00242          16  PB-I-UNDERWRITING-STATUS     PIC X.
00243              88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  PB-I-POLICY-DECLINED         VALUE 'D'.
00245              88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  PB-I-RESIDENT-STATE          PIC XX.
00249          16  PB-I-RATE-CODE               PIC X(4).
00250          16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  PB-I-BANK-NOCHRGB            PIC 99.
040504         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  PB-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
00254          16  PB-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  PB-C-CANCEL-ORIGIN           PIC X.
00257              88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  PB-C-LF-CANCEL-DT            PIC XX.
00259          16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  PB-C-LF-CALC-REQ             PIC X.
00261              88 PB-COMP-LF-CANCEL            VALUE '?'.
00262          16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  PB-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  PB-C-AH-CANCEL-DT            PIC XX.
00267          16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  PB-C-AH-CALC-REQ             PIC X.
00269              88 PB-COMP-AH-CANCEL            VALUE '?'.
00270          16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  PB-C-LAST-NAME               PIC X(15).
00273          16  PB-C-REFUND-SW               PIC X.
00274              88  PB-C-REFUND-CREATED          VALUE 'Y'.
00275              88  PB-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  PB-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  PB-C-PAYEE-CODE              PIC X(6).
00278          16  PB-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  PB-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  PB-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  PB-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  PB-C-REFERENCE               PIC X(12).
PEMMOD         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  PB-C-POST-CARD-IND           PIC X.
081606         16  PB-C-CANCEL-REASON           PIC X.
072308         16  PB-C-REF-INTERFACE-SW        PIC X.
00283          16  FILLER                       PIC X(09).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  PB-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
072308         16  PB-C-NH-INT-ON-REFS          PIC S9(7)V99   COMP-3.
00286          16  PB-CANCELED-CERT-DATA.
00287              20  PB-CI-INSURED-NAME.
00288                  24  PB-CI-LAST-NAME      PIC X(15).
00289                  24  PB-CI-INITIALS       PIC XX.
00290              20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  PB-CI-INSURED-SEX        PIC X.
00292              20  PB-CI-LF-TERM            PIC S999        COMP-3.
00293              20  PB-CI-LF-BENEFIT-CD      PIC XX.
00294              20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  PB-CI-AH-TERM            PIC S999        COMP-3.
00299              20  PB-CI-AH-BENEFIT-CD      PIC XX.
00300              20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  PB-CI-RATE-CLASS         PIC XX.
00303              20  PB-CI-RATE-DEV-LF        PIC XXX.
00304              20  PB-CI-RATE-DEV-AH        PIC XXX.
00305              20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  PB-CI-LF-ABBR            PIC X(3).
00310              20  PB-CI-AH-ABBR            PIC X(3).
00311              20  PB-CI-OB-FLAG            PIC X.
00312                  88  PB-CI-OB                VALUE 'B'.
00313              20  PB-CI-LF-POLICY-STATUS   PIC X.
00314                  88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  PB-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  PB-CI-AH-POLICY-STATUS   PIC X.
00328                  88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  PB-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  PB-CI-PAY-FREQUENCY      PIC 99.
00342              20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  PB-CI-SOC-SEC-NO         PIC X(11).
00344              20  PB-CI-MEMBER-NO          PIC X(12).
00345              20  PB-CI-INT-CODE           PIC X.
00346                  88  PB-CI-ADD-ON                  VALUE 'A'.
00347                  88  PB-CI-SIMPLE                  VALUE 'S'.
00348              20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  PB-CI-COMP-EXCP-SW       PIC X.
00351                  88  PB-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  PB-CI-ENTRY-STATUS       PIC X.
00354              20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  PB-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  PB-CI-DEATH-DT           PIC XX.
00358              20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  PB-CI-ENTRY-DT              PIC XX.
00365              20  PB-CI-ENTRY-BATCH           PIC X(6).
00366              20  PB-CI-LF-EXPIRE-DT          PIC XX.
00367              20  PB-CI-AH-EXPIRE-DT          PIC XX.
00368              20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  PB-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  PB-CI-INDV-GRP-CD           PIC X.
100703             20  PB-CI-BENEFICIARY-NAME.
100703                 24  PB-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  PB-CI-NOTE-SW               PIC X.
00377              20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  PB-CI-LOAN-OFFICER          PIC X(5).
032306             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  PB-CI-FIRST-NAME            PIC X(10).
00380
072209         16  FILLER                       PIC X(17).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  PB-M-INSURED-LAST-NAME       PIC X(15).
00386          16  PB-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  PB-M-INSURED-MID-INIT        PIC X.
00388          16  PB-M-INSURED-AGE             PIC 99.
00389          16  PB-M-INSURED-BIRTHDAY        PIC XX.
00390          16  PB-M-INSURED-SEX             PIC X.
00391          16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  PB-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  PB-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  PB-M-INSURED-CITY-STATE.
051810             20  PB-M-INSURED-CITY        PIC X(28).
051810             20  PB-M-INSURED-STATE       PIC XX.
00395          16  PB-M-INSURED-ZIP-CODE.
00396              20  PB-M-INSURED-ZIP-PRIME.
00397                  24  PB-M-INSURED-ZIP-1   PIC X.
00398                      88  PB-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         PB-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  PB-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  PB-M-JOINT-BIRTHDAY          PIC XX.
               16  PB-M-CRED-BENE-NAME          PIC X(30).
               16  PB-M-CRED-BENE-ADDR1         PIC X(30).
               16  PB-M-CRED-BENE-ADDR2         PIC X(30).
               16  PB-M-CRED-BENE-CITYST.
                   20  PB-M-CRED-BENE-CITY      PIC X(28).
                   20  PB-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  PB-ACCOUNT-NAME              PIC X(30).
00430          16  PB-PREM-REF-RPT-FLAG         PIC X.
00431          16  PB-REFERENCE                 PIC X(12).
00432          16  PB-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  PB-RECORD-STATUS.
00436          16  PB-CREDIT-SELECT-DT          PIC XX.
00437          16  PB-CREDIT-ACCEPT-DT          PIC XX.
00438          16  PB-BILLED-DT                 PIC XX.
00439          16  PB-BILLING-STATUS            PIC X.
00440              88  PB-ENTRY-REVERSED            VALUE 'R'.
00441              88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  PB-RECORD-BILL               PIC X.
00444              88  PB-RECORD-ON-HOLD            VALUE 'H'.
00445              88  PB-RECORD-RETURNED           VALUE 'R'.
00446              88  PB-RECORD-ENDORSED           VALUE 'E'.
00447              88  PB-OVERRIDE-LIFE             VALUE 'L'.
00448              88  PB-OVERRIDE-AH               VALUE 'A'.
00449              88  PB-OVERRIDE-BOTH             VALUE 'B'.
00450          16  PB-BATCH-ENTRY               PIC X.
00451              88  PB-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  PB-REIN-ONLY-CERT            VALUE 'R'.
00453              88  PB-REISSUED-CERT             VALUE 'E'.
122002             88  PB-MONTHLY-CERT              VALUE 'M'.
00454              88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  PB-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  PB-FORCE-CODE                PIC X.
00458              88  PB-FORCE-OFF                 VALUE ' ' '0'.
00459              88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  PB-CANCEL-FORCE              VALUE '8'.
00461              88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  PB-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
073107             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  PB-FATAL-FLAG                PIC X.
00468              88  PB-FATAL-ERRORS              VALUE 'X'.
00469          16  PB-FORCE-ER-CD               PIC X.
00470              88  PB-FORCE-ERRORS              VALUE 'F'.
00471              88  PB-UNFORCED-ERRORS           VALUE 'X'.
00472          16  PB-WARN-ER-CD                PIC X.
00473              88  PB-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  PB-OUT-BAL-CD                PIC X.
00476              88  PB-OUT-OF-BAL                VALUE 'O'.
00477          16  PB-LIFE-OVERRIDE-L1          PIC X.
00478          16  PB-AH-OVERRIDE-L1            PIC X.
00479          16  PB-INPUT-DT                  PIC XX.
00480          16  PB-INPUT-BY                  PIC X(4).
00481          16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  PB-TOLERANCE-REJECT-SW       PIC X.
00484          16  PB-LF-EARNING-METHOD         PIC X.
00485          16  PB-AH-EARNING-METHOD         PIC X.
00486          16  PB-LF-TERM-CALC-METHOD       PIC X.
00487          16  PB-AH-TERM-CALC-METHOD       PIC X.
00488          16  PB-REIN-CD                   PIC XXX.
00489          16  PB-LF-REFUND-TYPE            PIC X.
00490          16  PB-AH-REFUND-TYPE            PIC X.
00491          16  PB-ACCT-EFF-DT               PIC XX.
00492          16  PB-ACCT-EXP-DT               PIC XX.
00493          16  PB-COMPANY-ID                PIC X(3).
00494          16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  PB-SV-CARRIER                PIC X.
00497          16  PB-SV-GROUPING               PIC X(6).
00498          16  PB-SV-STATE                  PIC XX.
00499          16  PB-CONFIRMATION-REPT-DT      PIC XX.
00500          16  PB-GA-BILLING-INFO.
00501              20  PB-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  PB-SV-REMIT-TO  REDEFINES
00504              PB-GA-BILLING-INFO           PIC X(10).
00505          16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  PB-I-LOAN-OFFICER            PIC X(5).
081606         16  PB-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  PB-COMMON-ERRORS.
00514          16  PB-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
00223      EJECT
00224 *                            COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
00108          16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00125          16  CM-AH-POLICY-FEE              PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
072308         16  CM-NH-INTERFACE-SW            PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
072308     12  CM-NH-INT-ON-REFS                 PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
00286 ******************************************************************
00225      EJECT
00226 *                            COPY ERCNOTE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCNOTE                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE AND BILLING NOTES        *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 825    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERNOTE        RKP=2,LEN=33               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  CERT NOTES MOVED TO NEW FILE. THI
091509*                                FILE WILL CONTAIN BILLING NOTES O
00017 ******************************************************************
00018
00019  01  CERTIFICATE-NOTE.
00020      12  CN-RECORD-ID                PIC  XX.
00021          88  VALID-CN-ID                  VALUE 'CN'.
00022
00023      12  CN-CONTROL-PRIMARY.
00024          16  CN-COMPANY-CD           PIC X.
00025          16  CN-CARRIER              PIC X.
00026          16  CN-GROUPING.
00027              20 CN-GROUPING-PREFIX   PIC XXX.
00028              20 CN-GROUPING-PRIME    PIC XXX.
00029          16  CN-STATE                PIC XX.
00030          16  CN-ACCOUNT.
00031              20 CN-ACCOUNT-PREFIX    PIC X(4).
00032              20 CN-ACCOUNT-PRIME     PIC X(6).
00033          16  CN-CERT-EFF-DT          PIC XX.
00034          16  CN-CERT-NO.
00035              20  CN-CERT-PRIME       PIC X(10).
00036              20  CN-CERT-SFX         PIC X.
00037
00038      12  CN-BILLING-START-LINE-NO    PIC 99.
00039      12  CN-BILLING-END-LINE-NO      PIC 99.
00040
00041      12  CN-LINES.
00042          16  CN-LINE OCCURS 10       PIC X(77).
00043
00044      12  CN-LAST-MAINT-DT            PIC XX.
00045      12  CN-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00046      12  CN-LAST-MAINT-USER          PIC X(4).
00047      12  FILLER                      PIC X(6).
00048 ******************************************************************
00227      EJECT
00228 *                            COPY ERCMAIL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCMAIL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
111108* 111108                   PEMA  ADD CRED BENE ADDR2
00017 ******************************************************************
00018
00019  01  MAILING-DATA.
00020      12  MA-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'MA'.
00022
00023      12  MA-CONTROL-PRIMARY.
00024          16  MA-COMPANY-CD                 PIC X.
00025          16  MA-CARRIER                    PIC X.
00026          16  MA-GROUPING.
00027              20  MA-GROUPING-PREFIX        PIC XXX.
00028              20  MA-GROUPING-PRIME         PIC XXX.
00029          16  MA-STATE                      PIC XX.
00030          16  MA-ACCOUNT.
00031              20  MA-ACCOUNT-PREFIX         PIC X(4).
00032              20  MA-ACCOUNT-PRIME          PIC X(6).
00033          16  MA-CERT-EFF-DT                PIC XX.
00034          16  MA-CERT-NO.
00035              20  MA-CERT-PRIME             PIC X(10).
00036              20  MA-CERT-SFX               PIC X.
00037
00038      12  FILLER                            PIC XX.
00039
00040      12  MA-ACCESS-CONTROL.
00041          16  MA-SOURCE-SYSTEM              PIC XX.
00042              88  MA-FROM-CREDIT                VALUE 'CR'.
00043              88  MA-FROM-VSI                   VALUE 'VS'.
00044              88  MA-FROM-WARRANTY              VALUE 'WA'.
00045              88  MA-FROM-OTHER                 VALUE 'OT'.
00046          16  MA-RECORD-ADD-DT              PIC XX.
00047          16  MA-RECORD-ADDED-BY            PIC XXXX.
00048          16  MA-LAST-MAINT-DT              PIC XX.
00049          16  MA-LAST-MAINT-BY              PIC XXXX.
00050          16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00051
00052      12  MA-PROFILE-INFO.
00053          16  MA-QUALIFY-CODE-1             PIC XX.
00054          16  MA-QUALIFY-CODE-2             PIC XX.
00055          16  MA-QUALIFY-CODE-3             PIC XX.
00056          16  MA-QUALIFY-CODE-4             PIC XX.
00057          16  MA-QUALIFY-CODE-5             PIC XX.
00058
00059          16  MA-INSURED-LAST-NAME          PIC X(15).
00060          16  MA-INSURED-FIRST-NAME         PIC X(10).
00061          16  MA-INSURED-MIDDLE-INIT        PIC X.
00062          16  MA-INSURED-ISSUE-AGE          PIC 99.
00063          16  MA-INSURED-BIRTH-DT           PIC XX.
00064          16  MA-INSURED-SEX                PIC X.
00065              88  MA-SEX-MALE                   VALUE 'M'.
00066              88  MA-SEX-FEMALE                 VALUE 'F'.
00067          16  MA-INSURED-SOC-SEC-NO         PIC X(11).
00068
080406         16  MA-ADDRESS-CORRECTED          PIC X.
081108         16  MA-JOINT-BIRTH-DT             PIC XX.
00069 *        16  FILLER                        PIC X(12).
00070
00071          16  MA-ADDRESS-LINE-1             PIC X(30).
00072          16  MA-ADDRESS-LINE-2             PIC X(30).
00073          16  MA-CITY-STATE.
                   20  MA-CITY                   PIC X(28).
                   20  MA-ADDR-STATE             PIC XX.
00074          16  MA-ZIP.
00075              20  MA-ZIP-CODE.
00076                  24  MA-ZIP-CODE-1ST       PIC X(1).
00077                      88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00078                  24  FILLER                PIC X(4).
00079              20  MA-ZIP-PLUS4              PIC X(4).
00080          16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
00081              20  MA-CAN-POSTAL-CODE-1      PIC X(3).
00082              20  MA-CAN-POSTAL-CODE-2      PIC X(3).
00083              20  FILLER                    PIC X(3).
00084
00085          16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
00086
               16  FILLER                        PIC XXX.
00087 *        16  FILLER                        PIC X(10).
00088
           12  MA-CRED-BENE-INFO.
CIDMOD         16  MA-CRED-BENE-NAME                 PIC X(25).
CIDMOD         16  MA-CRED-BENE-ADDR                 PIC X(30).
               16  MA-CRED-BENE-ADDR2                PIC X(30).
CIDMOD         16  MA-CRED-BENE-CTYST.
                   20  MA-CRED-BENE-CITY             PIC X(28).
                   20  MA-CRED-BENE-STATE            PIC XX.
CIDMOD         16  MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-ZIP-CODE.
CIDMOD                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
CIDMOD                     88  MA-CB-CANADIAN-POST-CODE
                                                 VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                    PIC X(4).
CIDMOD             20  MA-CB-ZIP-PLUS4               PIC X(4).
CIDMOD         16  MA-CB-CANADIAN-POSTAL-CODE
                                  REDEFINES MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
CIDMOD             20  FILLER                        PIC X(3).
080406     12  MA-POST-CARD-MAIL-DATA.
080406         16  MA-MAIL-DATA OCCURS 7.
080406             20  MA-MAIL-TYPE              PIC X.
080406                 88  MA-12MO-MAILING           VALUE '1'.
080406                 88  MA-EXP-MAILING            VALUE '2'.
080406             20  MA-MAIL-STATUS            PIC X.
080406                 88  MA-MAIL-ST-MAILED         VALUE '1'.
080406                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  MA-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC XX.
080406*    12  FILLER                            PIC X(30).
00090 ******************************************************************
00229      EJECT
00230 *                            COPY ERCPNDM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING MAILING DATA                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERPNDM                 RKP=2,LEN=11      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
071108* 071108  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
00017 ******************************************************************
00018
00019  01  PENDING-MAILING-DATA.
00020      12  PM-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'PM'.
00022
00023      12  PM-CONTROL-PRIMARY.
00024          16  PM-COMPANY-CD                 PIC X.
00025          16  PM-ENTRY-BATCH                PIC X(6).
00026          16  PM-BATCH-SEQ-NO               PIC S9(4)     COMP.
00027          16  PM-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.
00028
00029      12  FILLER                            PIC X(14).
00030
00031      12  PM-ACCESS-CONTROL.
00032          16  PM-SOURCE-SYSTEM              PIC XX.
00033              88  PM-FROM-CREDIT                VALUE 'CR'.
00034              88  PM-FROM-VSI                   VALUE 'VS'.
00035              88  PM-FROM-WARRANTY              VALUE 'WA'.
00036              88  PM-FROM-OTHER                 VALUE 'OT'.
00037          16  PM-RECORD-ADD-DT              PIC XX.
00038          16  PM-RECORD-ADDED-BY            PIC XXXX.
00039          16  PM-LAST-MAINT-DT              PIC XX.
00040          16  PM-LAST-MAINT-BY              PIC XXXX.
00041          16  PM-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00042
00043      12  PM-PROFILE-INFO.
00044          16  PM-QUALIFY-CODE-1             PIC XX.
00045          16  PM-QUALIFY-CODE-2             PIC XX.
00046          16  PM-QUALIFY-CODE-3             PIC XX.
00047          16  PM-QUALIFY-CODE-4             PIC XX.
00048          16  PM-QUALIFY-CODE-5             PIC XX.
00049
00050          16  PM-INSURED-LAST-NAME          PIC X(15).
00051          16  PM-INSURED-FIRST-NAME         PIC X(10).
00052          16  PM-INSURED-MIDDLE-INIT        PIC X.
00053          16  PM-INSURED-ISSUE-AGE          PIC 99.
00054          16  PM-INSURED-BIRTH-DT           PIC XX.
00055          16  PM-INSURED-SEX                PIC X.
00056              88  PM-SEX-MALE                   VALUE 'M'.
00057              88  PM-SEX-FEMALE                 VALUE 'F'.
00058          16  PM-INSURED-SOC-SEC-NO         PIC X(11).
00059
080406         16  PM-ADDRESS-CORRECTED          PIC X.
081108         16  PM-JOINT-BIRTH-DT             PIC XX.
00060 *        16  FILLER                        PIC X(12).
00061
00062          16  PM-ADDRESS-LINE-1             PIC X(30).
00063          16  PM-ADDRESS-LINE-2             PIC X(30).
00064          16  PM-CITY-STATE.
                   20  PM-CITY                   PIC X(28).
                   20  PM-STATE                  PIC XX.
00065          16  PM-ZIP.
00066              20  PM-ZIP-CODE.
00067                  24  PM-ZIP-1              PIC X.
00068                      88  PM-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00069                  24  FILLER                PIC X(4).
00070              20  PM-ZIP-PLUS4              PIC X(4).
00071          16  PM-CANADIAN-ZIP  REDEFINES  PM-ZIP.
00072              20  PM-CAN-POST1              PIC XXX.
00073              20  PM-CAN-POST2              PIC XXX.
00074              20  FILLER                    PIC XXX.
00075
00076          16  PM-PHONE-NO                   PIC 9(11)       COMP-3.
00077
00078          16  FILLER                        PIC X(03).
00079
           12  PM-CRED-BENE-INFO.
CIDMOD         16  PM-CRED-BENE-NAME             PIC X(25).
CIDMOD         16  PM-CRED-BENE-ADDR             PIC X(30).
071108         16  PM-CRED-BENE-ADDR2            PIC X(30).
CIDMOD         16  PM-CRED-BENE-CTYST.
                   20  PM-CRED-BENE-CITY         PIC X(28).
                   20  PM-CRED-BENE-STATE        PIC XX.
CIDMOD         16  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-ZIP-CODE.
CIDMOD                 24  PM-CB-ZIP-1           PIC X.
CIDMOD                     88  PM-CB-CANADIAN-POST-CODE
                                        VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                PIC X(4).
CIDMOD             20  PM-CB-ZIP-PLUS4           PIC X(4).
CIDMOD         16  PM-CB-CANADIAN-ZIP  REDEFINES  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-CAN-POST1           PIC XXX.
CIDMOD             20  PM-CB-CAN-POST2           PIC XXX.
CIDMOD             20  FILLER                    PIC XXX.
080406     12  PM-POST-CARD-MAIL-DATA.
080406         16  PM-MAIL-DATA OCCURS 7.
080406             20  PM-MAIL-TYPE              PIC X.
080406                 88  PM-12MO-MAILING           VALUE '1'.
080406                 88  PM-EXP-MAILING            VALUE '2'.
080406             20  PM-MAIL-STATUS            PIC X.
080406                 88  PM-MAIL-ST-MAILED         VALUE '1'.
080406                 88  PM-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  PM-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  PM-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC X(12).
080406*    12  FILLER                            PIC X(30).
00075
00081 ******************************************************************
00231      EJECT
00232
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PARMLIST
                                CONTROL-FILE PENDING-BUSINESS
                                CERTIFICATE-MASTER CERTIFICATE-NOTE
                                MAILING-DATA PENDING-MAILING-DATA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL930' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00234      SERVICE RELOAD PARMLIST.
00235      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00236      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00237      IF EIBCALEN = 0
00238          GO TO 8800-UNAUTHORIZED-ACCESS.
00239      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00240      MOVE '5'                    TO DC-OPTION-CODE.
00241      MOVE LINK-CLDATCV           TO PGM-NAME.
00242
00243      
      * EXEC CICS LINK
00244 *        PROGRAM(PGM-NAME)
00245 *        COMMAREA(DATE-CONVERSION-DATA)
00246 *        LENGTH(DC-COMM-LENGTH)
00247 *        END-EXEC.
      *    MOVE '."C                   ''   #00004217' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00248      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
00249      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
00250
00251      MOVE PI-CALLING-PROGRAM TO PI-SAVE-CALLING-PGM.
00252      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00253          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00254              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00255              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00256              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00257              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00258              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00259              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00260              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00261              MOVE THIS-PGM TO PI-CALLING-PROGRAM
00262          ELSE
00263              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00264              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00265              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00266              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00267              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00268              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00269              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00270              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00271
00272      MOVE LOW-VALUES             TO EL930AI.
00273
00274      IF PI-SAVE-CALLING-PGM = XCTL-9302
00275          GO TO 4000-SHOW-TOTALS.
00276      IF PI-SAVE-CALLING-PGM = XCTL-9301
00277          MOVE 'N'                TO PI-EL930-FIRST-TIME-SW
00278          IF PI-ISS-CNT-ENTERED = ZEROS
00279            AND PI-CAN-CNT-ENTERED = ZEROS
00280              MOVE 'Y'            TO PI-VERIFY-DELETE-SW
00281              GO TO 3000-DELETE-ENTERED-BATCH
00282          ELSE
00283              GO TO 4000-SHOW-TOTALS.
00284      IF EIBTRNID NOT = TRANS-ID
00285          MOVE ZEROS              TO PI-LAST-SEQ-NO-ADDED
00286          MOVE QUESTION-MARKS     TO BATCHI
00287          MOVE AL-UANON           TO BATCHA
00288          GO TO 8100-SEND-INITIAL-MAP.
00289
00290      
      * EXEC CICS HANDLE CONDITION
00291 *        PGMIDERR  (9600-PGMID-ERROR)
00292 *        ERROR     (9990-ABEND)
00293 *        END-EXEC.
      *    MOVE '"$L.                  ! " #00004264' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00294
00295      IF EIBAID = DFHCLEAR
00296          GO TO 9400-CLEAR.
00297
00298      IF PI-PROCESSOR-ID = 'LGXX'
00299          GO TO 0200-RECEIVE.
00300
00301      
      * EXEC CICS READQ TS
00302 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00303 *        INTO   (SECURITY-CONTROL)
00304 *        LENGTH (SC-COMM-LENGTH)
00305 *        ITEM   (SC-ITEM)
00306 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00004275' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00307
00308      MOVE SC-CREDIT-DISPLAY (11)  TO PI-DISPLAY-CAP.
00309      MOVE SC-CREDIT-UPDATE  (11)  TO PI-MODIFY-CAP.
00310
00311      IF NOT DISPLAY-CAP
00312          MOVE 'READ'          TO SM-READ
00313          PERFORM 9995-SECURITY-VIOLATION
00314          MOVE ER-0070         TO  EMI-ERROR
00315          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00316          GO TO 8100-SEND-INITIAL-MAP.
00317
00318      EJECT
00319  0200-RECEIVE.
00320      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00321          MOVE ER-0008            TO EMI-ERROR
00322          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00323          MOVE -1                 TO MAINTL
00324          GO TO 8200-SEND-DATAONLY.
00325
00326      
      * EXEC CICS RECEIVE
00327 *        MAP      (MAP-NAME)
00328 *        MAPSET   (MAPSET-NAME)
00329 *        INTO     (EL930AI)
00330 *        END-EXEC.
           MOVE LENGTH OF
            EL930AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004300' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL930AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00331
00332      IF PFENTERL = 0
00333          GO TO 0300-CHECK-PFKEYS.
00334      IF (PFENTERI NUMERIC)
00335        AND (PFENTERI GREATER THAN 0 AND LESS THAN 25)
00336          MOVE PF-VALUES (PFENTERI) TO EIBAID
00337      ELSE
00338          MOVE ER-0029            TO EMI-ERROR
00339          GO TO 0320-INPUT-ERROR.
00340
00341  0300-CHECK-PFKEYS.
00342      IF EIBAID = DFHPF23
00343          GO TO 8810-PF23.
00344      IF EIBAID = DFHPF24
00345          GO TO 9200-RETURN-MAIN-MENU.
00346      IF EIBAID = DFHPF12
00347          GO TO 9500-PF12.
00348
00349      IF EIBAID = DFHPF1
00350         IF PI-DATA-UPDATED
00351              MOVE 'Y'            TO WS-PF1-SW
00352              MOVE SPACE          TO PI-NB-MONTH-END-DT
00353              PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT
00354              MOVE SPACES TO BATCH-TO-PROCESS
00355              MOVE PI-COMPANY-ID  TO EDIT-COMPANY-ID
00356              MOVE PI-COMPANY-CD  TO EDIT-COMPANY-CD
00357              MOVE PI-SAV-ENTRY-BATCH TO EDIT-BATCH
00358              
      * EXEC CICS START
00359 *               TRANSID       (EDIT-TRANS)
00360 *               FROM          (BATCH-TO-PROCESS)
00361 *               LENGTH        (PASS-AREA-LEN)
00362 *               END-EXEC
      *    MOVE '0( LF                 0   #00004332' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 EDIT-TRANS, 
                 DFHEIV99, 
                 BATCH-TO-PROCESS, 
                 PASS-AREA-LEN, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00363              MOVE SPACES         TO PI-UPDATE-SW
00364                                     PI-KEYED-SWITCHES
00365              MOVE ZEROS          TO PI-ISS-CNT-ENTERED
00366              MOVE ZEROS          TO PI-CAN-CNT-ENTERED
00367              MOVE ZEROS          TO EMI-ERROR
00368              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00369              MOVE LOW-VALUES     TO EL930AO
00370              MOVE QUESTION-MARKS TO BATCHI
00371              MOVE AL-UANON       TO BATCHA
00372              MOVE -1             TO MAINTL
00373              GO TO 8100-SEND-INITIAL-MAP
00374          ELSE
00375              IF  MAINTI = 'N'
00376                  MOVE -1             TO MAINTL
00377                  MOVE ER-2211        TO EMI-ERROR
00378                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00379                  GO TO 8200-SEND-DATAONLY.
00380
00381
00382      IF EIBAID = DFHPF1
00383              MOVE SPACE          TO PI-NB-MONTH-END-DT
00384              MOVE SPACES TO BATCH-TO-PROCESS
00385              MOVE PI-COMPANY-ID  TO EDIT-COMPANY-ID
00386              MOVE PI-COMPANY-CD  TO EDIT-COMPANY-CD
00387              MOVE PI-SAV-ENTRY-BATCH TO EDIT-BATCH
00388              
      * EXEC CICS START
00389 *               TRANSID       (EDIT-TRANS)
00390 *               FROM          (BATCH-TO-PROCESS)
00391 *               LENGTH        (PASS-AREA-LEN)
00392 *               END-EXEC
      *    MOVE '0( LF                 0   #00004362' TO DFHEIV0
           MOVE X'3028204C4620202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203020' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 EDIT-TRANS, 
                 DFHEIV99, 
                 BATCH-TO-PROCESS, 
                 PASS-AREA-LEN, 
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
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00393              MOVE SPACE          TO PI-UPDATE-SW
00394                                     PI-KEYED-SWITCHES
00395              MOVE ZEROS          TO PI-ISS-CNT-ENTERED
00396              MOVE ZEROS          TO PI-CAN-CNT-ENTERED
00397              MOVE ZEROS          TO EMI-ERROR
00398              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00399              MOVE LOW-VALUES     TO EL930AO
00400              MOVE QUESTION-MARKS TO BATCHI
00401              MOVE AL-UANON       TO BATCHA
00402              MOVE -1             TO MAINTL
00403              GO TO 8100-SEND-INITIAL-MAP.
00404
00405
00406      IF EIBAID = DFHPF3
00407         IF BATCHL GREATER THAN ZEROS
00408            MOVE AL-UNNON         TO BATCHA
00409            GO TO 6000-BROWSE-BATCH-HEADERS
00410         ELSE
00411            GO TO 6000-BROWSE-BATCH-HEADERS.
00412
00413      IF EIBAID = DFHPF4
00414          IF NOT PI-EL930-FIRST-TIME
00415              MOVE XCTL-9302      TO PGM-NAME
00416              GO TO 9300-XCTL
00417          ELSE
00418              PERFORM 0400-PRIME-BATCH-TOTALS THRU 0490-EXIT
00419              IF EMI-ERROR = ZEROS
00420                  MOVE 'N'        TO PI-EL930-FIRST-TIME-SW
00421                  MOVE XCTL-9302  TO PGM-NAME
00422                  GO TO 9300-XCTL
00423              ELSE
00424                  GO TO 8200-SEND-DATAONLY.
00425
00426      IF EIBAID = DFHPF2
00427          MOVE  AL-UANON          TO  BATCHA
00428          MOVE  BATCHI TO PI-SAV-ENTRY-BATCH
00429          GO TO 3000-DELETE-ENTERED-BATCH.
00430
00431      IF  MAINTI = 'S'
00432          MOVE  MAINTI            TO  PI-MAINT-FUNC
00433          PERFORM 0400-PRIME-BATCH-TOTALS  THRU  0490-EXIT
00434              IF EMI-ERROR = ZEROS
00435                  MOVE 'N'        TO PI-EL930-FIRST-TIME-SW
00436                  GO TO 4000-SHOW-TOTALS
00437              ELSE
00438                  GO TO 8200-SEND-DATAONLY.
00439
00440      IF EIBAID = DFHENTER
00441          GO TO 0330-EDIT-DATA.
00442  0320-INPUT-ERROR.
00443      MOVE ER-0029                TO EMI-ERROR.
00444      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00445      MOVE AL-UNBON               TO PFENTERA.
00446      IF PFENTERL = 0
00447          MOVE -1                 TO MAINTL
00448      ELSE
00449          MOVE -1                 TO PFENTERL.
00450      GO TO 8200-SEND-DATAONLY.
00451
00452      EJECT
00453  0330-EDIT-DATA.
00454
00455      IF MODIFY-CAP
00456          NEXT SENTENCE
00457        ELSE
00458          IF MAINTI NOT = 'S'
00459          MOVE 'UPDATE'       TO SM-READ
00460          PERFORM 9995-SECURITY-VIOLATION
00461          MOVE ER-0070        TO EMI-ERROR
00462          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00463          GO TO 8100-SEND-INITIAL-MAP.
00464
00465      MOVE SPACE                  TO  PI-NB-MONTH-END-DT.
00466
00467      PERFORM 1250-READ-COMPANY-REC THRU 1250-EXIT.
00468      IF MAINTI = 'N' OR 'C' OR 'B'
00469          NEXT SENTENCE
00470      ELSE
00471          MOVE ER-0023            TO EMI-ERROR
00472          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00473          MOVE -1                 TO MAINTL
00474          MOVE AL-UABON           TO MAINTA
00475          GO TO 8200-SEND-DATAONLY.
00476
00477      IF  NOT EMI-NO-ERRORS
00478          GO TO 8200-SEND-DATAONLY.
00479
00480      MOVE MAINTI                 TO PI-MAINT-FUNC.
00481      MOVE AL-UANON               TO MAINTA.
00482
00483      MOVE PI-COMPANY-CD          TO PI-SAV-COMP-CD.
00484      MOVE ZEROS                  TO PI-SAV-BATCH-SEQ
00485                                     PI-SAV-BATCH-CHG-SEQ.
00486      MOVE SPACES                 TO PI-SAV-CERT-EFF-DT
00487                                     PI-SAV-CERT-NO.
00488
00489
00490      IF BATCHL = ZEROS
00491          MOVE -1                 TO BATCHL
00492          MOVE AL-UNBON           TO BATCHA
00493          MOVE ER-2201            TO EMI-ERROR
00494          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00495      ELSE
00496          IF BATCHI = QUESTION-MARKS OR SPACES
00497              IF MAINTI = 'N'
00498                  PERFORM 1300-GET-BATCH-NO THRU 1300-EXIT
00499              ELSE
00500                  MOVE -1         TO BATCHL
00501                  MOVE AL-UNBON   TO BATCHA
00502                  MOVE ER-2201    TO EMI-ERROR
00503                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00504          ELSE
00505              IF BATCHI EQUAL PI-SAV-ENTRY-BATCH
00506                  MOVE AL-UANON   TO BATCHA
00507              ELSE
00508                  MOVE BATCHI TO PI-SAV-ENTRY-BATCH
00509                  IF MAINTI = 'C' OR 'B'
00510                      MOVE AL-UANON TO BATCHA
00511                      MOVE ZEROS  TO ELFISSL  ELFCANL
00512                                     EAHISSL  EAHCANL
00513                                     EISSCNTL ECANCNTL
00514                                     PI-LF-ISS-REMITTED
00515                                     PI-LF-CAN-REMITTED
00516                                     PI-AH-ISS-REMITTED
00517                                     PI-AH-CAN-REMITTED
00518                                     PI-ISS-CNT-REMITTED
00519                                     PI-CAN-CNT-REMITTED.
00520
00521  0340-CHECK-TOTALS.
00522
00523 *    ********************************************************
00524 *    *         THE FOLLOWING FIELDS ARE NOT REQUIRED        *
00525 *    *         IF ENTERED THE ONLY REQUIREMENT IS NUMERIC   *
00526 *    ********************************************************
00527
00528      IF ELFISSL GREATER THAN ZEROS
00529          MOVE ELFISSI            TO WS-DEEDIT-FIELD
00530          PERFORM 8600-DEEDIT THRU 8600-EXIT
00531          MOVE WS-DEEDIT-FIELD    TO PI-LF-ISS-REMITTED
00532          MOVE AL-UNNON           TO ELFISSA
00533      ELSE
00534          MOVE ZEROS              TO PI-LF-ISS-REMITTED.
00535
00536      IF ELFCANL GREATER THAN ZEROS
00537          MOVE ELFCANI            TO WS-DEEDIT-FIELD
00538          PERFORM 8600-DEEDIT THRU 8600-EXIT
00539          MOVE WS-DEEDIT-FIELD    TO PI-LF-CAN-REMITTED
00540          MOVE AL-UNNON           TO ELFCANA
00541      ELSE
00542          MOVE ZEROS              TO PI-LF-CAN-REMITTED.
00543
00544      IF EAHISSL GREATER THAN ZEROS
00545          MOVE EAHISSI            TO WS-DEEDIT-FIELD
00546          PERFORM 8600-DEEDIT THRU 8600-EXIT
00547          MOVE WS-DEEDIT-FIELD    TO PI-AH-ISS-REMITTED
00548          MOVE AL-UNNON           TO EAHISSA
00549      ELSE
00550          MOVE ZEROS              TO PI-AH-ISS-REMITTED.
00551
00552      IF EAHCANL GREATER THAN ZEROS
00553          MOVE EAHCANI            TO WS-DEEDIT-FIELD
00554          PERFORM 8600-DEEDIT THRU 8600-EXIT
00555          MOVE WS-DEEDIT-FIELD    TO PI-AH-CAN-REMITTED
00556          MOVE AL-UNNON           TO EAHCANA
00557      ELSE
00558          MOVE ZEROS              TO PI-AH-CAN-REMITTED.
00559
00560      IF EISSCNTL GREATER THAN ZEROS
00561          MOVE AL-UNNON           TO EISSCNTA
00562          
      * EXEC CICS BIF DEEDIT
00563 *            FIELD(EISSCNTI)
00564 *            LENGTH(06)
00565 *            END-EXEC
           MOVE 06
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004536' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EISSCNTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00566          MOVE EISSCNTI           TO PI-ISS-CNT-REMITTED
00567      ELSE
00568          MOVE ZEROS              TO PI-ISS-CNT-REMITTED.
00569
00570      IF ECANCNTL GREATER THAN ZEROS
00571          MOVE AL-UNNON           TO ECANCNTA
00572          
      * EXEC CICS BIF DEEDIT
00573 *            FIELD(ECANCNTI)
00574 *            LENGTH(06)
00575 *            END-EXEC
           MOVE 06
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004546' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ECANCNTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00576          MOVE ECANCNTI           TO PI-CAN-CNT-REMITTED
00577      ELSE
00578          MOVE ZEROS              TO PI-CAN-CNT-REMITTED.
00579
00580 ******************************************************************
00581 *                                                                *
00582 *               EDIT FOR NEW MONTH-END-DATE                      *
00583 *                        04/13/84                                *
00584 *                                                                *
00585 ******************************************************************
00586
00587      IF MNTHNDTL GREATER THAN ZEROS NEXT SENTENCE
00588        ELSE
00589         GO TO 0340-ERROR-CHECK.
00590
00591      MOVE MNTHNDTI               TO WS-DT-DEEDIT-FIELD.
00592      PERFORM 8600-DEEDIT THRU 8600-EXIT.
00593      MOVE WS-DT-DEEDIT-FIELD     TO DC-GREG-DATE-1-MDY.
00594      MOVE '4' TO DC-OPTION-CODE.
00595      MOVE LINK-CLDATCV TO PGM-NAME.
00596      
      * EXEC CICS LINK
00597 *         PROGRAM(PGM-NAME)
00598 *         COMMAREA(DATE-CONVERSION-DATA)
00599 *         LENGTH(DC-COMM-LENGTH)
00600 *         END-EXEC.
      *    MOVE '."C                   ''   #00004570' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00601      IF DATE-CONVERSION-ERROR
00602         GO TO 0340-DAY-ERROR.
00603      MOVE DC-GREG-DATE-1-EDIT    TO MNTHNDTO.
00604      MOVE DC-BIN-DATE-1          TO PI-NB-MONTH-END-DT.
00605      MOVE DC-GREG-DATE-1-MDY     TO DATE-TEST-AREA.
00606      IF DATE-TEST-MM = 4 OR 6 OR 9 OR 11
00607         IF DATE-TEST-DD  NOT = 30
00608            GO TO 0340-DAY-ERROR
00609         ELSE
00610            GO TO 0340-ERROR-CHECK.
00611
00612      IF DATE-TEST-MM = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
00613         IF DATE-TEST-DD  NOT = 31
00614            GO TO 0340-DAY-ERROR
00615         ELSE
00616            GO TO 0340-ERROR-CHECK.
00617      DIVIDE DATE-TEST-YY  BY 4 GIVING DIVIDE-RESULT
00618                                REMAINDER DIVIDE-REMAINDER
00619      IF (DATE-TEST-YY = 0) OR (DIVIDE-REMAINDER NOT = 0)
00620         IF DATE-TEST-DD  NOT = 28
00621            GO TO 0340-DAY-ERROR
00622
00623         ELSE
00624            GO TO 0340-ERROR-CHECK
00625      ELSE
00626         IF DATE-TEST-DD = 29
00627            GO TO 0340-ERROR-CHECK.
00628
00629  0340-DAY-ERROR.
00630      MOVE -1 TO MNTHNDTL.
00631      MOVE AL-UABON TO MNTHNDTA.
00632      MOVE ER-0340 TO EMI-ERROR.
00633      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00634
00635
00636  0340-ERROR-CHECK.
00637      IF NOT EMI-NO-ERRORS
00638          GO TO 8200-SEND-DATAONLY.
00639      IF PI-DELETE-IS-OK
00640          MOVE SPACE              TO PI-VERIFY-DELETE-SW.
00641      IF MAINTI = 'N'
00642        AND PI-DATA-UPDATED
00643          MOVE 'C'                TO PI-MAINT-FUNC
00644          GO TO 0360-XCTL-EL9301.
00645
00646      
      * EXEC CICS HANDLE CONDITION
00647 *              NOTFND  (0350-NO-RECORDS)
00648 *              ENDFILE (0350-NO-RECORDS)
00649 *              NOTOPEN (7000-PNDT-FILE-NOTOPEN)
00650 *              END-EXEC.
      *    MOVE '"$I''J                 ! # #00004620' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034363230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00651      
      * EXEC CICS READ
00652 *        SET (ERPNDT-POINTER)
00653 *        DATASET (ERPNDT-FILE-ID)
00654 *        RIDFLD (PI-SAV-ENDING-ERPNDT-KEY)
00655 *        GTEQ
00656 *        END-EXEC.
      *    MOVE '&"S        G          (   #00004625' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00657      SERVICE RELOAD PENDING-BUSINESS.
00658      IF PI-SAV-COMP-CD = PB-COMPANY-CD
00659        AND PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH
00660          IF MAINTI = 'N'
00661              MOVE ER-2229        TO EMI-ERROR
00662              MOVE -1             TO MAINTL
00663              MOVE AL-UABON       TO MAINTA
00664              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00665              GO TO 8200-SEND-DATAONLY
00666          ELSE
00667              NEXT SENTENCE
00668      ELSE
00669          GO TO 0350-NO-RECORDS.
00670
00671
00672      IF PB-BILLED-DT NOT = LOW-VALUES
00673          MOVE ER-2402            TO EMI-ERROR
00674          MOVE -1                 TO BATCHL
00675          MOVE AL-UABON           TO BATCHA
00676          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00677          GO TO 8200-SEND-DATAONLY.
00678
00679      IF PI-SAV-COMP-CD = PB-COMPANY-CD
00680        AND PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH
00681        AND MAINTI NOT = 'N'
00682          MOVE PB-CARRIER         TO PI-SAV-CARRIER
00683          MOVE PB-GROUPING        TO PI-SAV-GROUPING
00684          MOVE PB-STATE           TO PI-SAV-STATE
00685          MOVE PB-ACCOUNT         TO PI-SAV-ACCOUNT
00686          PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT
00687          MOVE 'EL930B  '         TO PI-MAP-NAME.
00688          GO TO 0360-XCTL-EL9301.
00689
00690  0350-NO-RECORDS.
00691      IF MAINTI = 'N'
00692          MOVE SPACE              TO PI-ISSUE-ADDED-SW
00693          MOVE ZEROS              TO PI-LF-ISS-ENTERED
00694                                     PI-LF-CAN-ENTERED
00695                                     PI-AH-ISS-ENTERED
00696                                     PI-AH-CAN-ENTERED
00697                                     PI-ISS-CNT-ENTERED
00698                                     PI-CAN-CNT-ENTERED
00699          PERFORM 2000-WRITE-BATCH-TOTAL-REC THRU 2990-EXIT
00700          IF PI-LF-ISS-REMITTED = ZEROS
00701            AND PI-AH-ISS-REMITTED = ZEROS
00702            AND PI-LF-CAN-REMITTED = ZEROS
00703            AND PI-AH-CAN-REMITTED = ZEROS
00704            AND PI-ISS-CNT-REMITTED = ZEROS
00705            AND PI-CAN-CNT-REMITTED = ZEROS
00706              MOVE 'EL930B  '     TO PI-MAP-NAME
00707              GO TO 0360-XCTL-EL9301
00708          ELSE
00709              IF PI-LF-ISS-REMITTED = ZEROS
00710                AND PI-AH-ISS-REMITTED = ZEROS
00711                AND PI-ISS-CNT-REMITTED = ZEROS
00712                  MOVE 'EL930C  '     TO PI-MAP-NAME
00713                  GO TO 0360-XCTL-EL9301
00714              ELSE
00715                  MOVE 'EL930B  '     TO PI-MAP-NAME
00716                  GO TO 0360-XCTL-EL9301
00717      ELSE
00718          MOVE ER-2212            TO EMI-ERROR
00719          MOVE -1                 TO BATCHL
00720          MOVE AL-UABON           TO BATCHA
00721          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00722          GO TO 8200-SEND-DATAONLY.
00723
00724  0360-XCTL-EL9301.
00725      IF PI-MAINT-FUNC = 'N'
00726          MOVE SPACES             TO PI-KEYED-SWITCHES.
00727
00728      MOVE XCTL-9301           TO PGM-NAME.
00729
00730      GO TO 9300-XCTL.
00731      EJECT
00732  0400-PRIME-BATCH-TOTALS.
00733      
      * EXEC CICS HANDLE CONDITION
00734 *        NOTFND (0480-NO-BATCH-TRAILER)
00735 *        END-EXEC.
      *    MOVE '"$I                   ! $ #00004707' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034373037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00736      MOVE PI-COMPANY-CD          TO PNDT-COMP-CD.
00737      MOVE 9999                   TO PNDT-BATCH-SEQ.
00738      MOVE BATCHI                 TO PNDT-ENTRY-BATCH.
00739
00740      
      * EXEC CICS READ
00741 *        DATASET (ERPNDT-FILE-ID)
00742 *        SET (ERPNDT-POINTER)
00743 *        RIDFLD (ERPNDT-KEY)
00744 *        END-EXEC.
      *    MOVE '&"S        E          (   #00004714' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00745      SERVICE RELOAD PENDING-BUSINESS.
00746
00747      IF PB-BILLED-DT NOT = LOW-VALUES
00748          MOVE ER-2402            TO EMI-ERROR
00749          MOVE -1                 TO BATCHL
00750          MOVE AL-UABON           TO BATCHA
00751          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00752          GO TO 0490-EXIT.
00753
00754      MOVE PI-COMPANY-CD          TO PI-SAV-COMP-CD.
00755      MOVE PB-ENTRY-BATCH           TO PI-SAV-ENTRY-BATCH.
00756      MOVE PB-CARRIER               TO PI-SAV-CARRIER.
00757      MOVE PB-GROUPING              TO PI-SAV-GROUPING.
00758      MOVE PB-STATE                 TO PI-SAV-STATE.
00759      MOVE PB-ACCOUNT               TO PI-SAV-ACCOUNT.
00760      MOVE PB-B-LF-ISS-PRM-REMITTED TO PI-LF-ISS-REMITTED.
00761      MOVE PB-B-LF-ISS-PRM-ENTERED  TO PI-LF-ISS-ENTERED.
00762      MOVE PB-B-AH-ISS-PRM-REMITTED TO PI-AH-ISS-REMITTED.
00763      MOVE PB-B-AH-ISS-PRM-ENTERED  TO PI-AH-ISS-ENTERED.
00764      MOVE PB-B-LF-CAN-PRM-REMITTED TO PI-LF-CAN-REMITTED.
00765      MOVE PB-B-LF-CAN-PRM-ENTERED  TO PI-LF-CAN-ENTERED.
00766      MOVE PB-B-AH-CAN-PRM-REMITTED TO PI-AH-CAN-REMITTED.
00767      MOVE PB-B-AH-CAN-PRM-ENTERED  TO PI-AH-CAN-ENTERED.
00768      MOVE PB-B-ISSUE-CNT-REMITTED  TO PI-ISS-CNT-REMITTED.
00769      MOVE PB-B-ISSUE-CNT-ENTERED   TO PI-ISS-CNT-ENTERED.
00770      MOVE PB-B-CANCEL-CNT-REMITTED TO PI-CAN-CNT-REMITTED.
00771      MOVE PB-B-CANCEL-CNT-ENTERED  TO PI-CAN-CNT-ENTERED.
00772
00773 ******************************************************************
00774 *                                                                *
00775 *    IF THE MONTH-END-DATE IN THE TRAILER RECORD (BATCH HEADER)  *
00776 *       IS NOT EQUAL TO THE ACCOUNTS MONTH-END-DATE PRIME THE    *
00777 *       TRAILER'S MONTH-END-DATE.                                *
00778 *                                                                *
00779 ******************************************************************
00780
00781      MOVE SPACE                    TO PI-NB-MONTH-END-DT.
00782      IF  PB-CREDIT-SELECT-DT = PI-CR-MONTH-END-DT
00783          NEXT SENTENCE
00784        ELSE
00785          MOVE PB-CREDIT-SELECT-DT  TO  PI-NB-MONTH-END-DT.
00786
00787      GO TO 0490-EXIT.
00788
00789  0480-NO-BATCH-TRAILER.
00790      MOVE ER-2212                TO EMI-ERROR.
00791      MOVE -1                     TO BATCHL.
00792      MOVE AL-UABON               TO BATCHA.
00793      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00794
00795  0490-EXIT.
00796      EXIT.
00797      EJECT
00798      EJECT
00799  1250-READ-COMPANY-REC.
00800      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
00801      MOVE '1'                    TO CNTL-REC-TYPE.
00802      MOVE SPACES                 TO CNTL-ACCESS.
00803      MOVE +0                     TO CNTL-SEQ.
00804
00805      
      * EXEC CICS HANDLE CONDITION
00806 *        NOTFND   (1250-NO-COMPANY-REC)
00807 *        NOTOPEN  (7100-CNTL-FILE-NOTOPEN)
00808 *        END-EXEC.
      *    MOVE '"$IJ                  ! % #00004779' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034373739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00809
00810      
      * EXEC CICS READ
00811 *        DATASET   (ELCNTL-FILE-ID)
00812 *        SET       (ELCNTL-POINTER)
00813 *        RIDFLD    (ELCNTL-KEY)
00814 *        END-EXEC.
      *    MOVE '&"S        E          (   #00004784' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-POINTER, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00815      SERVICE RELOAD CONTROL-FILE.
00816      MOVE CF-CREDIT-EDIT-CONTROLS TO PI-CREDIT-EDIT-CONTROLS.
00817      GO TO 1250-EXIT.
00818  1250-NO-COMPANY-REC.
00819      MOVE ER-2248                TO EMI-ERROR.
00820      MOVE -1                     TO BATCHL.
00821      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00822      GO TO 8200-SEND-DATAONLY.
00823  1250-EXIT.
00824      EXIT.
00825      EJECT
00826  1300-GET-BATCH-NO.
00827      
      * EXEC CICS HANDLE CONDITION
00828 *        NOTFND   (1300-NO-COMPANY-REC)
00829 *        NOTOPEN  (7100-CNTL-FILE-NOTOPEN)
00830 *        END-EXEC.
      *    MOVE '"$IJ                  ! & #00004801' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00831
00832      
      * EXEC CICS READ
00833 *        DATASET   (ELCNTL-FILE-ID)
00834 *        SET       (ELCNTL-POINTER)
00835 *        RIDFLD    (ELCNTL-KEY)
00836 *        UPDATE
00837 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00004806' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-POINTER, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00838      SERVICE RELOAD CONTROL-FILE.
00839
00840      MOVE 'B'                    TO  JP-RECORD-TYPE.
00841      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
00842
00843      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00844              WS-ELCNTL-RECORD-LENGTH  +  23.
00845
00846      PERFORM 8400-LOG-JOURNAL-RECORD.
00847
00848  1300-ASSIGN-NUMBER-LOOP.
00849      IF CF-LAST-BATCH-RESET
00850          MOVE ZEROS              TO CF-LAST-BATCH-NO
00851          ADD +1                  TO CF-LAST-BATCH-NO
00852      ELSE
00853          ADD +1                  TO CF-LAST-BATCH-NO.
00854      MOVE CF-LAST-BATCH-NO       TO WS-BATCH-NO.
00855      MOVE WS-BATCH-NO            TO PI-SAV-ENTRY-BATCH.
00856      
      * EXEC CICS HANDLE CONDITION
00857 *        NOTFND  (1300-REWRITE-COMPANY-REC)
00858 *        NOTOPEN (7000-PNDT-FILE-NOTOPEN)
00859 *        END-EXEC.
      *    MOVE '"$IJ                  ! '' #00004830' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034383330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00860      
      * EXEC CICS READ
00861 *        SET (ERPNDT-POINTER)
00862 *        DATASET (ERPNDT-FILE-ID)
00863 *        RIDFLD (PI-SAV-ENDING-ERPNDT-KEY)
00864 *        GTEQ
00865 *        END-EXEC.
      *    MOVE '&"S        G          (   #00004834' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00866      SERVICE RELOAD PENDING-BUSINESS.
00867      IF PI-SAV-COMP-CD NOT = PB-COMPANY-CD
00868          GO TO 1300-REWRITE-COMPANY-REC.
00869      IF PI-SAV-ENTRY-BATCH = PB-ENTRY-BATCH
00870          GO TO 1300-ASSIGN-NUMBER-LOOP.
00871  1300-REWRITE-COMPANY-REC.
00872      MOVE WS-BATCH-NO            TO BATCHI.
00873      MOVE AL-UANON               TO BATCHA.
00874
00875      MOVE 'C'                    TO  JP-RECORD-TYPE.
00876      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
00877
00878      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00879              WS-ELCNTL-RECORD-LENGTH  +  23.
00880
00881      
      * EXEC CICS REWRITE
00882 *        DATASET (ELCNTL-FILE-ID)
00883 *        FROM (CONTROL-FILE)
00884 *        END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004855' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00885
00886      PERFORM 8400-LOG-JOURNAL-RECORD.
00887
00888      GO TO 1300-EXIT.
00889
00890  1300-NO-COMPANY-REC.
00891      MOVE ER-2248                TO EMI-ERROR.
00892      MOVE -1                     TO BATCHL.
00893      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00894      GO TO 8200-SEND-DATAONLY.
00895  1300-EXIT.
00896      EXIT.
00897      EJECT
00898  2000-WRITE-BATCH-TOTAL-REC.
00899      MOVE PI-COMPANY-CD          TO PNDT-COMP-CD.
00900      MOVE PI-SAV-ENTRY-BATCH     TO PNDT-ENTRY-BATCH.
00901      MOVE 9999                   TO PNDT-BATCH-SEQ.
00902
00903      
      * EXEC CICS HANDLE CONDITION
00904 *        NOTFND (2100-ADD-BATCH-TOTAL-REC)
00905 *        NOTOPEN (7000-PNDT-FILE-NOTOPEN)
00906 *        END-EXEC.
      *    MOVE '"$IJ                  ! ( #00004877' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034383737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00907
00908      
      * EXEC CICS READ
00909 *        SET(ERPNDT-POINTER)
00910 *        DATASET(ERPNDT-FILE-ID)
00911 *        RIDFLD(ERPNDT-KEY)
00912 *        UPDATE
00913 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00004882' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00914      SERVICE RELOAD PENDING-BUSINESS.
00915
00916 ******************************************************************
00917 *                                                                *
00918 *    IF THE MONTH-END-DATE IN THE TRAILER RECORD (BATCH HEADER)  *
00919 *       IS NOT EQUAL TO THE ACCOUNTS MONTH-END-DATE AND THE      *
00920 *       OPERATOR HAS NOT ENTERED A NEW MONTH-END-DATE USE THE    *
00921 *       MONTH-END-DATE IN TRAILER RECORD FOR ALL DETAIL RECORDS. *
00922 *                                                                *
00923 ******************************************************************
00924
00925      IF  PI-NB-MONTH-END-DT = SPACES
00926          IF  PB-CREDIT-SELECT-DT = PI-CR-MONTH-END-DT
00927              NEXT SENTENCE
00928          ELSE
00929              MOVE PB-CREDIT-SELECT-DT  TO  PI-NB-MONTH-END-DT.
00930
00931      MOVE 'B'                    TO JP-RECORD-TYPE.
00932      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
00933
00934      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00935              WS-ERPNDT-RECORD-LENGTH  +  23.
00936
00937      PERFORM 8400-LOG-JOURNAL-RECORD.
00938
00939      IF ELFISSL NOT = ZEROS
00940          MOVE PI-LF-ISS-REMITTED  TO PB-B-LF-ISS-PRM-REMITTED.
00941      IF EAHISSL NOT = ZEROS
00942          MOVE PI-AH-ISS-REMITTED  TO PB-B-AH-ISS-PRM-REMITTED.
00943      IF EISSCNTL NOT = ZEROS
00944          MOVE PI-ISS-CNT-REMITTED TO PB-B-ISSUE-CNT-REMITTED.
00945      IF ECANCNTL NOT = ZEROS
00946          MOVE PI-CAN-CNT-REMITTED TO PB-B-CANCEL-CNT-REMITTED.
00947      IF ELFCANL NOT = ZEROS
00948          MOVE PI-LF-CAN-REMITTED  TO PB-B-LF-CAN-PRM-REMITTED.
00949      IF EAHCANL NOT = ZEROS
00950          MOVE PI-AH-CAN-REMITTED  TO PB-B-AH-CAN-PRM-REMITTED.
00951      IF WS-PF1
00952          MOVE PI-LF-ISS-ENTERED   TO PB-B-LF-ISS-PRM-ENTERED
00953          MOVE PI-AH-ISS-ENTERED   TO PB-B-AH-ISS-PRM-ENTERED
00954          MOVE PI-ISS-CNT-ENTERED  TO PB-B-ISSUE-CNT-ENTERED
00955          MOVE PI-CAN-CNT-ENTERED  TO PB-B-CANCEL-CNT-ENTERED
00956          MOVE PI-LF-CAN-ENTERED   TO PB-B-LF-CAN-PRM-ENTERED
00957          MOVE PI-AH-CAN-ENTERED   TO PB-B-AH-CAN-PRM-ENTERED
00958          MOVE PI-LAST-SEQ-NO-ADDED TO PB-B-HIGHEST-SEQ-NO.
00959
00960      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
00961      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
00962      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.
00963      MOVE 'C'                    TO JP-RECORD-TYPE.
00964      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
00965
00966      COMPUTE WS-JOURNAL-RECORD-LENGTH =
00967              WS-ERPNDT-RECORD-LENGTH  +  23.
00968
00969      
      * EXEC CICS REWRITE
00970 *        DATASET(ERPNDT-FILE-ID)
00971 *        FROM(PENDING-BUSINESS)
00972 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004943' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00973      PERFORM 8400-LOG-JOURNAL-RECORD.
00974
00975      IF  PI-NB-MONTH-END-DT = PB-CREDIT-SELECT-DT
00976          GO TO 2990-EXIT.
00977
00978      IF PI-NB-MONTH-END-DT GREATER THAN SPACES
00979         PERFORM 5000-UPDATE-ENTIRE-BATCH THRU
00980                 5900-EXIT.
00981
00982      GO TO 2990-EXIT.
00983      EJECT
00984  2100-ADD-BATCH-TOTAL-REC.
00985      
      * EXEC CICS GETMAIN
00986 *        SET(ERPNDT-POINTER)
00987 *        LENGTH(585)
00988 *        INITIMG(GETMAIN-SPACE)
00989 *        END-EXEC.
           MOVE 585
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004959' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-POINTER, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00990      SERVICE RELOAD PENDING-BUSINESS.
00991      MOVE 'PB'                   TO PB-RECORD-ID.
00992
00993      MOVE HIGH-VALUES            TO PB-CONTROL-BY-ACCOUNT.
00994
00995      MOVE PI-COMPANY-CD          TO PB-COMPANY-CD
00996                                     PB-COMPANY-CD-A1.
00997      MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.
00998      MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH
00999                                     PB-CERT-NO.
01000      MOVE 9999                   TO PB-BATCH-SEQ-NO.
01001      MOVE HIGH-VALUES            TO PB-CERT-EFF-DT.
01002      MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO
01003                                     PB-ALT-CHG-SEQ-NO.
01004
01005      MOVE '9'                    TO PB-RECORD-TYPE.
01006      MOVE PI-LF-ISS-REMITTED     TO PB-B-LF-ISS-PRM-REMITTED.
01007      MOVE PI-LF-ISS-ENTERED      TO PB-B-LF-ISS-PRM-ENTERED.
01008      MOVE PI-AH-ISS-REMITTED     TO PB-B-AH-ISS-PRM-REMITTED.
01009      MOVE PI-AH-ISS-ENTERED      TO PB-B-AH-ISS-PRM-ENTERED.
01010      MOVE PI-ISS-CNT-REMITTED    TO PB-B-ISSUE-CNT-REMITTED.
01011      MOVE PI-ISS-CNT-ENTERED     TO PB-B-ISSUE-CNT-ENTERED.
01012      MOVE PI-CAN-CNT-REMITTED    TO PB-B-CANCEL-CNT-REMITTED.
01013      MOVE PI-CAN-CNT-ENTERED     TO PB-B-CANCEL-CNT-ENTERED.
01014      MOVE PI-LF-CAN-REMITTED     TO PB-B-LF-CAN-PRM-REMITTED.
01015      MOVE PI-LF-CAN-ENTERED      TO PB-B-LF-CAN-PRM-ENTERED.
01016      MOVE PI-AH-CAN-REMITTED     TO PB-B-AH-CAN-PRM-REMITTED.
01017      MOVE PI-AH-CAN-ENTERED      TO PB-B-AH-CAN-PRM-ENTERED.
01018      MOVE ZEROS                  TO PB-B-LF-ISS-PRM-COMPUTED
01019                                     PB-B-LF-CAN-PRM-COMPUTED
01020                                     PB-B-AH-ISS-PRM-COMPUTED
01021                                     PB-B-AH-CAN-PRM-COMPUTED
01022                                     PB-LF-BILLED-AMTS
01023                                     PB-AH-BILLED-AMTS
01024                                     PB-CHG-COUNT
01025                                     PB-CALC-TOLERANCE.
01026      MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT
01027                                     PB-BILLED-DT
01028                                     PB-ACCT-EFF-DT
01029                                     PB-ACCT-EXP-DT.
01030      IF  PI-NB-MONTH-END-DT GREATER THAN SPACES
01031          MOVE PI-NB-MONTH-END-DT TO PB-CREDIT-SELECT-DT
01032         ELSE
01033          MOVE PI-CR-MONTH-END-DT     TO PB-CREDIT-SELECT-DT.
01034      MOVE PI-LAST-SEQ-NO-ADDED   TO PB-B-HIGHEST-SEQ-NO.
01035
01036      MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY
01037                                     PB-INPUT-BY.
01038      MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
01039      MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT
01040                                     PB-INPUT-DT.
01041      MOVE 'A'                    TO JP-RECORD-TYPE.
01042      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
01043
01044
01045      COMPUTE WS-JOURNAL-RECORD-LENGTH =
01046              WS-ERPNDT-RECORD-LENGTH  +  23.
01047
01048      MOVE PI-AM-NAME             TO  PB-ACCOUNT-NAME.
01049      MOVE PB-CONTROL-PRIMARY     TO  ERPNDT-KEY.
01050
01051      
      * EXEC CICS WRITE
01052 *        DATASET(ERPNDT-FILE-ID)
01053 *        FROM(PENDING-BUSINESS)
01054 *        RIDFLD(PB-CONTROL-PRIMARY)
01055 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005025' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01056      PERFORM 8400-LOG-JOURNAL-RECORD.
01057
01058  2990-EXIT.
01059      EXIT.
01060      EJECT
01061  3000-DELETE-ENTERED-BATCH.
01062
01063      MOVE SPACE          TO PI-NB-MONTH-END-DT.
01064
01065      IF NOT PI-DELETE-IS-OK
01066          MOVE 'Y'                TO PI-VERIFY-DELETE-SW
01067          GO TO 3300-FIRST-PF2.
01068      MOVE SPACE                  TO PI-VERIFY-DELETE-SW.
01069      MOVE PI-COMPANY-CD          TO PNDT-COMP-CD.
01070      MOVE PI-SAV-ENTRY-BATCH     TO PNDT-ENTRY-BATCH.
01071      MOVE 1                      TO PNDT-BATCH-SEQ.
01072
01073      
      * EXEC CICS SYNCPOINT
01074 *         END-EXEC.
      *    MOVE '6"                    !   #00005047' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01075
01076      
      * EXEC CICS HANDLE CONDITION
01077 *        NOTFND(3200-NO-RECORDS)
01078 *        NOTOPEN(7000-PNDT-FILE-NOTOPEN)
01079 *        END-EXEC.
      *    MOVE '"$IJ                  ! ) #00005050' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035303530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01080
01081
01082      
      * EXEC CICS STARTBR
01083 *        DATASET(ERPNDT-FILE-ID)
01084 *        RIDFLD(ERPNDT-KEY)
01085 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005056' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01086
01087  3000-GET-NEXT-RECORD.
01088
01089      
      * EXEC CICS HANDLE CONDITION
01090 *        NOTFND(3120-END-ROUTINE)
01091 *        END-EXEC.
      *    MOVE '"$I                   ! * #00005063' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035303633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01092
01093      ADD +1                      TO  WS-SYNC-CNTR.
01094      IF  WS-SYNC-CNTR GREATER THAN +100
01095          MOVE +0                 TO  WS-SYNC-CNTR
01096          
      * EXEC  CICS SYNCPOINT
01097 *              END-EXEC.
      *    MOVE '6"                    !   #00005070' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01098
01099      ON 1
01100         GO TO 3005-READ-NEXT-RECORD.
01101
01102      
      * EXEC CICS STARTBR
01103 *        DATASET(ERPNDT-FILE-ID)
01104 *        RIDFLD(ERPNDT-KEY)
01105 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005076' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01106
01107      
      * EXEC CICS HANDLE CONDITION
01108 *        NOTFND(3100-STOP-BROWSE)
01109 *        ENDFILE(3100-STOP-BROWSE)
01110 *        END-EXEC.
      *    MOVE '"$I''                  ! + #00005081' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035303831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01111
01112  3005-READ-NEXT-RECORD.
01113
01114      
      * EXEC CICS READNEXT
01115 *        SET(ERPNDT-POINTER)
01116 *        DATASET(ERPNDT-FILE-ID)
01117 *        RIDFLD(ERPNDT-KEY)
01118 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005088' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01119
01120      SERVICE RELOAD PENDING-BUSINESS.
01121
01122      IF PB-COMPANY-CD = PI-SAV-COMP-CD
01123        AND PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
01124          NEXT SENTENCE
01125      ELSE
01126          GO TO 3100-STOP-BROWSE.
01127
01128      IF PB-BILLED-DT NOT = LOW-VALUES
01129          MOVE ER-2402            TO EMI-ERROR
01130          MOVE -1                 TO BATCHL
01131          MOVE AL-UABON           TO BATCHA
01132          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01133          GO TO 8200-SEND-DATAONLY.
01134
01135 ******************************************************************
01136 *                                                                *
01137 *                DELETE PENDING ADDRESS RECORD                   *
01138 *                                                                *
01139 ******************************************************************
01140
01141      IF PB-I-MAIL-ADDRS-PRESENT
01142         NEXT SENTENCE
01143        ELSE
01144         GO TO 3006-DELETE-CERTIFICATE.
01145
01146      
      * EXEC CICS HANDLE CONDITION
01147 *         NOTOPEN  (3006-DELETE-CERTIFICATE)
01148 *         NOTFND   (3006-DELETE-CERTIFICATE)
01149 *         END-EXEC.
      *    MOVE '"$JI                  ! , #00005120' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035313230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01150
01151      
      * EXEC CICS READ
01152 *        EQUAL
01153 *        DATASET   (ERPNDM-FILE-ID)
01154 *        SET       (ERPNDM-POINTER)
01155 *        RIDFLD    (ERPNDT-KEY)
01156 *        UPDATE
01157 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00005125' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDM-FILE-ID, 
                 ERPNDM-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01158
01159         SERVICE RELOAD PENDING-MAILING-DATA.
01160
01161      MOVE 'D'                    TO  JP-RECORD-TYPE.
01162      MOVE PENDING-MAILING-DATA   TO  JP-RECORD-AREA.
01163
01164      COMPUTE WS-JOURNAL-RECORD-LENGTH =
01165              WS-ERPNDM-RECORD-LENGTH  +  23.
01166
01167      
      * EXEC CICS DELETE
01168 *        DATASET   (ERPNDM-FILE-ID)
01169 *        END-EXEC.
      *    MOVE '&(                    &   #00005141' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDM-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01170
01171      PERFORM 8400-LOG-JOURNAL-RECORD.
01172
01173  3006-DELETE-CERTIFICATE.
01174
01175      
      * EXEC CICS HANDLE CONDITION
01176 *         NOTFND  (3020-CONTINUE-DELETE)
01177 *         END-EXEC.
      *    MOVE '"$I                   ! - #00005149' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303035313439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01178
01179      MOVE PB-CONTROL-BY-ACCOUNT  TO ELCERT-KEY.
01180      MOVE PB-SV-CARRIER          TO CERT-CARRIER.
01181      MOVE PB-SV-GROUPING         TO CERT-GROUPING.
01182      MOVE PB-SV-STATE            TO CERT-STATE.
01183      
      * EXEC CICS READ
01184 *        SET     (ELCERT-POINTER)
01185 *        DATASET (ELCERT-FILE-ID)
01186 *        RIDFLD  (ELCERT-KEY)
01187 *        UPDATE
01188 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00005157' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 ELCERT-POINTER, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01189
01190      SERVICE RELOAD CERTIFICATE-MASTER.
01191
01192      IF INSURED-ADDR-PRESENT
01193         MOVE 'Y'                  TO WS-CERT-ADDRESS-SW
01194      ELSE
01195         MOVE ' '                  TO WS-CERT-ADDRESS-SW.
01196
01197      IF  CERT-NOTES-ARE-NOT-PRESENT
01198          MOVE ' '                TO WS-CERT-NOTE-SW
01199      ELSE
01200          MOVE 'Y'                TO WS-CERT-NOTE-SW.
01201
01202      IF PB-CANCELLATION
01203         IF PB-CI-LF-PRIOR-CANCEL-DT = LOW-VALUES
01204            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2
01205            MOVE ZERO                  TO CM-LF-ITD-CANCEL-AMT
01206            MOVE LOW-VALUE             TO CM-LF-CANCEL-EXIT-DT
01207            MOVE LOW-VALUE             TO CM-LF-CANCEL-DT
01208            MOVE CM-LF-STATUS-AT-CANCEL TO CM-LF-CURRENT-STATUS
01209            MOVE SPACE                 TO CM-LF-STATUS-AT-CANCEL
01210         ELSE
01211            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2
01212            MOVE PB-CI-LF-CANCEL-AMT   TO CM-LF-ITD-CANCEL-AMT
01213            MOVE PB-CI-LF-PRIOR-CANCEL-DT TO CM-LF-CANCEL-DT
01214            MOVE PB-CI-LF-POLICY-STATUS   TO CM-LF-CURRENT-STATUS
01215            MOVE PB-CI-LIVES           TO CM-LIVES.
01216
01217      IF PB-CANCELLATION
01218         IF PB-CI-AH-PRIOR-CANCEL-DT = LOW-VALUES
01219            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2
01220            MOVE ZERO                  TO CM-AH-ITD-CANCEL-AMT
01221            MOVE LOW-VALUE             TO CM-AH-CANCEL-EXIT-DT
01222            MOVE LOW-VALUE             TO CM-AH-CANCEL-DT
01223            MOVE CM-AH-STATUS-AT-CANCEL TO CM-AH-CURRENT-STATUS
01224            MOVE SPACE                 TO CM-AH-STATUS-AT-CANCEL
01225            GO TO 3010-REWRITE-CERT
01226         ELSE
01227            MOVE SPACE                 TO CM-CREDIT-INTERFACE-SW-2
01228            MOVE PB-CI-AH-CANCEL-AMT   TO CM-AH-ITD-CANCEL-AMT
01229            MOVE PB-CI-AH-PRIOR-CANCEL-DT TO CM-AH-CANCEL-DT
01230            MOVE PB-CI-AH-POLICY-STATUS   TO CM-AH-CURRENT-STATUS
01231            MOVE PB-CI-LIVES           TO CM-LIVES
01232            GO TO 3010-REWRITE-CERT.
01233
01234      IF CERT-ADDED-BATCH  AND
01235         (NO-CLAIM-ATTACHED OR CM-CLAIM-ATTACHED-COUNT = ZEROS)
01236         GO TO 3010-REWRITE-CERT.
01237
01238      IF NO-CLAIM-ATTACHED  NEXT SENTENCE
01239        ELSE
01240         MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-1
01241         MOVE '2'                    TO CM-CLAIM-INTERFACE-SW
01242         GO TO 3010-REWRITE-CERT.
01243
01244         
      * EXEC CICS DELETE
01245 *            DATASET    (ELCERT-FILE-ID)
01246 *            END-EXEC.
      *    MOVE '&(                    &   #00005218' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01247
01248 ******************************************************************
01249 *                                                                *
01250 *       DELETE CERTIFICATE NOTES FOR ALL DELETED CERTIFICATES    *
01251 *                                                                *
01252 ******************************************************************
01253
01254      IF CERT-NOTES-ARE-PRESENT
01255         NEXT SENTENCE
01256      ELSE
01257         GO TO 3007-DELETE-MAILING-DATA.
01258
01259      
      * EXEC CICS HANDLE CONDITION
01260 *         NOTFND   (3007-DELETE-MAILING-DATA)
01261 *         END-EXEC.
      *    MOVE '"$I                   ! . #00005233' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303035323333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01262
01263      
      * EXEC CICS READ
01264 *        EQUAL
01265 *        DATASET   (ERNOTE-FILE-ID)
01266 *        SET       (ERNOTE-POINTER)
01267 *        RIDFLD    (ELCERT-KEY)
01268 *        UPDATE
01269 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00005237' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 ERNOTE-POINTER, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01270
01271         SERVICE RELOAD CERTIFICATE-NOTE.
01272
01273      MOVE 'D'                    TO  JP-RECORD-TYPE.
01274      MOVE CERTIFICATE-NOTE       TO  JP-RECORD-AREA.
01275
01276      COMPUTE WS-JOURNAL-RECORD-LENGTH =
01277              WS-ERNOTE-RECORD-LENGTH  +  23.
01278
01279      
      * EXEC CICS DELETE
01280 *        DATASET   (ERNOTE-FILE-ID)
01281 *        END-EXEC.
      *    MOVE '&(                    &   #00005253' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01282
01283      PERFORM 8400-LOG-JOURNAL-RECORD.
01284
01285
01286
01287 ******************************************************************
01288 *                                                                *
01289 *     DELETE MAILING ADDRESS RECORDS FOR DELETED CERTIFICATES    *
01290 *                                                                *
01291 ******************************************************************
01292
01293  3007-DELETE-MAILING-DATA.
01294
01295      IF CERT-ADDRESS-PRESENT
01296         NEXT SENTENCE
01297      ELSE
01298         GO TO 3020-CONTINUE-DELETE.
01299
01300      
      * EXEC CICS HANDLE CONDITION
01301 *         NOTOPEN  (3020-CONTINUE-DELETE)
01302 *         NOTFND   (3020-CONTINUE-DELETE)
01303 *         END-EXEC.
      *    MOVE '"$JI                  ! / #00005274' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303035323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01304
01305      
      * EXEC CICS READ
01306 *        EQUAL
01307 *        DATASET   (ERMAIL-FILE-ID)
01308 *        SET       (ERMAIL-POINTER)
01309 *        RIDFLD    (ELCERT-KEY)
01310 *        UPDATE
01311 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00005279' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 ERMAIL-POINTER, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01312
01313         SERVICE RELOAD MAILING-DATA.
01314
01315      MOVE 'D'                    TO  JP-RECORD-TYPE.
01316      MOVE MAILING-DATA           TO  JP-RECORD-AREA.
01317
01318      COMPUTE WS-JOURNAL-RECORD-LENGTH =
01319              WS-ERMAIL-RECORD-LENGTH  +  23.
01320
01321      
      * EXEC CICS DELETE
01322 *        DATASET   (ERMAIL-FILE-ID)
01323 *        END-EXEC.
      *    MOVE '&(                    &   #00005295' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01324
01325      PERFORM 8400-LOG-JOURNAL-RECORD.
01326
01327      GO TO 3020-CONTINUE-DELETE.
01328
01329  3010-REWRITE-CERT.
01330
01331      MOVE SPACE                  TO CM-CREDIT-INTERFACE-SW-1.
01332
01333         
      * EXEC CICS REWRITE
01334 *            DATASET    (ELCERT-FILE-ID)
01335 *            FROM       (CERTIFICATE-MASTER)
01336 *            END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005307' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01337
01338  3020-CONTINUE-DELETE.
01339
01340      COMPUTE WS-JOURNAL-RECORD-LENGTH =
01341              WS-ERPNDT-RECORD-LENGTH  +  23.
01342
01343      MOVE 'D'                    TO JP-RECORD-TYPE.
01344      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
01345
01346      
      * EXEC CICS ENDBR
01347 *        DATASET(ERPNDT-FILE-ID)
01348 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005320' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01349
01350      
      * EXEC CICS DELETE
01351 *        DATASET(ERPNDT-FILE-ID)
01352 *        RIDFLD(ERPNDT-KEY)
01353 *        END-EXEC.
      *    MOVE '&(  R                 &   #00005324' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01354
01355      PERFORM 8400-LOG-JOURNAL-RECORD.
01356      ADD 1   TO WS-DELETE-CNT.
01357      GO TO 3000-GET-NEXT-RECORD.
01358
01359  3100-STOP-BROWSE.
01360
01361      
      * EXEC CICS ENDBR
01362 *        DATASET(ERPNDT-FILE-ID)
01363 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005335' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01364
01365  3120-END-ROUTINE.
01366
01367      MOVE SPACE                  TO PI-UPDATE-SW.
01368      IF WS-DELETE-CNT NOT GREATER THAN ZERO
01369          GO TO 3200-NO-RECORDS.
01370      IF PI-SAVE-CALLING-PGM NOT = XCTL-9301
01371          MOVE ZEROS              TO EMI-ERROR
01372          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01373      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.
01374      MOVE LOW-VALUES             TO EL930AO.
01375      MOVE QUESTION-MARKS         TO BATCHI.
01376      MOVE AL-UANON               TO BATCHA.
01377      MOVE -1                     TO MAINTL.
01378      GO TO 8100-SEND-INITIAL-MAP.
01379
01380  3200-NO-RECORDS.
01381      MOVE ER-2242                TO EMI-ERROR.
01382      MOVE -1 TO BATCHL
01383      MOVE AL-UABON               TO BATCHA.
01384      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01385      GO TO 8200-SEND-DATAONLY.
01386
01387  3300-FIRST-PF2.
01388      MOVE ER-2422                TO EMI-ERROR.
01389      MOVE -1                     TO PFENTERL.
01390      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01391      GO TO 8100-SEND-INITIAL-MAP.
01392      EJECT
01393  4000-SHOW-TOTALS.
01394
01395      MOVE LOW-VALUES             TO EL930AI.
01396
01397      MOVE PI-SAV-ENTRY-BATCH     TO BATCHO.
01398
01399      MOVE PI-MAINT-FUNC          TO MAINTO.
01400
01401
01402      IF PI-DATA-UPDATED
01403          MOVE AL-SANON           TO BATCHA
01404                                     MAINTA
01405      ELSE
01406          MOVE AL-UANON           TO BATCHA
01407                                     MAINTA.
01408
01409      MOVE PI-LF-ISS-ENTERED      TO ALFISSO.
01410
01411      IF PI-LF-ISS-REMITTED NOT = ZEROS
01412          MOVE PI-LF-ISS-REMITTED TO ELFISSO
01413          MOVE AL-UNNON           TO ELFISSA
01414          COMPUTE WS-OBAL = PI-LF-ISS-REMITTED - PI-LF-ISS-ENTERED
01415          IF WS-OBAL NOT = ZEROS
01416              MOVE WS-OBAL        TO OLFISSO.
01417
01418      MOVE PI-LF-CAN-ENTERED      TO ALFCANO.
01419      IF PI-LF-CAN-REMITTED NOT = ZEROS
01420          MOVE PI-LF-CAN-REMITTED TO ELFCANO
01421          MOVE AL-UNNON           TO ELFCANA
01422          COMPUTE WS-OBAL = PI-LF-CAN-REMITTED - PI-LF-CAN-ENTERED
01423          IF WS-OBAL NOT = ZEROS
01424              MOVE WS-OBAL        TO OLFCANO.
01425
01426      MOVE PI-AH-ISS-ENTERED      TO AAHISSO.
01427      IF PI-AH-ISS-REMITTED NOT = ZEROS
01428          MOVE PI-AH-ISS-REMITTED TO EAHISSO
01429          MOVE AL-UNNON           TO EAHISSA
01430          COMPUTE WS-OBAL = PI-AH-ISS-REMITTED - PI-AH-ISS-ENTERED
01431          IF WS-OBAL NOT = ZEROS
01432              MOVE WS-OBAL        TO OAHISSO.
01433
01434      MOVE PI-AH-CAN-ENTERED      TO AAHCANO.
01435      IF PI-AH-CAN-REMITTED NOT = ZEROS
01436          MOVE PI-AH-CAN-REMITTED TO EAHCANO
01437          MOVE AL-UNNON           TO EAHCANA
01438          COMPUTE WS-OBAL = PI-AH-CAN-REMITTED - PI-AH-CAN-ENTERED
01439          IF WS-OBAL NOT = ZEROS
01440              MOVE WS-OBAL        TO OAHCANO.
01441
01442      MOVE PI-ISS-CNT-ENTERED     TO AISSCNTO.
01443      IF PI-ISS-CNT-REMITTED NOT = ZEROS
01444          MOVE PI-ISS-CNT-REMITTED TO EISSCNTO
01445          MOVE AL-UNNON            TO EISSCNTA
01446          COMPUTE WS-OCNT = PI-ISS-CNT-REMITTED -
01447                                 PI-ISS-CNT-ENTERED
01448          IF WS-OCNT NOT = ZEROS
01449              MOVE WS-OCNT        TO OISSCNTO.
01450
01451      MOVE PI-CAN-CNT-ENTERED     TO ACANCNTO.
01452      IF PI-CAN-CNT-REMITTED NOT = ZEROS
01453          MOVE PI-CAN-CNT-REMITTED TO ECANCNTO
01454          MOVE AL-UNNON            TO ECANCNTA
01455          COMPUTE WS-OCNT = PI-CAN-CNT-REMITTED -
01456                                 PI-CAN-CNT-ENTERED
01457          IF WS-OCNT NOT = ZEROS
01458              MOVE WS-OCNT        TO OCANCNTO.
01459
01460
01461      IF  PI-NB-MONTH-END-DT GREATER THAN SPACES
01462          MOVE PI-NB-MONTH-END-DT TO DC-BIN-DATE-1
01463        ELSE
01464          MOVE PI-CR-MONTH-END-DT TO DC-BIN-DATE-1.
01465
01466      MOVE  SPACE                 TO  DC-OPTION-CODE.
01467      PERFORM  9700-DATE-LINK.
01468
01469      IF  DATE-CONVERSION-ERROR
01470          MOVE LOW-VALUES TO MNTHNDTO
01471          GO TO 0490-EXIT.
01472
01473      MOVE DC-GREG-DATE-1-EDIT    TO  MNTHNDTO.
01474      MOVE AL-UANON               TO  MNTHNDTA.
01475
01476      GO TO 8100-SEND-INITIAL-MAP.
01477      EJECT
01478
01479 *****************************************************************
01480 *                                                               *
01481 *            U P D A T E   E N T I R E   B A T C H              *
01482 *                                                               *
01483 *  THIS SECTION UPDATES AN ENTIRE BATCH WITH A NEW MONTH END     *
01484 *  DATE.                                                        *
01485 *                                                               *
01486 *****************************************************************
01487
01488  5000-UPDATE-ENTIRE-BATCH.
01489
01490      MOVE ZEROS                  TO  PNDT-BATCH-SEQ
01491                                      PNDT-BATCH-CHG-SEQ.
01492
01493      
      * EXEC CICS HANDLE CONDITION
01494 *        NOTFND(5900-EXIT)
01495 *        NOTOPEN(7000-PNDT-FILE-NOTOPEN)
01496 *        END-EXEC.
      *    MOVE '"$IJ                  ! 0 #00005467' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303035343637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01497
01498      
      * EXEC CICS STARTBR
01499 *        DATASET(ERPNDT-FILE-ID)
01500 *        RIDFLD(ERPNDT-KEY)
01501 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005472' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01502
01503  5010-GET-NEXT-RECORD.
01504      
      * EXEC CICS HANDLE CONDITION
01505 *        NOTFND(5100-STOP-BROWSE)
01506 *        ENDFILE(5100-STOP-BROWSE)
01507 *        END-EXEC.
      *    MOVE '"$I''                  ! 1 #00005478' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303035343738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01508
01509      
      * EXEC CICS READNEXT
01510 *        SET(ERPNDT-POINTER)
01511 *        DATASET(ERPNDT-FILE-ID)
01512 *        RIDFLD(ERPNDT-KEY)
01513 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005483' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01514
01515
01516      SERVICE RELOAD PENDING-BUSINESS.
01517
01518      ON 1
01519           MOVE PB-CONTROL-PRIMARY     TO WS-SAV-PNDT-KEY.
01520
01521      IF PB-COMPANY-CD = WS-SAV-COMP-CD
01522        AND PB-ENTRY-BATCH = WS-SAV-ENTRY-BATCH
01523          NEXT SENTENCE
01524      ELSE
01525          GO TO 5100-STOP-BROWSE.
01526
01527
01528      IF  PB-CREDIT-SELECT-DT = PI-NB-MONTH-END-DT
01529          GO TO 5010-GET-NEXT-RECORD.
01530
01531
01532
01533
01534      
      * EXEC CICS READ
01535 *        SET     (ERPNDT-POINTER)
01536 *        DATASET (ERPNDT-FILE-ID)
01537 *        RIDFLD  (ERPNDT-KEY)
01538 *        UPDATE
01539 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00005508' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01540
01541      SERVICE RELOAD PENDING-BUSINESS.
01542
01543
01544      COMPUTE WS-JOURNAL-RECORD-LENGTH =
01545              WS-ERPNDT-RECORD-LENGTH  +  23.
01546
01547      MOVE 'B'                    TO JP-RECORD-TYPE.
01548      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
01549      PERFORM 8400-LOG-JOURNAL-RECORD.
01550
01551      MOVE  PI-NB-MONTH-END-DT    TO PB-CREDIT-SELECT-DT.
01552
01553      MOVE 'C'                    TO JP-RECORD-TYPE.
01554      MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
01555
01556  5050-REWRITE-PENDING-BUS.
01557         
      * EXEC CICS REWRITE
01558 *            DATASET    (ERPNDT-FILE-ID)
01559 *            FROM       (PENDING-BUSINESS)
01560 *            END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005531' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01561
01562
01563      COMPUTE WS-JOURNAL-RECORD-LENGTH =
01564              WS-ERPNDT-RECORD-LENGTH  +  23.
01565
01566      PERFORM 8400-LOG-JOURNAL-RECORD.
01567
01568      GO TO 5010-GET-NEXT-RECORD.
01569
01570  5100-STOP-BROWSE.
01571
01572      
      * EXEC CICS ENDBR
01573 *        DATASET(ERPNDT-FILE-ID)
01574 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005546' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01575
01576  5900-EXIT.
01577       EXIT.
01578
01579      EJECT
01580
01581
01582  6000-BROWSE-BATCH-HEADERS.
01583
01584      
      * EXEC CICS HANDLE CONDITION
01585 *        NOTFND (6200-END-OF-FILE)
01586 *        END-EXEC.
      *    MOVE '"$I                   ! 2 #00005558' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303035353538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01587
01588      MOVE SPACES                 TO ERPNDT-KEY.
01589
01590      MOVE PI-COMPANY-CD          TO PNDT-COMP-CD.
01591
01592      IF  BATCHI = QUESTION-MARKS
01593          NEXT SENTENCE
01594      ELSE
01595          MOVE BATCHI             TO PNDT-ENTRY-BATCH
01596          MOVE 9999               TO PNDT-BATCH-SEQ.
01597
01598      
      * EXEC CICS STARTBR
01599 *        DATASET (ERPNDT-FILE-ID)
01600 *        RIDFLD  (ERPNDT-KEY)
01601 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005572' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01602
01603      
      * EXEC CICS HANDLE CONDITION
01604 *        ENDFILE  (6100-END-BROWSE)
01605 *        END-EXEC.
      *    MOVE '"$''                   ! 3 #00005577' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303035353737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01606
01607  6010-READ-NEXT-RECORD.
01608
01609      
      * EXEC CICS READNEXT
01610 *         DATASET   (ERPNDT-FILE-ID)
01611 *         SET       (ERPNDT-POINTER)
01612 *         RIDFLD    (ERPNDT-KEY)
01613 *         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005583' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 ERPNDT-POINTER, 
                 DFHEIV99, 
                 ERPNDT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01614
01615      SERVICE RELOAD PENDING-BUSINESS.
01616
01617      IF PB-COMPANY-CD NOT = PI-COMPANY-CD
01618         GO TO 6100-END-BROWSE.
01619
01620      IF PB-BATCH-TRAILER
01621         MOVE PB-ENTRY-BATCH      TO BATCHI
01622         MOVE +6                  TO BATCHL
01623         PERFORM 6100-END-BROWSE
01624         PERFORM 0400-PRIME-BATCH-TOTALS THRU 0490-EXIT
01625         GO TO 4000-SHOW-TOTALS.
01626
01627      MOVE PB-ENTRY-BATCH     TO PNDT-ENTRY-BATCH.
01628      MOVE 9998               TO PNDT-BATCH-SEQ.
01629
01630      GO TO 6010-READ-NEXT-RECORD.
01631
01632  6100-END-BROWSE.
01633
01634      
      * EXEC CICS ENDBR
01635 *        DATASET(ERPNDT-FILE-ID)
01636 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005608' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPNDT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01637
01638  6200-END-OF-FILE.
01639
01640      MOVE -1                  TO MAINTL.
01641      MOVE 2251                TO EMI-ERROR.
01642      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01643      GO TO 8100-SEND-INITIAL-MAP.
01644
01645      EJECT
01646
01647  7000-PNDT-FILE-NOTOPEN.
01648      MOVE -1 TO                  MAINTL.
01649      MOVE 2216                   TO EMI-ERROR.
01650      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01651      GO TO 8200-SEND-DATAONLY.
01652
01653
01654
01655  7100-CNTL-FILE-NOTOPEN.
01656      MOVE -1                     TO MAINTL.
01657      MOVE 2214                   TO EMI-ERROR.
01658      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01659      GO TO 8200-SEND-DATAONLY.
01660
01661
01662      EJECT
01663
01664  8100-SEND-INITIAL-MAP.
01665      MOVE WS-CURRENT-DT          TO DATEO.
01666      MOVE EIBTIME                TO TIME-IN.
01667      MOVE TIME-OUT               TO TIMEO.
01668      MOVE -1                     TO MAINTL.
01669      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
01670      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
01671
01672      MOVE PI-LIFE-OVERRIDE-L2    TO WS-PRM-OVERRIDE
01673                                     WS-REFUND-OVERRIDE.
01674      MOVE WS-PRM-HEADER          TO LFPHDGO.
01675      MOVE WS-REFUND-HEADER       TO LFRHDGO.
01676
01677      MOVE PI-AH-OVERRIDE-L2      TO WS-PRM-OVERRIDE
01678                                     WS-REFUND-OVERRIDE.
01679      MOVE WS-PRM-HEADER          TO AHPHDGO.
01680      MOVE WS-REFUND-HEADER       TO AHRHDGO.
01681
01682
01683      
      * EXEC CICS SEND
01684 *        MAP      (MAP-NAME)
01685 *        MAPSET   (MAPSET-NAME)
01686 *        FROM     (EL930AO)
01687 *        ERASE
01688 *        CURSOR
01689 *        END-EXEC.
           MOVE LENGTH OF
            EL930AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005657' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL930AO, 
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
           
01690      GO TO 9100-RETURN-TRAN.
01691
01692      EJECT
01693
01694  8200-SEND-DATAONLY.
01695      MOVE WS-CURRENT-DT          TO DATEO.
01696      MOVE EIBTIME                TO TIME-IN.
01697      MOVE TIME-OUT               TO TIMEO.
01698      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
01699      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
01700      
      * EXEC CICS SEND
01701 *        MAP      (MAP-NAME)
01702 *        MAPSET   (MAPSET-NAME)
01703 *        FROM     (EL930AO)
01704 *        DATAONLY
01705 *        ERASEAUP
01706 *        CURSOR
01707 *        END-EXEC.
           MOVE LENGTH OF
            EL930AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00005674' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL930AO, 
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
           
01708      GO TO 9100-RETURN-TRAN.
01709
01710  8300-SEND-TEXT.
01711      
      * EXEC CICS SEND TEXT
01712 *        FROM     (LOGOFF-TEXT)
01713 *        LENGTH   (LOGOFF-LENGTH)
01714 *        ERASE
01715 *        FREEKB
01716 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005685' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363835' TO DFHEIV0(25:11)
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
           
01717      
      * EXEC CICS RETURN
01718 *        END-EXEC.
      *    MOVE '.(                    &   #00005691' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01719      EJECT
01720  8400-LOG-JOURNAL-RECORD.
01721      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
01722      MOVE ERPNDT-FILE-ID         TO JP-FILE-ID.
01723      MOVE THIS-PGM               TO JP-PROGRAM-ID.
01724
01725 *    EXEC CICS JOURNAL
01726 *        JFILEID     (PI-JOURNAL-FILE-ID)
01727 *        JTYPEID     ('EL')
01728 *        FROM        (JOURNAL-RECORD)
01729 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
01730 *        END-EXEC.
01731
01732
01733
01734
01735  8600-DEEDIT.
01736
01737      
      * EXEC CICS BIF DEEDIT
01738 *        FIELD(WS-DEEDIT-FIELD)
01739 *        LENGTH(10)
01740 *        END-EXEC.
           MOVE 10
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005711' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01741  8600-EXIT.
01742      EXIT.
01743      EJECT
01744  8800-UNAUTHORIZED-ACCESS.
01745      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01746      GO TO 8300-SEND-TEXT.
01747
01748  8810-PF23.
01749
01750      IF NOT PI-DATA-UPDATED
01751          NEXT SENTENCE
01752      ELSE
01753          MOVE -1                 TO PFENTERL
01754          MOVE ER-2213            TO EMI-ERROR
01755          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01756          GO TO 4000-SHOW-TOTALS.
01757
01758      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01759      MOVE XCTL-005               TO PGM-NAME.
01760      GO TO 9300-XCTL.
01761  9000-RETURN-CICS.
01762      
      * EXEC CICS RETURN
01763 *        END-EXEC.
      *    MOVE '.(                    &   #00005736' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01764
01765  9100-RETURN-TRAN.
01766      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01767      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
01768      
      * EXEC CICS RETURN
01769 *        TRANSID    (TRANS-ID)
01770 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01771 *        LENGTH     (PI-COMM-LENGTH)
01772 *        END-EXEC.
      *    MOVE '.(CT                  &   #00005742' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01773
01774  9200-RETURN-MAIN-MENU.
01775
01776      IF NOT PI-DATA-UPDATED
01777          NEXT SENTENCE
01778      ELSE
01779          MOVE -1                 TO PFENTERL
01780          MOVE ER-2213            TO EMI-ERROR
01781          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01782          GO TO 4000-SHOW-TOTALS.
01783
01784      MOVE XCTL-626               TO PGM-NAME.
01785      GO TO 9300-XCTL.
01786
01787  9300-XCTL.
01788      
      * EXEC CICS XCTL
01789 *        PROGRAM    (PGM-NAME)
01790 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01791 *        LENGTH     (PI-COMM-LENGTH)
01792 *        END-EXEC.
      *    MOVE '.$C                   $   #00005762' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01793
01794  9400-CLEAR.
01795      IF NOT PI-DATA-UPDATED
01796          NEXT SENTENCE
01797      ELSE
01798          MOVE -1                 TO PFENTERL
01799          MOVE ER-2213            TO EMI-ERROR
01800          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01801          GO TO 4000-SHOW-TOTALS.
01802      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
01803      GO TO 9300-XCTL.
01804
01805  9500-PF12.
01806      MOVE XCTL-010               TO PGM-NAME.
01807      GO TO 9300-XCTL.
01808
01809  9600-PGMID-ERROR.
01810      
      * EXEC CICS HANDLE CONDITION
01811 *        PGMIDERR    (8300-SEND-TEXT)
01812 *        END-EXEC.
      *    MOVE '"$L                   ! 4 #00005784' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303035373834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01813      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01814      MOVE ' '                    TO PI-ENTRY-CD-1.
01815      MOVE XCTL-005               TO PGM-NAME.
01816      MOVE PGM-NAME               TO LOGOFF-PGM.
01817      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01818      GO TO 9300-XCTL.
01819
01820  9700-DATE-LINK.
01821      MOVE LINK-CLDATCV           TO PGM-NAME.
01822      
      * EXEC CICS LINK
01823 *        PROGRAM    (PGM-NAME)
01824 *        COMMAREA   (DATE-CONVERSION-DATA)
01825 *        LENGTH     (DC-COMM-LENGTH)
01826 *        END-EXEC.
      *    MOVE '."C                   ''   #00005796' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01827
01828
01829  9900-ERROR-FORMAT.
01830      IF NOT EMI-ERRORS-COMPLETE
01831          MOVE LINK-001           TO PGM-NAME
01832          
      * EXEC CICS LINK
01833 *            PROGRAM    (PGM-NAME)
01834 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01835 *            LENGTH     (EMI-COMM-LENGTH)
01836 *            END-EXEC.
      *    MOVE '."C                   ''   #00005806' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01837  9900-EXIT.
01838      EXIT.
01839
01840  9990-ABEND.
01841      MOVE LINK-004               TO PGM-NAME.
01842      MOVE DFHEIBLK               TO EMI-LINE1.
01843      
      * EXEC CICS LINK
01844 *        PROGRAM   (PGM-NAME)
01845 *        COMMAREA  (EMI-LINE1)
01846 *        LENGTH    (72)
01847 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005817' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01848      MOVE -1                     TO PFENTERL.
01849      GO TO 8200-SEND-DATAONLY.
01850      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL930' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01851
01852  9995-SECURITY-VIOLATION.
01853 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00005844' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383434' TO DFHEIV0(25:11)
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
01854
01855  9995-EXIT.
01856      EXIT.
01857
01858

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL930' TO DFHEIV1
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
               GO TO 0350-NO-RECORDS,
                     0350-NO-RECORDS,
                     7000-PNDT-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0480-NO-BATCH-TRAILER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1250-NO-COMPANY-REC,
                     7100-CNTL-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1300-NO-COMPANY-REC,
                     7100-CNTL-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1300-REWRITE-COMPANY-REC,
                     7000-PNDT-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 2100-ADD-BATCH-TOTAL-REC,
                     7000-PNDT-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3200-NO-RECORDS,
                     7000-PNDT-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 3120-END-ROUTINE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 3100-STOP-BROWSE,
                     3100-STOP-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 3006-DELETE-CERTIFICATE,
                     3006-DELETE-CERTIFICATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 3020-CONTINUE-DELETE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 3007-DELETE-MAILING-DATA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 3020-CONTINUE-DELETE,
                     3020-CONTINUE-DELETE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 5900-EXIT,
                     7000-PNDT-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 5100-STOP-BROWSE,
                     5100-STOP-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 6200-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 6100-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL930' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
