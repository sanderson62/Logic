00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL658 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/14/96 11:58:19.
00007 *                            VMOD=2.005
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS.    TRANSACTION - EXJ3 - DISPLAY GENERAL AGENT
00024 *                                 CROSS REFERENCE RECORDS.
00025
00026
00027  ENVIRONMENT DIVISION.
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL658 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.005 ************'.
00034
00035  77  NDX     PIC S9(5)  COMP-3 VALUE +1.
00036
00037  01  WS-DATE-AREA.
00038      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00039      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00040
00041  01  WS-CONTROL-PRIMARY.
00042      05  WS-COMPANY-CD       PIC X.
00043      05  WS-CARRIER          PIC X.
00044      05  WS-GROUPING         PIC X(6).
00045      05  WS-AGENT            PIC X(10).
00046
00047      05  WS-OPEN-COUNT       PIC S9(4)    COMP-3 VALUE ZEROS.
00048
00049 *                            COPY ELCATTR SUPPRESS.
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
00050
00051      EJECT
00052
00053  01  ERROR-MESSAGES.
00054      12  ER-0004                 PIC X(4)  VALUE '0004'.
00055      12  ER-0029                 PIC X(4)  VALUE '0029'.
00056      12  ER-0033                 PIC X(4)  VALUE '0033'.
00057      12  ER-0034                 PIC X(4)  VALUE '0034'.
00058      12  ER-0130                 PIC X(4)  VALUE '0130'.
00059      12  ER-0131                 PIC X(4)  VALUE '0131'.
00060      12  ER-0142                 PIC X(4)  VALUE '0142'.
00061      12  ER-0234                 PIC X(4)  VALUE '0234'.
00062      12  ER-0235                 PIC X(4)  VALUE '0235'.
CIDMOD     12  ER-1162                 PIC X(4)  VALUE '1162'.
00063      12  ER-1164                 PIC X(4)  VALUE '1164'.
00064      12  ER-2237                 PIC X(4)  VALUE '2237'.
00065      12  ER-2238                 PIC X(4)  VALUE '2238'.
00066      12  ER-5004                 PIC X(4)  VALUE '5004'.
00067      12  ER-5005                 PIC X(4)  VALUE '5005'.
00068      12  ER-6508                 PIC X(4)  VALUE '6508'.
00069
00070      EJECT
00071
00072  01  STANDARD-AREAS.
00073      12  QID.
00074          16  QID-TERM        PIC X(4).
00075          16  FILLER          PIC X(4)    VALUE '658A'.
00076      12  RETURNED-FROM       PIC X(8)    VALUE SPACES.
00077      12  QID-MAP-LENGTH      PIC S9(4)   VALUE +1376   COMP.
00078      12  MAP-NAME                PIC X(8)    VALUE 'EL658A'.
00079      12  MAPSET-NAME             PIC X(8)    VALUE 'EL658S'.
00080      12  TRANS-ID                PIC X(4)    VALUE 'EXJ3'.
00081      12  EL642-TRANS-ID          PIC X(4)    VALUE 'EXH7'.
00082      12  EL650-TRANS-ID          PIC X(4)    VALUE 'EXC4'.
00083      12  PGM-NAME                PIC X(8)    VALUE SPACES.
00084      12  THIS-PGM                PIC X(8)    VALUE 'EL658'.
00085      12  XCTL-642                PIC X(8)    VALUE 'EL642'.
00086      12  XCTL-650                PIC X(8)    VALUE 'EL650'.
00087      12  GXRF-FILE-ID            PIC X(8)    VALUE 'ERGXRF'.
00088      12  CNTL-FILE-ID            PIC X(8)    VALUE 'ELCNTL'.
00089      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00090      12  LINK-001                PIC X(8)    VALUE 'EL001'.
00091      12  LINK-004                PIC X(8)    VALUE 'EL004'.
00092      12  XCTL-005                PIC X(8)    VALUE 'EL005'.
00093      12  XCTL-010                PIC X(8)    VALUE 'EL010'.
00094      12  XCTL-626                PIC X(8)    VALUE 'EL626'.
00095      12  TIME-IN                 PIC S9(7).
00096      12  TIME-OUT-R   REDEFINES TIME-IN.
00097          16  FILLER              PIC X.
00098          16  TIME-OUT            PIC 99V99.
00099          16  FILLER              PIC X(2).
00100      12  BROWSE-STARTED-SW       PIC X       VALUE ' '.
00101          88  BROWSE-STARTED      VALUE 'Y'.
00102
00103  01  WS-RECORD-LENGTH COMP       SYNCHRONIZED.
00104      12  GXRF-MAX-REC-LENGTH     PIC S9(8)    VALUE +32062.
00105      12  GXRF-REC-LENGTH         PIC S9(4)    VALUE +0.
00106
00107
00108  01  BLD-LINE.
00109      12  BL-ACCOUNT          PIC X(10).
00110      12  FILLER              PIC X         VALUE SPACE.
00111      12  BL-EFFECT           PIC X(8).
00112      12  FILLER              PIC X(3)      VALUE SPACE.
00113      12  BL-EXPIRE           PIC X(8).
00114      12  FILLER              PIC X(3)      VALUE SPACE.
00115      12  BL-LEVEL            PIC ZZ.
00116      12  FILLER              PIC X(5)      VALUE SPACE.
00117      12  BL-LAST-BILL        PIC X(8).
00118      12  FILLER              PIC X(6)      VALUE SPACE.
00119      12  BL-OCCURRENCE       PIC ZZ,ZZZ.
00120      12  FILLER              PIC X(6)      VALUE SPACE.
00121      12  BL-STATE            PIC XX.
00122
00123      EJECT
00124 *                            COPY ELCDATE.
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
00125      EJECT
00126 *                                   COPY ELCEMIB SUPPRESS.
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
00127
00128 *                  COPY ELCLOGOF SUPPRESS.
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
00129
00130 *                                    COPY ELCINTF.
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
00131      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00132          16  PI-ERGXRF-KEY.
00133              20  PI-GXRF-COMP-CD     PIC X.
00134              20  PI-GXRF-CARRIER     PIC X.
00135              20  PI-GXRF-GROUPING    PIC X(6).
00136              20  PI-GXRF-AGENT       PIC X(10).
00137          16  PI-NDX                  PIC S9(5)  COMP-3.
00138          16  PI-EOF-SW               PIC X.
00139              88  PI-FILE-EOF             VALUE 'Y'.
00140          16  FILLER                  PIC X(618).
00141      EJECT
00142 *                            COPY ELCAID SUPPRESS.
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
00143  01  FILLER    REDEFINES DFHAID.
00144      12  FILLER              PIC X(8).
00145      12  PF-VALUES           PIC X       OCCURS 24.
00146
00147 *                            COPY EL658S.
       01  EL658AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
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
           05  AGENTL PIC S9(0004) COMP.
           05  AGENTF PIC  X(0001).
           05  FILLER REDEFINES AGENTF.
               10  AGENTA PIC  X(0001).
           05  AGENTI PIC  X(0010).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0008).
      *    -------------------------------
           05  TOTOPNL PIC S9(0004) COMP.
           05  TOTOPNF PIC  X(0001).
           05  FILLER REDEFINES TOTOPNF.
               10  TOTOPNA PIC  X(0001).
           05  TOTOPNI PIC  X(0005).
      *    -------------------------------
           05  TOTOCCL PIC S9(0004) COMP.
           05  TOTOCCF PIC  X(0001).
           05  FILLER REDEFINES TOTOCCF.
               10  TOTOCCA PIC  X(0001).
           05  TOTOCCI PIC  X(0005).
           05  XRFLINED OCCURS 15  TIMES.
      *    -------------------------------
               10  XRFLINEL PIC S9(0004) COMP.
               10  XRFLINEF PIC  X(0001).
               10  FILLER REDEFINES XRFLINEF.
                   15  XRFLINEA PIC  X(0001).
               10  XRFLINEI PIC  X(0079).
      *    -------------------------------
           05  ACCOCCL PIC S9(0004) COMP.
           05  ACCOCCF PIC  X(0001).
           05  FILLER REDEFINES ACCOCCF.
               10  ACCOCCA PIC  X(0001).
           05  ACCOCCI PIC  9(4).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0076).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL658AO REDEFINES EL658AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTOPNO PIC  Z,ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTOCCO PIC  Z,ZZ9.
      *    -------------------------------
           05  XRFLINED OCCURS 15  TIMES.
               10  FILLER        PIC  X(0003).
               10  XRFLINEO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCOCCO PIC  Z(4).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00148      EJECT
00149
00150 *                            COPY ERCGXRF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCGXRF                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = GENERAL AGENT CROSS REFERENCE             *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 62 - 32,062   RECFORM = VARIABLE
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERGXRF                   RKP=2,LEN=18    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
00021
00022  01  AGENT-CROSS-REFERENCE.
00023      12  GX-RECORD-ID                PIC XX.
00024          88  VALID-GX-ID             VALUE 'GX'.
00025
00026      12  GX-CONTROL-PRIMARY.
00027          16  GX-COMPANY-CD           PIC X.
00028          16  GX-CARRIER              PIC X.
00029          16  GX-GROUPING             PIC X(6).
00030          16  GX-AGENT-NO             PIC X(10).
00031
00032      12  GX-MAINT-INFORMATION.
00033          16  GX-LAST-MAINT-DT        PIC XX.
00034          16  GX-LAST-MAINT-HHMMSS    PIC S9(7)  COMP-3.
00035          16  GX-LAST-MAINT-USER      PIC X(4).
00036          16  FILLER                  PIC X(9).
00037
00038      12  FILLER                      PIC X(37).
00039
00040      12  GX-AGENT-POINTER-CNT        PIC S9(4)  COMP.
00041
00042      12  GX-AGENT-POINTER   OCCURS 1 TO 1006 TIMES
00043                             DEPENDING ON GX-AGENT-POINTER-CNT.
00044          16  GX-AM-CARRIER           PIC X.
00045          16  GX-AM-GROUPING          PIC X(6).
00046          16  GX-AM-STATE             PIC XX.
00047          16  GX-AM-ACCOUNT           PIC X(10).
00048          16  GX-AM-EXPIRATION-DT     PIC XX.
00049          16  GX-AM-LEVEL-NO          PIC S9(4)     COMP.
00050          16  GX-LAST-BILL-DT         PIC XX.
00051          16  GX-AM-EFF-DT            PIC XX.
00052          16  FILLER                  PIC X(4).
00053
00054 ******************************************************************
00151      EJECT
00152
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
00154  01  DFHCOMMAREA             PIC X(1024).
00155      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL658' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00157      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00158
00159      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00160      MOVE '5'                   TO DC-OPTION-CODE.
00161      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00162      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00163      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00164
00165      MOVE EIBTRMID              TO QID-TERM.
00166
00167      
      * EXEC CICS HANDLE CONDITION
00168 *        ERROR     (9990-ABEND)
00169 *        MAPFAIL   (8100-SEND-INITIAL-MAP)
00170 *        END-EXEC.
      *    MOVE '"$.?                  ! " #00001092' TO DFHEIV0
           MOVE X'22242E3F2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031303932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00171
00172      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00173          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00174              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00175              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00176              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00177              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00178              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00179              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00180              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00181              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00182          ELSE
00183              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00184              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00185              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00186              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00187              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00188              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00189              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00190              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00191
00192      EJECT
00193      IF EIBTRNID = TRANS-ID
00194          IF EIBAID = DFHCLEAR
00195              GO TO 9400-CLEAR
00196          ELSE
00197              GO TO 0200-RECEIVE-MAP.
00198
00199      IF EIBTRNID  = EL642-TRANS-ID
00200          MOVE DFHENTER       TO EIBAID
00201          MOVE PI-CR-CARRIER  TO CARRIERI
00202          MOVE PI-CR-GROUPING TO GROUPI
00203          MOVE PI-CR-FIN-RESP TO AGENTI
00204          MOVE 1              TO CARRIERL
00205          MOVE 6              TO GROUPL
00206          MOVE 10             TO AGENTL
00207          MOVE AL-UANON       TO CARRIERA GROUPA AGENTA
00208          GO TO 1000-EDIT-INPUT.
00209
00210      MOVE LOW-VALUES       TO PI-ERGXRF-KEY    EL658AI.
00211      MOVE PI-COMPANY-CD    TO PI-GXRF-COMP-CD  WS-COMPANY-CD.
00212
00213      IF EIBTRNID  = EL650-TRANS-ID
00214          GO TO 0600-RECOVER-TEMP-STORAGE.
00215
00216      GO TO 8100-SEND-INITIAL-MAP.
00217
00218      EJECT
00219
00220  0200-RECEIVE-MAP.
00221      MOVE LOW-VALUES TO EL658AI.
00222
00223      
      * EXEC CICS RECEIVE
00224 *        MAP (MAP-NAME)
00225 *        MAPSET (MAPSET-NAME)
00226 *        INTO (EL658AI)
00227 *        END-EXEC.
           MOVE LENGTH OF
            EL658AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001148' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL658AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00228
00229      IF PFENTERL = 0
00230          GO TO 0300-CHECK-PFKEYS.
00231      IF EIBAID NOT = DFHENTER
00232          MOVE ER-0004 TO EMI-ERROR
00233          GO TO 0320-INPUT-ERROR.
00234      IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)
00235          MOVE PF-VALUES (PFENTERI) TO EIBAID
00236      ELSE
00237          MOVE ER-0029 TO EMI-ERROR
00238          GO TO 0320-INPUT-ERROR.
00239
00240  0300-CHECK-PFKEYS.
00241      IF EIBAID = DFHPF1 OR DFHPF2
00242          GO TO 5000-BROWSE-FILE.
00243
00244      IF EIBAID = DFHPF3 OR DFHPF4
00245          GO TO 6000-OCCURRENCE.
00246
00247      IF EIBAID = DFHPF5
00248          GO TO 4000-ACCT-MAINT.
00249
00250      IF EIBAID = DFHENTER
00251          GO TO 1000-EDIT-INPUT.
00252
00253      IF EIBAID = DFHPF12
00254          GO TO 9500-PF12.
00255
00256      IF EIBAID = DFHPF23
00257          GO TO 8810-PF23.
00258
00259      IF EIBAID = DFHPF24
00260          GO TO 9200-RETURN-MAIN-MENU.
00261
00262      MOVE ER-0029                TO EMI-ERROR.
00263  0320-INPUT-ERROR.
00264      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00265      MOVE -1                     TO CARRIERL.
00266      GO TO 8200-SEND-DATAONLY.
00267      EJECT
00268  0500-CREATE-TEMP-STORAGE.
00269
00270      
      * EXEC CICS WRITEQ TS
00271 *        QUEUE   (QID)
00272 *        FROM    (EL658AI)
00273 *        LENGTH  (QID-MAP-LENGTH)
00274 *        END-EXEC.
      *    MOVE '*"                    ''   #00001195' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL658AI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00275
00276  0599-EXIT.
00277       EXIT.
00278
00279  0600-RECOVER-TEMP-STORAGE.
00280
00281      
      * EXEC CICS HANDLE CONDITION
00282 *        NOTFND  (1500-GXRF-NOT-FOUND)
00283 *        QIDERR  (0690-QIDERR)
00284 *        END-EXEC.
      *    MOVE '"$IN                  ! # #00001206' TO DFHEIV0
           MOVE X'2224494E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031323036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00285
00286      
      * EXEC CICS READQ TS
00287 *        QUEUE    (QID)
00288 *        INTO     (EL658AI)
00289 *        LENGTH   (QID-MAP-LENGTH)
00290 *        END-EXEC.
      *    MOVE '*$I    L              ''   #00001211' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL658AI, 
                 QID-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00291
00292      
      * EXEC CICS DELETEQ TS
00293 *        QUEUE   (QID)
00294 *        END-EXEC.
      *    MOVE '*&                    #   #00001217' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00295
00296      IF CARRIERL NOT = 0
00297          MOVE AL-UANON TO CARRIERA.
00298
00299      IF GROUPL NOT = 0
00300          MOVE AL-UANON TO GROUPA.
00301
00302      IF AGENTL NOT = 0
00303          MOVE AL-UANON TO AGENTA.
00304
00305      MOVE PI-COMPANY-CD          TO WS-COMPANY-CD.
00306      MOVE CARRIERO               TO WS-CARRIER.
00307      MOVE GROUPO                 TO WS-GROUPING.
00308      MOVE AGENTO                 TO WS-AGENT.
00309
00310      GO TO 1050-READ-GA.
00311
00312  0690-QIDERR.
00313      MOVE ER-0033 TO EMI-ERROR.
00314      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00315      GO TO 8100-SEND-INITIAL-MAP.
00316
00317      EJECT
00318
00319  1000-EDIT-INPUT.
00320      IF CARRIERL NOT = ZEROS
00321          MOVE CARRIERI           TO WS-CARRIER
00322        ELSE
00323          MOVE ER-0234            TO EMI-ERROR
00324          MOVE -1                 TO CARRIERL
00325          MOVE AL-UABON           TO CARRIERA
00326          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00327
00328      IF GROUPL NOT = ZEROS
00329          MOVE GROUPI             TO WS-GROUPING
00330        ELSE
00331          MOVE ER-0235            TO EMI-ERROR
00332          MOVE -1                 TO GROUPL
00333          MOVE AL-UABON           TO GROUPA
00334          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00335
00336      IF AGENTL NOT = ZEROS
00337          MOVE AGENTI             TO WS-AGENT
00338        ELSE
00339          MOVE ER-6508            TO EMI-ERROR
00340          MOVE -1                 TO AGENTL
00341          MOVE AL-UABON           TO AGENTA
00342          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00343
00344      IF NOT EMI-NO-ERRORS
00345          GO TO 8200-SEND-DATAONLY.
00346
00347      MOVE PI-COMPANY-CD   TO WS-COMPANY-CD.
00348
00349      EJECT
00350  1050-READ-GA.
00351      
      * EXEC CICS HANDLE CONDITION
00352 *        NOTFND (1500-GXRF-NOT-FOUND) END-EXEC.
      *    MOVE '"$I                   ! $ #00001276' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031323736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00353
00354      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.
00355
00356      
      * EXEC CICS READ
00357 *         INTO    (AGENT-CROSS-REFERENCE)
00358 *         DATASET ('ERGXRF')
00359 *         LENGTH  (GXRF-REC-LENGTH)
00360 *         RIDFLD  (WS-CONTROL-PRIMARY)
00361 *         END-EXEC.
           MOVE 'ERGXRF' TO DFHEIV1
      *    MOVE '&"IL       E          (   #00001281' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 AGENT-CROSS-REFERENCE, 
                 GXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00362
00363
00364      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
00365
00366      MOVE +1 TO PI-NDX.
00367      GO TO 5200-FORMAT-SCREEN.
00368
00369  1500-GXRF-NOT-FOUND.
00370      MOVE ER-0142            TO EMI-ERROR.
00371      MOVE -1                 TO CARRIERL.
00372      MOVE AL-UABON           TO CARRIERA.
00373      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00374      IF EIBTRNID  = EL642-TRANS-ID
00375          GO TO 8100-SEND-INITIAL-MAP
00376      ELSE
00377          GO TO 8200-SEND-DATAONLY.
00378
00379      EJECT
00380
00381  3000-BLD-LINE.
00382      MOVE 1  TO NDX.
00383      MOVE SPACES TO XRFLINEO (1) XRFLINEO (2) XRFLINEO (3)
00384                     XRFLINEO (4) XRFLINEO (5) XRFLINEO (6)
00385                     XRFLINEO (7) XRFLINEO (8) XRFLINEO (9)
00386                     XRFLINEO (10) XRFLINEO (11) XRFLINEO (12)
00387                     XRFLINEO (13) XRFLINEO (14) XRFLINEO (15)
00388                                   BLD-LINE.
00389  3000-BLD-LINE-LOOP.
00390      IF PI-NDX GREATER THAN GX-AGENT-POINTER-CNT
00391           GO TO 3000-XIT.
00392
00393      MOVE GX-AM-ACCOUNT (PI-NDX) TO BL-ACCOUNT.
00394
00395      IF GX-AM-EFF-DT (PI-NDX) NOT = SPACES AND LOW-VALUES
00396          MOVE GX-AM-EFF-DT (PI-NDX) TO DC-BIN-DATE-1
00397          MOVE SPACE                        TO DC-OPTION-CODE
00398          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00399          MOVE DC-GREG-DATE-1-EDIT       TO BL-EFFECT.
00400
00401      IF GX-AM-EXPIRATION-DT (PI-NDX) NOT = SPACES AND LOW-VALUES
00402          MOVE GX-AM-EXPIRATION-DT (PI-NDX) TO DC-BIN-DATE-1
00403          MOVE SPACE                        TO DC-OPTION-CODE
00404          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00405          MOVE DC-GREG-DATE-1-EDIT       TO BL-EXPIRE.
00406
00407      IF GX-AM-EXPIRATION-DT (PI-NDX) = HIGH-VALUES
00408          MOVE '99/99/99'                TO BL-EXPIRE.
00409
00410      MOVE GX-AM-LEVEL-NO (PI-NDX)       TO BL-LEVEL.
00411
00412      IF GX-LAST-BILL-DT (PI-NDX) NOT = SPACES AND LOW-VALUES
00413          MOVE GX-LAST-BILL-DT (PI-NDX) TO DC-BIN-DATE-1
00414          MOVE SPACE                 TO DC-OPTION-CODE
00415          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00416          MOVE DC-GREG-DATE-1-EDIT   TO BL-LAST-BILL.
00417
00418      MOVE PI-NDX                    TO BL-OCCURRENCE.
00419      MOVE GX-AM-STATE (PI-NDX)      TO BL-STATE.
00420
00421      MOVE BLD-LINE                  TO XRFLINEO (NDX).
00422      MOVE SPACES                    TO BLD-LINE.
00423
00424      IF (PI-NDX = GX-AGENT-POINTER-CNT)  OR  (NDX = 15)
00425           GO TO 3000-XIT.
00426      ADD  1 TO PI-NDX NDX.
00427      GO TO 3000-BLD-LINE-LOOP.
00428
00429  3000-XIT.
00430      EXIT.
00431      EJECT
00432  4000-ACCT-MAINT.
00433      IF CARRIERI NOT = PI-GXRF-CARRIER   OR
00434         GROUPI   NOT = PI-GXRF-GROUPING  OR
00435         AGENTI   NOT = PI-GXRF-AGENT
00436            MOVE ER-5005 TO EMI-ERROR
00437            GO TO 4050-ERR.
00438
00439      IF ACCOCCI NOT NUMERIC
00440         GO TO 4010-ERROR.
00441
00442      MOVE PI-COMPANY-CD      TO WS-COMPANY-CD.
00443      MOVE CARRIERI           TO WS-CARRIER.
00444      MOVE GROUPI             TO WS-GROUPING.
00445      MOVE AGENTI             TO WS-AGENT.
00446
00447      
      * EXEC CICS HANDLE CONDITION
00448 *        NOTFND (4010-ERROR) END-EXEC.
      *    MOVE '"$I                   ! % #00001372' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031333732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00449
00450
00451      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.
00452
00453      
      * EXEC CICS READ
00454 *         DATASET ('ERGXRF')
00455 *         INTO    (AGENT-CROSS-REFERENCE)
00456 *         LENGTH  (GXRF-REC-LENGTH)
00457 *         RIDFLD  (WS-CONTROL-PRIMARY)
00458 *         END-EXEC.
           MOVE 'ERGXRF' TO DFHEIV1
      *    MOVE '&"IL       E          (   #00001378' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 AGENT-CROSS-REFERENCE, 
                 GXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00459
00460      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
00461
00462      MOVE ACCOCCI TO NDX.
00463      IF NDX  NOT GREATER GX-AGENT-POINTER-CNT
00464          MOVE GX-AM-CARRIER  (NDX)   TO PI-CR-CARRIER
00465          MOVE GX-AM-GROUPING (NDX)   TO PI-CR-GROUPING
00466          MOVE GX-AM-STATE    (NDX)   TO PI-CR-STATE
00467          MOVE GX-AM-ACCOUNT  (NDX)   TO PI-CR-ACCOUNT
00468          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
00469          MOVE XCTL-650      TO PGM-NAME
00470          GO TO 9300-XCTL.
00471
00472  4010-ERROR.
00473       MOVE ER-5004 TO EMI-ERROR.
00474  4050-ERR.
00475       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00476       MOVE -1                     TO ACCOCCL.
00477       MOVE AL-UNBON               TO ACCOCCA.
00478       GO TO 8200-SEND-DATAONLY.
00479
00480      EJECT
00481  5000-BROWSE-FILE.
00482      MOVE SPACE TO BROWSE-STARTED-SW.
00483      
      * EXEC CICS HANDLE CONDITION
00484 *        NOTFND (5800-NO-RECORD)
00485 *        ENDFILE (5900-END-OF-FILE)
00486 *        END-EXEC.
      *    MOVE '"$I''                  ! & #00001408' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303031343038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00487
00488      MOVE PI-ERGXRF-KEY TO WS-CONTROL-PRIMARY.
00489
00490      
      * EXEC CICS STARTBR
00491 *        DATASET (GXRF-FILE-ID)
00492 *        RIDFLD (WS-CONTROL-PRIMARY)
00493 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001415' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 GXRF-FILE-ID, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00494
00495      MOVE 'Y' TO BROWSE-STARTED-SW.
00496
00497      IF EIBAID = DFHPF2
00498          GO TO 5100-BROWSE-BKWD.
00499      EJECT
00500  5010-READ-LOOP.
00501
00502      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.
00503
00504      
      * EXEC CICS READNEXT
00505 *        DATASET (GXRF-FILE-ID)
00506 *        INTO    (AGENT-CROSS-REFERENCE)
00507 *        RIDFLD  (WS-CONTROL-PRIMARY)
00508 *        LENGTH  (GXRF-REC-LENGTH)
00509 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00001429' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 GXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 GXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00510
00511
00512      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
00513
00514      MOVE +1 TO PI-NDX.
00515
00516      IF GX-COMPANY-CD NOT = PI-COMPANY-CD
00517          IF EIBAID = DFHENTER
00518              GO TO 5800-NO-RECORD
00519          ELSE
00520              GO TO 5900-END-OF-FILE.
00521
00522      IF EIBAID = DFHENTER
00523         IF WS-CARRIER    = GX-CARRIER     AND
00524            WS-GROUPING   = GX-GROUPING    AND
00525            WS-AGENT      = GX-AGENT-NO
00526               GO TO 5200-FORMAT-SCREEN
00527            ELSE
00528               GO TO 5800-NO-RECORD.
00529
00530      IF EIBAID = DFHPF1
00531        IF CARRIERO = GX-CARRIER  AND
00532           GROUPO   = GX-GROUPING AND
00533           AGENTO   = GX-AGENT-NO
00534          GO TO 5010-READ-LOOP.
00535
00536      GO TO 5200-FORMAT-SCREEN.
00537      EJECT
00538  5100-BROWSE-BKWD.
00539      
      * EXEC CICS HANDLE CONDITION
00540 *        NOTFND (5900-END-OF-FILE)
00541 *        END-EXEC.
      *    MOVE '"$I                   ! '' #00001464' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303031343634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00542
00543      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.
00544
00545      
      * EXEC CICS READPREV
00546 *        DATASET (GXRF-FILE-ID)
00547 *        INTO    (AGENT-CROSS-REFERENCE)
00548 *        LENGTH  (GXRF-REC-LENGTH)
00549 *        RIDFLD  (WS-CONTROL-PRIMARY)
00550 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0IL                  )   #00001470' TO DFHEIV0
           MOVE X'2630494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 GXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 GXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00551
00552      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.
00553
00554      IF PI-FILE-EOF
00555          MOVE SPACE    TO PI-EOF-SW
00556      ELSE
00557          
      * EXEC CICS READPREV
00558 *            DATASET (GXRF-FILE-ID)
00559 *            INTO    (AGENT-CROSS-REFERENCE)
00560 *            LENGTH  (GXRF-REC-LENGTH)
00561 *            RIDFLD  (WS-CONTROL-PRIMARY)
00562 *            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0IL                  )   #00001482' TO DFHEIV0
           MOVE X'2630494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 GXRF-FILE-ID, 
                 AGENT-CROSS-REFERENCE, 
                 GXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00563
00564
00565      MOVE GX-AGENT-POINTER-CNT TO GX-AGENT-POINTER-CNT.
00566
00567      MOVE +1 TO PI-NDX.
00568
00569      IF GX-COMPANY-CD NOT = PI-COMPANY-CD
00570          GO TO 5900-END-OF-FILE.
00571
00572      GO TO 5200-FORMAT-SCREEN.
00573
00574      EJECT
00575  5200-FORMAT-SCREEN.
00576      MOVE LOW-VALUES             TO EL658AI.
00577
00578      MOVE GX-CONTROL-PRIMARY     TO PI-ERGXRF-KEY.
00579
00580      MOVE GX-CARRIER             TO CARRIERO.
00581
00582      MOVE GX-GROUPING            TO GROUPO.
00583
00584      MOVE GX-AGENT-NO            TO AGENTO.
00585
00586      MOVE GX-AGENT-POINTER-CNT   TO TOTOCCO.
00587
00588      PERFORM 7500-COUNT-OPEN-RANGES  THRU  7500-EXIT.
00589
00590      MOVE WS-OPEN-COUNT          TO TOTOPNO.
00591
00592      IF GX-LAST-MAINT-DT NOT = SPACES AND LOW-VALUES AND ZEROS
00593          MOVE GX-LAST-MAINT-DT          TO DC-BIN-DATE-1
00594          MOVE SPACE                     TO DC-OPTION-CODE
00595          PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00596          MOVE DC-GREG-DATE-1-EDIT       TO MAINTO
00597       ELSE
00598          MOVE '00/00/00'                TO MAINTO.
00599
00600      PERFORM 3000-BLD-LINE THRU 3000-XIT.
00601
00602      MOVE AL-UANON               TO CARRIERA GROUPA
00603                                     AGENTA.
00604      GO TO 8100-SEND-INITIAL-MAP.
00605      EJECT
00606  5800-NO-RECORD.
00607      MOVE -1                     TO CARRIERL.
00608      MOVE AL-UANON               TO CARRIERA   GROUPA
00609                                      AGENTA.
00610      MOVE ER-1164                TO EMI-ERROR.
00611      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00612      IF BROWSE-STARTED
00613        
      * EXEC CICS ENDBR
00614 *           DATASET  (GXRF-FILE-ID)
00615 *           END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001538' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 GXRF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00616
00617      GO TO 8200-SEND-DATAONLY.
00618
00619  5900-END-OF-FILE.
00620      IF EIBAID = DFHPF1
00621          MOVE 'Y'                TO PI-EOF-SW
00622          MOVE ER-2237            TO EMI-ERROR
00623      ELSE
00624          MOVE LOW-VALUES         TO PI-ERGXRF-KEY
00625          MOVE PI-COMPANY-CD      TO PI-GXRF-COMP-CD
00626          MOVE ER-2238            TO EMI-ERROR.
00627
00628      MOVE -1            TO CARRIERL.
00629      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00630      IF BROWSE-STARTED
00631        
      * EXEC CICS ENDBR
00632 *           DATASET  (GXRF-FILE-ID)
00633 *           END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001556' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 GXRF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00634
00635      MOVE SPACE TO BROWSE-STARTED-SW.
00636      GO TO 8200-SEND-DATAONLY.
00637      EJECT
00638  6000-OCCURRENCE.
00639      MOVE PI-ERGXRF-KEY TO WS-CONTROL-PRIMARY.
00640
00641      
      * EXEC CICS HANDLE CONDITION
00642 *        NOTFND (1500-GXRF-NOT-FOUND) END-EXEC.
      *    MOVE '"$I                   ! ( #00001566' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303031353636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00643
00644      MOVE GXRF-MAX-REC-LENGTH    TO GXRF-REC-LENGTH.
00645
00646      
      * EXEC CICS READ
00647 *         DATASET ('ERGXRF')
00648 *         INTO    (AGENT-CROSS-REFERENCE)
00649 *         LENGTH  (GXRF-REC-LENGTH)
00650 *         RIDFLD  (WS-CONTROL-PRIMARY)
00651 *         END-EXEC.
           MOVE 'ERGXRF' TO DFHEIV1
      *    MOVE '&"IL       E          (   #00001571' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 AGENT-CROSS-REFERENCE, 
                 GXRF-REC-LENGTH, 
                 WS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00652
00653
00654      MOVE GX-AGENT-POINTER-CNT   TO GX-AGENT-POINTER-CNT.
00655
00656      IF EIBAID = DFHPF3
00657         GO TO 6500-NEXT-OCC
00658       ELSE
00659         GO TO 7000-PRIOR-OCC.
00660
00661  6500-NEXT-OCC.
00662      IF PI-NDX = GX-AGENT-POINTER-CNT
00663          MOVE -1            TO CARRIERL
00664          MOVE ER-0130       TO EMI-ERROR
00665          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00666          GO TO 8200-SEND-DATAONLY.
00667
00668      ADD +1 TO PI-NDX.
00669
00670      PERFORM 3000-BLD-LINE THRU 3000-XIT.
00671
00672      MOVE -1      TO CARRIERL.
00673      GO TO 8200-SEND-DATAONLY.
00674
00675  7000-PRIOR-OCC.
00676      IF PI-NDX LESS 16
00677          MOVE -1            TO CARRIERL
00678          MOVE ER-0131       TO EMI-ERROR
00679          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00680          GO TO 8200-SEND-DATAONLY.
00681
00682      COMPUTE PI-NDX = (PI-NDX - 1) / 15.
00683      IF PI-NDX NOT = 1
00684         COMPUTE PI-NDX = PI-NDX * 15
00685         COMPUTE PI-NDX = PI-NDX - 14.
00686
00687      PERFORM 3000-BLD-LINE THRU 3000-XIT.
00688
00689      MOVE -1      TO CARRIERL.
00690      GO TO 8200-SEND-DATAONLY.
00691
00692      EJECT
00693
00694  7500-COUNT-OPEN-RANGES.
00695
00696      MOVE +1                     TO PI-NDX.
00697      MOVE +0                     TO WS-OPEN-COUNT.
00698
00699  7500-LOOP.
00700
00701      IF PI-NDX GREATER THAN GX-AGENT-POINTER-CNT
00702           MOVE +1                TO PI-NDX
00703           GO TO 7500-EXIT.
00704
00705      IF GX-AM-EXPIRATION-DT (PI-NDX) = HIGH-VALUES
00706          ADD +1 TO WS-OPEN-COUNT.
00707
00708      ADD +1 TO PI-NDX.
00709
00710      GO TO 7500-LOOP.
00711
00712  7500-EXIT.
00713      EXIT.
00714
00715      EJECT
00716  8100-SEND-INITIAL-MAP.
00717      MOVE SAVE-DATE              TO DATEO.
00718      MOVE EIBTIME                TO TIME-IN.
00719      MOVE TIME-OUT               TO TIMEO.
00720      MOVE -1                     TO CARRIERL.
00721      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
00722
00723      
      * EXEC CICS SEND
00724 *        MAP    (MAP-NAME)
00725 *        MAPSET (MAPSET-NAME)
00726 *        FROM   (EL658AO)
00727 *        ERASE
00728 *        CURSOR
00729 *        END-EXEC.
           MOVE LENGTH OF
            EL658AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00001648' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL658AO, 
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
           
00730      GO TO 9100-RETURN-TRAN.
00731
00732  8200-SEND-DATAONLY.
00733      MOVE SAVE-DATE              TO DATEO.
00734      MOVE EIBTIME                TO TIME-IN.
00735      MOVE TIME-OUT               TO TIMEO.
00736      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
00737      
      * EXEC CICS SEND
00738 *        MAP    (MAP-NAME)
00739 *        MAPSET (MAPSET-NAME)
00740 *        FROM   (EL658AO)
00741 *        DATAONLY
00742 *        CURSOR
00743 *        END-EXEC.
           MOVE LENGTH OF
            EL658AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00001662' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL658AO, 
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
           
00744      GO TO 9100-RETURN-TRAN.
00745
00746      EJECT
00747  8500-DATE-CONVERT.
00748      MOVE LINK-ELDATCV           TO PGM-NAME.
00749      
      * EXEC CICS LINK
00750 *        PROGRAM    (PGM-NAME)
00751 *        COMMAREA   (DATE-CONVERSION-DATA)
00752 *        LENGTH     (DC-COMM-LENGTH)
00753 *        END-EXEC.
      *    MOVE '."C                   ''   #00001674' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00754  8500-EXIT.
00755      EXIT.
00756
00757  8810-PF23.
00758      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00759      MOVE XCTL-005               TO PGM-NAME.
00760      GO TO 9300-XCTL.
00761
00762  9100-RETURN-TRAN.
00763      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
00764      MOVE '658A'               TO PI-CURRENT-SCREEN-NO.
00765      
      * EXEC CICS RETURN
00766 *        TRANSID(TRANS-ID)
00767 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
00768 *        LENGTH(PI-COMM-LENGTH)
00769 *        END-EXEC.
      *    MOVE '.(CT                  &   #00001690' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00770
00771      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL658' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00772
00773  9200-RETURN-MAIN-MENU.
00774      MOVE XCTL-626               TO PGM-NAME.
00775      GO TO 9300-XCTL.
00776
00777      EJECT
00778
00779  9300-XCTL.
00780      
      * EXEC CICS XCTL
00781 *        PROGRAM    (PGM-NAME)
00782 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00783 *        LENGTH     (PI-COMM-LENGTH)
00784 *        END-EXEC.
      *    MOVE '.$C                   $   #00001705' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00785
00786  9400-CLEAR.
00787      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
00788      GO TO 9300-XCTL.
00789
00790  9500-PF12.
00791      MOVE XCTL-010               TO PGM-NAME.
00792      GO TO 9300-XCTL.
00793
00794  9900-ERROR-FORMAT.
00795      IF NOT EMI-ERRORS-COMPLETE
00796          MOVE LINK-001 TO PGM-NAME
00797          
      * EXEC CICS LINK
00798 *            PROGRAM(PGM-NAME)
00799 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
00800 *            LENGTH(EMI-COMM-LENGTH)
00801 *            END-EXEC.
      *    MOVE '."C                   ''   #00001722' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00802  9900-EXIT.
00803      EXIT.
00804
00805  9990-ABEND.
00806      MOVE LINK-004 TO PGM-NAME.
00807      MOVE DFHEIBLK TO EMI-LINE1.
00808      
      * EXEC CICS LINK
00809 *        PROGRAM   (PGM-NAME)
00810 *        COMMAREA  (EMI-LINE1)
00811 *        LENGTH    (72)
00812 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001733' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00813      GO TO 8200-SEND-DATAONLY.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL658' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     8100-SEND-INITIAL-MAP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1500-GXRF-NOT-FOUND,
                     0690-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1500-GXRF-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 4010-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 5800-NO-RECORD,
                     5900-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 5900-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1500-GXRF-NOT-FOUND
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL658' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
