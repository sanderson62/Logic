00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL103 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 11/28/95 11:14:54.
00007 *                            VMOD=2.021
00008 *
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
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *
00025 *REMARKS.
00026 *        TRANSACTION - EX09 - USER RECORD MAINTENANCE
031808******************************************************************
031808*                   C H A N G E   L O G
031808*
031808* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031808*-----------------------------------------------------------------
031808*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031808* EFFECTIVE    NUMBER
031808*-----------------------------------------------------------------
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
011812* 011812    2011022800001  AJRA  ADD CSR IND
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
031808******************************************************************
00027
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030      EJECT
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC  X(32) VALUE '********************************'.
00033  77  FILLER  PIC  X(32) VALUE '*    EL103 WORKING STORAGE     *'.
00034  77  FILLER  PIC  X(32) VALUE '********* VMOD=2.021 ***********'.
00035
00036  01  WS-MAP-AREAS.
00037      12  WS-QCDAYS                  PIC X(35)
00038          VALUE 'DIFFERENCE IN QUOTED/CALC.   DAYS :'.
00039      12  WS-AH                      PIC X(03)
00040          VALUE 'A/H'.
00041      12  WS-LIFE                    PIC X(04)
00042          VALUE 'LIFE'.
00043      12  WS-MAXDAYS                 PIC X(35)
00044          VALUE 'MAXIMUM REGULAR PAYMENT      DAYS :'.
00045      12  WS-AUTO-PAY                PIC X(35)
00046          VALUE 'MAXIMUM AUTO. PAYMENT TERM   MTHS :'.
00047      12  WS-AMOUNT                  PIC X(05)
00048          VALUE 'AMT :'.
00049      12  WS-EXPENSE                 PIC X(35)
00050          VALUE 'MAXIMUM EXPENSE PAYMENT            '.
00051
00052  01  WS-SAVE-PROCESSOR-RCDS.
00053      12  SAVE-PROCESSOR-ZERO        PIC X(750).
00054      12  SAVE-PROCESSOR-ONE         PIC X(750).
00055
00056  01  WS-PASSWORD-NUMERIC            PIC X.
00057  01  WS-PASSWORD-SIXDIGIT           PIC X.
00058
00059  01  WS-SAVE-PASSWORD.
00060      12  WS-PASSWORD-X1             PIC X.
00061      12  WS-PASSWORD-X2             PIC X.
00062      12  WS-PASSWORD-X3             PIC X.
00063      12  WS-PASSWORD-X4             PIC X.
00064      12  WS-PASSWORD-X5             PIC X.
00065      12  WS-PASSWORD-X6             PIC X.
00066      12  WS-PASSWORD-X7             PIC X.
00067      12  WS-PASSWORD-X8             PIC X.
00068      12  WS-PASSWORD-X9             PIC X.
00069      12  WS-PASSWORD-X10            PIC X.
00070      12  WS-PASSWORD-X11            PIC X.
00071
00072  01  WS-SAVE-PASSWORD-NUMERIC REDEFINES  WS-SAVE-PASSWORD.
00073      12  WS-PASSWORD-N1             PIC 9.
00074      12  WS-PASSWORD-N2             PIC 9.
00075      12  WS-PASSWORD-N3             PIC 9.
00076      12  WS-PASSWORD-N4             PIC 9.
00077      12  WS-PASSWORD-N5             PIC 9.
00078      12  WS-PASSWORD-N6             PIC 9.
00079      12  WS-PASSWORD-N7             PIC 9.
00080      12  WS-PASSWORD-N8             PIC 9.
00081      12  WS-PASSWORD-N9             PIC 9.
00082      12  WS-PASSWORD-N10            PIC 9.
00083      12  WS-PASSWORD-N11            PIC 9.
00084
00085  01  ERROR-MESSAGES.
00086      12  ER-0000             PIC  X(4)       VALUE '0000'.
00087      12  ER-0004             PIC  X(4)       VALUE '0004'.
00088      12  ER-0007             PIC  X(4)       VALUE '0007'.
00089      12  ER-0019             PIC  X(4)       VALUE '0019'.
00090      12  ER-0023             PIC  X(4)       VALUE '0023'.
00091      12  ER-0029             PIC  X(4)       VALUE '0029'.
00092      12  ER-0042             PIC  X(4)       VALUE '0042'.
00093      12  ER-0050             PIC  X(4)       VALUE '0050'.
00094      12  ER-0063             PIC  X(4)       VALUE '0063'.
00095      12  ER-0068             PIC  X(4)       VALUE '0068'.
00096      12  ER-0070             PIC  X(4)       VALUE '0070'.
00097      12  ER-0073             PIC  X(4)       VALUE '0073'.
00098      12  ER-0074             PIC  X(4)       VALUE '0074'.
00099      12  ER-0075             PIC  X(4)       VALUE '0075'.
00100      12  ER-0077             PIC  X(4)       VALUE '0077'.
00101      12  ER-0078             PIC  X(4)       VALUE '0078'.
00102      12  ER-0079             PIC  X(4)       VALUE '0079'.
00103      12  ER-0080             PIC  X(4)       VALUE '0080'.
00104      12  ER-0081             PIC  X(4)       VALUE '0081'.
00105      12  ER-0082             PIC  X(4)       VALUE '0082'.
00106      12  ER-0083             PIC  X(4)       VALUE '0083'.
00107      12  ER-0084             PIC  X(4)       VALUE '0084'.
00108      12  ER-0085             PIC  X(4)       VALUE '0085'.
00109      12  ER-0086             PIC  X(4)       VALUE '0086'.
00110      12  ER-0130             PIC  X(4)       VALUE '0130'.
00111      12  ER-0252             PIC  X(4)       VALUE '0252'.
00112      12  ER-0425             PIC  X(4)       VALUE '0425'.
00113      12  ER-0614             PIC  X(4)       VALUE '0614'.
00114      12  ER-1889             PIC  X(4)       VALUE '1889'.
00115      12  ER-2376             PIC  X(4)       VALUE '2376'.
00116      12  ER-2548             PIC  X(4)       VALUE '2548'.
00117      12  ER-2549             PIC  X(4)       VALUE '2549'.
00118      12  ER-7008             PIC  X(4)       VALUE '7008'.
00119      12  ER-7084             PIC  X(4)       VALUE '7084'.
00120      12  ER-7085             PIC  X(4)       VALUE '7085'.
00121      12  ER-7087             PIC  X(4)       VALUE '7087'.
00122      12  ER-7088             PIC  X(4)       VALUE '7088'.
00123      12  ER-7089             PIC  X(4)       VALUE '7089'.
00124      12  ER-8026             PIC  X(4)       VALUE '8026'.
00125      12  ER-9014             PIC  X(4)       VALUE '9014'.
00126      12  ER-9946             PIC  X(4)       VALUE '9946'.
00127
00128  01  WS-DATE-AREA.
00129      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.
00130      12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.
00131
00132      EJECT
00133  01  STANDARD-AREAS.
00134      12  MAP-NAME            PIC  X(8)       VALUE 'EL103A'.
00135      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL103S'.
00136      12  TRANS-ID            PIC  X(4)       VALUE 'EX09'.
00137      12  THIS-PGM            PIC  X(8)       VALUE 'EL103'.
00138      12  PGM-NAME            PIC  X(8).
00139      12  TIME-IN             PIC S9(7).
00140      12  TIME-OUT-R  REDEFINES  TIME-IN.
00141          16  FILLER          PIC  X.
00142          16  TIME-OUT        PIC  99V99.
00143          16  FILLER          PIC  XX.
00144      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00145      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00146      12  XCTL-126            PIC  X(8)       VALUE 'EL126'.
00147      12  XCTL-1031           PIC  X(8)       VALUE 'EL1031'.
00148      12  XCTL-EM626          PIC  X(8)       VALUE 'EM626'.
00149      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
00150      12  XCTL-800            PIC  X(8)       VALUE 'GL800'.
00151      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00152      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00153      12  LINK-ELDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00154      12  FILE-ID             PIC  X(8)       VALUE 'ELCNTL'.
00155      12  MORTGAGE-SCRTY      PIC  X(12)      VALUE 'ACCESS CODE:'.
00156      12  CREDIT-SCRTY        PIC  X(12)      VALUE '    ACCOUNT:'.
00157
00158  01  MISC-WORK-AREAS.
00159      12  SYS                 PIC S9(4)      VALUE +0    COMP.
00160      12  MAXSYS              PIC S9(4)      VALUE +4    COMP.
00161      12  SLOT                PIC S9(4)      VALUE +0    COMP.
00162      12  MAXSLOT             PIC S9(4)      VALUE +44   COMP.
00163      12  READ-PREV-SW        PIC X           VALUE SPACE.
00164          88  FIRST-READ-PREV                 VALUE '1'.
00165          88  SECOND-READ-PREV                VALUE '2'.
00166      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
00167      12  TERMINAL-TEST-AREA.
00168          16  TTA-1           PIC  X.
00169          16  TTA-2           PIC  X.
00170          16  TTA-3           PIC  X.
00171          16  TTA-4           PIC  X.
00172
00173  01  SYSTEM-DESCRIPTIONS.
00174      12  CREDIT      PIC X(26) VALUE 'CREDIT APPLICATIONS       '.
00175      12  CLAIMS      PIC X(26) VALUE 'CLAIMS APPLICATIONS       '.
00176      12  CREDIT-CARD PIC X(26) VALUE 'CREDIT CARD APPLICATIONS  '.
00177      12  ACCT-RECV   PIC X(26) VALUE 'ACCT RECV APPLICATIONS    '.
00178      12  MORTGAGE    PIC X(26) VALUE 'CONVENIENCE APPLICATIONS  '.
00179      12  GNRL-LDGR   PIC X(26) VALUE 'GENERAL LDGR APPLICATIONS '.
00180
00181  01  DESCRIPTION-TABLE  REDEFINES  SYSTEM-DESCRIPTIONS.
00182      12  DESCRIPT  OCCURS  6 TIMES  PIC X(26).
00183
00184      EJECT
00185
00186  01  ACCESS-KEYS.
00187      12  ELCNTL-KEY.
00188          16  CK-COMP-ID      PIC  X(3).
00189          16  CK-REC-TYPE     PIC  X          VALUE '2'.
00190          16  CK-USER-CD      PIC  X(4)       VALUE SPACES.
00191          16  CK-CARRIER  REDEFINES
00192              CK-USER-CD.
00193              20  FILLER      PIC  X(3).
00194              20  CK-CARR     PIC  X.
00195          16  CK-SEQ          PIC S9(4)       VALUE +0      COMP.
00196      EJECT
00197 *    COPY ELCDATE.
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
00198      EJECT
00199 *    COPY ELCLOGOF.
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
00200      EJECT
00201 *    COPY ELCATTR.
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
00202      EJECT
00203 *    COPY ELCEMIB.
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
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00204      EJECT
00205 *    COPY ELCINTF.
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
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
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
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
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
00206      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00207          16  PI-PREV-USER    PIC  X(4).
00208          16  PI-READ-SW      PIC S9.
00209              88  READ-PROCESSOR-ZERO   VALUE +0.
00210              88  READ-PROCESSOR-ONE    VALUE +1.
00211          16  PI-APPLICATION  PIC S9.
00212              88  PI-VALID-APP          VALUES ARE +1 +2 +3 +4.
00213              88  CREDIT-APP            VALUES ARE +1.
00214              88  CLAIMS-APP            VALUES ARE +2.
00215              88  CREDIT-CARD-APP       VALUES ARE +3.
00216              88  ACCT-RECV-APP         VALUES ARE +4.
00217              88  MORTGAGE-APP          VALUES ARE +1.
00218          16  FILLER          PIC  X(634).
00219      EJECT
00220 *    COPY ELCJPFX.
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
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
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
00221      PIC  X(750).
00222      EJECT
00223 *    COPY ELCAID.
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
00224
00225  01  FILLER  REDEFINES  DFHAID.
00226      12  FILLER              PIC  X(8).
00227      12  PF-VALUES           PIC  X          OCCURS 2.
00228
00229      EJECT
00230 *    COPY EL103S.
       01  EL103AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  COMPL PIC S9(0004) COMP.
           05  COMPF PIC  X(0001).
           05  FILLER REDEFINES COMPF.
               10  COMPA PIC  X(0001).
           05  COMPI PIC  X(0003).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  LSTUSRL PIC S9(0004) COMP.
           05  LSTUSRF PIC  X(0001).
           05  FILLER REDEFINES LSTUSRF.
               10  LSTUSRA PIC  X(0001).
           05  LSTUSRI PIC  X(0004).
      *    -------------------------------
           05  LSTDTEL PIC S9(0004) COMP.
           05  LSTDTEF PIC  X(0001).
           05  FILLER REDEFINES LSTDTEF.
               10  LSTDTEA PIC  X(0001).
           05  LSTDTEI PIC  X(0008).
      *    -------------------------------
           05  LSTTIMEL PIC S9(0004) COMP.
           05  LSTTIMEF PIC  X(0001).
           05  FILLER REDEFINES LSTTIMEF.
               10  LSTTIMEA PIC  X(0001).
           05  LSTTIMEI PIC  X(0005).
      *    -------------------------------
           05  USERCDL PIC S9(0004) COMP.
           05  USERCDF PIC  X(0001).
           05  FILLER REDEFINES USERCDF.
               10  USERCDA PIC  X(0001).
           05  USERCDI PIC  X(0004).
      *    -------------------------------
           05  SCRTYL PIC S9(0004) COMP.
           05  SCRTYF PIC  X(0001).
           05  FILLER REDEFINES SCRTYF.
               10  SCRTYA PIC  X(0001).
           05  SCRTYI PIC  X(0012).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  CARRDESL PIC S9(0004) COMP.
           05  CARRDESF PIC  X(0001).
           05  FILLER REDEFINES CARRDESF.
               10  CARRDESA PIC  X(0001).
           05  CARRDESI PIC  X(0008).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  NAMEL PIC S9(0004) COMP.
           05  NAMEF PIC  X(0001).
           05  FILLER REDEFINES NAMEF.
               10  NAMEA PIC  X(0001).
           05  NAMEI PIC  X(0030).
      *    -------------------------------
           05  TITLEL PIC S9(0004) COMP.
           05  TITLEF PIC  X(0001).
           05  FILLER REDEFINES TITLEF.
               10  TITLEA PIC  X(0001).
           05  TITLEI PIC  X(0026).
      *    -------------------------------
           05  LOGONL PIC S9(0004) COMP.
           05  LOGONF PIC  X(0001).
           05  FILLER REDEFINES LOGONF.
               10  LOGONA PIC  X(0001).
           05  LOGONI PIC  X(0001).
      *    -------------------------------
           05  USRALML PIC S9(0004) COMP.
           05  USRALMF PIC  X(0001).
           05  FILLER REDEFINES USRALMF.
               10  USRALMA PIC  X(0001).
           05  USRALMI PIC  X(0001).
      *    -------------------------------
           05  USRPWDL PIC S9(0004) COMP.
           05  USRPWDF PIC  X(0001).
           05  FILLER REDEFINES USRPWDF.
               10  USRPWDA PIC  X(0001).
           05  USRPWDI PIC  X(0011).
      *    -------------------------------
           05  NEWPWDL PIC S9(0004) COMP.
           05  NEWPWDF PIC  X(0001).
           05  FILLER REDEFINES NEWPWDF.
               10  NEWPWDA PIC  X(0001).
           05  NEWPWDI PIC  X(0011).
      *    -------------------------------
           05  CREDITL PIC S9(0004) COMP.
           05  CREDITF PIC  X(0001).
           05  FILLER REDEFINES CREDITF.
               10  CREDITA PIC  X(0001).
           05  CREDITI PIC  X(0001).
      *    -------------------------------
           05  LANGTYPL PIC S9(0004) COMP.
           05  LANGTYPF PIC  X(0001).
           05  FILLER REDEFINES LANGTYPF.
               10  LANGTYPA PIC  X(0001).
           05  LANGTYPI PIC  X(0001).
      *    -------------------------------
           05  USRPRNTL PIC S9(0004) COMP.
           05  USRPRNTF PIC  X(0001).
           05  FILLER REDEFINES USRPRNTF.
               10  USRPRNTA PIC  X(0001).
           05  USRPRNTI PIC  X(0004).
      *    -------------------------------
           05  CLAIMSL PIC S9(0004) COMP.
           05  CLAIMSF PIC  X(0001).
           05  FILLER REDEFINES CLAIMSF.
               10  CLAIMSA PIC  X(0001).
           05  CLAIMSI PIC  X(0001).
      *    -------------------------------
           05  LIFEL PIC S9(0004) COMP.
           05  LIFEF PIC  X(0001).
           05  FILLER REDEFINES LIFEF.
               10  LIFEA PIC  X(0001).
           05  LIFEI PIC  X(0001).
      *    -------------------------------
           05  CURRTRML PIC S9(0004) COMP.
           05  CURRTRMF PIC  X(0001).
           05  FILLER REDEFINES CURRTRMF.
               10  CURRTRMA PIC  X(0001).
           05  CURRTRMI PIC  X(0004).
      *    -------------------------------
           05  APPLVL PIC S9(0004) COMP.
           05  APPLVF PIC  X(0001).
           05  FILLER REDEFINES APPLVF.
               10  APPLVA PIC  X(0001).
           05  APPLVI PIC  X(0001).
      *    -------------------------------
           05  CSRL PIC S9(0004) COMP.
           05  CSRF PIC  X(0001).
           05  FILLER REDEFINES CSRF.
               10  CSRA PIC  X(0001).
           05  CSRI PIC  X(0001).
      *    -------------------------------
           05  DESCRL PIC S9(0004) COMP.
           05  DESCRF PIC  X(0001).
           05  FILLER REDEFINES DESCRF.
               10  DESCRA PIC  X(0001).
           05  DESCRI PIC  X(0026).
      *    -------------------------------
           05  SLCTSYSL PIC S9(0004) COMP.
           05  SLCTSYSF PIC  X(0001).
           05  FILLER REDEFINES SLCTSYSF.
               10  SLCTSYSA PIC  X(0001).
           05  SLCTSYSI PIC  X(0002).
      *    -------------------------------
           05  FORCEL PIC S9(0004) COMP.
           05  FORCEF PIC  X(0001).
           05  FILLER REDEFINES FORCEF.
               10  FORCEA PIC  X(0001).
           05  FORCEI PIC  X(0001).
      *    -------------------------------
           05  APP01L PIC S9(0004) COMP.
           05  APP01F PIC  X(0001).
           05  FILLER REDEFINES APP01F.
               10  APP01A PIC  X(0001).
           05  APP01I PIC  X(0002).
      *    -------------------------------
           05  APP02L PIC S9(0004) COMP.
           05  APP02F PIC  X(0001).
           05  FILLER REDEFINES APP02F.
               10  APP02A PIC  X(0001).
           05  APP02I PIC  X(0002).
      *    -------------------------------
           05  APP03L PIC S9(0004) COMP.
           05  APP03F PIC  X(0001).
           05  FILLER REDEFINES APP03F.
               10  APP03A PIC  X(0001).
           05  APP03I PIC  X(0002).
      *    -------------------------------
           05  APP04L PIC S9(0004) COMP.
           05  APP04F PIC  X(0001).
           05  FILLER REDEFINES APP04F.
               10  APP04A PIC  X(0001).
           05  APP04I PIC  X(0002).
      *    -------------------------------
           05  APP05L PIC S9(0004) COMP.
           05  APP05F PIC  X(0001).
           05  FILLER REDEFINES APP05F.
               10  APP05A PIC  X(0001).
           05  APP05I PIC  X(0002).
      *    -------------------------------
           05  APP06L PIC S9(0004) COMP.
           05  APP06F PIC  X(0001).
           05  FILLER REDEFINES APP06F.
               10  APP06A PIC  X(0001).
           05  APP06I PIC  X(0002).
      *    -------------------------------
           05  APP07L PIC S9(0004) COMP.
           05  APP07F PIC  X(0001).
           05  FILLER REDEFINES APP07F.
               10  APP07A PIC  X(0001).
           05  APP07I PIC  X(0002).
      *    -------------------------------
           05  APP08L PIC S9(0004) COMP.
           05  APP08F PIC  X(0001).
           05  FILLER REDEFINES APP08F.
               10  APP08A PIC  X(0001).
           05  APP08I PIC  X(0002).
      *    -------------------------------
           05  APP09L PIC S9(0004) COMP.
           05  APP09F PIC  X(0001).
           05  FILLER REDEFINES APP09F.
               10  APP09A PIC  X(0001).
           05  APP09I PIC  X(0002).
      *    -------------------------------
           05  APP10L PIC S9(0004) COMP.
           05  APP10F PIC  X(0001).
           05  FILLER REDEFINES APP10F.
               10  APP10A PIC  X(0001).
           05  APP10I PIC  X(0002).
      *    -------------------------------
           05  APP11L PIC S9(0004) COMP.
           05  APP11F PIC  X(0001).
           05  FILLER REDEFINES APP11F.
               10  APP11A PIC  X(0001).
           05  APP11I PIC  X(0002).
      *    -------------------------------
           05  APP12L PIC S9(0004) COMP.
           05  APP12F PIC  X(0001).
           05  FILLER REDEFINES APP12F.
               10  APP12A PIC  X(0001).
           05  APP12I PIC  X(0002).
      *    -------------------------------
           05  APP13L PIC S9(0004) COMP.
           05  APP13F PIC  X(0001).
           05  FILLER REDEFINES APP13F.
               10  APP13A PIC  X(0001).
           05  APP13I PIC  X(0002).
      *    -------------------------------
           05  APP14L PIC S9(0004) COMP.
           05  APP14F PIC  X(0001).
           05  FILLER REDEFINES APP14F.
               10  APP14A PIC  X(0001).
           05  APP14I PIC  X(0002).
      *    -------------------------------
           05  APP15L PIC S9(0004) COMP.
           05  APP15F PIC  X(0001).
           05  FILLER REDEFINES APP15F.
               10  APP15A PIC  X(0001).
           05  APP15I PIC  X(0002).
      *    -------------------------------
           05  APP16L PIC S9(0004) COMP.
           05  APP16F PIC  X(0001).
           05  FILLER REDEFINES APP16F.
               10  APP16A PIC  X(0001).
           05  APP16I PIC  X(0002).
      *    -------------------------------
           05  APP17L PIC S9(0004) COMP.
           05  APP17F PIC  X(0001).
           05  FILLER REDEFINES APP17F.
               10  APP17A PIC  X(0001).
           05  APP17I PIC  X(0002).
      *    -------------------------------
           05  APP18L PIC S9(0004) COMP.
           05  APP18F PIC  X(0001).
           05  FILLER REDEFINES APP18F.
               10  APP18A PIC  X(0001).
           05  APP18I PIC  X(0002).
      *    -------------------------------
           05  APP19L PIC S9(0004) COMP.
           05  APP19F PIC  X(0001).
           05  FILLER REDEFINES APP19F.
               10  APP19A PIC  X(0001).
           05  APP19I PIC  X(0002).
      *    -------------------------------
           05  APP20L PIC S9(0004) COMP.
           05  APP20F PIC  X(0001).
           05  FILLER REDEFINES APP20F.
               10  APP20A PIC  X(0001).
           05  APP20I PIC  X(0002).
      *    -------------------------------
           05  APP21L PIC S9(0004) COMP.
           05  APP21F PIC  X(0001).
           05  FILLER REDEFINES APP21F.
               10  APP21A PIC  X(0001).
           05  APP21I PIC  X(0002).
      *    -------------------------------
           05  APP22L PIC S9(0004) COMP.
           05  APP22F PIC  X(0001).
           05  FILLER REDEFINES APP22F.
               10  APP22A PIC  X(0001).
           05  APP22I PIC  X(0002).
      *    -------------------------------
           05  APP23L PIC S9(0004) COMP.
           05  APP23F PIC  X(0001).
           05  FILLER REDEFINES APP23F.
               10  APP23A PIC  X(0001).
           05  APP23I PIC  X(0002).
      *    -------------------------------
           05  APP24L PIC S9(0004) COMP.
           05  APP24F PIC  X(0001).
           05  FILLER REDEFINES APP24F.
               10  APP24A PIC  X(0001).
           05  APP24I PIC  X(0002).
      *    -------------------------------
           05  APP25L PIC S9(0004) COMP.
           05  APP25F PIC  X(0001).
           05  FILLER REDEFINES APP25F.
               10  APP25A PIC  X(0001).
           05  APP25I PIC  X(0002).
      *    -------------------------------
           05  APP26L PIC S9(0004) COMP.
           05  APP26F PIC  X(0001).
           05  FILLER REDEFINES APP26F.
               10  APP26A PIC  X(0001).
           05  APP26I PIC  X(0002).
      *    -------------------------------
           05  APP27L PIC S9(0004) COMP.
           05  APP27F PIC  X(0001).
           05  FILLER REDEFINES APP27F.
               10  APP27A PIC  X(0001).
           05  APP27I PIC  X(0002).
      *    -------------------------------
           05  APP28L PIC S9(0004) COMP.
           05  APP28F PIC  X(0001).
           05  FILLER REDEFINES APP28F.
               10  APP28A PIC  X(0001).
           05  APP28I PIC  X(0002).
      *    -------------------------------
           05  APP29L PIC S9(0004) COMP.
           05  APP29F PIC  X(0001).
           05  FILLER REDEFINES APP29F.
               10  APP29A PIC  X(0001).
           05  APP29I PIC  X(0002).
      *    -------------------------------
           05  APP30L PIC S9(0004) COMP.
           05  APP30F PIC  X(0001).
           05  FILLER REDEFINES APP30F.
               10  APP30A PIC  X(0001).
           05  APP30I PIC  X(0002).
      *    -------------------------------
           05  APP31L PIC S9(0004) COMP.
           05  APP31F PIC  X(0001).
           05  FILLER REDEFINES APP31F.
               10  APP31A PIC  X(0001).
           05  APP31I PIC  X(0002).
      *    -------------------------------
           05  APP32L PIC S9(0004) COMP.
           05  APP32F PIC  X(0001).
           05  FILLER REDEFINES APP32F.
               10  APP32A PIC  X(0001).
           05  APP32I PIC  X(0002).
      *    -------------------------------
           05  APP33L PIC S9(0004) COMP.
           05  APP33F PIC  X(0001).
           05  FILLER REDEFINES APP33F.
               10  APP33A PIC  X(0001).
           05  APP33I PIC  X(0002).
      *    -------------------------------
           05  APP34L PIC S9(0004) COMP.
           05  APP34F PIC  X(0001).
           05  FILLER REDEFINES APP34F.
               10  APP34A PIC  X(0001).
           05  APP34I PIC  X(0002).
      *    -------------------------------
           05  APP35L PIC S9(0004) COMP.
           05  APP35F PIC  X(0001).
           05  FILLER REDEFINES APP35F.
               10  APP35A PIC  X(0001).
           05  APP35I PIC  X(0002).
      *    -------------------------------
           05  APP36L PIC S9(0004) COMP.
           05  APP36F PIC  X(0001).
           05  FILLER REDEFINES APP36F.
               10  APP36A PIC  X(0001).
           05  APP36I PIC  X(0002).
      *    -------------------------------
           05  APP37L PIC S9(0004) COMP.
           05  APP37F PIC  X(0001).
           05  FILLER REDEFINES APP37F.
               10  APP37A PIC  X(0001).
           05  APP37I PIC  X(0002).
      *    -------------------------------
           05  APP38L PIC S9(0004) COMP.
           05  APP38F PIC  X(0001).
           05  FILLER REDEFINES APP38F.
               10  APP38A PIC  X(0001).
           05  APP38I PIC  X(0002).
      *    -------------------------------
           05  APP39L PIC S9(0004) COMP.
           05  APP39F PIC  X(0001).
           05  FILLER REDEFINES APP39F.
               10  APP39A PIC  X(0001).
           05  APP39I PIC  X(0002).
      *    -------------------------------
           05  APP40L PIC S9(0004) COMP.
           05  APP40F PIC  X(0001).
           05  FILLER REDEFINES APP40F.
               10  APP40A PIC  X(0001).
           05  APP40I PIC  X(0002).
      *    -------------------------------
           05  APP41L PIC S9(0004) COMP.
           05  APP41F PIC  X(0001).
           05  FILLER REDEFINES APP41F.
               10  APP41A PIC  X(0001).
           05  APP41I PIC  X(0002).
      *    -------------------------------
           05  APP42L PIC S9(0004) COMP.
           05  APP42F PIC  X(0001).
           05  FILLER REDEFINES APP42F.
               10  APP42A PIC  X(0001).
           05  APP42I PIC  X(0002).
      *    -------------------------------
           05  APP43L PIC S9(0004) COMP.
           05  APP43F PIC  X(0001).
           05  FILLER REDEFINES APP43F.
               10  APP43A PIC  X(0001).
           05  APP43I PIC  X(0002).
      *    -------------------------------
           05  APP44L PIC S9(0004) COMP.
           05  APP44F PIC  X(0001).
           05  FILLER REDEFINES APP44F.
               10  APP44A PIC  X(0001).
           05  APP44I PIC  X(0002).
      *    -------------------------------
           05  XCDAYSL PIC S9(0004) COMP.
           05  XCDAYSF PIC  X(0001).
           05  FILLER REDEFINES XCDAYSF.
               10  XCDAYSA PIC  X(0001).
           05  XCDAYSI PIC  X(0035).
      *    -------------------------------
           05  QCDAYSL PIC S9(0004) COMP.
           05  QCDAYSF PIC  X(0001).
           05  FILLER REDEFINES QCDAYSF.
               10  QCDAYSA PIC  X(0001).
           05  QCDAYSI PIC  9(3).
      *    -------------------------------
           05  XAHL PIC S9(0004) COMP.
           05  XAHF PIC  X(0001).
           05  FILLER REDEFINES XAHF.
               10  XAHA PIC  X(0001).
           05  XAHI PIC  X(0003).
      *    -------------------------------
           05  QCAMTL PIC S9(0004) COMP.
           05  QCAMTF PIC  X(0001).
           05  FILLER REDEFINES QCAMTF.
               10  QCAMTA PIC  X(0001).
           05  QCAMTI PIC  S9(5)V9(2).
      *    -------------------------------
           05  XLIFEL PIC S9(0004) COMP.
           05  XLIFEF PIC  X(0001).
           05  FILLER REDEFINES XLIFEF.
               10  XLIFEA PIC  X(0001).
           05  XLIFEI PIC  X(0004).
      *    -------------------------------
           05  XAXDAYL PIC S9(0004) COMP.
           05  XAXDAYF PIC  X(0001).
           05  FILLER REDEFINES XAXDAYF.
               10  XAXDAYA PIC  X(0001).
           05  XAXDAYI PIC  X(0035).
      *    -------------------------------
           05  MAXDAYL PIC S9(0004) COMP.
           05  MAXDAYF PIC  X(0001).
           05  FILLER REDEFINES MAXDAYF.
               10  MAXDAYA PIC  X(0001).
           05  MAXDAYI PIC  9(3).
      *    -------------------------------
           05  XAMT1L PIC S9(0004) COMP.
           05  XAMT1F PIC  X(0001).
           05  FILLER REDEFINES XAMT1F.
               10  XAMT1A PIC  X(0001).
           05  XAMT1I PIC  X(0005).
      *    -------------------------------
           05  MXAHAMTL PIC S9(0004) COMP.
           05  MXAHAMTF PIC  X(0001).
           05  FILLER REDEFINES MXAHAMTF.
               10  MXAHAMTA PIC  X(0001).
           05  MXAHAMTI PIC  S9(9)V9(2).
      *    -------------------------------
           05  XAMT2L PIC S9(0004) COMP.
           05  XAMT2F PIC  X(0001).
           05  FILLER REDEFINES XAMT2F.
               10  XAMT2A PIC  X(0001).
           05  XAMT2I PIC  X(0005).
      *    -------------------------------
           05  MXLFAMTL PIC S9(0004) COMP.
           05  MXLFAMTF PIC  X(0001).
           05  FILLER REDEFINES MXLFAMTF.
               10  MXLFAMTA PIC  X(0001).
           05  MXLFAMTI PIC  S9(9)V9(2).
      *    -------------------------------
           05  XUTMONL PIC S9(0004) COMP.
           05  XUTMONF PIC  X(0001).
           05  FILLER REDEFINES XUTMONF.
               10  XUTMONA PIC  X(0001).
           05  XUTMONI PIC  X(0035).
      *    -------------------------------
           05  AUTMONL PIC S9(0004) COMP.
           05  AUTMONF PIC  X(0001).
           05  FILLER REDEFINES AUTMONF.
               10  AUTMONA PIC  X(0001).
           05  AUTMONI PIC  9(3).
      *    -------------------------------
           05  AUTAMTL PIC S9(0004) COMP.
           05  AUTAMTF PIC  X(0001).
           05  FILLER REDEFINES AUTAMTF.
               10  AUTAMTA PIC  X(0001).
           05  AUTAMTI PIC  S9(9)V9(2).
      *    -------------------------------
           05  XEXP1L PIC S9(0004) COMP.
           05  XEXP1F PIC  X(0001).
           05  FILLER REDEFINES XEXP1F.
               10  XEXP1A PIC  X(0001).
           05  XEXP1I PIC  X(0035).
      *    -------------------------------
           05  EXPAMTL PIC S9(0004) COMP.
           05  EXPAMTF PIC  X(0001).
           05  FILLER REDEFINES EXPAMTF.
               10  EXPAMTA PIC  X(0001).
           05  EXPAMTI PIC  X(0005).
      *    -------------------------------
           05  MAXEXPL PIC S9(0004) COMP.
           05  MAXEXPF PIC  X(0001).
           05  FILLER REDEFINES MAXEXPF.
               10  MAXEXPA PIC  X(0001).
           05  MAXEXPI PIC  S9(9)V9(2).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0076).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  9(2).
      *    -------------------------------
           05  PFCRDTL PIC S9(0004) COMP.
           05  PFCRDTF PIC  X(0001).
           05  FILLER REDEFINES PFCRDTF.
               10  PFCRDTA PIC  X(0001).
           05  PFCRDTI PIC  X(0010).
      *    -------------------------------
           05  PFCARDL PIC S9(0004) COMP.
           05  PFCARDF PIC  X(0001).
           05  FILLER REDEFINES PFCARDF.
               10  PFCARDA PIC  X(0001).
           05  PFCARDI PIC  X(0016).
      *    -------------------------------
           05  PFMORTL PIC S9(0004) COMP.
           05  PFMORTF PIC  X(0001).
           05  FILLER REDEFINES PFMORTF.
               10  PFMORTA PIC  X(0001).
           05  PFMORTI PIC  X(0023).
      *    -------------------------------
           05  PFCLMSL PIC S9(0004) COMP.
           05  PFCLMSF PIC  X(0001).
           05  FILLER REDEFINES PFCLMSF.
               10  PFCLMSA PIC  X(0001).
           05  PFCLMSI PIC  X(0010).
      *    -------------------------------
           05  PFRECVL PIC S9(0004) COMP.
           05  PFRECVF PIC  X(0001).
           05  FILLER REDEFINES PFRECVF.
               10  PFRECVA PIC  X(0001).
           05  PFRECVI PIC  X(0016).
       01  EL103AO REDEFINES EL103AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTUSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SCRTYO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRDESO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TITLEO PIC  X(0026).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOGONO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRALMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRPWDO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NEWPWDO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREDITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LANGTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USRPRNTO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LIFEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CURRTRMO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APPLVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESCRO PIC  X(0026).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SLCTSYSO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORCEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP17O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP18O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP19O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP20O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP21O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP22O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP23O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP24O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP25O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP26O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP27O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP28O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP29O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP30O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP31O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP32O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP33O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP34O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP35O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP36O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP37O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP38O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP39O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP40O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP41O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP42O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP43O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APP44O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XCDAYSO PIC  X(0035).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  QCDAYSO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XAHO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  QCAMTO PIC  ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XLIFEO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XAXDAYO PIC  X(0035).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXDAYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XAMT1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MXAHAMTO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XAMT2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MXLFAMTO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XUTMONO PIC  X(0035).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUTMONO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUTAMTO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  XEXP1O PIC  X(0035).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPAMTO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXEXPO PIC  ZZZZZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFCRDTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFCARDO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFMORTO PIC  X(0023).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFCLMSO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFRECVO PIC  X(0016).
      *    -------------------------------
00231
00232  01  FILLER  REDEFINES  EL103AI.
011812     12  FILLER              PIC  X(291).
00234      12  APP-INFO     OCCURS 44 TIMES.
00235          16  APP-LENGTH      PIC S9(4)   COMP.
00236          16  APP-ATTRB       PIC  X.
00237          16  APP             PIC  XX.
00238
00239      EJECT
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
00241  01  DFHCOMMAREA             PIC  X(1024).
00242
00243 *01 PARMLIST .
00244 *    12  FILLER              PIC S9(8)   COMP.
00245 *    12  ELCNTL-POINTER      PIC S9(8)   COMP.
00246      EJECT
00247 *    COPY ELCCNTL.
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
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
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
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
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
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
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
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
012913         16  FILLER                         PIC X(185).
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
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
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
00248      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL103' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00250
00251      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00252      MOVE '5'                    TO DC-OPTION-CODE.
00253
00254      PERFORM 9700-LINK-DATE-CONVERT  THRU  9799-EXIT.
00255
00256      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.
00257      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE.
00258      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00259
00260      MOVE LOW-VALUES             TO EL103AO.
00261
00262 ***************************************************************
00263 *       IF ATTEMPTING TO EXECUTE PROGRAM WITHOUT SIGNING ON   *
00264 *       (COMM LENGTH EQUAL ZERO), SEND ERROR MESSAGE.         *
00265 ***************************************************************
00266
00267      IF EIBCALEN  EQUAL 0
00268          GO TO 8800-UNAUTHORIZED-ACCESS.
00269
00270      IF PI-CALLING-PROGRAM  NOT EQUAL THIS-PGM
00271          IF PI-RETURN-TO-PROGRAM  NOT EQUAL THIS-PGM
00272              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-6
00273              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-5
00274              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-4
00275              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-3
00276              MOVE PI-SAVED-PROGRAM-1    TO PI-SAVED-PROGRAM-2
00277              MOVE PI-RETURN-TO-PROGRAM  TO PI-SAVED-PROGRAM-1
00278              MOVE PI-CALLING-PROGRAM    TO PI-RETURN-TO-PROGRAM
00279              MOVE THIS-PGM              TO PI-CALLING-PROGRAM
00280              MOVE SPACES                TO PI-PREV-USER
00281              MOVE ZEROS                 TO PI-APPLICATION
00282          ELSE
00283              MOVE PI-RETURN-TO-PROGRAM  TO PI-CALLING-PROGRAM
00284              MOVE PI-SAVED-PROGRAM-1    TO PI-RETURN-TO-PROGRAM
00285              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-1
00286              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-2
00287              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-3
00288              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-4
00289              MOVE PI-SAVED-PROGRAM-6    TO PI-SAVED-PROGRAM-5
00290              MOVE SPACES                TO PI-SAVED-PROGRAM-6.
00291
00292      
      * EXEC CICS  HANDLE CONDITION
00293 *        DUPREC    (8840-DUPREC)
00294 *        NOTOPEN   (8870-NOTOPEN)
00295 *        NOTFND    (8880-NOT-FOUND)
00296 *        PGMIDERR  (9600-PGMID-ERROR)
00297 *        ERROR     (9999-ABEND)
00298 *    END-EXEC.
      *    MOVE '"$%JIL.               ! " #00003476' TO DFHEIV0
           MOVE X'2224254A494C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00299
00300      IF EIBTRNID  NOT EQUAL TRANS-ID
00301          IF PI-PREV-USER NOT EQUAL SPACES
00302              MOVE PI-PREV-USER   TO USERCDI
00303              MOVE +4             TO USERCDL
00304              GO TO 1000-SHOW-USER.
00305
00306      IF EIBTRNID  NOT EQUAL TRANS-ID
00307          IF CREDIT-SESSION
00308              MOVE ZERO           TO PI-READ-SW
00309              MOVE 1              TO PI-APPLICATION
00310              MOVE DESCRIPT(1)    TO DESCRO
00311              GO TO 8100-SEND-INITIAL-MAP
00312          ELSE
00313          IF CLAIM-SESSION
00314              MOVE ZERO           TO PI-READ-SW
00315              MOVE 2              TO PI-APPLICATION
00316              MOVE DESCRIPT(2)    TO DESCRO
00317              MOVE AL-UANON       TO AUTAMTA
00318                                     QCDAYSA
00319                                     QCAMTA
00320                                     MAXDAYA
00321                                     MXAHAMTA
00322                                     MXLFAMTA
00323                                     AUTMONA
00324                                     MAXEXPA
00325              MOVE AL-SANON       TO XAMT1A
00326                                     XAMT2A
00327                                     XUTMONA
00328                                     XCDAYSA
00329                                     XAHA
00330                                     XLIFEA
00331                                     XAXDAYA
00332                                     XEXP1A
00333                                     EXPAMTA
00334              MOVE WS-QCDAYS      TO XCDAYSI
00335              MOVE WS-AH          TO XAHI
00336              MOVE WS-LIFE        TO XLIFEI
00337              MOVE WS-MAXDAYS     TO XAXDAYI
00338              MOVE WS-AUTO-PAY    TO XUTMONI
00339              MOVE WS-AMOUNT      TO XAMT1I
00340                                     XAMT2I
00341                                     EXPAMTI
00342              MOVE WS-EXPENSE     TO XEXP1I
00343              GO TO 8100-SEND-INITIAL-MAP
00344          ELSE
00345          IF MORTGAGE-SESSION
00346              MOVE 1              TO PI-READ-SW
00347                                     PI-APPLICATION
00348              MOVE DESCRIPT(5)    TO DESCRO
00349              GO TO 8100-SEND-INITIAL-MAP.
00350
00351      IF EIBAID  EQUAL DFHCLEAR
00352          GO TO 9400-CLEAR.
00353
00354      EJECT
00355  0200-RECEIVE.
00356
00357      MOVE LOW-VALUES             TO EL103AI.
00358
00359      IF EIBAID  EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
00360          MOVE ER-7008            TO EMI-ERROR
00361          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00362          MOVE -1                 TO MAINTL
00363          GO TO 8200-SEND-DATAONLY.
00364
00365      
      * EXEC CICS  RECEIVE
00366 *        MAP     (MAP-NAME)
00367 *        MAPSET  (MAPSET-NAME)
00368 *        INTO    (EL103AI)
00369 *    END-EXEC.
           MOVE LENGTH OF
            EL103AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003549' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL103AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00370
00371      IF ENTERPFL  EQUAL ZERO
00372          GO TO 0300-CHECK-PFKEYS.
00373
00374      IF EIBAID  NOT EQUAL DFHENTER
00375          MOVE ER-0004            TO EMI-ERROR
00376          GO TO 0310-INPUT-ERROR.
00377
00378      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)
00379          MOVE PF-VALUES (ENTERPFI)  TO EIBAID
00380      ELSE
00381          MOVE ER-0029               TO EMI-ERROR
00382          GO TO 0310-INPUT-ERROR.
00383
00384      EJECT
00385  0300-CHECK-PFKEYS.
00386      IF EIBAID  EQUAL DFHPF23
00387          GO TO 8820-PF23.
00388
00389      IF EIBAID  EQUAL DFHPF24
00390          GO TO 9200-RETURN-MAIN-MENU.
00391
00392      IF PI-USER-ALMIGHTY-YES
00393          NEXT SENTENCE
00394      ELSE
00395      IF PI-PROCESSOR-ID EQUAL USERCDI
00396          NEXT SENTENCE
00397      ELSE
00398          MOVE ER-0007            TO EMI-ERROR
00399          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00400          MOVE LOW-VALUES         TO EL103AO
00401          GO TO 8100-SEND-INITIAL-MAP.
00402
00403      IF EIBAID  EQUAL DFHPF12
00404          GO TO 9500-PF12.
00405
00406      IF MAINTL GREATER ZERO AND
00407         MAINTI NOT EQUAL SPACE AND
00408         EIBAID NOT EQUAL DFHENTER
00409          MOVE ER-0050            TO EMI-ERROR
00410          GO TO 0310-INPUT-ERROR.
00411
00412      IF EIBAID  EQUAL DFHPF1
00413          GO TO 5000-FIND-NEXT-USER.
00414
00415      IF EIBAID  EQUAL DFHPF2
00416          GO TO 5100-FIND-PREV-USER.
00417
00418      IF EIBAID  EQUAL DFHPF3
00419          IF PI-PREV-USER NOT EQUAL SPACES
00420              MOVE 1              TO PI-APPLICATION
00421              MOVE ZERO           TO PI-READ-SW
00422              GO TO 1000-SHOW-USER
00423          ELSE
00424              MOVE ER-0019        TO EMI-ERROR
00425              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT
00426              MOVE -1             TO USERCDL
00427              MOVE AL-UABON       TO USERCDA
00428              GO TO 8200-SEND-DATAONLY.
00429
00430      IF EIBAID  EQUAL DFHPF4
00431          IF PI-PREV-USER NOT EQUAL SPACES
00432              MOVE 2              TO PI-APPLICATION
00433              MOVE ZERO           TO PI-READ-SW
00434              GO TO 1000-SHOW-USER
00435          ELSE
00436              MOVE ER-0019        TO EMI-ERROR
00437              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT
00438              MOVE -1             TO USERCDL
00439              MOVE AL-UABON       TO USERCDA
00440              GO TO 8200-SEND-DATAONLY.
00441
00442      IF EIBAID  EQUAL DFHPF5
00443          IF PI-PREV-USER NOT EQUAL SPACES
00444              MOVE 3              TO PI-APPLICATION
00445              MOVE ZERO           TO PI-READ-SW
00446              GO TO 1000-SHOW-USER
00447          ELSE
00448              MOVE ER-0019        TO EMI-ERROR
00449              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT
00450              MOVE -1             TO USERCDL
00451              MOVE AL-UABON       TO USERCDA
00452              GO TO 8200-SEND-DATAONLY.
00453
00454      IF EIBAID  EQUAL DFHPF6
00455          IF PI-PREV-USER NOT EQUAL SPACES
00456              MOVE 4              TO PI-APPLICATION
00457              MOVE ZERO           TO PI-READ-SW
00458              GO TO 1000-SHOW-USER
00459          ELSE
00460              MOVE ER-0019        TO EMI-ERROR
00461              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT
00462              MOVE -1             TO USERCDL
00463              MOVE AL-UABON       TO USERCDA
00464              GO TO 8200-SEND-DATAONLY.
00465
00466      IF EIBAID  EQUAL DFHPF7
00467          IF PI-PREV-USER NOT EQUAL SPACES
00468              MOVE 1              TO PI-APPLICATION
00469                                     PI-READ-SW
00470              GO TO 1000-SHOW-USER
00471          ELSE
00472              MOVE ER-0019        TO EMI-ERROR
00473              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT
00474              MOVE -1             TO USERCDL
00475              MOVE AL-UABON       TO USERCDA
00476              GO TO 8200-SEND-DATAONLY.
00477
00478      IF EIBAID  EQUAL DFHPF8
00479          IF PI-PREV-USER NOT EQUAL SPACES
00480              MOVE XCTL-1031      TO PGM-NAME
00481              GO TO 9300-XCTL
00482          ELSE
00483              MOVE ER-0019        TO EMI-ERROR
00484              PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT
00485              MOVE -1             TO USERCDL
00486              MOVE AL-UABON       TO USERCDA
00487              GO TO 8200-SEND-DATAONLY.
00488
00489      IF EIBAID  EQUAL DFHENTER
00490          GO TO 0320-EDIT-DATA.
00491
00492      MOVE ER-0029                TO EMI-ERROR.
00493
00494  0310-INPUT-ERROR.
00495
00496      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
00497
00498      MOVE AL-UNBON               TO ENTERPFA.
00499
00500      IF ENTERPFL EQUAL ZERO
00501          MOVE -1                 TO MAINTL
00502      ELSE
00503          MOVE -1                 TO ENTERPFL.
00504
00505      GO TO 8200-SEND-DATAONLY.
00506
00507      EJECT
00508  0320-EDIT-DATA.
00509
00510      IF MAINTI  EQUAL 'S'
00511          GO TO 1000-SHOW-USER.
00512
00513      IF MAINTI  EQUAL 'C'
00514          GO TO 2000-CHANGE-USER.
00515
00516      IF PI-USER-ALMIGHTY-YES AND
00517         MAINTI  EQUAL 'A'
00518          GO TO 3000-ADD-USER.
00519
00520      IF PI-USER-ALMIGHTY-YES AND
00521         MAINTI  EQUAL 'D'
00522          GO TO 4000-DELETE-USER.
00523
00524      MOVE ER-0023                TO EMI-ERROR.
00525
00526      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
00527
00528      MOVE -1                     TO MAINTL.
00529      MOVE AL-UABON               TO MAINTA.
00530
00531      GO TO 8200-SEND-DATAONLY.
00532
00533      EJECT
00534  1000-SHOW-USER.
00535
00536      IF USERCDL  EQUAL ZERO
00537          MOVE ER-0019            TO EMI-ERROR
00538          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00539          MOVE -1                 TO USERCDL
00540          MOVE AL-UABON           TO USERCDA
00541          GO TO 8200-SEND-DATAONLY.
00542
00543      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
00544      MOVE USERCDI                TO CK-USER-CD.
00545      MOVE '2'                    TO CK-REC-TYPE.
00546
00547      PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.
00548
00549      IF MORTGAGE-APP
00550          MOVE CONTROL-FILE       TO SAVE-PROCESSOR-ZERO
00551          MOVE PI-READ-SW         TO CK-SEQ
00552          PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.
00553
00554      IF PI-USER-ALMIGHTY-YES
00555             GO TO 7000-BUILD-OUTPUT-MAP.
00556
00557      IF CREDIT-APP
00558         IF CF-ADMINISTRATION-CONTROLS(1) EQUAL 'YN' OR 'YY'
00559             NEXT SENTENCE
00560         ELSE
00561             MOVE ER-0070                TO EMI-ERROR
00562             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00563             MOVE -1                     TO MAINTL
00564             MOVE AL-UABON               TO MAINTA
00565             GO TO 8200-SEND-DATAONLY.
00566
00567      IF CLAIMS-APP
00568         IF CF-ADMINISTRATION-CONTROLS(2) EQUAL 'YN' OR 'YY'
00569             NEXT SENTENCE
00570         ELSE
00571             MOVE ER-0070                TO EMI-ERROR
00572             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00573             MOVE -1                     TO MAINTL
00574             MOVE AL-UABON               TO MAINTA
00575             GO TO 8200-SEND-DATAONLY.
00576
00577      IF CREDIT-CARD-APP
00578         IF CF-ADMINISTRATION-CONTROLS(3) EQUAL 'YN' OR 'YY'
00579             NEXT SENTENCE
00580         ELSE
00581             MOVE ER-0070                TO EMI-ERROR
00582             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00583             MOVE -1                     TO MAINTL
00584             MOVE AL-UABON               TO MAINTA
00585             GO TO 8200-SEND-DATAONLY.
00586
00587      IF ACCT-RECV-APP
00588         IF CF-ADMINISTRATION-CONTROLS(4) EQUAL 'YN' OR 'YY'
00589             NEXT SENTENCE
00590         ELSE
00591             MOVE ER-0070                TO EMI-ERROR
00592             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00593             MOVE -1                     TO MAINTL
00594             MOVE AL-UABON               TO MAINTA
00595             GO TO 8200-SEND-DATAONLY.
00596
00597      IF MORTGAGE-APP
00598         IF CF-ADMINISTRATION-CONTROLS(1) EQUAL 'YN' OR 'YY'
00599             NEXT SENTENCE
00600         ELSE
00601             MOVE ER-0070                TO EMI-ERROR
00602             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00603             MOVE -1                     TO MAINTL
00604             MOVE AL-UABON               TO MAINTA
00605             GO TO 8200-SEND-DATAONLY.
00606
00607      GO TO 7000-BUILD-OUTPUT-MAP.
00608
00609      EJECT
00610  2000-CHANGE-USER.
00611
00612      IF USERCDL EQUAL ZERO
00613          MOVE ER-0019            TO EMI-ERROR
00614          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00615          MOVE -1                 TO USERCDL
00616          MOVE AL-UABON           TO USERCDA
00617          GO TO 8200-SEND-DATAONLY.
00618
00619      IF USERCDI  NOT EQUAL PI-PREV-USER
00620          MOVE ER-0074            TO EMI-ERROR
00621          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00622          MOVE -1                 TO USERCDL
00623          MOVE AL-UABON           TO USERCDA
00624          GO TO 8200-SEND-DATAONLY.
00625
00626      PERFORM 6000-EDIT-INPUT-DATA  THRU  6999-EXIT.
00627
00628      IF NOT EMI-NO-ERRORS
00629          GO TO 8200-SEND-DATAONLY.
00630
00631      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
00632      MOVE USERCDI                TO CK-USER-CD.
00633      MOVE '2'                    TO CK-REC-TYPE.
00634
00635      PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT.
00636
00637      IF CF-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR
00638         CF-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS
00639          
      * EXEC CICS  UNLOCK
00640 *            DATASET  ('ELCNTL')
00641 *            END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&*                    #   #00003823' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00642          MOVE ER-0068            TO EMI-ERROR
00643          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00644          GO TO 1000-SHOW-USER.
00645
00646      MOVE 'B'                    TO JP-RECORD-TYPE.
00647      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00648
00649      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
00650
00651      IF USRPWDL GREATER ZERO
00652          MOVE USRPWDI            TO CF-PROCESSOR-PASSWORD.
00653
00654      IF PI-USER-ALMIGHTY-YES
00655          NEXT SENTENCE
00656      ELSE
00657          GO TO 2200-JOURNAL-RECORDS.
00658
00659      IF NAMEL GREATER ZERO
00660          MOVE NAMEI              TO CF-PROCESSOR-NAME.
00661
00662      IF TITLEL GREATER ZERO
00663          MOVE TITLEI             TO CF-PROCESSOR-TITLE.
00664
00665      IF USRPRNTL GREATER ZERO
00666          MOVE USRPRNTI           TO CF-PROCESSOR-PRINTER.
00667
00668      IF CURRTRML GREATER ZERO
00669          MOVE CURRTRMI           TO CF-CURRENT-TERM-ON.
00670
00671      IF APPLVL GREATER ZERO
00672          MOVE APPLVI             TO CF-APPROVAL-LEVEL.
011812
011812     IF CSRL GREATER ZERO
011812         MOVE CSRI               TO CF-CSR-IND
011812     END-IF.
00673
00674      IF CREDITL GREATER ZERO
00675          MOVE CREDITI            TO CF-PROC-SYS-ACCESS-CREDIT.
00676
00677      IF CLAIMSL GREATER ZERO
00678          MOVE CLAIMSI            TO CF-PROC-SYS-ACCESS-CLAIMS.
00679
00680      IF LIFEL GREATER ZERO
00681          MOVE LIFEI              TO CF-PROC-SYS-ACCESS-LIFE.
00682
00683      IF LOGONL GREATER ZERO
00684          MOVE LOGONI             TO CF-MESSAGE-AT-LOGON-CAP.
00685
00686      IF USRALML GREATER ZERO
00687          MOVE USRALMI            TO CF-PROCESSOR-USER-ALMIGHTY.
00688
00689      IF LANGTYPL GREATER ZERO
00690          MOVE LANGTYPI           TO CF-LANGUAGE-TYPE.
00691
00692      IF NOT CLAIMS-APP
00693          GO TO 2050-SKIP-CLAIMS-UPDATE.
00694
00695      IF QCDAYSL GREATER ZERO
00696          MOVE QCDAYSI            TO CF-PROC-CALC-DAYS-TOL.
00697
00698      IF QCAMTL GREATER ZERO
00699          MOVE QCAMTI             TO CF-PROC-CALC-AMT-TOL.
00700
00701      IF MAXDAYL GREATER ZERO
00702          MOVE MAXDAYI            TO CF-PROC-MAX-REG-DAYS.
00703
00704      IF MXAHAMTL GREATER ZERO
00705          MOVE MXAHAMTI           TO CF-PROC-MAX-REG-PMT.
00706
00707      IF MXLFAMTL GREATER ZERO
00708          MOVE MXLFAMTI           TO CF-PROC-MAX-LF-PMT.
00709
00710      IF AUTMONL GREATER ZERO
00711          MOVE AUTMONI            TO CF-PROC-MAX-AUTO-MOS.
00712
00713      IF AUTAMTL GREATER ZERO
00714          MOVE AUTAMTI            TO CF-PROC-MAX-AUTO-PMT.
00715
00716      IF MAXEXPL GREATER ZERO
00717          MOVE MAXEXPI            TO CF-PROC-MAX-EXP-PMT.
00718
00719  2050-SKIP-CLAIMS-UPDATE.
00720
00721      IF READ-PROCESSOR-ONE
00722          MOVE PI-PROCESSOR-ID    TO CF-LAST-MAINT-BY
00723          MOVE EIBTIME            TO CF-LAST-MAINT-HHMMSS
00724          MOVE SAVE-BIN-DATE      TO CF-LAST-MAINT-DT
00725          MOVE 'C'                TO JP-RECORD-TYPE
00726          MOVE CONTROL-FILE       TO JP-RECORD-AREA
00727          PERFORM 7800-REWRITE-CONTROL-FILE THRU 7800-EXIT
00728          PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT
00729          MOVE 1                  TO CK-SEQ
00730          PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT
00731          MOVE 'B'                TO JP-RECORD-TYPE
00732          MOVE CONTROL-FILE       TO JP-RECORD-AREA
00733          PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
00734
00735      MOVE PI-APPLICATION         TO SYS.
00736
00737      IF SLCTSYSL GREATER ZERO
00738          MOVE SLCTSYSI           TO
00739                                  CF-ADMINISTRATION-CONTROLS(SYS).
00740
00741      IF FORCEL GREATER ZERO
00742          MOVE FORCEI             TO CF-APPLICATION-FORCE(SYS).
00743
00744      MOVE 1                      TO SLOT.
00745
00746  2100-APPLICATION-LOOP.
00747
00748      IF APP-LENGTH(SLOT) GREATER ZERO
00749          MOVE APP(SLOT)          TO CF-APP-SWITCHES(SYS SLOT).
00750
00751      IF SLOT LESS THAN MAXSLOT
00752          ADD 1                   TO SLOT
00753          GO TO 2100-APPLICATION-LOOP.
00754
00755      IF ACCTL GREATER ZERO
00756          MOVE ACCTI              TO CF-PROCESSOR-ACCOUNT.
00757
00758      IF CARRL GREATER ZERO
00759          MOVE CARRI              TO CF-PROCESSOR-CARRIER.
00760
00761  2200-JOURNAL-RECORDS.
00762
00763      MOVE PI-PROCESSOR-ID        TO CF-LAST-MAINT-BY.
00764      MOVE EIBTIME                TO CF-LAST-MAINT-HHMMSS.
00765      MOVE SAVE-BIN-DATE          TO CF-LAST-MAINT-DT.
00766      MOVE 'C'                    TO JP-RECORD-TYPE.
00767      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00768
00769      PERFORM 7800-REWRITE-CONTROL-FILE THRU 7800-EXIT
00770
00771      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
00772
00773      IF PI-USER-ALMIGHTY-YES
00774          MOVE ER-0000            TO EMI-ERROR
00775      ELSE
00776          MOVE ER-7089            TO EMI-ERROR.
00777
00778      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
00779
00780      MOVE SPACES                 TO PI-PREV-USER.
00781      MOVE ZERO                   TO CK-SEQ.
00782      GO TO 1000-SHOW-USER.
00783
00784      EJECT
00785  3000-ADD-USER.
00786
00787      IF USERCDL  EQUAL ZERO
00788          MOVE ER-0019            TO EMI-ERROR
00789          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00790          MOVE -1                 TO USERCDL
00791          MOVE AL-UABON           TO USERCDA
00792          GO TO 8200-SEND-DATAONLY.
00793
00794      IF USERCDI  EQUAL SPACES OR LOW-VALUES
00795          MOVE ER-0019            TO EMI-ERROR
00796          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00797          MOVE -1                 TO USERCDL
00798          MOVE AL-UABON           TO USERCDA
00799          GO TO 8200-SEND-DATAONLY.
00800
00801 ***  IF USERCDI  EQUAL 'LGXX'
00802 ***      MOVE ER-0425            TO EMI-ERROR
00803 ***      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00804 ***      MOVE -1                 TO USERCDL
00805 ***      MOVE AL-UABON           TO USERCDA
00806 ***      GO TO 8200-SEND-DATAONLY.
00807
00808      IF USRPWDL  EQUAL ZERO
00809          MOVE ER-0079            TO EMI-ERROR
00810          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00811          MOVE -1                 TO USRPWDL
00812          MOVE AL-UADON           TO USRPWDA
00813      ELSE
00814          MOVE AL-UADON           TO USRPWDA.
00815
00816      IF CURRTRML  EQUAL ZERO OR
00817         CURRTRMI  EQUAL SPACES
00818          MOVE AL-UANOF           TO CURRTRMA
00819      ELSE
00820          MOVE ER-0080            TO EMI-ERROR
00821          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
00822          MOVE -1                 TO CURRTRML
00823          MOVE AL-UABON           TO CURRTRMA.
00824
00825      PERFORM 6000-EDIT-INPUT-DATA  THRU  6999-EXIT.
00826
00827      IF NOT EMI-NO-ERRORS
00828          GO TO 8200-SEND-DATAONLY.
00829
00830      
      * EXEC CICS  GETMAIN
00831 *        SET      (ADDRESS OF CONTROL-FILE)
00832 *        LENGTH   (750)
00833 *        INITIMG  (GETMAIN-SPACE)
00834 *    END-EXEC.
           MOVE 750
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004018' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00835
00836      MOVE 'CF'                   TO CF-RECORD-ID.
00837      MOVE PI-COMPANY-ID          TO CF-COMPANY-ID.
00838      MOVE '2'                    TO CF-RECORD-TYPE.
00839      MOVE USERCDI                TO CF-PROCESSOR
00840                                     CK-USER-CD.
00841      MOVE +0                     TO CF-SEQUENCE-NO.
00842      MOVE SAVE-BIN-DATE          TO CF-LAST-MAINT-DT.
00843      MOVE PI-PROCESSOR-ID        TO CF-LAST-MAINT-BY.
00844      MOVE EIBTIME                TO CF-LAST-MAINT-HHMMSS.
00845      MOVE +0                     TO CF-PROC-CALC-AMT-TOL
00846                                     CF-PROC-MAX-REG-PMT
00847                                     CF-PROC-MAX-LF-PMT
00848                                     CF-PROC-MAX-REG-DAYS
00849                                     CF-PROC-MAX-AUTO-PMT
00850                                     CF-PROC-MAX-AUTO-MOS
00851                                     CF-PROC-CALC-DAYS-TOL
00852                                     CF-PROC-MAX-EXP-PMT.
00853
00854  3300-INITIALIZE-RECORD.
00855
00856      MOVE 1                      TO SYS
00857                                     SLOT.
00858
00859  3310-SYSTEM-INITIALIZATION.
00860
00861      MOVE 'NN'                   TO
00862                                  CF-ADMINISTRATION-CONTROLS(SYS).
00863
00864      MOVE 'N'                TO  CF-APPLICATION-FORCE (SYS)
00865                                  CF-PROC-SYS-ACCESS-CLAIMS
00866                                  CF-PROC-SYS-ACCESS-CREDIT
00867                                  CF-MESSAGE-AT-LOGON-CAP
00868                                  CF-PROCESSOR-USER-ALMIGHTY
00869                                  CF-LANGUAGE-TYPE
00870                                  CF-PROC-SYS-ACCESS-LIFE.
00871
00872  3320-APP-INITIALIZATION.
00873
00874      MOVE 'NN'                   TO CF-APP-SWITCHES(SYS SLOT).
00875
00876      IF SLOT LESS THAN MAXSLOT
00877          ADD 1                   TO SLOT
00878          GO TO 3320-APP-INITIALIZATION.
00879
00880      IF SYS LESS THAN MAXSYS
00881          ADD 1                   TO SYS
00882          MOVE 1                  TO SLOT
00883          GO TO 3310-SYSTEM-INITIALIZATION.
00884
00885      MOVE CONTROL-FILE           TO SAVE-PROCESSOR-ONE.
00886      MOVE PI-APPLICATION         TO SYS.
00887
00888      IF NAMEL GREATER ZERO
00889          MOVE NAMEI              TO CF-PROCESSOR-NAME.
00890
00891      IF TITLEL GREATER ZERO
00892          MOVE TITLEI             TO CF-PROCESSOR-TITLE.
00893
00894      IF USRPWDL GREATER ZERO
00895          MOVE USRPWDI            TO CF-PROCESSOR-PASSWORD.
00896
00897      IF USRPRNTL GREATER ZERO
00898          MOVE USRPRNTI           TO CF-PROCESSOR-PRINTER.
00899
00900      IF CURRTRML GREATER ZERO
00901          MOVE CURRTRMI           TO CF-CURRENT-TERM-ON.
00902
00903      IF CREDITL GREATER ZERO
00904          MOVE CREDITI            TO CF-PROC-SYS-ACCESS-CREDIT.
00905
00906      IF LANGTYPL GREATER ZERO
00907          MOVE LANGTYPI           TO CF-LANGUAGE-TYPE.
00908
00909      IF CLAIMSL GREATER ZERO
00910          MOVE CLAIMSI            TO CF-PROC-SYS-ACCESS-CLAIMS.
00911
00912      IF LIFEL GREATER ZERO
00913          MOVE LIFEI              TO CF-PROC-SYS-ACCESS-LIFE.
00914
00915      IF LOGONL GREATER ZERO
00916          MOVE LOGONI             TO CF-MESSAGE-AT-LOGON-CAP.
00917
00918      IF USRALML GREATER ZERO
00919          MOVE USRALMI            TO CF-PROCESSOR-USER-ALMIGHTY.
00920
00921      IF APPLVL GREATER ZERO
00922          MOVE APPLVI             TO CF-APPROVAL-LEVEL.
011812
011812     IF CSRL GREATER ZERO
011812         MOVE CSRI               TO CF-CSR-IND
011812     END-IF.
00923
00924 **************************************************************
00925
00926      IF SYS NOT EQUAL 2
00927          GO TO 3350-DONT-BUILD-CLAIMS.
00928
00929 *************   RELEVANT TO CLAIMS SYSTEM ONLY   *************
00930
00931      IF QCDAYSL GREATER ZERO
00932          MOVE QCDAYSI            TO CF-PROC-CALC-DAYS-TOL.
00933
00934      IF QCAMTL GREATER ZERO
00935          MOVE QCAMTI             TO CF-PROC-CALC-AMT-TOL.
00936
00937      IF MAXDAYL GREATER ZERO
00938          MOVE MAXDAYI            TO CF-PROC-MAX-REG-DAYS.
00939
00940      IF MXAHAMTL GREATER ZERO
00941          MOVE MXAHAMTI           TO CF-PROC-MAX-REG-PMT.
00942
00943      IF MXLFAMTL GREATER ZERO
00944          MOVE MXLFAMTI           TO CF-PROC-MAX-LF-PMT.
00945
00946      IF AUTMONL GREATER ZERO
00947          MOVE AUTMONI            TO CF-PROC-MAX-AUTO-MOS.
00948
00949      IF AUTAMTL GREATER ZERO
00950          MOVE AUTAMTI            TO CF-PROC-MAX-AUTO-PMT.
00951
00952      IF MAXEXPL GREATER ZERO
00953          MOVE MAXEXPI            TO CF-PROC-MAX-EXP-PMT.
00954
00955  3350-DONT-BUILD-CLAIMS.
00956
00957      IF READ-PROCESSOR-ONE
00958          MOVE 'A'                TO JP-RECORD-TYPE
00959          MOVE CONTROL-FILE       TO JP-RECORD-AREA
00960                                     SAVE-PROCESSOR-ZERO
00961          PERFORM 7700-WRITE-CONTROL-FILE THRU 7700-EXIT
00962          PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT
00963          MOVE SAVE-PROCESSOR-ONE TO CONTROL-FILE
00964          MOVE PI-READ-SW         TO CF-SEQUENCE-NO.
00965
00966      IF SLCTSYSL GREATER ZERO
00967          MOVE SLCTSYSI           TO
00968                                  CF-ADMINISTRATION-CONTROLS(SYS).
00969
00970      IF FORCEL GREATER ZERO
00971          MOVE FORCEI             TO CF-APPLICATION-FORCE(SYS).
00972
00973      MOVE 1                      TO SLOT.
00974
00975  3400-APPLICATION-LOOP.
00976
00977      IF APP-LENGTH(SLOT) GREATER ZERO
00978          MOVE APP(SLOT)          TO CF-APP-SWITCHES(SYS SLOT).
00979
00980      IF SLOT LESS THAN MAXSLOT
00981          ADD 1                   TO SLOT
00982          GO TO 3400-APPLICATION-LOOP.
00983
00984      IF CARRL GREATER ZERO
00985          MOVE CARRI              TO CF-PROCESSOR-CARRIER.
00986
00987      IF ACCTL GREATER ZERO
00988          MOVE ACCTI              TO CF-PROCESSOR-ACCOUNT.
00989
00990  3500-JOURNAL-RECORDS.
00991
00992      MOVE 'A'                    TO JP-RECORD-TYPE.
00993      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00994
00995      PERFORM 7700-WRITE-CONTROL-FILE THRU 7700-EXIT.
00996
00997      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
00998
00999      IF READ-PROCESSOR-ZERO
01000          MOVE 'A'                TO JP-RECORD-TYPE
01001          MOVE CONTROL-FILE       TO SAVE-PROCESSOR-ZERO
01002          MOVE SAVE-PROCESSOR-ONE TO CONTROL-FILE
01003          MOVE 1                  TO CF-SEQUENCE-NO
01004          MOVE CONTROL-FILE       TO JP-RECORD-AREA
01005          PERFORM 7700-WRITE-CONTROL-FILE THRU 7700-EXIT
01006          PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
01007
01008  3600-ADD-REMINDERS.
01009
01010      MOVE 'R'                    TO CF-RECORD-TYPE.
01011      MOVE +0                     TO CF-SEQUENCE-NO.
01012      MOVE SPACES                 TO CF-RECORD-BODY.
01013      MOVE LOW-VALUES             TO CF-START-REMIND-DT (1)
01014                                     CF-START-REMIND-DT (2)
01015                                     CF-START-REMIND-DT (3)
01016                                     CF-START-REMIND-DT (4)
01017                                     CF-START-REMIND-DT (5)
01018                                     CF-START-REMIND-DT (6)
01019                                     CF-START-REMIND-DT (7)
01020                                     CF-START-REMIND-DT (8)
01021                                     CF-END-REMIND-DT (1)
01022                                     CF-END-REMIND-DT (2)
01023                                     CF-END-REMIND-DT (3)
01024                                     CF-END-REMIND-DT (4)
01025                                     CF-END-REMIND-DT (5)
01026                                     CF-END-REMIND-DT (6)
01027                                     CF-END-REMIND-DT (7)
01028                                     CF-END-REMIND-DT (8).
01029
01030      MOVE 'A'                    TO JP-RECORD-TYPE.
01031      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
01032
01033      PERFORM 7700-WRITE-CONTROL-FILE THRU 7700-EXIT.
01034
01035      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
01036
01037      MOVE ER-0000                TO EMI-ERROR.
01038      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
01039
01040      GO TO 1000-SHOW-USER.
01041
01042      EJECT
01043  4000-DELETE-USER.
01044
01045      
      * EXEC CICS HANDLE CONDITION
01046 *        NOTFND     (4030-END-OF-DELETE)
01047 *    END-EXEC.
      *    MOVE '"$I                   ! # #00004237' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01048
01049      IF USERCDL  EQUAL ZERO
01050          MOVE ER-0019            TO EMI-ERROR
01051          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01052          MOVE -1                 TO USERCDL
01053          MOVE AL-UABON           TO USERCDA
01054          GO TO 8200-SEND-DATAONLY.
01055
01056      IF USERCDI  NOT EQUAL PI-PREV-USER
01057          MOVE ER-0074            TO EMI-ERROR
01058          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01059          MOVE -1                 TO USERCDL
01060          MOVE AL-UABON           TO USERCDA
01061          GO TO 8200-SEND-DATAONLY.
01062
01063      IF USERCDI  EQUAL PI-PROCESSOR-ID
01064          MOVE ER-0083            TO EMI-ERROR
01065          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01066          MOVE -1                 TO MAINTL
01067          GO TO 8200-SEND-DATAONLY.
01068
01069      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
01070      MOVE USERCDI                TO CK-USER-CD.
01071      MOVE '2'                    TO CK-REC-TYPE.
01072
01073      PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT.
01074
01075      IF CF-LAST-MAINT-BY      NOT EQUAL PI-UPDATE-BY OR
01076         CF-LAST-MAINT-HHMMSS  NOT EQUAL PI-UPDATE-HHMMSS
01077          MOVE ER-0068            TO EMI-ERROR
01078          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01079          GO TO 4050-DELETE-UNLOCK.
01080
01081      IF CF-CURRENT-TERM-ON  NOT EQUAL SPACES
01082          MOVE ER-0084            TO EMI-ERROR
01083          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01084          GO TO 4050-DELETE-UNLOCK.
01085
01086      MOVE 'D'                    TO JP-RECORD-TYPE.
01087      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
01088
01089      PERFORM 7900-DELETE-CONTROL-FILE THRU 7900-EXIT.
01090
01091      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
01092
01093      MOVE 1                      TO CK-SEQ.
01094      PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT.
01095      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
01096
01097      PERFORM 7900-DELETE-CONTROL-FILE THRU 7900-EXIT.
01098
01099      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
01100
01101  4020-DELETE-REMINDERS.
01102
01103      MOVE 'R'                    TO CK-REC-TYPE.
01104      MOVE ZERO                   TO CK-SEQ.
01105
01106      PERFORM 7600-READ-CONTROL-FILE-UPDATE THRU 7600-EXIT.
01107
01108      MOVE 'D'                    TO JP-RECORD-TYPE.
01109      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
01110
01111      PERFORM 7900-DELETE-CONTROL-FILE THRU 7900-EXIT.
01112
01113      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.
01114
01115  4030-END-OF-DELETE.
01116
01117      MOVE ER-9014                TO EMI-ERROR.
01118      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
01119
01120      MOVE LOW-VALUES             TO EL103AO.
01121      MOVE -1                     TO MAINTL.
01122      MOVE AL-UANOF               TO MAINTA.
01123      MOVE SPACES                 TO MAINTI
01124                                     PI-PREV-USER.
01125      MOVE CK-USER-CD             TO USERCDO.
01126      MOVE AL-UANON               TO USERCDA.
01127
01128      GO TO 8100-SEND-INITIAL-MAP.
01129
01130  4050-DELETE-UNLOCK.
01131      
      * EXEC CICS  UNLOCK
01132 *        DATASET  ('ELCNTL')
01133 *    END-EXEC
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&*                    #   #00004323' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01134
01135      GO TO 1000-SHOW-USER.
01136
01137      EJECT
01138  5000-FIND-NEXT-USER.
01139
01140      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
01141      MOVE 2                      TO CK-SEQ.
01142
01143      IF USERCDL  EQUAL ZERO
01144          MOVE PI-PREV-USER       TO CK-USER-CD
01145      ELSE
01146          MOVE USERCDI            TO CK-USER-CD.
01147
01148      MOVE '2'                    TO CK-REC-TYPE.
01149
01150      
      * EXEC CICS  HANDLE CONDITION
01151 *        NOTFND  (5010-USER-NOT-FOUND)
01152 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00004342' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01153
01154      PERFORM 7510-READ-CONTROL-FILE-GTEQ THRU 7510-EXIT.
01155
01156      IF CF-COMPANY-ID   NOT EQUAL PI-COMPANY-ID OR
01157         CF-RECORD-TYPE  NOT EQUAL '2'
01158         GO TO 5010-USER-NOT-FOUND.
01159
01160      IF READ-PROCESSOR-ONE
01161          MOVE CONTROL-FILE       TO SAVE-PROCESSOR-ZERO
01162          MOVE CF-CONTROL-PRIMARY TO ELCNTL-KEY
01163          ADD +1                  TO CK-SEQ
01164          PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.
01165
01166      IF PI-PREV-USER EQUAL SPACES
01167          MOVE ER-0085            TO EMI-ERROR
01168          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
01169
01170      GO TO 7000-BUILD-OUTPUT-MAP.
01171
01172  5010-USER-NOT-FOUND.
01173
01174      GO TO 8860-ENDFILE.
01175
01176      EJECT
01177  5100-FIND-PREV-USER.
01178
01179      MOVE '1'                    TO READ-PREV-SW.
01180      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
01181
01182      IF USERCDL  EQUAL ZERO
01183          MOVE PI-PREV-USER       TO CK-USER-CD
01184      ELSE
01185          MOVE USERCDI            TO CK-USER-CD.
01186
01187      MOVE ZERO                   TO CK-SEQ.
01188      MOVE '2'                    TO CK-REC-TYPE.
01189
01190      
      * EXEC CICS  HANDLE CONDITION
01191 *        NOTFND  (8860-ENDFILE)
01192 *    END-EXEC.
      *    MOVE '"$I                   ! % #00004382' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01193
01194      
      * EXEC CICS STARTBR
01195 *        DATASET  ('ELCNTL')
01196 *        RIDFLD   (ELCNTL-KEY)
01197 *        GTEQ
01198 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004386' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01199
01200  5100-READPREV-LOOP.
01201
01202      
      * EXEC CICS  READPREV
01203 *        DATASET  ('ELCNTL')
01204 *        SET      (ADDRESS OF CONTROL-FILE)
01205 *        RIDFLD   (ELCNTL-KEY)
01206 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004394' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01207
01208      IF CF-COMPANY-ID   NOT EQUAL PI-COMPANY-ID OR
01209         CF-RECORD-TYPE  NOT EQUAL '2'
01210         PERFORM 5100-END-BROWSE
01211         GO TO 8860-ENDFILE.
01212
01213      IF FIRST-READ-PREV
01214          MOVE '2'                TO READ-PREV-SW
01215          GO TO 5100-READPREV-LOOP.
01216
01217      IF SECOND-READ-PREV
01218          MOVE '3'                TO READ-PREV-SW
01219          IF READ-PROCESSOR-ONE
01220              MOVE CONTROL-FILE   TO SAVE-PROCESSOR-ONE
01221              GO TO 5100-READPREV-LOOP
01222          ELSE
01223              GO TO 5100-READPREV-LOOP.
01224
01225  5100-END-BROWSE.
01226
01227      
      * EXEC CICS ENDBR
01228 *        DATASET ('ELCNTL')
01229 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004419' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01230
01231  5100-EXIT-ROUTINE.
01232
01233      PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.
01234
01235      IF READ-PROCESSOR-ONE
01236          MOVE CONTROL-FILE       TO SAVE-PROCESSOR-ZERO
01237          MOVE SAVE-PROCESSOR-ONE TO CONTROL-FILE.
01238
01239      GO TO 7000-BUILD-OUTPUT-MAP.
01240
01241      EJECT
01242  6000-EDIT-INPUT-DATA.
01243
01244      IF CARRL  EQUAL ZERO
01245          GO TO 6200-EDIT-ACCT
01246      ELSE
01247          IF CARRI  EQUAL SPACES
01248              MOVE SPACES         TO ACCTI
01249              MOVE AL-UANON       TO CARRA
01250                                     ACCTA
01251              GO TO 6300-CONT.
01252
01253      MOVE PI-COMPANY-ID          TO CK-COMP-ID.
01254      MOVE SPACES                 TO CK-CARRIER.
01255      MOVE CARRI                  TO CK-CARR.
01256      MOVE '6'                    TO CK-REC-TYPE.
01257
01258      
      * EXEC CICS  HANDLE CONDITION
01259 *        NOTFND  (6100-CARRIER-MISSING)
01260 *    END-EXEC.
      *    MOVE '"$I                   ! & #00004450' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034343530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01261
01262      PERFORM 7500-READ-CONTROL-FILE THRU 7500-EXIT.
01263
01264      MOVE AL-UANON               TO CARRA.
01265
01266      GO TO 6200-EDIT-ACCT.
01267
01268  6100-CARRIER-MISSING.
01269
01270      MOVE -1                     TO CARRL.
01271      MOVE AL-UABON               TO CARRA.
01272      MOVE ER-0252                TO EMI-ERROR.
01273      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
01274
01275  6200-EDIT-ACCT.
01276
01277      IF NOT MORTGAGE-SESSION
01278          IF ACCTL GREATER ZERO
01279              IF CARRL GREATER ZERO
01280                  MOVE AL-UANON       TO ACCTA
01281              ELSE
01282                  IF ST-ACCNT-CNTL OR ACCNT-CNTL
01283                      NEXT SENTENCE
01284                  ELSE
01285                      MOVE ER-2376    TO EMI-ERROR
01286                      MOVE -1         TO CARRL
01287                      MOVE AL-UABON   TO CARRA
01288                      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
01289 *    ELSE
01290 *        MOVE AL-UANON               TO ACCTA.
01291
01292  6300-CONT.
01293
01294      IF CURRTRML GREATER ZERO
01295          MOVE CURRTRMI           TO TERMINAL-TEST-AREA
01296          IF TERMINAL-TEST-AREA  EQUAL SPACES
01297              MOVE AL-UANON       TO CURRTRMA
01298          ELSE
01299              IF ' '  EQUAL TTA-1 OR TTA-2 OR
01300                            TTA-3 OR TTA-4
01301                  MOVE -1         TO CURRTRML
01302                  MOVE AL-UABON   TO CURRTRMA
01303                  MOVE ER-0063    TO EMI-ERROR
01304                  PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01305              ELSE
01306                  MOVE AL-UANON   TO CURRTRMA.
01307
01308      IF CREDITL GREATER ZERO
01309          IF CREDITI  NOT EQUAL 'Y' AND 'N'
01310              MOVE -1             TO CREDITL
01311              MOVE AL-UABON       TO CREDITA
01312              MOVE ER-0075        TO EMI-ERROR
01313              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01314          ELSE
01315              MOVE AL-UANON       TO CREDITA.
01316
01317      IF CLAIMSL GREATER ZERO
01318          IF CLAIMSI  NOT EQUAL 'Y' AND 'N'
01319              MOVE -1             TO CLAIMSL
01320              MOVE AL-UABON       TO CLAIMSA
01321              MOVE ER-0075        TO EMI-ERROR
01322              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01323          ELSE
01324              MOVE AL-UANON       TO CLAIMSA.
01325
01326      IF LIFEL GREATER ZERO
01327          IF LIFEI  NOT EQUAL 'Y' AND 'N'
01328              MOVE -1             TO LIFEL
01329              MOVE AL-UABON       TO LIFEA
01330              MOVE ER-0075        TO EMI-ERROR
01331              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01332          ELSE
01333              MOVE AL-UANON       TO LIFEA.
01334
01335      IF LOGONL GREATER ZERO
01336          IF LOGONI  NOT EQUAL 'Y' AND 'N'
01337              MOVE -1             TO LOGONL
01338              MOVE AL-UABON       TO LOGONA
01339              MOVE ER-0075        TO EMI-ERROR
01340              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01341          ELSE
01342              MOVE AL-UANON       TO LOGONA.
01343
01344
01345      IF USRPWDL  GREATER ZERO
01346          MOVE SPACES TO  WS-SAVE-PASSWORD
01347          MOVE 'N'    TO  WS-PASSWORD-NUMERIC
01348          MOVE 'N'    TO  WS-PASSWORD-SIXDIGIT
01349          MOVE USRPWDI  TO  WS-SAVE-PASSWORD
01350          IF (WS-PASSWORD-X1 > SPACE) AND
01351             (WS-PASSWORD-X2 > SPACE) AND
01352             (WS-PASSWORD-X3 > SPACE) AND
01353             (WS-PASSWORD-X4 > SPACE) AND
01354             (WS-PASSWORD-X5 > SPACE) AND
01355             (WS-PASSWORD-X6 > SPACE)
01356             MOVE 'Y' TO  WS-PASSWORD-SIXDIGIT
01357          END-IF
01358          IF (WS-PASSWORD-N1  NUMERIC) OR
01359             (WS-PASSWORD-N2  NUMERIC) OR
01360             (WS-PASSWORD-N3  NUMERIC) OR
01361             (WS-PASSWORD-N4  NUMERIC) OR
01362             (WS-PASSWORD-N5  NUMERIC) OR
01363             (WS-PASSWORD-N6  NUMERIC) OR
01364             (WS-PASSWORD-N7  NUMERIC) OR
01365             (WS-PASSWORD-N8  NUMERIC) OR
01366             (WS-PASSWORD-N9  NUMERIC) OR
01367             (WS-PASSWORD-N10 NUMERIC) OR
01368             (WS-PASSWORD-N11 NUMERIC)
01369             MOVE 'Y' TO  WS-PASSWORD-NUMERIC
01370          END-IF
01371          IF WS-PASSWORD-NUMERIC = 'N'  OR
01372             WS-PASSWORD-SIXDIGIT = 'N'
01373              MOVE -1             TO USRPWDL
01374              MOVE AL-UADON       TO USRPWDA
01375              MOVE ER-2549        TO EMI-ERROR
01376              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01377          ELSE
01378              MOVE AL-UADON       TO USRPWDA
01379          END-IF
01380          IF NEWPWDI NOT EQUAL  WS-SAVE-PASSWORD
01381             MOVE SPACES         TO NEWPWDO
01382             MOVE -1             TO NEWPWDL
01383             MOVE AL-UADON       TO NEWPWDA
01384             MOVE ER-8026        TO EMI-ERROR
01385             PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01386          ELSE
01387              MOVE AL-UADON       TO NEWPWDA
01388          END-IF
01389      ELSE
01390         IF NEWPWDL  GREATER ZERO
01391            MOVE SPACES         TO NEWPWDO
01392            MOVE -1             TO NEWPWDL
01393            MOVE AL-UADON       TO NEWPWDA
01394            MOVE ER-2548        TO EMI-ERROR
01395            PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
01396
01397
01398      IF LANGTYPL GREATER ZERO
01399          IF LANGTYPI NOT EQUAL 'F' AND 'E' AND ' '
01400              MOVE -1             TO LANGTYPL
01401              MOVE AL-UABON       TO LANGTYPA
01402              MOVE ER-9946        TO EMI-ERROR
01403              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01404          ELSE
01405              MOVE AL-UANON       TO LANGTYPA.
01406
01407      IF USRALML GREATER ZERO
01408          IF USRALMI  NOT EQUAL 'Y' AND 'N'
01409              MOVE ER-0075        TO EMI-ERROR
01410              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01411              MOVE -1             TO USRALML
01412              MOVE AL-UABON       TO USRALMA
01413          ELSE
01414              MOVE AL-UANON       TO USRALMA.
01415
01416      IF APPLVL GREATER ZERO
01417          IF APPLVI NOT = '1' AND '2' AND '3' AND ' '
091813               AND '4' AND '5'
01418              MOVE ER-1889        TO EMI-ERROR
01419              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01420              MOVE -1             TO APPLVL
01421              MOVE AL-UABON       TO APPLVA
01422          ELSE
01423              MOVE AL-UANON       TO APPLVA.
01424
01425      IF SLCTSYSI GREATER ZERO
01426          IF SLCTSYSI  EQUAL 'YY' OR 'YN' OR 'NN'
01427              MOVE AL-UANON       TO SLCTSYSA
01428          ELSE
01429              MOVE ER-7087        TO EMI-ERROR
01430              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01431              MOVE -1             TO SLCTSYSL
01432              MOVE AL-UABON       TO SLCTSYSA.
01433
01434      IF FORCEL GREATER ZERO
01435          IF FORCEI    NOT EQUAL 'Y' AND 'N'
01436              MOVE ER-0075        TO EMI-ERROR
01437              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01438              MOVE -1             TO FORCEL
01439              MOVE AL-UABON       TO FORCEA
01440          ELSE
01441              MOVE AL-UANON       TO FORCEA.
01442
01443      MOVE PI-APPLICATION         TO SYS.
01444      MOVE 1                      TO SLOT.
01445
01446  6400-APPLICATION-LOOP.
01447
01448      IF APP-LENGTH (SLOT)  NOT EQUAL ZERO
01449          IF APP (SLOT)  EQUAL 'YY' OR 'YN' OR 'NN'
01450              MOVE AL-UANON       TO APP-ATTRB (SLOT)
01451          ELSE
01452              MOVE ER-7084        TO EMI-ERROR
01453              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01454              MOVE -1             TO APP-LENGTH (SLOT)
01455              MOVE AL-UABON       TO APP-ATTRB (SLOT).
01456
01457      IF SLOT LESS THAN MAXSLOT
01458          ADD 1                   TO SLOT
01459          GO TO 6400-APPLICATION-LOOP.
01460
01461 **********************************************
01462 *  CHECK TO SEE IF ADDITIONAL CLAIMS FIELDS  *
01463 *  SHOULD BE EDITED.                         *
01464 **********************************************
01465
01466      IF NOT CLAIMS-APP
01467          GO TO 6999-EXIT.
01468
01469      IF QCDAYSL GREATER ZERO AND
01470         QCDAYSI  EQUAL SPACES
01471          MOVE 0                  TO  QCDAYSI
01472          MOVE AL-UNNON           TO  QCDAYSA
01473      ELSE
01474          IF QCDAYSL GREATER ZERO
01475              IF QCDAYSI NOT NUMERIC
01476                  MOVE -1         TO  QCDAYSL
01477                  MOVE AL-UNBON   TO  QCDAYSA
01478                  MOVE ER-0082    TO EMI-ERROR
01479                  PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01480              ELSE
01481                  MOVE AL-UNNON   TO  QCDAYSA.
01482
01483      IF QCAMTL GREATER ZERO
01484          
      * EXEC CICS  BIF DEEDIT
01485 *            FIELD   (QCAMTI)
01486 *            LENGTH  (7)
01487 *        END-EXEC
           MOVE 7
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004677' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QCAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01488          IF QCAMTI LESS ZERO
01489              MOVE -1             TO  QCAMTL
01490              MOVE AL-UNBON       TO  QCAMTA
01491              MOVE ER-0078        TO EMI-ERROR
01492              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01493          ELSE
01494              MOVE AL-UNNON       TO  QCAMTA.
01495
01496      IF MAXDAYL  GREATER ZERO AND
01497         MAXDAYI  EQUAL SPACES
01498          MOVE 0                  TO MAXDAYI
01499          MOVE AL-UNNON           TO MAXDAYA
01500      ELSE
01501          IF MAXDAYL GREATER ZERO
01502              IF MAXDAYI NOT NUMERIC
01503                  MOVE -1         TO MAXDAYL
01504                  MOVE AL-UNBON   TO MAXDAYA
01505                  MOVE ER-0082    TO EMI-ERROR
01506                  PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01507              ELSE
01508                  MOVE AL-UNNON   TO MAXDAYA.
01509
01510      IF MXAHAMTL GREATER ZERO
01511          
      * EXEC CICS  BIF DEEDIT
01512 *            FIELD   (MXAHAMTI)
01513 *            LENGTH  (11)
01514 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004704' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MXAHAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01515          IF MXAHAMTI LESS ZERO
01516              MOVE -1             TO MXAHAMTL
01517              MOVE AL-UNBON       TO MXAHAMTA
01518              MOVE ER-0078        TO EMI-ERROR
01519              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01520          ELSE
01521              MOVE AL-UNNON       TO MXAHAMTA.
01522
01523      IF MXLFAMTL GREATER ZERO
01524          
      * EXEC CICS  BIF DEEDIT
01525 *            FIELD   (MXLFAMTI)
01526 *            LENGTH  (11)
01527 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004717' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MXLFAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01528          IF MXLFAMTI LESS ZERO
01529              MOVE -1             TO MXLFAMTL
01530              MOVE AL-UNBON       TO MXLFAMTA
01531              MOVE ER-0078        TO EMI-ERROR
01532              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01533          ELSE
01534              MOVE AL-UNNON       TO MXLFAMTA.
01535
01536      IF AUTMONL  GREATER ZERO AND
01537         AUTMONI  EQUAL SPACES
01538          MOVE 0                  TO  AUTMONI
01539          MOVE AL-UNNON           TO  AUTMONA
01540      ELSE
01541          IF AUTMONL GREATER ZERO
01542              IF AUTMONI  NOT NUMERIC
01543                  MOVE -1         TO  AUTMONL
01544                  MOVE AL-UNBON   TO  AUTMONA
01545                  MOVE ER-0077    TO EMI-ERROR
01546                  PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01547              ELSE
01548                  MOVE AL-UNNON   TO  AUTMONA.
01549
01550      IF AUTAMTL GREATER ZERO
01551          
      * EXEC CICS  BIF DEEDIT
01552 *            FIELD   (AUTAMTI)
01553 *            LENGTH  (11)
01554 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004744' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AUTAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01555          IF AUTAMTI  LESS  ZERO
01556              MOVE -1             TO  AUTAMTL
01557              MOVE AL-UNBON       TO  AUTAMTA
01558              MOVE ER-0078        TO EMI-ERROR
01559              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01560          ELSE
01561              MOVE AL-UNNON       TO  AUTAMTA.
01562
01563      IF MAXEXPL GREATER ZERO
01564          
      * EXEC CICS BIF DEEDIT
01565 *            FIELD   (MAXEXPI)
01566 *            LENGTH  (11)
01567 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004757' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAXEXPI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01568          IF MAXEXPI LESS ZERO
01569              MOVE -1             TO MAXEXPL
01570              MOVE AL-UNBON       TO MAXEXPA
01571              MOVE ER-0078        TO EMI-ERROR
01572              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT
01573          ELSE
01574              MOVE AL-UNNON       TO MAXEXPA.
01575
01576      IF EMI-NO-ERRORS
01577          GO TO 6999-EXIT.
01578
01579      IF QCAMTL GREATER ZERO
01580          MOVE QCAMTI             TO QCAMTO.
01581
01582      IF MXAHAMTL GREATER ZERO
01583          MOVE MXAHAMTI           TO MXAHAMTO.
01584
01585      IF MXLFAMTL GREATER ZERO
01586          MOVE MXLFAMTI           TO MXLFAMTO.
01587
01588      IF AUTAMTL GREATER ZERO
01589          MOVE AUTAMTI            TO AUTAMTO.
01590
01591      IF MAXEXPL GREATER ZERO
01592          MOVE MAXEXPI            TO MAXEXPO.
01593
01594  6999-EXIT.
01595      EXIT.
01596
01597      EJECT
01598  7000-BUILD-OUTPUT-MAP.
01599
01600      MOVE LOW-VALUES             TO EL103AO.
01601      MOVE -1                     TO MAINTL.
01602      MOVE AL-UANOF               TO MAINTA.
01603      MOVE SPACES                 TO MAINTI.
01604
01605      MOVE PI-APPLICATION         TO SYS.
01606
01607      IF READ-PROCESSOR-ONE
01608          COMPUTE SYS EQUAL SYS + +4
01609          MOVE DESCRIPT(SYS)      TO DESCRO
01610          COMPUTE SYS EQUAL SYS - +4
01611      ELSE
01612          MOVE DESCRIPT(SYS)      TO DESCRO.
01613
01614      MOVE CF-ADMINISTRATION-CONTROLS(SYS)
01615                                  TO SLCTSYSO.
01616      MOVE AL-UANON               TO SLCTSYSA.
01617
01618      MOVE CF-APPLICATION-FORCE(SYS)
01619                                  TO FORCEO.
01620      MOVE AL-UANON               TO FORCEA.
01621
01622      MOVE 1                      TO SLOT.
01623
01624  7100-APPLICATION-LOOP.
01625
01626      MOVE CF-APP-SWITCHES(SYS SLOT)
01627                                  TO APP (SLOT).
01628      MOVE AL-UANON               TO APP-ATTRB (SLOT).
01629
01630      IF SLOT   LESS   MAXSLOT
01631          ADD 1                   TO SLOT
01632          GO TO 7100-APPLICATION-LOOP.
01633
01634      IF NO-CARRIER-SECURITY
01635          MOVE SPACES                TO CARRO
01636          MOVE AL-UANOF              TO CARRA
01637      ELSE
01638          MOVE CF-PROCESSOR-CARRIER  TO CARRO
01639          MOVE AL-UANON              TO CARRA.
01640
01641      IF NO-ACCOUNT-SECURITY
01642          MOVE AL-UANOF              TO ACCTA
01643      ELSE
01644          MOVE CF-PROCESSOR-ACCOUNT  TO  ACCTO
01645          MOVE AL-UANON              TO  ACCTA.
01646
01647      IF READ-PROCESSOR-ONE
01648          MOVE SAVE-PROCESSOR-ZERO
01649                                  TO CONTROL-FILE.
01650
01651      MOVE CF-PROCESSOR           TO USERCDO
01652                                     PI-PREV-USER.
01653      MOVE AL-UANON               TO USERCDA.
01654
01655      MOVE CF-PROCESSOR-NAME      TO NAMEO.
01656      MOVE AL-UANON               TO NAMEA.
01657
01658      MOVE CF-PROCESSOR-TITLE     TO TITLEO.
01659      MOVE AL-UANON               TO TITLEA.
01660
01661      MOVE CF-APPROVAL-LEVEL      TO APPLVO.
01662      MOVE AL-UANON               TO APPLVA.
011812
011812     MOVE CF-CSR-IND             TO CSRO.
011812     MOVE AL-UANON               TO CSRA.
01663
01664      MOVE CF-LAST-MAINT-BY       TO LSTUSRO
01665                                     PI-UPDATE-BY.
01666      MOVE CF-LAST-MAINT-HHMMSS   TO TIME-IN
01667                                     PI-UPDATE-HHMMSS.
01668      MOVE TIME-OUT               TO LSTTIMEO.
01669      MOVE ' '                    TO DC-OPTION-CODE.
01670      MOVE CF-LAST-MAINT-DT       TO DC-BIN-DATE-1.
01671      MOVE LINK-ELDATCV           TO PGM-NAME.
01672
01673      
      * EXEC CICS  LINK
01674 *        PROGRAM   (PGM-NAME)
01675 *        COMMAREA  (DATE-CONVERSION-DATA)
01676 *        LENGTH    (DC-COMM-LENGTH)
01677 *    END-EXEC.
      *    MOVE '."C                   (   #00004869' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01678
01679      IF DATE-CONVERSION-ERROR
01680          MOVE ZEROS              TO LSTDTEO
01681      ELSE
01682          MOVE DC-GREG-DATE-1-EDIT  TO LSTDTEO.
01683
01684      MOVE CF-PROCESSOR-PRINTER   TO USRPRNTO.
01685
01686      IF PI-COMPANY-ID EQUAL 'SLI' AND
01687        (PI-PROCESSOR-ID NOT EQUAL USERCDI)
01688          MOVE SPACES             TO USRPWDO
01689          MOVE SPACES             TO NEWPWDO
01690      ELSE
01691          IF PI-USER-ALMIGHTY-YES  OR
01692             PI-PROCESSOR-ID EQUAL CF-PROCESSOR
01693              MOVE AL-UADOF       TO USRPWDA
01694          ELSE
01695              MOVE AL-UADOF       TO USRPWDA.
01696
01697      MOVE CF-CURRENT-TERM-ON     TO CURRTRMO.
01698      MOVE AL-UANON               TO CURRTRMA.
01699
01700      MOVE CF-PROC-SYS-ACCESS-CREDIT
01701                                  TO CREDITO.
01702
01703      MOVE CF-PROC-SYS-ACCESS-CLAIMS
01704                                  TO CLAIMSO.
01705
01706      MOVE CF-LANGUAGE-TYPE
01707                                  TO LANGTYPO.
01708
01709      MOVE CF-PROC-SYS-ACCESS-LIFE
01710                                  TO LIFEO.
01711
01712      MOVE CF-MESSAGE-AT-LOGON-CAP
01713                                  TO LOGONO.
01714
01715      MOVE CF-PROCESSOR-USER-ALMIGHTY
01716                                  TO USRALMO.
01717
01718  7200-SPECIAL-CLAIMS-FIELDS.
01719
01720      IF SYS EQUAL 2
01721          MOVE CF-PROC-CALC-DAYS-TOL
01722                                  TO QCDAYSO
01723          MOVE AL-UANON           TO QCDAYSA
01724          MOVE CF-PROC-CALC-AMT-TOL
01725                                  TO QCAMTO
01726          MOVE AL-UANON           TO QCAMTA
01727          MOVE CF-PROC-MAX-REG-DAYS
01728                                  TO MAXDAYO
01729          MOVE AL-UANON           TO MAXDAYA
01730          MOVE CF-PROC-MAX-REG-PMT
01731                                  TO MXAHAMTO
01732          MOVE AL-UANON           TO MXAHAMTA
01733          MOVE CF-PROC-MAX-LF-PMT TO MXLFAMTO
01734          MOVE AL-UANON           TO MXLFAMTA
01735          MOVE CF-PROC-MAX-AUTO-MOS
01736                                  TO AUTMONO
01737          MOVE AL-UANON           TO AUTMONA
01738          MOVE CF-PROC-MAX-AUTO-PMT
01739                                  TO AUTAMTO
01740          MOVE AL-UANON           TO AUTAMTA
01741          IF CF-PROC-MAX-EXP-PMT NOT NUMERIC
01742              MOVE 0              TO MAXEXPO
01743           ELSE
01744              MOVE CF-PROC-MAX-EXP-PMT
01745                                  TO MAXEXPO
01746          END-IF
01747          MOVE AL-UANON           TO MAXEXPA
01748          MOVE WS-QCDAYS          TO XCDAYSI
01749          MOVE WS-AH              TO XAHI
01750          MOVE WS-LIFE            TO XLIFEI
01751          MOVE WS-MAXDAYS         TO XAXDAYI
01752          MOVE WS-AUTO-PAY        TO XUTMONI
01753          MOVE WS-AMOUNT          TO XAMT1I
01754                                     XAMT2I
01755                                     EXPAMTI
01756          MOVE WS-EXPENSE         TO XEXP1I
01757          MOVE AL-SANON           TO XAMT1A
01758                                     XAMT2A
01759                                     XUTMONA
01760                                     XCDAYSA
01761                                     XAHA
01762                                     XLIFEA
01763                                     XAXDAYA
01764                                     XEXP1A
01765                                     EXPAMTA
01766      ELSE
01767          MOVE AL-SADOF           TO XCDAYSA
01768                                     QCDAYSA
01769                                     XAHA
01770                                     QCAMTA
01771                                     XLIFEA
01772                                     XAXDAYA
01773                                     MAXDAYA
01774                                     XAMT1A
01775                                     XAMT2A
01776                                     MXAHAMTA
01777                                     MXLFAMTA
01778                                     XUTMONA
01779                                     AUTMONA
01780                                     AUTAMTA
01781                                     XEXP1A
01782                                     EXPAMTA.
01783
01784      MOVE AL-SANON               TO PFCRDTA
01785                                     PFCARDA
01786                                     PFCLMSA
01787                                     PFMORTA
01788                                     PFRECVA.
01789
01790      IF CREDIT-APP
01791          IF READ-PROCESSOR-ZERO
01792              MOVE AL-SABON       TO PFCRDTA.
01793
01794      IF CLAIMS-APP
01795          IF READ-PROCESSOR-ZERO
01796              MOVE AL-SABON       TO PFCLMSA.
01797
01798      IF CREDIT-CARD-APP
01799          IF READ-PROCESSOR-ZERO
01800              MOVE AL-SABON       TO PFCARDA.
01801
01802      IF ACCT-RECV-APP
01803          IF READ-PROCESSOR-ZERO
01804              MOVE AL-SABON       TO PFRECVA.
01805
01806      IF MORTGAGE-APP
01807          IF READ-PROCESSOR-ONE
01808              MOVE AL-SABON       TO PFMORTA.
01809
01810      MOVE AL-UNNOF               TO ENTERPFA.
01811
01812      GO TO 8100-SEND-INITIAL-MAP.
01813
01814      EJECT
01815  7500-READ-CONTROL-FILE.
01816
01817      
      * EXEC CICS  READ
01818 *        DATASET  ('ELCNTL')
01819 *        SET      (ADDRESS OF CONTROL-FILE)
01820 *        RIDFLD   (ELCNTL-KEY)
01821 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00005013' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01822
01823  7500-EXIT.
01824      EXIT.
01825
01826  7510-READ-CONTROL-FILE-GTEQ.
01827
01828      
      * EXEC CICS  READ
01829 *        DATASET  ('ELCNTL')
01830 *        SET      (ADDRESS OF CONTROL-FILE)
01831 *        RIDFLD   (ELCNTL-KEY)
01832 *        GTEQ
01833 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00005024' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01834
01835  7510-EXIT.
01836      EXIT.
01837
01838  7600-READ-CONTROL-FILE-UPDATE.
01839
01840      
      * EXEC CICS  READ
01841 *        UPDATE
01842 *        DATASET  ('ELCNTL')
01843 *        SET      (ADDRESS OF CONTROL-FILE)
01844 *        RIDFLD   (ELCNTL-KEY)
01845 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00005036' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01846
01847  7600-EXIT.
01848      EXIT.
01849
01850  7700-WRITE-CONTROL-FILE.
01851
01852      
      * EXEC CICS  WRITE
01853 *        FROM     (CONTROL-FILE)
01854 *        DATASET  ('ELCNTL')
01855 *        RIDFLD   (CF-CONTROL-PRIMARY)
01856 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00005048' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01857
01858  7700-EXIT.
01859      EXIT.
01860
01861  7800-REWRITE-CONTROL-FILE.
01862
01863      
      * EXEC CICS  REWRITE
01864 *        DATASET  ('ELCNTL')
01865 *        FROM     (CONTROL-FILE)
01866 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %   #00005059' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01867
01868  7800-EXIT.
01869      EXIT.
01870
01871  7900-DELETE-CONTROL-FILE.
01872
01873      
      * EXEC CICS  DELETE
01874 *        DATASET  ('ELCNTL')
01875 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&(                    &   #00005069' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01876
01877  7900-EXIT.
01878      EXIT.
01879
01880      EJECT
01881  8100-SEND-INITIAL-MAP.
01882
01883      IF ST-ACCNT-CNTL
01884        OR  ACCNT-CNTL
01885          MOVE AL-SADOF           TO CARRA
01886                                     CARRDESA.
01887
01888      IF PI-VALID-APP
01889          GO TO 8110-APP-ASSIGNED.
01890
01891      IF CREDIT-SESSION
01892          MOVE 1                  TO PI-APPLICATION.
01893
01894      IF MORTGAGE-SESSION
01895          MOVE 1                  TO PI-APPLICATION.
01896
01897      IF CLAIM-SESSION
01898          MOVE 2                  TO PI-APPLICATION.
01899
01900  8110-APP-ASSIGNED.
01901
01902      IF READ-PROCESSOR-ZERO
01903          MOVE CREDIT-SCRTY       TO SCRTYO
01904      ELSE
01905          MOVE MORTGAGE-SCRTY     TO SCRTYO.
01906
01907      MOVE SAVE-DATE              TO RUNDTEO.
01908      MOVE EIBTIME                TO TIME-IN.
01909      MOVE TIME-OUT               TO RUNTIMEO.
01910      MOVE PI-COMPANY-ID          TO COMPO.
01911      MOVE -1                     TO MAINTL.
01912      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
01913
CIDMOD     IF PI-COMPANY-ID = 'CID'
CIDMOD         IF PI-USER-ALMIGHTY-YES
CIDMOD             MOVE AL-UANON       TO USRPWDA
CIDMOD                                    NEWPWDA
CIDMOD         ELSE
CIDMOD             MOVE AL-UADON       TO USRPWDA
CIDMOD                                    NEWPWDA
CIDMOD         END-IF
CIDMOD     END-IF
01914
01914      
      * EXEC CICS  SEND
01915 *        MAP     (MAP-NAME)
01916 *        MAPSET  (MAPSET-NAME)
01917 *        FROM    (EL103AO)
01918 *        ERASE
01919 *        CURSOR
01920 *    END-EXEC.
           MOVE LENGTH OF
            EL103AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005120' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL103AO, 
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
           
01921
01922      GO TO 9100-RETURN-TRAN.
01923
01924  8200-SEND-DATAONLY.
01925
01926      IF ST-ACCNT-CNTL
01927        OR  ACCNT-CNTL
01928          MOVE AL-SADOF           TO CARRA
01929                                     CARRDESA.
01930
01931      IF READ-PROCESSOR-ZERO
01932          MOVE CREDIT-SCRTY       TO SCRTYO
01933      ELSE
01934          MOVE MORTGAGE-SCRTY     TO SCRTYO.
01935
01936      MOVE SAVE-DATE              TO RUNDTEO.
01937      MOVE EIBTIME                TO TIME-IN.
01938      MOVE TIME-OUT               TO RUNTIMEO.
01939      MOVE PI-COMPANY-ID          TO COMPO.
01940      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
01941
CIDMOD     IF PI-COMPANY-ID = 'CID'
CIDMOD         IF PI-USER-ALMIGHTY-YES
CIDMOD             MOVE AL-UANON       TO USRPWDA
CIDMOD                                    NEWPWDA
CIDMOD         ELSE
CIDMOD             MOVE AL-UADON       TO USRPWDA
CIDMOD                                    NEWPWDA
CIDMOD         END-IF
CIDMOD     END-IF
CIDMOD
01942      
      * EXEC CICS  SEND
01943 *        MAP     (MAP-NAME)
01944 *        MAPSET  (MAPSET-NAME)
01945 *        FROM    (EL103AO)
01946 *        DATAONLY
01947 *        CURSOR
01948 *    END-EXEC.
           MOVE LENGTH OF
            EL103AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005158' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL103AO, 
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
           
01949
01950      GO TO 9100-RETURN-TRAN.
01951
01952  8300-SEND-TEXT.
01953      
      * EXEC CICS  SEND TEXT
01954 *        FROM    (LOGOFF-TEXT)
01955 *        LENGTH  (LOGOFF-LENGTH)
01956 *        ERASE
01957 *        FREEKB
01958 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005169' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313639' TO DFHEIV0(25:11)
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
           
01959
01960      
      * EXEC CICS  RETURN
01961 *    END-EXEC.
      *    MOVE '.(                    ''   #00005176' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01962
01963  8400-LOG-JOURNAL-RECORD.
01964 *    IF PI-JOURNAL-FILE-ID  EQUAL ZERO
01965 *        GO TO 8499-EXIT.
01966 *
01967 *    MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
01968 *    MOVE FILE-ID                TO JP-FILE-ID.
01969 *    MOVE THIS-PGM               TO JP-PROGRAM-ID.
01970 *
01971 *    EXEC CICS  JOURNAL
01972 *        JFILEID  (PI-JOURNAL-FILE-ID)
01973 *        JTYPEID  ('EL')
01974 *        FROM     (JOURNAL-RECORD)
01975 *        LENGTH   (773)
01976 *    END-EXEC.
01977 *
01978  8499-EXIT.
01979      EXIT.
01980
01981  8800-UNAUTHORIZED-ACCESS.
01982      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01983
01984      GO TO 8300-SEND-TEXT.
01985
01986  8820-PF23.
01987      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01988      MOVE XCTL-005               TO PGM-NAME.
01989
01990      GO TO 9300-XCTL.
01991
01992  8840-DUPREC.
01993      MOVE ER-0081                TO EMI-ERROR.
01994
01995      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
01996
01997      MOVE -1                     TO USERCDL.
01998      MOVE AL-UABON               TO USERCDA.
01999
02000      IF QCAMTL GREATER ZERO
02001          MOVE QCAMTI             TO QCAMTO.
02002
02003      IF MXAHAMTL GREATER ZERO
02004          MOVE MXAHAMTI           TO MXAHAMTO.
02005
02006      IF MXLFAMTL GREATER ZERO
02007          MOVE MXLFAMTI           TO MXLFAMTO.
02008
02009      IF AUTAMTL GREATER ZERO
02010          MOVE AUTAMTI            TO AUTAMTO.
02011
02012      IF MAXEXPL GREATER ZERO
02013          MOVE MAXEXPI            TO MAXEXPO.
02014
02015      GO TO 8200-SEND-DATAONLY.
02016
02017  8860-ENDFILE.
02018
02019      MOVE ER-0130                TO EMI-ERROR.
02020      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
02021      MOVE -1                     TO MAINTL.
02022      GO TO 8200-SEND-DATAONLY.
02023
02024  8870-NOTOPEN.
02025
02026      MOVE ER-0042                TO EMI-ERROR.
02027      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
02028      MOVE -1                     TO MAINTL.
02029      GO TO 8200-SEND-DATAONLY.
02030
02031  8880-NOT-FOUND.
02032
02033      MOVE ER-0073                TO EMI-ERROR.
02034      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.
02035      MOVE -1                     TO USERCDL.
02036      MOVE AL-UABON               TO USERCDA.
02037      GO TO 8200-SEND-DATAONLY.
02038
02039  9000-RETURN-CICS.
02040      
      * EXEC CICS  RETURN
02041 *    END-EXEC.
      *    MOVE '.(                    ''   #00005256' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02042
02043  9100-RETURN-TRAN.
02044
02045      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
02046      MOVE '103A'                 TO PI-CURRENT-SCREEN-NO.
02047
02048      
      * EXEC CICS  RETURN
02049 *        TRANSID   (TRANS-ID)
02050 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02051 *        LENGTH    (PI-COMM-LENGTH)
02052 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005264' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02053
02054  9200-RETURN-MAIN-MENU.
02055
02056      IF PI-SESSION-IN-PROGRESS  EQUAL '1'
02057          MOVE XCTL-126           TO PGM-NAME.
02058
02059      IF PI-SESSION-IN-PROGRESS  EQUAL '2'
02060          MOVE XCTL-626           TO PGM-NAME.
02061
02062      IF PI-SESSION-IN-PROGRESS  EQUAL '4'
02063          MOVE XCTL-EM626         TO PGM-NAME.
02064
02065      IF PI-SESSION-IN-PROGRESS  EQUAL '5'
02066          MOVE XCTL-800           TO PGM-NAME.
02067
02068  9300-XCTL.
02069      
      * EXEC CICS  XCTL
02070 *        PROGRAM   (PGM-NAME)
02071 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02072 *        LENGTH    (PI-COMM-LENGTH)
02073 *    END-EXEC.
      *    MOVE '.$C                   %   #00005285' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02074
02075  9400-CLEAR.
02076      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
02077
02078      GO TO 9300-XCTL.
02079
02080  9500-PF12.
02081      MOVE XCTL-010               TO PGM-NAME.
02082
02083      GO TO 9300-XCTL.
02084
02085  9600-PGMID-ERROR.
02086      
      * EXEC CICS  HANDLE CONDITION
02087 *        PGMIDERR  (8300-SEND-TEXT)
02088 *    END-EXEC.
      *    MOVE '"$L                   ! '' #00005302' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035333032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02089
02090      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
02091      MOVE ' '                    TO PI-ENTRY-CD-1.
02092      MOVE XCTL-005               TO PGM-NAME.
02093      MOVE PGM-NAME               TO LOGOFF-PGM.
02094      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
02095
02096      GO TO 9300-XCTL.
02097
02098  9700-LINK-DATE-CONVERT.
02099      
      * EXEC CICS  LINK
02100 *        PROGRAM   ('ELDATCV')
02101 *        COMMAREA  (DATE-CONVERSION-DATA)
02102 *        LENGTH    (DC-COMM-LENGTH)
02103 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005315' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02104
02105  9799-EXIT.
02106      EXIT.
02107
02108  9000-ERROR-FORMAT.
02109      IF NOT  EMI-ERRORS-COMPLETE
02110          MOVE LINK-001           TO PGM-NAME
02111          
      * EXEC CICS  LINK
02112 *            PROGRAM   (PGM-NAME)
02113 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
02114 *            LENGTH    (EMI-COMM-LENGTH)
02115 *        END-EXEC.
      *    MOVE '."C                   (   #00005327' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02116
02117  9099-EXIT.
02118      EXIT.
02119
02120  9999-ABEND.
02121
02122      MOVE LINK-004               TO PGM-NAME.
02123      MOVE DFHEIBLK               TO EMI-LINE1.
02124
02125      
      * EXEC CICS  LINK
02126 *        PROGRAM   (PGM-NAME)
02127 *        COMMAREA  (EMI-LINE1)
02128 *        LENGTH    (72)
02129 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005341' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02130
02131      GO TO 8200-SEND-DATAONLY.
02132
02133      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL103' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL103' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8840-DUPREC,
                     8870-NOTOPEN,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9999-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 4030-END-OF-DELETE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5010-USER-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8860-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6100-CARRIER-MISSING
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL103' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
