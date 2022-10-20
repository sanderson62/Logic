00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL1061.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:43:28.
00007 *                            VMOD=2.011.
00008 *
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
00023 *REMARKS.    TRANSACTION- EX10 - STATE BENEFIT MAINTENANCE.
00024
111219******************************************************************
111219*                   C H A N G E   L O G
111219*
111219* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111219*-----------------------------------------------------------------
111219*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111219* EFFECTIVE    NUMBER
111219*-----------------------------------------------------------------
111219* 111219  CR2019110800001  PEMA  ALLOW VALID BENEFIT CODES
00026  ENVIRONMENT DIVISION.
00027  DATA DIVISION.
00028  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00029  77  FILLER  PIC X(32)  VALUE '********************************'.
00030  77  FILLER  PIC X(32)  VALUE '*    EL1061 WORKING STORAGE    *'.
00031  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.011 *********'.
00032
00033 *                            COPY ELCSCTM.
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
00034 *                            COPY ELCSCRTY.
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
00035
00036  01  WS-DATE-AREA.
00037      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00038      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00039
00040  01  WS.
00041      05  WS-COMM-LENGTH      PIC S9(4) COMP VALUE +1500.
00042      05  WS-MAP-LENGTH       PIC S9(4) COMP VALUE +1000.
00043      05  RETURNED-FROM       PIC X(8)    VALUE SPACES.
00044      05  QID.
00045          16  QID-TERM        PIC X(4).
00046          16  FILLER          PIC X(4)    VALUE '106B'.
00047
00048      05  WS-ST-LF-EXP-PCT    PIC S9(3)V9(4) VALUE +0.
00049      05  WS-ST-AH-EXP-PCT    PIC S9(3)V9(4) VALUE +0.
00050
00051  01  STANDARD-AREAS.
00052      12  MAP-NAME            PIC X(8)    VALUE 'EL106B'.
00053      12  MAPSET-NAME         PIC X(8)    VALUE 'EL1061S'.
00054      12  TRANS-ID            PIC X(4)    VALUE 'EX1A'.
00055      12  PGM-NAME            PIC X(8).
00056      12  TIME-IN             PIC S9(7).
00057      12  TIME-OUT-R  REDEFINES TIME-IN.
00058          16  FILLER          PIC X.
00059          16  TIME-OUT        PIC 99V99.
00060          16  FILLER          PIC XX.
00061      12  XCTL-005            PIC X(8)    VALUE 'EL005'.
00062      12  XCTL-010            PIC X(8)    VALUE 'EL010'.
00063      12  XCTL-EL126          PIC X(8)    VALUE 'EL126'.
00064      12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.
00065      12  XCTL-EL106          PIC X(8)    VALUE 'EL106'.
00066      12  XCTL-6565           PIC X(8)    VALUE 'EL6565'.
00067      12  XCTL-EM626          PIC X(8)    VALUE 'EM626'.
00068      12  XCTL-GL800          PIC X(8)    VALUE 'GL800'.
00069      12  LINK-001            PIC X(8)    VALUE 'EL001'.
00070      12  LINK-004            PIC X(8)    VALUE 'EL004'.
00071      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00072      12  THIS-PGM            PIC X(8)    VALUE 'EL1061'.
00073      12  ELCNTL-ID           PIC X(8)    VALUE 'ELCNTL'.
00074      12  W-LETR-FILE-ID      PIC X(8)    VALUE 'ELLETR'.
00075      12  SUB                 PIC 99.
00076      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
00077
00078  01  SWITCH-WORK.
00079      12  WS-FIRST-TIME-SW    PIC X       VALUE 'Y'.
00080          88  FIRST-TIME                  VALUE 'Y'.
00081      12  WS-DISPLAY-SW       PIC X       VALUE 'N'.
00082          88  INITIAL-DISPLAY             VALUE 'Y'.
00083
00084  01  TBL-WORK.
00085      05  TBL-ITEMS OCCURS 50 TIMES INDEXED BY TBL-NDX.
00086          10  TBL-CODE        PIC XX.
00087          10  TBL-KIND        PIC X.
00088
00089  01  ACCESS-KEYS.
00090      12  ELCNTL-KEY.
00091          16  CK-COMP-ID      PIC X(3).
00092          16  FILLER          PIC X       VALUE '3'.
00093          16  CK-STATE-CD     PIC X(4)    VALUE SPACES.
00094          16  CK-SEQ          PIC S9(4)   VALUE +0    COMP.
00095
00096  01  ERROR-MESSAGES.
00097      12  ER-0000                 PIC X(4)  VALUE '0000'.
00098      12  ER-0004                 PIC X(4)  VALUE '0004'.
00099      12  ER-0013                 PIC X(4)  VALUE '0013'.
00100      12  ER-0023                 PIC X(4)  VALUE '0023'.
00101      12  ER-0029                 PIC X(4)  VALUE '0029'.
00102      12  ER-0042                 PIC X(4)  VALUE '0042'.
00103      12  ER-0050                 PIC X(4)  VALUE '0050'.
00104      12  ER-0068                 PIC X(4)  VALUE '0068'.
00105      12  ER-0070                 PIC X(4)  VALUE '0070'.
00106      12  ER-0144                 PIC X(4)  VALUE '0144'.
00107      12  ER-0145                 PIC X(4)  VALUE '0145'.
00108      12  ER-0146                 PIC X(4)  VALUE '0146'.
00109      12  ER-0147                 PIC X(4)  VALUE '0147'.
00110      12  ER-0148                 PIC X(4)  VALUE '0148'.
00111      12  ER-0149                 PIC X(4)  VALUE '0149'.
00112      12  ER-0150                 PIC X(4)  VALUE '0150'.
00113      12  ER-0151                 PIC X(4)  VALUE '0151'.
00114      12  ER-0152                 PIC X(4)  VALUE '0152'.
00115      12  ER-0153                 PIC X(4)  VALUE '0153'.
00116      12  ER-0159                 PIC X(4)  VALUE '0159'.
00117      12  ER-0160                 PIC X(4)  VALUE '0160'.
00118      12  ER-0161                 PIC X(4)  VALUE '0161'.
00119      12  ER-0582                 PIC X(4)  VALUE '0582'.
00120      12  ER-2009                 PIC X(4)  VALUE '2009'.
00121      12  ER-2010                 PIC X(4)  VALUE '2010'.
00122      12  ER-2012                 PIC X(4)  VALUE '2012'.
00123      12  ER-2014                 PIC X(4)  VALUE '2014'.
00124      12  ER-2024                 PIC X(4)  VALUE '2024'.
00125      12  ER-2028                 PIC X(4)  VALUE '2028'.
00126      12  ER-2033                 PIC X(4)  VALUE '2033'.
00127      12  ER-2298                 PIC X(4)  VALUE '2298'.
00128      12  ER-2299                 PIC X(4)  VALUE '2299'.
00129      12  ER-3030                 PIC X(4)  VALUE '3030'.
00130      12  ER-3031                 PIC X(4)  VALUE '3031'.
00131      12  ER-3032                 PIC X(4)  VALUE '3032'.
00132      12  ER-3033                 PIC X(4)  VALUE '3033'.
00133      12  ER-3034                 PIC X(4)  VALUE '3034'.
00134      12  ER-3035                 PIC X(4)  VALUE '3035'.
00135      12  ER-7008                 PIC X(4)  VALUE '7008'.
00136      12  ER-7531                 PIC X(4)  VALUE '7531'.
00137      12  ER-7534                 PIC X(4)  VALUE '7534'.
00138      12  ER-9074                 PIC X(4)  VALUE '9074'.
00139      12  ER-9447                 PIC X(4)  VALUE '9447'.
00140      12  ER-9448                 PIC X(4)  VALUE '9448'.
00141      EJECT
00142 *                            COPY ELCDATE.
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
00143      EJECT
00144 *                            COPY ELCLOGOF.
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
00145      EJECT
00146 *                            COPY ELCATTR.
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
00147      EJECT
00148 *                            COPY ELCEMIB.
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
00149      EJECT
00150 *                            COPY ELCJPFX.
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
00151                                   PIC X(530).
00152      EJECT
00153 *                            COPY ELCINTF.
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
00154      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00155          16  FILLER              PIC X(101).
00156          16  PI-WS-STATE         PIC XX.
00157          16  PI-WS-CLASS         PIC XX.
00158          16  PI-WS-DEV           PIC XXX.
00159          16  PI-WS-TYPE          PIC X.
00160          16  PI-WS-PLAN          PIC XX.
00161          16  PI-PREV-STATE       PIC X(4).
00162          16  FILLER              PIC X(525).
00163
00164      EJECT
00165 *                            COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00166  01  FILLER    REDEFINES DFHAID.
00167      12  FILLER              PIC X(8).
00168      12  PF-VALUES           PIC X       OCCURS 2.
00169
00170      EJECT
00171 *                             COPY EL1061S.
       01  EL106BI.
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
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  STCDL PIC S9(0004) COMP.
           05  STCDF PIC  X(0001).
           05  FILLER REDEFINES STCDF.
               10  STCDA PIC  X(0001).
           05  STCDI PIC  X(0002).
      *    -------------------------------
           05  STABRL PIC S9(0004) COMP.
           05  STABRF PIC  X(0001).
           05  FILLER REDEFINES STABRF.
               10  STABRA PIC  X(0001).
           05  STABRI PIC  X(0002).
      *    -------------------------------
           05  STNAMEL PIC S9(0004) COMP.
           05  STNAMEF PIC  X(0001).
           05  FILLER REDEFINES STNAMEF.
               10  STNAMEA PIC  X(0001).
           05  STNAMEI PIC  X(0028).
      *    -------------------------------
           05  CD1L PIC S9(0004) COMP.
           05  CD1F PIC  X(0001).
           05  FILLER REDEFINES CD1F.
               10  CD1A PIC  X(0001).
           05  CD1I PIC  X(0002).
      *    -------------------------------
           05  KIND1L PIC S9(0004) COMP.
           05  KIND1F PIC  X(0001).
           05  FILLER REDEFINES KIND1F.
               10  KIND1A PIC  X(0001).
           05  KIND1I PIC  X(0001).
      *    -------------------------------
           05  REFD1L PIC S9(0004) COMP.
           05  REFD1F PIC  X(0001).
           05  FILLER REDEFINES REFD1F.
               10  REFD1A PIC  X(0001).
           05  REFD1I PIC  X(0001).
      *    -------------------------------
           05  RMTRM1L PIC S9(0004) COMP.
           05  RMTRM1F PIC  X(0001).
           05  FILLER REDEFINES RMTRM1F.
               10  RMTRM1A PIC  X(0001).
           05  RMTRM1I PIC  X(0001).
      *    -------------------------------
           05  EXTRA1L PIC S9(0004) COMP.
           05  EXTRA1F PIC  X(0001).
           05  FILLER REDEFINES EXTRA1F.
               10  EXTRA1A PIC  X(0001).
           05  EXTRA1I PIC  X(0001).
      *    -------------------------------
           05  CD2L PIC S9(0004) COMP.
           05  CD2F PIC  X(0001).
           05  FILLER REDEFINES CD2F.
               10  CD2A PIC  X(0001).
           05  CD2I PIC  X(0002).
      *    -------------------------------
           05  KIND2L PIC S9(0004) COMP.
           05  KIND2F PIC  X(0001).
           05  FILLER REDEFINES KIND2F.
               10  KIND2A PIC  X(0001).
           05  KIND2I PIC  X(0001).
      *    -------------------------------
           05  REFD2L PIC S9(0004) COMP.
           05  REFD2F PIC  X(0001).
           05  FILLER REDEFINES REFD2F.
               10  REFD2A PIC  X(0001).
           05  REFD2I PIC  X(0001).
      *    -------------------------------
           05  RMTRM2L PIC S9(0004) COMP.
           05  RMTRM2F PIC  X(0001).
           05  FILLER REDEFINES RMTRM2F.
               10  RMTRM2A PIC  X(0001).
           05  RMTRM2I PIC  X(0001).
      *    -------------------------------
           05  EXTRA2L PIC S9(0004) COMP.
           05  EXTRA2F PIC  X(0001).
           05  FILLER REDEFINES EXTRA2F.
               10  EXTRA2A PIC  X(0001).
           05  EXTRA2I PIC  X(0001).
      *    -------------------------------
           05  CD3L PIC S9(0004) COMP.
           05  CD3F PIC  X(0001).
           05  FILLER REDEFINES CD3F.
               10  CD3A PIC  X(0001).
           05  CD3I PIC  X(0002).
      *    -------------------------------
           05  KIND3L PIC S9(0004) COMP.
           05  KIND3F PIC  X(0001).
           05  FILLER REDEFINES KIND3F.
               10  KIND3A PIC  X(0001).
           05  KIND3I PIC  X(0001).
      *    -------------------------------
           05  REFD3L PIC S9(0004) COMP.
           05  REFD3F PIC  X(0001).
           05  FILLER REDEFINES REFD3F.
               10  REFD3A PIC  X(0001).
           05  REFD3I PIC  X(0001).
      *    -------------------------------
           05  RMTRM3L PIC S9(0004) COMP.
           05  RMTRM3F PIC  X(0001).
           05  FILLER REDEFINES RMTRM3F.
               10  RMTRM3A PIC  X(0001).
           05  RMTRM3I PIC  X(0001).
      *    -------------------------------
           05  EXTRA3L PIC S9(0004) COMP.
           05  EXTRA3F PIC  X(0001).
           05  FILLER REDEFINES EXTRA3F.
               10  EXTRA3A PIC  X(0001).
           05  EXTRA3I PIC  X(0001).
      *    -------------------------------
           05  CD4L PIC S9(0004) COMP.
           05  CD4F PIC  X(0001).
           05  FILLER REDEFINES CD4F.
               10  CD4A PIC  X(0001).
           05  CD4I PIC  X(0002).
      *    -------------------------------
           05  KIND4L PIC S9(0004) COMP.
           05  KIND4F PIC  X(0001).
           05  FILLER REDEFINES KIND4F.
               10  KIND4A PIC  X(0001).
           05  KIND4I PIC  X(0001).
      *    -------------------------------
           05  REFD4L PIC S9(0004) COMP.
           05  REFD4F PIC  X(0001).
           05  FILLER REDEFINES REFD4F.
               10  REFD4A PIC  X(0001).
           05  REFD4I PIC  X(0001).
      *    -------------------------------
           05  RMTRM4L PIC S9(0004) COMP.
           05  RMTRM4F PIC  X(0001).
           05  FILLER REDEFINES RMTRM4F.
               10  RMTRM4A PIC  X(0001).
           05  RMTRM4I PIC  X(0001).
      *    -------------------------------
           05  EXTRA4L PIC S9(0004) COMP.
           05  EXTRA4F PIC  X(0001).
           05  FILLER REDEFINES EXTRA4F.
               10  EXTRA4A PIC  X(0001).
           05  EXTRA4I PIC  X(0001).
      *    -------------------------------
           05  CD5L PIC S9(0004) COMP.
           05  CD5F PIC  X(0001).
           05  FILLER REDEFINES CD5F.
               10  CD5A PIC  X(0001).
           05  CD5I PIC  X(0002).
      *    -------------------------------
           05  KIND5L PIC S9(0004) COMP.
           05  KIND5F PIC  X(0001).
           05  FILLER REDEFINES KIND5F.
               10  KIND5A PIC  X(0001).
           05  KIND5I PIC  X(0001).
      *    -------------------------------
           05  REFD5L PIC S9(0004) COMP.
           05  REFD5F PIC  X(0001).
           05  FILLER REDEFINES REFD5F.
               10  REFD5A PIC  X(0001).
           05  REFD5I PIC  X(0001).
      *    -------------------------------
           05  RMTRM5L PIC S9(0004) COMP.
           05  RMTRM5F PIC  X(0001).
           05  FILLER REDEFINES RMTRM5F.
               10  RMTRM5A PIC  X(0001).
           05  RMTRM5I PIC  X(0001).
      *    -------------------------------
           05  EXTRA5L PIC S9(0004) COMP.
           05  EXTRA5F PIC  X(0001).
           05  FILLER REDEFINES EXTRA5F.
               10  EXTRA5A PIC  X(0001).
           05  EXTRA5I PIC  X(0001).
      *    -------------------------------
           05  CD6L PIC S9(0004) COMP.
           05  CD6F PIC  X(0001).
           05  FILLER REDEFINES CD6F.
               10  CD6A PIC  X(0001).
           05  CD6I PIC  X(0002).
      *    -------------------------------
           05  KIND6L PIC S9(0004) COMP.
           05  KIND6F PIC  X(0001).
           05  FILLER REDEFINES KIND6F.
               10  KIND6A PIC  X(0001).
           05  KIND6I PIC  X(0001).
      *    -------------------------------
           05  REFD6L PIC S9(0004) COMP.
           05  REFD6F PIC  X(0001).
           05  FILLER REDEFINES REFD6F.
               10  REFD6A PIC  X(0001).
           05  REFD6I PIC  X(0001).
      *    -------------------------------
           05  RMTRM6L PIC S9(0004) COMP.
           05  RMTRM6F PIC  X(0001).
           05  FILLER REDEFINES RMTRM6F.
               10  RMTRM6A PIC  X(0001).
           05  RMTRM6I PIC  X(0001).
      *    -------------------------------
           05  EXTRA6L PIC S9(0004) COMP.
           05  EXTRA6F PIC  X(0001).
           05  FILLER REDEFINES EXTRA6F.
               10  EXTRA6A PIC  X(0001).
           05  EXTRA6I PIC  X(0001).
      *    -------------------------------
           05  CD7L PIC S9(0004) COMP.
           05  CD7F PIC  X(0001).
           05  FILLER REDEFINES CD7F.
               10  CD7A PIC  X(0001).
           05  CD7I PIC  X(0002).
      *    -------------------------------
           05  KIND7L PIC S9(0004) COMP.
           05  KIND7F PIC  X(0001).
           05  FILLER REDEFINES KIND7F.
               10  KIND7A PIC  X(0001).
           05  KIND7I PIC  X(0001).
      *    -------------------------------
           05  REFD7L PIC S9(0004) COMP.
           05  REFD7F PIC  X(0001).
           05  FILLER REDEFINES REFD7F.
               10  REFD7A PIC  X(0001).
           05  REFD7I PIC  X(0001).
      *    -------------------------------
           05  RMTRM7L PIC S9(0004) COMP.
           05  RMTRM7F PIC  X(0001).
           05  FILLER REDEFINES RMTRM7F.
               10  RMTRM7A PIC  X(0001).
           05  RMTRM7I PIC  X(0001).
      *    -------------------------------
           05  EXTRA7L PIC S9(0004) COMP.
           05  EXTRA7F PIC  X(0001).
           05  FILLER REDEFINES EXTRA7F.
               10  EXTRA7A PIC  X(0001).
           05  EXTRA7I PIC  X(0001).
      *    -------------------------------
           05  CD8L PIC S9(0004) COMP.
           05  CD8F PIC  X(0001).
           05  FILLER REDEFINES CD8F.
               10  CD8A PIC  X(0001).
           05  CD8I PIC  X(0002).
      *    -------------------------------
           05  KIND8L PIC S9(0004) COMP.
           05  KIND8F PIC  X(0001).
           05  FILLER REDEFINES KIND8F.
               10  KIND8A PIC  X(0001).
           05  KIND8I PIC  X(0001).
      *    -------------------------------
           05  REFD8L PIC S9(0004) COMP.
           05  REFD8F PIC  X(0001).
           05  FILLER REDEFINES REFD8F.
               10  REFD8A PIC  X(0001).
           05  REFD8I PIC  X(0001).
      *    -------------------------------
           05  RMTRM8L PIC S9(0004) COMP.
           05  RMTRM8F PIC  X(0001).
           05  FILLER REDEFINES RMTRM8F.
               10  RMTRM8A PIC  X(0001).
           05  RMTRM8I PIC  X(0001).
      *    -------------------------------
           05  EXTRA8L PIC S9(0004) COMP.
           05  EXTRA8F PIC  X(0001).
           05  FILLER REDEFINES EXTRA8F.
               10  EXTRA8A PIC  X(0001).
           05  EXTRA8I PIC  X(0001).
      *    -------------------------------
           05  CD9L PIC S9(0004) COMP.
           05  CD9F PIC  X(0001).
           05  FILLER REDEFINES CD9F.
               10  CD9A PIC  X(0001).
           05  CD9I PIC  X(0002).
      *    -------------------------------
           05  KIND9L PIC S9(0004) COMP.
           05  KIND9F PIC  X(0001).
           05  FILLER REDEFINES KIND9F.
               10  KIND9A PIC  X(0001).
           05  KIND9I PIC  X(0001).
      *    -------------------------------
           05  REFD9L PIC S9(0004) COMP.
           05  REFD9F PIC  X(0001).
           05  FILLER REDEFINES REFD9F.
               10  REFD9A PIC  X(0001).
           05  REFD9I PIC  X(0001).
      *    -------------------------------
           05  RMTRM9L PIC S9(0004) COMP.
           05  RMTRM9F PIC  X(0001).
           05  FILLER REDEFINES RMTRM9F.
               10  RMTRM9A PIC  X(0001).
           05  RMTRM9I PIC  X(0001).
      *    -------------------------------
           05  EXTRA9L PIC S9(0004) COMP.
           05  EXTRA9F PIC  X(0001).
           05  FILLER REDEFINES EXTRA9F.
               10  EXTRA9A PIC  X(0001).
           05  EXTRA9I PIC  X(0001).
      *    -------------------------------
           05  CD10L PIC S9(0004) COMP.
           05  CD10F PIC  X(0001).
           05  FILLER REDEFINES CD10F.
               10  CD10A PIC  X(0001).
           05  CD10I PIC  X(0002).
      *    -------------------------------
           05  KIND10L PIC S9(0004) COMP.
           05  KIND10F PIC  X(0001).
           05  FILLER REDEFINES KIND10F.
               10  KIND10A PIC  X(0001).
           05  KIND10I PIC  X(0001).
      *    -------------------------------
           05  REFD10L PIC S9(0004) COMP.
           05  REFD10F PIC  X(0001).
           05  FILLER REDEFINES REFD10F.
               10  REFD10A PIC  X(0001).
           05  REFD10I PIC  X(0001).
      *    -------------------------------
           05  RMTRM10L PIC S9(0004) COMP.
           05  RMTRM10F PIC  X(0001).
           05  FILLER REDEFINES RMTRM10F.
               10  RMTRM10A PIC  X(0001).
           05  RMTRM10I PIC  X(0001).
      *    -------------------------------
           05  EXTRA10L PIC S9(0004) COMP.
           05  EXTRA10F PIC  X(0001).
           05  FILLER REDEFINES EXTRA10F.
               10  EXTRA10A PIC  X(0001).
           05  EXTRA10I PIC  X(0001).
      *    -------------------------------
           05  CD11L PIC S9(0004) COMP.
           05  CD11F PIC  X(0001).
           05  FILLER REDEFINES CD11F.
               10  CD11A PIC  X(0001).
           05  CD11I PIC  X(0002).
      *    -------------------------------
           05  KIND11L PIC S9(0004) COMP.
           05  KIND11F PIC  X(0001).
           05  FILLER REDEFINES KIND11F.
               10  KIND11A PIC  X(0001).
           05  KIND11I PIC  X(0001).
      *    -------------------------------
           05  REFD11L PIC S9(0004) COMP.
           05  REFD11F PIC  X(0001).
           05  FILLER REDEFINES REFD11F.
               10  REFD11A PIC  X(0001).
           05  REFD11I PIC  X(0001).
      *    -------------------------------
           05  RMTRM11L PIC S9(0004) COMP.
           05  RMTRM11F PIC  X(0001).
           05  FILLER REDEFINES RMTRM11F.
               10  RMTRM11A PIC  X(0001).
           05  RMTRM11I PIC  X(0001).
      *    -------------------------------
           05  EXTRA11L PIC S9(0004) COMP.
           05  EXTRA11F PIC  X(0001).
           05  FILLER REDEFINES EXTRA11F.
               10  EXTRA11A PIC  X(0001).
           05  EXTRA11I PIC  X(0001).
      *    -------------------------------
           05  CD12L PIC S9(0004) COMP.
           05  CD12F PIC  X(0001).
           05  FILLER REDEFINES CD12F.
               10  CD12A PIC  X(0001).
           05  CD12I PIC  X(0002).
      *    -------------------------------
           05  KIND12L PIC S9(0004) COMP.
           05  KIND12F PIC  X(0001).
           05  FILLER REDEFINES KIND12F.
               10  KIND12A PIC  X(0001).
           05  KIND12I PIC  X(0001).
      *    -------------------------------
           05  REFD12L PIC S9(0004) COMP.
           05  REFD12F PIC  X(0001).
           05  FILLER REDEFINES REFD12F.
               10  REFD12A PIC  X(0001).
           05  REFD12I PIC  X(0001).
      *    -------------------------------
           05  RMTRM12L PIC S9(0004) COMP.
           05  RMTRM12F PIC  X(0001).
           05  FILLER REDEFINES RMTRM12F.
               10  RMTRM12A PIC  X(0001).
           05  RMTRM12I PIC  X(0001).
      *    -------------------------------
           05  EXTRA12L PIC S9(0004) COMP.
           05  EXTRA12F PIC  X(0001).
           05  FILLER REDEFINES EXTRA12F.
               10  EXTRA12A PIC  X(0001).
           05  EXTRA12I PIC  X(0001).
      *    -------------------------------
           05  CD13L PIC S9(0004) COMP.
           05  CD13F PIC  X(0001).
           05  FILLER REDEFINES CD13F.
               10  CD13A PIC  X(0001).
           05  CD13I PIC  X(0002).
      *    -------------------------------
           05  KIND13L PIC S9(0004) COMP.
           05  KIND13F PIC  X(0001).
           05  FILLER REDEFINES KIND13F.
               10  KIND13A PIC  X(0001).
           05  KIND13I PIC  X(0001).
      *    -------------------------------
           05  REFD13L PIC S9(0004) COMP.
           05  REFD13F PIC  X(0001).
           05  FILLER REDEFINES REFD13F.
               10  REFD13A PIC  X(0001).
           05  REFD13I PIC  X(0001).
      *    -------------------------------
           05  RMTRM13L PIC S9(0004) COMP.
           05  RMTRM13F PIC  X(0001).
           05  FILLER REDEFINES RMTRM13F.
               10  RMTRM13A PIC  X(0001).
           05  RMTRM13I PIC  X(0001).
      *    -------------------------------
           05  EXTRA13L PIC S9(0004) COMP.
           05  EXTRA13F PIC  X(0001).
           05  FILLER REDEFINES EXTRA13F.
               10  EXTRA13A PIC  X(0001).
           05  EXTRA13I PIC  X(0001).
      *    -------------------------------
           05  CD14L PIC S9(0004) COMP.
           05  CD14F PIC  X(0001).
           05  FILLER REDEFINES CD14F.
               10  CD14A PIC  X(0001).
           05  CD14I PIC  X(0002).
      *    -------------------------------
           05  KIND14L PIC S9(0004) COMP.
           05  KIND14F PIC  X(0001).
           05  FILLER REDEFINES KIND14F.
               10  KIND14A PIC  X(0001).
           05  KIND14I PIC  X(0001).
      *    -------------------------------
           05  REFD14L PIC S9(0004) COMP.
           05  REFD14F PIC  X(0001).
           05  FILLER REDEFINES REFD14F.
               10  REFD14A PIC  X(0001).
           05  REFD14I PIC  X(0001).
      *    -------------------------------
           05  RMTRM14L PIC S9(0004) COMP.
           05  RMTRM14F PIC  X(0001).
           05  FILLER REDEFINES RMTRM14F.
               10  RMTRM14A PIC  X(0001).
           05  RMTRM14I PIC  X(0001).
      *    -------------------------------
           05  EXTRA14L PIC S9(0004) COMP.
           05  EXTRA14F PIC  X(0001).
           05  FILLER REDEFINES EXTRA14F.
               10  EXTRA14A PIC  X(0001).
           05  EXTRA14I PIC  X(0001).
      *    -------------------------------
           05  CD15L PIC S9(0004) COMP.
           05  CD15F PIC  X(0001).
           05  FILLER REDEFINES CD15F.
               10  CD15A PIC  X(0001).
           05  CD15I PIC  X(0002).
      *    -------------------------------
           05  KIND15L PIC S9(0004) COMP.
           05  KIND15F PIC  X(0001).
           05  FILLER REDEFINES KIND15F.
               10  KIND15A PIC  X(0001).
           05  KIND15I PIC  X(0001).
      *    -------------------------------
           05  REFD15L PIC S9(0004) COMP.
           05  REFD15F PIC  X(0001).
           05  FILLER REDEFINES REFD15F.
               10  REFD15A PIC  X(0001).
           05  REFD15I PIC  X(0001).
      *    -------------------------------
           05  RMTRM15L PIC S9(0004) COMP.
           05  RMTRM15F PIC  X(0001).
           05  FILLER REDEFINES RMTRM15F.
               10  RMTRM15A PIC  X(0001).
           05  RMTRM15I PIC  X(0001).
      *    -------------------------------
           05  EXTRA15L PIC S9(0004) COMP.
           05  EXTRA15F PIC  X(0001).
           05  FILLER REDEFINES EXTRA15F.
               10  EXTRA15A PIC  X(0001).
           05  EXTRA15I PIC  X(0001).
      *    -------------------------------
           05  CD16L PIC S9(0004) COMP.
           05  CD16F PIC  X(0001).
           05  FILLER REDEFINES CD16F.
               10  CD16A PIC  X(0001).
           05  CD16I PIC  X(0002).
      *    -------------------------------
           05  KIND16L PIC S9(0004) COMP.
           05  KIND16F PIC  X(0001).
           05  FILLER REDEFINES KIND16F.
               10  KIND16A PIC  X(0001).
           05  KIND16I PIC  X(0001).
      *    -------------------------------
           05  REFD16L PIC S9(0004) COMP.
           05  REFD16F PIC  X(0001).
           05  FILLER REDEFINES REFD16F.
               10  REFD16A PIC  X(0001).
           05  REFD16I PIC  X(0001).
      *    -------------------------------
           05  RMTRM16L PIC S9(0004) COMP.
           05  RMTRM16F PIC  X(0001).
           05  FILLER REDEFINES RMTRM16F.
               10  RMTRM16A PIC  X(0001).
           05  RMTRM16I PIC  X(0001).
      *    -------------------------------
           05  EXTRA16L PIC S9(0004) COMP.
           05  EXTRA16F PIC  X(0001).
           05  FILLER REDEFINES EXTRA16F.
               10  EXTRA16A PIC  X(0001).
           05  EXTRA16I PIC  X(0001).
      *    -------------------------------
           05  CD17L PIC S9(0004) COMP.
           05  CD17F PIC  X(0001).
           05  FILLER REDEFINES CD17F.
               10  CD17A PIC  X(0001).
           05  CD17I PIC  X(0002).
      *    -------------------------------
           05  KIND17L PIC S9(0004) COMP.
           05  KIND17F PIC  X(0001).
           05  FILLER REDEFINES KIND17F.
               10  KIND17A PIC  X(0001).
           05  KIND17I PIC  X(0001).
      *    -------------------------------
           05  REFD17L PIC S9(0004) COMP.
           05  REFD17F PIC  X(0001).
           05  FILLER REDEFINES REFD17F.
               10  REFD17A PIC  X(0001).
           05  REFD17I PIC  X(0001).
      *    -------------------------------
           05  RMTRM17L PIC S9(0004) COMP.
           05  RMTRM17F PIC  X(0001).
           05  FILLER REDEFINES RMTRM17F.
               10  RMTRM17A PIC  X(0001).
           05  RMTRM17I PIC  X(0001).
      *    -------------------------------
           05  EXTRA17L PIC S9(0004) COMP.
           05  EXTRA17F PIC  X(0001).
           05  FILLER REDEFINES EXTRA17F.
               10  EXTRA17A PIC  X(0001).
           05  EXTRA17I PIC  X(0001).
      *    -------------------------------
           05  CD18L PIC S9(0004) COMP.
           05  CD18F PIC  X(0001).
           05  FILLER REDEFINES CD18F.
               10  CD18A PIC  X(0001).
           05  CD18I PIC  X(0002).
      *    -------------------------------
           05  KIND18L PIC S9(0004) COMP.
           05  KIND18F PIC  X(0001).
           05  FILLER REDEFINES KIND18F.
               10  KIND18A PIC  X(0001).
           05  KIND18I PIC  X(0001).
      *    -------------------------------
           05  REFD18L PIC S9(0004) COMP.
           05  REFD18F PIC  X(0001).
           05  FILLER REDEFINES REFD18F.
               10  REFD18A PIC  X(0001).
           05  REFD18I PIC  X(0001).
      *    -------------------------------
           05  RMTRM18L PIC S9(0004) COMP.
           05  RMTRM18F PIC  X(0001).
           05  FILLER REDEFINES RMTRM18F.
               10  RMTRM18A PIC  X(0001).
           05  RMTRM18I PIC  X(0001).
      *    -------------------------------
           05  EXTRA18L PIC S9(0004) COMP.
           05  EXTRA18F PIC  X(0001).
           05  FILLER REDEFINES EXTRA18F.
               10  EXTRA18A PIC  X(0001).
           05  EXTRA18I PIC  X(0001).
      *    -------------------------------
           05  CD19L PIC S9(0004) COMP.
           05  CD19F PIC  X(0001).
           05  FILLER REDEFINES CD19F.
               10  CD19A PIC  X(0001).
           05  CD19I PIC  X(0002).
      *    -------------------------------
           05  KIND19L PIC S9(0004) COMP.
           05  KIND19F PIC  X(0001).
           05  FILLER REDEFINES KIND19F.
               10  KIND19A PIC  X(0001).
           05  KIND19I PIC  X(0001).
      *    -------------------------------
           05  REFD19L PIC S9(0004) COMP.
           05  REFD19F PIC  X(0001).
           05  FILLER REDEFINES REFD19F.
               10  REFD19A PIC  X(0001).
           05  REFD19I PIC  X(0001).
      *    -------------------------------
           05  RMTRM19L PIC S9(0004) COMP.
           05  RMTRM19F PIC  X(0001).
           05  FILLER REDEFINES RMTRM19F.
               10  RMTRM19A PIC  X(0001).
           05  RMTRM19I PIC  X(0001).
      *    -------------------------------
           05  EXTRA19L PIC S9(0004) COMP.
           05  EXTRA19F PIC  X(0001).
           05  FILLER REDEFINES EXTRA19F.
               10  EXTRA19A PIC  X(0001).
           05  EXTRA19I PIC  X(0001).
      *    -------------------------------
           05  CD20L PIC S9(0004) COMP.
           05  CD20F PIC  X(0001).
           05  FILLER REDEFINES CD20F.
               10  CD20A PIC  X(0001).
           05  CD20I PIC  X(0002).
      *    -------------------------------
           05  KIND20L PIC S9(0004) COMP.
           05  KIND20F PIC  X(0001).
           05  FILLER REDEFINES KIND20F.
               10  KIND20A PIC  X(0001).
           05  KIND20I PIC  X(0001).
      *    -------------------------------
           05  REFD20L PIC S9(0004) COMP.
           05  REFD20F PIC  X(0001).
           05  FILLER REDEFINES REFD20F.
               10  REFD20A PIC  X(0001).
           05  REFD20I PIC  X(0001).
      *    -------------------------------
           05  RMTRM20L PIC S9(0004) COMP.
           05  RMTRM20F PIC  X(0001).
           05  FILLER REDEFINES RMTRM20F.
               10  RMTRM20A PIC  X(0001).
           05  RMTRM20I PIC  X(0001).
      *    -------------------------------
           05  EXTRA20L PIC S9(0004) COMP.
           05  EXTRA20F PIC  X(0001).
           05  FILLER REDEFINES EXTRA20F.
               10  EXTRA20A PIC  X(0001).
           05  EXTRA20I PIC  X(0001).
      *    -------------------------------
           05  CD21L PIC S9(0004) COMP.
           05  CD21F PIC  X(0001).
           05  FILLER REDEFINES CD21F.
               10  CD21A PIC  X(0001).
           05  CD21I PIC  X(0002).
      *    -------------------------------
           05  KIND21L PIC S9(0004) COMP.
           05  KIND21F PIC  X(0001).
           05  FILLER REDEFINES KIND21F.
               10  KIND21A PIC  X(0001).
           05  KIND21I PIC  X(0001).
      *    -------------------------------
           05  REFD21L PIC S9(0004) COMP.
           05  REFD21F PIC  X(0001).
           05  FILLER REDEFINES REFD21F.
               10  REFD21A PIC  X(0001).
           05  REFD21I PIC  X(0001).
      *    -------------------------------
           05  RMTRM21L PIC S9(0004) COMP.
           05  RMTRM21F PIC  X(0001).
           05  FILLER REDEFINES RMTRM21F.
               10  RMTRM21A PIC  X(0001).
           05  RMTRM21I PIC  X(0001).
      *    -------------------------------
           05  EXTRA21L PIC S9(0004) COMP.
           05  EXTRA21F PIC  X(0001).
           05  FILLER REDEFINES EXTRA21F.
               10  EXTRA21A PIC  X(0001).
           05  EXTRA21I PIC  X(0001).
      *    -------------------------------
           05  CD22L PIC S9(0004) COMP.
           05  CD22F PIC  X(0001).
           05  FILLER REDEFINES CD22F.
               10  CD22A PIC  X(0001).
           05  CD22I PIC  X(0002).
      *    -------------------------------
           05  KIND22L PIC S9(0004) COMP.
           05  KIND22F PIC  X(0001).
           05  FILLER REDEFINES KIND22F.
               10  KIND22A PIC  X(0001).
           05  KIND22I PIC  X(0001).
      *    -------------------------------
           05  REFD22L PIC S9(0004) COMP.
           05  REFD22F PIC  X(0001).
           05  FILLER REDEFINES REFD22F.
               10  REFD22A PIC  X(0001).
           05  REFD22I PIC  X(0001).
      *    -------------------------------
           05  RMTRM22L PIC S9(0004) COMP.
           05  RMTRM22F PIC  X(0001).
           05  FILLER REDEFINES RMTRM22F.
               10  RMTRM22A PIC  X(0001).
           05  RMTRM22I PIC  X(0001).
      *    -------------------------------
           05  EXTRA22L PIC S9(0004) COMP.
           05  EXTRA22F PIC  X(0001).
           05  FILLER REDEFINES EXTRA22F.
               10  EXTRA22A PIC  X(0001).
           05  EXTRA22I PIC  X(0001).
      *    -------------------------------
           05  CD23L PIC S9(0004) COMP.
           05  CD23F PIC  X(0001).
           05  FILLER REDEFINES CD23F.
               10  CD23A PIC  X(0001).
           05  CD23I PIC  X(0002).
      *    -------------------------------
           05  KIND23L PIC S9(0004) COMP.
           05  KIND23F PIC  X(0001).
           05  FILLER REDEFINES KIND23F.
               10  KIND23A PIC  X(0001).
           05  KIND23I PIC  X(0001).
      *    -------------------------------
           05  REFD23L PIC S9(0004) COMP.
           05  REFD23F PIC  X(0001).
           05  FILLER REDEFINES REFD23F.
               10  REFD23A PIC  X(0001).
           05  REFD23I PIC  X(0001).
      *    -------------------------------
           05  RMTRM23L PIC S9(0004) COMP.
           05  RMTRM23F PIC  X(0001).
           05  FILLER REDEFINES RMTRM23F.
               10  RMTRM23A PIC  X(0001).
           05  RMTRM23I PIC  X(0001).
      *    -------------------------------
           05  EXTRA23L PIC S9(0004) COMP.
           05  EXTRA23F PIC  X(0001).
           05  FILLER REDEFINES EXTRA23F.
               10  EXTRA23A PIC  X(0001).
           05  EXTRA23I PIC  X(0001).
      *    -------------------------------
           05  CD24L PIC S9(0004) COMP.
           05  CD24F PIC  X(0001).
           05  FILLER REDEFINES CD24F.
               10  CD24A PIC  X(0001).
           05  CD24I PIC  X(0002).
      *    -------------------------------
           05  KIND24L PIC S9(0004) COMP.
           05  KIND24F PIC  X(0001).
           05  FILLER REDEFINES KIND24F.
               10  KIND24A PIC  X(0001).
           05  KIND24I PIC  X(0001).
      *    -------------------------------
           05  REFD24L PIC S9(0004) COMP.
           05  REFD24F PIC  X(0001).
           05  FILLER REDEFINES REFD24F.
               10  REFD24A PIC  X(0001).
           05  REFD24I PIC  X(0001).
      *    -------------------------------
           05  RMTRM24L PIC S9(0004) COMP.
           05  RMTRM24F PIC  X(0001).
           05  FILLER REDEFINES RMTRM24F.
               10  RMTRM24A PIC  X(0001).
           05  RMTRM24I PIC  X(0001).
      *    -------------------------------
           05  EXTRA24L PIC S9(0004) COMP.
           05  EXTRA24F PIC  X(0001).
           05  FILLER REDEFINES EXTRA24F.
               10  EXTRA24A PIC  X(0001).
           05  EXTRA24I PIC  X(0001).
      *    -------------------------------
           05  CD25L PIC S9(0004) COMP.
           05  CD25F PIC  X(0001).
           05  FILLER REDEFINES CD25F.
               10  CD25A PIC  X(0001).
           05  CD25I PIC  X(0002).
      *    -------------------------------
           05  KIND25L PIC S9(0004) COMP.
           05  KIND25F PIC  X(0001).
           05  FILLER REDEFINES KIND25F.
               10  KIND25A PIC  X(0001).
           05  KIND25I PIC  X(0001).
      *    -------------------------------
           05  REFD25L PIC S9(0004) COMP.
           05  REFD25F PIC  X(0001).
           05  FILLER REDEFINES REFD25F.
               10  REFD25A PIC  X(0001).
           05  REFD25I PIC  X(0001).
      *    -------------------------------
           05  RMTRM25L PIC S9(0004) COMP.
           05  RMTRM25F PIC  X(0001).
           05  FILLER REDEFINES RMTRM25F.
               10  RMTRM25A PIC  X(0001).
           05  RMTRM25I PIC  X(0001).
      *    -------------------------------
           05  EXTRA25L PIC S9(0004) COMP.
           05  EXTRA25F PIC  X(0001).
           05  FILLER REDEFINES EXTRA25F.
               10  EXTRA25A PIC  X(0001).
           05  EXTRA25I PIC  X(0001).
      *    -------------------------------
           05  CD26L PIC S9(0004) COMP.
           05  CD26F PIC  X(0001).
           05  FILLER REDEFINES CD26F.
               10  CD26A PIC  X(0001).
           05  CD26I PIC  X(0002).
      *    -------------------------------
           05  KIND26L PIC S9(0004) COMP.
           05  KIND26F PIC  X(0001).
           05  FILLER REDEFINES KIND26F.
               10  KIND26A PIC  X(0001).
           05  KIND26I PIC  X(0001).
      *    -------------------------------
           05  REFD26L PIC S9(0004) COMP.
           05  REFD26F PIC  X(0001).
           05  FILLER REDEFINES REFD26F.
               10  REFD26A PIC  X(0001).
           05  REFD26I PIC  X(0001).
      *    -------------------------------
           05  RMTRM26L PIC S9(0004) COMP.
           05  RMTRM26F PIC  X(0001).
           05  FILLER REDEFINES RMTRM26F.
               10  RMTRM26A PIC  X(0001).
           05  RMTRM26I PIC  X(0001).
      *    -------------------------------
           05  EXTRA26L PIC S9(0004) COMP.
           05  EXTRA26F PIC  X(0001).
           05  FILLER REDEFINES EXTRA26F.
               10  EXTRA26A PIC  X(0001).
           05  EXTRA26I PIC  X(0001).
      *    -------------------------------
           05  CD27L PIC S9(0004) COMP.
           05  CD27F PIC  X(0001).
           05  FILLER REDEFINES CD27F.
               10  CD27A PIC  X(0001).
           05  CD27I PIC  X(0002).
      *    -------------------------------
           05  KIND27L PIC S9(0004) COMP.
           05  KIND27F PIC  X(0001).
           05  FILLER REDEFINES KIND27F.
               10  KIND27A PIC  X(0001).
           05  KIND27I PIC  X(0001).
      *    -------------------------------
           05  REFD27L PIC S9(0004) COMP.
           05  REFD27F PIC  X(0001).
           05  FILLER REDEFINES REFD27F.
               10  REFD27A PIC  X(0001).
           05  REFD27I PIC  X(0001).
      *    -------------------------------
           05  RMTRM27L PIC S9(0004) COMP.
           05  RMTRM27F PIC  X(0001).
           05  FILLER REDEFINES RMTRM27F.
               10  RMTRM27A PIC  X(0001).
           05  RMTRM27I PIC  X(0001).
      *    -------------------------------
           05  EXTRA27L PIC S9(0004) COMP.
           05  EXTRA27F PIC  X(0001).
           05  FILLER REDEFINES EXTRA27F.
               10  EXTRA27A PIC  X(0001).
           05  EXTRA27I PIC  X(0001).
      *    -------------------------------
           05  CD28L PIC S9(0004) COMP.
           05  CD28F PIC  X(0001).
           05  FILLER REDEFINES CD28F.
               10  CD28A PIC  X(0001).
           05  CD28I PIC  X(0002).
      *    -------------------------------
           05  KIND28L PIC S9(0004) COMP.
           05  KIND28F PIC  X(0001).
           05  FILLER REDEFINES KIND28F.
               10  KIND28A PIC  X(0001).
           05  KIND28I PIC  X(0001).
      *    -------------------------------
           05  REFD28L PIC S9(0004) COMP.
           05  REFD28F PIC  X(0001).
           05  FILLER REDEFINES REFD28F.
               10  REFD28A PIC  X(0001).
           05  REFD28I PIC  X(0001).
      *    -------------------------------
           05  RMTRM28L PIC S9(0004) COMP.
           05  RMTRM28F PIC  X(0001).
           05  FILLER REDEFINES RMTRM28F.
               10  RMTRM28A PIC  X(0001).
           05  RMTRM28I PIC  X(0001).
      *    -------------------------------
           05  EXTRA28L PIC S9(0004) COMP.
           05  EXTRA28F PIC  X(0001).
           05  FILLER REDEFINES EXTRA28F.
               10  EXTRA28A PIC  X(0001).
           05  EXTRA28I PIC  X(0001).
      *    -------------------------------
           05  CD29L PIC S9(0004) COMP.
           05  CD29F PIC  X(0001).
           05  FILLER REDEFINES CD29F.
               10  CD29A PIC  X(0001).
           05  CD29I PIC  X(0002).
      *    -------------------------------
           05  KIND29L PIC S9(0004) COMP.
           05  KIND29F PIC  X(0001).
           05  FILLER REDEFINES KIND29F.
               10  KIND29A PIC  X(0001).
           05  KIND29I PIC  X(0001).
      *    -------------------------------
           05  REFD29L PIC S9(0004) COMP.
           05  REFD29F PIC  X(0001).
           05  FILLER REDEFINES REFD29F.
               10  REFD29A PIC  X(0001).
           05  REFD29I PIC  X(0001).
      *    -------------------------------
           05  RMTRM29L PIC S9(0004) COMP.
           05  RMTRM29F PIC  X(0001).
           05  FILLER REDEFINES RMTRM29F.
               10  RMTRM29A PIC  X(0001).
           05  RMTRM29I PIC  X(0001).
      *    -------------------------------
           05  EXTRA29L PIC S9(0004) COMP.
           05  EXTRA29F PIC  X(0001).
           05  FILLER REDEFINES EXTRA29F.
               10  EXTRA29A PIC  X(0001).
           05  EXTRA29I PIC  X(0001).
      *    -------------------------------
           05  CD30L PIC S9(0004) COMP.
           05  CD30F PIC  X(0001).
           05  FILLER REDEFINES CD30F.
               10  CD30A PIC  X(0001).
           05  CD30I PIC  X(0002).
      *    -------------------------------
           05  KIND30L PIC S9(0004) COMP.
           05  KIND30F PIC  X(0001).
           05  FILLER REDEFINES KIND30F.
               10  KIND30A PIC  X(0001).
           05  KIND30I PIC  X(0001).
      *    -------------------------------
           05  REFD30L PIC S9(0004) COMP.
           05  REFD30F PIC  X(0001).
           05  FILLER REDEFINES REFD30F.
               10  REFD30A PIC  X(0001).
           05  REFD30I PIC  X(0001).
      *    -------------------------------
           05  RMTRM30L PIC S9(0004) COMP.
           05  RMTRM30F PIC  X(0001).
           05  FILLER REDEFINES RMTRM30F.
               10  RMTRM30A PIC  X(0001).
           05  RMTRM30I PIC  X(0001).
      *    -------------------------------
           05  EXTRA30L PIC S9(0004) COMP.
           05  EXTRA30F PIC  X(0001).
           05  FILLER REDEFINES EXTRA30F.
               10  EXTRA30A PIC  X(0001).
           05  EXTRA30I PIC  X(0001).
      *    -------------------------------
           05  CD31L PIC S9(0004) COMP.
           05  CD31F PIC  X(0001).
           05  FILLER REDEFINES CD31F.
               10  CD31A PIC  X(0001).
           05  CD31I PIC  X(0002).
      *    -------------------------------
           05  KIND31L PIC S9(0004) COMP.
           05  KIND31F PIC  X(0001).
           05  FILLER REDEFINES KIND31F.
               10  KIND31A PIC  X(0001).
           05  KIND31I PIC  X(0001).
      *    -------------------------------
           05  REFD31L PIC S9(0004) COMP.
           05  REFD31F PIC  X(0001).
           05  FILLER REDEFINES REFD31F.
               10  REFD31A PIC  X(0001).
           05  REFD31I PIC  X(0001).
      *    -------------------------------
           05  RMTRM31L PIC S9(0004) COMP.
           05  RMTRM31F PIC  X(0001).
           05  FILLER REDEFINES RMTRM31F.
               10  RMTRM31A PIC  X(0001).
           05  RMTRM31I PIC  X(0001).
      *    -------------------------------
           05  EXTRA31L PIC S9(0004) COMP.
           05  EXTRA31F PIC  X(0001).
           05  FILLER REDEFINES EXTRA31F.
               10  EXTRA31A PIC  X(0001).
           05  EXTRA31I PIC  X(0001).
      *    -------------------------------
           05  CD32L PIC S9(0004) COMP.
           05  CD32F PIC  X(0001).
           05  FILLER REDEFINES CD32F.
               10  CD32A PIC  X(0001).
           05  CD32I PIC  X(0002).
      *    -------------------------------
           05  KIND32L PIC S9(0004) COMP.
           05  KIND32F PIC  X(0001).
           05  FILLER REDEFINES KIND32F.
               10  KIND32A PIC  X(0001).
           05  KIND32I PIC  X(0001).
      *    -------------------------------
           05  REFD32L PIC S9(0004) COMP.
           05  REFD32F PIC  X(0001).
           05  FILLER REDEFINES REFD32F.
               10  REFD32A PIC  X(0001).
           05  REFD32I PIC  X(0001).
      *    -------------------------------
           05  RMTRM32L PIC S9(0004) COMP.
           05  RMTRM32F PIC  X(0001).
           05  FILLER REDEFINES RMTRM32F.
               10  RMTRM32A PIC  X(0001).
           05  RMTRM32I PIC  X(0001).
      *    -------------------------------
           05  EXTRA32L PIC S9(0004) COMP.
           05  EXTRA32F PIC  X(0001).
           05  FILLER REDEFINES EXTRA32F.
               10  EXTRA32A PIC  X(0001).
           05  EXTRA32I PIC  X(0001).
      *    -------------------------------
           05  CD33L PIC S9(0004) COMP.
           05  CD33F PIC  X(0001).
           05  FILLER REDEFINES CD33F.
               10  CD33A PIC  X(0001).
           05  CD33I PIC  X(0002).
      *    -------------------------------
           05  KIND33L PIC S9(0004) COMP.
           05  KIND33F PIC  X(0001).
           05  FILLER REDEFINES KIND33F.
               10  KIND33A PIC  X(0001).
           05  KIND33I PIC  X(0001).
      *    -------------------------------
           05  REFD33L PIC S9(0004) COMP.
           05  REFD33F PIC  X(0001).
           05  FILLER REDEFINES REFD33F.
               10  REFD33A PIC  X(0001).
           05  REFD33I PIC  X(0001).
      *    -------------------------------
           05  RMTRM33L PIC S9(0004) COMP.
           05  RMTRM33F PIC  X(0001).
           05  FILLER REDEFINES RMTRM33F.
               10  RMTRM33A PIC  X(0001).
           05  RMTRM33I PIC  X(0001).
      *    -------------------------------
           05  EXTRA33L PIC S9(0004) COMP.
           05  EXTRA33F PIC  X(0001).
           05  FILLER REDEFINES EXTRA33F.
               10  EXTRA33A PIC  X(0001).
           05  EXTRA33I PIC  X(0001).
      *    -------------------------------
           05  CD34L PIC S9(0004) COMP.
           05  CD34F PIC  X(0001).
           05  FILLER REDEFINES CD34F.
               10  CD34A PIC  X(0001).
           05  CD34I PIC  X(0002).
      *    -------------------------------
           05  KIND34L PIC S9(0004) COMP.
           05  KIND34F PIC  X(0001).
           05  FILLER REDEFINES KIND34F.
               10  KIND34A PIC  X(0001).
           05  KIND34I PIC  X(0001).
      *    -------------------------------
           05  REFD34L PIC S9(0004) COMP.
           05  REFD34F PIC  X(0001).
           05  FILLER REDEFINES REFD34F.
               10  REFD34A PIC  X(0001).
           05  REFD34I PIC  X(0001).
      *    -------------------------------
           05  RMTRM34L PIC S9(0004) COMP.
           05  RMTRM34F PIC  X(0001).
           05  FILLER REDEFINES RMTRM34F.
               10  RMTRM34A PIC  X(0001).
           05  RMTRM34I PIC  X(0001).
      *    -------------------------------
           05  EXTRA34L PIC S9(0004) COMP.
           05  EXTRA34F PIC  X(0001).
           05  FILLER REDEFINES EXTRA34F.
               10  EXTRA34A PIC  X(0001).
           05  EXTRA34I PIC  X(0001).
      *    -------------------------------
           05  CD35L PIC S9(0004) COMP.
           05  CD35F PIC  X(0001).
           05  FILLER REDEFINES CD35F.
               10  CD35A PIC  X(0001).
           05  CD35I PIC  X(0002).
      *    -------------------------------
           05  KIND35L PIC S9(0004) COMP.
           05  KIND35F PIC  X(0001).
           05  FILLER REDEFINES KIND35F.
               10  KIND35A PIC  X(0001).
           05  KIND35I PIC  X(0001).
      *    -------------------------------
           05  REFD35L PIC S9(0004) COMP.
           05  REFD35F PIC  X(0001).
           05  FILLER REDEFINES REFD35F.
               10  REFD35A PIC  X(0001).
           05  REFD35I PIC  X(0001).
      *    -------------------------------
           05  RMTRM35L PIC S9(0004) COMP.
           05  RMTRM35F PIC  X(0001).
           05  FILLER REDEFINES RMTRM35F.
               10  RMTRM35A PIC  X(0001).
           05  RMTRM35I PIC  X(0001).
      *    -------------------------------
           05  EXTRA35L PIC S9(0004) COMP.
           05  EXTRA35F PIC  X(0001).
           05  FILLER REDEFINES EXTRA35F.
               10  EXTRA35A PIC  X(0001).
           05  EXTRA35I PIC  X(0001).
      *    -------------------------------
           05  CD36L PIC S9(0004) COMP.
           05  CD36F PIC  X(0001).
           05  FILLER REDEFINES CD36F.
               10  CD36A PIC  X(0001).
           05  CD36I PIC  X(0002).
      *    -------------------------------
           05  KIND36L PIC S9(0004) COMP.
           05  KIND36F PIC  X(0001).
           05  FILLER REDEFINES KIND36F.
               10  KIND36A PIC  X(0001).
           05  KIND36I PIC  X(0001).
      *    -------------------------------
           05  REFD36L PIC S9(0004) COMP.
           05  REFD36F PIC  X(0001).
           05  FILLER REDEFINES REFD36F.
               10  REFD36A PIC  X(0001).
           05  REFD36I PIC  X(0001).
      *    -------------------------------
           05  RMTRM36L PIC S9(0004) COMP.
           05  RMTRM36F PIC  X(0001).
           05  FILLER REDEFINES RMTRM36F.
               10  RMTRM36A PIC  X(0001).
           05  RMTRM36I PIC  X(0001).
      *    -------------------------------
           05  EXTRA36L PIC S9(0004) COMP.
           05  EXTRA36F PIC  X(0001).
           05  FILLER REDEFINES EXTRA36F.
               10  EXTRA36A PIC  X(0001).
           05  EXTRA36I PIC  X(0001).
      *    -------------------------------
           05  CD37L PIC S9(0004) COMP.
           05  CD37F PIC  X(0001).
           05  FILLER REDEFINES CD37F.
               10  CD37A PIC  X(0001).
           05  CD37I PIC  X(0002).
      *    -------------------------------
           05  KIND37L PIC S9(0004) COMP.
           05  KIND37F PIC  X(0001).
           05  FILLER REDEFINES KIND37F.
               10  KIND37A PIC  X(0001).
           05  KIND37I PIC  X(0001).
      *    -------------------------------
           05  REFD37L PIC S9(0004) COMP.
           05  REFD37F PIC  X(0001).
           05  FILLER REDEFINES REFD37F.
               10  REFD37A PIC  X(0001).
           05  REFD37I PIC  X(0001).
      *    -------------------------------
           05  RMTRM37L PIC S9(0004) COMP.
           05  RMTRM37F PIC  X(0001).
           05  FILLER REDEFINES RMTRM37F.
               10  RMTRM37A PIC  X(0001).
           05  RMTRM37I PIC  X(0001).
      *    -------------------------------
           05  EXTRA37L PIC S9(0004) COMP.
           05  EXTRA37F PIC  X(0001).
           05  FILLER REDEFINES EXTRA37F.
               10  EXTRA37A PIC  X(0001).
           05  EXTRA37I PIC  X(0001).
      *    -------------------------------
           05  CD38L PIC S9(0004) COMP.
           05  CD38F PIC  X(0001).
           05  FILLER REDEFINES CD38F.
               10  CD38A PIC  X(0001).
           05  CD38I PIC  X(0002).
      *    -------------------------------
           05  KIND38L PIC S9(0004) COMP.
           05  KIND38F PIC  X(0001).
           05  FILLER REDEFINES KIND38F.
               10  KIND38A PIC  X(0001).
           05  KIND38I PIC  X(0001).
      *    -------------------------------
           05  REFD38L PIC S9(0004) COMP.
           05  REFD38F PIC  X(0001).
           05  FILLER REDEFINES REFD38F.
               10  REFD38A PIC  X(0001).
           05  REFD38I PIC  X(0001).
      *    -------------------------------
           05  RMTRM38L PIC S9(0004) COMP.
           05  RMTRM38F PIC  X(0001).
           05  FILLER REDEFINES RMTRM38F.
               10  RMTRM38A PIC  X(0001).
           05  RMTRM38I PIC  X(0001).
      *    -------------------------------
           05  EXTRA38L PIC S9(0004) COMP.
           05  EXTRA38F PIC  X(0001).
           05  FILLER REDEFINES EXTRA38F.
               10  EXTRA38A PIC  X(0001).
           05  EXTRA38I PIC  X(0001).
      *    -------------------------------
           05  CD39L PIC S9(0004) COMP.
           05  CD39F PIC  X(0001).
           05  FILLER REDEFINES CD39F.
               10  CD39A PIC  X(0001).
           05  CD39I PIC  X(0002).
      *    -------------------------------
           05  KIND39L PIC S9(0004) COMP.
           05  KIND39F PIC  X(0001).
           05  FILLER REDEFINES KIND39F.
               10  KIND39A PIC  X(0001).
           05  KIND39I PIC  X(0001).
      *    -------------------------------
           05  REFD39L PIC S9(0004) COMP.
           05  REFD39F PIC  X(0001).
           05  FILLER REDEFINES REFD39F.
               10  REFD39A PIC  X(0001).
           05  REFD39I PIC  X(0001).
      *    -------------------------------
           05  RMTRM39L PIC S9(0004) COMP.
           05  RMTRM39F PIC  X(0001).
           05  FILLER REDEFINES RMTRM39F.
               10  RMTRM39A PIC  X(0001).
           05  RMTRM39I PIC  X(0001).
      *    -------------------------------
           05  EXTRA39L PIC S9(0004) COMP.
           05  EXTRA39F PIC  X(0001).
           05  FILLER REDEFINES EXTRA39F.
               10  EXTRA39A PIC  X(0001).
           05  EXTRA39I PIC  X(0001).
      *    -------------------------------
           05  CD40L PIC S9(0004) COMP.
           05  CD40F PIC  X(0001).
           05  FILLER REDEFINES CD40F.
               10  CD40A PIC  X(0001).
           05  CD40I PIC  X(0002).
      *    -------------------------------
           05  KIND40L PIC S9(0004) COMP.
           05  KIND40F PIC  X(0001).
           05  FILLER REDEFINES KIND40F.
               10  KIND40A PIC  X(0001).
           05  KIND40I PIC  X(0001).
      *    -------------------------------
           05  REFD40L PIC S9(0004) COMP.
           05  REFD40F PIC  X(0001).
           05  FILLER REDEFINES REFD40F.
               10  REFD40A PIC  X(0001).
           05  REFD40I PIC  X(0001).
      *    -------------------------------
           05  RMTRM40L PIC S9(0004) COMP.
           05  RMTRM40F PIC  X(0001).
           05  FILLER REDEFINES RMTRM40F.
               10  RMTRM40A PIC  X(0001).
           05  RMTRM40I PIC  X(0001).
      *    -------------------------------
           05  EXTRA40L PIC S9(0004) COMP.
           05  EXTRA40F PIC  X(0001).
           05  FILLER REDEFINES EXTRA40F.
               10  EXTRA40A PIC  X(0001).
           05  EXTRA40I PIC  X(0001).
      *    -------------------------------
           05  CD41L PIC S9(0004) COMP.
           05  CD41F PIC  X(0001).
           05  FILLER REDEFINES CD41F.
               10  CD41A PIC  X(0001).
           05  CD41I PIC  X(0002).
      *    -------------------------------
           05  KIND41L PIC S9(0004) COMP.
           05  KIND41F PIC  X(0001).
           05  FILLER REDEFINES KIND41F.
               10  KIND41A PIC  X(0001).
           05  KIND41I PIC  X(0001).
      *    -------------------------------
           05  REFD41L PIC S9(0004) COMP.
           05  REFD41F PIC  X(0001).
           05  FILLER REDEFINES REFD41F.
               10  REFD41A PIC  X(0001).
           05  REFD41I PIC  X(0001).
      *    -------------------------------
           05  RMTRM41L PIC S9(0004) COMP.
           05  RMTRM41F PIC  X(0001).
           05  FILLER REDEFINES RMTRM41F.
               10  RMTRM41A PIC  X(0001).
           05  RMTRM41I PIC  X(0001).
      *    -------------------------------
           05  EXTRA41L PIC S9(0004) COMP.
           05  EXTRA41F PIC  X(0001).
           05  FILLER REDEFINES EXTRA41F.
               10  EXTRA41A PIC  X(0001).
           05  EXTRA41I PIC  X(0001).
      *    -------------------------------
           05  CD42L PIC S9(0004) COMP.
           05  CD42F PIC  X(0001).
           05  FILLER REDEFINES CD42F.
               10  CD42A PIC  X(0001).
           05  CD42I PIC  X(0002).
      *    -------------------------------
           05  KIND42L PIC S9(0004) COMP.
           05  KIND42F PIC  X(0001).
           05  FILLER REDEFINES KIND42F.
               10  KIND42A PIC  X(0001).
           05  KIND42I PIC  X(0001).
      *    -------------------------------
           05  REFD42L PIC S9(0004) COMP.
           05  REFD42F PIC  X(0001).
           05  FILLER REDEFINES REFD42F.
               10  REFD42A PIC  X(0001).
           05  REFD42I PIC  X(0001).
      *    -------------------------------
           05  RMTRM42L PIC S9(0004) COMP.
           05  RMTRM42F PIC  X(0001).
           05  FILLER REDEFINES RMTRM42F.
               10  RMTRM42A PIC  X(0001).
           05  RMTRM42I PIC  X(0001).
      *    -------------------------------
           05  EXTRA42L PIC S9(0004) COMP.
           05  EXTRA42F PIC  X(0001).
           05  FILLER REDEFINES EXTRA42F.
               10  EXTRA42A PIC  X(0001).
           05  EXTRA42I PIC  X(0001).
      *    -------------------------------
           05  CD43L PIC S9(0004) COMP.
           05  CD43F PIC  X(0001).
           05  FILLER REDEFINES CD43F.
               10  CD43A PIC  X(0001).
           05  CD43I PIC  X(0002).
      *    -------------------------------
           05  KIND43L PIC S9(0004) COMP.
           05  KIND43F PIC  X(0001).
           05  FILLER REDEFINES KIND43F.
               10  KIND43A PIC  X(0001).
           05  KIND43I PIC  X(0001).
      *    -------------------------------
           05  REFD43L PIC S9(0004) COMP.
           05  REFD43F PIC  X(0001).
           05  FILLER REDEFINES REFD43F.
               10  REFD43A PIC  X(0001).
           05  REFD43I PIC  X(0001).
      *    -------------------------------
           05  RMTRM43L PIC S9(0004) COMP.
           05  RMTRM43F PIC  X(0001).
           05  FILLER REDEFINES RMTRM43F.
               10  RMTRM43A PIC  X(0001).
           05  RMTRM43I PIC  X(0001).
      *    -------------------------------
           05  EXTRA43L PIC S9(0004) COMP.
           05  EXTRA43F PIC  X(0001).
           05  FILLER REDEFINES EXTRA43F.
               10  EXTRA43A PIC  X(0001).
           05  EXTRA43I PIC  X(0001).
      *    -------------------------------
           05  CD44L PIC S9(0004) COMP.
           05  CD44F PIC  X(0001).
           05  FILLER REDEFINES CD44F.
               10  CD44A PIC  X(0001).
           05  CD44I PIC  X(0002).
      *    -------------------------------
           05  KIND44L PIC S9(0004) COMP.
           05  KIND44F PIC  X(0001).
           05  FILLER REDEFINES KIND44F.
               10  KIND44A PIC  X(0001).
           05  KIND44I PIC  X(0001).
      *    -------------------------------
           05  REFD44L PIC S9(0004) COMP.
           05  REFD44F PIC  X(0001).
           05  FILLER REDEFINES REFD44F.
               10  REFD44A PIC  X(0001).
           05  REFD44I PIC  X(0001).
      *    -------------------------------
           05  RMTRM44L PIC S9(0004) COMP.
           05  RMTRM44F PIC  X(0001).
           05  FILLER REDEFINES RMTRM44F.
               10  RMTRM44A PIC  X(0001).
           05  RMTRM44I PIC  X(0001).
      *    -------------------------------
           05  EXTRA44L PIC S9(0004) COMP.
           05  EXTRA44F PIC  X(0001).
           05  FILLER REDEFINES EXTRA44F.
               10  EXTRA44A PIC  X(0001).
           05  EXTRA44I PIC  X(0001).
      *    -------------------------------
           05  CD45L PIC S9(0004) COMP.
           05  CD45F PIC  X(0001).
           05  FILLER REDEFINES CD45F.
               10  CD45A PIC  X(0001).
           05  CD45I PIC  X(0002).
      *    -------------------------------
           05  KIND45L PIC S9(0004) COMP.
           05  KIND45F PIC  X(0001).
           05  FILLER REDEFINES KIND45F.
               10  KIND45A PIC  X(0001).
           05  KIND45I PIC  X(0001).
      *    -------------------------------
           05  REFD45L PIC S9(0004) COMP.
           05  REFD45F PIC  X(0001).
           05  FILLER REDEFINES REFD45F.
               10  REFD45A PIC  X(0001).
           05  REFD45I PIC  X(0001).
      *    -------------------------------
           05  RMTRM45L PIC S9(0004) COMP.
           05  RMTRM45F PIC  X(0001).
           05  FILLER REDEFINES RMTRM45F.
               10  RMTRM45A PIC  X(0001).
           05  RMTRM45I PIC  X(0001).
      *    -------------------------------
           05  EXTRA45L PIC S9(0004) COMP.
           05  EXTRA45F PIC  X(0001).
           05  FILLER REDEFINES EXTRA45F.
               10  EXTRA45A PIC  X(0001).
           05  EXTRA45I PIC  X(0001).
      *    -------------------------------
           05  CD46L PIC S9(0004) COMP.
           05  CD46F PIC  X(0001).
           05  FILLER REDEFINES CD46F.
               10  CD46A PIC  X(0001).
           05  CD46I PIC  X(0002).
      *    -------------------------------
           05  KIND46L PIC S9(0004) COMP.
           05  KIND46F PIC  X(0001).
           05  FILLER REDEFINES KIND46F.
               10  KIND46A PIC  X(0001).
           05  KIND46I PIC  X(0001).
      *    -------------------------------
           05  REFD46L PIC S9(0004) COMP.
           05  REFD46F PIC  X(0001).
           05  FILLER REDEFINES REFD46F.
               10  REFD46A PIC  X(0001).
           05  REFD46I PIC  X(0001).
      *    -------------------------------
           05  RMTRM46L PIC S9(0004) COMP.
           05  RMTRM46F PIC  X(0001).
           05  FILLER REDEFINES RMTRM46F.
               10  RMTRM46A PIC  X(0001).
           05  RMTRM46I PIC  X(0001).
      *    -------------------------------
           05  EXTRA46L PIC S9(0004) COMP.
           05  EXTRA46F PIC  X(0001).
           05  FILLER REDEFINES EXTRA46F.
               10  EXTRA46A PIC  X(0001).
           05  EXTRA46I PIC  X(0001).
      *    -------------------------------
           05  CD47L PIC S9(0004) COMP.
           05  CD47F PIC  X(0001).
           05  FILLER REDEFINES CD47F.
               10  CD47A PIC  X(0001).
           05  CD47I PIC  X(0002).
      *    -------------------------------
           05  KIND47L PIC S9(0004) COMP.
           05  KIND47F PIC  X(0001).
           05  FILLER REDEFINES KIND47F.
               10  KIND47A PIC  X(0001).
           05  KIND47I PIC  X(0001).
      *    -------------------------------
           05  REFD47L PIC S9(0004) COMP.
           05  REFD47F PIC  X(0001).
           05  FILLER REDEFINES REFD47F.
               10  REFD47A PIC  X(0001).
           05  REFD47I PIC  X(0001).
      *    -------------------------------
           05  RMTRM47L PIC S9(0004) COMP.
           05  RMTRM47F PIC  X(0001).
           05  FILLER REDEFINES RMTRM47F.
               10  RMTRM47A PIC  X(0001).
           05  RMTRM47I PIC  X(0001).
      *    -------------------------------
           05  EXTRA47L PIC S9(0004) COMP.
           05  EXTRA47F PIC  X(0001).
           05  FILLER REDEFINES EXTRA47F.
               10  EXTRA47A PIC  X(0001).
           05  EXTRA47I PIC  X(0001).
      *    -------------------------------
           05  CD48L PIC S9(0004) COMP.
           05  CD48F PIC  X(0001).
           05  FILLER REDEFINES CD48F.
               10  CD48A PIC  X(0001).
           05  CD48I PIC  X(0002).
      *    -------------------------------
           05  KIND48L PIC S9(0004) COMP.
           05  KIND48F PIC  X(0001).
           05  FILLER REDEFINES KIND48F.
               10  KIND48A PIC  X(0001).
           05  KIND48I PIC  X(0001).
      *    -------------------------------
           05  REFD48L PIC S9(0004) COMP.
           05  REFD48F PIC  X(0001).
           05  FILLER REDEFINES REFD48F.
               10  REFD48A PIC  X(0001).
           05  REFD48I PIC  X(0001).
      *    -------------------------------
           05  RMTRM48L PIC S9(0004) COMP.
           05  RMTRM48F PIC  X(0001).
           05  FILLER REDEFINES RMTRM48F.
               10  RMTRM48A PIC  X(0001).
           05  RMTRM48I PIC  X(0001).
      *    -------------------------------
           05  EXTRA48L PIC S9(0004) COMP.
           05  EXTRA48F PIC  X(0001).
           05  FILLER REDEFINES EXTRA48F.
               10  EXTRA48A PIC  X(0001).
           05  EXTRA48I PIC  X(0001).
      *    -------------------------------
           05  CD49L PIC S9(0004) COMP.
           05  CD49F PIC  X(0001).
           05  FILLER REDEFINES CD49F.
               10  CD49A PIC  X(0001).
           05  CD49I PIC  X(0002).
      *    -------------------------------
           05  KIND49L PIC S9(0004) COMP.
           05  KIND49F PIC  X(0001).
           05  FILLER REDEFINES KIND49F.
               10  KIND49A PIC  X(0001).
           05  KIND49I PIC  X(0001).
      *    -------------------------------
           05  REFD49L PIC S9(0004) COMP.
           05  REFD49F PIC  X(0001).
           05  FILLER REDEFINES REFD49F.
               10  REFD49A PIC  X(0001).
           05  REFD49I PIC  X(0001).
      *    -------------------------------
           05  RMTRM49L PIC S9(0004) COMP.
           05  RMTRM49F PIC  X(0001).
           05  FILLER REDEFINES RMTRM49F.
               10  RMTRM49A PIC  X(0001).
           05  RMTRM49I PIC  X(0001).
      *    -------------------------------
           05  EXTRA49L PIC S9(0004) COMP.
           05  EXTRA49F PIC  X(0001).
           05  FILLER REDEFINES EXTRA49F.
               10  EXTRA49A PIC  X(0001).
           05  EXTRA49I PIC  X(0001).
      *    -------------------------------
           05  CD50L PIC S9(0004) COMP.
           05  CD50F PIC  X(0001).
           05  FILLER REDEFINES CD50F.
               10  CD50A PIC  X(0001).
           05  CD50I PIC  X(0002).
      *    -------------------------------
           05  KIND50L PIC S9(0004) COMP.
           05  KIND50F PIC  X(0001).
           05  FILLER REDEFINES KIND50F.
               10  KIND50A PIC  X(0001).
           05  KIND50I PIC  X(0001).
      *    -------------------------------
           05  REFD50L PIC S9(0004) COMP.
           05  REFD50F PIC  X(0001).
           05  FILLER REDEFINES REFD50F.
               10  REFD50A PIC  X(0001).
           05  REFD50I PIC  X(0001).
      *    -------------------------------
           05  RMTRM50L PIC S9(0004) COMP.
           05  RMTRM50F PIC  X(0001).
           05  FILLER REDEFINES RMTRM50F.
               10  RMTRM50A PIC  X(0001).
           05  RMTRM50I PIC  X(0001).
      *    -------------------------------
           05  EXTRA50L PIC S9(0004) COMP.
           05  EXTRA50F PIC  X(0001).
           05  FILLER REDEFINES EXTRA50F.
               10  EXTRA50A PIC  X(0001).
           05  EXTRA50I PIC  X(0001).
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
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  9(2).
       01  EL106BO REDEFINES EL106BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STABRO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STNAMEO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA16O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD17O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA17O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD18O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA18O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD19O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA19O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD20O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA20O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD21O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND21O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD21O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM21O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA21O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD22O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND22O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD22O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM22O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA22O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD23O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND23O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD23O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM23O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA23O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD24O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND24O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD24O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM24O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA24O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD25O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND25O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD25O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM25O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA25O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD26O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND26O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD26O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM26O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA26O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD27O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND27O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD27O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM27O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA27O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD28O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND28O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD28O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM28O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA28O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD29O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND29O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD29O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM29O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA29O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD30O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND30O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD30O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM30O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA30O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD31O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND31O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD31O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM31O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA31O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD32O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND32O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD32O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM32O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA32O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD33O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND33O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD33O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM33O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA33O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD34O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND34O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD34O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM34O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA34O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD35O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND35O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD35O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM35O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA35O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD36O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND36O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD36O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM36O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA36O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD37O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND37O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD37O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM37O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA37O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD38O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND38O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD38O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM38O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA38O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD39O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND39O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD39O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM39O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA39O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD40O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND40O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD40O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM40O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA40O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD41O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND41O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD41O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM41O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA41O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD42O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND42O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD42O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM42O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA42O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD43O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND43O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD43O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM43O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA43O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD44O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND44O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD44O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM44O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA44O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD45O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND45O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD45O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM45O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA45O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD46O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND46O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD46O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM46O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA46O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD47O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND47O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD47O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM47O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA47O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD48O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND48O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD48O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM48O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA48O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD49O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND49O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD49O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM49O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA49O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CD50O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  KIND50O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFD50O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTRM50O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTRA50O PIC  X(0001).
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
           05  ERRMSG1O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
00172  01  MAP-REDEF REDEFINES EL106BI.
00173      12  FILLER              PIC X(76).
00174      12  BENEFIT-CONTROLS OCCURS 50 TIMES INDEXED BY SC-INDX.
00175          16  SC-CDL          PIC S9(4)  COMP.
00176          16  SC-CDA          PIC X.
00177          16  SC-CD           PIC XX.
00178          16  SC-KINDL        PIC S9(4)  COMP.
00179          16  SC-KINDA        PIC X.
00180          16  SC-KIND         PIC X.
00181          16  SC-REFL         PIC S9(4)  COMP.
00182          16  SC-REFA         PIC X.
00183          16  SC-REF          PIC X.
00184          16  SC-REMTERML     PIC S9(4)  COMP.
00185          16  SC-REMTERMA     PIC X.
00186          16  SC-REMTERM      PIC X.
022415         16  sc-extral       pic s9(4)  comp.
022415         16  sc-extraa       pic x.
022415         16  sc-extra        pic x.
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
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
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
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
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00189  01  DFHCOMMAREA             PIC X(1500).
00190 *01 PARMLIST .
00191 *    02  FILLER              PIC S9(8)   COMP.
00192 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.
00193 *    02  ELLETR-POINTER      PIC S9(8)   COMP.
00194      EJECT
00195 *                            COPY ELCCNTL.
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
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
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
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
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
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
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
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
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
00196      EJECT
00197 *                            COPY ELCTEXT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCTEXT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
00008 *                                     FORM LETTERS,              *
00009 *                                     CERT FORM DISPLAY.
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 100   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
00015 *       ALTERNATE INDEX = NONE                                   *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
00018 *       ALTERNATE INDEX = NONE                                   *
00019 *                                                                *
00020 *   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
00021 *       ALTERNATE INDEX = NONE                                   *
00022 *                                                                *
00023 *   LOG = NO                                                     *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  TEXT-FILES.
00027      12  TEXT-FILE-ID                PIC XX.
00028          88  FORMS-FILE-TEXT            VALUE 'TF'.
00029          88  LETTER-FILE-TEXT           VALUE 'TL'.
00030          88  HELP-FILE-TEXT             VALUE 'TH'.
00031
00032      12  TX-CONTROL-PRIMARY.
00033          16  TX-COMPANY-CD           PIC X.
00034              88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
00035          16  TX-ACCESS-CD-GENL       PIC X(12).
00036
00037          16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00038              20  TX-LETTER-NO        PIC X(4).
00039              20  FILLER              PIC X(8).
00040
00041          16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00042              20  TX-FORM-NO          PIC X(12).
00043
00044          16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00045              20  TX-HELP-TYPE        PIC X.
00046                  88  HELP-FOR-GENERAL   VALUE ' '.
00047                  88  HELP-BY-SCREEN     VALUE 'S'.
00048                  88  HELP-BY-ERROR      VALUE 'E'.
00049              20  TX-SCREEN-OR-ERROR  PIC X(4).
00050                  88  GENERAL-INFO-HELP  VALUE '0000'.
00051              20  TX-HELP-FOR-COMPANY  PIC XXX.
00052                  88  NOT-COMPANY-SPECIFIC VALUE '   '.
00053              20  FILLER              PIC X(4).
00054
00055          16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
00056
00057      12  TX-PROCESS-CONTROL          PIC XX.
00058          88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
00059
00060      12  TX-TEXT-LINE                PIC X(70).
00061
00062      12  TX-FORM-SQUEEZE-CONTROL     PIC X.
00063          88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
00064          88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
00065          88  TX-VALID-FORM-SQUEEZE-VALUE
00066                                         VALUE 'Y' ' '.
00067
00068      12  TX-LINE-SQUEEZE-CONTROL     PIC X.
00069          88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
00070          88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
00071          88  TX-DO-NOT-ADJUST           VALUE 'N'.
00072          88  TX-FORM-CONTROL-LINE       VALUE 'K'.
00073          88  TX-NEW-PARAGRAPH           VALUE 'P'.
00074          88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
00075          88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
00076                                               'K' 'N' ' '
00077                                               'Z'.
00078
00079      12  TX-ARCHIVE-SW               PIC X.
00080          88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
00081          88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
00082          88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
00083
00084      12  TX-LAST-MAINTENANCED-BY     PIC X(4).
00085      12  TX-LAST-MAINTENANCED-DT     PIC X(2).
00086
00087      12  TX-BSR-CODE                 PIC X.
00088          88  TX-BSR-LETTER              VALUE 'B'.
00089          88  TX-NON-BSR-LETTER          VALUE ' '.
00090
00091      12  FILLER                      PIC X.
00092 *****************************************************************
00198      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                TEXT-FILES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1061' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00200
00201      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00202      MOVE '5'                   TO DC-OPTION-CODE.
00203      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00204      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00205      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00206
00207      MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.
00208      MOVE 2                     TO EMI-NUMBER-OF-LINES
00209                                    EMI-SWITCH2.
00210
00211      MOVE EIBTRMID              TO QID-TERM.
00212
00213      IF EIBCALEN = 0
00214          GO TO 8800-UNAUTHORIZED-ACCESS.
00215
00216      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00217          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM
00218        ELSE
00219          MOVE SPACES             TO RETURNED-FROM.
00220
00221      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00222          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00223              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00224              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00225              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00226              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00227              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00228              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00229              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00230              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00231          ELSE
00232              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00233              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00234              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00235              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00236              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00237              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00238              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00239              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00240
00241      MOVE 'N' TO WS-DISPLAY-SW.
00242
00243      
      * EXEC CICS HANDLE CONDITION
00244 *        DUPREC  (8850-DUPREC)
00245 *        NOTOPEN (8870-NOTOPEN)
00246 *        NOTFND  (8880-NOT-FOUND)
00247 *        PGMIDERR(9600-PGMID-ERROR)
00248 *        ERROR   (9990-ABEND)
00249 *    END-EXEC.
      *    MOVE '"$%JIL.               ! " #00005085' TO DFHEIV0
           MOVE X'2224254A494C2E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035303835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00250
00251      IF EIBTRNID NOT = TRANS-ID
00252         IF RETURNED-FROM = XCTL-6565
00253             MOVE LOW-VALUES           TO EL106BO
00254             PERFORM 6500-RECOVER-TEMP-STORAGE THRU 6500-EXIT
00255             GO TO 8100-SEND-INITIAL-MAP
00256         ELSE
00257             MOVE LOW-VALUES           TO EL106BO
00258             IF PI-WS-STATE EQUAL LOW-VALUES OR SPACES
00259                 GO TO 8100-SEND-INITIAL-MAP
00260             ELSE
00261                 MOVE PI-WS-STATE          TO STCDI
00262                 MOVE 'Y'                  TO WS-DISPLAY-SW
00263                 GO TO 1000-SHOW-STATE.
00264
00265      IF EIBAID = DFHCLEAR
00266          GO TO 9400-CLEAR.
00267
00268      IF NOT SYSTEM-DISPLAY-CAP
00269          MOVE 'READ'         TO SM-READ
00270          PERFORM 9995-SECURITY-VIOLATION
00271          MOVE ER-0070        TO EMI-ERROR
00272          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00273          GO TO 8100-SEND-INITIAL-MAP.
00274
00275      EJECT
00276  0200-RECEIVE.
00277      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00278          MOVE LOW-VALUES TO EL106BI
00279          MOVE ER-7008    TO EMI-ERROR
00280          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00281          MOVE -1         TO MAINTL
00282          GO TO 8200-SEND-DATAONLY.
00283
00284      
      * EXEC CICS RECEIVE
00285 *        MAP   (MAP-NAME)
00286 *        MAPSET(MAPSET-NAME)
00287 *        INTO  (EL106BI)
00288 *    END-EXEC.
           MOVE LENGTH OF
            EL106BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005126' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL106BI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00289
00290      IF ENTERPFL = 0
00291          GO TO 0300-CHECK-PFKEYS.
00292
00293      IF EIBAID NOT = DFHENTER
00294          MOVE ER-0004 TO EMI-ERROR
00295          GO TO 0320-INPUT-ERROR.
00296
00297      IF (ENTERPFI NUMERIC) AND (ENTERPFI GREATER 0 AND LESS 25)
00298          MOVE PF-VALUES (ENTERPFI) TO EIBAID
00299      ELSE
00300          MOVE ER-0029 TO EMI-ERROR
00301          GO TO 0320-INPUT-ERROR.
00302
00303  0300-CHECK-PFKEYS.
00304      IF EIBAID = DFHPF23
00305          GO TO 8810-PF23.
00306
00307      IF EIBAID = DFHPF24
00308          GO TO 9200-RETURN-MAIN-MENU.
00309
00310      IF EIBAID = DFHPF12
00311          GO TO 9500-PF12.
00312
00313      IF MAINTL NOT = 0  AND
00314         EIBAID NOT = DFHENTER
00315           MOVE ER-0050 TO EMI-ERROR
00316           GO TO 0320-INPUT-ERROR.
00317
00318      IF EIBAID = DFHPF1
00319          GO TO 5000-FIND-NEXT-STATE.
00320
00321      IF EIBAID = DFHPF2
00322          GO TO 5500-FIND-PREV-STATE.
00323
00324      IF EIBAID = DFHPF3
00325          MOVE PI-PREV-STATE      TO  PI-WS-STATE
00326          MOVE SPACES             TO  PI-WS-CLASS
00327                                      PI-WS-DEV
00328                                      PI-WS-TYPE
00329                                      PI-WS-PLAN
00330          PERFORM 6400-CREATE-TEMP-STORAGE THRU 6400-EXIT
00331          MOVE XCTL-6565          TO  PGM-NAME
00332          GO TO 9300-XCTL.
00333
00334      IF EIBAID = DFHPF4
00335          MOVE PI-PREV-STATE      TO  PI-WS-STATE
00336          MOVE SPACES             TO  PI-WS-CLASS
00337                                      PI-WS-DEV
00338                                      PI-WS-TYPE
00339                                      PI-WS-PLAN
00340          MOVE XCTL-EL106         TO  PGM-NAME
00341          GO TO 9300-XCTL.
00342
00343      IF EIBAID = DFHENTER
00344          GO TO 0330-EDIT-DATA.
00345
00346      MOVE ER-0029 TO EMI-ERROR.
00347  0320-INPUT-ERROR.
00348      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00349      MOVE AL-UNBON TO ENTERPFA.
00350
00351      IF ENTERPFL = 0
00352          MOVE -1 TO MAINTL
00353      ELSE
00354          MOVE -1 TO ENTERPFL.
00355
00356      GO TO 8200-SEND-DATAONLY.
00357
00358      EJECT
00359  0330-EDIT-DATA.
00360      IF STCDL = ZERO AND
00361         STABRL = ZERO
00362          MOVE ER-0144       TO EMI-ERROR
00363          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00364          MOVE -1            TO STCDL
00365          MOVE AL-UABON      TO STCDA
00366          GO TO 8200-SEND-DATAONLY.
00367
00368      IF STCDL = ZERO AND
00369         STABRL NOT = ZERO
00370          PERFORM 0500-GET-STATE-CD THRU 0600-EXIT.
00371
00372      IF MAINTI = 'S'
00373          GO TO 1000-SHOW-STATE.
00374
00375      IF SYSTEM-MODIFY-CAP
00376          NEXT SENTENCE
00377        ELSE
00378          IF MAINTI =  'C'
00379              MOVE 'UPDATE'       TO SM-READ
00380              PERFORM 9995-SECURITY-VIOLATION
00381              MOVE ER-0070        TO EMI-ERROR
00382              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00383              MOVE LOW-VALUES     TO EL106BO
00384              GO TO 8100-SEND-INITIAL-MAP.
00385
00386      IF MAINTI = 'C'
00387          GO TO 2000-CHANGE-STATE.
00388
00389      MOVE ER-0023 TO EMI-ERROR.
00390      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00391      MOVE -1       TO MAINTL.
00392      MOVE AL-UABON TO MAINTA.
00393      GO TO 8200-SEND-DATAONLY.
00394
00395      EJECT
00396  0500-GET-STATE-CD.
00397      MOVE PI-COMPANY-ID  TO CK-COMP-ID.
00398      MOVE LOW-VALUES     TO CK-STATE-CD.
00399      MOVE +0             TO CK-SEQ.
00400
00401      
      * EXEC CICS HANDLE CONDITION
00402 *        ENDFILE(8880-NOT-FOUND)
00403 *        NOTFND (8880-NOT-FOUND)
00404 *    END-EXEC.
      *    MOVE '"$''I                  ! # #00005243' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035323433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00405
00406      
      * EXEC CICS STARTBR
00407 *        DATASET  (ELCNTL-ID)
00408 *        RIDFLD   (ELCNTL-KEY)
00409 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005248' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00410
00411  0510-GET-NEXT-CD.
00412      
      * EXEC CICS READNEXT
00413 *        DATASET(ELCNTL-ID)
00414 *        SET    (ADDRESS OF CONTROL-FILE)
00415 *        RIDFLD (ELCNTL-KEY)
00416 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005254' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
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
           
00417
00418      IF CF-COMPANY-ID NOT = PI-COMPANY-ID
00419          
      * EXEC CICS ENDBR
00420 *            DATASET  (ELCNTL-ID)
00421 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005261' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00422          GO TO 8880-NOT-FOUND.
00423
00424      IF CF-RECORD-TYPE NOT = '3'
00425          GO TO 0510-GET-NEXT-CD.
00426
00427      IF CF-STATE-ABBREVIATION = STABRI
00428          NEXT SENTENCE
00429        ELSE
00430          GO TO 0510-GET-NEXT-CD.
00431
00432      MOVE CF-STATE-CODE      TO STCDI.
00433      MOVE AL-UANON           TO STCDA.
00434      MOVE +2                 TO STCDL.
00435
00436      
      * EXEC CICS ENDBR
00437 *        DATASET  (ELCNTL-ID)
00438 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005278' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00439
00440  0600-EXIT.
00441       EXIT.
00442      EJECT
00443  1000-SHOW-STATE.
00444      MOVE PI-COMPANY-ID TO CK-COMP-ID.
00445      MOVE STCDI         TO CK-STATE-CD
00446                            PI-WS-STATE.
00447
00448      
      * EXEC CICS READ
00449 *        DATASET(ELCNTL-ID)
00450 *        SET    (ADDRESS OF CONTROL-FILE)
00451 *        RIDFLD (ELCNTL-KEY)
00452 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005290' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00453
00454      GO TO 7000-BUILD-OUTPUT-MAP.
00455
00456      EJECT
00457  2000-CHANGE-STATE.
00458      IF STCDI NOT = PI-PREV-STATE
00459          MOVE ER-0145  TO EMI-ERROR
00460          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00461          MOVE -1       TO STCDL
00462          MOVE AL-UABON TO STCDA
00463          GO TO 8200-SEND-DATAONLY.
00464
00465      PERFORM 6000-EDIT-INPUT-DATA THRU 6099-EXIT.
00466
00467      IF NOT EMI-NO-ERRORS
00468          GO TO 8200-SEND-DATAONLY.
00469
00470      MOVE PI-COMPANY-ID TO CK-COMP-ID.
00471      MOVE STCDI         TO CK-STATE-CD
00472                            PI-WS-STATE.
00473
00474      
      * EXEC CICS READ
00475 *        UPDATE
00476 *        DATASET(ELCNTL-ID)
00477 *        SET    (ADDRESS OF CONTROL-FILE)
00478 *        RIDFLD (ELCNTL-KEY)
00479 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005316' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00480
00481      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY    OR
00482         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
00483          
      * EXEC CICS UNLOCK
00484 *            DATASET(ELCNTL-ID)
00485 *        END-EXEC
      *    MOVE '&*                    #   #00005325' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00486          MOVE ER-0068 TO EMI-ERROR
00487          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00488          GO TO 1000-SHOW-STATE.
00489
00490      MOVE 'B'                    TO JP-RECORD-TYPE.
00491      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00492      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
00493
00494      MOVE PI-PROCESSOR-ID TO CF-LAST-MAINT-BY.
00495      MOVE EIBTIME         TO CF-LAST-MAINT-HHMMSS.
00496      MOVE EIBDATE         TO DC-JULIAN-YYDDD.
00497      MOVE '5'             TO DC-OPTION-CODE.
00498      MOVE LINK-ELDATCV    TO PGM-NAME.
00499
00500      
      * EXEC CICS LINK
00501 *        PROGRAM (PGM-NAME)
00502 *        COMMAREA(DATE-CONVERSION-DATA)
00503 *        LENGTH  (DC-COMM-LENGTH)
00504 *    END-EXEC.
      *    MOVE '."C                   (   #00005342' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00505
00506      IF DATE-CONVERSION-ERROR
00507          MOVE LOW-VALUES          TO CF-LAST-MAINT-DT
00508      ELSE
00509          MOVE DC-BIN-DATE-1       TO CF-LAST-MAINT-DT.
00510
00511      IF STABRL NOT EQUAL ZEROS
00512          MOVE STABRI              TO CF-STATE-ABBREVIATION.
00513
00514      SET SC-INDX TO 1.
00515      MOVE 1 TO SUB.
00516  2001-LOOP.
00517      IF SC-CDL (SC-INDX) NOT = ZEROS
00518         IF SC-CD  (SC-INDX) NOT = SPACES  AND  ZEROS
00519             MOVE SC-CD (SC-INDX) TO CF-ST-BENEFIT-CD (SUB)
00520         ELSE
00521             MOVE ZEROS           TO CF-ST-BENEFIT-CD (SUB).
00522
00523      IF SC-KINDL (SC-INDX) NOT = ZEROS
00524          MOVE SC-KIND (SC-INDX)  TO CF-ST-BENEFIT-KIND (SUB).
00525
00526      IF SC-REFL (SC-INDX) NOT = ZEROS
00527          MOVE SC-REF (SC-INDX)   TO CF-ST-REFUND-CALC (SUB).
00528
00529      IF SC-REMTERML (SC-INDX) NOT = ZEROS
00530          MOVE SC-REMTERM (SC-INDX)  TO CF-ST-REM-TERM-CALC (SUB).
022415     if sc-extral (sc-indx) not = zeros
022415        move sc-extra (sc-indx)  to cf-st-extra-periods (sub)
022415     end-if
00531
00532      IF SC-INDX NOT = 50
00533          ADD 1 TO SUB
00534          SET SC-INDX UP BY 1
00535          GO TO 2001-LOOP.
00536
00537      MOVE 'C'                    TO JP-RECORD-TYPE.
00538      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00539      
      * EXEC CICS REWRITE
00540 *        DATASET(ELCNTL-ID)
00541 *        FROM   (CONTROL-FILE)
00542 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005384' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00543
00544      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
00545      MOVE ER-0000     TO EMI-ERROR.
00546      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00547      MOVE LOW-VALUES  TO EL106BO.
00548      MOVE -1          TO MAINTL.
00549      MOVE SPACES      TO PI-PREV-STATE.
00550      MOVE CK-STATE-CD TO STCDO.
00551      MOVE AL-UANON    TO STCDA.
00552      GO TO 1000-SHOW-STATE.
00553
00554      EJECT
00555  5000-FIND-NEXT-STATE.
00556      MOVE PI-COMPANY-ID  TO CK-COMP-ID.
00557
00558      IF STCDL = 0
00559          MOVE LOW-VALUES TO CK-STATE-CD
00560          MOVE +0         TO CK-SEQ
00561      ELSE
00562          MOVE STCDI      TO CK-STATE-CD
00563          MOVE +1         TO CK-SEQ.
00564
00565      MOVE SPACES TO PI-PREV-STATE.
00566
00567      
      * EXEC CICS HANDLE CONDITION
00568 *        NOTFND (8860-ENDFILE)
00569 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00005412' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00570
00571      
      * EXEC CICS READ
00572 *        DATASET(ELCNTL-ID)
00573 *        SET    (ADDRESS OF CONTROL-FILE)
00574 *        RIDFLD (ELCNTL-KEY)
00575 *        GTEQ
00576 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005416' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00577
00578      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR
00579         CF-RECORD-TYPE NOT = '3'
00580          GO TO 8860-ENDFILE.
00581
00582      IF STCDL = 0
00583          MOVE ER-0146 TO EMI-ERROR
00584          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00585
00586      GO TO 7000-BUILD-OUTPUT-MAP.
00587
00588      EJECT
00589  5500-FIND-PREV-STATE.
00590      MOVE PI-COMPANY-ID          TO  CK-COMP-ID.
00591      MOVE PI-PREV-STATE          TO  CK-STATE-CD.
00592
00593      IF STCDL GREATER +0
00594          MOVE STCDI              TO  CK-STATE-CD.
00595
00596      MOVE SPACES                 TO  PI-PREV-STATE.
00597
00598      
      * EXEC CICS HANDLE CONDITION
00599 *        NOTFND(8860-ENDFILE)
00600 *    END-EXEC.
      *    MOVE '"$I                   ! % #00005443' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035343433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00601
00602      
      * EXEC CICS STARTBR
00603 *        DATASET  (ELCNTL-ID)
00604 *        RIDFLD   (ELCNTL-KEY)
00605 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005447' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00606
00607  5600-READ-PREV-STATE-RECORD.
00608      
      * EXEC CICS READPREV
00609 *        DATASET  (ELCNTL-ID)
00610 *        SET      (ADDRESS OF CONTROL-FILE)
00611 *        RIDFLD   (ELCNTL-KEY)
00612 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005453' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
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
           
00613
00614      IF FIRST-TIME
00615          MOVE 'N'                TO  WS-FIRST-TIME-SW
00616          GO TO 5600-READ-PREV-STATE-RECORD.
00617
00618      IF CF-COMPANY-ID  NOT = PI-COMPANY-ID  OR
00619         CF-RECORD-TYPE NOT = '3'
00620          GO TO 8860-ENDFILE.
00621
00622      IF STCDL = 0
00623          MOVE ER-0146 TO EMI-ERROR
00624          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00625
00626      GO TO 7000-BUILD-OUTPUT-MAP.
00627
00628      EJECT
00629  6000-EDIT-INPUT-DATA.
00630
00631      PERFORM 6100-INITIALIZE-TABLE THRU 6199-EXIT
00632              VARYING TBL-NDX FROM +1 BY +1
00633                UNTIL TBL-NDX GREATER THAN +50.
00634
00635      SET SC-INDX TO 1.
00636
00637  6010-LOOP.
00638      IF SC-CDL (SC-INDX) = ZEROS  OR
00639         (SC-CD (SC-INDX) = SPACES  OR  ZEROS)
00640         GO TO 6020-NO-CD.
00641
111219     if sc-cd (sc-indx) = '90' OR '91' OR '92' OR '93' OR
111219          '94' OR '95' OR '96' OR '97' OR '98' OR '99'
00642 *    IF SC-CD (SC-INDX) NOT LESS THAN '90'
00643         MOVE ER-0150  TO EMI-ERROR
00644         MOVE -1       TO SC-CDL (SC-INDX)
00645         MOVE AL-UABON TO SC-CDA (SC-INDX)
00646         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00647        ELSE
00648         MOVE AL-UANON TO SC-CDA (SC-INDX).
00649
00650      IF SC-KINDL (SC-INDX) = ZEROS OR
00651         SC-KIND  (SC-INDX) = SPACES
00652           MOVE -1           TO SC-KINDL (SC-INDX)
00653           MOVE ER-0159      TO EMI-ERROR
00654           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00655        ELSE
00656           IF SC-KIND (SC-INDX) = PI-LIFE-OVERRIDE-L1 OR
00657                                  PI-AH-OVERRIDE-L1
00658              MOVE AL-UANON TO SC-KINDA (SC-INDX)
00659           ELSE
00660              MOVE ER-0151  TO EMI-ERROR
00661              MOVE -1       TO SC-KINDL (SC-INDX)
00662              MOVE AL-UABON TO SC-KINDA (SC-INDX)
00663              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00664
00665      IF CREDIT-SESSION OR CLAIM-SESSION
00666        IF (SC-REFL (SC-INDX) = ZEROS)    OR
00667           (SC-REF  (SC-INDX) = SPACES)
00668           NEXT SENTENCE
00669          ELSE
00670           IF SC-REF (SC-INDX) = ' ' OR '1' OR '2' OR '3' OR
00671                                 '4' OR '5' OR '6' OR '8' OR
00672                                 '9'
00673              MOVE AL-UANON        TO SC-REFA (SC-INDX)
00674             ELSE
00675              MOVE ER-0582         TO EMI-ERROR
00676              MOVE -1              TO SC-REFL (SC-INDX)
00677              MOVE AL-UABON        TO SC-REFA (SC-INDX)
00678              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00679
00680      IF SC-REMTERML (SC-INDX) NOT = ZEROS
00681          IF (SC-REMTERM (SC-INDX) = SPACE)
00682                             OR
00683             (SC-REMTERM (SC-INDX) GREATER '0' AND LESS '8')
00684              MOVE AL-UANON        TO SC-REMTERMA (SC-INDX)
00685          ELSE
00686              MOVE ER-2298         TO EMI-ERROR
00687              MOVE -1              TO SC-REMTERML (SC-INDX)
00688              MOVE AL-UABON        TO SC-REMTERMA (SC-INDX)
00689              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00690
00691      GO TO 6040-CHECK-FOR-DUPS.
00692
00693  6020-NO-CD.
00694      IF SC-KINDL (SC-INDX)   NOT = ZEROS
00695         IF SC-KIND (SC-INDX) NOT = SPACES
00696            MOVE -1       TO SC-KINDL (SC-INDX)
00697            MOVE AL-UABON TO SC-KINDA (SC-INDX)
00698            MOVE ER-0160  TO EMI-ERROR
00699            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00700
00701      IF CREDIT-SESSION OR CLAIM-SESSION
00702         IF SC-REFL (SC-INDX)   NOT = ZEROS
00703            IF SC-REF (SC-INDX) NOT = SPACE
00704               MOVE -1       TO SC-REFL (SC-INDX)
00705               MOVE AL-UABON TO SC-REFA (SC-INDX)
00706               MOVE ER-2033  TO EMI-ERROR
00707               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00708
00709      IF SC-REMTERML (SC-INDX)    NOT = ZEROS
00710          IF SC-REMTERM (SC-INDX) NOT = SPACE
00711              MOVE -1       TO SC-REMTERML (SC-INDX)
00712              MOVE AL-UABON TO SC-REMTERMA (SC-INDX)
00713              MOVE ER-2299  TO EMI-ERROR
00714              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00715
00716  6040-CHECK-FOR-DUPS.
00717
00718      SET TBL-NDX TO 1.
00719
00720  6050-DUPLICATE-LOOP.
00721
00722      INSPECT SC-CD (SC-INDX) REPLACING ALL LOW-VALUES
00723                                         BY SPACES.
00724
00725      IF SC-CD (SC-INDX) EQUAL SPACES
00726         GO TO 6060-CONTINUE.
00727
00728      IF TBL-CODE (TBL-NDX) = SC-CD   (SC-INDX) AND
00729         TBL-KIND (TBL-NDX) = SC-KIND (SC-INDX)
00730            MOVE -1       TO SC-CDL   (SC-INDX)
00731                             SC-KINDL (SC-INDX)
00732            MOVE AL-UABON TO SC-CDA   (SC-INDX)
00733                             SC-KINDA (SC-INDX)
00734            MOVE ER-7534  TO EMI-ERROR
00735            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00736            GO TO 6099-EXIT.
00737
00738      IF TBL-CODE (TBL-NDX) = SPACES
00739          MOVE SC-CD   (SC-INDX) TO TBL-CODE (TBL-NDX)
00740          MOVE SC-KIND (SC-INDX) TO TBL-KIND (TBL-NDX)
00741      ELSE
00742          SET TBL-NDX UP BY 1
00743          GO TO 6050-DUPLICATE-LOOP.
00744
00745  6060-CONTINUE.
00746
00747      IF SC-INDX NOT = 50
00748         SET SC-INDX UP BY 1
00749         GO TO 6010-LOOP.
00750
00751  6099-EXIT.
00752      EXIT.
00753
00754  6100-INITIALIZE-TABLE.
00755
00756      MOVE SPACES  TO  TBL-CODE (TBL-NDX)
00757                       TBL-KIND (TBL-NDX).
00758  6199-EXIT.
00759      EXIT.
00760
00761      EJECT
00762  6400-CREATE-TEMP-STORAGE.
00763      PERFORM 6600-DELETE-TEMP-STORAGE THRU 6600-EXIT.
00764
00765      
      * EXEC CICS WRITEQ TS
00766 *        QUEUE   (QID)
00767 *        FROM    (EL106BO)
00768 *        LENGTH  (WS-MAP-LENGTH)
00769 *    END-EXEC.
      *    MOVE '*"     L              ''   #00005612' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL106BO, 
                 WS-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00770
00771  6400-EXIT.
00772       EXIT.
00773
00774  6500-RECOVER-TEMP-STORAGE.
00775      
      * EXEC CICS READQ TS
00776 *        QUEUE   (QID)
00777 *        INTO    (EL106BO)
00778 *        LENGTH  (WS-MAP-LENGTH)
00779 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00005622' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL106BO, 
                 WS-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00780
00781      PERFORM 6600-DELETE-TEMP-STORAGE THRU 6600-EXIT.
00782
00783  6500-EXIT.
00784       EXIT.
00785
00786  6600-DELETE-TEMP-STORAGE.
00787      
      * EXEC CICS HANDLE CONDITION
00788 *        QIDERR  (6600-EXIT)
00789 *    END-EXEC.
      *    MOVE '"$N                   ! & #00005634' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035363334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00790
00791      
      * EXEC CICS DELETEQ TS
00792 *        QUEUE  (QID)
00793 *    END-EXEC.
      *    MOVE '*&                    #   #00005638' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00794
00795  6600-EXIT.
00796       EXIT.
00797
00798      EJECT
00799 ***************************************************************
00800 *                                                             *
00801 *     BUILD THE OUTPUT SCREEN TO BE DISPLAYED                 *
00802 *                                                             *
00803 ***************************************************************
00804  7000-BUILD-OUTPUT-MAP.
00805      MOVE LOW-VALUES            TO EL106BO.
00806      MOVE CF-STATE-CODE         TO STCDO
00807                                    PI-WS-STATE.
00808      MOVE CF-STATE-ABBREVIATION TO STABRO.
00809      MOVE CF-STATE-NAME         TO STNAMEO.
00810
00811      MOVE CF-LAST-MAINT-BY      TO LSTUSRO.
00812      MOVE ' '                   TO DC-OPTION-CODE.
00813      MOVE CF-LAST-MAINT-DT      TO DC-BIN-DATE-1.
00814      MOVE LINK-ELDATCV          TO PGM-NAME.
00815      
      * EXEC CICS LINK
00816 *        PROGRAM (PGM-NAME)
00817 *        COMMAREA(DATE-CONVERSION-DATA)
00818 *        LENGTH  (DC-COMM-LENGTH)
00819 *    END-EXEC.
      *    MOVE '."C                   (   #00005662' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00820
00821      IF DATE-CONVERSION-ERROR
00822          MOVE ZEROS            TO LSTDTEO
00823      ELSE
00824          MOVE DC-GREG-DATE-1-EDIT
00825                                TO LSTDTEO.
00826
00827      MOVE CF-LAST-MAINT-HHMMSS TO TIME-IN.
00828      MOVE TIME-OUT             TO LSTTIMEO.
00829      MOVE -1                   TO MAINTL.
00830      MOVE CF-LAST-MAINT-BY     TO PI-UPDATE-BY.
00831      MOVE CF-LAST-MAINT-HHMMSS TO PI-UPDATE-HHMMSS.
00832      MOVE CF-STATE-CODE        TO PI-PREV-STATE.
00833      MOVE AL-UANOF             TO MAINTA.
00834      MOVE AL-UABON             TO STCDA
00835                                   STABRA.
00836      MOVE AL-PABON             TO STNAMEA.
00837      SET SC-INDX TO 1.
00838      MOVE 1 TO SUB.
00839
00840  7100-LOOP.
00841      IF CF-ST-BENEFIT-CD (SUB) = ZEROS
00842         MOVE SPACES                   TO  SC-CD    (SC-INDX)
00843        ELSE
00844         MOVE CF-ST-BENEFIT-CD  (SUB)  TO  SC-CD    (SC-INDX).
00845
00846      MOVE AL-UANON                    TO  SC-CDA   (SC-INDX).
00847
00848      MOVE CF-ST-BENEFIT-KIND   (SUB)  TO  SC-KIND  (SC-INDX).
00849      MOVE AL-UANON                    TO  SC-KINDA (SC-INDX).
00850
00851      IF CREDIT-SESSION OR CLAIM-SESSION
00852         MOVE CF-ST-REFUND-CALC (SUB)  TO  SC-REF   (SC-INDX)
00853         MOVE AL-UANON                 TO  SC-REFA  (SC-INDX)
00854        ELSE
00855         MOVE AL-PANOF                 TO  SC-REFA   (SC-INDX).
00856
00857      MOVE CF-ST-REM-TERM-CALC  (SUB)  TO  SC-REMTERM  (SC-INDX).
00858      MOVE AL-UANON                    TO  SC-REMTERMA (SC-INDX).
           if (cf-st-extra-periods (sub) numeric)
              and (cf-st-extra-periods (sub) not = zeros)
              move cf-st-extra-periods (sub)
                                       to sc-extra (sc-indx)
              move al-uanon            to sc-extraa (sc-indx)
           end-if
00860      IF SUB NOT = 50
00861         ADD 1 TO SUB
00862         SET SC-INDX UP BY 1
00863         GO TO 7100-LOOP.
00864
00865      IF INITIAL-DISPLAY
00866          GO TO 8100-SEND-INITIAL-MAP
00867      ELSE
00868          GO TO 8200-SEND-DATAONLY.
00869
00870      EJECT
00871  8100-SEND-INITIAL-MAP.
00872      MOVE SAVE-DATE            TO RUNDTEO.
00873      MOVE EIBTIME              TO TIME-IN.
00874      MOVE TIME-OUT             TO RUNTIMEO.
00875      MOVE -1                   TO MAINTL.
00876      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
00877      MOVE EMI-MESSAGE-AREA (2) TO ERRMSG2O.
00878
00879      
      * EXEC CICS SEND
00880 *        MAP   (MAP-NAME)
00881 *        MAPSET(MAPSET-NAME)
00882 *        FROM  (EL106BO)
00883 *        ERASE
00884 *        CURSOR
00885 *    END-EXEC.
           MOVE LENGTH OF
            EL106BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005731' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL106BO, 
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
           
00886
00887      GO TO 9100-RETURN-TRAN.
00888      EJECT
00889  8200-SEND-DATAONLY.
00890      MOVE SAVE-DATE      TO RUNDTEO.
00891      MOVE EIBTIME        TO TIME-IN.
00892      MOVE TIME-OUT       TO RUNTIMEO.
00893
00894      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
00895      MOVE EMI-MESSAGE-AREA (2) TO ERRMSG2O.
00896
00897      
      * EXEC CICS SEND
00898 *        MAP   (MAP-NAME)
00899 *        MAPSET(MAPSET-NAME)
00900 *        FROM  (EL106BO)
00901 *        DATAONLY
00902 *        ERASEAUP
00903 *        CURSOR
00904 *    END-EXEC.
           MOVE LENGTH OF
            EL106BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00005749' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL106BO, 
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
           
00905
00906      GO TO 9100-RETURN-TRAN.
00907      EJECT
00908  8300-SEND-TEXT.
00909      
      * EXEC CICS SEND TEXT
00910 *        FROM  (LOGOFF-TEXT)
00911 *        LENGTH(LOGOFF-LENGTH)
00912 *        ERASE
00913 *        FREEKB
00914 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005761' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373631' TO DFHEIV0(25:11)
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
           
00915
00916      
      * EXEC CICS RETURN
00917 *    END-EXEC.
      *    MOVE '.(                    ''   #00005768' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00918
00919      EJECT
00920  8400-LOG-JOURNAL-RECORD.
00921      IF PI-JOURNAL-FILE-ID = 0
00922          GO TO 8400-EXIT.
00923
00924      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
00925      MOVE ELCNTL-ID              TO JP-FILE-ID.
00926      MOVE THIS-PGM               TO JP-PROGRAM-ID.
pemuni*    EXEC CICS JOURNAL
pemuni*         JFILEID(PI-JOURNAL-FILE-ID)
pemuni*         JTYPEID('EL')
pemuni*         FROM   (JOURNAL-RECORD)
pemuni*         LENGTH (527)
pemuni*    END-EXEC.
00933
00934  8400-EXIT.
00935      EXIT.
00936
00937  8800-UNAUTHORIZED-ACCESS.
00938      MOVE UNACCESS-MSG TO LOGOFF-MSG.
00939      GO TO 8300-SEND-TEXT.
00940
00941  8810-PF23.
00942      MOVE EIBAID   TO PI-ENTRY-CD-1.
00943      MOVE XCTL-005 TO PGM-NAME.
00944      GO TO 9300-XCTL.
00945
00946  8850-DUPREC.
00947      MOVE ER-0147  TO EMI-ERROR.
00948      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00949      MOVE -1       TO STCDL.
00950      MOVE AL-UABON TO STCDA.
00951      GO TO 8200-SEND-DATAONLY.
00952
00953  8860-ENDFILE.
00954      MOVE LOW-VALUES TO EL106BO.
00955      MOVE ER-0148    TO EMI-ERROR.
00956      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00957      MOVE -1         TO MAINTL.
00958      GO TO 8100-SEND-INITIAL-MAP.
00959
00960  8870-NOTOPEN.
00961      MOVE ER-0042 TO EMI-ERROR.
00962      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00963      MOVE -1      TO MAINTL.
00964      GO TO 8200-SEND-DATAONLY.
00965
00966  8880-NOT-FOUND.
00967      MOVE ER-0149  TO EMI-ERROR.
00968      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00969      MOVE -1       TO STCDL.
00970      MOVE AL-UABON TO STCDA.
00971      GO TO 8200-SEND-DATAONLY.
00972
00973  9000-RETURN-CICS.
00974      
      * EXEC CICS RETURN
00975 *    END-EXEC.
      *    MOVE '.(                    ''   #00005826' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00976
00977  9100-RETURN-TRAN.
00978      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
00979      MOVE '106B'               TO PI-CURRENT-SCREEN-NO.
00980
00981      
      * EXEC CICS RETURN
00982 *        TRANSID (TRANS-ID)
00983 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
00984 *        LENGTH  (WS-COMM-LENGTH)
00985 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005833' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00986
00987  9200-RETURN-MAIN-MENU.
00988      IF  CREDIT-SESSION
00989          MOVE XCTL-EL626           TO PGM-NAME
00990      ELSE
00991      IF  CLAIM-SESSION
00992          MOVE XCTL-EL126           TO PGM-NAME
00993      ELSE
00994      IF  MORTGAGE-SESSION
00995          MOVE XCTL-EM626           TO PGM-NAME
00996      ELSE
00997      IF  GENERAL-LEDGER-SESSION
00998          MOVE XCTL-GL800           TO PGM-NAME.
00999
01000  9300-XCTL.
01001      
      * EXEC CICS XCTL
01002 *        PROGRAM (PGM-NAME)
01003 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
01004 *        LENGTH  (WS-COMM-LENGTH)
01005 *    END-EXEC.
      *    MOVE '.$C                   %   #00005853' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01006
01007  9400-CLEAR.
01008      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
01009      GO TO 9300-XCTL.
01010
01011  9500-PF12.
01012      MOVE XCTL-010 TO PGM-NAME.
01013      GO TO 9300-XCTL.
01014
01015  9600-PGMID-ERROR.
01016      
      * EXEC CICS HANDLE CONDITION
01017 *        PGMIDERR(8300-SEND-TEXT)
01018 *    END-EXEC.
      *    MOVE '"$L                   ! '' #00005868' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035383638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01019
01020      MOVE PGM-NAME      TO PI-CALLING-PROGRAM.
01021      MOVE ' '           TO PI-ENTRY-CD-1.
01022      MOVE XCTL-005      TO PGM-NAME.
01023      MOVE PGM-NAME      TO LOGOFF-PGM.
01024      MOVE PGMIDERR-MSG  TO LOGOFF-FILL.
01025      GO TO 9300-XCTL.
01026
01027  9700-LINK-DATE-CONVERT.
01028      
      * EXEC CICS LINK
01029 *        PROGRAM    ('ELDATCV')
01030 *        COMMAREA   (DATE-CONVERSION-DATA)
01031 *        LENGTH     (DC-COMM-LENGTH)
01032 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005880' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01033
01034  9700-EXIT.
01035      EXIT.
01036
01037  9900-ERROR-FORMAT.
01038      IF NOT EMI-ERRORS-COMPLETE
01039          MOVE LINK-001 TO PGM-NAME
01040          
      * EXEC CICS LINK
01041 *            PROGRAM (PGM-NAME)
01042 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
01043 *            LENGTH  (EMI-COMM-LENGTH)
01044 *        END-EXEC.
      *    MOVE '."C                   (   #00005892' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01045
01046  9900-EXIT.
01047      EXIT.
01048
01049  9990-ABEND.
01050      MOVE LINK-004               TO PGM-NAME.
01051      MOVE DFHEIBLK               TO EMI-LINE1.
01052      
      * EXEC CICS LINK
01053 *        PROGRAM   (PGM-NAME)
01054 *        COMMAREA  (EMI-LINE1)
01055 *        LENGTH    (72)
01056 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005904' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01057
01058      GO TO 8200-SEND-DATAONLY.
01059
01060  9995-SECURITY-VIOLATION.
01061 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005930' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
01062
01063  9995-EXIT.
01064       EXIT.
01065

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1061' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8850-DUPREC,
                     8870-NOTOPEN,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8880-NOT-FOUND,
                     8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8860-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8860-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6600-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1061' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
