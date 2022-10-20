00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL6521.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 11/18/94 10:26:25.
00007 *                            VMOD=2.004
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
00018 *                                                                *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.    TRANSACTION - EXD8 - COMPENSATION GA STATUS DATA.
00025 *
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
092707* 092707    2004020600003  PEMA  ADD DELIVER TO MEL SWITCH
030211* 030211    2010012100001  PEMA  ADD RDS EMAILS TO LOGIC
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
101101******************************************************************
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6521 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.004 *********'.
00034
00035  01  WS-DATE-AREA.
00036      05  WS-SAVE-DATE                PIC X(8) VALUE SPACES.
00037      05  WS-SAVE-BIN-DT              PIC XX   VALUE SPACES.
00038      05  WS-EFF-YMD                  PIC X(6).
00039      05  WS-SAVE-BIN-EFFDT           PIC XX   VALUE SPACES.
00040      05  WS-TRM-YMD                  PIC X(6).
00041      05  WS-SAVE-BIN-TRMDT           PIC XX   VALUE SPACES.
00042      05  WS-ACH-SW                   PIC X    VALUE 'N'.
00043          88  ACH-HAS-CHANGED             VALUE 'Y'.
00044          88  NO-ACH-CHANGE               VALUE 'N'.
00045      05  SUPPRESS-MAP-SW             PIC X    VALUE SPACE.
00046          88  DO-NOT-MOVE-TO-MAP          VALUE 'N'.
00047          88  MOVE-TO-MAP                 VALUE 'Y'.
00048      05  MAP-CHANGED-SW              PIC X    VALUE 'N'.
00049          88  MAP-NOT-CHANGED             VALUE 'N'.
00050          88  MAP-CHANGED                 VALUE 'Y'.
           05  BILL-INST-SW                PIC X.
               88  BILL-INST-CHANGED           VALUE 'Y'.
00051
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
00052  01  STANDARD-AREAS.
00041      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.
00053      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1024.
00054      12  WS-SUB                      PIC S9(4) COMP VALUE +0.
00055      12  MAP-NAME                    PIC X(8) VALUE 'EL6521A'.
00056      12  MAPSET-NAME                 PIC X(8) VALUE 'EL6521S'.
00057      12  SCREEN-NUMBER               PIC X(4) VALUE '652B'.
00058      12  TRANS-ID                    PIC X(4) VALUE 'EXD8'.
00059      12  EL611-TRANS-ID              PIC X(4) VALUE 'EXL3'.
           12  EL6525-TRANS-ID             PIC X(4) VALUE 'EXDF'.
           12  EL6526-TRANS-ID             PIC X(4) VALUE 'EXDG'.
00060      12  THIS-PGM                    PIC X(8) VALUE 'EL6521'.
00061      12  PGM-NAME                    PIC X(8).
00062      12  TIME-IN                     PIC S9(7).
00063      12  TIME-OUT-R  REDEFINES TIME-IN.
00064          16  FILLER                  PIC X.
00065          16  TIME-OUT                PIC 99V99.
00066          16  FILLER                  PIC XX.
00067      12  XCTL-005                    PIC X(8) VALUE 'EL005'.
00068      12  XCTL-010                    PIC X(8) VALUE 'EL010'.
00069      12  XCTL-626                    PIC X(8) VALUE 'EL626'.
00070      12  XCTL-611                    PIC X(8) VALUE 'EL611'.
           12  XCTL-6525                   PIC X(8) VALUE 'EL6525'.
030211     12  XCTL-6526                   PIC X(8) VALUE 'EL6526'.
00071      12  XCTL-652                    PIC X(8) VALUE 'EL652'.
00072      12  LINK-001                    PIC X(8) VALUE 'EL001'.
00073      12  LINK-004                    PIC X(8) VALUE 'EL004'.
00074      12  QID.
00075          16  QID-TERM                PIC X(4) VALUE SPACES.
00076          16  FILLER                  PIC X(4) VALUE '521A'.
00077      12  MAP-LENGTH                  PIC S9(4) VALUE +600  COMP.
           12  ERCOBI-FILE-ID              PIC X(8) VALUE 'ERCOBI'.
00078      12  ERCOMP-FILE-ID              PIC X(8) VALUE 'ERCOMP'.
00079      12  ELACHP-FILE-ID              PIC X(8) VALUE 'ELACHP'.
00080      12  ELBANK-FILE-ID              PIC X(8) VALUE 'ELBANK'.
00081      12  WS-AGENT-BANK-DESC.
00082          16 FILLER                   PIC X(7) VALUE '  AGENT'.
00083          16 FILLER                   PIC X(1) VALUE X'7D'.
00084          16 FILLER                   PIC X(15)
00085                                       VALUE 'S BANK ACCOUNT:'.
00086
00087      12  DEEDIT-FIELD                PIC X(15).
00088      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
00089
           12  ERCOBI-KEY.
               16  ERCOBI-COMPANY-CD      PIC X.
               16  ERCOBI-STMT-OWNER      PIC X(4).
               16  ERCOBI-RGID            PIC X(12).
           12  WS-BILL-INST-SW            PIC X  VALUE SPACES.
               88  BILLING-INSTRUCTIONS-FOUND  VALUE 'Y'.
00091      12  ERROR-MESSAGES.
00092          16  ER-0000                 PIC X(4) VALUE '0000'.
00093          16  ER-0004                 PIC X(4) VALUE '0004'.
00094          16  ER-0008                 PIC X(4) VALUE '0008'.
00095          16  ER-0029                 PIC X(4) VALUE '0029'.
00096          16  ER-0068                 PIC X(4) VALUE '0068'.
00097          16  ER-0070                 PIC X(4) VALUE '0070'.
00098          16  ER-0348                 PIC X(4) VALUE '0348'.
00099          16  ER-0454                 PIC X(4) VALUE '0454'.
062907         16  ER-0876                 PIC X(4) VALUE '0876'.
00100          16  ER-1228                 PIC X(4) VALUE '1228'.
00101          16  ER-1626                 PIC X(4) VALUE '1626'.
00102          16  ER-1629                 PIC X(4) VALUE '1629'.
00103          16  ER-2039                 PIC X(4) VALUE '2039'.
00104          16  ER-2233                 PIC X(4) VALUE '2233'.
               16  ER-2797                 PIC X(4) VALUE '2797'.
00105          16  ER-7430                 PIC X(4) VALUE '7430'.
00106          16  ER-7431                 PIC X(4) VALUE '7431'.
00107          16  ER-7432                 PIC X(4) VALUE '7432'.
00108          16  ER-7434                 PIC X(4) VALUE '7434'.
00109          16  ER-7435                 PIC X(4) VALUE '7435'.
00110          16  ER-7436                 PIC X(4) VALUE '7436'.
00111          16  ER-7438                 PIC X(4) VALUE '7438'.
00112          16  ER-7440                 PIC X(4) VALUE '7440'.
00113          16  ER-7447                 PIC X(4) VALUE '7447'.
00114          16  ER-7449                 PIC X(4) VALUE '7449'.
00115          16  ER-7462                 PIC X(4) VALUE '7462'.
00116          16  ER-7465                 PIC X(4) VALUE '7465'.
00117          16  ER-7468                 PIC X(4) VALUE '7468'.
00118          16  ER-7469                 PIC X(4) VALUE '7469'.
062907         16  ER-8799                 PIC X(4) VALUE '8799'.
00119          16  ER-9388                 PIC X(4) VALUE '9388'.
00120          16  ER-9399                 PIC X(4) VALUE '9399'.
062907         16  ER-9999                 PIC X(4) VALUE '9999'.
00121
00122      12  ELCNTL-KEY.
00123          16  CNTL-COMP-ID            PIC X(3) VALUE SPACES.
00124          16  CNTL-REC-TYPE           PIC X    VALUE SPACES.
00125          16  CNTL-ACCESS             PIC X(4) VALUE SPACES.
00126          16  CNTL-SEQ-NO             PIC S9(4) VALUE +0  COMP.
00127
00128      12  WS-BANK-INFORMATION.
00129          16 WS-BANK-DATA.
00130              20 WS-TRANSIT1              PIC X(4).
00131              20 WS-TRANSIT2              PIC X(4).
00132              20 WS-BKACCTI               PIC X(17).
00133          16 WS-ACTCDI                    PIC X(1).
00134
00135      EJECT
00136 *    COPY ELCLOGOF.
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
00137      EJECT
00138 *    COPY ELCDATE.
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
00139      EJECT
00140 *    COPY ELCATTR.
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
00141      EJECT
00142 *    COPY ELCEMIB.
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
00143      EJECT
00144 *    COPY ELCSCTM.
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
00145      EJECT
00146 *    COPY ELCSCRTY.
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
00147      EJECT
00148 *    COPY ELCINTF.
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
00149      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
00150          16  PI-MAINT                PIC  X.
00151          16  PI-CHECK-TYPE           PIC  X.
00152          16  PI-CHECK-CARRY-BAL      PIC  X.
00153          16  PI-FIRST-TIME-SW        PIC  X.
00154              88  FIRST-TIME                  VALUE 'Y'.
00155          16  PI-ERCOMP-EOF-SW        PIC  X.
00156              88  ERCOMP-EOF                  VALUE 'Y'.
00157          16  PI-SAVE-PHONE           PIC  X(10).
00158          16  PI-SAVE-PHONE-RED REDEFINES PI-SAVE-PHONE  PIC 9(10).
00159          16  PI-ERC-END-BAL          PIC S9(9)V99       COMP-3.
00160          16  PI-ERCOMP-KEY.
00161              20  PI-ERC-GROUP-CD     PIC  X.
00162              20  PI-ERC-CARRIER      PIC  X.
00163              20  PI-ERC-GROUP        PIC  X(6).
00164              20  PI-ERC-RESP         PIC  X(10).
00165              20  PI-ERC-ACCT         PIC  X(10).
00166              20  PI-ERC-TYPE         PIC  X.
00167          16  PI-SAVE-ERCOMP-KEY      PIC  X(29).
00168          16  PI-BANK-TRANSIT-NUMBER.
00169              20  PI-BANK-COMPANY-CD  PIC X.
00170              20  PI-FEDERAL-NUMBER   PIC X(4).
00171              20  PI-BANK-NUMBER      PIC X(4).
00172          16  PI-BANK-ACCOUNT-NO      PIC X(17).
00173          16  PI-BANK-ACTION-CODE     PIC X.
               16  PI-ERCOBI-KEY.
                   20  PI-ERCOBI-COMPANY-CD PIC X.
                   20  PI-ERCOBI-STMT-OWNER PIC X(4).
                   20  PI-ERCOBI-RGID      PIC X(12).
               16  PI-SAVE-ERCOBI-KEY      PIC X(17).
00174          16  FILLER                  PIC  X(500).
00175
00176      EJECT
00177 *    COPY ELCAID.
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
00178  01  FILLER    REDEFINES DFHAID.
00179      12  FILLER                      PIC X(8).
00180      12  PF-VALUES                   PIC X       OCCURS 2.
00181
00182      EJECT
00183 *    COPY EL6521S.
       01  EL6521AI.
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
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  MAINTYPL PIC S9(0004) COMP.
           05  MAINTYPF PIC  X(0001).
           05  FILLER REDEFINES MAINTYPF.
               10  MAINTYPA PIC  X(0001).
           05  MAINTYPI PIC  X(0001).
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
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  FINRESPL PIC S9(0004) COMP.
           05  FINRESPF PIC  X(0001).
           05  FILLER REDEFINES FINRESPF.
               10  FINRESPA PIC  X(0001).
           05  FINRESPI PIC  X(0010).
      *    -------------------------------
           05  COACCTL PIC S9(0004) COMP.
           05  COACCTF PIC  X(0001).
           05  FILLER REDEFINES COACCTF.
               10  COACCTA PIC  X(0001).
           05  COACCTI PIC  X(0010).
      *    -------------------------------
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0008).
      *    -------------------------------
           05  TRMDTL PIC S9(0004) COMP.
           05  TRMDTF PIC  X(0001).
           05  FILLER REDEFINES TRMDTF.
               10  TRMDTA PIC  X(0001).
           05  TRMDTI PIC  X(0008).
      *    -------------------------------
           05  STATL PIC S9(0004) COMP.
           05  STATF PIC  X(0001).
           05  FILLER REDEFINES STATF.
               10  STATA PIC  X(0001).
           05  STATI PIC  X(0001).
      *    -------------------------------
           05  WTHLDL PIC S9(0004) COMP.
           05  WTHLDF PIC  X(0001).
           05  FILLER REDEFINES WTHLDF.
               10  WTHLDA PIC  X(0001).
           05  WTHLDI PIC  S99V9999.
      *    -------------------------------
           05  GADDL PIC S9(0004) COMP.
           05  GADDF PIC  X(0001).
           05  FILLER REDEFINES GADDF.
               10  GADDA PIC  X(0001).
           05  GADDI PIC  X(0001).
      *    -------------------------------
           05  APCHKL PIC S9(0004) COMP.
           05  APCHKF PIC  X(0001).
           05  FILLER REDEFINES APCHKF.
               10  APCHKA PIC  X(0001).
           05  APCHKI PIC  X(0001).
      *    -------------------------------
           05  MELSWL PIC S9(0004) COMP.
           05  MELSWF PIC  X(0001).
           05  FILLER REDEFINES MELSWF.
               10  MELSWA PIC  X(0001).
           05  MELSWI PIC  X(0001).
      *    -------------------------------
           05  MDACTL PIC S9(0004) COMP.
           05  MDACTF PIC  X(0001).
           05  FILLER REDEFINES MDACTF.
               10  MDACTA PIC  X(0001).
           05  MDACTI PIC  X(0010).
      *    -------------------------------
           05  MDDIVL PIC S9(0004) COMP.
           05  MDDIVF PIC  X(0001).
           05  FILLER REDEFINES MDDIVF.
               10  MDDIVA PIC  X(0001).
           05  MDDIVI PIC  X(0002).
      *    -------------------------------
           05  MDCNTRL PIC S9(0004) COMP.
           05  MDCNTRF PIC  X(0001).
           05  FILLER REDEFINES MDCNTRF.
               10  MDCNTRA PIC  X(0001).
           05  MDCNTRI PIC  X(0004).
      *    -------------------------------
           05  MDLOBL PIC S9(0004) COMP.
           05  MDLOBF PIC  X(0001).
           05  FILLER REDEFINES MDLOBF.
               10  MDLOBA PIC  X(0001).
           05  MDLOBI PIC  X(0006).
      *    -------------------------------
           05  MDSTL PIC S9(0004) COMP.
           05  MDSTF PIC  X(0001).
           05  FILLER REDEFINES MDSTF.
               10  MDSTA PIC  X(0001).
           05  MDSTI PIC  X(0002).
      *    -------------------------------
           05  MDAMTL PIC S9(0004) COMP.
           05  MDAMTF PIC  X(0001).
           05  FILLER REDEFINES MDAMTF.
               10  MDAMTA PIC  X(0001).
           05  MDAMTI PIC  S999999V99.
      *    -------------------------------
           05  DELTOL PIC S9(0004) COMP.
           05  DELTOF PIC  X(0001).
           05  FILLER REDEFINES DELTOF.
               10  DELTOA PIC  X(0001).
           05  DELTOI PIC  X(0004).
      *    -------------------------------
           05  RGIDL PIC S9(0004) COMP.
           05  RGIDF PIC  X(0001).
           05  FILLER REDEFINES RGIDF.
               10  RGIDA PIC  X(0001).
           05  RGIDI PIC  X(0012).
      *    -------------------------------
           05  COMM1L PIC S9(0004) COMP.
           05  COMM1F PIC  X(0001).
           05  FILLER REDEFINES COMM1F.
               10  COMM1A PIC  X(0001).
           05  COMM1I PIC  X(0040).
      *    -------------------------------
           05  COMM2L PIC S9(0004) COMP.
           05  COMM2F PIC  X(0001).
           05  FILLER REDEFINES COMM2F.
               10  COMM2A PIC  X(0001).
           05  COMM2I PIC  X(0040).
      *    -------------------------------
           05  COMM3L PIC S9(0004) COMP.
           05  COMM3F PIC  X(0001).
           05  FILLER REDEFINES COMM3F.
               10  COMM3A PIC  X(0001).
           05  COMM3I PIC  X(0040).
      *    -------------------------------
           05  COMM4L PIC S9(0004) COMP.
           05  COMM4F PIC  X(0001).
           05  FILLER REDEFINES COMM4F.
               10  COMM4A PIC  X(0001).
           05  COMM4I PIC  X(0040).
      *    -------------------------------
           05  BKLITL PIC S9(0004) COMP.
           05  BKLITF PIC  X(0001).
           05  FILLER REDEFINES BKLITF.
               10  BKLITA PIC  X(0001).
           05  BKLITI PIC  X(0023).
      *    -------------------------------
           05  TRNSIT1L PIC S9(0004) COMP.
           05  TRNSIT1F PIC  X(0001).
           05  FILLER REDEFINES TRNSIT1F.
               10  TRNSIT1A PIC  X(0001).
           05  TRNSIT1I PIC  X(0004).
      *    -------------------------------
           05  TRDASHL PIC S9(0004) COMP.
           05  TRDASHF PIC  X(0001).
           05  FILLER REDEFINES TRDASHF.
               10  TRDASHA PIC  X(0001).
           05  TRDASHI PIC  X(0001).
      *    -------------------------------
           05  TRNSIT2L PIC S9(0004) COMP.
           05  TRNSIT2F PIC  X(0001).
           05  FILLER REDEFINES TRNSIT2F.
               10  TRNSIT2A PIC  X(0001).
           05  TRNSIT2I PIC  X(0004).
      *    -------------------------------
           05  BKDESCL PIC S9(0004) COMP.
           05  BKDESCF PIC  X(0001).
           05  FILLER REDEFINES BKDESCF.
               10  BKDESCA PIC  X(0001).
           05  BKDESCI PIC  X(0023).
      *    -------------------------------
           05  BKACCTL PIC S9(0004) COMP.
           05  BKACCTF PIC  X(0001).
           05  FILLER REDEFINES BKACCTF.
               10  BKACCTA PIC  X(0001).
           05  BKACCTI PIC  X(0017).
      *    -------------------------------
           05  ACTLITL PIC S9(0004) COMP.
           05  ACTLITF PIC  X(0001).
           05  FILLER REDEFINES ACTLITF.
               10  ACTLITA PIC  X(0001).
           05  ACTLITI PIC  X(0023).
      *    -------------------------------
           05  ACTCDL PIC S9(0004) COMP.
           05  ACTCDF PIC  X(0001).
           05  FILLER REDEFINES ACTCDF.
               10  ACTCDA PIC  X(0001).
           05  ACTCDI PIC  X(0001).
      *    -------------------------------
           05  CRSLITL PIC S9(0004) COMP.
           05  CRSLITF PIC  X(0001).
           05  FILLER REDEFINES CRSLITF.
               10  CRSLITA PIC  X(0001).
           05  CRSLITI PIC  X(0017).
      *    -------------------------------
           05  CRSTATL PIC S9(0004) COMP.
           05  CRSTATF PIC  X(0001).
           05  FILLER REDEFINES CRSTATF.
               10  CRSTATA PIC  X(0001).
           05  CRSTATI PIC  X(0010).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0071).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0071).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
      *    -------------------------------
           05  ACHPF1L PIC S9(0004) COMP.
           05  ACHPF1F PIC  X(0001).
           05  FILLER REDEFINES ACHPF1F.
               10  ACHPF1A PIC  X(0001).
           05  ACHPF1I PIC  X(0015).
       01  EL6521AO REDEFINES EL6521AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FINRESPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRMDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  WTHLDO PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GADDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCHKO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MELSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDACTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDDIVO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDCNTRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDLOBO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MDAMTO PIC  ZZZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DELTOO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RGIDO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM1O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM2O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM3O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM4O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKLITO PIC  X(0023).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNSIT1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRDASHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRNSIT2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKDESCO PIC  X(0023).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKACCTO PIC  X(0017).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTLITO PIC  X(0023).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTCDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRSLITO PIC  X(0017).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRSTATO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0071).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0071).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACHPF1O PIC  X(0015).
      *    -------------------------------
00184
00185      EJECT
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
00187  01  DFHCOMMAREA                     PIC X(1024).
00188
00189 *    COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
071712* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
071712     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
071712         16  CO-OV120                      PIC S9(7)V99   COMP-3.
071712         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
      *    COPY ERCCOBI.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOBI                             *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER BILLING INSTRUCTIONS  *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 620   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOBI                   RKP=2,LEN=17    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
081808*                   C H A N G E   L O G
081808*
081808* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081808*-----------------------------------------------------------------
081808*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081808* EFFECTIVE    NUMBER
081808*-----------------------------------------------------------------
081808* 081808    2008061100001  PEMA  NEW FILE FOR BILLING INSTRUCTIONS
081808******************************************************************
00022  01  COMP-BILLING-INSTRUCTIONS.
00023      12  BL-RECORD-ID                          PIC XX.
00024          88  VALID-BL-ID                          VALUE 'BL'.
00026      12  BL-CONTROL-PRIMARY.
00027          16  BL-COMPANY-CD                     PIC X.
00028          16  BL-STMT-OWNER                     PIC X(4).
00031          16  BL-REPORT-GROUP-ID                PIC X(12).
00047      12  BL-MAINT-INFORMATION.
00048          16  BL-LAST-MAINT-DT                  PIC XX.
00049          16  BL-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  BL-LAST-MAINT-USER                PIC X(4).
00051          16  FILLER                            PIC X(10).
           12  BL-ACCOUNT-NAME                       PIC X(35).
           12  BL-CONTACT-NAME                       PIC X(35).
           12  BL-ADDR1                              PIC X(30).
           12  BL-ADDR2                              PIC X(30).
           12  BL-CITY                               PIC X(30).
           12  BL-STATE                              PIC XX.
00074      12  BL-ZIP.
00075          16  BL-ZIP-PRIME.
00076              20  BL-ZIP-PRI-1ST                PIC X.
00077                  88  BL-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  BL-ZIP-PLUS4                      PIC X(4).
00080      12  BL-CANADIAN-POSTAL-CODE  REDEFINES  BL-ZIP.
00081          16  BL-CAN-POSTAL-1                   PIC XXX.
00082          16  BL-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
      *    12  FILLER                                PIC X(30).
           12  BL-CHECK-HANDLING                     PIC X.
               88  BL-CHECKS-NET                VALUE '1' ' '.
               88  BL-CHECKS-SEPARATE           VALUE '2'.
           12  BL-SPECIAL-INSTRUCTIONS.
               16  BL-SI-LINE-1                      PIC X(70).
               16  BL-SI-LINE-2                      PIC X(70).
               16  BL-SI-LINE-3                      PIC X(70).
               16  BL-SI-LINE-4                      PIC X(70).
               16  BL-SI-LINE-5                      PIC X(70).
           12  FILLER                                PIC X(59).
00264
00265 ******************************************************************
00190 *    COPY ELCACHP.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCACHP                              *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACH PRE-NOTIFICATION                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 120  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELACHP                         RKP=2,LEN=29   *
00013 *       ALTERNATE (NONE)                                         *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCACHP                          *
00017 ******************************************************************
00018
00019  01  ACH-PRENOTIFICATION.
00020      12  AP-RECORD-ID                      PIC XX.
00021          88  VALID-AP-ID                      VALUE 'AP'.
00022
00023 ******************************************************************
00024 *   BASE CLUSTER = ELACHP         (BASE KEY)      RKP=2,LEN=29   *
00025 ******************************************************************
00026
00027      12  AP-CONTROL-PRIMARY.
00028          16  AP-CONTROL-CD                 PIC X.
00029          16  AP-CARRIER                    PIC X.
00030          16  AP-GROUPING                   PIC X(6).
00031          16  AP-FIN-RESP                   PIC X(10).
00032          16  AP-ACCT-AGENT                 PIC X(10).
00033          16  AP-CO-TYPE                    PIC X.
00034
00035      12  AP-CONTROL-ALT   REDEFINES  AP-CONTROL-PRIMARY.
00036          16  AP-COMPANY-CD-ALT             PIC X.
00037          16  AP-BENEFICIARY                PIC X(10).
00038          16  FILLER                        PIC X(18).
00039
00040 ******************************************************************
00041 *                 FILE SYNCHRONIZATION DATA                      *
00042 ******************************************************************
00043
00044      12  AP-FILE-SYNCH-DATA.
00045          16  AP-LAST-CHANGE-DT             PIC XX.
00046          16  AP-LAST-CHANGE-TIME           PIC S9(7)  COMP-3.
00047          16  AP-LAST-CHANGE-PROCESSOR      PIC X(4).
00048
00049 ******************************************************************
00050 *                     PRENOTE INFORMATION
00051 ******************************************************************
00052
00053      12  AP-BANK-INFORMATION.
00054          16  AP-TRANSIT-NUMBER             PIC X(8).
00055          16  AP-BANK-ACCOUNT-NO            PIC X(17).
00056          16  AP-BANK-NAME                  PIC X(23).
00057
00058      12  FILLER                            PIC X(31).
00059
00060 ******************************************************************
00191 *    COPY ELCBANK.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCBANK                              *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BANK MASTER FILE                          *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 210  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELBANK                         RKP=2,LEN=9    *
00013 *       ALTERNATE (NONE)                                         *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCBANK                          *
00017 ******************************************************************
00018
00019  01  BANK-MASTER.
00020      12  BM-RECORD-ID                      PIC XX.
00021          88  VALID-BM-ID                      VALUE 'BM'.
00022
00023 ******************************************************************
00024 *   BASE CLUSTER = MPBANK         (BASE KEY)      RKP=2,LEN=9    *
00025 ******************************************************************
00026
00027      12  BM-CONTROL-PRIMARY.
00028          16  BM-COMPANY-CD                 PIC X.
00029          16  BM-TRANSIT-NUMBER.
00030              20  BM-FEDERAL-NUMBER         PIC X(4).
00031              20  BM-BANK-NUMBER            PIC X(4).
00032      12  FILLER                            PIC X(20).
00033
00034 ******************************************************************
00035 *                 FILE SYNCHRONIZATION DATA                      *
00036 ******************************************************************
00037
00038      12  BM-FILE-SYNCH-DATA.
00039          16  BM-LAST-CHANGE-DT             PIC XX.
00040          16  BM-LAST-CHANGE-TIME           PIC S9(7)  COMP-3.
00041          16  BM-LAST-CHANGE-PROCESSOR      PIC X(4).
00042
00043 ******************************************************************
00044 *                       BANK INFORMATION                         *
00045 ******************************************************************
00046
00047      12  BM-BANK-INFORMATION.
00048          16  BM-NAME                       PIC X(30).
00049          16  BM-ADDRESS1                   PIC X(30).
00050          16  BM-ADDRESS2                   PIC X(30).
00051          16  BM-CITY                       PIC X(25).
00052          16  BM-STATE                      PIC X(25).
00053          16  BM-ZIP.
00054              20  BM-ZIP1                   PIC X(5).
00055              20  BM-ZIP2                   PIC X(4).
00056      12  FILLER                            PIC X(20).
00057
00058 ******************************************************************
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                COMPENSATION-MASTER
                                COMP-BILLING-INSTRUCTIONS
                                ACH-PRENOTIFICATION BANK-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6521' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00195
00196      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00197      MOVE '5'                    TO DC-OPTION-CODE.
00198      PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.
00199      MOVE DC-GREG-DATE-1-EDIT    TO WS-SAVE-DATE.
00200      MOVE DC-BIN-DATE-1          TO WS-SAVE-BIN-DT.
00201
00202      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00203      MOVE EIBTRMID               TO QID-TERM.
00204      MOVE +2                     TO EMI-NUMBER-OF-LINES.
00205      MOVE SPACE                  TO SUPPRESS-MAP-SW.
00206
00207      IF EIBCALEN = 0
00208         GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF
           IF PI-RETURN-TO-PROGRAM = THIS-PGM
              MOVE PI-CALLING-PROGRAM  TO RETURNED-FROM
           END-IF
           IF PI-CALLING-PROGRAM NOT = THIS-PGM
              IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
                 MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-6
                 MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-5
                 MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-1    TO PI-SAVED-PROGRAM-2
                 MOVE PI-RETURN-TO-PROGRAM  TO PI-SAVED-PROGRAM-1
                 MOVE PI-CALLING-PROGRAM    TO PI-RETURN-TO-PROGRAM
                 MOVE THIS-PGM              TO PI-CALLING-PROGRAM
              ELSE
                 MOVE PI-RETURN-TO-PROGRAM  TO PI-CALLING-PROGRAM
                 MOVE PI-SAVED-PROGRAM-1    TO PI-RETURN-TO-PROGRAM
                 MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-1
                 MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-2
                 MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-6    TO PI-SAVED-PROGRAM-5
                 MOVE SPACES                TO PI-SAVED-PROGRAM-6
              END-IF
           END-IF
           MOVE LOW-VALUES             TO EL6521AI
00222      IF EIBTRNID = EL611-TRANS-ID OR EL6525-TRANS-ID OR
                 EL6526-TRANS-ID
00223         CONTINUE
00224      ELSE
00225         GO TO 0100-NOT-EL611-RETURN
           END-IF
00226
00227      PERFORM 0600-RECOVER-TS  THRU  0600-EXIT.
00228      MOVE XCTL-652           TO PI-RETURN-TO-PROGRAM.
00229
00230      MOVE PI-FEDERAL-NUMBER      TO TRNSIT1O.
00231      MOVE PI-BANK-NUMBER         TO TRNSIT2O.
00232      MOVE PI-BANK-ACCOUNT-NO     TO BKACCTO.
00233
00234      IF PI-BANK-ACTION-CODE EQUAL   'P'
00235         MOVE 'PENDING'           TO CRSTATO.
00236
00237      IF PI-BANK-ACTION-CODE EQUAL   'A'
00238         MOVE 'ACTIVE '           TO CRSTATO.
00239
00240      IF MAINTYPL GREATER THAN ZERO
00241          MOVE AL-UABON           TO MAINTYPA.
00242
00243      IF STATL    GREATER THAN ZERO
00244          MOVE 'Y'                TO MAP-CHANGED-SW
00245          MOVE AL-UANON           TO STATA.
00246
062907     IF WTHLDL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UNNON            TO WTHLDA
062907     END-IF
062907     IF GADDL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO GADDA
062907     END-IF
062907     IF MELSWL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO MELSWA
062907     END-IF
062907     IF APCHKL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO APCHKA
062907     END-IF
062907     IF MDACTL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO MDACTA
062907     END-IF
062907     IF MDDIVL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO MDDIVA
062907     END-IF
062907     IF MDCNTRL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UANON            TO MDCNTRA
062907     END-IF
062907     IF MDAMTL > ZERO
062907        MOVE 'Y'                 TO MAP-CHANGED-SW
062907        MOVE AL-UNNON            TO MDAMTA
062907     END-IF
00247      IF EFFDTL   GREATER THAN ZERO
00248          MOVE 'Y'                TO MAP-CHANGED-SW
00249          MOVE AL-UANON           TO EFFDTA.
00250
00251      IF TRMDTL   GREATER THAN ZERO
00252          MOVE 'Y'                TO MAP-CHANGED-SW
00253          MOVE AL-UANON           TO TRMDTA.
00254
00255      IF COMM1L   GREATER THAN ZERO
00256          MOVE 'Y'                TO MAP-CHANGED-SW
00257          MOVE AL-UANON           TO COMM1A.
00258
00259      IF COMM2L   GREATER THAN ZERO
00260          MOVE 'Y'                TO MAP-CHANGED-SW
00261          MOVE AL-UANON           TO COMM2A.
00262
00263      IF COMM3L   GREATER THAN ZERO
00264          MOVE 'Y'                TO MAP-CHANGED-SW
00265          MOVE AL-UANON           TO COMM3A.
00266
00267      IF COMM4L   GREATER THAN ZERO
00268          MOVE 'Y'                TO MAP-CHANGED-SW
00269          MOVE AL-UANON           TO COMM4A.
00270
00271      IF TRNSIT1L GREATER THAN ZERO
00272          MOVE 'Y'                TO MAP-CHANGED-SW
00273          MOVE AL-UABON           TO TRNSIT1A.
00274
00275      IF TRNSIT2L GREATER THAN ZERO
00276          MOVE 'Y'                TO MAP-CHANGED-SW
00277          MOVE AL-UABON           TO TRNSIT2A.
00278
00279      IF BKACCTL  GREATER THAN ZERO
00280          MOVE 'Y'                TO MAP-CHANGED-SW
00281          MOVE AL-UABON           TO BKACCTA.
00282
00283      IF ACTCDL   GREATER THAN ZERO
00284          MOVE 'Y'                TO MAP-CHANGED-SW
00285          MOVE AL-UABON           TO ACTCDA.
00286
           IF DELTOL > 0
              MOVE 'Y'                 TO MAP-CHANGED-SW
              MOVE AL-UANON            TO DELTOA
           END-IF
           IF RGIDL > 0
              MOVE 'Y'                 TO MAP-CHANGED-SW
              MOVE AL-UANON            TO RGIDA
           END-IF
00287      IF MAP-NOT-CHANGED
00288          GO TO 0100-NOT-EL611-RETURN.
00289
00290      GO TO 8100-SEND-INITIAL-MAP.
00291
00292  0100-NOT-EL611-RETURN.
00293
00294      IF EIBTRNID NOT = TRANS-ID
00295          MOVE PI-MAINT           TO MAINTYPO
00296          MOVE AL-UANON           TO MAINTYPA
00297          MOVE -1                 TO MAINTYPL
00298          IF PI-MAINT = 'S' OR 'C'
00299              GO TO 4000-SHOW
00300          ELSE
00301              GO TO 8100-SEND-INITIAL-MAP.
00302
00303      
      * EXEC CICS HANDLE CONDITION
00304 *        PGMIDERR  (9600-PGMID-ERROR)
00305 *        ERROR     (9990-ABEND)
00306 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00002123' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032313233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00307
00308      IF EIBAID = DFHCLEAR
00309          GO TO 9400-CLEAR.
00310
00311      EJECT
00312  0200-RECEIVE.
00313      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00314          MOVE ER-0008            TO EMI-ERROR
00315          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00316          MOVE -1                 TO PFENTERL
00317          GO TO 8200-SEND-DATAONLY.
00318
00319      
      * EXEC CICS RECEIVE
00320 *        MAP      (MAP-NAME)
00321 *        MAPSET   (MAPSET-NAME)
00322 *        INTO     (EL6521AI)
00323 *    END-EXEC.
           MOVE LENGTH OF
            EL6521AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002139' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6521AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00324
00325      IF PFENTERL = 0
00326          GO TO 0300-CHECK-PFKEYS.
00327
00328      IF EIBAID NOT = DFHENTER
00329          MOVE ER-0004            TO EMI-ERROR
00330          GO TO 0320-INPUT-ERROR.
00331
00332      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
00333          MOVE PF-VALUES (PFENTERI) TO EIBAID
00334      ELSE
00335          MOVE ER-0029              TO EMI-ERROR
00336          GO TO 0320-INPUT-ERROR.
00337
00338      EJECT
00339
00340  0300-CHECK-PFKEYS.
00341      IF PI-AR-PROCESSING
00342         IF EIBAID = DFHPF1
00343              PERFORM 0500-WRITE-TS  THRU  0500-EXIT
00344              MOVE XCTL-611       TO PGM-NAME
00345              GO TO 9300-XCTL.
00342      IF EIBAID = DFHPF2
              MOVE PI-COMPANY-CD       TO PI-ERCOBI-COMPANY-CD
              MOVE DELTOI              TO PI-ERCOBI-STMT-OWNER
              MOVE RGIDI               TO PI-ERCOBI-RGID
00343         PERFORM 0500-WRITE-TS    THRU 0500-EXIT
00344         MOVE XCTL-6525           TO PGM-NAME
00345         GO TO 9300-XCTL
           END-IF
030211     IF EIBAID = DFHPF3
030211        PERFORM 0500-WRITE-TS    THRU 0500-EXIT
030211        MOVE XCTL-6526           TO PGM-NAME
030211        GO TO 9300-XCTL
030211     END-IF
00347      IF EIBAID = DFHPF23
00348          GO TO 8810-PF23.
00349      IF EIBAID = DFHPF24
00350          GO TO 9200-RETURN-MAIN-MENU.
00351      IF EIBAID = DFHPF12
00352          GO TO 9500-PF12.
00353      IF EIBAID = DFHENTER
00354          GO TO 0330-CHECK-MAINTYP.
00355
00356      MOVE ER-0029                TO EMI-ERROR.
00357  0320-INPUT-ERROR.
00358      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00359      MOVE AL-UNBON               TO PFENTERA.
00360      MOVE -1                     TO PFENTERL.
00361      GO TO 8200-SEND-DATAONLY.
00362
00363  EJECT
00364  0330-CHECK-MAINTYP.
00365
00366      IF MAINTYPL GREATER ZERO
00367          IF MAINTYPI = 'S' OR 'C'
00368              MOVE AL-UANON       TO MAINTYPA
00369              MOVE MAINTYPI       TO PI-MAINT
00370          ELSE
00371              MOVE -1             TO MAINTYPL
00372              MOVE AL-UABON       TO MAINTYPA
00373              MOVE ER-2039        TO EMI-ERROR
00374              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00375              GO TO 8200-SEND-DATAONLY
00376      ELSE
00377          MOVE -1                 TO MAINTYPL
00378          MOVE AL-UABON           TO MAINTYPA
00379          MOVE ER-2039            TO EMI-ERROR
00380          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00381          GO TO 8200-SEND-DATAONLY.
00382
00383      IF PI-MAINT = 'S'
00384          GO TO 4000-SHOW.
00385
00386      GO TO 4200-MAINT.
00387
00388      EJECT
00389
00390  0500-WRITE-TS.
00391       
      * EXEC CICS WRITEQ TS
00392 *         QUEUE  (QID)
00393 *         FROM   (EL6521AO)
00394 *         LENGTH (MAP-LENGTH)
00395 *    END-EXEC.
      *    MOVE '*"     L              ''   #00002223' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL6521AO, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00396
00397  0500-EXIT.
00398       EXIT.
00399
00400  0600-RECOVER-TS.
00401       
      * EXEC CICS READQ TS
00402 *         QUEUE  (QID)
00403 *         INTO (EL6521AO)
00404 *         LENGTH (MAP-LENGTH)
00405 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00002233' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 EL6521AO, 
                 MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00406
00407      PERFORM 0700-DELETE-TS  THRU  0700-EXIT.
00408
00409  0600-EXIT.
00410       EXIT.
00411
00412  0700-DELETE-TS.
00413      
      * EXEC CICS HANDLE CONDITION
00414 *        QIDERR (0700-EXIT)
00415 *    END-EXEC.
      *    MOVE '"$N                   ! # #00002245' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032323435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00416
00417      
      * EXEC CICS DELETEQ TS
00418 *        QUEUE (QID)
00419 *    END-EXEC.
      *    MOVE '*&                    #   #00002249' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00420
00421  0700-EXIT.
00422       EXIT.
00423       EJECT
00424
00425  4000-SHOW.
00426
00427      PERFORM 7100-READ-ERCOMP THRU 7100-EXIT.
00428
00429      IF EIBTRNID     = EL611-TRANS-ID OR EL6525-TRANS-ID OR
                 EL6526-TRANS-ID
00430          NEXT SENTENCE
00431      ELSE
00432          MOVE LOW-VALUES         TO EL6521AO.
00433
00434      GO TO 5000-BUILD-INITIAL-SCREEN.
00435
00436      EJECT
00437  4200-MAINT.
00438      IF NOT MODIFY-CAP
00439          MOVE 'UPDATE'           TO SM-READ
00440          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00441          MOVE ER-0070            TO EMI-ERROR
00442          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00443          GO TO 8100-SEND-INITIAL-MAP.
00444
00445      MOVE 'N'                     TO SUPPRESS-MAP-SW.
00446      PERFORM 7100-READ-ERCOMP THRU 7100-EXIT.
00447
00448      IF EMI-NO-ERRORS
00449          NEXT SENTENCE
00450      ELSE
00451        IF EIBTRNID     = EL611-TRANS-ID OR EL6525-TRANS-ID OR
                    EL6526-TRANS-ID
00452            GO TO 8100-SEND-INITIAL-MAP
00453        ELSE
00454            GO TO 8200-SEND-DATAONLY.
00455
00456      PERFORM 7000-EDIT THRU 7099-EXIT.
00457
00458      IF EMI-NO-ERRORS
00459          NEXT SENTENCE
00460      ELSE
00461        IF EIBTRNID     = EL611-TRANS-ID OR EL6525-TRANS-ID OR
                   EL6526-TRANS-ID
00462            GO TO 8100-SEND-INITIAL-MAP
00463        ELSE
00464            GO TO 8200-SEND-DATAONLY.
00465
00466      MOVE ER-0000               TO EMI-ERROR
00467      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00468
00469      PERFORM 7300-READ-ERCOMP-UPDATE THRU 7300-EXIT.
00470
00471      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT.
00472
00473      MOVE PI-PROCESSOR-ID        TO CO-LAST-MAINT-USER.
00474      MOVE EIBTIME                TO CO-LAST-MAINT-HHMMSS.
00475      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00476      MOVE '5'                    TO DC-OPTION-CODE.
00477      PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.
00478
00479      MOVE DC-BIN-DATE-1          TO CO-LAST-MAINT-DT.
00480
00481      PERFORM 5500-PROCESS-ELACHP THRU 5599-PROCESS-EXIT
           PERFORM 6100-CHECK-BILL-INSTR
                                       THRU 6100-EXIT
00482
00483      
      * EXEC CICS REWRITE
00484 *        DATASET  (ERCOMP-FILE-ID)
00485 *        FROM     (COMPENSATION-MASTER)
00486 *    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002320' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00487
00488      PERFORM 7100-READ-ERCOMP THRU 7100-EXIT.
00489      MOVE LOW-VALUES             TO EL6521AO.
00490      MOVE 'C'                    TO PI-MAINT.
00491
00492      EJECT
00493
00494  5000-BUILD-INITIAL-SCREEN.
00495      MOVE CO-CARRIER             TO CARRIERO.
00496      MOVE CO-GROUPING            TO GROUPO.
00497      MOVE CO-TYPE                TO TYPEO.
00498      MOVE CO-RESP-NO             TO FINRESPO.
00499      MOVE CO-CTL-2               TO COACCTO.
00500      MOVE CO-GA-STATUS-CODE      TO STATO.
00502      IF CO-GA-STATUS-CODE = 'A' OR 'I' OR 'P'
00503          MOVE CO-GA-STATUS-CODE  TO STATO
00504          MOVE AL-UANON           TO STATA
00505          MOVE +1                 TO STATL
00506      ELSE
00507          MOVE AL-UANOF           TO STATA
           END-IF
           IF CO-GA-WITHOLD-PCT NOT NUMERIC
              MOVE ZEROS               TO CO-GA-WITHOLD-PCT
           END-IF
062907     MOVE CO-GA-WITHOLD-PCT      TO WTHLDO
           IF CO-GA-WITHOLD-PCT NOT = ZEROS
              MOVE +3                  TO WTHLDL
              MOVE AL-UANON            TO WTHLDA
           END-IF
062907     IF CO-GA-DIRECT-DEP = 'Y'
              MOVE 'Y'                 TO GADDO
              MOVE AL-UANON            TO GADDA
              MOVE +1                  TO GADDL
           ELSE
              MOVE 'N'                 TO GADDO
              MOVE AL-UANON            TO GADDA
              MOVE +1                  TO GADDL
           END-IF
062907     IF CO-DELIVER-CK-TO-MEL = 'Y'
              MOVE 'Y'                 TO MELSWO
              MOVE AL-UANON            TO MELSWA
              MOVE +1                  TO MELSWL
           ELSE
              MOVE 'N'                 TO MELSWO
              MOVE AL-UANON            TO MELSWA
              MOVE +1                  TO MELSWL
           END-IF
062907     IF CO-CREATE-AP-CHECK = 'Y'
              MOVE 'Y'                 TO APCHKO
              MOVE AL-UANON            TO APCHKA
              MOVE +1                  TO APCHKL
           ELSE
              MOVE 'N'                 TO APCHKO
              MOVE AL-UANON            TO APCHKA
              MOVE +1                  TO APCHKL
           END-IF
           IF CO-MD-GL-ACCT = SPACES OR LOW-VALUES
              MOVE AL-UANOF            TO MDACTA
           ELSE
              MOVE CO-MD-GL-ACCT       TO MDACTO
              MOVE +2                  TO MDACTL
              MOVE AL-UANON            TO MDACTA
           END-IF
           IF CO-MD-DIV = SPACES OR LOW-VALUES
              MOVE AL-UANOF            TO MDDIVA
           ELSE
              MOVE CO-MD-DIV           TO MDDIVO
              MOVE AL-UANON            TO MDDIVA
           END-IF
           IF CO-MD-CENTER = SPACES OR LOW-VALUES
              MOVE AL-UANOF            TO MDCNTRA
           ELSE
              MOVE CO-MD-CENTER        TO MDCNTRO
              MOVE +2                  TO MDCNTRL
              MOVE AL-UANON            TO MDCNTRA
           END-IF
           IF CO-MD-AMT NOT NUMERIC
              MOVE ZEROS               TO CO-MD-AMT
           END-IF
           MOVE CO-MD-AMT              TO MDAMTO
           IF CO-MD-AMT NOT = ZEROS
              MOVE +3                  TO MDAMTL
              MOVE AL-UANON            TO MDAMTA
           END-IF
           MOVE ZEROS                  TO MDLOBO
                                          MDSTO
00509      IF CO-GA-EFFECTIVE-DT = LOW-VALUES OR SPACES
00510          MOVE AL-UANOF           TO EFFDTA
00511      ELSE
00512          MOVE CO-GA-EFFECTIVE-DT TO DC-BIN-DATE-1
00513          MOVE ' '                TO DC-OPTION-CODE
00514          PERFORM 9700-DATE-CONVERT THRU 9700-EXIT
00515          MOVE DC-GREG-DATE-1-EDIT
00516                                  TO EFFDTO
00517          MOVE AL-UANON           TO EFFDTA
00518          MOVE +8                 TO EFFDTL.
00519
           IF CO-GA-TERMINATION-DT = HIGH-VALUES
              MOVE '99/99/99'          TO TRMDTO
              MOVE AL-UANON            TO TRMDTA
           ELSE
00520         IF CO-GA-TERMINATION-DT = LOW-VALUES OR SPACES
00521             MOVE AL-UANOF        TO TRMDTA
00522         ELSE
00523            MOVE CO-GA-TERMINATION-DT
00524                                  TO DC-BIN-DATE-1
00525            MOVE ' '              TO DC-OPTION-CODE
00526            PERFORM 9700-DATE-CONVERT
                                       THRU 9700-EXIT
00527            MOVE DC-GREG-DATE-1-EDIT
00528                                  TO TRMDTO
00529            MOVE AL-UANON         TO TRMDTA
00530            MOVE +8               TO TRMDTL
              END-IF
           END-IF
00531
00532      IF CO-GA-COMMENT-1 = LOW-VALUES OR SPACES
00533          MOVE AL-UANOF            TO COMM1A
00534      ELSE
00535          MOVE CO-GA-COMMENT-1    TO COMM1O
00536          MOVE AL-UANON           TO COMM1A
00537          MOVE +40                TO COMM1L.
00538
00539      IF CO-GA-COMMENT-2 = LOW-VALUES OR SPACES
00540          MOVE AL-UANOF            TO COMM2A
00541      ELSE
00542          MOVE CO-GA-COMMENT-2    TO COMM2O
00543          MOVE AL-UANON           TO COMM2A
00544          MOVE +40                TO COMM2L.
00545
00546      IF CO-GA-COMMENT-3 = LOW-VALUES OR SPACES
00547          MOVE AL-UANOF           TO COMM3A
00548      ELSE
00549          MOVE CO-GA-COMMENT-3    TO COMM3O
00550          MOVE AL-UANON           TO COMM3A
00551          MOVE +40                TO COMM3L.
00552
00553      IF CO-GA-COMMENT-4 = LOW-VALUES OR SPACES
00554          MOVE AL-UANOF           TO COMM4A
00555      ELSE
00556          MOVE CO-GA-COMMENT-4    TO COMM4O
00557          MOVE AL-UANON           TO COMM4A
00558          MOVE +40                TO COMM4L.
00559
020816     IF PI-COMPANY-ID = 'CID' OR 'DCC' OR 'AHL' or 'VPP'
062121           or 'FNL'
              CONTINUE
           ELSE
00560         IF (CO-BANK-TRANSIT-NO (1:4) = LOW-VALUES OR SPACES) AND
00561            (CO-BANK-TRANSIT-NO (5:4) = LOW-VALUES OR SPACES)
00562             MOVE AL-UANOF        TO TRNSIT1A
00563             MOVE AL-UANOF        TO TRNSIT2A
00564         ELSE
00565            MOVE CO-BANK-TRANSIT-NO (1:4)
00566                                  TO PI-BANK-TRANSIT-NUMBER (2:4)
00567                                     TRNSIT1O
00568            MOVE CO-BANK-TRANSIT-NO (5:4)
00569                                  TO PI-BANK-TRANSIT-NUMBER (6:4)
00570                                     TRNSIT2O
              END-IF
00572         IF CO-BANK-ACCOUNT-NUMBER = LOW-VALUES OR SPACES
00573            MOVE AL-UANOF         TO BKACCTA
00574         ELSE
00575            MOVE CO-BANK-ACCOUNT-NUMBER
00576                                  TO PI-BANK-ACCOUNT-NO
00577                                     BKACCTO
              END-IF
           END-IF
00579      MOVE    '       '           TO CRSTATO.
00580
00581      MOVE CO-ACH-STATUS          TO PI-BANK-ACTION-CODE.
00582
00583      IF CO-ACH-STATUS IS EQUAL      'P'
00584         MOVE 'PENDING'           TO CRSTATO.
00585
00586      IF CO-ACH-STATUS IS EQUAL      'A'
00587         MOVE 'ACTIVE '           TO CRSTATO.
           IF CO-STMT-OWNER NOT = SPACES AND LOW-VALUES
              MOVE CO-STMT-OWNER       TO DELTOO
              MOVE AL-UANON            TO DELTOA
              MOVE +4                  TO DELTOL
           ELSE
              MOVE AL-UANOF            TO DELTOA
           END-IF
           IF CO-REPORT-GROUP-ID NOT = SPACES AND LOW-VALUES
              MOVE CO-REPORT-GROUP-ID  TO RGIDO
              MOVE AL-UANON            TO RGIDA
              MOVE +12                 TO RGIDL
           ELSE
              MOVE AL-UANOF            TO RGIDA
           END-IF
00589      MOVE PI-MAINT               TO MAINTYPO.
00590      MOVE AL-UANOF               TO MAINTYPA.
00591      MOVE -1                     TO MAINTYPL.
00592
00593      GO TO 8100-SEND-INITIAL-MAP.
00594
00595  5099-EXIT.
00596      EXIT.
00597      EJECT
00598  5500-PROCESS-ELACHP.
00599
00600      IF ACTCDI = 'D'
00601         PERFORM 7400-DELETE-PRE-NOTE THRU 7499-EXIT
00602         MOVE SPACES              TO CO-BANK-INFORMATION
00603                                     CO-ACH-STATUS
00604         GO TO 5599-PROCESS-EXIT.
00605
00606      IF ACTCDI = 'C'
00607          PERFORM 7400-DELETE-PRE-NOTE THRU 7499-EXIT
00608          PERFORM 7600-WRITE-ELACHP THRU 7620-WRITE-EXIT
00609          MOVE WS-BANK-DATA       TO CO-BANK-INFORMATION
00610          MOVE WS-BKACCTI         TO CO-BANK-ACCOUNT-NUMBER
00611          MOVE 'P'                TO CO-ACH-STATUS
00612          GO TO 5599-PROCESS-EXIT.
00613
00614      IF ACTCDI =  'N'
00615          PERFORM 7600-WRITE-ELACHP THRU 7620-WRITE-EXIT
00616          MOVE WS-BANK-DATA       TO CO-BANK-INFORMATION
00617          MOVE WS-BKACCTI         TO CO-BANK-ACCOUNT-NUMBER
00618          MOVE 'P'                TO CO-ACH-STATUS  WS-ACTCDI
00619          GO TO 5599-PROCESS-EXIT.
00620
00621      IF ACTCDI = 'A'
00622          MOVE 'A'                TO CO-ACH-STATUS  WS-ACTCDI
00623          GO TO 5599-PROCESS-EXIT.
00624
00625  5599-PROCESS-EXIT.
00626
00627      EXIT.
00628  6000-CHECK-FOR-UPDATE.
00629      IF STATL GREATER ZERO
00630         MOVE STATI               TO CO-GA-STATUS-CODE.
00631
00632      IF EFFDTL GREATER ZERO
00633         MOVE WS-SAVE-BIN-EFFDT   TO CO-GA-EFFECTIVE-DT.
00634
00635      IF TRMDTL GREATER ZERO
00636         MOVE WS-SAVE-BIN-TRMDT   TO CO-GA-TERMINATION-DT.
00637
062907     IF GADDL > ZERO
062907        MOVE GADDI               TO CO-GA-DIRECT-DEP
062907     END-IF
062907     IF MELSWL > ZERO
062907        MOVE MELSWI              TO CO-DELIVER-CK-TO-MEL
062907     END-IF
062907     IF APCHKL > ZERO
062907        MOVE APCHKI              TO CO-CREATE-AP-CHECK
062907     END-IF
062907     IF MDACTL > ZERO
062907        MOVE MDACTI              TO CO-MD-GL-ACCT
062907     END-IF
062907     IF MDDIVL > ZERO
062907        MOVE MDDIVI              TO CO-MD-DIV
062907     END-IF
062907     IF MDCNTRL > ZERO
062907        MOVE MDCNTRI             TO CO-MD-CENTER
062907     END-IF
062907     IF WTHLDL > ZERO
062907        MOVE WTHLDI              TO CO-GA-WITHOLD-PCT
062907     END-IF
062907     IF MDAMTL > ZERO
062907        MOVE MDAMTI              TO CO-MD-AMT
062907     END-IF
           IF DELTOL > ZERO
              MOVE DELTOI              TO CO-STMT-OWNER
           END-IF
           IF RGIDL > ZERO
              MOVE RGIDI               TO CO-REPORT-GROUP-ID
           END-IF
00638      IF COMM1L GREATER ZERO
00639         MOVE COMM1I              TO CO-GA-COMMENT-1.
00640
00641      IF COMM2L GREATER ZERO
00642         MOVE COMM2I              TO CO-GA-COMMENT-2.
00643
00644      IF COMM3L GREATER ZERO
00645         MOVE COMM3I              TO CO-GA-COMMENT-3.
00646
00647      IF COMM4L GREATER ZERO
00648         MOVE COMM4I              TO CO-GA-COMMENT-4.
00649
00650  6049-EXIT.
00651      EXIT.
       6100-CHECK-BILL-INSTR.
           MOVE PI-COMPANY-CD          TO ERCOBI-COMPANY-CD
           MOVE DELTOI                 TO ERCOBI-STMT-OWNER
           MOVE RGIDI                  TO ERCOBI-RGID
           
      * EXEC CICS READ
      *       DATASET (ERCOBI-FILE-ID)
      *       SET     (ADDRESS OF COMP-BILLING-INSTRUCTIONS)
      *       RIDFLD  (ERCOBI-KEY)
      *       RESP    (WS-RESPONSE)
      *       UPDATE
      *    END-EXEC
      *    MOVE '&"S        EU         (  N#00002612' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032363132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOBI-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOBI-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMP-BILLING-INSTRUCTIONS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              MOVE AL-UABON            TO DELTOA
                                          RGIDA
              MOVE ER-2797             TO EMI-ERROR
              MOVE -1                  TO DELTOL
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
           .
       6100-EXIT.
           EXIT.
00653
00654  7000-EDIT.
00655      IF STATL GREATER ZERO
00656          IF STATI = 'A' OR 'I' OR 'P'
00657              NEXT SENTENCE
00658           ELSE
00659              MOVE -1             TO STATL
00660              MOVE AL-UABON       TO STATA
00661              MOVE ER-7438        TO EMI-ERROR
00662              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00663
00664      MOVE LOW-VALUES             TO WS-SAVE-BIN-EFFDT.
00665      MOVE LOW-VALUES             TO WS-SAVE-BIN-TRMDT.
00666
00667      IF EFFDTL NOT = ZERO
00668          MOVE EFFDTI             TO DEEDIT-FIELD
00669          PERFORM 8600-DEEDIT
00670          MOVE DEEDIT-FIELD-V0    TO DC-GREG-DATE-1-MDY
00671          MOVE '4'                TO DC-OPTION-CODE
00672          PERFORM 9700-DATE-CONVERT THRU 9700-EXIT
00673          IF DATE-CONVERSION-ERROR
00674              MOVE ER-0348        TO EMI-ERROR
00675              MOVE -1             TO EFFDTL
00676              MOVE AL-UABON       TO EFFDTA
00677              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00678          ELSE
00679              MOVE DC-GREG-DATE-1-EDIT  TO EFFDTI
00680              MOVE AL-UANON             TO EFFDTA
00681              MOVE DC-BIN-DATE-1        TO WS-SAVE-BIN-EFFDT.
00682
00683      IF TRMDTL NOT = ZERO
00684         MOVE TRMDTI              TO DEEDIT-FIELD
00685         PERFORM 8600-DEEDIT
00686         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
              IF DC-GREG-DATE-1-MDY = 999999
                 MOVE HIGH-VALUES      TO WS-SAVE-BIN-TRMDT
              ELSE
00687            MOVE '4'              TO DC-OPTION-CODE
00688            PERFORM 9700-DATE-CONVERT
                                       THRU 9700-EXIT
00689            IF DATE-CONVERSION-ERROR
00690               MOVE ER-0454       TO EMI-ERROR
00691               MOVE -1            TO TRMDTL
00692               MOVE AL-UABON      TO TRMDTA
00693               PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
00694            ELSE
00695               MOVE DC-GREG-DATE-1-EDIT
                                       TO TRMDTI
00696               MOVE AL-UANON      TO TRMDTA
00697               MOVE DC-BIN-DATE-1 TO WS-SAVE-BIN-TRMDT
                 END-IF
              END-IF
           END-IF
062907     IF GADDL > ZERO
062907        IF GADDI = 'Y' OR 'N' OR ' '
062907           MOVE AL-UANON         TO GADDA
062907        ELSE
062907           MOVE AL-UABON         TO GADDA
062907           MOVE ER-0876          TO EMI-ERROR
062907           MOVE -1               TO GADDL
062907           PERFORM 9900-ERROR-FORMAT
062907                                 THRU 9900-EXIT
062907        END-IF
062907     END-IF
092707     IF MELSWL > ZERO
092707        IF MELSWI = 'Y' OR 'N' OR ' '
092707           MOVE AL-UANON         TO MELSWA
092707        ELSE
092707           MOVE AL-UABON         TO MELSWA
092707           MOVE ER-0876          TO EMI-ERROR
092707           MOVE -1               TO MELSWL
092707           PERFORM 9900-ERROR-FORMAT
092707                                 THRU 9900-EXIT
092707        END-IF
092707     END-IF
092707     IF APCHKL > ZERO
092707        IF APCHKI = 'Y' OR 'N' OR ' '
092707           MOVE AL-UANON         TO APCHKA
092707        ELSE
092707           MOVE AL-UABON         TO APCHKA
092707           MOVE ER-0876          TO EMI-ERROR
092707           MOVE -1               TO APCHKL
092707           PERFORM 9900-ERROR-FORMAT
092707                                 THRU 9900-EXIT
092707        END-IF
092707     END-IF
062907     IF WTHLDL  > ZERO
062907        
      * EXEC CICS BIF
062907*          DEEDIT
062907*          FIELD   (WTHLDI)
062907*          LENGTH  (6)
062907*       END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002718' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WTHLDI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062907        IF WTHLDI NUMERIC
062907           MOVE AL-UNNON         TO WTHLDA
062907        ELSE
062907           MOVE -1               TO WTHLDL
062907           MOVE AL-UABON         TO WTHLDA
062907           MOVE ER-8799          TO EMI-ERROR
062907           PERFORM 9900-ERROR-FORMAT
062907                                 THRU  9900-EXIT
062907        END-IF
062907     END-IF
062907     IF MDAMTL  > ZERO
062907        
      * EXEC CICS BIF
062907*          DEEDIT
062907*          FIELD   (MDAMTI)
062907*          LENGTH  (8)
062907*       END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002734' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MDAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062907        IF MDAMTI NUMERIC
062907           MOVE AL-UNNON         TO MDAMTA
062907        ELSE
062907           MOVE -1               TO MDAMTL
062907           MOVE AL-UABON         TO MDAMTA
062907           MOVE ER-8799          TO EMI-ERROR
062907           PERFORM 9900-ERROR-FORMAT
062907                                 THRU  9900-EXIT
062907        END-IF
062907     END-IF
00699      IF WS-SAVE-BIN-EFFDT = LOW-VALUES OR
00700         WS-SAVE-BIN-TRMDT = LOW-VALUES
00701          NEXT SENTENCE
00702        ELSE
00703          IF WS-SAVE-BIN-TRMDT LESS THAN WS-SAVE-BIN-EFFDT
00704              MOVE ER-1228        TO EMI-ERROR
00705              MOVE -1             TO EFFDTL
00706              MOVE AL-UABON       TO EFFDTA
00707              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00708
            IF ((DELTOL > +0)
               OR (RGIDL > +0))
               AND ((DELTOI NOT = SPACES)
               AND (RGIDI NOT = SPACES))
               PERFORM 6100-CHECK-BILL-INSTR
                                       THRU 6100-EXIT
            END-IF
00709      MOVE SPACES                 TO WS-BANK-INFORMATION.
00710
00711      IF PI-AR-PROCESSING
00712         NEXT SENTENCE
00713      ELSE
00714         MOVE ZEROS               TO ACTCDL, TRNSIT1L TRNSIT2L
00715                                     BKACCTL
00716         GO TO 7099-EXIT.
00717
00718      IF (TRNSIT1L NOT = ZERO) OR
00719         (TRNSIT2L NOT = ZERO) OR
00720         (BKACCTL  NOT = ZERO) OR
00721         (ACTCDL   NOT = ZERO)
00722          MOVE 'Y'                TO WS-ACH-SW
00723      ELSE
00724          GO TO 7099-EXIT.
00725
00726      IF (ACTCDL = ZERO) OR
00727         (ACTCDI NOT = 'N' AND 'C' AND 'A' AND 'D')
00728          MOVE -1                 TO ACTCDL
00729          MOVE AL-UABON           TO ACTCDA
00730          MOVE ER-7436            TO EMI-ERROR
00731          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00732          GO TO 7099-EXIT.
00733
00734      IF ACTCDI = 'N'
00735          GO TO 7020-NEW-ACH.
00736
00737      IF ACTCDI = 'C'
00738          GO TO 7040-CHANGE-ACH.
00739
00740      IF ACTCDI = 'A'
00741          GO TO 7060-ACTIVATE-ACH.
00742
00743 ****   ACH DELETE PROCESSING EDITS
00744
00745      IF TRNSIT1L NOT = ZERO OR
00746         TRNSIT2L NOT = ZERO OR
00747         BKACCTL  NOT = ZERO
00748          MOVE -1                 TO ACTCDL
00749          MOVE AL-UABON           TO ACTCDA
00750          MOVE ER-7430            TO EMI-ERROR
00751          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00752
00753      GO TO 7099-EXIT.
00754
00755 ****   ACH NEW ENTRY PROCESSING EDITS
00756  7020-NEW-ACH.
00757
00758      IF CO-ACH-STATUS = 'A' OR 'P'
00759          MOVE -1                 TO ACTCDL
00760          MOVE AL-UABON           TO ACTCDA
00761          MOVE ER-7434            TO EMI-ERROR
00762          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00763          GO TO 7099-EXIT.
00764
00765      IF TRNSIT1L = ZERO OR
00766         TRNSIT2L = ZERO OR
00767         BKACCTL = ZERO
00768          MOVE -1                 TO TRNSIT1L
00769          MOVE ER-7431            TO EMI-ERROR
00770          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00771          GO TO 7099-EXIT.
00772
00773      IF TRNSIT1L GREATER THAN ZERO
00774          IF TRNSIT1I = SPACES
00775              MOVE -1             TO TRNSIT1L
00776              MOVE ER-7435        TO EMI-ERROR
00777              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00778              GO TO 7099-EXIT
00779          ELSE
00780              MOVE TRNSIT1I       TO WS-TRANSIT1.
00781
00782      IF TRNSIT2L GREATER THAN ZERO
00783          IF TRNSIT2I = SPACES
00784              MOVE -1             TO TRNSIT2L
00785              MOVE ER-7435        TO EMI-ERROR
00786              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00787              GO TO 7099-EXIT
00788          ELSE
00789              MOVE TRNSIT2I       TO WS-TRANSIT2.
00790
00791      IF BKACCTL GREATER THAN ZERO
00792          IF BKACCTI = SPACES
00793              MOVE -1             TO BKACCTL
00794              MOVE ER-7435        TO EMI-ERROR
00795              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00796              GO TO 7099-EXIT
00797          ELSE
00798              MOVE BKACCTI        TO WS-BKACCTI.
00799          MOVE BKACCTI            TO WS-BKACCTI.
00800
00801      PERFORM 7500-VALIDATE-TRANSIT  THRU  7500-VALIDATE-EXIT.
00802
00803      GO TO 7099-EXIT.
00804
00805 ****   ACH CHANGE ENTRY PROCESSING EDITS
00806  7040-CHANGE-ACH.
00807
00808      IF (TRNSIT1L = ZERO) AND
00809         (TRNSIT2L = ZERO) AND
00810         (BKACCTL = ZERO)
00811          MOVE -1                 TO TRNSIT1L
00812          MOVE ER-7435            TO EMI-ERROR
00813          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00814          GO TO 7099-EXIT.
00815
00816      IF CO-BANK-INFORMATION = SPACES OR LOW-VALUES
00817          MOVE -1                 TO ACTCDL
00818          MOVE ER-7432            TO EMI-ERROR
00819          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00820          GO TO 7099-EXIT.
00821
00822      MOVE CO-BANK-INFORMATION    TO WS-BANK-INFORMATION.
00823
00824      IF TRNSIT1L GREATER THAN ZERO
00825          IF TRNSIT1I = SPACES
00826              MOVE -1             TO TRNSIT1L
00827              MOVE ER-7435        TO EMI-ERROR
00828              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00829              GO TO 7099-EXIT
00830          ELSE
00831              MOVE TRNSIT1I       TO WS-TRANSIT1.
00832
00833      IF TRNSIT2L GREATER THAN ZERO
00834          IF TRNSIT2I = SPACES
00835              MOVE -1             TO TRNSIT2L
00836              MOVE ER-7435        TO EMI-ERROR
00837              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00838              GO TO 7099-EXIT
00839          ELSE
00840              MOVE TRNSIT2I       TO WS-TRANSIT2.
00841
00842      IF BKACCTL GREATER THAN ZERO
00843          IF BKACCTI = SPACES
00844              MOVE -1             TO BKACCTL
00845              MOVE ER-7435        TO EMI-ERROR
00846              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00847              GO TO 7099-EXIT
00848          ELSE
00849              MOVE BKACCTI        TO WS-BKACCTI.
00850
00851      IF CO-BANK-INFORMATION = WS-BANK-INFORMATION
00852          MOVE -1                 TO ACTCDL
00853          MOVE ER-7449            TO EMI-ERROR
00854          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00855          GO TO 7099-EXIT.
00856
00857      IF TRNSIT1L GREATER THAN ZERO OR
00858         TRNSIT2L GREATER THAN ZERO
00859          PERFORM 7500-VALIDATE-TRANSIT  THRU  7500-VALIDATE-EXIT.
00860
00861      GO TO 7099-EXIT.
00862
00863 ****   ACH ACTIVATE ENTRY PROCESSING EDITS
00864  7060-ACTIVATE-ACH.
00865
00866      IF NOT CO-ACH-PENDING
00867          MOVE -1                 TO ACTCDL
00868          MOVE ER-7465            TO EMI-ERROR
00869          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00870          GO TO 7099-EXIT.
00871
00872      IF TRNSIT1L NOT = ZERO OR
00873         TRNSIT2L NOT = ZERO OR
00874         BKACCTL  NOT = ZERO
00875          MOVE -1                 TO ACTCDL
00876          MOVE ER-7440            TO EMI-ERROR
00877          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00878          GO TO 7099-EXIT.
00879
00880  7099-EXIT.
00881      EXIT.
00882      EJECT
00883
00884  7100-READ-ERCOMP.
00885
00886      
      * EXEC CICS HANDLE CONDITION
00887 *        NOTOPEN (7100-NOTOPEN)
00888 *        NOTFND  (7100-NOTFND)
00889 *    END-EXEC.
      *    MOVE '"$JI                  ! $ #00002943' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032393433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00890
00891      
      * EXEC CICS READ
00892 *         DATASET  (ERCOMP-FILE-ID)
00893 *         SET      (ADDRESS OF COMPENSATION-MASTER)
00894 *         RIDFLD   (PI-ERCOMP-KEY)
00895 *    END-EXEC.
      *    MOVE '&"S        E          (   #00002948' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00896
      *    PERFORM 7200-READ-ERCOBI    THRU 7200-EXIT
00897      IF DO-NOT-MOVE-TO-MAP
00898         MOVE 'Y'                 TO SUPPRESS-MAP-SW
00899         GO TO 7100-EXIT.
00900
00901      MOVE CO-LAST-MAINT-USER     TO PI-UPDATE-BY.
00902      MOVE CO-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
00903      MOVE CO-CONTROL-PRIMARY     TO PI-ERCOMP-KEY.
00904      MOVE CO-CARRIER             TO CARRIERO.
00905      MOVE CO-GROUPING            TO GROUPO.
00906      MOVE CO-TYPE                TO TYPEO.
00907      MOVE CO-RESP-NO             TO FINRESPO.
00908      MOVE CO-CTL-2               TO COACCTO.
00909      MOVE CO-BANK-INFORMATION    TO PI-BANK-TRANSIT-NUMBER (2:8)
00910      MOVE PI-FEDERAL-NUMBER      TO TRNSIT1O.
00911      MOVE PI-BANK-NUMBER         TO TRNSIT2O.
00912      GO TO 7100-EXIT.
00913
00914  7100-NOTFND.
00915      MOVE ER-7462               TO EMI-ERROR
00916      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00917      GO TO 7100-EXIT.
00918
00919  7100-NOTOPEN.
00920      MOVE ER-2233               TO EMI-ERROR
00921      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00922      GO TO 7100-EXIT.
00923
00924  7100-EXIT.
00925      EXIT.
00926      EJECT
00927
       7200-READ-ERCOBI.
           MOVE CO-CONTROL-PRIMARY     TO ERCOBI-KEY
           
      * EXEC CICS READ
      *       DATASET (ERCOBI-FILE-ID)
      *       SET     (ADDRESS OF COMP-BILLING-INSTRUCTIONS)
      *       RIDFLD  (ERCOBI-KEY)
      *       RESP    (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        E          (  N#00002988' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303032393838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOBI-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOBI-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMP-BILLING-INSTRUCTIONS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              SET BILLING-INSTRUCTIONS-FOUND TO TRUE
           END-IF
           .
       7200-EXIT.
           EXIT.
00928  7300-READ-ERCOMP-UPDATE.
00929      
      * EXEC CICS HANDLE CONDITION
00930 *        NOTFND  (7300-NOTFND)
00931 *        NOTOPEN (7300-NOTOPEN)
00932 *    END-EXEC.
      *    MOVE '"$IJ                  ! % #00003001' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033303031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00933
00934      
      * EXEC CICS READ
00935 *         DATASET  (ERCOMP-FILE-ID)
00936 *         SET      (ADDRESS OF COMPENSATION-MASTER)
00937 *         RIDFLD   (PI-ERCOMP-KEY)
00938 *         UPDATE
00939 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003006' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00940      GO TO 7300-EXIT.
00941
00942  7300-NOTOPEN.
00943
00944      MOVE ER-2233               TO EMI-ERROR
00945      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00946      GO TO 7300-EXIT.
00947  7300-NOTFND.
00948
00949      MOVE ER-7462               TO EMI-ERROR
00950      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00951      GO TO 7300-EXIT.
00952
00953  7300-EXIT.
00954      EXIT.
00955      EJECT
00956
00957  7400-DELETE-PRE-NOTE.
00958
00959      
      * EXEC CICS HANDLE CONDITION
00960 *        NOTFND  (7499-EXIT)
00961 *        NOTOPEN (7450-NOTOPEN)
00962 *    END-EXEC.
      *    MOVE '"$IJ                  ! & #00003031' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033303331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00963
00964      
      * EXEC CICS READ
00965 *         DATASET  (ELACHP-FILE-ID)
00966 *         SET     (ADDRESS OF ACH-PRENOTIFICATION)
00967 *         RIDFLD   (CO-CONTROL-PRIMARY)
00968 *         UPDATE
00969 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003036' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACHP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACH-PRENOTIFICATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00970
00971      
      * EXEC CICS DELETE
00972 *        DATASET   (ELACHP-FILE-ID)
00973 *    END-EXEC.
      *    MOVE '&(                    &   #00003043' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACHP-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00974
00975      GO TO 7499-EXIT.
00976
00977  7450-NOTOPEN.
00978      MOVE ER-7469               TO EMI-ERROR
00979      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00980
00981  7499-EXIT.
00982      EXIT.
00983      EJECT
00984
00985 ******************************************************************
00986 *             V A L I D T E  T R A N S I T  N U M B E R
00987 ******************************************************************
00988  7500-VALIDATE-TRANSIT.
00989
00990      
      * EXEC CICS HANDLE CONDITION
00991 *        NOTFND   (7500-NOT-FOUND)
00992 *        NOTOPEN  (7500-NOT-OPEN)
00993 *    END-EXEC.
      *    MOVE '"$IJ                  ! '' #00003062' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033303632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00994
00995      MOVE WS-TRANSIT1          TO PI-FEDERAL-NUMBER.
00996      MOVE WS-TRANSIT2          TO PI-BANK-NUMBER.
00997      MOVE PI-COMPANY-CD        TO PI-BANK-COMPANY-CD.
00998
00999      
      * EXEC CICS READ
01000 *        EQUAL
01001 *        DATASET   (ELBANK-FILE-ID)
01002 *        SET       (ADDRESS OF BANK-MASTER)
01003 *        RIDFLD    (PI-BANK-TRANSIT-NUMBER)
01004 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003071' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELBANK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-BANK-TRANSIT-NUMBER, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BANK-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01005
01006      GO TO 7500-VALIDATE-EXIT.
01007
01008  7500-NOT-OPEN.
01009
01010      MOVE -1                    TO TRNSIT1L
01011      MOVE AL-UABON              TO TRNSIT1A
01012      MOVE AL-UABON              TO TRNSIT2A
01013      MOVE ER-7468               TO EMI-ERROR .
01014      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01015      GO TO 7500-VALIDATE-EXIT.
01016
01017  7500-NOT-FOUND.
01018
01019      MOVE -1                    TO TRNSIT1L
01020      MOVE AL-UABON              TO TRNSIT1A
01021      MOVE AL-UABON              TO TRNSIT2A
01022      MOVE ER-9388               TO EMI-ERROR .
01023      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01024      GO TO 7500-VALIDATE-EXIT.
01025
01026  7500-VALIDATE-EXIT.
01027      EXIT.
01028      EJECT
01029
01030  7600-WRITE-ELACHP.
01031      
      * EXEC CICS HANDLE CONDITION
01032 *        NOTOPEN (7450-NOTOPEN)
01033 *        DUPREC   (7610-DUPREC)
01034 *    END-EXEC.
      *    MOVE '"$J%                  ! ( #00003103' TO DFHEIV0
           MOVE X'22244A252020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033313033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01035
01036      
      * EXEC CICS GETMAIN
01037 *        SET      (ADDRESS OF ACH-PRENOTIFICATION)
01038 *        LENGTH   (210)
01039 *    END-EXEC.
           MOVE 210
             TO DFHEIV11
      *    MOVE '," L                  $   #00003108' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 DFHEIV99
           SET ADDRESS OF ACH-PRENOTIFICATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01040
01041      INITIALIZE  ACH-PRENOTIFICATION.
01042
01043      MOVE  'AP'                  TO AP-RECORD-ID.
01044
01045      MOVE PI-PROCESSOR-ID        TO AP-LAST-CHANGE-PROCESSOR
01046      MOVE CO-CONTROL-PRIMARY     TO AP-CONTROL-PRIMARY
01047      MOVE EIBTIME                TO AP-LAST-CHANGE-TIME
01048      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
01049      MOVE '5'                    TO DC-OPTION-CODE.
01050      PERFORM 9700-DATE-CONVERT THRU 9700-EXIT.
01051
01052      MOVE DC-BIN-DATE-1          TO AP-LAST-CHANGE-DT.
01053      MOVE WS-BANK-DATA           TO AP-BANK-INFORMATION.
01054      MOVE BM-NAME                TO AP-BANK-NAME.
01055
01056      
      * EXEC CICS WRITE
01057 *        FROM      (ACH-PRENOTIFICATION)
01058 *        DATASET   (ELACHP-FILE-ID)
01059 *        RIDFLD    (CO-CONTROL-PRIMARY)
01060 *    END-EXEC.
           MOVE LENGTH OF
            ACH-PRENOTIFICATION
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003128' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACHP-FILE-ID, 
                 ACH-PRENOTIFICATION, 
                 DFHEIV11, 
                 CO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01061
01062      GO TO 7620-WRITE-EXIT.
01063
01064  7610-DUPREC.
01065
01066      MOVE    ER-7447            TO EMI-ERROR
01067      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01068      GO TO 7620-WRITE-EXIT.
01069
01070  7620-WRITE-EXIT.
01071       EXIT.
01072      EJECT
01073 ******************************************************************
01074  8100-SEND-INITIAL-MAP.
01075      MOVE WS-SAVE-DATE           TO DATEO.
01076      MOVE EIBTIME                TO TIME-IN.
01077      MOVE TIME-OUT               TO TIMEO.
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
01078      MOVE -1                     TO MAINTYPL.
01079      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
01080      MOVE WS-AGENT-BANK-DESC    TO BKDESCO.
01081
01082 * HIDE ACH FIELDS IF NOT AR PROCESSING
01083      IF PI-AR-PROCESSING
01084         NEXT SENTENCE
01085      ELSE
01086         MOVE AL-SADOF             TO BKLITA, BKDESCA  CRSTATA
01087                                      ACTLITA CRSLITA ACHPF1A
01088                                      TRNSIT1A TRNSIT2A TRDASHA
01089                                      BKACCTA, ACTCDA.
01090      
      * EXEC CICS SEND
01091 *        MAP      (MAP-NAME)
01092 *        MAPSET   (MAPSET-NAME)
01093 *        FROM     (EL6521AO)
01094 *        ERASE
01095 *        CURSOR
01096 *    END-EXEC.
           MOVE LENGTH OF
            EL6521AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003164' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6521AO, 
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
           
01097
01098      GO TO 9100-RETURN-TRAN.
01099
01100  EJECT
01101  8200-SEND-DATAONLY.
01102      MOVE WS-SAVE-DATE           TO DATEO.
01103      MOVE EIBTIME                TO TIME-IN.
01104      MOVE TIME-OUT               TO TIMEO.
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO USERIDO.
01105      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
01106      MOVE WS-AGENT-BANK-DESC     TO BKDESCO.
01107
01108 * HIDE ACH FIELDS IF NOT AR PROCESSING
01109      IF PI-AR-PROCESSING
01110         NEXT SENTENCE
01111      ELSE
01112         MOVE AL-SADOF             TO BKLITA, BKDESCA  CRSTATA
01113                                      ACTLITA CRSLITA ACHPF1A
01114                                      TRNSIT1A TRNSIT2A TRDASHA
01115                                      BKACCTA, ACTCDA.
01116      
      * EXEC CICS SEND
01117 *        MAP      (MAP-NAME)
01118 *        MAPSET   (MAPSET-NAME)
01119 *        FROM     (EL6521AO)
01120 *        DATAONLY
01121 *        CURSOR
01122 *    END-EXEC.
           MOVE LENGTH OF
            EL6521AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003192' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6521AO, 
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
           
01123
01124      GO TO 9100-RETURN-TRAN.
01125
01126  EJECT
01127  8300-SEND-TEXT.
01128      
      * EXEC CICS SEND TEXT
01129 *        FROM     (LOGOFF-TEXT)
01130 *        LENGTH   (LOGOFF-LENGTH)
01131 *        ERASE
01132 *        FREEKB
01133 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003204' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323034' TO DFHEIV0(25:11)
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
           
01134
01135      
      * EXEC CICS RETURN
01136 *    END-EXEC.
      *    MOVE '.(                    ''   #00003211' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01137
01138  EJECT
01139  8600-DEEDIT.
01140      
      * EXEC CICS BIF DEEDIT
01141 *         FIELD(DEEDIT-FIELD)
01142 *         LENGTH(15)
01143 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003216' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01144
01145  EJECT
01146  8800-UNAUTHORIZED-ACCESS.
01147      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01148      GO TO 8300-SEND-TEXT.
01149
01150  8810-PF23.
01151      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01152      MOVE XCTL-005               TO PGM-NAME.
01153      GO TO 9300-XCTL.
01154  EJECT
01155
01156  9100-RETURN-TRAN.
01157      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01158      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
01159      
      * EXEC CICS RETURN
01160 *        TRANSID    (TRANS-ID)
01161 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01162 *        LENGTH     (WS-COMM-LENGTH)
01163 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00003235' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01164
01165  9200-RETURN-MAIN-MENU.
01166      MOVE XCTL-626               TO PGM-NAME.
01167      GO TO 9300-XCTL.
01168
01169  EJECT
01170  9300-XCTL.
01171      
      * EXEC CICS XCTL
01172 *        PROGRAM    (PGM-NAME)
01173 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01174 *        LENGTH     (WS-COMM-LENGTH)
01175 *    END-EXEC.
      *    MOVE '.$C                   %   #00003247' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01176
01177  9400-CLEAR.
01178      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
01179      GO TO 9300-XCTL.
01180
01181  9500-PF12.
01182      MOVE XCTL-010               TO PGM-NAME.
01183      GO TO 9300-XCTL.
01184
01185  9600-PGMID-ERROR.
01186      
      * EXEC CICS HANDLE CONDITION
01187 *        PGMIDERR    (8300-SEND-TEXT)
01188 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00003262' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303033323632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01189
01190      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01191      MOVE ' '                    TO PI-ENTRY-CD-1.
01192      MOVE XCTL-005               TO PGM-NAME.
01193      MOVE PGM-NAME               TO LOGOFF-PGM.
01194      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01195      GO TO 9300-XCTL.
01196
01197  9700-DATE-CONVERT.
01198      
      * EXEC CICS LINK
01199 *        PROGRAM    ('ELDATCV')
01200 *        COMMAREA   (DATE-CONVERSION-DATA)
01201 *        LENGTH     (DC-COMM-LENGTH)
01202 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00003274' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01203
01204  9700-EXIT.
01205      EXIT.
01206
01207  EJECT
01208  9900-ERROR-FORMAT.
01209
01210      IF NOT EMI-ERRORS-COMPLETE
01211          MOVE LINK-001           TO PGM-NAME
01212          
      * EXEC CICS LINK
01213 *            PROGRAM    (PGM-NAME)
01214 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01215 *            LENGTH     (EMI-COMM-LENGTH)
01216 *        END-EXEC.
      *    MOVE '."C                   (   #00003288' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01217
01218  9900-EXIT.
01219      EXIT.
01220
01221  9990-ABEND.
01222      MOVE LINK-004               TO PGM-NAME.
01223      MOVE DFHEIBLK               TO EMI-LINE1.
01224      
      * EXEC CICS LINK
01225 *        PROGRAM   (PGM-NAME)
01226 *        COMMAREA  (EMI-LINE1)
01227 *        LENGTH    (72)
01228 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00003300' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01229
01230      GO TO 8200-SEND-DATAONLY.
01231
01232  9995-SECURITY-VIOLATION.
01233 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00003326' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333236' TO DFHEIV0(25:11)
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
01234
01235  9995-EXIT.
01236       EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6521' TO DFHEIV1
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
               GO TO 0700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7100-NOTOPEN,
                     7100-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7300-NOTFND,
                     7300-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7499-EXIT,
                     7450-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7500-NOT-FOUND,
                     7500-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7450-NOTOPEN,
                     7610-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6521' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
