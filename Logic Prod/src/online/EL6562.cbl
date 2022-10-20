00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL6562.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 15:49:07.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.008
00008 *
00009
00010 *AUTHOR.     LOGIC,INC.
00011 *            DALLAS, TEXAS.
00012
00013 *DATE-COMPILED.
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
00025 *REMARKS.    TRANSACTION - EXE3 - RATE MASTER MAINTENANCE
00026 *                                 (RATES INPUT)
00027      EJECT
081413******************************************************************
081413*                   C H A N G E   L O G
081413*
081413* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081413*-----------------------------------------------------------------
081413*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081413* EFFECTIVE    NUMBER
081413*-----------------------------------------------------------------
081413* 081413    2013080700002  PEMA ADD JOURNALING OF ERRATE FILE
081413******************************************************************
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6562 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.008 *********'.
00034
00035  01  WS-DATE-AREA.
00036      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00037      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00038
00039 ***  Y2K PROJ 7744
00040  01  WS-RATE-HOLD-AREA.
00041      05  WS-HOLD-RATE-EXP-AL PIC X(11).
00042      05  WS-HOLD-RATE-EXP-DT REDEFINES
00043          WS-HOLD-RATE-EXP-AL PIC 9(11).
00044 ***  Y2K PROJ 7744
00045
00046  01  STANDARD-AREAS.
00047      05  GETMAIN-SPACE       PIC X       VALUE SPACE.
00048      05  TRANS-ID            PIC X(4)    VALUE 'EXE3'.
00049      05  THIS-PGM            PIC X(8)    VALUE 'EL6562'.
00050      05  WS-MAPNAME          PIC X(8)    VALUE 'EL6562A'.
00051      05  WS-MAPSET-NAME      PIC X(8)    VALUE 'EL6562S'.
00052      05  PGM-NAME            PIC X(8).
00053      05  SUB1                PIC S999    VALUE +0.
00054      05  SUB2                PIC S999    VALUE +0.
00055      05  SUB3                PIC S99     VALUE +0.
00056          88  EVEN-SUB                    VALUE +2 +4 +6 +8.
00057          88  ODD-SUB                     VALUE +1 +3 +5 +7.
00058      05  SUB4                PIC S99     VALUE +0.
00059      05  SC-ITEM             PIC S9(4)   VALUE +1     COMP.
00060      05  WS-COMP-CD-R.
00061          10  FILLER          PIC X.
00062          10  WS-COMP-CD-X    PIC X.
00063      05  WS-COMP-CD   REDEFINES WS-COMP-CD-R  PIC S9(4)     COMP.
00064      05  TIME-IN             PIC S9(7).
00065      05  TIME-OUT-R  REDEFINES TIME-IN.
00066          10  FILLER          PIC X.
00067          10  TIME-OUT        PIC 99V99.
00068          10  FILLER          PIC X(2).
00069      05  XCTL-005            PIC X(8)    VALUE 'EL005'.
00070      05  XCTL-010            PIC X(8)    VALUE 'EL010'.
00071      05  XCTL-626            PIC X(8)    VALUE 'EL126'.
00072      05  XCTL-656            PIC X(8)    VALUE 'EL656'.
00073      05  XCTL-6561           PIC X(8)    VALUE 'EL6561'.
00074      05  XCTL-6563           PIC X(8)    VALUE 'EL6563'.
00075      05  XCTL-6564           PIC X(8)    VALUE 'EL6564'.
00076      05  LINK-001            PIC X(8)    VALUE 'EL001'.
00077      05  LINK-004            PIC X(8)    VALUE 'EL004'.
00078      05  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00079
00080      05  ER-0000             PIC X(4)    VALUE  '0000'.
00081      05  ER-0004             PIC X(4)    VALUE  '0004'.
00082      05  ER-0008             PIC X(4)    VALUE  '0008'.
00083      05  ER-0029             PIC X(4)    VALUE  '0029'.
00084      05  ER-0068             PIC X(4)    VALUE  '0068'.
00085      05  ER-0070             PIC X(4)    VALUE  '0070'.
00086      05  ER-0142             PIC X(4)    VALUE  '0142'.
00087      05  ER-2055             PIC X(4)    VALUE  '2055'.
00088      05  ER-2056             PIC X(4)    VALUE  '2056'.
00089      05  ER-2271             PIC X(4)    VALUE  '2271'.
00090      05  ER-2272             PIC X(4)    VALUE  '2272'.
00091      05  ER-2280             PIC X(4)    VALUE  '2280'.
00092      05  ER-2285             PIC X(4)    VALUE  '2285'.
00093      05  ER-2296             PIC X(4)    VALUE  '2296'.
00094
00095      05  RATE-FILE-ID        PIC X(8)    VALUE 'ERRATE'.
00096      05  OE-RATE-FILE-ID     PIC X(8)    VALUE 'OERATE'.
00097      05  FILE-ID             PIC X(8)    VALUE  SPACES.
00098      05  CNTL-FILE-ID        PIC X(8)    VALUE 'ELCNTL'.
00099      05  BIN-CURRENT-SAVE    PIC XX      VALUE SPACES.
00100
00101      05  DEEDIT-FIELD            PIC X(15).
00102      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
00103      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(10)V99999.
00104
00105      05  ERRATE-LENGTH       PIC S9(4)   VALUE +1765 COMP.
00106
00107      05  WS-PREV-AGE         PIC 99       VALUE ZERO.
00108      05  WS-PREV-TRM         PIC 999      VALUE ZERO.
00109      05  WS-PREV-AMT         PIC S9(7)V99 VALUE ZERO.
00110      05  WS-TERM.
00111          10  WS-FROM         PIC 999.
00112          10  FILLER          PIC X        VALUE '-'.
00113          10  WS-TO           PIC 999.
00114
00115      05  WS-SAVE-KEY         PIC X(28)    VALUE SPACES.
00116      05  WS-SAVE-STRUCTURE   PIC X(7)     VALUE SPACES.
00117
00118      05  ELCNTL-KEY.
00119          10  CNTL-COMP-ID    PIC X(3)    VALUE SPACES.
00120          10  CNTL-REC-TYPE   PIC X       VALUE SPACES.
00121          10  CNTL-ACCESS     PIC X(4)    VALUE SPACES.
00122          10  CNTL-SEQ-NO     PIC S9(4)   VALUE +0  COMP.
00123
00124      05  MISC-SAVE-AREAS.
00125          10  WS-SAVE-RATE    OCCURS 48 TIMES
00126                              PIC S9(2)V9(5).
00127
081413 01  filler.
081413     12  WS-RESPONSE             PIC S9(8)   COMP.
081413         88  WS-RESP-NORMAL              VALUE +00.
081413         88  WS-RESP-ERROR               VALUE +01.
081413         88  WS-RESP-NOTFND              VALUE +13.
081413         88  WS-RESP-DUPKEY              VALUE +15.
081413         88  WS-RESP-NOTOPEN             VALUE +19.
081413         88  WS-RESP-ENDFILE             VALUE +20.
081413
00129 *                          COPY ELCSCTM.
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
00130  EJECT
00131 *                          COPY ELCSCRTY.
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
00132      EJECT
00133 *                                  COPY ELCDATE.
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
00134      EJECT
00135 *                                  COPY ELCLOGOF.
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
00136      EJECT
00137 *                                  COPY ELCATTR.
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
00138      EJECT
00139 *                                  COPY ELCEMIB.
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
00140      EJECT
00141 *                                  COPY ELCINTF.
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
00142      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00143          16  PI-FILE-ID                    PIC XX.
00144          16  PI-MAINT                      PIC X.
00145
00146          16  PI-ERRATE-KEY.
00147              20  PI-RATE-COMPANY-CD         PIC X.
00148              20  PI-RATE-STATE-CODE.
00149                  24  PI-RATE-CODE           PIC XX.
00150                  24  PI-RATE-CLASS          PIC XX.
00151                  24  PI-RATE-DEV            PIC X(3).
00152              20  PI-RATE-L-AH-CODE.
00153                  24  PI-RATE-L-AH           PIC X.
00154                  24  PI-RATE-LAH-NUM        PIC XX.
00155              20  PI-RATE-LIMITS.
00156                  24  PI-RATE-HIGH-AGE       PIC 99.
00157                  24  PI-RATE-HIGH-AMT       PIC 9(6).
00158                  24  PI-RATE-FUTURE         PIC XX.
00159                  24  PI-RATE-SEX            PIC X.
00160              20  PI-RATE-EXPIRY-DATE        PIC 9(11) COMP-3.
00161
00162          16  PI-SAVE-ERRATE-KEY             PIC X(28).
00163
00164          16  PI-BROWSE-SW                  PIC X.
00165              88  BROWSE-STARTED                  VALUE 'Y'.
00166          16  PI-ERRATE-EOF-SW              PIC X.
00167              88  ERRATE-EOF                      VALUE 'Y'.
00168
00169          16  PI-SUB                        PIC S999.
00170          16  PI-YEAR                       PIC S99.
00171          16  FILLER                        PIC X(574).
00172
00173      EJECT
00174 *                            COPY ELCJPFX.
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
081413                             PIC X(2000).
00176
00177      EJECT
00178 *                            COPY ELCAID.
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
00179
00180  01  FILLER    REDEFINES DFHAID.
00181      05  FILLER              PIC X(8).
00182      05  PF-VALUES           PIC X       OCCURS 2.
00183
00184      EJECT
00185 *                            COPY EL6562S.
       01  EL6562AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDATEL PIC S9(0004) COMP.
           05  RUNDATEF PIC  X(0001).
           05  FILLER REDEFINES RUNDATEF.
               10  RUNDATEA PIC  X(0001).
           05  RUNDATEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  CLASSL PIC S9(0004) COMP.
           05  CLASSF PIC  X(0001).
           05  FILLER REDEFINES CLASSF.
               10  CLASSA PIC  X(0001).
           05  CLASSI PIC  X(0002).
      *    -------------------------------
           05  DEVL PIC S9(0004) COMP.
           05  DEVF PIC  X(0001).
           05  FILLER REDEFINES DEVF.
               10  DEVA PIC  X(0001).
           05  DEVI PIC  X(0003).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  PLANL PIC S9(0004) COMP.
           05  PLANF PIC  X(0001).
           05  FILLER REDEFINES PLANF.
               10  PLANA PIC  X(0001).
           05  PLANI PIC  X(0002).
      *    -------------------------------
           05  RTAGEL PIC S9(0004) COMP.
           05  RTAGEF PIC  X(0001).
           05  FILLER REDEFINES RTAGEF.
               10  RTAGEA PIC  X(0001).
           05  RTAGEI PIC  99.
      *    -------------------------------
           05  EXPIREL PIC S9(0004) COMP.
           05  EXPIREF PIC  X(0001).
           05  FILLER REDEFINES EXPIREF.
               10  EXPIREA PIC  X(0001).
           05  EXPIREI PIC  X(0006).
      *    -------------------------------
           05  COMMENTL PIC S9(0004) COMP.
           05  COMMENTF PIC  X(0001).
           05  FILLER REDEFINES COMMENTF.
               10  COMMENTA PIC  X(0001).
           05  COMMENTI PIC  X(0050).
      *    -------------------------------
           05  YR1L PIC S9(0004) COMP.
           05  YR1F PIC  X(0001).
           05  FILLER REDEFINES YR1F.
               10  YR1A PIC  X(0001).
           05  YR1I PIC  9(2).
      *    -------------------------------
           05  M01L PIC S9(0004) COMP.
           05  M01F PIC  X(0001).
           05  FILLER REDEFINES M01F.
               10  M01A PIC  X(0001).
           05  M01I PIC  X(0007).
      *    -------------------------------
           05  YR1M01L PIC S9(0004) COMP.
           05  YR1M01F PIC  X(0001).
           05  FILLER REDEFINES YR1M01F.
               10  YR1M01A PIC  X(0001).
           05  YR1M01I PIC  X(0008).
      *    -------------------------------
           05  YR1M02L PIC S9(0004) COMP.
           05  YR1M02F PIC  X(0001).
           05  FILLER REDEFINES YR1M02F.
               10  YR1M02A PIC  X(0001).
           05  YR1M02I PIC  X(0008).
      *    -------------------------------
           05  YR1M03L PIC S9(0004) COMP.
           05  YR1M03F PIC  X(0001).
           05  FILLER REDEFINES YR1M03F.
               10  YR1M03A PIC  X(0001).
           05  YR1M03I PIC  X(0008).
      *    -------------------------------
           05  YR1M04L PIC S9(0004) COMP.
           05  YR1M04F PIC  X(0001).
           05  FILLER REDEFINES YR1M04F.
               10  YR1M04A PIC  X(0001).
           05  YR1M04I PIC  X(0008).
      *    -------------------------------
           05  YR1M05L PIC S9(0004) COMP.
           05  YR1M05F PIC  X(0001).
           05  FILLER REDEFINES YR1M05F.
               10  YR1M05A PIC  X(0001).
           05  YR1M05I PIC  X(0008).
      *    -------------------------------
           05  YR1M06L PIC S9(0004) COMP.
           05  YR1M06F PIC  X(0001).
           05  FILLER REDEFINES YR1M06F.
               10  YR1M06A PIC  X(0001).
           05  YR1M06I PIC  X(0008).
      *    -------------------------------
           05  YR1AL PIC S9(0004) COMP.
           05  YR1AF PIC  X(0001).
           05  FILLER REDEFINES YR1AF.
               10  YR1AA PIC  X(0001).
           05  YR1AI PIC  9(2).
      *    -------------------------------
           05  M01AL PIC S9(0004) COMP.
           05  M01AF PIC  X(0001).
           05  FILLER REDEFINES M01AF.
               10  M01AA PIC  X(0001).
           05  M01AI PIC  X(0007).
      *    -------------------------------
           05  YR1M07L PIC S9(0004) COMP.
           05  YR1M07F PIC  X(0001).
           05  FILLER REDEFINES YR1M07F.
               10  YR1M07A PIC  X(0001).
           05  YR1M07I PIC  X(0008).
      *    -------------------------------
           05  YR1M08L PIC S9(0004) COMP.
           05  YR1M08F PIC  X(0001).
           05  FILLER REDEFINES YR1M08F.
               10  YR1M08A PIC  X(0001).
           05  YR1M08I PIC  X(0008).
      *    -------------------------------
           05  YR1M09L PIC S9(0004) COMP.
           05  YR1M09F PIC  X(0001).
           05  FILLER REDEFINES YR1M09F.
               10  YR1M09A PIC  X(0001).
           05  YR1M09I PIC  X(0008).
      *    -------------------------------
           05  YR1M10L PIC S9(0004) COMP.
           05  YR1M10F PIC  X(0001).
           05  FILLER REDEFINES YR1M10F.
               10  YR1M10A PIC  X(0001).
           05  YR1M10I PIC  X(0008).
      *    -------------------------------
           05  YR1M11L PIC S9(0004) COMP.
           05  YR1M11F PIC  X(0001).
           05  FILLER REDEFINES YR1M11F.
               10  YR1M11A PIC  X(0001).
           05  YR1M11I PIC  X(0008).
      *    -------------------------------
           05  YR1M12L PIC S9(0004) COMP.
           05  YR1M12F PIC  X(0001).
           05  FILLER REDEFINES YR1M12F.
               10  YR1M12A PIC  X(0001).
           05  YR1M12I PIC  X(0008).
      *    -------------------------------
           05  YR2L PIC S9(0004) COMP.
           05  YR2F PIC  X(0001).
           05  FILLER REDEFINES YR2F.
               10  YR2A PIC  X(0001).
           05  YR2I PIC  9(2).
      *    -------------------------------
           05  M02L PIC S9(0004) COMP.
           05  M02F PIC  X(0001).
           05  FILLER REDEFINES M02F.
               10  M02A PIC  X(0001).
           05  M02I PIC  X(0007).
      *    -------------------------------
           05  YR2M01L PIC S9(0004) COMP.
           05  YR2M01F PIC  X(0001).
           05  FILLER REDEFINES YR2M01F.
               10  YR2M01A PIC  X(0001).
           05  YR2M01I PIC  X(0008).
      *    -------------------------------
           05  YR2M02L PIC S9(0004) COMP.
           05  YR2M02F PIC  X(0001).
           05  FILLER REDEFINES YR2M02F.
               10  YR2M02A PIC  X(0001).
           05  YR2M02I PIC  X(0008).
      *    -------------------------------
           05  YR2M03L PIC S9(0004) COMP.
           05  YR2M03F PIC  X(0001).
           05  FILLER REDEFINES YR2M03F.
               10  YR2M03A PIC  X(0001).
           05  YR2M03I PIC  X(0008).
      *    -------------------------------
           05  YR2M04L PIC S9(0004) COMP.
           05  YR2M04F PIC  X(0001).
           05  FILLER REDEFINES YR2M04F.
               10  YR2M04A PIC  X(0001).
           05  YR2M04I PIC  X(0008).
      *    -------------------------------
           05  YR2M05L PIC S9(0004) COMP.
           05  YR2M05F PIC  X(0001).
           05  FILLER REDEFINES YR2M05F.
               10  YR2M05A PIC  X(0001).
           05  YR2M05I PIC  X(0008).
      *    -------------------------------
           05  YR2M06L PIC S9(0004) COMP.
           05  YR2M06F PIC  X(0001).
           05  FILLER REDEFINES YR2M06F.
               10  YR2M06A PIC  X(0001).
           05  YR2M06I PIC  X(0008).
      *    -------------------------------
           05  YR2AL PIC S9(0004) COMP.
           05  YR2AF PIC  X(0001).
           05  FILLER REDEFINES YR2AF.
               10  YR2AA PIC  X(0001).
           05  YR2AI PIC  9(2).
      *    -------------------------------
           05  M02AL PIC S9(0004) COMP.
           05  M02AF PIC  X(0001).
           05  FILLER REDEFINES M02AF.
               10  M02AA PIC  X(0001).
           05  M02AI PIC  X(0007).
      *    -------------------------------
           05  YR2M07L PIC S9(0004) COMP.
           05  YR2M07F PIC  X(0001).
           05  FILLER REDEFINES YR2M07F.
               10  YR2M07A PIC  X(0001).
           05  YR2M07I PIC  X(0008).
      *    -------------------------------
           05  YR2M08L PIC S9(0004) COMP.
           05  YR2M08F PIC  X(0001).
           05  FILLER REDEFINES YR2M08F.
               10  YR2M08A PIC  X(0001).
           05  YR2M08I PIC  X(0008).
      *    -------------------------------
           05  YR2M09L PIC S9(0004) COMP.
           05  YR2M09F PIC  X(0001).
           05  FILLER REDEFINES YR2M09F.
               10  YR2M09A PIC  X(0001).
           05  YR2M09I PIC  X(0008).
      *    -------------------------------
           05  YR2M10L PIC S9(0004) COMP.
           05  YR2M10F PIC  X(0001).
           05  FILLER REDEFINES YR2M10F.
               10  YR2M10A PIC  X(0001).
           05  YR2M10I PIC  X(0008).
      *    -------------------------------
           05  YR2M11L PIC S9(0004) COMP.
           05  YR2M11F PIC  X(0001).
           05  FILLER REDEFINES YR2M11F.
               10  YR2M11A PIC  X(0001).
           05  YR2M11I PIC  X(0008).
      *    -------------------------------
           05  YR2M12L PIC S9(0004) COMP.
           05  YR2M12F PIC  X(0001).
           05  FILLER REDEFINES YR2M12F.
               10  YR2M12A PIC  X(0001).
           05  YR2M12I PIC  X(0008).
      *    -------------------------------
           05  YR3L PIC S9(0004) COMP.
           05  YR3F PIC  X(0001).
           05  FILLER REDEFINES YR3F.
               10  YR3A PIC  X(0001).
           05  YR3I PIC  9(2).
      *    -------------------------------
           05  M03L PIC S9(0004) COMP.
           05  M03F PIC  X(0001).
           05  FILLER REDEFINES M03F.
               10  M03A PIC  X(0001).
           05  M03I PIC  X(0007).
      *    -------------------------------
           05  YR3M01L PIC S9(0004) COMP.
           05  YR3M01F PIC  X(0001).
           05  FILLER REDEFINES YR3M01F.
               10  YR3M01A PIC  X(0001).
           05  YR3M01I PIC  X(0008).
      *    -------------------------------
           05  YR3M02L PIC S9(0004) COMP.
           05  YR3M02F PIC  X(0001).
           05  FILLER REDEFINES YR3M02F.
               10  YR3M02A PIC  X(0001).
           05  YR3M02I PIC  X(0008).
      *    -------------------------------
           05  YR3M03L PIC S9(0004) COMP.
           05  YR3M03F PIC  X(0001).
           05  FILLER REDEFINES YR3M03F.
               10  YR3M03A PIC  X(0001).
           05  YR3M03I PIC  X(0008).
      *    -------------------------------
           05  YR3M04L PIC S9(0004) COMP.
           05  YR3M04F PIC  X(0001).
           05  FILLER REDEFINES YR3M04F.
               10  YR3M04A PIC  X(0001).
           05  YR3M04I PIC  X(0008).
      *    -------------------------------
           05  YR3M05L PIC S9(0004) COMP.
           05  YR3M05F PIC  X(0001).
           05  FILLER REDEFINES YR3M05F.
               10  YR3M05A PIC  X(0001).
           05  YR3M05I PIC  X(0008).
      *    -------------------------------
           05  YR3M06L PIC S9(0004) COMP.
           05  YR3M06F PIC  X(0001).
           05  FILLER REDEFINES YR3M06F.
               10  YR3M06A PIC  X(0001).
           05  YR3M06I PIC  X(0008).
      *    -------------------------------
           05  YR3AL PIC S9(0004) COMP.
           05  YR3AF PIC  X(0001).
           05  FILLER REDEFINES YR3AF.
               10  YR3AA PIC  X(0001).
           05  YR3AI PIC  9(2).
      *    -------------------------------
           05  M03AL PIC S9(0004) COMP.
           05  M03AF PIC  X(0001).
           05  FILLER REDEFINES M03AF.
               10  M03AA PIC  X(0001).
           05  M03AI PIC  X(0007).
      *    -------------------------------
           05  YR3M07L PIC S9(0004) COMP.
           05  YR3M07F PIC  X(0001).
           05  FILLER REDEFINES YR3M07F.
               10  YR3M07A PIC  X(0001).
           05  YR3M07I PIC  X(0008).
      *    -------------------------------
           05  YR3M08L PIC S9(0004) COMP.
           05  YR3M08F PIC  X(0001).
           05  FILLER REDEFINES YR3M08F.
               10  YR3M08A PIC  X(0001).
           05  YR3M08I PIC  X(0008).
      *    -------------------------------
           05  YR3M09L PIC S9(0004) COMP.
           05  YR3M09F PIC  X(0001).
           05  FILLER REDEFINES YR3M09F.
               10  YR3M09A PIC  X(0001).
           05  YR3M09I PIC  X(0008).
      *    -------------------------------
           05  YR3M10L PIC S9(0004) COMP.
           05  YR3M10F PIC  X(0001).
           05  FILLER REDEFINES YR3M10F.
               10  YR3M10A PIC  X(0001).
           05  YR3M10I PIC  X(0008).
      *    -------------------------------
           05  YR3M11L PIC S9(0004) COMP.
           05  YR3M11F PIC  X(0001).
           05  FILLER REDEFINES YR3M11F.
               10  YR3M11A PIC  X(0001).
           05  YR3M11I PIC  X(0008).
      *    -------------------------------
           05  YR3M12L PIC S9(0004) COMP.
           05  YR3M12F PIC  X(0001).
           05  FILLER REDEFINES YR3M12F.
               10  YR3M12A PIC  X(0001).
           05  YR3M12I PIC  X(0008).
      *    -------------------------------
           05  YR4L PIC S9(0004) COMP.
           05  YR4F PIC  X(0001).
           05  FILLER REDEFINES YR4F.
               10  YR4A PIC  X(0001).
           05  YR4I PIC  9(2).
      *    -------------------------------
           05  M04L PIC S9(0004) COMP.
           05  M04F PIC  X(0001).
           05  FILLER REDEFINES M04F.
               10  M04A PIC  X(0001).
           05  M04I PIC  X(0007).
      *    -------------------------------
           05  YR4M01L PIC S9(0004) COMP.
           05  YR4M01F PIC  X(0001).
           05  FILLER REDEFINES YR4M01F.
               10  YR4M01A PIC  X(0001).
           05  YR4M01I PIC  X(0008).
      *    -------------------------------
           05  YR4M02L PIC S9(0004) COMP.
           05  YR4M02F PIC  X(0001).
           05  FILLER REDEFINES YR4M02F.
               10  YR4M02A PIC  X(0001).
           05  YR4M02I PIC  X(0008).
      *    -------------------------------
           05  YR4M03L PIC S9(0004) COMP.
           05  YR4M03F PIC  X(0001).
           05  FILLER REDEFINES YR4M03F.
               10  YR4M03A PIC  X(0001).
           05  YR4M03I PIC  X(0008).
      *    -------------------------------
           05  YR4M04L PIC S9(0004) COMP.
           05  YR4M04F PIC  X(0001).
           05  FILLER REDEFINES YR4M04F.
               10  YR4M04A PIC  X(0001).
           05  YR4M04I PIC  X(0008).
      *    -------------------------------
           05  YR4M05L PIC S9(0004) COMP.
           05  YR4M05F PIC  X(0001).
           05  FILLER REDEFINES YR4M05F.
               10  YR4M05A PIC  X(0001).
           05  YR4M05I PIC  X(0008).
      *    -------------------------------
           05  YR4M06L PIC S9(0004) COMP.
           05  YR4M06F PIC  X(0001).
           05  FILLER REDEFINES YR4M06F.
               10  YR4M06A PIC  X(0001).
           05  YR4M06I PIC  X(0008).
      *    -------------------------------
           05  YR4AL PIC S9(0004) COMP.
           05  YR4AF PIC  X(0001).
           05  FILLER REDEFINES YR4AF.
               10  YR4AA PIC  X(0001).
           05  YR4AI PIC  9(2).
      *    -------------------------------
           05  M04AL PIC S9(0004) COMP.
           05  M04AF PIC  X(0001).
           05  FILLER REDEFINES M04AF.
               10  M04AA PIC  X(0001).
           05  M04AI PIC  X(0007).
      *    -------------------------------
           05  YR4M07L PIC S9(0004) COMP.
           05  YR4M07F PIC  X(0001).
           05  FILLER REDEFINES YR4M07F.
               10  YR4M07A PIC  X(0001).
           05  YR4M07I PIC  X(0008).
      *    -------------------------------
           05  YR4M08L PIC S9(0004) COMP.
           05  YR4M08F PIC  X(0001).
           05  FILLER REDEFINES YR4M08F.
               10  YR4M08A PIC  X(0001).
           05  YR4M08I PIC  X(0008).
      *    -------------------------------
           05  YR4M09L PIC S9(0004) COMP.
           05  YR4M09F PIC  X(0001).
           05  FILLER REDEFINES YR4M09F.
               10  YR4M09A PIC  X(0001).
           05  YR4M09I PIC  X(0008).
      *    -------------------------------
           05  YR4M10L PIC S9(0004) COMP.
           05  YR4M10F PIC  X(0001).
           05  FILLER REDEFINES YR4M10F.
               10  YR4M10A PIC  X(0001).
           05  YR4M10I PIC  X(0008).
      *    -------------------------------
           05  YR4M11L PIC S9(0004) COMP.
           05  YR4M11F PIC  X(0001).
           05  FILLER REDEFINES YR4M11F.
               10  YR4M11A PIC  X(0001).
           05  YR4M11I PIC  X(0008).
      *    -------------------------------
           05  YR4M12L PIC S9(0004) COMP.
           05  YR4M12F PIC  X(0001).
           05  FILLER REDEFINES YR4M12F.
               10  YR4M12A PIC  X(0001).
           05  YR4M12I PIC  X(0008).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0072).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL6562AO REDEFINES EL6562AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLASSO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEVO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLANO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTAGEO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPIREO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMMENTO PIC  X(0050).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M01O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M01O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M02O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M03O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M04O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M05O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M06O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M01AO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M07O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M08O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M09O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M10O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M11O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1M12O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M02O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M01O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M02O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M03O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M04O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M05O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M06O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M02AO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M07O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M08O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M09O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M10O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M11O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2M12O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M03O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M01O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M02O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M03O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M04O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M05O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M06O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M03AO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M07O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M08O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M09O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M10O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M11O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3M12O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M04O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M01O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M02O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M03O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M04O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M05O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M06O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M04AO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M07O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M08O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M09O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M10O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M11O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4M12O PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
00186
00187  01  EL6562AO-R   REDEFINES EL6562AI.
00188      05  FILLER             PIC X(123).
00189      05  SCREEN-TABLE       OCCURS 8 TIMES
00190                             INDEXED BY ST-INDX.
00191          10  YEAR-L         PIC S9(4)         COMP.
00192          10  YEAR-A         PIC X.
00193          10  YEAR           PIC 99.
00194          10  YEAR-1         REDEFINES
00195              YEAR           PIC XX.
00196          10  MONTH-L        PIC S9(4)         COMP.
00197          10  MONTH-A        PIC X.
00198          10  MONTH          PIC X(7).
00199
00200          10  RATE-TABLE     OCCURS 6 TIMES
00201                             INDEXED BY RT-INDX.
00202
00203              15  RATE-L     PIC S9(4)         COMP.
00204              15  RATE-A     PIC X.
00205              15  RATE       PIC S9(8).
00206              15  RATE1      REDEFINES
00207                  RATE       PIC ZZ.99999.
00208              15  RATE2      REDEFINES
00209                  RATE       PIC X(8).
00210
00211      05  FILLER             PIC X(5).
00212      05  FILLER             PIC X(75).
00213
00214      EJECT
00215
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
00217
00218  01  DFHCOMMAREA             PIC X(1024).
00219
00220      EJECT
00221 *                            COPY ERCRATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCRATE                             *
00003 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00004 *                            VMOD=2.008                          *
00005 *                                                                *
00006 *   ONLINE CREDIT SYSTEM                                         *
00007 *                                                                *
00008 *   FILE DESCRIPTION = RATES MASTER FILE                         *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 1765  RECFORM = FIXED                          *
00012 *                                                                *
00013 *   BASE CLUSTER NAME = ERRATE                   RKP=2,LEN=28    *
00014 *       ALTERNATE PATH = NONE                                    *
00015 *                                                                *
00016 *   LOG = NO                                                     *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 *                                                                *
00019 ******************************************************************
00020
00021  01  RATE-RECORD.
00022      12  RT-RECORD-ID                      PIC XX.
00023          88  VALID-RT-ID                      VALUE 'RT'.
00024
00025      12  RT-CONTROL-PRIMARY.
00026          16  RT-COMPANY-CD                 PIC X.
00027          16  RT-STATE-CODE.
00028              20  RT-ST-CODE                PIC XX.
00029              20  RT-ST-CLASS               PIC XX.
00030              20  RT-ST-DEV                 PIC XXX.
00031          16  RT-L-AH-CODE.
00032              20  RT-L-AH                   PIC X.
00033              20  RT-LAH-NUM                PIC XX.
00034          16  RT-LIMITS.
00035              20  RT-HIGH-AGE               PIC 99.
00036              20  RT-HIGH-AMT               PIC 9(6).
00037              20  RT-FUTURE                 PIC XX.
00038              20  RT-SEX                    PIC X.
00039          16  RT-EXPIRY-DATE                PIC 9(11)  COMP-3.
00043
00044      12  RT-MAINT-INFORMATION.
00045          16  RT-LAST-MAINT-DT              PIC XX.
00046          16  RT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00047          16  RT-LAST-MAINT-USER            PIC X(4).
00048          16  FILLER                        PIC X(10).
00049
00050      12  RT-STRUCTURE-COMMENT              PIC X(50).
00051      12  RT-RATE-COMMENT                   PIC X(50).
00052
00053      12  CSL-RESERVED                      PIC X(10).
00054      12  FILLER                            PIC X(12).
00055
00056      12  RT-MAX-AGE                        PIC 99.
00057
00058      12  RT-LIFE-LIMS-FLDS.
00059          16  RT-LIFE-MORT-CODE             PIC X(4).
00060          16  RT-LIFE-EXCEPTIONS   OCCURS 8 TIMES.
00061              20  RT-L-EX-AGE               PIC 99.
00062              20  RT-L-EX-TERM              PIC S999       COMP-3.
00063              20  RT-L-EX-FACE              PIC S9(7)      COMP-3.
00064          16  FILLER                        PIC X(20).
00065
00066      12  RT-AH-LIMS-FLDS   REDEFINES   RT-LIFE-LIMS-FLDS.
00067          16  RT-AH-EXCEPTIONS   OCCURS 8 TIMES.
00068              20  RT-AH-AGE                 PIC 99.
00069              20  RT-AH-TERM                PIC S999       COMP-3.
00070              20  RT-AH-BEN-M               PIC S9(5)      COMP-3.
00071              20  RT-AH-BEN-F               PIC S9(7)      COMP-3.
00072
00073      12  RT-LIFE-RATES.
00074          16  RT-L-RATE  OCCURS 360 TIMES   PIC S99V9(5)   COMP-3.
00075
00076      12  RT-AH-RATES   REDEFINES   RT-LIFE-RATES.
00077          16  RT-AH-RATE  OCCURS 360 TIMES  PIC S99V9(5)   COMP-3.
00078
00079      12  RT-DAILY-RATE                     PIC S99V9(5)   COMP-3.
00080
00081      12  RT-DISCOUNT-OPTION                PIC X.
00082          88  RT-DO-NOT-USE                     VALUE ' '.
00083          88  RT-USE-DISCOUNT-FACTOR            VALUE '1'.
00084          88  RT-USE-APR-AS-DISCOUNT            VALUE '2'.
00085
00086      12  RT-DISCOUNT-RATE                  PIC S99V9(5)   COMP-3.
00087      12  RT-DISCOUNT-OB-RATE               PIC S99V9(5)   COMP-3.
00088
00089      12  RT-COMPOSITE-OPTION               PIC X.
00090          88  RT-NO-COMPOSITE                   VALUE ' '.
00091          88  RT-USE-COMPOSITE-RATE             VALUE '1'.
00092
00093      12  RT-COMPOSITE-RATE                 PIC S99V9(5)   COMP-3.
00094
00095      12  RT-POLICY-FEE                     PIC S9(3)V99   COMP-3.
00096      12  FILLER                            PIC X(13).
00097
00098      12  RT-TYPE-RATE                      PIC X.
00099          88  RT-IS-STND                        VALUE ' ' 'S'.
00100          88  RT-IS-OB                          VALUE 'O'.
00101
00102      12  RT-SRT-ALPHA                      PIC X.
00103
00104      12  RT-CONTROL-2.
00105          16  RTC-1                         PIC X(7).
00106          16  RTC-3                         PIC X(11).
00107          16  RTC-4                         PIC 9(11) COMP-3.
00108          16  RTC-2                         PIC X(3).
00109 ******************************************************************
00222      EJECT
00223 *                            COPY ELCCNTL SUPPRESS.
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
032813         16  FILLER                         PIC X(79).
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
00224      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA RATE-RECORD
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6562' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00226
00227      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00228      MOVE '5'                   TO DC-OPTION-CODE.
00229      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00230      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00231      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00232
00233      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00234
00235  0100-START.
00236      IF EIBCALEN = 0
00237          GO TO 8800-UNAUTHORIZED-ACCESS.
00238
00239      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00240          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00241              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00242              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00243              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00244              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00245              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00246              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00247              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00248              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00249          ELSE
00250              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00251              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00252              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00253              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00254              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00255              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00256              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00257              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00258
00259      
      * EXEC CICS HANDLE CONDITION
00260 *        NOTOPEN  (9990-ABEND)
00261 *        NOTFND   (8880-NOT-FOUND)
00262 *        PGMIDERR (9600-PGMID-ERROR)
00263 *        ERROR    (9990-ABEND)
00264 *        END-EXEC.
      *    MOVE '"$JIL.                ! " #00003411' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00265
00266      IF PI-FILE-ID = 'OE'
00267         MOVE OE-RATE-FILE-ID TO RATE-FILE-ID.
00268
00269      IF EIBTRNID NOT = TRANS-ID
00270          MOVE LOW-VALUES TO EL6562AI
00271          MOVE ZEROS      TO MISC-SAVE-AREAS
00272          MOVE +1         TO PI-SUB
00273                             PI-YEAR
00274          GO TO 3000-BUILD-INITIAL-SCREEN.
00275
00276      IF EIBAID = DFHCLEAR
00277          GO TO 9400-CLEAR.
00278
00279      EJECT
00280
00281  0200-RECEIVE.
00282      MOVE LOW-VALUES TO EL6562AI.
00283
00284      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00285          MOVE ER-0008 TO EMI-ERROR
00286          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00287          MOVE -1   TO COMMENTL
00288          GO TO 8200-SEND-DATAONLY.
00289
00290      
      * EXEC CICS RECEIVE
00291 *        MAP    (WS-MAPNAME)
00292 *        MAPSET (WS-MAPSET-NAME)
00293 *        INTO   (EL6562AI)
00294 *        END-EXEC.
           MOVE LENGTH OF
            EL6562AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003442' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAPNAME, 
                 EL6562AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00295
00296      IF PFENTERL = 0
00297          GO TO 0300-CHECK-PFENTERS.
00298
00299      IF EIBAID NOT = DFHENTER
00300          MOVE ER-0004 TO EMI-ERROR
00301          GO TO 0310-INPUT-ERROR.
00302
00303      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
00304          MOVE PF-VALUES (PFENTERI) TO EIBAID
00305          GO TO 0300-CHECK-PFENTERS
00306      ELSE
00307          MOVE ER-0029 TO EMI-ERROR
00308          GO TO 0310-INPUT-ERROR.
00309
00310      EJECT
00311
00312  0300-CHECK-PFENTERS.
00313      IF EIBAID = DFHPF23
00314          GO TO 8810-PF23.
00315
00316      IF EIBAID = DFHPF24
00317          GO TO 9200-RETURN-MAIN-MENU.
00318
00319      IF EIBAID = DFHPF12
00320          GO TO 9500-PF12.
00321
00322      IF EIBAID = DFHPF1
00323          MOVE ZEROS TO MISC-SAVE-AREAS
00324          ADD +48    TO PI-SUB
00325          ADD +4     TO PI-YEAR
00326          GO TO 3000-BUILD-INITIAL-SCREEN.
00327
00328      IF EIBAID = DFHPF2
00329          MOVE ZEROS TO MISC-SAVE-AREAS
00330          SUBTRACT +48 FROM PI-SUB
00331          SUBTRACT +4  FROM PI-YEAR
00332          GO TO 3000-BUILD-INITIAL-SCREEN.
00333
00334      IF EIBAID = DFHPF3
00335          GO TO 7100-NEXT-PLAN.
00336
00337      IF EIBAID = DFHPF5
00338          MOVE XCTL-6561 TO PGM-NAME
00339          GO TO 9300-XCTL.
00340
00341      IF EIBAID = DFHPF7
00342          IF NOT MODIFY-CAP
00343              MOVE 'UPDATE'       TO SM-READ
00344              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00345              MOVE ER-0070 TO EMI-ERROR
00346              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00347              GO TO 8200-SEND-DATAONLY
00348          ELSE
00349              MOVE XCTL-6563 TO PGM-NAME
00350              GO TO 9300-XCTL.
00351
00352      IF EIBAID = DFHPF8
00353          IF NOT MODIFY-CAP
00354              MOVE 'UPDATE'       TO SM-READ
00355              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00356              MOVE ER-0070 TO EMI-ERROR
00357              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00358              GO TO 8200-SEND-DATAONLY
00359          ELSE
00360              MOVE XCTL-6564 TO PGM-NAME
00361              GO TO 9300-XCTL.
00362
00363      IF EIBAID = DFHENTER
00364          GO TO 1000-CHANGE.
00365
00366      MOVE ER-0029 TO EMI-ERROR.
00367
00368  0310-INPUT-ERROR.
00369      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00370      MOVE AL-UNBON TO PFENTERA.
00371      MOVE -1 TO PFENTERL.
00372
00373      GO TO 8200-SEND-DATAONLY.
00374
00375      EJECT
00376
00377  1000-CHANGE.
00378      IF NOT MODIFY-CAP
00379          MOVE 'UPDATE'       TO SM-READ
00380          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00381          MOVE ER-0070        TO  EMI-ERROR
00382          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00383          GO TO 8100-SEND-INITIAL-MAP.
00384
00385      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY
00386          NEXT SENTENCE
00387      ELSE
00388          MOVE ER-2056  TO EMI-ERROR
00389          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00390          MOVE -1    TO COMMENTL
00391          GO TO 8200-SEND-DATAONLY.
00392
00393      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.
00394
00395      PERFORM 5000-EDIT THRU 5099-EXIT.
00396
00397      IF EMI-NO-ERRORS
00398          NEXT SENTENCE
00399      ELSE
00400          GO TO 8200-SEND-DATAONLY.
00401
00402      PERFORM 7750-READ-ERRATE-UPDATE THRU 7750-EXIT.
00403
00404      MOVE RATE-RECORD TO JP-RECORD-AREA.
00405
00406      PERFORM 4000-UPDATE THRU 4099-EXIT.
00407
00408      IF RT-LAST-MAINT-USER   = PI-UPDATE-BY OR
00409         RT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00410          NEXT SENTENCE
00411      ELSE
00412          
      * EXEC CICS UNLOCK
00413 *             DATASET  (RATE-FILE-ID)
00414 *        END-EXEC
      *    MOVE '&*                    #   #00003564' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00415          MOVE ER-0068 TO EMI-ERROR
00416          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00417          GO TO 3000-BUILD-INITIAL-SCREEN.
00418
00419      MOVE PI-PROCESSOR-ID     TO RT-LAST-MAINT-USER.
00420      MOVE EIBTIME             TO RT-LAST-MAINT-HHMMSS.
00421
00422      MOVE SAVE-BIN-DATE       TO RT-LAST-MAINT-DT
00423                                  BIN-CURRENT-SAVE.
00424      MOVE 'B'                 TO JP-RECORD-TYPE.
00425      MOVE RATE-FILE-ID        TO FILE-ID.
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00427
00428      
      * EXEC CICS REWRITE
00429 *        DATASET  (RATE-FILE-ID)
00430 *        FROM     (RATE-RECORD)
00431 *    END-EXEC.
           MOVE LENGTH OF
            RATE-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003581' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 RATE-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00432
00433      MOVE RATE-RECORD         TO JP-RECORD-AREA.
00434      MOVE 'C'                 TO JP-RECORD-TYPE
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00436      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00437      MOVE ER-0000 TO EMI-ERROR.
00438      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00439
00440  1299-EXIT.
00441      EXIT.
00442      EJECT
00443
00444  3000-BUILD-INITIAL-SCREEN.
00445      IF PI-SUB GREATER +360 OR
00446         PI-SUB LESS THAN +0
00447             MOVE +1 TO PI-SUB
00448                        PI-YEAR.
00449
00450      MOVE LOW-VALUES                    TO  EL6562AO.
00451      MOVE ZEROS                         TO  MISC-SAVE-AREAS.
00452      MOVE PI-COMPANY-CD                 TO  PI-RATE-COMPANY-CD.
00453      MOVE PI-SUB                        TO  SUB1.
00454      MOVE PI-YEAR                       TO  SUB4.
00455      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.
00456
00457      MOVE PI-ERRATE-KEY                 TO PI-SAVE-ERRATE-KEY.
00458
00459  3025-SET-UP-SCREEN.
00460      MOVE RT-ST-CODE                    TO STATEO.
00461      MOVE RT-ST-CLASS                   TO CLASSO.
00462      MOVE RT-ST-DEV                     TO DEVO.
00463
00464 ***  Y2K PROJ 7744
00465      MOVE RT-EXPIRY-DATE                TO WS-HOLD-RATE-EXP-DT
00466      MOVE WS-HOLD-RATE-EXP-AL(6:6)      TO EXPIREO
00467 ***  Y2K PROJ 7744
00468
00469      MOVE RT-L-AH                       TO TYPEO.
00470      MOVE RT-LAH-NUM                    TO PLANO.
00471      MOVE RT-RATE-COMMENT               TO COMMENTO.
00472      MOVE AL-UANON                      TO COMMENTA.
00473      MOVE RT-HIGH-AGE                   TO RTAGEO.
00474
00475      MOVE +1     TO SUB2
00476                     SUB3.
00477      SET ST-INDX TO +1.
00478      SET RT-INDX TO +1.
00479
00480  3050-SET-UP.
00481      IF ST-INDX GREATER +8
00482          MOVE +1     TO SUB1
00483                         SUB2
00484                         SUB3
00485                         SUB4
00486          SET ST-INDX TO +1
00487          SET RT-INDX TO +1
00488          GO TO 8100-SEND-INITIAL-MAP.
00489
00490      MOVE SUB4 TO YEAR (ST-INDX).
00491
00492      IF SUB4 GREATER +30
00493          MOVE SPACES       TO MONTH    (ST-INDX)
00494                               YEAR-1   (ST-INDX)
00495          MOVE AL-SADOF     TO MONTH-A  (ST-INDX)
00496                               YEAR-A   (ST-INDX)
00497      ELSE
00498          IF EVEN-SUB
00499              COMPUTE WS-FROM = (SUB4 * 12) - 5
00500              COMPUTE WS-TO   = WS-FROM + 5
00501              ADD +1 TO SUB4
00502              MOVE WS-TERM  TO MONTH   (ST-INDX)
00503              MOVE AL-SADOF TO YEAR-A  (ST-INDX)
00504              MOVE AL-SANON TO MONTH-A (ST-INDX)
00505          ELSE
00506              COMPUTE WS-FROM = (SUB4 * 12) - 11
00507              COMPUTE WS-TO   = WS-FROM + 5
00508              MOVE WS-TERM  TO MONTH   (ST-INDX)
00509              MOVE AL-SANON TO YEAR-A  (ST-INDX)
00510                               MONTH-A (ST-INDX).
00511
00512  3075-SET-UP-RATES.
00513      IF SUB1 GREATER +360
00514          MOVE SPACES             TO RATE2 (ST-INDX RT-INDX)
00515          MOVE AL-SADOF           TO RATE-A (ST-INDX RT-INDX)
00516      ELSE
00517          MOVE AL-UNNON           TO RATE-A (ST-INDX RT-INDX)
00518          IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1
00519              MOVE RT-AH-RATE (SUB1) TO RATE1 (ST-INDX RT-INDX)
00520                                        WS-SAVE-RATE (SUB2)
00521          ELSE
00522              IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1
00523                  MOVE RT-L-RATE (SUB1)  TO RATE1 (ST-INDX RT-INDX)
00524                                            WS-SAVE-RATE (SUB2).
00525
00526
00527      ADD +1 TO SUB1
00528                SUB2.
00529      SET RT-INDX UP BY +1.
00530
00531      IF RT-INDX GREATER +6
00532          SET RT-INDX TO +1
00533          SET ST-INDX UP BY +1
00534          ADD +1  TO  SUB3
00535          GO TO 3050-SET-UP
00536      ELSE
00537          GO TO 3075-SET-UP-RATES.
00538
00539
00540  3099-EXIT.
00541      EXIT.
00542      EJECT
00543
00544  4000-UPDATE.
00545       IF COMMENTL GREATER ZERO
00546           MOVE COMMENTI TO RT-RATE-COMMENT.
00547
00548       MOVE +1     TO SUB1.
00549       MOVE PI-SUB TO SUB2.
00550
00551  4025-CONT.
00552      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1
00553          MOVE WS-SAVE-RATE (SUB1) TO RT-L-RATE (SUB2)
00554      ELSE
00555          MOVE WS-SAVE-RATE (SUB1) TO RT-AH-RATE (SUB2).
00556
00557      ADD +1 TO SUB1
00558                SUB2.
00559
00560      IF SUB1 GREATER +48 OR SUB2 GREATER +360
00561         NEXT SENTENCE
00562      ELSE
00563          GO TO 4025-CONT.
00564
00565  4099-EXIT.
00566      EXIT.
00567      EJECT
00568
00569  5000-EDIT.
00570       SET ST-INDX TO +1.
00571       SET RT-INDX TO +1.
00572       MOVE +1 TO SUB1.
00573
00574  5050-CONT.
00575      IF ST-INDX GREATER +8
00576          SET ST-INDX TO +1
00577          SET RT-INDX TO +1
00578          MOVE +1 TO SUB1
00579          GO TO 5099-EXIT.
00580
00581      IF RATE-L (ST-INDX RT-INDX) GREATER ZERO
00582          MOVE RATE (ST-INDX RT-INDX) TO DEEDIT-FIELD
00583          PERFORM 7500-DEEDIT THRU 7500-EXIT
00584          IF DEEDIT-FIELD-V0 NUMERIC
00585              MOVE DEEDIT-FIELD-V1 TO WS-SAVE-RATE (SUB1)
00586                                      RATE1 (ST-INDX RT-INDX)
00587              MOVE AL-UNNON        TO RATE-A (ST-INDX RT-INDX)
00588          ELSE
00589              MOVE -1       TO RATE-L (ST-INDX RT-INDX)
00590              MOVE AL-UNBON TO RATE-A (ST-INDX RT-INDX)
00591              MOVE ER-2280  TO EMI-ERROR
00592              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00593      ELSE
00594          MOVE ZEROS        TO RATE1 (ST-INDX RT-INDX)
00595                               WS-SAVE-RATE (SUB1)
00596          MOVE AL-UNNON     TO RATE-A (ST-INDX RT-INDX).
00597
00598      SET RT-INDX UP BY +1.
00599      ADD +1 TO SUB1.
00600
00601      IF RT-INDX GREATER +6
00602          SET ST-INDX UP BY +1
00603          SET RT-INDX TO +1.
00604
00605      GO TO 5050-CONT.
00606
00607  5099-EXIT.
00608      EXIT.
00609      EJECT
00610
00611  7100-NEXT-PLAN.
00612      MOVE SPACES             TO PI-ERRATE-EOF-SW.
00613
00614      MOVE PI-COMPANY-CD      TO PI-RATE-COMPANY-CD.
00615      MOVE PI-ERRATE-KEY      TO PI-SAVE-ERRATE-KEY.
00616      MOVE PI-RATE-STATE-CODE TO WS-SAVE-STRUCTURE.
00617
00618      PERFORM 7800-START-BROWSE THRU 7800-EXIT.
00619
00620  7125-READ-NEXT.
00621      
      * EXEC CICS HANDLE CONDITION
00622 *        ENDFILE  (7150-ENDFILE)
00623 *        NOTFND   (7175-NOTFOUND)
00624 *    END-EXEC.
      *    MOVE '"$''I                  ! # #00003775' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033373735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00625
00626      PERFORM 7850-READNEXT THRU 7850-EXIT.
00627
00628      IF ERRATE-EOF
00629          IF BROWSE-STARTED
00630              PERFORM 7950-END-BROWSE THRU 7950-EXIT
00631              MOVE LOW-VALUES TO PI-ERRATE-KEY
00632              GO TO 7100-NEXT-PLAN.
00633
00634      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY
00635          GO TO 7125-READ-NEXT.
00636
00637      IF WS-SAVE-STRUCTURE = LOW-VALUES
00638          MOVE PI-RATE-STATE-CODE TO WS-SAVE-STRUCTURE.
00639
00640      IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE
00641          NEXT SENTENCE
00642      ELSE
00643          PERFORM 7950-END-BROWSE THRU 7950-EXIT
00644          MOVE PI-SAVE-ERRATE-KEY TO PI-ERRATE-KEY
00645          MOVE LOW-VALUES         TO PI-RATE-L-AH-CODE
00646          MOVE ZEROS              TO PI-RATE-EXPIRY-DATE
00647          MOVE ER-2285            TO EMI-ERROR
00648          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00649          GO TO 7100-NEXT-PLAN.
00650
00651  7130-DISPLAY-SCREEN.
00652      IF BROWSE-STARTED
00653          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
00654
00655      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.
00656      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
00657      MOVE PI-ERRATE-KEY          TO PI-SAVE-ERRATE-KEY.
00658      MOVE +1                     TO PI-SUB  PI-YEAR.
00659
00660      GO TO 3000-BUILD-INITIAL-SCREEN.
00661
00662  7150-ENDFILE.
00663      IF BROWSE-STARTED
00664          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
00665
00666      MOVE ER-2271 TO EMI-ERROR.
00667      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00668      MOVE LOW-VALUES TO EL6562AO
00669                         PI-ERRATE-KEY.
00670      GO TO 7100-NEXT-PLAN.
00671
00672  7175-NOTFOUND.
00673      IF BROWSE-STARTED
00674          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
00675
00676      GO TO 8880-NOT-FOUND.
00677
00678  7199-EXIT.
00679      EXIT.
00680      EJECT
00681
00682  7500-DEEDIT.
00683      
      * EXEC CICS BIF
00684 *         DEEDIT
00685 *         FIELD  (DEEDIT-FIELD)
00686 *         LENGTH (15)
00687 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003837' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00688
00689  7500-EXIT.
00690      EXIT.
00691      EJECT
00692
00693  7650-READ-ERRATE.
00694      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.
00695
00696      
      * EXEC CICS READ
00697 *         DATASET  (RATE-FILE-ID)
00698 *         SET      (ADDRESS OF RATE-RECORD)
00699 *         RIDFLD   (PI-ERRATE-KEY)
00700 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003850' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00701
00702      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.
00703      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
00704
00705  7650-EXIT.
00706      EXIT.
00707      EJECT
00708
00709  7750-READ-ERRATE-UPDATE.
00710      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.
00711
00712      
      * EXEC CICS READ
00713 *         DATASET  (RATE-FILE-ID)
00714 *         SET      (ADDRESS OF RATE-RECORD)
00715 *         RIDFLD   (PI-ERRATE-KEY)
00716 *         UPDATE
00717 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003866' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00718
00719  7750-EXIT.
00720      EXIT.
00721      EJECT
00722
00723  7800-START-BROWSE.
00724      
      * EXEC CICS STARTBR
00725 *         DATASET  (RATE-FILE-ID)
00726 *         RIDFLD   (PI-ERRATE-KEY)
00727 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003878' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00728
00729      MOVE 'Y' TO PI-BROWSE-SW.
00730
00731  7800-EXIT.
00732      EXIT.
00733      EJECT
00734
00735  7850-READNEXT.
00736      
      * EXEC CICS READNEXT
00737 *         DATASET  (RATE-FILE-ID)
00738 *         SET      (ADDRESS OF RATE-RECORD)
00739 *         RIDFLD   (PI-ERRATE-KEY)
00740 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003890' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00741
00742      IF PI-COMPANY-CD NOT = RT-COMPANY-CD
00743          MOVE LOW-VALUES TO EL6562AO
00744          MOVE ER-2271    TO EMI-ERROR
00745          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00746          MOVE 'Y'        TO PI-ERRATE-EOF-SW.
00747
00748  7850-EXIT.
00749      EXIT.
00750      EJECT
00751
00752  7950-END-BROWSE.
00753      
      * EXEC CICS ENDBR
00754 *         DATASET  (RATE-FILE-ID)
00755 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003907' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00756
00757      MOVE SPACE TO PI-BROWSE-SW.
00758
00759  7950-EXIT.
00760      EXIT.
00761      EJECT
00762
00763  8000-UPDATE-MAINT-DATE.
00764      MOVE SPACES                 TO ELCNTL-KEY.
00765
00766      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
00767      MOVE '1'                    TO CNTL-REC-TYPE.
00768      MOVE +0                     TO CNTL-SEQ-NO.
00769
00770      
      * EXEC CICS HANDLE CONDITION
00771 *        NOTFND   (8000-EXIT)
00772 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00003924' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033393234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00773
00774      
      * EXEC CICS READ
00775 *        UPDATE
00776 *        DATASET   (CNTL-FILE-ID)
00777 *        SET       (ADDRESS OF CONTROL-FILE)
00778 *        RIDFLD    (ELCNTL-KEY)
00779 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00003928' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00780
00781      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00782      MOVE 'B'                    TO JP-RECORD-TYPE.
00783      MOVE CNTL-FILE-ID           TO FILE-ID.
00784 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00785
00786      MOVE BIN-CURRENT-SAVE       TO CF-RATES-FILE-MAINT-DT.
00787
00788      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
00789      MOVE 'C'                    TO JP-RECORD-TYPE.
00790      MOVE CNTL-FILE-ID           TO FILE-ID.
00791
00792      
      * EXEC CICS REWRITE
00793 *        DATASET   (CNTL-FILE-ID)
00794 *        FROM      (CONTROL-FILE)
00795 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003946' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00796
00797 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00798
00799  8000-EXIT.
00800       EXIT.
00801      EJECT
00802
00803  8100-SEND-INITIAL-MAP.
00804      MOVE EIBTIME              TO TIME-IN.
00805      MOVE SAVE-DATE            TO RUNDATEO.
00806      MOVE TIME-OUT             TO RUNTIMEO.
00807      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
00808      MOVE -1 TO COMMENTL.
00809      
      * EXEC CICS SEND
00810 *        MAP   (WS-MAPNAME)
00811 *        MAPSET(WS-MAPSET-NAME)
00812 *        FROM  (EL6562AO)
00813 *        ERASE
00814 *        CURSOR
00815 *    END-EXEC.
           MOVE LENGTH OF
            EL6562AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003963' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAPNAME, 
                 EL6562AO, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00816
00817      GO TO 9100-RETURN-TRAN.
00818
00819  8200-SEND-DATAONLY.
00820      MOVE EIBTIME              TO TIME-IN.
00821      MOVE SAVE-DATE            TO RUNDATEO.
00822      MOVE TIME-OUT             TO RUNTIMEO.
00823      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
00824      
      * EXEC CICS SEND
00825 *        MAP   (WS-MAPNAME)
00826 *        MAPSET(WS-MAPSET-NAME)
00827 *        FROM  (EL6562AO)
00828 *        DATAONLY
00829 *        CURSOR
00830 *    END-EXEC.
           MOVE LENGTH OF
            EL6562AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003978' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAPNAME, 
                 EL6562AO, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00831
00832      GO TO 9100-RETURN-TRAN.
00833
00834  8300-SEND-TEXT.
00835      
      * EXEC CICS SEND TEXT
00836 *        FROM  (LOGOFF-TEXT)
00837 *        LENGTH(LOGOFF-LENGTH)
00838 *        ERASE
00839 *        FREEKB
00840 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003989' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393839' TO DFHEIV0(25:11)
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
           
00841
00842      
      * EXEC CICS RETURN
00843 *    END-EXEC.
      *    MOVE '.(                    ''   #00003996' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00844
081413 8400-LOG-JOURNAL-RECORD.
081413
081413     if pi-journal-file-id = 0
081413        go to 8400-exit
081413     end-if
081413
081413     move eibdate                to jp-date
081413     move eibtime                to jp-time
081413     move RATE-FILE-ID           TO JP-FILE-ID
081413     MOVE PI-PROCESSOR-ID        TO JP-USER-ID
081413     MOVE 02                     TO PI-JOURNAL-FILE-ID
081413     MOVE THIS-PGM               TO JP-PROGRAM-ID
081413
081413     
      * EXEC CICS JOURNAL
081413*       JFILEID   (PI-JOURNAL-FILE-ID)
081413*       JTYPEID   ('EL')
081413*       FROM      (JOURNAL-RECORD)
081413*       LENGTH    (2000)
081413*       resp      (ws-response)
081413*    END-EXEC
           MOVE 'EL' TO DFHEIV7
           MOVE 2000
             TO DFHEIV11
      *    MOVE '4"LF                  (  N#00004012' TO DFHEIV0
           MOVE X'34224C462020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034303132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-JOURNAL-FILE-ID, 
                 DFHEIV7, 
                 JOURNAL-RECORD, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
081413
081413     if ws-resp-normal
081413        continue
081413     else
081413        display ' error-el656-journal ' ws-response
081413     end-if
081413
081413     .
081413 8400-exit.
081413     exit.
00858  8800-UNAUTHORIZED-ACCESS.
00859      MOVE UNACCESS-MSG TO LOGOFF-MSG.
00860      GO TO 8300-SEND-TEXT.
00861
00862  8810-PF23.
00863      MOVE EIBAID   TO PI-ENTRY-CD-1.
00864      MOVE XCTL-005 TO PGM-NAME.
00865      GO TO 9300-XCTL.
00866
00867  8880-NOT-FOUND.
00868      MOVE ER-0142 TO EMI-ERROR.
00869      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00870      MOVE -1   TO COMMENTL.
00871
00872      IF EIBTRNID NOT = TRANS-ID
00873          GO TO 8100-SEND-INITIAL-MAP.
00874
00875      GO TO 8200-SEND-DATAONLY.
00876
00877  9100-RETURN-TRAN.
00878      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
00879      MOVE '656C'               TO PI-CURRENT-SCREEN-NO.
00880      
      * EXEC CICS RETURN
00881 *        TRANSID (TRANS-ID)
00882 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
00883 *        LENGTH  (PI-COMM-LENGTH)
00884 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00004051' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00885
00886  9200-RETURN-MAIN-MENU.
00887      MOVE XCTL-626 TO PGM-NAME.
00888      GO TO 9300-XCTL.
00889
00890  9300-XCTL.
00891      
      * EXEC CICS XCTL
00892 *        PROGRAM (PGM-NAME)
00893 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
00894 *        LENGTH  (PI-COMM-LENGTH)
00895 *    END-EXEC.
      *    MOVE '.$C                   %   #00004062' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00896
00897  9400-CLEAR.
00898      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
00899      GO TO 9300-XCTL.
00900
00901  9500-PF12.
00902      MOVE XCTL-010 TO PGM-NAME.
00903      GO TO 9300-XCTL.
00904
00905  9600-PGMID-ERROR.
00906      
      * EXEC CICS HANDLE CONDITION
00907 *        PGMIDERR(8300-SEND-TEXT)
00908 *    END-EXEC.
      *    MOVE '"$L                   ! % #00004077' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034303737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00909
00910      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.
00911      MOVE ' '          TO PI-ENTRY-CD-1.
00912      MOVE XCTL-005     TO PGM-NAME.
00913      MOVE PGM-NAME     TO LOGOFF-PGM.
00914      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
00915      GO TO 9300-XCTL.
00916
00917  9700-LINK-DATE-CONVERT.
00918      MOVE LINK-ELDATCV TO PGM-NAME.
00919
00920      
      * EXEC CICS LINK
00921 *        PROGRAM (PGM-NAME)
00922 *        COMMAREA(DATE-CONVERSION-DATA)
00923 *        LENGTH  (DC-COMM-LENGTH)
00924 *    END-EXEC.
      *    MOVE '."C                   (   #00004091' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00925
00926  9700-EXIT.
00927      EXIT.
00928
00929  9900-ERROR-FORMAT.
00930      IF NOT EMI-ERRORS-COMPLETE
00931          MOVE LINK-001 TO PGM-NAME
00932          
      * EXEC CICS LINK
00933 *            PROGRAM (PGM-NAME)
00934 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
00935 *            LENGTH  (EMI-COMM-LENGTH)
00936 *        END-EXEC.
      *    MOVE '."C                   (   #00004103' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00937
00938  9900-EXIT.
00939      EXIT.
00940
00941  9990-ABEND.
00942      MOVE LINK-004 TO PGM-NAME.
00943      MOVE DFHEIBLK               TO EMI-LINE1
00944      
      * EXEC CICS LINK
00945 *        PROGRAM   (PGM-NAME)
00946 *        COMMAREA  (EMI-LINE1)
00947 *        LENGTH    (72)
00948 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004115' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00949
00950      GO TO 8200-SEND-DATAONLY.
00951
00952      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6562' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00953
00954  9995-SECURITY-VIOLATION.
00955 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00004143' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313433' TO DFHEIV0(25:11)
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
00956
00957  9995-EXIT.
00958       EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6562' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 7150-ENDFILE,
                     7175-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6562' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
