00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL631.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 04/20/94 16:44:20.
00007 *                            VMOD=2.021.
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
00023 *REMARKS.    TRANSACTION - EXB0 - NEW BUSINESS
00024 *                                 REVIEW AND CORRECTION
00025
020816******************************************************************
020816*                   C H A N G E   L O G
020816*
020816* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
020816*-----------------------------------------------------------------
020816*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
020816* EFFECTIVE    NUMBER
020816*-----------------------------------------------------------------
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
      ******************************************************************
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL631   WORKING STORAGE   *'.
00033  77  FILLER  PIC X(32)  VALUE '************VMOD=2.021 *********'.
00034
00035 *    COPY ELCSCTM.
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
00036 *    COPY ELCSCRTY.
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
00037
00038     EJECT
00039
00040  01  WS-DATE-AREA.
00041      05  SAVE-DATE               PIC X(8)    VALUE SPACES.
00042      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00043
00044  01  STANDARD-AREAS.
00045      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.
00046      12  MAP-NAME                PIC X(8)    VALUE 'EL631A'.
00047      12  MAPSET-NAME             PIC X(8)    VALUE 'EL631S'.
00048      12  SCREEN-NUMBER           PIC X(4)    VALUE '631A'.
00049      12  TRANS-ID                PIC X(4)    VALUE 'EXB0'.
00050      12  THIS-PGM                PIC X(8)    VALUE 'EL631'.
00051      12  PGM-NAME                PIC X(8).
00052      12  TIME-IN                 PIC S9(7).
00053      12  TIME-OUT-R  REDEFINES TIME-IN.
00054          16  FILLER              PIC X.
00055          16  TIME-OUT            PIC 99V99.
00056          16  FILLER              PIC XX.
00057      12  XCTL-005                PIC X(8)    VALUE 'EL005'.
00058      12  XCTL-010                PIC X(8)    VALUE 'EL010'.
00059      12  XCTL-626                PIC X(8)    VALUE 'EL626'.
00060      12  XCTL-6311               PIC X(8)    VALUE 'EL6311'.
00061      12  XCTL-630                PIC X(8)    VALUE 'EL630'.
00062      12  XCTL-656                PIC X(8)    VALUE 'EL656'.
00063      12  XCTL-663                PIC X(8)    VALUE 'EL663'.
00064      12  LINK-001                PIC X(8)    VALUE 'EL001'.
00065      12  LINK-004                PIC X(8)    VALUE 'EL004'.
00066      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00067      12  FILE-ID                 PIC X(8)    VALUE SPACES.
00068      12  ERPNDB-FILE-ID          PIC X(8)    VALUE 'ERPNDB'.
00069      12  ERPNDB3-FILE-ID         PIC X(8)    VALUE 'ERPNDB3'.
00070      12  ERACCT2-FILE-ID         PIC X(8)    VALUE 'ERACCT2'.
00071
00072      EJECT
00073
00074  01  ERROR-MESSAGES.
00075      12  ER-0004                 PIC X(4)  VALUE '0004'.
00076      12  ER-0008                 PIC X(4)  VALUE '0008'.
00077      12  ER-0029                 PIC X(4)  VALUE '0029'.
00078      12  ER-0033                 PIC X(4)  VALUE '0033'.
00079      12  ER-0070                 PIC X(4)  VALUE '0070'.
00080      12  ER-0144                 PIC X(4)  VALUE '0144'.
00081      12  ER-0179                 PIC X(4)  VALUE '0179'.
00082      12  ER-0193                 PIC X(4)  VALUE '0193'.
00083      12  ER-0195                 PIC X(4)  VALUE '0195'.
00084      12  ER-0329                 PIC X(4)  VALUE '0329'.
00085      12  ER-0348                 PIC X(4)  VALUE '0348'.
00086      12  ER-2059                 PIC X(4)  VALUE '2059'.
00087      12  ER-2150                 PIC X(4)  VALUE '2150'.
00088      12  ER-2190                 PIC X(4)  VALUE '2190'.
00089      12  ER-2191                 PIC X(4)  VALUE '2191'.
00090      12  ER-2192                 PIC X(4)  VALUE '2192'.
00091      12  ER-2193                 PIC X(4)  VALUE '2193'.
00092      12  ER-2201                 PIC X(4)  VALUE '2201'.
00093      12  ER-2314                 PIC X(4)  VALUE '2314'.
00094      12  ER-2372                 PIC X(4)  VALUE '2372'.
00095      12  ER-2375                 PIC X(4)  VALUE '2375'.
00096      12  ER-2377                 PIC X(4)  VALUE '2377'.
00097      12  ER-2971                 PIC X(4)  VALUE '2971'.
00098
00099      12  DEEDIT-FIELD            PIC X(15).
00100      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00101
00102      12  CLIENT-MIC              PIC X(3)  VALUE 'MIC'.
00103      12  CLIENT-PEM              PIC X(3)  VALUE 'PEM'.
00104      12  CLIENT-CSO              PIC X(3)  VALUE 'CSO'.
00105      12  CLIENT-FLA              PIC X(3)  VALUE 'FLA'.
00106
00107      12  WS-ACCT-KEY.
00108          16  FILLER              PIC X(20).
00109          16  WS-EFF-DT           PIC XX.
00110
00111      12  RETURN-FROM             PIC X(8).
00112      12  QID.
00113          16  QID-TERM            PIC X(4).
00114          16  FILLER              PIC X(4)    VALUE '631A'.
00115      EJECT
00116 *    COPY ELCDATE.
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
00117      EJECT
00118 *    COPY ELCLOGOF.
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
00119      EJECT
00120 *    COPY ELCATTR.
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
00121      EJECT
00122 *    COPY ELCEMIB.
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
00123      EJECT
00124 *    COPY ELCINTF.
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
00125      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00126 *    COPY ELC631PI.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELC631PI                            *
00004 *                            VMOD=2.012                          *
00005 *                                                                *
00006 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
00007 *    REVIEW AND CORRRECTION SUB-SYSTEM.  ANY CHANGES WILL        *
00008 *    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
00009 *                                                                *
00010 *    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
00011 *    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
00012 *    BETWEEN PROGRAMS.                                           *
00013 *                                                                *
00014 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
00015 *                                                                *
00016 *               EL631 - EL6311 - EL6312 - EL6313                 *
00017 *                                                                *
00018 ******************************************************************
021414*                   C H A N G E   L O G
021414*
021414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
021414*-----------------------------------------------------------------
021414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
021414* EFFECTIVE    NUMBER
021414*-----------------------------------------------------------------
021414* 021414    2003053000001  PEMA  changes for auto chk request
021414******************************************************************
00019
00020          16  PI-631-DATA.
00021              20  PI-ERPNDB-KEY.
00022                  24  PI-PB-COMPANY-CD     PIC X.
00023                  24  PI-PB-ENTRY-BATCH    PIC X(6).
00024                  24  PI-PB-BATCH-SEQ-NO   PIC S9(4) COMP.
00025                  24  PI-PB-BATCH-CHG-SEQ-NO PIC S9(4) COMP.
00026
00027              20  PI-ERPNDB-ALT-KEY.
00028                  24  PI-PB-COMPANY-CD-A1  PIC X.
00029                  24  PI-PB-CARRIER        PIC X.
00030                  24  PI-PB-GROUPING       PIC X(6).
00031                  24  PI-PB-STATE          PIC XX.
00032                  24  PI-PB-ACCOUNT        PIC X(10).
00033                  24  PI-PB-CERT-EFF-DT    PIC XX.
00034                  24  PI-PB-CERT-NO.
00035                      28  PI-PB-CERT-PRIME PIC X(10).
00036                      28  PI-PB-CERT-SFX   PIC X.
00037                  24  PI-PB-ALT-CHG-SEQ-NO PIC S9(4) COMP.
00038                  24  PI-PB-RECORD-TYPE    PIC X.
00039
00040              20  PI-ERPNDB-CSR-KEY.
00041                  24  PI-PB-CSR-COMPANY-CD-A2  PIC X.
00042                  24  PI-PB-CSR-ID             PIC X(4).
00043                  24  PI-PB-CSR-ENTRY-BATCH    PIC X(6).
00044                  24  PI-PB-CSR-BTCH-SEQ-NO    PIC S9(4) COMP.
00045                  24  PI-PB-CSR-BTCH-CHG-SEQ-NO PIC S9(4) COMP.
00046
00047              20  PI-BROWSE-TYPE               PIC X.
00048                  88  PI-FILE-BROWSE             VALUE ' '.
00049                  88  PI-PRIMARY-BROWSE          VALUE '1'.
00050                  88  PI-ALTERNATE-BROWSE        VALUE '2'.
00051                  88  PI-PRIMARY-WITH-SELECT     VALUE '3'.
00052                  88  PI-CSR-BROWSE              VALUE '4'.
00053
00054              20  PI-MAINT-FUNCTION            PIC X.
00055                  88  PI-ADD-FUNCTION            VALUE 'A'.
00056                  88  PI-BROWSE-FUNCTION         VALUE 'B'.
00057                  88  PI-CHANGE-FUNCTION         VALUE 'C'.
00058                  88  PI-DELETE-FUNCTION         VALUE 'D'.
00059                  88  PI-SHOW-FUNCTION           VALUE 'S'.
00060                  88  PI-PF5-FUNCTION            VALUE '5'.
00061                  88  PI-PF6-FUNCTION            VALUE '6'.
00062
00063              20  PI-FILE-SWITCHES.
00064                  24  PI-ALL-ISSUES-SW         PIC X.
00065                      88  ALL-ISSUES             VALUE 'Y'.
00066                  24  PI-ALL-CANCELS-SW        PIC X.
00067                      88  ALL-CANCELS            VALUE 'Y'.
00068                  24  PI-ISSUES-IN-ERROR-SW    PIC X.
00069                      88  ISSUES-IN-ERROR        VALUE 'Y'.
00070                  24  PI-CANCELS-IN-ERROR-SW   PIC X.
00071                      88  CANCEL-IN-ERROR        VALUE 'Y'.
00072                  24  PI-ONLY-BATCH-HEADERS-SW PIC X.
00073                      88  ONLY-BATCH-HEADERS     VALUE 'Y'.
00074                  24  PI-ALL-OUT-OF-BAL-SW     PIC X.
00075                      88  ALL-OUT-OF-BAL         VALUE 'Y'.
00076                  24  PI-HOLD-REC-SW           PIC X.
00077                      88  DISPLAY-HOLD-RECORDS   VALUE 'Y'.
00078                  24  PI-CHANGE-REC-SW         PIC X.
00079                      88  DISPLAY-CHANGE-RECORDS VALUE 'Y'.
00080                  24  PI-CHK-REQ-REC-SW        PIC X.
00081                      88  DISPLAY-CHK-REQ-RECORDS VALUE 'Y'.
00082                  24  PI-ISSUE-WARNING-SW      PIC X.
00083                      88  ISSUE-WITH-WARNING     VALUE 'Y'.
00084                  24  PI-CANCEL-WARNING-SW     PIC X.
00085                      88  CANCEL-WITH-WARNING    VALUE 'Y'.
00086              20  PI-DISPLAY-SCREEN-SW         PIC X.
00087                      88  PI-DISPLAY-SCREEN      VALUE 'Y'.
00088              20  PI-ORIGINAL-BATCH-SW         PIC X.
00089                      88  PI-DISPLAY-ORIGINAL-BATCH VALUE 'Y'.
00090
00091              20  PI-MAP-NAME                  PIC X(8).
00092
00093              20  PI-CURSOR                    PIC S9(4) COMP.
00094
00095              20  PI-PREV-ALT-KEY              PIC X(36).
00096              20  PI-PREV-CSR-KEY              PIC X(15).
00097              20  PI-PREV-KEY.
00098                  24  PI-PREV-COMPANY-CD       PIC X.
00099                  24  PI-PREV-BATCH            PIC X(6).
00100                  24  PI-PREV-SEQ-NO           PIC S9(4) COMP.
00101                  24  PI-PREV-CHG-SEQ-NO       PIC S9(4) COMP.
00102              20  PI-PREV-CONTROL-PRIMARY      PIC X(11).
00103              20  PI-BROWSE-SW                 PIC X.
00104                  88  PI-GOOD-BROWSE             VALUE 'Y'.
00105                  88  PI-NO-PB-RECS-FOUND        VALUE '9'.
00106              20  PI-SV-CARRIER                PIC X.
00107              20  PI-SV-GROUPING               PIC X(6).
00108              20  PI-SV-STATE                  PIC XX.
00109              20  PI-EDIT-SW                   PIC X.
00110              20  PI-DISPLAY-SW                PIC XX.
00111                  88 PI-DISPLAY-LIFE        VALUE 'LF'.
00112                  88 PI-DISPLAY-AH          VALUE 'AH'.
00113              20  PI-CRITERIA-DATA             PIC X(350).
00114              20  PI-BMODE                     PIC X.
00115              20  PI-BPMTAMT                   PIC S9(7)V99 COMP-3.
00116              20  PI-BPMTS                     PIC S999     COMP-3.
00117              20  PI-BTYPE                     PIC XXX OCCURS 2.
00118              20  PI-HIGH-SEQ-NO               PIC S9(4) COMP.
                   20  PI-CSR-SESSION-SW            PIC X.
                       88  CSR-EDIT-SESSION           VALUE 'Y'.
                   20  PI-ERRORS-SW                 PIC X.
062712                 88  FATAL-ERRORS               VALUE 'X'.
062712*                88  FATAL-OR-UNFORCED          VALUE 'X'.
021414             20  pi-unforced-sw               pic x.
021414                 88  unforced-errors            value 'X'.
021414             20  FILLER                       PIC X(4).
00120
00127          16  FILLER              PIC X(94).
00128
00129      EJECT
00130
00131 *    COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '?'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00132  01  FILLER    REDEFINES DFHAID.
00133      12  FILLER                  PIC X(8).
00134      12  PF-VALUES               PIC X       OCCURS 24 TIMES.
00135
00136      EJECT
00137
00138 *    COPY EL631S.
       01  EL631AI.
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
           05  HEADL PIC S9(0004) COMP.
           05  HEADF PIC  X(0001).
           05  FILLER REDEFINES HEADF.
               10  HEADA PIC  X(0001).
           05  HEADI PIC  X(0038).
      *    -------------------------------
           05  MODL PIC S9(0004) COMP.
           05  MODF PIC  X(0001).
           05  FILLER REDEFINES MODF.
               10  MODA PIC  X(0001).
           05  MODI PIC  X(0003).
      *    -------------------------------
           05  BATCHL PIC S9(0004) COMP.
           05  BATCHF PIC  X(0001).
           05  FILLER REDEFINES BATCHF.
               10  BATCHA PIC  X(0001).
           05  BATCHI PIC  X(0006).
      *    -------------------------------
           05  SEQL PIC S9(0004) COMP.
           05  SEQF PIC  X(0001).
           05  FILLER REDEFINES SEQF.
               10  SEQA PIC  X(0001).
           05  SEQI PIC  X(0004).
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
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCOUNTL PIC S9(0004) COMP.
           05  ACCOUNTF PIC  X(0001).
           05  FILLER REDEFINES ACCOUNTF.
               10  ACCOUNTA PIC  X(0001).
           05  ACCOUNTI PIC  X(0010).
      *    -------------------------------
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0008).
      *    -------------------------------
           05  CERTNOL PIC S9(0004) COMP.
           05  CERTNOF PIC  X(0001).
           05  FILLER REDEFINES CERTNOF.
               10  CERTNOA PIC  X(0001).
           05  CERTNOI PIC  X(0010).
      *    -------------------------------
           05  SUFFIXL PIC S9(0004) COMP.
           05  SUFFIXF PIC  X(0001).
           05  FILLER REDEFINES SUFFIXF.
               10  SUFFIXA PIC  X(0001).
           05  SUFFIXI PIC  X(0001).
      *    -------------------------------
           05  CSRHDL PIC S9(0004) COMP.
           05  CSRHDF PIC  X(0001).
           05  FILLER REDEFINES CSRHDF.
               10  CSRHDA PIC  X(0001).
           05  CSRHDI PIC  X(0004).
      *    -------------------------------
           05  CSRL PIC S9(0004) COMP.
           05  CSRF PIC  X(0001).
           05  FILLER REDEFINES CSRF.
               10  CSRA PIC  X(0001).
           05  CSRI PIC  X(0004).
      *    -------------------------------
           05  ANYISSL PIC S9(0004) COMP.
           05  ANYISSF PIC  X(0001).
           05  FILLER REDEFINES ANYISSF.
               10  ANYISSA PIC  X(0001).
           05  ANYISSI PIC  X(0001).
      *    -------------------------------
           05  ANYCANL PIC S9(0004) COMP.
           05  ANYCANF PIC  X(0001).
           05  FILLER REDEFINES ANYCANF.
               10  ANYCANA PIC  X(0001).
           05  ANYCANI PIC  X(0001).
      *    -------------------------------
           05  ALLISSL PIC S9(0004) COMP.
           05  ALLISSF PIC  X(0001).
           05  FILLER REDEFINES ALLISSF.
               10  ALLISSA PIC  X(0001).
           05  ALLISSI PIC  X(0001).
      *    -------------------------------
           05  ALLCANL PIC S9(0004) COMP.
           05  ALLCANF PIC  X(0001).
           05  FILLER REDEFINES ALLCANF.
               10  ALLCANA PIC  X(0001).
           05  ALLCANI PIC  X(0001).
      *    -------------------------------
           05  ONLYHDRL PIC S9(0004) COMP.
           05  ONLYHDRF PIC  X(0001).
           05  FILLER REDEFINES ONLYHDRF.
               10  ONLYHDRA PIC  X(0001).
           05  ONLYHDRI PIC  X(0001).
      *    -------------------------------
           05  OUTBALL PIC S9(0004) COMP.
           05  OUTBALF PIC  X(0001).
           05  FILLER REDEFINES OUTBALF.
               10  OUTBALA PIC  X(0001).
           05  OUTBALI PIC  X(0001).
      *    -------------------------------
           05  ORGINALL PIC S9(0004) COMP.
           05  ORGINALF PIC  X(0001).
           05  FILLER REDEFINES ORGINALF.
               10  ORGINALA PIC  X(0001).
           05  ORGINALI PIC  X(0001).
      *    -------------------------------
           05  ORGHDRL PIC S9(0004) COMP.
           05  ORGHDRF PIC  X(0001).
           05  FILLER REDEFINES ORGHDRF.
               10  ORGHDRA PIC  X(0001).
           05  ORGHDRI PIC  X(0028).
      *    -------------------------------
           05  HLDRECL PIC S9(0004) COMP.
           05  HLDRECF PIC  X(0001).
           05  FILLER REDEFINES HLDRECF.
               10  HLDRECA PIC  X(0001).
           05  HLDRECI PIC  X(0001).
      *    -------------------------------
           05  CHGRECL PIC S9(0004) COMP.
           05  CHGRECF PIC  X(0001).
           05  FILLER REDEFINES CHGRECF.
               10  CHGRECA PIC  X(0001).
           05  CHGRECI PIC  X(0001).
      *    -------------------------------
           05  CHKREQL PIC S9(0004) COMP.
           05  CHKREQF PIC  X(0001).
           05  FILLER REDEFINES CHKREQF.
               10  CHKREQA PIC  X(0001).
           05  CHKREQI PIC  X(0001).
      *    -------------------------------
           05  STARTATL PIC S9(0004) COMP.
           05  STARTATF PIC  X(0001).
           05  FILLER REDEFINES STARTATF.
               10  STARTATA PIC  X(0001).
           05  STARTATI PIC  X(0001).
      *    -------------------------------
           05  ANYISSWL PIC S9(0004) COMP.
           05  ANYISSWF PIC  X(0001).
           05  FILLER REDEFINES ANYISSWF.
               10  ANYISSWA PIC  X(0001).
           05  ANYISSWI PIC  X(0001).
      *    -------------------------------
           05  ANYCANWL PIC S9(0004) COMP.
           05  ANYCANWF PIC  X(0001).
           05  FILLER REDEFINES ANYCANWF.
               10  ANYCANWA PIC  X(0001).
           05  ANYCANWI PIC  X(0001).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0079).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
       01  EL631AO REDEFINES EL631AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HEADO PIC  X(0038).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MODO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BATCHO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCOUNTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUFFIXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSRHDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANYISSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANYCANO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALLISSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALLCANO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ONLYHDRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OUTBALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ORGINALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ORGHDRO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLDRECO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHGRECO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKREQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STARTATO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANYISSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANYCANWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
00139
00140      EJECT
00141
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
00143  01  DFHCOMMAREA                 PIC X(1300).
00144
00145 *01 PARMLIST .
00146 *    02  FILLER                  PIC S9(8)   COMP.
00147 *    02  ERPNDB-POINTER          PIC S9(8)   COMP.
00148 *    02  ERACCT-POINTER          PIC S9(8)   COMP.
00149
00150      EJECT
00151 *    COPY ERCPNDB.
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
071211* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
073114* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
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
010716         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
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
                   88  PB-I-POLICY-IS-CASH          VALUE 'C'.
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
071211         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
071211                                          PIC S9(5)V99    COMP-3.
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
071211         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
071211         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
00283          16  FILLER                       PIC X(01).
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
                       88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
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
                       88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
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
071211             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
00380
072209         16  FILLER                       PIC X(13).
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
                   88  PB-CASH-CERT                 VALUE 'C'.
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
00152      EJECT
00153 *    COPY ERCACCT.
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
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
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
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
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
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
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
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
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
00154      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-BUSINESS
                                ACCOUNT-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL631' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00156
00157      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00158      MOVE '5'                   TO DC-OPTION-CODE.
00159      PERFORM 9700-DATE-LINK.
00160      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00161      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00162
00163      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00164      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00165      MOVE EIBTRMID               TO QID-TERM.
00166
00167      IF EIBCALEN = 0
00168          GO TO 8800-UNAUTHORIZED-ACCESS.
00169
00170      MOVE LOW-VALUES             TO EL631AI.
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
CIDMOD             IF PI-COMPANY-ID = CLIENT-MIC OR CLIENT-PEM OR
CIDMOD                                CLIENT-FLA
00183                 MOVE 'Y'               TO ORGINALI
CIDMOD             END-IF
00186          ELSE
00187              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM
00188              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00189              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00190              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00191              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00192              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00193              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00194              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00195              MOVE SPACES               TO PI-SAVED-PROGRAM-6
00196              PERFORM 6000-BUILD-SCREEN THRU 6099-EXIT.
00197
00198 ******************************************************************
00199 *                                                                *
00200 *        IF THERE WERE NO RECORDS FOUND FOR CARRIER AND ACCOUNT  *
00201 *        DISPLAY ORIGINAL SCREEN.                                *
00202 *                                                                *
00203 ******************************************************************
00204
00205      IF PI-NO-PB-RECS-FOUND
00206          MOVE SPACE TO PI-BROWSE-SW
00207          GO TO 8100-SEND-INITIAL-MAP.
00208
00209      IF EIBTRNID NOT = TRANS-ID
00210          GO TO 8100-SEND-INITIAL-MAP.
00211
00212      
      * EXEC CICS HANDLE CONDITION
00213 *        PGMIDERR  (9600-PGMID-ERROR)
00214 *        ERROR     (9990-ABEND)
00215 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00002568' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032353638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00216
00217      IF EIBAID = DFHCLEAR
00218          GO TO 9400-CLEAR.
00219
00220      IF PI-PROCESSOR-ID = 'LGXX'
00221          GO TO 0200-RECEIVE.
00222
00223      
      * EXEC CICS READQ TS
00224 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00225 *        INTO   (SECURITY-CONTROL)
00226 *        LENGTH (SC-COMM-LENGTH)
00227 *        ITEM   (SC-ITEM)
00228 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00002579' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00229
00230      MOVE SC-CREDIT-DISPLAY (12)  TO PI-DISPLAY-CAP.
00231      MOVE SC-CREDIT-UPDATE  (12)  TO PI-MODIFY-CAP.
00232
00233      IF NOT DISPLAY-CAP
00234          MOVE 'READ'          TO SM-READ
00235          PERFORM 9995-SECURITY-VIOLATION
00236          MOVE ER-0070         TO  EMI-ERROR
00237          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00238          GO TO 8100-SEND-INITIAL-MAP.
00239
00240      EJECT
00241
00242  0200-RECEIVE.
00243      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00244          MOVE ER-0008            TO EMI-ERROR
00245          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00246          MOVE -1                 TO BATCHL
00247          GO TO 8200-SEND-DATAONLY.
00248
00249      
      * EXEC CICS RECEIVE
00250 *        MAP      (MAP-NAME)
00251 *        MAPSET   (MAPSET-NAME)
00252 *        INTO     (EL631AI)
00253 *    END-EXEC.
           MOVE LENGTH OF
            EL631AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002605' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL631AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00254
00255      IF PFENTERL = 0
00256          GO TO 0300-CHECK-PFKEYS.
00257
00258      IF EIBAID NOT = DFHENTER
00259          MOVE ER-0004            TO EMI-ERROR
00260          GO TO 0320-INPUT-ERROR.
00261
00262      IF PFENTERI NUMERIC
00263         IF PFENTERI GREATER 0 AND LESS 25
00264            MOVE PF-VALUES (PFENTERI) TO EIBAID
00265         ELSE
00266            MOVE ER-0029            TO EMI-ERROR
00267            GO TO 0320-INPUT-ERROR.
00268
00269  0300-CHECK-PFKEYS.
00270      IF EIBAID = DFHPF1 OR DFHPF2 OR DFHPF3
00271         GO TO 330-EDIT-INPUT-DATA.
00272
00273      IF EIBAID = DFHPF23
00274          GO TO 8810-PF23.
00275
00276      IF EIBAID = DFHPF24
00277          GO TO 9200-RETURN-MAIN-MENU.
00278
00279      IF EIBAID = DFHPF12
00280          GO TO 9500-PF12.
00281
00282      IF EIBAID = DFHENTER
00283          GO TO 330-EDIT-INPUT-DATA.
00284
00285      MOVE ER-0029                TO EMI-ERROR.
00286
00287  0320-INPUT-ERROR.
00288      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00289      MOVE AL-UNBON               TO PFENTERA.
00290
00291      IF PFENTERL = 0
00292          MOVE -1                 TO BATCHL
00293      ELSE
00294          MOVE -1                 TO PFENTERL.
00295
00296      GO TO 8200-SEND-DATAONLY.
00297
00298      EJECT
00299
00300  330-EDIT-INPUT-DATA.
00301      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.
           MOVE PI-ENTRY-CD-1          TO PI-CSR-SESSION-SW
00302      MOVE LOW-VALUES             TO PI-ERPNDB-KEY
00303                                     PI-ERPNDB-ALT-KEY.
00304
00305      MOVE PI-COMPANY-CD          TO PI-PB-COMPANY-CD
00306                                     PI-PB-COMPANY-CD-A1.
00307
00308      IF ANYISSI NOT = 'N' AND 'Y'
00309         MOVE -1                  TO ANYISSL
00310         MOVE AL-UABON            TO ANYISSA
00311         PERFORM 0350-SELECTION-ERROR
00312      ELSE
00313         MOVE ANYISSI             TO PI-ISSUES-IN-ERROR-SW.
00314
00315      IF ANYCANI NOT = 'N' AND 'Y'
00316         MOVE -1                  TO ANYCANL
00317         MOVE AL-UABON            TO ANYCANA
00318         PERFORM 0350-SELECTION-ERROR
00319      ELSE
00320         MOVE ANYCANI             TO PI-CANCELS-IN-ERROR-SW.
00321
00322      IF ALLISSI NOT = 'N' AND 'Y'
00323         MOVE -1                  TO ALLISSL
00324         MOVE AL-UABON            TO ALLISSA
00325         PERFORM 0350-SELECTION-ERROR
00326      ELSE
00327         MOVE ALLISSI             TO PI-ALL-ISSUES-SW.
00328
00329      IF ALLCANI NOT = 'N' AND 'Y'
00330         MOVE -1                  TO ALLCANL
00331         MOVE AL-UABON            TO ALLCANA
00332         PERFORM 0350-SELECTION-ERROR
00333      ELSE
00334         MOVE ALLCANI             TO PI-ALL-CANCELS-SW.
00335
00336      IF ONLYHDRI NOT = 'N' AND 'Y'
00337         MOVE -1                  TO ONLYHDRL
00338         MOVE AL-UABON            TO ONLYHDRA
00339         PERFORM 0350-SELECTION-ERROR
00340      ELSE
00341         MOVE ONLYHDRI             TO PI-ONLY-BATCH-HEADERS-SW.
00342
00343      IF OUTBALI NOT = 'N' AND 'Y'
00344         MOVE -1                  TO OUTBALL
00345         MOVE AL-UABON            TO OUTBALA
00346         PERFORM 0350-SELECTION-ERROR
00347      ELSE
00348         MOVE OUTBALI             TO PI-ALL-OUT-OF-BAL-SW.
00349
00350      IF HLDRECI NOT = 'N' AND 'Y'
00351         MOVE -1                  TO HLDRECL
00352         MOVE AL-UABON            TO HLDRECA
00353         PERFORM 0350-SELECTION-ERROR
00354      ELSE
00355         MOVE HLDRECI             TO PI-HOLD-REC-SW.
00356
00357      IF CHGRECI NOT = 'N' AND 'Y'
00358         MOVE -1                  TO CHGRECL
00359         MOVE AL-UABON            TO CHGRECA
00360         PERFORM 0350-SELECTION-ERROR
00361      ELSE
00362         MOVE CHGRECI             TO PI-CHANGE-REC-SW.
00363
00364      IF CHKREQI NOT = 'N' AND 'Y'
00365         MOVE -1                  TO CHKREQL
00366         MOVE AL-UABON            TO CHKREQA
00367         PERFORM 0350-SELECTION-ERROR
00368      ELSE
00369         MOVE CHKREQI             TO PI-CHK-REQ-REC-SW.
00370
00371      IF STARTATI NOT = 'N' AND 'Y'
00372         MOVE -1                  TO STARTATL
00373         MOVE AL-UABON            TO STARTATA
00374         PERFORM 0350-SELECTION-ERROR.
00375
00376      IF ORGINALI NOT = 'N' AND 'Y'
00377         MOVE -1                  TO ORGINALL
00378         MOVE AL-UABON            TO ORGINALA
00379         PERFORM 0350-SELECTION-ERROR
00380      ELSE
00381         MOVE ORGINALI            TO PI-ORIGINAL-BATCH-SW.
00382
00383      IF ANYISSWI NOT = 'N' AND 'Y'
00384         MOVE -1                  TO ANYISSWL
00385         MOVE AL-UABON            TO ANYISSWA
00386         PERFORM 0350-SELECTION-ERROR
00387      ELSE
00388         MOVE ANYISSWI            TO PI-ISSUE-WARNING-SW.
00389
00390      IF ANYCANWI NOT = 'N' AND 'Y'
00391         MOVE -1                  TO ANYCANWL
00392         MOVE AL-UABON            TO ANYCANWA
00393         PERFORM 0350-SELECTION-ERROR
00394      ELSE
00395         MOVE ANYCANWI            TO PI-CANCEL-WARNING-SW.
00396
00397      IF NOT EMI-NO-ERRORS
00398         GO TO 8200-SEND-DATAONLY.
00399
00400      IF ANYISSI = 'Y' AND (ALLISSI = 'Y' OR ANYISSWI = 'Y')
00401         MOVE -1                  TO ANYISSL
00402         MOVE AL-UABON            TO ANYISSA  ANYISSWA ALLISSA
00403         PERFORM 0360-COMBINATION-ERROR.
00404
00405      IF ANYISSWI = 'Y' AND (ALLISSI = 'Y' OR ANYISSI = 'Y')
00406         MOVE -1                  TO ANYISSL
00407         MOVE AL-UABON            TO ANYISSA  ANYISSWA ALLISSA
00408         PERFORM 0360-COMBINATION-ERROR.
00409
00410      IF ANYCANI = 'Y' AND (ALLCANI = 'Y' OR ANYCANWI = 'Y')
00411         MOVE -1                  TO ANYCANL
00412         MOVE AL-UABON            TO ANYCANA  ANYCANWA ALLCANA
00413         PERFORM 0360-COMBINATION-ERROR.
00414
00415      IF ANYCANWI = 'Y' AND (ALLCANI = 'Y' OR ANYCANI = 'Y')
00416         MOVE -1                  TO ANYCANL
00417         MOVE AL-UABON            TO ANYCANA  ANYCANWA ALLCANA
00418         PERFORM 0360-COMBINATION-ERROR.
00419
00420      IF OUTBALI = 'Y' AND ONLYHDRI = 'Y'
00421         MOVE -1                  TO ONLYHDRL
00422         MOVE AL-UABON            TO ONLYHDRA  OUTBALA
00423         PERFORM 0360-COMBINATION-ERROR.
00424
00425      IF STARTATI = 'Y'
00426         IF BATCHL NOT GREATER THAN ZEROS
00427            MOVE -1                  TO BATCHL
00428            MOVE AL-UANOF            TO BATCHA
00429            MOVE ER-2201             TO EMI-ERROR
00430            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00431
00432      IF ANYISSI  = 'N' AND ALLISSI  = 'N' AND
00433         ANYCANI  = 'N' AND ALLCANI  = 'N' AND
00434         OUTBALI  = 'N' AND ONLYHDRI = 'N' AND
00435         ANYISSWI = 'N' AND ANYCANWI = 'N'
00436         MOVE ER-0329             TO EMI-ERROR
00437         MOVE -1                  TO ANYISSL
00438         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00439
CIDMOD     IF PI-COMPANY-ID = CLIENT-MIC OR CLIENT-PEM OR CLIENT-FLA
00441         IF ORGINALI = 'N'
00442            NEXT SENTENCE
00443         ELSE
00444            IF CARRIERL = ZEROS AND GROUPL = ZEROS AND
00445               STATEL = ZEROS   AND ACCOUNTL = ZEROS AND
00446               BATCHL = ZEROS
00447               MOVE -1            TO BATCHL
00448               MOVE ER-2150       TO EMI-ERROR
00449               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00450
00451      IF (ONLYHDRI = 'Y' OR OUTBALI = 'Y' OR
00452         CHGRECI  = 'Y') AND ORGINALI = 'Y'
00453         MOVE -1                  TO ORGINALL
00454         PERFORM 0360-COMBINATION-ERROR.
00455
00456      IF (ANYISSI  = 'Y' OR ALLISSI  = 'Y' OR
00457          ANYISSWI = 'Y' OR ANYCANWI = 'Y' OR
00458          ANYCANI  = 'Y' OR ALLCANI  = 'Y')   AND
00459         (OUTBALI  = 'Y' OR ONLYHDRI = 'Y')
00460         MOVE -1                  TO ANYISSL
00461         PERFORM 0360-COMBINATION-ERROR.
00462
00463      IF (ANYISSI = 'Y' OR ALLISSI   = 'Y' OR
00464          CHGRECI = 'Y' OR ONLYHDRI  = 'Y' OR
00465          OUTBALI = 'Y' OR ANYISSWI  = 'Y')
00466          AND CHKREQI = 'Y'
00467          MOVE -1                  TO ANYISSL
00468          PERFORM 0360-COMBINATION-ERROR.
00469
00470      IF  EIBAID = DFHPF1
00471          GO TO 330-CHECK-ERRORS.
00472
00473      IF PI-CARRIER-SECURITY GREATER THAN SPACES
00474         IF  CARRIERL GREATER THAN ZEROS
00475             IF PI-CARRIER-SECURITY = CARRIERI
00476                MOVE AL-UANON     TO CARRIERA
00477             ELSE
00478                MOVE ER-2372      TO EMI-ERROR
00479                MOVE -1           TO CARRIERL
00480                MOVE AL-UABON     TO CARRIERA
00481                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00482
00483      IF PI-ACCOUNT-SECURITY GREATER THAN SPACES
00484         IF  ACCOUNTL GREATER THAN ZEROS
00485             IF PI-ACCOUNT-SECURITY = ACCOUNTI
00486                MOVE AL-UANON     TO ACCOUNTA
00487             ELSE
00488                MOVE ER-2372      TO EMI-ERROR
00489                MOVE -1           TO ACCOUNTL
00490                MOVE AL-UABON     TO ACCOUNTA
00491                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00492
00493      IF BATCHL  GREATER THAN +0
00494         IF CSRL GREATER THAN +0
00495            MOVE AL-UABON         TO BATCHA
00496                                     CSRA
00497            MOVE ER-2971          TO EMI-ERROR
00498            MOVE -1               TO BATCHL
00499            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00500
00501  330-CHECK-ERRORS.
00502      IF NOT EMI-NO-ERRORS
00503         GO TO 8200-SEND-DATAONLY.
00504
00505      IF EIBAID = DFHPF1
00506          PERFORM 5000-CREATE-TEMP-STORAGE THRU 5000-EXIT
00507          MOVE XCTL-630            TO PGM-NAME
00508          GO TO 9300-XCTL.
00509
00510      IF EIBAID = DFHPF2
00511          PERFORM 5000-CREATE-TEMP-STORAGE THRU 5000-EXIT
00512          MOVE XCTL-663            TO PGM-NAME
00513          GO TO 9300-XCTL.
00514
00515      IF EIBAID = DFHPF3
00516          PERFORM 5000-CREATE-TEMP-STORAGE THRU 5000-EXIT
00517          MOVE XCTL-656            TO PGM-NAME
00518          GO TO 9300-XCTL.
00519
00520      IF PI-NO-ACCOUNT-SECURITY
00521         IF PI-NO-CARRIER-SECURITY
00522            GO TO 330-CONTINUE-EDIT.
00523
00524      IF ONLY-BATCH-HEADERS OR ALL-OUT-OF-BAL
00525          GO TO 330-CONTINUE-EDIT.
00526
00527      IF BATCHL GREATER THAN ZEROS
00528          GO TO 330-CONTINUE-EDIT.
00529
00530      IF CARRIERL GREATER THAN ZEROS
00531          GO TO 330-CONTINUE-EDIT.
00532
00533      MOVE -1                     TO BATCHL.
00534      MOVE ER-2377                TO EMI-ERROR.
00535      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00536      GO TO 8200-SEND-DATAONLY.
00537
00538  330-CONTINUE-EDIT.
00539      MOVE ZEROS                  TO PI-PB-BATCH-SEQ-NO.
00540
00541      IF BATCHL NOT = ZERO
00542         MOVE BATCHI              TO PI-PB-ENTRY-BATCH
00543         PERFORM 6200-READ-FILE  THRU 6299-EXIT
00544      ELSE
00545         GO TO 330-SET-BROWSE-CONTROL.
00546
00547      IF SEQL NOT = ZERO
00548          IF SEQI NOT NUMERIC
00549              MOVE ER-2314 TO EMI-ERROR
00550              MOVE -1             TO SEQL
00551              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00552              GO TO 8200-SEND-DATAONLY
00553          ELSE
00554              MOVE SEQI TO PI-PB-BATCH-SEQ-NO.
00555
00556      IF PI-CARRIER-SECURITY GREATER THAN SPACES
00557         IF PI-CARRIER-SECURITY = PB-CARRIER NEXT SENTENCE
00558      ELSE
00559         MOVE ER-2372             TO EMI-ERROR
00560         MOVE -1                  TO BATCHL
00561         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00562         GO TO 8200-SEND-DATAONLY.
00563
00564      IF PI-ACCOUNT-SECURITY GREATER THAN SPACES
00565         IF PI-ACCOUNT-SECURITY = PB-ACCOUNT
00566          NEXT SENTENCE
00567      ELSE
00568         MOVE ER-2372             TO EMI-ERROR
00569         MOVE -1                  TO BATCHL
00570         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00571         GO TO 8200-SEND-DATAONLY.
00572
00573  330-SET-BROWSE-CONTROL.
00574
00575      IF CSRL GREATER THAN +0
00576         IF OUTBALI = 'Y' OR ONLYHDRI = 'Y'
00577            MOVE LOW-VALUES          TO PI-ERPNDB-CSR-KEY
00578            MOVE PI-COMPANY-CD       TO PI-PB-CSR-COMPANY-CD-A2
00579            MOVE CSRI                TO PI-PB-CSR-ID
00580            MOVE SPACES           TO PI-PB-CARRIER   PI-PB-GROUPING
00581                                     PI-PB-STATE     PI-PB-ACCOUNT
00582            MOVE '4'                 TO PI-BROWSE-TYPE
00583            GO TO 0340-XCTL.
00584
00585      IF OUTBALI = 'Y' OR ONLYHDRI = 'Y'
00586         IF CARRIERL = ZEROS AND GROUPL = ZEROS AND
00587            STATEL = ZEROS   AND ACCOUNTL = ZEROS
00588            MOVE SPACES           TO PI-PB-CARRIER   PI-PB-GROUPING
00589                                     PI-PB-STATE     PI-PB-ACCOUNT
00590            MOVE  '1'             TO PI-BROWSE-TYPE
00591            GO TO 0340-XCTL
00592         ELSE
00593            PERFORM 6100-BUILD-KEY THRU 6199-EXIT
00594            MOVE  '1'             TO PI-BROWSE-TYPE
00595            GO TO 0340-XCTL.
00596
00597      IF CERTNOL NOT = ZEROS
00598         MOVE CERTNOI             TO PI-PB-CERT-PRIME
00599         MOVE SPACE               TO PI-PB-CERT-SFX
00600         IF SUFFIXL NOT = ZEROS
00601            MOVE SUFFIXI          TO PI-PB-CERT-SFX
00602         ELSE
00603            NEXT SENTENCE
00604      ELSE
00605         MOVE LOW-VALUES          TO PI-PB-CERT-NO.
00606
00607      IF EFFDTL NOT = ZEROS
00608         MOVE EFFDTI                 TO DEEDIT-FIELD
00609         PERFORM 8600-DEEDIT
00610         MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY
00611         MOVE '4'                    TO DC-OPTION-CODE
00612         PERFORM 9700-DATE-LINK
00613         IF DATE-CONVERSION-ERROR
00614            MOVE ER-0348             TO EMI-ERROR
00615            MOVE -1                  TO EFFDTL
00616            MOVE AL-UABON            TO EFFDTA
00617            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00618         ELSE
00619            MOVE DC-GREG-DATE-1-EDIT TO EFFDTI
00620            MOVE AL-UANON            TO EFFDTA
00621            MOVE DC-BIN-DATE-1       TO PI-PB-CERT-EFF-DT.
00622
00623      IF CSRL GREATER THAN +0
00624         MOVE LOW-VALUES             TO PI-ERPNDB-CSR-KEY
00625         MOVE PI-COMPANY-CD          TO PI-PB-CSR-COMPANY-CD-A2
00626         MOVE CSRI                   TO PI-PB-CSR-ID
00627         MOVE '4'                    TO PI-BROWSE-TYPE
00628         GO TO 0340-XCTL.
00629
00630      IF CARRIERL = ZEROS AND
00631         GROUPL   = ZEROS AND
00632         STATEL   = ZEROS AND
00633         ACCOUNTL = ZEROS
00634         IF BATCHL NOT = ZEROS
00635            GO TO 0335-SET-BROWSE-TYPE
00636         ELSE
00637            MOVE ' '                 TO PI-BROWSE-TYPE
00638            GO TO 0340-XCTL.
00639
00640      PERFORM 6100-BUILD-KEY THRU 6199-EXIT.
00641
00642      IF NOT EMI-NO-ERRORS
00643         GO TO 8200-SEND-DATAONLY.
00644
00645      IF PI-COMPANY-ID NOT = 'DMD'
00646          PERFORM 6300-READ-ACCOUNT-FILE THRU 6399-EXIT.
00647
00648  0335-SET-BROWSE-TYPE.
00649      IF STARTATI = 'Y'
00650         MOVE BATCHI              TO PI-PB-ENTRY-BATCH
00651         MOVE SPACE               TO PI-BROWSE-TYPE
00652         GO TO 0340-XCTL.
00653
00654      IF BATCHL NOT = ZEROS
00655         IF CERTNOL = ZEROS AND EFFDTL = ZEROS
00656            MOVE '1'              TO PI-BROWSE-TYPE
00657         ELSE
00658            MOVE '3'              TO PI-BROWSE-TYPE
00659      ELSE
00660         MOVE '2'              TO PI-BROWSE-TYPE.
00661
00662  0340-XCTL.
020816     if pi-company-id = 'VPP'
020816        move 'VP6311'            TO PGM-NAME
020816     ELSE
020816        MOVE XCTL-6311           TO PGM-NAME
020816     END-IF
00665      
      * EXEC CICS XCTL
00666 *        PROGRAM    (PGM-NAME)
00667 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00668 *        LENGTH     (1300)
00669 *    END-EXEC.
           MOVE 1300
             TO DFHEIV11
      *    MOVE '.$C                   %   #00003025' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00670
00671  0350-SELECTION-ERROR.
00672      MOVE ER-2190                TO EMI-ERROR.
00673      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00674
00675  0360-COMBINATION-ERROR.
00676      MOVE ER-2191                TO EMI-ERROR.
00677      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00678
00679      EJECT
00680
00681  5000-CREATE-TEMP-STORAGE.
00682      
      * EXEC CICS WRITEQ TS
00683 *        QUEUE    (QID)
00684 *        FROM     (PROGRAM-INTERFACE-BLOCK)
00685 *        LENGTH   (PI-COMM-LENGTH)
00686 *    END-EXEC.
      *    MOVE '*"                    ''   #00003042' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00687
00688  5000-EXIT.
00689       EXIT.
00690
00691      EJECT
00692
00693  5050-RECOVER-TEMP-STORAGE.
00694      
      * EXEC CICS HANDLE CONDITION
00695 *        QIDERR  (5050-QID-ERROR)
00696 *    END-EXEC.
      *    MOVE '"$N                   ! # #00003054' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033303534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00697
00698      
      * EXEC CICS READQ TS
00699 *         QUEUE   (QID)
00700 *         INTO    (PROGRAM-INTERFACE-BLOCK)
00701 *         LENGTH  (PI-COMM-LENGTH)
00702 *     END-EXEC.
      *    MOVE '*$I    L              ''   #00003058' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00703
00704      
      * EXEC CICS DELETEQ TS
00705 *        QUEUE   (QID)
00706 *    END-EXEC.
      *    MOVE '*&                    #   #00003064' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00707
00708      GO TO 5050-EXIT.
00709
00710  5050-QID-ERROR.
00711      MOVE ER-0033                TO EMI-ERROR.
00712      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00713
00714  5050-EXIT.
00715       EXIT.
00716
00717      EJECT
00718
00719  6000-BUILD-SCREEN.
00720      IF RETURN-FROM = XCTL-630 OR XCTL-663 OR XCTL-656
00721         PERFORM 5050-RECOVER-TEMP-STORAGE THRU 5050-EXIT.
00722
00723      MOVE PI-ISSUES-IN-ERROR-SW      TO ANYISSI.
00724      MOVE PI-CANCELS-IN-ERROR-SW     TO ANYCANI.
00725      MOVE PI-ALL-ISSUES-SW           TO ALLISSI.
00726      MOVE PI-ALL-CANCELS-SW          TO ALLCANI.
00727      MOVE PI-ONLY-BATCH-HEADERS-SW   TO ONLYHDRI.
00728      MOVE PI-ALL-OUT-OF-BAL-SW       TO OUTBALI.
00729      MOVE PI-HOLD-REC-SW             TO HLDRECI.
00730      MOVE PI-CHANGE-REC-SW           TO CHGRECI.
00731      MOVE PI-ORIGINAL-BATCH-SW       TO ORGINALI.
00732      MOVE PI-ISSUE-WARNING-SW        TO ANYISSWI.
00733      MOVE PI-CANCEL-WARNING-SW       TO ANYCANWI.
00734
00735      IF PI-PB-ENTRY-BATCH  = SPACES OR = LOW-VALUES
00736         NEXT SENTENCE
00737      ELSE
00738         MOVE AL-UANON            TO BATCHA
00739         MOVE PI-PB-ENTRY-BATCH   TO BATCHI.
00740
00741      IF PI-PB-CERT-NO = SPACES OR = LOW-VALUES
00742         NEXT SENTENCE
00743      ELSE
00744         MOVE AL-UANON            TO CERTNOA
00745         MOVE PI-PB-CERT-PRIME    TO CERTNOI
00746         MOVE AL-UANON            TO SUFFIXA
00747         MOVE PI-PB-CERT-SFX      TO SUFFIXI.
00748
00749      IF PI-PB-CERT-EFF-DT = LOW-VALUES OR SPACES
00750         NEXT SENTENCE
00751      ELSE
00752         MOVE PI-PB-CERT-EFF-DT      TO DC-BIN-DATE-1
00753         MOVE ' '                    TO DC-OPTION-CODE
00754         PERFORM 9700-DATE-LINK
00755         MOVE DC-GREG-DATE-1-EDIT    TO EFFDTI
00756         MOVE AL-UANON               TO EFFDTA.
00757
00758      IF PI-NO-PB-RECS-FOUND
00759         MOVE ER-2375             TO EMI-ERROR
00760         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00761
00762      IF NOT PI-ALTERNATE-BROWSE
00763         GO TO 6099-EXIT.
00764
00765      IF PI-PB-CARRIER = SPACES OR = LOW-VALUES
00766         NEXT SENTENCE
00767      ELSE
00768         MOVE PI-PB-CARRIER       TO CARRIERI
00769         MOVE AL-UANON            TO CARRIERA.
00770
00771      IF PI-PB-GROUPING = SPACES OR = LOW-VALUES
00772         NEXT SENTENCE
00773      ELSE
00774         MOVE PI-PB-GROUPING      TO GROUPI
00775         MOVE AL-UANON            TO GROUPA.
00776
00777      IF PI-PB-STATE  = SPACES OR = LOW-VALUES
00778         NEXT SENTENCE
00779      ELSE
00780         MOVE PI-PB-STATE         TO STATEI
00781         MOVE AL-UANON            TO STATEA.
00782
00783      IF PI-PB-ACCOUNT  = SPACES OR = LOW-VALUES
00784         NEXT SENTENCE
00785      ELSE
00786         MOVE PI-PB-ACCOUNT       TO ACCOUNTI
00787         MOVE AL-UANON            TO ACCOUNTA.
00788
00789  6099-EXIT.
00790      EXIT.
00791      EJECT
00792  6100-BUILD-KEY.
00793      MOVE SPACES                 TO PI-PB-CARRIER
00794                                     PI-PB-GROUPING
00795                                     PI-PB-STATE.
00796
00797      IF CARRIERL GREATER THAN ZEROS
00798         IF (CARR-ACCNT-CNTL       OR
00799             CARR-ST-ACCNT-CNTL    OR
00800             CARR-GROUP-ST-ACCNT-CNTL)
00801             MOVE CARRIERI         TO PI-PB-CARRIER.
00802
00803
00804      IF GROUPL GREATER THAN ZEROS
00805         IF CARR-GROUP-ST-ACCNT-CNTL
00806            MOVE GROUPI            TO PI-PB-GROUPING.
00807
00808
00809      IF STATEL GREATER THAN ZEROS
00810         IF (ST-ACCNT-CNTL         OR
00811             CARR-ST-ACCNT-CNTL    OR
00812             CARR-GROUP-ST-ACCNT-CNTL)
00813             MOVE STATEI           TO PI-PB-STATE.
00814
00815      IF ACCOUNTI = SPACES OR = LOW-VALUES
00816         MOVE SPACES              TO PI-PB-ACCOUNT
00817         ELSE
00818         MOVE ACCOUNTI            TO PI-PB-ACCOUNT.
00819
00820      IF PI-PB-CARRIER = SPACES AND
00821         (CARR-ACCNT-CNTL       OR
00822          CARR-ST-ACCNT-CNTL    OR
00823          CARR-GROUP-ST-ACCNT-CNTL)
00824         MOVE ER-0193             TO EMI-ERROR
00825         MOVE -1                  TO CARRIERL
00826         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
00827
00828      IF PI-PB-GROUPING = SPACES AND
00829          CARR-GROUP-ST-ACCNT-CNTL
00830         MOVE ER-0195             TO EMI-ERROR
00831         MOVE -1                  TO GROUPL
00832         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
00833
00834      IF PI-PB-STATE = SPACES AND
00835         (ST-ACCNT-CNTL         OR
00836          CARR-ST-ACCNT-CNTL    OR
00837          CARR-GROUP-ST-ACCNT-CNTL)
00838         MOVE ER-0144             TO EMI-ERROR
00839         MOVE -1                  TO STATEL
00840         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
00841
00842  6199-EXIT.
00843      EXIT.
00844      EJECT
00845  6200-READ-FILE.
00846      
      * EXEC CICS HANDLE CONDITION
00847 *        NOTFND   (6250-NOT-FOUND)
00848 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00003206' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033323036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00849
00850      IF PI-COMPANY-ID = CLIENT-MIC OR CLIENT-PEM OR CLIENT-FLA
00851         IF PI-DISPLAY-ORIGINAL-BATCH
00852            MOVE ERPNDB3-FILE-ID     TO FILE-ID
00853         ELSE
00854            MOVE ERPNDB-FILE-ID      TO FILE-ID
00855      ELSE
00856         MOVE ERPNDB-FILE-ID         TO FILE-ID.
00857
00858      
      * EXEC CICS READ
00859 *        GTEQ
00860 *        DATASET   (FILE-ID)
00861 *        SET       (ADDRESS OF PENDING-BUSINESS)
00862 *        RIDFLD    (PI-ERPNDB-KEY)
00863 *    END-EXEC.
      *    MOVE '&"S        G          (   #00003218' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00864
00865      IF ORGINALL GREATER THAN ZEROS
00866         AND ORGINALI = 'Y'
00867         IF PI-PB-ENTRY-BATCH = PB-ORIGINAL-ENTRY-BATCH AND
00868            PI-PB-COMPANY-CD  = PB-ORIGINAL-COMPANY-CD
00869            GO TO 6299-EXIT.
00870
00871      IF PI-PB-COMPANY-CD  = PB-COMPANY-CD
00872         IF STARTATI = 'Y'
00873             MOVE PB-ENTRY-BATCH TO PI-PB-ENTRY-BATCH
00874             GO TO 6299-EXIT
00875         ELSE
00876             IF PI-PB-ENTRY-BATCH = PB-ENTRY-BATCH
00877                 GO TO 6299-EXIT.
00878
00879  6250-NOT-FOUND.
00880      MOVE ER-2193                TO EMI-ERROR
00881      MOVE -1                     TO BATCHL
00882      MOVE AL-UABON               TO BATCHA
00883      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00884      GO TO 8200-SEND-DATAONLY.
00885
00886  6299-EXIT.
00887       EXIT.
00888      EJECT
00889  6300-READ-ACCOUNT-FILE.
00890      MOVE PI-ERPNDB-ALT-KEY       TO WS-ACCT-KEY.
00891      MOVE LOW-VALUES              TO WS-EFF-DT.
00892
00893      
      * EXEC CICS HANDLE CONDITION
00894 *        NOTFND   (6350-NOT-FOUND)
00895 *    END-EXEC.
      *    MOVE '"$I                   ! % #00003253' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00896
00897      
      * EXEC CICS READ
00898 *        GTEQ
00899 *        DATASET   (ERACCT2-FILE-ID)
00900 *        SET       (ADDRESS OF ACCOUNT-MASTER)
00901 *        RIDFLD    (WS-ACCT-KEY)
00902 *    END-EXEC.
      *    MOVE '&"S        G          (   #00003257' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT2-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00903
00904      IF PI-PB-ACCOUNT    = AM-VG-ACCOUNT   AND
00905         PI-PB-STATE      = AM-VG-STATE     AND
00906         PI-PB-GROUPING   = AM-VG-GROUPING  AND
00907         PI-PB-CARRIER    = AM-VG-CARRIER   AND
00908         PI-PB-COMPANY-CD = AM-COMPANY-CD-A1
00909           GO TO 6399-EXIT.
00910
00911  6350-NOT-FOUND.
00912      MOVE ER-0179                TO EMI-ERROR.
00913      MOVE -1                     TO BATCHL.
00914      MOVE AL-UABON               TO BATCHA.
00915      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00916      GO TO 8200-SEND-DATAONLY.
00917
00918  6399-EXIT.
00919       EXIT.
00920      EJECT
00921  8100-SEND-INITIAL-MAP.
00922      MOVE SPACE                  TO PI-DISPLAY-SCREEN-SW.
00923
00924      IF PI-COMPANY-ID = CLIENT-MIC OR CLIENT-PEM OR CLIENT-FLA
00925         MOVE AL-SANOF            TO ORGHDRA
00926         MOVE AL-UANON            TO ORGINALA
00927         MOVE 'N'                 TO ORGINALI
00928      ELSE
00929         MOVE AL-SADOF            TO ORGHDRA
00930         MOVE 'N'                 TO ORGINALI
00931         MOVE AL-SADON            TO ORGINALA.
00932
           MOVE PI-ENTRY-CD-1          TO PI-CSR-SESSION-SW
           IF CSR-EDIT-SESSION
              MOVE '- CUSTOMER SERVICE REVIEW/CORRECTION -'
                                       TO HEADO
           END-IF
00933      MOVE SAVE-DATE              TO DATEO.
00934      MOVE EIBTIME                TO TIME-IN.
00935      MOVE TIME-OUT               TO TIMEO.
00936      MOVE -1                     TO BATCHL.
00937      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
00938      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
00939
00940      
      * EXEC CICS SEND
00941 *        MAP      (MAP-NAME)
00942 *        MAPSET   (MAPSET-NAME)
00943 *        FROM     (EL631AO)
00944 *        ERASE
00945 *        CURSOR
00946 *    END-EXEC.
           MOVE LENGTH OF
            EL631AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003305' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL631AO, 
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
           
00947
00948      GO TO 9100-RETURN-TRAN.
00949
00950  8200-SEND-DATAONLY.
00951      MOVE SAVE-DATE              TO DATEO.
00952      MOVE EIBTIME                TO TIME-IN.
00953      MOVE TIME-OUT               TO TIMEO.
00954      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
00955      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
00956
00957      
      * EXEC CICS SEND
00958 *        MAP      (MAP-NAME)
00959 *        MAPSET   (MAPSET-NAME)
00960 *        FROM     (EL631AO)
00961 *        DATAONLY
00962 *        ERASEAUP
00963 *        CURSOR
00964 *    END-EXEC.
           MOVE LENGTH OF
            EL631AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00003322' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL631AO, 
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
           
00965
00966      GO TO 9100-RETURN-TRAN.
00967
00968  8300-SEND-TEXT.
00969      
      * EXEC CICS SEND TEXT
00970 *        FROM     (LOGOFF-TEXT)
00971 *        LENGTH   (LOGOFF-LENGTH)
00972 *        ERASE
00973 *        FREEKB
00974 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003334' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333334' TO DFHEIV0(25:11)
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
           
00975
00976      
      * EXEC CICS RETURN
00977 *    END-EXEC.
      *    MOVE '.(                    ''   #00003341' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00978
00979  8600-DEEDIT.
00980      
      * EXEC CICS BIF DEEDIT
00981 *         FIELD   (DEEDIT-FIELD)
00982 *         LENGTH  (15)
00983 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003345' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00984
00985  8800-UNAUTHORIZED-ACCESS.
00986      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00987      GO TO 8300-SEND-TEXT.
00988
00989  8810-PF23.
00990      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00991      MOVE XCTL-005               TO PGM-NAME.
00992      GO TO 9300-XCTL.
00993
00994  9000-RETURN-CICS.
00995      
      * EXEC CICS RETURN
00996 *    END-EXEC.
      *    MOVE '.(                    ''   #00003360' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00997
00998  9100-RETURN-TRAN.
00999      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01000      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
01001      
      * EXEC CICS RETURN
01002 *        TRANSID    (TRANS-ID)
01003 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01004 *        LENGTH     (PI-COMM-LENGTH)
01005 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00003366' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01006
01007  9200-RETURN-MAIN-MENU.
01008      MOVE XCTL-626               TO PGM-NAME.
01009      GO TO 9300-XCTL.
01010
01011  9300-XCTL.
01012      
      * EXEC CICS XCTL
01013 *        PROGRAM    (PGM-NAME)
01014 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01015 *        LENGTH     (PI-COMM-LENGTH)
01016 *    END-EXEC.
      *    MOVE '.$C                   %   #00003377' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01017
01018  9400-CLEAR.
01019      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
01020      GO TO 9300-XCTL.
01021
01022  9500-PF12.
01023      MOVE XCTL-010               TO PGM-NAME.
01024      GO TO 9300-XCTL.
01025
01026  9600-PGMID-ERROR.
01027      
      * EXEC CICS HANDLE CONDITION
01028 *        PGMIDERR    (8300-SEND-TEXT)
01029 *    END-EXEC.
      *    MOVE '"$L                   ! & #00003392' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033333932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01030
01031      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01032      MOVE ' '                    TO PI-ENTRY-CD-1.
01033      MOVE XCTL-005               TO PGM-NAME.
01034      MOVE PGM-NAME               TO LOGOFF-PGM.
01035      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01036      GO TO 9300-XCTL.
01037
01038  9700-DATE-LINK.
01039      MOVE LINK-ELDATCV           TO PGM-NAME
01040      
      * EXEC CICS LINK
01041 *        PROGRAM    (PGM-NAME)
01042 *        COMMAREA   (DATE-CONVERSION-DATA)
01043 *        LENGTH     (DC-COMM-LENGTH)
01044 *    END-EXEC.
      *    MOVE '."C                   (   #00003405' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01045
01046  9900-ERROR-FORMAT.
01047      IF NOT EMI-ERRORS-COMPLETE
01048          MOVE LINK-001           TO PGM-NAME
01049          
      * EXEC CICS LINK
01050 *            PROGRAM    (PGM-NAME)
01051 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01052 *            LENGTH     (EMI-COMM-LENGTH)
01053 *        END-EXEC.
      *    MOVE '."C                   (   #00003414' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01054
01055  9900-EXIT.
01056      EXIT.
01057
01058  9990-ABEND.
01059      MOVE LINK-004               TO PGM-NAME.
01060      MOVE DFHEIBLK               TO EMI-LINE1.
01061      
      * EXEC CICS LINK
01062 *        PROGRAM   (PGM-NAME)
01063 *        COMMAREA  (EMI-LINE1)
01064 *        LENGTH    (72)
01065 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00003426' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01066
01067      GO TO 8200-SEND-DATAONLY.
01068
01069      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL631' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01070
01071  9995-SECURITY-VIOLATION.
01072 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00003454' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343534' TO DFHEIV0(25:11)
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
01073
01074  9995-EXIT.
01075      EXIT.
01076
01077

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL631' TO DFHEIV1
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
               GO TO 5050-QID-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 6250-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 6350-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL631' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
