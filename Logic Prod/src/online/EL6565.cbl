00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL6565.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 11/29/95 08:43:10.
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.011
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
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.    TRANSACTION - EXEA - RATE TABLE DISPLAY
00025      EJECT
00026  ENVIRONMENT DIVISION.
00027  DATA DIVISION.
00028  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00029  77  FILLER  PIC X(32)  VALUE '********************************'.
00030  77  FILLER  PIC X(32)  VALUE '*    EL6565 WORKING STORAGE    *'.
00031  77  FILLER  PIC X(32)  VALUE '************ V/M 2.011 *********'.
00032
00033  01  WS-DATE-AREA.
00034      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00035      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00036
00037  01  WS-AREA.
00038      05  DEEDIT-FIELD                PIC X(15).
00039      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
00040      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(14)V9.
00041      05  DEEDIT-FIELD-V2  REDEFINES DEEDIT-FIELD PIC S9(13)V99.
00042      05  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V999.
00043      05  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).
00044      05  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).
00045      05  DEEDIT-FIELD-V6  REDEFINES DEEDIT-FIELD PIC S9(9)V9(6).
00046
00047      05  WS-DEV-PCT          PIC S9V9(6) VALUE ZEROS.
00048      05  WS-DEV-RATE         PIC S99V9(6) VALUE ZEROS.
00049      05  WS-DEV-RATES        PIC X       VALUE ' '.
00050          88  RATES-DEVIATED              VALUE 'X'.
00051      05  WS-CALLED-FROM-ACCT PIC X(8)    VALUE 'EL6507'.
00052      05  WS-CALL-FROM-STATE  PIC X(8)    VALUE 'EL106'.
00053      05  WS-CALL-FROM-STATE-ALT PIC X(8) VALUE 'EL1061'.
00054      05  WS-COMM-LENGTH      PIC S9(4) COMP VALUE +1525.
00055      05  TRANS-ID            PIC X(4)    VALUE 'EXEA'.
00056      05  THIS-PGM            PIC X(8)    VALUE 'EL6565'.
00057      05  WS-MAPNAME          PIC X(8)    VALUE 'EL6565A'.
00058      05  WS-MAPSET-NAME      PIC X(8)    VALUE 'EL6565S'.
00059      05  PGM-NAME            PIC X(8).
00060      05  SUB1                PIC S999    VALUE +0.
00061      05  SUB2                PIC S999    VALUE +0.
00062      05  SUB3                PIC S99     VALUE +0.
00063          88  EVEN-SUB              VALUE +2  +4  +6  +8 +10
00064                                         +12 +14 +16 +18 +20.
00065      05  SUB4                PIC S99     VALUE +0.
00066      05  TIME-IN             PIC S9(7)   VALUE +0.
00067      05  TIME-OUT-R  REDEFINES TIME-IN.
00068          10  FILLER          PIC X.
00069          10  TIME-OUT        PIC 99V99.
00070          10  FILLER          PIC XX.
00071      05  XCTL-005            PIC X(8)    VALUE 'EL005'.
00072      05  XCTL-010            PIC X(8)    VALUE 'EL010'.
00073      05  XCTL-626            PIC X(8)    VALUE 'EL126'.
00074      05  LINK-001            PIC X(8)    VALUE 'EL001'.
00075      05  LINK-004            PIC X(8)    VALUE 'EL004'.
00076      05  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00077
00078      05  ER-0004             PIC X(4)    VALUE  '0004'.
00079      05  ER-0008             PIC X(4)    VALUE  '0008'.
00080      05  ER-0029             PIC X(4)    VALUE  '0029'.
00081      05  ER-0130             PIC X(4)    VALUE  '0130'.
00082      05  ER-0142             PIC X(4)    VALUE  '0142'.
00083      05  ER-2055             PIC X(4)    VALUE  '2055'.
00084      05  ER-2067             PIC X(4)    VALUE  '2067'.
00085      05  ER-2154             PIC X(4)    VALUE  '2154'.
00086      05  ER-2238             PIC X(4)    VALUE  '2238'.
00087      05  ER-2263             PIC X(4)    VALUE  '2263'.
00088
00089      05  ACCT-FILE-ID        PIC X(8)    VALUE 'ERACCT'.
00090      05  PLAN-FILE-ID        PIC X(8)    VALUE 'ERPLAN'.
00091      05  RATE-FILE-ID        PIC X(8)    VALUE 'ERRATE'.
00092
00093      05  WS-SAVE-RATE-KEY    PIC X(28)   VALUE SPACES.
00094
00095  EJECT
00096 *                                  COPY ELCSCTM.
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
00097  EJECT
00098 *                                  COPY ELCSCRTY.
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
00099      EJECT
00100 *                                  COPY ELCDATE.
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
00101      EJECT
00102 *                                  COPY ELCLOGOF.
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
00103      EJECT
00104 *                                  COPY ELCATTR.
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
00105      EJECT
00106 *                                  COPY ELCEMIB.
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
00107      EJECT
00108 *                                  COPY ELCINTF.
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
00109      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00110          16  FILLER                       PIC X(41).
00111          16  PI-ACCT-KEY.
00112              20  PI-ACCT-CCGSA-KEY.
00113                  24  PI-ACCT-CO             PIC X.
00114                  24  PI-ACCT-CARRIER        PIC X.
00115                  24  PI-ACCT-GROUPING       PIC X(6).
00116                  24  PI-ACCT-STATE          PIC XX.
00117                  24  PI-ACCT-ACCOUNT        PIC X(10).
00118              20  PI-ACCT-EXP-DT           PIC XX.
00119              20  PI-ACCT-REST-OF-EXP      PIC X(4).
00120          16  FILLER                       PIC X(08).
00121          16  PI-PLAN-KEY.
00122              20  PI-PLAN-ACCT-KEY.
00123                  24  PI-PLAN-COMPANY-CD PIC X.
00124                  24  PI-PLAN-CARRIER    PIC X.
00125                  24  PI-PLAN-GROUP      PIC X(6).
00126                  24  PI-PLAN-STATE      PIC X(2).
00127                  24  PI-PLAN-ACCOUNT    PIC X(10).
00128              20  PI-PLAN-BEN-TYPE       PIC X.
00129              20  PI-PLAN-BEN            PIC XX.
00130              20  PI-PLAN-REVISION       PIC X(3).
00131
00132          16  PI-WS-STATE                PIC XX.
00133          16  PI-WS-CLASS                PIC XX.
00134          16  PI-WS-DEV                  PIC X(3).
00135          16  PI-WS-TYPE                 PIC X.
00136          16  PI-WS-PLAN                 PIC XX.
00137          16  PI-PREV-STATE              PIC X(4).
00138
00139 *****************
00140          16  PI-SUB                            PIC S999.
00141          16  PI-YEAR                           PIC S99.
00142          16  PI-NDX                            PIC S99.
00143          16  PI-SAVE-CALLING-PGM               PIC X(8).
00144          16  PI-ERRATE-EOF-SW                  PIC X.
00145              88 ERRATE-EOF                            VALUE 'Y'.
00146          16  PI-BROWSE-SW                      PIC X.
00147              88 BROWSE-STARTED               VALUE 'Y'.
00148          16  PI-STATE-FOUND-SW                 PIC X.
00149              88 STATE-NOT-FOUND              VALUE 'N'.
00150
00151          16  PI-ERRATE-KEY.
00152              20  PI-RATE-COMPANY-CD         PIC X.
00153              20  PI-RATE-STATE-CODE.
00154                  24  PI-RATE-CODE           PIC XX.
00155                  24  PI-RATE-CLASS          PIC XX.
00156                  24  PI-RATE-DEV            PIC XXX.
00157              20  PI-RATE-L-AH-CODE.
00158                  24  PI-RATE-L-AH           PIC X.
00159                  24  PI-RATE-LAH-NUM        PIC XX.
00160              20  PI-RATE-LIMITS             PIC X(11).
00161              20  PI-RATE-EXPIRY-DATE        PIC 9(11)   COMP-3.
00162          16  PI-LF-DEV-PCT                  PIC S9V9(6) COMP-3.
00163          16  PI-AH-DEV-PCT                  PIC S9V9(6) COMP-3.
00164          16  PI-SAVE-RATE-CODE              PIC XX.
00165          16  PI-1ST-TIME                    PIC X.
00166              88  PI-FIRST-TIME-THRU          VALUE 'X'.
00167          16  FILLER                        PIC X(468).
00168      EJECT
00169 *                            COPY ELCAID.
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
00170
00171  01  FILLER REDEFINES DFHAID.
00172      05  FILLER              PIC X(8).
00173      05  PF-VALUES           PIC X       OCCURS 2.
00174
00175      EJECT
00176 *                            COPY EL6565S.
       01  EL6565AI.
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
           05  PCTL PIC S9(0004) COMP.
           05  PCTF PIC  X(0001).
           05  FILLER REDEFINES PCTF.
               10  PCTA PIC  X(0001).
           05  PCTI PIC  X(0008).
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
           05  YR1L PIC S9(0004) COMP.
           05  YR1F PIC  X(0001).
           05  FILLER REDEFINES YR1F.
               10  YR1A PIC  X(0001).
           05  YR1I PIC  X(0002).
      *    -------------------------------
           05  M01L PIC S9(0004) COMP.
           05  M01F PIC  X(0001).
           05  FILLER REDEFINES M01F.
               10  M01A PIC  X(0001).
           05  M01I PIC  X(0005).
      *    -------------------------------
           05  LINE1L PIC S9(0004) COMP.
           05  LINE1F PIC  X(0001).
           05  FILLER REDEFINES LINE1F.
               10  LINE1A PIC  X(0001).
           05  LINE1I PIC  X(0060).
      *    -------------------------------
           05  YR1AL PIC S9(0004) COMP.
           05  YR1AF PIC  X(0001).
           05  FILLER REDEFINES YR1AF.
               10  YR1AA PIC  X(0001).
           05  YR1AI PIC  X(0002).
      *    -------------------------------
           05  M01AL PIC S9(0004) COMP.
           05  M01AF PIC  X(0001).
           05  FILLER REDEFINES M01AF.
               10  M01AA PIC  X(0001).
           05  M01AI PIC  X(0005).
      *    -------------------------------
           05  LINE2L PIC S9(0004) COMP.
           05  LINE2F PIC  X(0001).
           05  FILLER REDEFINES LINE2F.
               10  LINE2A PIC  X(0001).
           05  LINE2I PIC  X(0060).
      *    -------------------------------
           05  YR2L PIC S9(0004) COMP.
           05  YR2F PIC  X(0001).
           05  FILLER REDEFINES YR2F.
               10  YR2A PIC  X(0001).
           05  YR2I PIC  X(0002).
      *    -------------------------------
           05  M02L PIC S9(0004) COMP.
           05  M02F PIC  X(0001).
           05  FILLER REDEFINES M02F.
               10  M02A PIC  X(0001).
           05  M02I PIC  X(0005).
      *    -------------------------------
           05  LINE3L PIC S9(0004) COMP.
           05  LINE3F PIC  X(0001).
           05  FILLER REDEFINES LINE3F.
               10  LINE3A PIC  X(0001).
           05  LINE3I PIC  X(0060).
      *    -------------------------------
           05  YR2AL PIC S9(0004) COMP.
           05  YR2AF PIC  X(0001).
           05  FILLER REDEFINES YR2AF.
               10  YR2AA PIC  X(0001).
           05  YR2AI PIC  X(0002).
      *    -------------------------------
           05  M02AL PIC S9(0004) COMP.
           05  M02AF PIC  X(0001).
           05  FILLER REDEFINES M02AF.
               10  M02AA PIC  X(0001).
           05  M02AI PIC  X(0005).
      *    -------------------------------
           05  LINE4L PIC S9(0004) COMP.
           05  LINE4F PIC  X(0001).
           05  FILLER REDEFINES LINE4F.
               10  LINE4A PIC  X(0001).
           05  LINE4I PIC  X(0060).
      *    -------------------------------
           05  YR3L PIC S9(0004) COMP.
           05  YR3F PIC  X(0001).
           05  FILLER REDEFINES YR3F.
               10  YR3A PIC  X(0001).
           05  YR3I PIC  X(0002).
      *    -------------------------------
           05  M03L PIC S9(0004) COMP.
           05  M03F PIC  X(0001).
           05  FILLER REDEFINES M03F.
               10  M03A PIC  X(0001).
           05  M03I PIC  X(0005).
      *    -------------------------------
           05  LINE5L PIC S9(0004) COMP.
           05  LINE5F PIC  X(0001).
           05  FILLER REDEFINES LINE5F.
               10  LINE5A PIC  X(0001).
           05  LINE5I PIC  X(0060).
      *    -------------------------------
           05  YR3AL PIC S9(0004) COMP.
           05  YR3AF PIC  X(0001).
           05  FILLER REDEFINES YR3AF.
               10  YR3AA PIC  X(0001).
           05  YR3AI PIC  X(0002).
      *    -------------------------------
           05  M03AL PIC S9(0004) COMP.
           05  M03AF PIC  X(0001).
           05  FILLER REDEFINES M03AF.
               10  M03AA PIC  X(0001).
           05  M03AI PIC  X(0005).
      *    -------------------------------
           05  LINE6L PIC S9(0004) COMP.
           05  LINE6F PIC  X(0001).
           05  FILLER REDEFINES LINE6F.
               10  LINE6A PIC  X(0001).
           05  LINE6I PIC  X(0060).
      *    -------------------------------
           05  YR4L PIC S9(0004) COMP.
           05  YR4F PIC  X(0001).
           05  FILLER REDEFINES YR4F.
               10  YR4A PIC  X(0001).
           05  YR4I PIC  X(0002).
      *    -------------------------------
           05  M04L PIC S9(0004) COMP.
           05  M04F PIC  X(0001).
           05  FILLER REDEFINES M04F.
               10  M04A PIC  X(0001).
           05  M04I PIC  X(0005).
      *    -------------------------------
           05  LINE7L PIC S9(0004) COMP.
           05  LINE7F PIC  X(0001).
           05  FILLER REDEFINES LINE7F.
               10  LINE7A PIC  X(0001).
           05  LINE7I PIC  X(0060).
      *    -------------------------------
           05  YR4AL PIC S9(0004) COMP.
           05  YR4AF PIC  X(0001).
           05  FILLER REDEFINES YR4AF.
               10  YR4AA PIC  X(0001).
           05  YR4AI PIC  X(0002).
      *    -------------------------------
           05  M04AL PIC S9(0004) COMP.
           05  M04AF PIC  X(0001).
           05  FILLER REDEFINES M04AF.
               10  M04AA PIC  X(0001).
           05  M04AI PIC  X(0005).
      *    -------------------------------
           05  LINE8L PIC S9(0004) COMP.
           05  LINE8F PIC  X(0001).
           05  FILLER REDEFINES LINE8F.
               10  LINE8A PIC  X(0001).
           05  LINE8I PIC  X(0060).
      *    -------------------------------
           05  YR5L PIC S9(0004) COMP.
           05  YR5F PIC  X(0001).
           05  FILLER REDEFINES YR5F.
               10  YR5A PIC  X(0001).
           05  YR5I PIC  X(0002).
      *    -------------------------------
           05  M05L PIC S9(0004) COMP.
           05  M05F PIC  X(0001).
           05  FILLER REDEFINES M05F.
               10  M05A PIC  X(0001).
           05  M05I PIC  X(0005).
      *    -------------------------------
           05  LINE9L PIC S9(0004) COMP.
           05  LINE9F PIC  X(0001).
           05  FILLER REDEFINES LINE9F.
               10  LINE9A PIC  X(0001).
           05  LINE9I PIC  X(0060).
      *    -------------------------------
           05  YR5AL PIC S9(0004) COMP.
           05  YR5AF PIC  X(0001).
           05  FILLER REDEFINES YR5AF.
               10  YR5AA PIC  X(0001).
           05  YR5AI PIC  X(0002).
      *    -------------------------------
           05  M05AL PIC S9(0004) COMP.
           05  M05AF PIC  X(0001).
           05  FILLER REDEFINES M05AF.
               10  M05AA PIC  X(0001).
           05  M05AI PIC  X(0005).
      *    -------------------------------
           05  LINE10L PIC S9(0004) COMP.
           05  LINE10F PIC  X(0001).
           05  FILLER REDEFINES LINE10F.
               10  LINE10A PIC  X(0001).
           05  LINE10I PIC  X(0060).
      *    -------------------------------
           05  YR6L PIC S9(0004) COMP.
           05  YR6F PIC  X(0001).
           05  FILLER REDEFINES YR6F.
               10  YR6A PIC  X(0001).
           05  YR6I PIC  X(0002).
      *    -------------------------------
           05  M06L PIC S9(0004) COMP.
           05  M06F PIC  X(0001).
           05  FILLER REDEFINES M06F.
               10  M06A PIC  X(0001).
           05  M06I PIC  X(0005).
      *    -------------------------------
           05  LINE11L PIC S9(0004) COMP.
           05  LINE11F PIC  X(0001).
           05  FILLER REDEFINES LINE11F.
               10  LINE11A PIC  X(0001).
           05  LINE11I PIC  X(0060).
      *    -------------------------------
           05  YR6AL PIC S9(0004) COMP.
           05  YR6AF PIC  X(0001).
           05  FILLER REDEFINES YR6AF.
               10  YR6AA PIC  X(0001).
           05  YR6AI PIC  X(0002).
      *    -------------------------------
           05  M06AL PIC S9(0004) COMP.
           05  M06AF PIC  X(0001).
           05  FILLER REDEFINES M06AF.
               10  M06AA PIC  X(0001).
           05  M06AI PIC  X(0005).
      *    -------------------------------
           05  LINE12L PIC S9(0004) COMP.
           05  LINE12F PIC  X(0001).
           05  FILLER REDEFINES LINE12F.
               10  LINE12A PIC  X(0001).
           05  LINE12I PIC  X(0060).
      *    -------------------------------
           05  YR7L PIC S9(0004) COMP.
           05  YR7F PIC  X(0001).
           05  FILLER REDEFINES YR7F.
               10  YR7A PIC  X(0001).
           05  YR7I PIC  X(0002).
      *    -------------------------------
           05  M07L PIC S9(0004) COMP.
           05  M07F PIC  X(0001).
           05  FILLER REDEFINES M07F.
               10  M07A PIC  X(0001).
           05  M07I PIC  X(0005).
      *    -------------------------------
           05  LINE13L PIC S9(0004) COMP.
           05  LINE13F PIC  X(0001).
           05  FILLER REDEFINES LINE13F.
               10  LINE13A PIC  X(0001).
           05  LINE13I PIC  X(0060).
      *    -------------------------------
           05  YR7AL PIC S9(0004) COMP.
           05  YR7AF PIC  X(0001).
           05  FILLER REDEFINES YR7AF.
               10  YR7AA PIC  X(0001).
           05  YR7AI PIC  X(0002).
      *    -------------------------------
           05  M07AL PIC S9(0004) COMP.
           05  M07AF PIC  X(0001).
           05  FILLER REDEFINES M07AF.
               10  M07AA PIC  X(0001).
           05  M07AI PIC  X(0005).
      *    -------------------------------
           05  LINE14L PIC S9(0004) COMP.
           05  LINE14F PIC  X(0001).
           05  FILLER REDEFINES LINE14F.
               10  LINE14A PIC  X(0001).
           05  LINE14I PIC  X(0060).
      *    -------------------------------
           05  YR8L PIC S9(0004) COMP.
           05  YR8F PIC  X(0001).
           05  FILLER REDEFINES YR8F.
               10  YR8A PIC  X(0001).
           05  YR8I PIC  X(0002).
      *    -------------------------------
           05  M08L PIC S9(0004) COMP.
           05  M08F PIC  X(0001).
           05  FILLER REDEFINES M08F.
               10  M08A PIC  X(0001).
           05  M08I PIC  X(0005).
      *    -------------------------------
           05  LINE15L PIC S9(0004) COMP.
           05  LINE15F PIC  X(0001).
           05  FILLER REDEFINES LINE15F.
               10  LINE15A PIC  X(0001).
           05  LINE15I PIC  X(0060).
      *    -------------------------------
           05  YR8AL PIC S9(0004) COMP.
           05  YR8AF PIC  X(0001).
           05  FILLER REDEFINES YR8AF.
               10  YR8AA PIC  X(0001).
           05  YR8AI PIC  X(0002).
      *    -------------------------------
           05  M08AL PIC S9(0004) COMP.
           05  M08AF PIC  X(0001).
           05  FILLER REDEFINES M08AF.
               10  M08AA PIC  X(0001).
           05  M08AI PIC  X(0005).
      *    -------------------------------
           05  LINE16L PIC S9(0004) COMP.
           05  LINE16F PIC  X(0001).
           05  FILLER REDEFINES LINE16F.
               10  LINE16A PIC  X(0001).
           05  LINE16I PIC  X(0060).
      *    -------------------------------
           05  YR9L PIC S9(0004) COMP.
           05  YR9F PIC  X(0001).
           05  FILLER REDEFINES YR9F.
               10  YR9A PIC  X(0001).
           05  YR9I PIC  X(0002).
      *    -------------------------------
           05  M09L PIC S9(0004) COMP.
           05  M09F PIC  X(0001).
           05  FILLER REDEFINES M09F.
               10  M09A PIC  X(0001).
           05  M09I PIC  X(0005).
      *    -------------------------------
           05  LINE17L PIC S9(0004) COMP.
           05  LINE17F PIC  X(0001).
           05  FILLER REDEFINES LINE17F.
               10  LINE17A PIC  X(0001).
           05  LINE17I PIC  X(0060).
      *    -------------------------------
           05  YR9AL PIC S9(0004) COMP.
           05  YR9AF PIC  X(0001).
           05  FILLER REDEFINES YR9AF.
               10  YR9AA PIC  X(0001).
           05  YR9AI PIC  X(0002).
      *    -------------------------------
           05  M09AL PIC S9(0004) COMP.
           05  M09AF PIC  X(0001).
           05  FILLER REDEFINES M09AF.
               10  M09AA PIC  X(0001).
           05  M09AI PIC  X(0005).
      *    -------------------------------
           05  LINE18L PIC S9(0004) COMP.
           05  LINE18F PIC  X(0001).
           05  FILLER REDEFINES LINE18F.
               10  LINE18A PIC  X(0001).
           05  LINE18I PIC  X(0060).
      *    -------------------------------
           05  YRAL PIC S9(0004) COMP.
           05  YRAF PIC  X(0001).
           05  FILLER REDEFINES YRAF.
               10  YRAA PIC  X(0001).
           05  YRAI PIC  X(0002).
      *    -------------------------------
           05  M0AL PIC S9(0004) COMP.
           05  M0AF PIC  X(0001).
           05  FILLER REDEFINES M0AF.
               10  M0AA PIC  X(0001).
           05  M0AI PIC  X(0005).
      *    -------------------------------
           05  LINE19L PIC S9(0004) COMP.
           05  LINE19F PIC  X(0001).
           05  FILLER REDEFINES LINE19F.
               10  LINE19A PIC  X(0001).
           05  LINE19I PIC  X(0060).
      *    -------------------------------
           05  YRAAL PIC S9(0004) COMP.
           05  YRAAF PIC  X(0001).
           05  FILLER REDEFINES YRAAF.
               10  YRAAA PIC  X(0001).
           05  YRAAI PIC  X(0002).
      *    -------------------------------
           05  M0AAL PIC S9(0004) COMP.
           05  M0AAF PIC  X(0001).
           05  FILLER REDEFINES M0AAF.
               10  M0AAA PIC  X(0001).
           05  M0AAI PIC  X(0005).
      *    -------------------------------
           05  LINE20L PIC S9(0004) COMP.
           05  LINE20F PIC  X(0001).
           05  FILLER REDEFINES LINE20F.
               10  LINE20A PIC  X(0001).
           05  LINE20I PIC  X(0060).
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
           05  PFENTERI PIC  99.
      *    -------------------------------
           05  PF3DEVL PIC S9(0004) COMP.
           05  PF3DEVF PIC  X(0001).
           05  FILLER REDEFINES PF3DEVF.
               10  PF3DEVA PIC  X(0001).
           05  PF3DEVI PIC  X(0020).
       01  EL6565AO REDEFINES EL6565AI.
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
           05  PCTO PIC  Z.999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLANO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M01O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE1O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR1AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M01AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE2O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M02O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE3O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR2AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M02AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE4O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M03O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE5O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR3AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M03AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE6O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M04O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE7O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR4AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M04AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE8O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M05O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE9O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR5AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M05AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE10O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M06O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE11O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR6AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M06AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE12O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M07O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE13O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR7AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M07AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE14O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M08O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE15O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR8AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M08AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE16O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M09O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE17O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YR9AO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M09AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE18O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YRAO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M0AO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE19O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YRAAO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M0AAO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE20O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF3DEVO PIC  X(0020).
      *    -------------------------------
00177
00178  01  FILLER REDEFINES EL6565AI.
00179      05  FILLER             PIC X(66).
00180      05  SCREEN-TABLE       OCCURS 20 TIMES
00181                             INDEXED BY ST-INDX.
00182          10  FILLER         PIC XXX.
00183          10  YEAR           PIC 99.
00184          10  YEAR-1 REDEFINES YEAR
00185                             PIC XX.
00186          10  FILLER         PIC XXX.
00187          10  MONTH          PIC X(5).
00188          10  FILLER         PIC XXX.
00189          10  RATE-TABLE     OCCURS 6 TIMES
00190                             INDEXED BY RT-INDX.
00191              15  RATE1      PIC Z(4).99999.
00192              15  RATE2 REDEFINES RATE1
00193                             PIC X(10).
00194
00195      EJECT
00196
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
00198
00199  01  DFHCOMMAREA             PIC X(1520).
00200
00201 *01 PARMLIST        COMP.
00202 *    02  FILLER              PIC S9(8).
00203 *    02  ERACCT-POINTER      PIC S9(8).
00204 *    02  ERPLAN-POINTER      PIC S9(8).
00205 *    02  ERRATE-POINTER      PIC S9(8).
00206      EJECT
00207 *                            COPY ERCACCT.
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
00208      EJECT
00209 *                            COPY ERCPLAN.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPLAN                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT PLAN MASTER FILE                       *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR THE ONLINE VSAM ACCOUNT            *
00010 *   PLAN MASTER                                                  *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER PLAN MASTER           *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 420   RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERPLAN                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = N/A                                    *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
00025
00026  01  PLAN-MASTER.
00027      12  PL-RECORD-ID                      PIC XX.
00028          88  VALID-PL-ID                      VALUE 'PL'.
00029
00030      12  PL-CONTROL-PRIMARY.
00031          16  PL-COMPANY-CD                 PIC X.
00032          16  PL-MSTR-CNTRL.
00033              20  PL-CONTROL-A.
00034                  24  PL-CARRIER            PIC X.
00035                  24  PL-GROUPING           PIC X(6).
00036                  24  PL-STATE              PIC XX.
00037                  24  PL-ACCOUNT            PIC X(10).
00038              20  PL-BENEFIT-TYPE           PIC X.
00039              20  PL-BENEFIT-CODE           PIC XX.
00040              20  PL-REVISION-NO            PIC X(3).
00041
00042      12  PL-CONTROL-BY-VAR-GRP.
00043          16  PL-COMPANY-CD-A1              PIC X.
00044          16  PL-VG-CARRIER                 PIC X.
00045          16  PL-VG-GROUPING                PIC X(6).
00046          16  PL-VG-STATE                   PIC XX.
00047          16  PL-VG-ACCOUNT                 PIC X(10).
00048          16  PL-BENEFIT-TYPE-A1            PIC X.
00049          16  PL-BENEFIT-CODE-A1            PIC XX.
00050          16  PL-REVISION-NO-A1             PIC 999.
00051
00052      12  PL-MAINT-INFORMATION.
00053          16  PL-LAST-MAINT-DT              PIC XX.
00054          16  PL-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00055          16  PL-LAST-MAINT-USER            PIC X(4).
00056          16  FILLER                        PIC XX.
00057
00058      12  PL-LIABILITY-LIMITS.
00059          16  PL-ATT-AGE                    PIC S99        COMP-3.
00060          16  PL-L-LIMITS        OCCURS 8 TIMES.
00061              20  PL-LM-AGE                 PIC S99        COMP-3.
00062              20  PL-LM-DUR                 PIC S999       COMP-3.
00063              20  PL-LM-MOA                 PIC S9(4)      COMP-3.
00064              20  PL-LM-AMT                 PIC S9(6)      COMP-3.
00065
00066      12  FILLER                            PIC X(30).
00067
00068      12  PL-GL-ACCOUNT-NOS.
00069          16  PL-PREMIUM-GL                 PIC X(8).
00070          16  PL-COMM-GL                    PIC X(8).
00071          16  PL-CLAIM-GL                   PIC X(8).
00072      12  FILLER                            PIC X(24).
00073      12  PL-TOLERANCES.
00074          16  PL-TOL-PREM-AMT               PIC S999V99    COMP-3.
00075          16  PL-TOL-REF-AMT                PIC S999V99    COMP-3.
00076          16  PL-TOL-CLM-AMT                PIC S999V99    COMP-3.
00077          16  PL-TOL-PREM-PCT               PIC S9V9999    COMP-3.
00078          16  PL-TOL-REF-PCT                PIC S9V9999    COMP-3.
00079          16  PL-TOL-CLM-PCT                PIC S9V9999    COMP-3.
00080      12  PL-OVER-SHORT.
00081          16  PL-OVER-SHORT-AMT             PIC S999V99    COMP-3.
00082          16  PL-OVER-SHORT-PCT             PIC S9V9(4)    COMP-3.
00083      12  FILLER                            PIC X(24).
00084      12  PLAN-MISC.
00085          16  PL-POLICY-FEE                 PIC S9(3)V99   COMP-3.
00086          16  PL-STATE-TAX                  PIC S9V9999    COMP-3.
00087          16  PL-IG                         PIC X.
00088          16  PL-RETRO-RET                  PIC S9V9999    COMP-3.
00089          16  PL-POLICY-FORM                PIC X(12).
00090          16  PL-EDIT-FOR-FORM              PIC X(01).
00091          16  PL-DEV-CODE                   PIC XXX.
00092          16  PL-DEV-PCT                    PIC S9V9(6)    COMP-3.
00093          16  PL-CALC-METHOD                PIC X.
00094          16  PL-BENEFIT-GROUP              PIC X(05).
00095          16  PL-SALES-TAX                  PIC S9V9999    COMP-3.
00096
00097          16  FILLER                        PIC X(99).
00098 ******************************************************************
00210      EJECT
00211 *                            COPY ERCRATE.
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
00212      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ACCOUNT-MASTER
                                PLAN-MASTER RATE-RECORD.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6565' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00214      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00215      MOVE '5'                   TO DC-OPTION-CODE.
00216      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00217      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE.
00218      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE.
00219
00220      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00221
00222  0100-START.
00223      IF EIBCALEN = 0
00224          GO TO 8800-UNAUTHORIZED-ACCESS.
00225
00226      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00227          MOVE PI-CALLING-PROGRAM TO PI-SAVE-CALLING-PGM
00228          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00229              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00230              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00231              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00232              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00233              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00234              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00235              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00236              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00237          ELSE
00238              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00239              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00240              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00241              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00242              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00243              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00244              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00245              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00246
00247      
      * EXEC CICS HANDLE CONDITION
00248 *        PGMIDERR (9600-PGMID-ERROR)
00249 *        ERROR    (9990-ABEND)
00250 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00002380' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032333830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00251
00252      IF EIBTRNID NOT = TRANS-ID
00253          MOVE LOW-VALUES TO EL6565AI
00254          MOVE +1         TO PI-SUB
00255                             PI-YEAR
00256          MOVE 'X'        TO PI-1ST-TIME
00257          GO TO 2000-INITIAL-DISPLAY.
00258
00259      EJECT
00260      IF EIBAID = DFHCLEAR
00261          GO TO 9400-CLEAR.
00262
00263  0200-RECEIVE.
00264      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00265          MOVE ER-0008 TO EMI-ERROR
00266          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00267          MOVE -1   TO STATEL
00268          GO TO 8200-SEND-DATAONLY.
00269
00270      
      * EXEC CICS RECEIVE
00271 *        MAP    (WS-MAPNAME)
00272 *        MAPSET (WS-MAPSET-NAME)
00273 *        INTO   (EL6565AI)
00274 *    END-EXEC.
           MOVE LENGTH OF
            EL6565AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002403' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAPNAME, 
                 EL6565AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00275
00276      IF PFENTERL = 0
00277          GO TO 0300-CHECK-PFENTERS.
00278
00279      IF EIBAID NOT = DFHENTER
00280          MOVE ER-0004 TO EMI-ERROR
00281          GO TO 0310-INPUT-ERROR.
00282
00283      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
00284          MOVE PF-VALUES (PFENTERI) TO EIBAID
00285          GO TO 0300-CHECK-PFENTERS
00286      ELSE
00287          MOVE ER-0029 TO EMI-ERROR
00288          GO TO 0310-INPUT-ERROR.
00289
00290      EJECT
00291
00292  0300-CHECK-PFENTERS.
00293      IF EIBAID = DFHPF23
00294          GO TO 8810-PF23.
00295
00296      IF EIBAID = DFHPF24
00297          GO TO 9200-RETURN-MAIN-MENU.
00298
00299      IF EIBAID = DFHPF1
00300          GO TO 5300-PAGE-FORWARD.
00301
00302      IF EIBAID = DFHPF2
00303          GO TO 5400-PAGE-BACKWARD.
00304
00305      IF EIBAID = DFHPF3
00306          MOVE 'X'            TO WS-DEV-RATES
00307          GO TO 1000-EDIT.
00308
00309      IF EIBAID = DFHENTER
00310          GO TO 1000-EDIT.
00311
00312      MOVE ER-0029 TO EMI-ERROR.
00313
00314  0310-INPUT-ERROR.
00315      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00316      MOVE AL-UNBON TO PFENTERA.
00317      MOVE -1       TO PFENTERL.
00318
00319      GO TO 8200-SEND-DATAONLY.
00320
00321      EJECT
00322
00323  1000-EDIT.
00324      MOVE PI-COMPANY-CD             TO PI-RATE-COMPANY-CD.
00325
00326      IF STATEL GREATER ZERO
00327          MOVE STATEI                TO PI-RATE-CODE
00328          MOVE AL-UANON              TO STATEA.
00329
00330      IF CLASSL GREATER ZERO
00331          MOVE CLASSI                TO PI-RATE-CLASS
00332          MOVE AL-UANON              TO CLASSA.
00333
00334      IF DEVL GREATER ZERO
00335          MOVE DEVI                  TO PI-RATE-DEV
00336          MOVE AL-UANON              TO DEVA.
00337
00338      MOVE +1                        TO WS-DEV-PCT.
00339
00340      IF PCTL GREATER ZERO
00341         MOVE PCTI                   TO DEEDIT-FIELD
00342         PERFORM 8600-DEEDIT
00343         IF DEEDIT-FIELD-V6 NUMERIC
00344            MOVE DEEDIT-FIELD-V6   TO WS-DEV-PCT
00345                                      PCTO
00346            MOVE AL-UANON          TO PCTA
00347         ELSE
00348            MOVE -1                TO PCTL
00349            MOVE AL-UABON          TO PCTA
00350            MOVE ER-2263           TO EMI-ERROR
00351            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00352
00353      IF TYPEL GREATER ZERO
00354          MOVE TYPEI                 TO PI-RATE-L-AH
00355          MOVE AL-UANON              TO TYPEA.
00356
00357      IF PLANL GREATER ZERO
00358          MOVE PLANI                 TO PI-RATE-LAH-NUM
00359          MOVE AL-UANON              TO PLANA.
00360
00361      MOVE '99999999999'             TO PI-RATE-LIMITS.
00362      MOVE 99999999999               TO PI-RATE-EXPIRY-DATE.
00363
00364      
      * EXEC CICS HANDLE CONDITION
00365 *        NOTOPEN  (9990-ABEND)
00366 *        NOTFND   (8880-NOT-FOUND)
00367 *    END-EXEC.
      *    MOVE '"$JI                  ! # #00002497' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00368
00369      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.
00370
00371      MOVE +1                     TO PI-SUB
00372                                     PI-YEAR.
00373      GO TO 3000-DISPLAY-RATE.
00374
00375  EJECT
00376  2000-INITIAL-DISPLAY.
00377      MOVE +1                        TO PI-AH-DEV-PCT
00378                                        PI-LF-DEV-PCT
00379                                        WS-DEV-PCT.
00380
00381      IF PI-SAVE-CALLING-PGM NOT = WS-CALLED-FROM-ACCT
00382          GO TO 2010-NO-ACCOUNT.
00383
00384      MOVE 1 TO PI-NDX.
00385
00386  2001-GET-ACCOUNT.
00387      
      * EXEC CICS HANDLE CONDITION
00388 *        NOTOPEN  (9990-ABEND)
00389 *        NOTFND   (2525-NO-ACCOUNT)
00390 *    END-EXEC.
      *    MOVE '"$JI                  ! $ #00002520' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032353230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00391
00392      PERFORM 7850-READ-ERACCT THRU 7850-EXIT.
00393
00394      PERFORM 7950-READ-ERPLAN THRU 7950-EXIT.
00395
00396      MOVE PI-COMPANY-CD             TO PI-RATE-COMPANY-CD.
00397
00398      MOVE PI-WS-STATE               TO STATEO
00399                                        PI-RATE-CODE.
00400      MOVE ZEROS                     TO CLASSO
00401                                        PI-RATE-CLASS.
00402  2005-CHECK-TYPE.
00403      IF AM-BENEFIT-TYPE (PI-NDX) = SPACES OR ZEROS OR LOW-VALUES
00404        IF PI-NDX = +1
00405          GO TO 5200-NO-RATE
00406         ELSE
00407          GO TO 5350-ENDFILE.
00408
00409      IF PI-FIRST-TIME-THRU
00410        IF AM-BENEFIT-TYPE (PI-NDX) = PI-WS-TYPE  AND
00411           AM-BENEFIT-CODE (PI-NDX) = PI-WS-PLAN
00412              NEXT SENTENCE
00413            ELSE
00414              ADD +1 TO PI-NDX
00415              GO TO 2005-CHECK-TYPE.
00416
00417      MOVE SPACE                     TO PI-1ST-TIME.
00418
00419      MOVE AM-BENEFIT-TYPE (PI-NDX)  TO TYPEO
00420                                        PI-RATE-L-AH.
00421      MOVE AM-BENEFIT-CODE (PI-NDX)  TO PLANO
00422                                        PI-RATE-LAH-NUM.
00423
00424      MOVE ZEROS                     TO DEVO
00425                                        PI-RATE-DEV.
00426
00427      MOVE ZEROS                     TO PCTO.
00428
00429      IF AM-BENEFIT-TYPE (PI-NDX) = PI-AH-OVERRIDE-L1
00430        IF PL-DEV-PCT NUMERIC AND
00431           PL-DEV-PCT GREATER ZEROS
00432          MOVE PL-DEV-PCT            TO WS-DEV-PCT
00433        ELSE
00434          MOVE AM-AH-DEVIATION-PCT   TO WS-DEV-PCT
00435        ELSE
00436      IF AM-BENEFIT-TYPE (PI-NDX) = PI-LIFE-OVERRIDE-L1
00437        IF PL-DEV-PCT NUMERIC AND
00438           PL-DEV-PCT GREATER ZEROS
00439          MOVE PL-DEV-PCT            TO WS-DEV-PCT
00440        ELSE
00441          MOVE AM-LF-DEVIATION-PCT   TO WS-DEV-PCT.
00442
00443      IF WS-DEV-PCT = ZERO
00444          MOVE +1.0                  TO WS-DEV-PCT.
00445
00446      GO TO 2020-CONTINUE.
00447
00448  EJECT
00449  2010-NO-ACCOUNT.
00450      MOVE PI-COMPANY-CD             TO PI-RATE-COMPANY-CD.
00451
00452      MOVE PI-WS-STATE               TO STATEO
00453                                        PI-RATE-CODE
00454                                        PI-SAVE-RATE-CODE.
00455      MOVE PI-WS-CLASS               TO CLASSO
00456                                        PI-RATE-CLASS.
00457      MOVE PI-WS-DEV                 TO DEVO
00458                                        PI-RATE-DEV.
00459      MOVE PI-WS-TYPE                TO TYPEO
00460                                        PI-RATE-L-AH.
00461      MOVE PI-WS-PLAN                TO PLANO
00462                                        PI-RATE-LAH-NUM.
00463
00464  2020-CONTINUE.
00465      MOVE '99999999999'             TO PI-RATE-LIMITS.
00466      MOVE 99999999999               TO PI-RATE-EXPIRY-DATE.
00467
00468      MOVE AL-UANON                  TO STATEA
00469                                        CLASSA
00470                                        DEVA
00471                                        TYPEA
00472                                        PLANA
00473                                        PCTA.
00474
00475      MOVE +2                        TO STATEL.
00476      MOVE +2                        TO CLASSL.
00477      MOVE +3                        TO DEVL.
00478      MOVE +1                        TO TYPEL.
00479      MOVE +2                        TO PLANL.
00480      MOVE +7                        TO PCTL.
00481
00482      
      * EXEC CICS HANDLE CONDITION
00483 *        NOTOPEN  (9990-ABEND)
00484 *        NOTFND   (5200-NO-RATE)
00485 *    END-EXEC.
      *    MOVE '"$JI                  ! % #00002615' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032363135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00486
00487      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.
00488
00489      MOVE +1                     TO PI-SUB
00490                                     PI-YEAR.
00491      GO TO 3000-DISPLAY-RATE.
00492
00493  2525-NO-ACCOUNT.
00494      MOVE ER-2154            TO EMI-ERROR.
00495      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00496      GO TO 8100-SEND-INITIAL-MAP.
00497  EJECT
00498  3000-DISPLAY-RATE.
00499      MOVE PI-SUB                    TO SUB1.
00500      MOVE PI-YEAR                   TO SUB4.
00501
00502  3025-SET-UP-SCREEN.
00503      MOVE PI-COMPANY-CD             TO PI-RATE-COMPANY-CD.
00504
00505      MOVE WS-DEV-PCT                TO PCTO.
00506
00507      MOVE RT-ST-CODE                TO STATEO
00508                                        PI-RATE-CODE.
00509      MOVE RT-ST-CLASS               TO CLASSO
00510                                        PI-RATE-CLASS.
00511      MOVE RT-ST-DEV                 TO DEVO
00512                                        PI-RATE-DEV.
00513      MOVE RT-L-AH                   TO TYPEO
00514                                        PI-RATE-L-AH.
00515      MOVE RT-LAH-NUM                TO PLANO
00516                                        PI-RATE-LAH-NUM.
00517
00518      MOVE '99999999999'             TO PI-RATE-LIMITS.
00519      MOVE 99999999999               TO PI-RATE-EXPIRY-DATE.
00520
00521      MOVE +1     TO SUB2
00522                     SUB3.
00523      SET ST-INDX
00524          RT-INDX TO +1.
00525
00526  3050-SET-UP.
00527      IF ST-INDX GREATER +20
00528          MOVE +1     TO SUB1
00529                         SUB2
00530                         SUB3
00531                         SUB4
00532          SET ST-INDX
00533              RT-INDX TO +1
00534          GO TO 8100-SEND-INITIAL-MAP.
00535
00536 *    MOVE SUB4 TO YEAR (ST-INDX).
00537
00538      IF SUB4 GREATER +10
00539          MOVE SPACES       TO MONTH    (ST-INDX)
00540                               YEAR-1   (ST-INDX)
00541        ELSE
00542          IF EVEN-SUB
00543              ADD +1 TO SUB4
00544              MOVE SPACES   TO YEAR-1  (ST-INDX)
00545              MOVE '07-12'  TO MONTH   (ST-INDX)
00546            ELSE
00547              MOVE '01-06'  TO MONTH   (ST-INDX).
00548
00549      EJECT
00550  3075-SET-UP-RATES.
00551      IF SUB1 GREATER +120
00552          MOVE SPACES             TO RATE2  (ST-INDX RT-INDX)
00553      ELSE
00554          IF RT-L-AH = PI-AH-OVERRIDE-L1
00555            IF RATES-DEVIATED AND
00556               WS-DEV-PCT        GREATER ZERO AND
00557               RT-AH-RATE (SUB1) GREATER ZERO
00558              COMPUTE WS-DEV-RATE = (RT-AH-RATE (SUB1)
00559                                   * WS-DEV-PCT)
00560              MOVE WS-DEV-RATE       TO RATE1 (ST-INDX RT-INDX)
00561              MOVE AL-UABON          TO PF3DEVA
00562             ELSE
00563              MOVE RT-AH-RATE (SUB1) TO RATE1 (ST-INDX RT-INDX)
00564          ELSE
00565          IF RT-L-AH = PI-LIFE-OVERRIDE-L1
00566            IF RATES-DEVIATED AND
00567               WS-DEV-PCT       GREATER ZERO AND
00568               RT-L-RATE (SUB1) GREATER ZERO
00569              COMPUTE WS-DEV-RATE = (RT-L-RATE (SUB1)
00570                                   * WS-DEV-PCT)
00571              MOVE WS-DEV-RATE       TO RATE1 (ST-INDX RT-INDX)
00572              MOVE AL-UABON          TO PF3DEVA
00573             ELSE
00574              MOVE RT-L-RATE (SUB1)  TO RATE1 (ST-INDX RT-INDX).
00575
00576      ADD +1 TO SUB1
00577                SUB2.
00578      SET RT-INDX UP BY +1.
00579
00580      IF RT-INDX GREATER +6
00581          SET RT-INDX TO +1
00582          SET ST-INDX UP BY +1
00583          ADD +1  TO  SUB3
00584          GO TO 3050-SET-UP
00585      ELSE
00586          GO TO 3075-SET-UP-RATES.
00587
00588  3099-EXIT.
00589       EXIT.
00590      EJECT
00591  5200-NO-RATE.
00592      IF PI-SAVE-CALLING-PGM = WS-CALLED-FROM-ACCT
00593          MOVE ER-2154            TO EMI-ERROR
00594          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00595          GO TO 8100-SEND-INITIAL-MAP.
00596
00597  5300-PAGE-FORWARD.
00598      IF PI-SAVE-CALLING-PGM = WS-CALLED-FROM-ACCT
00599          ADD +1 TO PI-NDX
00600          IF PI-NDX GREATER +20
00601              GO TO 5350-ENDFILE
00602           ELSE
00603              GO TO 2001-GET-ACCOUNT.
00604
00605      MOVE SPACES                TO PI-ERRATE-EOF-SW
00606                                    PI-STATE-FOUND-SW.
00607
00608      
      * EXEC CICS HANDLE CONDITION
00609 *        ENDFILE  (5350-ENDFILE)
00610 *        NOTFND   (5350-ENDFILE)
00611 *    END-EXEC.
      *    MOVE '"$''I                  ! & #00002741' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032373431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00612
00613      MOVE PI-COMPANY-CD         TO PI-RATE-COMPANY-CD.
00614      MOVE PI-ERRATE-KEY         TO WS-SAVE-RATE-KEY.
00615
00616      PERFORM 6000-START-BROWSE THRU 6000-EXIT.
00617
00618  EJECT
00619  5305-READ-NEXT.
00620      
      * EXEC CICS HANDLE CONDITION
00621 *        ENDFILE  (5350-ENDFILE)
00622 *        NOTFND   (5350-ENDFILE)
00623 *    END-EXEC.
      *    MOVE '"$''I                  ! '' #00002753' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303032373533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00624
00625      PERFORM 6050-READNEXT     THRU 6050-EXIT.
00626
00627      IF ERRATE-EOF
00628          MOVE ER-0130            TO EMI-ERROR
00629          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00630          GO TO 8100-SEND-INITIAL-MAP.
00631
00632      IF STATE-NOT-FOUND
00633          MOVE ER-2154            TO EMI-ERROR
00634          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00635          GO TO 8100-SEND-INITIAL-MAP.
00636
00637      MOVE +1                     TO PI-SUB
00638                                     PI-YEAR.
00639      GO TO 3000-DISPLAY-RATE.
00640
00641      EJECT
00642  5350-ENDFILE.
00643      IF BROWSE-STARTED
00644          PERFORM 6500-ENDBROWSE THRU 6500-EXIT.
00645
00646      MOVE ER-0130            TO EMI-ERROR.
00647      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00648      SUBTRACT +1 FROM PI-NDX.
00649      GO TO 8200-SEND-DATAONLY.
00650
00651  EJECT
00652  5400-PAGE-BACKWARD.
00653      IF PI-SAVE-CALLING-PGM = WS-CALLED-FROM-ACCT
00654         IF PI-NDX LESS +2
00655             GO TO 5500-ENDFILE
00656          ELSE
00657             SUBTRACT +1 FROM PI-NDX
00658             GO TO 2001-GET-ACCOUNT.
00659
00660      MOVE SPACES             TO PI-ERRATE-EOF-SW.
00661      MOVE PI-COMPANY-CD      TO PI-RATE-COMPANY-CD.
00662
00663      
      * EXEC CICS HANDLE CONDITION
00664 *        ENDFILE  (5500-ENDFILE)
00665 *        NOTFND   (5500-ENDFILE)
00666 *    END-EXEC.
      *    MOVE '"$''I                  ! ( #00002796' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303032373936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00667
00668      PERFORM 6000-START-BROWSE THRU 6000-EXIT.
00669
00670      PERFORM 6800-READ-PREV    THRU 6800-EXIT.
00671
00672  5450-READ-PREV.
00673      
      * EXEC CICS HANDLE CONDITION
00674 *        ENDFILE  (5500-ENDFILE)
00675 *        NOTFND   (5500-ENDFILE)
00676 *    END-EXEC.
      *    MOVE '"$''I                  ! ) #00002806' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303032383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00677
00678      PERFORM 6800-READ-PREV    THRU 6800-EXIT.
00679
00680      IF PI-COMPANY-CD NOT = RT-COMPANY-CD
00681          GO TO 5500-ENDFILE.
00682
00683      MOVE +1                     TO PI-SUB
00684                                     PI-YEAR.
00685      GO TO 3000-DISPLAY-RATE.
00686
00687      EJECT
00688  5500-ENDFILE.
00689      IF BROWSE-STARTED
00690          PERFORM 6500-ENDBROWSE    THRU 6500-EXIT.
00691
00692      MOVE ER-2238            TO EMI-ERROR.
00693      MOVE -1                 TO STATEL.
00694      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00695      GO TO 8200-SEND-DATAONLY.
00696
00697      EJECT
00698  6000-START-BROWSE.
00699      
      * EXEC CICS STARTBR
00700 *        DATASET  (RATE-FILE-ID)
00701 *        RIDFLD   (PI-ERRATE-KEY)
00702 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002832' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00703
00704      MOVE 'Y'                   TO PI-BROWSE-SW.
00705
00706  6000-EXIT.
00707       EXIT.
00708
00709  6050-READNEXT.
00710      
      * EXEC CICS READNEXT
00711 *        DATASET  (RATE-FILE-ID)
00712 *        SET      (ADDRESS OF RATE-RECORD)
00713 *        RIDFLD   (PI-ERRATE-KEY)
00714 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002843' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383433' TO DFHEIV0(25:11)
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
           
00715
00716      IF PI-COMPANY-CD NOT = RT-COMPANY-CD
00717          MOVE 'Y'            TO PI-ERRATE-EOF-SW.
00718
00719      IF (PI-SAVE-CALLING-PGM = WS-CALL-FROM-STATE  OR
00720                                WS-CALL-FROM-STATE-ALT)
00721           AND
00722          PI-SAVE-RATE-CODE NOT = RT-ST-CODE
00723              MOVE 'N'       TO PI-STATE-FOUND-SW.
00724
00725      IF PI-ERRATE-KEY = WS-SAVE-RATE-KEY
00726          GO TO 6050-READNEXT.
00727
00728      MOVE RT-CONTROL-PRIMARY TO PI-ERRATE-KEY.
00729
00730  6050-EXIT.
00731       EXIT.
00732
00733      EJECT
00734  6500-ENDBROWSE.
00735      
      * EXEC CICS ENDBR
00736 *        DATASET  (RATE-FILE-ID)
00737 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002868' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00738
00739      MOVE 'N'                TO PI-BROWSE-SW.
00740
00741  6500-EXIT.
00742       EXIT.
00743
00744      EJECT
00745  6800-READ-PREV.
00746      
      * EXEC CICS READPREV
00747 *        DATASET  (RATE-FILE-ID)
00748 *        SET      (ADDRESS OF RATE-RECORD)
00749 *        RIDFLD   (PI-ERRATE-KEY)
00750 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00002879' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383739' TO DFHEIV0(25:11)
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
           
00751
00752  6800-EXIT.
00753       EXIT.
00754
00755  EJECT
00756
00757  7650-READ-ERRATE.
00758      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.
00759
00760      
      * EXEC CICS READ
00761 *        DATASET  (RATE-FILE-ID)
00762 *        SET      (ADDRESS OF RATE-RECORD)
00763 *        RIDFLD   (PI-ERRATE-KEY)
00764 *    END-EXEC.
      *    MOVE '&"S        E          (   #00002893' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383933' TO DFHEIV0(25:11)
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
           
00765
00766  7650-EXIT.
00767       EXIT.
00768      EJECT
00769
00770  7850-READ-ERACCT.
00771      
      * EXEC CICS READ
00772 *        DATASET  (ACCT-FILE-ID)
00773 *        SET      (ADDRESS OF ACCOUNT-MASTER)
00774 *        RIDFLD   (PI-ACCT-KEY)
00775 *    END-EXEC.
      *    MOVE '&"S        E          (   #00002904' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00776
00777  7850-EXIT.
00778       EXIT.
00779
00780  7950-READ-ERPLAN.
00781      
      * EXEC CICS READ
00782 *        DATASET  (PLAN-FILE-ID)
00783 *        SET      (ADDRESS OF PLAN-MASTER)
00784 *        RIDFLD   (PI-PLAN-KEY)
00785 *    END-EXEC.
      *    MOVE '&"S        E          (   #00002914' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00786
00787  7950-EXIT.
00788       EXIT.
00789
00790      EJECT
00791
00792  8100-SEND-INITIAL-MAP.
00793      MOVE EIBTIME              TO TIME-IN.
00794      MOVE SAVE-DATE            TO RUNDATEO.
00795      MOVE TIME-OUT             TO RUNTIMEO.
00796      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
00797      MOVE -1                   TO STATEL.
00798      
      * EXEC CICS SEND
00799 *        MAP   (WS-MAPNAME)
00800 *        MAPSET(WS-MAPSET-NAME)
00801 *        FROM  (EL6565AO)
00802 *        ERASE
00803 *        CURSOR
00804 *    END-EXEC.
           MOVE LENGTH OF
            EL6565AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002931' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAPNAME, 
                 EL6565AO, 
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
           
00805
00806      GO TO 9100-RETURN-TRAN.
00807
00808  8200-SEND-DATAONLY.
00809      MOVE EIBTIME              TO TIME-IN.
00810      MOVE SAVE-DATE            TO RUNDATEO.
00811      MOVE TIME-OUT             TO RUNTIMEO.
00812      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
00813      MOVE -1                   TO STATEL.
00814      
      * EXEC CICS SEND
00815 *        MAP   (WS-MAPNAME)
00816 *        MAPSET(WS-MAPSET-NAME)
00817 *        FROM  (EL6565AO)
00818 *        DATAONLY
00819 *        CURSOR
00820 *    END-EXEC.
           MOVE LENGTH OF
            EL6565AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002947' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAPNAME, 
                 EL6565AO, 
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
           
00821
00822      GO TO 9100-RETURN-TRAN.
00823
00824      EJECT
00825  8300-SEND-TEXT.
00826      
      * EXEC CICS SEND TEXT
00827 *        FROM  (LOGOFF-TEXT)
00828 *        LENGTH(LOGOFF-LENGTH)
00829 *        ERASE
00830 *        FREEKB
00831 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002959' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393539' TO DFHEIV0(25:11)
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
           
00832
00833      
      * EXEC CICS RETURN
00834 *    END-EXEC.
      *    MOVE '.(                    &   #00002966' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00835
00836  8600-DEEDIT.
00837      
      * EXEC CICS BIF DEEDIT
00838 *         FIELD (DEEDIT-FIELD)
00839 *         LENGTH(15)
00840 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002970' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00841
00842  8800-UNAUTHORIZED-ACCESS.
00843      MOVE UNACCESS-MSG TO LOGOFF-MSG.
00844      GO TO 8300-SEND-TEXT.
00845
00846  8810-PF23.
00847      MOVE EIBAID   TO PI-ENTRY-CD-1.
00848      MOVE XCTL-005 TO PGM-NAME.
00849      GO TO 9300-XCTL.
00850
00851  8880-NOT-FOUND.
00852      MOVE ER-0142 TO EMI-ERROR.
00853      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00854      MOVE -1        TO STATEL.
00855
00856      GO TO 8200-SEND-DATAONLY.
00857
00858      EJECT
00859  9000-RETURN-CICS.
00860      
      * EXEC CICS RETURN
00861 *    END-EXEC.
      *    MOVE '.(                    &   #00002993' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00862
00863  9100-RETURN-TRAN.
00864      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
00865      MOVE '656C'               TO PI-CURRENT-SCREEN-NO.
00866      
      * EXEC CICS RETURN
00867 *        TRANSID (TRANS-ID)
00868 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
00869 *        LENGTH  (WS-COMM-LENGTH)
00870 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002999' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00871
00872  9200-RETURN-MAIN-MENU.
00873      MOVE XCTL-626 TO PGM-NAME.
00874      GO TO 9300-XCTL.
00875
00876  9300-XCTL.
00877      
      * EXEC CICS XCTL
00878 *        PROGRAM (PGM-NAME)
00879 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
00880 *        LENGTH  (WS-COMM-LENGTH)
00881 *    END-EXEC.
      *    MOVE '.$C                   $   #00003010' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00882
00883      EJECT
00884  9400-CLEAR.
00885      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
00886      GO TO 9300-XCTL.
00887
00888  9500-PF12.
00889      MOVE XCTL-010 TO PGM-NAME.
00890      GO TO 9300-XCTL.
00891
00892  9600-PGMID-ERROR.
00893      
      * EXEC CICS HANDLE CONDITION
00894 *        PGMIDERR(8300-SEND-TEXT)
00895 *    END-EXEC.
      *    MOVE '"$L                   ! * #00003026' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303033303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00896
00897      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.
00898      MOVE ' '          TO PI-ENTRY-CD-1.
00899      MOVE XCTL-005     TO PGM-NAME.
00900      MOVE PGM-NAME     TO LOGOFF-PGM.
00901      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
00902      GO TO 9300-XCTL.
00903
00904      EJECT
00905  9700-LINK-DATE-CONVERT.
00906      MOVE LINK-ELDATCV TO PGM-NAME.
00907
00908      
      * EXEC CICS LINK
00909 *        PROGRAM (PGM-NAME)
00910 *        COMMAREA(DATE-CONVERSION-DATA)
00911 *        LENGTH  (DC-COMM-LENGTH)
00912 *    END-EXEC.
      *    MOVE '."C                   ''   #00003041' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00913
00914  9700-EXIT.
00915       EXIT.
00916
00917  9900-ERROR-FORMAT.
00918      IF NOT EMI-ERRORS-COMPLETE
00919          MOVE LINK-001 TO PGM-NAME
00920          
      * EXEC CICS LINK
00921 *            PROGRAM (PGM-NAME)
00922 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
00923 *            LENGTH  (EMI-COMM-LENGTH)
00924 *        END-EXEC.
      *    MOVE '."C                   ''   #00003053' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00925
00926  9900-EXIT.
00927       EXIT.
00928
00929      EJECT
00930  9990-ABEND.
00931      MOVE LINK-004               TO PGM-NAME.
00932      MOVE DFHEIBLK               TO EMI-LINE1.
00933      
      * EXEC CICS LINK
00934 *        PROGRAM   (PGM-NAME)
00935 *        COMMAREA  (EMI-LINE1)
00936 *        LENGTH    (72)
00937 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00003066' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00938
00939      GO TO 8200-SEND-DATAONLY.
00940

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6565' TO DFHEIV1
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
               GO TO 9990-ABEND,
                     8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 9990-ABEND,
                     2525-NO-ACCOUNT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 9990-ABEND,
                     5200-NO-RATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 5350-ENDFILE,
                     5350-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 5350-ENDFILE,
                     5350-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 5500-ENDFILE,
                     5500-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 5500-ENDFILE,
                     5500-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6565' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
