00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6312.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 01/12/96 10:01:33.
00007 *                            VMOD=2.015.
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
00024 *REMARKS.
00025 *        TRANSACTION - EXB2 - REVIEW AND CORRECTION
00026 *                             DISPLAY EDIT CRITERIA.
00027  EJECT
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6312 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.015 ************'.
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
00036
00037 *    COPY ELCSCRTY.
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
00038  EJECT
00039  01  STANDARD-AREAS.
00040      12  MAP-NAME            PIC X(8)    VALUE 'EL631E'.
00041      12  MAPSET-NAME         PIC X(8)    VALUE 'EL6312S'.
00042      12  SCREEN-NUMBER       PIC X(4)    VALUE '631E'.
00043      12  TRANS-ID            PIC X(4)    VALUE 'EXB2'.
00044      12  THIS-PGM            PIC X(8)    VALUE 'EL6312'.
00045      12  PGM-NAME            PIC X(8).
00046      12  TIME-IN             PIC S9(7).
00047      12  TIME-OUT-R  REDEFINES TIME-IN.
00048          16  FILLER          PIC X.
00049          16  TIME-OUT        PIC 99V99.
00050          16  FILLER          PIC X(2).
00051      12  XCTL-005            PIC X(8)    VALUE 'EL005'.
00052      12  XCTL-010            PIC X(8)    VALUE 'EL010'.
00053      12  XCTL-626            PIC X(8)    VALUE 'EL626'.
00054      12  XCTL-6311           PIC X(8)    VALUE 'EL6311'.
00055      12  LINK-001            PIC X(8)    VALUE 'EL001'.
00056      12  LINK-004            PIC X(8)    VALUE 'EL004'.
00057      12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00058      12  WS-PHONE.
00059          16  WS-PH1              PIC XXX.
00060          16  WS-PH2              PIC XXX.
00061          16  WS-PH3              PIC XXXX.
00062      12  WS-PHONE-NUM REDEFINES WS-PHONE PIC 9(10).
00063      12  WS-DEV-RT               PIC S9V9(6) COMP-3.
00064
00065  01  ERROR-MESSAGES.
00066      12  ER-0004                 PIC X(4)  VALUE '0004'.
00067      12  ER-0008                 PIC X(4)  VALUE '0008'.
00068      12  ER-0029                 PIC X(4)  VALUE '0029'.
00069  EJECT
00070 *    COPY ELCDATE.
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
00071  EJECT
00072 *    COPY ELCLOGOF.
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
00073  EJECT
00074 *    COPY ELCATTR.
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
00075  EJECT
00076 *    COPY ELCEMIB.
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
00077  EJECT
00078 *    COPY ELCINTF.
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
00079      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00080 *    COPY ELC631PI.
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
00119              20  FILLER                       PIC X(7).
00120
00081         16  FILLER          PIC X(94).
00082  EJECT
00083  01  EDIT-CRITERIA-AREA.
00084 *    COPY ELCEDITC.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEDITC                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *       THIS COPYBOOK IS USED BY EL6311, EL6312,                 *
00008 *                     EL6313, EL050 AND EL517.                   *
00009 *                                                                *
00010 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
122002******************************************************************
00011
00012      12  EDIT-CRITERIA-DATA.
00013          16  EC-CO-MONTH-END-DT       PIC XX.
00014          16  EC-CO-TOL-PREM           PIC S9(3)V99  COMP-3.
00015          16  EC-CO-TOL-PREM-PCT       PIC S9V9(4)   COMP-3.
00016          16  EC-CO-TOL-REFUND         PIC S9(3)V99  COMP-3.
00017          16  EC-CO-TOL-REFUND-PCT     PIC S9V9(4)   COMP-3.
00018          16  EC-CO-MIN-AGE            PIC S9(3)     COMP-3.
00019          16  EC-CO-MIN-PREMIUM        PIC S9(5)V99  COMP-3.
00020          16  EC-CO-MIN-TERM           PIC S9(3)     COMP-3.
00021          16  EC-CO-MAX-TERM           PIC S9(3)     COMP-3.
00022          16  EC-CO-REM-TERM-CALC      PIC X.
00023                88  EC-CO-EARN-AFTER-15TH           VALUE '1'.
00024                88  EC-CO-EARN-ON-HALF-MO           VALUE '2'.
00025                88  EC-CO-EARN-ON-1ST-DAY           VALUE '3'.
00026                88  EC-CO-EARN-ON-FULL-MO           VALUE '4'.
00027
00028          16  EC-BR-LF-EARNINGS-CALC    PIC X.
00029                88  EC-BR-LF-EARNING-NOT-USED        VALUE SPACE.
00030                88  EC-BR-LF-EARN-BY-R78             VALUE '1'.
00031                88  EC-BR-LF-EARN-BY-PRO-RATA        VALUE '2'.
00032                88  EC-BR-LF-EARN-AS-TEXAS           VALUE '4'.
00033                88  EC-BR-LF-EARN-AS-FARM-PLAN       VALUE '4'.
00034                88  EC-BR-LF-EARN-IS-NET-PAY         VALUE '5'.
00035                88  EC-BR-LF-EARN-ANTICIPATION       VALUE '6'.
00036                88  EC-BR-LF-EARN-MEAN               VALUE '8'.
00037                88  EC-BR-LF-EARN-SUM-OF-DIGIT       VALUE '9'.
00038                88  EC-BR-LF-EARN-REG-BALLOON        VALUE 'B'.
00039
00040          16  EC-BR-AH-EARNINGS-CALC    PIC X.
00041                88  EC-BR-AH-EARNING-NOT-USED        VALUE SPACE.
00042                88  EC-BR-AH-EARN-BY-R78             VALUE '1'.
00043                88  EC-BR-AH-EARN-BY-PRO-RATA        VALUE '2'.
00044                88  EC-BR-AH-EARN-AS-CALIF           VALUE '3'.
00045                88  EC-BR-AH-EARN-IS-NET-PAY         VALUE '5'.
00046                88  EC-BR-AH-EARN-ANTICIPATION       VALUE '6'.
00047                88  EC-BR-AH-EARN-MEAN               VALUE '8'.
00048                88  EC-BR-AH-EARN-REG-BALLOON        VALUE 'B'.
00049
00050          16  EC-BR-AH-SPECIAL-CALC-CD  PIC X.
00051                  88  EC-CF-AH-OUTSTANDING-BAL         VALUE 'O'.
00052                  88  EC-CF-AH-CRITICAL-PERIOD         VALUE 'C'.
00053
00054          16  EC-BR-LF-SPECIAL-CALC-CD  PIC X.
00055                  88  EC-CF-LF-OUTSTANDING-BAL         VALUE 'O'.
00056                  88  EC-CF-LF-TRUNCATED-LIFE-0        VALUE 'T'.
00057                  88  EC-CF-LF-TRUNCATED-LIFE-1        VALUE 'U'.
00058                  88  EC-CF-LF-TRUNCATED-LIFE-2        VALUE 'V'.
00059                  88  EC-CF-LF-ALTERNATE-NET-PAY       VALUE 'A'.
00060                  88  EC-CF-LF-NET-PAY-SIMPLE          VALUE 'S'.
00061                  88  EC-CF-LF-CRITICAL-PERIOD         VALUE 'C'.
00062                  88  EC-CF-LF-TERM-IN-DAYS            VALUE 'D'.
00063
00064          16  EC-BR-LF-COVERAGE-TYPE    PIC X.
00065                88  EC-BR-REDUCING                   VALUE 'R'.
00066                88  EC-BR-LEVEL                      VALUE 'L' 'P'.
00067
00068          16  EC-BR-AH-COVERAGE-TYPE    PIC X(3).
00069          16  EC-BR-LF-DESC             PIC X(10).
00070          16  EC-BR-AH-DESC             PIC X(10).
00071          16  EC-BR-LF-COMMENT          PIC X(10).
00072          16  EC-BR-AH-COMMENT          PIC X(10).
00073
00074          16  EC-BR-LF-REM-TERM-CALC    PIC X.
00075                88  EC-BR-LF-EARN-AFTER-15TH         VALUE '1'.
00076                88  EC-BR-LF-EARN-ON-HALF-MO         VALUE '2'.
00077                88  EC-BR-LF-EARN-ON-1ST-DAY         VALUE '3'.
00078                88  EC-BR-LF-EARN-ON-FULL-MO         VALUE '4'.
00079
00080          16  EC-BR-AH-REM-TERM-CALC    PIC X.
00081                88  EC-BR-AH-EARN-AFTER-15TH         VALUE '1'.
00082                88  EC-BR-AH-EARN-ON-HALF-MO         VALUE '2'.
00083                88  EC-BR-AH-EARN-ON-1ST-DAY         VALUE '3'.
00084                88  EC-BR-AH-EARN-ON-FULL-MO         VALUE '4'.
00085
00086          16  EC-BR-LF-REFUND-CALC      PIC X.
00087                  88  EC-BR-LF-REFD-BY-R78           VALUE '1'.
00088                  88  EC-BR-LF-REFD-BY-PRO-RATA      VALUE '2'.
00089                  88  EC-BR-LF-REFD-AS-TEXAS         VALUE '4'.
00090                  88  EC-BR-LF-REFD-IS-NET-PAY       VALUE '5'.
00091                  88  EC-BR-LF-REFD-ANTICIPATION     VALUE '6'.
00092                  88  EC-BR-LF-REFD-IS-MEAN          VALUE '8'.
00093                  88  EC-BR-LF-REFD-IS-SUM-OF-DIGIT  VALUE '9'.
00094
00095          16  EC-BR-AH-REFUND-CALC      PIC X.
00096                  88  EC-BR-AH-REFD-BY-R78           VALUE '1'.
00097                  88  EC-BR-AH-REFD-BY-PRO-RATA      VALUE '2'.
00098                  88  EC-BR-AH-REFD-AS-CALIF         VALUE '3'.
00099                  88  EC-BR-AH-REFD-AS-TEXAS         VALUE '4'.
00100                  88  EC-BR-AH-REFD-IS-NET-PAY       VALUE '5'.
00101                  88  EC-BR-AH-REFD-ANTICIPATION     VALUE '6'.
00102                  88  EC-BR-AH-REFD-IS-MEAN          VALUE '8'.
00103                  88  EC-BR-AH-REFD-SUM-OF-DIGIT     VALUE '9'.
00104
00105          16  EC-BR-LF-BEN-I-G-CD       PIC X.
00106                  88  EC-BR-LF-I-G-NOT-USED          VALUE ' '.
00107                  88  EC-BR-LF-I-G-IS-INDV           VALUE 'I'.
00108                  88  EC-BR-LF-I-G-IS-GRP            VALUE 'G'.
00109
00110          16  EC-BR-AH-BEN-I-G-CD       PIC X.
00111                  88  EC-BR-AH-I-G-NOT-USED          VALUE ' '.
00112                  88  EC-BR-AH-I-G-IS-INDV           VALUE 'I'.
00113                  88  EC-BR-AH-I-G-IS-GRP            VALUE 'G'.
00114
00115          16  EC-ST-TOL-PREM           PIC S9(3)V99  COMP-3.
00116          16  EC-ST-TOL-REFUND         PIC S9(3)V99  COMP-3.
00117          16  EC-ST-TOL-PREM-PCT       PIC S9V9(4)   COMP-3.
00118          16  EC-ST-TOL-REFUND-PCT     PIC S9V9(4)   COMP-3.
00119
00120          16  EC-ST-FST-PMT-DAYS-MAX   PIC S9(3)     COMP-3.
00121          16  EC-ST-FST-PMT-DAYS-CHG   PIC X.
00122                  88  EC-ST-EXT-NO-CHG               VALUE ' '.
00123                  88  EC-ST-EXT-CHG-LF               VALUE '1'.
00124                  88  EC-ST-EXT-CHG-AH               VALUE '2'.
00125                  88  EC-ST-EXT-CHG-LF-AH            VALUE '3'.
00126
00127          16  EC-ST-LF-REM-TERM-CALC    PIC X.
00128                88  EC-ST-LF-EARN-AFTER-15TH         VALUE '1'.
00129                88  EC-ST-LF-EARN-ON-HALF-MO         VALUE '2'.
00130                88  EC-ST-LF-EARN-ON-1ST-DAY         VALUE '3'.
00131                88  EC-ST-LF-EARN-ON-FULL-MO         VALUE '4'.
00132                88  EC-ST-LF-EARN-WITH-NO-DAYS       VALUE '5'.
00133
00134          16  EC-ST-AH-REM-TERM-CALC    PIC X.
00135                88  EC-ST-AH-EARN-AFTER-15TH         VALUE '1'.
00136                88  EC-ST-AH-EARN-ON-HALF-MO         VALUE '2'.
00137                88  EC-ST-AH-EARN-ON-1ST-DAY         VALUE '3'.
00138                88  EC-ST-AH-EARN-ON-FULL-MO         VALUE '4'.
00139                88  EC-ST-AH-EARN-WITH-NO-DAYS       VALUE '5'.
00140
00141          16  EC-ST-LF-REFUND-CALC      PIC X.
00142                  88  EC-ST-LF-REFD-BY-R78           VALUE '1'.
00143                  88  EC-ST-LF-REFD-BY-PRO-RATA      VALUE '2'.
00144                  88  EC-ST-LF-REFD-AS-CALIF         VALUE '3'.
00145                  88  EC-ST-LF-REFD-AS-TEXAS         VALUE '4'.
00146                  88  EC-ST-LF-REFD-IS-NET-PAY       VALUE '5'.
00147                  88  EC-ST-LF-REFD-ANTICIPATION     VALUE '6'.
00148                  88  EC-ST-LF-REFD-IS-MEAN          VALUE '8'.
00149                  88  EC-ST-LF-REFD-IS-SUM-OF-DIGIT  VALUE '9'.
00150
00151          16  EC-ST-AH-REFUND-CALC      PIC X.
00152                  88  EC-ST-AH-REFD-BY-R78           VALUE '1'.
00153                  88  EC-ST-AH-REFD-BY-PRO-RATA      VALUE '2'.
00154                  88  EC-ST-AH-REFD-AS-CALIF         VALUE '3'.
00155                  88  EC-ST-AH-REFD-AS-TEXAS         VALUE '4'.
00156                  88  EC-ST-AH-REFD-IS-NET-PAY       VALUE '5'.
00157                  88  EC-ST-AH-REFD-ANTICIPATION     VALUE '6'.
00158                  88  EC-ST-AH-REFD-IS-MEAN          VALUE '8'.
00159                  88  EC-ST-AH-REFD-SUM-OF-DIGIT     VALUE '9'.
00160
00161          16  EC-AM-LF-TOL-PREM         PIC S9(3)V99  COMP-3.
00162          16  EC-AM-AH-TOL-PREM         PIC S9(3)V99  COMP-3.
00163          16  EC-AM-LF-TOL-REFUND       PIC S9(3)V99  COMP-3.
00164          16  EC-AM-AH-TOL-REFUND       PIC S9(3)V99  COMP-3.
00165          16  EC-AM-EXPIRATION-DT       PIC XX.
00166          16  EC-AM-EFFECTIVE-DT        PIC XX.
00167          16  EC-AM-CLASS-CD            PIC XX.
00168          16  EC-AM-LF-DEVIATION        PIC XXX.
00169          16  EC-AM-LF-DEV-PERCENT      PIC S9V9(6)  COMP-3.
00170          16  EC-AM-AH-DEVIATION        PIC XXX.
00171          16  EC-AM-AH-DEV-PERCENT      PIC S9V9(6)  COMP-3.
00172          16  EC-AM-PHONE-NO            PIC X(10).
00173
00174          16  EC-AM-LF-REFUND-CALC      PIC X.
00175                  88  EC-AM-LF-REFD-BY-R78           VALUE '1'.
00176                  88  EC-AM-LF-REFD-BY-PRO-RATA      VALUE '2'.
00177                  88  EC-AM-LF-REFD-AS-TEXAS         VALUE '4'.
00178                  88  EC-AM-LF-REFD-IS-NET-PAY       VALUE '5'.
00179                  88  EC-AM-LF-REFD-ANTICIPATION     VALUE '6'.
00180                  88  EC-AM-LF-REFD-IS-MEAN          VALUE '8'.
00181                  88  EC-AM-LF-REFD-IS-SUM-OF-DIGIT  VALUE '9'.
00182
00183          16  EC-AM-AH-REFUND-CALC      PIC X.
00184                  88  EC-AM-AH-REFD-BY-R78           VALUE '1'.
00185                  88  EC-AM-AH-REFD-BY-PRO-RATA      VALUE '2'.
00186                  88  EC-AM-AH-REFD-AS-CALIF         VALUE '3'.
00187                  88  EC-AM-AH-REFD-IS-NET           VALUE '5'.
00188                  88  EC-AM-AH-REFD-ANTICIPATION     VALUE '6'.
00189                  88  EC-AM-AH-REFD-IS-MEAN          VALUE '8'.
00190                  88  EC-AM-AH-REFD-SUM-OF-DIGIT     VALUE '9'.
00191
00192          16  EC-AM-BEN-I-G-CD          PIC X.
00193                  88  WK-AM-I-G-NOT-USED             VALUE ' '.
00194                  88  EC-AM-I-G-IS-INDV              VALUE 'I'.
00195                  88  EC-AM-I-G-IS-GRP               VALUE 'G'.
00196
00197          16  EC-AM-LF-MAX-ATT-AGE      PIC S9(3)       COMP-3.
00198          16  EC-AM-LF-MAX-AGE          PIC S9(3)       COMP-3.
00199          16  EC-AM-LF-MAX-TERM         PIC S9(3)       COMP-3.
00200          16  EC-AM-LF-MAX-TOT-BEN      PIC S9(7)V99    COMP-3.
00201          16  EC-AM-AH-MAX-ATT-AGE      PIC S9(3)       COMP-3.
00202          16  EC-AM-AH-MAX-AGE          PIC S9(3)       COMP-3.
00203          16  EC-AM-AH-MAX-TERM         PIC S9(3)       COMP-3.
00204          16  EC-AM-AH-MAX-MON-BEN      PIC S9(7)V99    COMP-3.
00205          16  EC-AM-AH-MAX-TOT-BEN      PIC S9(7)V99    COMP-3.
00206
00207          16  EC-RT-LF-MAX-ATT-AGE      PIC S9(3)       COMP-3.
00208          16  EC-RT-LF-MAX-AGE          PIC S9(3)       COMP-3.
00209          16  EC-RT-LF-MAX-TERM         PIC S9(3)       COMP-3.
00210          16  EC-RT-LF-MAX-TOT-BEN      PIC S9(7)V99    COMP-3.
00211          16  EC-RT-AH-MAX-ATT-AGE      PIC S9(3)       COMP-3.
00212          16  EC-RT-AH-MAX-AGE          PIC S9(3)       COMP-3.
00213          16  EC-RT-AH-MAX-TERM         PIC S9(3)       COMP-3.
00214          16  EC-RT-AH-MAX-MON-BEN      PIC S9(7)V99    COMP-3.
00215          16  EC-RT-AH-MAX-TOT-BEN      PIC S9(7)V99    COMP-3.
00216          16  EC-RT-AH-RATE             PIC S9(3)V9(5)  COMP-3.
00217          16  EC-RT-LF-RATE             PIC S9(3)V9(5)  COMP-3.
00218          16  EC-RT-LF-NSP-ST           PIC XX.
00219          16  EC-RT-AH-NSP-ST           PIC XX.
00220          16  EC-RT-LF-NSP-RATE         PIC S9(3)V9(5)  COMP-3.
00221          16  EC-RT-AH-NSP-RATE         PIC S9(3)V9(5)  COMP-3.
00222
00223          16  EC-CM-LF-PRIOR-STATUS     PIC X.
00224              88  EC-CM-LF-POLICY-IS-ACTIVE     VALUE '1' '3' '4'
00225                                              'M' '5' '9' '2'.
00226              88  EC-CM-LF-NORMAL-ENTRY         VALUE '1'.
00227              88  EC-CM-LF-POLICY-PENDING       VALUE '2'.
00228              88  EC-CM-LF-POLICY-IS-RESTORE    VALUE '3'.
00229              88  EC-CM-LF-CONVERSION-ENTRY     VALUE '4'.
00230              88  EC-CM-LF-POLICY-IS-REISSUE    VALUE '5'.
122002             88  EC-CM-LF-POLICY-IS-MONTHLY    VALUE 'M'.
00231              88  EC-CM-LF-LUMP-SUM-DISAB       VALUE '6'.
00232              88  EC-CM-LF-DEATH-CLAIM-APPLIED  VALUE '7'.
00233              88  EC-CM-LF-CANCEL-APPLIED       VALUE '8'.
00234              88  EC-CM-LF-IS-REIN-ONLY         VALUE '9'.
00235
00236          16  EC-CM-AH-PRIOR-STATUS     PIC X.
00237              88  EC-CM-AH-POLICY-IS-ACTIVE     VALUE '1' '3' '4'
00238                                              'M' '5' '9' '2'.
00239              88  EC-CM-AH-NORMAL-ENTRY         VALUE '1'.
00240              88  EC-CM-AH-POLICY-PENDING       VALUE '2'.
00241              88  EC-CM-AH-POLICY-IS-RESTORE    VALUE '3'.
00242              88  EC-CM-AH-CONVERSION-ENTRY     VALUE '4'.
00243              88  EC-CM-AH-POLICY-IS-REISSUE    VALUE '5'.
122002             88  EC-CM-AH-POLICY-IS-MONTHLY    VALUE 'M'.
00244              88  EC-CM-AH-LUMP-SUM-DISAB       VALUE '6'.
00245              88  EC-CM-AH-DEATH-CLAIM-APPLIED  VALUE '7'.
00246              88  EC-CM-AH-CANCEL-APPLIED       VALUE '8'.
00247              88  EC-CM-AH-IS-REIN-ONLY         VALUE '9'.
00248
00249          16  EC-CM-LF-PRIOR-REFUND     PIC S9(7)V99     COMP-3.
00250          16  EC-CM-AH-PRIOR-REFUND     PIC S9(7)V99     COMP-3.
00251          16  EC-CM-LF-CANCEL-DT        PIC XX.
00252          16  EC-CM-AH-CANCEL-DT        PIC XX.
00253          16  EC-CM-DEATH-DT            PIC XX.
00254
00255 *DMD CUSTOM CODING FIELDS BELOW
00256 *POPULATED BY CF-RATING-SWITCH AT EL050 TIME
00257          16  EC-BR-DMD-RATING-SW       PIC X.
00258                  88  EC-DMD-AH-RATING            VALUE 'Y'.
00259                  88  EC-DMD-LF-RATING            VALUE 'Y'.
00260                  88  NO-DMD-RATING               VALUE 'N'.
00261
00262          16  EC-NEW-CERT               PIC X(11).
00263          16  EC-NEW-STATE              PIC XX.
00264          16  FILLER                    PIC X.
00265 *FIELDS FOR THE REFUND OVER AND SHORT TOLERANCES.
00266          16  OVER-SHORT-EDIT-FIELDS.
00267              20  EC-CO-OVR-SHT-AMT     PIC S999V99   COMP-3.
00268              20  EC-CO-OVR-SHT-PCT     PIC S9V9(4)   COMP-3.
00269              20  EC-ST-OVR-SHT-AMT     PIC S999V99   COMP-3.
00270              20  EC-ST-OVR-SHT-PCT     PIC S9V9(4)   COMP-3.
00271              20  EC-AM-TOL-REF-PCT     PIC S9V9(4)   COMP-3.
00272              20  EC-AM-LF-OVR-SHT-AMT  PIC S999V99   COMP-3.
00273              20  EC-AM-LF-OVR-SHT-PCT  PIC S9V9(4)   COMP-3.
00274              20  EC-AM-AH-OVR-SHT-AMT  PIC S999V99   COMP-3.
00275              20  EC-AM-AH-OVR-SHT-PCT  PIC S9V9(4)   COMP-3.
00276          16  FILLER                    PIC X(71).
00085  EJECT
00086 *    COPY ELCAID.
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
00087
00088  01  FILLER    REDEFINES DFHAID.
00089      12  FILLER              PIC X(8).
00090      12  PF-VALUES           PIC X       OCCURS 24 TIMES.
00091  EJECT
00092 *   COPY EL6312S.
       01  EL631EI.
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
           05  HEADI PIC  X(0006).
      *    -------------------------------
           05  CISSTOLL PIC S9(0004) COMP.
           05  CISSTOLF PIC  X(0001).
           05  FILLER REDEFINES CISSTOLF.
               10  CISSTOLA PIC  X(0001).
           05  CISSTOLI PIC  X(0006).
      *    -------------------------------
           05  CCANTOLL PIC S9(0004) COMP.
           05  CCANTOLF PIC  X(0001).
           05  FILLER REDEFINES CCANTOLF.
               10  CCANTOLA PIC  X(0001).
           05  CCANTOLI PIC  X(0006).
      *    -------------------------------
           05  COVSAMTL PIC S9(0004) COMP.
           05  COVSAMTF PIC  X(0001).
           05  FILLER REDEFINES COVSAMTF.
               10  COVSAMTA PIC  X(0001).
           05  COVSAMTI PIC  X(0006).
      *    -------------------------------
           05  CMAGEL PIC S9(0004) COMP.
           05  CMAGEF PIC  X(0001).
           05  FILLER REDEFINES CMAGEF.
               10  CMAGEA PIC  X(0001).
           05  CMAGEI PIC  X(0002).
      *    -------------------------------
           05  CMPREML PIC S9(0004) COMP.
           05  CMPREMF PIC  X(0001).
           05  FILLER REDEFINES CMPREMF.
               10  CMPREMA PIC  X(0001).
           05  CMPREMI PIC  X(0008).
      *    -------------------------------
           05  CMENDL PIC S9(0004) COMP.
           05  CMENDF PIC  X(0001).
           05  FILLER REDEFINES CMENDF.
               10  CMENDA PIC  X(0001).
           05  CMENDI PIC  X(0008).
      *    -------------------------------
           05  CMTERML PIC S9(0004) COMP.
           05  CMTERMF PIC  X(0001).
           05  FILLER REDEFINES CMTERMF.
               10  CMTERMA PIC  X(0001).
           05  CMTERMI PIC  X(0003).
      *    -------------------------------
           05  CMXTERML PIC S9(0004) COMP.
           05  CMXTERMF PIC  X(0001).
           05  FILLER REDEFINES CMXTERMF.
               10  CMXTERMA PIC  X(0001).
           05  CMXTERMI PIC  X(0003).
      *    -------------------------------
           05  CRTERML PIC S9(0004) COMP.
           05  CRTERMF PIC  X(0001).
           05  FILLER REDEFINES CRTERMF.
               10  CRTERMA PIC  X(0001).
           05  CRTERMI PIC  X(0010).
      *    -------------------------------
           05  BTYPEL PIC S9(0004) COMP.
           05  BTYPEF PIC  X(0001).
           05  FILLER REDEFINES BTYPEF.
               10  BTYPEA PIC  X(0001).
           05  BTYPEI PIC  X(0008).
      *    -------------------------------
           05  BCOMML PIC S9(0004) COMP.
           05  BCOMMF PIC  X(0001).
           05  FILLER REDEFINES BCOMMF.
               10  BCOMMA PIC  X(0001).
           05  BCOMMI PIC  X(0010).
      *    -------------------------------
           05  BEARNML PIC S9(0004) COMP.
           05  BEARNMF PIC  X(0001).
           05  FILLER REDEFINES BEARNMF.
               10  BEARNMA PIC  X(0001).
           05  BEARNMI PIC  X(0010).
      *    -------------------------------
           05  BSPECL PIC S9(0004) COMP.
           05  BSPECF PIC  X(0001).
           05  FILLER REDEFINES BSPECF.
               10  BSPECA PIC  X(0001).
           05  BSPECI PIC  X(0010).
      *    -------------------------------
           05  BRMETHL PIC S9(0004) COMP.
           05  BRMETHF PIC  X(0001).
           05  FILLER REDEFINES BRMETHF.
               10  BRMETHA PIC  X(0001).
           05  BRMETHI PIC  X(0010).
      *    -------------------------------
           05  BIGL PIC S9(0004) COMP.
           05  BIGF PIC  X(0001).
           05  FILLER REDEFINES BIGF.
               10  BIGA PIC  X(0001).
           05  BIGI PIC  X(0001).
      *    -------------------------------
           05  BRTERML PIC S9(0004) COMP.
           05  BRTERMF PIC  X(0001).
           05  FILLER REDEFINES BRTERMF.
               10  BRTERMA PIC  X(0001).
           05  BRTERMI PIC  X(0010).
      *    -------------------------------
           05  SISSTOLL PIC S9(0004) COMP.
           05  SISSTOLF PIC  X(0001).
           05  FILLER REDEFINES SISSTOLF.
               10  SISSTOLA PIC  X(0001).
           05  SISSTOLI PIC  X(0006).
      *    -------------------------------
           05  SISSPCTL PIC S9(0004) COMP.
           05  SISSPCTF PIC  X(0001).
           05  FILLER REDEFINES SISSPCTF.
               10  SISSPCTA PIC  X(0001).
           05  SISSPCTI PIC  X(0006).
      *    -------------------------------
           05  SOVSTOLL PIC S9(0004) COMP.
           05  SOVSTOLF PIC  X(0001).
           05  FILLER REDEFINES SOVSTOLF.
               10  SOVSTOLA PIC  X(0001).
           05  SOVSTOLI PIC  X(0006).
      *    -------------------------------
           05  SCANTOLL PIC S9(0004) COMP.
           05  SCANTOLF PIC  X(0001).
           05  FILLER REDEFINES SCANTOLF.
               10  SCANTOLA PIC  X(0001).
           05  SCANTOLI PIC  X(0006).
      *    -------------------------------
           05  SOVSPCTL PIC S9(0004) COMP.
           05  SOVSPCTF PIC  X(0001).
           05  FILLER REDEFINES SOVSPCTF.
               10  SOVSPCTA PIC  X(0001).
           05  SOVSPCTI PIC  X(0006).
      *    -------------------------------
           05  SCANPCTL PIC S9(0004) COMP.
           05  SCANPCTF PIC  X(0001).
           05  FILLER REDEFINES SCANPCTF.
               10  SCANPCTA PIC  X(0001).
           05  SCANPCTI PIC  X(0006).
      *    -------------------------------
           05  SRMETHL PIC S9(0004) COMP.
           05  SRMETHF PIC  X(0001).
           05  FILLER REDEFINES SRMETHF.
               10  SRMETHA PIC  X(0001).
           05  SRMETHI PIC  X(0009).
      *    -------------------------------
           05  SRTERML PIC S9(0004) COMP.
           05  SRTERMF PIC  X(0001).
           05  FILLER REDEFINES SRTERMF.
               10  SRTERMA PIC  X(0001).
           05  SRTERMI PIC  X(0010).
      *    -------------------------------
           05  AISSTOLL PIC S9(0004) COMP.
           05  AISSTOLF PIC  X(0001).
           05  FILLER REDEFINES AISSTOLF.
               10  AISSTOLA PIC  X(0001).
           05  AISSTOLI PIC  X(0006).
      *    -------------------------------
           05  AEFFDTL PIC S9(0004) COMP.
           05  AEFFDTF PIC  X(0001).
           05  FILLER REDEFINES AEFFDTF.
               10  AEFFDTA PIC  X(0001).
           05  AEFFDTI PIC  X(0008).
      *    -------------------------------
           05  AEXPDTEL PIC S9(0004) COMP.
           05  AEXPDTEF PIC  X(0001).
           05  FILLER REDEFINES AEXPDTEF.
               10  AEXPDTEA PIC  X(0001).
           05  AEXPDTEI PIC  X(0008).
      *    -------------------------------
           05  ARMETHL PIC S9(0004) COMP.
           05  ARMETHF PIC  X(0001).
           05  FILLER REDEFINES ARMETHF.
               10  ARMETHA PIC  X(0001).
           05  ARMETHI PIC  X(0010).
      *    -------------------------------
           05  AOVSTOLL PIC S9(0004) COMP.
           05  AOVSTOLF PIC  X(0001).
           05  FILLER REDEFINES AOVSTOLF.
               10  AOVSTOLA PIC  X(0001).
           05  AOVSTOLI PIC  X(0006).
      *    -------------------------------
           05  ACANTOLL PIC S9(0004) COMP.
           05  ACANTOLF PIC  X(0001).
           05  FILLER REDEFINES ACANTOLF.
               10  ACANTOLA PIC  X(0001).
           05  ACANTOLI PIC  X(0006).
      *    -------------------------------
           05  AIGL PIC S9(0004) COMP.
           05  AIGF PIC  X(0001).
           05  FILLER REDEFINES AIGF.
               10  AIGA PIC  X(0001).
           05  AIGI PIC  X(0001).
      *    -------------------------------
           05  ACLASSL PIC S9(0004) COMP.
           05  ACLASSF PIC  X(0001).
           05  FILLER REDEFINES ACLASSF.
               10  ACLASSA PIC  X(0001).
           05  ACLASSI PIC  X(0002).
      *    -------------------------------
           05  ADEVL PIC S9(0004) COMP.
           05  ADEVF PIC  X(0001).
           05  FILLER REDEFINES ADEVF.
               10  ADEVA PIC  X(0001).
           05  ADEVI PIC  X(0003).
      *    -------------------------------
           05  ADEVPCTL PIC S9(0004) COMP.
           05  ADEVPCTF PIC  X(0001).
           05  FILLER REDEFINES ADEVPCTF.
               10  ADEVPCTA PIC  X(0001).
           05  ADEVPCTI PIC  99V999999.
      *    -------------------------------
           05  AHEADML PIC S9(0004) COMP.
           05  AHEADMF PIC  X(0001).
           05  FILLER REDEFINES AHEADMF.
               10  AHEADMA PIC  X(0001).
           05  AHEADMI PIC  X(0011).
      *    -------------------------------
           05  OBHEADL PIC S9(0004) COMP.
           05  OBHEADF PIC  X(0001).
           05  FILLER REDEFINES OBHEADF.
               10  OBHEADA PIC  X(0001).
           05  OBHEADI PIC  X(0011).
      *    -------------------------------
           05  AMAAGEL PIC S9(0004) COMP.
           05  AMAAGEF PIC  X(0001).
           05  FILLER REDEFINES AMAAGEF.
               10  AMAAGEA PIC  X(0001).
           05  AMAAGEI PIC  X(0003).
      *    -------------------------------
           05  AMAGEL PIC S9(0004) COMP.
           05  AMAGEF PIC  X(0001).
           05  FILLER REDEFINES AMAGEF.
               10  AMAGEA PIC  X(0001).
           05  AMAGEI PIC  X(0003).
      *    -------------------------------
           05  AMTERML PIC S9(0004) COMP.
           05  AMTERMF PIC  X(0001).
           05  FILLER REDEFINES AMTERMF.
               10  AMTERMA PIC  X(0001).
           05  AMTERMI PIC  X(0003).
      *    -------------------------------
           05  AMTBENL PIC S9(0004) COMP.
           05  AMTBENF PIC  X(0001).
           05  FILLER REDEFINES AMTBENF.
               10  AMTBENA PIC  X(0001).
           05  AMTBENI PIC  X(0010).
      *    -------------------------------
           05  AMMBENL PIC S9(0004) COMP.
           05  AMMBENF PIC  X(0001).
           05  FILLER REDEFINES AMMBENF.
               10  AMMBENA PIC  X(0001).
           05  AMMBENI PIC  X(0010).
      *    -------------------------------
           05  OBRATEL PIC S9(0004) COMP.
           05  OBRATEF PIC  X(0001).
           05  FILLER REDEFINES OBRATEF.
               10  OBRATEA PIC  X(0001).
           05  OBRATEI PIC  X(0010).
      *    -------------------------------
           05  PHONEL PIC S9(0004) COMP.
           05  PHONEF PIC  X(0001).
           05  FILLER REDEFINES PHONEF.
               10  PHONEA PIC  X(0001).
           05  PHONEI PIC  X(0012).
      *    -------------------------------
           05  RMAAGEL PIC S9(0004) COMP.
           05  RMAAGEF PIC  X(0001).
           05  FILLER REDEFINES RMAAGEF.
               10  RMAAGEA PIC  X(0001).
           05  RMAAGEI PIC  X(0003).
      *    -------------------------------
           05  RMAGEL PIC S9(0004) COMP.
           05  RMAGEF PIC  X(0001).
           05  FILLER REDEFINES RMAGEF.
               10  RMAGEA PIC  X(0001).
           05  RMAGEI PIC  X(0003).
      *    -------------------------------
           05  RMTERML PIC S9(0004) COMP.
           05  RMTERMF PIC  X(0001).
           05  FILLER REDEFINES RMTERMF.
               10  RMTERMA PIC  X(0001).
           05  RMTERMI PIC  X(0003).
      *    -------------------------------
           05  RMTBENL PIC S9(0004) COMP.
           05  RMTBENF PIC  X(0001).
           05  FILLER REDEFINES RMTBENF.
               10  RMTBENA PIC  X(0001).
           05  RMTBENI PIC  X(0010).
      *    -------------------------------
           05  RMMBENL PIC S9(0004) COMP.
           05  RMMBENF PIC  X(0001).
           05  FILLER REDEFINES RMMBENF.
               10  RMMBENA PIC  X(0001).
           05  RMMBENI PIC  X(0010).
      *    -------------------------------
           05  RRATESL PIC S9(0004) COMP.
           05  RRATESF PIC  X(0001).
           05  FILLER REDEFINES RRATESF.
               10  RRATESA PIC  X(0001).
           05  RRATESI PIC  X(0009).
      *    -------------------------------
           05  RNSPSTL PIC S9(0004) COMP.
           05  RNSPSTF PIC  X(0001).
           05  FILLER REDEFINES RNSPSTF.
               10  RNSPSTA PIC  X(0001).
           05  RNSPSTI PIC  X(0002).
      *    -------------------------------
           05  RNSPRTL PIC S9(0004) COMP.
           05  RNSPRTF PIC  X(0001).
           05  FILLER REDEFINES RNSPRTF.
               10  RNSPRTA PIC  X(0001).
           05  RNSPRTI PIC  X(0008).
      *    -------------------------------
           05  CTDATEHL PIC S9(0004) COMP.
           05  CTDATEHF PIC  X(0001).
           05  FILLER REDEFINES CTDATEHF.
               10  CTDATEHA PIC  X(0001).
           05  CTDATEHI PIC  X(0010).
      *    -------------------------------
           05  DRATESL PIC S9(0004) COMP.
           05  DRATESF PIC  X(0001).
           05  FILLER REDEFINES DRATESF.
               10  DRATESA PIC  X(0001).
           05  DRATESI PIC  X(0009).
      *    -------------------------------
           05  CTSTL PIC S9(0004) COMP.
           05  CTSTF PIC  X(0001).
           05  FILLER REDEFINES CTSTF.
               10  CTSTA PIC  X(0001).
           05  CTSTI PIC  X(0009).
      *    -------------------------------
           05  CTRFUNDL PIC S9(0004) COMP.
           05  CTRFUNDF PIC  X(0001).
           05  FILLER REDEFINES CTRFUNDF.
               10  CTRFUNDA PIC  X(0001).
           05  CTRFUNDI PIC  X(0010).
      *    -------------------------------
           05  CTDATEL PIC S9(0004) COMP.
           05  CTDATEF PIC  X(0001).
           05  FILLER REDEFINES CTDATEF.
               10  CTDATEA PIC  X(0001).
           05  CTDATEI PIC  X(0008).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0074).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
      *    -------------------------------
           05  PFOPTL PIC S9(0004) COMP.
           05  PFOPTF PIC  X(0001).
           05  FILLER REDEFINES PFOPTF.
               10  PFOPTA PIC  X(0001).
           05  PFOPTI PIC  X(0006).
       01  EL631EO REDEFINES EL631EI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HEADO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CISSTOLO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANTOLO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVSAMTO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMAGEO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPREMO PIC  ZZZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMENDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMTERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMXTERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTERMO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCOMMO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEARNMO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSPECO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRMETHO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BIGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRTERMO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SISSTOLO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SISSPCTO PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SOVSTOLO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SCANTOLO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SOVSPCTO PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SCANPCTO PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SRMETHO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SRTERMO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AISSTOLO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEXPDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARMETHO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AOVSTOLO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACANTOLO PIC  ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIGO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLASSO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADEVO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADEVPCTO PIC  9.999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEADMO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OBHEADO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMAAGEO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMAGEO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMTERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMTBENO PIC  ZZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMMBENO PIC  ZZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OBRATEO PIC  ZZZ9.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMAAGEO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMAGEO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMTBENO PIC  ZZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RMMBENO PIC  ZZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RRATESO PIC  ZZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RNSPSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RNSPRTO PIC  ZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTDATEHO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DRATESO PIC  ZZZ.99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTSTO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTRFUNDO PIC  ZZZZZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0074).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFOPTO PIC  X(0006).
      *    -------------------------------
00093  EJECT
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
00095  01  DFHCOMMAREA             PIC X(1300).
00096  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6312' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00098      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00099      MOVE PI-CRITERIA-DATA       TO EDIT-CRITERIA-DATA.
00100
00101      IF EIBCALEN = 0
00102          GO TO 8800-UNAUTHORIZED-ACCESS.
00103
00104      MOVE LOW-VALUES             TO EL631EI.
00105
00106      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00107          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00108              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00109              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00110              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00111              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00112              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00113              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00114              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00115              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00116              GO TO 0350-DISPLAY-COMMON-DATA.
00117
00118      IF EIBAID = DFHCLEAR
00119          GO TO 9400-CLEAR.
00120
00121  EJECT
00122  0200-RECEIVE.
00123      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00124          MOVE ER-0008            TO EMI-ERROR
00125          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00126          MOVE -1                 TO PFENTERL
00127          GO TO 8100-SEND-INITIAL-MAP.
00128
00129      
      * EXEC CICS RECEIVE
00130 *        MAP      (MAP-NAME)
00131 *        MAPSET   (MAPSET-NAME)
00132 *        INTO     (EL631EI)
00133 *    END-EXEC.
           MOVE LENGTH OF
            EL631EI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001906' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL631EI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00134
00135      IF PFENTERL = 0
00136          GO TO 0300-CHECK-PFKEYS.
00137      IF EIBAID NOT = DFHENTER
00138          MOVE ER-0004            TO EMI-ERROR
00139          GO TO 0320-INPUT-ERROR.
00140      IF (PFENTERI NUMERIC) AND (PFENTERI > 0 AND < 25)
00141          MOVE PF-VALUES (PFENTERI) TO EIBAID
00142      ELSE
00143          MOVE ER-0029            TO EMI-ERROR
00144          GO TO 0320-INPUT-ERROR.
00145
00146  0300-CHECK-PFKEYS.
00147      IF EIBAID = DFHPF1
00148         GO TO 0350-DISPLAY-COMMON-DATA.
00149
00150      IF EIBAID = DFHPF23
00151          GO TO 8810-PF23.
00152
00153      IF EIBAID = DFHPF24
00154          GO TO 9200-RETURN-MAIN-MENU.
00155
00156      IF EIBAID = DFHPF12
00157          GO TO 9500-PF12.
00158
00159      IF EIBAID = DFHENTER
00160          GO TO 8200-SEND-DATAONLY.
00161
00162      MOVE ER-0029                TO EMI-ERROR.
00163  0320-INPUT-ERROR.
00164      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00165      MOVE AL-UNBON               TO PFENTERA.
00166      MOVE -1                     TO PFENTERL.
00167      GO TO 8200-SEND-DATAONLY.
00168
00169  EJECT
00170  0350-DISPLAY-COMMON-DATA.
00171      IF EC-CO-TOL-PREM    NUMERIC
00172         IF EC-CO-TOL-PREM NOT = ZEROS
00173            MOVE EC-CO-TOL-PREM      TO CISSTOLO.
00174
00175      IF EC-CO-TOL-REFUND  NUMERIC
00176         IF EC-CO-TOL-REFUND NOT = ZEROS
00177            MOVE EC-CO-TOL-REFUND    TO COVSAMTO.
00178
00179      IF EC-CO-OVR-SHT-AMT NUMERIC
00180         IF EC-CO-OVR-SHT-AMT > +0
00181            MOVE EC-CO-OVR-SHT-AMT   TO CCANTOLO.
00182
00183      IF EC-CO-MIN-AGE     NUMERIC
00184         IF EC-CO-MIN-AGE  NOT = ZEROS
00185            MOVE EC-CO-MIN-AGE       TO CMAGEO.
00186
00187      IF EC-CO-MIN-PREMIUM NUMERIC
00188         IF EC-CO-MIN-PREMIUM NOT = ZEROS
00189            MOVE EC-CO-MIN-PREMIUM   TO CMPREMO.
00190
00191      IF EC-CO-MIN-TERM    NUMERIC
00192         IF EC-CO-MIN-TERM NOT = ZEROS
00193            MOVE EC-CO-MIN-TERM      TO CMTERMO.
00194
00195      IF EC-CO-MAX-TERM    NUMERIC
00196         IF EC-CO-MAX-TERM NOT = ZEROS
00197            MOVE EC-CO-MAX-TERM      TO CMXTERMO.
00198
00199      IF EC-CO-REM-TERM-CALC   = '1'
00200        MOVE 'AFTER 15TH'   TO CRTERMO
00201       ELSE
00202        IF EC-CO-REM-TERM-CALC   = '2'
00203          MOVE 'FIRST HALF'   TO CRTERMO
00204         ELSE
00205          IF EC-CO-REM-TERM-CALC   = '3'
00206            MOVE 'ON 1ST DAY'   TO CRTERMO
00207           ELSE
00208            IF EC-CO-REM-TERM-CALC   = '4'
00209              MOVE 'ON LAST DAY'   TO CRTERMO
00210             ELSE
00211              IF EC-CO-REM-TERM-CALC   = '5'
00212                MOVE 'NO DAYS'       TO CRTERMO
00213               ELSE
00214                IF EC-CO-REM-TERM-CALC   = '6'
00215                  MOVE 'AFTER 14TH'    TO CRTERMO
00216                 ELSE
00217                  IF EC-CO-REM-TERM-CALC   = '7'
00218                    MOVE 'AFTER 16TH'    TO CRTERMO.
00219
00220      MOVE EC-CO-MONTH-END-DT     TO DC-BIN-DATE-1.
00221      MOVE SPACE                  TO DC-OPTION-CODE.
00222      PERFORM 9700-DATE-LINK.
00223      IF NO-CONVERSION-ERROR
00224         MOVE DC-GREG-DATE-1-EDIT TO CMENDO.
00225
00226       IF EIBTRNID  NOT = TRANS-ID
00227          IF EC-BR-LF-DESC NOT = SPACES
00228             GO TO 500-DISPLAY-LIFE.
00229
00230      IF PI-DISPLAY-AH
00231         GO TO 500-DISPLAY-LIFE.
00232  EJECT
00233  400-DISPLAY-AH.
00234      MOVE 'AH'                   TO PI-DISPLAY-SW.
00235      MOVE PI-AH-OVERRIDE-L6      TO HEADI.
00236      MOVE PI-LIFE-OVERRIDE-L6    TO PFOPTI.
00237
00238      IF EC-BR-AH-EARN-BY-R78
00239          MOVE 'RULE 78'           TO BEARNMO
00240         ELSE
00241          IF EC-BR-AH-EARN-BY-PRO-RATA
00242              MOVE 'PRO-RATA'          TO BEARNMO
00243            ELSE
00244              IF EC-BR-AH-EARN-AS-CALIF
00245                  MOVE 'CALIF'          TO BEARNMO
00246               ELSE
00247                  IF EC-BR-AH-EARN-IS-NET-PAY
00248                     MOVE 'NET PAY'        TO BEARNMO
00249                  ELSE
00250                     IF EC-BR-AH-EARN-ANTICIPATION
00251                        MOVE 'ANTICIPAT'      TO BEARNMO
00252                     ELSE
00253                        IF EC-BR-AH-EARN-MEAN
00254                           MOVE 'MEAN'        TO BEARNMO
00255                        ELSE
00256                           IF EC-BR-AH-EARN-REG-BALLOON
00257                              MOVE 'BALLOON'  TO BEARNMO.
00258
00259         IF EC-CF-AH-OUTSTANDING-BAL
00260            MOVE 'OUT BAL'           TO BSPECO
00261         ELSE
00262            IF EC-CF-AH-CRITICAL-PERIOD
00263               MOVE 'CRT PERIOD'    TO BSPECO.
00264
00265         EVALUATE TRUE
00266            WHEN EC-BR-AH-REFD-BY-R78
00267               MOVE 'RULE 78'           TO BRMETHO
00268            WHEN EC-BR-AH-REFD-BY-PRO-RATA
00269               MOVE 'PRO-RATA'          TO BRMETHO
00270            WHEN EC-BR-AH-REFD-AS-CALIF
00271               MOVE 'CALIFORNIA'        TO BRMETHO
00272            WHEN EC-BR-AH-REFD-AS-TEXAS
00273               MOVE 'IRREG'             TO BRMETHO
00274            WHEN EC-BR-AH-REFD-IS-NET-PAY
00275               MOVE 'NET PAY'        TO BRMETHO
00276            WHEN EC-BR-AH-REFD-ANTICIPATION
00277               MOVE 'ANTICIPAT'      TO BRMETHO
00278            WHEN EC-BR-AH-REFD-IS-MEAN
00279               MOVE 'MEAN'           TO BRMETHO
00280            WHEN EC-BR-AH-REFD-SUM-OF-DIGIT
00281               MOVE 'SUM DIGIT'      TO BRMETHO
00282         END-EVALUATE.
00283
00284
00285      MOVE EC-BR-AH-DESC          TO BTYPEO.
00286      MOVE EC-BR-AH-COMMENT       TO BCOMMO.
00287      MOVE EC-BR-AH-BEN-I-G-CD    TO BIGO.
00288
00289      IF EC-BR-AH-REM-TERM-CALC = '1'
00290        MOVE 'AFTER 15TH'   TO BRTERMO
00291       ELSE
00292        IF EC-BR-AH-REM-TERM-CALC = '2'
00293          MOVE 'FIRST HALF'   TO BRTERMO
00294         ELSE
00295          IF EC-BR-AH-REM-TERM-CALC = '3'
00296            MOVE 'ON 1ST DAY'   TO BRTERMO
00297           ELSE
00298            IF EC-BR-AH-REM-TERM-CALC = '4'
00299              MOVE 'ON LAST DAY'   TO BRTERMO
00300             ELSE
00301              IF EC-BR-AH-REM-TERM-CALC = '5'
00302                MOVE 'NO DAYS'       TO BRTERMO
00303               ELSE
00304                IF EC-BR-AH-REM-TERM-CALC = '6'
00305                  MOVE 'AFTER 14TH'    TO BRTERMO
00306                 ELSE
00307                  IF EC-BR-AH-REM-TERM-CALC = '7'
00308                    MOVE 'AFTER 16TH'    TO BRTERMO.
00309
00310      IF EC-ST-TOL-PREM    NUMERIC
00311         IF EC-ST-TOL-PREM NOT = ZEROS
00312            MOVE EC-ST-TOL-PREM   TO SISSTOLO.
00313
00314      IF EC-ST-TOL-REFUND NUMERIC
00315         IF EC-ST-TOL-REFUND NOT = ZEROS
00316            MOVE EC-ST-TOL-REFUND TO SCANTOLO.
00317
00318      IF EC-ST-OVR-SHT-AMT NUMERIC
00319         IF EC-ST-OVR-SHT-AMT >+ 0
00320            MOVE EC-ST-OVR-SHT-AMT TO SOVSTOLO.
00321
00322      IF EC-ST-TOL-PREM-PCT NUMERIC
00323         IF EC-ST-TOL-PREM-PCT GREATER THAN +0
00324            MOVE EC-ST-TOL-PREM-PCT
00325                                  TO SISSPCTO.
00326
00327      IF EC-ST-TOL-REFUND-PCT NUMERIC
00328         IF EC-ST-TOL-REFUND-PCT GREATER THAN +0
00329            MOVE EC-ST-TOL-REFUND-PCT
00330                                  TO SCANPCTO.
00331
00332      IF EC-ST-OVR-SHT-PCT NUMERIC
00333         IF EC-ST-OVR-SHT-PCT > +0
00334            MOVE EC-ST-OVR-SHT-PCT TO SOVSPCTO.
00335
00336      IF EC-ST-AH-REM-TERM-CALC   = '1'
00337        MOVE 'AFTER 15TH'   TO SRTERMO
00338       ELSE
00339        IF EC-ST-AH-REM-TERM-CALC   = '2'
00340          MOVE 'FIRST HALF'   TO SRTERMO
00341         ELSE
00342          IF EC-ST-AH-REM-TERM-CALC   = '3'
00343            MOVE 'ON 1ST DAY'     TO SRTERMO
00344           ELSE
00345            IF EC-ST-AH-REM-TERM-CALC   = '4'
00346              MOVE 'ON LAST DAY'   TO SRTERMO
00347             ELSE
00348              IF EC-ST-AH-REM-TERM-CALC   = '5'
00349                MOVE 'NO DAYS'       TO SRTERMO
00350               ELSE
00351                IF EC-ST-AH-REM-TERM-CALC   = '6'
00352                  MOVE 'AFTER 14TH'    TO SRTERMO
00353                 ELSE
00354                  IF EC-ST-AH-REM-TERM-CALC   = '7'
00355                    MOVE 'AFTER 16TH'    TO SRTERMO.
00356
00357      EVALUATE TRUE
00358         WHEN EC-ST-AH-REFD-BY-R78
00359            MOVE 'RULE 78'        TO SRMETHO
00360         WHEN EC-ST-AH-REFD-BY-PRO-RATA
00361            MOVE 'PRO-RATA'       TO SRMETHO
00362         WHEN EC-ST-AH-REFD-AS-CALIF
00363            MOVE 'CALIFORNIA'     TO SRMETHO
00364         WHEN EC-ST-AH-REFD-AS-TEXAS
00365            MOVE 'IRREG'          TO SRMETHO
00366         WHEN EC-ST-AH-REFD-IS-NET-PAY
00367            MOVE 'NET PAY'        TO SRMETHO
00368         WHEN EC-ST-AH-REFD-ANTICIPATION
00369            MOVE 'ANTICIPAT'      TO SRMETHO
00370         WHEN EC-ST-AH-REFD-IS-MEAN
00371            MOVE 'MEAN'           TO SRMETHO
00372         WHEN EC-ST-AH-REFD-SUM-OF-DIGIT
00373            MOVE 'SUM DIGIT'      TO SRMETHO
00374      END-EVALUATE.
00375
00376      IF EC-AM-AH-TOL-PREM    NUMERIC
00377         IF EC-AM-AH-TOL-PREM NOT = ZEROS
00378            MOVE EC-AM-AH-TOL-PREM   TO AISSTOLO.
00379
00380      IF EC-AM-AH-TOL-REFUND  NUMERIC
00381         IF EC-AM-AH-TOL-REFUND NOT = ZEROS
00382            MOVE EC-AM-AH-TOL-REFUND TO ACANTOLO.
00383
00384      IF EC-AM-AH-OVR-SHT-AMT NUMERIC
00385         IF EC-AM-AH-OVR-SHT-AMT > +0
00386            MOVE EC-AM-AH-OVR-SHT-AMT TO AOVSTOLO.
00387
00388      EVALUATE TRUE
00389         WHEN EC-AM-AH-REFD-BY-R78
00390            MOVE 'RULE 78'           TO ARMETHO
00391         WHEN EC-AM-AH-REFD-BY-PRO-RATA
00392            MOVE 'PRO-RATA'          TO ARMETHO
00393         WHEN EC-AM-AH-REFD-AS-CALIF
00394            MOVE 'CALIFORNIA'        TO ARMETHO
00395         WHEN EC-AM-AH-REFD-ANTICIPATION
00396            MOVE 'ANTICIPAT'      TO ARMETHO
00397         WHEN EC-AM-AH-REFD-IS-MEAN
00398            MOVE 'MEAN'           TO ARMETHO
00399         WHEN EC-AM-AH-REFD-IS-NET
00400           MOVE 'NET PAY'        TO ARMETHO
00401         WHEN EC-AM-AH-REFD-SUM-OF-DIGIT
00402            MOVE 'SUM DIGIT'      TO ARMETHO
00403      END-EVALUATE.
00404
00405      MOVE EC-AM-BEN-I-G-CD       TO AIGO.
00406      MOVE EC-AM-CLASS-CD         TO ACLASSO.
00407      MOVE EC-AM-AH-DEVIATION     TO ADEVO.
00408      MOVE EC-AM-PHONE-NO         TO WS-PHONE.
00409      MOVE WS-PHONE-NUM           TO PHONEO.
00410      INSPECT PHONEO REPLACING ALL ' ' BY '-'.
00411
00412      IF EC-AM-AH-DEV-PERCENT NUMERIC
00413         IF  EC-AM-AH-DEV-PERCENT > +0
00414             MOVE EC-AM-AH-DEV-PERCENT   TO ADEVPCTO.
00415
00416      IF EC-AM-EXPIRATION-DT NOT = HIGH-VALUES
00417         MOVE EC-AM-EXPIRATION-DT    TO DC-BIN-DATE-1
00418         MOVE SPACE                  TO DC-OPTION-CODE
00419         PERFORM 9700-DATE-LINK
00420         IF NO-CONVERSION-ERROR
00421            MOVE DC-GREG-DATE-1-EDIT TO AEXPDTEO
00422           ELSE
00423            NEXT SENTENCE
00424         ELSE
00425          MOVE '99/99/99'          TO AEXPDTEO.
00426
00427      MOVE EC-AM-EFFECTIVE-DT     TO DC-BIN-DATE-1.
00428      MOVE SPACE                  TO DC-OPTION-CODE.
00429      PERFORM 9700-DATE-LINK.
00430      IF NO-CONVERSION-ERROR
00431         MOVE DC-GREG-DATE-1-EDIT TO AEFFDTO.
00432
00433      IF EC-AM-AH-MAX-ATT-AGE  NUMERIC
00434         IF EC-AM-AH-MAX-ATT-AGE NOT = ZEROS
00435            MOVE EC-AM-AH-MAX-ATT-AGE  TO AMAAGEO.
00436
00437      IF EC-AM-AH-MAX-AGE  NUMERIC
00438         IF EC-AM-AH-MAX-AGE NOT = ZEROS
00439            MOVE EC-AM-AH-MAX-AGE      TO AMAGEO.
00440
00441      IF EC-AM-AH-MAX-TERM   NUMERIC
00442         IF EC-AM-AH-MAX-TERM NOT = ZEROS
00443            MOVE EC-AM-AH-MAX-TERM   TO AMTERMO.
00444
00445      IF EC-AM-AH-MAX-TOT-BEN NUMERIC
00446         IF EC-AM-AH-MAX-TOT-BEN NOT = ZEROS
00447            MOVE EC-AM-AH-MAX-TOT-BEN  TO AMTBENO.
00448
00449      IF EC-AM-AH-MAX-MON-BEN NUMERIC
00450         IF EC-AM-AH-MAX-MON-BEN NOT = ZEROS
00451            MOVE EC-AM-AH-MAX-MON-BEN  TO AMMBENO
00452            MOVE 'MAX-MON-BEN'         TO AHEADMO.
00453
00454      IF EC-RT-AH-MAX-ATT-AGE  NUMERIC
00455         IF EC-RT-AH-MAX-ATT-AGE NOT = ZEROS
00456            MOVE EC-RT-AH-MAX-ATT-AGE  TO RMAAGEO.
00457
00458      IF EC-RT-AH-MAX-AGE  NUMERIC
00459         IF EC-RT-AH-MAX-AGE NOT = ZEROS
00460            MOVE EC-RT-AH-MAX-AGE      TO RMAGEO.
00461
00462      IF EC-RT-AH-MAX-TERM   NUMERIC
00463         IF EC-RT-AH-MAX-TERM NOT = ZEROS
00464            MOVE EC-RT-AH-MAX-TERM   TO RMTERMO.
00465
00466      IF EC-RT-AH-MAX-TOT-BEN NUMERIC
00467         IF EC-RT-AH-MAX-TOT-BEN NOT = ZEROS
00468            MOVE EC-RT-AH-MAX-TOT-BEN  TO RMTBENO.
00469
00470      IF EC-RT-AH-MAX-MON-BEN NUMERIC
00471         IF EC-RT-AH-MAX-MON-BEN NOT = ZEROS
00472            MOVE EC-RT-AH-MAX-MON-BEN  TO RMMBENO.
00473
00474      IF EC-RT-AH-RATE   NUMERIC
00475          IF EC-CF-AH-OUTSTANDING-BAL
00476              MOVE ZEROS           TO RRATESO
00477              MOVE EC-RT-AH-RATE   TO OBRATEO
00478              MOVE ' OB RATE'      TO OBHEADO
00479          ELSE
00480              MOVE EC-RT-AH-RATE   TO RRATESO
00481              COMPUTE WS-DEV-RT = EC-RT-AH-RATE *
00482                                  EC-AM-AH-DEV-PERCENT
00483              MOVE WS-DEV-RT       TO DRATESO.
00484
00485      MOVE EC-RT-AH-NSP-ST        TO RNSPSTO.
00486
00487      IF EC-RT-AH-NSP-ST NOT = SPACES
00488         IF EC-RT-AH-NSP-RATE   NUMERIC
00489            MOVE EC-RT-AH-NSP-RATE       TO RNSPRTO.
00490
00491      IF EC-CM-AH-POLICY-IS-ACTIVE
00492         MOVE 'ACTIVE'            TO CTSTO
00493         ELSE
00494         IF EC-CM-AH-LUMP-SUM-DISAB
00495            MOVE 'LUMP SUM'    TO CTSTO
00496            ELSE
00497            IF EC-CM-AH-DEATH-CLAIM-APPLIED
00498               MOVE 'DEATH'    TO CTSTO  CTDATEHO
00499               MOVE 'DEATH-DT' TO CTDATEHO
00500               ELSE
00501               IF EC-CM-AH-CANCEL-APPLIED
00502                  MOVE 'CANCELLED'   TO CTSTO  CTDATEHO
00503                  MOVE 'CANCEL-DT'   TO CTDATEHO.
00504
00505      IF EC-CM-AH-PRIOR-REFUND  NUMERIC
00506         IF EC-CM-AH-PRIOR-REFUND  NOT = ZEROS
00507            MOVE EC-CM-AH-PRIOR-REFUND    TO CTRFUNDO.
00508
00509      IF EC-CM-AH-DEATH-CLAIM-APPLIED
00510         MOVE EC-CM-DEATH-DT         TO DC-BIN-DATE-1
00511         MOVE SPACE                  TO DC-OPTION-CODE
00512         PERFORM 9700-DATE-LINK
00513         IF NO-CONVERSION-ERROR
00514            MOVE DC-GREG-DATE-1-EDIT TO CTDATEO.
00515
00516      IF EC-CM-AH-CANCEL-APPLIED
00517         MOVE EC-CM-AH-CANCEL-DT     TO DC-BIN-DATE-1
00518         MOVE SPACE                  TO DC-OPTION-CODE
00519         PERFORM 9700-DATE-LINK
00520         IF NO-CONVERSION-ERROR
00521            MOVE DC-GREG-DATE-1-EDIT TO CTDATEO.
00522      GO TO 8100-SEND-INITIAL-MAP.
00523  EJECT
00524  500-DISPLAY-LIFE.
00525      MOVE 'LF'                   TO PI-DISPLAY-SW
00526      MOVE PI-LIFE-OVERRIDE-L6    TO HEADI
00527      MOVE PI-AH-OVERRIDE-L6      TO PFOPTI.
00528
00529      EVALUATE TRUE
00530         WHEN EC-BR-LF-EARN-BY-R78
00531            MOVE 'RULE 78'           TO BEARNMO
00532         WHEN EC-BR-LF-EARN-BY-PRO-RATA
00533            MOVE 'PRO-RATA'          TO BEARNMO
00534         WHEN EC-BR-LF-EARN-AS-TEXAS
00535            MOVE 'IRREG'             TO BEARNMO
00536         WHEN EC-BR-LF-EARN-AS-FARM-PLAN
00537            MOVE 'FARM'              TO BEARNMO
00538         WHEN EC-BR-LF-EARN-IS-NET-PAY
00539            MOVE 'NET PAY'           TO BEARNMO
00540         WHEN EC-BR-LF-EARN-ANTICIPATION
00541            MOVE 'ANTICIPAT'         TO BEARNMO
00542         WHEN EC-BR-LF-EARN-MEAN
00543            MOVE 'MEAN'              TO BEARNMO
00544         WHEN EC-BR-LF-EARN-REG-BALLOON
00545            MOVE 'BALLOON'           TO BEARNMO
00546         WHEN EC-BR-LF-EARN-SUM-OF-DIGIT
00547            MOVE 'SUM DIGIT'         TO BEARNMO
00548         WHEN OTHER
00549            MOVE SPACES              TO BEARNMO
00550      END-EVALUATE.
00551
00552         IF EC-CF-LF-OUTSTANDING-BAL
00553            MOVE 'OUT BAL'           TO BSPECO
00554          ELSE
00555            IF EC-CF-LF-ALTERNATE-NET-PAY
00556               MOVE 'ALT NET PY'        TO BSPECO
00557            ELSE
00558               IF EC-CF-LF-NET-PAY-SIMPLE
00559                     MOVE 'NET PAY'           TO BSPECO
00560                ELSE
00561                  IF EC-CF-LF-CRITICAL-PERIOD
00562                     MOVE 'CRT PERIOD'    TO BSPECO
00563                  ELSE
00564                        IF EC-CF-LF-TERM-IN-DAYS
00565                           MOVE 'TRM IN DAY'    TO BSPECO.
00566
00567      EVALUATE TRUE
00568         WHEN EC-BR-LF-REFD-BY-R78
00569            MOVE 'RULE 78'        TO BRMETHO
00570         WHEN EC-BR-LF-REFD-BY-PRO-RATA
00571            MOVE 'PRO-RATA'       TO BRMETHO
00572         WHEN EC-BR-LF-REFD-AS-TEXAS
00573            MOVE 'IRREG'          TO BRMETHO
00574         WHEN EC-BR-LF-REFD-IS-NET-PAY
00575            MOVE 'NET PAY'        TO BRMETHO
00576         WHEN EC-BR-LF-REFD-ANTICIPATION
00577            MOVE 'ANTICIPAT'      TO BRMETHO
00578         WHEN EC-BR-LF-REFD-IS-MEAN
00579            MOVE 'MEAN'           TO BRMETHO
00580         WHEN EC-BR-LF-REFD-IS-SUM-OF-DIGIT
00581            MOVE 'SUM DIGIT'      TO BRMETHO
00582         WHEN OTHER
00583            MOVE SPACES           TO BRMETHO
00584      END-EVALUATE.
00585
00586      MOVE EC-BR-LF-DESC          TO BTYPEO.
00587      MOVE EC-BR-LF-COMMENT       TO BCOMMO.
00588      MOVE EC-BR-LF-BEN-I-G-CD    TO BIGO.
00589
00590      IF EC-BR-LF-REM-TERM-CALC = '1'
00591        MOVE 'AFTER 15TH'   TO BRTERMO
00592       ELSE
00593        IF EC-BR-LF-REM-TERM-CALC = '2'
00594          MOVE 'FIRST HALF'   TO BRTERMO
00595         ELSE
00596          IF EC-BR-LF-REM-TERM-CALC = '3'
00597            MOVE 'ON 1ST DAY'   TO BRTERMO
00598           ELSE
00599            IF EC-BR-LF-REM-TERM-CALC = '4'
00600              MOVE 'ON LAST DAY'   TO BRTERMO
00601             ELSE
00602              IF EC-BR-LF-REM-TERM-CALC = '5'
00603                MOVE 'NO DAYS'       TO BRTERMO
00604               ELSE
00605               IF EC-BR-LF-REM-TERM-CALC = '6'
00606                 MOVE 'AFTER 14TH'    TO BRTERMO
00607                ELSE
00608                 IF EC-BR-LF-REM-TERM-CALC = '7'
00609                   MOVE 'AFTER 16TH'    TO BRTERMO.
00610
00611      IF EC-ST-TOL-PREM    NUMERIC
00612         IF EC-ST-TOL-PREM NOT = ZEROS
00613            MOVE EC-ST-TOL-PREM      TO SISSTOLO.
00614
00615      IF EC-ST-OVR-SHT-AMT NUMERIC
00616         IF EC-ST-OVR-SHT-AMT > +0
00617            MOVE EC-ST-OVR-SHT-AMT   TO SOVSTOLO.
00618
00619      IF EC-ST-TOL-REFUND  NUMERIC
00620         IF EC-ST-TOL-REFUND  NOT = ZEROS
00621            MOVE EC-ST-TOL-REFUND    TO SCANTOLO.
00622
00623      IF EC-ST-TOL-PREM-PCT NUMERIC
00624         IF EC-ST-TOL-PREM-PCT GREATER THAN +0
00625            MOVE EC-ST-TOL-PREM-PCT
00626                                  TO SISSPCTO.
00627
00628      IF EC-ST-TOL-REFUND-PCT NUMERIC
00629         IF EC-ST-TOL-REFUND-PCT GREATER THAN +0
00630            MOVE EC-ST-TOL-REFUND-PCT
00631                                  TO SCANPCTO.
00632
00633      IF EC-ST-OVR-SHT-PCT NUMERIC
00634         IF EC-ST-OVR-SHT-PCT > +0
00635            MOVE EC-ST-OVR-SHT-PCT TO SOVSPCTO.
00636
00637      IF EC-ST-LF-REM-TERM-CALC   = '1'
00638        MOVE 'AFTER 15TH'   TO SRTERMO
00639       ELSE
00640        IF EC-ST-LF-REM-TERM-CALC   = '2'
00641          MOVE 'FIRST HALF'   TO SRTERMO
00642         ELSE
00643          IF EC-ST-LF-REM-TERM-CALC   = '3'
00644            MOVE 'ON 1ST DAY'     TO SRTERMO
00645           ELSE
00646            IF EC-ST-LF-REM-TERM-CALC   = '4'
00647              MOVE 'ON LAST DAY'   TO SRTERMO
00648           ELSE
00649            IF EC-ST-LF-REM-TERM-CALC   = '5'
00650              MOVE 'NO DAYS'       TO SRTERMO
00651           ELSE
00652            IF EC-ST-LF-REM-TERM-CALC   = '6'
00653              MOVE 'AFTER 14TH'    TO SRTERMO
00654           ELSE
00655            IF EC-ST-LF-REM-TERM-CALC   = '7'
00656              MOVE 'AFTER 16TH'    TO SRTERMO.
00657
00658      EVALUATE TRUE
00659         WHEN EC-ST-LF-REFD-BY-R78
00660            MOVE 'RULE 78'           TO SRMETHO
00661         WHEN EC-ST-LF-REFD-BY-PRO-RATA
00662            MOVE 'PRO-RATA'          TO SRMETHO
00663         WHEN EC-ST-LF-REFD-AS-CALIF
00664            MOVE 'CALIFORNIA'        TO SRMETHO
00665         WHEN EC-ST-LF-REFD-AS-TEXAS
00666            MOVE 'IRREG'             TO SRMETHO
00667         WHEN EC-ST-LF-REFD-IS-NET-PAY
00668            MOVE 'NET PAY'           TO SRMETHO
00669         WHEN EC-ST-LF-REFD-ANTICIPATION
00670            MOVE 'ANTICIPAT'         TO SRMETHO
00671         WHEN EC-ST-LF-REFD-IS-MEAN
00672            MOVE 'MEAN'              TO SRMETHO
00673         WHEN EC-ST-LF-REFD-IS-SUM-OF-DIGIT
00674            MOVE 'SUM DIGIT'         TO SRMETHO
00675         WHEN OTHER
00676            MOVE SPACES              TO SRMETHO
00677      END-EVALUATE.
00678
00679      IF EC-AM-LF-TOL-PREM    NUMERIC
00680         IF EC-AM-LF-TOL-PREM  NOT = ZEROS
00681            MOVE EC-AM-LF-TOL-PREM   TO AISSTOLO.
00682
00683      IF EC-AM-LF-TOL-REFUND  NUMERIC
00684         IF EC-AM-LF-TOL-REFUND  NOT = ZEROS
00685            MOVE EC-AM-LF-TOL-REFUND TO ACANTOLO.
00686
00687      IF EC-AM-LF-OVR-SHT-AMT NUMERIC
00688         IF EC-AM-LF-OVR-SHT-AMT > +0
00689            MOVE EC-AM-LF-OVR-SHT-AMT TO AOVSTOLO.
00690
00691      EVALUATE TRUE
00692         WHEN EC-AM-LF-REFD-BY-R78
00693            MOVE 'RULE 78'        TO ARMETHO
00694         WHEN EC-AM-LF-REFD-BY-PRO-RATA
00695            MOVE 'PRO-RATA'       TO ARMETHO
00696         WHEN EC-AM-LF-REFD-AS-TEXAS
00697            MOVE 'IRREG'          TO ARMETHO
00698         WHEN EC-AM-LF-REFD-IS-NET-PAY
00699            MOVE 'NET PAY'        TO ARMETHO
00700         WHEN EC-AM-LF-REFD-ANTICIPATION
00701            MOVE 'ANTICIPAT'      TO ARMETHO
00702         WHEN EC-AM-LF-REFD-IS-MEAN
00703            MOVE 'MEAN'           TO ARMETHO
00704         WHEN EC-AM-LF-REFD-IS-SUM-OF-DIGIT
00705            MOVE 'SUM DIGIT'      TO ARMETHO
00706         WHEN OTHER
00707            MOVE SPACES           TO ARMETHO
00708      END-EVALUATE.
00709
00710      MOVE EC-AM-BEN-I-G-CD       TO AIGO.
00711      MOVE EC-AM-CLASS-CD         TO ACLASSO.
00712      MOVE EC-AM-LF-DEVIATION     TO ADEVO.
00713      MOVE EC-AM-LF-DEV-PERCENT   TO ADEVPCTO.
00714      MOVE EC-AM-PHONE-NO         TO WS-PHONE.
00715      MOVE WS-PHONE-NUM           TO PHONEO.
00716      INSPECT PHONEO CONVERTING ' ' TO '-'.
00717
00718      IF EC-AM-EXPIRATION-DT NOT = HIGH-VALUES
00719         MOVE EC-AM-EXPIRATION-DT    TO DC-BIN-DATE-1
00720         MOVE SPACE                  TO DC-OPTION-CODE
00721         PERFORM 9700-DATE-LINK
00722         IF NO-CONVERSION-ERROR
00723            MOVE DC-GREG-DATE-1-EDIT TO AEXPDTEO
00724            ELSE
00725            NEXT SENTENCE
00726         ELSE
00727         MOVE '99/99/99'          TO AEXPDTEO.
00728
00729      MOVE EC-AM-EFFECTIVE-DT     TO DC-BIN-DATE-1.
00730      MOVE SPACE                  TO DC-OPTION-CODE.
00731      PERFORM 9700-DATE-LINK.
00732      IF NO-CONVERSION-ERROR
00733         MOVE DC-GREG-DATE-1-EDIT TO AEFFDTO.
00734
00735      IF EC-AM-LF-MAX-ATT-AGE  NUMERIC
00736         IF EC-AM-LF-MAX-ATT-AGE  NOT = ZEROS
00737            MOVE EC-AM-LF-MAX-ATT-AGE  TO AMAAGEO.
00738
00739      IF EC-AM-LF-MAX-AGE  NUMERIC
00740         IF EC-AM-LF-MAX-AGE  NOT = ZEROS
00741            MOVE EC-AM-LF-MAX-AGE      TO AMAGEO.
00742
00743      IF EC-AM-LF-MAX-TERM   NUMERIC
00744         IF EC-AM-LF-MAX-TERM  NOT = ZEROS
00745            MOVE EC-AM-LF-MAX-TERM   TO AMTERMO.
00746
00747      IF EC-AM-LF-MAX-TOT-BEN NUMERIC
00748         IF EC-AM-LF-MAX-TOT-BEN  NOT = ZEROS
00749            MOVE EC-AM-LF-MAX-TOT-BEN  TO AMTBENO.
00750
00751      IF EC-RT-LF-MAX-ATT-AGE  NUMERIC
00752         IF EC-RT-LF-MAX-ATT-AGE  NOT = ZEROS
00753            MOVE EC-RT-LF-MAX-ATT-AGE  TO RMAAGEO.
00754
00755      IF EC-RT-LF-MAX-AGE  NUMERIC
00756         IF EC-RT-LF-MAX-AGE  NOT = ZEROS
00757            MOVE EC-RT-LF-MAX-AGE      TO RMAGEO.
00758
00759      IF EC-RT-LF-MAX-TERM   NUMERIC
00760         IF EC-RT-LF-MAX-TERM NOT = ZEROS
00761            MOVE EC-RT-LF-MAX-TERM   TO RMTERMO.
00762
00763      IF EC-RT-LF-MAX-TOT-BEN NUMERIC
00764         IF EC-RT-LF-MAX-TOT-BEN NOT = ZEROS
00765            MOVE EC-RT-LF-MAX-TOT-BEN  TO RMTBENO.
00766
00767      IF EC-RT-LF-RATE   NUMERIC
00768          IF  EC-CF-LF-OUTSTANDING-BAL
00769              MOVE ZEROS           TO RRATESO
00770              MOVE EC-RT-LF-RATE   TO OBRATEO
00771              MOVE ' OB RATE'      TO OBHEADO
00772          ELSE
00773              MOVE EC-RT-LF-RATE   TO RRATESO
00774              COMPUTE WS-DEV-RT = EC-RT-LF-RATE *
00775                                  EC-AM-LF-DEV-PERCENT
00776              MOVE WS-DEV-RT       TO DRATESO.
00777
00778      MOVE EC-RT-LF-NSP-ST        TO RNSPSTO.
00779
00780      IF EC-RT-LF-NSP-ST NOT = SPACES
00781         IF EC-RT-LF-NSP-RATE   NUMERIC
00782            MOVE EC-RT-LF-NSP-RATE       TO RNSPRTO.
00783
00784      IF EC-CM-LF-POLICY-IS-ACTIVE
00785         MOVE 'ACTIVE'            TO CTSTO
00786         ELSE
00787         IF EC-CM-LF-LUMP-SUM-DISAB
00788            MOVE 'LUMP SUM'    TO CTSTO
00789            ELSE
00790            IF EC-CM-LF-DEATH-CLAIM-APPLIED
00791               MOVE 'DEATH'    TO CTSTO
00792               MOVE 'DEATH-DT' TO CTDATEHO
00793               ELSE
00794               IF EC-CM-LF-CANCEL-APPLIED
00795                  MOVE 'CANCELLED'   TO CTSTO
00796                  MOVE 'CANCEL-DT'   TO CTDATEHO.
00797
00798      IF EC-CM-LF-PRIOR-REFUND  NUMERIC
00799         IF EC-CM-LF-PRIOR-REFUND NOT = ZEROS
00800            MOVE EC-CM-LF-PRIOR-REFUND    TO CTRFUNDO.
00801
00802      IF EC-CM-LF-DEATH-CLAIM-APPLIED
00803         MOVE EC-CM-DEATH-DT         TO DC-BIN-DATE-1
00804         MOVE SPACE                  TO DC-OPTION-CODE
00805         PERFORM 9700-DATE-LINK
00806         IF NO-CONVERSION-ERROR
00807            MOVE DC-GREG-DATE-1-EDIT TO CTDATEO.
00808
00809      IF EC-CM-LF-CANCEL-APPLIED
00810         MOVE EC-CM-LF-CANCEL-DT     TO DC-BIN-DATE-1
00811         MOVE SPACE                  TO DC-OPTION-CODE
00812         PERFORM 9700-DATE-LINK
00813         IF NO-CONVERSION-ERROR
00814            MOVE DC-GREG-DATE-1-EDIT TO CTDATEO.
00815  EJECT
00816  8100-SEND-INITIAL-MAP.
00817      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00818      MOVE '5'                   TO DC-OPTION-CODE.
00819      PERFORM 9700-DATE-LINK.
00820      MOVE DC-GREG-DATE-1-EDIT   TO  DATEO.
00821      MOVE EIBTIME                TO TIME-IN.
00822      MOVE TIME-OUT               TO TIMEO.
00823      MOVE -1                     TO PFENTERL
00824      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
00825      
      * EXEC CICS SEND
00826 *        MAP      (MAP-NAME)
00827 *        MAPSET   (MAPSET-NAME)
00828 *        FROM     (EL631EO)
00829 *        ERASE
00830 *        CURSOR
00831 *    END-EXEC.
           MOVE LENGTH OF
            EL631EO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002602' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL631EO, 
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
           
00832
00833      GO TO 9100-RETURN-TRAN.
00834
00835  8200-SEND-DATAONLY.
00836      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00837      MOVE '5'                   TO DC-OPTION-CODE.
00838      PERFORM 9700-DATE-LINK.
00839      MOVE DC-GREG-DATE-1-EDIT   TO  DATEO.
00840      MOVE EIBTIME                TO TIME-IN.
00841      MOVE TIME-OUT               TO TIMEO.
00842      MOVE -1                     TO PFENTERL
00843      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
00844      
      * EXEC CICS SEND
00845 *        MAP      (MAP-NAME)
00846 *        MAPSET   (MAPSET-NAME)
00847 *        FROM     (EL631EO)
00848 *        DATAONLY
00849 *        ERASEAUP
00850 *        CURSOR
00851 *    END-EXEC.
           MOVE LENGTH OF
            EL631EO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00002621' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL631EO, 
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
           
00852
00853      GO TO 9100-RETURN-TRAN.
00854
00855  8300-SEND-TEXT.
00856      
      * EXEC CICS SEND TEXT
00857 *        FROM     (LOGOFF-TEXT)
00858 *        LENGTH   (LOGOFF-LENGTH)
00859 *        ERASE
00860 *        FREEKB
00861 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002633' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363333' TO DFHEIV0(25:11)
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
           
00862
00863      
      * EXEC CICS RETURN
00864 *    END-EXEC.
      *    MOVE '.(                    &   #00002640' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00865
00866  8800-UNAUTHORIZED-ACCESS.
00867      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00868      GO TO 8300-SEND-TEXT.
00869
00870  8810-PF23.
00871      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00872      MOVE XCTL-005               TO PGM-NAME.
00873      GO TO 9300-XCTL.
00874  9000-RETURN-CICS.
00875      
      * EXEC CICS RETURN
00876 *    END-EXEC.
      *    MOVE '.(                    &   #00002652' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00877
00878  9100-RETURN-TRAN.
00879      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
00880      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
00881
00882      
      * EXEC CICS RETURN
00883 *        TRANSID    (TRANS-ID)
00884 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00885 *        LENGTH     (1300)
00886 *    END-EXEC.
           MOVE 1300
             TO DFHEIV11
      *    MOVE '.(CT                  &   #00002659' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00887
00888  9200-RETURN-MAIN-MENU.
00889      MOVE XCTL-626               TO PGM-NAME.
00890      GO TO 9300-XCTL.
00891
00892  9300-XCTL.
00893      
      * EXEC CICS XCTL
00894 *        PROGRAM    (PGM-NAME)
00895 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00896 *        LENGTH     (1300)
00897 *    END-EXEC.
           MOVE 1300
             TO DFHEIV11
      *    MOVE '.$C                   $   #00002670' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00898
00899  9400-CLEAR.
00900      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
00901
00902      
      * EXEC CICS XCTL
00903 *        PROGRAM    (PGM-NAME)
00904 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
00905 *        LENGTH     (1300)
00906 *    END-EXEC.
           MOVE 1300
             TO DFHEIV11
      *    MOVE '.$C                   $   #00002679' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00907
00908  9500-PF12.
00909      MOVE XCTL-010               TO PGM-NAME.
00910      GO TO 9300-XCTL.
00911
00912  9600-PGMID-ERROR.
00913      
      * EXEC CICS HANDLE CONDITION
00914 *        PGMIDERR    (8300-SEND-TEXT)
00915 *    END-EXEC.
      *    MOVE '"$L                   ! " #00002690' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032363930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00916
00917      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
00918      MOVE ' '                    TO PI-ENTRY-CD-1.
00919      MOVE XCTL-005               TO PGM-NAME.
00920      MOVE PGM-NAME               TO LOGOFF-PGM.
00921      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
00922      GO TO 9300-XCTL.
00923
00924  9700-DATE-LINK.
00925      MOVE LINK-ELDATCV           TO PGM-NAME.
00926
00927      
      * EXEC CICS LINK
00928 *        PROGRAM    (PGM-NAME)
00929 *        COMMAREA   (DATE-CONVERSION-DATA)
00930 *        LENGTH     (DC-COMM-LENGTH)
00931 *    END-EXEC.
      *    MOVE '."C                   ''   #00002704' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00932
00933  9900-ERROR-FORMAT.
00934      IF NOT EMI-ERRORS-COMPLETE
00935          MOVE LINK-001           TO PGM-NAME
00936          
      * EXEC CICS LINK
00937 *            PROGRAM    (PGM-NAME)
00938 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
00939 *            LENGTH     (EMI-COMM-LENGTH)
00940 *        END-EXEC.
      *    MOVE '."C                   ''   #00002713' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00941
00942  9900-EXIT.
00943      EXIT.
00944
00945  9990-ABEND.
00946      MOVE LINK-004               TO PGM-NAME.
00947      MOVE DFHEIBLK               TO EMI-LINE1.
00948      
      * EXEC CICS LINK
00949 *        PROGRAM   (PGM-NAME)
00950 *        COMMAREA  (EMI-LINE1)
00951 *        LENGTH    (72)
00952 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002725' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00953
00954      GO TO 8200-SEND-DATAONLY.
00955
00956      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6312' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00957
00958  9995-SECURITY-VIOLATION.
00959 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00002753' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373533' TO DFHEIV0(25:11)
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
00960
00961  9995-EXIT.
00962      EXIT.
00963

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6312' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6312' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
