00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL113.
00004 *                            VMOD=2.002.
00005
00006  AUTHOR.     LOGIC,INC.
00007              DALLAS, TEXAS.
00008
00009  DATE-COMPILED.
00010
00011  SECURITY.   *****************************************************
00012              *                                                   *
00013              *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00014              *                                                   *
00015              *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00016              *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00017              *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00018              *                                                   *
00019              *****************************************************
00020
00021  REMARKS.    TRANSACTION - EX38 - ACCOUNT MASTER DISPLAY.
00022
00023      EJECT
00024  ENVIRONMENT DIVISION.
00025  DATA DIVISION.
00026  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00027  77  FILLER  PIC X(32)  VALUE '*** PAN EL113     PANLVL 006 ***'.
00028  77  FILLER  PIC X(32)  VALUE '********************************'.
00029  77  FILLER  PIC X(32)  VALUE '*    EL113 WORKING STORAGE     *'.
00030  77  FILLER  PIC X(32)  VALUE '********** V/M 2.002 ***********'.
00031
00032  01  WS-DATE-AREA.
00033      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00034      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00035
00036  01  STANDARD-AREAS.
00037      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
00038      12  MAP-NAME            PIC X(8)    VALUE 'EL113A'.
00039      12  MAPSET-NAME         PIC X(8)    VALUE 'EL113S'.
00040      12  TRANS-ID            PIC X(4)    VALUE 'EX38'.
00041      12  PGM-NAME            PIC X(8).
00042      12  TIME-IN             PIC S9(7).
00043      12  TIME-OUT-R  REDEFINES TIME-IN.
00044          16  FILLER          PIC X.
00045          16  TIME-OUT        PIC 99V99.
00046          16  FILLER          PIC X(2).
00047      12  XCTL-005            PIC X(5)    VALUE 'EL005'.
00048      12  XCTL-010            PIC X(5)    VALUE 'EL010'.
00049      12  XCTL-126            PIC X(5)    VALUE 'EL126'.
00050      12  LINK-001            PIC X(5)    VALUE 'EL001'.
00051      12  LINK-004            PIC X(5)    VALUE 'EL004'.
00052      12  LINK-CLDATCV        PIC X(7)    VALUE 'ELDATCV'.
00053      12  PGM-EL113           PIC X(8)    VALUE 'EL113'.
00054      12  ACCT-ID             PIC X(8)    VALUE 'ERACCT'.
00055      12  ACCT-VG-ID          PIC X(8)    VALUE 'ERACCT2'.
00056      12  SAVE-ACCT-PARTIAL-KEY   PIC X(20)  VALUE LOW-VALUES.
00057      12  WS-ZIP.
00058          16  WS-ZIP-CODE         PIC X(5).
00059          16  WS-DASH             PIC X(1)   VALUE '-'.
00060          16  WS-ZIP-PLUS4        PIC X(4).
00061      12  WS-CANADIAN-POSTAL-CODES REDEFINES WS-ZIP.
00062          16  WS-CAN-POSTAL-CD-1  PIC X(3).
00063          16  WS-DASH-CAN         PIC X(1).
00064          16  WS-CAN-POSTAL-CD-2  PIC X(3).
00065          16  WS-CAN-FILLER       PIC X(3).
00066      12  WS-WORK-PHONE           PIC X(10)  VALUE ZEROS.
00067      12  WS-NUMERIC-PHONE REDEFINES WS-WORK-PHONE
00068                                  PIC 9(10).
00069      EJECT
00070
00071  01  WORK-AREAS.
00072      12  BROWSE-STARTED-SW       PIC X       VALUE ' '.
00073          88  BROWSE-STARTED      VALUE 'Y'.
00074      12  FIRST-TIME-SW           PIC X       VALUE ' '.
00075          88  FIRST-TIME          VALUE 'Y'.
00076      12  DEEDIT-FIELD            PIC X(15).
00077      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00078      12  DEEDIT-FIELD-V1 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.
00079      12  EXPDTE-SAVE             PIC XX.
00080      12  EFFDTE-SAVE             PIC XX.
00081      12  ALLOW-SAVE              PIC S9(3)V99  VALUE ZEROS.
00082      12  E023                    PIC X(4)    VALUE '0023'.
00083      12  E042                    PIC X(4)    VALUE '0042'.
00084      12  E052                    PIC X(4)    VALUE '0052'.
00085      12  E066                    PIC X(4)    VALUE '0066'.
00086      12  E067                    PIC X(4)    VALUE '0067'.
00087      12  E144                    PIC X(4)    VALUE '0144'.
00088      12  E168                    PIC X(4)    VALUE '0168'.
00089      12  E193                    PIC X(4)    VALUE '0193'.
00090      12  E195                    PIC X(4)    VALUE '0195'.
00091      12  E226                    PIC X(4)    VALUE '0226'.
00092      12  E348                    PIC X(4)    VALUE '0348'.
00093      12  E453                    PIC X(4)    VALUE '0453'.
00094      12  E454                    PIC X(4)    VALUE '0454'.
00095      12  E455                    PIC X(4)    VALUE '0455'.
00096      12  E456                    PIC X(4)    VALUE '0456'.
00097      12  E457                    PIC X(4)    VALUE '0457'.
00098      12  E470                    PIC X(4)    VALUE '0470'.
00099      12  E471                    PIC X(4)    VALUE '0471'.
00100      12  E472                    PIC X(4)    VALUE '0472'.
00101      12  E482                    PIC X(4)    VALUE '0482'.
00102
00103      EJECT
00104 *                            COPY ELCDATE.
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
00105      EJECT
00106 *                            COPY ELCLOGOF.
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
00107      EJECT
00108 *                            COPY ELCATTR.
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
00109      EJECT
00110 *                                  COPY ELCEMIB.
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
00111      EJECT
00112 *                            COPY ELCJPFX.
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
00113                                   PIC X(2000).
00114      EJECT
00115 *                            COPY ELCINTF.
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
00116      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00117          16  PI-PREV-ACCOUNT     PIC X(26).
00118          16  PI-PREV-VG-ACCOUNT  PIC X(26).
00119
00120          16  ACCT-KEY.
00121            18  ACCT-PARTIAL-KEY.
00122              20  ACCT-CO             PIC X.
00123              20  ACCT-CARRIER        PIC X.
00124              20  ACCT-GROUPING       PIC X(6).
00125              20  ACCT-STATE          PIC XX.
00126              20  ACCT-ACCOUNT        PIC X(10).
00127            18  ACCT-EXP-DT           PIC XX.
00128            18  ACCT-FILLER           PIC X(04).
00129      EJECT
00130 *                            COPY ELCAID.
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
00131  01  FILLER    REDEFINES DFHAID.
00132      12  FILLER              PIC X(8).
00133      12  PF-VALUES           PIC X       OCCURS 2.
00134      EJECT
00135 *                            COPY EL113S.
       01  EL113AI.
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
           05  AEXPDTL PIC S9(0004) COMP.
           05  AEXPDTF PIC  X(0001).
           05  FILLER REDEFINES AEXPDTF.
               10  AEXPDTA PIC  X(0001).
           05  AEXPDTI PIC  X(0008).
      *    -------------------------------
           05  AEFFDTL PIC S9(0004) COMP.
           05  AEFFDTF PIC  X(0001).
           05  FILLER REDEFINES AEFFDTF.
               10  AEFFDTA PIC  X(0001).
           05  AEFFDTI PIC  X(0008).
      *    -------------------------------
           05  ANAMEL PIC S9(0004) COMP.
           05  ANAMEF PIC  X(0001).
           05  FILLER REDEFINES ANAMEF.
               10  ANAMEA PIC  X(0001).
           05  ANAMEI PIC  X(0030).
      *    -------------------------------
           05  ACAREOFL PIC S9(0004) COMP.
           05  ACAREOFF PIC  X(0001).
           05  FILLER REDEFINES ACAREOFF.
               10  ACAREOFA PIC  X(0001).
           05  ACAREOFI PIC  X(0030).
      *    -------------------------------
           05  AADDR1L PIC S9(0004) COMP.
           05  AADDR1F PIC  X(0001).
           05  FILLER REDEFINES AADDR1F.
               10  AADDR1A PIC  X(0001).
           05  AADDR1I PIC  X(0030).
      *    -------------------------------
           05  ACITYSTL PIC S9(0004) COMP.
           05  ACITYSTF PIC  X(0001).
           05  FILLER REDEFINES ACITYSTF.
               10  ACITYSTA PIC  X(0001).
           05  ACITYSTI PIC  X(0030).
      *    -------------------------------
           05  AZIPL PIC S9(0004) COMP.
           05  AZIPF PIC  X(0001).
           05  FILLER REDEFINES AZIPF.
               10  AZIPA PIC  X(0001).
           05  AZIPI PIC  X(0010).
      *    -------------------------------
           05  APHONEL PIC S9(0004) COMP.
           05  APHONEF PIC  X(0001).
           05  FILLER REDEFINES APHONEF.
               10  APHONEA PIC  X(0001).
           05  APHONEI PIC  X(0012).
      *    -------------------------------
           05  ALIFLEVL PIC S9(0004) COMP.
           05  ALIFLEVF PIC  X(0001).
           05  FILLER REDEFINES ALIFLEVF.
               10  ALIFLEVA PIC  X(0001).
           05  ALIFLEVI PIC  X(0001).
      *    -------------------------------
           05  ADECLIFL PIC S9(0004) COMP.
           05  ADECLIFF PIC  X(0001).
           05  FILLER REDEFINES ADECLIFF.
               10  ADECLIFA PIC  X(0001).
           05  ADECLIFI PIC  X(0001).
      *    -------------------------------
           05  ADECAHL PIC S9(0004) COMP.
           05  ADECAHF PIC  X(0001).
           05  FILLER REDEFINES ADECAHF.
               10  ADECAHA PIC  X(0001).
           05  ADECAHI PIC  X(0001).
      *    -------------------------------
           05  AALLOWL PIC S9(0004) COMP.
           05  AALLOWF PIC  X(0001).
           05  FILLER REDEFINES AALLOWF.
               10  AALLOWA PIC  X(0001).
           05  AALLOWI PIC  X(0006).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  AEMSG2L PIC S9(0004) COMP.
           05  AEMSG2F PIC  X(0001).
           05  FILLER REDEFINES AEMSG2F.
               10  AEMSG2A PIC  X(0001).
           05  AEMSG2I PIC  X(0079).
      *    -------------------------------
           05  AEMSG3L PIC S9(0004) COMP.
           05  AEMSG3F PIC  X(0001).
           05  FILLER REDEFINES AEMSG3F.
               10  AEMSG3A PIC  X(0001).
           05  AEMSG3I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  9(2).
       01  EL113AO REDEFINES EL113AI.
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
           05  AEXPDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEFFDTO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACAREOFO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACITYSTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALIFLEVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADECLIFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADECAHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AALLOWO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG3O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  99.
      *    -------------------------------
00136 *01  MAP-REDEF REDEFINES EL113AI.
00137 *    12  FILLER                  PIC X(70).
00138 *    12  MAP-RECORD-AREA         PIC X(214).
00139      EJECT
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
00141  01  DFHCOMMAREA             PIC X(1024).
00142
00143  01  PARMLIST.
00144      02  FILLER              PIC S9(8)   COMP.
00145      02  ACCT-POINTER        PIC S9(8)   COMP.
00146      EJECT
00147 *                            COPY ERCACCT.
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
00148      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PARMLIST
                                ACCOUNT-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL113' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00150      SERVICE RELOAD PARMLIST.
00151      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00152      MOVE 3  TO EMI-NUMBER-OF-LINES.
00153      IF EIBCALEN = 0
00154          GO TO 8800-UNAUTHORIZED-ACCESS.
00155
00156      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00157      MOVE '5'                   TO DC-OPTION-CODE.
00158      PERFORM 9700-DATE-LINK.
00159      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00160      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00161
00162
00163      IF PI-CALLING-PROGRAM NOT = PGM-EL113
00164          IF PI-RETURN-TO-PROGRAM NOT = PGM-EL113
00165              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00166              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00167              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00168              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00169              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00170              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00171              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00172              MOVE PGM-EL113 TO PI-CALLING-PROGRAM
00173              MOVE LOW-VALUES TO EL113AO
00174                                 PI-PROGRAM-WORK-AREA
00175              GO TO 8100-SEND-INITIAL-MAP.
00176
00177
00178      
      * EXEC CICS HANDLE CONDITION
00179 *        PGMIDERR(9600-PGMID-ERROR)
00180 *        ERROR   (9990-ABEND)
00181 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00001655' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00182
00183      IF EIBAID = DFHCLEAR
00184          GO TO 9400-CLEAR.
00185
00186      EJECT
00187  0200-RECEIVE.
00188      MOVE LOW-VALUES             TO EL113AI.
00189      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00190          MOVE 0008               TO EMI-ERROR
00191          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00192          MOVE -1                 TO AMAINTL
00193          GO TO 8200-SEND-DATAONLY.
00194
00195      
      * EXEC CICS RECEIVE
00196 *        MAP   (MAP-NAME)
00197 *        MAPSET(MAPSET-NAME)
00198 *        INTO  (EL113AI)
00199 *    END-EXEC.
           MOVE LENGTH OF
            EL113AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001672' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL113AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00200
00201      IF APFKL = 0
00202          GO TO 0300-CHECK-PFKEYS.
00203
00204      IF EIBAID NOT = DFHENTER
00205          MOVE 0004               TO EMI-ERROR
00206          GO TO 0320-INPUT-ERROR.
00207      IF (APFKI NUMERIC) AND (APFKI > 0 AND < 25)
00208          MOVE PF-VALUES (APFKI) TO EIBAID
00209      ELSE
00210          MOVE 0029               TO EMI-ERROR
00211          GO TO 0320-INPUT-ERROR.
00212
00213  0300-CHECK-PFKEYS.
00214      IF EIBAID = DFHPF23
00215          GO TO 8810-PF23.
00216      IF EIBAID = DFHPF24
00217          GO TO 9200-RETURN-MAIN-MENU.
00218      IF EIBAID = DFHPF12
00219          GO TO 9500-PF12.
00220      IF (AMAINTL NOT = 0 AND AMAINTI NOT = SPACE) AND
00221         EIBAID NOT = DFHENTER
00222          MOVE 0050               TO EMI-ERROR
00223          GO TO 0320-INPUT-ERROR.
00224      IF EIBAID = DFHPF1
00225          GO TO 5000-FIND-NEXT-ACCOUNT.
00226      IF EIBAID = DFHPF2
00227          GO TO 5100-FIND-PREV-ACCOUNT.
00228      IF EIBAID = DFHENTER
00229          GO TO 0330-EDIT-DATA.
00230
00231      MOVE 0029                   TO EMI-ERROR.
00232  0320-INPUT-ERROR.
00233      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00234
00235      MOVE AL-UNBON               TO APFKA.
00236
00237      IF APFKL = 0
00238          MOVE -1                 TO AMAINTL
00239         ELSE
00240          MOVE -1                 TO APFKL.
00241
00242      GO TO 8200-SEND-DATAONLY.
00243
00244      EJECT
00245  0330-EDIT-DATA.
00246      MOVE AL-UANON               TO AMAINTA.
00247
00248      IF AMAINTI = 'S'
00249          GO TO 1000-SHOW-ACCOUNT.
00250
00251      MOVE E023                   TO EMI-ERROR.
00252      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00253      MOVE -1                     TO AMAINTL.
00254      MOVE AL-UABON               TO AMAINTA.
00255      GO TO 8200-SEND-DATAONLY.
00256
00257      EJECT
00258  1000-SHOW-ACCOUNT.
00259      PERFORM 6100-BUILD-KEY THRU 6199-EXIT.
00260      MOVE ACCT-PARTIAL-KEY TO SAVE-ACCT-PARTIAL-KEY.
00261
00262      
      * EXEC CICS HANDLE CONDITION
00263 *         NOTFND(1010-NOT-FOUND)
00264 *    END-EXEC.
      *    MOVE '"$I                   ! # #00001739' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031373339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00265
00266  1005-READ-ACCT.
00267      MOVE LOW-VALUES             TO ACCT-FILLER.
00268
00269      IF ACCT-EXP-DT = LOW-VALUES
00270         
      * EXEC CICS READ
00271 *           DATASET(ACCT-ID)
00272 *           SET    (ACCT-POINTER)
00273 *           RIDFLD (ACCT-KEY)
00274 *           GTEQ
00275 *       END-EXEC
      *    MOVE '&"S        G          (   #00001747' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCT-POINTER, 
                 DFHEIV99, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00276        ELSE
00277         
      * EXEC CICS READ
00278 *           DATASET(ACCT-ID)
00279 *           SET    (ACCT-POINTER)
00280 *           RIDFLD (ACCT-KEY)
00281 *       END-EXEC.
      *    MOVE '&"S        E          (   #00001754' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCT-POINTER, 
                 DFHEIV99, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00282
00283      SERVICE RELOAD ACCOUNT-MASTER.
00284
00285      MOVE AM-CONTROL-PRIMARY     TO PI-PREV-ACCOUNT
00286                                     ACCT-KEY.
00287      IF ACCT-PARTIAL-KEY NOT = SAVE-ACCT-PARTIAL-KEY
00288         GO TO 1010-NOT-FOUND.
00289
00290      MOVE AM-CONTROL-BY-VAR-GRP  TO PI-PREV-VG-ACCOUNT.
00291      GO TO 7000-BUILD-OUTPUT-MAP.
00292
00293
00294  1010-NOT-FOUND.
00295      MOVE E226                   TO EMI-ERROR.
00296      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00297      MOVE -1                     TO AMAINTL.
00298      MOVE AL-UABON               TO ACARIERA
00299                                     AGROUPA
00300                                     ASTATEA
00301                                     AACCTA
00302                                     AEXPDTA.
00303
00304      MOVE LOW-VALUES             TO PI-PROGRAM-WORK-AREA.
00305      GO TO 8100-SEND-INITIAL-MAP.
00306
00307      EJECT
00308  5000-FIND-NEXT-ACCOUNT.
00309      MOVE PI-PREV-ACCOUNT        TO ACCT-KEY.
00310
00311      IF PI-PREV-ACCOUNT = LOW-VALUES
00312         MOVE PI-COMPANY-CD       TO ACCT-CO
00313       ELSE
00314         MOVE 'Y' TO FIRST-TIME-SW.
00315
00316      
      * EXEC CICS HANDLE CONDITION
00317 *         NOTFND (5030-NOT-FOUND)
00318 *         ENDFILE(5030-NOT-FOUND)
00319 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00001793' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031373933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00320
00321  5005-START-BR.
00322      MOVE LOW-VALUES             TO ACCT-FILLER.
00323
00324      
      * EXEC CICS STARTBR
00325 *         DATASET(ACCT-ID)
00326 *         RIDFLD (ACCT-KEY)
00327 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001801' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00328
00329      MOVE 'Y'                    TO BROWSE-STARTED-SW.
00330
00331  5010-READ-NEXT.
00332      
      * EXEC CICS READNEXT
00333 *         DATASET(ACCT-ID)
00334 *         SET    (ACCT-POINTER)
00335 *         RIDFLD (ACCT-KEY)
00336 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001809' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCT-POINTER, 
                 DFHEIV99, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00337
00338      SERVICE RELOAD ACCOUNT-MASTER.
00339
00340      IF PI-COMPANY-CD  NOT = ACCT-CO
00341         IF PI-PREV-ACCOUNT = LOW-VALUES
00342            MOVE E472             TO EMI-ERROR
00343            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00344            MOVE -1               TO AMAINTL
00345            GO TO 8200-SEND-DATAONLY
00346         ELSE
00347            MOVE E066                   TO EMI-ERROR
00348            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00349            MOVE -1               TO AMAINTL
00350            GO TO 8200-SEND-DATAONLY.
00351
00352      IF FIRST-TIME
00353         MOVE SPACES TO FIRST-TIME-SW
00354         GO TO 5010-READ-NEXT.
00355
00356  5020-DISPLAY-RECORD.
00357      MOVE AM-CONTROL-PRIMARY     TO PI-PREV-ACCOUNT.
00358      MOVE AM-CONTROL-BY-VAR-GRP  TO PI-PREV-VG-ACCOUNT.
00359      GO TO 7000-BUILD-OUTPUT-MAP.
00360
00361  5030-NOT-FOUND.
00362      IF BROWSE-STARTED
00363         MOVE SPACE               TO BROWSE-STARTED-SW
00364         
      * EXEC CICS ENDBR
00365 *            DATASET(ACCT-ID)
00366 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001841' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00367
00368      IF ACCT-ACCOUNT = LOW-VALUE
00369         MOVE E472                   TO EMI-ERROR
00370         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00371         MOVE -1 TO AMAINTL
00372         GO TO 8200-SEND-DATAONLY.
00373
00374      MOVE E066                   TO EMI-ERROR.
00375      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00376      MOVE PI-PREV-ACCOUNT        TO ACCT-KEY.
00377      GO TO 5005-START-BR.
00378
00379      EJECT
00380  5100-FIND-PREV-ACCOUNT.
00381      IF PI-PREV-ACCOUNT = LOW-VALUES
00382         GO TO 5130-NOT-FOUND
00383      ELSE
00384         MOVE PI-PREV-ACCOUNT TO ACCT-KEY.
00385
00386      
      * EXEC CICS HANDLE CONDITION
00387 *         NOTFND (5130-NOT-FOUND)
00388 *         ENDFILE(5130-NOT-FOUND)
00389 *    END-EXEC.
      *    MOVE '"$I''                  ! % #00001863' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031383633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00390
00391      
      * EXEC CICS STARTBR
00392 *         DATASET(ACCT-ID)
00393 *         RIDFLD (ACCT-KEY)
00394 *         EQUAL
00395 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         E          &   #00001868' TO DFHEIV0
           MOVE X'262C20202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00396
00397      MOVE 'Y'                    TO BROWSE-STARTED-SW
00398                                     FIRST-TIME-SW.
00399
00400  5110-READ-PREV.
00401      
      * EXEC CICS READPREV
00402 *         DATASET(ACCT-ID)
00403 *         SET    (ACCT-POINTER)
00404 *         RIDFLD (ACCT-KEY)
00405 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00001878' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCT-POINTER, 
                 DFHEIV99, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00406
00407      SERVICE RELOAD ACCOUNT-MASTER.
00408
00409      IF ACCT-CO NOT = PI-COMPANY-CD
00410         GO TO 5130-NOT-FOUND.
00411
00412      IF FIRST-TIME
00413         MOVE SPACES TO FIRST-TIME-SW
00414         GO TO 5110-READ-PREV.
00415
00416      MOVE AM-CONTROL-PRIMARY     TO PI-PREV-ACCOUNT.
00417      MOVE AM-CONTROL-BY-VAR-GRP  TO PI-PREV-VG-ACCOUNT.
00418      GO TO 7000-BUILD-OUTPUT-MAP.
00419
00420  5130-NOT-FOUND.
00421      IF BROWSE-STARTED
00422         MOVE SPACE               TO BROWSE-STARTED-SW
00423         
      * EXEC CICS ENDBR
00424 *            DATASET(ACCT-ID)
00425 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001900' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00426
00427      MOVE E067                   TO EMI-ERROR.
00428      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00429      MOVE -1                     TO AMAINTL.
00430
00431      IF PI-PREV-ACCOUNT  =  LOW-VALUES
00432         GO TO 5000-FIND-NEXT-ACCOUNT.
00433
00434      MOVE PI-PREV-ACCOUNT        TO ACCT-KEY.
00435      GO TO 1005-READ-ACCT.
00436
00437      EJECT
00438
00439      EJECT
00440  6100-BUILD-KEY.
00441      MOVE LOW-VALUES             TO ACCT-KEY.
00442      MOVE PI-COMPANY-CD          TO ACCT-CO.
00443
00444      IF ACARIERI = SPACES OR LOW-VALUES
00445          MOVE SPACES             TO ACCT-CARRIER
00446      ELSE
00447          MOVE AL-UANON           TO ACARIERA
00448          MOVE ACARIERI           TO ACCT-CARRIER.
00449
00450      IF AGROUPI = SPACES OR LOW-VALUES
00451          MOVE SPACES             TO ACCT-GROUPING
00452      ELSE
00453          MOVE AL-UANON           TO AGROUPA
00454          MOVE AGROUPI            TO ACCT-GROUPING.
00455
00456      IF ASTATEI = SPACES OR LOW-VALUES
00457          MOVE SPACES             TO ACCT-STATE
00458      ELSE
00459          MOVE AL-UANON           TO ASTATEA
00460          MOVE ASTATEI            TO ACCT-STATE.
00461
00462      IF AACCTI = SPACES OR LOW-VALUES
00463          MOVE E471               TO EMI-ERROR
00464          MOVE -1                 TO AACCTL
00465          PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT
00466      ELSE
00467          MOVE AL-UANON           TO AACCTA
00468          MOVE AACCTI             TO ACCT-ACCOUNT.
00469
00470      IF ACCT-STATE = SPACES AND
00471         (ST-ACCNT-CNTL         OR
00472          CARR-ST-ACCNT-CNTL    OR
00473          CARR-GROUP-ST-ACCNT-CNTL)
00474         MOVE E144                TO EMI-ERROR
00475         MOVE -1                  TO ASTATEL
00476         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
00477
00478      IF ACCT-CARRIER = SPACES AND
00479         (CARR-ACCNT-CNTL       OR
00480          CARR-ST-ACCNT-CNTL    OR
00481          CARR-GROUP-ST-ACCNT-CNTL)
00482         MOVE E193                TO EMI-ERROR
00483         MOVE -1                  TO ACARIERL
00484         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
00485
00486      IF ACCT-GROUPING = SPACES AND
00487          CARR-GROUP-ST-ACCNT-CNTL
00488         MOVE E195                TO EMI-ERROR
00489         MOVE -1                  TO AGROUPL
00490         PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
00491
00492      IF AEXPDTL = ZEROS
00493         GO TO 6150-CHECK-ERRORS.
00494
00495      MOVE AEXPDTI                TO DEEDIT-FIELD.
00496      PERFORM 8600-DEEDIT.
00497      IF DEEDIT-FIELD-V0 NOT LESS 999999
00498         MOVE DEEDIT-FIELD-V0     TO AEXPDTO
uktdel*       TRANSFORM AEXPDTI FROM SPACES TO '/'
uktins        INSPECT AEXPDTI REPLACING ALL ' ' BY '/'
00500         MOVE HIGH-VALUES         TO EXPDTE-SAVE
00501                                     ACCT-EXP-DT
00502         GO TO 6150-CHECK-ERRORS.
00503
00504      MOVE DEEDIT-FIELD-V0        TO DC-GREG-DATE-1-MDY.
00505      MOVE '4'                    TO DC-OPTION-CODE.
00506      PERFORM 9700-DATE-LINK.
00507      IF DATE-CONVERSION-ERROR
00508         MOVE E454                TO EMI-ERROR
00509         MOVE -1                  TO AEXPDTL
00510         MOVE AL-UABON            TO AEXPDTA
00511         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00512      ELSE
00513         MOVE AL-UANON            TO AEXPDTA
00514         MOVE DC-GREG-DATE-1-EDIT TO AEXPDTI
00515         MOVE AL-UANON            TO AEXPDTA
00516         MOVE DC-BIN-DATE-1       TO EXPDTE-SAVE
00517                                     ACCT-EXP-DT.
00518
00519  6150-CHECK-ERRORS.
00520      IF NOT EMI-NO-ERRORS
00521         GO TO 8200-SEND-DATAONLY.
00522
00523      IF ACCT-CARRIER  = SPACES  OR
00524         ACCT-STATE    = SPACES  OR
00525         ACCT-GROUPING = SPACES
00526         MOVE ACCT-VG-ID          TO ACCT-ID.
00527
00528  6199-EXIT.
00529      EXIT.
00530
00531      EJECT
00532  7000-BUILD-OUTPUT-MAP.
00533      MOVE AL-UANON               TO ACARIERA
00534                                     AGROUPA
00535                                     ASTATEA
00536                                     AACCTA
00537                                     AEXPDTA.
00538      MOVE AM-CARRIER             TO ACARIERO.
00539      MOVE AM-GROUPING            TO AGROUPO.
00540      MOVE AM-STATE               TO ASTATEO.
00541      MOVE AM-ACCOUNT             TO AACCTO.
00542
00543      IF AM-EXPIRATION-DT = HIGH-VALUES
00544         MOVE 999999              TO AEXPDTO
uktdel*       TRANSFORM AEXPDTI FROM SPACES TO '/'
uktins        INSPECT AEXPDTI REPLACING ALL ' ' BY '/'
00546        ELSE
00547         MOVE AM-EXPIRATION-DT       TO DC-BIN-DATE-1
00548         MOVE SPACE                  TO DC-OPTION-CODE
00549         PERFORM 9700-DATE-LINK
00550         IF DATE-CONVERSION-ERROR
00551             MOVE SPACES             TO AEXPDTI
00552            ELSE
00553             MOVE DC-GREG-DATE-1-EDIT TO AEXPDTI.
00554
00555      MOVE AM-EFFECTIVE-DT        TO DC-BIN-DATE-1.
00556      MOVE SPACE                  TO DC-OPTION-CODE.
00557      PERFORM 9700-DATE-LINK.
00558      IF DATE-CONVERSION-ERROR
00559          MOVE SPACES             TO AEFFDTI
00560         ELSE
00561          MOVE DC-GREG-DATE-1-EDIT TO AEFFDTI.
00562
00563      MOVE AM-NAME                TO ANAMEO.
00564      MOVE AM-PERSON              TO ACAREOFO.
00565      MOVE AM-ADDRS               TO AADDR1O.
00566 *    MOVE AM-CITY                TO ACITYSTO.
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
              DELIMITED BY '  ' INTO ACITYSTO
           END-STRING
00567      MOVE AM-TEL-NO              TO WS-WORK-PHONE.
uktdel*    TRANSFORM WS-WORK-PHONE FROM SPACES TO '0'.
uktins     INSPECT WS-WORK-PHONE REPLACING ALL ' ' BY '0'.
00569      MOVE WS-NUMERIC-PHONE       TO APHONEO.
uktdel*    TRANSFORM APHONEI FROM SPACES TO '-'.
uktins     INSPECT APHONEI REPLACING ALL ' ' BY '/'
00571
00572      IF  AM-CANADIAN-POST-CODE
00573          MOVE AM-CAN-POSTAL-1    TO WS-CAN-POSTAL-CD-1
00574          MOVE AM-CAN-POSTAL-2    TO WS-CAN-POSTAL-CD-2
00575          MOVE SPACES             TO WS-DASH-CAN
uktdel*                                TO WS-CAN-FILLER
uktins                                    WS-CAN-FILLER
00577          MOVE WS-CANADIAN-POSTAL-CODES
00578                                  TO AZIPO
00579
00580      ELSE
00581          MOVE AM-ZIP-PRIME       TO WS-ZIP-CODE
00582
00583          IF  AM-ZIP-PLUS4 = SPACES OR ZEROS
00584              MOVE SPACES         TO WS-ZIP-PLUS4
00585                                     WS-DASH
00586              MOVE WS-ZIP         TO AZIPO
00587
00588          ELSE
00589              MOVE AM-ZIP-PLUS4   TO WS-ZIP-PLUS4
00590              MOVE '-'            TO WS-DASH
00591              MOVE WS-ZIP         TO AZIPO.
00592
00593      MOVE AM-EARN-METHOD-A       TO ADECAHO.
00594      MOVE AM-EARN-METHOD-L       TO ALIFLEVO.
00595      MOVE AM-EARN-METHOD-R       TO ADECLIFO.
00596      MOVE AM-TOL-CLM             TO AALLOWO.
00597
00598      MOVE -1 TO AMAINTL.
00599      IF BROWSE-STARTED
00600         MOVE SPACE               TO BROWSE-STARTED-SW
00601         
      * EXEC CICS ENDBR
00602 *            DATASET(ACCT-ID)
00603 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002086' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00604
00605      GO TO 8100-SEND-INITIAL-MAP.
00606
00607      EJECT
00608  8100-SEND-INITIAL-MAP.
00609      MOVE SAVE-DATE    TO ADATEO.
00610      MOVE EIBTIME      TO TIME-IN.
00611      MOVE TIME-OUT     TO ATIMEO.
00612      MOVE -1           TO AMAINTL.
00613      MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O.
00614      MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O.
00615      
      * EXEC CICS SEND
00616 *        MAP   (MAP-NAME)
00617 *        MAPSET(MAPSET-NAME)
00618 *        FROM  (EL113AO)
00619 *        ERASE
00620 *        CURSOR
00621 *    END-EXEC.
           MOVE LENGTH OF
            EL113AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002100' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL113AO, 
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
           
00622
00623      GO TO 9100-RETURN-TRAN.
00624
00625  8200-SEND-DATAONLY.
00626      MOVE SAVE-DATE    TO ADATEO.
00627      MOVE EIBTIME TO TIME-IN.
00628      MOVE TIME-OUT TO ATIMEO.
00629      MOVE EMI-MESSAGE-AREA (1) TO AEMSG1O.
00630      MOVE EMI-MESSAGE-AREA (2) TO AEMSG2O.
00631
00632      
      * EXEC CICS SEND
00633 *        MAP   (MAP-NAME)
00634 *        MAPSET(MAPSET-NAME)
00635 *        FROM  (EL113AO)
00636 *        DATAONLY
00637 *        CURSOR
00638 *    END-EXEC.
           MOVE LENGTH OF
            EL113AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002117' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL113AO, 
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
           
00639
00640      GO TO 9100-RETURN-TRAN.
00641
00642  8300-SEND-TEXT.
00643      
      * EXEC CICS SEND TEXT
00644 *        FROM  (LOGOFF-TEXT)
00645 *        LENGTH(LOGOFF-LENGTH)
00646 *        ERASE
00647 *        FREEKB
00648 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002128' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313238' TO DFHEIV0(25:11)
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
           
00649
00650      
      * EXEC CICS RETURN
00651 *    END-EXEC.
      *    MOVE '.(                    &   #00002135' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00652
00653      EJECT
00654  8400-LOG-JOURNAL-RECORD.
00655      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
00656      MOVE ACCT-ID                TO JP-FILE-ID.
00657      MOVE PGM-EL113              TO JP-PROGRAM-ID.
pemuni*    EXEC CICS JOURNAL
pemuni*         JFILEID(PI-JOURNAL-FILE-ID)
pemuni*         JTYPEID('CL')
pemuni*         FROM   (JOURNAL-RECORD)
pemuni*         LENGTH (273)
pemuni*    END-EXEC.
00664
00665
00666  8600-DEEDIT.
00667      
      * EXEC CICS BIF DEEDIT
00668 *         FIELD (DEEDIT-FIELD)
00669 *         LENGTH(15)
00670 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002152' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00671
00672  8800-UNAUTHORIZED-ACCESS.
00673      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00674      GO TO 8300-SEND-TEXT.
00675
00676  8810-PF23.
00677      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00678      MOVE XCTL-005               TO PGM-NAME.
00679      GO TO 9300-XCTL.
00680
00681  8850-DUPREC.
00682      MOVE 0147                   TO EMI-ERROR.
00683      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00684      MOVE -1                     TO AACCTL.
00685      MOVE AL-UABON               TO AACCTA.
00686      GO TO 8200-SEND-DATAONLY.
00687
00688  8870-CNTL-NOTOPEN.
00689      MOVE E042                   TO EMI-ERROR.
00690      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00691      MOVE -1                     TO AMAINTL.
00692      GO TO 8200-SEND-DATAONLY.
00693
00694  8880-ACCT-NOTOPEN.
00695      MOVE E168                   TO EMI-ERROR.
00696      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00697      MOVE -1                     TO AMAINTL.
00698      GO TO 8200-SEND-DATAONLY.
00699
00700  9000-RETURN-CICS.
00701      
      * EXEC CICS RETURN
00702 *    END-EXEC.
      *    MOVE '.(                    &   #00002186' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00703
00704  9100-RETURN-TRAN.
00705      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
00706      MOVE '106A' TO PI-CURRENT-SCREEN-NO.
00707      
      * EXEC CICS RETURN
00708 *        TRANSID(TRANS-ID)
00709 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
00710 *        LENGTH(PI-COMM-LENGTH)
00711 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002192' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00712
00713  9200-RETURN-MAIN-MENU.
00714      MOVE XCTL-126 TO PGM-NAME.
00715      GO TO 9300-XCTL.
00716
00717  9300-XCTL.
00718      
      * EXEC CICS XCTL
00719 *        PROGRAM(PGM-NAME)
00720 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
00721 *        LENGTH(PI-COMM-LENGTH)
00722 *    END-EXEC.
      *    MOVE '.$C                   $   #00002203' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00723
00724  9400-CLEAR.
00725      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
00726      GO TO 9300-XCTL.
00727
00728  9500-PF12.
00729      MOVE XCTL-010 TO PGM-NAME.
00730      GO TO 9300-XCTL.
00731
00732  9700-DATE-LINK.
00733      MOVE LINK-CLDATCV           TO PGM-NAME
00734      
      * EXEC CICS LINK
00735 *        PROGRAM   (PGM-NAME)
00736 *        COMMAREA  (DATE-CONVERSION-DATA)
00737 *        LENGTH    (DC-COMM-LENGTH)
00738 *    END-EXEC.
      *    MOVE '."C                   ''   #00002219' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00739
00740  9600-PGMID-ERROR.
00741      
      * EXEC CICS HANDLE CONDITION
00742 *        PGMIDERR(8300-SEND-TEXT)
00743 *    END-EXEC.
      *    MOVE '"$L                   ! & #00002226' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032323236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00744
00745      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.
00746      MOVE ' '          TO PI-ENTRY-CD-1.
00747      MOVE XCTL-005     TO PGM-NAME.
00748      MOVE PGM-NAME     TO LOGOFF-PGM.
00749      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
00750      GO TO 9300-XCTL.
00751
00752  9900-ERROR-FORMAT.
00753      IF NOT EMI-ERRORS-COMPLETE
00754          MOVE LINK-001 TO PGM-NAME
00755          
      * EXEC CICS LINK
00756 *            PROGRAM(PGM-NAME)
00757 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
00758 *            LENGTH(EMI-COMM-LENGTH)
00759 *        END-EXEC.
      *    MOVE '."C                   ''   #00002240' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00760
00761  9900-EXIT.
00762      EXIT.
00763
00764  9990-ABEND.
00765      MOVE LINK-004 TO PGM-NAME.
00766      MOVE DFHEIBLK TO EMI-LINE1.
00767      
      * EXEC CICS LINK
00768 *        PROGRAM  (PGM-NAME)
00769 *        COMMAREA (EMI-LINE1)
00770 *        LENGTH   (72)
00771 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002252' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00772
00773      GO TO 8200-SEND-DATAONLY.
00774      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL113' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL113' TO DFHEIV1
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
               GO TO 1010-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5030-NOT-FOUND,
                     5030-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 5130-NOT-FOUND,
                     5130-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL113' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
