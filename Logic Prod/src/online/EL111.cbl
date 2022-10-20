00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL111 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:47:58.
00007 *                            VMOD=2.004
00007 *
00008 *AUTHOR.        LOGIC, INC.
00009 *               DALLAS, TEXAS.
00010 *DATE-COMPILED.
00011 *SECURITY.   *****************************************************
00012 *            *                                                   *
00013 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00014 *            *                                                   *
00015 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00016 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00017 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00018 *            *                                                   *
00019 *            *****************************************************
00020 *REMARKS. TRANSACTION EX36 - PROGRAM FILE MAINTENANCE.
00021    EJECT
00022  ENVIRONMENT DIVISION.
00023  DATA DIVISION.
00024  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00025  01  LCP-TIME-OF-DAY-XX.
00026      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00027      05  FILLER                    PIC 99.
00028  01  LCP-CICS-TIME                 PIC 9(15).
00029
00030  77  FILLER  PIC X(32)  VALUE '********************************'.
00031  77  FILLER  PIC X(32)  VALUE '*    EL111 WORKING STORAGE     *'.
00032  77  FILLER  PIC X(32)  VALUE '*********** VMOD 2.004 *********'.
00033
00034  01  WS-DATE-AREA.
00035      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00036      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00037
00038  01  LITERALS-NUMBERS.
00039      12  LIT-A                   PIC X       VALUE 'A'.
00040      12  LIT-B                   PIC X       VALUE 'B'.
00041      12  LIT-C                   PIC X       VALUE 'C'.
00042      12  LIT-D                   PIC X       VALUE 'D'.
00043      12  LIT-E                   PIC X       VALUE 'E'.
00044      12  LIT-S                   PIC X       VALUE 'S'.
00045      12  LIT-X                   PIC X       VALUE 'X'.
00046      12  LIT-SPACE               PIC X       VALUE SPACE.
00047      12  LIT-PO                  PIC XX      VALUE 'PO'.
00048      12  LIT-CLAIM               PIC XX      VALUE 'EL'.
00049      12  LIT-GL                  PIC XX      VALUE 'GL'.
00050      12  LIT-EM                  PIC XX      VALUE 'EM'.
00051      12  LIT-EC                  PIC XX      VALUE 'EC'.
00052      12  LIT-FILE                PIC X(8)    VALUE 'ELPGMO'.
00053      12  LIT-FILE-2              PIC X(8)    VALUE 'ELPGMN'.
00054      12  LIT-PGM                 PIC X(8)    VALUE 'EL111'.
00055      12  LIT-SYS                 PIC X(4)    VALUE 'SYS'.
00056      12  LIT-TRANS               PIC X(4)    VALUE 'EX36'.
00057      12  LIT-HELP                PIC X(8)    VALUE 'EL010'.
00058      12  LIT-MAP                 PIC X(4)    VALUE '111A'.
00059      12  CALL-PGM                PIC X(8)    VALUE SPACES.
00060      12  XCTL-005                PIC X(8)    VALUE 'EL005'.
00061      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
00062      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.
00063      12  XCTL-EM626              PIC X(8)    VALUE 'EM626'.
00064      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.
00065      12  COUNT-1                 PIC 9.
00066      EJECT
00067  01  EDIT-WORK-AREA.
00068      12  CHECK-MAINT             PIC X.
00069          88  SHOW-OPTION                     VALUE 'S'.
00070          88  CHANGE-OPTION                   VALUE 'C'.
00071          88  DELETE-OPTION                   VALUE 'D'.
00072          88  VALID-OPTION                    VALUE 'C' 'D' 'S'.
00073      12  CHECK-TYPE              PIC X.
00074          88  VALID-TYPE                      VALUE 'F' 'P' 'T'.
00075      12  CHECK-PFKEYS            PIC 99.
00076      12  SCREEN-SWITCH           PIC X.
00077          88  UPDATES-MADE                    VALUE 'A'.
00078          88  DELETE-CODE-PRESENT             VALUE 'D'.
00079          88  SCREEN-ERROR                    VALUE 'X'.
00080      12  UPDATE-SWITCH           PIC X.
00081          88  NOTHING-CHANGED                 VALUE 'E'.
00082          88  UPDATE-COMPLETE                 VALUE 'D'.
00083      12  FIND-SWITCH             PIC X.
00084          88  END-UPDATES                     VALUE 'E'.
00085          88  UPDATE-FOUND                    VALUE 'D'.
00086      12  BROWSE-KEY.
00087          16  KEY-GENERIC.
00088              20  PGM-NO.
00089                  24  PGM-PREF    PIC XX.
00090                  24  PGM-NUM     PIC XXX.
00091              20  OPT-TYPE        PIC X.
00092          16  OPT-CODE            PIC X.
00093      12  SAVE-KEY                PIC X(6).
00094      12  HOLD-OPT                PIC 9.
00095      12  NAME-KEY                PIC X(5).
00096      12  RECORD-KEY.
00097          16  GENERIC-KEY         PIC X(6).
00098          16  FILLER              PIC X.
00099      12  DESC-HOLD.
00100          16  HOLD-DESC OCCURS 8 TIMES PIC X(40).
00101
00102  01  TIME-UNFORMATTED.
00103      12  UN-HOURS                PIC XX.
00104      12  UN-MINUTES              PIC XX.
00105      12  FILLER                  PIC X(4).
00106
00107  01  TIME-FORMATTED.
00108      12  FOR-HOURS               PIC XX.
00109      12  FILLER                  PIC X       VALUE '.'.
00110      12  FOR-MINUTES             PIC XX.
00111      EJECT
00112  01  ERROR-MESSAGES.
00113      12  MAINT-MIS.
00114          16  FILLER              PIC X(39)
00115              VALUE 'MAINTENANCE FUNCTION INVALID OR UNAUTHO'.
00116          16  FILLER              PIC X(18)
00117              VALUE 'RIZED             '.
00118
00119      12  PGM-MIS.
00120          16  FILLER              PIC X(33)
00121              VALUE 'PROGRAM NUMBER MISSING OR INVALID'.
00122          16  FILLER              PIC X(18)
00123              VALUE ' - PLEASE RE-ENTER'.
00124
00125      12  PGM-FILE-MIS.
00126          16  FILLER              PIC X(38)
00127              VALUE 'PROGRAM NUMBER MUST BE DEFINED IN THE '.
00128          16  FILLER              PIC X(24)
00129              VALUE 'PROGRAM DESCRIPTION FILE'.
00130
00131      12  PGM-DUP                 PIC X(42)
00132          VALUE 'PROGRAM NUMBER DUPLICATE - PLEASE RE-ENTER'.
00133
00134      12  OPT-MIS.
00135          16  FILLER              PIC X(30)
00136              VALUE 'OPTION TYPE MISSING OR INVALID'.
00137          16  FILLER              PIC X(18)
00138              VALUE ' - PLEASE RE-ENTER'.
00139
00140      12  DEL-OPT-MIS.
00141          16  FILLER              PIC X(35)
00142              VALUE 'DELETE OPTION MUST BE ENTERED WITH '.
00143          16  FILLER              PIC X(22)
00144              VALUE 'MAINTENANCE FUNCTION D'.
00145
00146      12  DEL-OPT-INV             PIC X(35)
00147              VALUE 'NOTHING TO DELETE FOR OPTIONS GIVEN'.
00148
00149      12  DEL-OPT-ERROR.
00150          16  FILLER              PIC X(39)
00151              VALUE 'DELETE OPTION CAN ONLY BE ENTERED WITH'.
00152          16  FILLER              PIC X(22)
00153              VALUE 'MAINTENANCE FUNCTION D'.
00154
00155      12  INVALID-PFKEY           PIC X(45)
00156          VALUE 'INVALID PF-OPTION REQUESTED - PLEASE RE-ENTER'.
00157
00158      12  END-UPDATE              PIC X(16)
00159          VALUE 'UPDATE COMPLETED'.
00160
00161      12  NO-UPDATES              PIC X(32)
00162          VALUE 'NO UPDATES MADE - SCREEN IGNORED'.
00163
00164      12  SCREEN-TERM             PIC X(42)
00165            VALUE '     CLAS-IC PROGRAM MAINTENANCE COMPLETED'.
00166
00167      12  FILE-MSG                PIC X(25)
00168            VALUE '     FILE ELPGMO NOT OPEN'.
00169
00170      12  NO-OPTIONS-PRESENT      PIC X(37)
00171          VALUE 'NO OPTIONS AVAILABLE FOR THIS PROGRAM'.
00172      EJECT
00173  01  COMP-LENGTHS.
00174      12  LIT-IC                  PIC S9(4)  COMP VALUE -1.
00175      12  FILE-LENGTH             PIC S9(4)  COMP VALUE +25.
00176      12  TERM-LENGTH             PIC S9(4)  COMP VALUE +42.
00177      12  JOURNAL-LENGTH          PIC S9(4)  COMP VALUE +72.
00178      12  SHOW-LENGTH             PIC S9(4)  COMP VALUE +6.
00179      12  PGMO-LENGTH             PIC S9(4)  COMP VALUE +49.
00180
00181      EJECT
00182 *    COPY ELCDATE.
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
00183      EJECT
00184 *    COPY ELCLOGOF.
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
00185      EJECT
00186 *    COPY ELCATTR.
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
00187      EJECT
00188 *    COPY ELCAID.
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
00189  01  FILLER REDEFINES DFHAID.
00190      12  FILLER                  PIC X(8).
00191      12  AID-KEYS OCCURS 24 TIMES.
00192          16  FILLER              PIC X.
00193      EJECT
00194 *    COPY ELCJPFX.
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
00195                                  PIC X(49).
00196      EJECT
00197 *    COPY ELCINTF.
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
00198      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00199          16  MENU-SWITCH         PIC X.
00200          16  SHOW-SWITCH         PIC X.
00201              88   NOT-SHOWN                 VALUE 'X'.
00202          16  FILLER              PIC X(638).
00203      EJECT
00204 *    COPY EL111S.
       01  EL111AI.
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
           05  PGRML PIC S9(0004) COMP.
           05  PGRMF PIC  X(0001).
           05  FILLER REDEFINES PGRMF.
               10  PGRMA PIC  X(0001).
           05  PGRMI PIC  X(0005).
      *    -------------------------------
           05  OPTTYPEL PIC S9(0004) COMP.
           05  OPTTYPEF PIC  X(0001).
           05  FILLER REDEFINES OPTTYPEF.
               10  OPTTYPEA PIC  X(0001).
           05  OPTTYPEI PIC  X(0001).
      *    -------------------------------
           05  DELOPTL PIC S9(0004) COMP.
           05  DELOPTF PIC  X(0001).
           05  FILLER REDEFINES DELOPTF.
               10  DELOPTA PIC  X(0001).
           05  DELOPTI PIC  X(0001).
      *    -------------------------------
           05  DESC1L PIC S9(0004) COMP.
           05  DESC1F PIC  X(0001).
           05  FILLER REDEFINES DESC1F.
               10  DESC1A PIC  X(0001).
           05  DESC1I PIC  X(0040).
      *    -------------------------------
           05  DESC2L PIC S9(0004) COMP.
           05  DESC2F PIC  X(0001).
           05  FILLER REDEFINES DESC2F.
               10  DESC2A PIC  X(0001).
           05  DESC2I PIC  X(0040).
      *    -------------------------------
           05  DESC3L PIC S9(0004) COMP.
           05  DESC3F PIC  X(0001).
           05  FILLER REDEFINES DESC3F.
               10  DESC3A PIC  X(0001).
           05  DESC3I PIC  X(0040).
      *    -------------------------------
           05  DESC4L PIC S9(0004) COMP.
           05  DESC4F PIC  X(0001).
           05  FILLER REDEFINES DESC4F.
               10  DESC4A PIC  X(0001).
           05  DESC4I PIC  X(0040).
      *    -------------------------------
           05  DESC5L PIC S9(0004) COMP.
           05  DESC5F PIC  X(0001).
           05  FILLER REDEFINES DESC5F.
               10  DESC5A PIC  X(0001).
           05  DESC5I PIC  X(0040).
      *    -------------------------------
           05  DESC6L PIC S9(0004) COMP.
           05  DESC6F PIC  X(0001).
           05  FILLER REDEFINES DESC6F.
               10  DESC6A PIC  X(0001).
           05  DESC6I PIC  X(0040).
      *    -------------------------------
           05  DESC7L PIC S9(0004) COMP.
           05  DESC7F PIC  X(0001).
           05  FILLER REDEFINES DESC7F.
               10  DESC7A PIC  X(0001).
           05  DESC7I PIC  X(0040).
      *    -------------------------------
           05  DESC8L PIC S9(0004) COMP.
           05  DESC8F PIC  X(0001).
           05  FILLER REDEFINES DESC8F.
               10  DESC8A PIC  X(0001).
           05  DESC8I PIC  X(0040).
      *    -------------------------------
           05  MSGL PIC S9(0004) COMP.
           05  MSGF PIC  X(0001).
           05  FILLER REDEFINES MSGF.
               10  MSGA PIC  X(0001).
           05  MSGI PIC  X(0070).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  X(0002).
       01  EL111AO REDEFINES EL111AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PGRMO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OPTTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DELOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC1O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC2O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC3O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC4O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC5O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC6O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC7O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC8O PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
00205      EJECT
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
00207  01  DFHCOMMAREA                 PIC X(1024).
00208
00209 *01 PARM-LIST .
00210 *    12  FILLER                  PIC S9(8)  COMP.
00211 *    12  PGMO-PNT                PIC S9(8)  COMP.
00212 *    12  PGMN-PNT                PIC S9(8)  COMP.
00213      EJECT
00214 *    COPY ELCPGMO.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCPGMO.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = AVAILABLE PROGRAM OPTIONS                 *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 49    RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELPGMO                RKP=2,LEN=7        *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  PROGRAM-OPTIONS-AVAILABLE.
00019      12  PO-RECORD-ID                PIC XX.
00020          88  VALID-PO-ID                VALUE 'PO'.
00021
00022      12  PO-CONTROL-PRIMARY.
00023          16  PO-PROGRAM-NUMBER.
00024              20  PO-SYSTEM-CODE      PIC XX.
00025                  88  CLAS-IC-ONLINE     VALUE 'EL'.
00026                  88  CLAS-IC-REPORT     VALUE 'EC'.
00027                  88  CLAS-GL-BATCH      VALUE 'GL'.
00028              20  PO-PROGRAM-SEQUENCE PIC 999.
00029          16  PO-OPTION-TYPE          PIC X.
00030              88  PO-FORMAT-OPTION       VALUE 'F'.
00031              88  PO-PROCESS-OPTION      VALUE 'P'.
00032              88  PO-TOTAL-OPTION        VALUE 'T'.
00033          16  PO-PGM-OPTION-CD        PIC X.
00034
00035      12  PO-OPTION-DESCRIPTION       PIC X(40).
00036
00037 ******************************************************************
00215      EJECT
00216 *    COPY ELCPGMN.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCPGMN.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PROGRAM DESCRIPTIONS                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 52    RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELPGMN                 RKP=2,LEN=5       *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  PROGRAM-DESCRIPTIONS.
00019      12  PN-RECORD-ID                PIC XX.
00020          88  VALID-PN-ID                VALUE 'PN'.
00021
00022      12  PN-CONTROL-PRIMARY.
00023          16  PN-PROGRAM-NUMBER.
00024              20  PN-SYSTEM-CODE      PIC XX.
00025                  88  CLAS-IC-ONLINE     VALUE 'EL'.
00026                  88  CLAS-IC-REPORT     VALUE 'EC'.
00027                  88  CLAS-GL-BATCH      VALUE 'GL'.
00028              20  PN-PROGRAM-SEQUENCE PIC 999.
00029      12  PN-PROGRAM-DESCRIPTION      PIC X(40).
00030      12  PN-TRANSACTION-CODE         PIC X(4).
00031      12  PN-ENTRY-METHOD             PIC X.
00032          88  ENTERED-FROM-CICS          VALUE 'C'.
00033          88  ENTERED-FROM-CLASIC-MENU   VALUE 'M'.
00034          88  ENTERED-FROM-BATCH         VALUE 'B'.
00035
00036 ******************************************************************
00217      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                PROGRAM-OPTIONS-AVAILABLE
                                PROGRAM-DESCRIPTIONS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL111' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00219
00220      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00221
00222      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00223      MOVE '5'                    TO DC-OPTION-CODE.
00224      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00225      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00226      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00227
00228      IF EIBCALEN = ZERO
00229          GO TO 8800-UNAUTHORIZED-ACCESS.
00230
00231      IF PI-CALLING-PROGRAM NOT = LIT-PGM
00232          IF PI-RETURN-TO-PROGRAM NOT = LIT-PGM
00233              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00234              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00235              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00236              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00237              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00238              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00239              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00240              MOVE LIT-PGM               TO  PI-CALLING-PROGRAM
00241              MOVE LOW-VALUES            TO  EL111AO
00242              GO TO 8100-SEND-MAP
00243          ELSE
00244              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00245              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00246              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00247              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00248              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00249              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00250              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00251              MOVE SPACES                TO  PI-SAVED-PROGRAM-6
00252              MOVE LOW-VALUES            TO  EL111AO
00253              GO TO 8100-SEND-MAP.
00254
00255
00256      IF EIBAID = DFHCLEAR
00257              GO TO 9000-XCTL.
00258
00259      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00260          MOVE LOW-VALUES         TO EL111AO
00261          MOVE LIT-IC             TO PFKEYL
00262          MOVE INVALID-PFKEY      TO MSGO
00263          GO TO 8110-SEND-DATA.
00264
00265      
      * EXEC CICS RECEIVE
00266 *        MAP    ('EL111A')
00267 *        MAPSET ('EL111S')
00268 *    END-EXEC.
           MOVE 'EL111A' TO DFHEIV1
           MOVE 'EL111S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00001203' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL111AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00269
00270      MOVE SPACES                 TO SCREEN-SWITCH MSGO.
00271      PERFORM 0200-SET-ATTRB THRU 0210-EXIT.
00272
00273      IF PFKEYL GREATER ZERO
00274          PERFORM 0100-TRANS-PF THRU 0110-EXIT.
00275
00276      IF SCREEN-ERROR
00277          MOVE LIT-IC             TO PFKEYL
00278          GO TO 8110-SEND-DATA.
00279
00280      IF EIBAID = DFHPF23
00281          GO TO 9100-XCTL.
00282
00283      IF EIBAID = DFHPF24
00284          GO TO 9200-XCTL.
00285
00286      IF EIBAID = DFHPF12
00287          GO TO 8300-GET-HELP.
00288
00289      IF EIBAID NOT = DFHENTER
00290          MOVE INVALID-PFKEY      TO MSGO
00291          MOVE LIT-IC             TO MAINTL
00292          GO TO 8110-SEND-DATA.
00293
00294      MOVE SPACE                  TO SCREEN-SWITCH.
00295      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
00296
00297      IF SCREEN-ERROR
00298          GO TO 8110-SEND-DATA.
00299
00300      PERFORM 1100-SAVE-DESC   THRU 1110-EXIT.
00301      PERFORM 1200-UPDATE-FILE THRU 1210-EXIT.
00302
00303      IF SCREEN-ERROR
00304          GO TO 8110-SEND-DATA.
00305
00306      MOVE LIT-IC                 TO MAINTL.
00307      MOVE SPACE                  TO MAINTO.
00308      MOVE SPACE                  TO DELOPTO.
00309
00310      IF NOT SHOW-OPTION
00311          MOVE END-UPDATE         TO MSGO.
00312
00313      PERFORM 0300-SET-DESC-ATTRB THRU 0310-EXIT.
00314
00315      GO TO 8110-SEND-DATA.
00316      EJECT
00317  0100-TRANS-PF.
00318      IF EIBAID NOT = DFHENTER
00319          MOVE INVALID-PFKEY      TO MSGO
00320          MOVE LIT-X              TO SCREEN-SWITCH
00321          GO TO 0110-EXIT.
00322
00323      IF PFKEYI NOT NUMERIC
00324          MOVE INVALID-PFKEY      TO MSGO
00325          MOVE LIT-X              TO SCREEN-SWITCH
00326          GO TO 0110-EXIT.
00327
00328      IF PFKEYI LESS 1 OR GREATER 24
00329          MOVE INVALID-PFKEY      TO MSGO
00330          MOVE LIT-X              TO SCREEN-SWITCH
00331          GO TO 0110-EXIT.
00332
00333      MOVE PFKEYI                  TO CHECK-PFKEYS.
00334      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
00335
00336  0110-EXIT.
00337      EXIT.
00338
00339  0200-SET-ATTRB.
00340      MOVE AL-UANON               TO MAINTA
00341                                     OPTTYPEA
00342                                     DELOPTA
00343                                     PGRMA.
00344
00345  0210-EXIT.
00346      EXIT.
00347
00348  0300-SET-DESC-ATTRB.
00349      MOVE AL-UANOF               TO DESC1A
00350                                     DESC2A
00351                                     DESC3A
00352                                     DESC4A
00353                                     DESC5A
00354                                     DESC6A
00355                                     DESC7A
00356                                     DESC8A.
00357
00358  0310-EXIT.
00359      EXIT.
00360      EJECT
00361  1000-EDIT-SCREEN.
00362      MOVE MAINTI                 TO CHECK-MAINT.
00363
00364      IF (NOT VALID-OPTION) OR
CIDMOD        (PI-PROCESSOR-ID NOT = 'LGXX' AND 'JJPA' AND 'PEMA'
CIDMOD                         AND NOT SHOW-OPTION)
00365 **      (PI-PROCESSOR-ID NOT = 'LGXX' AND NOT SHOW-OPTION)
00366          MOVE AL-UABON           TO MAINTA
00367          MOVE LIT-IC             TO MAINTL
00368          MOVE MAINT-MIS          TO MSGO
00369          MOVE LIT-X              TO SCREEN-SWITCH
00370          GO TO 1010-EXIT.
00371
00372      IF PGRMI = SPACES OR LOW-VALUES
00373          MOVE LIT-IC             TO PGRML
00374          MOVE AL-UABON           TO PGRMA
00375          MOVE PGM-MIS            TO MSGO
00376          MOVE LIT-X              TO SCREEN-SWITCH
00377          GO TO 1010-EXIT.
00378
00379      MOVE PGRMI                  TO PGM-NO.
00380      IF PGM-PREF NOT = LIT-CLAIM AND LIT-EC AND LIT-GL AND LIT-EM
00381          MOVE LIT-IC             TO PGRML
00382          MOVE AL-UABON           TO PGRMA
00383          MOVE PGM-MIS            TO MSGO
00384          MOVE LIT-X              TO SCREEN-SWITCH
00385          GO TO 1010-EXIT.
00386
00387      IF CHANGE-OPTION
00388          PERFORM 1050-VERIFY-PGM THRU 1070-EXIT.
00389
00390      IF SCREEN-ERROR
00391          MOVE LIT-IC             TO PGRML
00392          MOVE AL-UABON           TO PGRMA
00393          MOVE PGM-FILE-MIS       TO MSGO
00394          GO TO 1010-EXIT.
00395
00396      MOVE OPTTYPEI TO CHECK-TYPE.
00397
00398      IF NOT VALID-TYPE
00399          MOVE AL-UABON           TO OPTTYPEA
00400          MOVE LIT-IC             TO OPTTYPEL
00401          MOVE OPT-MIS            TO MSGO
00402          MOVE LIT-X              TO SCREEN-SWITCH
00403          GO TO 1010-EXIT.
00404
00405      PERFORM 1020-VERIFY-DEL-OPT THRU 1040-EXIT.
00406
00407  1010-EXIT.
00408      EXIT.
00409
00410  1020-VERIFY-DEL-OPT.
00411      IF DELOPTI = SPACES  AND
00412         NOT DELETE-OPTION
00413          GO TO 1040-EXIT.
00414
00415      IF DELOPTL = ZEROS  AND
00416         NOT DELETE-OPTION
00417          GO TO 1040-EXIT.
00418
00419      MOVE LIT-D                  TO SCREEN-SWITCH.
00420
00421      IF DELOPTI = LIT-A
00422          GO TO 1040-EXIT.
00423
00424      IF DELOPTI LESS '1'
00425          GO TO 1030-FORMAT-ERR.
00426
00427      IF DELOPTI GREATER '8'
00428          GO TO 1030-FORMAT-ERR.
00429
00430      IF DELETE-CODE-PRESENT AND DELETE-OPTION
00431          GO TO 1040-EXIT.
00432
00433      IF DELETE-OPTION AND NOT DELETE-CODE-PRESENT
00434          MOVE LIT-IC             TO DELOPTL
00435          MOVE AL-UABON           TO DELOPTA
00436          MOVE DEL-OPT-MIS        TO MSGO
00437          MOVE LIT-X              TO SCREEN-SWITCH
00438          GO TO 1040-EXIT.
00439
00440      IF DELETE-CODE-PRESENT AND NOT DELETE-OPTION
00441          MOVE LIT-IC             TO DELOPTL
00442          MOVE AL-UABON           TO DELOPTA
00443          MOVE DEL-OPT-ERROR      TO MSGO
00444          MOVE LIT-X              TO SCREEN-SWITCH.
00445
00446      GO TO 1040-EXIT.
00447
00448  1030-FORMAT-ERR.
00449      MOVE LIT-IC                 TO DELOPTL.
00450      MOVE AL-UABON               TO DELOPTA.
00451      MOVE DEL-OPT-INV            TO MSGO.
00452      MOVE LIT-X                  TO SCREEN-SWITCH.
00453
00454  1040-EXIT.
00455      EXIT.
00456
00457  1050-VERIFY-PGM.
00458      MOVE PGRMI                  TO NAME-KEY.
00459      
      * EXEC CICS HANDLE CONDITION
00460 *        NOTFND (1060-PGM-NOTFND)
00461 *    END-EXEC.
      *    MOVE '"$I                   ! " #00001399' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00462
00463      PERFORM 1680-READ-PGMN THRU 1690-EXIT.
00464      GO TO 1070-EXIT.
00465
00466  1060-PGM-NOTFND.
00467      MOVE LIT-X                  TO SCREEN-SWITCH.
00468
00469  1070-EXIT.
00470      EXIT.
00471      EJECT
00472  1100-SAVE-DESC.
00473      IF DESC1I GREATER LOW-VALUES
00474          MOVE DESC1I             TO HOLD-DESC (1)
00475      ELSE
00476          MOVE SPACES             TO HOLD-DESC (1).
00477
00478      IF DESC2I GREATER LOW-VALUES
00479          MOVE DESC2I             TO HOLD-DESC (2)
00480      ELSE
00481          MOVE SPACES             TO HOLD-DESC (2).
00482
00483      IF DESC3I GREATER LOW-VALUES
00484          MOVE DESC3I             TO HOLD-DESC (3)
00485      ELSE
00486          MOVE SPACES             TO HOLD-DESC (3).
00487
00488      IF DESC4I GREATER LOW-VALUES
00489          MOVE DESC4I             TO HOLD-DESC (4)
00490      ELSE
00491          MOVE SPACES             TO HOLD-DESC (4).
00492
00493      IF DESC5I GREATER LOW-VALUES
00494          MOVE DESC5I             TO HOLD-DESC (5)
00495      ELSE
00496          MOVE SPACES             TO HOLD-DESC (5).
00497
00498      IF DESC6I GREATER LOW-VALUES
00499          MOVE DESC6I             TO HOLD-DESC (6)
00500      ELSE
00501          MOVE SPACES             TO HOLD-DESC (6).
00502
00503      IF DESC7I GREATER LOW-VALUES
00504          MOVE DESC7I             TO HOLD-DESC (7)
00505      ELSE
00506          MOVE SPACES             TO HOLD-DESC (7).
00507
00508      IF DESC8I GREATER LOW-VALUES
00509          MOVE DESC8I             TO HOLD-DESC (8)
00510      ELSE
00511          MOVE SPACES             TO HOLD-DESC (8).
00512
00513  1110-EXIT.
00514      EXIT.
00515      EJECT
00516  1200-UPDATE-FILE.
00517      MOVE ZEROS                  TO COUNT-1.
00518      MOVE SPACES                 TO SCREEN-SWITCH UPDATE-SWITCH.
00519
00520      IF DELETE-OPTION
00521          PERFORM 1300-DELETE-OPTION THRU 1310-EXIT.
00522
00523      IF CHANGE-OPTION
00524          PERFORM 1400-CHANGE-OPTION THRU 1410-EXIT
00525                  UNTIL UPDATE-COMPLETE OR NOTHING-CHANGED.
00526
00527      IF UPDATE-COMPLETE
00528          MOVE ZEROES             TO COUNT-1
00529          MOVE SPACES             TO SCREEN-SWITCH UPDATE-SWITCH
00530          MOVE LIT-S              TO CHECK-MAINT.
00531
00532      IF SHOW-OPTION
00533          PERFORM 1500-SHOW-OPTION THRU 1540-EXIT.
00534
00535  1210-EXIT.
00536      EXIT.
00537      EJECT
00538  1300-DELETE-OPTION.
00539      MOVE PGRMI                  TO PGM-NO.
00540      MOVE OPTTYPEI               TO OPT-TYPE.
00541
00542      IF DELOPTI = LIT-A
00543          MOVE ZEROS              TO COUNT-1
00544          PERFORM 1320-DELETE-ALL THRU 1330-EXIT 9 TIMES
00545          GO TO 1310-EXIT.
00546
00547      MOVE DELOPTI                TO OPT-CODE.
00548      PERFORM 1340-DELETE-ONE THRU 1360-EXIT.
00549
00550  1310-EXIT.
00551      EXIT.
00552
00553  1320-DELETE-ALL.
00554      
      * EXEC CICS HANDLE CONDITION
00555 *        NOTFND (1330-EXIT)
00556 *    END-EXEC.
      *    MOVE '"$I                   ! # #00001494' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031343934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00557
00558      ADD 1 TO COUNT-1.
00559      MOVE COUNT-1 TO OPT-CODE.
00560      PERFORM 1600-READ-UPDATE THRU 1610-EXIT.
00561      MOVE LIT-D                     TO JP-RECORD-TYPE.
00562      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.
00563      PERFORM 1620-DELETE THRU 1630-EXIT.
00564      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00565
00566  1330-EXIT.
00567      EXIT.
00568
00569  1340-DELETE-ONE.
00570      
      * EXEC CICS HANDLE CONDITION
00571 *        NOTFND (1350-DELETE-NOTFND)
00572 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00001510' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00573
00574      PERFORM 1600-READ-UPDATE THRU 1610-EXIT.
00575      MOVE LIT-D                  TO JP-RECORD-TYPE.
00576      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.
00577      PERFORM 1620-DELETE THRU 1630-EXIT.
00578      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00579      GO TO 1360-EXIT.
00580
00581  1350-DELETE-NOTFND.
00582      MOVE LIT-IC                 TO DELOPTL.
00583      MOVE AL-UABON               TO DELOPTA.
00584      MOVE DEL-OPT-INV            TO MSGO.
00585      MOVE LIT-X                  TO SCREEN-SWITCH.
00586
00587  1360-EXIT.
00588      EXIT.
00589      EJECT
00590  1400-CHANGE-OPTION.
00591      MOVE PGRMI                  TO PGM-NO.
00592      MOVE OPTTYPEI               TO OPT-TYPE.
00593      MOVE SPACES                 TO FIND-SWITCH.
00594      PERFORM 1430-FIND-UPDATE THRU 1440-EXIT
00595          UNTIL END-UPDATES OR UPDATE-FOUND.
00596
00597      IF END-UPDATES
00598          PERFORM 1450-CHECK-UPDATE THRU 1460-EXIT
00599          GO TO 1410-EXIT.
00600
00601      MOVE LIT-A                  TO SCREEN-SWITCH.
00602      MOVE COUNT-1                TO OPT-CODE.
00603
00604      
      * EXEC CICS HANDLE CONDITION
00605 *        NOTFND (1410-ADD-OPTION)
00606 *    END-EXEC.
      *    MOVE '"$I                   ! % #00001544' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031353434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00607
00608      PERFORM 1600-READ-UPDATE THRU 1610-EXIT.
00609      MOVE LIT-B                  TO JP-RECORD-TYPE.
00610      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.
00611      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00612      MOVE HOLD-DESC (COUNT-1)    TO PO-OPTION-DESCRIPTION.
00613      MOVE LIT-C                  TO JP-RECORD-TYPE.
00614      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.
00615      PERFORM 1640-REWRITE THRU 1650-EXIT.
00616      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00617      GO TO 1410-EXIT.
00618
00619  1410-ADD-OPTION.
00620      
      * EXEC CICS GETMAIN
00621 *        SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)
00622 *        LENGTH  (PGMO-LENGTH)
00623 *        INITIMG (LIT-SPACE)
00624 *    END-EXEC.
      *    MOVE ',"IL                  $   #00001560' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 PGMO-LENGTH, 
                 LIT-SPACE
           SET ADDRESS OF PROGRAM-OPTIONS-AVAILABLE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00625
00626      MOVE BROWSE-KEY                TO PO-CONTROL-PRIMARY.
00627      MOVE HOLD-DESC (COUNT-1)       TO PO-OPTION-DESCRIPTION.
00628      MOVE LIT-PO                    TO PO-RECORD-ID.
00629      MOVE LIT-A                     TO JP-RECORD-TYPE.
00630      MOVE PROGRAM-OPTIONS-AVAILABLE TO JP-RECORD-AREA.
00631
00632      PERFORM 1660-WRITE THRU 1670-EXIT.
00633      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00634
00635  1410-EXIT.
00636      EXIT.
00637
00638  1430-FIND-UPDATE.
00639      ADD 1 TO COUNT-1.
00640
00641      IF COUNT-1 = 9
00642          MOVE LIT-E              TO FIND-SWITCH
00643          GO TO 1440-EXIT.
00644
00645      IF HOLD-DESC (COUNT-1) = SPACES
00646          GO TO 1440-EXIT.
00647
00648      MOVE LIT-D                  TO FIND-SWITCH.
00649
00650  1440-EXIT.
00651      EXIT.
00652
00653  1450-CHECK-UPDATE.
00654      IF UPDATES-MADE
00655          MOVE LIT-D              TO UPDATE-SWITCH
00656          GO TO 1460-EXIT.
00657
00658      MOVE LIT-E                  TO UPDATE-SWITCH.
00659      MOVE NO-UPDATES             TO MSGO.
00660      MOVE LIT-IC                 TO MAINTL.
00661      MOVE LIT-X                  TO SCREEN-SWITCH.
00662
00663  1460-EXIT.
00664      EXIT.
00665      EJECT
00666  1500-SHOW-OPTION.
00667      
      * EXEC CICS HANDLE CONDITION
00668 *        NOTFND (1530-SHOW-BUILD)
00669 *        ENDFILE (1520-END-BROWSE)
00670 *    END-EXEC.
      *    MOVE '"$I''                  ! & #00001607' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303031363037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00671
00672      MOVE PGRMI                  TO PGM-NO.
00673      MOVE OPTTYPEI               TO OPT-TYPE.
00674      MOVE SPACES                 TO OPT-CODE DESC-HOLD.
00675      MOVE BROWSE-KEY             TO SAVE-KEY.
00676      PERFORM 1700-START-BROWSE THRU 1710-EXIT.
00677      PERFORM 1720-RESET-BROWSE THRU 1730-EXIT.
00678
00679  1510-BROWSE-CONT.
00680      PERFORM 1740-READ-NEXT THRU 1750-EXIT.
00681      MOVE PO-CONTROL-PRIMARY     TO RECORD-KEY.
00682
00683      IF GENERIC-KEY NOT = SAVE-KEY
00684          GO TO 1520-END-BROWSE.
00685      ADD 1 TO COUNT-1.
00686
00687      MOVE OPT-CODE               TO HOLD-OPT.
00688      MOVE PO-OPTION-DESCRIPTION  TO HOLD-DESC (HOLD-OPT).
00689      GO TO 1510-BROWSE-CONT.
00690
00691  1520-END-BROWSE.
00692      PERFORM 1760-END-BROWSE THRU 1770-EXIT.
00693
00694  1530-SHOW-BUILD.
00695      IF DESC-HOLD = SPACES
00696          MOVE NO-OPTIONS-PRESENT TO MSGO
00697          GO TO 1540-EXIT.
00698
00699      MOVE HOLD-DESC (1)          TO DESC1O.
00700      MOVE HOLD-DESC (2)          TO DESC2O.
00701      MOVE HOLD-DESC (3)          TO DESC3O.
00702      MOVE HOLD-DESC (4)          TO DESC4O.
00703      MOVE HOLD-DESC (5)          TO DESC5O.
00704      MOVE HOLD-DESC (6)          TO DESC6O.
00705      MOVE HOLD-DESC (7)          TO DESC7O.
00706      MOVE HOLD-DESC (8)          TO DESC8O.
00707
00708  1540-EXIT.
00709      EXIT.
00710      EJECT
00711  1600-READ-UPDATE.
00712      
      * EXEC CICS READ
00713 *        SET (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)
00714 *        DATASET (LIT-FILE)
00715 *        RIDFLD (BROWSE-KEY)
00716 *        UPDATE
00717 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00001652' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-AVAILABLE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00718
00719  1610-EXIT.
00720      EXIT.
00721
00722  1620-DELETE.
00723      
      * EXEC CICS DELETE
00724 *        DATASET (LIT-FILE)
00725 *    END-EXEC.
      *    MOVE '&(                    &   #00001663' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00726
00727  1630-EXIT.
00728      EXIT.
00729
00730  1640-REWRITE.
00731      
      * EXEC CICS REWRITE
00732 *        FROM (PROGRAM-OPTIONS-AVAILABLE)
00733 *        DATASET (LIT-FILE)
00734 *    END-EXEC.
           MOVE LENGTH OF
            PROGRAM-OPTIONS-AVAILABLE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00001671' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 PROGRAM-OPTIONS-AVAILABLE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00735
00736  1650-EXIT.
00737      EXIT.
00738
00739  1660-WRITE.
00740      
      * EXEC CICS WRITE
00741 *        FROM (PROGRAM-OPTIONS-AVAILABLE)
00742 *        DATASET (LIT-FILE)
00743 *        RIDFLD (BROWSE-KEY)
00744 *    END-EXEC.
           MOVE LENGTH OF
            PROGRAM-OPTIONS-AVAILABLE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00001680' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 PROGRAM-OPTIONS-AVAILABLE, 
                 DFHEIV11, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00745
00746  1670-EXIT.
00747      EXIT.
00748
00749  1680-READ-PGMN.
00750      
      * EXEC CICS READ
00751 *        SET     (ADDRESS OF PROGRAM-DESCRIPTIONS)
00752 *        DATASET (LIT-FILE-2)
00753 *        RIDFLD  (NAME-KEY)
00754 *    END-EXEC.
      *    MOVE '&"S        E          (   #00001690' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE-2, 
                 DFHEIV20, 
                 DFHEIV99, 
                 NAME-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-DESCRIPTIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00755
00756  1690-EXIT.
00757      EXIT.
00758      EJECT
00759  1700-START-BROWSE.
00760      
      * EXEC CICS STARTBR
00761 *        DATASET   (LIT-FILE)
00762 *        RIDFLD    (BROWSE-KEY)
00763 *        KEYLENGTH (SHOW-LENGTH)
00764 *        GENERIC
00765 *        EQUAL
00766 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00001700' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 BROWSE-KEY, 
                 SHOW-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00767
00768  1710-EXIT.
00769      EXIT.
00770
00771  1720-RESET-BROWSE.
00772      
      * EXEC CICS RESETBR
00773 *        DATASET (LIT-FILE)
00774 *        RIDFLD  (BROWSE-KEY)
00775 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         G          &   #00001712' TO DFHEIV0
           MOVE X'263420202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00776
00777  1730-EXIT.
00778      EXIT.
00779
00780  1740-READ-NEXT.
00781      
      * EXEC CICS READNEXT
00782 *        DATASET (LIT-FILE)
00783 *        RIDFLD  (BROWSE-KEY)
00784 *        SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)
00785 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001721' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-AVAILABLE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00786
00787  1750-EXIT.
00788      EXIT.
00789
00790  1760-END-BROWSE.
00791      
      * EXEC CICS ENDBR
00792 *        DATASET (LIT-FILE)
00793 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001731' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00794
00795  1770-EXIT.
00796      EXIT.
00797      EJECT
00798  2000-JOURNAL-WRITE.
00799      MOVE LIT-SYS                TO JP-USER-ID.
00800      MOVE LIT-FILE               TO JP-FILE-ID.
00801      MOVE LIT-PGM                TO JP-PROGRAM-ID.
00802
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO
pemuni*        EXEC CICS JOURNAL
pemuni*            JFILEID (1)
pemuni*            JTYPEID ('EL')
pemuni*            FROM (JOURNAL-RECORD)
pemuni*            LENGTH (JOURNAL-LENGTH)
pemuni*        END-EXEC.
00810
00811  2010-EXIT.
00812      EXIT.
00813      EJECT
00814  8100-SEND-MAP.
00815      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.
00816      MOVE LIT-IC                 TO MAINTL.
00817      
      * EXEC CICS SEND
00818 *        MAP    ('EL111A')
00819 *        MAPSET ('EL111S')
00820 *        ERASE
00821 *        FREEKB
00822 *        CURSOR
00823 *    END-EXEC.
           MOVE 'EL111A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL111S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00001757' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL111AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00824
00825      GO TO 8120-RETURN-TRANS.
00826
00827  8110-SEND-DATA.
00828      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.
00829
00830      
      * EXEC CICS SEND
00831 *        MAP    ('EL111A')
00832 *        MAPSET ('EL111S')
00833 *        DATAONLY
00834 *        ERASEAUP
00835 *        FREEKB
00836 *        CURSOR
00837 *    END-EXEC.
           MOVE 'EL111A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL111S' TO DFHEIV2
      *    MOVE '8$D    CT  A F  H     ,   #00001770' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL111AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00838
00839  8120-RETURN-TRANS.
00840      
      * EXEC CICS RETURN
00841 *        TRANSID  (LIT-TRANS)
00842 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00843 *        LENGTH   (PI-COMM-LENGTH)
00844 *    END-EXEC.
      *    MOVE '.(CT                  &   #00001780' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-TRANS, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00845      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL111' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00846
00847  8130-FORMAT-DATE-TIME.
00848      MOVE SAVE-DATE              TO DATEO.
00849      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
00850 *    END-EXEC
      *    MOVE '0"A                   "   #00001789' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00851      
      * EXEC CICS FORMATTIME
00852 *              ABSTIME(LCP-CICS-TIME)
00853 *              TIME(LCP-TIME-OF-DAY-XX)
00854 *    END-EXEC
      *    MOVE 'j$(     (             #   #00001791' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00855      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.
00856      MOVE UN-HOURS               TO FOR-HOURS.
00857      MOVE UN-MINUTES             TO FOR-MINUTES.
00858      MOVE TIME-FORMATTED         TO TIMEO.
00859      MOVE LIT-MAP                TO PI-CURRENT-SCREEN-NO.
00860
00861  8140-EXIT.
00862      EXIT.
00863
00864  8200-NO-UPDATES.
00865      
      * EXEC CICS SEND TEXT
00866 *        FROM   (SCREEN-TERM)
00867 *        LENGTH (TERM-LENGTH)
00868 *        ERASE
00869 *        FREEKB
00870 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001805' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SCREEN-TERM, 
                 TERM-LENGTH, 
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
           
00871
00872  8220-RETURN-CICS.
00873      
      * EXEC CICS RETURN
00874 *    END-EXEC.
      *    MOVE '.(                    &   #00001813' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00875      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL111' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00876
00877  EJECT
00878
00879  8300-GET-HELP.
00880      MOVE LIT-HELP TO CALL-PGM.
00881      GO TO 9400-XCTL.
00882
00883      EJECT
00884  8800-UNAUTHORIZED-ACCESS.
00885      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
00886      GO TO 8800-SEND-TEXT.
00887
00888  8800-SEND-TEXT.
00889      
      * EXEC CICS SEND TEXT
00890 *        FROM    (LOGOFF-TEXT)
00891 *        LENGTH  (LOGOFF-LENGTH)
00892 *        ERASE
00893 *        FREEKB
00894 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001829' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383239' TO DFHEIV0(25:11)
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
           
00895
00896  EJECT
00897  9000-XCTL.
00898      MOVE PI-RETURN-TO-PROGRAM   TO CALL-PGM.
00899      GO TO 9400-XCTL.
00900
00901  9100-XCTL.
00902      MOVE XCTL-005               TO CALL-PGM.
00903      MOVE EIBAID                 TO PI-ENTRY-CD-1.
00904      GO TO 9400-XCTL.
00905
00906  9200-XCTL.
00907
00908      IF  CREDIT-SESSION
00909          MOVE XCTL-EL626         TO CALL-PGM
00910
00911      ELSE
00912          IF  CLAIM-SESSION
00913              MOVE XCTL-EL126     TO CALL-PGM
00914
00915          ELSE
00916              IF  MORTGAGE-SESSION
00917                  MOVE XCTL-EM626 TO CALL-PGM
00918
00919              ELSE
00920                  IF  GENERAL-LEDGER-SESSION
00921                      MOVE XCTL-GL800
00922                                  TO CALL-PGM.
00923
00924      GO TO 9400-XCTL.
00925
00926  9400-XCTL.
00927      
      * EXEC CICS XCTL
00928 *        PROGRAM (CALL-PGM)
00929 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00930 *        LENGTH   (PI-COMM-LENGTH)
00931 *    END-EXEC.
      *    MOVE '.$C                   $   #00001867' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00932
00933      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL111' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00934
00935  9700-LINK-DATE-CONVERT.
00936      
      * EXEC CICS LINK
00937 *        PROGRAM    ('ELDATCV')
00938 *        COMMAREA   (DATE-CONVERSION-DATA)
00939 *        LENGTH     (DC-COMM-LENGTH)
00940 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001876' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00941
00942  9700-EXIT.
00943      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL111' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 1060-PGM-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1330-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1350-DELETE-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1410-ADD-OPTION
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1530-SHOW-BUILD,
                     1520-END-BROWSE
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL111' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
