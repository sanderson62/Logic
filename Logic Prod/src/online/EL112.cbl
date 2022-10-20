00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL112 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:48:50.
00007 *                            VMOD=2.004.
00008 *
00008 *
00009 *AUTHOR.        LOGIC, INC.
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
00024 *REMARKS. TRANSACTION EX37 - PROGRAM FILE MAINTENANCE.
00025      EJECT
00026  ENVIRONMENT DIVISION.
00027  DATA DIVISION.
00028  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00029  01  LCP-TIME-OF-DAY-XX.
00030      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00031      05  FILLER                    PIC 99.
00032  01  LCP-CICS-TIME                 PIC 9(15).
00033
00034  77  FILLER  PIC X(32)  VALUE '********************************'.
00035  77  FILLER  PIC X(32)  VALUE '*    EL112 WORKING STORAGE     *'.
00036  77  FILLER  PIC X(32)  VALUE '*********** VMOD 2.004 *********'.
00037
00038  01  WS-DATE-AREA.
00039      05  SAVE-DATE               PIC X(8)    VALUE SPACES.
00040      05  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00041
00042  01  LITERALS-NUMBERS.
00043      12  XCTL-EL126              PIC X(5)    VALUE 'EL126'.
00044      12  XCTL-EL626              PIC X(5)    VALUE 'EL626'.
00045      12  XCTL-EM626              PIC X(5)    VALUE 'EM626'.
00046      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.
00047      12  LIT-A                   PIC X       VALUE 'A'.
00048      12  LIT-B                   PIC X       VALUE 'B'.
00049      12  LIT-C                   PIC X       VALUE 'C'.
00050      12  LIT-D                   PIC X       VALUE 'D'.
00051      12  LIT-E                   PIC X       VALUE 'E'.
00052      12  LIT-F                   PIC X       VALUE 'F'.
00053      12  LIT-S                   PIC X       VALUE 'S'.
00054      12  LIT-X                   PIC X       VALUE 'X'.
00055      12  LIT-PN                  PIC XX      VALUE 'PN'.
00056      12  LIT-CLAIM               PIC XX      VALUE 'EL'.
00057      12  LIT-GL                  PIC XX      VALUE 'GL'.
00058      12  LIT-MORTG               PIC XX      VALUE 'EM'.
00059      12  LIT-LF                  PIC XX      VALUE 'LF'.
00060      12  LIT-EC                  PIC XX      VALUE 'EC'.
00061      12  LIT-WA                  PIC XX      VALUE 'WA'.
00062      12  LIT-FILE                PIC X(8)    VALUE 'ELPGMN'.
00063      12  LIT-PROG                PIC X(8)    VALUE 'EL112'.
00064      12  SCREEN-NUMBER           PIC X(4)    VALUE '112A'.
00065      12  LIT-SYS                 PIC X(4)    VALUE 'SYS'.
00066      12  LIT-CICS                PIC X(4)    VALUE 'CICS'.
00067      12  LIT-MENU                PIC X(4)    VALUE 'MENU'.
00068      12  LIT-BATCH               PIC X(5)    VALUE 'BATCH'.
00069      12  LIT-SPACE               PIC X(5)    VALUE SPACE.
00070      12  LIT-TRANS               PIC X(4)    VALUE 'EX37'.
00071      12  CALL-PROG               PIC X(8)    VALUE 'EL119'.
00072      EJECT
00073  01  COUNT-FIELDS.
00074      12  COUNT-1                 PIC 99.
00075      12  COUNT-2                 PIC 99.
00076      12  COUNT-3                 PIC 99.
00077
00078  01  EDIT-WORK-AREA.
00079      12  CHECK-MAINT             PIC X.
00080          88  SHOW-OPTION                     VALUE 'S'.
00081          88  ADD-OPTION                      VALUE 'A'.
00082          88  CHANGE-OPTION                   VALUE 'C'.
00083          88  DELETE-OPTION                   VALUE 'D'.
00084          88  VALID-OPTION                  VALUE 'A' 'C' 'D' 'S'.
00085      12  CHECK-CALL              PIC X.
00086          88  CICS-CALL                       VALUE 'C'.
00087          88  MENU-CALL                       VALUE 'M'.
00088          88  BATCH-PROG                      VALUE 'B'.
00089          88  VALID-CALL                      VALUE 'M' 'C' 'B'.
00090      12  CHECK-PFKEYS            PIC 99.
00091      12  SCREEN-SWITCH           PIC X.
00092          88  END-OF-FILE                     VALUE 'E'.
00093          88  SCREEN-FULL                     VALUE 'F'.
00094          88  SCREEN-ERROR                    VALUE 'X'.
00095      12  BROWSE-KEY.
00096          16  PGM-PREF            PIC XX.
00097          16  FILLER              PIC X(3).
00098      EJECT
00099  01  DISPLAY-LINE.
00100      12  FILLER                  PIC X.
00101      12  PGM-NO                  PIC X(5).
00102      12  FILLER                  PIC X(5).
00103      12  PGM-DESC                PIC X(40).
00104      12  FILLER                  PIC X(6).
00105      12  PGM-TRANS               PIC X(4).
00106      12  FILLER                  PIC X(7).
00107      12  PGM-CALLED              PIC X(5).
00108
00109  01  TIME-UNFORMATTED.
00110      12  UN-HOURS                PIC XX.
00111      12  UN-MINUTES              PIC XX.
00112      12  FILLER                  PIC X(4).
00113
00114  01  TIME-FORMATTED.
00115      12  FOR-HOURS               PIC XX.
00116      12  FILLER                  PIC X       VALUE '.'.
00117      12  FOR-MINUTES             PIC XX.
00118      EJECT
00119  01  ERROR-MESSAGES.
00120      12  MAINT-MIS.
00121          16  FILLER              PIC X(39)
00122              VALUE 'MAINTENANCE FUNCTION INVALID OR UNAUTHO'.
00123          16  FILLER              PIC X(18)
00124              VALUE 'RIZED             '.
00125
00126      12  PGM-MIS.
00127          16  FILLER              PIC X(33)
00128              VALUE 'PROGRAM NUMBER MISSING OR INVALID'.
00129          16  FILLER              PIC X(18)
00130              VALUE ' - PLEASE RE-ENTER'.
00131
00132      12  PGM-DUP                 PIC X(42)
00133          VALUE 'PROGRAM NUMBER DUPLICATE - PLEASE RE-ENTER'.
00134
00135      12  DESC-MIS.
00136          16  FILLER              PIC X(38)
00137              VALUE 'PROGRAM DESCRIPTION MISSING OR INVALID'.
00138          16  FILLER              PIC X(18)
00139              VALUE ' - PLEASE RE-ENTER'.
00140      12  TRANS-MIS               PIC X(45)
00141          VALUE 'TRANSACTION MISSING OR INVALID - PLEASE ENTER'.
00142
00143      12  CALL-MIS.
00144          16  FILLER              PIC X(32)
00145              VALUE 'CALLABLE CODE MISSING OR INVALID'.
00146          16  FILLER              PIC X(18)
00147              VALUE ' - PLEASE RE-ENTER'.
00148      12  INVALID-PFKEY           PIC X(45)
00149          VALUE 'INVALID PF-OPTION REQUESTED - PLEASE RE-ENTER'.
00150
00151      12  END-MSG                 PIC X(11)
00152          VALUE 'END OF FILE'.
00153
00154      12  END-UPDATE              PIC X(16)
00155          VALUE 'UPDATE COMPLETED'.
00156
00157      12  SCREEN-TERM             PIC X(42)
00158            VALUE '     CLAS-IC PROGRAM MAINTENANCE COMPLETED'.
00159
00160      12  FILE-MSG                PIC X(25)
00161            VALUE '     FILE ELPGMN NOT OPEN'.
00162
00163  01  COMP-LENGTHS.
00164      12  LIT-IC                  PIC S9(4)  COMP VALUE -1.
00165      12  FILE-LENGTH             PIC S9(4)  COMP VALUE +25.
00166      12  TERM-LENGTH             PIC S9(4)  COMP VALUE +41.
00167      12  PGMN-LENGTH             PIC S9(4)  COMP VALUE +52.
00168      12  JOURNAL-LENGTH          PIC S9(4)  COMP VALUE +75.
00169      EJECT
00170 *    COPY ELCDATE.
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
00171      EJECT
00172 *    COPY ELCATTR.
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
00173      EJECT
00174 *    COPY ELCAID.
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
00175  01  FILLER REDEFINES DFHAID.
00176      12  FILLER                  PIC X(8).
00177      12  AID-KEYS OCCURS 24 TIMES.
00178          16  FILLER              PIC X.
00179
00180 *    COPY ELCINTF.
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
00181      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00182          16  SAVE-BEGIN          PIC X(5).
00183          16  SAVE-ENDING         PIC X(5).
00184          16  SHOW-SWITCH         PIC X.
00185              88  NOT-SHOWN                   VALUE 'X'.
00186          16  FILLER              PIC X(629).
00187      EJECT
00188 *    COPY ELCJPFX.
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
00189                                  PIC X(52).
00190      EJECT
00191 *    COPY EL112S.
       01  EL112AI.
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
           05  DESCL PIC S9(0004) COMP.
           05  DESCF PIC  X(0001).
           05  FILLER REDEFINES DESCF.
               10  DESCA PIC  X(0001).
           05  DESCI PIC  X(0040).
      *    -------------------------------
           05  TRANCDL PIC S9(0004) COMP.
           05  TRANCDF PIC  X(0001).
           05  FILLER REDEFINES TRANCDF.
               10  TRANCDA PIC  X(0001).
           05  TRANCDI PIC  X(0004).
      *    -------------------------------
           05  CALLL PIC S9(0004) COMP.
           05  CALLF PIC  X(0001).
           05  FILLER REDEFINES CALLF.
               10  CALLA PIC  X(0001).
           05  CALLI PIC  X(0001).
           05  PGMDESCD OCCURS 11  TIMES.
      *    -------------------------------
               10  PGMDESCL PIC S9(0004) COMP.
               10  PGMDESCF PIC  X(0001).
               10  FILLER REDEFINES PGMDESCF.
                   15  PGMDESCA PIC  X(0001).
               10  PGMDESCI PIC  X(0079).
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
       01  EL112AO REDEFINES EL112AI.
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
           05  DESCO PIC  X(0040).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRANCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CALLO PIC  X(0001).
      *    -------------------------------
           05  PGMDESCD OCCURS 11  TIMES.
               10  FILLER        PIC  X(0003).
               10  PGMDESCO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
00192      EJECT
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
00194  01  DFHCOMMAREA                 PIC X(1024).
00195
00196 *01 PARM-LIST .
00197 *    12  FILLER                  PIC S9(8)  COMP.
00198 *    12  PGMN-PNT                PIC S9(8)  COMP.
00199      EJECT
00200 *    COPY ELCPGMN.
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
00201      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                PROGRAM-DESCRIPTIONS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL112' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00203
00204      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00205
00206      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00207      MOVE '5'                    TO DC-OPTION-CODE.
00208      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00209      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00210      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00211
00212      IF EIBCALEN = ZERO
00213          MOVE HIGH-VALUES        TO SAVE-BEGIN
00214          MOVE LOW-VALUES         TO SAVE-ENDING
00215          MOVE LIT-X              TO SHOW-SWITCH
00216          MOVE LOW-VALUES         TO EL112AO
00217          MOVE ZEROS              TO COUNT-1
00218          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00219              UNTIL SCREEN-FULL
00220          MOVE LIT-IC             TO MAINTL
00221          GO TO 8100-SEND-MAP.
00222
00223      IF PI-CALLING-PROGRAM NOT = LIT-PROG
00224          IF PI-RETURN-TO-PROGRAM NOT = LIT-PROG
00225              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00226              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00227              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00228              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00229              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00230              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00231              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00232              MOVE LIT-PROG             TO  PI-CALLING-PROGRAM
00233            ELSE
00234              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00235              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00236              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00237              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00238              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00239              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00240              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00241              MOVE SPACES               TO  PI-SAVED-PROGRAM-6.
00242
00243      IF EIBTRNID NOT = LIT-TRANS
00244          MOVE HIGH-VALUES TO SAVE-BEGIN
00245          MOVE LOW-VALUES TO SAVE-ENDING
00246          MOVE LIT-X TO SHOW-SWITCH
00247          MOVE LOW-VALUES         TO EL112AO
00248          MOVE ZEROS              TO COUNT-1
00249          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00250              UNTIL SCREEN-FULL
00251          MOVE LIT-IC             TO MAINTL
00252          GO TO 8100-SEND-MAP.
00253
00254      IF EIBAID = DFHCLEAR
00255          GO TO 9400-CLEAR.
00256
00257      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00258          MOVE LOW-VALUES         TO EL112AO
00259          MOVE LIT-IC             TO PFKEYL
00260          MOVE INVALID-PFKEY      TO MSGO
00261          GO TO 8110-SEND-DATA.
00262
00263      
      * EXEC CICS RECEIVE
00264 *        MAP     ('EL112A')
00265 *        MAPSET  ('EL112S')
00266 *    END-EXEC.
           MOVE 'EL112A' TO DFHEIV1
           MOVE 'EL112S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00001082' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031303832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL112AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00267
00268      MOVE SPACE                  TO SCREEN-SWITCH.
00269
00270      IF PFKEYL GREATER ZERO
00271          PERFORM 0300-TRANS-PF THRU 0310-EXIT.
00272
00273      IF SCREEN-ERROR
00274          MOVE LIT-IC             TO PFKEYL
00275          GO TO 8110-SEND-DATA.
00276
00277      IF EIBAID = DFHPF23
00278          GO TO 9100-RETURN-CICS.
00279
00280      IF  EIBAID = DFHPF24
00281
00282          IF  CREDIT-SESSION
00283              MOVE XCTL-EL626     TO CALL-PROG
00284              GO TO 9000-XCTL
00285
00286          ELSE
00287              IF  CLAIM-SESSION
00288                  MOVE XCTL-EL126 TO CALL-PROG
00289                  GO TO 9000-XCTL
00290
00291              ELSE
00292                  IF  MORTGAGE-SESSION
00293                      MOVE XCTL-EM626
00294                                  TO CALL-PROG
00295                      GO TO 9000-XCTL
00296
00297                  ELSE
00298                      IF  GENERAL-LEDGER-SESSION
00299                          MOVE XCTL-GL800
00300                                  TO CALL-PROG
00301                          GO TO 9000-XCTL.
00302
00303      IF EIBAID = DFHPF12
00304          MOVE 'EL010'         TO  CALL-PROG
00305          GO TO 9000-XCTL.
00306
00307      IF EIBAID = DFHPF1
00308          GO TO 0100-PAGE-FORWARD.
00309
00310      IF EIBAID = DFHPF2
00311          GO TO 0200-PAGE-BACKWARD.
00312
00313      IF EIBAID NOT = DFHENTER
00314          MOVE INVALID-PFKEY      TO MSGO
00315          MOVE LIT-IC             TO MAINTL
00316          GO TO 8110-SEND-DATA.
00317
00318      PERFORM 0400-SET-ATTRB THRU 0410-EXIT.
00319      MOVE SPACE                  TO SCREEN-SWITCH.
00320      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
00321
00322      IF SCREEN-ERROR
00323          GO TO 8110-SEND-DATA.
00324
00325      PERFORM 1100-UPDATE-FILE THRU 1110-EXIT.
00326      MOVE LIT-IC                 TO MAINTL.
00327
00328      IF SHOW-OPTION
00329          MOVE SPACES             TO MSGO
00330          GO TO 8110-SEND-DATA
00331      ELSE
00332          MOVE LIT-X TO SHOW-SWITCH
00333          MOVE LOW-VALUES         TO EL112AO
00334          MOVE LIT-IC             TO MAINTL
00335          MOVE END-UPDATE         TO MSGO
00336          MOVE HIGH-VALUES        TO SAVE-BEGIN
00337          MOVE LOW-VALUES         TO SAVE-ENDING
00338          GO TO 8100-SEND-MAP.
00339      EJECT
00340  0100-PAGE-FORWARD.
00341      MOVE ZEROS                  TO COUNT-1.
00342      MOVE SPACES                 TO SCREEN-SWITCH  MSGO
00343                                     SHOW-SWITCH.
00344
00345      IF PGRML = ZEROS
00346          MOVE SAVE-ENDING TO BROWSE-KEY
00347      ELSE
00348          MOVE AL-UANOF           TO PGRMA
00349          MOVE PGRMI              TO BROWSE-KEY.
00350
00351      MOVE SPACES                 TO MAINTO
00352                                     DESCO
00353                                     TRANCDO
00354                                     CALLO.
00355
00356      
      * EXEC CICS HANDLE CONDITION
00357 *        NOTOPEN  (8230-NOT-OPEN)
00358 *        NOTFND   (0110-REC-NOT-FND)
00359 *    END-EXEC.
      *    MOVE '"$JI                  ! " #00001175' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031313735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00360
00361      
      * EXEC CICS STARTBR
00362 *        DATASET  (LIT-FILE)
00363 *        RIDFLD   (BROWSE-KEY)
00364 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001180' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00365
00366      PERFORM 3000-BUILD-FWD-PAGE THRU 3020-EXIT
00367          UNTIL SCREEN-FULL OR END-OF-FILE.
00368
00369      
      * EXEC CICS ENDBR
00370 *        DATASET (LIT-FILE)
00371 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001188' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031313838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00372
00373      MOVE LIT-IC                 TO MAINTL.
00374      GO TO 8110-SEND-DATA.
00375
00376  0110-REC-NOT-FND.
00377      MOVE HIGH-VALUES            TO SAVE-BEGIN SAVE-ENDING.
00378      PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00379          UNTIL SCREEN-FULL.
00380      MOVE LIT-IC                 TO MAINTL.
00381      MOVE END-MSG                TO MSGO.
00382      GO TO 8110-SEND-DATA.
00383      EJECT
00384  0200-PAGE-BACKWARD.
00385      MOVE 11                     TO COUNT-1.
00386      MOVE SPACES                 TO SCREEN-SWITCH  MSGO
00387                                     SHOW-SWITCH.
00388
00389      IF PGRML = ZEROS
00390          MOVE SAVE-BEGIN         TO BROWSE-KEY
00391      ELSE
00392          MOVE AL-UANOF           TO PGRMA
00393          MOVE PGRMI              TO BROWSE-KEY.
00394
00395      MOVE SPACES                 TO MAINTO
00396                                     DESCO
00397                                     TRANCDO
00398                                     CALLO.
00399
00400      
      * EXEC CICS HANDLE CONDITION
00401 *        NOTOPEN  (8230-NOT-OPEN)
00402 *        NOTFND   (0210-REC-NOT-FND)
00403 *    END-EXEC.
      *    MOVE '"$JI                  ! # #00001219' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031323139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00404
00405      
      * EXEC CICS STARTBR
00406 *        DATASET  (LIT-FILE)
00407 *        RIDFLD   (BROWSE-KEY)
00408 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001224' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00409
00410      PERFORM 3030-BUILD-BCK-PAGE THRU 3050-EXIT
00411          UNTIL SCREEN-FULL OR END-OF-FILE.
00412
00413      
      * EXEC CICS ENDBR
00414 *        DATASET (LIT-FILE)
00415 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001232' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031323332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00416
00417      MOVE COUNT-1                TO COUNT-3.
00418      PERFORM 3060-RAISE-PAGE THRU 3070-EXIT
00419          COUNT-3 TIMES.
00420      MOVE LIT-IC                 TO MAINTL.
00421      GO TO 8110-SEND-DATA.
00422
00423  0210-REC-NOT-FND.
00424      MOVE HIGH-VALUES            TO SAVE-BEGIN.
00425      MOVE ZEROS                  TO PGRML.
00426      GO TO 0200-PAGE-BACKWARD.
00427      EJECT
00428  0300-TRANS-PF.
00429      IF EIBAID NOT = DFHENTER
00430          MOVE INVALID-PFKEY      TO MSGO
00431          MOVE LIT-X              TO SCREEN-SWITCH
00432          GO TO 0310-EXIT.
00433
00434      IF PFKEYI NOT NUMERIC
00435          MOVE INVALID-PFKEY      TO MSGO
00436          MOVE LIT-X              TO SCREEN-SWITCH
00437          GO TO 0310-EXIT.
00438
00439      IF PFKEYI LESS 1 OR GREATER 24
00440          MOVE INVALID-PFKEY      TO MSGO
00441          MOVE LIT-X              TO SCREEN-SWITCH
00442          GO TO 0310-EXIT.
00443
00444      MOVE PFKEYI                 TO CHECK-PFKEYS.
00445      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
00446
00447  0310-EXIT.
00448      EXIT.
00449
00450  0400-SET-ATTRB.
00451      MOVE AL-UANON               TO MAINTA
00452                                     PGRMA
00453                                     DESCA
00454                                     TRANCDA
00455                                     CALLA.
00456
00457  0410-EXIT.
00458      EXIT.
00459      EJECT
00460  1000-EDIT-SCREEN.
00461      MOVE MAINTI                 TO CHECK-MAINT.
00462
00463      IF (NOT VALID-OPTION) OR
CIDMOD*       (PI-PROCESSOR-ID NOT = 'LGXX' AND NOT SHOW-OPTION)
CIDMOD        (PI-PROCESSOR-ID NOT = 'LGXX' AND 'PEMA' AND 'SMVA'
CIDMOD                        AND NOT SHOW-OPTION)
00465          MOVE LIT-IC             TO MAINTL
00466          MOVE AL-UABON           TO MAINTA
00467          MOVE MAINT-MIS          TO MSGO
00468          MOVE LIT-X              TO SCREEN-SWITCH
00469          GO TO 1010-EXIT.
00470
00471      IF PGRMI = ZEROS OR LOW-VALUES
00472          MOVE LIT-IC             TO PGRML
00473          MOVE AL-UABON           TO PGRMA
00474          MOVE PGM-MIS            TO MSGO
00475          MOVE LIT-X              TO SCREEN-SWITCH
00476          GO TO 1010-EXIT.
00477
00478      MOVE PGRMI                  TO BROWSE-KEY.
00479
00480      IF PGM-PREF NOT = LIT-CLAIM AND LIT-EC AND LIT-WA
00481        AND LIT-GL AND LIT-LF AND LIT-MORTG
00482          MOVE LIT-IC             TO PGRML
00483          MOVE AL-UABON           TO PGRMA
00484          MOVE PGM-MIS            TO MSGO
00485          MOVE LIT-X              TO SCREEN-SWITCH
00486          GO TO 1010-EXIT.
00487
00488      IF CHANGE-OPTION AND NOT-SHOWN
00489          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH
00490          MOVE ZEROS              TO COUNT-1
00491          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00492              UNTIL SCREEN-FULL
00493          MOVE LIT-S              TO CHECK-MAINT.
00494
00495      PERFORM 1020-VERIFY-PGM THRU 1050-VERIFY-PGM-EXIT.
00496
00497      IF SCREEN-ERROR OR DELETE-OPTION
00498          GO TO 1010-EXIT.
00499
00500      IF SHOW-OPTION
00501          MOVE SPACES             TO SHOW-SWITCH SCREEN-SWITCH
00502          MOVE ZEROS              TO COUNT-1
00503          PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00504              UNTIL SCREEN-FULL
00505          GO TO 1010-EXIT.
00506
00507      IF DESCI = SPACES OR LOW-VALUES
00508          MOVE LIT-IC             TO DESCL
00509          MOVE AL-UABON           TO DESCA
00510          MOVE DESC-MIS           TO MSGO
00511          MOVE LIT-X              TO SCREEN-SWITCH
00512          GO TO 1010-EXIT.
00513
00514      MOVE CALLI                  TO CHECK-CALL.
00515
00516      IF NOT VALID-CALL
00517          MOVE LIT-IC             TO CALLL
00518          MOVE AL-UABON           TO CALLA
00519          MOVE CALL-MIS           TO MSGO
00520          MOVE LIT-X              TO SCREEN-SWITCH.
00521
00522      IF TRANCDI = SPACES OR LOW-VALUES
00523         IF NOT BATCH-PROG
00524            MOVE LIT-IC           TO TRANCDL
00525            MOVE AL-UABON         TO TRANCDA
00526            MOVE TRANS-MIS        TO MSGO
00527            MOVE LIT-X            TO SCREEN-SWITCH
00528            GO TO 1010-EXIT.
00529
00530  1010-EXIT.
00531      EXIT.
00532      EJECT
00533  1020-VERIFY-PGM.
00534      
      * EXEC CICS HANDLE CONDITION
00535 *        NOTOPEN  (1040-RECORD-NOT-FOUND)
00536 *        NOTFND   (1040-RECORD-NOT-FOUND)
00537 *    END-EXEC.
      *    MOVE '"$JI                  ! $ #00001355' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00538
00539      MOVE PGRMI                  TO BROWSE-KEY.
00540
00541      
      * EXEC CICS READ
00542 *        DATASET  (LIT-FILE)
00543 *        RIDFLD   (BROWSE-KEY)
00544 *        SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)
00545 *    END-EXEC.
      *    MOVE '&"S        E          (   #00001362' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-DESCRIPTIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00546
00547      IF ADD-OPTION
00548          MOVE PGM-DUP            TO MSGO
00549          MOVE AL-UABON           TO PGRMA
00550          MOVE LIT-IC             TO PGRML
00551          MOVE LIT-X              TO SCREEN-SWITCH
00552          GO TO 1050-VERIFY-PGM-EXIT.
00553
00554      IF NOT SHOW-OPTION
00555          GO TO 1050-VERIFY-PGM-EXIT.
00556
00557      MOVE PN-PROGRAM-DESCRIPTION TO DESCO.
00558      MOVE PN-TRANSACTION-CODE    TO TRANCDO.
00559      MOVE PN-ENTRY-METHOD        TO CALLO.
00560      GO TO 1050-VERIFY-PGM-EXIT.
00561
00562  1040-RECORD-NOT-FOUND.
00563      IF ADD-OPTION
00564          GO TO 1050-VERIFY-PGM-EXIT.
00565
00566      MOVE PGM-MIS                TO MSGO.
00567      MOVE AL-UABON               TO PGRMA.
00568      MOVE LIT-IC                 TO PGRML.
00569      MOVE LIT-X                  TO SCREEN-SWITCH.
00570
00571  1050-VERIFY-PGM-EXIT.
00572      EXIT.
00573      EJECT
00574  1100-UPDATE-FILE.
00575      IF SHOW-OPTION
00576          GO TO 1110-EXIT.
00577
00578      IF ADD-OPTION
00579          PERFORM 1120-ADD-OPTION THRU 1140-EXIT.
00580
00581      IF CHANGE-OPTION
00582          PERFORM 1150-CHANGE-OPTION THRU 1160-EXIT.
00583
00584      IF DELETE-OPTION
00585          PERFORM 1170-DELETE-OPTION THRU 1180-EXIT.
00586
00587  1110-EXIT.
00588      EXIT.
00589
00590  1120-ADD-OPTION.
00591      
      * EXEC CICS HANDLE CONDITION
00592 *        NOTOPEN  (8230-NOT-OPEN)
00593 *        NOSPACE  (1130-FILE-FULL)
00594 *    END-EXEC.
      *    MOVE '"$JE                  ! % #00001412' TO DFHEIV0
           MOVE X'22244A452020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00595
00596      
      * EXEC CICS GETMAIN
00597 *        SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)
00598 *        LENGTH   (PGMN-LENGTH)
00599 *        INITIMG  (LIT-SPACE)
00600 *    END-EXEC.
      *    MOVE ',"IL                  $   #00001417' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 PGMN-LENGTH, 
                 LIT-SPACE
           SET ADDRESS OF PROGRAM-DESCRIPTIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00601
00602      MOVE LIT-PN                 TO PN-RECORD-ID.
00603      MOVE PGRMI                  TO BROWSE-KEY PN-PROGRAM-NUMBER.
00604      MOVE DESCI                  TO PN-PROGRAM-DESCRIPTION.
00605      MOVE TRANCDI                TO PN-TRANSACTION-CODE.
00606      MOVE CALLI                  TO PN-ENTRY-METHOD.
00607      MOVE LIT-A                  TO JP-RECORD-TYPE.
00608      MOVE PROGRAM-DESCRIPTIONS   TO JP-RECORD-AREA.
00609
00610      
      * EXEC CICS WRITE
00611 *        DATASET  (LIT-FILE)
00612 *        FROM     (PROGRAM-DESCRIPTIONS)
00613 *        RIDFLD   (BROWSE-KEY)
00614 *    END-EXEC.
           MOVE LENGTH OF
            PROGRAM-DESCRIPTIONS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00001431' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 PROGRAM-DESCRIPTIONS, 
                 DFHEIV11, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00615
00616      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00617
00618  1130-FILE-FULL.
00619      MOVE LIT-X                  TO SCREEN-SWITCH.
00620
00621  1140-EXIT.
00622      EXIT.
00623
00624  1150-CHANGE-OPTION.
00625      
      * EXEC CICS HANDLE CONDITION
00626 *        NOTOPEN (8230-NOT-OPEN)
00627 *    END-EXEC.
      *    MOVE '"$J                   ! & #00001446' TO DFHEIV0
           MOVE X'22244A202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303031343436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00628
00629      MOVE PGRMI                  TO BROWSE-KEY.
00630      PERFORM 1190-READ-FILE THRU 1200-EXIT.
00631      MOVE LIT-B                  TO JP-RECORD-TYPE.
00632      MOVE PROGRAM-DESCRIPTIONS   TO JP-RECORD-AREA.
00633      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00634      MOVE DESCI                  TO PN-PROGRAM-DESCRIPTION.
00635      MOVE TRANCDI                TO PN-TRANSACTION-CODE.
00636      MOVE CALLI                  TO PN-ENTRY-METHOD.
00637      MOVE LIT-C                  TO JP-RECORD-TYPE.
00638      MOVE PROGRAM-DESCRIPTIONS   TO JP-RECORD-AREA.
00639
00640      
      * EXEC CICS REWRITE
00641 *        DATASET  (LIT-FILE)
00642 *        FROM     (PROGRAM-DESCRIPTIONS)
00643 *    END-EXEC.
           MOVE LENGTH OF
            PROGRAM-DESCRIPTIONS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00001461' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 PROGRAM-DESCRIPTIONS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00644
00645      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00646
00647  1160-EXIT.
00648      EXIT.
00649
00650  1170-DELETE-OPTION.
00651      
      * EXEC CICS HANDLE CONDITION
00652 *        NOTOPEN (8230-NOT-OPEN)
00653 *    END-EXEC.
      *    MOVE '"$J                   ! '' #00001472' TO DFHEIV0
           MOVE X'22244A202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303031343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00654
00655      MOVE PGRMI                  TO BROWSE-KEY.
00656      PERFORM 1190-READ-FILE THRU 1200-EXIT.
00657      MOVE LIT-D                  TO JP-RECORD-TYPE.
00658      MOVE PROGRAM-DESCRIPTIONS   TO JP-RECORD-AREA.
00659
00660      
      * EXEC CICS DELETE
00661 *        DATASET (LIT-FILE)
00662 *    END-EXEC.
      *    MOVE '&(                    &   #00001481' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00663
00664      PERFORM 2000-JOURNAL-WRITE THRU 2010-EXIT.
00665
00666  1180-EXIT.
00667      EXIT.
00668
00669  1190-READ-FILE.
00670      
      * EXEC CICS READ
00671 *        DATASET  (LIT-FILE)
00672 *        RIDFLD   (BROWSE-KEY)
00673 *        SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)
00674 *        UPDATE
00675 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00001491' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-DESCRIPTIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00676
00677  1200-EXIT.
00678      EXIT.
00679
00680  2000-JOURNAL-WRITE.
00681      MOVE LIT-SYS                TO JP-USER-ID.
00682      MOVE LIT-FILE               TO JP-FILE-ID.
00683      MOVE LIT-PROG               TO JP-PROGRAM-ID.
00684
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO
pemuni*        EXEC CICS JOURNAL
pemuni*            JFILEID  (1)
pemuni*            JTYPEID  ('EL')
pemuni*            FROM     (JOURNAL-RECORD)
pemuni*            LENGTH   (JOURNAL-LENGTH)
pemuni*        END-EXEC.
00692
00693  2010-EXIT.
00694      EXIT.
00695      EJECT
00696  3000-BUILD-FWD-PAGE.
00697      
      * EXEC CICS HANDLE CONDITION
00698 *        ENDFILE  (3010-END-FILE)
00699 *        NOTFND   (3010-END-FILE)
00700 *    END-EXEC.
      *    MOVE '"$''I                  ! ( #00001518' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303031353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00701
00702      
      * EXEC CICS READNEXT
00703 *        DATASET  (LIT-FILE)
00704 *        RIDFLD   (BROWSE-KEY)
00705 *        SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)
00706 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001523' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-DESCRIPTIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00707
00708      MOVE SPACES                 TO DISPLAY-LINE.
00709      MOVE PN-PROGRAM-NUMBER      TO PGM-NO.
00710      MOVE PN-PROGRAM-DESCRIPTION     TO PGM-DESC.
00711      MOVE PN-TRANSACTION-CODE    TO PGM-TRANS.
00712      MOVE PN-ENTRY-METHOD TO CHECK-CALL.
00713
00714      IF CICS-CALL
00715          MOVE LIT-CICS           TO PGM-CALLED
00716      ELSE
00717          IF MENU-CALL
00718              MOVE LIT-MENU       TO PGM-CALLED
00719          ELSE
00720              MOVE LIT-BATCH      TO PGM-CALLED.
00721
00722      ADD 1                       TO COUNT-1.
00723      MOVE DISPLAY-LINE           TO PGMDESCO (COUNT-1).
00724
00725      IF COUNT-1 = 1
00726          MOVE PN-PROGRAM-NUMBER  TO SAVE-BEGIN.
00727
00728      IF COUNT-1 GREATER 10
00729          MOVE PN-PROGRAM-NUMBER  TO SAVE-ENDING
00730          MOVE LIT-F              TO SCREEN-SWITCH.
00731
00732      GO TO 3020-EXIT.
00733
00734  3010-END-FILE.
00735      PERFORM 3100-FILL-SCREEN THRU 3110-EXIT
00736          UNTIL SCREEN-FULL.
00737      MOVE PGM-NO                 TO BROWSE-KEY.
00738      MOVE BROWSE-KEY             TO SAVE-ENDING.
00739      MOVE END-MSG                TO MSGO.
00740      MOVE LIT-E                  TO SCREEN-SWITCH.
00741
00742  3020-EXIT.
00743      EXIT.
00744      EJECT
00745  3030-BUILD-BCK-PAGE.
00746      
      * EXEC CICS HANDLE CONDITION
00747 *        ENDFILE  (3040-END-FILE)
00748 *        NOTFND   (3040-END-FILE)
00749 *    END-EXEC.
      *    MOVE '"$''I                  ! ) #00001567' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303031353637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00750
00751      
      * EXEC CICS READPREV
00752 *        DATASET  (LIT-FILE)
00753 *        RIDFLD   (BROWSE-KEY)
00754 *        SET      (ADDRESS OF PROGRAM-DESCRIPTIONS)
00755 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00001572' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BROWSE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-DESCRIPTIONS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00756
00757      MOVE SPACES                 TO DISPLAY-LINE.
00758      MOVE PN-PROGRAM-NUMBER      TO PGM-NO.
00759      MOVE PN-PROGRAM-DESCRIPTION TO PGM-DESC.
00760      MOVE PN-TRANSACTION-CODE    TO PGM-TRANS.
00761      MOVE PN-ENTRY-METHOD        TO CHECK-CALL.
00762
00763      IF CICS-CALL
00764          MOVE LIT-CICS           TO PGM-CALLED
00765      ELSE
00766          IF MENU-CALL
00767              MOVE LIT-MENU       TO PGM-CALLED
00768          ELSE
00769              MOVE LIT-BATCH      TO PGM-CALLED.
00770
00771      MOVE DISPLAY-LINE           TO PGMDESCO (COUNT-1).
00772
00773      IF COUNT-1 = 11
00774          MOVE PN-PROGRAM-NUMBER  TO SAVE-ENDING.
00775
00776      IF COUNT-1 = 1
00777          MOVE PN-PROGRAM-NUMBER  TO SAVE-BEGIN
00778          MOVE LIT-F              TO SCREEN-SWITCH.
00779
00780      SUBTRACT 1 FROM COUNT-1.
00781      GO TO 3050-EXIT.
00782
00783  3040-END-FILE.
00784      MOVE PGM-NO                 TO BROWSE-KEY.
00785      MOVE BROWSE-KEY             TO SAVE-BEGIN.
00786      MOVE END-MSG                TO MSGO.
00787      MOVE LIT-E                  TO SCREEN-SWITCH.
00788
00789  3050-EXIT.
00790      EXIT.
00791      EJECT
00792  3060-RAISE-PAGE.
00793      MOVE 1                      TO COUNT-1.
00794      MOVE 2                      TO COUNT-2.
00795      PERFORM 3080-SHIFT-SCREEN THRU 3090-EXIT
00796           UNTIL COUNT-1 GREATER 10.
00797      MOVE SPACES                 TO PGMDESCO (COUNT-1).
00798
00799  3070-EXIT.
00800      EXIT.
00801
00802  3080-SHIFT-SCREEN.
00803      MOVE PGMDESCO (COUNT-2)     TO PGMDESCO (COUNT-1).
00804      ADD 1                       TO COUNT-1 COUNT-2.
00805
00806  3090-EXIT.
00807      EXIT.
00808
00809  3100-FILL-SCREEN.
00810      ADD 1                       TO COUNT-1.
00811      MOVE SPACES                 TO PGMDESCO (COUNT-1).
00812
00813      IF COUNT-1 GREATER 10
00814          MOVE LIT-F              TO SCREEN-SWITCH.
00815
00816  3110-EXIT.
00817      EXIT.
00818      EJECT
00819  8100-SEND-MAP.
00820      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.
00821
00822      
      * EXEC CICS SEND
00823 *        MAP     ('EL112A')
00824 *        MAPSET  ('EL112S')
00825 *        ERASE
00826 *        FREEKB
00827 *        CURSOR
00828 *    END-EXEC.
           MOVE 'EL112A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL112S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00001643' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL112AO, 
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
           
00829
00830      GO TO 8120-RETURN-TRANS.
00831
00832  8110-SEND-DATA.
00833      PERFORM 8130-FORMAT-DATE-TIME THRU 8140-EXIT.
00834      
      * EXEC CICS SEND
00835 *        MAP     ('EL112A')
00836 *        MAPSET  ('EL112S')
00837 *        DATAONLY
00838 *        ERASEAUP
00839 *        FREEKB
00840 *        CURSOR
00841 *    END-EXEC.
           MOVE 'EL112A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL112S' TO DFHEIV2
      *    MOVE '8$D    CT  A F  H     ,   #00001655' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL112AO, 
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
           
00842
00843  8120-RETURN-TRANS.
00844      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.
00845      
      * EXEC CICS RETURN
00846 *        TRANSID   (LIT-TRANS)
00847 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00848 *        LENGTH    (PI-COMM-LENGTH)
00849 *    END-EXEC.
      *    MOVE '.(CT                  &   #00001666' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIT-TRANS, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00850
00851      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL112' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00852
00853  8130-FORMAT-DATE-TIME.
00854      MOVE SAVE-DATE              TO DATEO.
00855      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
00856 *    END-EXEC
      *    MOVE '0"A                   "   #00001676' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00857      
      * EXEC CICS FORMATTIME
00858 *              ABSTIME(LCP-CICS-TIME)
00859 *              TIME(LCP-TIME-OF-DAY-XX)
00860 *    END-EXEC
      *    MOVE 'j$(     (             #   #00001678' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00861      MOVE  LCP-TIME-OF-DAY-68 TO TIME-UNFORMATTED.
00862      MOVE UN-HOURS               TO FOR-HOURS.
00863      MOVE UN-MINUTES             TO FOR-MINUTES.
00864      MOVE TIME-FORMATTED         TO TIMEO.
00865
00866  8140-EXIT.
00867      EXIT.
00868
00869  8200-NO-UPDATES.
00870      
      * EXEC CICS SEND TEXT
00871 *        FROM    (SCREEN-TERM)
00872 *        LENGTH  (TERM-LENGTH)
00873 *        ERASE
00874 *        FREEKB
00875 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001691' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363931' TO DFHEIV0(25:11)
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
           
00876
00877  8220-RETURN-CICS.
00878      
      * EXEC CICS RETURN
00879 *    END-EXEC.
      *    MOVE '.(                    &   #00001699' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00880
00881      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL112' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00882
00883  8230-NOT-OPEN.
00884      
      * EXEC CICS SEND TEXT
00885 *        FROM    (FILE-MSG)
00886 *        LENGTH  (FILE-LENGTH)
00887 *        ERASE
00888 *        FREEKB
00889 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001705' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-MSG, 
                 FILE-LENGTH, 
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
           
00890
00891      GO TO 8220-RETURN-CICS.
00892
00893  9000-XCTL.
00894      
      * EXEC CICS XCTL
00895 *        PROGRAM  (CALL-PROG)
00896 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00897 *        LENGTH   (PI-COMM-LENGTH)
00898 *    END-EXEC.
      *    MOVE '.$C                   $   #00001715' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PROG, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00899
00900      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL112' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00901
00902  9100-RETURN-CICS SECTION.
00903      MOVE 'EL005'                TO  CALL-PROG.
00904      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00905      GO TO 9000-XCTL.
00906
00907  9100-EXIT.
00908      EXIT.
00909
00910  9400-CLEAR SECTION.
00911      MOVE PI-RETURN-TO-PROGRAM  TO  CALL-PROG.
00912      GO TO 9000-XCTL.
00913
00914  9400-EXIT.
00915      EXIT.
00916
00917  9700-LINK-DATE-CONVERT.
00918      
      * EXEC CICS LINK
00919 *        PROGRAM    ('ELDATCV')
00920 *        COMMAREA   (DATE-CONVERSION-DATA)
00921 *        LENGTH     (DC-COMM-LENGTH)
00922 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001739' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00923
00924  9700-EXIT.
00925      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL112' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8230-NOT-OPEN,
                     0110-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8230-NOT-OPEN,
                     0210-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1040-RECORD-NOT-FOUND,
                     1040-RECORD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8230-NOT-OPEN,
                     1130-FILE-FULL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8230-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8230-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 3010-END-FILE,
                     3010-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3040-END-FILE,
                     3040-END-FILE
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL112' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
